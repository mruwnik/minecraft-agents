(ns engine.contract-ending-test
  "Contract jobs end with an ending (engine.contract): call keys frozen at call start, conditions, done?, :because,
  and how a root job's ending is told."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core.legacy :as legacy]
            [engine.core :as core]
            [engine.core.ending :as ending]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.choose :as choose]))

(deftest an-ending-names-its-cause-keyword
  (are [e k] (= k (contract/cause-key e))
    {:kind :condition-failed :cause :free-slots} :free-slots
    {:kind :condition-failed :cause {:fact :reach :why :no-path}} :reach
    {:kind :limit-reached :cause {:line :health :floor 12}} :health
    {:kind :nothing-possible :cause {:ways {:craft :no-table}}} :nothing-possible
    {:kind :done} :done))

(deftest ctx-end-refuses-an-unknown-kind
  (is (= {:kind :nothing-possible :cause :no-tree :data {:seen 3}}
         (ctx/end {} :nothing-possible :no-tree {:seen 3})))
  (is (thrown? js/Error (ctx/end {} :gave-up :no-tree))))

(deftest a-because-chain-is-capped-in-events
  (let [chain (reduce (fn [e k] {:kind :nothing-possible :cause k :because e :report {:uses {}}})
                      {:kind :condition-failed :cause :c0} [:c1 :c2 :c3 :c4 :c5])
        depth #(count (take-while some? (iterate :because %)))]
    (is (= 6 (depth chain)))
    (is (= 4 (depth (contract/for-event chain))))
    (is (not-any? :report (rest (take-while some? (iterate :because (contract/for-event chain))))))))

;; ---------------------------------------------------------------- call keys at submit

(def wayed
  {:conditions [] :ways [:carried :craft] :args nil
   :round (fn [c] (ctx/end c :done nil))})

(deftest call-keys-are-specced-for-every-job
  (let [reg {'wayed wayed 'plain {:conditions [] :round #(ctx/end % :done nil) :args nil}}]
    (is (nil? (expr/problem reg '(wayed {:ways #{:craft} :spend {:time 30 :items {"dirt" 4}} :floor {:health 12}
                                        :least :all :deadline 5}))))
    (is (nil? (expr/problem reg '(plain {:spend {:time 30}}))) "a job that does not read them still takes them")
    (is (re-find #":spend must be .*got" (expr/problem reg '(plain {:spend {:time "soon"}}))))
    (is (re-find #":least must be" (expr/problem reg '(plain {:least 0}))))
    (is (re-find #"wayed has no way :gather; its ways are :carried, :craft"
                 (expr/problem reg '(wayed {:ways #{:gather}}))))
    (is (re-find #"plain has no way :walk; it has no ways" (expr/problem reg '(plain {:ways #{:walk}}))))
    (is (re-find #"plain has no arg :bogus; it takes no args" (expr/problem reg '(plain {:bogus 1 :least 2})))
        "the unknown-key message does not list the call keys")))

;; ---------------------------------------------------------------- the engine

(def world (atom {}))

(defn setup [jobs & [triggers inventory]]
  (reset! world {})
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake (if inventory {:inventory inventory} {}))
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers (or triggers {})
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :clock clock :p p}))

(defn ev [seen source kind] (filterv #(= [source kind] [(:source %) (:kind %)]) @seen))
(defn job-ev [seen kind] (ev seen :job kind))

(defn raw-mem [eng root slots] (mem/job-mem (mem/view (:store eng)) root slots))

(defn gate [k cause] (fn [_] (when-not (get @world k) cause)))

(def leaf
  {:conditions [[:fn (gate :leaf-ok :no-leaf)]]
   :round (fn [c]
            (swap! world update :leaf-rounds (fnil inc 0))
            (swap! world assoc :leaf-call (ctx/frozen-call c) :leaf-way? (ctx/way? c :craft) :leaf-use (ctx/use c :tools))
            (if (:leaf-done @world) (ctx/end c :done nil) :continue))
   :ways [:carried :craft]
   :defaults {:spend {:time ::leaf-time} :ways #{:craft}}
   :settings {::leaf-time {:default 45}}})

(defn parent-round [ending-of]
  (fn ^:async parent-round [c]
    (let [e (await (ctx/call c :leaf leaf (merge {:use {:tools ["stone_pickaxe"]}} (:child-args @world))))]
      (swap! world assoc :child-ending e)
      (if (= :continue e) :continue (ending-of c e)))))

(def jobs
  {'parent {:conditions [] :round (parent-round (fn [c _] (ctx/end c :nothing-possible :no-leaf-way)))}
   'explains {:conditions [] :round (parent-round (fn [c _] (assoc (ctx/end c :nothing-possible :mine) :because {:kind :cut-off})))}
   'gated {:conditions [[:fn (gate :go {:fact :go :why :not-yet})]]
           :round (fn [c] (if-let [e (:end @world)] (apply ctx/end c e) :continue))}
   'abandons {:conditions [] :round (fn ^:async abandons [c]
                                      (await (ctx/call c :leaf leaf {}))
                                      (ctx/end c :nothing-possible :gave-up))}
   'finished {:conditions [] :round (fn [_] (swap! world assoc :ran true) :continue)
              :done? (fn [_] {:done? (boolean (:all-in @world)) :units :items :done 3 :left 0})}})

(deftest a-child-whose-conditions-fail-ends-condition-failed-without-a-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(parent) {})]
          (await (core/tick! eng))
          (is (nil? (:leaf-rounds @world)) "the child's round never ran")
          (is (= {:kind :condition-failed :cause :no-leaf} (dissoc (:child-ending @world) :report)))
          (is (map? (:report (:child-ending @world))) "the engine fills the report")
          (is (= {:kind :nothing-possible :cause :no-leaf-way :because {:kind :condition-failed :cause :no-leaf}}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause :because]))
              "the parent's ending carries the child's as its because")
          (is (= :no-leaf-way (:reason (:data (first (job-ev seen :stopped))))) "today's readers read :reason")
          (is (empty? (:list (core/state eng))))
          (is (empty? (raw-mem eng id []))))))))

(deftest a-job-that-says-its-because-keeps-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)]
          (core/submit! eng '(explains) {})
          (await (core/tick! eng))
          (is (= {:kind :cut-off} (:because (:data (first (job-ev seen :stopped)))))))))))

(deftest call-keys-are-frozen-at-call-start-with-defaults-and-the-callers-deadline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(parent {:deadline 9000000}) {})]
          (swap! world assoc :leaf-ok true :child-args {:spend {:items {"dirt" 2}}})
          (await (core/tick! eng))
          (is (= {:least 1 :ways #{:craft} :spend {:time 45 :items {"dirt" 2}} :use {:tools ["stone_pickaxe"]}
                  :deadline 9000000}
                 (:leaf-call @world)))
          (is (true? (:leaf-way? @world)))
          (is (= ["stone_pickaxe"] (:leaf-use @world)))
          (swap! world assoc :child-args {:spend {:time 1} :ways #{:carried}})
          (await (core/tick! eng))
          (is (= 45 (get-in @world [:leaf-call :spend :time])) "a later round's args do not change the call")
          (is (= #{:craft} (get-in (raw-mem eng id [:leaf]) [:contract :call :ways])))
          (swap! world assoc :leaf-done true)
          (await (core/tick! eng))
          (let [ended (last (job-ev seen :child_ended))]
            (is (= [:done :done] [(:status (:data ended)) (get-in ended [:data :ending :kind])]))
            (is (map? (get-in ended [:data :report])))))))))

(def digger
  {:conditions [] :defaults {:use {:tools :dig}}
   :round (fn [c] (swap! world assoc :dig-use (ctx/use c :tools) :dig-call (ctx/frozen-call c)) (ctx/end c :done nil))})

(defn pick [name left] {:name name :count 1 :durability left :max-durability 250})

(deftest a-default-use-is-the-choosers-list-at-call-start-and-a-given-one-wins
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'digger digger} nil [(pick "stone_pickaxe" 12) (pick "iron_pickaxe" 180)])]
          (core/submit! eng '(digger {:block "stone"}) {})
          (await (core/tick! eng))
          (is (= ["stone_pickaxe" "iron_pickaxe"] (:dig-use @world)))
          (is (= {:tools ["stone_pickaxe" "iron_pickaxe"]} (:use (:dig-call @world))))
          (core/submit! eng '(digger {:block "stone" :use {:tools ["iron_pickaxe"]}}) {})
          (await (core/tick! eng))
          (is (= ["iron_pickaxe"] (:dig-use @world)) "an explicit :use wins"))))))

(def purposes
  [[{:blocks :build} [{:name "dirt" :count 4} {:name "diamond" :count 1} {:name "oak_planks" :count 2}] {} {:blocks ["dirt" "oak_planks"]}]
   [{:blocks :seal} [{:name "dirt" :count 4} {:name "cobblestone" :count 2}] {} {:blocks ["cobblestone" "dirt"]}]
   [{:tools :attack} [(pick "iron_pickaxe" 100) (pick "stone_sword" 20) (pick "iron_axe" 100)] {} {:tools ["iron_axe" "stone_sword"]}]
   [{:tools :till} [(pick "iron_hoe" 100)] {} {:tools ["iron_hoe"]}]
   [{:tools :shear} [(pick "shears" 100)] {} {:tools ["shears"]}]])

(deftest a-default-use-covers-every-filler-and-tool-purpose
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[use-defaults inventory args expected] purposes]
          (let [job {:conditions [] :defaults {:use use-defaults}
                     :round (fn [c] (swap! world assoc :call (ctx/frozen-call c)) (ctx/end c :done nil))}
                {:keys [eng]} (setup {'purposed job} nil inventory)]
            (core/submit! eng (list 'purposed args) {})
            (await (core/tick! eng))
            (is (= expected (:use (:call @world))) (pr-str use-defaults))))))))

(deftest a-dig-default-without-a-block-or-pos-says-so
  (is (thrown-with-msg? js/Error #"neither :block nor :pos"
                        (choose/default-use {:args {} :primitives (tu/fake {})} :tools :dig))))

(deftest a-listed-job-waits-on-its-conditions-then-any-ending-ends-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(gated) {})]
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [id] (:list (core/state eng))) "not started: it waits")
          (is (= [{:reason :go :cause {:fact :go :why :not-yet}}] (mapv #(select-keys (:data %) [:reason :cause]) (job-ev seen :waiting))))
          (is (empty? (job-ev seen :round_started)))
          (swap! world assoc :go true)
          (await (core/tick! eng))
          (is (= 1 (count (job-ev seen :round_started))))
          (swap! world assoc :go false)
          (await (core/tick! eng))
          (is (= {:status :stopped :kind :condition-failed :cause {:fact :go :why :not-yet} :reason :go}
                 (select-keys (:data (first (job-ev seen :stopped))) [:status :kind :cause :reason]))
              "once started, failing conditions end it")
          (is (empty? (:list (core/state eng)))))))))

(deftest a-done-ending-completes-and-a-ready-job-ends-without-a-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)]
          (swap! world assoc :go true :end [:done nil])
          (core/submit! eng '(gated) {})
          (await (core/tick! eng))
          (is (= {:status :completed :kind :done} (select-keys (:data (first (job-ev seen :completed))) [:status :kind])))
          (swap! world assoc :all-in true)
          (core/submit! eng '(finished) {})
          (await (core/tick! eng))
          (is (nil? (:ran @world)) "done? said it was done: no round")
          (is (= {:units :items :done 3 :left 0}
                 (select-keys (:report (:data (second (job-ev seen :completed)))) [:units :done :left]))))))))

(def never
  {:conditions [[:fn (gate :go :not-yet)] [:fn (fn [_] (contract/impossible {:reason :not-smeltable :item "diamond"}))]]
   :round (fn [_] (swap! world assoc :never-ran true) :continue)})

(def looks
  "Waits on :tree; the condition can tell only after a look from here."
  {:conditions [[:fn (fn [c] (cond (:tree @world) nil
                                   (:looked (ctx/mem c)) (when-not (:tree-after-look @world) :no-tree)
                                   :else (contract/look-first :no-tree)))]]
   :round (fn [c] (swap! world update :looks-rounds (fnil inc 0)) (ctx/end c :done nil))})

(defn looks-at [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

(deftest an-impossible-condition-ends-the-job-and-it-leaves-the-queue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'never never
                                         'calls-never {:conditions []
                                                       :round (fn ^:async calls-never [c]
                                                                (swap! world assoc :child-ending (await (ctx/call c :kid never {})))
                                                                (ctx/end c :nothing-possible :kid))}})]
          (core/submit! eng '(never) {})
          (await (core/tick! eng))
          (is (nil? (:never-ran @world)) "its round never ran")
          (is (empty? (:list (core/state eng))) "removed from the queue, though another condition only skips")
          (is (empty? (job-ev seen :waiting)))
          (is (= {:status :stopped :kind :impossible :cause {:reason :not-smeltable :item "diamond"} :reason :not-smeltable}
                 (select-keys (:data (first (job-ev seen :stopped))) [:status :kind :cause :reason])))
          (core/submit! eng '(calls-never) {})
          (await (core/tick! eng))
          (is (= {:kind :impossible :cause {:reason :not-smeltable :item "diamond"}} (dissoc (:child-ending @world) :report))))))))

(def holds
  "Its goal holds while :holds; done? keeps the call it was asked under."
  {:conditions [[:fn (gate :go :not-needed)]]
   :defaults {:spend {:time 20}}
   :round (fn [_] (swap! world assoc :holds-ran true) :continue)
   :done? (fn [c] (swap! world assoc :asked-call (ctx/frozen-call c))
            {:done? (boolean (:holds @world)) :units :items :done 0 :left 0})})

(deftest a-goal-that-already-holds-ends-done-before-its-conditions-are-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'holds holds
                                         'calls-holds {:conditions []
                                                       :round (fn ^:async calls-holds [c]
                                                                (swap! world assoc :child-ending (await (ctx/call c :kid holds {})))
                                                                (ctx/end c :done nil))}})]
          (swap! world assoc :holds true)
          (core/submit! eng '(holds) {})
          (await (core/tick! eng))
          (is (empty? (job-ev seen :waiting)) "never waits on the failing condition")
          (is (nil? (:holds-ran @world)) "no round")
          (is (empty? (:list (core/state eng))))
          (is (= {:kind :done :cause :already} (select-keys (:data (first (job-ev seen :completed))) [:kind :cause])))
          (is (= {:units :items :done 0 :left 0}
                 (select-keys (:report (:data (first (job-ev seen :completed)))) [:units :done :left])))
          (is (= {:time 20} (:spend (:asked-call @world))) "done? reads the call it would start with")
          (core/submit! eng '(calls-holds) {})
          (await (core/tick! eng))
          (is (= {:kind :done :cause :already} (select-keys (:child-ending @world) [:kind :cause])) "a child too")
          (swap! world assoc :holds false)
          (core/submit! eng '(holds) {})
          (await (core/tick! eng))
          (is (= [:not-needed] (mapv #(:reason (:data %)) (job-ev seen :waiting))) "a goal not held waits as before"))))))

(def wraps-up
  "Reaches its goal in its first round and yields; its second round wraps up."
  {:conditions []
   :round (fn [c]
            (if (:holds @world)
              (do (swap! world assoc :wrapped true) (ctx/end c :done nil {:placed 1}))
              (do (swap! world assoc :holds true) :continue)))
   :done? (fn [_] {:done? (boolean (:holds @world)) :units :blocks :done 1 :left 0})})

(deftest once-started-the-jobs-own-round-decides-it-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'wraps-up wraps-up})]
          (core/submit! eng '(wraps-up) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (:wrapped @world) "the wrap-up round ran though done? held")
          (is (= {:kind :done :placed 1} (select-keys (:data (first (job-ev seen :completed))) [:kind :cause :placed])))
          (is (= :blocks (:units (:report (:data (first (job-ev seen :completed)))))) "done?'s count is still reported"))))))

(def done-throws
  {:conditions [[:fn (gate :go :not-yet)]]
   :round (fn [c] (swap! world assoc :ran true) (ctx/end c :done nil))
   :done? (fn [_] (throw (js/Error. "no input")))})

(deftest a-throwing-done-before-the-start-counts-as-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'done-throws done-throws})
              id (core/submit! eng '(done-throws) {})]
          (await (core/tick! eng))
          (is (= [id] (:list (core/state eng))) "a failing condition still waits")
          (is (= [:not-yet] (mapv #(:reason (:data %)) (job-ev seen :waiting))))
          (swap! world assoc :go true)
          (await (core/tick! eng))
          (is (:ran @world) "the round runs")
          (is (= :done (:kind (:data (first (job-ev seen :completed))))))
          (is (seq (job-ev seen :done_failed)) "the throw is told"))))))

(def held-triggers
  {:held {:name :held :job '(holds) :persistence :retry :backoff {:after 2 :first-s 60 :max-s 600}
          :when (fn [_ _ _] true)}})

(deftest a-reflex-whose-goal-already-holds-is-a-fruitless-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'holds holds} held-triggers)
              fired #(count (ev seen :reflex :fired))]
          (swap! world assoc :holds true)
          (core/register-reflex! eng {:trigger :held})
          (dotimes [_ 5] (await (core/tick! eng)))
          (is (= 2 (fired)) "backs off after :after runs that ended :already")
          (is (seq (core/backoff-entries eng))))))))

(deftest an-impossible-condition-wins-over-a-goal-that-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'never (assoc never :done? (constantly {:done? true}))})]
          (core/submit! eng '(never) {})
          (await (core/tick! eng))
          (is (= :impossible (:kind (:data (first (job-ev seen :stopped))))))
          (is (empty? (job-ev seen :completed))))))))

(def tried
  {:conditions [[:fn (ctx/after-tries 2 (fn [_] (:cause @world)))]]
   :round (fn [c] (swap! world assoc :tried-ran true) (ctx/end c :done nil))})

(deftest a-job-held-up-n-times-turns-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'tried tried})
              step! (fn ^:async step [cause] (swap! world assoc :cause cause) (await (core/tick! eng)))]
          (core/submit! eng '(tried) {})
          (await (step! :a))
          (await (step! :a))
          (is (= 1 (:tries (raw-mem eng "j1" []))) "a wait that goes on is one try")
          (await (step! :b))
          (is (= 2 (:tries (raw-mem eng "j1" []))))
          (is (seq (:list (core/state eng))) "still queued after two tries")
          (await (step! :c))
          (is (empty? (:list (core/state eng))) "the third check is impossible")
          (is (nil? (:tried-ran @world)))
          (is (= {:kind :impossible :cause {:reason :tries :tries 2 :cause :c}}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause]))))))))

(deftest a-reason-whose-detail-changes-is-one-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'tried tried})
              step! (fn ^:async step [cause] (swap! world assoc :cause cause) (await (core/tick! eng)))]
          (core/submit! eng '(tried) {})
          (await (step! {:reason :far :distance 9}))
          (await (step! {:reason :far :distance 7}))
          (await (step! {:reason :far :distance 5}))
          (is (= 1 (:tries (raw-mem eng "j1" []))) "same reason kind, detail changing")
          (is (seq (:list (core/state eng))))
          (await (step! {:reason :dark}))
          (is (= 2 (:tries (raw-mem eng "j1" []))) "a new reason kind is a try"))))))

(deftest a-condition-that-needs-a-look-looks-once-then-the-job-waits-unstarted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'looks looks})
              id (core/submit! eng '(looks) {})]
          (await (core/tick! eng))
          (let [n (looks-at p)]
            (is (pos? n) "it looked")
            (is (nil? (:looks-rounds @world)) "the job's round did not run")
            (is (= [id] (:list (core/state eng))) "still queued")
            (is (nil? (:call (:contract (raw-mem eng id [])))) "not started")
            (await (core/tick! eng))
            (await (core/tick! eng))
            (is (= n (looks-at p)) "no second look from the same place")
            (is (= [:no-tree] (mapv #(:reason (:data %)) (job-ev seen :waiting)))))
          (swap! world assoc :tree true)
          (await (core/tick! eng))
          (is (= 1 (:looks-rounds @world)))
          (is (= 1 (count (job-ev seen :completed)))))))))

(deftest a-look-that-finds-it-starts-the-job-in-the-same-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'looks looks})]
          (swap! world assoc :tree-after-look true)
          (core/submit! eng '(looks) {})
          (await (core/tick! eng))
          (is (pos? (looks-at p)))
          (is (= 1 (:looks-rounds @world)))
          (is (= 1 (count (job-ev seen :completed)))))))))

(deftest a-cancelled-job-is-cut-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(gated) {})]
          (core/cancel! eng id)
          (is (= {:kind :cut-off :cause :agent} (select-keys (:data (first (job-ev seen :cancelled))) [:kind :cause]))))))))

(deftest a-started-child-left-behind-is-cut-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(abandons) {})]
          (swap! world assoc :leaf-ok true)
          (await (core/tick! eng))
          (is (= [[:continue nil] [:cut-off {:kind :cut-off :cause :ended}]]
                 (mapv (juxt (comp :status :data) (comp :ending :data))
                       (filter #(= (str id "/leaf") (get-in % [:context :job-id] (:job %))) (job-ev seen :child_ended)))))
          (is (nil? (:because (:data (first (job-ev seen :stopped))))) "a child that went on is no because"))))))

;; ---------------------------------------------------------------- started frames left behind

(defn cut-offs
  "[job-id cause] of each child_ended :cut-off in seen."
  [seen]
  (->> (job-ev seen :child_ended)
       (filter #(= :cut-off (:status (:data %))))
       (mapv (juxt #(get-in % [:context :job-id] (:job %)) #(get-in % [:data :ending :cause])))))

(defn ^:async next-turn [] (await (js/Promise. (fn [r] (js/setTimeout r 0)))))

(def reflex-jobs
  {'abandons (get jobs 'abandons)
   'holds {:conditions [] :round (fn ^:async holds [c]
                                   (await (ctx/call c :leaf leaf {}))
                                   (await (js/Promise. (fn [r] (swap! world assoc :release r))))
                                   (ctx/end c :done nil))}
   'old-done {:conditions [] :round (fn ^:async old-done [c] (await (ctx/call c :leaf leaf {})) (ctx/end c :done nil))}})

(def reflex-triggers
  (into {} (for [k [:abandons :holds :old-done]]
             [k {:name k :job (list (symbol (name k))) :persistence :retry :when (fn [_ _ _] (= k (:fire @world)))}])))

(deftest a-reflex-root-that-ends-cuts-off-its-started-contract-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup reflex-jobs reflex-triggers)]
          (core/register-reflex! eng {:trigger :abandons})
          (core/register-reflex! eng {:trigger :old-done})
          (swap! world assoc :leaf-ok true :fire :abandons)
          (await (core/tick! eng))
          (let [[stopped] (job-ev seen :stopped)
                [ended] (ev seen :reflex :ended)
                id (get-in ended [:context :job-id] (:job ended))]
            (is (= {:kind :nothing-possible :cause :gave-up :reason :gave-up}
                   (select-keys (:data stopped) [:kind :cause :reason]))
                "a contract reflex's ending is told as job.stopped")
            (is (= [:stopped :gave-up] [(get-in ended [:data :outcome]) (get-in ended [:data :reason])]))
            (is (map? (get-in ended [:data :report])))
            (is (= [[(str id "/leaf") :ended]] (cut-offs seen))))
          (swap! world assoc :fire :old-done)
          (await (core/tick! eng))
          (is (= 2 (count (cut-offs seen))) "an old reflex ending :done cuts off its started child too"))))))

(deftest a-reflex-dropped-cuts-off-its-started-contract-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[by drop!] [[:death #(core/record-body-event! % #js {:kind "died"})] [:shutdown core/shutdown!]]]
          (let [{:keys [eng seen]} (setup reflex-jobs reflex-triggers)]
            (core/register-reflex! eng {:trigger :holds})
            (swap! world assoc :leaf-ok true :fire :holds)
            (let [round (core/tick! eng)]
              (await (next-turn))
              (drop! eng)
              ((:release @world))
              (await round))
            (let [[ended] (ev seen :reflex :ended)
                  id (get-in ended [:context :job-id] (:job ended))]
              (is (= [:dropped {:kind :cut-off :cause by}]
                     [(get-in ended [:data :outcome]) (get-in ended [:data :ending])]))
              (is (map? (get-in ended [:data :report])))
              (is (= [[(str id "/leaf") by]] (cut-offs seen))))))))))

(def refusing-jobs
  {'never never
   'gives-up {:conditions [] :round (fn [c] (swap! world assoc :call (ctx/frozen-call c)) (ctx/end c :nothing-possible :gave-up))}})

(def refusing-triggers
  (into {} (for [k [:never :gives-up]]
             [k {:name k :job (list (symbol (name k))) :persistence :retry :backoff false
                 :when (fn [_ _ _] (contains? (:fire @world) k))}])))

(deftest a-reflex-ending-impossible-fires-again-only-once-its-trigger-let-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup refusing-jobs refusing-triggers)
              fired (fn [k] (count (filter #(= k (get-in % [:context :reflex-id])) (ev seen :reflex :fired))))]
          (core/register-reflex! eng {:trigger :never})
          (core/register-reflex! eng {:trigger :gives-up})
          (swap! world assoc :go true :fire #{:never})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= 1 (fired :never)) "impossible: not fired again while the trigger holds")
          (is (= :impossible (get-in (first (ev seen :reflex :ended)) [:data :kind])))
          (is (= {:job 'never :kind :impossible :cause {:reason :not-smeltable :item "diamond"}}
                 (select-keys (:data (mem/latest (mem/view (:store eng)) :job-report)) [:job :kind :cause]))
              "the ending is remembered with the report")
          (swap! world assoc :fire #{:gives-up})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= 3 (fired :gives-up)) "nothing-possible: tried again as its persistence says")
          (swap! world assoc :fire #{:never})
          (await (core/tick! eng))
          (is (= 2 (fired :never)) "the trigger let go and holds again: a new cause"))))))

(deftest a-register-entry-may-set-its-jobs-spend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup refusing-jobs refusing-triggers)]
          (core/register-reflex! eng {:trigger :gives-up :job '(gives-up {:spend {:time 30}})})
          (swap! world assoc :fire #{:gives-up})
          (await (core/tick! eng))
          (is (= {:time 30} (:spend (:call @world)))))))))

(deftest an-abandoned-slot-is-cut-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'drops {:conditions []
                                                 :round (fn ^:async drops [c]
                                                          (if (:drop @world)
                                                            (ctx/update-mem! c update :children dissoc :leaf)
                                                            (await (ctx/call c :leaf leaf {})))
                                                          :continue)}})
              id (core/submit! eng '(drops) {})]
          (swap! world assoc :leaf-ok true)
          (await (core/tick! eng))
          (is (empty? (cut-offs seen)))
          (swap! world assoc :drop true)
          (await (core/tick! eng))
          (is (= [[(str id "/leaf") :abandoned]] (cut-offs seen))))))))

;; ---------------------------------------------------------------- old jobs (engine.core.legacy)

(def old-kid
  {:check (fn [c] (or (not (:need @world)) (ctx/wait c {:reason :need :what "thing"})))
   :round (fn [c]
            (swap! world update :old-rounds (fnil inc 0))
            (let [r (:old @world)] (if (map? r) (do (ctx/result! c r) :done) r)))})

(def calls-old
  {:conditions [] :round (fn ^:async calls-old [c]
                           (swap! world assoc :old-ending (await (ctx/call c :kid old-kid {})))
                           :continue)})

(deftest an-old-child-called-with-ctx-call-ends-with-an-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'calls-old calls-old})
              ending #(let [e (:old-ending @world)] (if (map? e) (dissoc e :report) e))]
          (core/submit! eng '(calls-old) {})
          (are [world-state want]
               (do (reset! world world-state) (await (core/tick! eng)) (= want (ending)))
            {:need true} {:kind :condition-failed :cause {:reason :need :what "thing"}}
            {:old :continue} :continue
            {:old :declined} {:kind :condition-failed :cause :declined}
            {:need true} {:kind :condition-failed :cause {:reason :need :what "thing"}}
            {:old {:status :stopped :reason :timeout :text "slow"}} {:kind :limit-reached :cause {:line :time :reason :timeout} :data {:text "slow"}}
            {:old {:status :stopped :reason :no-way}} {:kind :nothing-possible :cause :no-way}
            {:old {:status :stopped :reason :no-tool :need "stone_pickaxe"}}
            {:kind :condition-failed :cause :no-tool :data {:need "stone_pickaxe"}}
            {:old {:status :done :got 3}} {:kind :done :cause nil :data {:got 3}}
            {:old :done} {:kind :done :cause nil})
          (is (map? (:report (:old-ending @world))))
          (is (= {:status :done :ending {:kind :done :cause nil}}
                 (select-keys (:data (last (job-ev seen :child_ended))) [:status :ending]))))))))

(def waits-on
  "An old job whose check waits with the reason in :wait of the world, its round yielding."
  {:check (fn [c] (or (not (:wait @world)) (ctx/wait c {:reason (:wait @world)})))
   :round (constantly :continue)})

(defn calls-waiting [job]
  {:conditions [] :round (fn ^:async calls-waiting [c]
                           (swap! world assoc :old-ending (await (ctx/call c :kid job {})))
                           :continue)})

(deftest an-old-childs-check-declining-after-its-first-round-is-condition-failed-whatever-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [reason [:cooking :no-source]]
          (let [{:keys [eng]} (setup {'calls-waiting (calls-waiting waits-on)})
                ending #(let [e (:old-ending @world)] (if (map? e) (select-keys e [:kind :cause]) e))]
            (core/submit! eng '(calls-waiting) {})
            (reset! world {})
            (await (core/tick! eng))
            (is (= :continue (ending)))
            (swap! world assoc :wait reason)
            (await (core/tick! eng))
            (is (= {:kind :condition-failed :cause reason} (ending)) "a pause is a noop child, never a decline")))))))

(deftest an-old-child-with-saved-memory-but-no-round-yet-declining-at-once-is-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'calls-seeded {:conditions []
                                                   :round (fn ^:async calls-seeded [c]
                                                            (ctx/update-mem! c assoc-in [:children :kid] {:args {} :children {} :saved 1})
                                                            (swap! world assoc :old-ending (await (ctx/call c :kid waits-on {})))
                                                            :continue)}})]
          (core/submit! eng '(calls-seeded) {})
          (reset! world {:wait :cooking})
          (await (core/tick! eng))
          (is (= {:kind :condition-failed :cause :cooking} (select-keys (:old-ending @world) [:kind :cause]))))))))

(deftest an-old-child-declining-before-its-first-round-is-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'calls-old calls-old})]
          (core/submit! eng '(calls-old) {})
          (reset! world {:need true})
          (await (core/tick! eng))
          (is (= {:kind :condition-failed :cause {:reason :need :what "thing"}} (dissoc (:old-ending @world) :report)))
          (await (core/tick! eng))
          (is (= :condition-failed (:kind (:old-ending @world))) "no child memory was kept, so again before its first round"))))))

(deftest an-old-stopped-reason-reads-as-the-doc-kind
  (are [reason kind cause]
       (= {:kind kind :cause cause} (legacy/ending-of :done {:status :stopped :reason reason} nil))
    :no-tool :condition-failed :no-tool
    :hungry :limit-reached {:line :food :reason :hungry}
    :danger :condition-failed :danger
    :hostile-near :condition-failed :hostile-near
    :gone :condition-failed :gone
    :no-target :condition-failed :no-target
    :no-source :condition-failed :no-source
    :no-tree :condition-failed :no-tree
    :no-sapling :condition-failed :no-sapling
    :no-composter :condition-failed :no-composter
    :no-boat :condition-failed :no-boat
    :no-vehicle :condition-failed :no-vehicle
    :not-found :condition-failed :not-found
    :chest-lacks :condition-failed :chest-lacks
    :no-food :condition-failed :no-food
    :timeout :limit-reached {:line :time :reason :timeout}
    :low-health :limit-reached {:line :health :reason :low-health}
    :tool-spent :condition-failed :tool-spent
    :bad-args :impossible :bad-args
    :bad-pos :impossible :bad-pos
    :unsupported :impossible :unsupported
    :no-pen :impossible :no-pen
    :unreachable :nothing-possible :unreachable
    :cannot :nothing-possible :cannot
    :no-stone-found :nothing-possible :no-stone-found
    :no-drops :nothing-possible :no-drops
    :not-mounted :done nil
    :no-hostile :done nil
    :no-way :nothing-possible :no-way))

(deftest a-limit-reason-survives-the-round-trip-to-an-old-parent
  (is (= :low-health (:reason (legacy/result-of (legacy/ending-of :done {:status :stopped :reason :low-health} nil))))))

(def migrated
  {:conditions [] :round (fn [c] (ctx/end c :done nil)) :args nil
   :legacy-args (fn [{:keys [timeout-s] :as args}]
                  (cond-> (dissoc args :timeout-s) timeout-s (assoc :spend {:time timeout-s})))})

(deftest legacy-args-map-an-old-callers-options-to-call-keys
  (let [reg {'migrated migrated}]
    (is (nil? (expr/problem reg '(migrated {:timeout-s 30}))))
    (is (= {:time 30} (:spend (second (expr/leaf reg 'migrated {:timeout-s 30})))))
    (is (re-find #"migrated has no arg :bogus" (expr/problem reg '(migrated {:bogus 1}))))))

(deftest registering-a-job-with-a-bad-fact-form-is-refused-naming-it
  (are [form re] (thrown-with-msg? js/Error re (ending/wrap {:conditions [[:holds form :why]]}))
    '(>= (inventory) 4) #"inventory"
    '(held-for 5 (daytime)) #"held-for"
    '(>= (carried "torch") 4) #"carried"))
