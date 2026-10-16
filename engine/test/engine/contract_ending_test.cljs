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
  {:ways [:carried :craft] :args nil
   :round (fn [c] (ctx/end c :done nil))})

(deftest call-keys-are-specced-for-every-job
  (let [reg {'wayed wayed 'plain {:round #(ctx/end % :done nil) :args nil}}]
    (is (nil? (expr/problem reg '(wayed {:ways #{:craft} :spend {:job-time 30 :items {"dirt" 4}} :floor {:health 12}
                                        :least :all :deadline 5}))))
    (is (nil? (expr/problem reg '(plain {:spend {:job-time 30}}))) "a job that does not read them still takes them")
    (is (re-find #":spend must be .*got" (expr/problem reg '(plain {:spend {:job-time "soon"}}))))
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

(defn gate [k reason] (fn [_] (if (get @world k) (contract/ok :open "open") (contract/later reason (name reason)))))

(defn goal "A done? status: :ok while holds?, data the count." [holds? & data]
  (apply (if holds? contract/ok contract/later) (if holds? :done :not-done) "goal" data))

(def leaf
  {:can-start? (gate :leaf-ok :no-leaf)
   :round (fn [c]
            (swap! world update :leaf-rounds (fnil inc 0))
            (swap! world assoc :leaf-call (ctx/frozen-call c) :leaf-way? (ctx/way? c :craft) :leaf-use (ctx/use c :tools))
            (if (:leaf-done @world) (ctx/end c :done nil) :continue))
   :ways [:carried :craft]
   :defaults {:spend {:job-time ::leaf-time} :ways #{:craft}}
   :settings {::leaf-time {:default 45}}})

(defn parent-round [ending-of]
  (fn ^:async parent-round [c]
    (let [e (await (ctx/call c :leaf leaf (merge {:use {:tools ["stone_pickaxe"]}} (:child-args @world))))]
      (swap! world assoc :child-ending e)
      (if (= :continue e) :continue (ending-of c e)))))

(def jobs
  {'parent {:round (parent-round (fn [c _] (ctx/end c :nothing-possible :no-leaf-way)))}
   'explains {:round (parent-round (fn [c _] (assoc (ctx/end c :nothing-possible :mine) :because {:kind :cut-off})))}
   'gated {:can-start? (gate :go :not-yet)
           :round (fn [c] (if-let [e (:end @world)] (apply ctx/end c e) :continue))}
   'abandons {:round (fn ^:async abandons [c]
                                      (await (ctx/call c :leaf leaf {}))
                                      (ctx/end c :nothing-possible :gave-up))}
   'finished {:round (fn [_] (swap! world assoc :ran true) :continue)
              :done? (fn [_] (goal (:all-in @world) :units :items :done 3 :left 0))}})

(deftest a-child-whose-conditions-fail-ends-condition-failed-without-a-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(parent) {})]
          (await (core/tick! eng))
          (is (nil? (:leaf-rounds @world)) "the child's round never ran")
          (is (= {:kind :condition-failed :cause {:reason :no-leaf :text "no-leaf"}} (dissoc (:child-ending @world) :report)))
          (is (map? (:report (:child-ending @world))) "the engine fills the report")
          (is (= {:kind :nothing-possible :cause :no-leaf-way :because {:kind :condition-failed :cause {:reason :no-leaf :text "no-leaf"}}}
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
          (is (= {:least 1 :ways #{:craft} :spend {:job-time 45 :items {"dirt" 2}} :use {:tools ["stone_pickaxe"]}
                  :deadline 9000000}
                 (:leaf-call @world)))
          (is (true? (:leaf-way? @world)))
          (is (= ["stone_pickaxe"] (:leaf-use @world)))
          (swap! world assoc :child-args {:spend {:job-time 1} :ways #{:carried}})
          (await (core/tick! eng))
          (is (= 45 (get-in @world [:leaf-call :spend :job-time])) "a later round's args do not change the call")
          (is (= #{:craft} (get-in (raw-mem eng id [:leaf]) [:contract :call :ways])))
          (swap! world assoc :leaf-done true)
          (await (core/tick! eng))
          (let [ended (last (job-ev seen :child_ended))]
            (is (= [:done :done] [(:status (:data ended)) (get-in ended [:data :ending :kind])]))
            (is (map? (get-in ended [:data :report])))))))))

(def digger
  {:defaults {:use {:tools :dig}}
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
          (let [job {:defaults {:use use-defaults}
                     :round (fn [c] (swap! world assoc :call (ctx/frozen-call c)) (ctx/end c :done nil))}
                {:keys [eng]} (setup {'purposed job} nil inventory)]
            (core/submit! eng (list 'purposed args) {})
            (await (core/tick! eng))
            (is (= expected (:use (:call @world))) (pr-str use-defaults))))))))

(deftest a-dig-default-without-a-block-or-pos-names-no-tools-and-leaves-the-ending-to-the-job
  (is (nil? (choose/default-use {:args {} :primitives (tu/fake {})} :tools :dig))))

(deftest a-listed-job-waits-on-its-conditions-then-any-ending-ends-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup jobs)
              id (core/submit! eng '(gated) {})]
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= [id] (:list (core/state eng))) "not started: it waits")
          (is (= [:not-yet] (mapv #(:reason (:data %)) (job-ev seen :waiting))))
          (is (empty? (job-ev seen :round_started)))
          (swap! world assoc :go true :end [:nothing-possible :stuck])
          (await (core/tick! eng))
          (is (= 1 (count (job-ev seen :round_started))))
          (is (= {:status :stopped :kind :nothing-possible :cause :stuck}
                 (select-keys (:data (first (job-ev seen :stopped))) [:status :kind :cause])))
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
  {:can-start? (constantly (contract/never :not-smeltable "diamond" :item "diamond"))
   :round (fn [_] (swap! world assoc :never-ran true) :continue)})

(def looks
  "Waits on :tree; the condition can tell only after a look from here."
  {:can-start? (fn [c] (cond (or (:tree @world) (and (:looked (ctx/mem c)) (:tree-after-look @world))) (contract/ok :tree "tree")
                             (:looked (ctx/mem c)) (contract/later :no-tree "no tree")
                             :else (contract/look :no-tree "no tree seen")))
   :round (fn [c] (swap! world update :looks-rounds (fnil inc 0)) (ctx/end c :done nil))})

(defn looks-at [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

(deftest an-impossible-condition-ends-the-job-and-it-leaves-the-queue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'never never
                                         'calls-never {:round (fn ^:async calls-never [c]
                                                                (swap! world assoc :child-ending (await (ctx/call c :kid never {})))
                                                                (ctx/end c :nothing-possible :kid))}})]
          (core/submit! eng '(never) {})
          (await (core/tick! eng))
          (is (nil? (:never-ran @world)) "its round never ran")
          (is (empty? (:list (core/state eng))) "removed from the queue, though another condition only skips")
          (is (empty? (job-ev seen :waiting)))
          (is (= {:status :stopped :kind :impossible :cause {:reason :not-smeltable :item "diamond" :text "diamond"} :reason :not-smeltable}
                 (select-keys (:data (first (job-ev seen :stopped))) [:status :kind :cause :reason])))
          (core/submit! eng '(calls-never) {})
          (await (core/tick! eng))
          (is (= {:kind :impossible :cause {:reason :not-smeltable :item "diamond" :text "diamond"}} (dissoc (:child-ending @world) :report))))))))

(def holds
  "Its goal holds while :holds; done? keeps the call it was asked under."
  {:can-start? (gate :go :not-needed)
   :defaults {:spend {:job-time 20}}
   :round (fn [_] (swap! world assoc :holds-ran true) :continue)
   :done? (fn [c] (swap! world assoc :asked-call (ctx/frozen-call c))
            (goal (:holds @world) :units :items :done 0 :left 0))})

(deftest a-goal-that-already-holds-ends-done-before-its-conditions-are-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'holds holds
                                         'calls-holds {:round (fn ^:async calls-holds [c]
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
          (is (= {:job-time 20} (:spend (:asked-call @world))) "done? reads the call it would start with")
          (core/submit! eng '(calls-holds) {})
          (await (core/tick! eng))
          (is (= {:kind :done :cause :already} (select-keys (:child-ending @world) [:kind :cause])) "a child too")
          (swap! world assoc :holds false)
          (core/submit! eng '(holds) {})
          (await (core/tick! eng))
          (is (= [:not-needed] (mapv #(:reason (:data %)) (job-ev seen :waiting))) "a goal not held waits as before"))))))

(defn held-goal [start]
  {:can-start? (constantly start)
   :round (fn [_] (swap! world assoc :ran true) :continue)
   :done? (constantly (goal true :units :items :done 0 :left 0))})

(deftest bad-args-beat-a-goal-that-holds-but-another-never-does-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'bad (held-goal (contract/never :bad-args "count 0"))
                                         'other (held-goal (contract/never :not-smeltable "no"))
                                         'bare {:round (fn [c] (ctx/end c :done nil))}})
              ended (fn ^:async e [spec] (core/submit! eng spec {}) (await (core/tick! eng))
                      (select-keys (:data (last (filter #(#{:completed :stopped} (:kind %)) @seen))) [:kind :cause]))]
          (is (= {:kind :impossible :cause {:reason :bad-args :text "count 0"}} (await (ended '(bad)))))
          (is (= {:kind :done :cause :already} (await (ended '(other)))))
          (is (= {:kind :done} (select-keys (await (ended '(bare))) [:kind])) "a job with only a round runs"))))))

(def done-throws
  {:can-start? (gate :go :not-yet)
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

(def looks-blind
  "A condition no look can settle: it says :look for ever."
  {:can-start? (fn [_] (contract/look :unseen "never seen"))
   :round (fn [c] (swap! world update :looks-rounds (fnil inc 0)) (ctx/end c :done nil))})

(deftest a-look-that-settles-nothing-is-later-in-every-round-after-the-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'looks-blind looks-blind})]
          (core/submit! eng '(looks-blind) {})
          (await (core/tick! eng))
          (let [n (looks-at p)]
            (is (pos? n) "it looked once")
            (dotimes [_ 3] (await (core/tick! eng)))
            (is (= n (looks-at p)) "no look in later rounds")
            (is (nil? (:looks-rounds @world)) "never started")
            (is (= [:unseen] (distinct (mapv #(:reason (:data %)) (job-ev seen :waiting)))))))))))

(def looks-up
  "Starts once the cell above the body was looked at; until then it asks for a look at it."
  {:can-start? (fn [c]
                 (if (some #(and (= "look" (.-name %)) (= 70 (js/Math.floor (.-y (.-pos (.-args %))))))
                           (.-calls (.-world (:primitives c))))
                   (contract/ok :seen "seen")
                   (contract/look :unseen "cell above never seen" :look-at [[0 70 0]])))
   :round (fn [c] (swap! world update :looks-rounds (fnil inc 0)) (ctx/end c :done nil))})

(deftest a-look-status-names-the-cells-the-look-round-aims-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'looks-up looks-up})
              sensed-at (.-sensedAt p)]
          (aset p "sensedAt" (fn [pos] (if (= 70 (.-y pos)) #js {:unknown true} (sensed-at pos))))
          (core/submit! eng '(looks-up) {})
          (await (core/tick! eng))
          (is (= 1 (:looks-rounds @world)) "the look at the named cell made the check ok at once")
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
   'holds {:round (fn ^:async holds [c]
                                   (await (ctx/call c :leaf leaf {}))
                                   (await (js/Promise. (fn [r] (swap! world assoc :release r))))
                                   (ctx/end c :done nil))}
   'old-done {:round (fn ^:async old-done [c] (await (ctx/call c :leaf leaf {})) (ctx/end c :done nil))}})

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
  {'never {:round (fn [c] (ctx/end c :impossible {:reason :not-smeltable :item "diamond" :text "diamond"}))}
   'gives-up {:round (fn [c] (swap! world assoc :call (ctx/frozen-call c)) (ctx/end c :nothing-possible :gave-up))}})

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
          (is (= {:job 'never :kind :impossible :cause {:reason :not-smeltable :item "diamond" :text "diamond"}}
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
          (core/register-reflex! eng {:trigger :gives-up :job '(gives-up {:spend {:job-time 30}})})
          (swap! world assoc :fire #{:gives-up})
          (await (core/tick! eng))
          (is (= {:job-time 30} (:spend (:call @world)))))))))

(deftest an-abandoned-slot-is-cut-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'drops {:round (fn ^:async drops [c]
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

;; ---------------------------------------------------------------- old results (engine.core.legacy)

(deftest an-old-stopped-reason-reads-as-the-doc-kind
  (are [reason kind cause]
       (= {:kind kind :cause cause} (legacy/ending-of {:status :stopped :reason reason}))
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
    :timeout :limit-reached {:line :job-time :reason :timeout}
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
  (is (= :low-health (:reason (legacy/result-of (legacy/ending-of {:status :stopped :reason :low-health}))))))
