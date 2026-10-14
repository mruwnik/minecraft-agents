(ns engine.contract-ending-test
  "Contract jobs end with an ending (engine.contract): call keys frozen at call start, conditions, done?, :because,
  and how a root job's ending is told."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core.legacy :as legacy]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

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
  (let [reg {'wayed wayed 'plain {:check (constantly true) :round (constantly :done) :args nil}}]
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

(defn setup [jobs & [triggers]]
  (reset! world {})
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers (or triggers {})
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :clock clock}))

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

(def calls-leaf
  "An old job whose round starts the contract leaf (it goes on) and ends :done, or stopped with the world's :old-result."
  {:check (constantly true)
   :round (fn ^:async calls-leaf [c]
            (await (ctx/call c :leaf leaf {}))
            (when-let [r (:old-result @world)] (ctx/result! c r))
            :done)})

(deftest an-old-root-that-ends-cuts-off-its-started-contract-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'calls-leaf calls-leaf
                                         'calls-kid {:check (constantly true)
                                                     :round (fn ^:async calls-kid [c]
                                                              (await (ctx/call-child c :kid calls-leaf {}))
                                                              :continue)}})
              a (core/submit! eng '(calls-leaf) {})]
          (swap! world assoc :leaf-ok true)
          (await (core/tick! eng))
          (swap! world assoc :leaf-ok true :old-result {:status :stopped :reason :no-way})
          (let [b (core/submit! eng '(calls-leaf) {})]
            (await (core/tick! eng))
            (is (= [[(str a "/leaf") :ended] [(str b "/leaf") :ended]] (cut-offs seen))))
          (swap! world dissoc :old-result)
          (let [k (core/submit! eng '(calls-kid) {})]
            (await (core/tick! eng))
            (is (= [(str k "/kid/leaf") :ended] (last (cut-offs seen))) "so does an old child called with call-child")))))))

(defn ^:async next-turn [] (await (js/Promise. (fn [r] (js/setTimeout r 0)))))

(def reflex-jobs
  {'abandons (get jobs 'abandons)
   'holds {:conditions [] :round (fn ^:async holds [c]
                                   (await (ctx/call c :leaf leaf {}))
                                   (await (js/Promise. (fn [r] (swap! world assoc :release r))))
                                   (ctx/end c :done nil))}
   'old-done {:check (constantly true) :round (fn ^:async old-done [c] (await (ctx/call c :leaf leaf {})) :done)}})

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
            {:old {:status :stopped :reason :timeout :text "slow"}} {:kind :limit-reached :cause {:line :time :reason :timeout} :data {:text "slow"}}
            {:old {:status :stopped :reason :no-way}} {:kind :nothing-possible :cause :no-way}
            {:old {:status :stopped :reason :no-tool :need "stone_pickaxe"}}
            {:kind :condition-failed :cause :no-tool :data {:need "stone_pickaxe"}}
            {:old {:status :done :got 3}} {:kind :done :cause nil :data {:got 3}}
            {:old :done} {:kind :done :cause nil})
          (is (map? (:report (:old-ending @world))))
          (is (= {:status :done :ending {:kind :done :cause nil}}
                 (select-keys (:data (last (job-ev seen :child_ended))) [:status :ending]))))))))

(deftest an-old-stopped-reason-reads-as-the-doc-kind
  (are [reason kind cause]
       (= {:kind kind :cause cause} (legacy/ending-of :done {:status :stopped :reason reason} nil))
    :no-tool :condition-failed :no-tool
    :hungry :condition-failed :hungry
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
    :timeout :limit-reached {:line :time :reason :timeout}
    :low-health :limit-reached {:line :health :reason :low-health}
    :tool-spent :limit-reached {:line :wear :reason :tool-spent}
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
