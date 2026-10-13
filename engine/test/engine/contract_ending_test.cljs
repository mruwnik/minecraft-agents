(ns engine.contract-ending-test
  "Contract jobs end with an ending (engine.contract): call keys frozen at call start, conditions, done?, :because,
  and how a root job's ending is told."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
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

(defn setup [jobs]
  (reset! world {})
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
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
