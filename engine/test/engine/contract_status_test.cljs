(ns engine.contract-status-test
  "Statuses [kind text data] (docs/contract-conditions-design.md 1.1, 2.1, 2.3): constructors, verdict, the reader of
  old condition returns, and the can-start?/can-run? exports as the engine asks them."
  (:require [cljs.test :refer [deftest is async]]
            [engine.composite :as composite]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.pace :as pace]))

(deftest constructors-give-kind-text-and-data-with-a-reason
  (is (= [:ok "aboard" {:reason :mounted}] (contract/ok :mounted "aboard")))
  (is (= [:later "torches 0 < 4" {:reason :no-torches :have 0 :want 4}]
         (contract/later :no-torches "torches 0 < 4" :have 0 :want 4)))
  (is (= [:never "diamond does not smelt" {:reason :not-smeltable :item "diamond"}]
         (contract/never :not-smeltable "diamond does not smelt" :item "diamond")))
  (is (= [:look "no tree seen" {:reason :no-tree}] (contract/look :no-tree "no tree seen"))))

(def ok (constantly (contract/ok :fine "fine")))
(def later (constantly (contract/later :not-yet "not yet")))
(def never (constantly (contract/never :bad-args "count 0")))
(def peek (constantly (contract/look :unseen "not seen")))

(deftest verdict-is-any-never-else-the-first-later-else-the-first-look-else-ok
  (let [kind #(first (contract/verdict {} %))]
    (is (= :ok (kind {:can-start? ok :can-run? ok})))
    (is (= :ok (kind {})) "no export counts as :ok")
    (is (= :never (kind {:can-start? later :can-run? never})))
    (is (= :later (kind {:can-start? peek :can-run? later})))
    (is (= :look (kind {:can-start? ok :can-run? peek})))
    (is (= "not yet" (second (contract/verdict {} {:can-start? later}))))))

(deftest the-reader-maps-old-condition-returns-onto-statuses
  (let [kind #(first (contract/verdict {} {:conditions [[:fn (constantly %)]]}))]
    (is (= :ok (kind nil)))
    (is (= :later (kind :no-tree)))
    (is (= :later (kind {:reason :no-tree :near 3})))
    (is (= :never (kind (contract/impossible :bad-args))))
    (is (= :look (kind (contract/look-first :unseen))))
    (is (= :ok (kind (contract/ok :x "x"))) "a status passes as it is")))

(defn odd-check [_] :not-a-status)

(deftest an-export-that-returns-no-status-is-an-error-naming-it
  (is (thrown-with-msg? js/Error #"odd-check" (contract/verdict {} {:can-run? odd-check}))))

;; ---------------------------------------------------------------- the engine

(def world (atom {}))

(defn setup [jobs]
  (reset! world {})
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        eng (core/create {:primitives (tu/fake {}) :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :clock clock}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))

(defn gate
  "A check reading world key k: true :ok, :never :never, else :later."
  [k]
  (fn [_]
    (case (get @world k)
      true (contract/ok k "go")
      :never (contract/never :gone "gone for good" :item "x")
      (contract/later :paused "torches 0 < 4" :have 0))))

(def gated
  {:can-start? (gate :start) :can-run? (gate :run)
   :defaults {:spend {:elapsed 60}}
   :round (fn [_] (swap! world update :rounds (fnil inc 0)) :continue)})

(deftest a-listed-job-waits-on-can-start-with-its-text-and-stops-asking-it-once-started
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'gated gated})
              id (core/submit! eng '(gated) {})]
          (swap! world assoc :run true)
          (await (core/tick! eng))
          (is (nil? (:rounds @world)))
          (is (= ["waiting: torches 0 < 4"] (mapv :message (job-ev seen :waiting))))
          (swap! world assoc :start true)
          (await (core/tick! eng))
          (is (= 1 (:rounds @world)))
          (swap! world assoc :start false)
          (await (core/tick! eng))
          (is (= 2 (:rounds @world)) "can-start? is not asked once started")
          (is (= [id] (:list (core/state eng)))))))))

(deftest can-start-never-ends-impossible-with-the-status-as-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'gated gated})]
          (swap! world assoc :start :never :run true)
          (core/submit! eng '(gated) {})
          (await (core/tick! eng))
          (is (= {:kind :impossible :cause {:reason :gone :item "x" :text "gone for good"} :reason :gone}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause :reason])))
          (is (= "gone for good" (:message (first (job-ev seen :stopped))))))))))

(deftest can-run-later-yields-a-started-job-until-its-elapsed-budget-ends-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {'gated gated})
              id (core/submit! eng '(gated) {})]
          (swap! world assoc :start true :run true)
          (await (core/tick! eng))
          (swap! world assoc :run false)
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= 1 (:rounds @world)) "no round while can-run? says :later")
          (is (= [id] (:list (core/state eng))) "it waits, still listed")
          (is (= {:reason :paused :have 0 :text "torches 0 < 4"} (select-keys (:data (last (job-ev seen :waiting))) [:reason :have :text])))
          (swap! world assoc :run true)
          (await (core/tick! eng))
          (is (= 2 (:rounds @world)) "runs again once can-run? holds")
          (swap! world assoc :run false)
          (swap! clock + 61000)
          (await (core/tick! eng))
          (is (= {:kind :limit-reached :reason :elapsed}
                 (select-keys (:data (first (job-ev seen :stopped))) [:kind :reason]))))))))

(deftest can-run-never-ends-a-started-job-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'gated gated})]
          (swap! world assoc :start true :run true)
          (core/submit! eng '(gated) {})
          (await (core/tick! eng))
          (swap! world assoc :run :never)
          (await (core/tick! eng))
          (is (= 1 (:rounds @world)))
          (is (= :impossible (:kind (:data (first (job-ev seen :stopped)))))))))))

(def stepper
  "Steps until :steps-left runs out; can-run? reads :run."
  {:can-run? (gate :run)
   :round (fn ^:async stepper [c]
            (await (pace/steps! c (fn []
                                    (swap! world update :steps (fnil inc 0))
                                    (when (= 1 (:steps @world)) (swap! world assoc :run (:then @world)))
                                    :again))))})

(deftest can-run-is-asked-between-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {'stepper stepper})
              id (core/submit! eng '(stepper) {})]
          (swap! world assoc :run true :then false)
          (await (core/tick! eng))
          (is (= 1 (:steps @world)) "a :later after the first step yields the round")
          (is (= [id] (:list (core/state eng))))
          (swap! world assoc :run true :then :never :steps 0)
          (await (core/tick! eng))
          (is (= 1 (:steps @world)))
          (is (= {:kind :impossible :reason :gone} (select-keys (:data (first (job-ev seen :stopped))) [:kind :reason]))))))))

(deftest a-called-child-whose-can-start-says-later-ends-condition-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {'parent {:can-run? ok
                                             :round (fn ^:async parent [c]
                                                      (swap! world assoc :child (await (ctx/call c :kid gated {})))
                                                      (ctx/end c :done nil))}})]
          (swap! world assoc :run true)
          (core/submit! eng '(parent) {})
          (await (core/tick! eng))
          (is (= {:kind :condition-failed :cause {:reason :paused :have 0 :text "torches 0 < 4"}}
                 (select-keys (:child @world) [:kind :cause]))))))))

(deftest a-combinator-passes-its-childs-status-on-with-its-slot
  (let [c {:view (constantly {:data mem/empty-data :now 0}) :slots []}
        kid [{:can-start? (gate :start) :round #(ctx/end % :done nil)} {}]]
    (reset! world {})
    (with-redefs [ctx/mem (constantly {})]
      (is (= [:later "torches 0 < 4" {:reason :paused :have 0 :slot :c0}]
             (ctx/check-status c (composite/seq-def [kid]))))
      (is (= :c0 (:slot (nth (ctx/check-status c (composite/repeat-def kid nil)) 2)))))))
