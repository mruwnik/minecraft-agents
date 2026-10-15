(ns engine.movement-contract-test
  "movement.go-to, follow, linger-near, mount, leave-vehicle and boat-drive as contract jobs: they export the contract,
  quote validly, and end with a kind and a cause."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.library-test :as lt]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]
            [jobs.movement.boat-drive :as boat-drive]
            [jobs.movement.follow :as follow]
            [jobs.movement.go-to :as go-to]))

(defn job [sym] (get registry/jobs sym))

(deftest each-job-exports-the-contract-and-quotes-validly
  (doseq [[sym args] [['jobs.movement.follow {:player "Steve"}]
                      ['jobs.movement.linger-near {:pos [0 64 0] :wait-s 5}]
                      ['jobs.movement.mount {:id 9}]
                      ['jobs.movement.leave-vehicle {}]
                      ['jobs.movement.boat-drive {:pos [10 63 2]}]
                      ['jobs.movement.boat-launch {}]
                      ['jobs.movement.boat-launch {:action :recover}]
                      ['jobs.movement.boat-land {:recover true}]
                      ['jobs.movement.cross-water {:pos [7 64 0]}]]]
    (is (some? (:conditions (job sym))) (str sym " has conditions"))
    (is (nil? (:check (job sym))) (str sym " has no old check"))
    (is (some? (:done? (job sym))) (str sym " has done?"))
    (is (nil? (contract/quote-problem ((:quote-of (job sym)) {:primitives (tu/fake {}) :args args :quote-call {:spend {:time 60}}}))) (str sym))))

(deftest every-movement-access-block-job-has-a-time-line-default
  (let [syms (->> (keys registry/jobs)
                  (filter #(re-find #"^jobs\.(movement|access|blocks|primitive)\." (str %)))
                  (filter #(:round (job %))))]
    (is (seq syms))
    (doseq [sym syms]
      (is (some? (get-in (job sym) [:defaults :spend :time])) (str sym " defaults :spend :time")))))

(deftest go-to-s-time-line-default-is-a-setting
  (let [{:keys [defaults] :as def} (job 'jobs.movement.go-to)
        resolve #(contract/resolve-settings defaults (fn [k] (settings/get (:settings def) k)))]
    (is (= 1800 (get-in (resolve) [:spend :time])))
    (is (= 60 (settings/with-settings {:jobs.movement.go-to/time-s 60} #(get-in (resolve) [:spend :time]))))))

(deftest targets-and-moves-to
  (is (= [:player] (:target (job 'jobs.movement.follow))))
  (is (= [:pos] (:target (job 'jobs.movement.linger-near))))
  (is (= [:pos] (:target (job 'jobs.movement.boat-drive))))
  (is (= [:action :pos :id] (:target (job 'jobs.movement.boat-launch))))
  (is (= [:pos] (:target (job 'jobs.movement.boat-land))))
  (is (= [10.5 63 2.5] (:moves-to ((:quote-of (job 'jobs.movement.boat-drive))
                                   {:primitives (tu/fake {}) :args {:pos [10 63 2]} :quote-call {}})))))

(deftest old-time-limits-become-the-spend
  (is (= {:player "S" :spend {:time 9}} (follow/legacy-args {:player "S" :timeout-s 9})))
  (is (= {:pos [1 63 1] :spend {:time 9}} (boat-drive/legacy-args {:pos [1 63 1] :max-s 9}))))

(defn ^:async ending-of
  "Submit spec over a fake world and tick until the job ends: {:event :completed|:stopped :kind :reason}, plus the
  event's data keys ks."
  ([world spec] (ending-of world spec []))
  ([world spec ks]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        now (tu/act-clock clock p 20)
        eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir) :now now
                          :world (world/of-data {} {} [])
                          :events (events/make {:sinks [sink] :now now})})]
    (core/submit! eng spec {})
    (loop [i 0]
      (when (and (< i 80) (not-any? #(#{:completed :stopped} (:kind %)) @seen))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    (let [e (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
      (core/shutdown! eng)
      (assoc (select-keys (:data e) (into [:kind :reason] ks)) :event (:kind e))))))

(def steve {:id 5 :name "Steve" :kind "player" :pos {:x 1 :y 64 :z 0}})
(def boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5}})

(deftest follow-ends-on-its-time-and-when-the-player-never-came
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :stopped :kind :limit-reached :reason :timeout}
               (await (ending-of {:entities [steve]} '(jobs.movement.follow {:player "Steve" :timeout-s 2})))))
        (is (= {:event :stopped :kind :condition-failed :reason :absent}
               (await (ending-of {} '(jobs.movement.follow {:player "Steve"}))))))))) 

(deftest linger-near-waits-its-time-then-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {} '(jobs.movement.linger-near {:pos {:x 0 :y 64 :z 0} :wait-s 1}))) [:event])))))))

(deftest mount-is-done-or-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {:entities [boat]} '(jobs.movement.mount {:id 9}))) [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :not-mountable}
               (await (ending-of {:entities [(assoc boat :name "cow")]} '(jobs.movement.mount {:id 9})))))
        (is (= {:event :stopped :kind :nothing-possible :reason :timeout}
               (await (ending-of {:entities [boat] :mountFails true} '(jobs.movement.mount {:id 9})))))))))

(deftest leave-vehicle-ends-done-or-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {:self {:vehicle 9} :entities [boat]} '(jobs.movement.leave-vehicle {}))) [:event])))
        (is (= {:event :stopped :kind :nothing-possible :reason :dismount-failed}
               (await (ending-of {:self {:vehicle 9} :entities [(assoc boat :dismountFails true)] :dismountFails true}
                                 '(jobs.movement.leave-vehicle {:max-tries 2 :wait-ms 1})))))))))

(def lake (into {} (for [x (range 0 21) z (range 0 21)] [(str x ",63," z) "water"])))

(deftest boat-drive-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world args kind reason]
                [["not a boat" {:blocks lake :entities [(assoc boat :name "minecart" :pos {:x 2.5 :y 63.5 :z 2.5})] :self {:vehicle 9}} {:pos [10 63 2]} :condition-failed :not-a-boat]
                 ["not water" {:blocks lake :entities [(assoc boat :pos {:x 2.5 :y 63.5 :z 2.5})] :self {:vehicle 9}} {:pos [10 64 2]} :condition-failed :not-water]
                 ["out of strokes" {:blocks lake :entities [(assoc boat :pos {:x 2.5 :y 63.5 :z 2.5})] :self {:vehicle 9}} {:pos [18 63 18] :max-strokes 2} :nothing-possible :strokes]]]
          (is (= {:event :stopped :kind kind :reason reason}
                 (await (ending-of world (list 'jobs.movement.boat-drive args)))) label))))))

(deftest go-to-exports-the-contract
  (let [j (job 'jobs.movement.go-to)]
    (is (some? (:conditions j)))
    (is (nil? (:check j)) "no old check")
    (is (= [:pos :place] (:target j)) "a go-to may call go-to to another place")
    (is (= [:walk :clear-seal :clear-path :pillar :stair :dig-out] (:ways j)))))

(deftest go-to-old-args-become-the-call-keys
  (are [old call] (= (merge {:pos [1 64 1]} call) (go-to/legacy-args (merge {:pos [1 64 1]} old)))
    {:escalate false} {:ways #{:walk :clear-seal}}
    {:escalate true} {}
    {:clear-seal false} {:ways #{:walk :clear-path :pillar :stair :dig-out}}
    {:escalate false :clear-seal false} {:ways #{:walk}}
    {:clear-seal true} {}
    {:escalate false :ways #{:walk :stair}} {:ways #{:walk :stair}}
    {:min-health 12} {:floor {:health 12}}
    {:min-health 1} {:floor {:health 1}}
    {:allow-over-budget true} {:floor {:health 1}}
    {:allow-over-budget false} {}
    {:allow-over-budget true :min-health 9} {:floor {:health 9}}
    {:max-damage 3} {:spend {:health 3}}
    {:leg-s 5} {:spend {:time 5}}
    {:leg-s 5 :max-damage 0} {:spend {:time 5 :health 0}}
    {:leg-s 5 :spend {:time 9}} {:spend {:time 9}}
    {:min-health 12 :floor {:health 4}} {:floor {:health 4}}))

(deftest go-to-reads-its-walk-limits-from-the-call
  (are [given frozen limits] (= limits (go-to/walk-limits given frozen 12))
    {} {} {}
    {:floor {:health 14}} {:floor {:health 14}} {:min-health 14}
    {:floor {:health 1}} {:floor {:health 1}} {:allow-over-budget true}
    {:floor {:health 0}} {:floor {:health 0}} {:allow-over-budget true}
    {:floor {:health 1}} {:floor {:health 6}} {:min-health 6}
    {} {:floor {:health 6}} {}
    {} {:floor {:health 15}} {:min-health 15}
    {:spend {:health 2 :time 4}} {:spend {:health 1 :time 0}} {:max-damage 2 :leg-s 4}))

(deftest go-to-endings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:event :completed}
               (select-keys (await (ending-of {} '(jobs.movement.go-to {:pos [5 64 0]}))) [:event])))
        (is (= {:event :stopped :kind :condition-failed :reason :unknown-place}
               (await (ending-of {} '(jobs.movement.go-to {:place :nowhere})))))
        (is (= {:event :stopped :kind :nothing-possible :reason :no-way}
               (await (ending-of {} '(jobs.movement.go-to {:pos [5 64 0] :ways #{:stair}}))))
            "a call that may not walk")
        (let [e (await (ending-of {:unreachable ["5,64,0"]} '(jobs.movement.go-to {:pos [5 64 0] :range 0}) [:cause]))]
          (is (= {:event :stopped :kind :nothing-possible :reason :unreachable} (dissoc e :cause)))
          (is (= :unreachable (:reason (:cause e))) (pr-str e))
          (is (keyword? (:why (:cause e))) "the planner's reason"))))))

(defn ^:async waits?
  "Whether job sym with args, submitted as a root job in world, is still listed (waiting on a condition) after 12 ticks."
  [world sym args]
  (let [eng (core/create {:primitives (tu/seeing-all (tu/fake-on-floor world)) :jobs registry/jobs :dir (tu/tmp-dir)})]
    (core/submit! eng (list sym args) {})
    (await (tu/run-until-empty eng 12))
    (boolean (seq (:list (core/state eng))))))

(deftest boat-drive-on-foot-and-mount-aboard-other-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (await (waits? {:entities [boat]} 'jobs.movement.boat-drive {:pos [10 63 2]}))) "on foot")
        (is (true? (await (waits? {:self {:vehicle 5} :entities [boat (assoc boat :id 5 :name "minecart")]}
                                  'jobs.movement.mount {:id 9}))) "aboard another vehicle")))))

(deftest look-around-waits-in-a-noop-pause-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pauses (atom [])
              stub {:conditions [] :target []
                    :round (fn ^:async stub-round [c] (swap! pauses conj (:args c)) (ctx/end c :done nil {}))}
              {:keys [eng p]} (lt/setup {})
              res (await (tu/child-outcome (assoc-in eng [:jobs 'jobs.time.noop] stub) 'jobs.movement.look-around {:every-ms 1500} 20))]
          (is (= :done (:status res)) (pr-str res))
          (is (= [{:seconds 1.5 :why :looking}] @pauses))
          (is (empty? (filter #(= "wait" (.-name %)) (.-calls (.-world p)))) "no bare wait act"))))))
