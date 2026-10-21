(ns engine.night-b-test
  "The one night handler: the :night trigger and jobs.survival.night. A bed first (seen, remembered within the default-bed-radius setting
  under any roof, or carried, also in the open), then a sleeper on the server (the action bar's sleep count, or a
  sleeping player in sight): logged out in short stints until morning, then roofed or buried: the queue runs, else
  dig in."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.events :as events]
            [engine.hut-shelter-test :as hut]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.targets :as targets]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night :as night]
            [jobs.survival.night.sites :as sites]
            [engine.night-support :refer [night noon roof holds? write! status! closed-hut holds-after? sleeper offline-why on-return! sleep-count! log-out-child fired wide-ground hillside breached-room run-night-in home-roof lit-world know-home! night-at-home with-child-log walled-home walled-pen bed-world bed-places dug-into-home stone-room block-at room-with-doorway doorway-shut-early dawn-holds-now? dark-world flee-home-night stone-and-dirt-patch flee-with cave-blocks cave-night zombie]]))

(deftest a-queued-log-out-ends-done-without-acting-when-another-job-logged-out-meanwhile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night})]
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.log-out) {})
          (swap! clock + 1000)
          (mem/write! (:store eng) :log-out {:ms 30000 :status "ok"} nil)
          (await (tu/run-until-empty eng 3))
          (is (= [] (st/calls p "offline")) "the body was not logged out again")
          (is (= 0 (count (:list (core/state eng)))) "the job ended, it does not wait"))))))

(deftest a-log-out-before-the-submit-does-not-end-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (st/setup {:time night})]
          (sleep-count! p 1 2)
          (mem/write! (:store eng) :log-out {:ms 30000 :status "ok"} nil)
          (swap! clock + 1000)
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (tu/run-until-empty eng 3))
          (is (= 1 (count (st/calls p "offline")))))))))

(deftest a-log-out-stint-ends-at-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time 23000})]
          (sleep-count! p 1 2)
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (tu/run-until-empty eng 3))
          (is (= [25050] (st/ms-of-offline p))))))))

(deftest a-log-out-ends-done-with-how-it-went
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stint (await (log-out-child {:time night} {:offline-ms 20000 :news-ms 0}))]
          (is (= :done (:status stint)))
          (is (= "ok" (:how stint))))))))

(deftest a-log-out-needs-night-and-says-so
  (let [can-start? (:can-start? (get registry/jobs 'jobs.survival.log-out))
        status (fn [world args]
                 (let [[kind _ {:keys [reason]}] (can-start? {:primitives (tu/fake world) :args args :slots [:x]
                                                              :view (fn [] {:data mem/empty-data :now 0})})]
                   [kind reason]))]
    (is (= [:later :not-night] (status {:time noon} {})))
    (is (= [:never :offline-forbidden] (status {:time night} {:offline-allowed false})))
    (is (= :ok (first (status {:time night} {}))))))

(deftest a-log-out-quotes-the-stint-in-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [job (get registry/jobs 'jobs.survival.log-out)
              q (await ((:quote-of job) {:primitives (tu/fake {:time night}) :args {:offline-ms 20000 :news-ms 3000}}))]
          (is (nil? (contract/quote-problem q)))
          (is (<= 3 (second (get-in q [:uses :job-time])))))))))

(deftest the-night-ends-once-the-body-is-dug-in-and-the-queue-runs
  (async done
    (tu/run-async done
      (st/walls-cheap (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/wall-stack :blocks st/floor})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (st/tick-n eng 4))
          (is (= {:holder false :notified 1 :places 18}
                 {:holder (some? (core/holder eng)) :notified (st/notified seen) :places (count (st/calls p "place"))})
              "walled in and safe: no hold, the queued job ran at night")
          (is (= {:x 0.5 :y 64 :z 0.5} (st/pos-of p)) "still in the shelter")
          (is (= 1 (fired seen)) "not fired again while safe")))))))

(deftest at-night-a-respawned-body-recovers-its-drops-before-it-shelters
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})
              fired-first (fn [] (some #(when (= :fired (:kind %)) (:reflex %)) @seen))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :died} {:trigger :night}]}"))
          (write! eng :died {:pos {:x 40 :y 64 :z 0}})
          (swap! clock + 1000)
          (write! eng :respawned {})
          (swap! clock + 5000) ; past recover-drops' settle wait: this clock only moves between ticks
          (await (core/tick! eng))
          (is (= :died (fired-first)) "the drops are fetched first: the pile despawns, the night waits"))))))

(deftest a-site-whose-roof-fails-is-left-for-another-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks wide-ground})]
          (st/refuse-cell! p [0 63 0])
          (st/dawn-after! p 3)
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (let [roofs (filterv #(= 63 (:y %)) (mapv st/arg-pos (st/calls p "place")))]
            (is (= {:x 0 :y 63 :z 0} (first roofs)) "the first pit's roof was refused")
            (is (<= 9 (js/Math.hypot (:x (last roofs)) (:z (last roofs))))
                "a second pit 9+ blocks away was roofed (8.06 away, dig-in's futile check from the body declines it)"))
          (is (= 1 (count (st/emitted seen :shut-in.relocated))) "moved once, after the failed site")
          (is (= [] (st/entries eng :shut-in-failed)) "failed sites are forgotten at morning: the next night digs there again")
          (is (empty? (st/emitted seen :shelter.exposed))))))))

(deftest a-relocation-site-is-not-by-a-known-zombie
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks wide-ground :self {:food 20 :health 4}
                                         :entities [(assoc zombie :pos {:x -7.5 :y 64 :z 0.5})]})]
          (st/refuse-cell! p [0 63 0])
          (st/dawn-after! p 3)
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (let [roofs (filterv #(= 63 (:y %)) (mapv st/arg-pos (st/calls p "place")))]
            (is (> (count roofs) 1) "a second pit was roofed")
            (is (pos? (:x (last roofs))) "on the side away from the zombie, though the near side is as close")))))))

(deftest a-listed-dig-pit-waits-while-go-to-says-later-on-its-walk-to-a-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time night :blocks wide-ground :inventory st/dirt-stack
                                            :entities [(assoc zombie :pos {:x -4.5 :y 64 :z 0.5})]})]
          (mem/write! (:store eng) :shut-in-failed {:pos {:x 0 :y 64 :z 0} :way :pit :reason :no-floor} {:cap 5 :ttl 600000})
          (core/submit! eng '(jobs.survival.dig-pit {:for :night}) {})
          (await (tu/run-until-empty eng 5))
          (is (= [:hostile-near] (distinct (map :reason (filter #(= :waiting (:kind %)) @seen)))))
          (is (empty? (filter #(= :stopped (:kind %)) @seen)) "it waits, it does not end walk-failed"))))))

(deftest a-night-dig-pit-with-no-ground-for-a-pit-in-sight-waits-no-site-instead-of-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks (into {} (for [x (range -1 2) z (range -1 2)] [(str x ",63," z) "dirt"]))
                                              :inventory st/dirt-stack})]
          (mem/write! (:store eng) :shut-in-failed {:pos {:x 0 :y 64 :z 0} :way :pit :reason :no-floor} {:cap 5 :ttl 600000})
          (core/submit! eng '(jobs.survival.dig-pit {:for :night}) {})
          (await (tu/run-until-empty eng 5))
          (is (= [:no-site] (distinct (map :reason (filter #(= :waiting (:kind %)) @seen)))))
          (is (empty? (filter #(= :stopped (:kind %)) @seen)) "it waits at the start, it does not run a round to fail")
          (is (empty? (st/calls p "place"))))))))

(deftest a-failed-site-is-not-dug-again-after-a-cut-however-long-the-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (st/setup {:time night :inventory [{:name "iron_pickaxe" :count 1}]
                                                       :blocks (into {} (for [x (range -4 5) z (range -4 5) y (range 54 64)] [(str x "," y "," z) "cobblestone"]))})]
          (st/refuse-placing! p)
          ;; the 2nd wait (an exposed hold) is cut by a higher reflex 11 minutes on: the night was lengthened
          ;; with time set, so dig-in's own 10-minute futile entry has expired
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (st/on-wait! p (fn [n]
                        (case n
                          2 (do (swap! clock + 660000) (core/cut! eng (core/holder eng) :test nil))
                          8 (.setTime (.-world p) noon)
                          nil)))
          (await (st/tick-n eng 40))
          (swap! clock + 11000)
          (await (st/tick-n eng 40))
          (is (= [63 62 61] (keep #(when (= [0 0] [(:x %) (:z %)]) (:y %)) (mapv st/arg-pos (st/calls p "dig"))))
              "one pit; the second firing does not dig deeper (the other digs are the stair out by day)")
          (is (= 2 (fired seen)) "fired again after the cut"))))))

(deftest with-no-site-left-the-night-logs-out-and-ends-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks st/ground :inventory [{:name "iron_pickaxe" :count 1}]})
              ]
          (st/refuse-placing! p)
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (seq (filterv #(and (= :child_started (:kind %)) (= :log-out (:slot %))) @seen)) "a log-out stint")
          (is (= 1 (count (st/emitted seen :shelter.exposed))))
          (let [[s & more] (st/emitted seen :stopped)]
            (is (empty? more))
            (is (= :exposed (:reason s)) "a night with no shelter is no success")
            (is (= [:roof-failed] (take 1 (mapv :reason (:sites s)))) "the first site's reason is dig-in's")
          (is (= (inc (sites/max-sites)) (count (:sites s))) "max-sites sites tried, then exposed"))
          (is (= [] (:list (core/state eng)))))))))

(deftest exposed-why-names-the-site-reasons
  (is (re-find #"2 walls-failed" (sites/exposed-why [{:reason :walls-failed} {:reason :walls-failed} {:reason :unreachable}])))
  (is (re-find #"1 unreachable" (sites/exposed-why [{:reason :walls-failed} {:reason :unreachable}])))
  (is (not (re-find #"spawn protection" (sites/exposed-why [{:reason :walls-failed} {:reason :walls-failed}]))))
  (is (not (re-find #"spawn protection" (sites/exposed-why [{:reason :walls-server-refused} {:reason :unreachable}]))))
  (is (re-find #"spawn protection.*move" (sites/exposed-why [{:reason :walls-server-refused} {:reason :walls-server-refused}])))
  (is (= "no site tried" (sites/exposed-why [])))
  (is (= "no block to wall" (sites/exposed-why [] "no block to wall"))))

(deftest a-body-held-exposed-says-why-when-the-server-refuses-every-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks (merge st/floor (tu/box -4 60 -4 4 62 4 "stone")) :inventory st/dirt-stack :floor [-4 -4 4 4]})
              ]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "failed" :reason "Server refused the placement"}))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (re-find #"spawn protection" (:text (first (st/emitted seen :shelter.exposed)))) "the notice too")
          (is (re-find #"spawn protection" (:text (first (st/emitted seen :stopped))))))))))

(deftest dig-in-is-one-call-and-stops-with-the-site-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory [{:name "dirt" :count 20}] :blocks st/floor})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
          (await (core/tick! eng))
          (is (= 18 (count (st/calls p "place"))) "walled and roofed in one call")
          (is (= [] (:list (core/state eng)))))
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks (merge st/ground st/floor)})]
          (st/refuse-placing! p)
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (core/tick! eng))
          (is (= 3 (count (st/calls p "dig"))))
          (is (= [:no-way] (mapv :reason (st/emitted seen :stopped))))
          (is (= [] (:list (core/state eng)))))))))

;; ------------------------------------------------------------------ review follow-ups
(deftest a-roofed-end-forgets-tonights-failed-sites
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks st/floor})
              call ctx/call]
          ;; the dig-in leaves the body roofed some other way (a passer-by walled and roofed it), the night then ends :roofed
          (set! ctx/call (fn ^:async f [c k sym a]
                           (doseq [cell [[0 66 0] [1 64 0] [-1 64 0] [0 64 1] [0 64 -1] [1 65 0] [-1 65 0] [0 65 1] [0 65 -1]]]
                             (swap! (.-state (.-world p)) assoc-in [:blocks cell] "stone"))
                           (contract/ending :condition-failed :declined nil)))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (set! ctx/call call)
          (is (= [] (st/entries eng :night-site)) "no entry outlives the night that wrote it"))))))

(deftest a-dig-in-that-yields-is-not-a-failed-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks st/floor})
              calls (atom 0)
              seen-sites (atom nil)
              call ctx/call]
          (set! ctx/call (fn ^:async f [c k sym a]
                           (let [n (swap! calls inc)]
                             (when (= 2 n)
                               (reset! seen-sites (st/entries eng :shut-in-failed))
                               (.setTime (.-world p) st/noon))
                             (if (= :shut-in k) :continue (await (call c k sym a))))))
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (set! ctx/call call)
          (is (= [] @seen-sites) "the yield left no failed site, and the next call resumed the dig-in"))))))

(deftest a-pit-site-is-judged-by-the-surface-a-player-sees
  (let [p (tu/fake {:blocks (into {} (for [x (range -2 3) z (range -2 3)] [(str x ",63," z) "dirt"]))})]
    (is (pit-site/pit-site? p {:x 1 :y 64 :z 1}) "one layer of dirt shows nothing against a pit; dig-in finds out")
    (is (not (pit-site/pit-site? p {:x 9 :y 64 :z 9})) "no ground to stand on")))

(deftest a-pit-site-needs-ground-the-carried-tools-dig
  (let [stone (into {} (for [x (range -2 3) z (range -2 3)] [(str x ",63," z) "stone"]))
        site {:x 1 :y 64 :z 1}]
    (is (not (pit-site/pit-site? (tu/fake {:blocks stone}) site)) "no pickaxe: stone cannot be dug")
    (is (pit-site/pit-site? (tu/fake {:blocks stone :inventory [{:name "iron_pickaxe" :count 1}]}) site))))

(deftest with-no-pit-site-left-the-night-digs-a-niche-into-the-hill
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks hillside :floor [-2 -2 8 2] :inventory [{:name "iron_pickaxe" :count 1}]})
              mid (atom nil)]
          (st/refuse-cell! p [0 63 0])
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (reset! mid {:at (st/pos-of p) :holder (some? (core/holder eng))})
          (is (= {:at {:x 5.5 :y 64 :z -0.5} :holder false} @mid) "shut in the far pair of the 2-deep pocket (the stair out of the failed pit went north), safe: not held")
          (is (= #{{:x 4 :y 64 :z -1} {:x 4 :y 65 :z -1}}
                 (set (filterv #(= 4 (:x %)) (mapv st/arg-pos (st/calls p "place")))))
              "the near pair, feet and head, plugged: rock all round")
          (is (empty? (st/emitted seen :shelter.exposed)))
          (is (empty? (st/emitted seen :stopped)) "the night ended done"))))))

(deftest the-nights-result-sums-what-its-niche-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks hillside :floor [-2 -2 8 2] :inventory [{:name "iron_pickaxe" :count 1}]})]
          (st/refuse-cell! p [0 63 0])
          (let [r (await (tu/child-outcome eng 'jobs.survival.night {} 40))]
            (is (= :dug-in (:night r)) (pr-str r))
            (is (= 2 (reduce + (vals (:spent r)))) (pr-str r))
            (is (re-find #"^used .+ \(night shelter at" (:text r)) (pr-str r))))))))
