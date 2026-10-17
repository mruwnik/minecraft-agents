(ns engine.night-d-test
  "The one night handler: the :night trigger and jobs.survival.night. A bed first (seen, remembered within :bed-radius
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
            
            [jobs.lib.targets :as targets]
            [jobs.survival.night :as night]
            [jobs.survival.night.sites :as sites]
            [engine.night-support :refer [night noon roof holds? write! status! closed-hut holds-after? sleeper offline-why on-return! sleep-count! log-out-child fired wide-ground hillside breached-room run-night-in home-roof lit-world know-home! night-at-home with-child-log walled-home walled-pen bed-world bed-places dug-into-home stone-room block-at room-with-doorway doorway-shut-early night-holds-now? dark-world flee-home-night stone-and-dirt-patch flee-with cave-blocks cave-night held-with-raw-beef furnace-ops held-starving cow zombie]]))

(deftest with-flee-radius-0-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {:flee-radius 0}))]
          (st/dawn-after! p 4)
          (await (tu/run-until-empty eng 80))
          (is (empty? (st/emitted seen :shelter.fled)))
          (is (= 1 (count (st/emitted seen :shelter.exposed)))))))))

(deftest a-flee-that-cannot-arrive-is-reported-and-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge walled-home) {}))]
          (st/dawn-after! p 6)
          (let [log (await (with-child-log #(tu/run-until-empty eng 80)))]
            (is (= 1 (count (filter #(= :flee (first %)) log))) "one walk, never the same target twice")
            (is (= {:place :home :range 0} (nth (first (filter #(= :flee (first %)) log)) 2))))
          (is (= 1 (count (st/emitted seen :shelter.flee_failed))))
          (is (= 1 (count (st/emitted seen :shelter.exposed))))
          (is (= [[:exposed 1]] (mapv (juxt :reason :fled) (st/emitted seen :stopped)))))))))

(deftest a-flee-cut-mid-walk-resumes-the-same-target-from-where-the-body-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))
              n (atom 0)
              log (await (with-child-log #(tu/run-until-empty eng 80)
                           (fn [_ k _ _] (when (and (= :flee k) (= 1 (swap! n inc)))
                                           (st/teleport! p 4 64 0)
                                           :continue))))
              flees (filter #(= :flee (first %)) log)]
          (is (<= 2 (count flees)) "walked again after the cut")
          (is (apply = (map #(nth % 2) flees)) "the same target"))))))

(deftest with-nothing-to-dig-here-the-night-flees-to-seen-ground-the-carried-tools-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})]
          (tu/seeing-all p)
          (.override (.-world p) "place"
                     (fn ^:async f [token a impl]
                       (if (< (.-x (.-pos a)) 20)
                         #js {:status "no-support"}
                         (await (impl token a)))))
          (st/dawn-after! p 12)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 120))
          (is (= [:ground] (mapv :target (st/emitted seen :shelter.fled))))
          (is (some #(<= 27 (:x (st/arg-pos %))) (st/calls p "place")) "the pit's roof is placed at the patch"))))))

(deftest the-flee-walks-to-the-target-go-to-costs-cheapest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[log seen calls] (await (flee-with (fn [_] {:status :found :index 0 :cost 7})))]
          (is (seq (filter #(= :flee (first %)) log)))
          (is (= [7] (mapv :cost (st/emitted seen :shelter.fled))) "the planner's cost is reported")
          (is (every? #(and (:x %) (:z %)) (first calls)) "the candidates are given as cells"))))))

(deftest a-flee-search-still-going-walks-nowhere-until-it-answers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              [log seen calls] (await (flee-with (fn [_] (if (< (swap! n inc) 4) {:status :searching} {:status :found :index 0 :cost 3}))))]
          (is (= 4 (count calls)) "asked again after each :searching")
          (is (= 1 (count (filter #(= :flee (first %)) log))) "walks once, after the answer")
          (is (= [3] (mapv :cost (st/emitted seen :shelter.fled)))))))))

(deftest with-no-reachable-target-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[log seen] (await (flee-with (fn [_] {:status :none :reason :exhausted :proved true})))]
          (is (empty? (filter #(= :flee (first %)) log)))
          (is (= 1 (count (st/emitted seen :shelter.exposed)))))))))

(deftest with-blocks-to-close-the-open-sides-the-night-flees-to-an-overhang-and-walls-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (cave-night [{:name "dirt" :count 8}])
              log (await (with-child-log #(tu/run-until-empty eng 160)))
              enclose (first (filter #(and (= 'jobs.survival.dig-in (second %)) (contains? (nth % 2) :enclose)) log))]
          (is (= [:cave] (mapv :target (st/emitted seen :shelter.fled))))
          (is (= true (:enclose (nth enclose 2))))
          (is (= 1 (count (st/emitted seen :dig-in.sealed))))
          (is (some? enclose) "walls it in, never a pit")
          (is (every? #(<= 21 (:x (st/arg-pos %))) (st/calls p "place")) "placed only at the cave"))))))

(deftest with-too-few-blocks-the-night-ignores-the-overhang
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (cave-night [{:name "dirt" :count 3}])]
          (await (tu/run-until-empty eng 160))
          (is (empty? (st/emitted seen :shelter.fled))))))))

(deftest logs-are-no-wall-blocks-for-an-overhang
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (cave-night [{:name "oak_log" :count 20}])
              log (await (with-child-log #(tu/run-until-empty eng 160)))]
          (is (empty? (st/emitted seen :shelter.fled)))
          (is (seq (filter #(and (= 'jobs.survival.dig-in (second %)) (not (:enclose (nth % 2)))) log))
              "dug in where it stood, not walled in at the cave"))))))

(deftest the-night-hands-its-wall-value-to-dig-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (cave-night [{:name "dirt" :count 8}] {:wall-value 5})
              log (await (with-child-log #(tu/run-until-empty eng 160)))
              dig-ins (filter #(= 'jobs.survival.dig-in (second %)) log)]
          (is (seq dig-ins))
          (is (every? #(= 5 (:wall-value (nth % 2))) dig-ins) (pr-str (map #(nth % 2) dig-ins))))))))

(deftest an-overhang-whose-roof-the-body-has-not-seen-is-no-flee-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (cave-night [{:name "dirt" :count 8}])
              all (.-seenBlocks p)]
          (aset p "seenBlocks" (fn [q] (.filter (all q) (fn [b] (< (.-y (.-pos b)) 66)))))
          (await (tu/run-until-empty eng 160))
          (is (empty? (st/emitted seen :shelter.fled))))))))

(deftest with-nothing-seen-the-night-looks-around-and-then-flees-to-the-ground-it-saw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})]
          (tu/seeing-after-look p)
          (.override (.-world p) "place"
                     (fn ^:async f [token a impl]
                       (if (< (.-x (.-pos a)) 20)
                         #js {:status "no-support"}
                         (await (impl token a)))))
          (st/dawn-after! p 12)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 160))
          (is (seq (st/calls p "look")))
          (is (= [:ground] (mapv :target (st/emitted seen :shelter.fled)))))))))

;; ------------------------------------------------------------------ a bed not yet looked at
(deftest a-bed-in-the-room-not-yet-looked-at-is-slept-in-not-replaced-by-the-carried-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup (assoc hut/night-bed-world :inventory [{:name "red_bed" :count 1}]))]
          (tu/seeing-after-look p)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (seq (st/calls p "look")) "looked around first")
          (is (= 1 (count (st/calls p "sleep"))) "slept in the seen bed")
          (is (empty? (st/calls p "place")) "no second bed")
          (is (empty? (st/ended-because seen :bed-place-failed)) "did not decide to put the carried bed down unlooked"))))))

(deftest monsters-near-a-free-bed-hold-the-sleep-as-monsters-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"1,64,0" "red_bed"} :monstersNear true})
              _ (core/submit! eng '(jobs.survival.sleep {:bed {:x 1 :y 64 :z 0}}) {})]
          (tu/seeing-after-look p)
          (await (st/tick-n eng 6))
          (is (empty? (st/emitted seen :bed_unusable)) "not a failed sleep")
          (is (= 1 (count (:list (core/state eng)))) "the call holds")
          (is (= [:monsters-near] (distinct (map :reason (st/emitted seen :holding))))))))))

(deftest a-hostile-near-holds-the-night-as-monsters-near-without-calling-bed-down-every-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (held-starving [zombie] {:food 20 :health 20} [{:name "red_bed" :count 1}])]
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (empty? (st/calls p "sleep")) "no sleep with a hostile near")
          (is (>= 12 (count (filterv #(and (= :child_started (:kind %)) (= :bed-down (:slot %))) @seen))) "one bed-down per hold, not per pass")
          (is (= [:monsters-near] (distinct (map :reason (st/emitted seen :holding))))))))))

(deftest an-occupied-bed-not-yet-looked-at-is-refused-before-the-sleep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})
              id (core/submit! eng '(jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}}) {})]
          (tu/seeing-after-look p)
          (await (st/tick-n eng 6))
          (is (empty? (st/calls p "sleep")) "no sleep on an occupied bed")
          (is (= 1 (count (:list (core/state eng)))) "the call holds for the bed")
          (is (= [:bed-occupied] (distinct (map :reason (st/emitted seen :holding))))))))))

(deftest a-room-not-yet-looked-at-is-looked-at-before-the-carried-bed-has-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}] :skipNight false})]
          (tu/seeing-after-look p)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (seq (st/calls p "place")) "put the bed down")
          (let [names (mapv #(.-name %) (.-calls (.-world p)))]
            (is (< (.indexOf names "look") (.indexOf names "place")) "the room was looked at before the bed went down")
            (is (not= -1 (.indexOf names "look"))))
          (is (empty? (st/ended-because seen :bed-place-failed))))))))

(deftest a-held-hungry-body-cooks-raw-beef-at-a-furnace-in-the-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (held-with-raw-beef "2,64,0")]
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (some #{"take"} (furnace-ops p)) "the cooked beef was taken")
          (is (> (.-food (.self p)) 5) "and eaten"))))))

(deftest a-held-hungry-body-eats-raw-beef-when-the-furnace-is-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (held-with-raw-beef "40,64,0")]
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (= [] (furnace-ops p)) "no walk out to a furnace at night")
          (is (> (.-food (.self p)) 5) "ate raw"))))))

(deftest a-held-starving-body-hunts-a-cow-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (held-starving [cow (assoc cow :id 8 :pos {:x 6 :y 64 :z 0}) (assoc cow :id 9 :pos {:x 7 :y 64 :z 0})])
              ticked (await (js/Promise.race #js [(.then (core/tick! eng) (constantly true))
                                                  (js/Promise. (fn [res _] (js/setTimeout #(res false) 20000)))]))]
          (is ticked "the tick ended (a body that waits on the night yields instead of spinning)")
          (is (seq (filterv #(= "attack" (.-name %)) (.-calls (.-world p)))) "it went for the cow instead of starving in the hold"))))))

(deftest a-held-hungry-body-leaves-the-bait-an-animal-job-reserved
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ate (fn ^:async ate* [reserve?]
                    (let [{:keys [eng p]} (held-starving [] {:food 5 :health 20} [{:name "carrot" :count 10}])]
                      (when reserve? (mem/write! (:store eng) :bait-reserve {:foods ["carrot"]} {:cap 5 :ttl :forever}))
                      (await (core/tick! eng))
                      (count (filterv #(= "eat" (.-name %)) (.-calls (.-world p))))))]
          (is (zero? (await (ate true))) "reserved bait is not eaten at food 5")
          (is (pos? (await (ate false))) "unreserved carrots are"))))))

(deftest a-held-starving-body-stays-put-with-a-hostile-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (held-starving [cow zombie])]
          (await (core/tick! eng))
          (is (empty? (filterv #(= "attack" (.-name %)) (.-calls (.-world p)))) "no hunt with a danger near"))))))

(deftest the-dig-in-at-an-open-bed-never-digs-a-bed-half
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :self {:pos {:x 1 :y 64 :z 0}}
                                         :blocks (merge st/floor {"1,64,0" "red_bed" "2,64,0" "red_bed"})})]
          (st/know-bed! eng {:x 1 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 60))
          (is (empty? (filter #(#{{:x 1 :y 64 :z 0} {:x 2 :y 64 :z 0}} %) (map st/arg-pos (st/calls p "dig")))) "no bed half dug")
          (is (= "red_bed" (.-name (.blockAt p (tu/pos 2 64 0))))))))))

(deftest a-carried-bed-is-never-put-down-on-a-chest-or-a-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}]
                                         :blocks (merge (tu/box -4 63 -4 4 63 4 "chest") {"2,63,2" "crafting_table"})})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 30))
          (is (empty? (st/calls p "place")) "no bed on a container floor")
          (is (= 1 (count (st/entries eng :bed-place-failed)))))))))

(deftest sleep-holds-an-occupied-bed-for-a-bounded-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})
              id (core/submit! eng '(jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}}) {})]
          (tu/seeing-after-look p)
          (await (st/tick-n eng 3))
          (is (= 1 (count (:list (core/state eng)))) "holds, not condition-failed at the start")
          (is (= [:bed-occupied] (distinct (map :reason (st/emitted seen :holding)))))
          (swap! clock + 3600000)
          (await (st/tick-n eng 3))
          (is (empty? (:list (core/state eng))) "the hold is bounded")
          (is (empty? (st/calls p "sleep"))))))))
