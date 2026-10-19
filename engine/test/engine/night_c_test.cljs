(ns engine.night-c-test
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
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.targets :as targets]
            [jobs.survival.night :as night]
            [jobs.survival.night.sites :as sites]
            [engine.night-support :refer [night noon roof holds? write! status! closed-hut holds-after? sleeper offline-why on-return! sleep-count! log-out-child fired wide-ground hillside breached-room run-night-in home-roof lit-world know-home! night-at-home with-child-log walled-home walled-pen bed-world bed-places dug-into-home stone-room block-at room-with-doorway doorway-shut-early night-holds-now? dark-world flee-home-night stone-and-dirt-patch flee-with cave-blocks cave-night held-with-raw-beef furnace-ops held-starving cow zombie]]))

(deftest a-roof-over-a-breached-room-does-not-end-the-night
  (run-night-in {:blocks breached-room}
                (fn [{:keys [p seen]}]
                  (is (seq (st/calls p "place")) "the body walled itself in under the roof instead of calling a hole in the wall roofed"))))

(deftest a-closed-room-with-a-stale-bed-open-entry-ends-the-night-roofed
  (run-night-in {:blocks st/bed-room}
                (fn [{:keys [p]}]
                  (is (empty? (st/calls p "place")) "shut in: nothing to do")
                  (is (empty? (tu/walk-calls p))))))

(deftest a-breached-room-is-said
  (run-night-in {:blocks breached-room}
                (fn [{:keys [seen]}]
                  (let [e (first (st/emitted seen :shelter.room_open))]
                    (is (re-find #"not shut in" (str (:text e))))
                    (is (<= (:x (:escape e)) -2) "the event names the cell the flood got out at, beyond the hole in the west wall")
                    (is (re-find #"open to" (str (:text e))))))))

(deftest a-lit-route-to-a-close-roofed-home-is-walked-instead-of-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 40))
          (is (= [] (st/calls p "place")) "no pit dug")
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z]))))))))

(deftest a-lit-route-the-body-has-not-looked-at-yet-is-looked-at-before-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0} {} {:unseen true}))]
          (await (tu/run-until-empty eng 40))
          (is (= [] (st/calls p "place")) "no pit dug: the route was looked at, found lit, and walked")
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z]))))))))

(deftest a-long-unlit-route-is-dug-in-where-the-body-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (night-at-home (-> lit-world (assoc :light-default [15 0])
                                                             (update :blocks merge {"30,66,0" "stone"}))
                                                         {:x 30 :y 64 :z 0}))]
          (await (core/tick! eng))
          (is (seq (st/calls p "place")) "go-to's planned cost of the dark route beats digging in: dug in")
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-short-lit-route-with-one-dark-cell-is-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (update lit-world :light assoc [5 64 0] [15 0]) {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 40))
          (is (= [] (st/calls p "place")) "one dark cell costs a little more, not a refusal: walked")
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z]))))))))

(deftest a-short-unlit-route-costs-less-than-digging-in-and-is-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc lit-world :light-default [15 0]) {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 40))
          (is (= [] (st/calls p "place")) "10 dark blocks cost about twice a lit walk, still under the dig-in")
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z]))))))))

(deftest a-home-beyond-the-walk-radius-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc lit-world :blocks (assoc wide-ground "40,66,0" "stone")) {:x 40 :y 64 :z 0}))]
          (await (core/tick! eng))
          (is (seq (st/calls p "place")))
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-home-with-no-seen-roof-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc lit-world :blocks wide-ground) {:x 10 :y 64 :z 0}))]
          (await (core/tick! eng))
          (is (seq (st/calls p "place")))
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-roof-hidden-behind-its-near-edge-is-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0}))
              at (.-sensedAt p)]
          ;; from the ground far off, the roof cell over a home stays unknown however long the body looks; under it, it is seen
          (aset p "sensedAt" (fn [pos] (if (and (= [10 66 0] [(.-x pos) (.-y pos) (.-z pos)]) (< (:x (st/pos-of p)) 8))
                                         #js {:unknown true :pos pos}
                                         (.call at p pos))))
          (await (tu/run-until-empty eng 40))
          (is (= [] (st/calls p "place")) "no pit dug: walked home")
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z]))))))))

(deftest a-roofed-home-go-to-finds-no-way-to-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (update lit-world :blocks merge walled-home) {:x 10 :y 64 :z 0}))]
          (let [log (await (with-child-log #(tu/run-until-empty eng 60)))]
            (is (empty? (filter #(= :roof-walk (first %)) log)) "go-to's quote finds no route: no walk"))
          (is (seq (st/calls p "place")) "the night dug in"))))))

(deftest a-roof-walk-that-does-not-arrive-digs-in-and-is-not-retried-tonight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (update lit-world :blocks merge walled-pen) {:x 10 :y 64 :z 0}))
              log (await (with-child-log #(tu/run-until-empty eng 60)
                           (fn [c k sym a] (when (:place a) (ctx/call c k sym (-> a (dissoc :place) (assoc :pos {:x 10 :y 64 :z 4})))))))]
          (is (= 1 (count (filter #(and (= :roof-walk (first %)) (:place (nth % 2))) log))) "one walk attempted, never repeated")
          (is (seq (st/calls p "place")) "the night dug in after the walk failed"))))))

(deftest a-roof-walk-that-arrives-but-is-still-not-roofed-is-not-repeated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0}))
              beside {:x 10 :y 64 :z 1}
              log (await (with-child-log #(tu/run-until-empty eng 60)
                           (fn [c k sym a] (when (:place a) (ctx/call c k sym (-> a (dissoc :place) (assoc :pos beside)))))))]
          (is (= 1 (count (filter #(and (= :roof-walk (first %)) (:place (nth % 2))) log))) "walked once, arrived unroofed, then dug in")
          (is (seq (st/calls p "place")) "the night dug in"))))))

(deftest a-carried-bed-is-put-down-inside-a-close-home-not-in-the-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home bed-world {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 40))
          (is (<= 9 (:x (st/pos-of p))) "walked into the home first")
          (is (= [] (filterv #(< (:x (st/arg-pos %)) 9) (bed-places p))) "no bed was set down on the open ground")
          (is (seq (bed-places p)) "it was set down in the home"))))))

(deftest a-home-that-cannot-be-entered-leaves-the-carried-bed-in-the-bag
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [j [0.0 0.5 0.99]]
          (let [{:keys [eng p]} (await (night-at-home (update bed-world :blocks merge walled-home) {:x 10 :y 64 :z 0} {} {:rand (constantly j) :ray-deg 1}))]
            (await (tu/run-until-empty eng 60))
            (is (= [] (bed-places p)) (str "no bed in the open, sight jitter " j))
            (is (= [] (st/calls p "sleep")) "and no sleep out there")
            (is (seq (st/calls p "place")) "the shelter step dug in instead")))))))

(deftest a-roof-walk-into-a-breached-home-is-not-repeated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (night-at-home bed-world {:x 10 :y 64 :z 0}))
              _ (mem/write! (:store eng) :bed-open {:pos {:x 10 :y 64 :z 0}} sh-beds/bed-open-policy)
              walks (atom 0)
              log (await (with-child-log #(tu/run-until-empty eng 60)
                           (fn [_ k _ _] (when (and (= :roof-walk k) (< 2 (swap! walks inc)))
                                           {:kind :nothing-possible :data {}}))))]
          (is (= 1 (count (filter #(= :roof-walk (first %)) log))) "walked once, found the home open, did not walk again"))))))

(deftest a-carried-bed-in-a-home-go-to-dug-into-goes-under-the-roof-behind-a-shut-doorway-opened-at-dawn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p log]} (await (dug-into-home stone-room))
              calls (vec (.-calls (.-world p)))
              slept-at (first (keep-indexed #(when (= "sleep" (.-name %2)) %1) calls))
              before-sleep (subvec calls 0 (or slept-at 0))
              cells-of (fn [pred cs] (set (map st/arg-pos (filter pred cs))))
              dug (cells-of #(= "dig" (.-name %)) before-sleep)
              filled (cells-of #(and (= "place" (.-name %)) (not= "red_bed" (.-item (.-args %)))) before-sleep)]
          (is (= 1 (count (filter #(= :roof-walk (first %)) log))) "walked into the home once")
          (is (some? slept-at) "slept")
          (is (= [] (filterv #(not (let [{:keys [x z]} (st/arg-pos %)] (and (<= 10 x 12) (<= 0 z 1)))) (bed-places p)))
              "the bed went down under the roof, never outside it")
          (is (seq dug) "go-to dug its way in")
          (is (= dug (into #{} (filter dug) filled)) "every cell it dug was filled again before the sleep")
          (is (seq filled))
          (is (every? (cells-of #(= "dig" (.-name %)) (subvec calls (or slept-at 0))) filled) "at dawn the filled cells were dug again")
          (is (every? #(= "air" (block-at p %)) filled) "the doorway is open as it was"))))))

(deftest a-carried-door-shuts-the-dug-doorway-and-is-taken-out-at-dawn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (dug-into-home stone-room {:name "oak_door" :count 1}))
              door (first (filter #(= "oak_door" (.-item (.-args %))) (st/calls p "place")))]
          (is (some? door) "the door went in the doorway")
          (is (seq (st/calls p "sleep")))
          (is (= "air" (block-at p (st/arg-pos door))) "the door was taken out in the morning"))))))

(deftest a-carried-bed-in-a-dug-into-home-with-no-room-for-it-stays-in-the-bag
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p log]} (await (dug-into-home (update-vals walled-home #(if (= "stone_bricks" %) "stone" %))))]
          (is (= 1 (count (filter #(= :roof-walk (first %)) log))) "walked into the home once, not back and forth with the bed walk")
          (is (= [] (bed-places p)) "no bed in the open beside the home")
          (is (= [] (st/calls p "sleep")) "and no sleep out there"))))))

(deftest a-doorway-never-shut-is-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (dug-into-home room-with-doorway))]
          (is (seq (filterv #(= -1 (:z (st/arg-pos %))) (st/calls p "place"))) "the doorway was filled"))))))

(deftest a-doorway-with-an-animal-in-it-is-not-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> bed-world (update :blocks merge room-with-doorway)
                        (assoc :entities [{:id 8 :name "sheep" :kind "passive" :pos {:x 11.5 :y 64 :z -0.5}}]))
              {:keys [eng p]} (await (night-at-home world {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 60))
          (is (= [] (filterv #(= [11 -1] ((juxt :x :z) (st/arg-pos %))) (st/calls p "place"))) "no block in the sheep's cell"))))))

(deftest a-doorway-shut-on-a-night-that-ended-before-dawn-is-opened-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p filled]} (await (doorway-shut-early))]
          (is (seq filled) "the doorway was shut")
          (is (night-holds-now? eng p) "by day the night trigger holds for the shut doorway")
          (core/submit! eng (list 'jobs.survival.night {}) {})
          (await (tu/run-until-empty eng 40))
          (is (every? #(= "air" (block-at p %)) filled) "the night job by day opened it"))))))

(deftest a-doorway-that-cannot-be-opened-by-day-is-warned-and-tried-again-later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen filled]} (await (doorway-shut-early))]
          (core/submit! eng (list 'jobs.survival.night {}) {})
          (await (with-child-log #(tu/run-until-empty eng 40)
                   (fn [_ k _ _] (when (= :doorway-open k) {:kind :nothing-possible :data {}}))))
          (is (seq filled))
          (is (not-any? #(= "air" (block-at p %)) filled) "the walk failed, nothing dug")
          (is (seq (st/emitted seen :night.doorway-stuck)) "the stuck doorway is warned")
          (is (not (night-holds-now? eng p)) "not tried again at once")
          (mem/forget! (:store eng) :doorway-open-failed)
          (is (night-holds-now? eng p) "once the wait lapses the trigger holds for it again")
          (core/submit! eng (list 'jobs.survival.night {}) {})
          (await (tu/run-until-empty eng 40))
          (is (every? #(= "air" (block-at p %)) filled) "and the next try opens it"))))))

(deftest a-doorway-that-stays-stuck-is-warned-once-and-retried-quietly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (doorway-shut-early))
              fail-walk (fn [_ k _ _] (when (= :doorway-open k) {:kind :nothing-possible :data {}}))
              try-once! (fn ^:async once []
                          (mem/forget! (:store eng) :doorway-open-failed)
                          (core/submit! eng (list 'jobs.survival.night {}) {})
                          (await (with-child-log #(tu/run-until-empty eng 40) fail-walk)))]
          (await (try-once!))
          (await (try-once!))
          (await (try-once!))
          (is (= 1 (count (st/emitted seen :night.doorway-stuck))) "three failed tries, one warning")
          (mem/forget! (:store eng) :doorway-open-failed)
          (is (night-holds-now? eng p) "still tried again once the wait lapses"))))))

(deftest a-filled-doorway-cell-someone-else-changed-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p filled look-round!]} (await (doorway-shut-early))]
          (is (seq filled))
          (doseq [cell filled] (fake/set-block! p cell "oak_planks"))
          (fake/swap-self! p assoc :pos [6 64 0]) ; outside the room, looking at the filled cells in its west wall
          (look-round!)
          (is (not (night-holds-now? eng p)) "no doorway of its own to open")
          (core/submit! eng (list 'jobs.survival.night {}) {})
          (await (tu/run-until-empty eng 40))
          (is (every? #(= "oak_planks" (block-at p %)) filled) "the planks someone put there stay"))))))

(deftest the-night-never-fetches-a-tool-for-its-niche
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :blocks hillside :floor [-2 -2 8 2]})
              _ (st/refuse-cell! p [0 63 0])
              _ (st/dawn-after! p 4)
              _ (core/submit! eng '(jobs.survival.night) {})
              log (await (with-child-log #(core/tick! eng)))
              niche (first (filter #(= :niche (first %)) log))]
          (is (not (:fetch (nth niche 2))) "the niche child is not told to fetch")
          (is (empty? (filter #(= 'jobs.items.get-tool (second %)) log)) "no tool fetched in the dark"))))))

(deftest with-nothing-to-dig-the-night-flees-to-a-roofed-home-beyond-the-walk-radius-by-an-unlit-route
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))]
          (await (tu/run-until-empty eng 80))
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z])) "walked to the roofed home")
          (is (empty? (st/emitted seen :shelter.exposed)))
          (let [fled (st/emitted seen :shelter.fled)]
            (is (= [:roofed-place] (mapv :target fled)))))))))
