(ns engine.night-c-test
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
            [jobs.lib.cost :as cost]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.targets :as targets]
            [jobs.survival.night :as night]
            [jobs.survival.night.sites :as sites]
            [engine.night-support :refer [night noon roof holds? write! status! closed-hut holds-after? sleeper offline-why on-return! sleep-count! log-out-child fired wide-ground hillside breached-room run-night-in home-roof lit-world know-home! night-at-home with-child-log walled-home walled-pen bed-world bed-places with-room dug-into-home stone-room block-at room-with-doorway doorway-shut-early dawn-holds-now? dark-world flee-home-night stone-and-dirt-patch flee-with cave-blocks cave-night]]))

(deftest a-roof-over-a-breached-room-does-not-end-the-night
  (run-night-in {:blocks breached-room :inventory st/wall-stack}
                (fn [{:keys [p seen]}]
                  (is (seq (st/calls p "place")) "the body walled itself in under the roof instead of calling a hole in the wall roofed"))))

(defn nothing-placed-at-the-start?
  "No block was placed near the start (x below 5): a bare roof is no shelter, so the body may wall itself in at the home it walked to."
  [p]
  (not-any? #(< (:x (st/arg-pos %)) 5) (st/calls p "place")))

(deftest a-closed-room-with-a-stale-bed-open-entry-ends-the-night-roofed
  (run-night-in {:blocks st/bed-room}
                (fn [{:keys [p]}]
                  (is (empty? (st/calls p "place")) "shut in: nothing to do")
                  (is (empty? (tu/walk-calls p))))))

(deftest a-breached-room-without-blocks-to-wall-in-gives-up-instead-of-retrying
  (run-night-in {:blocks breached-room}
                (fn [{:keys [seen]}]
                  (is (seq (st/emitted seen :shelter.room_open))))))

(deftest a-failed-wall-in-blocks-walling-at-that-site-only
  (let [mem {:enclose-failed #{{:x 4 :y 64 :z 0}}}]
    (is (not (night/may-enclose? mem {:x 4 :y 64 :z 0})) "the site that failed")
    (is (night/may-enclose? mem {:x 44 :y 64 :z 0}) "another site")
    (is (night/may-enclose? {} {:x 4 :y 64 :z 0}))
    (is (not (night/may-enclose? {:enclose {:x 4 :y 64 :z 0}} {:x 44 :y 64 :z 0})) "a wall-in already under way")))

(deftest a-breached-room-is-said
  (run-night-in {:blocks breached-room :inventory st/wall-stack}
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
          (is (nothing-placed-at-the-start? p) "no pit dug")
          (is (= 10 (js/Math.floor (:x (st/pos-of p)))) "walked home"))))))

(defn ^:async night-at-seen-hut
  "The night with no home set, the body outside the hut of hut-shelter-test (door at z 2) at dusk with extra items; {:eng :p}."
  [inventory]
  (let [world (-> (hut/hut-world {:x 5 :y 64 :z 6} {}) (assoc :time night :inventory inventory :light-default [15 4])
                  (update :light assoc [5 64 6] [15 0]))
        {:keys [eng p] :as r} (await (night-at-home world {:x 5 :y 64 :z 0} {} {:home? false}))]
    (aset p "seenBlockAt" (fn [pos] (let [b (.blockAt p pos)] #js {:name (.-name b) :pos pos :age-ms 0 :properties (.-properties b)})))
    (await (tu/run-until-empty eng 80))
    r))

(deftest a-finished-hut-seen-is-walked-into-before-any-home-is-set
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (night-at-seen-hut st/dirt-stack))]
          (is (empty? (filter #(< 3 (:z (st/arg-pos %))) (st/calls p "place"))) "no pit dug beside the hut")
          (is (< (:z (st/pos-of p)) 2) "inside the hut")
          (is (false? (hut/door-open? p)) "the door is shut"))))))

(deftest of-two-huts-seen-the-one-go-to-reaches-is-walked-into-not-the-one-whose-door-is-nearer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shift (fn [m dx] (into {} (map (fn [[k v]] [(let [[x y z] (map #(js/parseInt %) (.split (name k) ","))] (str (+ x dx) "," y "," z)) v])) m))
              base (hut/hut-world {:x 5 :y 64 :z 6} {})
              ring (into {} (for [x (range 2 9) y [64 65] z (range -3 5)
                                  :when (or (#{2 8} x) (#{-3 4} z))] [(str x "," y "," z) "bedrock"]))
              world (-> base
                        (update :blocks #(merge % ring (shift % 20)))
                        (update :states #(merge % (shift % 20)))
                        (assoc :time night :light-default [15 4]))
              {:keys [eng p]} (await (night-at-home world {:x 5 :y 64 :z 0} {} {:home? false}))]
          (aset p "seenBlockAt" (fn [pos] (let [b (.blockAt p pos)] #js {:name (.-name b) :pos pos :age-ms 0 :properties (.-properties b)})))
          (let [at (.-sensedAt p)]
            ;; the inside of the far hut, as of a look through its door
            (aset p "sensedAt" (fn [pos] (let [k (str (.-x pos) "," (.-y pos) "," (.-z pos))]
                                           (if (and (<= 23 (.-x pos) 27) (<= -1 (.-z pos) 1))
                                             #js {:name (get (:blocks world) k "air") :pos pos :properties (clj->js (get (:states world) k))}
                                             (.call at p pos))))))
          (await (tu/run-until-empty eng 120))
          (is (< 20 (:x (st/pos-of p))) "in the far hut, the near one is walled off from the body"))))))

(defn ^:async night-at-hut-seen-outside
  "night-at-seen-hut with the inside of the hut never seen from outside (its door shut): a room not seen inside yet. The body starts z blocks south of the hut."
  [inventory z]
  (let [world (-> (hut/hut-world {:x 5 :y 64 :z z} {}) (assoc :time night :inventory inventory :light-default [15 4])
                  (update :light assoc [5 64 z] [15 0]))
        {:keys [eng p] :as r} (await (night-at-home world {:x 5 :y 64 :z 0} {} {:home? false}))
        at (.-sensedAt p)]
    (aset p "seenBlockAt" (fn [pos] (let [b (.blockAt p pos)] #js {:name (.-name b) :pos pos :age-ms 0 :properties (.-properties b)})))
    (aset p "sensedAt" (fn [pos] (if (and (<= 4 (.-x pos) 6) (<= -1 (.-z pos) 1) (<= 64 (.-y pos) 66) (< 2 (:z (st/pos-of p))))
                                   #js {:unknown true :pos pos}
                                   (.call at p pos))))
    (await (tu/run-until-empty eng 80))
    r))

(deftest a-hut-not-seen-inside-is-walked-to-when-the-walk-and-its-chance-of-being-open-cost-less-than-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (night-at-hut-seen-outside st/dirt-stack 6))]
          (is (>= 2 (:z (st/pos-of p))) "the walk to the hut and its chance of being open cost less than the dig-in quoted at 12 s: walked in to look")
          (is (= [{:seen 12 :unseen 0 :closed true}] (mapv #(select-keys % [:seen :unseen :closed]) (st/emitted seen :enter-room.arrived)))
              "the arrival reports the room's sides as it set out and that it is closed: a count for the prior"))))))

(deftest a-room-seen-inside-whose-sides-are-not-all-seen-is-priced-maybe-open
  (let [p (tu/fake (hut/hut-world {:x 5 :y 64 :z 6} {}))
        at (.-sensedAt p)
        room {:x 5 :y 64 :z 0}]
    (aset p "sensedAt" (fn [pos] (if (and (= 3 (.-x pos)) (= 64 (.-y pos)) (<= -1 (.-z pos) 1))
                                   #js {:unknown true :pos pos}
                                   (.call at p pos))))
    (let [{:keys [seen unseen]} (reach-room/boundary p room)]
      (is (seq unseen) "west wall feet cells unseen, every cell inside seen")
      (is (= (cost/closed-chance seen (count unseen)) (night/room-chance p room)))
      (is (< (night/room-chance p room) 1) "an opening may hide in a side not seen"))))

(deftest a-hut-not-seen-inside-is-dug-in-against-when-the-walk-and-its-chance-of-being-open-cost-more-than-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (night-at-hut-seen-outside st/dirt-stack 40))]
          (is (< 2 (:z (st/pos-of p))) "a walk dearer than digging in, with the room maybe open, is not made: the body digs in where it stands"))))))

(deftest a-carried-bed-goes-down-inside-the-hut-seen-not-outside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (night-at-seen-hut (conj st/dirt-stack {:name "red_bed" :count 1})))]
          (is (= 1 (count (bed-places p))))
          (is (every? #(< (:z (st/arg-pos %)) 2) (bed-places p)) "the bed is put down in the hut"))))))

(deftest a-lit-route-the-body-has-not-looked-at-yet-is-looked-at-before-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0} {} {:unseen true}))]
          (await (tu/run-until-empty eng 40))
          (is (nothing-placed-at-the-start? p) "no pit dug: the route was looked at, found lit, and walked")
          (is (= 10 (js/Math.floor (:x (st/pos-of p)))) "walked home"))))))

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
          (is (nothing-placed-at-the-start? p) "one dark cell costs a little more, not a refusal: walked")
          (is (= 10 (js/Math.floor (:x (st/pos-of p)))) "walked home"))))))

(deftest a-short-unlit-route-costs-less-than-digging-in-and-is-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc lit-world :light-default [15 3]) {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 40))
          (is (nothing-placed-at-the-start? p) "10 dim blocks cost more than a lit walk, still under the pit's quote")
          (is (= 10 (js/Math.floor (:x (st/pos-of p)))) "walked home"))))))

(def far-home-world (assoc lit-world :blocks (assoc wide-ground "40,66,0" "stone")))

(def no-flee
  "The far home is one roof block: no shelter on arrival, so a flight from it would follow; without one the walk's end is seen."
  {:jobs.survival.night.sites/flee-radius 0})

(deftest a-home-beyond-the-time-left-before-dark-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home far-home-world {:x 40 :y 64 :z 0} {:settings {:jobs.survival.night.sites/home-walk-margin-s 0}}))]
          (await (core/tick! eng))
          (is (seq (st/calls p "place")))
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-home-the-walk-to-fits-the-time-before-dark-is-walked-to-however-far-when-it-costs-less-than-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home far-home-world {:x 40 :y 64 :z 0} {:settings no-flee}))]
          (await (tu/run-until-empty eng 60))
          (is (= 40 (js/Math.floor (:x (st/pos-of p)))) "walked home with no bed to use or carry"))))))

(deftest of-two-far-homes-the-one-go-to-quotes-a-walk-to-is-taken-not-the-straight-line-nearer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walled-near (into {"-18,66,0" "stone"}
                                (for [x [-19 -18 -17] y [64 65] z [-1 0 1] :when (not= [x z] [-18 0])] [(str x "," y "," z) "stone_bricks"]))
              world (assoc lit-world :blocks (merge wide-ground walled-near (into {} (map (fn [[k v]] [(let [[x y z] (map #(js/parseInt %) (.split k ","))] (str (+ x 12) "," y "," z)) v])) home-roof)))
              {:keys [eng p]} (await (night-at-home world {:x -18 :y 64 :z 0} {:roofed-places [:home :cabin]}))]
          (mem/write! (:store eng) :cabin {:pos {:x 22 :y 64 :z 0}} mem/place-policy)
          (await (tu/run-until-empty eng 80))
          (is (= 22 (js/Math.floor (:x (st/pos-of p)))) "walked to the cabin go-to can reach, past the nearer home it cannot"))))))

(deftest a-remembered-place-too-far-to-walk-in-the-time-left-is-not-quoted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quoted (atom [])
              quote! pick/quote!
              {:keys [eng p]} (await (night-at-home far-home-world {:x 40 :y 64 :z 0} {:roofed-places [:home :far] :settings no-flee}))]
          (mem/write! (:store eng) :far {:pos {:x 4000 :y 64 :z 0}} mem/place-policy)
          (set! pick/quote! (fn [c args] (swap! quoted conj args) (quote! c args)))
          (try (await (tu/run-until-empty eng 60))
               (finally (set! pick/quote! quote!)))
          (is (= 40 (js/Math.floor (:x (st/pos-of p)))) "walked to the home that fits")
          (is (seq @quoted))
          (is (not-any? #(= :far (:place %)) @quoted) "the place no walk could cover in the time left costs no search"))))))

(deftest a-carried-bed-waits-for-a-home-far-off-whose-walk-go-to-quotes-no-dearer-than-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (update far-home-world :inventory conj {:name "red_bed" :count 1}) {:x 40 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 80))
          (is (= [] (filterv #(< (:x (st/arg-pos %)) 38) (bed-places p))) "no bed was set down on the open ground on the way")
          (is (<= 39 (:x (st/pos-of p))) "walked to the far home"))))))

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
          (is (nothing-placed-at-the-start? p) "no pit dug: walked home")
          (is (= 10 (js/Math.floor (:x (st/pos-of p)))) "walked home"))))))

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
              ended (atom 0)
              _ (await (with-child-log #(tu/run-until-empty eng 60)
                         (fn [c k sym a]
                           (when (and (= :roof-walk k) (:place a))
                             (.then (ctx/call c k sym (dissoc a :place))
                                    (fn [r] (when-not (= :continue r) (swap! ended inc)) r))))))]
          (is (= 1 @ended) "one walk made, never repeated")
          (is (seq (st/calls p "place")) "the night dug in after the walk failed"))))))

(deftest a-roof-walk-that-arrives-but-is-still-not-roofed-is-not-repeated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home lit-world {:x 10 :y 64 :z 0}))
              log (await (with-child-log #(tu/run-until-empty eng 60)
                           (fn [c k sym a] (when (:place a) (ctx/call c k sym (dissoc a :place))))))]
          (is (= 1 (count (filter #(and (= :roof-walk (first %)) (:place (nth % 2))) log))) "walked once, arrived unroofed, then dug in")
          (is (seq (st/calls p "place")) "the night dug in"))))))

(deftest a-carried-bed-is-put-down-inside-a-close-home-not-in-the-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (with-room room-with-doorway) {:x 10 :y 64 :z 0}))]
          (await (tu/run-until-empty eng 40))
          (is (<= 9 (:x (st/pos-of p))) "walked into the home first")
          (is (= [] (filterv #(< (:x (st/arg-pos %)) 9) (bed-places p))) "no bed was set down on the open ground")
          (is (seq (bed-places p)) "it was set down in the home"))))))

(deftest a-home-that-cannot-be-entered-leaves-the-carried-bed-in-the-bag
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [j [0.0 0.5 0.99]]
          (let [{:keys [eng p]} (await (night-at-home (update bed-world :blocks merge walled-home) {:x 10 :y 64 :z 0} {} {:rand (constantly j) :ray-deg 1 :radius 14}))]
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

(deftest a-doorway-the-room-cannot-be-closed-for-want-of-blocks-is-marked-tried-by-the-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (night-at-home (assoc (with-room room-with-doorway) :inventory [{:name "red_bed" :count 1}]) {:x 10 :y 64 :z 0}))
              log (await (with-child-log #(tu/run-until-empty eng 60)))]
          (is (<= (count (filter #(= :enter-room (first %)) log)) 2) "enter-room is not called again for a gap it said :no-blocks to: the night marked it tried")
          (is (empty? (filter #(not= "red_bed" (.-item (.-args %))) (st/calls p "place"))) "nothing placed in the doorway"))))))

(deftest a-doorway-with-an-animal-in-it-is-not-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (with-room room-with-doorway)
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
          (is (dawn-holds-now? eng p) "by day the dawn trigger holds for the shut doorway")
          (core/submit! eng '(jobs.survival.leave-shelter) {})
          (await (tu/run-until-empty eng 40))
          (is (every? #(= "air" (block-at p %)) filled) "leave-shelter opened it"))))))

(deftest a-doorway-that-cannot-be-opened-by-day-is-warned-and-tried-again-later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen filled]} (await (doorway-shut-early))
              failing? (atom true)]
          (.override (.-world p) "dig" (fn ^:async f [tok a impl] (if @failing? #js {:status "timeout"} (await (impl tok a)))))
          (core/submit! eng '(jobs.survival.leave-shelter) {})
          (await (tu/run-until-empty eng 40))
          (is (seq filled))
          (is (not-any? #(= "air" (block-at p %)) filled) "the digs failed, nothing opened")
          (is (seq (st/emitted seen :night.doorway-stuck)) "the stuck doorway is warned")
          (is (not (dawn-holds-now? eng p)) "not tried again at once")
          (mem/forget! (:store eng) ledger/held-kind)
          (is (dawn-holds-now? eng p) "once the hold lapses the trigger holds for it again")
          (reset! failing? false)
          (core/submit! eng '(jobs.survival.leave-shelter) {})
          (await (tu/run-until-empty eng 40))
          (is (every? #(= "air" (block-at p %)) filled) "and the next try opens it"))))))

(deftest a-doorway-that-stays-stuck-is-warned-once-and-retried-quietly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (doorway-shut-early))
              try-once! (fn ^:async once []
                          (mem/forget! (:store eng) ledger/held-kind)
                          (core/submit! eng '(jobs.survival.leave-shelter) {})
                          (await (tu/run-until-empty eng 40)))]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (await (try-once!))
          (await (try-once!))
          (await (try-once!))
          (is (= 1 (count (st/emitted seen :night.doorway-stuck))) "three failed tries, one warning")
          (mem/forget! (:store eng) ledger/held-kind)
          (is (dawn-holds-now? eng p) "still tried again once the hold lapses"))))))

(deftest a-filled-doorway-cell-someone-else-changed-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p filled look-round!]} (await (doorway-shut-early))]
          (is (seq filled))
          (doseq [cell filled] (fake/set-block! p cell "oak_planks"))
          (fake/swap-self! p assoc :pos [7 64 0]) ; outside the room, looking at the filled cells in its west wall
          (look-round!)
          (is (not (dawn-holds-now? eng p)) "no doorway of its own to open")
          (core/submit! eng '(jobs.survival.leave-shelter) {})
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
              log (await (with-child-log #(core/tick! eng)))]
          (is (empty? (filter #(= 'jobs.items.get-tool (second %)) log)) "no tool fetched in the dark"))))))

(deftest with-nothing-to-dig-the-night-flees-to-a-roofed-home-the-time-left-does-not-cover-by-an-unlit-route
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))]
          (await (tu/run-until-empty eng 80))
          (is (= [10 64 0] (mapv #(js/Math.floor (% (st/pos-of p))) [:x :y :z])) "walked to the roofed home")
          (is (seq (st/emitted seen :shelter.exposed)) "a bare roof is no shelter: the body holds exposed under it")
          (let [fled (st/emitted seen :shelter.fled)]
            (is (= [:roofed-place] (mapv :target fled)))))))))

(deftest a-night-with-no-way-to-shut-in-logs-out-before-ending-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time night :blocks (update-vals wide-ground (constantly "obsidian")) :inventory [] :self {:pos {:x 0.5 :y 64 :z 0.5}}})
              log (do (core/submit! eng '(jobs.survival.night) {})
                      (await (with-child-log #(tu/run-until-empty eng 80))))]
          (is (= 1 (count (st/emitted seen :shelter.exposed))))
          (is (some #(= 'jobs.survival.log-out (second %)) log) "the owner's fallback: no shelter, so log out"))))))

(deftest a-quote-is-asked-once-per-pass-and-again-in-the-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quote! pick/quote!
              asked (atom 0)
              places (mapv #(hash-map :pos {:x (* 10 %) :y 64 :z 0} :range 0) (range 1 6))
              opts {:target identity :walk identity}
              c {:primitives (tu/fake {}) ::pick/pass (atom {})}]
          (set! pick/quote! (fn [_ _] (swap! asked inc) (js/Promise.resolve {:uses {:job-time [0 ##Inf ##Inf]}})))
          (try
            (is (nil? (await (night/cheapest-walk! c places opts))) "none of them reachable: no pick")
            (is (= 5 @asked))
            (await (night/cheapest-walk! c places opts))
            (is (= 5 @asked) "a second sweep of the same pass asks no quote again")
            (await (night/cheapest-walk! (assoc c ::pick/pass (atom {})) places opts))
            (is (= 10 @asked) "the next pass asks again: the world may have changed")
            (finally (set! pick/quote! quote!))))))))

(deftest a-place-whose-straight-walk-costs-more-than-digging-in-is-not-quoted-and-the-body-digs-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quoted (atom [])
              quote! pick/quote!
              {:keys [eng p]} (await (night-at-home (assoc lit-world :blocks (assoc wide-ground "100,66,0" "stone")) {:x 100 :y 64 :z 0}))]
          (set! pick/quote! (fn [c args] (swap! quoted conj args) (quote! c args)))
          (try (await (tu/run-until-empty eng 60))
               (finally (set! pick/quote! quote!)))
          (is (< (* 100 cost/least-block-s) 60) "the walk's straight bound fits the time left before the dark plus the margin")
          (is (not-any? #(= :home (:place %)) @quoted) "its straight walk alone is dearer than the dig-in's quote: no search")
          (is (seq (st/calls p "place")) "dug in")
          (is (< (:x (st/pos-of p)) 5)))))))

(deftest a-quote-of-the-walk-to-a-place-still-searching-keeps-the-body-from-digging-in-for-a-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quote! pick/quote!
              searches (atom 0)
              placed-when-answered (atom nil)
              {:keys [eng p]} (await (night-at-home far-home-world {:x 40 :y 64 :z 0} {:settings {:jobs.survival.night.sites/home-walk-margin-s 0}}))]
          (set! pick/quote! (fn ^:async q [c args]
                              (cond
                                (not (:place args)) (await (quote! c args))
                                (< (swap! searches inc) 4) {:estimate true}
                                :else (do (when-not @placed-when-answered (reset! placed-when-answered [(count (st/calls p "place"))]))
                                          (await (quote! c args))))))
          (try (await (core/tick! eng))
               (finally (set! pick/quote! quote!)))
          (is (= [0] @placed-when-answered) "shelter! answered :again while the search went on: nothing dug in beside a place it has no price for"))))))
