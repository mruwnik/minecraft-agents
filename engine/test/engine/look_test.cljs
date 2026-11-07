(ns engine.look-test
  "jobs.lib.look sight helpers over a real perception on the fake world: nothing is sensed through walls."
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.explore.look :as explore-look]
            [jobs.lib.cost.base-rates :as base-rates]
            [engine.perception.store :as store]
            [jobs.lib.look :as look]
            [jobs.lib.seen-light :as seen-light]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.util :as u]
            [jobs.lib.cost :as cost]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.reach.room :as reach-room]
            [clojure.string :as str]))

(defn seeing
  "Fake primitives over world with a perception (the body's rules, a shorter radius), one sight pass done. world's :rand
  pins the ray jitter."
  [world]
  (let [p (tu/fake-on-floor world)
        wrapped (perception/wrap p (perception/create (fake-raw/create p) (merge {:radius 16 :ray-deg 2} (select-keys world [:rand]))))]
    (.setOwner p "t1")
    (perception/pass! (aget wrapped "perception"))
    wrapped))

(def wall (tu/box 3 64 -3 3 66 3 "stone"))

(defn remember!
  "Memory takes the true state of each cell of blocks (a fake world's :blocks map), as if the body had seen it."
  [p blocks]
  (doseq [k (keys blocks)] (perception/touch! (aget p "perception") (mapv js/parseInt (str/split k #","))))
  p)

(defn names-of [hits] (mapv :name hits))

(deftest a-look-shows-the-top-of-a-floor-block-whose-centre-the-floor-in-front-hides
  (let [p (remember! (seeing {:blocks wall}) wall)]
    (is (look/look-shows? p {:x -4 :y 63 :z 0}) "the floor 4 blocks off: its top is in view")
    (is (not (look/look-shows? p {:x 6 :y 63 :z 0})) "behind the wall neither its centre nor a face is")))

(deftest a-look-shows-a-cell-past-the-cells-the-body-has-not-seen
  (let [p (tu/fake-on-floor {:blocks wall})
        unlooked (perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))]
    (.setOwner p "t1")
    (is (look/look-shows? unlooked {:x 6 :y 64 :z 0}) "the wall is not seen yet: what lies behind it may show")
    (remember! unlooked wall)
    (is (not (look/look-shows? unlooked {:x 6 :y 64 :z 0})) "the wall seen hides it")))

(deftest a-ray-is-unseen-only-up-to-the-first-seen-rock
  (let [p (tu/fake-on-floor {:blocks wall})
        wrap #(perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))
        eye {:x 2.5 :y 65.6 :z 0.5}
        behind {:x 6.5 :y 65.5 :z 0.5}]
    (.setOwner p "t1")
    (is (look/ray-unseen? (wrap) eye behind) "nothing seen yet: a look may show something")
    (let [w (remember! (wrap) wall)]
      (is (not (look/ray-unseen? w eye behind)) "the wall seen first hides the cell behind it: a look adds nothing")
      (is (look/ray-unseen? w eye {:x -6.5 :y 65.5 :z 0.5}) "the other way is still unseen"))))

(deftest a-block-behind-a-wall-is-not-seen-and-one-in-view-is
  (let [p (seeing {:blocks (merge wall {"6,64,0" "coal_ore" "0,64,5" "iron_ore"})})]
    (is (= ["iron_ore"] (names-of (look/seen-blocks p {:names ["coal_ore" "iron_ore"] :radius 16}))))
    (is (= {:x 0 :y 64 :z 5} (:pos (first (look/seen-blocks p {:names ["iron_ore"] :radius 16})))))))

(deftest seen-block-tells-a-never-seen-cell-from-a-seen-one
  (let [p (seeing {:blocks (merge wall {"6,64,0" "coal_ore" "0,64,5" "iron_ore"})})]
    (is (= "iron_ore" (:name (look/seen-block p {:x 0 :y 64 :z 5}))))
    (is (:unknown (look/seen-block p {:x 6 :y 64 :z 0})))))

(deftest beliefs-know-the-biome-of-seen-ground-only
  (let [p (seeing {:blocks (merge wall {"6,64,0" "coal_ore" "0,64,5" "iron_ore"})})
        _ (aset p "rawWorld" #js {:biomeAt (fn [_ _ _] "plains")})
        biome-at (:biome-at (look/beliefs p))]
    (is (= "plains" (biome-at {:x 0 :y 64 :z 5})))
    (is (nil? (biome-at {:x 6 :y 64 :z 0})) "behind the wall: never seen")))

(deftest live-drops-a-remembered-block-that-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:blocks {"0,64,2" "iron_ore"}})
              q {:names ["iron_ore"] :live? true}]
          (is (= 1 (count (look/seen-blocks p q))))
          (is (= 1 (count (look/seen-blocks p (dissoc q :live?)))))
          (await (.dig p "t1" (clj->js {:pos {:x 0 :y 64 :z 2}})))
          (is (zero? (count (look/seen-blocks p q)))))))))

(deftest seen-blocks-without-perception-is-empty
  (is (= [] (look/seen-blocks (tu/fake {:blocks {"0,64,5" "iron_ore"}}) {:names ["iron_ore"]}))))

(def entities
  [{:id 1 :name "item" :kind "item" :pos [5 64 0] :visible false :item {:name "diamond" :count 1}}
   {:id 2 :name "item" :kind "item" :pos [0 64 4] :visible true :item {:name "coal" :count 1}}
   {:id 3 :name "Steve" :kind "player" :pos [5 64 1] :visible false}
   {:id 4 :name "cow" :kind "passive" :pos [0 64 3]}])

(deftest an-item-behind-a-wall-is-not-seen-a-player-is
  (let [p (seeing {:blocks wall :entities entities})
        seen (look/seen-entities p {})]
    (is (= #{2 3 4} (set (map :id seen))))))

(defn stub-ctx [p]
  (let [mem (atom {})]
    {:primitives p
     :act (fn [k args] (.call (aget p (name k)) p "t1" args))
     :update-mem (fn [f args] (apply swap! mem f args))}))

(deftest find-seen-turns-once-and-finds-a-block-behind-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:blocks {"-4,64,0" "iron_ore"}})
              c (stub-ctx p)
              q {:names ["iron_ore"] :radius 16}]
          (is (empty? (look/seen-blocks p q)) "behind the body: not seen yet")
          (is (= ["iron_ore"] (names-of (await (look/find-seen! c q)))))
          (let [n (count (.-calls (.-world p)))]
            (is (empty? (await (look/find-seen! c (assoc q :names ["gold_ore"])))))
            (is (= 8 (- (count (.-calls (.-world p))) n)) "one look around, four headings by two glances")))))))

(deftest a-glance-is-level-and-down-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {})
              n (count (.-calls (.-world p)))]
          (await (look/glance! (stub-ctx p) [[0 1]]))
          (is (= 2 (- (count (.-calls (.-world p))) n))))))))

(deftest a-glance-until-tests-after-the-level-look-before-the-look-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {})
              n (count (.-calls (.-world p)))
              r (await (look/glance-until! (stub-ctx p) [[0 1] [1 0]] (constantly :found)))]
          (is (= :found r))
          (is (= 1 (- (count (.-calls (.-world p))) n)) "stops after the first look, which is level"))))))

(deftest a-look-around-under-a-seen-ceiling-adds-no-look-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:blocks (tu/box -6 66 -6 6 66 6 "stone")})
              c (stub-ctx p)
              _ (await (look/look-around! c))
              n (count (.-calls (.-world p)))]
          (await (look/look-around! c))
          (is (= 8 (- (count (.-calls (.-world p))) n))))))))

(deftest the-look-up-ray-reaches-a-stone-three-off-and-five-up
  (is (some #{{:x 3 :y 6 :z 0}} (look/up-cells {:x 0.5 :y 1.6 :z 0.5} [1 0])) "a stone on a pillar is under the ray, so an unseen cell there makes the body look up"))

(deftest seen-blocks-rejects-a-query-that-is-not-names-or-match
  (let [p (seeing {:blocks {"0,64,2" "iron_ore"}})]
    (is (thrown? js/Error (look/seen-blocks p {:names (fn [n] (= n "iron_ore"))})) "names must be a coll")
    (is (thrown? js/Error (look/seen-blocks p {:match #{"iron_ore"}})) "match must be a fn")
    (is (thrown? js/Error (look/seen-blocks p {:names ["iron_ore"] :match any?})) "not both")
    (is (thrown? js/Error (look/seen-blocks p {:radius 8})) "every block must be asked for")
    (is (= ["iron_ore"] (names-of (look/seen-blocks p {:names #{"iron_ore"}}))))
    (is (= ["iron_ore"] (names-of (look/seen-blocks p {:match #(= % "iron_ore")}))))
    (is (some #{"iron_ore"} (names-of (look/seen-blocks p {:all? true :radius 3}))))))

(defn change-unseen!
  "The block at pos is air now, without the body having looked."
  [p pos]
  (swap! (fake/state p) update :blocks dissoc pos))

(deftest a-seen-block-changed-out-of-view-keeps-its-old-name-and-age
  (let [p (seeing {:blocks {"0,64,2" "iron_ore"}})
        pos {:x 0 :y 64 :z 2}]
    (change-unseen! p [0 64 2])
    (is (= "iron_ore" (:name (look/seen-block p pos))) "memory is what was seen")
    (is (number? (:age-ms (look/seen-block p pos))))
    (is (= ["iron_ore"] (names-of (look/seen-blocks p {:names ["iron_ore"]}))))
    (is (empty? (look/seen-blocks p {:names ["iron_ore"] :live? true})))))

(deftest live-within-checks-only-cells-seen-that-recently
  (let [gone (fn [window]
               (let [p (seeing {:blocks {"0,64,2" "iron_ore"}})]
                 (change-unseen! p [0 64 2])
                 (look/seen-blocks p {:names ["iron_ore"] :live? true :live-within-ms window})))]
    (is (empty? (gone 600000)) "seen just now: checked, gone")
    (is (= 1 (count (gone -1))) "older than the window: trusted")))

(defn hostile-ids [p o]
  (set (map :id (filter #(= "hostile" (:kind %)) (look/seen-entities p o)))))

(defn mob-stub
  "Primitives whose entities() lists raw and whose knownMobs() lists known (JS maps as perception returns them)."
  [raw known]
  #js {:entities (fn [_] (clj->js raw)) :knownMobs (fn [] (clj->js known))})

(def known-rows
  [{:id 10 :name "zombie" :kind "hostile" :pos [3 64 0] :distance 3 :visible false :remembered true :ageMs 4000}
   {:id 11 :name "skeleton" :kind "hostile" :pos [9 64 0] :distance 9 :visible true :seen true}
   {:id 12 :name "creeper" :kind "hostile" :pos [20 64 0] :distance 20 :visible false :heard true}])

(deftest seen-entities-lists-known-hostiles-and-filters-them
  (let [p (mob-stub [{:id 4 :name "cow" :kind "passive" :pos [0 64 3]}
                     {:id 99 :name "zombie" :kind "hostile" :pos [5 64 5] :visible false}] known-rows)
        ids (fn [o] (set (map :id (look/seen-entities p o))))]
    (is (= #{4 10 11 12} (ids {})) "raw hostiles are replaced by the known ones, a remembered one included")
    (is (= #{4 10 11} (ids {:radius 10})))
    (is (= #{10} (hostile-ids p {:radius 5})))
    (is (= #{11} (hostile-ids p {:names ["skeleton"]})))
    (is (= 3 (count (filter #(= "hostile" (:kind %)) (look/seen-entities p {:max 1})))) "max is not applied to known")
    (is (= 4000 (:ageMs (first (filter #(= 10 (:id %)) (look/seen-entities p {}))))))))

(deftest seen-blocks-properties-are-the-last-seen-ones
  (let [p (seeing {:blocks {"0,64,2" "iron_ore"}})
        b (first (look/seen-blocks p {:names ["iron_ore"] :properties? true}))]
    (is (contains? b :properties))
    (is (nil? (:properties (first (look/seen-blocks p {:names ["iron_ore"]})))))))

;; ---- dark-fn: the planner's test of a feet cell

(def day 6000)
(def midnight 18000)

(defn dark-of
  "1 or 0: dark-fn's call on cell x y z of a body that looked round a walled-off room at time t (with blocks more)."
  ([t cell] (dark-of t cell {}))
  ([t cell blocks]
   (let [p (seeing {:time t :blocks (merge wall blocks)})]
     ((:at (look/dark-fn p)) (first cell) (second cell) (nth cell 2)))))

(deftest dark-fn-needs-a-perception
  (is (nil? (look/dark-fn (tu/fake {})))))

(deftest an-unseen-cell-is-dark-at-night-only
  (is (= 1 (dark-of midnight [6 64 0])))
  (is (= 0 (dark-of day [6 64 0]))))

(defn sealed
  "A body that has looked round the walled-off room by day with every cell at [sky block] light (sky 0: sealed from
  the sky), the cells of lights at their own, blocks more; at time t."
  ([t light lights] (sealed t light lights {}))
  ([t light lights blocks]
   (let [p (seeing {:time day :blocks (merge wall blocks) :light-default light :light lights})]
     (swap! (fake/state p) assoc :time t)
     p)))

(deftest an-unseen-cell-in-a-dark-sealed-place-is-dark-by-day
  (let [p (sealed day [0 0] {})]
    (is (= 1 ((:at (look/dark-fn p)) 6 64 0)))
    (is (false? (sh-sense/lit-here? p {:x 6 :y 64 :z 0})) "the old guess read the open sky by day")))

(deftest an-unseen-cell-in-a-block-lit-sealed-hall-is-lit-at-night
  (let [p (sealed midnight [0 14] {} (tu/box -12 68 -12 12 68 12 "glowstone"))]
    (is (= 0 ((:at (look/dark-fn p)) 4 67 0)) "just past the wall's top, beside the seen lit air: the old guess read every unseen cell dark at night")
    (is (true? (sh-sense/lit-here? p {:x 4 :y 67 :z 0})))))

(deftest seen-block-light-lights-an-unseen-cell-less-one-a-block
  ;; a light 5 (a lit candle's 3 and a two-candle's 6 less one) three blocks ahead, falling one a block to the body's cell
  (let [p (sealed midnight [0 0] (into {} (for [z (range 4)] [[0 64 z] [0 (+ 2 z)]])) {"0,64,4" "candle"})]
    (is (true? (look/unseen-lit? p {:x 0 :y 64 :z -1})) "2 less 1 block, behind the body")
    (is (false? (look/unseen-lit? p {:x 0 :y 64 :z -2})) "2 less 2 blocks")))

(defn unlooked
  "Fake primitives over world with a perception that has seen nothing yet."
  [world]
  (let [p (tu/fake world)
        wrapped (perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))]
    (.setOwner p "t1")
    wrapped))

(deftest unseen-cells-between-dim-the-chance-the-body-s-light-reaches-past-them
  ;; nothing seen yet at y 0: the old guess spread the light 14 through them as air
  (let [p (unlooked {:self {:pos [0 0 0]} :light {[0 0 0] [0 14]} :light-default [0 0]})]
    (is (= (- 1 (base-rates/unseen-solid 0 0 1 true nil)) (look/unseen-lit-chance p {:x 2 :y 0 :z 0})) "one unseen cell between, next to the body's lit cell")
    (is (= (base-rates/open-run-chance 0 0 2 true nil) (look/unseen-lit-chance p {:x 3 :y 0 :z 0})) "two: the second further from the cut, likelier rock")))

(deftest a-seen-wall-stops-the-spread
  (let [inside (for [x [-1 0 1] y [64 65 66] z [-1 0 1]] [x y z])
        p (seeing {:time midnight :blocks (apply dissoc (tu/box -2 63 -2 2 67 2 "stone") (map #(.join (clj->js %) ",") inside))})]
    (doseq [yaw [0 90 180 270] pitch [-60 0 60]]
      (swap! (fake/state p) assoc :yaw yaw :pitch pitch)
      (perception/pass! (aget p "perception")))
    (swap! (fake/state p) assoc :light-default [0 0] :light (zipmap inside (repeat [0 14])))
    (is (every? #(sh-sense/seen-cell? p %) [{:x 2 :y 64 :z 0} {:x 0 :y 67 :z 0} {:x 0 :y 64 :z -2}]) "the walls round the room are seen")
    (is (zero? (look/unseen-lit-chance p {:x 4 :y 64 :z 0})) "the room's light 14 does not pass its seen walls")))

(deftest a-sealed-room-s-dark-is-no-evidence-for-the-light-outside-its-walls
  ;; by day in a dark stone room seen whole: the old guess pooled its dark air into the sky share, so the open ground
  ;; past the walls read dark
  (let [inside (for [x [-1 0 1] y [64 65] z [-1 0 1]] [x y z])
        p (seeing {:time day :light-default [0 0] :blocks (apply dissoc (tu/box -2 63 -2 2 66 2 "stone") (map #(.join (clj->js %) ",") inside))})]
    (is (seq (look/room-air-unseen p)) "one look ahead leaves cells beside the room's air unseen")
    (doseq [yaw [0 90 180 270] pitch [-60 0 60]]
      (swap! (fake/state p) assoc :yaw yaw :pitch pitch)
      (perception/pass! (aget p "perception")))
    (is (every? #(sh-sense/seen-cell? p %) [{:x 2 :y 64 :z 0} {:x 0 :y 66 :z 0} {:x 0 :y 64 :z -2}]) "the walls round the room are seen")
    (is (= [] (look/room-air-unseen p)) "looked round: the air is seen sealed")
    (is (= (base-rates/sky-share 64 (look/dark-light) 0 0) (look/unseen-lit-chance p {:x 6 :y 64 :z 0}))
        "outside: the measured prior at its height and the sky light day needs")))

(deftest an-unseen-gap-under-seen-open-sky-is-priced-not-free
  ;; a shaft down from a hole in the floor by day, seen open to the sky round the body: under the hole's seen air, past
  ;; every seen light's reach, the measured chance under a hole (the old guess: the open ground's sky share at that
  ;; height, lit)
  (let [p (seeing {:time day :rand (constantly 0.5) :blocks {"0,63,5" "air" "0,63,6" "air" "0,63,7" "air"}})
        top (first (filter #(sh-sense/seen-cell? p {:x 0 :y % :z 7}) (range 49 64)))
        chance (look/unseen-lit-chance p {:x 0 :y 48 :z 7})]
    (is (< 52 top) "the hole is seen, the shaft under it not")
    (is (= (base-rates/sky-under (- top 49) (look/dark-light) true) chance) "the seen floor beside the hole: narrow")
    (is (< chance 0.5))))

(deftest unseen-ground-behind-a-wall-by-day-is-priced-by-the-open-sky-seen-over-it
  ;; the old guess took each unseen cell under the seen sky over the wall as a rock guess of its own, so the ground just
  ;; behind a 5-high wall read about 0.06 lit by full day
  (let [p (seeing {:time day :blocks (tu/box 6 64 -8 6 68 8 "stone")})]
    (doseq [yaw [0 90 180 270]]
      (swap! (fake/state p) assoc :yaw yaw)
      (perception/pass! (aget p "perception")))
    (let [top (first (filter #(sh-sense/seen-cell? p {:x 7 :y % :z 0}) (range 65 90)))]
      (is (= 69 top) "the sky over the wall is seen, the ground behind it not")
      (is (= (base-rates/sky-under 4 (look/dark-light) false) (look/unseen-lit-chance p {:x 7 :y 64 :z 0} 0)))
      (is (true? (look/unseen-lit? p {:x 7 :y 64 :z 0} 0)) "more likely lit than not"))))

(deftest an-unseen-cell-under-open-sky-is-lit-by-day-only
  (is (true? (look/unseen-lit? (seeing {:time day}) {:x 40 :y 64 :z 0})) "far past sight, where the seen ground is under open sky")
  (is (false? (look/unseen-lit? (seeing {:time midnight}) {:x 40 :y 64 :z 0})))
  (is (true? (look/unseen-lit? (tu/fake {}) {:x 40 :y 64 :z 0})) "no perception: the open sky"))

(deftest a-seen-cell-is-lit-by-block-light-or-by-day-sky
  (is (= 0 (dark-of midnight [0 64 5] {"0,64,7" "torch"})) "a seen torch's light at night")
  (is (= 1 (dark-of midnight [0 64 5])) "open sky at night")
  (is (= 0 (dark-of day [0 64 5])) "open sky by day")
  (is (= 1 ((:at (look/dark-fn (sealed day [0 0] {}))) 0 64 1)) "a seen cave cell is dark by day"))

(deftest a-seen-cell-keeps-the-light-it-was-seen-in
  ;; the torch's cell and the one by it at the light it gives them, as the world lights them; the body turned away first
  (let [lit {[0 64 7] [15 14] [0 64 6] [15 13]}
        dark-at (fn [p] ((:at (look/dark-fn p)) 0 64 6))
        broken (seeing {:time midnight :blocks (merge wall {"0,64,7" "torch"}) :light lit})
        placed (seeing {:time midnight :blocks wall})]
    (swap! (fake/state broken) #(-> % (assoc :yaw 180) (update :blocks dissoc "0,64,7") (assoc :light {})))
    (is (= 0 (dark-at broken)) "a torch broken out of sight still lights the cell it lit")
    (swap! (fake/state placed) #(-> % (assoc :yaw 180) (assoc-in [:blocks "0,64,7"] "torch") (assoc :light lit)))
    (is (= 1 (dark-at placed)) "a torch placed out of sight does not")))

;; shelter.sense, plant-sapling, patch and see-lit-fn read light through known-light-at
(deftest known-light-at-is-the-remembered-light-and-nil-where-unseen
  (let [lit {[0 64 7] [15 14] [0 64 6] [15 13]}
        p (seeing {:time midnight :blocks (merge wall {"0,64,7" "torch"}) :light lit})
        block-at #(some-> (look/known-light-at p 0 64 %) (bit-and 15))]
    (swap! (fake/state p) #(-> % (assoc :yaw 180) (update :blocks dissoc "0,64,7") (assoc :light {})))
    (is (pos? (block-at 6)) "a torch broken out of sight still lights the cell it lit")
    (is (nil? (look/known-light-at p 7 64 0)) "behind the wall, never seen: nothing known")
    (swap! (fake/state p) assoc :light {[7 64 0] [15 15]})
    (is (nil? (look/known-light-at p 7 64 0)) "the live light of an unseen cell is not read")))

(deftest an-unseen-cell-the-walk-reads-as-water-is-water-a-drowned-may-spawn-in
  (let [p (seeing {:blocks (merge wall (tu/box 6 63 -1 8 63 1 "water"))})
        class (fn [wet?] ((look/water-fn p {:wet? wet?}) 7 63 0))]
    (is (= 2 (class (constantly true))) "behind the wall, never seen: the worse case")
    (is (= 0 (class (constantly false))) "read as dry by the walk: no water")
    (is (= 0 (class nil)))))

(deftest a-phantom-floor-is-classed-by-the-nearest-seen-water-along-an-axis
  (let [class (fn [floor phantom? [x y z]]
                (let [p (seeing {:blocks (merge wall (tu/box 1 63 -1 8 63 1 floor))})]
                  (swap! (fake/state p) assoc :yaw 270 :pitch 30)
                  (perception/pass! (aget p "perception"))
                  ((look/water-fn p {:phantom? phantom? :lags 8}) x y z)))]
    (is (= look/lag-class (class "water" (constantly true) [5 64 0])) "seen water 1 off, before the wall")
    (is (= look/lag-class (class "water" (constantly true) [1 65 0])) "a floor one up: seen air over the water 1 off")
    (is (= 0 (class "water" nil [5 64 0])) "unmasked: the walk reads the floor as it is")
    (is (= 0 (class "stone" (constantly true) [5 64 0])) "a seen dry floor ends each axis")))

;; water-fn skips the axis scans when no memory section they cross holds seen water, and keeps that per section
(deftest a-phantom-floor-is-classed-by-water-in-the-next-section-and-by-what-memory-holds-now
  (let [p (remember! (seeing {:blocks {"16,63,0" "stone"}}) {"16,63,0" "stone"})
        class #((look/water-fn p {:phantom? (constantly true) :lags 8}) 15 64 0)]
    (is (= 0 (class)) "seen stone 1 off, in the next section")
    (fake/set-block! p [16 63 0] "water")
    (remember! p {"16,63,0" "water"})
    (is (= look/lag-class (class)) "the same cell seen as water since")))

(deftest explore-look-lists-a-seen-hostile-and-a-heard-one-only-roughly
  (let [p (seeing {:blocks wall
                   :entities [{:id 7 :name "zombie" :kind "hostile" :pos [0 64 5]}
                              {:id 8 :name "zombie" :kind "hostile" :pos [6 64 0]}]})
        r (explore-look/observe p (explore-look/options {:entity-names ["zombie"] :max-blocks 0}))
        [seen heard] (sort-by #(boolean (:heard? %)) (:entities r))]
    (is (= 2 (count (:entities r))))
    (is (= [0 64 5] (:pos seen)))
    (is (true? (:heard? heard)))
    (is (not (contains? heard :pos)))
    (is (= {:direction :east :band :near} (select-keys heard [:direction :band])))))

(deftest seen-objs-hides-what-sits-behind-a-wall-but-not-players-or-hostiles
  (let [p (mob-stub [{:id 1 :name "cow" :kind "passive" :pos [0 64 3] :visible true}
                     {:id 2 :name "cow" :kind "passive" :pos [0 64 4] :visible false}
                     {:id 3 :name "zombie" :kind "hostile" :pos [0 64 5] :visible false}
                     {:id 4 :name "Bob" :kind "player" :pos [0 64 6] :visible false}] [])]
    (is (= [1 3 4] (map #(.-id %) (look/seen-objs p {:radius 10}))))
    (is (= [1] (map #(.-id %) (look/seen-objs p {:radius 10 :max 1}))) "max counts after the hiding")))


(deftest a-look-for-entities-or-other-blocks-is-no-look-for-the-wanted-blocks
  (let [c {:primitives (seeing {})}
        mem (atom {})
        crops {:names #{"melon"} :radius 8}
        surveyed? (fn [per want] (with-redefs [ctx/mem (fn [_] @mem)] (look/surveyed? c per want)))
        note! (fn [want] (with-redefs [ctx/update-mem! (fn [_ f & args] (apply swap! mem f args))]
                           (look/note-surveyed! c (look/cell-of (u/self-pos c)) want)))]
    (note! look/hazards-only)
    (is (surveyed? :cell look/hazards-only) "the entity look covers an entity want")
    (is (not (surveyed? :cell nil)) "but no full pass")
    (is (not (surveyed? :cell crops)) "and no look for blocks")
    (note! {:names #{"carrots"} :radius 8})
    (is (not (surveyed? :cell crops)) "a look for other block names is no look for melons")
    (note! crops)
    (is (surveyed? :near crops))
    (is (not (surveyed? :cell nil)) "a narrow look is no full pass")
    (is (not (surveyed? :cell (assoc crops :radius 40))) "a look 8 blocks out is no look 40 out")
    (is (surveyed? :cell (assoc crops :radius 4)) "but covers a nearer one")
    (note! nil)
    (is (every? #(surveyed? :cell %) [nil look/hazards-only crops]) "a full pass covers every want")))

(deftest a-look-for-a-match-fn-or-cells-is-shared-only-with-the-same-id-fn-or-cells
  (let [c {:primitives (seeing {})}
        mem (atom {})
        surveyed? (fn [want] (with-redefs [ctx/mem (fn [_] @mem)] (look/surveyed? c :cell want)))
        note! (fn [want] (with-redefs [ctx/update-mem! (fn [_ f & args] (apply swap! mem f args))]
                           (look/note-surveyed! c (look/cell-of (u/self-pos c)) want)))
        trees {:id :trees :match (fn [_] true) :radius 8}]
    (note! trees)
    (is (surveyed? (assoc trees :match (fn [_] true))) "the same id is the same look, whatever closure carries it")
    (is (not (surveyed? {:id :doors :match (fn [_] true) :radius 8})) "another id is another look")
    (is (not (surveyed? {:match (fn [_] true) :radius 8})) "a match fn with no id is shared only with itself")
    (note! {:cells [{:x 1 :y 64 :z 1}]})
    (is (surveyed? {:cells #{{:x 1 :y 64 :z 1}}}))
    (is (not (surveyed? {:cells [{:x 2 :y 64 :z 1}]})) "other cells are not looked at")))

(deftest see-samples-the-entity-cache-but-not-twice-in-a-row
  (let [n (atom 0)
        p (doto (seeing {})
            (aset "sampleEntities" (fn [] (swap! n inc))))
        c {:primitives p}]
    (look/see! c)
    (is (= 1 @n) "a look books the entity cache")
    (look/see! c)
    (is (= 1 @n) "a look right after shares that sample")
    (aset p "lastForcedSampleAt" -1000)
    (look/see! c)
    (is (= 2 @n) "a later look samples again")))

(deftest ground-past-the-walls-of-a-lit-sealed-room-at-night-is-lowered-by-dawn
  (let [inside (for [x [-1 0 1] y [64 65] z [-1 0 1]] [x y z])
        p (seeing {:time midnight :light-default [0 0] :light (zipmap inside (repeat [0 14]))
                   :blocks (apply dissoc (tu/box -2 63 -2 2 66 2 "stone") (map #(.join (clj->js %) ",") inside))})]
    (doseq [yaw [0 90 180 270] pitch [-60 0 60]]
      (swap! (fake/state p) assoc :yaw yaw :pitch pitch)
      (perception/pass! (aget p "perception")))
    (let [cell {:x 6 :y 64 :z 0}]
      (is (= [] (look/room-air-unseen p)))
      (is (< (spawn-light/chance-at p cell false 0) (spawn-light/chance-at p cell false)))
      (is (spawn-light/dawn-lowers? p cell)))))

(deftest remembered-light-never-passes-a-cell-the-body-has-not-seen
  ;; a remembered torch two cells from a seen air cell with an unseen cell between: the seen cell stays dark
  (let [^js table @fake-raw/light-table
        torch (first (filter #(and (== 14 (aget (.-emit table) %)) (zero? (aget (.-filter table) %))) (range (.-length (.-emit table)))))
        sec #js {:ids (js/Uint16Array. 4096) :wrote 1}
        st #js {:stores (js/Map.) :dim "overworld" :writes 1 :lightTable table}
        put! (fn [x id] (aset (.-ids sec) (store/cell-index x 0 0) (inc id)))]
    (.set (store/store-of st "overworld") (store/section-key 0 0 0) sec)
    (put! 0 0) (put! 2 torch) (put! 3 0)
    (let [f (seen-light/light-fn #js {:perception {:st st}})]
      (is (= 0 (bit-and (f 0 0 0) 15)) "behind an unseen cell")
      (is (= 13 (bit-and (f 3 0 0) 15)) "beside the torch's neighbour on the seen side: 14 less a cell"))))

(deftest block-only-light-floods-no-sky-and-nothing-without-a-light-source
  ;; a night's dark test needs no sky (it cannot light): open air seen at full sky floods nothing until a torch is remembered
  (let [^js table @fake-raw/light-table
        torch (first (filter #(and (== 14 (aget (.-emit table) %)) (zero? (aget (.-filter table) %))) (range (.-length (.-emit table)))))
        floods (atom 0)
        counted (js/Object.assign #js {} table #js {:sectionLight (fn [t ids bits] (swap! floods inc) (.sectionLight table t ids bits))})
        sec #js {:ids (js/Uint16Array. 4096) :sky (doto (js/Uint8Array. 512) (.fill 255)) :wrote 1}
        st #js {:stores (js/Map.) :dim "overworld" :writes 1 :lightTable counted}
        light (fn [block-only?] ((seen-light/light-fn #js {:perception {:st st}} block-only?) 3 0 0))]
    (.set (store/store-of st "overworld") (store/section-key 0 0 0) sec)
    (.fill (.-ids sec) 1)
    (is (= 0 (light true)) "block light only: none, and no sky")
    (is (zero? @floods) "nothing flooded")
    (is (= 15 (bit-shift-right (light false) 4)) "the sky light, flooded")
    (reset! floods 0)
    (aset (.-ids sec) (store/cell-index 2 0 0) (inc torch))
    (set! (.-wrote sec) 2)
    (set! (.-writes st) 2)
    (is (= 13 (light true)) "beside the torch's cell: 14 less a cell")
    (is (= 1 @floods))))

(defn lit-yard
  "A body in a yard walled 3 high (x and z -5..5, feet y 64) by night, a shut door west, four torches lighting its inside
  12, ground blocks more, after looking round from where it stands: every 30 degrees, up, level and down. From the wall
  top first when over? (it sees the ground past the wall)."
  [ground over?]
  (let [inside (for [x (range -4 5) y [64 65 66] z (range -4 5)] [x y z])
        ring (into {} (for [x (range -5 6) z (range -5 6) y [64 65 66]
                            :when (or (= 5 (js/Math.abs x)) (= 5 (js/Math.abs z)))]
                        [(str x "," y "," z) "cobblestone"]))
        p (seeing {:time midnight :light-default [15 0] :light (zipmap inside (repeat [15 12]))
                   :blocks (merge (tu/box -12 63 -12 12 63 12 "stone") ground ring
                                  {"-5,64,0" "oak_door" "-5,65,0" "oak_door"
                                   "2,64,2" "torch" "-2,64,2" "torch" "2,64,-2" "torch" "-2,64,-2" "torch"})
                   :states {"-5,64,0" {:open false} "-5,65,0" {:open false}}})
        look! #(doseq [yaw (range 0 360 30) pitch [-80 -45 -20 0 20 45 80]]
                 (swap! (fake/state p) assoc :yaw yaw :pitch pitch)
                 (perception/pass! (aget p "perception")))
        feet (get-in @(fake/state p) [:self :pos])]
    (when over?
      (swap! (fake/state p) assoc-in [:self :pos] [0.5 67 -4.5])
      (look!)
      (swap! (fake/state p) assoc-in [:self :pos] feet))
    (look!)
    p))

(defn stay
  "[stay-hall's verdict, night-chance] of the lit yard's centre."
  [p]
  (let [at {:x 0 :y 64 :z 0}]
    [(sh-inside/stay-hall p at (look/sky-subtract-at 18000 0 0)) (spawn-light/night-chance p at)]))

(deftest a-lit-yard-walled-3-high-is-entered-over-the-rim-only-at-the-chance-the-unseen-ground-outside-is-that-high
  ;; a walker may drop 3 from the wall's rim into the yard, so the rim is in the flood, and a cell past it stands on
  ;; ground the body has not seen: the old flood took that ground as solid and the yard open to a dark hall for sure
  (let [p (lit-yard {} false)
        [{:keys [hall edge]} night] (stay p)]
    (is (= :sky hall) "the rim is open to the unseen night beyond")
    (is (< 0 edge 0.5) "ground three over the yard's floor is likelier air than not")
    (is (= (* (cost/hall-chance :sky edge) (cost/spawn-chance [0 64 0])) night) "priced at that chance, the torches' light kept")))

(deftest a-lit-yard-whose-seen-ground-outside-is-level-with-its-rim-is-open-to-the-dark
  (let [ground (into {} (remove (fn [[k _]] (let [[x _ z] (map js/parseInt (.split k ","))] (and (< (js/Math.abs x) 5) (< (js/Math.abs z) 5)))))
                     (tu/box -12 64 -12 12 66 12 "stone"))
        [{:keys [hall edge]} night] (stay (lit-yard ground true))]
    (is (= [:sky 1] [hall edge]) "a walker on the seen ground steps onto the rim and drops in")
    (is (= (* (cost/hall-chance :sky) (cost/spawn-chance [0 64 0])) night))))

;; The yard's flood meets guessed ground past the rim at chances under 1. Would fail on the old code by reading: it walked
;; depth first whatever the chance, so a cell first reached by a guessed floor was walked again each time a likelier way
;; reached it, and every guessed cell was walked before the flood ended.
(deftest the-yard-s-flood-walks-each-cell-once-at-its-likeliest-chance-and-ends-at-the-likeliest-edge
  (let [p (lit-yard {} false)
        walked (atom {})
        edge? reach-room/unseen-edge?]
    (with-redefs [reach-room/unseen-edge? (fn ([p kind-at c] (edge? p kind-at c))
                                                ([p kind-at c unseen?]
                                                 (swap! walked update [(aget c 0) (aget c 1) (aget c 2)] (fnil inc 0))
                                                 (edge? p kind-at c unseen?)))]
      (let [{:keys [hall edge]} (sh-inside/stay-hall p {:x 0 :y 64 :z 0} (look/sky-subtract-at 18000 0 0))]
        (is (= :sky hall))
        (is (< 0 edge 0.5))))
    (is (seq @walked))
    (is (= #{1} (set (vals @walked))) "no cell walked twice")))

(deftest unseen-key-is-unique-for-a-bigger-table
  (let [qn 5 dn 20 dyn 12
        keys (for [y [-3 0 1 64] adj (range 3) qb (range qn) lit? [true false] di (range dn) dyb (range dyn)]
               (base-rates/unseen-key qn dn dyn y adj qb lit? di dyb))]
    (is (= (count keys) (count (set keys))) "a table with a 5th q bucket, 20 distances and 12 dy buckets keeps every bucket its own key")))
