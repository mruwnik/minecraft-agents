(ns engine.look-test
  "jobs.lib.look sight helpers over a real perception on the fake world: nothing is sensed through walls."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.explore.look :as explore-look]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]))

(defn seeing
  "Fake primitives over world with a perception (the body's rules, a shorter radius), one sight pass done."
  [world]
  (let [p (tu/fake-on-floor world)
        wrapped (perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))]
    (.setOwner p "t1")
    (perception/pass! (aget wrapped "perception"))
    wrapped))

(def wall (tu/box 3 64 -3 3 66 3 "stone"))

(defn names-of [hits] (mapv :name hits))

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
  "1 or 0: dark-fn's call on cell x y z of a body that looks round a walled-off room at time t, with the light
  [sky block] given for the cell."
  [t cell light]
  (let [p (seeing {:time t :blocks wall})]
    (when light (swap! (fake/state p) assoc :light {cell light}))
    ((:at (look/dark-fn p)) (first cell) (second cell) (nth cell 2))))

(deftest dark-fn-needs-a-perception
  (is (nil? (look/dark-fn (tu/fake {})))))

(deftest an-unseen-cell-is-dark-at-night-only
  (is (= 1 (dark-of midnight [6 64 0] nil)))
  (is (= 0 (dark-of day [6 64 0] nil))))

(defn sealed
  "A body that has looked round the walled-off room by day, at time t, every cell at [sky block] light (sky 0: sealed
  from the sky), the cells of lights at their own."
  [t light lights]
  (let [p (seeing {:time day :blocks wall})]
    (swap! (fake/state p) assoc :time t :light-default light :light lights)
    p))

(deftest an-unseen-cell-in-a-dark-sealed-place-is-dark-by-day
  (let [p (sealed day [0 0] {})]
    (is (= 1 ((:at (look/dark-fn p)) 6 64 0)))
    (is (false? (sh-sense/lit-here? p {:x 6 :y 64 :z 0})) "the old guess read the open sky by day")))

(deftest an-unseen-cell-in-a-block-lit-sealed-hall-is-lit-at-night
  (let [p (sealed midnight [0 14] {})]
    (is (= 0 ((:at (look/dark-fn p)) 6 64 0)) "the old guess read every unseen cell dark at night")
    (is (true? (sh-sense/lit-here? p {:x 6 :y 64 :z 0})))))

(deftest seen-block-light-lights-an-unseen-cell-less-one-a-block
  (let [p (sealed midnight [0 0] {[0 64 3] [0 5]})]
    (is (true? (look/unseen-lit? p {:x 0 :y 64 :z -1})) "5 less 4 blocks, behind the body")
    (is (false? (look/unseen-lit? p {:x 0 :y 64 :z -2})) "5 less 5 blocks")))

(deftest an-unseen-cell-under-open-sky-is-lit-by-day-only
  (is (true? (look/unseen-lit? (seeing {:time day}) {:x 40 :y 64 :z 0})) "far past sight, where the seen ground is under open sky")
  (is (false? (look/unseen-lit? (seeing {:time midnight}) {:x 40 :y 64 :z 0})))
  (is (true? (look/unseen-lit? (tu/fake {}) {:x 40 :y 64 :z 0})) "no perception: the open sky"))

(deftest a-seen-cell-is-lit-by-block-light-or-by-day-sky
  (is (= 0 (dark-of midnight [0 64 5] [0 1])) "a torch's light 1 at night")
  (is (= 1 (dark-of midnight [0 64 5] [15 0])) "open sky at night")
  (is (= 0 (dark-of day [0 64 5] [15 0])) "open sky by day")
  (is (= 1 (dark-of day [0 64 5] [0 0])) "a seen cave cell is dark by day"))

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
