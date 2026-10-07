(ns engine.look-test
  "jobs.lib.look sight helpers over a real perception on the fake world: nothing is sensed through walls."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.lib.look :as look]))

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

(deftest a-seen-cell-is-lit-by-block-light-or-by-day-sky
  (is (= 0 (dark-of midnight [0 64 5] [0 1])) "a torch's light 1 at night")
  (is (= 1 (dark-of midnight [0 64 5] [15 0])) "open sky at night")
  (is (= 0 (dark-of day [0 64 5] [15 0])) "open sky by day")
  (is (= 1 (dark-of day [0 64 5] [0 0])) "a seen cave cell is dark by day"))

(deftest dark-fn-says-whether-the-sky-is-dark
  (is (true? (:night? (look/dark-fn (seeing {:time midnight})))))
  (is (false? (:night? (look/dark-fn (seeing {:time day}))))))
