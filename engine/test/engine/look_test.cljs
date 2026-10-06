(ns engine.look-test
  "jobs.lib.look sight helpers over a real perception on the fake world: nothing is sensed through walls."
  (:require [cljs.test :refer [deftest is async]]
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
