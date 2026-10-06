(ns engine.sensing-walls-test
  "Entities behind a wall: items are left alone (only visible ones count); hostiles count when known (seen or heard)."
  (:require [cljs.test :refer [deftest is]]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [jobs.combat.attack :as attack]
            [jobs.debug.notify :as notify]
            [jobs.farm.compost :as compost]
            [jobs.items.give :as give]
            [jobs.lib.combat :as combat]
            [jobs.storage.make-room :as make-room]))

(def wall (into {} (for [x [2 3] y [64 65 66]] [(str x "," y ",0") "stone"])))
(def behind {:x 5 :y 64 :z 0})
(def open {:x 0 :y 64 :z 3})

(defn item [id name pos] {:id id :name name :kind "item" :item {:name name :count 1} :pos pos})

(defn world [& entities] (fake/create {:blocks wall :entities (vec entities)}))

(deftest items-behind-a-wall-are-not-counted
  (let [p (world (item 1 "bone_meal" behind) (item 2 "bone_meal" open))]
    (is (= [2] (mapv :id (make-room/ground-items {:primitives p} 20))))
    (is (= [2] (mapv :id (give/drops p "bone_meal" 20))))
    (is (nil? (compost/meal-near {:primitives (world (item 1 "bone_meal" behind))} {:x 5 :y 64 :z 1})))))

(defn stub
  "Primitives whose entities() lists raw and whose knownMobs() lists known."
  [raw known]
  #js {:entities (fn [_] (clj->js raw))
       :knownMobs (fn [] (clj->js known))
       :self (fn [] #js {:username "me" :pos #js {:x 0 :y 64 :z 0}})})

(defn zombie [id & {:as m}] (merge {:id id :name "zombie" :kind "hostile" :pos {:x 5 :y 64 :z 0} :distance 5 :visible false} m))

(deftest hostiles-are-the-known-ones
  (let [silent (zombie 1)
        heard (zombie 2 :heard true)
        p (stub [silent heard] [heard])]
    (is (= [2] (mapv #(.-id %) (combat/hostiles p 8))))
    (is (= [2] (mapv #(.-id %) (combat/sensed p {:radius 8}))))
    (is (= [] (mapv #(.-id %) (combat/hostiles (stub [silent] []) 8))))
    (is (re-find #"hostiles 0" (notify/snapshot (doto (stub [silent] []) (aset "self" (fn [] #js {:pos #js {:x 0 :y 64 :z 0}}))))))))

(deftest sensed-skips-an-unknown-mob-behind-a-wall
  (let [p (stub [(zombie 1) {:id 3 :name "cow" :kind "passive" :pos {:x 1 :y 64 :z 0} :distance 1}] [])]
    (is (= [3] (mapv #(.-id %) (combat/sensed p {:radius 8}))))))

(deftest a-player-behind-a-wall-is-still-sensed
  (let [alex {:id 9 :name "Alex" :username "Alex" :kind "player" :pos behind}
        p (world alex)]
    (is (= false (.-visible (first (array-seq (.entities p #js {:kind "player"}))))) "the fake marks the player hidden")
    (is (= [9] (mapv #(.-id %) (combat/sensed p {:radius 10}))) "the player stays")
    (with-redefs [ctx/mem (constantly {})]
      (is (= [9] (mapv #(.-id %) (attack/present {:primitives p :args {:targets ["Alex"] :radius 10}}))) "attack finds the player by name"))))
