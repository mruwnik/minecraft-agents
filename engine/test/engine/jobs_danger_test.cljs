(ns engine.jobs-danger-test
  "jobs.lib.cost/route-danger: the danger of walking a route past the mobs the body senses, after its armour."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.cost :as d]
            [jobs.lib.reach :as reach]
            [engine.game :as game]
            [engine.test-util :as tu]))

(def body {:x 0.5 :y 64 :z 0.5})

(def route
  "A straight walk east along z 0 from x 0 to x 30 on the floor."
  (mapv (fn [x] {:x x :y 64 :z 0}) (range 0 31)))

(defn mob [id name x z] {:id id :name name :kind "hostile" :pos {:x (+ x 0.5) :y 64 :z (+ z 0.5)}})

(def skeleton-wall
  "A closed stone box around the cell (15 64 8): a skeleton inside has no line of fire and no way out."
  (into {} (for [x (range 13 18) z (range 6 11) y [64 65 66 67]
                 :when (or (#{13 17} x) (#{6 10} z) (= y 67))]
             [(str x "," y "," z) "stone"])))

(defn world [& {:keys [blocks]}] (tu/fake {:self {:pos body} :floor [-10 -20 40 20] :blocks (or blocks {})}))

(def naked {})
(def iron-armour {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"}
                  :legs {:name "iron_leggings"} :feet {:name "iron_boots"}})

(defn route-danger [p route mobs equipment & opts] (apply d/route-danger game/default-version (reach/lookup p) route mobs equipment opts))

(defn danger [p mobs & opts] (:danger (apply route-danger p route mobs naked opts)))

(deftest no-mobs-no-danger
  (is (= 0 (danger (world) []))))

(deftest a-zombie-beside-the-route-is-a-danger
  (is (pos? (danger (world) [(mob 1 "zombie" 15 2)]))))

(deftest a-creeper-is-worse-than-a-zombie
  (is (> (danger (world) [(mob 1 "creeper" 15 2)]) (danger (world) [(mob 1 "zombie" 15 2)]))))

(deftest closer-is-worse-and-far-off-is-nothing
  (let [p (world)]
    (is (> (danger p [(mob 1 "zombie" 15 1)]) (danger p [(mob 1 "zombie" 15 8)]) 0))
    (is (= 0 (danger p [(mob 1 "zombie" 15 19)])))))

(deftest a-walled-off-skeleton-is-no-danger-and-an-open-one-is
  (is (= 0 (danger (world :blocks skeleton-wall) [(mob 1 "skeleton" 15 8)])))
  (is (pos? (danger (world) [(mob 1 "skeleton" 15 8)]))))

(deftest a-walled-off-zombie-is-no-danger
  (is (= 0 (danger (world :blocks skeleton-wall) [(mob 1 "zombie" 15 8)]))))

(deftest passive-mobs-are-no-danger
  (is (= 0 (danger (world) [{:id 1 :name "cow" :kind "passive" :pos {:x 15.5 :y 64 :z 1.5}}]))))

(deftest dangers-add-up-and-the-main-contributor-is-first
  (let [r (route-danger (world) route [(mob 1 "zombie" 15 2) (mob 2 "creeper" 20 2)] naked)]
    (is (= "creeper" (:name (first (:mobs r)))))
    (is (= (:danger r) (reduce + (map :danger (:mobs r)))))))

(deftest overrides-set-a-mob-s-threat
  (let [p (world)]
    (is (= 0 (danger p [(mob 1 "zombie" 15 2)] :overrides {"zombie" 0})))
    (is (> (danger p [(mob 1 "zombie" 15 2)] :overrides {"zombie" 100}) (danger p [(mob 1 "zombie" 15 2)])))
    (is (= (* 2 (danger p [(mob 1 "zombie" 15 2)])) (danger p [(mob 1 "zombie" 15 2)] :overrides {"zombie" {:times 2}})))))

(deftest armour-lowers-the-danger
  (let [p (world)]
    (are [m] (< (:danger (route-danger p route [m] iron-armour))
                (:danger (route-danger p route [m] naked)))
      (mob 1 "zombie" 15 2)
      (mob 1 "creeper" 15 2)
      (mob 1 "skeleton" 15 8))))

(defn with-enchant [ench]
  (update iron-armour :torso assoc :enchants [{:name ench :lvl 4}]))

(deftest blast-protection-lowers-the-creeper-and-projectile-protection-the-skeleton
  (let [p (world)
        dz (fn [m eq] (:danger (route-danger p route [m] eq)))
        creeper (mob 1 "creeper" 15 2)
        skeleton (mob 2 "skeleton" 15 8)]
    (is (< (dz creeper (with-enchant "blast_protection")) (dz creeper iron-armour)))
    (is (= (dz skeleton (with-enchant "blast_protection")) (dz skeleton iron-armour)) "blast does nothing for arrows")
    (is (< (dz skeleton (with-enchant "projectile_protection")) (dz skeleton iron-armour)))
    (is (= (dz creeper (with-enchant "projectile_protection")) (dz creeper iron-armour)))
    (is (< (dz skeleton (with-enchant "protection")) (dz skeleton iron-armour)) "protection helps against all")))

(deftest js-entities-are-read-too
  (let [p (world)]
    (is (= (danger p [(mob 1 "zombie" 15 2)])
           (danger p [#js {:id 1 :name "zombie" :kind "hostile" :pos #js {:x 15.5 :y 64 :z 2.5}}])))))

(deftest an-empty-route-is-no-danger
  (is (= 0 (:danger (route-danger (world) [] [(mob 1 "zombie" 1 1)] naked)))))

(deftest straight-route-stands-on-the-ground
  (let [blocks {"5,64,0" "stone"}
        r (d/straight-route (reach/lookup (world :blocks blocks)) {:x 0.5 :y 64 :z 0.5} {:x 10.5 :y 64 :z 0.5})]
    (is (= {:x 0 :y 64 :z 0} (first r)))
    (is (= {:x 10 :y 64 :z 0} (last r)))
    (is (= 65 (:y (first (filter #(= 5 (:x %)) r)))) "a step up onto the block")))
