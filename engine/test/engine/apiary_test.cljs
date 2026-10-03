(ns engine.apiary-test
  "engine.jobs.apiary: the smoke and fire rules."
  (:require [cljs.test :refer [deftest is are]]
            [engine.jobs.apiary :as apiary]))

(defn block-at-from
  "A block-at over {\"x,y,z\" {:name n :lit bool}} cells; absent cells are air."
  [cells]
  (fn [{:keys [x y z]}]
    (let [cell (get cells (str x "," y "," z) {:name "air"})]
      (when-not (= :unloaded cell)
        #js {:name (:name cell) :properties (clj->js (select-keys cell [:lit]))}))))

(deftest smoke-source-follows-the-vanilla-rule
  (let [hive-pos {:x 0 :y 10 :z 0}
        fire {:name "campfire" :lit true}
        cold {:name "campfire" :lit false}]
    (are [cells expected] (= expected (apiary/smoke-source (block-at-from cells) hive-pos))
      {"0,9,0" fire} {:x 0 :y 9 :z 0}
      {"0,5,0" fire} {:x 0 :y 5 :z 0}
      {"0,4,0" fire} nil
      {"0,9,0" cold} nil
      {"0,9,0" {:name "stone"} "0,8,0" fire} {:x 0 :y 8 :z 0}
      {"0,9,0" {:name "white_carpet"} "0,8,0" fire} {:x 0 :y 8 :z 0}
      {"0,9,0" {:name "stone"} "0,7,0" fire} nil
      {"0,9,0" {:name "water"} "0,8,0" fire} {:x 0 :y 8 :z 0}
      {"0,9,0" :unloaded "0,8,0" fire} nil
      {} nil)))

(deftest open-fire-is-a-lit-fire-nothing-covers
  (let [fire {:name "campfire" :lit true}]
    (are [cells expected] (= expected (apiary/open-fire? (block-at-from cells) {:x 0 :y 5 :z 0}))
      {"0,5,0" fire} true
      {"0,5,0" fire "0,6,0" {:name "white_carpet"}} false
      {"0,5,0" fire "0,6,0" {:name "moss_carpet"}} true
      {"0,5,0" fire "0,6,0" {:name "stone"}} false
      {"0,5,0" fire "0,6,0" :unloaded} true)))

(defn blk [n] #js {:name n})

(deftest solid-counts-what-cannot-be-read-as-ground
  (are [b expected] (= expected (apiary/solid? b))
    nil true
    (blk "stone") true
    (blk "lava") true
    (blk "air") false
    (blk "water") false
    (blk "short_grass") false))

(def fire {:name "campfire" :lit true})
(def stone {:name "stone"})
(def fire-pos {:x 0 :y 5 :z 0})
(def ring {"1,5,0" stone "-1,5,0" stone "0,5,1" stone "0,5,-1" stone})

(deftest walled-needs-all-four-sides-solid
  (are [cells expected] (= expected (apiary/walled? (block-at-from cells) fire-pos))
    ring true
    (dissoc ring "1,5,0") false
    (assoc ring "1,5,0" {:name "water"}) false
    (assoc ring "1,5,0" :unloaded) true
    {} false))

(deftest raised-is-a-fire-with-a-side-open
  (are [cells expected] (= expected (apiary/raised? (block-at-from (assoc cells "0,5,0" fire)) fire-pos))
    ring false
    (dissoc ring "0,5,1") true
    {} true))

(def below-ring {"1,4,0" stone "-1,4,0" stone "0,4,1" stone "0,4,-1" stone})

(deftest sinkable-needs-walled-real-ground-below
  (are [cells expected] (= expected (apiary/sinkable? (block-at-from cells) fire-pos))
    (assoc below-ring "0,4,0" stone) true
    (assoc below-ring "0,4,0" {:name "bedrock"}) false
    (assoc below-ring "0,4,0" {:name "lava"}) false
    (assoc below-ring "0,4,0" {:name "air"}) false
    (assoc below-ring "0,4,0" :unloaded) false
    (assoc (dissoc below-ring "1,4,0") "0,4,0" stone) false))

(deftest carpet-names
  (are [n expected] (= expected (apiary/carpet? n))
    "white_carpet" true
    "light_blue_carpet" true
    "moss_carpet" false
    "carpet" false
    "stone" false
    "white_carpet_x" false))

(deftest what-is-carried
  (let [inv (fn [& ns] (mapv (fn [n] {:name n :count 1}) ns))]
    (are [f args expected] (= expected (apply f args))
      apiary/carpet-in [(inv "stick" "red_carpet" "white_carpet")] "red_carpet"
      apiary/carpet-in [(inv "moss_carpet" "stick")] nil
      apiary/carpet-in [[]] nil
      apiary/campfire-in [(inv "campfire" "soul_campfire") "soul_campfire"] "soul_campfire"
      apiary/campfire-in [(inv "soul_campfire") "campfire"] "soul_campfire"
      apiary/campfire-in [(inv "stick") "campfire"] nil)))

(deftest needs-reads-the-fire-cell-and-surroundings
  (let [full (merge ring below-ring {"0,5,0" fire "0,4,0" stone})]
    (are [cells expected] (= expected (apiary/needs (block-at-from cells) fire-pos))
      full #{:carpet}
      (assoc full "0,6,0" {:name "white_carpet"}) #{}
      (dissoc full "1,5,0") #{:sink :carpet}
      (assoc (dissoc full "1,5,0") "0,6,0" {:name "white_carpet"}) #{:sink}
      (assoc (dissoc full "1,5,0" "1,4,0") "0,6,0" {:name "white_carpet"}) #{})))
