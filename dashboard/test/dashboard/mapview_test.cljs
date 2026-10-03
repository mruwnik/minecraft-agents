(ns dashboard.mapview-test
  (:require [cljs.test :refer [deftest is are testing]]
            [dashboard.mapview :as m]))

(def claude-pos {:x 60.5 :y 64 :z -151.6})
(defn seer [name players at]
  {:name name :up true :at at :state {:pos claude-pos :players players}})
(def agent-names ["Claude" "Chani"])

(deftest human-sightings-freshest-per-human
  (is (= [{:name "Alex" :x 5 :y 66 :z 6 :seenBy "Chani" :at 2000}
          {:name "Steve" :x 3 :y 66 :z 4 :seenBy "Chani" :at 2000}]
         (m/human-sightings
          [(seer "Claude" {"Steve" {:x 1 :y 65 :z 2} "Chani" {:x 9 :y 65 :z 9}} 1000)
           (seer "Chani" {"Steve" {:x 3 :y 66 :z 4} "Alex" {:x 5 :y 66 :z 6}} 2000)]
          agent-names))))

(deftest human-sightings-nobody
  (doseq [[why bodies]
          [["an agent seen by an agent" [(seer "Claude" {"Chani" {:x 1 :y 65 :z 2}} 1000)]]
           ["nobody lists anyone" [(seer "Claude" {} 1000)]]
           ["out of sight" [(seer "Claude" {"Steve" "out of sight"} 1000)]]
           ["the only body is down" [{:name "Claude" :up false :at 1 :state nil}]]
           ["no bodies" []]]]
    (testing why
      (is (= [] (m/human-sightings bodies agent-names))))))

(def places [{:name "hut" :x 116 :y 69 :z -141}])
(def zones [{:name "pen" :x1 100 :y1 60 :z1 -150 :x2 110 :y2 70 :z2 -140}])

(deftest map-points-collects-everything
  (is (= [{:x 60.5 :z -151.6} {:x 116 :z -141} {:x 100 :z -150} {:x 110 :z -140} {:x 0 :z 0}]
         (m/map-points [(seer "Claude" {} 1)] places zones [{:name "Steve" :x 0 :y 64 :z 0}] []))))

(deftest map-points-down-body-and-village-bounds
  (is (= [] (m/map-points [{:name "Perrin" :up false :state nil}] [] [] [] [])))
  (is (= [{:x 10 :z 20} {:x 14 :z 27}]
         (m/map-points [] [{:name "v" :x 10 :z 20 :village {:bounds {:x 10 :z 20 :width 4 :depth 7}}}] [] [] []))))

(deftest map-points-include-plan-boxes
  (is (= [{:x 1 :z 2} {:x 5 :z 9}] (m/map-points [] [] [] [] [{:x1 1 :z1 2 :x2 5 :z2 9} nil]))))

(deftest world-bounds-cases
  (is (= {:min-x -5 :max-x 15 :min-z -9 :max-z 5} (m/world-bounds [{:x 0 :z 0} {:x 10 :z -4}] 5)))
  (is (nil? (m/world-bounds [] 5)))
  (is (= {:min-x -16 :max-x 16 :min-z -16 :max-z 16} (m/world-bounds [{:x 0 :z 0}] nil))))

(deftest fit-view-cases
  (is (= {:scale 2 :origin-x 0 :origin-z -25} (m/fit-view {:min-x 0 :max-x 100 :min-z 0 :max-z 50} 200 200)))
  (let [v (m/fit-view {:min-x 5 :max-x 5 :min-z 5 :max-z 5} 100 100)]
    (is (and (js/Number.isFinite (:scale v)) (pos? (:scale v)))))
  (is (nil? (m/fit-view nil 200 200))))

(deftest project-corners
  (let [view (m/fit-view {:min-x 0 :max-x 100 :min-z 0 :max-z 50} 200 200)]
    (doseq [[what x z expected] [["north-west" 0 0 {:px 0 :py 50}]
                                 ["south-east" 100 50 {:px 200 :py 150}]
                                 ["east is right, south is down" 50 25 {:px 100 :py 100}]]]
      (testing what
        (is (= expected (m/project view x z)))))))

(deftest zone-rect-back-to-front
  (let [view (m/fit-view {:min-x 0 :max-x 100 :min-z 0 :max-z 100} 100 100)]
    (is (= {:px 20 :py 10 :w 20 :h 50} (m/zone-rect view {:x1 40 :z1 60 :x2 20 :z2 10})))))

(defn box [text px py] {:text text :px px :py py :w 10 :h 10})

(deftest fit-labels-cases
  (doseq [[why boxes expected]
          [["clear labels all kept" [(box "a" 0 0) (box "b" 20 0) (box "c" 0 20)] ["a" "b" "c"]]
           ["first offered wins" [(box "body" 0 0) (box "place" 5 5) (box "far" 40 40)] ["body" "far"]]
           ["touching edges do not overlap" [(box "a" 0 0) (box "b" 10 0)] ["a" "b"]]
           ["nothing" [] []]]]
    (testing why
      (is (= expected (mapv :text (m/fit-labels boxes)))))))

(def up-body {:name "A" :up true :state {:pos {:x 5 :y 64 :z 6}}})
(def down-body {:name "B" :up false :state nil})
(def hut {:name "hut" :x 10 :z 20})
(def zone {:name "z" :x1 1 :z1 2 :x2 3 :z2 4})

(deftest empty-world-cases
  (doseq [[why world expected]
          [["nothing" {:bodies [] :places [] :zones []} true]
           ["a down body" {:bodies [down-body] :places [] :zones []} true]
           ["a body up" {:bodies [up-body] :places [] :zones []} false]
           ["a place" {:bodies [] :places [hut] :zones []} false]
           ["a zone" {:bodies [] :places [] :zones [zone]} false]]]
    (testing why
      (is (= expected (m/empty-world? world))))))

(deftest world-bounds-default-pad-grows-with-span
  (is (= {:min-x -16 :max-x 26 :min-z -16 :max-z 26} (m/world-bounds [{:x 0 :z 0} {:x 10 :z 10}])))
  (is (= {:min-x -40 :max-x 1040 :min-z -40 :max-z 40} (m/world-bounds [{:x 0 :z 0} {:x 1000 :z 0}]))))

(deftest trimmed-points
  (let [cluster (vec (for [i (range 18)] {:x i :z i}))
        far [{:x 5000 :z 5000} {:x -4000 :z -4000}]]
    (are [points trim expected] (= expected (count (m/trimmed-points points trim)))
      [] 0.1 0
      [{:x 1 :z 1} {:x 9000 :z 9000}] 0.1 2
      cluster 0.1 16
      (into cluster far) 0.1 16
      (into cluster far) 0 20)
    (is (every? #(< (:x %) 100) (m/trimmed-points (into cluster far) 0.1)))))
