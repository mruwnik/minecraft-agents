(ns engine.fenced-field-test
  "A field fenced all round is closed ground to walkers: no flood, threat field or danger crosses its fence, and
  a gate left open lets them in."
  (:require [cljs.test :refer [deftest is]]
            [engine.test-util :as tu]
            [jobs.lib.danger :as danger]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.threat-field :as field]))

(defn fence-ring
  "A ring of fence on stone at x0..x1, z0..z1 (the fake leaves no floor in a column it builds on)."
  [x0 z0 x1 z1]
  (into {} (for [x (range x0 (inc x1)) z (range z0 (inc z1)) :when (or (#{x0 x1} x) (#{z0 z1} z))
                 [y b] [[64 "oak_fence"] [63 "stone"]]]
             [(str x "," y "," z) b])))

(def body {:x 0.5 :y 64 :z 0.5})
(def shut-field (fence-ring -12 -12 12 12))
(def gated-field (assoc shut-field "12,64,0" "oak_fence_gate"))

(defn zombie [x] {:id 1 :name "zombie" :kind "hostile" :visible true :pos {:x (+ x 0.5) :y 64 :z 0.5}})

(defn world
  ([blocks mob] (world blocks mob false))
  ([blocks mob gate-open?]
   (tu/seeing-all (tu/fake-on-floor {:self {:pos body} :floor [-24 -24 24 24] :blocks blocks :entities [mob]
                                     :states {"12,64,0" {:open gate-open? :facing "east"}}}))))

(defn dangers [p] (mapv #(.-id %) (danger/dangers p 40 {} {})))
(defn field-at [p mob] (field/reach-fn (field/field p {:mob "zombie" :pos (:pos mob)})))

(deftest a-zombie-outside-a-fenced-field-is-no-danger-and-its-field-stops-at-the-fence
  (doseq [blocks [shut-field gated-field]
          :let [mob (zombie 16)
                p (world blocks mob)]]
    (is (= [] (dangers p)))
    (is (nil? ((field-at p mob) 0 64 0)) "its field holds no inside cell")
    (is (false? (reach/walkable-way? p (:pos mob) body)))
    (is (false? (:open? (sh-inside/spawn-region p {:x 0 :y 64 :z 0} 0))) "the flood of the field is closed")
    (is (some? (sh-inside/sealed-region p {:x 0 :y 64 :z 0})) "the field is a sealed region")))

(deftest the-same-field-with-its-gate-open-is-no-safe-ground
  (let [mob (zombie 16)
        p (world gated-field mob true)]
    (is (= [1] (dangers p)))
    (is (some? ((field-at p mob) 0 64 0)))
    (is (nil? (sh-inside/sealed-region p {:x 0 :y 64 :z 0})))))

(deftest a-zombie-inside-a-fenced-field-is-a-danger
  (let [mob (zombie 8)]
    (is (= [1] (dangers (world shut-field mob))))))

