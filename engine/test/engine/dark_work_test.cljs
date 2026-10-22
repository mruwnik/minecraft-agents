(ns engine.dark-work-test
  "The :dark-work trigger: a body with a job live on dark spawn ground under a roof, carrying a torch, in a cave."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [triggers.survival.dark-work :as dark-work]))

(def torch [{:name "torch" :count 4}])

(defn cave
  "A stone box of 61 blocks across (a cave past the spawn-free radius) with the air carved out at y 64-65, floor y 63."
  [{:keys [inventory lit?] :or {inventory torch}}]
  (let [air (for [x (range -30 31) z (range -2 3) y [64 65]] [x y z])
        key-of (fn [[x y z]] (str x "," y "," z))]
    (tu/fake {:time 18000
              :self {:pos {:x 0.5 :y 64 :z 0.5}}
              :blocks (apply dissoc (tu/box -31 62 -3 31 66 3 "stone") (map key-of air))
              :inventory inventory
              
              :light (into {} (for [c air] [c (if lit? [0 14] [0 0])])) :light-default [0 0]})))

(defn holds? [p live]
  (dark-work/dark-work p (mem/view (mem/open (tu/tmp-dir) {:now (constantly 1000000)})) {} nil live))

(deftest it-holds-for-a-working-body-with-a-torch-in-a-large-dark-cave
  (is (true? (holds? (cave {}) #{"j1"}))))

(deftest it-does-not-hold-without-a-job-a-torch-or-in-the-light
  (are [p live] (false? (holds? p live))
    (cave {}) #{}
    (cave {:inventory []}) #{"j1"}
    (cave {:lit? true}) #{"j1"}))
