(ns world-test.arrow-shots-test
  (:require [cljs.test :refer [deftest is]]
            [engine.settings :as settings]
            [world-test.arrow-shots :as s]))

(defn pos [t x] {:k :pos :t t :gen "g" :x x :y 64 :z 0})
(defn decided [t mob decision] {:k :decided :t t :gen "g" :job "j1" :mob mob :decision decision :at {:x 0 :y 64 :z 0}})
(defn hurt [t x] {:k :hurt :t t :gen "g" :causes #{"skeleton"} :hits 1 :at {:x x :y 64 :z 0}})

(deftest a-flight-is-exposed-while-the-body-is-within-the-bow-range-of-where-the-skeleton-was-seen
  (let [[m] (s/meets [(pos 0 4) (decided 500 "skeleton" :flee) (hurt 3000 9) (pos 6000 14) (pos 9000 20) (hurt 9500 22)
                      {:k :ended :t 12000 :gen "g" :job "j1"}])]
    (is (= {:decision :flee :d0 4 :hits 2 :end :ended} (select-keys m [:decision :d0 :hits :end])) "the hurt out of range still counts, as the skeleton walks in")
    (is (< (js/Math.abs (- (:in-s m) (+ 2.5 3 1.5))) 1e-9) "from the decision's position: 2.5 s to the hurt, 3 s to 14 blocks, half of the 3 s to 20")))

(deftest a-run-that-meets-another-kind-is-no-lone-meet
  (is (empty? (s/meets [(pos 0 4) (decided 500 "skeleton" :flee) (decided 1000 "zombie" :flee) (pos 3000 9) {:k :ended :t 4000 :gen "g" :job "j1"}]))))

(deftest the-hit-chance-is-the-beta-posterior-mean-of-hits-over-shots
  (settings/with-settings {:jobs.lib.cost.threat/difficulty 1}
    (fn []
      (let [r (s/report [{:decision :fight :mob "skeleton" :d0 3 :in-s 30 :hits 4 :end :ended}
                         {:decision :fight :mob "skeleton" :d0 20 :in-s 30 :hits 9 :end :ended}])]
        (is (= {:meets 1 :shots 10 :hits 4 :fraction (/ (js/Math.round (* 1000 (/ 5 12))) 1000)}
               (select-keys (get-in r [:fight :in-range]) [:meets :shots :hits :fraction]))
            "10 shots in 30 s on easy, 4 hit; the one seen out of range is left out")))))
