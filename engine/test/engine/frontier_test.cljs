(ns engine.frontier-test
  "jobs.lib.frontier over a real perception on the fake world: the block-at-a-time distance is the search's distance."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.perception.store :as store]
            [engine.test-util :as tu]
            [jobs.lib.cost.base-rates :as base-rates]
            [jobs.lib.frontier :as frontier]
            [jobs.lib.look :as look]
            [jobs.lib.walk.rock :as rock]))

(def cave
  "A seen slab east of the body with a window in it, a pillar and a roof: frontiers where reach cuts sight, through the
  window and round the slab, and unseen cells far from any behind the slab."
  (merge (tu/box 3 63 -6 5 70 6 "stone")
         (tu/box -3 63 -3 -3 66 -3 "stone")
         (tu/box -4 68 -4 1 68 4 "stone")))

(defn seeing [p]
  (let [per (perception/create (fake-raw/create p) {:radius 10 :ray-deg 2 :now (constantly 1000000) :rand (constantly 0.5)})
        w (perception/wrap p per)]
    (perception/pass! per)
    w))

(defn unseen-cells
  "Every third cell round the cave the body has not seen."
  [w]
  (let [st (:st (aget w "perception"))]
    (take-nth 3 (for [x (range -8 14) y (range 60 74) z (range -8 9)
                      :when (neg? (store/remembered-id st x y z))]
                  [x y z]))))

(defn searched
  "The oracle: the Chebyshev distance from cell [x y z] to the nearest remembered frontier cell (open, with a face neighbour never seen), searched out ring by ring; d-max when none lies nearer."
  [w [x y z] d-max]
  (let [st (:st (aget w "perception"))
        ^js open (rock/open-ids)
        id #(inc (store/remembered-id st %1 %2 %3))
        frontier? (fn [cx cy cz]
                    (let [v (id cx cy cz)]
                      (and (pos? v) (== 1 (aget open (dec v)))
                           (some #(zero? (apply id %)) [[(inc cx) cy cz] [(dec cx) cy cz] [cx (inc cy) cz] [cx (dec cy) cz] [cx cy (inc cz)] [cx cy (dec cz)]]))))]
    (or (some (fn [r]
                (when (some (fn [[dx dy dz]] (frontier? (+ x dx) (+ y dy) (+ z dz)))
                            (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))
                                  :when (== r (max (js/Math.abs dx) (js/Math.abs dy) (js/Math.abs dz)))]
                              [dx dy dz]))
                  r))
              (range 1 d-max))
        d-max)))

(defn distances [w f] (mapv #(f w %) (unseen-cells w)))

(deftest the-block-distance-of-an-unseen-cell-is-the-searched-distance
  (let [p (tu/fake-on-floor {:blocks (assoc cave "3,65,0" "air")})
        w (seeing p)
        by-search #(searched %1 %2 base-rates/unseen-d-max)]
    (is (= (distances w by-search) (distances w frontier/distance)))
    (is (= (set (range 1 (inc base-rates/unseen-d-max))) (set (distances w frontier/distance))) "every distance, the cap too")
    (fake/remove-block! p {:x 3 :y 66 :z 0})
    (perception/pass! (aget w "perception"))
    (is (= (distances w by-search) (distances w frontier/distance)) "a memory write: worked out again")))

(deftest a-weighing-of-a-block-of-cells-works-the-block-out-once
  (let [w (seeing (tu/fake-on-floor {:blocks cave}))
        built (atom 0)
        f frontier/block-field
        solid-at (look/unseen-solid-at-fn w)]
    (with-redefs [frontier/block-field (fn [& args] (swap! built inc) (apply f args))]
      (doseq [x (range 8 16) y (range 64 72) z (range 0 8)] (solid-at [x y z])))
    (is (= 1 @built))))

(deftest a-write-to-a-far-section-keeps-the-block-and-one-in-its-margin-works-it-out-again
  (let [w (seeing (tu/fake-on-floor {:blocks cave}))
        ^js st (:st (aget w "perception"))
        built (atom 0)
        f frontier/block-field
        price #(doseq [x (range 8 16) y (range 64 72) z (range 0 8)] (frontier/distance w [x y z]))
        write! (fn [x] (store/record! st (store/store-of st (.-dim st)) x 65 0 1 1000000))]
    (with-redefs [frontier/block-field (fn [& args] (swap! built inc) (apply f args))]
      (price)
      (write! 400)
      (price)
      (is (= 1 @built) "a section far off: kept")
      (write! 12)
      (price)
      (is (= 2 @built) "a section the block reads: worked out again"))))

(deftest the-solid-verdict-is-the-chance-over-a-half-from-the-block-distance
  (let [w (seeing (tu/fake-on-floor {:blocks (assoc cave "3,65,0" "air")}))
        chance (look/unseen-solid-at-fn w)
        verdict (look/unseen-solid?-fn w)
        cells (unseen-cells w)]
    (is (= (mapv #(> (chance %) 0.5) cells) (mapv verdict cells)))
    (is (< 1 (count (distinct (map verdict cells)))) "both verdicts occur")))

(deftest an-unseen-cell-is-priced-by-its-nearest-frontier-s-light-not-the-feet-s
  (let [w (seeing (tu/fake-on-floor {:blocks (assoc cave "3,65,0" "air")}))
        fy (js/Math.floor (.-y (.-pos (.self w))))
        q (look/near-share w)
        near (remove #(= base-rates/unseen-d-max (frontier/distance w %)) (unseen-cells w))
        chance (fn [] (mapv (look/unseen-solid-at-fn w) near))
        priced (fn [lit?] (mapv (fn [[_ y _ :as c]] (base-rates/unseen-solid fy (- y fy) (frontier/distance w c) lit? q)) near))]
    (with-redefs [look/feet-lit? (constantly true)
                  look/remembered-light-fn (fn [_ _] (constantly 0))]
      (is (seq near))
      (is (= (priced false) (chance)) "lit feet, dark frontier: the dark table")
      (is (not= (priced true) (chance))))))
