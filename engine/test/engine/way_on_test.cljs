(ns engine.way-on-test
  "How often the seen ways near a body go on (jobs.lib.walk.way-on/way-counts), the counts the one unseen price reads
  (jobs.lib.cost/way-on-chance, unseen-share)."
  (:require [cljs.test :refer [deftest is]]
            [engine.path.blocks :as blocks]
            [engine.perception.store :as store]
            [jobs.lib.cost :as cost]
            [jobs.lib.walk.way-on :as way-on]))

(defn known-of
  "A block memory ((fn [cx sy cz]) ids + 1) in which every cell of the box [x0 y0 z0 x1 y1 z1] is seen: the name blocks
  ({[x y z] name}) gives it, else solid's (stone or air); nothing else seen."
  [solid? blocks [x0 y0 z0 x1 y1 z1]]
  (let [t (blocks/default-state-table)
        id #(inc (first (blocks/state-ids t %)))
        m (js/Map.)]
    (doseq [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))]
      (let [k (store/section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4))
            ^js ids (or (.get m k) (let [a (js/Uint16Array. 4096)] (.set m k a) a))]
        (aset ids (store/cell-index x y z) (id (get blocks [x y z] (if (solid? [x y z]) "stone" "air"))))))
    (fn [cx sy cz] (.get m (store/section-key cx sy cz)))))

(def seen-box [-8 60 -8 8 68 8])

(def open-ground
  "A stone floor at y 63 under air, seen 8 round the body; past it nothing seen (the way on runs into the unseen)."
  (known-of (fn [[_ y _]] (< y 64)) {} seen-box))

(def corridors
  "Rock seen 8 round the body, a corridor along x at z 0 (y 64 and 65) that runs on out of the seen box, with three dead-end stubs off it, each two long."
  (let [open (into #{} (concat (for [x (range -8 9) y [64 65]] [x y 0])
                               (for [x [-4 0 4] z [1 2] y [64 65]] [x y z])))]
    (known-of (fn [cell] (not (open cell))) {} seen-box)))

(def open-corridor
  "A corridor along x at z 0 (y 64 and 65) in rock, seen 8 round the body, that runs on out of the seen box at both ends."
  (known-of (fn [[_ y z]] (not (and (= z 0) (#{64 65} y)))) {} seen-box))

(def pocket
  "Rock seen 8 round the body, a corridor of two cells along x (x 0 and 1, y 64 and 65): shut at both ends."
  (known-of (fn [[x y z]] (not (and (= z 0) (#{0 1} x) (#{64 65} y)))) {} seen-box))

(defn carved
  "Rock seen 8 round the body with the cells open? holds air."
  [open?]
  (known-of (fn [cell] (not (open? cell))) {} seen-box))

(def shut-corridor
  "A corridor of five cells along x from the body (x 0 to 4, y 64 and 65), shut at both ends."
  (carved (fn [[x y z]] (and (= z 0) (<= 0 x 4) (#{64 65} y)))))

(def closed-room
  "A closed room 5 by 5 round the body (x and z -2 to 2, y 64 and 65)."
  (carved (fn [[x y z]] (and (<= -2 x 2) (<= -2 z 2) (#{64 65} y)))))

(defn dead-end [known] (- 1 (cost/way-on-chance (way-on/way-counts known [0 64 0]))))

(deftest the-ways-out-of-open-ground-and-a-corridor-go-on-and-a-pocket-ends
  (is (< (dead-end open-ground) 0.01) "open ground: a way on seldom ends")
  (is (= {:k 2 :n 2} (way-on/way-counts open-corridor [0 64 0])) "a 1-wide corridor open at both ends: two ways, both go on (the walls beside it are no ways)")
  (is (< (dead-end open-corridor) (dead-end corridors) (dead-end pocket)) "a corridor with three stubs and shut ends: some ways end")
  (is (> (dead-end pocket) 0.5) "a shut pocket: the way ends")
  (is (> (dead-end shut-corridor) 0.5) "a shut corridor of five: no cell of it leads out")
  (is (> (dead-end closed-room) 0.5) "a closed room: no cell of it leads out"))

(deftest a-shut-corridor-is-one-dead-end-however-long
  (let [shut (fn [n] (carved (fn [[x y z]] (and (= z 0) (<= 0 x n) (#{64 65} y)))))]
    (is (= {:k 0 :n 1} (way-on/way-counts (shut 2) [0 64 0]) (way-on/way-counts (shut 6) [0 64 0]))
        "its cells share one fate: seeing more of it is not more dead ends")))

(deftest a-cell-not-seen-counts-neither-way
  (is (= {:k 0 :n 0} (way-on/way-counts (constantly nil) [0 64 0])) "nothing seen: no count, the prior stands")
  (is (= 0.5 (cost/way-on-chance (way-on/way-counts (constantly nil) [0 64 0])))))

(deftest an-unseen-way-costs-more-where-the-seen-ways-end
  (let [kit {:weapon "stone_sword" :equipment nil}
        share #(cost/unseen-share (way-on/way-counts % [0 64 0]) kit)]
    (is (> (share corridors) (share open-ground)) "the same unseen corridor ahead: dearer among dead ends than past open ground")))


(deftest the-ground-past-the-unseen-cells-by-the-feet-is-counted
  (let [by-feet (set (for [x (range -2 3) z (range -2 3) y [63 64 65] :when (not= [x z] [0 0])] [x y z]))
        known (fn [cx sy cz]
                (when-let [^js ids (open-ground cx sy cz)]
                  (let [a (js/Uint16Array.from ids)]
                    (doseq [[x y z] by-feet
                            :when (= [cx sy cz] (mapv #(bit-shift-right % 4) [x y z]))]
                      (aset a (store/cell-index x y z) 0))
                    a)))]
    (is (pos? (:n (way-on/way-counts known [0 64 0]))) "a body that has not seen its own footing still counts the ground ahead")
    (is (< (dead-end known) 0.05))))

(deftest seen-ground-past-a-wide-unseen-gap-is-not-counted
  (let [gap (set (for [x (range -8 9) z (range -8 9) y [63 64 65] :when (and (<= (max (js/Math.abs x) (js/Math.abs z)) 5) (not= [x z] [0 0]))] [x y z]))
        known (fn [cx sy cz]
                (when-let [^js ids (open-ground cx sy cz)]
                  (let [a (js/Uint16Array.from ids)]
                    (doseq [[x y z] gap :when (= [cx sy cz] (mapv #(bit-shift-right % 4) [x y z]))]
                      (aset a (store/cell-index x y z) 0))
                    a)))]
    (is (= {:k 0 :n 0} (way-on/way-counts known [0 64 0])) "seen cells only past an unseen gap: nothing counted, the prior stands")))

(deftest a-long-seen-strip-with-an-unseen-side-is-read-only-so-far
  (let [t (blocks/default-state-table)
        stone (inc (first (blocks/state-ids t "stone")))
        air (inc (first (blocks/state-ids t "air")))
        sections (js/Map.)
        id-at (fn [x y z] (cond (< y 64) stone
                                (zero? z) air
                                (== y 64) stone
                                :else 0))
        known (fn [cx sy cz]
                (let [k (str cx "," sy "," cz)]
                  (or (.get sections k)
                      (when (and (<= -1 cz 0) (<= 3 sy 4) (<= -1 cx 20000))
                        (let [a (js/Uint16Array. 4096)]
                          (dotimes [i 16] (dotimes [j 16] (dotimes [l 16]
                            (let [z (+ (* cz 16) l)]
                              (when (<= -1 z 1) (aset a (store/cell-index (+ (* cx 16) i) (+ (* sy 16) j) z) (id-at 0 (+ (* sy 16) j) z)))))))
                          (.set sections k a)
                          a)))))]
    (is (= {:k 0 :n 0} (way-on/way-counts known [0 64 0])) "every stand of the strip has an unseen side above its wall: none counted")
    (is (< (.-size sections) 1000) "and the strip is not walked to its far end")))

(defn zeroed
  "known with the cells unseen."
  [known cells]
  (fn [cx sy cz]
    (when-let [^js ids (known cx sy cz)]
      (let [a (js/Uint16Array.from ids)]
        (doseq [[x y z] cells :when (= [cx sy cz] (mapv #(bit-shift-right % 4) [x y z]))]
          (aset a (store/cell-index x y z) 0))
        a))))

(defn cells [& boxes]
  (set (for [[x0 y0 z0 x1 y1 z1] boxes x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [x y z])))

(deftest a-fork-keeps-its-dead-and-open-ways-apart
  (let [fork (carved (let [o (cells [0 64 0 8 65 0] [3 64 1 3 65 3])] #(contains? o %)))]
    (is (= {:k 2 :n 3} (way-on/way-counts fork [0 64 0])) "a shut branch and one running out of the seen box: two ways on of three")))

(deftest a-stand-with-no-way-back-keeps-its-two-forward-ways-apart
  (let [o (cells [0 65 0 0 65 0] [1 64 0 3 65 0] [1 64 1 1 65 8])
        ladder (into {} (for [y (range 61 65)] [[0 y 0] "ladder"]))
        known (known-of (fn [cell] (not (or (o cell) (contains? ladder cell)))) ladder seen-box)]
    (is (= {:k 2 :n 3} (way-on/way-counts known [0 64 0]))
        "the start is a ladder cell nobody stands on: the step off it has no way back, so its shut stub and open corridor both count")))
