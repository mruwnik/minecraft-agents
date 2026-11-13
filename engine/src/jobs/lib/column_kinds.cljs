(ns jobs.lib.column-kinds
  "How a body reads the cells of a column: the kind of a seen block, and the highest stand of a column."
  (:require [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.surface :as surface]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]))

(def top-y "The highest feet y a column goal looks for ground at (the head cell above is the build limit's last)." (- places/max-y 2))

(defn kind
  "A block as a stand test reads it: :unknown (nil: unseen or unloaded), :hazard, else reach/kind-of."
  [b]
  (cond (nil? b) :unknown
        (bs/harms-in-cell (.-name b)) :hazard
        :else (reach/kind-of b)))

(defn trunk-over?
  "Whether the cell above x y z is a seen log; reads the cell without noting it as an unseen read."
  [p x y z]
  (let [^js b (.sensedAt p #js {:x x :y (inc y) :z z})]
    (boolean (and b (not (true? (.-unknown b))) (trees/log-name? (.-name b))))))

(defn surface-kind-at
  "As seen-kind-at, a log or leaf block read as :open: a tree is not the surface. The ground under a trunk is never in
  view (the trunk covers it, the ground beside it hides its sides): an unseen cell under a log reads :solid."
  [p]
  (if (some? (.-sensedAt p))
    (fn [x y z] (let [b (u/seen-block p {:x x :y y :z z})]
                  (cond (nil? b) (if (trunk-over? p x y z) :solid :unknown)
                        (surface/canopy? (.-name b)) :open
                        :else (kind b))))
    (constantly :unknown)))

(defn seen-kind-at
  "(fn [x y z]) -> kind of the block the body of primitives p sees or remembers there, :unknown when never seen (or when
  p senses nothing)."
  [p]
  (if (some? (.-sensedAt p))
    (fn [x y z] (kind (u/seen-block p {:x x :y y :z z})))
    (constantly :unknown)))

(defn free? [k] (or (keyword-identical? :open k) (keyword-identical? :water k)))

(defn built-at
  "(fn [x y z]) -> whether the body of p sees a built block there that a walker cannot pass (not rules/natural?: a floor,
  roof, wall or fence)."
  [p]
  (if (some? (.-sensedAt p))
    (fn [x y z] (let [b (u/seen-block p {:x x :y y :z z})]
                  (boolean (and b (not (surface/canopy? (.-name b))) (not (rules/natural? (.-name b)))
                                (#{:solid :tall} (kind b))))))
    (constantly false)))

(def safe-fall "Blocks a body drops without hp lost (vanilla); a space under a deck deeper than this is a gap, not a room." 3)

(defn room-under?
  "Whether the free cells from y down are a room: air over a stand no deeper than safe-fall."
  [kind-at x y z lo]
  (loop [y y n 1]
    (case (kind-at x y z)
      :open (and (<= n safe-fall) (>= y lo) (recur (dec y) (inc n)))
      :solid (> n 1)
      false)))

(defn roof-under
  "For a built floor at x y z: the y of the cell under its built blocks when they are a roof, not ground (they cover a
  room: air over a stand within safe-fall; or hold a wall or fence, which no one walks on), else nil. A deck over
  water or a gap is ground."
  [kind-at built? x y z lo]
  (loop [y y tall? false]
    (if (and (>= y lo) (built? x y z))
      (recur (dec y) (or tall? (keyword-identical? :tall (kind-at x y z))))
      (when (or tall? (room-under? kind-at x y z lo)) y))))

(defn top
  "The y of the highest stand of the column x z (from lo to hi: default the build height) with no solid or hazard over it
  up to hi, else nil. With built? ((fn [x y z]), built-at), a roof (roof-under) is cover, not a stand: the stand under it
  counts."
  ([kind-at x z lo hi] (top kind-at x z lo hi (constantly false)))
  ([kind-at x z lo hi built?]
   ;; one read per cell, as stands? reads them: the feet of y are the head of y - 1
   (loop [y hi head (kind-at x (inc y) z) feet (kind-at x y z)]
     (when (>= y lo)
       (let [floor (kind-at x (dec y) z)]
         (cond (and (free? feet) (free? head) (keyword-identical? :solid floor))
               (if-let [b (roof-under kind-at built? x (dec y) z lo)]
                 (recur b (kind-at x (inc b) z) (kind-at x b z))
                 y)
               (and (free? feet) (free? head) (keyword-identical? :water feet)) y
               (#{:solid :hazard} feet) nil
               :else (recur (dec y) feet floor)))))))
