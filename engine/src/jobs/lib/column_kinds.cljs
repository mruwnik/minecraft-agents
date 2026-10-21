(ns jobs.lib.column-kinds
  "How a body reads the cells of a column: the kind of a seen block, and the highest stand of a column."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.surface :as surface]
            [jobs.lib.util :as u]))

(def top-y "The highest feet y a column goal looks for ground at (the head cell above is the build limit's last)." (- places/max-y 2))

(defn kind
  "A block as a stand test reads it: :unknown (nil: unseen or unloaded), :hazard, else reach/kind-of."
  [b]
  (cond (nil? b) :unknown
        (bs/harms-in-cell (.-name b)) :hazard
        :else (reach/kind-of b)))

(defn surface-kind-at
  "As seen-kind-at, a log or leaf block read as :open: a tree is not the surface."
  [p]
  (if (some? (.-sensedAt p))
    (fn [x y z] (let [b (u/seen-block p {:x x :y y :z z})]
                  (if (and b (surface/canopy? (.-name b))) :open (kind b))))
    (constantly :unknown)))

(defn seen-kind-at
  "(fn [x y z]) -> kind of the block the body of primitives p sees or remembers there, :unknown when never seen (or when
  p senses nothing)."
  [p]
  (if (some? (.-sensedAt p))
    (fn [x y z] (kind (u/seen-block p {:x x :y y :z z})))
    (constantly :unknown)))

(defn free? [k] (or (keyword-identical? :open k) (keyword-identical? :water k)))

(defn top
  "The y of the highest stand of the column x z (from lo to hi: default the build height) with no solid or hazard over it
  up to hi, else nil."
  ([kind-at x z] (top kind-at x z places/min-y top-y))
  ([kind-at x z lo hi]
   ;; one read per cell, as stands? reads them: the feet of y are the head of y - 1
   (loop [y hi head (kind-at x (inc y) z) feet (kind-at x y z)]
     (when (>= y lo)
       (let [floor (kind-at x (dec y) z)]
         (cond (and (free? feet) (free? head) (or (keyword-identical? :water feet) (keyword-identical? :solid floor))) y
               (#{:solid :hazard} feet) nil
               :else (recur (dec y) feet floor)))))))
