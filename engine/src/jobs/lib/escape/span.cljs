(ns jobs.lib.escape.span
  "The ways of go-to that cross to a goal below or beyond a gap (jobs.lib.escape/choose only climbs and digs out): a stair
  down, a bridge over a gap. Pure over block-at and the cells the body has sensed."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.escape :as escape]
            [plan.shape :as shape]))

(a/defargs settings
  {::max-descent {:default 32 :doc "The most steps a go-to's stair down to a goal below cuts." :spec (a/int-in 1 nil)}
   ::max-aside {:default 16 :doc "The most blocks a go-to's stair down may end beside or past its goal's column (the rest is dug flat)." :spec (a/int-in 0 nil)}
   ::max-bridge {:default 16 :doc "The longest gap (blocks) a go-to bridges." :spec (a/int-in 1 nil)}})

(defn max-descent [] (settings/get settings ::max-descent))
(defn max-aside [] (settings/get settings ::max-aside))
(defn max-bridge [] (settings/get settings ::max-bridge))

(defn headings-toward
  "The four headings (keywords), the one along the larger axis from feet to goal first."
  [feet goal]
  (let [dir (escape/heading feet goal)]
    (map escape/heading-names (distinct (cond->> escape/cardinals dir (cons dir))))))

(defn down-stair
  "{:step :stair :dir :down :heading kw :steps n :aside a} for a goal at least 2 below feet: n steps down to the goal's
  height (at most max-descent) along the heading, among the four, whose landing is nearest the goal's column and at most
  max-aside blocks off it (the rest is dug flat), not in skip as [:stair heading], every cut cell escape/diggable? and most of them solid (no stair through air); nil when none."
  [block-at feet goal may-dig? own? skip]
  (let [n (- (second feet) (second goal))]
    (when (and (>= n 2) (<= n (max-descent)) (not (skip :stair)))
      (->> (headings-toward feet goal)
           (remove #(skip [:stair %]))
           (keep (fn [heading]
                   (let [cells (shape/stair-cells {:from feet :heading heading :to-y (second goal)})
                         [lx _ lz] (last cells)
                         aside (js/Math.round (js/Math.hypot (- lx (goal 0)) (- lz (goal 2))))]
                     (when (and (<= aside (max-aside))
                                (every? #(escape/diggable? block-at may-dig? own? %) cells)
                                (> (* 2 (count (filter #(escape/solid? (block-at %)) cells))) (count cells)))
                       {:step :stair :dir :down :heading heading :steps n :aside aside}))))
           (sort-by :aside)
           first))))

(defn gap-along
  "The bridge along d ([dx dz]) from feet: {:length g :landing cell} when the g floor cells ahead (g from 1, at most
  max-bridge) are not solid, the stand over each has room for the body and the floor after them is solid and not
  protected; nil when the floor ahead is solid already, a cell is unsensed, or no floor comes within max-bridge."
  [block-at unseen? feet d]
  (let [stand (fn [k] (escape/ahead feet d k))
        floor (fn [k] (escape/up (stand k) -1))
        room? (fn [k] (and (not (unseen? (stand k))) (bs/passable? (block-at (stand k)))
                           (not (unseen? (escape/up (stand k) 1))) (bs/passable? (block-at (escape/up (stand k) 1)))))]
    (when (not (escape/solid? (block-at (floor 1))))
      (loop [k 1]
        (let [f (block-at (floor k))]
          (cond
            (or (> k (inc (max-bridge))) (unseen? (floor k)) (not (room? k))) nil
            (and (> k 1) (escape/solid? f)) (when-not (escape/protected? f) {:length (dec k) :landing (stand k)})
            (escape/solid? f) nil
            :else (recur (inc k))))))))

(defn bridge-span
  "{:step :bridge :heading kw :length n :item item :short s :landing cell}: the gap along the heading whose landing is
  nearest goal, at least 2 blocks nearer than feet; item the climb block carried ({:item :count} in pillar-item, else
  none: the bridge job's own pick), s the blocks short of length (0 when fetching is not allowed and some are short:
  then nil). nil when no gap leads nearer."
  [block-at unseen? feet goal {:keys [pillar-item skip fetch?]}]
  (when-not (skip :bridge)
    (let [dist (fn [cell] (js/Math.hypot (- (cell 0) (goal 0)) (- (cell 1) (goal 1)) (- (cell 2) (goal 2))))
          spans (for [d escape/cardinals
                      :let [g (gap-along block-at unseen? feet d)]
                      :when (and g (<= (+ 2 (dist (:landing g))) (dist feet)))]
                  (assoc g :heading (escape/heading-names d)))
          best (first (sort-by (comp dist :landing) spans))
          {:keys [item count] :or {count 0}} pillar-item
          short (when best (max 0 (- (:length best) count)))]
      (when (and best (or (zero? short) fetch?))
        (assoc best :step :bridge :item item :short short)))))
