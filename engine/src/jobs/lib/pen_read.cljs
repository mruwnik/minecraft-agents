(ns jobs.lib.pen-read
  "The one way a job reads a pen: jobs.lib.pen/check over the cells the body sees now or remembers. A remembered cell may
  have changed since (a blast, a cow-proof fence taken down), so read! looks at the remembered cells in sight range
  before it answers, nearest first, at most sense/max-looks looks. The answer carries :remembered, the cells it read
  from memory that stay out of view."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [jobs.lib.pen :as pen]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(defn block-at-fn
  "A block-at over what the body sees or remembers that adds each cell it reads from memory only to stale (a volatile set of [x y z])."
  [p stale]
  (fn [pos]
    (let [s (u/sensed p pos)]
      (when-not (or (nil? s) (true? (.-unknown s)))
        (when-not (or (true? (.-visible s)) (true? (.-felt s)))
          (vswap! stale conj (u/cell-vec pos)))
        s))))

(defn read
  "jobs.lib.pen/check with opts (without :block-at), plus :remembered, a vector of [x y z]."
  [p opts]
  (let [stale (volatile! #{})
        answer (pen/check (assoc opts :block-at (block-at-fn p stale)))]
    (assoc answer :remembered (vec @stale))))

(defn ^:async read!
  "read, looking at the remembered cells within sight first (see the ns), and at unseen cells as sense/decide! does
  while retry? (default: the pen is not closed) holds. Call it from a round only: it acts."
  ([c opts] (read! c opts #(not (:closed? %))))
  ([c opts retry?]
   (let [p (:primitives c)
         here (u/self-pos c)]
     (loop [looked #{} n 0]
       (let [answer (await (sense/decide! c ::pen #(read p opts) {:retry? retry?}))
             target (when (< n (sense/max-looks))
                      (first (sense/targets p here (:remembered answer) looked)))]
         (if-not target
           answer
           (do (await (sense/look-at! c target))
               (recur (conj looked target) (inc n)))))))))

(defn listed
  "The :remembered cells of answer nearest to the body at here, as {:x :y :z}, at most pen/max-listed, plus
  :remembered-total when more."
  [answer here]
  (let [cells (sort-by #(u/eye-dist here %) (:remembered answer))
        shown (mapv u/cell-pos (take (pen/max-listed) cells))]
    (cond-> {:remembered shown}
      (> (count cells) (count shown)) (assoc :remembered-total (count cells)))))

(def box-arg
  "A pen box with its corners as {:x :y :z} or [x y z] (a plan's form)."
  (a/map-with {:min a/position? :max a/position?}))

(defn norm-box
  "box-arg as {:min {:x :y :z} :max {:x :y :z}}."
  [{:keys [min max]}]
  {:min (a/cell min) :max (a/cell max)})

(defn want-blocks
  "The block names a want can place: one for a block or a state map, the choices of an :any."
  [want]
  (cond
    (string? want) [want]
    (vector? want) (mapcat #(want-blocks (if (string? %) % (:block %))) (rest want))
    (and (map? want) (:block want)) [(:block want)]))

(defn barrier?
  "True when a want places a fence, a fence gate or a wall."
  [want]
  (boolean (some #(or (str/ends-with? % "_fence") (str/ends-with? % "_fence_gate") (str/ends-with? % "_wall"))
                 (want-blocks want))))

(defn barrier-box
  "The inclusive box {:min :max} around the fence, gate and wall cells of cells ({:pos :want}), or nil without any."
  [cells]
  (let [ps (map :pos (filter #(barrier? (:want %)) cells))]
    (when (seq ps)
      {:min (zipmap [:x :y :z] (apply map min ps))
       :max (zipmap [:x :y :z] (apply map max ps))})))

(defn plan-box
  "{:box} around the barrier cells of plan id (of part, when given), or {:trouble text}."
  [c id part]
  (let [answer (known/plan c id)
        cells (filter #(or (nil? part) (= part (:part %))) (:cells answer))
        box (when (and answer (not (:broken answer))) (barrier-box cells))]
    (cond
      (nil? answer) {:trouble "no such plan"}
      (:broken answer) {:trouble (str "the plan cannot be read: " (:broken answer))}
      (nil? box) {:trouble "the plan has no fence, wall or gate cells"}
      :else {:box box})))
