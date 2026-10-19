(ns jobs.lib.pen-read
  "The one way a job reads a pen: jobs.lib.pen/check over the cells the body sees now or remembers. A remembered cell may
  have changed since (a blast, a cow-proof fence taken down), so read! looks at the remembered cells in sight range
  before it answers, nearest first, at most sense/max-looks looks. The answer carries :remembered, the cells it read
  from memory that stay out of view."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [jobs.lib.look :as look]
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

(defn remembered?
  "True while the body knows cell [x y z] from memory only."
  [p cell]
  (let [s (u/sensed p (u/cell-pos cell))]
    (not (or (nil? s) (true? (.-unknown s)) (true? (.-visible s)) (true? (.-felt s))))))

(def looks-per-read
  "Looks of a look round between two reads: a read fills the whole pen, which costs more than a look in open ground."
  3)

(defn positive-every
  "A fn that reads the pen on every looks-per-read'th call and returns the answer when retry? no longer holds, else nil."
  [p opts retry?]
  (let [n (volatile! 0)]
    (fn []
      (when (zero? (mod (vswap! n inc) looks-per-read))
        (let [a (read p opts)] (when-not (retry? a) a))))))

(defn ^:async read!
  "read, looking at the remembered cells within sight first (see the ns); while retry? (default: the pen is not
  closed) holds, after a look round (once per body cell, ended at the first look whose read is positive) and at unseen cells as sense/decide! does. A read fills the
  whole pen, so the looks come in batches (usually 3 reads a call). Call it from a round only: it acts."
  ([c opts] (read! c opts #(not (:closed? %))))
  ([c opts retry?]
   (let [p (:primitives c)
         answer (read p opts)
         stale (take (sense/max-looks) (sense/targets p (u/self-pos c) (:remembered answer) #{}))]
     (if (and (empty? stale) (not (retry? answer)))
       answer
       (do (when-not (and (retry? answer) (await (look/survey-until! c :cell (positive-every p opts retry?))))
             (loop [[cell & more] stale]
               (when cell
                 (when (remembered? p cell) (await (sense/look-at! c cell)))
                 (recur more))))
           (await (sense/decide! c ::pen #(read p opts) {:retry? retry? :batch true})))))))

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
  "The inclusive box {:min :max :cols} around the fence, gate and wall cells of cells ({:pos :want}), or nil without any."
  [cells]
  (let [ps (map :pos (filter #(barrier? (:want %)) cells))]
    (when (seq ps)
      (pen/with-cols {:min (zipmap [:x :y :z] (apply map min ps))
                      :max (zipmap [:x :y :z] (apply map max ps))}
                     ps))))

(defn plan-box
  "{:box} around the barrier cells of plan id (of part, when given), or {:trouble text}, with :later? true when the
  plan is missing or unreadable (it may be saved or fixed later)."
  [c id part]
  (let [answer (known/plan c id)
        cells (filter #(or (nil? part) (= part (:part %))) (:cells answer))
        box (when (and answer (not (:broken answer))) (barrier-box cells))]
    (cond
      (nil? answer) {:trouble "no such plan" :later? true}
      (:broken answer) {:trouble (str "the plan cannot be read: " (:broken answer)) :later? true}
      (nil? box) {:trouble "the plan has no fence, wall or gate cells"}
      :else {:box box})))
