(ns jobs.maintenance.shut-doors
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.result :as res]
            [jobs.lib.pass :as pass]
            [triggers.maintenance.door-left :as dl]))

(def doc
  "Shut the doors, gates and trapdoors a walk opened and left open (the job of the door-left trigger,
  triggers.maintenance.door-left).
  Only blocks with an :opened entry count (written by jobs.lib.pass when a walk opens a block, dropped when it
  shuts it), whatever their age. A block the walker did not open is never touched. Blocks a :leave-open walk left
  open on purpose are left alone while that walk's job lives (once it is gone, they count).
  One run shuts them all, nearest first. For each it walks within :reach (a jobs.movement.go-to child, doors :never:
  it opens nothing on the way), then shuts it as the walker does (pass/shut-column!). An animal in the cell is waited
  out, never pushed. A block that cannot be reached, that the body stands in the column of, or that is not shut after
  :tries clicks is given up with one warn (shut-doors.gave-up) and its entry dropped, so the trigger leaves it. A
  block the body stands in the column of, or whose walk was interrupted (:walk-interrupted, info only), is left and
  keeps its entry, stamped afresh so the trigger waits :open-s again: a later run shuts it.
  Ends done with info shut-doors.done {:shut n :left []} when every block was shut (or none stood open); with any
  left, stopped :left with warn shut-doors.stopped and {:shut n :left [{:cell :reason}]}.")

(def args
  {:radius {:doc "how far from the body a left block is looked for, in blocks" :default 16}
   :reach {:doc "walk until within this many cells of the block (a click reaches 4.5 from the eye)" :default 3}
   :tries {:doc "clicks on one block before it is given up" :default 3}})

;; the blocks given up in this run are skipped; a later run starts afresh from the world and the :opened entries
(defn cell-key [{:keys [x y z]}] [x y z])

(defn targets
  "The left-open blocks (dl/left-open, any age) within :radius, minus the cells in left ({:cell [x y z]})."
  [c left]
  (let [given-up (set (map :cell left))]
    (remove #(given-up (cell-key (:cell %))) (dl/left-open (:primitives c) (ctx/view c) (:args c) 0))))

(defn ^:async walk!
  "nil when the body stands within reach, else why not: :walk-interrupted (the walk ended :continue) or :unreachable."
  [c {:keys [cell]}]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos cell :range (:reach (:args c)) :doors :never :escalate false}))]
    (cond
      (= :continue r) :walk-interrupted
      (and (= :done r) (:arrived (ctx/child-result c :walk))) nil
      :else :unreachable)))

(def keep-entry
  "Reasons that leave the :opened entry: the block may well be shut by a later run."
  #{:standing-in :walk-interrupted})

(defn ^:async shut-once!
  "Shut the target: nil when shut, else the reason it stays open."
  [c {:keys [cell column dist]}]
  (if (pass/in-column? column (dl/feet-cell (u/self-pos c)))
    :standing-in
    (if-let [why (when (> dist pass/leftover-reach) (await (walk! c {:cell cell})))]
      why
      (loop [n 1]
        (await (pass/shut-column! c column))
        (cond
          (not (dl/open-block (:primitives c) cell)) nil
          (< n (:tries (:args c))) (recur (inc n))
          :else :shut-failed)))))

(defn give-up!
  "Record why the target stays open. A kept entry is stamped afresh (the trigger waits :open-s again); a walk that was
  interrupted is not a failure, so it only gets an info."
  [c {:keys [cell by]} reason]
  (let [k (cell-key cell)]
    (if (keep-entry reason)
      (do (ctx/forget-where! c :opened #(= cell (:cell %)))
          (ctx/remember! c :opened {:cell cell :by by :t (ctx/now c) :shut? true} pass/opened-policy))
      (ctx/forget-where! c :opened #(= cell (:cell %))))
    (ctx/emit! c :shut-doors.gave-up (if (= :walk-interrupted reason) :info :warn)
               {:cell k :reason reason :text (str "the block at " k " stays open: " (name reason))})
    {:cell k :reason reason}))

(defn finish! [c shut left]
  (let [result {:shut shut :left left}]
    (if (empty? left)
      (do (ctx/emit! c :shut-doors.done :info (assoc result :text (str "shut " shut " left open by a walk")))
          (res/finish! c result))
      (let [text (str "shut " shut " left open by a walk, gave up on " (count left))
            level (if (every? #(= :walk-interrupted (:reason %)) left) :info :warn)]
        (ctx/emit! c :shut-doors.stopped level (assoc result :text text))
        (res/stop! c :left text :shut shut :left left)))))

(defn ^:async round [c]
  (loop [shut 0 left []]
    (if-let [target (first (targets c left))]
      (if-let [reason (await (shut-once! c target))]
        (recur shut (conj left (give-up! c target reason)))
        (recur (inc shut) left))
      (finish! c shut left))))

(defn check [_c] true)
