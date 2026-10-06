(ns jobs.maintenance.shut-doors
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pass :as pass]
            [triggers.maintenance.door-left :as dl]))

(def doc
  "Shut the doors, gates and trapdoors a walk opened and left open (the job of the door-left trigger,
  triggers.maintenance.door-left).
  Only blocks with an :opened entry count (written by jobs.lib.pass when a walk opens a block, dropped when it
  shuts it), whatever their age. A block the walker did not open is never touched. Blocks a :leave-open walk left
  open on purpose are left alone.
  Each round takes the nearest such block within :radius that stands open. It walks within :reach of it (doors
  :never: it opens nothing on the way), then shuts it as the walker does (pass/shut-column!). An animal in the
  cell is waited out, never pushed. A block the body stands in the column of waits for a later round.
  A block not shut after :tries rounds (no way there, a click that does nothing) is given up with one warn
  (shut-doors.gave-up) and its entry dropped, so the trigger leaves it.
  Ends with info shut-doors.done and {:shut n :left [{:cell :reason}]}.")

(def args
  {:radius {:doc "how far from the body a left block is looked for, in blocks" :default 16}
   :reach {:doc "walk until within this many cells of the block (a click reaches 4.5 from the eye)" :default 3}
   :tries {:doc "rounds on one block before it is given up" :default 3}})

;; a few left blocks that cannot be shut are several fruitless rounds in a row, each given up after :tries
(def backoff {:after 9})

(defn cell-key [{:keys [x y z]}] [x y z])

(defn targets
  "The left-open blocks (dl/left-open, any age) within :radius that this run has not given up on."
  [c]
  (let [given-up (set (map :cell (:left (ctx/mem c))))]
    (remove #(given-up (cell-key (:cell %))) (dl/left-open (:primitives c) (ctx/view c) (:args c) 0))))

(defn finish! [c]
  (let [{:keys [shut left] :or {shut 0 left []}} (ctx/mem c)]
    (ctx/emit! c :shut-doors.done :info {:shut shut :left left
                                         :text (str "shut " shut " left open by a walk"
                                                    (when (seq left) (str ", gave up on " (count left))))})
    (ctx/result! c {:shut shut :left left})
    :done))

(defn fail!
  "Count a failed round on target; at :tries give it up: one warn, its entry dropped."
  [c {:keys [cell]} reason]
  (let [k (cell-key cell)
        n (inc (get-in (ctx/mem c) [:fails k] 0))]
    (if (< n (:tries (:args c)))
      (ctx/update-mem! c assoc-in [:fails k] n)
      (do (ctx/update-mem! c #(-> % (update :fails dissoc k) (update :left (fnil conj []) {:cell k :reason reason})))
          (ctx/forget-where! c :opened #(= cell (:cell %)))
          (ctx/emit! c :shut-doors.gave-up :warn {:cell k :reason reason
                                                  :text (str "the block at " k " stays open: " (name reason))})))
    :continue))

(defn ^:async shut! [c {:keys [cell column] :as target}]
  (await (pass/shut-column! c column))
  (if (dl/open-block (:primitives c) cell)
    (fail! c target :shut-failed)
    (do (ctx/update-mem! c update :shut (fnil inc 0)) :continue)))

(defn ^:async round [c]
  (let [target (first (targets c))
        here (dl/feet-cell (u/self-pos c))]
    (cond
      (nil? target) (finish! c)
      (pass/in-column? (:column target) here) (fail! c target :standing-in)
      (> (:dist target) pass/leftover-reach)
      (case (await (near/walk-near! c (:cell target) (:reach (:args c)) {:doors :never}))
        :blocked (fail! c target :unreachable)
        :continue)
      :else (await (shut! c target)))))

(defn check [_c] true)
