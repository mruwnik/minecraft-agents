(ns jobs.lib.need
  "A contract job's way to get what it lacks: call the fetch job as a child under the caller's remainder (ctx/call) and
  read its ending. tool!, item! and blocks! resolve to :continue while the child waits on the world, else to the ending
  to return or read: :done as the child gave it; any other with the cause {:reason :need|:tool|:free-slots ..} naming
  what was wanted, the child's ending as its :because, the kind kept (a limit stays a limit). A duplicate call is
  the engine's refusal, not a depth option. Options {:slot :how :depth :explore} pass on to the fetch job.

  A fetch that ended :done, asked again with nothing of what it got used up since, ends condition-failed
  {:why :not-solved}: the child does not cure the caller's need. :return? true keeps the cell the body stood on when
  the first fetch began; return! walks back to it once a fetch is done. Both live in the caller's job memory, so a cut
  goes on with them."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]))

(def full-reasons "Fetch endings that mean the bag has no room." #{:inventory-full :bag-full})

(defn ending-for
  "Child ending e of a fetch for wanted (a map of what was asked, e.g. {:item .. :count ..}) as this job's ending:
  :done and :continue pass; a bag-full ending has the cause :free-slots, else cause-reason (:need or :tool)."
  [e cause-reason wanted]
  (if (or (= :continue e) (= :done (:kind e)))
    e
    (let [why (contract/cause-key e)
          reason (if (full-reasons why) :free-slots cause-reason)]
      (-> (contract/ending (:kind e) (assoc wanted :reason reason :why why) (:data e))
          (contract/with-because e)))))

(defn counts
  "{name n} carried of names (0 when none)."
  [c names]
  (let [inv (u/inventory (:primitives c))]
    (into {} (map (fn [n] [n (transduce (comp (filter #(= n (:name %))) (map :count)) + 0 inv)])) names)))

(defn got-names
  "The names a done fetch's ending e is about: what was wanted, or the tool it found."
  [wanted e]
  (or (some-> (:item wanted) vector) (:any-of wanted) (some-> (get-in e [:data :tool]) vector) []))

(defn repeat-done?
  "Whether the done fetch booked in the caller's memory was for want, and none of what it got has been used up."
  [c want]
  (when-let [{w :want have :have} (:need/done (ctx/mem c))]
    (and (= want w) (let [now (counts c (keys have))] (every? (fn [[n k]] (>= (now n) k)) have)))))

(defn feet [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn ^:async fetch!
  [c job args cause-reason wanted {:keys [slot how depth explore return?]}]
  (let [slot (or slot :need)
        args (cond-> (assoc args :explore (boolean explore))
               how (assoc :how how)
               depth (assoc :depth depth))
        want [job args]
        fresh? (nil? (get-in (ctx/mem c) [:children slot]))]
    (if (and fresh? (repeat-done? c want))
      (contract/ending :condition-failed (assoc wanted :reason cause-reason :why :not-solved) nil)
      (do
        (when (and fresh? return? (not (:need/return (ctx/mem c))))
          (ctx/update-mem! c assoc :need/return {:cell (feet c)}))
        (let [e (ending-for (await (ctx/call c slot job args)) cause-reason wanted)]
          (when (= :done (:kind e))
            (ctx/update-mem! c #(cond-> (assoc % :need/done {:want want :have (counts c (got-names wanted e))})
                                  (:need/return %) (assoc-in [:need/return :back] true))))
          e)))))

(defn ^:async return!
  "For a caller using :return?: nil while nothing is to walk back (no mark, no done fetch yet, or there); else one
  round of a go-to child (slot :need-back, range 0) to the kept cell: :continue while it walks, nil once it ended
  (arrived or not: the caller judges where it stands). Call it first in each round."
  [c]
  (let [{:keys [cell back]} (:need/return (ctx/mem c))]
    (cond
      (not back) nil
      (= cell (feet c)) (do (ctx/update-mem! c dissoc :need/return) nil)
      :else (let [[x y z] cell
                  r (await (child/call-walk* c :need-back {:pos {:x x :y y :z z} :range 0 :escalate false :zone-tolls true
                                                            :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
              (when-not (= :continue r)
                (ctx/update-mem! c dissoc :need/return))
              (when (= :continue r) :continue)))))

(defn item!
  "Get n (default 1) of item (a name) with jobs.items.obtain."
  ([c item] (item! c item 1 nil))
  ([c item n] (item! c item n nil))
  ([c item n opts]
   (fetch! c 'jobs.items.obtain {:item item :count n} :need {:item item :count n} opts)))

(defn blocks!
  "Get n of any of the names (building blocks) with jobs.items.obtain."
  ([c names n] (blocks! c names n nil))
  ([c names n opts]
   (fetch! c 'jobs.items.obtain {:any-of (vec names) :count n} :need {:any-of (vec names) :count n} opts)))

(defn tool!
  "Get a tool with jobs.items.get-tool; what is {:block b}, {:item i} or {:kind k}."
  ([c what] (tool! c what nil))
  ([c what opts]
   (fetch! c 'jobs.items.get-tool what :tool what opts)))
