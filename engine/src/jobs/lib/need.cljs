(ns jobs.lib.need
  "A contract job's way to get what it lacks: call the fetch job as a child under the caller's remainder (ctx/call) and
  read its ending. tool!, item! and blocks! resolve to :continue while the child waits on the world, else to the ending
  to return or read: :done as the child gave it; any other with the cause {:reason :need|:tool|:free-slots ..} naming
  what was wanted, the child's ending as its :because, the kind kept (a limit stays a limit). A duplicate call is
  the engine's refusal, not a depth option. Options {:slot :how :depth :explore} pass on to the fetch job."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]))

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

(defn ^:async fetch!
  [c job args cause-reason wanted {:keys [slot how depth explore]}]
  (let [args (cond-> (assoc args :explore (boolean explore))
               how (assoc :how how)
               depth (assoc :depth depth))]
    (ending-for (await (ctx/call c (or slot :need) job args)) cause-reason wanted)))

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
