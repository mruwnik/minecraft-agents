(ns jobs.lib.makers
  "Which listed job will make an item, and whether waiting for it costs less than getting the item now: a reflex that
  would fetch one itself then waits for it instead (needs-bed, needs-weapon)."
  (:require [engine.contract :as contract]
            [engine.memory :as mem]
            [jobs.lib.cost :as cost]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]))

(def iron-kit
  "jobs.items.get-kit's default :items."
  ["iron_pickaxe" "iron_sword" "iron_axe" "iron_shovel" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(defn makes
  "The item names a listed leaf job {:job label :args} brings: an obtain's :item, :any-of and :bill, a get-kit's :items,
  the blocks of a from-plan's plan (its :part only, when named; plans from world-files kn)."
  [kn {:keys [job args]}]
  (case (str job)
    "jobs.items.obtain" (concat (some-> (:item args) vector) (:any-of args) (keys (:bill args)))
    "jobs.items.get-kit" (or (:items args) iron-kit)
    "jobs.build.from-plan" (->> (:cells (world/plan kn (:plan args)))
                                (filter #(or (nil? (:part args)) (= (:part args) (:part %))))
                                (mapcat (comp pen-read/want-blocks :want)))
    nil))

(defn makers
  "The ids in live (the listed ids) of the jobs that bring an item `wanted?` holds for, with the names they bring of it."
  [view kn live wanted?]
  (keep (fn [id] (when-let [names (seq (filter wanted? (makes kn (:data (mem/latest view (mem/job-kind id))))))] [id names])) live))

(defn time-left
  "Seconds of :job-time listed job memory m has left: a started one's remainder (contract/remainder), a queued one's
  :spend :job-time arg; ##Inf with neither."
  [m now]
  (let [state (:contract m)]
    (or (if (:call state)
          (get (contract/remainder state now 0) :job-time)
          (let [t (get-in m [:args :spend :job-time])] (when (number? t) t)))
        ##Inf)))

(defn wait-s
  "Seconds before listed job id starts: 0 once it has; else the time-left of every listed job queued before it (their
  :queued-t: the queue runs in that order)."
  [view live id]
  (let [mine (mem/job-mem view id [])
        now (:now view)]
    (if (:call (:contract mine))
      0
      (->> live
           (map #(mem/job-mem view % []))
           (filter #(< (:queued-t % ##Inf) (:queued-t mine 0)))
           (map #(time-left % now))
           (reduce + 0)))))

(defn waiting-pays?
  "Whether a listed job brings an item `wanted?` holds for soon enough that waiting for it (cost/wait-price over need-s,
  the seconds until the item is needed, of the wait-s of the soonest) costs less than getting it now: a trip at least,
  else what it is worth to the body (a priced search spends that)."
  [p view kn live wanted? need-s]
  (boolean
   (when-let [found (seq (makers view kn live wanted?))]
     (let [body (cost/death-body p view)
           at (u/self-pos {:primitives p})]
       (some (fn [[id names]]
               (let [worth (cost/worth-to body names at)]
                 (< (cost/wait-price worth (wait-s view live id) need-s) (max (cost/trip) worth))))
             found)))))
