(ns jobs.lib.makers
  "Which listed job will make an item, and whether waiting for it costs less than getting the item now: a reflex that
  would fetch one itself then waits for it instead (needs-bed, needs-weapon)."
  (:require [engine.contract :as contract]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.build.from-plan :as from-plan]
            [jobs.items.get-kit :as kit]
            [jobs.items.obtain :as obtain]
            [jobs.lib.cost :as cost]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]
            [jobs.time.noop :as noop]))

(defn makes
  "The item names a listed leaf job {:job label :args} brings: an obtain's :item, :any-of and :bill, a get-kit's :items,
  the blocks of a from-plan's plan (its :part only, when named; plans from world-files kn)."
  [kn {:keys [job args]}]
  (case (str job)
    "jobs.items.obtain" (concat (some-> (:item args) vector) (:any-of args) (keys (:bill args)))
    "jobs.items.get-kit" (or (:items args) kit/iron-kit)
    "jobs.build.from-plan" (->> (:cells (world/plan kn (:plan args)))
                                (filter #(or (nil? (:part args)) (= (:part args) (:part %))))
                                (mapcat (comp pen-read/want-blocks :want)))
    nil))

(defn makers
  "The ids in live (the listed ids) of the jobs that bring an item `wanted?` holds for, with the names they bring of it."
  [view kn live wanted?]
  (keep (fn [id] (when-let [names (seq (filter wanted? (makes kn (:data (mem/latest view (mem/job-kind id))))))] [id names])) live))

(defn default-job-time
  "The :spend :job-time a queued job gets when its caller names none (its contract default), for the jobs a body queues
  in front of a maker; nil for any other."
  [label args]
  (case (str label)
    "jobs.items.obtain" (obtain/default-time args)
    "jobs.items.get-kit" (settings/get kit/settings ::kit/job-time-s)
    "jobs.build.from-plan" (settings/get from-plan/settings ::from-plan/job-time-s)
    "jobs.time.noop" (settings/get noop/settings ::noop/job-time-s)
    nil))

(defn time-left
  "Seconds of :job-time listed job memory m has left: a started one's remainder (contract/remainder), a queued one's
  :spend :job-time arg, else its default; ##Inf with none."
  [m now]
  (let [state (:contract m)]
    (or (if (:call state)
          (get (contract/remainder state now 0) :job-time)
          (let [t (get-in m [:args :spend :job-time])]
            (if (number? t) t (default-job-time (:job m) (:args m)))))
        ##Inf)))

(defn wait-s
  "Seconds before listed job id has the item: a started one's time-left; else the time-left of every listed job queued
  before it (their :queued-t: the queue runs in that order) plus its own, for the item exists only when it ends."
  [view live id]
  (let [mine (mem/job-mem view id [])
        now (:now view)]
    (if (:call (:contract mine))
      (time-left mine now)
      (->> live
           (map #(mem/job-mem view % []))
           (filter #(< (:queued-t % ##Inf) (:queued-t mine 0)))
           (cons mine)
           (map #(time-left % now))
           (reduce + 0)))))

(defn waiting-pays?
  "Whether a listed job brings an item `wanted?` holds for in time, before need-s, the seconds until the item is needed, and
  waiting for it (cost/wait-price over need-s, of the wait-s of the soonest) costs less than getting it now: a trip at
  least, else what it is worth to the body (a priced search spends that)."
  [p view kn live wanted? need-s]
  (boolean
   (when-let [found (seq (makers view kn live wanted?))]
     (let [body (cost/death-body p view)
           at (u/self-pos {:primitives p})]
       (some (fn [[id names]]
               (let [worth (cost/worth-to body names at)
                     wait (wait-s view live id)]
                 (and (< wait need-s)
                      (< (cost/wait-price worth wait need-s) (max (cost/trip) worth)))))
             found)))))
