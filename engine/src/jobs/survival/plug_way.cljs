(ns jobs.survival.plug-way
  "What the plug ways share (jobs.survival.plug-walls, plug-pit, plug-pocket; picked between by jobs.survival.plug-in):
  the enclosed test, the goal, the quote and the stepping round."
  (:require [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter :as sh]
            [jobs.lib.spent :as spent]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-seal :as seal]))

(defn enclosed? [c]
  (dig-cells/sealed-in? (:primitives c) :walls (:roof-height (:args c))))

(defn done? [c way]
  (let [done (enclosed? c)]
    (checks/goal done [:enclosed "the body is enclosed"] [:open "the body is not enclosed"]
                 {:units :enclosures :done (if done 1 0) :left (if done 0 1)
                  :result {:pos (sh/feet (:primitives c)) :way way}})))

(defn quote-of
  "Time of a way: 0 once enclosed, else seconds (##Inf when it cannot be done here, which plug-in drops)."
  [c seconds job-time-s]
  (let [mean (if (enclosed? c) 0 seconds)]
    {:uses {:job-time [0 mean (if (infinite? mean) mean (max (* 3 mean) job-time-s))]} :done [0 1 1]}))

(defn ^:async walls-step!
  "One step of the walls (retreat-seal/seal!): :done once enclosed, :again, or nil when they cannot be placed."
  [c]
  (case (await (seal/seal! c))
    :sealed :done
    :again :again
    nil))

(defn ^:async run!
  "Steps of the way (step!: :done, :again, or nil when it fails) with a timer between, to the call's end. A place refused for a mob
  yields (:continue) so the world moves and the mob may clear."
  [c way step!]
  (await (pace/steps! c (fn ^:async way-step []
                          (case (await (step! c))
                            :done (ctx/end c :done nil (merge {:pos (sh/feet (:primitives c)) :way way} (spent/summary (spent/log c))))
                            :again (if (:seal-mob-since (ctx/mem c)) :continue :again)
                            (ctx/end c :nothing-possible :failed (merge {:text (or (:seal-refusal (ctx/mem c)) (str "the " (name way) " cannot enclose the body here"))} (spent/summary (spent/log c)))))))))
