(ns jobs.lib.standing
  "A standing job (one kept in the queue under repeat, or a site job) declines while a job that is not standing can run
  (away-cause), so it does not walk the body home between trips; standing jobs are the listed ones under repeat (standing-ids).
  While such jobs only wait, it declines when the walk there and back would delay the soonest of them by more than a pass is
  worth. Runnable means listed, not failed and without a wait entry: a job never asked, or one that rounds with :continue and
  no reason, counts as runnable (the scheduler's own check-passes? is engine core)."
  (:require [clojure.string :as str]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]))

(defn root-id [c] (first (str/split (:id c) #"/")))

(def site-jobs
  "The jobs that decline here: they must not hold each other off, or two far ones wait for ever."
  #{'jobs.farm.tend 'jobs.animals.maintain 'jobs.items.kitchen})

(defn standing-ids
  "The ids of the listed jobs kept under repeat, and of the site jobs."
  [c]
  (into #{}
        (comp (filter (fn [[_ inst]] (let [spec (:spec inst)] (or (= :repeat (:op spec)) (site-jobs (:job spec))))))
              (map key))
        (:instances @(:state (:engine c)))))

(defn listed-others
  "The ids of the listed jobs that are neither standing (nor this one's root) nor failed."
  [c]
  (let [standing (conj (standing-ids c) (root-id c))
        st @(:state (:engine c))]
    (into [] (remove #(or (standing %) (contains? (:failed st) %))) (:list st))))

(defn runnable-others
  "The ids of the listed jobs that are not standing (nor this one's root), not waiting on a declining check and not failed:
  the ones that run or can start now."
  [c]
  (let [waiting @(:waiting (:engine c))]
    (into [] (remove waiting) (listed-others c))))

(defn wait-left-s
  "Seconds until waiting job id rechecks (its reason's :recheck-after-ms less the time since it was told), or nil when the
  reason names no length."
  [c id]
  (let [eng (:engine c)
        w (get @(:waiting eng) id)]
    (when-let [ms (:recheck-after-ms w)]
      (/ (max 0 (- ms (- ((:now eng)) (:t (meta w))))) 1000))))

(defn delay-s
  "Seconds a pass at site delays the soonest of the waiting others, a round trip (cost/walk-cost both ways) over the
  wait left; 0 when none has a known wait."
  [c site]
  (let [lefts (keep #(wait-left-s c %) (listed-others c))]
    (if (empty? lefts)
      0
      (max 0 (- (* 2 (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) site)}))) (apply min lefts))))))

(defn away-cause
  "nil, or the cause {:reason :away :site :others ids} when site (a position, or nil) is set and a job that is not
  standing runs or can start now: the body's time is spoken for, so a standing job does not walk it to its site. When
  they all wait, the cause also has :delay-s once the round trip delays the soonest by more than a trip (cost/trip),
  the least a pass is worth. With nothing else to run the body walks there, however far or deep."
  [c site]
  (when site
    (let [others (runnable-others c)]
      (if (seq others)
        {:reason :away :site site :others others}
        (let [delay (delay-s c site)]
          (when (> delay (cost/trip))
            {:reason :away :site site :others (listed-others c) :delay-s delay}))))))
