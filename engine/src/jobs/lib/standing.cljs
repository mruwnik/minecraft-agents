(ns jobs.lib.standing
  "A standing job (one kept in the queue under repeat, or a site job) declines while a job that is not standing can run
  (away-cause), so it does not walk the body home between trips; standing jobs are the listed ones under repeat (standing-ids).
  While such jobs only wait, it declines when the walk there and back and the pass's work would delay the soonest of them
  by more than the pass is worth. Runnable means listed, not failed and without a wait entry: a job never asked, or one that rounds with :continue and
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
  "Seconds until waiting job id is expected to run again: what is left, since it was told, of the length its reason
  declares (:wait-ms: a wait whose end is known, as the light's or the dawn's), else cost/unknown-wait-s (a recheck
  interval is no length); nil when it does not wait."
  [c id]
  (let [eng (:engine c)
        w (get @(:waiting eng) id)
        now ((:now eng))]
    (when w
      (if-let [ms (:wait-ms w)]
        (/ (max 0 (- ms (- now (:t (meta w) now)))) 1000)
        cost/unknown-wait-s))))

(defn delay-s
  "Seconds a pass at site that works work-s there delays the soonest of the waiting others: the walk there and back
  (cost/walk-cost both ways) and the work, over its wait left; 0 when none waits."
  [c site work-s]
  (let [lefts (keep #(wait-left-s c %) (listed-others c))]
    (if (empty? lefts)
      0
      (max 0 (- (+ (* 2 (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) site)}))) work-s) (apply min lefts))))))

(defn away-cause
  "nil, or the cause {:reason :away :site :others ids} when site (a position, or nil) is set and a job that is not
  standing runs or can start now: the body's time is spoken for, so a standing job does not walk it to its site. When
  they all wait, the cause also has :delay-s and :worth once the pass (pass: (fn [c]) {:worth :work-s}, the seconds the
  pass is worth and works) delays the soonest by more than it is worth. With nothing else listed the body walks there,
  however far or deep."
  [c site pass]
  (when site
    (let [others (runnable-others c)]
      (cond
        (seq others) {:reason :away :site site :others others}
        (seq (listed-others c))
        (let [{:keys [worth work-s]} (pass c)
              delay (delay-s c site work-s)]
          (when (> delay worth)
            {:reason :away :site site :others (listed-others c) :delay-s delay :worth worth}))))))
