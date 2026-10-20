(ns jobs.lib.job-common
  "What the farm jobs share as contract jobs: the walk to a cell (a go-to child) and the
  test of a partial outcome against the call's :least."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.near :as near]
            [jobs.lib.places :as places]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.world :as known]))

(defn enough?
  "Whether n of total units (cells, uses, bone meal) meet the call's :least (default 1; :all wants every one)."
  [c n total]
  (let [least (:least (ctx/frozen-call c) 1)]
    (>= n (if (= :all least) total least))))

(defn conclude!
  "The ending of a run with no cell left to work: :done when n cells were worked enough (or none was skipped), else
  :nothing-possible, or :condition-failed when the cause is in condition-skips. The cause is the one skip reason of
  skipped ({pos reason}), or mixed when they differ; texts maps a cause to its own text. result is the ending's data."
  [c {:keys [verb mixed condition-skips texts]} n skipped result]
  (let [total (+ n (count skipped))
        summary (str verb " " n ", skipped " (count skipped))]
    (if (or (empty? skipped) (enough? c n total))
      (ctx/end c :done nil (assoc result :text summary))
      (let [counts (frequencies (vals skipped))
            reason (if (= 1 (count counts)) (key (first counts)) mixed)
            text (or (get texts reason)
                     (str "nothing " verb ": " (str/join ", " (map (fn [[r k]] (str k " " (name r))) (sort-by (comp name key) counts)))))
            [cause text] (if (pos? n) [:partial summary] [reason text])]
        (ctx/end c (if (condition-skips cause) :condition-failed :nothing-possible) cause (assoc result :text text))))))

(defn no-zones
  "The cause {:reason :no-zones} while no zone list has been read (unless the call's :ignore-zones?); else nil."
  [c]
  (when-not (or (:ignore-zones? (:args c)) (some? (known/zones c)))
    {:reason :no-zones}))

(defn ^:async walk!
  "jobs.lib.near/go-near! to within range of pos (opts: :tolls): :there, :continue (go-to waits on the world) or :blocked."
  [c pos range {:keys [tolls]}]
  (let [w (await (near/go-near! c pos range {:tolls tolls}))]
    (if (= :partial w) :continue w)))

(defn ^:async walk-to!
  "walk! to pos with the tolls of the cell: :there, :continue or :blocked."
  [c pos range]
  (await (walk! c pos range {:tolls (tc/walk-tolls c (:pos (places/parse-pos pos)))})))
