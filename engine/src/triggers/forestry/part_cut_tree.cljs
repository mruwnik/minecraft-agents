(ns triggers.forestry.part-cut-tree
  "The part-cut-tree trigger: a trunk in sight whose lowest log hangs over air, because the logs under it were cut
  (jobs.lib.part-cut). Its job, jobs.forestry.fell-tree with :part-cut? true, fells the nearest such trunk with the
  logs the body carries (no fetch trip), collects the logs and writes the replant debt on the ground under it.
  The sight is read at most every ::recheck-s seconds: a scan of every seen log is too much for each tick."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.part-cut :as part-cut]
            [jobs.lib.trees :refer [default-radius]]))

(a/defargs settings
  {::recheck-s {:default 5 :doc "Seconds a sight of part-cut trunks is kept before the logs in view are read again." :spec (a/num-in 0 nil)}})

(def defaults {})

(def last-read (atom nil))

(defn trunks-seen?
  "Whether a part-cut trunk is in sight, as read at most ::recheck-s seconds ago for the same body."
  [p]
  (let [now (.now js/Date)
        {:keys [body at seen]} @last-read]
    (if (and (identical? body p) (< (- now at) (* 1000 (settings/get settings ::recheck-s))))
      seen
      (let [seen (boolean (seq (part-cut/part-cut-trees p default-radius nil)))]
        (reset! last-read {:body p :at now :seen seen})
        seen))))

(defn part-cut-tree
  "Holds alive, on the ground, when a part-cut trunk is in sight. A trunk a zone refuses holds it too: the job's
  checks keep it from starting."
  [p view _args _kn _live]
  (boolean
   (and (not (body/recovery-pending? view :part-cut-tree))
        (not (false? (.-onGround (.self p))))
        (trunks-seen? p))))
