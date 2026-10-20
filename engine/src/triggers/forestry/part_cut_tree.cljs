(ns triggers.forestry.part-cut-tree
  "The part-cut-tree trigger: a trunk in sight whose lowest log hangs over air, because the logs under it were cut
  (jobs.lib.part-cut). Its job, jobs.forestry.fell-tree with :part-cut? true, fells the nearest such trunk with the
  logs the body carries (no fetch trip), collects the logs and writes the replant debt on the ground under it.
  A trunk whose base cell a zone or claim of another player refuses is not a part-cut tree to finish: it does not hold
  the trigger, nor one whose column fell-tree lately gave up on (:tree-unreachable, 10 min). The sight is read at most every ::recheck-s seconds: a scan of every seen log is too much for each tick."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.part-cut :as part-cut]
            [jobs.lib.trees :refer [default-radius recently-unreachable]]
            [jobs.lib.world-files :as world]))

(a/defargs settings
  {::recheck-s {:default 5 :doc "Seconds a sight of part-cut trunks is kept before the logs in view are read again." :spec (a/num-in 0 nil)}})

(def defaults {})

(def last-read (atom nil))

(defn refused?
  "Whether another player's zone or claim refuses digging the trunk's base cell (an unread zone list refuses nothing here)."
  [p kn t]
  (let [{:keys [x y z]} (:base t)
        now (.now js/Date)
        v (zones/verdict {:zones (world/zones kn) :claims (world/live-claims (world/area-claims kn) now)
                          :self (.-username (.self p)) :now now :action :dig :cell [x y z]})]
    (contains? #{:zone :claim} (:reason v))))

(defn trunks-seen?
  "Whether a part-cut trunk is in sight, as read at most ::recheck-s seconds ago for the same body."
  [p kn view]
  (let [now (.now js/Date)
        {:keys [body at seen]} @last-read]
    (if (and (identical? body p) (< (- now at) (* 1000 (settings/get settings ::recheck-s))))
      seen
      (let [seen (let [given-up (recently-unreachable view)]
                   (boolean (seq (remove #(or (refused? p kn %) (given-up [(:x (:column %)) (:z (:column %))]))
                                         (part-cut/part-cut-trees p default-radius nil)))))]
        (reset! last-read {:body p :at now :seen seen})
        seen))))

(defn part-cut-tree
  "Holds alive, on the ground, when a part-cut trunk that no other player's zone or claim refuses is in sight."
  [p view _args kn _live]
  (boolean
   (and (not (body/recovery-pending? view :part-cut-tree))
        (not (false? (.-onGround (.self p))))
        (trunks-seen? p kn view))))
