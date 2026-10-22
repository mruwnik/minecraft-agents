(ns triggers.forestry.part-cut-tree
  "The part-cut-tree trigger: a trunk in sight whose lowest log hangs over air, because the logs under it were cut
  (jobs.lib.part-cut). Its job, jobs.forestry.fell-tree with :part-cut? true, fells the nearest such trunk with the
  logs the body carries (no fetch trip), collects the logs and writes the replant debt on the ground under it.
  A trunk whose base cell a zone or claim of another player refuses is not a part-cut tree to finish: it does not hold
  the trigger, nor one whose column fell-tree lately gave up on (:tree-unreachable, 10 min). A finished chore, not a
  forest to clear: it does not hold while the body carries ::max-logs logs or more, nor for a trunk farther than
  ::home-radius from the remembered home (else bed; with none known the distance is not judged). It does not hold at night: the
  night job walks the body in and a felling walk out again would undo it. The sight is read at most every ::recheck-s seconds: a scan of every seen log is too much for each tick."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.part-cut :as part-cut]
            [jobs.lib.shelter :as sh]
            [jobs.lib.trees :as trees :refer [default-radius recently-unreachable]]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]))

(a/defargs settings
  {::recheck-s {:default 5 :doc "Seconds a sight of part-cut trunks is kept before the logs in view are read again." :spec (a/num-in 0 nil)}
   ::max-logs {:default 64 :doc "Logs carried (all species) from which the trigger no longer fells a trunk: a stack is enough for the chore's purpose." :spec (a/int-in 1 nil)}
   ::home-radius {:default 40 :doc "Blocks from the remembered home or bed beyond which a part-cut trunk is not the body's to finish." :spec (a/num-in 0 nil)}})

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

(defn too-far?
  "Whether the trunk lies farther than ::home-radius from home (nil home: never)."
  [home t]
  (boolean (and home (> (u/flat-dist home (:base t)) (settings/get settings ::home-radius)))))

(defn stocked?
  "Whether the body carries ::max-logs logs or more."
  [p]
  (<= (settings/get settings ::max-logs)
      (transduce (comp (filter (comp trees/log-name? :name)) (map :count)) + 0 (u/inventory p))))

(defn trunks-seen?
  "Whether a part-cut trunk is in sight, as read at most ::recheck-s seconds ago for the same body."
  [p kn view]
  (let [now (.now js/Date)
        {:keys [body at seen]} @last-read]
    (if (and (identical? body p) (< (- now at) (* 1000 (settings/get settings ::recheck-s))))
      seen
      (let [seen (let [given-up (recently-unreachable view) home (sh/home-target view)]
                   (boolean (seq (remove #(or (refused? p kn %) (too-far? home %) (given-up [(:x (:column %)) (:z (:column %))]))
                                         (part-cut/part-cut-trees p default-radius nil)))))]
        (reset! last-read {:body p :at now :seen seen})
        seen))))

(defn part-cut-tree
  "Holds alive, on the ground, when a part-cut trunk that no other player's zone or claim refuses is in sight."
  [p view _args kn _live]
  (boolean
   (and (not (body/recovery-pending? view :part-cut-tree))
        (not (false? (.-onGround (.self p))))
        (not (stocked? p))
        (trunks-seen? p kn view))))
