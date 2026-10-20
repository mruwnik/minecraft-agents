(ns jobs.survival.recover-drops.ending
  "How a recover-drops trip ends: the :recovered entry and the ending by decision."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.survival.recover-drops.config :as cfg]
            [jobs.survival.recover-drops.deaths :as dth]
            [jobs.survival.recover-drops.estimate :as est]))

(def endings
  "The kind and cause of a trip's end by its decision (and for an abandoned pile its reason); a pile not worth the trip or despawned is a fact about the goal, not a limit of the call."
  {:collected [:done nil]
   :skip [:condition-failed :not-worth]
   [:abandoned :window-closed] [:condition-failed :gone]
   [:abandoned :danger] [:condition-failed :danger]
   [:abandoned :nothing-found] [:condition-failed :nothing-found]})

(defn ending-for
  "The ending of a trip that wrote decision with fields: collected is done, a skip condition-failed :not-worth, an abandoned
  pile by its reason (:window-closed and :danger and :nothing-found condition-failed), the rest nothing-possible
  with the reason (:unreachable, or for a partial :unreachable / :not-visible)."
  [c decision fields text pos]
  (let [[kind cause] (or (endings decision) (endings [decision (:reason fields)]) [:nothing-possible (:reason fields)])]
    (ctx/end c kind cause (assoc (select-keys fields [:items :left :detail :value]) :decision decision :text text :pos pos))))

(defn finish!
  "Write the :recovered entry and end the job with its ending."
  [c decision fields]
  (let [death (dth/current-death c)
        pos (:pos (:data death))
        text (cond-> (est/decision-text decision fields (cfg/margin))
               (= :abandoned decision) (str "; the pile at " (str/join " " (map #(js/Math.floor (% pos)) [:x :y :z]))))]
    (ctx/remember! c :recovered (merge {:decision decision :death-t (:t death)} (dissoc fields :at :flight-s)) cfg/recovered-policy)
    (ending-for c decision fields text pos)))
