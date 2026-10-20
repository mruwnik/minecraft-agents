(ns jobs.movement.chase
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.aim :as aim]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.places :as places]
            [jobs.lib.util :as u]))

(def doc
  "Walk one leg toward a thing that moves: a go-to child of at most :leg-s seconds aimed at where the thing is now. The
  thing is the entity :entity (an id) or the player :player, seen within the radius setting (64); out of sight, the last
  :pos given. A caller that wants more legs calls again, so it can look at its target, fight or check danger between legs.
  The :walk map is the go-to call (its args and call keys, such as :ways :doors :see :checks :settings :floor, and a
  :spend; :tries is 1 unless it says otherwise: a leg is re-aimed by the caller, not retried); :pos, :range and :spend :job-time are this job's.
  Ends:
  - :done {:arrived true} when the body is within :range of the thing (at once when it already is; with :walk {:see true} only when it also sees the thing's cell).
  - limit-reached {:reason :leg :line :job-time} with {:arrived false :leg true :nearer bool} when the leg's seconds ran out
    (also mid-search): :nearer says the body got more than 1 block closer. A leg that got no nearer is no failure: the next
    leg is aimed afresh.
  - go-to's own ending otherwise (a give-up with its :why, a refusal), unchanged.
  :continue while the go-to child waits on the world. Quote: one leg.")

(a/defargs args
  {:pos {:doc "where the thing is, or was last seen" :spec ::a/pos :default nil}
   :entity {:doc "id of the entity to walk to; its position now replaces :pos" :spec (a/int-in 0 nil) :default nil}
   :player {:doc "name of the player to walk to; their position now replaces :pos" :spec a/name? :default nil}
   :range {:doc "how close counts as there, in blocks" :spec (a/num-in 0 nil) :default 1}
   :leg-s {:doc "seconds one leg of the walk may take" :spec (a/num-in 0.1 120) :default nil}
   :walk {:doc "the go-to call to walk with: its args and call keys, but not :pos, :range or :spend :job-time" :spec map? :default {}}
   :ignore-zones? :shared})

(a/defargs settings
  {::radius {:default 64 :doc "How far the entity or player may be and still be seen, in blocks." :spec (a/num-in 0 nil)}
   ::nearer-by {:default 1 :doc "Blocks a leg must close for the walk to count as nearer." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 130 :doc "Seconds one call may take: the leg and the waits on the world." :spec (a/num-in 0 nil)}})

(def target [:pos])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def safe? (fn [_] (contract/ok :holds "go-to judges the walk")))
(def fit? safe?)

(def can-start?
  (checks/args-fit "needs a :leg-s and one of :pos, :entity, :player"
                   (fn [c] (let [{:keys [pos entity player leg-s]} (:args c)]
                             (and (number? leg-s) (or pos entity player))))))

(defn thing-pos
  "Where the :entity or :player of the call stands now within the radius setting, else nil."
  [c]
  (let [{:keys [entity player]} (:args c)
        seen (array-seq (.entities (:primitives c) (clj->js (cond-> {:radius (settings/get settings ::radius)} player (assoc :kind "player")))))
        match? (cond entity #(= entity (.-id %))
                     player #(or (= player (.-username %)) (= player (.-name %))))]
    (when match?
      (some-> (first (filter match? seen)) .-pos u/pos-of))))

(defn aim-of
  "The cell {:x :y :z} to walk to: the thing's position now, else :pos."
  [c]
  (some-> (or (thing-pos c) (:pos (:args c))) places/parse-pos :pos))

(defn done?
  [c]
  (let [cell (aim-of c)
        there? (and cell (u/within? (u/self-pos c) cell (:range (:args c)))
                    (or (not (:see (:walk (:args c)))) (aim/body-sees? c [(:x cell) (:y cell) (:z cell)])))]
    (checks/goal there? [:there "within range of the thing"] [:walking "not within range yet"] {:result {:arrived true}})))

(defn quote-of [c]
  (let [leg (:leg-s (:args c))
        aim (aim-of c)]
    (cond-> {:uses {:job-time [0 leg leg]}}
      aim (assoc :moves-to [(:x aim) (:y aim) (:z aim)]))))

(defn leg-end?
  "Whether go-to's ending e is the leg's seconds running out."
  [e]
  (and (= :limit-reached (:kind e)) (= :job-time (:line (:cause e)))))

(defn forward
  "go-to's ending e as this job's own, with e as its :because."
  [c e]
  (contract/with-because (ctx/end c (:kind e) (:cause e) (:data e)) e))

(defn ^:async round
  [c]
  (let [{:keys [range leg-s walk]} (:args c)
        aim (aim-of c)
        before (u/dist (u/self-pos c) aim)
        e (await (child/call-walk* c :walk (-> walk (assoc :pos aim :range range) (update :spend #(merge {:tries 1} % {:job-time leg-s})))))]
    (cond
      (= :continue e) :continue
      (:arrived (:data e)) (ctx/end c :done nil {:arrived true})
      (leg-end? e) (ctx/end c :limit-reached {:reason :leg :line :job-time}
                            {:arrived false :leg true
                             :nearer (< (u/dist (u/self-pos c) aim) (- before (settings/get settings ::nearer-by)))})
      :else (forward c e))))
