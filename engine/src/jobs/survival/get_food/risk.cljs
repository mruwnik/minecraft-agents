(ns jobs.survival.get-food.risk
  "Whether a fragile body (jobs.lib.foods/fragile?) may walk out for food. Death is a cost like any other, but a body on
  its last hp loses everything to one hit, so it walks out only on a calm day (jobs.lib.dawn/calm?: not at night, past
  the dawn grace, no hostile known within its radius) and not where the danger of the hostiles it knows along a straight
  way (jobs.lib.cost/route-danger) is its health or more."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.foods :as foods]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(defn walk-danger
  "route-danger of the straight way from the body to pos past the hostiles it knows."
  [p pos]
  (let [kind-at (reach/lookup p)
        self (.self p)
        mobs (map (fn [e] {:name (.-name e) :pos (danger-q/mob-pos p e)}) (danger-q/seen-hostiles p))]
    (cost/route-danger (game/version-of p) kind-at (conj (cost/straight-route kind-at (u/pos-of (.-pos self)) pos) pos)
                       mobs (.-equipment self)
                       :weapon (combat/best-weapon p combat/default-weapons) :health (.-health self))))

(defn refusal
  "Why a fragile body does not walk to pos (nil pos: a search of unknown ground) now, as text, else nil."
  [c pos]
  (let [p (:primitives c)
        self (.self p)
        health (.-health self)
        at (str "health " (.toFixed health 1))]
    (cond
      (not (foods/fragile? self)) nil
      (or (sh-sense/night? p) (not (dawn/calm? p)))
      (str at ": waits sheltered for a calm day (night, first light or a hostile known near)")
      (nil? pos) nil
      :else (let [{:keys [danger mobs]} (walk-danger p pos)]
              (when (>= danger health)
                (str at ": the way to " (pr-str (mapv js/Math.floor [(:x pos) (:y pos) (:z pos)])) " passes "
                     (->> mobs (map :name) distinct (str/join ", ")) ", which would likely kill it"))))))

(defn refused?
  "refusal, kept in job memory (:too-dangerous) for the ending when nothing else feeds the body."
  [c pos]
  (when-let [why (refusal c pos)]
    (ctx/update-mem! c assoc :too-dangerous why)
    true))
