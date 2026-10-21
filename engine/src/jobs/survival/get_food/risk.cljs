(ns jobs.survival.get-food.risk
  "Whether a fragile body (jobs.lib.foods/fragile?: hurt, hungry, no food carried, so waiting cannot heal it) may walk out
  for food. Where the sky is dark above ground or a hostile is known near (jobs.lib.dawn/hostile-known?) it walks only to food
  within foods/::starving-walk-range (no search of unknown ground), and never where the danger of the hostiles it knows along a straight way
  (jobs.lib.cost/route-danger) is its health or more."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.foods :as foods]
            [jobs.lib.reach :as reach]
            [jobs.lib.ready :as ready]
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

(defn calm-now?
  "Whether the sky is not dark above ground and no hostile is known near."
  [p c]
  (and (not= :later (first (ready/dark-sky c))) (not (dawn/hostile-known? p))))

(defn refusal
  "Why a fragile body does not walk to pos (nil pos: a search of unknown ground) now, as text, else nil."
  [c pos]
  (let [p (:primitives c)
        self (.self p)
        health (.-health self)
        at (str "health " (.toFixed health 1))]
    (cond
      (not (foods/fragile? self)) nil
      (and (nil? pos) (not (calm-now? p c)))
      (str at ": will not search unknown ground in the dark or with a hostile known near")
      (and pos (not (calm-now? p c)) (> (u/dist (u/pos-of (.-pos self)) pos) (foods/starving-walk-range)))
      (str at ": food at " (pr-str (mapv js/Math.floor [(:x pos) (:y pos) (:z pos)])) " is out of the walk a body that cannot heal risks in the dark or with a hostile known near")
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
