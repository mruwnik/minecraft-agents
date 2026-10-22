(ns jobs.lib.death-spot
  "Where the body died lately, by any cause, from the :deaths memory (engine.core/record-death!), and where it was hurt down to ::near-death-hp:
  a place a player avoids or goes back to prepared. A death or near-death weighs 1 when fresh and fades to 0 (jobs.lib.cost/death-weight);
  it counts for cells within ::radius of its spot. A near-death fades over the :hurt memory's own ttl, and counts once with a death or newer near-death within ::merge-radius."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::near-death-hp {:default 6 :doc "Health at or under which a hit makes its spot count like a death." :spec (a/num-in 0 nil)}
   ::merge-radius {:default 8 :doc "Blocks within which near-death hits are one spot (the newest)." :spec (a/num-in 0 nil)}
   ::radius {:default 24 :doc "Blocks around a death spot that the death still counts for." :spec (a/num-in 0 nil)}})

(defn mob-cause?
  "Whether `cause` (the killer's name, else a damage type like fall or lava) is a mob the danger code knows; any other is a death by terrain."
  [p cause]
  (boolean (and cause (or (contains? threat/mob-hit cause) (threat/hostile? (game/version-of p) {:name cause})))))

(defn hurt-cause
  "What hurt the body in a :hurt entry's data: the attacker's name, else the damage type, else the block cause."
  [{:keys [attacker damageType cause]}]
  (or (:name attacker) (some-> damageType (str/replace #"^minecraft:" "")) cause))

(defn near-deaths
  "The :hurt entries of c that left the body at or under ::near-death-hp and carry a :pos, newest first, one per ::merge-radius and none within it of a death."
  [c]
  (let [limit (settings/get settings ::near-death-hp)
        merge-r (settings/get settings ::merge-radius)
        deaths (keep #(:pos (:data %)) (ctx/entries c :deaths))]
    (reduce (fn [kept {:keys [data] :as e}]
              (if (or (some #(<= (u/dist (:pos data) %) merge-r) deaths)
                      (some #(<= (u/dist (:pos data) (:pos (:data %))) merge-r) kept))
                kept
                (conj kept e)))
            []
            (->> (ctx/entries c :hurt)
                 (filter #(let [{:keys [health pos]} (:data %)] (and pos health (<= health limit))))
                 reverse))))

(defn recent
  "The deaths and near-deaths of c still remembered: [{:pos {:x :y :z} :cause :mob? :weight :age-ms}], weight falling from 1 to 0 with age, mob? by mob-cause?."
  [c]
  (let [now (ctx/now c)
        hurt-ms (let [ttl (:ttl (or (mem/policy (ctx/view c) :hurt) mem/default-policy))] (when (number? ttl) ttl))
        spot (fn [t data fade-ms]
               (let [age (- now t) w (if fade-ms (cost/death-weight age fade-ms) (cost/death-weight age)) {:keys [x y z]} (:pos data)
                     cause (if (:death-t data) (:cause data) (hurt-cause data))]
                 (when (and (pos? w) x)
                   {:pos {:x x :y y :z z} :cause cause :mob? (mob-cause? (:primitives c) cause) :weight w :age-ms age})))]
    (vec (concat (keep #(spot (:death-t (:data %)) (:data %) nil) (ctx/entries c :deaths))
                 (keep #(spot (:t %) (:data %) hurt-ms) (near-deaths c))))))

(defn near
  "The remembered deaths of `deaths` (from recent) within ::radius of cell `pos`."
  [deaths pos]
  (filterv #(<= (u/dist pos (:pos %)) (settings/get settings ::radius)) deaths))

(defn lit?
  "Whether the body has seen the cell at pos and senses it lit enough that no mob spawns there (shelter.sense/lit-here?)."
  [p pos]
  (boolean (and (sh-sense/cell-light p pos) (sh-sense/lit-here? p pos))))

(defn seen-clear?
  "Whether a mob death's spot is clear: looked at since the death (fresh sight beats the record) and lit, as a dark spot spawns the killer again.
  A death by terrain never clears by sight: the place itself killed."
  [p {:keys [pos age-ms mob?]}]
  (let [b (look/seen-block p pos)]
    (boolean (and mob? b (not (:unknown b)) (< (:age-ms b) age-ms) (lit? p pos)))))

(defn uncleared
  "The remembered deaths of c not seen-clear?."
  [c]
  (remove #(seen-clear? (:primitives c) %) (recent c)))
