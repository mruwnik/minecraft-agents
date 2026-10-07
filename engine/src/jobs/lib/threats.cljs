(ns jobs.lib.threats
  "Mobs that chase: how far each follows its target (vanilla follow_range), and the :threat entries a flight leaves in
  body memory. One :threat entry per mob fled per flight {:mob :id :uuid :key :pos (last known) :ended}, kept
  threat-policy. The third flight from one mob (same :key, uuid else id) within the ttl warns hostile.chased.
  planner-dangers: the known dangers go-to's searches cost (the planner's options.dangers), costed by
  jobs.lib.cost/danger-list."
  (:require [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(def follow-ranges
  "Blocks a mob keeps chasing its target out to (vanilla follow_range attribute)."
  {"zombie" 35 "husk" 35 "drowned" 35 "zombie_villager" 35 "zombified_piglin" 35
   "enderman" 64 "pillager" 32 "vindicator" 12 "evoker" 12 "blaze" 48 "ghast" 100})

(def default-follow-range 16)

(defn follow-range [mob-name] (get follow-ranges mob-name default-follow-range))

(def max-follow-range (apply max (vals follow-ranges)))

(def threat-policy {:cap 20 :ttl (* 5 60 1000)})

(def chased-flights "Flights from one mob within the ttl that tell the agent." 3)

(defn place-of
  "Where the body knows mob e of primitives p to be: {:pos} for one it has seen, else (heard only) what a sound tells,
  {:direction :band :from} (obs/rough-hearing from the body's place), never the exact place."
  [p e]
  (if (reach/seen-only? e)
    {:pos (u/pos-of (.-pos e))}
    (let [from (u/pos-of (.-pos (.self p)))]
      (assoc (obs/rough-hearing from (u/pos-of (.-pos e))) :from from))))

(def rough-pos reach/rough-pos)

(defn mob-key [e] (or (.-uuid e) (.-id e)))

(defn flights
  "How many :threat entries within the ttl name the mob key k."
  [c k]
  (count (filter #(= k (:key (:data %))) (ctx/entries c :threat))))

(defn remember!
  "Write the :threat entry for a mob fled ({:mob :id :uuid :ended and :pos, or :direction :band :from}); at the chased-flights-th from it, warn
  hostile.chased."
  [c {:keys [mob id uuid ended] :as t}]
  (let [k (or uuid id)
        place (select-keys t [:pos :direction :band :from])]
    (ctx/remember! c :threat (merge {:mob mob :id id :uuid uuid :key k :ended ended} place) threat-policy)
    (let [n (flights c k)]
      (when (= chased-flights n)
        (ctx/emit! c :hostile.chased :warn
                   (merge {:mob mob :id id :flights n}
                          (select-keys place [:pos :direction :band])
                          {                    :text (str "the " mob " " id " has chased the body " n " times in "
                               (/ (:ttl threat-policy) 60000) " min")}))))))

;; ---------------------------------------------------------------- dangers for the planner

(def sensed-radius "Blocks out to which a sensed real danger (jobs.lib.reach/dangers) is costed." 24)

(defn body-of [p]
  (let [self (.self p)]
    {:health (.-health self) :equipment (cost/equipment-of (.-equipment self))
     :weapon (combat/best-weapon p combat/default-weapons) :pos (u/pos-of (.-pos self))}))

(defn known-dangers
  "danger-list of the body of primitives p: the real dangers it senses within sensed-radius (jobs.lib.reach/dangers:
  seen or heard, with a way to the body or a line of fire; never x-ray) and the remembered :threat entries' data."
  [p remembered]
  (cost/danger-list (body-of p)
                    (mapv (fn [e] {:key (mob-key e) :name (.-name e) :pos (reach/mob-pos p e)})
                          (reach/dangers p sensed-radius {:ranged-radius sensed-radius}))
                    remembered))

(defn planner-dangers
  "The planner's options.dangers for the body of c (JS array, nil for none): known-dangers with the :threat spots its
  flights left."
  [c]
  (let [spots (keep #(when-let [pos (rough-pos (:data %))] (assoc (:data %) :pos pos)) (ctx/entries c :threat))
        ds (known-dangers (:primitives c) spots)]
    (when (seq ds) (clj->js ds))))
