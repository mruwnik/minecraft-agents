(ns jobs.lib.threats
  "Mobs that chase: how far each follows its target (vanilla follow_range), and the :threat entries a flight leaves in
  body memory. One :threat entry per mob fled per flight {:mob :id :uuid :key :pos (last known) :ended}, kept
  threat-policy. The third flight from one mob (same :key, uuid else id) within the ttl warns hostile.chased.
  planner-dangers: the known dangers go-to's searches cost (the planner's options.dangers), costed by
  jobs.lib.cost/danger-list."
  (:require [engine.ctx :as ctx]
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

(defn mob-key [e] (or (.-uuid e) (.-id e)))

(defn flights
  "How many :threat entries within the ttl name the mob key k."
  [c k]
  (count (filter #(= k (:key (:data %))) (ctx/entries c :threat))))

(defn remember!
  "Write the :threat entry for a mob fled ({:mob :id :uuid :pos :ended}); at the chased-flights-th from it, warn
  hostile.chased."
  [c {:keys [mob id uuid pos ended]}]
  (let [k (or uuid id)]
    (ctx/remember! c :threat {:mob mob :id id :uuid uuid :key k :pos pos :ended ended} threat-policy)
    (let [n (flights c k)]
      (when (= chased-flights n)
        (ctx/emit! c :hostile.chased :warn
                   {:mob mob :id id :flights n :pos pos
                    :text (str "the " mob " " id " has chased the body " n " times in "
                               (/ (:ttl threat-policy) 60000) " min")})))))

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
                    (mapv (fn [e] {:key (mob-key e) :name (.-name e) :pos (u/pos-of (.-pos e))})
                          (reach/dangers p sensed-radius {:ranged-radius sensed-radius}))
                    remembered))

(defn planner-dangers
  "The planner's options.dangers for the body of c (JS array, nil for none): known-dangers with the :threat spots its
  flights left."
  [c]
  (let [ds (known-dangers (:primitives c) (map :data (ctx/entries c :threat)))]
    (when (seq ds) (clj->js ds))))
