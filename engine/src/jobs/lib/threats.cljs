(ns jobs.lib.threats
  "Mobs that chase: how far each follows its target (vanilla follow_range), and the :threat entries a flight leaves in
  body memory. One :threat entry per mob fled per flight {:mob :id :uuid :key :pos (last known) :ended}, kept
  threat-policy. The third flight from one mob (same :key, uuid else id) within the ttl warns hostile.chased.
  planner-dangers: the known dangers go-to's searches cost (the planner's options.dangers)."
  (:require [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
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
(def max-dangers 8)
(def danger-shape
  "close and radius of a danger's cost (planner options.dangers): full within close blocks, 0 past radius. A remembered
  spot is wider: the mob has moved on from it."
  {:sensed {:close 3 :radius 12} :remembered {:close 3 :radius 16} :creeper {:close 4 :radius 8}})
(def stances "Rate factor: :flee from a mob costs the body far more than one it would :fight." {:flee 4 :fight 0.1})
(def max-rate "hp a second one danger costs at most (the planner caps all of them together at its dangerCap, 4)." 4)
(def reserve "Health a fight must leave (respond-to-hostile's default :reserve)." 4)

(defn stance
  "What the hostile reflex would do about mob-name alone (combat/decide): :fight or :flee."
  [{:keys [health armour weapon]} mob-name]
  (combat/decide {:health health :reserve reserve :creeper? (= "creeper" mob-name)
                  :damage (combat/fight-damage {:weapon weapon :armour armour :mobs [{:name mob-name :distance 0}]})}))

(defn danger-rate
  "hp a second near mob-name costs body {:health :armour :weapon}: its dps after armour, times its stance factor, times
  20 / health (at most 4), at most max-rate."
  [{:keys [health armour] :as body} mob-name]
  (min max-rate
       (* (get combat/mob-dps mob-name 3)
          (/ (- 100 (* 4 (min 20 (or armour 0)))) 100)
          (get stances (stance body mob-name))
          (min 4 (/ 20 (max 1 health))))))

(defn danger-of [body kind {:keys [name pos]}]
  (merge {:x (:x pos) :y (:y pos) :z (:z pos) :rate (danger-rate body name) :mob name}
         (get danger-shape (if (= "creeper" name) :creeper kind))))

(defn danger-list
  "The dangers ({:x :y :z :close :radius :rate :mob}) of the sensed mobs [{:key :name :pos}] (nearest first) and the
  remembered :threat entries' data [{:key :mob :pos}] (nearest the body first; one whose :key is sensed now is left out:
  the sensed place wins), at most max-dangers. body {:health :armour :weapon :pos}."
  [body sensed remembered]
  (let [now (set (map :key sensed))
        spots (->> remembered
                   (remove #(contains? now (:key %)))
                   (filter :pos)
                   (sort-by #(u/dist (:pos body) (:pos %))))]
    (vec (take max-dangers (concat (map #(danger-of body :sensed %) sensed)
                                   (map #(danger-of body :remembered {:name (:mob %) :pos (:pos %)}) spots))))))

(defn body-of [p]
  (let [self (.self p)]
    {:health (.-health self) :armour (combat/armour-points (.-equipment self))
     :weapon (combat/best-weapon p combat/default-weapons) :pos (u/pos-of (.-pos self))}))

(defn known-dangers
  "danger-list of the body of primitives p: the real dangers it senses within sensed-radius (jobs.lib.reach/dangers:
  seen or heard, with a way to the body or a line of fire; never x-ray) and the remembered :threat entries' data."
  [p remembered]
  (danger-list (body-of p)
               (mapv (fn [e] {:key (mob-key e) :name (.-name e) :pos (u/pos-of (.-pos e))})
                     (reach/dangers p sensed-radius {:ranged-radius sensed-radius}))
               remembered))

(defn planner-dangers
  "The planner's options.dangers for the body of c (JS array, nil for none): known-dangers with the :threat spots its
  flights left."
  [c]
  (let [ds (known-dangers (:primitives c) (map :data (ctx/entries c :threat)))]
    (when (seq ds) (clj->js ds))))
