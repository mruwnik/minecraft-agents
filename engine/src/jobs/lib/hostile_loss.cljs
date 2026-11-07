(ns jobs.lib.hostile-loss
  "What doing nothing about the dangers near costs the body (cost/stand-loss over a respond call): the hostile-near
  trigger holds only while it is positive, and respond-to-hostile is worth it; too-hurt? counts it in the low-health
  comparison."
  (:require [engine.args :as a]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.threat-field :as threat-field]
            [jobs.lib.game-facts :as game-facts]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shot :as shot]
            [jobs.lib.util :as u]))

(def call-deciles? (a/pred "10 seconds" #(and (vector? %) (= 10 (count %)) (every? (fn [x] (and (a/finite? x) (>= x 0))) %))))

(a/defargs settings
  {::call-s {:default {:n 1413 :deciles [0.47 0.82 1.94 3.06 4.47 6.15 9.36 15.11 15.62 44.75]}
             :doc "Seconds a respond-to-hostile call lasts, its deciles at (i + 0.5) / 10: the stays a call's loss is priced over (loss), the :setting of node tools/event-counts.mjs respond-calls (each hostile-near reflex, fired to ended, of the trial bodies)."
             :spec (a/pred "{:n :deciles}" #(and (map? %) (call-deciles? (:deciles %))))}})

(defn call-s
  "The ten equally likely seconds a respond call lasts (::call-s)."
  []
  (:deciles (settings/get settings ::call-s)))

(defn mean-call-s [] (let [s (call-s)] (/ (reduce + s) (count s))))

(defn mob-near
  "Hostile e as the answers take a mob: {:name :held :distance :burns (a melee mob of burns-in-sun: whether the sun burns it where it stands, nil when unknown) :speed (its chase speed) :follow (its follow range)}; a
  seen mob's distance from a body afloat at the surface is from the surface cell's floor (breath/start-y), not the bob."
  [p e]
  {:name (.-name e) :held (.-held e)
   :distance (danger-q/mob-distance p e (when (and (breath/swimming? p) (not (breath/head-under? p)) (danger-q/seen-only? e))
                                        (assoc (u/pos-of (.-pos (.self p))) :y (breath/start-y p))))
   :burns (let [at (update-vals (danger-q/mob-pos p e) js/Math.floor)]
            (when (and (contains? threat/burns-in-sun (.-name e)) (not (threat/ranged? {:name (.-name e) :held (.-held e)})) (sh-sense/sky-state p) (sh-sense/sky-light-at p at))
              (boolean (and (death-spot/bare-headed? e) (death-spot/burns-by-day? p (.-name e) at)))))
   :speed (cost/chase-speed (cost/mob-kind e)) :follow (threat/follow-range (.-name e))})

(defn blast-s*
  [p own?]
  (let [{:keys [x y z]} (u/pos-of (.-pos (.self p)))
        [fx fy fz] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]
        sensed (fn [[x y z]] (u/sensed p {:x x :y y :z z}))
        destroyed (cost/destroyed-fn sensed)
        blocks (.-blocksByName (game-facts/data @game/version))
        r (js/Math.ceil cost/blast-reach)]
    (cost/blast-env-s
     (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))
           :let [d (js/Math.hypot (- (+ fx dx 0.5) x) (- (+ fy dy 0.5) (+ y 0.5)) (- (+ fz dz 0.5) z))]
           :when (< d cost/blast-reach)
           :let [cell [(+ fx dx) (+ fy dy) (+ fz dz)]
                 b (sensed cell)
                 n (when (and b (not (true? (.-unknown b)))) (.-name b))
                 worth (if n (destroyed n cell (own? cell n)) 0)]
           :when (pos? worth)]
       [d (or (some-> (aget blocks n) .-resistance) 0) worth]))))

(def blast-memo (atom nil))

(defn blast-s
  "Seconds a creeper's blast at the body's feet costs what is about it (cost/blast-env-s): each sensed cell within
  cost/blast-reach at what destroying it costs (cost/destroyed-fn: a block the body placed itself (ledger/own-placed
  of its memory view) or one that looks built, a chest, farmland and crops; natural terrain nothing) and its blast
  resistance. Kept while the body stays in its cell and neither the world it knows nor its placed blocks change."
  [p view]
  (let [self (u/pos-of (.-pos (.self p)))
        key [(threat-field/world-epoch p) (mapv #(js/Math.floor %) [(:x self) (:y self) (:z self)])
             (count (mem/entries view :escalation-placed)) (hash (ledger/open-entries view))]
        hit @blast-memo]
    (if (and (some? (first key)) (identical? p (:p hit)) (= key (:key hit)))
      (:value hit)
      (let [v (blast-s* p (ledger/own-placed view))]
        (reset! blast-memo {:p p :key key :value v})
        v))))

(defn body-at
  "What the answers' prices read of the body of p (memory view): health, absorption, food, what a death costs it
  (cost/death-body) and, with a creeper known near, what its blast breaks about the body (:blast-s, blast-s)."
  [p view]
  (let [self (.self p)]
    (cond-> (assoc (cost/death-body p view)
                   :health (.-health self) :absorption (.-absorption self) :food (or (.-food self) 20))
      (some #(= "creeper" (.-name %)) (danger-q/known-hostiles p (danger-q/hostile-radius) {})) (assoc :blast-s (blast-s p view)))))

(defn mobs-of
  "Hostiles hs (JS entities) of p as the answers take them (mob-near) and, while no shooter of hs is seen, one of each kind
  (shot/shooter-of) whose shot from a shooter not seen has a line of fire (shot/view-spots), where the shot came from."
  [p view hs]
  (cond-> (mapv #(mob-near p %) hs)
    (not-any? #(and (danger-q/seen-only? %) (threat/ranged? {:name (.-name %) :held (.-held %)})) hs)
    (into (map (fn [k] {:name k :distance (settings/get shot/settings ::shot/reach)})) (distinct (map :shooter (shot/view-spots p view))))))

(defn loss-over
  "Seconds doing nothing about hostiles hs (JS entities) of p for seconds (a number, or a spread as cost/stand-loss
  takes) costs the body: cost/stand-loss of mobs-of them."
  [p view hs seconds]
  (let [mobs (mobs-of p view hs)]
    (if (empty? mobs)
      0
      (cost/stand-loss (body-at p view) (cost/equipment-of (.-equipment (.self p))) mobs (u/pos-of (.-pos (.self p))) seconds))))

(defn loss
  "Seconds doing nothing about hostiles hs costs the body over a respond call (loss-over the spread of call-s)."
  [p view hs]
  (loss-over p view hs (call-s)))

(defn present-fn
  "fn of a health h: what the hostiles the body knows of near (danger/known-hostiles within the hostile radius, shooters
  within their fire range) cost a body of p at h over a respond call (loss, with its health h); nil when none is known."
  [p view]
  (let [mobs (mobs-of p view (danger-q/known-hostiles p (danger-q/hostile-radius) {:ranged-radius :fire-range}))]
    (when (seq mobs)
      (let [body (body-at p view)
            equipment (cost/equipment-of (.-equipment (.self p)))
            at (u/pos-of (.-pos (.self p)))]
        (fn [h] (cost/stand-loss (assoc body :health h) equipment mobs at (call-s)))))))

(defn too-hurt?
  "cost.risk/too-hurt? of the body of p now for a job's work of category (`seconds` its own quoted stay, nil: the category's),
  the hostiles it knows of near counted (present-fn): the one low-health comparison (Q140)."
  [p view category seconds]
  (risk/too-hurt? (.-health (.self p)) category (cost/death-fn p view) {:seconds seconds :present (present-fn p view)}))

(defn hostile-near?
  "Whether the hostile-near trigger holds (args :radius, :ranged-radius, :visible-only): the loss of doing nothing about
  its dangers (danger/hostile-dangers) is positive. A dead body sees no danger."
  [world memory args]
  (boolean (and (not (body/dead? memory))
                (pos? (loss world memory (danger-q/hostile-dangers world memory args))))))
