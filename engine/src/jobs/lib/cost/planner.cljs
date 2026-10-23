(ns jobs.lib.cost.planner
  "The prices go-to's :costs may override, in seconds: the planner's per-move costs (engine.path.planner.base/DEFAULT-COSTS
  holds the defaults, which apply to every price left out). A drop's price is go-to's :drop-cost, a hurt hp's :hp-seconds.
  :lethal-air is no price: 1 lets a swim to air drown the body to death when no other way exists (breathe's), 0 refuses it (:air-lethal); :drown-slack is the seconds of margin in its count of 2 hp hits (ceil(seconds + slack)). Left out: airSupply, airLimit and airUsed (the body's breath, a fact, not a price), airDrain and airGrace (its helmet, see air-profile) and dropFactor (:drop-cost)."
  (:require [engine.args :as a]
            [engine.path.blocks :as blocks]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.lib.cost.armour :as armour]
            [engine.path.planner.base :as base]))

(def walk-floor
  "Seconds a block of walking costs: the planner's heuristic counts this per horizontal block, so a swim-h or exit price below it
  would make the search inadmissible (the spec refusal names it)."
  base/WALK-S)

(def swim-sub-ms
  "Milliseconds a swim block takes with the head under water (the planner's swimSubH): breathe's trigger prices the swim to air the same."
  (* 1000 (unchecked-get base/DEFAULT-COSTS "swimSubH")))

(def floored
  "The :costs keys that stand in for a block of horizontal walking and may not go under walk-floor."
  #{:swim-h :swim-sub-h :exit})

(def planner-names
  "The :costs keys and the planner cost each sets."
  {:climb-up "climbUp" :climb-down "climbDown" :jump-climb "jumpClimb" :open "open" :swim-h "swimH" :swim-sub-h "swimSubH" :swim-up "swimUp"
   :swim-down "swimDown" :exit "exit" :current "current" :bubble-up "bubbleUp" :bubble-down "bubbleDown"
   :open-redstone "openRedstone" :open-lever "openLever" :open-plate "openPlate" :beside-magma-column "besideMagmaColumn"
   :max-water-drop "maxWaterDrop" :dripleaf "dripleaf" :dripleaf-risk "dripleafRisk" :lethal-air "lethalAir"
   :drown-slack "drownSlack" :descent "descentS" :gap-miss "gapMiss"})

(defn water-breathing-s
  "Seconds of Water Breathing left in self's :effects (their :left ticks), 0 when none or infinite."
  [effects]
  (/ (->> effects (filter #(= "water_breathing" (.-name %))) (map #(max 0 (or (.-left %) 0))) (reduce max 0)) 20))

(defn air-profile
  "{:air-drain :air-grace} of the body's helmet, a key only where it differs from no gear. equipment: self's :equipment
  (cljs or JS). Respiration n drains 1/(n+1) a second (the expected value); a turtle helmet gives each dive 10 s in which
  the air does not drain. Water Breathing adds its seconds left (an effect's :left ticks, from self's :effects) to the
  grace: one pool counted from the dive start, so the walk to the dive is not taken off it. Conduit Power is left out (it
  lasts ~13 s once out of the conduit's range)."
  [equipment effects]
  (let [head (get (armour/equipment-of equipment) "head")
        pool (water-breathing-s effects)
        respiration (->> (:enchants head) (some #(when (= "respiration" (:name %)) (:level %))))]
    (cond-> {}
      (pos? (or respiration 0)) (assoc :air-drain (/ 1 (inc respiration)))
      (or (= "turtle_helmet" (:name head)) (pos? pool))
      (assoc :air-grace (+ (if (= "turtle_helmet" (:name head)) 10 0) pool)))))

(defn swim-s
  "Seconds a block of swimming at the surface takes with Depth Strider level (vanilla water travel: inertia 0.8 and
  acceleration 0.02 move toward 0.546 and the walking speed 0.1 by level/3, max 3, half that off the ground)."
  [level]
  (let [e (/ (min level 3) 6)
        inertia (+ 0.8 (* (- 0.546 0.8) e))
        accel (+ 0.02 (* (- 0.1 0.02) e))]
    (/ (- 1 inertia) (* 0.98 20 accel))))

(defn depth-strider
  "The Depth Strider level of the boots in equipment (self's :equipment, cljs or JS), 0 none."
  [equipment]
  (or (->> (:enchants (get (armour/equipment-of equipment) "feet"))
           (some #(when (= "depth_strider" (:name %)) (:level %))))
      0))

(defn swim-speed
  "Blocks a second the body swims at the surface wearing equipment (swim-s of its Depth Strider): about 2 with none."
  [equipment]
  (/ 1 (swim-s (depth-strider equipment))))

(defn swim-profile
  "{:swim-h :swim-sub-h} for Depth Strider on the boots (none: {}): the planner's swimH and swimSubH (the sprint swim with the head under water) scaled as the surface swim speeds up, about
  0.73 at level 1, 0.61 at 2 and 0.55 at 3, never under walk-floor. equipment: self's :equipment (cljs or JS)."
  [equipment]
  (let [level (depth-strider equipment)]
    (if (pos? level)
      (let [scale (/ (swim-s level) (swim-s 0))]
        {:swim-h (max walk-floor (* (unchecked-get base/DEFAULT-COSTS "swimH") scale))
         :swim-sub-h (max walk-floor (* (unchecked-get base/DEFAULT-COSTS "swimSubH") scale))})
      {})))

(defn air-used
  "The seconds of air a body with oxygen (0-20 bubbles, 15 s; unknown: full) has used; mineflayer rounds the air to a
  bubble, so 0.375 s (half a bubble) more."
  [oxygen]
  (if (number? oxygen) (+ (* (- 20 oxygen) 0.75) 0.375) 0))

(defn air-costs
  "The planner's options.costs airDrain (with airLimit, the supply less the default margin in seconds at that drain), airGrace, airUsed and swimH (a JS object) for a policy's :air-drain, :air-limit (a cap that wins over the drain's),
  :air-grace, :air-used and :swim-h, those it has."
  [policy]
  (let [o #js {}]
    (when-some [h (:swim-h policy)] (unchecked-set o "swimH" h))
    (when-some [h (:swim-sub-h policy)] (unchecked-set o "swimSubH" h))
    (when-some [d (:air-drain policy)]
      (unchecked-set o "airDrain" d)
      ;; the planner's 3 s margin under the supply is seconds of time, so Respiration shrinks it with the drain
      (let [supply (unchecked-get base/DEFAULT-COSTS "airSupply")
            margin (- supply (unchecked-get base/DEFAULT-COSTS "airLimit"))]
        (unchecked-set o "airLimit" (- supply (* margin d)))))
    (when-some [l (:air-limit policy)] (unchecked-set o "airLimit" l))
    (when-some [g (:air-grace policy)] (unchecked-set o "airGrace" g))
    (when-some [u (:air-used policy)] (unchecked-set o "airUsed" u))
    o))

(defn planner-costs-problem
  "Why costs is not a usable go-to :costs (nil, or a map of planner-names keys to finite numbers >= 0), else nil."
  [costs]
  (cond
    (nil? costs) nil
    (not (map? costs)) (str ":costs must be a map of price name to seconds, got " (pr-str costs))
    :else (some (fn [[k v]]
                  (cond
                    (not (contains? planner-names k)) (str ":costs has no price " (pr-str k) ", the prices are " (pr-str (vec (keys planner-names))))
                    (not (and (number? v) (js/isFinite v) (>= v 0))) (str ":costs " k " must be a number >= 0, got " (pr-str v))
                    (and (floored k) (< v walk-floor)) (str ":costs " k " must be at least " walk-floor " s (the cost of walking a block: the planner's search assumes no move is quicker), got " v)))
                costs)))

(defn planner-costs
  "The planner's options.costs (a JS object) for the go-to :costs map."
  [costs]
  (let [o #js {}]
    (doseq [[k v] costs] (unchecked-set o (planner-names k) v))
    o))

(def gaits
  "go-to's :gait values: :auto (sprints where the executor does), :walk (never sprints), :sneak (sneaks on level steps, never sprints)."
  #{:auto :walk :sneak})

(defn gait-problem
  "Why gait is not a usable go-to :gait (nil, or one of gaits), else nil."
  [gait]
  (when-not (or (nil? gait) (contains? gaits gait))
    (str ":gait must be one of " (pr-str (sort gaits)) ", got " (pr-str gait))))

(defn gait-costs
  "The planner's options.costs walkS and sprintS (seconds per block walked and per block of a gap jump) for gait: a walking
  body runs a gap jump at walking speed, a sneaking one walks at sneak speed; :auto: the planner's defaults."
  [gait]
  (case gait
    :walk #js {:sprintS base/WALK-S :swimSubH js/Infinity}
    :sneak #js {:walkS base/SNEAK-S :sprintS base/SNEAK-S :swimSubH js/Infinity}
    #js {}))

(def default-landing
  "Block name -> share of a fall's damage a body takes landing on it (go-to's :landing, which overrides entry by entry): hay
  and honey take 80% off, a bed 50% (the server halves the fall, so this overstates it a little); a negative factor (slime) is a
  bounce: no damage, but the body takes seconds to settle."
  (into {"hay_block" 0.2 "honey_block" 0.2 "slime_block" -1}
        (map #(vector (str % "_bed") 0.5))
        ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray" "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"]))

(defn landing-problem
  "Why landing is not a usable go-to :landing (nil, or a map of block name to a finite number), else nil."
  [landing]
  (let [table (blocks/default-state-table)]
    (cond
      (nil? landing) nil
      (not (map? landing)) (str ":landing must be a map of block name to damage factor, got " (pr-str landing))
      :else (some (fn [[k v]]
                    (cond
                      (not (and (string? k) (seq (blocks/state-ids table k)))) (str ":landing has no block " (pr-str k))
                      (not (and (number? v) (js/isFinite v))) (str ":landing " k " must be a number, got " (pr-str v))))
                  landing))))

(defn planner-landing
  "The planner's options.landing (a Map of state id to damage factor) for the go-to :landing map over default-landing. Under
  the :sneak gait a negative factor is 1: a sneaking body does not bounce, it takes the full fall."
  ([landing] (planner-landing landing nil))
  ([landing gait]
   (let [table (blocks/default-state-table)
         m (js/Map.)]
     (doseq [[k v] (merge default-landing landing)
             id (blocks/state-ids table k)]
       (.set m id (if (and (= :sneak gait) (neg? v)) 1 v)))
     m)))

(def unknown-prices
  "The planner's prices of stands on floors the body has not seen (options.unknown), seconds: :peek-s a drop onto one (a stop
  at the edge to look), :aim-s a level or up step onto one (a look down at the takeoff); and the depths, in blocks:
  :see-lit-depth the deepest drop a lit landing is seen over from the walk's worst peek stop, :light-depth the deepest a
  torch at the takeoff lights. The price of the unseen cells themselves is jobs.lib.cost/unseen-share."
  {:peek-s 0.7 :aim-s 0.1 :see-lit-depth 10 :light-depth 5})

(a/defargs settings
  {::drop-cost {:default 1 :doc "Scales the cost of a drop (fall seconds and damage; 1 as is, 0 free, 5 dear); false: no drop of 2 or 3 at all. :one-way :closed instead refuses only a drop the body cannot climb back." :spec (a/or-of (a/num-in 0 nil) false?)}
   ::costs {:default false :doc (str "Map of price name to seconds, overriding the planner's price of a move (" (str/join " " (map name (keys planner-names))) "); a price left out keeps the planner's default. false: none.")
            :spec (a/or-of false? (a/valid-by "a map of planner price name to a number of seconds >= 0" #(planner-costs-problem %)))}
   ::lethal-air {:default false :doc "true lets a plan drown the body when no other way to air exists (breathe's); false: such a plan is refused (:why :air-lethal)." :spec boolean?}
   ::landing {:default false :doc "Map of block name to the share of a fall's damage a landing on it takes (0.2: 80% off), negative: a bounce, no damage but settle time (only onto a pad in view, bouncing blocks or a wall all round, not under :gait :sneak); entries override the defaults hay_block 0.2, honey_block 0.2, every bed 0.5 and slime_block -1."
              :spec (a/or-of false? (a/valid-by "a map of block name to a number" #(landing-problem %)))}
   ::wet-pace {:default {:swim 1.5 :swim-up 2.5 :swim-down 2.5 :climb-up 2.6 :climb-down 2.5}
               :doc "Map of move kind (:swim :swim-up :swim-down :climb-up :climb-down) to how many times its planner price a step of it really takes under water in a walk over unseen cells, which stops to look and replan (wet-move-s); :swim and :climb-up measured live, the others left at the safer 2.5 until measured."
               :spec (a/pred "a map of :swim :swim-up :swim-down :climb-up :climb-down to numbers > 0" #(and (map? %) (= #{:swim :swim-up :swim-down :climb-up :climb-down} (set (keys %))) (every? (fn [v] (and (number? v) (pos? v))) (vals %))))}
   ::wet-margin-s {:default 2 :doc "Seconds of air a walk over unseen water keeps for when it is back at its last breath (wet-ok?)." :spec (a/num-in 0 nil)}
   ::unknown-prices {:default false :doc "Map of :peek-s :aim-s :see-lit-depth :light-depth to numbers overriding the prices of cells a walk over :unknown has not seen (unknown-prices); a go-to :unknown map wins over it." :spec (a/or-of false? (a/valid-by "a map of unknown-price name to a number >= 0" (fn [m] (when-not (or (nil? m) (and (map? m) (every? #(and (contains? unknown-prices (key %)) (number? (val %)) (>= (val %) 0)) m))) "not a map of unknown-price names to numbers >= 0"))))}})

(def wet-prices
  "Per move under water: the planner price of one step (a level step swims with the head under, swimSubH), the kind of
  slowdown (wet-pace) it runs at, and the move that undoes it on the way back."
  {:swim ["swimSubH" :swim :swim] :walk ["swimSubH" :swim :walk] :diagonal ["swimSubH" :swim :diagonal] :corner ["swimSubH" :swim :corner]
   :swim-up ["swimUp" :swim-up :swim-down] :swim-down ["swimDown" :swim-down :swim-up] :drop ["swimDown" :swim-down :swim-up]
   :exit ["exit" :swim :swim] :climb-up ["climbUp" :climb-up :climb-down] :climb-down ["climbDown" :climb-down :climb-up]
   :jump-climb ["jumpClimb" :climb-up :climb-down]})

(defn policy-gait "The gait a walk under policy has: its :gait, :walk when it does not sprint." [policy]
  (or (:gait policy) (when (false? (:sprint policy)) :walk)))

(defn policy-costs
  "The planner's options.costs (a JS object) a walk under policy gets: the defaults, the gait's, the body's air and swim, then the policy's :costs."
  [policy]
  (js/Object.assign #js {} base/DEFAULT-COSTS (gait-costs (policy-gait policy)) (air-costs policy) (planner-costs (:costs policy))))

(defn wet-move-s
  "Real seconds a body takes for one step of move under water: the price the planner has for it under policy (policy-costs)
  times the setting ::wet-pace of its kind; a move without a price (the start) takes none. As in the planner, swimSubH is at
  most swimH: a body that does not sprint (swimSubH infinite) swims a level cell at swimH."
  [policy move]
  (if-let [[price kind] (get wet-prices move)]
    (let [costs (policy-costs policy)
          s (cond-> (unchecked-get costs price)
              (= "swimSubH" price) (min (unchecked-get costs "swimH")))]
      (* s (get (settings/get settings ::wet-pace) kind)))
    0))

(defn air-left-s
  "Seconds a body under policy (:air-used, :air-drain, :air-grace: the body's) may still stay under water."
  [policy]
  (+ (or (:air-grace policy) 0)
     (/ (- (unchecked-get base/DEFAULT-COSTS "airSupply") (or (:air-used policy) 0)) (or (:air-drain policy) 1))))

(defn wet-way-s "Real seconds of the wet moves one way (wet-move-s)." [policy moves] (reduce + 0 (map #(wet-move-s policy %) moves)))

(defn wet-spare-s
  "Seconds of air a body under policy keeps when it goes on through the wet moves and back the same way to its last
  breath: air-left-s less the moves out and undone (wet-prices), less the way it has come under water already (its air
  used, at the drain)."
  [policy moves]
  (- (air-left-s policy)
     (/ (or (:air-used policy) 0) (or (:air-drain policy) 1))
     (wet-way-s policy moves)
     (wet-way-s policy (map #(get-in wet-prices [% 2] %) moves))))

(defn wet-ok?
  "Whether a body may go on through the wet moves (from where it is): back at its last breath it keeps the setting
  ::wet-margin-s of air (wet-spare-s)."
  [policy moves]
  (>= (wet-spare-s policy moves) (settings/get settings ::wet-margin-s)))

(defn walk-costs
  "The planner price overrides every walk keeps: the setting ::costs, with :lethal-air 1 when ::lethal-air."
  []
  (let [m (settings/get settings ::costs)]
    (cond-> (or m nil) (settings/get settings ::lethal-air) (assoc :lethal-air 1))))

(defn walk-landing "The setting ::landing, nil when none." [] (or (settings/get settings ::landing) nil))
(defn walk-drop-cost "The setting ::drop-cost." [] (settings/get settings ::drop-cost))
(defn walk-unknown-prices "The unknown-prices as set: the defaults over the setting ::unknown-prices." []
  (merge unknown-prices (or (settings/get settings ::unknown-prices) nil)))
