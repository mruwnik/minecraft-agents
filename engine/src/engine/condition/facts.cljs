(ns engine.condition.facts
  "The facts a trigger condition may name (see engine.condition). A fact reads
  the body's sensed world and its memory and returns a value, or unknown when
  it has none (the body is offline, no such place is known). Each declares its
  argument types, its result type and its cost: :cheap is a synchronous read
  done every tick, :scan is served from a per-instance cache refreshed every
  :refresh-s seconds. A new fact is written here, with a test."
  (:require [engine.memory :as mem]
            [engine.jobs.util :as u]
            [engine.jobs.combat :as combat]
            [engine.jobs.shelter :as sh]
            [engine.triggers.burning :as burning]
            [engine.triggers.suffocating :as suffocating]
            [engine.triggers.stuck :as stuck]))

(def unknown
  "The value of a fact, and of anything built on it, that is not known now."
  ::unknown)

(def max-scan-radius 32)

(def max-scan-count 256)

(defn self
  "The sensed self of p, or nil when the body is offline."
  [p]
  (let [s (.self p)]
    (when-not (= "offline" (.-status s)) s)))

(defn number-or-unknown [x] (if (number? x) x unknown))

(defn boolean-or-unknown [x] (if (boolean? x) x unknown))

(defn position? [x]
  (and (map? x) (every? #(number? (get x %)) [:x :y :z])))

(defn online
  "A fact reader that is unknown while the body is offline and otherwise
  (f p self & args)."
  [f]
  (fn [{:keys [world]} & args]
    (if-let [s (self world)]
      (apply f world s args)
      unknown)))

(defn item-count [s item]
  (transduce (comp (filter #(= item (.-name %))) (map #(.-count %))) + 0
             (array-seq (.-inventory s))))

(defn wearing?
  "Whether an armour slot or the off-hand holds item (the main hand is the held item, not worn)."
  [s item]
  (let [gear (.-equipment s)]
    (boolean (some #(= item (some-> (aget gear %) .-name)) ["head" "torso" "legs" "feet" "offHand"]))))

(defn distance-to [p s pos]
  (if (position? pos)
    (u/dist (u/pos-of (.-pos s)) pos)
    unknown))

(defn blocks-near [p _ item radius]
  (count (.blocks p #js {:radius (min radius max-scan-radius) :names #js [item] :max max-scan-count})))

(def table
  "Every fact by symbol: {:args [type ...] :type type :cost :cheap|:scan
  :refresh-s s (scan only) :doc text :read (fn [env & arg-values] value)}.
  env is {:world primitives :memory view}."
  {'health {:args [] :type :number :cost :cheap :doc "health, 0 to 20"
            :read (online (fn [_ s] (number-or-unknown (.-health s))))}
   'food {:args [] :type :number :cost :cheap :doc "food, 0 to 20"
          :read (online (fn [_ s] (number-or-unknown (.-food s))))}
   'inventory {:args [:string] :type :number :cost :cheap
               :doc "how many of the named item are carried (main and hotbar slots)"
               :read (online (fn [_ s item] (item-count s item)))}
   'wearing {:args [:string] :type :boolean :cost :cheap
             :doc "the named item is worn (head, torso, legs, feet) or held in the off-hand"
             :read (online (fn [_ s item] (wearing? s item)))}
   'free-slots {:args [] :type :number :cost :cheap :doc "empty main and hotbar slots, of 36"
                :read (online (fn [p _] (u/free-slots p)))}
   'place {:args [:keyword] :type :position :cost :cheap
           :doc "the latest remembered place of that kind (:home :bed :chest ...); unknown when none"
           :read (fn [{:keys [memory]} kind]
                   (let [pos (mem/place memory kind)]
                     (if (position? pos) pos unknown)))}
   'distance-to {:args [:position] :type :number :cost :cheap :doc "blocks from the body to the position"
                 :read (online distance-to)}
   'daytime {:args [] :type :boolean :cost :cheap :doc "the sun is up"
             :read (online (fn [_ s] (boolean-or-unknown (.-isDay s))))}
   'hostile-near {:args [:number] :type :boolean :cost :cheap
                  :doc "a hostile mob the body can see is within that many blocks"
                  :read (online (fn [p _ radius] (boolean (seq (combat/hostiles p radius {:sight :only})))))}
   'in-water {:args [] :type :boolean :cost :cheap :doc "the body is in water"
              :read (online (fn [_ s] (boolean-or-unknown (.-inWater s))))}
   'burning {:args [] :type :boolean :cost :cheap
             :doc "on fire or in lava without fire resistance (the burning trigger)"
             :read (online (fn [_ s] (burning/burning? s)))}
   'suffocating {:args [] :type :boolean :cost :cheap
                 :doc "drowning or enclosed (the suffocating trigger, default oxygen)"
                 :read (online (fn [p _] (some? (suffocating/situation p suffocating/default-min-oxygen))))}
   'night-unsafe {:args [] :type :boolean :cost :cheap
                  :doc "night, awake and nothing overhead (the night-unsafe trigger)"
                  :read (online (fn [p _] (sh/unsafe-night? p sh/default-roof-height)))}
   'stuck {:args [] :type :boolean :cost :cheap
           :doc "the last moves all failed (the stuck trigger, default args)"
           :read (fn [{:keys [memory]}] (stuck/stuck? memory {}))}
   'blocks-near {:args [:string :number] :type :number :cost :scan :refresh-s 5
                 :doc "how many blocks of that name are within the radius (at most 32), counted up to 256"
                 :read (online blocks-near)}})
