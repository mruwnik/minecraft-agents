(ns triggers.facts
  "The facts a trigger condition may name (see engine.condition). A fact reads
  the body's sensed world and its memory and returns a value, or unknown when
  it has none (the body is offline, no such place is known). Each declares its
  argument types, its result type and its cost: :cheap is a synchronous read
  done every tick, :scan is served from a per-instance cache refreshed every
  :refresh-s seconds. A new fact is written here, with a test."
  (:require [engine.condition.facts :refer [online number-or-unknown boolean-or-unknown position? item-count
                                            wearing? seconds-since distance-to blocks-near unknown]]
            [engine.memory :as mem]
            [jobs.lib.util :as u]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [triggers.survival.burning :as burning]
            [triggers.survival.hostile-near :as hostile-near]
            [triggers.survival.suffocating :as suffocating]
            [triggers.survival.stuck :as stuck]))

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
   'since {:args [:keyword] :type :number :cost :cheap
           :doc "seconds since memory last recorded an entry of that kind (:slept :fed :looked ...); unknown when none"
           :read (fn [{:keys [memory]} kind] (seconds-since memory kind))}
   'distance-to {:args [:position] :type :number :cost :cheap :doc "blocks from the body to the position"
                 :read (online distance-to)}
   'daytime {:args [] :type :boolean :cost :cheap :doc "the sun is up"
             :read (online (fn [_ s] (boolean-or-unknown (.-isDay s))))}
   'hostile-near {:args [:number] :type :boolean :cost :cheap
                  :doc "a real danger (as the hostile-near trigger: a mob that can reach the body, or a ranged one with a line of fire) is within that many blocks"
                  :read (online (fn [p _ radius] (reach/danger-near? p radius (max radius hostile-near/ranged-radius))))}
   'in-water {:args [] :type :boolean :cost :cheap :doc "the body is in water"
              :read (online (fn [_ s] (boolean-or-unknown (.-inWater s))))}
   'burning {:args [] :type :boolean :cost :cheap
             :doc "on fire or in lava without fire resistance (the burning trigger)"
             :read (online (fn [_ s] (burning/burning? s)))}
   'suffocating {:args [] :type :boolean :cost :cheap
                 :doc "drowning or enclosed (the suffocating trigger, default oxygen)"
                 :read (online (fn [p _] (some? (suffocating/situation p suffocating/default-min-oxygen))))}
   'night-unsafe {:args [] :type :boolean :cost :cheap
                  :doc "night, awake, nothing overhead and not buried"
                  :read (online (fn [p _] (sh/unsafe-night? p sh/default-roof-height)))}
   'stuck {:args [] :type :boolean :cost :cheap
           :doc "the last moves all failed and the body is really held (the stuck trigger, default args)"
           :read (fn [{:keys [world memory]}] (stuck/body-stuck? world memory {}))}
   'blocks-near {:args [:string :number] :type :number :cost :scan :refresh-s 5
                 :doc "how many blocks of that name the body has seen within the radius (at most 32), counted up to 256"
                 :read (online blocks-near)}})
