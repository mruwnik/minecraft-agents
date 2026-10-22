(ns jobs.lib.death-spot
  "Where the body died lately, from the :deaths memory (engine.core/record-death!): a place a player avoids or goes back to
  prepared. A death weighs 1 when fresh and fades to 0 over ::memory-ms; it counts for cells within ::radius of its spot."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::memory-ms {:default 7200000 :doc "Milliseconds over which the memory of a death fades from full weight to none." :spec (a/num-in 1 nil)}
   ::radius {:default 24 :doc "Blocks around a death spot that the death still counts for." :spec (a/num-in 0 nil)}})

(def terrain-causes
  "Death causes that are no mob: the cells that kill are access rules and the planner's, not a fight to price."
  #{"fall" "lava" "drown" "starve" "in_fire" "on_fire" "hot_floor" "cactus" "in_wall" "fly_into_wall" "freeze" "dry_out" "out_of_world" "generic_kill" "magic" "wither" "lightning_bolt"})

(defn recent
  "The deaths by a mob of c still remembered: [{:pos {:x :y :z} :cause :weight}], weight falling from 1 to 0 with age."
  [c]
  (let [now (ctx/now c)
        ttl (settings/get settings ::memory-ms)]
    (vec (for [{:keys [data]} (ctx/entries c :deaths)
               :let [w (- 1 (/ (- now (:death-t data)) ttl))
                     {:keys [x y z]} (:pos data)]
               :when (and (pos? w) x (not (terrain-causes (:cause data))))]
           {:pos {:x x :y y :z z} :cause (:cause data) :weight (min 1 w)}))))

(defn near
  "The remembered deaths of `deaths` (from recent) within ::radius of cell `pos`."
  [deaths pos]
  (filterv #(<= (u/dist pos (:pos %)) (settings/get settings ::radius)) deaths))
