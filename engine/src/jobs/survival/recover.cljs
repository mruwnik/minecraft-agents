(ns jobs.survival.recover
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.memory :as mem]
            [engine.jobs.combat :as combat]))

(def doc
  "A hurt body flees and heals; it does not fight. Starts when health is
  below :health (default 7). Because regeneration is slow it then keeps going
  until health reaches :healed (default 16) rather than stopping at the
  trigger threshold: between the two it runs only while a :hurt entry from the
  last five minutes exists, so a long-healed body is left alone. Each round: a
  hostile within :sight blocks is fled (jobs.survival.retreat); else walk to
  the latest :bed, else :home, else stay; at the safe point eat if hungry and
  carrying food, then do nothing so health regenerates. Done once health reaches :healed.
  One :hurt entry is written per spell.")

(def args
  {:health {:doc "start recovering when health is below this" :default 7}
   :healed {:doc "keep recovering until health reaches this" :default 16}
   :sight {:doc "hostiles within this many blocks are fled" :default 16}})

(def hurt-window-ms (* 5 60 1000))

(def hurt-policy {:cap 20 :ttl (* 60 60 1000)})

(def safe-distance 3)

(def food-names
  #{"cooked_beef" "cooked_porkchop" "cooked_mutton" "cooked_chicken" "cooked_rabbit" "cooked_salmon" "cooked_cod"
    "bread" "baked_potato" "carrot" "golden_carrot" "apple" "golden_apple" "sweet_berries" "melon_slice"
    "beef" "porkchop" "mutton" "chicken" "potato" "pumpkin_pie" "mushroom_stew" "beetroot" "cookie"})

(defn health-of [c] (.-health (.self (:primitives c))))

(defn check
  "Health is below :health, or below :healed with a :hurt entry in the last
  five minutes, or a spell is under way (so the round gets to see it healed
  and finish, instead of the job lingering declined)."
  [c]
  (let [{:keys [health healed]} (:args c)
        h (health-of c)]
    (or (< h health)
        (boolean (:spell-started (ctx/mem c)))
        (and (< h healed) (pos? (ctx/count-in c :hurt hurt-window-ms))))))

(defn safe-point
  "The latest :bed, else the latest :home, else here."
  [c]
  (or (mem/place (ctx/view c) :bed)
      (mem/place (ctx/view c) :home)
      (u/self-pos c)))

(defn has-food? [c]
  (boolean (some (comp food-names :name) (u/inventory (:primitives c)))))

(defn start-spell!
  "Once per spell (a job instance), record why we are hurt."
  [c threat]
  (when-not (:spell-started (ctx/mem c))
    (ctx/update-mem! c assoc :spell-started true)
    (ctx/remember! c :hurt
                   (cond-> {:health (health-of c) :pos (u/self-pos c)}
                     threat (assoc :hostile {:name (.-name threat) :pos (u/pos-of (.-pos threat))}))
                   hurt-policy)))

(defn ^:async flee! [c threat sight]
  (if threat
    (await (ctx/call-child c :flee 'jobs.survival.retreat {:radius sight}))
    :done))

(defn ^:async go-safe! [c]
  (let [dest (safe-point c)]
    (if (> (u/dist (u/self-pos c) dest) safe-distance)
      (await (ctx/call-child c :safety 'jobs.movement.go-to {:pos dest :range 2}))
      :done)))

(defn ^:async round [c]
  (let [{:keys [sight healed]} (:args c)
        threat (first (combat/hostiles (:primitives c) sight))]
    (cond
      (>= (health-of c) healed) :done
      :else
      (do (start-spell! c threat)
          (if (= :continue (await (flee! c threat sight)))
            :continue
            (if (= :continue (await (go-safe! c)))
              :continue
              (do (when (and (< (.-food (.self (:primitives c))) 20) (has-food? c))
                    (await (ctx/call-child c :eat 'jobs.survival.eat {})))
                  (if (>= (health-of c) healed) :done :continue))))))))
