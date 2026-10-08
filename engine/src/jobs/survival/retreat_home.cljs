(ns jobs.survival.retreat-home
  "The body's home as a flight's refuge: a closed room (jobs.lib.reach/closed-room?: walls, a roof or none needed,
  doors that shut) that the body gets to before every melee chaser (jobs.lib.cost/reach-first?)."
  (:require [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(defn refuge
  "home ({:x :y :z}, its feet cell) when it is a closed room and no run to it failed this flight, else nil. The room
  check is kept in mem :refuge-room for the flight."
  [c home]
  (when (and home (not (:home-failed (ctx/mem c))))
    (let [[at closed] (:refuge-room (ctx/mem c))
          closed (if (= at home)
                   closed
                   (let [v (reach/closed-room? (:primitives c) home)]
                     (ctx/update-mem! c assoc :refuge-room [home v])
                     v))]
      (when closed home))))

(defn at-home? [p home] (= (select-keys (sh/feet p) [:x :y :z]) (select-keys home [:x :y :z])))

(defn race-won?
  "Whether the body gets to home before every melee threat (ranged ones shoot, they do not race): cost/reach-first?
  over straight distances."
  [p home threats]
  (cost/reach-first? (u/dist (u/self-pos {:primitives p}) home)
                     (->> threats (remove combat/ranged?) (map #(u/dist (danger-q/mob-pos p %) home)))))

(defn say-once!
  "One retreat.home info a flight: it runs to its home."
  [c {:keys [x y z]}]
  (when-not (:home-said (ctx/mem c))
    (ctx/update-mem! c assoc :home-said true)
    (ctx/emit! c :retreat.home :info {:home [x y z] :text (str "runs home to [" x " " y " " z "], ahead of the hostile")})))

(defn failed! [c] (ctx/update-mem! c assoc :home-failed true))
