(ns jobs.survival.retreat-home
  "The body's home as a flight's refuge: a closed room (jobs.lib.reach/room: walls, a roof or none needed, doors that
  shut) that the body gets into before every melee chaser reaches a door of it (jobs.lib.cost/reach-first?)."
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

(defn inside?
  "Whether the body is in room (jobs.lib.reach/room): its feet cell or the cell it stands on (as go-to arrives) is one of
  the room's cells."
  [p {:keys [cells]}]
  (boolean (some #(contains? cells (mapv % [:x :y :z])) [(sh/feet p) (reach/standing-cell p)])))

(defn mid [{:keys [x y z]}] {:x (+ x 0.5) :y y :z (+ z 0.5)})

(defn way-in
  "The door of room the body walks to in fewest steps over the blocks it has seen (jobs.lib.reach/walk-steps to the room
  cell inside it): {:cell :inside :steps}, nil when it finds no walk to any."
  [p room]
  (->> (:doors room)
       (keep (fn [d] (when-let [n (reach/walk-steps p (sh/feet p) (:inside d))] (assoc d :steps n))))
       (sort-by :steps)
       first))

(defn race-won?
  "Whether the body, at its own speed (food), walks way-in's steps before any melee threat (ranged ones shoot, they do
  not race) gets to a door of room (straight line, the nearest door): cost/reach-first?."
  [p room {:keys [steps]} threats]
  (cost/reach-first? steps
                     (->> threats
                          (remove combat/ranged?)
                          (map (fn [t] (apply min (map #(u/dist (danger-q/mob-pos p t) (mid (:cell %))) (:doors room))))))
                     (or (.-food (.self p)) 20)))

(defn say-once!
  "One retreat.home info a flight: it runs to its home."
  [c {:keys [x y z]}]
  (when-not (:home-said (ctx/mem c))
    (ctx/update-mem! c assoc :home-said true)
    (ctx/emit! c :retreat.home :info {:home [x y z] :text (str "runs home to [" x " " y " " z "], ahead of the hostile")})))

(defn failed! [c] (ctx/update-mem! c assoc :home-failed true))
