(ns jobs.survival.retreat-home
  "A closed building as a flight's refuge: the body's home or any closed room it has seen (jobs.lib.reach/room: walls, a
  roof or none needed, doors that shut) that it gets into before every melee chaser reaches a door of it
  (jobs.lib.cost/reach-first?)."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(defn mid [{:keys [x y z]}] {:x (+ x 0.5) :y y :z (+ z 0.5)})

(defn food "The body's food, 20 (full) when unknown." [p] (or (.-food (.self p)) 20))

(a/defargs settings
  {::door-radius {:default 32 :doc "Blocks from the body within which a seen door or gate may lead into a closed building to flee to." :spec (a/num-in 0 64)}
   ::max-doors {:default 8 :doc "Seen doors and gates, nearest first, whose two sides a flight checks for a closed room." :spec (a/int-in 0 nil)}
   ::max-rooms {:default 3 :doc "Closed rooms, the home first then the nearest, a flight step races for." :spec (a/int-in 1 nil)}})
(defn door-radius [] (settings/get settings ::door-radius))
(defn max-doors [] (settings/get settings ::max-doors))
(defn max-rooms [] (settings/get settings ::max-rooms))

(defn cell-key [{:keys [x y z]}] (str x "," y "," z))

(defn door-sides
  "The cells on either side of the lower half of a seen wooden door or gate {:pos :properties}, along its facing."
  [{:keys [pos properties]}]
  (when-let [[dx dz] (reach/door-panel-step (:facing properties))]
    [(-> pos (update :x + dx) (update :z + dz)) (-> pos (update :x - dx) (update :z - dz))]))

(defn starts
  "Cells a closed room may hold: home ({:x :y :z}, its feet cell, or nil) first, then the sides (door-sides) of the
  wooden doors and gates the body has seen within door-radius, nearest first, at most max-doors of them. Each
  {:pos :home?}."
  [p home]
  (let [doors (->> (look/seen-blocks p {:match #(and (or (str/ends-with? % "_door") (str/ends-with? % "_fence_gate"))
                                                     (not (str/starts-with? % "iron_")))
                                        :radius (door-radius) :max (* 2 (max-doors)) :properties? true})
                   (remove #(= "upper" (:half (:properties %))))
                   (take (max-doors)))]
    (cond->> (for [d doors s (door-sides d)] {:pos s :home? false})
      home (cons {:pos home :home? true}))))

(defn closed?
  "Whether cell pos holds a closed room (jobs.lib.reach/closed-room?), kept in mem :refuge-rooms for the job."
  [c pos]
  (let [k (cell-key pos)
        known (get (:refuge-rooms (ctx/mem c)) k)]
    (if (some? known)
      known
      (let [v (reach/closed-room? (:primitives c) pos)]
        (ctx/update-mem! c assoc-in [:refuge-rooms k] v)
        v))))

(defn survey
  "The closed rooms to flee to, {:rooms [...]} (jobs.lib.reach/room plus :start and :home?): the home's (home
  {:x :y :z} or nil) and those behind the doors the body has seen (starts), at most max-rooms, the home first then the
  nearest; and {:dropped [{:at [x y z] :own? :why}]}, the cells that are none: :run-failed (a run to it failed this
  flight) or :not-closed."
  [c home]
  (let [p (:primitives c)
        me (u/self-pos c)
        failed (:refuges-failed (ctx/mem c) #{})
        judged (mapv (fn [{:keys [pos] :as s}]
                       (assoc s :why (cond (failed (cell-key pos)) :run-failed (not (closed? c pos)) :not-closed)))
                     (starts p home))]
    {:dropped (vec (for [{:keys [pos home? why]} judged :when why] {:at (mapv pos [:x :y :z]) :own? home? :why why}))
     :rooms (->> judged
                 (remove :why)
                 (sort-by (fn [{:keys [pos home?]}] [(if home? 0 1) (u/dist me (mid pos))]))
                 (keep (fn [{:keys [pos home?]}] (some-> (reach/room p pos) (assoc :start pos :home? home?))))
                 (reduce (fn [acc r] (if (some #(contains? (:cells %) (mapv (:start r) [:x :y :z])) acc) acc (conj acc r))) [])
                 (take (max-rooms))
                 vec)}))

(defn rooms
  "The closed rooms to flee to (survey), none a run to failed this flight."
  [c home]
  (:rooms (survey c home)))

(defn inside?
  "Whether the body is in room (jobs.lib.reach/room): its feet cell or the cell it stands on (as go-to arrives) is one of
  the room's cells."
  [p {:keys [cells]}]
  (boolean (some #(contains? cells (mapv % [:x :y :z])) [(sh/feet p) (reach/standing-cell p)])))

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
                     (food p)))

(defn verdicts
  "What the flight makes of each of rooms (rooms): [{:room r :inside? true}] alone for the room the body is in, else
  per room {:room r :way w} with :why it is not run to: :no-run (way? false: no run may be looked for now), :creeper
  (one chases: it would follow to the door and blow the door, chest and bed), :no-way (way-in finds none) or
  :race-lost (a melee chaser gets to a door first: race-won?); no :why: a run there is on."
  [p rooms threats way?]
  (if-let [r (some #(when (inside? p %) %) rooms)]
    [{:room r :inside? true}]
    (let [creeper? (some combat/creeper? threats)]
      (mapv (fn [r]
              (cond
                (not way?) {:room r :why :no-run}
                creeper? {:room r :why :creeper}
                :else (let [w (way-in p r)]
                        (cond
                          (nil? w) {:room r :why :no-way}
                          (race-won? p r w threats) {:room r :way w}
                          :else {:room r :way w :why :race-lost}))))
            rooms))))

(defn pick
  "Where the flight goes among verdicts: the room the body is in, else the one a run is on to in fewest steps, the home
  on a tie; nil when none."
  [verdicts]
  (->> verdicts
       (remove :why)
       (sort-by (comp :steps :way))
       first))

(defn say-once!
  "One retreat.home info a flight: it runs to its home or to another closed building, room."
  [c {:keys [start home?]}]
  (when-not (:home-said (ctx/mem c))
    (ctx/update-mem! c assoc :home-said true)
    (let [{:keys [x y z]} start]
      (ctx/emit! c :retreat.home :info {:home [x y z] :own? home?
                                        :text (str (if home? "runs home to [" "runs into the closed building at [")
                                                   x " " y " " z "], ahead of the hostile")}))))

(defn failed!
  "A run to room got no nearer: no run to it again this flight."
  [c room]
  (ctx/update-mem! c update :refuges-failed (fnil conj #{}) (cell-key (:start room))))
