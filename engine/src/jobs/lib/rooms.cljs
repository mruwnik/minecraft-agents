(ns jobs.lib.rooms
  "Closed rooms, for a flight and for shutting in: the body's home or any closed room it has seen (jobs.lib.reach.room/room: walls, a
  roof or none needed, doors that shut) that it gets into before every melee chaser reaches a door of it
  (jobs.lib.cost/reach-first?). Shared by jobs.survival.respond-flight and jobs.survival.enter-room."
  (:require [jobs.lib.block-sets :as bs]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.access :as access]
            [jobs.lib.look :as look]
            [jobs.lib.pillar :as pillar]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]))

(defn mid [{:keys [x y z]}] {:x (+ x 0.5) :y y :z (+ z 0.5)})

(defn food "The body's food, 20 (full) when unknown." [p] (or (.-food (.self p)) 20))

(a/defargs settings
  {::door-radius {:default 32 :doc "Blocks from the body within which a seen door or gate may lead into a closed building to flee to." :spec (a/num-in 0 64)}
   ::max-doors {:default 8 :doc "Seen doors and gates, nearest first, whose two sides a flight checks for a closed room." :spec (a/int-in 0 nil)}
   ::door-looks {:default 4 :doc "Looks a flight takes from a building's doorway at the unseen sides of its room before it drops it as unseen." :spec (a/int-in 0 nil)}
   ::max-rooms {:default 3 :doc "Closed rooms, the home first then the nearest, a flight step races for." :spec (a/int-in 1 nil)}
   ::judge-per-step {:default 2 :doc "Door sides not judged before that a flight step floods for a closed room (an open one reads up to room-cells cells), nearest first; none while a melee chaser is within cover-gap. The home is always judged." :spec (a/int-in 0 nil)}})
(defn door-radius [] (settings/get settings ::door-radius))
(defn max-doors [] (settings/get settings ::max-doors))
(defn door-looks [] (settings/get settings ::door-looks))
(defn max-rooms [] (settings/get settings ::max-rooms))
(defn judge-per-step [] (settings/get settings ::judge-per-step))

(defn cell-key [{:keys [x y z]}] (str x "," y "," z))

(defn door-sides
  "The cells on either side of the lower half of a seen wooden door or gate {:pos :properties}, along its facing."
  [{:keys [pos properties]}]
  (when-let [[dx dz] (reach-room/door-panel-step (:facing properties))]
    [(-> pos (update :x + dx) (update :z + dz)) (-> pos (update :x - dx) (update :z - dz))]))

(defn starts
  "Cells a closed room may hold: home ({:x :y :z}, its feet cell, or nil) first, then the sides (door-sides) of the
  wooden doors and gates the body has seen within door-radius, nearest first, at most max-doors of them. Each
  {:pos :home? :door} (:door the door's cell)."
  [p home]
  (let [doors (->> (look/seen-blocks p {:match #(and (or (str/ends-with? % "_door") (str/ends-with? % "_fence_gate"))
                                                     (not (str/starts-with? % "iron_")))
                                        :radius (door-radius) :max (* 2 (max-doors)) :properties? true})
                   (remove #(= "upper" (:half (:properties %))))
                   (take (max-doors)))]
    (cond->> (for [d doors s (door-sides d)] {:pos s :home? false :door (:pos d)})
      home (cons {:pos home :home? true}))))

(def open-policy
  "Cells judged no closed room, kept in body memory so each new flight does not flood an open home again."
  {:cap 16 :ttl (* 2 60 1000)})

(defn known-open?
  "Whether a flight within open-policy's ttl judged cell pos no closed room."
  [c pos]
  (let [k (cell-key pos)]
    (boolean (some #(= k (:cell (:data %))) (mem/entries (ctx/view c) :refuge-open)))))

(defn closed-past-sight?
  "Whether room's walls hold past the edge of sight: a flood from it (doors shut) that also steps onto any column free
  at feet and head over a floor not sensed stays within reach-room/room-cells. Unseen floor inside walls changes nothing; past
  a gap the flood runs on over ground not seen, which no wall is known to bound."
  [p {:keys [cells]}]
  (let [kind-at (reach-room/shut-kind-at p)
        next-cells (fn [x y z]
                     (let [out (reach/forward kind-at x y z)]
                       (doseq [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                               :let [nx (+ x dx) nz (+ z dz)]
                               :when (and (reach/passable? kind-at nx y nz) (reach/passable? kind-at nx (inc y) nz)
                                          (nil? (u/seen-block p {:x nx :y (dec y) :z nz})))]
                         (.push out #js [nx y nz]))
                       out))]
    (some? (reach-room/flood-cells next-cells (first cells) reach-room/room-cells))))

(defn refuge-room?
  "Whether cell pos holds a closed room (jobs.lib.reach.room/room) that stays closed past the edge of sight (closed-past-sight?)."
  [p pos]
  (boolean (some->> (reach-room/room p pos) (closed-past-sight? p))))

(defn closed?
  "Whether cell pos holds a closed room (refuge-room?), kept in mem :refuge-rooms for the job; no closed room at all
  (reach-room/room nil) also in body memory (known-open?) for a while. :unseen, kept nowhere, when a cell of the room is not
  seen yet (reach-room/unseen-room?): judged again at the next look."
  [c pos]
  (let [k (cell-key pos)
        known (get (:refuge-rooms (ctx/mem c)) k)]
    (cond
      (some? known) known
      (known-open? c pos) false
      :else (let [p (:primitives c) v (refuge-room? p pos)]
              (if (and (not v) (reach-room/unseen-room? p pos))
                :unseen
                (do (ctx/update-mem! c assoc-in [:refuge-rooms k] v)
                    (when-not (or v (reach-room/room p pos)) (mem/write! (:store (:engine c)) :refuge-open {:cell k} open-policy))
                    v))))))

(defn survey
  "The closed rooms to flee to, {:rooms [...]} (jobs.lib.reach.room/room plus :start and :home?): the home's (home
  {:x :y :z} or nil) and those behind the doors the body has seen (starts), at most max-rooms, the home first then the
  nearest; and {:dropped [{:at [x y z] :own? :why}]}, the cells that are none: :run-failed (a run to it failed this
  flight), :not-closed, :unseen (a cell of the room not seen yet), or :unjudged (a door side not judged this flight beyond the fresh ones
  it may flood now; the home is always judged)."
  ([c home] (survey c home js/Infinity))
  ([c home fresh]
   (let [p (:primitives c)
         me (u/self-pos c)
         failed (:refuges-failed (ctx/mem c) #{})
         left (volatile! fresh)
         judge (fn [pos home?]
                 (let [known (get (:refuge-rooms (ctx/mem c)) (cell-key pos))]
                   (cond
                     (some? known) (when-not known :not-closed)
                     (or home? (pos? @left)) (do (when-not home? (vswap! left dec))
                                                 (case (closed? c pos) true nil :unseen :unseen :not-closed))
                     :else :unjudged)))
         judged (mapv (fn [{:keys [pos home?] :as s}]
                        (assoc s :why (if (failed (cell-key pos)) :run-failed (judge pos home?))))
                      (starts p home))]
     {:dropped (vec (for [{:keys [pos home? why]} judged :when why] {:at (mapv pos [:x :y :z]) :own? home? :why why}))
      :rooms (->> judged
                  (remove :why)
                  (sort-by (fn [{:keys [pos home?]}] [(if home? 0 1) (u/dist me (mid pos))]))
                  (keep (fn [{:keys [pos home?]}] (some-> (reach-room/room p pos) (assoc :start pos :home? home?))))
                  (reduce (fn [acc r] (if (some #(contains? (:cells %) (mapv (:start r) [:x :y :z])) acc) acc (conj acc r))) [])
                  (take (max-rooms))
                  vec)})))

(def door-glance-radius
  "Blocks from a building's door side within which a flight looks in once when the building was dropped :unseen."
  6)

(defn door-cell
  "The seen door or gate cell beside door side pos {:x :y :z}, nil when none."
  [p {:keys [x y z]}]
  (first (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
               :let [cell {:x (+ x dx) :y y :z (+ z dz)}]
               :when (some-> (u/seen-block p cell) .-name reach/openable?)]
           cell)))

(defn ^:async glance-in!
  "Once a flight per building: when survey dropped a building only for :unseen cells and the body is within
  door-glance-radius of its door side, step into the doorway (a go-to child; a wall of a 1-wide door shows a wedge from
  outside) and look at the nearest unseen cell beside its room, again for each still unseen, at most ::door-looks looks at each spot; cells a door gap shows only at a graze stay unseen, so
  the body then steps to the far side of the door and looks again (sight passes; the time is part of the flight). True when it looked: the caller surveys again."
  [c survey]
  (let [p (:primitives c)
        me (u/self-pos c)
        glanced (:door-glanced (ctx/mem c) #{})
        at (first (for [{[x y z] :at why :why} (:dropped survey)
                        :let [k (cell-key {:x x :y y :z z})]
                        :when (and (= :unseen why) (not (glanced k)) (<= (u/dist me {:x x :y y :z z}) door-glance-radius))]
                    {:k k :pos {:x x :y y :z z}}))]
    (when at
      (ctx/update-mem! c update :door-glanced (fnil conj #{}) (:k at))
      (let [d (door-cell p (:pos at))
            walk-to! (fn ^:async w [cell]
                       (when-not (= (cell-key (u/self-pos c)) (cell-key cell))
                         (await (ctx/call c :door-glance 'jobs.movement.go-to {:pos (mid cell) :range 0.7 :ways #{:walk} :spend {:job-time 4 :tries 1}}))))
            look-at-sides! (fn ^:async l [n looked]
                             (loop [n n looked looked]
                               (let [me (u/self-pos c)
                                     target (->> (reach-room/unseen-sides p (:pos at))
                                                 (remove looked)
                                                 (sort-by #(u/dist me {:x (% 0) :y (% 1) :z (% 2)}))
                                                 first)]
                                 (if (and target (< n (door-looks)))
                                   (do (await (sense/look-at! c target))
                                       (recur (inc n) (conj looked target)))
                                   [n looked]))))
            beyond (when d (let [{:keys [x y z]} (:pos at)] {:x (- (* 2 (:x d)) x) :y y :z (- (* 2 (:z d)) z)}))]
        (when d (await (walk-to! d)))
        (let [[n _] (await (look-at-sides! 0 #{}))
              inner (when (and beyond (reach-room/unseen-room? p (:pos at)))
                      (if (> (u/dist me beyond) (u/dist me (:pos at))) beyond (:pos at)))]
          (if-not inner
            (pos? n)
            (do (await (walk-to! inner))
                (await (look-at-sides! 0 #{}))
                true)))))))

(defn rooms
  "The closed rooms to flee to (survey), none a run to failed this flight."
  [c home]
  (:rooms (survey c home)))

(defn inside?
  "Whether the body is in room (jobs.lib.reach.room/room): its feet cell or the cell it stands on (as go-to arrives) is one of
  the room's cells."
  [p {:keys [cells]}]
  (boolean (some #(contains? cells (mapv % [:x :y :z])) [(sh-sense/feet p) (reach/standing-cell p)])))

(defn in-home?
  "Whether the body is in the closed room that holds home ({:x :y :z} or nil); no other room is looked at."
  [c home]
  (let [p (:primitives c)]
    (boolean (when (and home (true? (closed? c home)))
               (some->> (reach-room/room p home) (inside? p))))))

(defn way-in
  "The door of room the body walks to in fewest steps over the blocks it has seen (jobs.lib.reach.room/walk-steps to the room
  cell inside it): {:cell :inside :steps}, nil when it finds no walk to any."
  [p room]
  (->> (:doors room)
       (keep (fn [d] (when-let [n (reach-room/walk-steps p (sh-sense/feet p) (:inside d))] (assoc d :steps n))))
       (sort-by :steps)
       first))

(defn race-won?
  "Whether the body, at its own speed (food), walks way-in's steps before any melee threat (ranged ones shoot, they do
  not race), each at its own speed, gets to a door of room (straight line, the nearest door): cost/reach-first?."
  [p room {:keys [steps]} threats]
  (let [racers (remove combat/ranged? threats)]
    (cost/reach-first? steps
                       (map (fn [t] (apply min (map #(u/dist (danger-q/mob-pos p t) (mid (:cell %))) (:doors room)))) racers)
                       (food p)
                       (map cost/mob-kind racers))))

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

;; ------------------------------------------------------------------ doorways

(defn carried-door
  "A wooden door item the body carries, or nil."
  [p]
  (some #(let [n (:name %)] (when (and (.endsWith n "_door") (not= "iron_door" n)) n)) (u/inventory p)))

(defn around
  "The cell and its four horizontal neighbours."
  [{:keys [x y z] :as cell}]
  (cons cell (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] {:x (+ x dx) :y y :z (+ z dz)})))

(defn shut-by
  "The first doorway (sh-sense/doorway?, not the feet, passing ok?) among the cells round e whose two cells [d, d above] shut the
  room: the flood from the feet has no unroofed cell once they are solid. An overhang beside a gap has one."
  [p feet e ok?]
  (first (filter #(and (not= feet %) (sh-sense/doorway? p %) (ok? %)
                       (nil? (:cell (reach-room/unroofed-cell p feet #{[(:x %) (:y %) (:z %)] [(:x %) (inc (:y %)) (:z %)]}))))
                 (around e))))

(defn closed-home?
  "Whether the body stands in a home closed to the night but for a doorway: every cell of its room is roofed, or one
  doorway (sh-sense/doorway?) shuts it so that every cell is (shut-by). Under an overhang or a lone roof: false."
  [p]
  (let [feet (sh-sense/feet p)
        cell (:cell (reach-room/unroofed-cell p feet))]
    (or (nil? cell) (some? (shut-by p feet cell any?)))))

(def doorway-tried-policy
  "The :doorway-tried entries: doorways filled or refused tonight; a day at most."
  {:cap 4 :ttl 86400000})

(defn gap
  "The cells [feet head] that shut the room around the body: the doorway where the room's flood first leaves the roof
  (reach-room/unroofed-cell, closed flood or not: a walled yard is open to the night too) or beside that cell that shuts the
  room (shut-by), those still free; nil when the room is shut, no doorway shuts it (open ground), the body or a seen
  animal or mob stands in it, the place is refused, or this doorway was tried tonight (body memory :doorway-tried, doorway-tried-policy)."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        seen (delay (look/seen-entities p {:radius 8}))
        free? (fn [{:keys [x y z] :as cell}]
                (and (bs/air (u/seen-name p cell)) (not (pillar/mob-in @seen [x y z]))))]
    (when-let [e (:cell (reach-room/unroofed-cell p feet))]
      (when-let [d (shut-by p feet e free?)]
        (when-not (or (some #(= d (:data %)) (ctx/entries c :doorway-tried))
                      (access/trespass-refusal (access/rules-input c) :place d))
          (seq (filter free? [d (update d :y inc)])))))))


;; ------------------------------------------------------------------ named places

(defn roofed-places-in
  "The remembered places of names (the night's :roofed-places) within radius and roofed within roof-height, as
  [{:name :pos}] nearest first. A roof cell the body has not seen is guessed solid (u/block-name-or): from the ground the
  cell over a home hides behind the roof's near edge; a column seen open is no roof."
  [c {:keys [roofed-places roof-height]} radius]
  (let [p (:primitives c)
        here (sh-sense/feet p)
        roofed? (fn [pos] (some #(solid/solid? (u/block-name-or p (update pos :y + %) "stone")) (range 1 (inc roof-height))))]
    (->> roofed-places
         (keep (fn [nm] (when-let [pos (mem/place (ctx/view c) nm)] {:name nm :pos pos})))
         (filter (fn [{:keys [pos]}] (and (<= (u/dist here pos) radius) (roofed? pos))))
         (sort-by #(u/dist here (:pos %))))))

(defn ^:async route-cost
  "Seconds go-to's quote plans for the walk go-to-args, dark cells priced in (the planner's own cost), or nil with no route."
  [c go-to-args]
  (let [r (:route (await (ctx/quote c 'jobs.movement.go-to go-to-args)))]
    (when (:found r) (+ (:seconds r) (or (:dark-seconds r) 0)))))
