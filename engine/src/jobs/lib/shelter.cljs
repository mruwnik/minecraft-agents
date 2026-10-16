(ns jobs.lib.shelter
  "What the night jobs (night, sleep, log-out, dig-in) and the night trigger share: night and roof tests, the bed to
  use, who sleeps on the server, the shelter entry."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.click :as click]
            [jobs.lib.reach :as reach]
            [jobs.lib.solid :as solid]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [engine.memory :as mem]
            [jobs.lib.world-files :as world]))

(a/defargs settings
  {::seen-bed-radius {:default 6 :spec (a/int-in 1 nil)
                      :doc "Blocks (square) within which a bed in the body's room counts as seen."}
   ::spawn-near {:default 3 :spec (a/int-in 0 nil)
                 :doc "Blocks within which a :spawn-set or :spawn-bed entry counts as the respawn point of a bed (either half)."}
   ::room-roof-height {:default 16 :spec (a/int-in 1 nil)
                       :doc "How high above the feet a roof may be and still make the closed room around the body a shelter."}
   ::breach-near {:default 8 :spec (a/int-in 0 nil)
                  :doc "Blocks within which a fresh :bed-open entry makes a roofed body's room count as breached when it is not shut in."}
   ::door-failed-ms {:default 600000 :spec (a/int-in 0 nil)
                     :doc "How long (ms) a room door the night could not shut is left alone from the cell it was tried at."}
   ::default-player-radius {:default 128 :spec (a/int-in 1 nil)
                            :doc "Blocks within which a sleeping player counts when the night is skipped."}})

(defn seen-bed-radius [] (settings/get settings ::seen-bed-radius))
(defn default-player-radius [] (settings/get settings ::default-player-radius))
(defn spawn-near [] (settings/get settings ::spawn-near))
(defn room-roof-height [] (settings/get settings ::room-roof-height))
(defn breach-near [] (settings/get settings ::breach-near))
(defn door-failed-ms [] (settings/get settings ::door-failed-ms))

(def ms-per-day
  "One in-game day in engine-clock milliseconds (20 minutes). \"A day\" in memory policies and :max-days-awake is
  this long."
  1200000)

(def default-roof-height 4)

(def default-bed-radius 48)

(defn feet [p] (solid/cell (u/self-pos {:primitives p})))


(defn solid-at? [p pos] (solid/solid? (u/seen-name p pos)))

(defn head-in-block?
  "Whether the body's eyes are inside a full block (it suffocates): buried in, not sheltered."
  [p]
  (breath/suffocates-here? p (breath/eye-cell (.self p))))

(defn roofed?
  "Whether a solid block lies within height blocks straight above the feet cell, the head not inside one."
  [p height]
  (let [{:keys [x y z]} (feet p)]
    (boolean (and (not (head-in-block? p))
                  (some #(solid-at? p {:x x :y (+ y %) :z z}) (range 1 (inc height)))))))

(defn sky-light-at
  "Sky light (0-15, before the night's darkening) of the cell, or nil when no light data is loaded there. Read only for the body's cells and cells it has seen."
  [p {:keys [x y z]}]
  (let [raw (some-> (aget p "perception") :raw)]
    (when (and raw (<= 0 (.stateAt ^js raw x y z)))
      (bit-shift-right (.lightAt ^js raw x y z) 4))))

(defn buried-by-column?
  "Fallback without light data: the cell two over the feet cell is a block the body has seen (only that one shows from inside)."
  ([p] (buried-by-column? p (feet p)))
  ([p {:keys [x y z]}]
   (let [above (look/seen-block p {:x x :y (+ y 2) :z z})]
     (boolean (and above (not (:unknown above)) (solid/solid? (:name above)))))))

(defn seen-cell?
  "Whether the body has seen the cell (its light data is no sight of it)."
  [p cell]
  (let [b (look/seen-block p cell)]
    (boolean (and b (not (:unknown b))))))

(defn buried-at?
  "Whether the cell is underground: no sky light at it and the cell over it (a cell at 0 is sealed from the sky, so
  the night changes nothing there; a ravine, shaft or cave mouth keeps sky light and stays unsafe). The brightness of
  a cell the body has seen is sensed like the light at its feet; an unseen cell is not buried. Without light data there,
  buried-by-column? decides."
  [p {:keys [x y z] :as cell}]
  (let [own (feet p)
        own? (= [x y z] [(:x own) (:y own) (:z own)])
        lights (keep #(sky-light-at p %) [cell {:x x :y (inc y) :z z}])]
    (cond
      (not (or own? (seen-cell? p cell))) false
      (seq lights) (every? zero? lights)
      :else (buried-by-column? p cell))))

(defn buried?
  "The body is underground (buried-at? its feet)."
  [p]
  (buried-at? p (feet p)))

(defn sheltered-here?
  "Whether a roof lies within default-roof-height over the body or it is underground: night finds it safe where it stands."
  [p]
  (or (roofed? p default-roof-height) (buried? p)))

(defn night? [p] (not (.-isDay (.self p))))

(defn sleeping? [p] (boolean (.-isSleeping (.self p))))

(def cave-radius 3)
(def cave-width
  "Standable cells under a ceiling within cave-radius of the feet from which the body counts as in an open cave, not a tunnel it dug."
  16)
(def cave-ceiling 8)
(def lit-level "Block light at the feet from which a cell is lit enough that the body counts it a lit pocket." 8)

(declare shut-in-room?)

(defn block-light-at
  "Block light (0-15) of the cell, or nil when no light data is loaded there."
  [p {:keys [x y z]}]
  (let [raw (some-> (aget p "perception") :raw)]
    (when (and raw (<= 0 (.stateAt ^js raw x y z)))
      (bit-and (.lightAt ^js raw x y z) 15))))

(defn light-at
  "The brighter of block and sky light (0-15, before the night's darkening) of the cell, or nil when no light data is
  loaded there."
  [p cell]
  (when-let [block (block-light-at p cell)]
    (max block (sky-light-at p cell))))

(defn cave-cells
  "How many standable cells (feet, head and the cell over it free, a solid floor, a seen solid within cave-ceiling over them) lie
  within cave-radius of the feet, a block up or down: a cave is wide and high, what the body digs is 2 high (a tunnel, a mine's room)."
  [p]
  (let [{:keys [x y z]} (feet p)
        solid? (fn [cx cy cz] (solid-at? p {:x cx :y cy :z cz}))]
    (count (for [dx (range (- cave-radius) (inc cave-radius))
                 dz (range (- cave-radius) (inc cave-radius))
                 dy [-1 0 1]
                 :let [cx (+ x dx) cy (+ y dy) cz (+ z dz)]
                 :when (and (solid? cx (dec cy) cz) (not (solid? cx cy cz)) (not (solid? cx (inc cy) cz)) (not (solid? cx (+ cy 2) cz))
                            (some #(solid? cx (+ cy %) cz) (range 2 (inc cave-ceiling))))]
             1))))

(defn open-cave?
  "Buried in a dark, wide natural cave: no sky light, not shut in a closed room, under lit-level block light at the
  feet (no light data counts as dark) and cave-cells wide. Mobs spawn there whatever the hour."
  [p]
  (boolean (and (buried? p) (not (shut-in-room? p))
                (< (or (block-light-at p (feet p)) 0) lit-level)
                (>= (cave-cells p) cave-width))))

(defn unsafe-night?
  "Night, awake, and not under a roof and not buried underground, or buried in a dark open cave (open-cave?)."
  [p roof-height]
  (and (night? p) (not (sleeping? p))
       (or (and (not (roofed? p roof-height)) (not (buried? p))) (open-cave? p))))

(defn bed-in-view
  "The remembered bed position when it is within radius of the body, else nil."
  [p view radius]
  (let [pos (mem/place view :bed)]
    (when (and pos (<= (u/dist (u/self-pos {:primitives p}) pos) radius))
      pos)))

(def slept-tonight-ms
  "A :slept entry younger than this counts as \"slept tonight\" (half an in-game day)."
  (/ ms-per-day 2))

(defn walkable-at?
  "Whether the cell at pos is one a body stands or walks in: not solid, or a bed."
  [p pos]
  (let [n (u/seen-name p pos)]
    (and (some? n) (or (not (solid/solid? n)) (.endsWith n "_bed")))))

(defn doorway?
  "Whether the cell is a gap in a wall: solid on both sides along one horizontal axis and free on both ends of the other."
  [p {:keys [x y z]}]
  (let [solid (fn [dx dz] (solid-at? p {:x (+ x dx) :y y :z (+ z dz)}))
        free (fn [dx dz] (not (solid dx dz)))]
    (boolean (or (and (solid 1 0) (solid -1 0) (free 0 1) (free 0 -1))
                 (and (solid 0 1) (solid 0 -1) (free 1 0) (free -1 0))))))


(defn bed-occupied?
  "Whether the bed at pos is occupied as a player sees it: the bed block's occupied state, or another player lying within
  a block of it (a bed is two cells)."
  [p {:keys [x y z] :as pos}]
  (let [b (u/seen-block p pos)
        props (when b (or (.-properties b) (some-> (.-getProperties b) (.call b))))
        me (.-username (.self p))]
    (boolean (or (true? (some-> props .-occupied))
                 (= "true" (some-> props .-occupied str))
                 (some #(and (.-sleeping %) (not= me (.-username %))
                             (<= (js/Math.abs (- (.-x (.-pos %)) x)) 1.5)
                             (<= (js/Math.abs (- (.-z (.-pos %)) z)) 1.5)
                             (<= (js/Math.abs (- (.-y (.-pos %)) y)) 1.5))
                       (array-seq (.entities p #js {:radius 6 :kind "player" :max 32})))))))

(defn in-own-zone?
  "Whether pos lies in a zone the body owns. kn is the engine world (nil: no zone known)."
  [p kn {:keys [x y z]}]
  (let [me (.-username (.self p))]
    (boolean (some #(and (zones/in-box? [x y z] %) (zones/same-owner? (:owner %) me)) (when kn (world/zones kn))))))

(defn bed-permit
  "A predicate (fn [pos]) for whether the body may use the bed at pos: any bed, in anyone's zone or claim (sleeping sets
  only the sleeper's own spawn), unless it is occupied; a bed in a zone the body owns is always a candidate (the server
  refuses an occupied one, which is a failed sleep). kn is the engine world (nil: no zone known)."
  [p kn]
  (fn [pos]
    (or (in-own-zone? p kn pos)
        (not (bed-occupied? p pos)))))

(defn seen-bed
  "A bed block the body can see in its room: found by a flood fill from its feet over free cells at feet height, within
  seen-bed-radius (a bed behind a wall is not seen), skipping beds permit? refuses (default all permitted). The nearest
  cell of it, or nil."
  ([p] (seen-bed p (constantly true)))
  ([p permit?]
  (let [{:keys [x y z] :as start} (feet p)]
    (loop [queue [start] seen #{start}]
      (when-let [pos (first queue)]
        (let [named (u/seen-name p pos)]
          (if (and (some? named) (.endsWith named "_bed") (not= pos start) (permit? pos))
            pos
            (let [next (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                             :let [n {:x (+ (:x pos) dx) :y y :z (+ (:z pos) dz)}]
                             :when (and (not (seen n))
                                        (<= (max (js/Math.abs (- (:x n) x)) (js/Math.abs (- (:z n) z))) (seen-bed-radius))
                                        (walkable-at? p n))]
                         n)]
              (recur (into (subvec (vec queue) 1) next) (into seen next))))))))))

(def sleep-failed-policy
  "The :sleep-failed entry the night job writes when a sleep ended without sleeping (bed taken, monsters near, ...):
  one, five minutes, so the night trigger does not refire on the same bed every cooldown."
  {:cap 1 :ttl 300000})

(def urgent-bed-radius 128)

(def max-days-awake 3)

(defn rest-since
  "When the body's time since rest began, in engine-clock ms, or nil on an empty memory: the latest of its :slept
  entry, its :respawned entry and its latest death (:deaths, kept for good), else its oldest entry of any kind (the
  first thing it remembers)."
  [view]
  (let [death (some-> (mem/latest view :deaths) :data :death-t)
        ts (concat (keep #(:t (mem/latest view %)) [:slept :respawned]) (when death [death]))]
    (if (seq ts)
      (apply max ts)
      (some->> (keys (get-in view [:data :entries])) (mapcat #(mem/entries view %)) (map :t) seq (apply min)))))

(defn days-awake-in
  "In-game days since the body last rested (rest-since: its sleep or respawn, else its first memory), or nil when
  it remembers nothing."
  [view]
  (when-let [t (rest-since view)]
    (/ (- (:now view) t) ms-per-day)))

(defn bed-radius
  "How far a remembered bed may be: :bed-radius, or :urgent-bed-radius once the latest :slept entry is :max-days-awake
  in-game days old (phantoms come), or while a :displaced entry stands (a far flight drove the body off its bed)."
  [view {:keys [bed-radius urgent-bed-radius max-days-awake]
         :or {bed-radius default-bed-radius urgent-bed-radius urgent-bed-radius max-days-awake max-days-awake}}]
  (let [days (days-awake-in view)]
    (if (or (and days (>= days max-days-awake)) (seq (mem/entries view :displaced))) urgent-bed-radius bed-radius)))

(declare walk-leaves-shelter? standing-beds)

(def kept-shelters
  "The shelters (walk-leaves-shelter?) a body does not leave at night for a bed: a bare roof is not one of them."
  #{:underground :dug-in :room})

(def bed-open-policy
  "Policy of the :bed-open entries the night job writes for a bed it found open and dug in beside instead: half a day."
  {:cap 5 :ttl (quot ms-per-day 2)})

(defn bed-to-use
  "The bed a body awake at night sleeps in: one it sees (seen-bed), else the remembered :bed within radius, under any
  roof or none. Never one permit? refuses (occupied), one in an unexpired :bed-unreachable entry, or one whose walk
  would take the body out of a kept-shelters shelter (unless overdue: max-days-awake days since :slept, phantoms);
  none after a sleep tonight (:slept within half an in-game day)
  or while a :sleep-failed entry is unexpired. nil otherwise."
  [p view radius permit?]
  (when (and (night? p) (not (sleeping? p))
             (zero? (mem/count-in view :slept slept-tonight-ms))
             (empty? (mem/entries view :sleep-failed)))
    (let [shelter (:data (mem/latest view :shelter))
          overdue? (>= (or (days-awake-in view) 0) max-days-awake)
          ok? (fn [pos] (and (permit? pos) (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-unreachable))
                             (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-open))
                             (or overdue? (not (kept-shelters (walk-leaves-shelter? p shelter pos 1))))))]
      (or (seen-bed p ok?)
          (first (filter ok? (standing-beds p view radius)))))))

(def bed-place-failed-policy
  "Policy of the :bed-place-failed entry the night job writes when it could not set up a carried bed: one, ten
  minutes."
  {:cap 1 :ttl 600000})

(def bed-placed-policy
  "Policy of the :bed-placed entries: a bed the night job put down outside the body's own zones, picked up by day."
  {:cap 4 :ttl (* 7 ms-per-day)})

(def doorway-filled-policy
  "Policy of the :doorway-filled entries: a doorway cell the night filled (jobs.survival.night.doorway), opened by day."
  {:cap 4 :ttl ms-per-day})

(def doorway-open-failed-policy
  "Policy of the :doorway-open-failed entries: a filled doorway cell the day failed to open, not tried again until it lapses."
  {:cap 4 :ttl (* 2 60 1000)})

(defn carried-bed
  "The name of a bed item the body carries (any colour), or nil."
  [p]
  (some #(when (.endsWith (:name %) "_bed") (:name %)) (u/inventory p)))

(defn near-failed-place?
  "Whether a :bed-place-failed entry still holds: the body is within 6 blocks of where it failed (no position: always)."
  [p entry]
  (let [pos (:pos (:data entry))]
    (or (nil? pos) (< (u/dist (u/self-pos {:primitives p}) pos) 6))))

(defn bed-place-wanted?
  "Whether to put a carried bed down: night, awake, a bed item carried, no bed-to-use, no sleep tonight, and no
  :bed-place-failed or :sleep-failed entry. Roofed or in the open."
  [p view radius permit?]
  (boolean (and (night? p) (not (sleeping? p))
                (some? (carried-bed p))
                (zero? (mem/count-in view :slept slept-tonight-ms))
                (not-any? #(near-failed-place? p %) (mem/entries view :bed-place-failed))
                (empty? (mem/entries view :sleep-failed))
                (nil? (bed-to-use p view radius permit?)))))

(def spawn-bed-policy {:cap 1 :ttl :forever})

(defn spawn-bed-entry
  "The latest :spawn-bed entry, live or not."
  [view]
  (mem/latest view :spawn-bed))

(defn spawn-ended-at
  "When the respawn point was last reset (:spawn-reset, the server's word) or found lost (:spawn-lost), or nil."
  [view]
  (some->> [:spawn-reset :spawn-lost] (keep #(:t (mem/latest view %))) seq (apply max)))

(defn spawn-bed-live
  "The durable :spawn-bed entry of view unless a :spawn-reset or :spawn-lost came after it."
  [view]
  (let [e (spawn-bed-entry view)
        ended (spawn-ended-at view)]
    (when-not (and e ended (> ended (:t e)))
      e)))

(defn spawn-lost-pos
  "The position of the respawn bed the body knows is gone: the latest :spawn-bed when a reset or loss came after it,
  or the live one whose block is seen and no longer a bed."
  [p view]
  (let [e (spawn-bed-entry view)
        pos (:pos (:data e))
        ended (spawn-ended-at view)]
    (cond
      (nil? pos) nil
      (and ended (> ended (:t e))) pos
      (let [n (u/seen-name p pos)] (and n (not (.endsWith n "_bed")))) pos)))

(defn seen-not-bed?
  "Whether the block at pos is seen and is no bed."
  [p pos]
  (let [n (u/seen-name p pos)]
    (and (some? n) (not (.endsWith n "_bed")))))

(defn known-beds
  "The remembered :bed place and the live :spawn-bed (unless its block is seen not a bed), at any distance, nearest first."
  [p view]
  (let [me (u/self-pos {:primitives p})
        spawn (:pos (:data (spawn-bed-live view)))
        place (mem/place view :bed)
        ok? #(or (nil? %) (not (seen-not-bed? p %)))]
    (->> [(when (ok? place) place) (when (ok? spawn) spawn)]
         (filter some?)
         distinct
         (sort-by #(u/dist me %)))))

(defn standing-beds
  "The known-beds within radius, nearest first (the :bed place's job retracts it on finding it gone; the live
  :spawn-bed is set to a bed the body slept in or set the spawn at, whichever half the place key holds)."
  [p view radius]
  (let [me (u/self-pos {:primitives p})]
    (filter #(<= (u/dist me %) radius) (known-beds p view))))

(defn standing-bed
  "The nearest of standing-beds, or nil."
  [p view radius]
  (first (standing-beds p view radius)))

(defn spawn-state
  "What the body knows of its respawn point: {:bed pos} (the live :spawn-bed), {:lost pos :why w} (:why the :spawn-lost entry's,
  :reset, or :seen-gone), else nil."
  [p view]
  (if-let [pos (spawn-lost-pos p view)]
    (let [t (:t (spawn-bed-entry view))
          after? (fn [kind] (when-let [e (mem/latest view kind)] (> (:t e) t)))]
      {:lost pos :why (cond (after? :spawn-lost) (:why (:data (mem/latest view :spawn-lost)))
                            (after? :spawn-reset) :reset
                            :else :seen-gone)})
    (when-let [e (spawn-bed-live view)]
      {:bed (:pos (:data e))})))

(def spawn-lost-policy {:cap 1 :ttl :forever})

(defn lose-spawn!
  "A job found the bed at pos gone or cut off: when it is the live respawn bed, remember :spawn-lost and warn
  spawn_lost once (why: a short word)."
  [c pos why]
  (let [view (ctx/view c)]
    (when (some-> (:pos (:data (spawn-bed-live view))) (u/dist pos) (<= (spawn-near)))
      (ctx/remember! c :spawn-lost {:pos pos :why why} spawn-lost-policy)
      (ctx/emit! c :spawn_lost :warn {:pos pos :why why :text (str "the respawn bed at " (pr-str pos) " is gone (" (name why) "): the next death respawns at the world spawn")}))))

(defn spawn-bed-at?
  "Whether the live :spawn-bed is within spawn-near of pos (the bed's other half counts)."
  [view pos]
  (boolean (some-> (:pos (:data (spawn-bed-live view))) (u/dist pos) (<= (spawn-near)))))

(defn spawn-known?
  "Whether the body has seen the respawn point set at bed since t0, or earlier and not reset since (the server says it
  only when the point changes, so a bed slept in before is silent; :spawn-bed outlives the :spawn-set entry)."
  [view bed t0]
  (let [near? #(some-> (:pos (:data %)) (u/dist bed) (<= (spawn-near)))
        reset-t (:t (mem/latest view :spawn-reset))
        current? #(or (>= (:t %) t0) (nil? reset-t) (> (:t %) reset-t))]
    (boolean (or (some #(and (current? %) (or (>= (:t %) t0) (near? %))) (mem/entries view :spawn-set))
                 (some-> (spawn-bed-live view) near?)))))

(defn bed-to-collect
  "By day: the latest :bed-placed position while a bed block still stands there and is not the respawn point
  (:spawn-bed), else nil."
  [p view]
  (when-not (night? p)
    (let [pos (:pos (:data (mem/latest view :bed-placed)))]
      (when (and pos (.endsWith (or (u/seen-name p pos) "") "_bed")
                 (not (spawn-bed-at? view pos)))
        pos))))

(defn still-filled?
  "Whether a :doorway-filled entry's cell still holds one of its :blocks (the block the night put there)."
  [p {:keys [cell blocks]}]
  (some? (some #{(u/seen-name p cell)} blocks)))

(defn fill-gone?
  "Whether a :doorway-filled entry's cell is seen holding none of its :blocks: opened, or changed by someone else."
  [p {:keys [cell blocks]}]
  (let [n (u/seen-name p cell)]
    (and (some? n) (not (some #{n} blocks)))))

(defn doorway-to-open
  "By day: the cells {:x :y :z} of the :doorway-filled entries that are still-filled?, none failed to open lately
  (:doorway-open-failed), oldest first; nil when none."
  [p view]
  (when-not (night? p)
    (let [failed (set (map (comp :cell :data) (mem/entries view :doorway-open-failed)))]
      (seq (keep #(let [{:keys [cell] :as d} (:data %)] (when (and (not (failed cell)) (still-filled? p d)) cell))
                 (mem/entries view :doorway-filled))))))

(defn bed
  "The remembered bed position when it is within radius of the body, else nil."
  [c radius]
  (bed-in-view (:primitives c) (ctx/view c) radius))

(defn sleeping-players
  "Other players within radius the body sees asleep (the primitives report a sleeping pose only in sight)."
  [p radius]
  (let [me (.-username (.self p))]
    (filterv #(and (.-sleeping %) (not= me (.-username %)))
             (array-seq (.entities p #js {:radius radius :kind "player" :max 32})))))

(defn log-out-unsupported?
  "Whether the latest :log-out entry says the server does not support logging out."
  [view]
  (= "unsupported" (:status (:data (mem/latest view :log-out)))))

(def morning-tick
  "The first time of day (ticks) that self().isDay counts as day again after a night."
  23461)

(def dusk-tick
  "The first time of day (ticks) that self().isDay counts as night."
  12542)

(def home-limit "Farthest home, in blocks, worth the walk (go-home's :limit default, the dusk-home trigger's bound)." 160)

(def day-tick "The time of day (game ticks) at which the sun is up again (engine.senses/day-at?)." 23460)

(def night-end-tick "The tick the night ends at most." 24000)

(defn ticks->s [n] (/ (settings/ticks->ms n) 1000))

(defn night-left-quote
  "The :uses :job-time quote of the night left, from the body's self: [0 mean most] seconds at the live game rate (##Inf
  most while the server's tick is frozen); zero by day."
  [c]
  (let [self (.self (:primitives c))
        t (.-timeOfDay self)]
    (if (.-isDay self)
      {:uses {:job-time [0 0 0]}}
      {:uses {:job-time [0 (ticks->s (max 0 (- day-tick t)))
                     (if (settings/frozen?) ##Inf (ticks->s (max 0 (- night-end-tick t))))]}})))

(def morning-margin-ms
  "Extra time away, so a body that logged out until morning comes back after the night has ended."
  2000)

(defn ms-until-morning
  "Milliseconds (at the live game rate) until the night ends at time of day t (ticks), plus morning-margin-ms; 0 by day."
  [t]
  (if (or (< t dusk-tick) (>= t morning-tick))
    0
    (+ (settings/ticks->ms (- morning-tick t)) morning-margin-ms)))

(defn ms-since-dusk
  "Milliseconds since this night began at time of day t (ticks); 0 by day."
  [t]
  (if (or (< t dusk-tick) (>= t morning-tick)) 0 (settings/ticks->ms (- t dusk-tick))))

(defn tonight
  "The engine time this night began: entries written since are tonight's."
  [p view]
  (- (:now view) (ms-since-dusk (.-timeOfDay (.self p)))))

(defn latest-since [view kind t]
  (let [e (mem/latest view kind)]
    (when (and e (>= (:t e) t)) e)))

(defn others-asleep?
  "Whether someone on the server sleeps tonight, as a player can tell: the latest :sleep-status entry tonight (the
  action bar's count, sent when it changes) counts a sleeper or says the night is skipped; or a sleeping player is
  in sight; or the body is back from a log-out tonight and no count has come since its return (:online): unknown is
  taken as still asleep. A count sent before the body woke from its own sleep tonight counts the body: it then needs
  another sleeper, and a skipped night tells nothing."
  [p view]
  (let [since (tonight p view)
        status (latest-since view :sleep-status since)
        out (latest-since view :log-out since)
        back (:t (mem/latest view :online))
        woke (:t (mem/latest view :woke))
        slept (:t (latest-since view :slept since))
        own? (and status woke slept (<= slept woke) (<= (:t status) woke))]
    (boolean (or (and out back (#{"ok" "cut"} (:status (:data out))) (or (nil? status) (< (:t status) back)))
                 (and status (if own?
                               (> (:sleeping (:data status) 0) 1)
                               (or (:skipping (:data status)) (pos? (:sleeping (:data status) 0)))))
                 (seq (sleeping-players p (default-player-radius)))))))

(defn log-out-for-sleepers?
  "Night, awake, someone else asleep (others-asleep?), and no log-out tonight that failed (unsupported, closed)."
  [p view]
  (let [out (latest-since view :log-out (tonight p view))]
    (and (night? p) (not (sleeping? p))
         (not (log-out-unsupported? view))
         (or (nil? out) (contains? #{"ok" "cut"} (:status (:data out))))
         (others-asleep? p view))))

(defn days-awake
  "In-game days since the body last rested (days-awake-in), or nil when it remembers nothing."
  [c]
  (days-awake-in (ctx/view c)))

(defn shut-in?
  "Whether the body is still shut in by its shelter entry. Never for a mended :room (its door is the way out). With a
  pit :start, when below that height and walled in (a pit left by a stair is open). With a :door, when a door cell is solid. With neither, when under a solid
  block within default-roof-height."
  [p {:keys [start door room]}]
  (cond
    room false
    start (and (< (:y (feet p)) (:y start)) (reach/enclosed? p))
    door (boolean (some #(solid-at? p %) door))
    :else (roofed? p default-roof-height)))

(defn in-own-shelter
  "The shelter entry data when the body stands in its :pos and is shut in it, else nil."
  [p entry]
  (when (and entry (= (:pos entry) (feet p)) (shut-in? p entry))
    entry))

(defn sheltered-in
  "The body's latest :shelter entry when the body stands in its :pos and is shut in it, else nil."
  [c]
  (in-own-shelter (:primitives c) (:data (ctx/latest c :shelter))))


(defn safe-from-night?
  "Whether the body is safe from the night where it stands: a roof within roof-height (default default-roof-height) over it, buried,
  or shut in a closed room (shut-in-room?), never with the head inside a block. The one notion the night trigger,
  night-sheltered and walk-leaves-shelter? share."
  ([p] (safe-from-night? p default-roof-height))
  ([p roof-height] (and (not (head-in-block? p)) (or (not (unsafe-night? p roof-height)) (shut-in-room? p)))))

(defn night-sheltered
  "At night, awake and safe from it (roofed or buried, or shut-in-room?), the kind of shelter the body is in (:dug-in
  the one shelter says it is shut in, :room, :underground, :roofed), else nil: outdoor work waits for day then."
  [p shelter]
  (when (and (night? p) (not (sleeping? p)) (safe-from-night? p))
    (cond
      (in-own-shelter p shelter) :dug-in
      (shut-in-room? p) :room
      (buried? p) :underground
      :else :roofed)))

(defn open-air-above?
  "Whether every cell over goal up to height y is seen and not solid: a spot in the open, like a hillside below a roofed hut."
  [p goal y]
  (every? #(let [n (u/seen-name p (update goal :y + %))] (and n (not (solid/solid? n)))) (range 1 (inc (- y (:y goal))))))

(defn seen-open-over?
  "Whether some cell within default-roof-height over goal is seen and not solid: rock shows no cell beyond its face."
  [p goal]
  (boolean (some #(some-> (u/seen-name p (update goal :y + %)) solid/solid? not) (range 1 (inc default-roof-height)))))

(defn walk-leaves-shelter?
  "At night, awake and safe from it (roofed or buried, or shut-in-room?), the shelter a walk to goal ({:x :y :z}, or {:x
  :z} a column) within dist would leave, else nil. A goal within dist of the feet leaves none. :dug-in: shelter (the
  latest :shelter entry) is the one the body is shut in. :room: no cell of the closed room is within dist of the goal.
  :underground: buried, and the goal is not below the feet (unless open-air-above?), has nothing solid within default-roof-height over it, and is no solid block with no open cell seen over it (a wall in the rock: a walk deeper stays underground). :roofed: any other roof."
  [p shelter goal dist]
  (when (and (night? p) (not (sleeping? p)) (safe-from-night? p))
    (let [goal (update-vals goal js/Math.floor)
          near? (fn [cell] (if (:y goal) (u/within? cell goal dist) (u/within? (assoc cell :y 0) (assoc goal :y 0) dist)))
          room (when (shut-in-room? p) (reach/room p (feet p)))]
      (cond
        (near? (feet p)) nil
        (in-own-shelter p shelter) :dug-in
        room (when-not (some (fn [[x y z]] (near? {:x x :y y :z z})) (:cells room)) :room)
        (buried? p) (when-not (and (:y goal) (or (and (< (:y goal) (:y (feet p))) (not (open-air-above? p goal (:y (feet p)))))
                                                 (some #(solid-at? p (update goal :y + %)) (range 1 (inc default-roof-height)))
                                                 (and (solid-at? p goal) (not (seen-open-over? p goal)))))
                      :underground)
        :else :roofed))))

(def trapped-policy
  "Policy of the :shelter-trapped entries written when leave! found no way out: up to 3, five minutes."
  {:cap 3 :ttl 300000})

(defn shut-in-by-day?
  "By day: the body is shut in its own shelter (data) and no :shelter-trapped entry (data) says leave! found no way
  out of this cell. Never at night."
  [p shelter trapped]
  (boolean (and (not (night? p))
                (in-own-shelter p shelter)
                (not= (:pos shelter) (:pos trapped)))))

(defn shelter-hint
  "{:inside-own-shelter pos :hint text} when the body is sealed in its own shelter (a go-to, attack or hunt that cannot
  reach its target says so), else nil."
  [c]
  (when-let [{{:keys [x y z]} :pos :as entry} (sheltered-in c)]
    {:inside-own-shelter (:pos entry)
     :hint (str "the body is sealed in its own shelter at [" x " " y " " z "]: the shelter job lets it out by day "
                "(jobs.survival.night), or run jobs.survival.dig-in leave")}))

(defn wide-room?
  "Whether the room has a 2x2 floor patch: a hut, not a one-wide tunnel the body dug."
  [{:keys [cells]}]
  (let [in? (set cells)]
    (boolean (some (fn [[x y z]] (and (in? [(inc x) y z]) (in? [x y (inc z)]) (in? [(inc x) y (inc z)]))) in?))))

(defn shut-in-room?
  "Whether the body stands in a closed room (reach/closed-room?) with a roof over it within room-roof-height blocks:
  safe from the night though the roof is above :roof-height. Sealed from the sky (buried?) counts as roofed: the roof
  over the feet cannot be looked at from inside."
  [p]
  (and (or (roofed? p (room-roof-height)) (buried? p)) (reach/closed-room? p (feet p))))

(defn shut-in-hut?
  "shut-in-room? and wide: a hut, not a one-wide tunnel the body dug."
  [p]
  (and (shut-in-room? p) (wide-room? (reach/room p (feet p)))))

(def night-looked-policy {:cap 1 :ttl (quot ms-per-day 2)})

(defn looked-here?
  "Whether the night's look pass (night.cljs, remembered as :night-looked {:cell}) was from this room or within two
  blocks: a cell that shows no face (a tunnel's diagonals) stays unseen whatever is looked at."
  [p view]
  (let [self (feet p)
        {:keys [cell]} (:data (mem/latest view :night-looked))]
    (boolean (and cell
                  (or (<= (u/dist self cell) 2)
                      (contains? (:cells (reach/room p self)) [(:x cell) (:y cell) (:z cell)]))))))

(defn unlooked-for-bed?
  "Night, awake, not looked from here (looked-here?), and either a bed carried or remembered with a cell beside the feet
  (floor, feet or head) never seen (the room checks, shut-in-hut? and walk-leaves-shelter?, would read the room smaller
  than it is), or the body shut in a hut with no sleep tonight and no :sleep-failed entry (a bed may stand where it has
  not looked, or have been set down since it saw the cell): look before deciding."
  [p view]
  (let [{:keys [x y z]} (feet p)]
    (boolean (and (night? p) (not (sleeping? p))
                  (not (looked-here? p view))
                  (or (and (or (some? (carried-bed p)) (some? (mem/place view :bed)))
                           (look/unseen? p (for [dx [-1 0 1] dz [-1 0 1] dy [-1 0 1]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
                      (and (zero? (mem/count-in view :slept slept-tonight-ms))
                           (empty? (mem/entries view :sleep-failed))
                           (shut-in-hut? p)))))))

(defn unseen-roof-cell
  "[x y z] of the first cell over the feet, within room-roof-height and under no seen solid block, the body has not seen:
  a level look never shows the roof, so it is looked at before the room is judged open; nil when roofed or all seen."
  [p]
  (let [{:keys [x y z]} (feet p)]
    (loop [dy 1]
      (when (<= dy (room-roof-height))
        (let [cell {:x x :y (+ y dy) :z z}]
          (cond
            (solid-at? p cell) nil
            (look/unseen? p [cell]) [x (+ y dy) z]
            :else (recur (inc dy))))))))

(defn unlooked-for-roof?
  "Night, awake, not looked from here (looked-here?) and a cell over the feet never seen (unseen-roof-cell): look up."
  [p view]
  (boolean (and (night? p) (not (sleeping? p)) (not (looked-here? p view)) (unseen-roof-cell p))))

(defn breached-home?
  "Whether the roofed body's room has a hole: a :bed-open entry (a bed found open) within breach-near blocks and the body
  not shut in a closed room. A bare roof alone is no sign of a home."
  [p view]
  (let [self (feet p)]
    (boolean (and (roofed? p (room-roof-height))
                  (some #(<= (u/dist self (:pos (:data %))) (breach-near)) (mem/entries view :bed-open))
                  (not (shut-in-room? p))
                  (not (in-own-shelter p (:data (mem/latest view :shelter))))))))

(defn door-key [{:keys [x y z]}] [x y z])

(defn open-door?
  "Whether the {:cell} door entry stands open (a door or gate seen open)."
  [p {:keys [cell]}]
  (let [b (u/seen-block p cell)]
    (and b (#{:openable :iron} (click/kind-of (.-name b))) (click/reached? :open (click/props-of b)))))

(defn open-room-doors
  "[{:cell :inside} ...]: the doors and gates standing open in the closed room around the body, the nearest first."
  [p]
  (when (roofed? p (room-roof-height))
    (let [self (u/self-pos {:primitives p})]
      (->> (:doors (reach/room p (feet p)))
           (filter #(open-door? p %))
           (sort-by #(u/dist self (:cell %)))))))

(def breach-door-reach
  "Blocks (square) around the feet searched for an open door when the room has a hole."
  3)

(defn open-doors
  "open-room-doors, else (breached-home?) the doors and gates standing open within breach-door-reach of the feet, the
  nearest first: a hole in the walls hides the room, not its door."
  [p view]
  (or (seq (open-room-doors p))
      (when (breached-home? p view)
        (let [{:keys [x y z] :as self} (feet p)
              r (range (- breach-door-reach) (inc breach-door-reach))]
          (->> (for [dx r dz r :let [e {:cell {:x (+ x dx) :y y :z (+ z dz)} :inside self}] :when (open-door? p e)] e)
               (sort-by #(u/dist (u/self-pos {:primitives p}) (:cell %))))))))

(defn open-room-door
  "{:cell :inside} of the nearest door or gate standing open in the closed room around the body (shut: the room keeps the
  night out), not one the night failed to shut (the :night-door-failed entry in view, keyed on the door); nil when none."
  [p view]
  (let [failed (:door (:data (mem/latest view :night-door-failed)))]
    (first (remove #(= failed (door-key (:cell %))) (open-doors p view)))))

(defn night-holds?
  "Whether the night trigger holds (see triggers.survival.night). permit? decides whether a bed may be used (bed-permit).
  Without :bed in (:ways args) (all when absent) there are no bed terms and no bed-look.
  A buried body is not pulled out of the ground to place a bed or log out: the night changes nothing there."
  [p view args permit?]
  (let [shelter (:data (mem/latest view :shelter))
        buried (and (buried? p) (not (shut-in-hut? p)))
        bed-way (or (nil? (:ways args)) (contains? (:ways args) :bed))]
    (boolean
     (if (night? p)
       (and (not (sleeping? p))
            (or (and bed-way
                     (or (some? (bed-to-use p view (bed-radius view args) permit?))
                         (unlooked-for-bed? p view)
                         (and (not buried) (bed-place-wanted? p view (bed-radius view args) permit?))
                         (and (not buried) (log-out-for-sleepers? p view))))
                (not (safe-from-night? p (:roof-height args default-roof-height)))
                (breached-home? p view)
                (seq (open-doors p view))))
       (or (shut-in-by-day? p shelter (:data (mem/latest view :shelter-trapped)))
           (some? (bed-to-collect p view))
           (some? (doorway-to-open p view)))))))

(defn home-target
  "The place to go home to before night: the remembered :home, else :bed (view: an engine.memory view)."
  [view]
  (or (mem/place view :home) (mem/place view :bed)))

(def shelter-policy {:cap 10 :ttl ms-per-day})
