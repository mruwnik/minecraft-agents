(ns jobs.lib.shelter
  "What the night jobs (night, sleep, log-out, shut-in) and the night trigger share: the night's look pass, open doors, the
  night trigger's test. The rest lives in jobs.lib.shelter.*."
  (:require [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.look :as look]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.util :as u]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]
            [jobs.lib.shelter.spawn :as sh-spawn]))

(a/defargs settings
  {::breach-near {:default 8 :spec (a/int-in 0 nil)
                  :doc "Blocks within which a fresh :bed-open entry makes a roofed body's room count as breached when it is not shut in."}
   ::door-failed-ms {:default 600000 :spec (a/int-in 0 nil)
                     :doc "How long (ms) a room door the night could not shut is left alone from the cell it was tried at."}})

(defn breach-near [] (settings/get settings ::breach-near))

(defn door-failed-ms [] (settings/get settings ::door-failed-ms))

(def night-looked-policy {:cap 1 :ttl (quot sh-clock/ms-per-day 2)})

(defn looked-here?
  "Whether the night's look pass (night.cljs, remembered as :night-looked {:cell}) was from this room or within two
  blocks: a cell that shows no face (a tunnel's diagonals) stays unseen whatever is looked at."
  [p view]
  (let [self (sh-sense/feet p)
        {:keys [cell]} (:data (mem/latest view :night-looked))]
    (boolean (and cell
                  (or (<= (u/dist self cell) 2)
                      (contains? (:cells (reach-room/room p self)) [(:x cell) (:y cell) (:z cell)]))))))

(defn unlooked-for-bed?
  "Night, awake, not looked from here (looked-here?), and either a bed carried or remembered within bed-radius with a cell beside the feet
  (floor, feet or head) never seen (the room checks, shut-in-hut? and walk-leaves-shelter?, would read the room smaller
  than it is), or the body shut in a hut with no sleep tonight and no :sleep-failed entry (a bed may stand where it has
  not looked, or have been set down since it saw the cell): look before deciding."
  [p view]
  (let [{:keys [x y z]} (sh-sense/feet p)]
    (boolean (and (sh-sense/night? p) (not (sh-sense/sleeping? p))
                  (not (looked-here? p view))
                  (or (and (or (some? (sh-beds/carried-bed p)) (some? (sh-spawn/standing-bed p view (sh-beds/bed-radius view))))
                           (look/unseen? p (for [dx [-1 0 1] dz [-1 0 1] dy [-1 0 1]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
                      (and (zero? (mem/count-in view :slept sh-clock/slept-tonight-ms))
                           (empty? (mem/entries view :sleep-failed))
                           (sh-sense/shut-in-hut? p)))))))

(defn unseen-roof-cell
  "[x y z] of the first cell over the feet, within room-roof-height and under no seen solid block, the body has not seen:
  a level look never shows the roof, so it is looked at before the room is judged open; nil when roofed or all seen."
  [p]
  (let [{:keys [x y z]} (sh-sense/feet p)]
    (loop [dy 1]
      (when (<= dy (sh-sense/room-roof-height))
        (let [cell {:x x :y (+ y dy) :z z}]
          (cond
            (sh-sense/solid-at? p cell) nil
            (look/unseen? p [cell]) [x (+ y dy) z]
            :else (recur (inc dy))))))))

(defn unlooked-for-roof?
  "Night, awake, not looked from here (looked-here?) and a cell over the feet never seen (unseen-roof-cell): look up."
  [p view]
  (boolean (and (sh-sense/night? p) (not (sh-sense/sleeping? p)) (not (looked-here? p view)) (unseen-roof-cell p))))

(defn breached-home?
  "Whether the roofed body's room has a hole: a :bed-open entry (a bed found open) within breach-near blocks and the body
  not shut in a closed room. A bare roof alone is no sign of a home."
  [p view]
  (let [self (sh-sense/feet p)]
    (boolean (and (sh-sense/roofed? p (sh-sense/room-roof-height))
                  (some #(<= (u/dist self (:pos (:data %))) (breach-near)) (mem/entries view :bed-open))
                  (not (sh-sense/shut-in-room? p))
                  (not (sh-inside/in-own-shelter p (:data (mem/latest view :shelter))))))))

(defn door-key [{:keys [x y z]}] [x y z])

(defn open-room-doors
  "[{:cell :inside} ...]: the doors and gates standing open in the closed room around the body, the nearest first."
  [p]
  (when (sh-sense/roofed? p (sh-sense/room-roof-height))
    (let [self (u/self-pos {:primitives p})]
      (->> (:doors (reach-room/room p (sh-sense/feet p)))
           (filter #(sh-inside/open-door? p %))
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
        (let [{:keys [x y z] :as self} (sh-sense/feet p)
              r (range (- breach-door-reach) (inc breach-door-reach))]
          (->> (for [dx r dz r :let [e {:cell {:x (+ x dx) :y y :z (+ z dz)} :inside self}] :when (sh-inside/open-door? p e)] e)
               (sort-by #(u/dist (u/self-pos {:primitives p}) (:cell %))))))))

(defn open-room-door
  "{:cell :inside} of the nearest door or gate standing open in the closed room around the body (shut: the room keeps the
  night out), not one the night failed to shut (the :night-door-failed entry in view, keyed on the door); nil when none."
  [p view]
  (let [failed (:door (:data (mem/latest view :night-door-failed)))]
    (first (remove #(= failed (door-key (:cell %))) (open-doors p view)))))

(defn buried-outside-hut? [p] (and (sh-sense/buried? p) (not (sh-sense/shut-in-hut? p))))

(defn bed-holds?
  "The bed terms of night-holds?: night by the clock, awake, the :bed way, and a bed to use, a look round for one, a
  carried bed to put down, or someone else asleep."
  [p view args permit?]
  (let [buried (buried-outside-hut? p)]
    (boolean
     (and (sh-sense/night? p)
          (not (sh-sense/sleeping? p))
          (or (nil? (:ways args)) (contains? (:ways args) :bed))
          (or (some? (sh-beds/bed-to-use p view (sh-beds/bed-radius view) permit?))
              (unlooked-for-bed? p view)
              (and (not buried) (sh-beds/bed-place-wanted? p view (sh-beds/bed-radius view) permit?))
              (and (or (not buried) (sh-inside/in-own-shelter p (:data (mem/latest view :shelter)))) (sh-sleepers/log-out-for-sleepers? p view)))))))

(defn night-holds?
  "Whether the night trigger holds (see triggers.survival.night). permit? decides whether a bed may be used (bed-permit).
  Without :bed in (:ways args) (all when absent) there are no bed terms and no bed-look.
  A buried body is not pulled out of the ground to place a bed and logs out only from its own night shelter (a miner
  mines on); with :buried-works in args (the trigger) it is not unsafe from the night either: no sky light reaches it,
  so waiting changes nothing, and cave dangers are the hostile reflexes'. The night job asked for by hand still shelters
  or lights it. The bed terms hold by the clock (a bed takes a sleeper from dusk); the shelter terms only while the sky
  is dark (sh-sense/dark-outside?), as the night job's own end is. By day never (leave-wanted?)."
  [p view args permit?]
  (boolean
   (and (not (sh-sense/sleeping? p))
        (or (bed-holds? p view args permit?)
            (and (sh-sense/dark-outside? p)
                 (or (and (not (and (buried-outside-hut? p) (:buried-works args))) (not (sh-inside/safe-from-night? p)))
                     (breached-home? p view)
                     (seq (open-doors p view))))))))

(defn leave-wanted?
  "Whether the dawn trigger holds (see triggers.survival.dawn): by day, shut in the body's own shelter (no
  :shelter-trapped entry for its cell), a doorway the night filled to open, or a bed it put down to take up."
  [p view]
  (boolean
   (and (not (sh-sense/night? p))
        (or (sh-inside/shut-in-by-day? p (:data (mem/latest view :shelter)) (:data (mem/latest view :shelter-trapped)))
            (some? (sh-beds/bed-to-collect p view))
            (some? (sh-beds/doorway-to-open p view))))))

(defn home-target
  "The place to go home to before night: the remembered :home, else :bed (view: an engine.memory view)."
  [view]
  (or (mem/place view :home) (mem/place view :bed)))

(def home-due-defaults "The dusk-home trigger's timing (triggers.survival.dusk-home)." {:margin-s 60 :speed 4.3 :slack 1.4 :arrive 4})

(defn home-due-at?
  "Whether a body standing at pos would be due home before night: its home-target within :limit and farther than
  :arrive, and the day left no longer covering the walk (:slack times the straight line at :speed blocks a second) plus
  :margin-s. args are merged over home-due-defaults and sh-clock/home-limit."
  [p view pos args]
  (let [{:keys [limit margin-s speed slack arrive]} (merge home-due-defaults {:limit sh-clock/home-limit} args)
        left-s (/ (- sh-clock/dusk-tick (.-timeOfDay (.self p))) 20)
        home (home-target view)]
    (boolean
     (and home (pos? left-s)
          (let [d (u/dist pos home)]
            (and (< arrive d) (<= d limit) (<= left-s (+ margin-s (/ (* slack d) speed)))))))))

(def shelter-policy {:cap 10 :ttl sh-clock/ms-per-day})
