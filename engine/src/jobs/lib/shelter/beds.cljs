(ns jobs.lib.shelter.beds
  "The bed a body uses, carries, places and collects, the rest clock behind it, and the doorways the night fills."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.solid :as solid]))

(a/defargs settings
  {::seen-bed-radius {:default 6 :spec (a/int-in 1 nil)
                      :doc "Blocks (square) within which a bed in the body's room counts as seen."}
   ::default-bed-radius {:default 48 :spec (a/num-in 0 nil)
                         :doc "Blocks within which a remembered bed may be used, when the job or trigger names no :bed-radius."}
   ::urgent-bed-radius {:default 128 :spec (a/num-in 0 nil)
                        :doc "The bed radius once the body is overdue for sleep or displaced, when none is named."}})

(defn seen-bed-radius [] (settings/get settings ::seen-bed-radius))

(defn default-bed-radius [] (settings/get settings ::default-bed-radius))
(defn urgent-bed-radius [] (settings/get settings ::urgent-bed-radius))
(defn arg-radius "The :bed-radius a job names, else the default-bed-radius setting." [args] (or (:bed-radius args) (default-bed-radius)))

(defn bed-in-view
  "The remembered bed position when it is within radius of the body, else nil."
  [p view radius]
  (let [pos (mem/place view :bed)]
    (when (and pos (<= (u/dist (u/self-pos {:primitives p}) pos) radius))
      pos)))

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
  cell of it, or nil. A radius replaces the setting."
  ([p] (seen-bed p (constantly true)))
  ([p permit?] (seen-bed p permit? (seen-bed-radius)))
  ([p permit? radius]
   (let [{:keys [x y z] :as start} (sh-sense/feet p)
         bed? (fn [named] (and (some? named) (.endsWith named "_bed")))
         free? (fn [named] (and (some? named) (or (bed? named) (not (solid/solid? named)))))]
     (loop [queue (conj cljs.core/PersistentQueue.EMPTY start) seen #{start}]
       (when-let [pos (peek queue)]
         (let [step (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                          :let [n {:x (+ (:x pos) dx) :y y :z (+ (:z pos) dz)}]
                          :when (and (not (seen n))
                                     (<= (max (js/Math.abs (- (:x n) x)) (js/Math.abs (- (:z n) z))) radius))
                          :let [named (u/seen-name p n)]
                          :when (free? named)]
                      [n named])
               found (some (fn [[n named]] (when (and (bed? named) (permit? n)) n)) step)]
           (or found
               (recur (into (pop queue) (map first step)) (into seen (map first step))))))))))

(def sleep-failed-policy
  "The :sleep-failed entry the night job writes when a sleep ended without sleeping (bed taken, monsters near, ...):
  one, five minutes, so the night trigger does not refire on the same bed every cooldown."
  {:cap 1 :ttl 300000})

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
    (/ (- (:now view) t) sh-clock/ms-per-day)))

(defn bed-radius
  "How far a remembered bed may be: :bed-radius, or :urgent-bed-radius once the latest :slept entry is :max-days-awake
  in-game days old (phantoms come), or while a :displaced entry stands (a far flight drove the body off its bed)."
  [view {:keys [bed-radius max-days-awake] :or {max-days-awake max-days-awake} :as args}]
  (let [days (days-awake-in view)]
    (if (or (and days (>= days max-days-awake)) (seq (mem/entries view :displaced)))
      (or (:urgent-bed-radius args) (urgent-bed-radius))
      (or bed-radius (default-bed-radius)))))

(def kept-shelters
  "The shelters (walk-leaves-shelter?) a body does not leave at night for a bed: a bare roof is not one of them."
  #{:underground :dug-in :room})

(def bed-open-policy
  "Policy of the :bed-open entries the night job writes for a bed it found open and dug in beside instead: half a day."
  {:cap 5 :ttl (quot sh-clock/ms-per-day 2)})

(defn bed-to-use
  "The bed a body awake at night sleeps in: one it sees (seen-bed), else the remembered :bed within radius, under any
  roof or none. Never one permit? refuses (occupied), one in an unexpired :bed-unreachable entry, or one whose walk
  would take the body out of a kept-shelters shelter (unless overdue: max-days-awake days since :slept, phantoms);
  none after a sleep tonight (:slept within half an in-game day)
  or while a :sleep-failed entry is unexpired. nil otherwise."
  [p view radius permit?]
  (when (and (sh-sense/night? p) (not (sh-sense/sleeping? p))
             (zero? (mem/count-in view :slept sh-clock/slept-tonight-ms))
             (empty? (mem/entries view :sleep-failed)))
    (let [shelter (:data (mem/latest view :shelter))
          overdue? (>= (or (days-awake-in view) 0) max-days-awake)
          ok? (fn [pos] (and (permit? pos) (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-unreachable))
                             (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-open))
                             (or overdue? (not (kept-shelters (sh-inside/walk-leaves-shelter? p shelter pos 1))))))]
      (or (seen-bed p ok?)
          (first (filter ok? (sh-spawn/standing-beds p view radius)))))))

(def bed-place-failed-policy
  "Policy of the :bed-place-failed entry the night job writes when it could not set up a carried bed: one, ten
  minutes."
  {:cap 1 :ttl 600000})

(def bed-placed-policy
  "Policy of the :bed-placed entries: a bed the night job put down outside the body's own zones, picked up by day."
  {:cap 4 :ttl (* 7 sh-clock/ms-per-day)})

(def doorway-filled-policy
  "Policy of the :doorway-filled entries: a doorway cell the night filled (jobs.survival.night.doorway), opened by day."
  {:cap 4 :ttl sh-clock/ms-per-day})

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
  (boolean (and (sh-sense/night? p) (not (sh-sense/sleeping? p))
                (some? (carried-bed p))
                (zero? (mem/count-in view :slept sh-clock/slept-tonight-ms))
                (not-any? #(near-failed-place? p %) (mem/entries view :bed-place-failed))
                (empty? (mem/entries view :sleep-failed))
                (nil? (bed-to-use p view radius permit?)))))

(defn bed-to-collect
  "By day: the latest :bed-placed position while a bed block still stands there and is not the respawn point
  (:spawn-bed), else nil."
  [p view]
  (when-not (sh-sense/night? p)
    (let [pos (:pos (:data (mem/latest view :bed-placed)))]
      (when (and pos (.endsWith (or (u/seen-name p pos) "") "_bed")
                 (not (sh-spawn/spawn-bed-at? view pos)))
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
  (when-not (sh-sense/night? p)
    (let [failed (set (map (comp :cell :data) (mem/entries view :doorway-open-failed)))]
      (seq (keep #(let [{:keys [cell] :as d} (:data %)] (when (and (not (failed cell)) (still-filled? p d)) cell))
                 (mem/entries view :doorway-filled))))))

(defn bed
  "The remembered bed position when it is within radius of the body, else nil."
  [c radius]
  (bed-in-view (:primitives c) (ctx/view c) radius))

(defn days-awake
  "In-game days since the body last rested (days-awake-in), or nil when it remembers nothing."
  [c]
  (days-awake-in (ctx/view c)))
