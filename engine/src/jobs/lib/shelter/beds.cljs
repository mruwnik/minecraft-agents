(ns jobs.lib.shelter.beds
  "The bed a body uses, carries, places and collects, the rest clock behind it, and the doorways the night fills."
  (:require [engine.args :as a]
            [jobs.lib.access :as access]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.tidy-rules :as tidy-rules]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.build.wood :as wood]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.solid :as solid]))

(a/defargs settings
  {::seen-bed-radius {:default 6 :spec (a/int-in 1 nil)
                      :doc "Blocks (square) within which a bed in the body's room counts as seen."}
   ::default-bed-radius {:default 48 :spec (a/num-in 0 nil)
                         :doc "Blocks within which a remembered bed may be used."}
   ::urgent-bed-radius {:default 128 :spec (a/num-in 0 nil)
                        :doc "The bed radius once the body is overdue for sleep or displaced."}
   ::max-days-awake {:default 3 :spec (a/num-in 0 nil)
                     :doc "In-game days without sleep before finding a bed becomes urgent: a bed in the open is slept in and the urgent radius applies."}
   ::courtesy-radius {:default 32 :spec (a/num-in 0 nil)
                      :doc "Blocks a body underground may walk to a bed to sleep beside another sleeping player."}})

(defn seen-bed-radius [] (settings/get settings ::seen-bed-radius))

(defn default-bed-radius [] (settings/get settings ::default-bed-radius))
(defn urgent-bed-radius [] (settings/get settings ::urgent-bed-radius))
(defn courtesy-radius [] (settings/get settings ::courtesy-radius))

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

(defn max-days-awake [] (settings/get settings ::max-days-awake))

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
  "How far a remembered bed may be: the default-bed-radius setting, or the urgent one once the latest :slept entry is :max-days-awake
  in-game days old (phantoms come), or while a :displaced entry stands (a far flight drove the body off its bed)."
  [view]
  (let [days (days-awake-in view)]
    (if (or (and days (>= days (max-days-awake))) (seq (mem/entries view :displaced)))
      (urgent-bed-radius)
      (default-bed-radius))))

(def kept-shelters
  "The shelters (walk-leaves-shelter?) a body does not leave at night for a bed: a bare roof is not one of them."
  #{:dug-in :room})

(defn courtesy-walk?
  "Whether a body walks to the bed at pos only to sleep as a courtesy: another player sleeps (others-asleep?), the bed
  is within courtesy-radius of the feet, and the body is not shut in a dug-in or room shelter (mobs may wait outside it)."
  [p view pos]
  (and (<= (u/dist (u/self-pos {:primitives p}) pos) (courtesy-radius))
       (sh-sleepers/others-asleep? p view)
       (not (#{:dug-in :room} (sh-inside/walk-leaves-shelter? p (:data (mem/latest view :shelter)) pos 1)))))

(def bed-open-policy
  "Policy of the :bed-open entries the night job writes for a bed it found open and dug in beside instead: half a day."
  {:cap 5 :ttl (quot sh-clock/ms-per-day 2)})

(defn seen-bed-places
  "The bed cells the body has seen (still beds) within radius, nearest first."
  [p radius]
  (map :pos (look/seen-blocks p {:names wood/bed-names :radius (min 16 radius) :live? true :max 8})))

(defn bed-to-use
  "The bed a body awake at night sleeps in: one it sees (seen-bed), else the remembered :bed within radius or the live
  :spawn-bed at any distance (go-to prices the walk), under any roof or none. Never one permit? refuses (occupied), one in an unexpired :bed-unreachable or :bed-walk-unsafe entry, or one whose walk
  would take the body out of a kept-shelters shelter (unless overdue: max-days-awake days since :slept, phantoms, or a
  courtesy-walk?: another player sleeps and the bed is near);
  none after a sleep tonight (:slept within half an in-game day) while the body is safe from the night (a body woken
  outside goes back to a bed), or while a :sleep-failed entry is unexpired. nil otherwise. Beds the body has seen
  (perception's memory, within radius) count as well as the remembered one."
  [p view radius permit?]
  (when (and (sh-sense/night? p) (not (sh-sense/sleeping? p))
             (or (zero? (mem/count-in view :slept sh-clock/slept-tonight-ms)) (not (sh-inside/safe-from-night? p)))
             (empty? (mem/entries view :sleep-failed)))
    (let [shelter (:data (mem/latest view :shelter))
          overdue? (>= (or (days-awake-in view) 0) (max-days-awake))
          ok? (fn [pos] (and (permit? pos) (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-unreachable))
                             (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-open))
                             (not-any? #(= pos (:pos (:data %))) (mem/entries view :bed-walk-unsafe))
                             (or overdue? (not (kept-shelters (sh-inside/walk-leaves-shelter? p shelter pos 1))) (courtesy-walk? p view pos))))]
      (or (seen-bed p ok?)
          (->> (concat (sh-spawn/standing-beds p view radius) (seen-bed-places p radius) (some-> (sh-spawn/spawn-bed-pos p view) vector))
               distinct
               (sort-by #(u/dist (u/self-pos {:primitives p}) %))
               (filter ok?)
               first)))))

(def walk-unsafe-policy
  "Policy of the :bed-walk-unsafe entries the night job writes for a bed whose walk it priced unsafe: five minutes, so
  the night trigger does not hold for it meanwhile."
  {:cap 5 :ttl 300000})

(def bed-place-failed-policy
  "Policy of the :bed-place-failed entry the night job writes when it could not set up a carried bed: one, ten
  minutes."
  {:cap 1 :ttl 600000})

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

(defn standing-entries
  "By day: the open ledger entries of view with purpose (a night's :bed or :doorway) whose block still stands (seen) and that no
  cleanup held lately, oldest first."
  [p view purpose]
  (when-not (sh-sense/night? p)
    (let [held (ledger/held-cells view)]
      (filterv #(and (= purpose (:purpose %)) (not (held (:cell %)))
                     (ledger/same-block? (:item %) (u/seen-name p (zipmap [:x :y :z] (:cell %)))))
               (ledger/open-entries view)))))

(defn bed-to-collect
  "By day: the foot cell [x y z] of the bed the night put down outside the body's own zones (ledger purpose :bed; never the live respawn bed, which the ledger does not list) while it still stands, else nil."
  [p view]
  (:cell (first (standing-entries p view :bed))))

(defn doorway-to-open
  "By day: the cells [x y z] of the doorway blocks the night filled (ledger purpose :doorway) that still stand and that no
  cleanup held lately, oldest first; nil when none."
  [p view]
  (seq (map :cell (standing-entries p view :doorway))))

(defn bed
  "The remembered bed position when it is within radius of the body, else nil."
  [c radius]
  (bed-in-view (:primitives c) (ctx/view c) radius))

(defn days-awake
  "In-game days since the body last rested (days-awake-in), or nil when it remembers nothing."
  [c]
  (days-awake-in (ctx/view c)))

(def bed-dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn refused-twice? [c {:keys [x y z]}]
  (<= 2 (get (:bed-refused (ctx/mem c)) [x y z] 0)))

(defn bed-layout
  "{:stand :foot :head} for a bed set down beside the body: three cells in a row, the body standing at one end (a placed
  bed faces away from the body), the foot next to it, the head after it. All three free (air, with a solid floor that is no container, bed or other kept block, and
  the body's headroom for the stand cell) within 3 cells of the body, the foot and head not in a doorway or another's
  zone, claim or plan footprint (a missing zone list refuses nothing), and roofed within roof-height when roofed? is
  set, and not a foot cell the server refused twice. The stand cell nearest the body first (its own cell when it fits); nil
  when no row fits."
  [c roofed?]
  (let [p (:primitives c)
        roof (sh-sense/roof-height)
        {:keys [x y z]} (sh-sense/feet p)
        in (access/rules-input c)
        air? #(bs/air (u/seen-name p %))
        free? (fn [pos] (and (air? pos) (sh-sense/solid-at? p (update pos :y dec))
                             (nil? (tidy-rules/keep-why (u/seen-name p (update pos :y dec))))))
        covered? (fn [pos] (or (not roofed?) (some #(sh-sense/solid-at? p (update pos :y + %)) (range 1 (inc roof)))))
        permitted? #(and (nil? (access/trespass-refusal in :place %)) (not (sh-sense/doorway? p %)))
        stands (sort-by (fn [[dx dz]] (+ (js/Math.abs dx) (js/Math.abs dz)))
                        (for [dx (range -3 4) dz (range -3 4)] [dx dz]))]
    (first (for [[sx sz] stands
                 [dx dz] bed-dirs
                 :let [stand {:x (+ x sx) :y y :z (+ z sz)}
                       foot {:x (+ x sx dx) :y y :z (+ z sz dz)}
                       head {:x (+ x sx dx dx) :y y :z (+ z sz dz dz)}]
                 :when (and (or (zero? (+ (js/Math.abs sx) (js/Math.abs sz))) (free? stand))
                            (air? (update stand :y inc))
                            (free? foot) (free? head) (covered? foot) (covered? head)
                            (permitted? foot) (permitted? head)
                            (not= foot (sh-sense/feet p))
                            (not (refused-twice? c foot)))]
             {:stand stand :foot foot :head head}))))
