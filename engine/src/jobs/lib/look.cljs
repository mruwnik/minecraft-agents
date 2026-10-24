(ns jobs.lib.look
  "Looking around like a player. A job that needs a block it has seen turns the body through the four headings (level,
  then down at the floor ahead, then up) with a sight pass after each look, so what lies beside or behind it enters
  perception's memory. Nothing is sensed through walls: jobs read blocks and entities through the seen-* helpers here
  (memory of what the body saw, players, hostiles it saw or heard), never through blocks/entities as a scan."
  (:require [jobs.lib.block-sets :as bs]
            [engine.args :as a]
            [engine.game :as game]
            [engine.settings :as settings]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [engine.perception :as perception]
            [engine.perception.light :as light]
            [engine.perception.rays :as rays]
            [engine.perception.store :as store]))

(a/defargs settings
  {::glance-ahead {:default 4 :spec (a/num-in 0 nil) :doc "How far ahead a level look takes in, in blocks."}
   ::glance-near {:default 1.5 :spec (a/num-in 0 nil) :doc "How far ahead the look down at the floor takes in, in blocks."}
   ::dark-light {:default 8 :spec (a/int-in 0 15)
                 :doc "A feet cell under this effective light is dark: hostiles spawn and walk in from it."}
   ::glance-effort {:default 0.25 :spec (a/num-in 0 1)
                    :doc "How hard a look around or a sight pass after a look looks, 0-1 (perception's effort): a glance."}
   ::search-effort {:default 1 :spec (a/num-in 0 1)
                    :doc "The effort of a look around that searches for something (find-spot, find-seen!, searches)."}
   ::toss-reach {:default 10 :spec (a/num-in 0 nil) :doc "Blocks within which an item entity counts as the body's toss."}})

(defn glance-ahead [] (settings/get settings ::glance-ahead))
(defn glance-near [] (settings/get settings ::glance-near))
(defn dark-light [] (settings/get settings ::dark-light))
(defn toss-reach [] (settings/get settings ::toss-reach))
(defn glance-effort [] (settings/get settings ::glance-effort))
(defn search-effort [] (settings/get settings ::search-effort))

(def headings {"north" [0 -1] "south" [0 1] "east" [1 0] "west" [-1 0]})
(def heading-short {"n" "north" "s" "south" "e" "east" "w" "west"})

(defn cell-of [pos] {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})

(defn heading-name
  "north, south, east or west for a heading arg (a name or n/s/e/w, string or keyword), else nil."
  [d]
  (when d
    (let [n (str/lower-case (name d))]
      (cond (headings n) n
            (heading-short n) (heading-short n)))))

(defn facing
  "The heading nearest the way the body looks (perception's eye; mineflayer yaw 0 looks north), south without a
  perception."
  [p]
  (if-let [eye (some-> (aget p "perception") :raw (.eye))]
    (let [yaw (.-yaw eye)
          dx (- (js/Math.sin yaw))
          dz (- (js/Math.cos yaw))]
      (if (> (js/Math.abs dx) (js/Math.abs dz))
        (if (pos? dx) "east" "west")
        (if (pos? dz) "south" "north")))
    "south"))

(defn glances
  "The points a look along [dx dz] takes in from eye: level, 4 blocks ahead, then down (about 50 degrees) at the
  floor ahead."
  [eye [dx dz]]
  [{:x (+ (:x eye) (* (glance-ahead) dx)) :y (:y eye) :z (+ (:z eye) (* (glance-ahead) dz))}
   {:x (+ (:x eye) (* (glance-near) dx)) :y (- (:y eye) game/body-height) :z (+ (:z eye) (* (glance-near) dz))}])

(defn up-cells
  "The cells along the ray from eye up (about 60 degrees) along [dx dz], twice as far as the level look takes in: what stands high beside the body is up there."
  [eye [dx dz]]
  (let [n (* 4 (glance-ahead))]
    (distinct (for [i (range 1 (inc n))
                    :let [t (/ (* i 2 (glance-ahead)) n)]]
                (cell-of {:x (+ (:x eye) (* t 0.5 dx)) :y (+ (:y eye) (* t 0.87)) :z (+ (:z eye) (* t 0.5 dz))})))))

(defn up-unseen?
  "Whether a cell along the up ray is unseen before the first seen solid block (a ceiling hides what is behind it)."
  [p eye dir]
  (loop [cells (up-cells eye dir)]
    (when-let [cell (first cells)]
      (let [b (u/sensed p cell)]
        (cond (true? (some-> b .-unknown)) true
              (not (contains? bs/air (some-> b .-name))) false
              :else (recur (rest cells)))))))

(defn up-glance
  "The look up (about 60 degrees) along [dx dz] from eye, which the level look's field of view misses: a point, or nil
  when the ray's cells were seen already so the look would add nothing."
  [p eye [dx dz :as dir]]
  (when (or (nil? (.-sensedAt p)) (up-unseen? p eye dir))
    {:x (+ (:x eye) (* (glance-near) dx)) :y (+ (:y eye) (* 1.7 (glance-near))) :z (+ (:z eye) (* (glance-near) dz))}))

(def hazards-only
  "A want for a look that searches for entities: no block is wanted, only the hazards every targeted look checks."
  {})

(defn see!
  "A sight pass now at effort (default glance-effort), so what the last look faced is in memory before the next decision.
  With want ({:names :match :radius}; {} wants only hazards, for a look for entities) it ray-checks just those kinds
  (rays/look-for!) and leaves the rest to the ambient passes. A mob sample too (knownMobs): a silent creeper is sensed only inside the view cone, and the next look may turn away
  before the timer's sample."
  ([c] (see! c (glance-effort)))
  ([c effort] (see! c effort :forced))
  ([c effort why] (see! c effort why nil))
  ([c effort why want]
   (when-let [per (aget (:primitives c) "perception")]
     (if want
       (perception/look-for! per (assoc want :effort effort :why why))
       (perception/pass! per {:effort effort :reuse? true :why why}))
     (when (fn? (.-knownMobs (:primitives c))) (.knownMobs (:primitives c))))))

(defn ^:async glance-until!
  "glance!, testing (f) after each look and stopping at the first truthy one, returned (nil when none): entities are
  sensed only inside the view cone, and the last look of a heading points down."
  ([c dirs f] (glance-until! c dirs f (glance-effort)))
  ([c dirs f effort]
   (let [{:keys [x y z]} (cell-of (u/self-pos c))
         eye {:x (+ x 0.5) :y (+ y game/eye-height) :z (+ z 0.5)}]
     (loop [points (mapcat #(glances eye %) dirs)]
       (when-let [pt (first points)]
         (await (ctx/act c :look (clj->js {:pos pt})))
         (see! c effort :look-around)
         (or (f) (recur (rest points))))))))

(defn ^:async glance!
  "Look along each of dirs ([dx dz]), level and down (look-around! adds the look up), a sight pass at effort (default glance-effort) after each look."
  ([c dirs] (glance! c dirs (glance-effort)))
  ([c dirs effort] (await (glance-until! c dirs (constantly nil) effort)) nil))

(defn note-surveyed!
  "Book that the job has looked around from the cell the body stands in (what surveyed? reads)."
  ([c] (note-surveyed! c (cell-of (u/self-pos c))))
  ([c cell] (ctx/update-mem! c assoc ::looked cell) nil))

(defn forget-survey!
  "Drop the note that the job has looked around, so surveyed? is false again."
  [c]
  (ctx/update-mem! c dissoc ::looked)
  nil)

(defn ^:async look-around-until!
  "Look along every heading from here at effort (default glance-effort), level, down and, unless that ray is seen
  already, up. Test (f) after each look: stop at the first truthy one, returned (nil when none; the cell is noted in
  job memory under ::looked). Entities are sensed only inside the view cone, so what a job wants in view must be
  checked between looks, not after the last. want: see!."
  ([c f] (look-around-until! c f (glance-effort)))
  ([c f effort] (look-around-until! c f effort nil))
  ([c f effort want]
   (let [{:keys [x y z]} (cell-of (u/self-pos c))
         eye {:x (+ x 0.5) :y (+ y game/eye-height) :z (+ z 0.5)}
         p (:primitives c)]
     (loop [steps (mapcat (fn [d] (concat (map (fn [pt] [pt]) (glances eye d)) [[nil d]])) (vals headings))]
       (if-let [[pt d] (first steps)]
         (if-let [pt (or pt (up-glance p eye d))]
           (do (await (ctx/act c :look (clj->js {:pos pt})))
               (see! c effort :look-around want)
               (or (f) (recur (rest steps))))
           (recur (rest steps)))
         (do (note-surveyed! c)
             nil))))))

(defn ^:async look-around!
  "Look around as look-around-until! does, without a test; :continue."
  ([c] (look-around! c (glance-effort)))
  ([c effort] (look-around! c effort nil))
  ([c effort want]
   (await (look-around-until! c (constantly nil) effort want))
   :continue))

(def near-blocks 3)

(defn surveyed?
  "Whether the job has looked around: this run (per :run, the default), from the cell the body stands in (:cell) or
  from within near-blocks of it (:near)."
  ([c] (surveyed? c :run))
  ([c per]
   (let [looked (::looked (ctx/mem c))]
     (case per
       :cell (= looked (cell-of (u/self-pos c)))
       :near (boolean (and looked (<= (u/dist looked (cell-of (u/self-pos c))) near-blocks)))
       (some? looked)))))

(defn ^:async survey!
  "Look around once before choosing work from what the body has seen, as a player turns before deciding there is
  nothing (or only one thing) to do: look-around! unless (surveyed? c per). :continue when it looked, else nil.
  A check that finds no work passes while the job has not surveyed (wait-unless-surveyed), so the round looks first."
  ([c] (survey! c :run))
  ([c per] (survey! c per (glance-effort)))
  ([c per effort] (survey! c per effort nil))
  ([c per effort want]
   (when-not (surveyed? c per)
     (await (look-around! c effort want)))))

(defn ^:async survey-until!
  "survey! for entities: test (f) after each look and return the first truthy one (entities count only while in view);
  (f) once when already surveyed."
  ([c f] (survey-until! c :run f))
  ([c per f] (survey-until! c per f (glance-effort)))
  ([c per f effort] (survey-until! c per f effort nil))
  ([c per f effort want]
   (if (surveyed? c per)
     (f)
     (await (look-around-until! c f effort want)))))

(defn unseen?
  "Whether the body has never seen one of cells ({:x :y :z}); false without a perception."
  [p cells]
  (boolean (and (some? (.-sensedAt p)) (some #(true? (some-> (u/sensed p %) .-unknown)) cells))))

(defn wait-unless-surveyed
  "For a check that found no work in what the body has seen: true while the job has not surveyed this run (its round
  looks around first), else (ctx/wait c reason). With cells, only while one of them was never seen."
  ([c reason] (if (surveyed? c) (ctx/wait c reason) true))
  ([c reason cells]
   (if (or (surveyed? c) (not (unseen? (:primitives c) cells))) (ctx/wait c reason) true)))

;; ------------------------------------------------------------------ what the body has seen

(defn seen-blocks
  "The blocks the body has seen (perception's memory, radius capped at 64, never x-ray), nearest first, as
  {:name :pos :age-ms (:properties)}. q: :names (coll) or :match (name -> truthy), :radius (default 16), :max (64);
  :live? drops cells whose block now differs from what was seen (with :live-within-ms only those seen that recently;
  an older memory is trusted); :properties? adds the properties as last seen. One of :names, :match or :all? true
  is required. Empty without a perception."
  [p {:keys [names match radius max live? live-within-ms properties? all?]}]
  (assert (or (nil? names) (sequential? names) (set? names) (array? names)) ":names is a coll of block names")
  (assert (or (nil? match) (fn? match)) ":match is a fn of a block name")
  (assert (not (and names match)) ":names or :match, not both")
  (assert (or names match all?) "name the blocks wanted (:names or :match), or pass :all? true")
  (u/note-read! :area (cond-> {:radius (or radius 16)} names (assoc :names (set names)) match (assoc :match match)))
  (if-let [f (aget p "seenBlocks")]
    (let [q (cond-> {} names (assoc :names (vec names)) match (assoc :match match) radius (assoc :radius radius) max (assoc :max max))]
      (->> (array-seq (.call f p (clj->js q)))
           (keep (fn [b]
                   (let [pos (u/pos-of (.-pos b))]
                     (when (or (not live?)
                             (and live-within-ms (> (aget b "age-ms") live-within-ms))
                             (= (.-name b) (u/seen-name p pos)))
                       (cond-> {:name (.-name b) :pos pos :age-ms (aget b "age-ms")}
                         properties? (assoc :properties (some-> (.call (aget p "seenBlockAt") p (clj->js pos)) .-properties
                                                                (js->clj :keywordize-keys true))))))))
           vec))
    []))

(defn water-source?
  "Whether a seen water block is a source (a bucket takes only that): level 0, or no level known."
  [{:keys [properties]}]
  (let [l (:level properties)]
    (or (nil? l) (zero? (js/parseInt l)))))

(defn seen-water-sources
  "The seen live water source blocks within radius, nearest first (the nearest 256 water blocks are read: a spill on flat ground is mostly flowing cells round its source)."
  [p radius]
  (filterv water-source? (seen-blocks p {:names ["water"] :radius radius :max 256 :live? true :properties? true})))

(defn seen-block
  "The block at cell pos as last seen: {:name :pos :age-ms (:properties)}, or {:unknown true :pos} for a cell never
  seen; nil without a perception."
  [p pos]
  (when-let [f (aget p "seenBlockAt")]
    (let [b (js->clj (.call f p (clj->js pos)) :keywordize-keys true)]
      (when (:unknown b) (u/note-read! :cells (u/cell-vec pos)))
      (update b :pos #(if (map? %) % (u/pos-of %))))))

(defn never-seen?
  "Whether the cell was never seen (seen-block's :unknown), without building the block map."
  [p pos]
  (when-let [f (aget p "seenBlockAt")]
    (let [unknown (true? (.-unknown ^js (.call f p (clj->js pos))))]
      (when unknown (u/note-read! :cells (u/cell-vec pos)))
      unknown)))

(defn beliefs
  "What the body knows of ground, the body argument of jobs.lib.cost.base-rates: {:biome-at}, a fn of a pos -> the
  biome of that cell when the body has seen the cell and its chunk is loaded, else nil."
  [p]
  {:biome-at (fn [pos]
               (let [raw (.-rawWorld p)]
                 (when (and raw (aget raw "biomeAt") (:name (seen-block p pos)))
                   (.biomeAt raw (:x pos) (:y pos) (:z pos)))))})

(defn seen-entities
  "The entities the body can place, as cljs maps of entities() (opts: its :radius :kind :names :max): players always;
  hostiles as perception's known mobs (seen or heard, nearest first) when it has them; every other kind only when
  it is `visible`. Passive mobs are already sight-filtered by entities()."
  [p opts]
  (u/note-read! :entities (select-keys opts [:radius :kind :names]))
  (let [raw (js->clj (.entities p (clj->js opts)) :keywordize-keys true)
        known (when-let [f (aget p "knownMobs")]
                (let [{:keys [radius names kind]} opts]
                  (->> (js->clj (.call f p) :keywordize-keys true)
                       (filter #(and (or (nil? radius) (<= (:distance %) radius))
                                     (or (nil? names) (some #{(:name %)} names))
                                     (or (nil? kind) (= kind (:kind %))))))))
        hostile? #(= "hostile" (:kind %))
        shown? #(or (= "player" (:kind %)) (not (contains? % :visible)) (:visible %))]
    (if known
      (vec (concat (remove hostile? (filter shown? raw)) (filter hostile? known)))
      (vec (filter shown? raw)))))

(defn seen-objs
  "seen-entities as the JS entities of entities(): players and hostiles always (hostile
  sight is perception's known mobs, see seen-entities), every other kind only when not hidden behind a wall (visible
  false). :max caps after the hiding (entities() gets a huge cap, else its default 32 cuts before the hiding)."
  [p opts]
  (->> (array-seq (.entities p (clj->js (assoc opts :max 1000000))))
       (remove #(and (not (#{"player" "hostile"} (.-kind %))) (false? (.-visible %))))
       (take (or (:max opts) 64))))

(defn heard-entities
  "The passive mobs the body hears but cannot see, nearest first, as cljs maps {:id :name :kind :distance :direction
  :band} (opts: :radius :names :max): what a sound tells, never a place. Empty for primitives without hearing."
  [p opts]
  (u/note-read! :entities (select-keys opts [:radius :names]))
  (if-let [f (aget p "heardMobs")]
    (js->clj (.call f p (clj->js opts)) :keywordize-keys true)
    []))

(defn seen-items
  "The item entities within opts (:radius, :max) the body can see, as JS entities of entities(): not those hidden
  behind a wall (visible false)."
  [p opts]
  (seen-objs p (assoc opts :kind "item")))

(defn ^:async find-seen!
  "seen-blocks for q; when it is empty, one look-around! at search-effort (four headings, a sight pass after each) and again."
  [c q]
  (let [p (:primitives c)
        hits (seen-blocks p q)]
    (if (seq hits)
      hits
      (do (await (look-around! c (search-effort)))
          (seen-blocks p q)))))

;; ------------------------------------------------------------------ light

(defn raw-of
  "The raw world reader of p (light, sky, block states): its perception's, else the primitives' rawWorld; nil without one."
  [p]
  (or (some-> (aget p "perception") :raw) (.-rawWorld p)))

(defn light-known?
  "Whether raw has light data for the cell: it is loaded, and lightAt does not say -1 (no light read)."
  [^js raw x y z]
  (and (<= 0 (.stateAt raw x y z)) (<= 0 (.lightAt raw x y z))))

(defn sky-subtract-at
  "What the sky's light is reduced by at time of day t with rain and thunder (0-1): 0 at noon, 11 at a clear midnight."
  [t rain thunder]
  (js/Math.round (* 11 (/ (- 1 (perception/sky-darken t rain thunder)) 0.8))))

(defn sky-subtract
  "sky-subtract-at for the sky of raw now."
  [raw]
  (let [s (.sky raw)]
    (sky-subtract-at (.-timeOfDay s) (.-rain s) (.-thunder s))))

(defn lit-light?
  "Whether a cell with this block and sky light is lit enough that monsters cannot spawn: block light of 1 or more, or sky
  light less the sky darkening subtract at dark-light."
  [block sky subtract]
  (or (pos? block) (>= (- sky subtract) (dark-light))))

(defn dark-guess?
  "The guess for a cell the body has not seen: whether open sky, less its darkening, is below dark-light. False without a perception."
  [p]
  (boolean (some-> (raw-of p) sky-subtract (->> (- 15)) (< (dark-light)))))

(defn buried-feet?
  "Whether the sky light at the feet cell and the cell over it is 0 (underground), read from raw; false without light data there."
  [^js raw pos]
  (let [x (js/Math.floor (.-x pos)) y (js/Math.floor (.-y pos)) z (js/Math.floor (.-z pos))]
    (and (light-known? raw x y z)
         (zero? (bit-shift-right (.lightAt raw x y z) 4))
         (zero? (bit-shift-right (.lightAt raw x (inc y) z) 4)))))

(defn dark-here?
  "Whether the light at the body's feet is not lit-light? (hostiles spawn and walk in from it, at any hour). False without light data."
  [p]
  (let [raw (raw-of p)
        ^js pos (.-pos (.self p))
        x (js/Math.floor (.-x pos)) y (js/Math.floor (.-y pos)) z (js/Math.floor (.-z pos))]
    (boolean (and raw (light-known? raw x y z) (not (lit-light? (bit-and (.lightAt raw x y z) 15) (bit-shift-right (.lightAt raw x y z) 4) (sky-subtract raw)))))))

(defn dark-fn
  "The planner's test of a feet cell (options.dark.at), {:at (fn [x y z] 1 dark, 0 lit) :night? whether the sky is dark},
  for the body of primitives p; nil without a perception. A cell the body has seen is lit by block light of 1 or more
  (mobs do not spawn there) or by sky light still at dark-light after the sky darkening; a seen cave stays dark by day.
  A cell never seen is dark when the sky is (night?) or the body is underground: otherwise it costs like a lit one; with :unseen-dark? false never
  (a masked plan prices unseen cells itself). Light is read only for cells seen, as a player would know them."
  ([p] (dark-fn p nil))
  ([p {:keys [unseen-dark?]}]
   (when-let [per (aget p "perception")]
     (when-let [raw (:raw per)]
       (let [^js st (:st per)
             subtract (sky-subtract raw)
             night? (< (- 15 subtract) (dark-light))
             unseen (if (and (or night? (buried-feet? raw (.-pos (.self p)))) (not (false? unseen-dark?))) 1 0)
             ^js sections (store/store-of st (.-dim st))]
         {:night? night?
          :at (fn [x y z]
                (let [^js sec (.get sections (store/section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)))]
                  (if (or (nil? sec) (zero? (aget (.-ids sec) (store/cell-index x y z))))
                    unseen
                    (let [packed (.lightAt ^js raw x y z)]
                      (if (lit-light? (bit-and packed 15) (bit-shift-right packed 4) subtract) 0 1)))))})))))

(def lag-class
  "water-fn's class of a phantom floor whose nearest seen cell along an axis at its level is water 1 block off; d blocks
  off is lag-class + d - 1."
  6)

(defn water-fn
  "The planner's water class of a feet cell (options.dark.at past dark) for the body of primitives p: 2 for a cell seen
  as water whose column holds, down from it, a water cell at block light 0 over water, or reaches a cell not seen (dark
  depths do not show), where a drowned can spawn; 3 for other seen water; a cell never seen 2 when wet? (fn [x y z]: the
  walk reads it as water) says so, the worse case; a cell on a floor phantom? (fn [x y z]: the walk reads it as an
  unseen floor) says is one, whose nearest seen cell along an axis at the floor's level within lags blocks is water d off
  (or air over water), lag-class + d - 1; 0 for any other cell. nil without a perception."
  [p {:keys [wet? phantom? lags]}]
  (when-let [per (aget p "perception")]
    (when-let [^js raw (:raw per)]
      (let [^js st (:st per)
            ^js sections (store/store-of st (.-dim st))
            kinds (js/Map.)
            id-at (fn [x y z]
                    (let [^js sec (.get sections (store/section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)))]
                      (if (nil? sec) 0 (aget (.-ids sec) (store/cell-index x y z)))))
            kind (fn [v]
                   (let [k (.get kinds v)]
                     (if (some? k)
                       k
                       (let [n (.-name ^js (.stateInfo raw (dec v)))
                             k (cond (= "water" n) :water (#{"air" "cave_air"} n) :air :else :dry)]
                         (.set kinds v k)
                         k))))
            water? (fn [v] (and (pos? v) (keyword-identical? :water (kind v))))
            ;; lag of the nearest seen water along an axis at level y, or at y - 1 under seen air (a phantom floor over a
            ;; pond's air reads the surface below); a seen dry cell, or seen air over no water, ends its axis; nil: none
            lag (fn [x y z]
                  (reduce (fn [best [dx dz]]
                            (loop [d 1]
                              (if (or (> d lags) (and best (>= d best)))
                                best
                                (let [cx (+ x (* dx d)) cz (+ z (* dz d))
                                      v (id-at cx y cz)]
                                  (cond (zero? v) (recur (inc d))
                                        (water? v) d
                                        (and (keyword-identical? :air (kind v)) (water? (id-at cx (dec y) cz))) d
                                        :else best)))))
                          nil [[1 0] [-1 0] [0 1] [0 -1]]))
            phantom (fn [x y z]
                      (or (when (and phantom? (phantom? x (dec y) z))
                            (some->> (lag x (dec y) z) (+ lag-class -1)))
                          0))]
        (fn [x y z]
          (let [v (id-at x y z)]
            (cond
              (zero? v) (if (and wet? (wet? x y z)) 2 (phantom x y z))
              (not (water? v)) (phantom x y z)
              :else
              (loop [y y]
                (let [below (id-at x (dec y) z)
                      dark? (zero? (bit-and (.lightAt raw x y z) 15))]
                  (cond
                    (zero? below) 2
                    (not (water? below)) 3
                    dark? 2
                    :else (recur (dec y))))))))))))

(defn see-lit-fn
  "The planner's options.unknown.seeLit for the body of primitives p: (fn [x y z]) 1 when the cell's live light makes it
  visible by perception's own table (seeing-min, the sky now), else 0; nil without a perception."
  [p]
  (when-let [per (aget p "perception")]
    (when-let [^js raw (:raw per)]
      (let [visible (light/table-now raw (:seeing-min (:opts per)))]
        (fn [x y z] (aget visible (.lightAt raw x y z)))))))

(defn near-of
  "How close a dark cell must be for the body of primitives p to see it (more with a torch in a hand); nil without a perception."
  [p]
  (some-> (aget p "perception") rays/near-of))

(defn glimpse?
  "Whether the cell {:x :y :z} could be seen by turning to it: a clear line from the eye (nothing between, no wall) and it is lit enough to see or within near-of.
  False without a perception."
  [p {:keys [x y z]}]
  (boolean
   (when-let [^js raw (some-> (aget p "perception") :raw)]
     (let [^js eye (.eye raw)
           lit? (when-let [f (see-lit-fn p)] (= 1 (f x y z)))
           near? (<= (js/Math.hypot (- (+ x 0.5) (.-x eye)) (- (+ y 0.5) (.-y eye)) (- (+ z 0.5) (.-z eye))) (near-of p))]
       (and (or lit? near?)
            (rays/line-clear? raw (.sightTable raw) (.-x eye) (.-y eye) (.-z eye) (+ x 0.5) (+ y 0.5) (+ z 0.5)))))))

(defn dark-in-sight?
  "Whether a look at the cell {:x :y :z} would show it dark: within sight's :radius, a clear line from the eye, neither lit
  enough to see nor within near-of. False without a perception."
  [p {:keys [x y z]}]
  (boolean
   (when-let [per (aget p "perception")]
     (let [^js raw (:raw per)
           ^js eye (.eye raw)
           d (js/Math.hypot (- (+ x 0.5) (.-x eye)) (- (+ y 0.5) (.-y eye)) (- (+ z 0.5) (.-z eye)))]
       (and (<= d (:radius (:opts per))) (> d (near-of p))
            (not= 1 ((see-lit-fn p) x y z))
            (rays/line-clear? raw (.sightTable raw) (.-x eye) (.-y eye) (.-z eye) (+ x 0.5) (+ y 0.5) (+ z 0.5)))))))

(defn drops
  "Item entities of name within radius as [{:id :pos :count}], nearest first; only those within toss-reach of near
  (a position) when given."
  ([p name radius] (drops p name radius nil))
  ([p name radius near]
   (->> (seen-items p {:radius radius :max 32})
        (filter #(= name (some-> (.-item %) .-name)))
        (mapv (fn [e] {:id (.-id e) :pos (u/pos-of (.-pos e)) :count (or (some-> (.-item e) .-count) 1)}))
        (filterv #(or (nil? near) (u/within? near (:pos %) (toss-reach)))))))
