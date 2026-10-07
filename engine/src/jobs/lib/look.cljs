(ns jobs.lib.look
  "Looking around like a player. A job that needs a block it has seen turns the body through the four headings (level,
  then down at the floor ahead) with a sight pass after each look, so what lies beside or behind it enters
  perception's memory. Nothing is sensed through walls: jobs read blocks and entities through the seen-* helpers here
  (memory of what the body saw, players, hostiles it saw or heard), never through blocks/entities as a scan."
  (:require [engine.args :as a]
            [engine.game :as game]
            [engine.settings :as settings]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [engine.perception :as perception]
            [engine.perception.store :as store]))

(a/defargs settings
  {::glance-ahead {:default 4 :spec (a/num-in 0 nil) :doc "How far ahead a level look takes in, in blocks."}
   ::glance-near {:default 1.5 :spec (a/num-in 0 nil) :doc "How far ahead the look down at the floor takes in, in blocks."}
   ::dark-light {:default 8 :spec (a/int-in 0 15)
                 :doc "A feet cell under this effective light is dark: hostiles spawn and walk in from it."}
   ::toss-reach {:default 10 :spec (a/num-in 0 nil) :doc "Blocks within which an item entity counts as the body's toss."}})

(defn glance-ahead [] (settings/get settings ::glance-ahead))
(defn glance-near [] (settings/get settings ::glance-near))
(defn dark-light [] (settings/get settings ::dark-light))
(defn toss-reach [] (settings/get settings ::toss-reach))

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

(defn see!
  "A sight pass now, so what the last look faced is in memory before the next decision."
  [c]
  (when-let [per (aget (:primitives c) "perception")]
    (perception/pass! per)))

(defn ^:async glance!
  "Look along each of dirs ([dx dz]), level and down, a sight pass after each look."
  [c dirs]
  (let [{:keys [x y z]} (cell-of (u/self-pos c))
        eye {:x (+ x 0.5) :y (+ y game/eye-height) :z (+ z 0.5)}]
    (loop [points (mapcat #(glances eye %) dirs)]
      (when-let [pt (first points)]
        (await (ctx/act c :look (clj->js {:pos pt})))
        (see! c)
        (recur (rest points))))))

(defn ^:async look-around!
  "Look along every heading from here and note the cell in job memory under :looked."
  [c]
  (await (glance! c (vals headings)))
  (ctx/update-mem! c assoc :looked (cell-of (u/self-pos c)))
  :continue)

(defn ^:async look-around-until!
  "Look around as look-around! does, but test after each look: stop at the first truthy (f), returned. Entities are
  sensed only inside the view cone, so what a job wants in view must be checked between looks, not after the last."
  [c f]
  (let [{:keys [x y z]} (cell-of (u/self-pos c))
        eye {:x (+ x 0.5) :y (+ y game/eye-height) :z (+ z 0.5)}]
    (loop [points (mapcat #(glances eye %) (vals headings))]
      (if-let [pt (first points)]
        (do (await (ctx/act c :look (clj->js {:pos pt})))
            (see! c)
            (or (f) (recur (rest points))))
        (do (ctx/update-mem! c assoc :looked (cell-of (u/self-pos c)))
            nil)))))

(defn surveyed?
  "Whether the job has looked around: this run (per :run, the default) or from the cell the body stands in (:cell)."
  ([c] (surveyed? c :run))
  ([c per]
   (let [looked (:looked (ctx/mem c))]
     (if (= :cell per) (= looked (cell-of (u/self-pos c))) (some? looked)))))

(defn ^:async survey!
  "Look around once before choosing work from what the body has seen, as a player turns before deciding there is
  nothing (or only one thing) to do: look-around! unless (surveyed? c per). :continue when it looked, else nil.
  A check that finds no work passes while the job has not surveyed (wait-unless-surveyed), so the round looks first."
  ([c] (survey! c :run))
  ([c per]
   (when-not (surveyed? c per)
     (await (look-around! c)))))

(defn ^:async survey-until!
  "survey! for entities: test (f) after each look and return the first truthy one (entities count only while in view);
  (f) once when already surveyed."
  ([c f] (survey-until! c :run f))
  ([c per f]
   (if (surveyed? c per)
     (f)
     (await (look-around-until! c f)))))

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
  (u/note-read! :area {:radius (or radius 16)})
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

(defn seen-block
  "The block at cell pos as last seen: {:name :pos :age-ms (:properties)}, or {:unknown true :pos} for a cell never
  seen; nil without a perception."
  [p pos]
  (when-let [f (aget p "seenBlockAt")]
    (let [b (js->clj (.call f p (clj->js pos)) :keywordize-keys true)]
      (when (:unknown b) (u/note-read! :cells (u/cell-vec pos)))
      (update b :pos #(if (map? %) % (u/pos-of %))))))

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

(defn seen-items
  "The item entities within opts (:radius, :max) the body can see, as JS entities of entities(): not those hidden
  behind a wall (visible false)."
  [p opts]
  (->> (array-seq (.entities p (clj->js (assoc opts :kind "item"))))
       (remove #(false? (.-visible %)))))

(defn ^:async find-seen!
  "seen-blocks for q; when it is empty, one look-around! (four headings, a sight pass after each) and again."
  [c q]
  (let [p (:primitives c)
        hits (seen-blocks p q)]
    (if (seq hits)
      hits
      (do (await (look-around! c))
          (seen-blocks p q)))))

;; ------------------------------------------------------------------ light

(defn sky-subtract
  "What the sky's light is reduced by now (0 at noon, 11 at a clear midnight): time of day, rain and thunder of raw."
  [raw]
  (let [s (.sky raw)
        darken (perception/sky-darken (.-timeOfDay s) (.-rain s) (.-thunder s))]
    (js/Math.round (* 11 (/ (- 1 darken) 0.8)))))

(defn effective-light
  "Light at cell x y z of raw: the brighter of block light and sky light less the sky darkening."
  [raw x y z]
  (let [packed (.lightAt raw x y z)]
    (max (bit-and packed 15) (- (bit-shift-right packed 4) (sky-subtract raw)))))

(defn dark-fn
  "The planner's test of a feet cell (options.dark.at), {:at (fn [x y z] 1 dark, 0 lit) :night? whether the sky is dark},
  for the body of primitives p; nil without a perception. A cell the body has seen is lit by block light of 1 or more
  (mobs do not spawn there) or by sky light still at dark-light after the sky darkening; a seen cave stays dark by day.
  A cell never seen is dark only when the sky is (night?): by day it costs like a lit one. Light is read only for cells
  seen, as a player would know them."
  [p]
  (when-let [per (aget p "perception")]
    (when-let [raw (:raw per)]
      (let [^js st (:st per)
            subtract (sky-subtract raw)
            dark (dark-light)
            night? (< (- 15 subtract) dark)
            unseen (if night? 1 0)
            ^js sections (store/store-of st (.-dim st))]
        {:night? night?
         :at (fn [x y z]
               (let [^js sec (.get sections (store/section-key (bit-shift-right x 4) (bit-shift-right y 4) (bit-shift-right z 4)))]
                 (if (or (nil? sec) (zero? (aget (.-ids sec) (store/cell-index x y z))))
                   unseen
                   (let [packed (.lightAt ^js raw x y z)]
                     (if (or (pos? (bit-and packed 15)) (>= (- (bit-shift-right packed 4) subtract) dark)) 0 1)))))}))))

(defn drops
  "Item entities of name within radius as [{:id :pos :count}], nearest first; only those within toss-reach of near
  (a position) when given."
  ([p name radius] (drops p name radius nil))
  ([p name radius near]
   (->> (seen-items p {:radius radius :max 32})
        (filter #(= name (some-> (.-item %) .-name)))
        (mapv (fn [e] {:id (.-id e) :pos (u/pos-of (.-pos e)) :count (or (some-> (.-item e) .-count) 1)}))
        (filterv #(or (nil? near) (u/within? near (:pos %) (toss-reach)))))))
