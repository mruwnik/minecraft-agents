(ns world-test.fixture
  "World fixtures: an EDN file describes a live test case on a plot of its own (the blocks to build, the body's start,
  the steps to act, the events to expect). This namespace is pure: it reads the files, resolves plot-relative
  positions against a plot's origin and turns a case into the RCON commands the runner sends. The format is in
  engine/fixtures/world/README.md."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [world-test.expect :as x]
            [world-test.lease :as lease]))

;; ------------------------------------------------------------------ the plot grid

(def default-grid
  "The reserved plot grid: plot i is column (mod i cols), row (quot i cols); plots are size x size, floor at y - 1,
  body level y."
  {:x0 20000 :z0 20000 :y 150 :size 32 :cols 20 :rows 20})

(def large-lanes
  "Plots longer or wider than 32 lie in lanes south of the grid (plot indexes 400..415): lane j starts at z z0 + 96 j,
  up to 1024 long and 64 wide, so a lane never touches the grid or another lane."
  {:z0 20704 :stride 96 :count 16 :max-length 1024 :max-width 64 :min-length 16})

(def mob-rows
  "Grid rows 0..mob-rows-1 hold the mob plots; plain cases use the rows from mob-rows + 1 on."
  9)

(def mob-stride
  "A mob plot lies on every mob-stride-th row and column: 64 blocks of air between two, beyond the body's sight (about
  48) and a zombie's follow range (35)."
  3)

(def mob-margin
  "Blocks of air a mob plot's setup clears on every side (half the gap to the next one): no floor joins it to another
  plot, so no mob walks over and none spawns near."
  32)

(defn mob-cols [{:keys [cols]}] (quot (+ cols (dec mob-stride)) mob-stride))

(defn mob-plot-count [grid] (* (quot (+ mob-rows (dec mob-stride)) mob-stride) (mob-cols grid)))

(defn plain-row-count [{:keys [rows]}] (quot (+ (- rows mob-rows 1) (dec mob-stride)) mob-stride))

(defn plain-plot-count [grid] (* (plain-row-count grid) (mob-cols grid)))

(defn plot-origin
  "The origin [x y z] of plot i: its north-west corner at body level. Plots n + lanes and up are the mob plots, then
  the plain plots (rows from mob-rows + 1, every mob-stride-th row and column, like the mob plots)."
  [{:keys [x0 z0 y size cols rows] :as grid} i]
  (let [n (* cols rows) {lz :z0 :keys [stride count]} large-lanes
        k (- i n count)
        m (- k (mob-plot-count grid))]
    (cond
      (< -1 (- i n) count) [x0 y (+ lz (* stride (- i n)))]
      (< -1 k (mob-plot-count grid)) [(+ x0 (* size mob-stride (mod k (mob-cols grid)))) y
                                      (+ z0 (* size mob-stride (quot k (mob-cols grid))))]
      (< -1 m (plain-plot-count grid)) [(+ x0 (* size mob-stride (mod m (mob-cols grid)))) y
                                        (+ z0 (* size (+ mob-rows 1 (* mob-stride (quot m (mob-cols grid))))))]
      (>= i n) (throw (js/Error. (str "plot " i " is outside the grid (" n " cells, " count " large lanes, " (mob-plot-count grid) " mob plots, " (plain-plot-count grid) " plain plots)")))
      :else [(+ x0 (* size (mod i cols))) y (+ z0 (* size (quot i cols)))])))

(declare hostile-types)

(defn mob-case?
  "Does the case summon a hostile mob? Such a case needs a mob plot: its mobs must be out of hearing of every other plot."
  [c]
  (boolean (some (fn [x] (and (vector? x) (= :summon (first x)) (string? (second x))
                              (some #{(str/replace (second x) #"^minecraft:" "")} hostile-types)))
                 (tree-seq coll? seq (:act c)))))

(defn case-grid
  "The grid with the case's plot size: :size-x (length along x) and :size-z (width), 32 by default."
  [c]
  (let [{:keys [length width]} (:plot c)]
    (cond-> (assoc default-grid :size-x (or length (:size default-grid)) :size-z (or width (:size default-grid)))
      (mob-case? c) (assoc :mob-plots? true))))

(defn plot-range
  "[first end) of the plot indexes a case may lease: the plain rows of the 32x32 grid, the mob plots for a case that
  summons a hostile, or the large lanes. Plain plots lie three apart like the mob ones: 64 blocks of air between two,
  beyond the body's sight (about 48) and a chest search (32)."
  [{:keys [size-x size-z size cols rows mob-plots?] :as grid}]
  (let [n (* cols rows) lanes (:count large-lanes) mobs (+ n lanes (mob-plot-count grid))]
    (cond
      (or (> (or size-x size) 32) (> (or size-z size) 32)) [n (+ n lanes)]
      mob-plots? [(+ n lanes) mobs]
      :else [mobs (+ mobs (plain-plot-count grid))])))

(defn dims [{:keys [size size-x size-z]}] [(or size-x size) (or size-z size)])

(defn grid-centre [{:keys [x0 z0 y size cols rows]}]
  [(+ x0 (quot (* size cols) 2)) y (+ z0 (quot (* size rows) 2))])

;; ------------------------------------------------------------------ reading

(defn rel
  "The #at tag: a plot-relative position written [x y z] (or a column [x z]), resolved to an absolute vector."
  [v] {::rel (vec v) ::as :vec})

(defn rel-map
  "The #xyz tag: a plot-relative position written [x y z] (or a column [x z]), resolved to an absolute {:x :y :z} map."
  [v] {::rel (vec v) ::as :map})

(defn seg
  "The #seg tag: a strip segment {:axis :y :line :from :to [:open-ends]} with plot-relative y, line and from/to (x or z along
  the axis), resolved to the absolute segment map jobs.lib.strip reads, with its :id."
  [m] {::seg m})

(def readers {'at rel 'xyz rel-map 'seg seg})

(defn read-edn [text]
  (reader/read-string {:readers readers} text))

(def defaults
  {:time :day
   :register []
   :plot {:height 16 :floor "stone"}
   :blocks []
   :memory []
   :zones []
   :places []
   :plans []
   :body {:at [16.5 0 16.5] :inventory [] :effects [] :settle-s 1}
   :act []
   :expect []
   :after []
   :limit-s 120
   :idle-grace-s 3})

(def concatenated
  "Keys a case adds to its file's :defaults instead of replacing them."
  #{:blocks :memory :zones :places :act :expect :after :plans})

(defn merge-case
  "A case over its file's defaults: :body and :plot merge key by key, the concatenated keys append, the rest replace."
  [file-defaults c]
  (reduce-kv (fn [acc k v]
               (cond
                 (concatenated k) (update acc k (fnil into []) v)
                 (#{:body :plot} k) (update acc k merge v)
                 :else (assoc acc k v)))
             (merge-with (fn [a b] (if (map? a) (merge a b) b)) defaults file-defaults)
             c))

(defn body-start-plan
  "What the runner does with the body before a case: :restart-clean (stop it, delete its engine/memory.edn, start with
  --fresh), :restart-keep (restart but keep memory.edn; a :keep-memory case that opens a group) or :keep (a
  :keep-memory case after another case: the running body goes on with the memory it has)."
  [c first-in-group?]
  (cond
    (not (:keep-memory c)) :restart-clean
    first-in-group? :restart-keep
    :else :keep))

(defn memory-entries
  "A case's :memory entries, and for a night case a :log-out \"unsupported\" entry (unless the case names log-out or keeps
  memory): other probe bodies sleep in the same night, and the night job would log out for them instead of doing what the case tests."
  [c]
  (cond-> (vec (:memory c))
    (and (#{:night :night-x} (lease/time-phase c))
         (not (:keep-memory c))
         (not (str/includes? (pr-str (dissoc c :id :file)) "log-out")))
    (conj {:kind :log-out :data {:status "unsupported"}})))

(def default-memory-policy
  "The engine's default policy for a memory kind (engine.memory/default-policy): 50 entries, one hour."
  {:cap 50 :ttl (* 60 60 1000)})

(defn settings-text
  "The text of the body's settings.edn for a case's :settings map (nil: the case sets none)."
  [c]
  (some-> (:settings c) pr-str))

(defn memory-seed
  "The body's memory data (what engine/memory.edn holds) for a case's :memory entries, positions already absolute:
  each {:kind k :data {...}} (optionally :policy {:cap :ttl}, :age-s seconds old) becomes an entry written at now-ms (minus its age), in order."
  [entries now-ms]
  (reduce (fn [acc {:keys [kind data policy age-s]}]
            (-> acc
                (update-in [:entries kind] (fnil conj []) {:t (- now-ms (* 1000 (or age-s 0))) :data data})
                (assoc-in [:policies kind] (or policy default-memory-policy))))
          {:entries {} :policies {}}
          entries))

;; The body reads worlds/<world>/zones.edn and places.json, which every shard shares: a case's :zones and :places are
;; merged in tagged with the body (plot coordinates never overlap between shards) and removed again by that tag.

(defn shared-tag [body] (str "wt-" (str/lower-case body)))

(defn zone-entries
  "A case's :zones (positions absolute) as zone-file entries named <tag>-<name>; :owner defaults to \"Other\", \"$body\" is the probe body."
  [body zones]
  (mapv #(-> (assoc % :name (str (shared-tag body) "-" (:name %)))
             (update :owner (fn [o] (cond (nil? o) "Other" (= "$body" o) body :else o))))
        zones))

(defn marker-entries
  "A case's :places ({:name :kind :pos [x y z] :note}, positions absolute) as places.json markers by the body's tag."
  [body places]
  (mapv (fn [{:keys [name kind pos note]}]
          (let [[x y z] pos]
            (cond-> {:name name :kind (or kind "place") :x x :y y :z z :by (shared-tag body)}
              note (assoc :note note))))
        places))

(defn zone-str [zone]
  (str "{" (str/join " " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) zone)) "}"))

(defn zones-text [zones]
  (str "[" (str/join "\n " (map zone-str zones)) "]\n"))

(defn read-zones [text]
  (if (str/blank? text) [] (reader/read-string text)))

(defn zones-with
  "The zone file text with entries appended; throws when the text is not a zone vector."
  [text entries]
  (zones-text (into (read-zones text) entries)))

(defn zones-without
  "The zone file text without the zones named <tag>-...; text without such zones comes back unchanged."
  [text tag]
  (let [zones (read-zones text)
        kept (vec (remove #(str/starts-with? (:name %) (str tag "-")) zones))]
    (if (= (count kept) (count zones)) text (zones-text kept))))

(defn read-markers [text]
  (if (str/blank? text) #js [] (js/JSON.parse text)))

(defn markers-text [arr] (str (js/JSON.stringify arr nil 1) "\n"))

(defn markers-with
  "The places.json text with markers appended."
  [text markers]
  (let [arr (read-markers text)]
    (run! #(.push arr (clj->js %)) markers)
    (markers-text arr)))

(defn markers-without
  "The places.json text without the markers by tag."
  [text tag]
  (let [arr (read-markers text)
        kept (.filter arr (fn [m] (not= tag (.-by m))))]
    (if (= (.-length kept) (.-length arr)) text (markers-text kept))))

(def step-kinds #{:summon :rcon :rcon-until :job :wait-s :await :kill-body :time-set :restart-body :cli :http :until :until-seen :until-rcon :until-effect-gone :until-pos})
(def after-kinds #{:block :not-block :any-not-block :body-near :body-far :item :entities :memory :file})
(def http-ops #{:submit :cancel :cancel-all})
(def cli-tools #{"jobs" "plans" "map" "world" "say" "time" "triggers" "entities" "observe" "workspace" "blueprints" "quote" "snapshot"})

(defn seen-check
  "The :until check of an :until-seen target: {:entity type} (at least :min, default 1, entities the body has seen) or {:block name} (blocks in its memory)."
  [{:keys [entity block min]}]
  (if entity
    [:cli "entities" ["$body" "--world" "$world" "--type" entity] {:total [:>= (or min 1)]}]
    [:cli "observe" ["$body" "--world" "$world" "seen" "--names" block] {:ok true :blocks [:has {:name block}]}]))

(defn rcon-reply-matches?
  "Whether the server's reply contains a match of the regex pattern."
  [reply pattern]
  (boolean (and reply (re-find (re-pattern pattern) (str reply)))))

(defn step-problem?
  "Whether an :act step has a kind or shape the runner cannot run (kinds without extra shape are only checked by name)."
  [[op a b :as step]]
  (case op
    :until (not (and (= 3 (count step)) (number? b) (vector? a) (or (#{:memory :file} (first a)) (and (= :cli (first a)) (not (step-problem? a))))))
    :until-seen (not (and (= 3 (count step)) (number? b) (map? a) (string? (or (:entity a) (:block a))) (or (nil? (:min a)) (number? (:min a)))))
    :until-rcon (not (and (= 4 (count step)) (string? a) (string? b) (number? (nth step 3))))
    :until-effect-gone (not (and (= 3 (count step)) (string? a) (number? b)))
    :until-pos (not (and (= 3 (count step)) (map? a) (number? b)))
    :cli (not (and (cli-tools a) (vector? b) (every? #(or (string? %) (map? %) (vector? %)) b)))
    :http (not (http-ops a))
    (not (step-kinds op))))

(defn unbounded-selectors
  "The :rcon / :rcon-until commands of the act steps with an @e selector that is not a box (no dx= or $BOX), bare or
  bracketed: a radius or an unbounded selector reaches the neighbouring plots' mobs (kill, tp, effect, ...). Write $BOX
  (the plot's box) instead. @p/@s/@a target the test's own body and are not checked; `if entity` reads are allowed."
  [c]
  (->> (:act c)
       (filter (comp #{:rcon :rcon-until :until-rcon} first))
       (map second)
       (remove #(re-find #"(?:if|unless) entity @e" (str %)))
       (filter #(some (fn [[_ sel]] (not (or (str/includes? (or sel "") "dx=") (str/includes? (or sel "") "$BOX"))))
                      (re-seq #"@e(?:\[([^\]]*)\])?" (str %))))))

(defn centred-offsets
  "The :rcon commands with `execute positioned` whose relative offset ends in .5: positioned already centres the origin
  on the block, so ~N.5 lands on a block corner."
  [c]
  (->> (:act c)
       (filter (comp #{:rcon :rcon-until} first))
       (map second)
       (filter #(and (re-find #"execute positioned" (str %)) (re-find #"~-?\d+\.5(?!\d)" (str %))))))

(defn coords? [v] (and (vector? v) (= 3 (count v)) (every? number? v)))

(defn bare-plot-positions
  "The :pos / :at / :target [x y z] vectors in the act steps written without #at whose y is a small plot-relative
  height (-16..16): in the real world they send the job to another place."
  [c]
  (->> (tree-seq coll? seq (:act c))
       (filter map?)
       (mapcat (fn [m] (keep (fn [k] (get m k)) [:pos :at :target])))
       (filter #(and (coords? %) (<= -16 (second %) 16)))))

(def fill-limit "Most blocks one /fill command accepts." 32768)

(defn fill-volume [[x y z] [x2 y2 z2]]
  (* (inc (js/Math.abs (- x2 x))) (inc (js/Math.abs (- y2 y))) (inc (js/Math.abs (- z2 z)))))

(defn unsplittable-fills
  "The :blocks fills with a :hollow / :outline mode (splitting them would change the shape) that are over fill-limit."
  [c]
  (filter (fn [[op a b _ d]] (and (= :fill op) d (> (fill-volume (mapv #(js/Math.floor %) a) (mapv #(js/Math.floor %) b)) fill-limit)))
          (:blocks c)))

;; Full cubes the check treats as solid; anything else (torch, door, water, a bed...) never flags a position.
(def solid-block #"^(stone|dirt|coarse_dirt|rooted_dirt|grass_block|cobblestone|bedrock|obsidian|netherrack|gravel|sand|granite|diorite|andesite|deepslate|clay|glass|glowstone|sea_lantern|hay_block|magma_block|slime_block|melon|moss_block|soul_sand|crafting_table|furnace|smoker|blast_furnace|snow_block|stone_bricks)$|_ore$|_log$|_leaves$|_planks$|_block$")

(defn solid-block? [block]
  (boolean (re-find solid-block (-> (str block) (str/replace #"^minecraft:" "") (str/replace #"\[.*" "")))))

(defn block-op-effect
  "What one :blocks entry does to the plot-relative cell: :solid, :clear, or nil (not touched, or a mode this check does not model)."
  [[op a b c d] cell]
  (let [block (if (= op :fill) c b)
        mode (when (= op :fill) d)
        lo (mapv #(js/Math.floor %) (if (= op :fill) (mapv min a b) a))
        hi (mapv #(js/Math.floor %) (if (= op :fill) (mapv max a b) a))
        inside? (every? true? (map <= lo cell hi))
        flat-ring? (and (= mode :outline) (= (lo 1) (hi 1)))
        border? (some true? (map-indexed (fn [i [l c h]] (and (not (and flat-ring? (= i 1))) (or (= c l) (= c h)))) (map vector lo cell hi)))
        effect (if (solid-block? block) :solid :clear)]
    (cond
      (not inside?) nil
      (= op :set) effect
      (nil? mode) effect
      (not (#{:hollow :outline} mode)) nil
      border? effect
      (= mode :hollow) :clear)))

(defn solid-cell?
  "Does the cell (plot-relative, floored) end solid after the case's :blocks are applied in order?"
  [c cell]
  (= :solid (some #(block-op-effect % cell) (reverse (:blocks c)))))

(defn solid-placements
  "The :summon positions, :body :at feet and head cells and go-to :pos #at positions of the case that end inside a solid :blocks cell, as [what position].
  A go-to with :unknown true may aim into ground by design, and an :rcon setblock / fill in :act changes the blocks after the check: neither is judged."
  [c]
  (let [rcon-edits? (some #(and (string? %) (re-find #"setblock|fill" %)) (tree-seq coll? seq (:act c)))
        cell #(mapv (fn [v] (js/Math.floor v)) %)
        [bx by bz] (get-in c [:body :at])
        go-to-pos (->> (tree-seq coll? seq (:act c))
                       (filter #(and (seq? %) (= 'jobs.movement.go-to (first %)) (not (:unknown (second %)))))
                       (keep #(let [p (:pos (second %))] (when (map? p) (::rel p)))))
        summons (->> (tree-seq coll? seq (:act c))
                     (filter #(and (vector? %) (= :summon (first %)) (coords? (nth % 2 nil))))
                     (map #(nth % 2)))]
    (->> (concat (when-not rcon-edits?
                   (concat (map vector (repeat :summon) summons)
                           (map vector (repeat :go-to-pos) (filter coords? go-to-pos))))
                 (when (coords? [bx by bz]) [[:body-feet [bx by bz]] [:body-head [bx (inc by) bz]]]))
         (filter (fn [[_ p]] (solid-cell? c (cell p)))))))

;; A goal this far outside the plot is deliberately far (an unloaded-goal case), not a mistake.
(def far-margin 64)

(defn out-of-plot-positions
  "The plot-relative x z positions of the case's :blocks corners and #at act positions that lie outside the plot
  (0..length-1, 0..width-1; 32 by default). #at positions more than far-margin outside are deliberate and allowed."
  [c]
  (let [{:keys [length width]} (:plot c)
        lx (or length 32) lz (or width 32)
        out? (fn [margin [x _ z]] (or (< x (- margin)) (>= x (+ lx margin)) (< z (- margin)) (>= z (+ lz margin))))
        block-corners (mapcat (fn [[op a b]] (case op :fill [a b] :set [a] [])) (:blocks c))
        at-positions (->> (tree-seq coll? seq (:act c)) (filter map?) (keep ::rel))]
    (concat (filter #(and (coords? %) (out? 0 %)) block-corners)
            (filter #(and (coords? %) (out? 0 %) (not (out? far-margin %))) at-positions))))

(defn problems
  "Why a merged case cannot run, as a vector of strings (empty when it can)."
  [c]
  (let [h (get-in c [:plot :height])
        {:keys [length width]} (:plot c)
        {:keys [max-length max-width min-length]} large-lanes]
    (cond-> []
      (not (or (nil? length) (and (int? length) (<= min-length length max-length)))) (conj (str ":plot :length must be an integer " min-length ".." max-length))
      (not (or (nil? width) (and (int? width) (<= 16 width max-width)))) (conj (str ":plot :width must be an integer 16.." max-width))
      (not (contains? #{nil :keep} (:mobs c))) (conj ":mobs must be :keep (or absent)")
      (not (string? (:name c))) (conj ":name must be a string")
      (not (#{:day :night :night-exclusive :any} (:time c))) (conj ":time must be :day, :night, :night-exclusive or :any")
      (not (contains? #{nil :rain :thunder :clear} (:weather c))) (conj ":weather must be :rain, :thunder or :clear")
      (not (and (int? h) (< 1 h 32))) (conj ":plot :height must be an integer 2..31")
      (not (every? #(and (keyword? (:kind %)) (map? (:data %))) (:memory c))) (conj ":memory entries need a keyword :kind and a map :data")
      (not (every? #(or (nil? (:age-s %)) (number? (:age-s %))) (:memory c))) (conj ":memory :age-s must be a number of seconds")
      (not (or (nil? (get-in c [:body :pitch])) (number? (get-in c [:body :pitch])))) (conj ":body :pitch must be a number of degrees")
      (and (seq (:memory c)) (:keep-memory c)) (conj ":memory cannot be combined with :keep-memory")
      (not (or (nil? (:settings c)) (and (map? (:settings c)) (every? #(and (keyword? %) (namespace %)) (keys (:settings c)))))) (conj ":settings must be a map of namespaced setting keys")
      (and (:settings c) (:keep-memory c)) (conj ":settings cannot be combined with :keep-memory")
      (not (every? #(and (string? (:name %)) (not (str/blank? (:name %))) (coords? (:min %)) (coords? (:max %))) (:zones c))) (conj ":zones entries need a :name and :min / :max [x y z]")
      (not (every? #(and (string? (:name %)) (not (str/blank? (:name %))) (coords? (:pos %))) (:places c))) (conj ":places entries need a :name and :pos [x y z]")
      (not (vector? (get-in c [:body :at]))) (conj ":body :at must be [x y z]")
      (some step-problem? (:act c)) (conj (str ":act steps must be one of " (sort step-kinds)))
      (seq (bare-plot-positions c)) (conj (str "a job position " (first (bare-plot-positions c)) " with a plot-relative height needs #at"))
      (seq (out-of-plot-positions c)) (conj (str "position " (first (out-of-plot-positions c)) " is outside the plot: give :plot :length / :width or move it inside"))
      (seq (solid-placements c)) (conj (str "the " (name (ffirst (solid-placements c))) " position " (second (first (solid-placements c))) " is inside a solid :blocks cell"))
      (seq (unsplittable-fills c)) (conj (str "a :hollow / :outline fill over " fill-limit " blocks cannot be split: " (first (unsplittable-fills c))))
      (seq (centred-offsets c)) (conj "`execute positioned` already centres the origin on the block: write ~N, not ~N.5 (that is a block corner)")
      (seq (unbounded-selectors c)) (conj "an @e selector must be bounded to the plot: use $BOX (x,y,z,dx,dy,dz), not distance")
      (some #(not (and (after-kinds (first %)) (or (not= :file (first %)) (string? (second %))))) (:after c)) (conj (str ":after checks must be one of " (sort after-kinds)))
      (some #(and (map? %) (contains? % ::rel) (= :vec (::as %))) (tree-seq coll? seq (:after c)))
      (conj "positions in :after are plot-relative; drop #at")
      (and (empty? (:expect c)) (empty? (:after c))) (conj "the case checks nothing: it needs an :expect or :after")
      (and (empty? (:after c)) (seq (:expect c)) (not-any? #(or (:event %) (:from-event %) (pos? (or (:at-least %) 0))) (:expect c)))
      (conj "the case has no positive anchor: silence on an empty log passes; add an :event expectation, :from-event, a :count-event with :at-least or an :after check")
      (some #(not (or (:event %) (:no-event %) (:count-event %))) (:expect c)) (conj ":expect entries need :event, :no-event or :count-event")
      (some #(and (:count-event %) (not (and (number? (:for-s %)) (or (number? (:at-least %)) (number? (:at-most %)))))) (:expect c))
      (conj ":count-event expectations need :for-s and :at-least or :at-most")
      (some #(and (:event %) (not (number? (:within-s %)))) (:expect c)) (conj ":event expectations need :within-s")
      (some #(and (:no-event %) (not (number? (:for-s %)))) (:expect c)) (conj ":no-event expectations need :for-s")
      (not (or (nil? (:idle-grace-s c)) (and (number? (:idle-grace-s c)) (pos? (:idle-grace-s c))))) (conj ":idle-grace-s must be a positive number of seconds"))))

(defn kinds-in-text
  "Every keyword name in source text (`;;` comments cut) plus, for JS, every quoted word: an upper bound of the event kinds the file can emit."
  [text]
  (into #{} (map second) (re-seq #"[:'\"]([a-zA-Z][\w.*!?<>=+/-]*)" (str/replace text #";;.*" ""))))

;; Kinds the engine builds at run time, so no literal names them.
(def computed-kinds #"^trip\..+|.+_missing$|.+\.trespass-last-resort$")

(defn unknown-kinds
  "The event kinds the case names in a pattern with :source :job, :reflex or :trigger (an :event, :no-event, :count-event,
  :from-event, :until or :await pattern) that no source emits: not in known (kind names), not a computed kind."
  [c known]
  (->> (tree-seq coll? seq (dissoc c :id :file))
       (filter #(and (map? %) (#{:job :reflex :trigger} (:source %))))
       (mapcat #(let [k (:kind %)] (if (set? k) k [k])))
       (filter keyword?)
       (remove #(let [n (subs (str %) 1)] (or (known n) (re-find computed-kinds n))))
       distinct))

(defn job-step? [step] (and (vector? step) (= :job (first step))))

(defn file-cases
  "The cases of one fixture file's text: a single case map, or {:defaults {...} :cases [...]}. Each case gets :id
  \"<stem>/<name>\" and :file; a case with problems carries them under :problems."
  [text stem]
  (let [form (read-edn text)
        [file-defaults cases] (if (contains? form :cases) [(:defaults form {}) (:cases form)] [{} [form]])]
    (mapv (fn [c]
            (let [m (merge-case file-defaults c)
                  ps (cond-> (problems m)
                       (and (some job-step? (:act file-defaults)) (some job-step? (:act c)))
                       (conj "a job in :defaults :act runs before this case's own job: put it in the cases that need it"))]
              (cond-> (assoc m :id (str stem "/" (:name m)) :file stem)
                (seq ps) (assoc :problems ps))))
          cases)))

(defn case-uuids
  "The literal entity UUIDs a case mentions (not in :memory, which holds no entities), normalised: NBT int arrays \"I;a,b,c,d\" and hex strings (lower case)."
  [c]
  (let [text (pr-str (dissoc c :id :file :memory))]
    (into (set (map (fn [[_ a]] (str "I;" (str/join "," (map str/trim (str/split a #",")))))
                    (re-seq #"UUID:\s*\[I;([^\]]*)\]" text)))
          (map str/lower-case)
          (re-seq #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}" text))))

(defn uuid-reuse
  "{uuid [case ids]} for each UUID literal that appears in more than one case (cases run at once on neighbouring plots)."
  [cases]
  (->> cases
       (mapcat (fn [c] (map (fn [u] [u (:id c)]) (case-uuids c))))
       (reduce (fn [m [u id]] (update m u (fnil conj []) id)) {})
       (into {} (filter (fn [[_ ids]] (< 1 (count ids)))))))

(defn select-cases
  "Cases whose :tags hold tag (when given), whose id contains match (when given) and is one of match-ids (when given)."
  [cases {:keys [tag match match-ids]}]
  (filterv #(and (or (nil? tag) (contains? (set (:tags %)) (keyword tag)))
                 (or (nil? match) (str/includes? (:id %) match))
                 (or (empty? match-ids) (contains? match-ids (:id %))))
           cases))

;; ------------------------------------------------------------------ resolving

(defn abs-pos [[ox oy oz] [x y z]] [(+ ox x) (+ oy y) (+ oz z)])

(defn abs-segment
  "A #seg map made absolute against origin, with the :open-ends default and the id jobs.lib.strip/seg-id gives."
  [{:keys [axis y line from to] :as m} [ox oy oz]]
  (let [x? (= :x axis)
        y (+ oy y) line (+ line (if x? oz ox)) from (+ from (if x? ox oz)) to (+ to (if x? ox oz))]
    (assoc m :y y :line line :from from :to to :open-ends (or (:open-ends m) #{})
           :id (str (name axis) y "-" line ":" from ".." to))))

(defn resolve-tags
  "Every #at / #xyz value in form made absolute against origin."
  [form origin]
  (walk/postwalk (fn [v]
                   (cond
                     (and (map? v) (contains? v ::seg)) (abs-segment (::seg v) origin)
                     (and (map? v) (contains? v ::rel))
                     (if (= 2 (count (::rel v)))
                       (let [[x z] (::rel v)
                             [ox _ oz] origin]
                         (if (= :map (::as v)) {:x (+ ox x) :z (+ oz z)} [(+ ox x) (+ oz z)]))
                       (let [[x y z] (abs-pos origin (::rel v))]
                         (if (= :map (::as v)) {:x x :y y :z z} [x y z])))
                     :else v))
                 form))

(defn num-str
  "A coordinate as RCON wants it: integers bare, others as decimals."
  [n]
  (if (== n (js/Math.floor n)) (str (js/Math.floor n)) (str n)))

(defn xyz-str [p] (str/join " " (map num-str p)))

(defn nbt-with-tag
  "The summon NBT text with Tags:[\"wt\"] added, so the runner can find and remove what it summoned."
  [nbt]
  (let [nbt (str/trim (or nbt ""))]
    (if (or (str/blank? nbt) (= "{}" nbt))
      "{Tags:[\"wt\"],PersistenceRequired:1b}"
      (str "{Tags:[\"wt\"],PersistenceRequired:1b," (subs nbt 1)))))

(defn substitute
  "Raw RCON text with $BODY, $X, $Y, $Z (the plot origin) and $BOX (a selector box over the plot, when given) filled in."
  ([text body origin] (substitute text body origin nil))
  ([text body [ox oy oz] box]
   (-> text
       (str/replace "$BOX" (or box "$BOX"))
       (str/replace "$BODY" body)
       (str/replace "$X" (str ox))
       (str/replace "$Y" (str oy))
       (str/replace "$Z" (str oz)))))

;; ------------------------------------------------------------------ commands

(defn box-selector
  "An entity selector box over the whole plot, from the floor up to the top of the cleared space."
  [grid [ox oy oz] height]
  (let [[sx sz] (dims grid)]
    (str "x=" ox ",y=" (dec oy) ",z=" oz ",dx=" (dec sx) ",dy=" (inc height) ",dz=" (dec sz))))

(defn segments
  "[from to] x ranges of at most n blocks covering [ox, ox + len)."
  [ox len n]
  (for [x (range ox (+ ox len) n)] [x (min (+ ox (dec len)) (+ x (dec n)))]))

(defn lane-grid
  "The grid a plot's footprint is cleared and loaded over: a large plot (it lies in a lane) takes the lane's full width,
  so the floor an earlier, wider plot left beside it is removed."
  [grid]
  (let [[sx sz] (dims grid)]
    (if (or (> sx 32) (> sz 32)) (assoc grid :size-z (:max-width large-lanes)) grid)))

(defn island
  "[grid origin height] of the box a mob plot's setup clears to air: mob-margin round the plot and 8 below its floor."
  [grid [ox oy oz] height]
  (let [[sx sz] (dims grid) m mob-margin]
    [(assoc grid :size-x (+ sx m m) :size-z (+ sz m m)) [(- ox m) (- oy 7) (- oz m)] (+ height 7)]))

(defn load-box
  "[grid origin] of the area a plot loads: a large plot its lane's width, a mob plot its island."
  [grid origin]
  (if (:mob-plots? grid)
    (take 2 (island grid origin 0))
    [(lane-grid grid) origin]))

(defn forceload-commands
  "One forceload per 128-block slice of the plot's load-box (a command takes at most 256 chunks)."
  [op grid origin]
  (let [[grid [ox _ oz]] (load-box grid origin)
        [sx sz] (dims grid)]
    (for [[x0 x1] (segments ox sx 128)]
      (str "forceload " op " " x0 " " oz " " x1 " " (+ oz (dec sz))))))

(defn clear-commands
  "Air over the plot from the floor up to height, then the floor layer, in 64-long slices so that no fill exceeds 32768 blocks."
  [grid [ox oy oz] height floor]
  (let [[sx sz] (dims grid)
        top (+ oy height)]
    (vec (mapcat (fn [[x0 x1]]
                   (let [layers (max 1 (quot 32768 (* (inc (- x1 x0)) sz)))]
                     (conj (vec (for [y0 (range oy (inc top) layers)]
                                  (str "fill " x0 " " y0 " " oz " " x1 " " (min top (+ y0 (dec layers))) " " (+ oz (dec sz)) " air")))
                           (str "fill " x0 " " (dec oy) " " oz " " x1 " " (dec oy) " " (+ oz (dec sz)) " " floor))))
                 (segments ox sx 64)))))

(defn kill-command [grid origin height]
  (str "kill @e[type=!player," (box-selector grid origin height) "]"))

(def hostile-types
  "Hostile mob types the runner kills near a plot (never players, bodies or animals)."
  ["zombie" "zombie_villager" "husk" "drowned" "skeleton" "stray" "bogged" "spider" "cave_spider" "creeper" "enderman"
   "endermite" "witch" "slime" "phantom" "silverfish" "pillager" "vindicator" "evoker" "vex" "ravager" "guardian"
   "breeze" "wither_skeleton" "blaze" "ghast" "hoglin" "zoglin" "piglin_brute"])

(defn clear-hostiles-commands
  "One kill per hostile type inside the plot's box (never a radius: it reaches the neighbouring plots' mobs); none when
  the case has :mobs :keep."
  [grid origin c]
  (if (= :keep (:mobs c))
    []
    (let [box (box-selector grid origin (get-in c [:plot :height] 6))]
      (vec (for [t hostile-types] (str "kill @e[type=minecraft:" t "," box "]"))))))

(def natural-hostile-types
  "Hostile mob types that spawn by themselves in the dark overworld."
  ["zombie" "zombie_villager" "husk" "drowned" "skeleton" "stray" "bogged" "spider" "creeper" "enderman" "witch" "slime"
   "phantom"])

(def natural-margin
  "How far around the plot a case clears naturally spawned hostiles: beyond a skeleton's 16-block line of fire."
  24)

(defn natural-hostiles-commands
  "One kill per natural hostile type in the plot's box widened by natural-margin, never one a runner
  summoned (tag wt): mobs spawned on the dark plots around (or left from dusk) would answer the body's hostile reflex.
  None with :mobs :keep."
  [grid [ox oy oz] c]
  (if (= :keep (:mobs c))
    []
    (let [[sx sz] (dims grid)
          m natural-margin
          box (str "x=" (- ox m) ",y=" (- (dec oy) m) ",z=" (- oz m) ",dx=" (+ (dec sx) (* 2 m))
                   ",dy=" (+ (inc (get-in c [:plot :height] 6)) (* 2 m)) ",dz=" (+ (dec sz) (* 2 m)))]
      (vec (for [t natural-hostile-types] (str "kill @e[type=minecraft:" t ",tag=!wt," box "]"))))))

(defn setup-commands
  "Forceload the plot, kill every non-player entity in it, clear it to air and lay the floor (a large plot first drops
  the floor of its whole lane width, a mob plot first clears its island)."
  [grid origin c]
  (let [{:keys [height floor]} (:plot c)
        lane (lane-grid grid)
        [ig io ih] (island grid origin height)]
    (-> (vec (forceload-commands "add" grid origin))
        (cond-> (:mob-plots? grid) (-> (conj (kill-command ig io ih)) (into (clear-commands ig io ih "air"))))
        (conj (kill-command grid origin height))
        (cond-> (not= lane grid) (into (clear-commands lane origin 0 "air")))
        (into (clear-commands grid origin height floor)))))

(defn reset-plot-commands
  "Clear a plot that may hold a left-over case (its full height), without a body connected: forceload, kill, clear,
  forceload removed. The runner sends it for the plot the body was last left on, before the body starts."
  [grid origin]
  (let [plot {:plot {:height 31 :floor "stone"}}]
    (into (setup-commands grid origin plot) (forceload-commands "remove" grid origin))))

(def clean-start-files
  "The body's files (in its engine dir) a clean start deletes: engine memory and the seen-blocks memory, whose cells
  of the last case's plot would otherwise hold blocks that are gone."
  ["memory.edn" "seen.bin"])

(defn block-pos
  "A plot-relative position as a block position: absolute, each coordinate floored (block commands take integers)."
  [origin p]
  (mapv #(js/Math.floor %) (abs-pos origin p)))

(defn block-command
  "One :blocks entry: [:fill [x y z] [x y z] block] (optionally :hollow / :outline as a fifth element) or
  [:set [x y z] block], plot-relative."
  [origin [op a b c d]]
  (case op
    :fill (str "fill " (xyz-str (block-pos origin a)) " " (xyz-str (block-pos origin b)) " " c (when d (str " " (name d))))
    :set (str "setblock " (xyz-str (block-pos origin a)) " " b)
    (throw (js/Error. (str "unknown block op " op)))))

(defn split-fill
  "The [a b] corner pairs that together cover the fill a..b with none over fill-limit blocks, cut in halves along
  the longest side."
  [a b]
  (if (<= (fill-volume a b) fill-limit)
    [[a b]]
    (let [axis (apply max-key #(js/Math.abs (- (b %) (a %))) [0 1 2])
          mid (quot (+ (a axis) (b axis)) 2)
          up? (<= (a axis) (b axis))]
      (into (split-fill a (assoc b axis mid))
            (split-fill (assoc a axis (if up? (inc mid) (dec mid))) b)))))

(defn flat-ring
  "The four [a b] side lines of the ring around a one-layer a..b: /fill outline would fill the whole slab."
  [[x1 y z1] [x2 _ z2]]
  [[[x1 y z1] [x2 y z1]] [[x1 y z2] [x2 y z2]] [[x1 y z1] [x1 y z2]] [[x2 y z1] [x2 y z2]]])

(defn block-commands
  "RCON commands for the case's :blocks. A plain fill over fill-limit blocks (the server refuses it) is split; a
  one-layer :outline fill is the hollow ring (four lines)."
  [origin c]
  (vec (mapcat (fn [[op a b block d :as entry]]
                 (let [fa (mapv #(js/Math.floor %) a)
                       fb (mapv #(js/Math.floor %) b)]
                   (cond
                     (and (= :fill op) (nil? d))
                     (map (fn [[x y]] (block-command origin [:fill x y block])) (split-fill fa fb))
                     (and (= :fill op) (= :outline d) (= (fa 1) (fb 1)))
                     (map (fn [[x y]] (block-command origin [:fill x y block])) (flat-ring fa fb))
                     :else [(block-command origin entry)])))
               (:blocks c))))

(defn build-failure
  "The first RCON reply of a setup or block command that says the plot was not built (a position not loaded yet or outside
  the world), else nil. \"No blocks were filled\" is not one: a fill over identical blocks says that too."
  [replies]
  (first (filter #(re-find #"(?i)not loaded|outside (of )?the world|out of the world" (or % "")) replies)))

(defn register-put-argvs
  "The `triggers.mjs` argument vectors that put each register entry on a running body, in order. The runner starts the
  body with an empty register and puts these once the plot, the time and the body are in place, so no trigger fires
  on the body's old spot while the case is being built. #at / #xyz markers anywhere in the entries are made absolute
  against origin."
  [body world register origin]
  (mapv (fn [{:keys [id trigger persistence cooldown-s job args backoff] condition :when}]
          (let [id (name (or id trigger))
                opt (fn [flag v f] (when (some? v) [flag (f v)]))]
            (-> [body "--world" world "put" id]
                (into (if condition (opt "--when" condition pr-str) ["--trigger" (name trigger)]))
                (into (opt "--persistence" persistence name))
                (into (opt "--cooldown-s" cooldown-s str))
                (into (opt "--job" job pr-str))
                (into (opt "--args" args pr-str))
                (into (opt "--backoff" backoff pr-str))
                (into ["--by" "world-test"]))))
        (resolve-tags register origin)))

(defn park-commands
  "Before the plot is built: the body (still wherever the last case left it, maybe inside this plot) becomes a spectator, so stone filled over it does not hurt it; ensure-at-start! puts it back in survival."
  [body]
  [(str "gamemode spectator " body)])

(def worn-slots
  "Slots /clear is not trusted with: a helmet left by an earlier case changes go-to's air plan."
  ["armor.head" "armor.chest" "armor.legs" "armor.feet" "weapon.offhand"])

(defn body-commands
  "Put the body at its start (facing :yaw, 0 = south, 180 = north; :pitch, 0 level, positive down), still a spectator
  (ensure-at-start! makes it survival once it stands there), healed and fed, its inventory and effects as the case says."
  [origin body c]
  (let [{:keys [at yaw pitch inventory effects spawnpoint]} (:body c)]
    (-> [(str "clear " body)
         (str "effect clear " body)]
        (into (for [slot worn-slots] (str "item replace entity " body " " slot " with minecraft:air")))
        (into [(str "tp " body " " (xyz-str (abs-pos origin at)) " " (or yaw 0) " " (or pitch 0))
               (str "effect give " body " minecraft:instant_health 1 10 true")
               (str "effect give " body " minecraft:saturation 1 10 true")])
        (into (for [[item n] inventory] (str "give " body " " item " " (or n 1))))
        (into (for [[effect seconds amp] effects]
                (str "effect give " body " " effect " " (or seconds 600) " " (or amp 0) " true")))
        (cond-> spawnpoint (conj (str "spawnpoint " body " " (xyz-str (abs-pos origin spawnpoint))))))))

(declare reply-pos)

(defn start-hurt
  "The setup error when the events logged while the case set up show the body suffocating (it stood in a block), else nil.
  A hurt whose merge window began before parked-at (ms) is a hit from the body's login, not from the start."
  [parked-at events]
  (when-let [e (some #(when (and (= :hurt (:kind %)) (some #{"suffocation"} (get-in % [:data :causes]))
                                 (>= (get-in % [:data :since-ms] 0) parked-at))
                        %)
                     events)]
    (str "the body was hurt by suffocation at the start (" (get-in e [:data :amount]) " damage): it stood inside a block")))

(defn survival-commands [body] [(str "gamemode survival " body)])

(defn start-check-command [body] (str "data get entity " body " Pos"))

(defn judge-start
  "Is the body at the case's start (within 4 blocks of :body :at) given the reply of start-check-command? {:pass? :why}."
  [origin c reply]
  (let [want (abs-pos origin (get-in c [:body :at]))
        p (reply-pos reply)
        d (when p (js/Math.hypot (- (p 0) (want 0)) (- (p 1) (want 1)) (- (p 2) (want 2))))]
    (if (and d (<= d 4))
      {:pass? true}
      {:pass? false
       :why (str "the body is not at its start: it is at " (if p (xyz-str p) (str/trim reply)) ", the start is " (xyz-str want))})))

(defn summon-command [origin [_ type pos nbt]]
  (str "summon " type " " (xyz-str (abs-pos origin pos)) " " (nbt-with-tag nbt)))

(def park-height
  "How far above a plot's origin the body waits between cases: beyond its sight (48) over the tallest plot (31), so a
  restarted body, which logs in where it stood, never sees a plot before its blocks are built (memory would keep air)."
  100)

(defn cleanup-commands
  "Leave the plot: its entities killed, the plot cleared to its floor, the body's inventory and effects cleared, the
  body parked as a spectator park-height above the plot, the forceload removed."
  [grid origin body c]
  (let [{:keys [height floor]} (:plot c)]
    (-> [(kill-command grid origin height)]
        (into (clear-commands grid origin height floor))
        (into [(str "clear " body)
               (str "effect clear " body)
               (str "gamemode spectator " body)
               (str "tp " body " " (xyz-str (abs-pos origin [1.5 park-height 1.5])) " 0 0")])
        (into (forceload-commands "remove" grid origin)))))

(defn plan-file-text
  "A :plans entry ({:id .. :parts ..}, positions already absolute) as the plan file text, its id prefixed so the
  runner owns it."
  [plan prefix]
  (pr-str (assoc plan :id (str prefix (:id plan)))))

(defn resolve-plan-refs
  "Replaces every \"$plan:<id>\" string in form with the runner's plan id (prefix + id), so a fixture names its
  own plan without knowing the body."
  [form prefix]
  (walk/postwalk (fn [v] (if (and (string? v) (str/starts-with? v "$plan:"))
                           (str prefix (subs v 6))
                           v))
                 form))

(defn resolve-expect-refs
  "Case c with \"$plan:<id>\" resolved inside its :expect (event and :no-event data), as in setup and args."
  [c prefix]
  (cond-> c (:expect c) (update :expect resolve-plan-refs prefix)))

(defn resolve-body-refs
  "Replaces \"$body\" in every string of form with the name of the body running the case (a plan's :metadata :by names
  its own maker whatever probe body runs it) and \"$tag\" with its lowercase shared tag (a marker name no other shard shares)."
  [form body]
  (walk/postwalk (fn [v] (if (string? v) (-> v (str/replace "$body" body) (str/replace "$tag" (shared-tag body))) v)) form))

(defn entity-id-types
  "The mob types named by \"$entity-id:<type>\" strings in form (a :job spec), as a set."
  [form]
  (let [found (atom #{})]
    (walk/postwalk (fn [v] (when (and (string? v) (str/starts-with? v "$entity-id:")) (swap! found conj (subs v 11))) v) form)
    @found))

(defn resolve-entity-refs
  "Replaces every \"$entity-id:<type>\" string in form with (ids type), the numeric id of a summoned mob of that type."
  [form ids]
  (walk/postwalk (fn [v] (if (and (string? v) (str/starts-with? v "$entity-id:")) (get ids (subs v 11) v) v)) form))

;; ------------------------------------------------------------------ after checks

(defn entities-selector
  "The @e[...] selector of an :entities check's [sel [a b]] args (positions plot-relative), limited to one entity for a probe."
  [origin [sel [a b]]]
  (let [[ax ay az] (abs-pos origin a)
        [bx by bz] (abs-pos origin b)]
    (str "@e[" sel ",x=" (min ax bx) ",y=" (min ay by) ",z=" (min az bz)
         ",dx=" (js/Math.abs (- bx ax)) ",dy=" (js/Math.abs (- by ay)) ",dz=" (js/Math.abs (- bz az)) "]")))

(def any-not-block-chunk
  "Cells per command of an :any-not-block check: one command of 40 cells exceeds the RCON request limit and gets an empty reply."
  10)

(defn after-command
  "The RCON command whose reply answers one :after check (positions plot-relative); :any-not-block gives a vector of commands."
  [origin body grid c [op & args]]
  (case op
    :block (str "execute if block " (xyz-str (block-pos origin (first args))) " " (second args))
    :not-block (str "execute if block " (xyz-str (block-pos origin (first args))) " " (second args))
    :any-not-block (let [[boxes block] args]
                     (->> boxes
                          (mapcat (fn [[[ax ay az] [bx by bz]]]
                                    (for [x (range ax (inc bx)) y (range ay (inc by)) z (range az (inc bz))] [x y z])))
                          (map #(str "if block " (xyz-str (block-pos origin %)) " " block))
                          (partition-all any-not-block-chunk)
                          (mapv #(str "execute " (str/join " " %)))))
    (:body-near :body-far) (str "data get entity " body " Pos")
    :item (str "execute if items entity " body " container.* " (first args))
    :entities (str "execute if entity " (entities-selector origin args))
    nil))

(defn after-commands
  "The RCON commands of one :after check, always a vector."
  [origin body grid c check]
  (let [cmd (after-command origin body grid c check)]
    (if (vector? cmd) cmd [cmd])))

(defn group-replies
  "Splits replies into consecutive groups of the given sizes."
  [sizes replies]
  (loop [out [] [n & more] sizes rs replies]
    (if (nil? n) out (recur (conj out (take n rs)) more (drop n rs)))))

(defn join-after-replies
  "The single reply of a check from its commands' replies. An :any-not-block cell set differs when any chunk's test failed; a
  chained `execute if` that fails before its last condition prints nothing, so a blank reply counts as failed."
  [[op] replies]
  (if (= op :any-not-block)
    (if (some #(str/blank? %) replies) "Test failed" (or (first (filter #(str/includes? % "Test failed") replies)) (last replies)))
    (last replies)))

(defn entity-probe-command
  "The RCON command that dumps one entity matching a failed :entities check (nil for other checks)."
  [origin [op & args]]
  (when (= op :entities)
    (let [sel (entities-selector origin args)]
      (str "data get entity " (subs sel 0 (dec (count sel))) ",limit=1] Pos"))))

(defn reply-pos
  "The [x y z] in a `data get entity <body> Pos` reply, or nil."
  [reply]
  (when-let [[_ x y z] (re-find #"\[(-?[\d.E-]+)d, (-?[\d.E-]+)d, (-?[\d.E-]+)d\]" reply)]
    [(js/Number x) (js/Number y) (js/Number z)]))

(defn pos-in-bounds?
  "Whether the Pos reply lies inside bounds, offsets from the plot origin: any of :x-min :x-max :y-min :y-max :z-min :z-max."
  [reply [ox oy oz] {:keys [x-min x-max y-min y-max z-min z-max]}]
  (if-let [[x y z] (reply-pos (str reply))]
    (let [[dx dy dz] [(- x ox) (- y oy) (- z oz)]]
      (and (or (nil? x-min) (>= dx x-min)) (or (nil? x-max) (<= dx x-max))
           (or (nil? y-min) (>= dy y-min)) (or (nil? y-max) (<= dy y-max))
           (or (nil? z-min) (>= dz z-min)) (or (nil? z-max) (<= dz z-max))))
    false))

(def leave-margin
  "Blocks beyond the plot a body may stand before the runner fails its case: the next plot's setup would build over it."
  8)

(defn may-leave-plot?
  "A case with a deliberately far #at position (see far-margin) sends the body out of its plot on purpose; a case
  that kills its body respawns it at the world spawn."
  [c]
  (let [{:keys [length width]} (:plot c)
        lx (or length 32) lz (or width 32)]
    (boolean (or (some #(= :kill-body (first %)) (filter vector? (:act c))) (some (fn [[x _ z]] (or (< x 0) (>= x lx) (< z 0) (>= z lz)))
                   (filter coords? (keep ::rel (filter map? (tree-seq coll? seq (:act c))))))))))

(defn left-plot-why
  "Why the body at reply (a start-check-command reply) left the case's plot by more than leave-margin, else nil."
  [origin c reply]
  (let [{:keys [length width]} (:plot c)
        [ox _ oz] origin
        [x _ z :as p] (reply-pos reply)]
    (when (and p (not (may-leave-plot? c))
               (or (< x (- ox leave-margin)) (>= x (+ ox (or length 32) leave-margin))
                   (< z (- oz leave-margin)) (>= z (+ oz (or width 32) leave-margin))))
      (str "body left its plot: it is at " (xyz-str p) ", the plot starts at " (xyz-str origin)))))

(defn describe-entity-reply
  "Names the entity in a `data get entity <sel> Pos` reply: its type and plot-relative position."
  [origin reply]
  (let [[_ type] (re-find #"^(.+?) has the following entity data" reply)
        pos (reply-pos reply)]
    (if-not (and type pos)
      (str "stray entity: no data in reply: " reply)
      (let [[ox oy oz] origin
            [x y z] pos]
        (str "stray entity " (str/lower-case type) " at plot " [(- x ox) (- y oy) (- z oz)])))))

(defn reply-count
  "The count an `execute if` reply reports: 0 for a failed test, 1 for a pass without a count, nil for any other reply."
  [reply]
  (cond
    (str/includes? reply "Test failed") 0
    (re-find #"Count: (\d+)" reply) (js/Number (second (re-find #"Count: (\d+)" reply)))
    (str/includes? reply "Test passed") 1))

(defn count-ok? [want n]
  (if (vector? want)
    (let [[op v] want]
      (case op :>= (>= n v) :> (> n v) :<= (<= n v) :< (< n v) := (= n v)))
    (= want n)))

(defn too-few?
  "Whether count n misses the :entities expectation want (a number or [op v]) by being under it."
  [want n]
  (let [[op v] (if (vector? want) want [:= want])]
    (case op (:>= :>) true (:<= :<) false := (< n v))))

(defn wanted-type
  "The entity type a positive type= of the selector names, else \"entities\"."
  [selector]
  (or (second (re-find #"(?:^|,)type=([a-z_:]+)" selector)) "entities"))

(defn entities-evidence
  "Evidence of an :entities check: a missed minimum says what was expected, anything else the count."
  [[selector _ want] n]
  (let [[op v] (if (vector? want) want [:= want])]
    (if (and (not (count-ok? want n)) (too-few? want n))
      (str "expected " (case op :> "more than " :>= "at least " "") v " " (wanted-type selector) ", found " n)
      (str "count " n))))

(defn judge-after
  "Pass or fail of one :after check from its command's reply: {:check :pass? :evidence}."
  [origin [op & args :as check] reply]
  (let [n (when (#{:block :not-block :any-not-block :item :entities} op) (reply-count reply))
        result (case op
                 (:block :not-block :any-not-block :item :entities)
                 (cond
                   (nil? n) [false (str "unrecognised reply: " reply)]
                   (= op :block) [(pos? n) reply]
                   (#{:not-block :any-not-block} op) [(zero? n) reply]
                   (= op :item) [(count-ok? (second args) n) (str "count " n)]
                   :else [(count-ok? (nth args 2) n) (entities-evidence args n)])
                 (:body-near :body-far)
                 (let [p (reply-pos reply)
                       [x y z] (abs-pos origin (first args))
                       d (when p (js/Math.hypot (- (p 0) x) (- (p 1) y) (- (p 2) z)))
                       ok? (if (= op :body-far) >= <=)]
                   [(boolean (and d (ok? d (second args)))) (if d (str "distance " (.toFixed d 2)) reply)]))]
    {:check check :pass? (first result) :evidence (second result)
     :stray? (boolean (and (= op :entities) (not (first result)) n (not (too-few? (nth args 2) n))))}))

;; ------------------------------------------------------------------ tool steps and file checks

(defn http-argv
  "The agent-tool arguments of an [:http op arg ...] step: jobs.mjs for submit/cancel (arg: the spec and flags / the job id)."
  [body world [_ op arg flags]]
  (case op
    :submit (into ["engine/tools/jobs.mjs" body "--world" world "submit" (pr-str arg)] (map #(str "--" (name %)) flags))
    :cancel ["engine/tools/jobs.mjs" body "--world" world "cancel" arg]
    :cancel-all ["engine/tools/jobs.mjs" body "--world" world "cancel-all"]))

(defn step-argv
  "The node arguments of a :cli or :http step. In strings \"$body\", \"$world\", \"$job\" (the last submitted job id) and
  \"$event-job\" (the job of the last awaited event, e.g. a reflex's) are filled in, \"$tag\" (inside a string) is the body's
  shared tag and \"$plan:<id>\" the id of the case's plan. A :cli argument that is a map is printed as EDN; its :at
  ([x y z], resolved) becomes :x :y :z; a vector (a resolved position) is spread into one argument per number."
  ([body world step last-job] (step-argv body world step last-job nil))
  ([body world [op tool args :as step] last-job event-job]
   (let [fill (fn [v]
                (cond
                  (not (string? v)) v
                  (str/starts-with? v "$plan:") (str "test-" (str/lower-case body) "-" (subs v 6))
                  :else (case v
                          "$body" body "$world" world "$job" last-job "$event-job" event-job
                          (str/replace v "$tag" (shared-tag body)))))
         texts (fn [v]
                 (cond
                   (string? v) [(fill v)]
                   (vector? v) (mapv str v)
                   :else [(pr-str (walk/postwalk fill (if-let [[x y z] (:at v)] (-> v (dissoc :at) (assoc :x x :y y :z z)) v)))]))]
     (if (= :cli op)
       (into [(str "engine/tools/" tool ".mjs")] (mapcat texts args))
       (http-argv body world (walk/postwalk fill step))))))

(defn argv-gap
  "Why argv cannot run (a placeholder had no value to fill it), nil when it can. With the step and the two ids it names the
  placeholder: \"$event-job\" (no awaited event had a job) or \"$job\" (no job submitted)."
  ([argv] (argv-gap argv nil nil nil))
  ([argv step last-job event-job]
   (when (some nil? argv)
     (if (and (nil? event-job) (str/includes? (pr-str step) "$event-job"))
       "$event-job is used before an awaited event had a job"
       "$job is used before any job was submitted"))))

(defn submitted-id
  "The job id jobs.mjs submit printed, nil when it printed a refusal."
  [out]
  (second (re-find #":id \"(j\d+)\"" (str out))))

(defn step-pattern
  "The answer pattern of a :cli or :http step, nil when it has none."
  [[op a :as step]]
  (case op :cli (nth step 3 nil) :http (nth step (if (= :submit a) 4 3) nil) nil))

(defn writes-shared?
  "True when a case edits the world's shared zones.edn / places.json: it has :zones or :places, or a :cli map step."
  [c]
  (boolean (or (seq (:zones c)) (seq (:places c))
               (some #(and (= :cli (first %)) (= "map" (second %))) (:act c)))))

(defn fill-pattern
  "pattern with the strings \"$job\" and \"$event-job\" replaced by the ids (an item of a jobs list is matched by its id)."
  [pattern last-job event-job]
  (walk/postwalk (fn [v] (case v "$job" last-job "$event-job" event-job v)) pattern))

(defn judge-reply
  "Pass or fail of a tool's answer: with a pattern, the EDN printed must match it (a refusal with exit 1 can be the wanted
  answer); without one, exit code 0. {:pass? :evidence}."
  [pattern code out]
  (if (nil? pattern)
    {:pass? (zero? code) :evidence (str "exit " code ": " (str/trim (str out)))}
    (let [value (try (read-edn out) (catch :default _ ::unreadable))]
      (cond
        (= ::unreadable value) {:pass? false :evidence (str "answer is not EDN: " (str/trim (str out)))}
        (x/matches? pattern value) {:pass? true :evidence (pr-str value)}
        :else {:pass? false :evidence (str "answer " (pr-str value) " does not match " (pr-str pattern))}))))

(defn after-file
  "The file (relative to the body's directory under the world's agents/) a :memory or :file check reads, nil for other checks."
  [[op & args]]
  (case op :memory "engine/memory.edn" :file (first args) nil))

(defn judge-file-after
  "Pass or fail of a :memory / :file check from the file's text (nil: missing): its EDN must match the pattern."
  [[op & args :as check] text]
  (let [pattern (if (= op :memory) (first args) (second args))
        r (if (nil? text)
            {:pass? false :evidence (str (after-file check) " does not exist")}
            (judge-reply pattern 0 text))]
    (assoc r :check check)))
