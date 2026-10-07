(ns world-test.fixture
  "World fixtures: an EDN file describes a live test case on a plot of its own (the blocks to build, the body's start,
  the steps to act, the events to expect). This namespace is pure: it reads the files, resolves plot-relative
  positions against a plot's origin and turns a case into the RCON commands the runner sends. The format is in
  engine/fixtures/world/README.md."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [clojure.walk :as walk]))

;; ------------------------------------------------------------------ the plot grid

(def default-grid
  "The reserved plot grid: plot i is column (mod i cols), row (quot i cols); plots are size x size, floor at y - 1,
  body level y."
  {:x0 20000 :z0 20000 :y 150 :size 32 :cols 20 :rows 20})

(def large-lanes
  "Plots longer or wider than 32 lie in lanes south of the grid (plot indexes 400..415): lane j starts at z z0 + 96 j,
  up to 1024 long and 64 wide, so a lane never touches the grid or another lane."
  {:z0 20704 :stride 96 :count 16 :max-length 1024 :max-width 64 :min-length 16})

(defn plot-origin
  "The origin [x y z] of plot i: its north-west corner at body level."
  [{:keys [x0 z0 y size cols rows]} i]
  (let [n (* cols rows) {lz :z0 :keys [stride count]} large-lanes]
    (cond
      (< -1 (- i n) count) [x0 y (+ lz (* stride (- i n)))]
      (>= i n) (throw (js/Error. (str "plot " i " is outside the grid (" n " plots, then " count " large lanes)")))
      :else [(+ x0 (* size (mod i cols))) y (+ z0 (* size (quot i cols)))])))

(defn case-grid
  "The grid with the case's plot size: :size-x (length along x) and :size-z (width), 32 by default."
  [{:keys [plot]}]
  (let [{:keys [length width]} plot]
    (assoc default-grid :size-x (or length (:size default-grid)) :size-z (or width (:size default-grid)))))

(defn plot-range
  "[first end) of the plot indexes a case may lease: the 32x32 grid, or the large lanes."
  [{:keys [size-x size-z size cols rows]}]
  (let [n (* cols rows)]
    (if (and (<= (or size-x size) 32) (<= (or size-z size) 32)) [0 n] [n (+ n (:count large-lanes))])))

(defn dims [{:keys [size size-x size-z]}] [(or size-x size) (or size-z size)])

(defn grid-centre [{:keys [x0 z0 y size cols rows]}]
  [(+ x0 (quot (* size cols) 2)) y (+ z0 (quot (* size rows) 2))])

;; ------------------------------------------------------------------ reading

(defn rel
  "The #at tag: a plot-relative position written [x y z], resolved to an absolute vector."
  [v] {::rel (vec v) ::as :vec})

(defn rel-map
  "The #xyz tag: a plot-relative position written [x y z], resolved to an absolute {:x :y :z} map."
  [v] {::rel (vec v) ::as :map})

(def readers {'at rel 'xyz rel-map})

(defn read-edn [text]
  (reader/read-string {:readers readers} text))

(def defaults
  {:time :day
   :register []
   :plot {:height 16 :floor "stone"}
   :blocks []
   :memory []
   :plans []
   :body {:at [16.5 0 16.5] :inventory [] :effects [] :settle-s 1}
   :act []
   :expect []
   :after []
   :limit-s 120})

(def concatenated
  "Keys a case adds to its file's :defaults instead of replacing them."
  #{:blocks :memory :act :expect :after :plans})

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

(def default-memory-policy
  "The engine's default policy for a memory kind (engine.memory/default-policy): 50 entries, one hour."
  {:cap 50 :ttl (* 60 60 1000)})

(defn memory-seed
  "The body's memory data (what engine/memory.edn holds) for a case's :memory entries, positions already absolute:
  each {:kind k :data {...}} (optionally :policy {:cap :ttl}) becomes an entry written at now-ms, in order."
  [entries now-ms]
  (reduce (fn [acc {:keys [kind data policy]}]
            (-> acc
                (update-in [:entries kind] (fnil conj []) {:t now-ms :data data})
                (assoc-in [:policies kind] (or policy default-memory-policy))))
          {:entries {} :policies {}}
          entries))

(def step-kinds #{:summon :rcon :rcon-until :job :wait-s :await :kill-body :time-set})
(def after-kinds #{:block :not-block :body-near :body-far :item :entities})

(defn unbounded-selectors
  "The :rcon / :rcon-until commands of the act steps with an @e selector that is not a box (no dx= or $BOX), bare or
  bracketed: a radius or an unbounded selector reaches the neighbouring plots' mobs (kill, tp, effect, ...). Write $BOX
  (the plot's box) instead. @p/@s/@a target the test's own body and are not checked; `if entity` reads are allowed."
  [c]
  (->> (:act c)
       (filter (comp #{:rcon :rcon-until} first))
       (map second)
       (remove #(re-find #"(?:if|unless) entity @e" (str %)))
       (filter #(some (fn [[_ sel]] (not (or (str/includes? (or sel "") "dx=") (str/includes? (or sel "") "$BOX"))))
                      (re-seq #"@e(?:\[([^\]]*)\])?" (str %))))))

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
      (not (and (int? h) (< 1 h 32))) (conj ":plot :height must be an integer 2..31")
      (not (every? #(and (keyword? (:kind %)) (map? (:data %))) (:memory c))) (conj ":memory entries need a keyword :kind and a map :data")
      (and (seq (:memory c)) (:keep-memory c)) (conj ":memory cannot be combined with :keep-memory")
      (not (vector? (get-in c [:body :at]))) (conj ":body :at must be [x y z]")
      (some #(not (step-kinds (first %))) (:act c)) (conj (str ":act steps must be one of " (sort step-kinds)))
      (seq (unbounded-selectors c)) (conj "an @e selector must be bounded to the plot: use $BOX (x,y,z,dx,dy,dz), not distance")
      (some #(not (after-kinds (first %))) (:after c)) (conj (str ":after checks must be one of " (sort after-kinds)))
      (and (empty? (:expect c)) (empty? (:after c))) (conj "the case checks nothing: it needs an :expect or :after")
      (and (empty? (:after c)) (seq (:expect c)) (not-any? #(or (:event %) (:from-event %)) (:expect c)))
      (conj "the case has no positive anchor: silence on an empty log passes; add an :event expectation, :from-event or an :after check")
      (some #(not (or (:event %) (:no-event %))) (:expect c)) (conj ":expect entries need :event or :no-event")
      (some #(and (:event %) (not (number? (:within-s %)))) (:expect c)) (conj ":event expectations need :within-s")
      (some #(and (:no-event %) (not (number? (:for-s %)))) (:expect c)) (conj ":no-event expectations need :for-s"))))

(defn file-cases
  "The cases of one fixture file's text: a single case map, or {:defaults {...} :cases [...]}. Each case gets :id
  \"<stem>/<name>\" and :file; a case with problems carries them under :problems."
  [text stem]
  (let [form (read-edn text)
        [file-defaults cases] (if (contains? form :cases) [(:defaults form {}) (:cases form)] [{} [form]])]
    (mapv (fn [c]
            (let [m (merge-case file-defaults c)]
              (cond-> (assoc m :id (str stem "/" (:name m)) :file stem)
                (seq (problems m)) (assoc :problems (problems m)))))
          cases)))

(defn select-cases
  "Cases whose :tags hold tag (when given) and whose id contains match (when given)."
  [cases {:keys [tag match]}]
  (filterv #(and (or (nil? tag) (contains? (set (:tags %)) (keyword tag)))
                 (or (nil? match) (str/includes? (:id %) match)))
           cases))

;; ------------------------------------------------------------------ resolving

(defn abs-pos [[ox oy oz] [x y z]] [(+ ox x) (+ oy y) (+ oz z)])

(defn resolve-tags
  "Every #at / #xyz value in form made absolute against origin."
  [form origin]
  (walk/postwalk (fn [v]
                   (if (and (map? v) (contains? v ::rel))
                     (let [[x y z] (abs-pos origin (::rel v))]
                       (if (= :map (::as v)) {:x x :y y :z z} [x y z]))
                     v))
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

(defn forceload-commands
  "One forceload per 128-block slice of the plot (a command takes at most 256 chunks); a large plot loads its lane's width."
  [op grid [ox _ oz]]
  (let [[sx sz] (dims (lane-grid grid))]
    (for [[x0 x1] (segments ox sx 128)]
      (str "forceload " op " " x0 " " oz " " x1 " " (+ oz (dec sz))))))

(defn forceload-command [op grid origin] (first (forceload-commands op grid origin)))

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
  "One kill per hostile type within 32 blocks of the plot's centre; none when the case has :mobs :keep."
  [grid origin c]
  (if (= :keep (:mobs c))
    []
    (let [[sx sz] (dims grid)
          [x y z] (abs-pos origin [(quot sx 2) 0 (quot sz 2)])]
      (vec (for [t hostile-types] (str "kill @e[type=minecraft:" t ",x=" x ",y=" y ",z=" z ",distance=..32]"))))))

(defn setup-commands
  "Forceload the plot, kill every non-player entity in it, clear it to air and lay the floor (a large plot first drops
  the floor of its whole lane width)."
  [grid origin c]
  (let [{:keys [height floor]} (:plot c)
        lane (lane-grid grid)]
    (-> (vec (forceload-commands "add" grid origin))
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

(defn block-commands [origin c] (mapv #(block-command origin %) (:blocks c)))

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

(defn body-commands
  "Put the body at its start: on the plot, survival, healed and fed, its inventory and effects as the case says."
  [origin body c]
  (let [{:keys [at inventory effects spawnpoint]} (:body c)]
    (-> [(str "gamemode survival " body)
         (str "clear " body)
         (str "effect clear " body)
         (str "tp " body " " (xyz-str (abs-pos origin at)) " 0 0")
         (str "effect give " body " minecraft:instant_health 1 10 true")
         (str "effect give " body " minecraft:saturation 1 10 true")]
        (into (for [[item n] inventory] (str "give " body " " item " " (or n 1))))
        (into (for [[effect seconds amp] effects]
                (str "effect give " body " " effect " " (or seconds 600) " " (or amp 0) " true")))
        (cond-> spawnpoint (conj (str "spawnpoint " body " " (xyz-str (abs-pos origin spawnpoint))))))))

(declare reply-pos)

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

(defn cleanup-commands
  "Leave the plot: its entities killed, the plot cleared to its floor and the body put back on it (a restarted body
  starts where it last stood, so no hut or bed of this case may remain), its inventory and effects cleared, the
  forceload removed."
  [grid origin body c]
  (let [{:keys [height floor]} (:plot c)]
    (-> [(kill-command grid origin height)]
        (into (clear-commands grid origin height floor))
        (into [(str "clear " body)
               (str "effect clear " body)
               (str "tp " body " " (xyz-str (abs-pos origin [1.5 0 1.5])) " 0 0")])
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

;; ------------------------------------------------------------------ after checks

(defn entities-selector
  "The @e[...] selector of an :entities check's [sel [a b]] args (positions plot-relative), limited to one entity for a probe."
  [origin [sel [a b]]]
  (let [[ax ay az] (abs-pos origin a)
        [bx by bz] (abs-pos origin b)]
    (str "@e[" sel ",x=" (min ax bx) ",y=" (min ay by) ",z=" (min az bz)
         ",dx=" (js/Math.abs (- bx ax)) ",dy=" (js/Math.abs (- by ay)) ",dz=" (js/Math.abs (- bz az)) "]")))

(defn after-command
  "The RCON command whose reply answers one :after check (positions plot-relative)."
  [origin body grid c [op & args]]
  (case op
    :block (str "execute if block " (xyz-str (abs-pos origin (first args))) " " (second args))
    :not-block (str "execute if block " (xyz-str (abs-pos origin (first args))) " " (second args))
    (:body-near :body-far) (str "data get entity " body " Pos")
    :item (str "execute if items entity " body " container.* " (first args))
    :entities (str "execute if entity " (entities-selector origin args))))

(defn entity-probe-command
  "The RCON command that dumps one entity matching a failed :entities check (nil for other checks)."
  [origin [op & args]]
  (when (= op :entities)
    (let [sel (entities-selector origin args)]
      (str "data get entity " (subs sel 0 (dec (count sel))) ",limit=1]"))))

(defn describe-entity-reply
  "Names the entity in a `data get entity` reply: its type and plot-relative position."
  [origin reply]
  (let [[_ id] (re-find #"id: \"minecraft:([a-z_]+)\"" reply)
        [_ x y z] (re-find #"Pos: \[(-?[\d.E-]+)d, (-?[\d.E-]+)d, (-?[\d.E-]+)d\]" reply)]
    (if-not (and id x)
      (str "stray entity: no data in reply: " reply)
      (let [[ox oy oz] origin]
        (str "stray entity " id " at plot " [(- (js/Number x) ox) (- (js/Number y) oy) (- (js/Number z) oz)])))))

(defn reply-count
  "The count an `execute if` reply reports: 0 for a failed test, 1 for a pass without a count, nil for any other reply."
  [reply]
  (cond
    (str/includes? reply "Test failed") 0
    (re-find #"Count: (\d+)" reply) (js/Number (second (re-find #"Count: (\d+)" reply)))
    (str/includes? reply "Test passed") 1))

(defn reply-pos
  "The [x y z] in a `data get entity <body> Pos` reply, or nil."
  [reply]
  (when-let [[_ x y z] (re-find #"\[(-?[\d.E-]+)d, (-?[\d.E-]+)d, (-?[\d.E-]+)d\]" reply)]
    [(js/Number x) (js/Number y) (js/Number z)]))

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
  (let [n (when (#{:block :not-block :item :entities} op) (reply-count reply))
        result (case op
                 (:block :not-block :item :entities)
                 (cond
                   (nil? n) [false (str "unrecognised reply: " reply)]
                   (= op :block) [(pos? n) reply]
                   (= op :not-block) [(zero? n) reply]
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
