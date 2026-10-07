(ns agent-tools.snapshot
  "A picture of what a body sees, plus a short text of what is in front. Read-only: it reads the body's view/pose.json
  and the world's chunk dumps and asks the body's /entities for what it perceives, so it never moves it. The picture is drawn by the JS
  software renderer (tools/view/render.mjs), which the launcher engine/tools/snapshot.mjs passes in as `render`;
  everything else (options, the online check, the camera, file names, the bounded folder, the summary) is here."
  (:require [clojure.string :as str]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            ["minecraft-data" :as minecraft-data]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def usage
  (str "snapshot.mjs <body> --world WORLD [--worlds DIR] [--workspace DIR] [--width 640] [--height 360]\n"
       "            [--yaw DEG] [--pitch DEG] [--look-at X,Y,Z] [--max-dist 64]\n"
       "Draws what the body sees now into <workspace>/snapshots/snap-<UTC time>.png (the newest " 20 " are kept) and\n"
       "prints EDN: :png (relative to the workspace), :facing, :crosshair (the block the centre of the view hits, its\n"
       "cell, distance and light), :entities (mobs/players the body sees, in the picture, nearest first, left/centre/right),\n"
       ":heard (noises it hears but does not see: name, direction, band) and :text.\n"
       "Default direction: where the body faces. --yaw/--pitch in degrees (yaw 0 north, 90 west, 180 south, 270 east;\n"
       "pitch +90 straight up), either alone; --look-at aims at the centre of block X,Y,Z (or X Y Z). Only the picture turns: the\n"
       "body never moves. Needs the body online (pose.json written in the last " 10 " s). Drawn from the chunks the\n"
       "body has seen; unloaded terrain shows as sky. Sizes 16..1920 x 16..1080, --max-dist 4..96 blocks."))

(def keep-count 20)
(def stale-ms 10000)
(def degrees (/ 180 js/Math.PI))

(defn- invalid [message] (data/fail :invalid-option (str message "\n" usage)))

(defn- number-in [values k lo hi integer?]
  (when-let [text (get values k)]
    (let [n (js/Number text)]
      (when-not (and (not (str/blank? text)) (js/Number.isFinite n) (<= lo n hi) (or (not integer?) (js/Number.isInteger n)))
        (throw (invalid (str "--" (name k) " must be " (if integer? "an integer " "a number ") lo ".." hi))))
      n)))

(defn- look-at [text]
  (when text
    (let [parts (str/split text #",")
          nums (map js/Number parts)]
      (when-not (and (= 3 (count parts)) (not-any? str/blank? parts) (every? js/Number.isFinite nums))
        (throw (invalid "--look-at must be X,Y,Z (three numbers)")))
      (vec nums))))

(defn- joined-negatives
  "`--pitch -30` as `--pitch=-30`: Node's parseArgs reads a value starting with a dash as a missing value. `--look-at X Y Z` as `--look-at=X,Y,Z`."
  [argv]
  (loop [[a b c d & more :as args] argv out []]
    (cond
      (empty? args) out
      (and (= "--look-at" a) (every? #(and (string? %) (re-matches #"-?[\d.]+" %)) [b c d]))
      (recur more (conj out (str a "=" b "," c "," d)))
      (and (#{"--width" "--height" "--yaw" "--pitch" "--look-at" "--max-dist"} a) (string? b) (re-matches #"-[\d.].*" b))
      (recur (drop 2 args) (conj out (str a "=" b)))
      :else (recur (rest args) (conj out a)))))

(defn options [argv]
  (let [{:keys [positionals values]}
        (map-tool/parse-options (joined-negatives argv) {:world {:type "string"} :worlds {:type "string"} :state {:type "string"}
                                      :workspace {:type "string"} :width {:type "string"} :height {:type "string"}
                                      :yaw {:type "string"} :pitch {:type "string"} :look-at {:type "string"}
                                      :max-dist {:type "string"}})
        [body & extra] positionals]
    (when-not (and (string? body) (re-matches #"[A-Za-z0-9_-]{1,64}" body) (empty? extra))
      (throw (invalid "give exactly one body name")))
    (when (and (:look-at values) (or (:yaw values) (:pitch values)))
      (throw (invalid "choose --look-at or --yaw/--pitch")))
    (let [ctx (data/context (select-keys values [:state :worlds :world]))]
      {:ctx ctx :body body
       :workspace (.resolve path (or (:workspace values) (.join path (:world-dir ctx) "agents" body)))
       :width (or (number-in values :width 16 1920 true) 640)
       :height (or (number-in values :height 16 1080 true) 360)
       :max-dist (or (number-in values :max-dist 4 96 false) 64)
       :yaw (number-in values :yaw -360 360 false)
       :pitch (number-in values :pitch -90 90 false)
       :look-at (look-at (:look-at values))})))

(defn look-angles
  "Mineflayer yaw/pitch (radians) from `eye` to the point [x y z]: yaw 0 looks at -z, pi/2 at -x; pitch > 0 up."
  [{:keys [x y z]} [tx ty tz]]
  (let [dx (- tx x) dy (- ty y) dz (- tz z)]
    {:yaw (js/Math.atan2 (- dx) (- dz))
     :pitch (js/Math.atan2 dy (js/Math.hypot dx dz))}))

(defn compass [yaw]
  (let [names ["north" "north-west" "west" "south-west" "south" "south-east" "east" "north-east"]
        eighth (js/Math.round (/ (* yaw degrees) 45))]
    (nth names (mod eighth 8))))

(defn camera
  "The yaw/pitch (radians) to draw from: the pose's, with the given degrees or --look-at target instead."
  [pose {:keys [yaw pitch look-at]}]
  (if look-at
    (look-angles (:eye pose) (mapv #(+ (js/Math.floor %) 0.5) look-at))
    {:yaw (if yaw (/ yaw degrees) (:yaw pose))
     :pitch (if pitch (/ pitch degrees) (:pitch pose))}))

(defn pose-problem
  "nil when `pose` is a fresh online pose of `world` with a position; else {:reason :message}."
  [pose world now]
  (let [finite? #(and (number? %) (js/Number.isFinite %))]
    (cond
      (nil? pose) {:reason :no-pose :message "the body has no view/pose.json yet; has it ever been started?"}
      (not= world (:world pose)) {:reason :no-pose :message (str "the pose is of world " (:world pose) ", not " world)}
      (not= "online" (:status pose)) {:reason :body-offline :message "the body is offline; start it first"}
      (not (and (finite? (:t pose)) (<= (- now (:t pose)) stale-ms)))
      {:reason :body-offline :message (str "the body's pose is older than " (/ stale-ms 1000) " s; the body is not running")}
      (not (and (every? #(finite? (get-in pose [:eye %])) [:x :y :z]) (finite? (:yaw pose)) (finite? (:pitch pose))))
      {:reason :no-pose :message "the pose has no position; the body never fully started"})))

(defn file-name [now]
  (str "snap-" (str/replace (.toISOString (js/Date. now)) #"[:.]" "-") ".png"))

(defn prune
  "The snapshot files among `names` beyond the newest `keep` (oldest first); other files are never listed."
  [names keep]
  (let [ours (sort (filter #(re-matches #"snap-\d{4}-\d\d-\d\dT\d\d-\d\d-\d\d-\d{3}Z\.png" %) names))]
    (vec (take (max 0 (- (count ours) keep)) ours))))

(defn- round1 [n] (/ (js/Math.round (* 10 n)) 10))
(defn- coords [xs] (str/join " " xs))

(def mc-version "26.1")

(defn- hostile-name?
  "Same rule as jobs.lib.cost.threat/hostile?: minecraft-data's entity type \"hostile\" (or category \"Hostile mobs\")."
  [mob-name]
  (let [e (some-> (minecraft-data mc-version) .-entitiesByName (aget mob-name))]
    (boolean (and e (or (= "hostile" (.-type e)) (= "Hostile mobs" (.-category e)))))))

(defn- entity-kind [type]
  (cond (= "player" type) "player"
        (hostile-name? type) "hostile"))

(defn perceived
  "{:entities :heard} from the body's /entities rows: the :seen rows as pose entities for the renderer, and the :heard
  rows (no position) as {:name :direction :band}. Remembered rows and the body itself are not drawn."
  [rows]
  {:entities (->> rows
                  (filter #(and (= :seen (:sense %)) (:pos %) (not (:self? %))))
                  (mapv (fn [{:keys [type id username pos]}]
                          (cond-> {:id id :name type :type (entity-kind type) :pos pos}
                            username (assoc :username username)))))
   :heard (->> rows
               (filter #(= :heard (:sense %)))
               (mapv (fn [{:keys [type direction band]}] {:name type :direction direction :band band})))})

(defn summary
  "{:facing :crosshair :entities :heard :text} from the pose, the camera drawn, the centre ray's hit, the entities drawn
  and the noises heard."
  [{:keys [pose camera center seen heard width]}]
  (let [yaw-deg (mod (js/Math.round (* (:yaw camera) degrees)) 360)
        facing {:yaw yaw-deg :pitch (js/Math.round (* (:pitch camera) degrees)) :compass (compass (:yaw camera))}
        crosshair (when center
                    (cond-> {:block (:name center) :at [(:x center) (:y center) (:z center)] :face (:face center)
                             :distance (round1 (:t center))}
                      (:light center) (assoc :light (:light center))))
        side #(cond (< % (/ width 3)) "left" (> % (/ (* 2 width) 3)) "right" :else "centre")
        entities (->> seen
                      (sort-by :dist)
                      (take 8)
                      (mapv (fn [{:keys [name kind px dist light]}]
                                 (cond-> {:name name :kind kind :distance dist :side (side px)}
                                   light (assoc :light light)))))
        eye (:eye pose)
        text (str "Facing " (:compass facing) " (yaw " (:yaw facing) ", pitch " (:pitch facing) ") from "
                  (coords (map #(round1 (get eye %)) [:x :y :z])) ". "
                  (if crosshair
                    (str "Crosshair: " (:block crosshair) " at " (coords (:at crosshair)) ", "
                         (.toFixed (:distance crosshair) 1) " blocks"
                         (when-let [l (:light crosshair)] (str ", light " (max (:sky l) (:block l))))
                         ".")
                    "Crosshair: nothing within reach of the loaded world (sky or unloaded).")
                  " "
                  (if (seq entities)
                    (str "In view: " (str/join ", " (map #(str (:name %) " " (:distance %) " (" (:side %) ")") entities)) ".")
                    "No mobs or players in view.")
                  (when (seq heard)
                    (str " Heard: " (str/join ", " (map #(str (:name %) " " (name (:direction %)) " (" (name (:band %)) ")") heard)) ".")))]
    {:facing facing :crosshair crosshair :entities entities :heard (vec heard) :text text}))

(def max-pose-bytes 1048576)

(defn read-pose
  "The body's pose.json as a map; nil when missing or over 1 MB."
  [ctx body]
  (let [file (.join path (:world-dir ctx) "agents" body "view" "pose.json")]
    (when (and (.existsSync fs file) (<= (.-size (.statSync fs file)) max-pose-bytes))
      (js->clj (js/JSON.parse (.readFileSync fs file "utf8")) :keywordize-keys true))))

(defn- symlink?
  "True when `file` is itself a symbolic link (never followed); false when missing."
  [file]
  (boolean (some-> (.lstatSync fs file #js {:throwIfNoEntry false}) (.isSymbolicLink))))

(defn- real-target
  "`file` with its deepest existing ancestor resolved through symbolic links (the file itself need not exist)."
  [file]
  (loop [rest-parts [] cur file]
    (let [real (try (.realpathSync fs cur) (catch :default _ nil))
          parent (.dirname path cur)]
      (cond
        real (apply (.-join path) real rest-parts)
        (= parent cur) file
        :else (recur (cons (.basename path cur) rest-parts) parent)))))

(def unsafe-dir
  {:reason :unsafe-snapshots-dir
   :message "the workspace or its snapshots/ folder is a symbolic link, or a folder on its path changed; refusing to write or prune through it"})

(defn- unsafe-dir-problem
  "unsafe-dir when the workspace or snapshots/ is a link, or when snapshots/ no longer resolves to `expected` (its real path when the run began)."
  [workspace expected]
  (when (or (some symlink? [workspace (.join path workspace "snapshots")])
            (not= expected (real-target (.join path workspace "snapshots"))))
    unsafe-dir))

(defn- write-snapshot!
  "Writes `png` as a new file in the verified real folder `dir` (never following a link at the file) and prunes old ones."
  [dir name png]
  (let [fd (.openSync fs (.join path dir name)
                      (bit-or (.. fs -constants -O_WRONLY) (.. fs -constants -O_CREAT) (.. fs -constants -O_EXCL) (.. fs -constants -O_NOFOLLOW)))]
    (try (.writeSync fs fd png) (finally (.closeSync fs fd))))
  (doseq [old (prune (array-seq (.readdirSync fs dir)) keep-count)]
    (.rmSync fs (.join path dir old) #js {:force true})))

(defn- center-of [center]
  (when center (js->clj center :keywordize-keys true)))

(declare execute-with!)

(def entities-timeout-ms 3000)
(def entities-max-bytes (+ (* 4 1024 1024) 4096))

(defn fetch-entities
  "A promise of the body's /entities snapshot map, asked over its control socket."
  [ctx body]
  (-> (http/request {:socket-path (.join path (:world-dir ctx) "agents" body "engine" "control.sock") :path "/entities"
                     :label "entities" :timeout-ms entities-timeout-ms :max-bytes entities-max-bytes})
      (.then (fn [{:keys [status text]}]
               (let [snapshot (when (= 200 status) (data/read-edn text))]
                 (if (and (map? snapshot) (:ok snapshot))
                   snapshot
                   (throw (js/Error. (str "the body's /entities answered " status)))))))))

(defn execute!
  "Renders and writes one snapshot; `render` is renderView of tools/view/render.mjs. `entities-fn` (default: ask the
  body) returns a promise of its /entities snapshot; what the body perceives, not the live entity list, is drawn, and
  with no answer nothing is drawn and :entities-error says why. Resolves to the result map."
  ([opts render now] (execute! opts render now nil))
  ([{:keys [ctx body] :as opts} render now entities-fn]
   (let [pose (read-pose ctx body)]
     (if (pose-problem pose (:world ctx) now)
       (execute-with! opts render now pose {})
       (-> (js/Promise.resolve ((or entities-fn #(fetch-entities ctx body))))
           (.then (fn [snapshot] (perceived (:entities snapshot))))
           (.catch (fn [error] {:entities [] :heard [] :entities-error (str (or (some-> error .-message) error))}))
           (.then #(execute-with! opts render now pose %)))))))

(defn execute-with!
  [{:keys [ctx body workspace width height max-dist] :as opts} render now pose {:keys [entities heard entities-error]}]
  (let [pose (when pose (assoc pose :entities entities))
        expected (real-target (.join path workspace "snapshots"))]
    (if-let [problem (or (pose-problem pose (:world ctx) now) (unsafe-dir-problem workspace expected))]
      (js/Promise.resolve (merge {:ok false :body body} problem))
      (let [cam (camera pose opts)]
        (-> (js/Promise.resolve
             (render #js {:world (:world ctx) :agentName body :pose (clj->js pose)
                          :stateDir #js {:worldsDir (.dirname path (:world-dir ctx))}
                          :width width :height height :maxDist max-dist
                          :override #js {:yaw (:yaw cam) :pitch (:pitch cam)} :aim true}))
            (.then (fn [out]
                     (if-let [problem (unsafe-dir-problem workspace expected)]
                       (merge {:ok false :body body} problem)
                       (let [dir (.join path workspace "snapshots")
                             _ (.mkdirSync fs dir #js {:recursive true})
                             real (.realpathSync fs dir)
                             name (file-name now)]
                         (if (or (not= real expected) (symlink? dir))
                           (merge {:ok false :body body} unsafe-dir)
                           (do
                             (write-snapshot! real name (unchecked-get out "png"))
                             (merge {:ok true :body body :png (.relative path workspace (.join path dir name)) :size [width height]}
                                    (summary {:pose pose :camera cam :width width :height height
                                              :center (center-of (unchecked-get out "center"))
                                              :heard heard
                                              :seen (js->clj (unchecked-get out "seen") :keywordize-keys true)})
                                    (when entities-error {:entities-error entities-error})
                                    {:eye (mapv #(round1 (get-in pose [:eye %])) [:x :y :z])
                                     :columns (unchecked-get out "columns") :render-ms (js/Math.round (unchecked-get out "ms"))}))))))))))))

(defn main!
  "Exit code 0 drawn, 1 refused (body offline, no pose), 2 bad arguments or a failure."
  [argv render]
  (-> (js/Promise.resolve nil)
      (.then (fn [_] (execute! (options (vec argv)) render (js/Date.now))))
      (.then (fn [result] (.write (.-stdout js/process) (str (data/write-edn result) "\n")) (if (:ok result) 0 1)))
      (.catch (fn [error] (.write (.-stdout js/process) (str (data/write-edn (map-tool/error-result error)) "\n")) 2))))
