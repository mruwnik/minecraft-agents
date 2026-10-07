(ns engine.path.courses
  "The live tester's courses as fixture snapshots (engine.path.fixture): its server commands (setblock / fill) are replayed
  into fixture entries on top of the lane as the tester prepares it, so unit tests and live runs share their terrain.
  The course data is courses.edn."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            ["fs" :as fs]
            [engine.path.blocks :as blocks]
            [engine.path.fixture :as fx :refer [UNLOADED require-here]]))


(def courses-file "test/engine/path/courses.edn")

(def courses-vector
  "[[name {:family :start :to :cmds (:walls)}] ...] in the file's order"
  (delay (reader/read-string (fs/readFileSync courses-file "utf8"))))

(def courses (delay (into {} @courses-vector)))

(def BLOCK-RE #"^(?:minecraft:)?([a-z0-9_]+)(?:\[([^\]]*)\])?$")
;; commands that change entities or the player, not terrain
(def NOT-TERRAIN #{"summon" "kill" "say" "tp" "effect"})
;; names the live scripts use that this version's registry renamed
(def RENAMED {"chain" "iron_chain"})

(defn prop-value [v]
  (cond (= v "true") true
        (= v "false") false
        (re-matches #"-?\d+" v) (js/Number v)
        :else v))

(defn parse-block
  "[name] or [name props] of text like minecraft:cocoa[age=2,facing=north]."
  [text]
  (let [[_ written props] (or (re-matches BLOCK-RE text) (throw (js/Error. (str "cannot parse block '" text "'"))))
        name (get RENAMED written written)]
    (if (str/blank? props)
      [name]
      [name (into {} (map (fn [p] (let [[k v] (str/split p #"=")] [k (prop-value v)]))) (str/split props #","))])))

(defn coords [tokens what]
  (when (some #(re-find #"^[~^]" %) tokens) (throw (js/Error. (str what ": relative coordinates are not supported"))))
  (let [n (mapv js/Number tokens)]
    (when (some js/Number.isNaN n) (throw (js/Error. (str what ": bad coordinates " (str/join " " tokens)))))
    n))

(defn check-mode! [what mode command]
  (when-not (contains? #{nil "replace" "destroy"} mode)
    (throw (js/Error. (str what " mode '" mode "' is not supported: " command)))))

(defn entries-for
  "one command -> fixture fill entries (a setblock is a one-cell fill, so order between the two is kept)"
  [command]
  (let [[verb & args] (str/split (str/trim command) #"\s+")
        args (vec args)]
    (cond
      (NOT-TERRAIN verb) []
      (= verb "setblock")
      (let [[x y z] (coords (subvec args 0 3) "setblock")]
        (check-mode! "setblock" (get args 4) command)
        [(into [x y z x y z] (parse-block (get args 3)))])
      (= verb "fill")
      (let [[x0 y0 z0 x1 y1 z1] (coords (subvec args 0 6) "fill")
            mode (get args 7)]
        ;; replace without a filter replaces everything: the same as a plain fill
        (when (and (= mode "replace") (> (count args) 8))
          (throw (js/Error. (str "fill replace with a filter is not supported: " command))))
        (check-mode! "fill" mode command)
        [(into [(min x0 x1) (min y0 y1) (min z0 z1) (max x0 x1) (max y0 y1) (max z0 z1)] (parse-block (get args 6)))])
      :else (throw (js/Error. (str "unsupported command '" verb "': " command))))))

(defn apply-commands
  "entries: fixture :fill entries so far; returns them plus the commands' entries, in order"
  [entries cmds]
  (into (vec entries) (mapcat entries-for) cmds))

;; ---- what the server does after the commands: attached blocks whose support is gone drop (their neighbour update), so a
;; replay that only copies the commands would keep ladders, vines and torches the live world no longer has ----

(def OFFSETS {"east" [1 0 0] "west" [-1 0 0] "south" [0 0 1] "north" [0 0 -1] "up" [0 1 0] "down" [0 -1 0]})
(def OPPOSITE {"east" "west" "west" "east" "south" "north" "north" "south" "up" "down" "down" "up"})
(def FACES ["north" "east" "south" "west" "up"])
(def AIR (delay (fx/state-id "air")))

(def info-of (memoize fx/block-at-id))

(defn touched
  "the box [x0 y0 z0 x1 y1 z1] the commands touched, one cell wider on every side"
  [entries]
  (reduce (fn [[a b c d e f] [x0 y0 z0 x1 y1 z1]]
            [(min a (dec x0)) (min b (dec y0)) (min c (dec z0)) (max d (inc x1)) (max e (inc y1)) (max f (inc z1))])
          [js/Infinity js/Infinity js/Infinity (- js/Infinity) (- js/Infinity) (- js/Infinity)]
          entries))

(defn sturdy-at?
  "a full cube a face can be attached to (the planner's collision table: a whole block, no gaps)"
  [snapshot table x y z]
  (let [id (.stateAt ^js snapshot x y z)]
    (and (not= id UNLOADED) (fx/full-cube? table id))))

(defn settled
  "what keeps the block at (x, y, z) in place: a vine keeps the faces that still have a block (or a vine above with the
  face), the rest need their one support. Returns the state id to leave there, or nil when the block is not an attached one."
  [snapshot table x y z id]
  (let [{:keys [name props]} (info-of id)
        at (fn [dir] (let [[dx dy dz] (OFFSETS dir)] [(+ x dx) (+ y dy) (+ z dz)]))
        sturdy? (fn [dir] (let [[ax ay az] (at dir)] (sturdy-at? snapshot table ax ay az)))
        keep (fn [ok] (if ok id @AIR))]
    (cond
      (or (= name "ladder") (re-find #"wall_torch$" name)) (keep (sturdy? (OPPOSITE (:facing props))))
      (re-find #"(^|_)torch$" name) (keep (sturdy? "down"))
      (or (= name "lever") (re-find #"_button$" name))
      (keep (sturdy? (case (:face props) "floor" "down" "ceiling" "up" (OPPOSITE (:facing props)))))
      (= name "cocoa") (let [[ax ay az] (at (:facing props))
                             log (.stateAt ^js snapshot ax ay az)]
                         (keep (and (not= log UNLOADED) (= "jungle_log" (:name (info-of log))))))
      (= name "vine")
      (let [[ux uy uz] (at "up")
            above-id (.stateAt ^js snapshot ux uy uz)
            above (if (= above-id UNLOADED) {:name "" :props {}} (info-of above-id))
            faces (into {} (map (fn [f] [f (boolean (and (= true (get props (keyword f)))
                                                         (or (sturdy? f) (and (= "vine" (:name above)) (= true (get (:props above) (keyword f)))))))])
                              FACES))]
        (if (some faces FACES)
          (fx/state-id "vine" (update-keys faces keyword))
          @AIR)))))

(defn drop-unsupported!
  "repeat until nothing more drops: a vine may have been hanging from one that just went"
  [snapshot [x0 y0 z0 x1 y1 z1]]
  (let [table (blocks/default-state-table)]
    (loop []
      (let [changed (volatile! false)]
        (doseq [y (range y1 (dec y0) -1) z (range z0 (inc z1)) x (range x0 (inc x1))]
          (let [id (.stateAt ^js snapshot x y z)]
            (when-not (or (= id UNLOADED) (= id @AIR))
              (let [now (settled snapshot table x y z id)]
                (when (and (some? now) (not= now id))
                  (.setState ^js snapshot x y z now)
                  (vreset! changed true))))))
        (when @changed (recur))))))

;; ---- water, as the server settles it after the commands (vanilla rules, simplified): a source falls into air below it as
;; falling water (level 8), a flowing cell or a source resting on something spreads to its four sides one level weaker (a
;; falling cell starts at level 1) as far as level 7, and a source over soul sand or magma becomes a bubble column up through
;; the sources above. The horizontal spread has no pull toward nearby holes (a source over a hole spreads to all four sides);
;; air is the only block water replaces; the box bounds it. ----

(def FALLING 8)
(def MAX-FLOW 7)
(def FLOW-SIDES [[1 0] [-1 0] [0 1] [0 -1]])

(defn water-id? [id] (and (not= id UNLOADED) (= "water" (:name (info-of id)))))
(defn water-level [id] (js/Number (:level (:props (info-of id)))))

(defn flow-water! [snapshot [x0 y0 z0 x1 y1 z1]]
  (let [in-box? (fn [x y z] (and (<= x0 x x1) (<= y0 y y1) (<= z0 z z1)))
        levels (atom {})
        queue (array)
        put! (fn [x y z level]
               (swap! levels assoc [x y z] level)
               (.setState ^js snapshot x y z (fx/state-id "water" {:level level}))
               (.push queue [x y z]))
        ;; a cell water can enter at `level`: air, or flowing water that is weaker (a higher number); never a source or falling water
        takes? (fn [x y z level]
                 (and (in-box? x y z)
                      (or (= (.stateAt ^js snapshot x y z) @AIR)
                          (let [have (get @levels [x y z])]
                            (and (some? have) (not= have 0) (not= have FALLING)
                                 (or (= level FALLING) (> have level)))))))]
    (doseq [y (range y0 (inc y1)) z (range z0 (inc z1)) x (range x0 (inc x1))]
      (let [id (.stateAt ^js snapshot x y z)]
        (when (water-id? id)
          (swap! levels assoc [x y z] (water-level id))
          (.push queue [x y z]))))
    (loop [head 0]
      (when (< head (.-length queue))
        (let [[x y z] (aget queue head)
              level (get @levels [x y z])
              below (.stateAt ^js snapshot x (dec y) z)
              hole? (or (= below @AIR) (water-id? below))
              next-level (inc (if (= level FALLING) 0 level))]
          ;; over air or water the cell falls into it; a flowing cell over such a hole only falls, a source spreads as well
          (when (takes? x (dec y) z FALLING) (put! x (dec y) z FALLING))
          (when-not (or (and (not= level 0) hole?) (> next-level MAX-FLOW))
            (doseq [[dx dz] FLOW-SIDES]
              (when (takes? (+ x dx) y (+ z dz) next-level) (put! (+ x dx) y (+ z dz) next-level))))
          (recur (inc head)))))))

(defn bubble-columns!
  "soul sand lifts (drag=false), magma drags (drag=true), through the water sources above it"
  [snapshot [x0 y0 z0 x1 y1 z1]]
  (doseq [z (range z0 (inc z1)) x (range x0 (inc x1)) y (range y0 (inc y1))]
    (let [id (.stateAt ^js snapshot x y z)
          name (when-not (= id UNLOADED) (:name (info-of id)))]
      (when (contains? #{"soul_sand" "magma_block"} name)
        (let [bubble (fx/state-id "bubble_column" {:drag (= name "magma_block")})]
          (doseq [k (range (inc y) (inc y1))
                  :let [above (.stateAt ^js snapshot x k z)]
                  :while (and (water-id? above) (zero? (water-level above)))]
            (.setState ^js snapshot x k z bubble)))))))

(defn fill [x0 y0 z0 x1 y1 z1 name] [x0 y0 z0 x1 y1 z1 name])

;; the lanes as the live scripts clear and floor them: stone floor at y160, glass walls; everything else is air
(def LANES
  {:tricky (fn [_] [(fill 2853 160 3208 2907 160 3224 "stone")
                    (fill 2853 161 3208 2907 170 3208 "glass") (fill 2853 161 3224 2907 170 3224 "glass")
                    (fill 2853 161 3209 2853 170 3223 "glass") (fill 2907 161 3209 2907 170 3223 "glass")])
   :bamboo (fn [{:keys [walls]}]
             (concat [(fill 2853 160 3208 2907 160 3224 "stone")]
                     (when-not (false? walls)
                       [(fill 2853 161 3208 2907 167 3208 "glass") (fill 2853 161 3224 2907 167 3224 "glass")])
                     [(fill 2853 161 3209 2853 167 3223 "glass") (fill 2907 161 3209 2907 167 3223 "glass")]))
   :hazards (fn [_] [(fill 2854 160 3206 2906 160 3226 "stone")
                     (fill 2853 161 3213 2907 163 3213 "glass") (fill 2853 161 3219 2907 163 3219 "glass")])})

(defn course-names [] (mapv first @courses-vector))

(defn lane-snapshot
  "a lane of the given family (see LANES) with the commands replayed into it: for ad-hoc variations of a course"
  ([family cmds] (lane-snapshot family cmds {}))
  ([family cmds options]
   (let [added (vec (mapcat entries-for cmds))
         snapshot (fx/fixture-snapshot {:fill (into (vec ((LANES (keyword family)) options)) added)})]
     (when (seq added)
       (let [[a b c d e f :as box] (touched added)]
         (drop-unsupported! snapshot box)
         (flow-water! snapshot [(- a MAX-FLOW) b (- c MAX-FLOW) (+ d MAX-FLOW) e (+ f MAX-FLOW)])
         (bubble-columns! snapshot box)))
     snapshot)))

(defn course-snapshot
  "{:snapshot :from :goal}: the course's terrain, the start cell (with the exact position as :px, :pz) and the go-to goal
  (a 'near' goal of range 1, the job's default)"
  [name]
  (let [course (or (get @courses name) (throw (js/Error. (str "unknown course " name))))
        {:keys [start to]} course
        floor (comp int js/Math.floor)]
    {:snapshot (lane-snapshot (:family course) (:cmds course) course)
     :from {:x (floor (:x start)) :y (:y start) :z (floor (:z start)) :px (:x start) :pz (:z start)}
     :goal {:kind "near" :x (floor (:x to)) :y (:y to) :z (floor (:z to)) :range 1}}))
