(ns world-test.kit-trial
  "tools/kit-trial.mjs: repeatable iron-kit trials on fresh bodies. A run is one spot and one new body name (no playerdata, no
  memory), set up by `start`, judged from the body's event log by `judge`. Terrain changes are never undone: the next run is a
  new body name on the next spot.
  Commands: spots | plan SPOT [--n N] | start SPOT [--n N] --build DIR [--go] [--allow-time] | judge RUN [--world W]
  Pure parts: spots, run-name, steps, milestones, report, clean."
  (:require ["child_process" :as cp]
            ["fs" :as fs]
            ["path" :as path]
            [cljs.reader :as reader]
            [clojure.string :as str]))

(def usage
  (str "usage: node tools/kit-trial.mjs spots | plan SPOT [--n N] | start SPOT [--n N] --build DIR [--go] [--allow-time] | judge RUN [--world test]\n"
       "plan prints the steps start would run (RCON, files, body start, the one goal job); start runs them only with --go (without it, the same as plan).\n"
       "judge reads worlds/<world>/kit-trials/RUN/run.edn and the body's event logs, prints the report EDN and the milestone table; exit 0 clean, 1 not clean."))

(def ms-per-day
  "One in-game day of wall time at 20 ticks a second; the days of a run count from its start."
  1200000)

(def spots
  "Fixed start spots. :x :z nil = not chosen yet (start refuses it): pick a place whose terrain fits :want and set it here."
  [{:id :jungle :label "jungle canopy" :x -400 :z -315 :want "trees at hand, stone within a few hundred blocks"}
   {:id :oak :label "oak forest" :x 674 :z -408 :want "oak/birch trees within 48 blocks"}
   {:id :birch :label "birch forest" :x -518 :z 432 :want "birch trees within 48 blocks"}
   {:id :hills :label "hills with exposed stone" :x nil :z nil :want "bare stone within 32 blocks, a few trees"}
   {:id :water :label "beside water" :x nil :z nil :want "a lake or river within 32 blocks, trees near"}])

(def goal-spec "(jobs.items.get-kit {})")

(def kit-pieces
  ["iron_pickaxe" "iron_sword" "iron_axe" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(def max-days 3)
(def stall-ms 300000)

(defn run-name
  "The body name of run n on spot (letters and digits, at most 16): a name never played on the server before."
  [spot n]
  (str "Kit" (str/capitalize (name (:id spot))) (when (< n 10) "0") n))

(defn run-dir [world run] (str "worlds/" world "/kit-trials/" run))

(defn steps
  "The setup of a run as data, in order: [:whitelist name] [:file path text] [:body-start name] [:rcon text] [:submit name spec] [:record path m].
  Needs the spot's coordinates; :time? adds `time set 1000` (server-wide, only under --allow-time)."
  [{:keys [spot n world build time? now] :or {world "test"}}]
  (let [body (run-name spot n)
        dir (run-dir world body)
        {:keys [x z]} spot]
    (when-not (and x z) (throw (js/Error. (str "spot " (name (:id spot)) " has no coordinates yet"))))
    (vec (concat
          [[:whitelist body]
           [:file (str "worlds/" world "/agents/" body "/config.json")
            (js/JSON.stringify (clj->js {:username body :character {:name body :note "iron-kit trial body"}}) nil 2)]
           [:workspace body world]
           [:body-start body {:world world :build build}]]
          (when time? [[:rcon "time set 1000"]])
          [[:rcon (str "spreadplayers " x " " z " 0 24 false " body)]
           [:rcon (str "gamemode survival " body)]
           [:rcon (str "clear " body)]
           [:rcon (str "effect give " body " minecraft:instant_health 1 5 true")]
           [:record (str dir "/run.edn") {:run body :spot (:id spot) :n n :world world :build build :started-ms now :goal goal-spec}]
           [:submit body goal-spec]]))))

;; ---- judging

(defn- ms [e] (:time-ms e))

(defn- smelted-count [v]
  (cond (map? v) (reduce + (vals v)) (number? v) v :else 0))

(defn- child-done
  "The ending data of the child in slot, when this event is that child ending done."
  [{:keys [kind data]} slot]
  (when (and (= :child_ended kind) (= slot (:slot data)) (= :done (get-in data [:ending :kind])))
    (get-in data [:ending :data])))

(defn- piece-of
  "The kit piece or milestone an event proves, or nil."
  [{:keys [kind data] :as e}]
  (let [item (:item data)
        crafted (:item (child-done e :craft))]
    (cond
      (and (= :picked-up kind) (str/ends-with? (str item) "_log")) :logs
      (and (= :picked-up kind) (= "cobblestone" item)) :cobblestone
      (= "crafting_table" crafted) :table
      (= "wooden_pickaxe" crafted) :wooden-pickaxe
      (= "stone_pickaxe" crafted) :stone-pickaxe
      (= "furnace" crafted) :furnace
      (= :smelt.furnace kind) :furnace
      (pos? (smelted-count (:smelted (child-done e :smelt)))) :first-ingot
      (and (= :kit.progress kind) (some #{item} kit-pieces)) (keyword item)
      :else nil)))

(defn milestones
  "events (time-ordered, from the run's start) -> {milestone ms-of-first-event}. Kit pieces are :iron_pickaxe ...; :kit-done from the get-kit job completing."
  [events]
  (reduce (fn [m e]
            (let [k (or (piece-of e) (when (and (= :completed (:kind e)) (= "jobs.items.get-kit" (get-in e [:data :name]))) :kit-done))]
              (cond-> m (and k (not (m k))) (assoc k (ms e)))))
          {} events))

(defn deaths [events]
  (mapv (fn [e] {:at-ms (ms e) :cause (get-in e [:data :cause])}) (filter #(= :died (:kind %)) events)))

(def progress-kinds #{:picked-up :kit.progress :stair.step :completed})

(defn progress?
  "A progress kind, or a child ending that reports a dig (a long dig inside one child emits nothing else) or a craft or smelt."
  [e]
  (or (contains? progress-kinds (:kind e))
      (some? (child-done e :craft))
      (some? (child-done e :smelt))
      (and (= :child_ended (:kind e)) (boolean? (get-in e [:data :ending :data :dug])))))

(defn stalls
  "Gaps over stall-ms between progress events, while the goal job is listed: [{:from-ms :to-ms}] up to end-ms."
  [events start-ms end-ms]
  (let [times (concat [start-ms] (map ms (filter progress? events)) [end-ms])]
    (vec (for [[a b] (partition 2 1 times) :when (> (- b a) stall-ms)] {:from-ms a :to-ms b}))))

(defn give-ups
  "Warn events that end a job or give up, with whether they state a reason: [{:kind :why? :text}]."
  [events]
  (vec (for [e events
             :when (and (= :warn (:level e)) (re-find #"gave-up|stopped|declined" (name (:kind e))))]
         {:kind (:kind e) :at-ms (ms e)
          :why? (boolean (or (get-in e [:data :reason]) (get-in e [:data :why]) (get-in e [:data :text]) (:message e)))
          :text (:message e)})))

(defn goal-job-of
  "The id of the first top-level get-kit job queued."
  [events]
  (some #(when (and (= :queued (:kind %)) (= "jobs.items.get-kit" (get-in % [:data :name]))) (first (get-in % [:context :chain]))) events))

(defn interventions
  "Jobs queued at top level that are not the goal job: nothing but the goal and the body's own reflexes act."
  [events goal-job]
  (vec (for [e events
             :let [chain (get-in e [:context :chain])]
             :when (and (= :queued (:kind e)) (= :job (:source e)) (not= goal-job (first chain)) (= :agent (get-in e [:data :by])))]
         {:kind (:kind e) :at-ms (ms e) :job (first chain)})))

(defn crashes
  "Body restarts: generation ids seen after the first."
  [events]
  (max 0 (dec (count (distinct (keep :generation-id events))))))

(defn report
  "run (the run.edn map), events from its start, end-ms -> the report map."
  [{:keys [started-ms goal-job]} events end-ms]
  (let [goal-job (or goal-job (goal-job-of events))
        m (milestones events)
        days #(when % (/ (- % started-ms) ms-per-day))]
    {:milestones (into (sorted-map) (map (fn [[k t]] [k {:ms (- t started-ms) :days (days t)}])) m)
     :deaths (deaths events)
     :stalls (stalls events started-ms end-ms)
     :give-ups (give-ups events)
     :interventions (interventions events goal-job)
     :crashes (crashes events)
     :kit-days (days (:kit-done m))}))

(defn clean
  "The clean criterion of a report -> {:clean? bool :failed [why ...] :review [what only a person can check]}: the kit done within max-days
  in-game days, no death, no intervention, no crash, every give-up states its reason."
  [{:keys [kit-days deaths interventions crashes give-ups]}]
  (let [failed (cond-> []
                 (nil? kit-days) (conj :kit-not-done)
                 (and kit-days (> kit-days max-days)) (conj :kit-too-slow)
                 (seq deaths) (conj :death)
                 (seq interventions) (conj :intervention)
                 (pos? crashes) (conj :crash)
                 (some (complement :why?) give-ups) (conj :give-up-without-reason))]
    {:clean? (empty? failed) :failed failed
     :review (cond-> [] (seq deaths) (conj "a death counts only when a system bug caused it: read its cause and job")
                        (seq give-ups) (conj "after each give-up the next call must recover"))}))

;; ---- node edge

(defn- repo [] (or js/process.env.WORLD_TEST_REPO "."))

(defn- parse-args [argv]
  (loop [[a & more :as args] argv o {:pos []}]
    (cond
      (empty? args) o
      (= a "--n") (recur (rest more) (assoc o :n (js/parseInt (first more))))
      (= a "--build") (recur (rest more) (assoc o :build (first more)))
      (= a "--world") (recur (rest more) (assoc o :world (first more)))
      (= a "--go") (recur more (assoc o :go true))
      (= a "--allow-time") (recur more (assoc o :time? true))
      :else (recur more (update o :pos conj a)))))

(defn- read-events
  "The body's event logs (oldest rotation first) from start-ms on."
  [world body start-ms]
  (let [dir (path/join (repo) "worlds" world "agents" body "engine")
        files (->> (array-seq (fs/readdirSync dir)) (filter #(re-matches #"events\.edn(\.\d+)?" %))
                   (sort-by #(or (some-> (re-find #"\.(\d+)$" %) second js/parseInt) 0) >))]
    (->> files
         (mapcat #(str/split-lines (str (fs/readFileSync (path/join dir %) "utf8"))))
         (keep #(try (reader/read-string %) (catch :default _ nil)))
         (filter #(and (:time-ms %) (>= (:time-ms %) start-ms)))
         (sort-by :time-ms)
         vec)))

(defn- run-node! [script args]
  (cp/spawnSync js/process.execPath (clj->js (into [(path/join (repo) script)] args)) #js {:stdio "inherit" :cwd (repo)}))

(defn- exec-step!
  "Run one step of `steps`. Not run by the tests: a live server and a built body are needed."
  [[kind & a]]
  (case kind
    :whitelist (run-node! "tools/rcon.mjs" ["whitelist" (first a)])
    :file (let [[f text] a, p (path/join (repo) f)]
            (fs/mkdirSync (path/dirname p) #js {:recursive true})
            (fs/writeFileSync p text))
    :workspace (let [[body world] a]
                 (run-node! "engine/tools/workspace.mjs" [(path/join "worlds" world "agents" body) "--body" body "--world" world "--adopt-existing"]))
    :body-start (let [[body {:keys [world build]}] a
                      child (cp/spawn (path/join (repo) "tools/res-slot")
                                      #js ["body" "--" js/process.execPath "--max-old-space-size=192" "--heap-growing-percent=100" "out/body-release.cjs" "--agent" body "--world" world
                                           "--scenario" (path/join build "engine/scenarios/survival.edn")
                                           "--worlds" (path/join (repo) "worlds") "--blueprints" (path/join (repo) "blueprints") "--fresh"]
                                      #js {:cwd (path/join build "engine") :detached true :stdio "ignore"
                                           :env (js/Object.assign #js {} js/process.env #js {:NODE_COMPILE_CACHE (path/join (repo) "engine/out/compile-cache")})})]
                  (.unref child))
    :rcon (run-node! "tools/rcon-raw.mjs" (str/split (first a) #" "))
    :record (let [[f m] a, p (path/join (repo) f)]
              (fs/mkdirSync (path/dirname p) #js {:recursive true})
              (fs/writeFileSync p (pr-str m)))
    :submit (let [[body spec] a]
              (run-node! "engine/tools/jobs.mjs" [body "--world" "test" "submit" spec]))))

(defn- table [{:keys [milestones]}]
  (str/join "\n" (for [[k {:keys [ms days]}] milestones]
                   (str (name k) "\t" (.toFixed (/ ms 60000) 1) " min\t" (.toFixed days 2) " days"))))

(defn main
  "argv (array) -> promise of the exit code."
  [argv]
  (let [{[cmd & pos] :pos :keys [n go build world] :or {n 1 world "test"} :as o} (parse-args (vec (array-seq argv)))
        spot-of #(first (filter (fn [s] (= (first pos) (name (:id s)))) spots))]
    (js/Promise.resolve
     (case cmd
       "spots" (do (doseq [s spots] (js/console.log (pr-str s))) 0)
       ("plan" "start") (let [spot (spot-of)]
                          (if-not (and spot (or build (not go)))
                            (do (js/console.error usage) 2)
                            (try
                              (let [ss (steps {:spot spot :n n :world world :build (or build "<build dir>") :time? (:time? o) :now (js/Date.now)})]
                                (doseq [s ss] (js/console.log (pr-str s)))
                                (when (and go (= "start" cmd))
                                  (doseq [s ss] (exec-step! s)))
                                0)
                              (catch :default e (js/console.error (.-message e)) 2))))
       "judge" (let [run (first pos)
                     f (path/join (repo) (run-dir world run) "run.edn")
                     r (reader/read-string (str (fs/readFileSync f "utf8")))
                     now (js/Date.now)
                     rep (report r (read-events world run (:started-ms r)) now)
                     c (clean rep)]
                 (js/console.log (pr-str (assoc rep :verdict c)))
                 (js/console.log (table rep))
                 (if (:clean? c) 0 1))
       (do (js/console.error usage) 2)))))
