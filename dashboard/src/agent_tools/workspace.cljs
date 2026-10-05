(ns agent-tools.workspace
  "Workspace generation and binding policy; Node launchers only load tools."
  (:require [clojure.string :as str]
            [agent-tools.map :as map-tool]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:url" :refer [pathToFileURL]]))

(def commands ["observe" "jobs" "triggers" "say" "entities" "drive" "world"
               "map" "plans" "blueprints" "world-changes" "time" "snapshot"])
(def body-tools #{"observe" "jobs" "triggers" "say" "entities" "drive" "world" "snapshot"})
(def format-id :minecraft-agent-workspace/v1)
(def wrapper-marker "// Generated Minecraft agent workspace tool v1\n")
(def usage "usage: node engine/tools/workspace.mjs <directory> --body <body> --world <world> [--worlds <directory>] [--update-tools] [--adopt-existing]\nCreate an agent workspace for an existing or future body; does not start it.\nReruns preserve AGENTS.md, briefing.md and notes/. --update-tools refreshes generated wrappers only; bindings cannot change.\n--adopt-existing requires a canonical body folder and matching config.json; it preserves existing documents and runtime files.")
(defn fail! [message] (throw (js/Error. message)))
(defn file-exists? [file] (try (.lstatSync fs file) true (catch :default e (if (= "ENOENT" (.-code e)) false (throw e)))))
(defn regular! [file directory?]
  (when (file-exists? file)
    (let [stat (.lstatSync fs file)]
      (when-not (if directory? (.isDirectory stat) (.isFile stat))
        (fail! (str "refusing non-" (if directory? "directory " "file ") file))))))
(defn context! [file]
  (regular! file false)
  (let [ctx (map-tool/read-edn (.readFileSync fs file "utf8"))]
    (when-not (and (map? ctx) (= format-id (:format ctx))
                   (string? (:body ctx)) (re-matches #"[A-Za-z0-9_-]{1,40}" (:body ctx))
                   (string? (:world ctx)) (re-matches #"[A-Za-z0-9_-]{1,64}" (:world ctx))
                   (every? #(and (string? %) (.isAbsolute path %)) [(:worlds ctx) (:repo ctx)]))
      (fail! "invalid workspace context.edn"))
    ctx))

(defn route [ctx command argv]
  (when-not ((set commands) command) (fail! (str "unknown workspace tool " command)))
  ;; Match option tokens, not message/EDN substrings. '--' makes the rest literal.
  (doseq [arg (take-while #(not= "--" %) argv)]
    (when (re-matches #"^--(?:body|agent|world|worlds|state|repo|repo-root|workspace)(?:=.*)?$" arg)
      (fail! (str "workspace binds body/world/worlds/repo/workspace; cannot override " arg))))
  (let [prefix (cond-> []
                 (body-tools command) (conj (:body ctx))
                 true (into ["--world" (:world ctx) "--worlds" (:worlds ctx)])
                 (#{"plans" "blueprints"} command) (into ["--repo" (:repo ctx)])
                 (#{"map" "world-changes"} command) (into ["--repo-root" (:repo ctx)])
                 (= "snapshot" command) (into ["--workspace" (:dir ctx)]))]
    (into prefix argv)))
;; :dir, the workspace itself, is where context.edn lies; snapshot writes its pictures there.
(defn route-js [file command argv]
  (clj->js (route (assoc (context! file) :dir (.dirname path (.resolve path file))) command (vec argv))))

;; Player help: the wrapper supplies the body, world and directories, so they are cut from the usage lines.
(def player-cuts
  [[#"(\S+)\.mjs" "./bin/$1"]
   [#"\[--worlds DIR --state LEGACY_PARENT " "["]
   [#" \[--worlds (?:DIR|<dir>) --state (?:LEGACY_PARENT|<legacy-parent>)\]" ""]
   [#" --worlds (?:DIR|<dir>) --state (?:LEGACY_PARENT|<legacy-parent>)" ""]
   [#" \[--worlds (?:DIR|<dir>)\]| \[--state (?:LEGACY_PARENT|<legacy-parent>)\]" ""]
   [#" \[--worlds <dir>\]| \[--state <legacy-parent>\]| --worlds <dir>| --state <legacy-parent>| --repo <dir>" ""]
   [#" (?:<agent>|<body>|BODY)(?= )" ""]
   [#" --world (?:<world>|WORLD)" ""]])
(defn player-usage [text]
  (reduce (fn [t [from to]] (str/replace t (js/RegExp. (.-source from) "g") to)) text player-cuts))

(defn wrapper [repo command]
  ;; Dynamic imports work even beneath a caller's type:commonjs package.json.
  ;; No require/__dirname or top-level await: the same script also works in ESM.
  (str "#!/usr/bin/env node\n" wrapper-marker
       "Promise.all([import(" (js/JSON.stringify (str (pathToFileURL (.join path repo "engine/tools/workspace.mjs"))))
       "), import('node:url'), import('node:path')]).then(async ([{runBound}, {pathToFileURL}, {resolve, dirname}]) => {\n"
       "  process.exitCode = await runBound(pathToFileURL(resolve(dirname(process.argv[1]), '../context.edn')), "
       (js/JSON.stringify command) ", process.argv.slice(2));\n"
       "}).catch(error => { process.stderr.write(error.message + " (js/JSON.stringify "\n") "); process.exitCode = 2; });\n"))
(defn agents-text [{:keys [body world]}]
  (str "# Agent workspace\n\n"
       "You operate body `" body "` in world `" world "`. Read `context.edn`, `briefing.md`, and relevant `notes/` before acting. Keep private working notes and handoffs in `notes/`; publish lasting world discoveries through shared map/plans tools.\n\n"
       "Prefer the bound `bin/` tools even when older briefings mention `mc`. Read any existing `BRIEFING.md` and `journal.md` for mission history. Legacy `mc` and `start` launchers are obsolete; use `bin/` for agent commands. Body lifecycle is separate; workspace generation does not start or restart a body.\n\n"
       "Run `./bin/<tool>` here, or use its absolute path from elsewhere. Body/world/worlds/repository are bound; do not supply them. Each tool has `--help`. Bindings guide routing, not a security sandbox.\n\n"
       "Start with `./bin/observe`, then `./bin/observe inventory` or `./bin/entities` as needed. Discover jobs and triggers with `./bin/observe catalog jobs` and `./bin/observe catalog triggers`; inspect exact catalog entries before submitting unfamiliar work.\n\n"
       "Use `./bin/jobs` for managed work and `./bin/triggers` for event rules. `./bin/jobs submit` appends a job to the list; `--now` cuts the current job and runs the new one at once (the cut one continues after it); `--wait` blocks until the job ends and prints what happened meanwhile. Follow completion with `./bin/observe --wait --watch j12`; retrieve recorded outcomes with `./bin/observe result j12` after completion. Use `./bin/say 'message'` to communicate. `./bin/snapshot` draws what the body sees into `snapshots/` (a PNG) and says what is under the crosshair; `--yaw`/`--pitch`/`--look-at` turn only the picture.\n\n"
       "Manual actions require `./bin/drive take --why 'reason' --idle-s 30`; use the same `--who` for drive and world actions, poll returned request IDs, then release control.\n\n"
       "Shared memory: `./bin/map`, `./bin/plans`, `./bin/blueprints`, and `./bin/world-changes`. Read records before edits and use their revisions and an explicit author. `./bin/time clock` reads world time; `./bin/time dawn` waits for daylight.\n\n"
       "Observe before acting, protect existing builds and starter stock, and record task constraints in briefing.md. The repository's AGENTS.md governs code changes.\n"))

(defn adoption! [dir {:keys [worlds world body]}]
  (let [expected (.resolve path worlds world "agents" body)
        config (.join path dir "config.json")]
    (when-not (= dir expected)
      (fail! "--adopt-existing requires the canonical worlds/<world>/agents/<body> directory"))
    (when-not (and (file-exists? dir) (= dir (.realpathSync fs dir)))
      (fail! "--adopt-existing requires an existing directory without symlinked parents"))
    (regular! config false)
    (when-not (file-exists? config) (fail! "--adopt-existing requires config.json"))
    (when-not (= body (.-username (js/JSON.parse (.readFileSync fs config "utf8"))))
      (fail! "config.json username differs from workspace body"))))

(defn generate! [argv repo]
  (let [{:keys [positionals values]} (map-tool/parse-options (vec argv)
         {:body {:type "string"} :world {:type "string"} :worlds {:type "string"} :state {:type "string"}
          :update-tools {:type "boolean"} :adopt-existing {:type "boolean"}})
        [destination & extra] positionals
        {:keys [body world worlds state update-tools adopt-existing]} values
        repo (.resolve path repo)]
    (when (and worlds state) (fail! "choose --worlds or legacy --state"))
    (when-not (and destination (empty? extra)
                   (string? body) (re-matches #"[A-Za-z0-9_-]{1,40}" body)
                   (string? world) (re-matches #"[A-Za-z0-9_-]{1,64}" world)) (fail! usage))
    (let [dir (.resolve path destination) bin (.join path dir "bin")
          context-file (.join path dir "context.edn")
          ctx {:format format-id :body body :world world :worlds (.resolve path (or worlds (when state (.join path state "worlds")) (.join path repo "worlds"))) :repo repo}
          contents (into {} (map (fn [command] [(.join path bin command) (wrapper repo command)]) commands))
          had-context? (file-exists? context-file)]
      (regular! dir true)
      (when adopt-existing (adoption! dir ctx))
      (when (and (not adopt-existing) (file-exists? dir) (not had-context?) (seq (array-seq (.readdirSync fs dir))))
        (fail! "destination is not an empty directory or an existing generated workspace"))
      (when (and had-context? (not= ctx (context! context-file)))
        (fail! "workspace bindings differ; choose a new directory"))
      ;; Preflight every destination; never follow symlinks or replace unknown files.
      (doseq [directory [bin (.join path dir "notes")]] (regular! directory true))
      (doseq [file [(.join path dir "AGENTS.md") (.join path dir "WORKSPACE.md") (.join path dir "briefing.md") (.join path dir "BRIEFING.md")]] (regular! file false))
      (doseq [[file content] contents]
        (regular! file false)
        (when (file-exists? file)
          (let [old (.readFileSync fs file "utf8")]
            (when-not (str/starts-with? old (str "#!/usr/bin/env node\n" wrapper-marker))
              (fail! (str "refusing to replace unrecognized wrapper " file)))
            (when (and (not= old content) (not update-tools))
              (fail! "generated wrappers differ; rerun with --update-tools")))))
      (.mkdirSync fs bin #js {:recursive true})
      (.mkdirSync fs (.join path dir "notes") #js {:recursive true})
      (when-not had-context? (.writeFileSync fs context-file (str (pr-str ctx) "\n") #js {:flag "wx"}))
      (doseq [[file text] [[(.join path dir "AGENTS.md") "# Agent workspace\n\nRead WORKSPACE.md for your body/world bindings and tool instructions, then briefing.md and relevant notes/ before acting. The repository AGENTS.md governs code changes.\n"]
                         [(.join path dir "WORKSPACE.md") (agents-text ctx)]
                         [(.join path dir "briefing.md")
                          (if (file-exists? (.join path dir "BRIEFING.md"))
                            "# Briefing\n\nRead [the existing mission briefing](BRIEFING.md) and any journal.md before acting. Record new observations and handoffs in notes/. Workspace tool instructions are in WORKSPACE.md.\n"
                            "# Briefing\n\nRecord the mission, constraints, current priorities, and handoff here.\n")]]]
        (when-not (file-exists? file) (.writeFileSync fs file text #js {:flag "wx"})))
      (doseq [[file content] contents]
        (when (or (not (file-exists? file)) update-tools) (.writeFileSync fs file content #js {:mode 493}))
        (.chmodSync fs file 493))
      (clj->js {:ok true :directory dir :body body :world world :updated-tools (boolean update-tools) :adopted-existing (boolean adopt-existing)}))))
