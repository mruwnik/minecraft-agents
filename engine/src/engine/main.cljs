(ns engine.main
  "Entry point: npm run body -- --agent <name> --world <world> --scenario <file> [--fresh] [--upgrade] [--worlds <dir>] [--state-dir <legacy-parent>]"
  (:require [engine.bodies :as bodies]
            [engine.core :as core]
            [engine.entity-observations :as entity-observations]
            [engine.fsutil :as fsu]
            [engine.event-api :as event-api]
            [engine.backoff :as backoff]
            [engine.events :as events]
            [engine.hurt :as hurt]
            [engine.lease :as lease]
            [engine.notes :as notes]
            [engine.path.offsets :as offsets]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.settings-registry :as settings-registry]
            [jobs.survival.recover-drops :as recover-drops]
            [engine.scenario :as scenario]
            [engine.settings :as settings]
            [engine.senses :as senses]
            [engine.single :as single]
            [engine.takeover :as takeover]
            [engine.trigger-api :as trigger-api]
            [engine.triggers :as triggers]
            ["fs" :as fs]
            ["path" :as path]
            ["module" :refer [createRequire]]
            [engine.hooks :as hooks]))

(def settings
  {:engine.main/shutdown-limit-ms {:default 5000 :type :int :min 1
                                   :doc "The signal handler exits after this long even when the body has not stopped, ms."}
   :engine.game/follow-tick-rate {:default true :type :bool
                                  :doc "Body physics follows the server's /tick rate (freeze, step, faster or slower); off runs stock 20 TPS physics. Read at body start."}})

(defn parse-args [args]
  (loop [[a b & more :as all] args
         opts {:agent nil :world nil :scenario nil :fresh? false :upgrade? false :state-dir nil :drive-idle-s 15}]
    (cond
      (empty? all) opts
      (= a "--agent") (recur more (assoc opts :agent b))
      (= a "--world") (recur more (assoc opts :world b))
      (= a "--scenario") (recur more (assoc opts :scenario b))
      (= a "--state-dir") (recur more (assoc opts :state-dir b))
      (= a "--worlds") (recur more (assoc opts :worlds b))
      (= a "--drive-idle-s") (recur more (assoc opts :drive-idle-s (js/parseFloat b)))
      (= a "--fresh") (recur (rest all) (assoc opts :fresh? true))
      (= a "--upgrade") (recur (rest all) (assoc opts :upgrade? true))
      :else (recur (rest all) opts))))

(defn load-agent
  "Config for agent in world under state-dir: {:username :host :port :world :engine-dir}, or {:error kw :text}.
  The world is where the body folder is, never a field of its config."
  [state-dir world-name agent]
  (let [dir (bodies/body-dir state-dir world-name agent)
        config (fsu/read-json (path/join dir "config.json"))
        world (when config (fsu/read-json (path/join (bodies/worlds-dir state-dir) world-name "world.json")))]
    (cond
      (nil? config) {:error :no-config :text (str "no config for agent " agent " in world " world-name ": no " dir "/config.json")}
      (nil? world) {:error :no-world :text (str "no world.json for world " world-name)}
      :else {:username (or (:username config) agent)
             :host (:host world)
             :port (:port world)
             :world world-name
             :view-distance (:viewDistance config)
             :engine-dir (path/join dir "engine")})))

(defn connect-options
  "The connection part of createPrimitives' options; viewDistance only when the config sets it (connect.mjs defaults it)."
  [{:keys [host port username view-distance]}]
  (cond-> #js {:host host :port port :username username
               :followTickRate (settings/get settings :engine.game/follow-tick-rate)}
    (some? view-distance) (doto (unchecked-set "viewDistance" view-distance))))

(defn missing-primitives-message [file]
  (str "engine: " file " does not exist. The body needs the real primitives layer"
       " (js/primitives.mjs exporting createPrimitives); refusing to start."))

(defn body-triggers
  "The built-in triggers plus :condition, the trigger of ad hoc :when entries."
  []
  (trigger-api/with-conditions triggers/all trigger-api/compile-condition))

(def usage "usage: npm run body -- --agent <name> --world <world> --scenario <file> [--fresh] [--upgrade] [--worlds <dir>] [--state-dir <legacy-parent>]")

(defn preflight
  "Everything run needs before connecting, or {:error text}. On a restart (a saved engine.edn, no --fresh) a scenario
  entry naming an unknown trigger is left out and returned as :stale [{:id :message}]; any other scenario problem,
  and any problem on a first start, is an error."
  [{:keys [agent world scenario state-dir worlds engine-root fresh?]}]
  (let [root (or engine-root (js/process.cwd))
        state-dir (bodies/storage-root {:state state-dir :worlds worlds} (path/resolve root ".."))
        prims-file (path/join root "js" "primitives.mjs")
        names-ok? (and (string? agent) (string? world) (re-matches bodies/name-re agent) (re-matches bodies/name-re world))
        cfg (when names-ok? (load-agent state-dir world agent))
        read (when (and scenario (fs/existsSync scenario)) (scenario/read-file scenario))
        restoring? (and (not fresh?) (not (:error cfg)) cfg (fs/existsSync (path/join (:engine-dir cfg) "engine.edn")))
        full-plan (scenario/with-defaults read)
        stale (when (and restoring? full-plan)
                (vec (keep (fn [e] (let [p (trigger-api/entry-problem registry/jobs (body-triggers) e)]
                                     (when (= :unknown-trigger (:reason p))
                                       {:id (trigger-api/scenario-id e) :message (:message p)})))
                           (:register full-plan))))
        stale-ids (set (map :id stale))
        plan (cond-> full-plan
               (seq stale) (update :register (fn [r] (filterv #(not (stale-ids (trigger-api/scenario-id %))) r))))
        issues (when plan (scenario/problems registry/jobs (body-triggers) plan))]
    (cond
      (nil? agent) {:error usage}
      (nil? world) {:error (str (bodies/missing-world-error "--world") "\n" usage)}
      (not names-ok?) {:error "--agent and --world take names of letters, digits, _ and -"}
      (:error cfg) {:error (:text cfg)}
      (not (fs/existsSync prims-file)) {:error (missing-primitives-message prims-file)}
      (and scenario (nil? read)) {:error (str "no scenario file " scenario)}
      (seq issues) {:error (str "scenario problems: " (pr-str issues) "; fix or remove them in " scenario)}
      :else {:root root :cfg cfg :plan plan :stale stale :state-dir state-dir})))

(defn ^:async start-control!
  "Serve the manual-control socket under the engine dir; resolves to the control, or nil (with an
  error event) when it cannot listen."
  [root eng cfg opts]
  (let [create-control (.-createControl ((createRequire (str root "/")) "./js/control.mjs"))
        control (create-control #js {:socketPath (path/join (:engine-dir cfg) "control.sock")
                                     :handle (fn [method path body content-type]
                                               (takeover/handle eng opts method path body content-type))})]
    (try
      (await (.listen control))
      control
      (catch :default e
        (core/emit! eng {:source :system :kind :control_unavailable :level :error :text (str (.-message e))})
        nil))))

(defn open-world
  "The body's world store (the :world/open hook: plans, blueprints, zones, claims) with its notes store
  (engine.notes) as :notes, in the world folder worlds/<world>; the body's notes are written as agent's."
  [{:keys [state-dir world agent root emit]}]
  (let [dir (path/join (bodies/worlds-dir state-dir) world)]
    (assoc ((:world/open hooks/all) {:plans-dir (path/join dir "plans")
                                     :blueprint-dir (path/resolve root ".." "blueprints")
                                     :zones-file (path/join dir "zones.edn")
                                     :claims-file (path/join dir "claims.edn")
                                     :emit emit})
           :notes (notes/open {:world-dir dir :body agent :emit emit}))))

(defn boot-scenario!
  "A first start loads the scenario. A restore keeps the saved register: dropped (stale) entries are reported, and
  resume-scenario! offers or adds the new defaults; with no scenario it closes any offer saved from an earlier run."
  [eng {:keys [plan stale restoring? upgrade?]}]
  (if-not restoring?
    (when plan (trigger-api/load-scenario! eng plan))
    (do (doseq [{:keys [id message]} stale]
          (let [text (str "scenario names " (pr-str id) ", which is not a known trigger (renamed or misspelt); skipped")]
            (core/emit! eng {:source :system :kind :dropped :level :warn :reflex id :error message :text text})
            (core/request-attention! eng {:job-id (str "reflex:" (name id)) :reason :reflex-dropped
                                          :kind :reflex-dropped :data {:reflex id :error message} :message text})))
        (trigger-api/resume-scenario! eng plan upgrade?))))

(defn ^:async start
  "Open the body's files, log in and run, once the one-process guard is held (release frees it).
  Resolves to {:engine eng :stop f}."
  [{:keys [fresh? upgrade?] :as opts} {:keys [root cfg plan stale state-dir]} release]
  (let [engine-file (path/join (:engine-dir cfg) "engine.edn")
        _ (offsets/set-root! root)
        ;; the settings files; their problems wait here until the engine exists to emit them
        settings-events (atom [])
        _ (settings/load! {:specs (merge registry/settings settings-registry/settings settings)
                           :world-file (path/join (bodies/worlds-dir state-dir) (:world cfg) "settings.edn")
                           :body-file (path/join (bodies/body-dir state-dir (:world cfg) (:agent opts)) "settings.edn")
                           :body (:agent opts)
                           :emit #(swap! settings-events conj %)})
        _ (when (and fresh? (fs/existsSync engine-file)) (fs/unlinkSync engine-file))
        restoring? (fs/existsSync engine-file)
        create-primitives (.-createPrimitives ((createRequire (str root "/")) "./js/primitives.mjs"))
        eng-ref (atom nil)
        ;; the view dump's view.stats and view.error go straight to the event stream (docs/view-format.md)
        on-view-event (fn [e]
                        (when-let [eng @eng-ref]
                          (core/emit! eng (-> (js->clj e :keywordize-keys true)
                                              (update :kind keyword) (update :source keyword) (update :level keyword)))))
        raw-p (await (create-primitives (doto (connect-options cfg)
                                             (unchecked-set "view" #js {:stateDir (clj->js state-dir) :agent (:agent opts) :world (:world cfg)
                                                                        :onEvent on-view-event}))))
        ;; what the body has seen (engine.perception); BODY_PERCEPTION=0 runs without it
        per (when (and (.-rawWorld raw-p) (not= "0" (.. js/process -env -BODY_PERCEPTION)))
              (perception/create (.-rawWorld raw-p) {}))
        sensed (senses/wrap raw-p (.-rawWorld raw-p))
        p (if per (perception/wrap sensed per) sensed)
        world (open-world {:state-dir state-dir :world (:world cfg) :agent (:agent opts) :root root
                           :emit (fn [e] (some-> @eng-ref (core/emit! e)))})
        base-eng (core/create {:primitives p :jobs registry/jobs :triggers (body-triggers) :dir (:engine-dir cfg)
                               :body (:username cfg) :world world})
        _ (reset! eng-ref base-eng)
        _ (run! #(core/emit! base-eng %) @settings-events)
        _ (when-let [gc (.-gameClock raw-p)] (settings/wire-game-clock! gc #(core/emit! base-eng %)))
        _ (trigger-api/restore-conditions! base-eng)
        _ (boot-scenario! base-eng {:plan plan :stale stale :restoring? restoring? :upgrade? upgrade?})
        seen (entity-observations/start! p {:world (:world cfg) :body (:agent opts)})
        eng (assoc base-eng :seen-entities seen)
        _ (reset! eng-ref eng)
        stop-perception (if per
                          (perception/start! per {:file (path/join (:engine-dir cfg) "seen.bin")
                                                  :io ((createRequire (str root "/")) "./js/seen-file.mjs")
                                                  :on-event (fn [e] (some-> @eng-ref (core/emit! e)))})
                          (fn []))]
    (let [event-socket (event-api/create (path/join (:engine-dir cfg) "events.sock") eng
                                             {:died recover-drops/death-status})]
      (try
        (await ((:listen event-socket)))
        (let [lease-opts {:idle-ms (* 1000 (:drive-idle-s opts))}
              control (await (start-control! root eng cfg lease-opts))
              stop-ticks (core/start! eng {:tick-ms 250 :before-tick #(do (takeover/tick! eng lease-opts) (trigger-api/tick! eng))})]
          {:engine eng :stop (fn ^:async stop []
                               ((:stop seen))
                               (let [saved (stop-perception)]
                                 (stop-ticks) (takeover/close! eng) (some-> control .close)
                                 ((:close event-socket)) (core/shutdown! eng)
                                 (await saved)
                                 (core/save-memory! eng)
                                 (await (.close p))
                                 (release)))})
        (catch :default e
          ((:stop seen))
          (stop-perception)
          ((:close event-socket))
          (core/shutdown! eng)
          (await (.close p))
          (throw e))))))

(defn shutdown-handler
  "The signal handler: stops the body, then exits once stop has finished (memory saved) or after limit-ms, whichever
  comes first, and also when stop fails. A second signal returns the first one's promise: stop runs once."
  ([stop exit!] (shutdown-handler stop exit! (settings/get settings :engine.main/shutdown-limit-ms)))
  ([stop exit! limit-ms]
   (let [running (atom nil)]
     (fn []
       (or @running
           (let [timer (atom nil)
                 limit (js/Promise. (fn [resolve _] (reset! timer (js/setTimeout resolve limit-ms))))
                 stopped (-> (js/Promise.resolve) (.then stop))
                 done (-> (js/Promise.race #js [stopped limit])
                          (.catch (fn [_]))
                          (.then (fn [] (js/clearTimeout @timer) (exit!))))]
             (reset! running done)))))))

(defn ^:async run
  "Start a body. Resolves to {:engine eng :stop f}, or {:error text}; :exit-code 3 when the body already runs.
  The one-process guard (engine.single) is taken right after the read-only preflight: before any file in the body's
  folder is opened, truncated or deleted (--fresh included) and before the login."
  [opts]
  (let [{:keys [error cfg] :as pre} (preflight opts)]
    (if error
      {:error error}
      (let [sock (single/socket-path (:engine-dir cfg))
            claim (await (single/claim! sock {:pid js/process.pid :world (:world cfg) :body (:agent opts)}))]
        (if-let [running (:running claim)]
          {:error (single/refusal (:world cfg) (:agent opts) sock running) :exit-code single/exit-code}
          (try
            (await (start opts pre (:held claim)))
            (catch :default e
              (await ((:held claim)))
              (throw e))))))))

(defn fail!
  ([text] (fail! text 2))
  ([text code]
   (.write js/process.stderr (str text "\n"))
   (js/process.exit code)))

(defn main [& args]
  (-> (run (parse-args args))
      (.then (fn [{:keys [error stop exit-code]}]
               (when error (fail! error (or exit-code 2)))
               (let [shutdown (shutdown-handler stop #(js/process.exit 0))]
                 (.on js/process "SIGINT" shutdown)
                 (.on js/process "SIGTERM" shutdown))))
      (.catch (fn [e] (fail! (str "engine: " (.-stack e)))))))
