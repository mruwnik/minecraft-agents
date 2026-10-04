(ns engine.main
  "Entry point: npm run body -- --agent <name> --scenario <file> [--fresh] [--state-dir <dir>]"
  (:require [engine.core :as core]
            [engine.fsutil :as fsu]
            [engine.event-api :as event-api]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.takeover :as takeover]
            [engine.triggers :as triggers]
            [engine.world :as world]
            ["fs" :as fs]
            ["path" :as path]
            ["module" :refer [createRequire]]))

(defn parse-args [args]
  (loop [[a b & more :as all] args
         opts {:agent nil :scenario nil :fresh? false :state-dir nil :drive-idle-s 15 :events-max-bytes nil}]
    (cond
      (empty? all) opts
      (= a "--agent") (recur more (assoc opts :agent b))
      (= a "--scenario") (recur more (assoc opts :scenario b))
      (= a "--state-dir") (recur more (assoc opts :state-dir b))
      (= a "--drive-idle-s") (recur more (assoc opts :drive-idle-s (js/parseFloat b)))
      (= a "--events-max-bytes") (recur more (assoc opts :events-max-bytes (or b "")))
      (= a "--fresh") (recur (rest all) (assoc opts :fresh? true))
      :else (recur (rest all) opts))))

(def default-events-max-bytes 67108864)

(defn event-cap [override configured]
  (let [raw (cond (some? override) override (some? configured) configured :else default-events-max-bytes)
        valid-text? (or (number? raw) (and (string? raw) (re-matches #"[0-9]+" raw)))
        n (if (string? raw) (js/Number raw) raw)]
    (when (and valid-text? (js/Number.isSafeInteger n) (<= 1024 n)) n)))

(defn load-agent
  "Config for agent under state-dir: {:username :host :port :engine-dir}, or {:error kw :text}."
  [state-dir agent]
  (let [config (fsu/read-json (path/join state-dir "agents" agent "config.json"))
        world (when config (fsu/read-json (path/join state-dir "worlds" (:world config) "world.json")))]
    (cond
      (nil? config) {:error :no-config :text (str "no config for agent " agent " under " state-dir)}
      (nil? world) {:error :no-world :text (str "no world.json for world " (:world config))}
      :else {:username (or (:username config) agent)
             :host (:host world)
             :port (:port world)
             :world (:world config)
             :events-max-bytes (get-in config [:engine :events :maxBytes])
             :engine-dir (path/join state-dir "agents" agent "engine")})))

(defn missing-primitives-message [file]
  (str "engine: " file " does not exist. The body needs the real primitives layer"
       " (js/primitives.mjs exporting createPrimitives); refusing to start."))

(def usage "usage: npm run body -- --agent <name> --scenario <file> [--fresh] [--state-dir <dir>]")

(defn preflight
  "Everything run needs before connecting, or {:error text}."
  [{:keys [agent scenario state-dir engine-root events-max-bytes]}]
  (let [root (or engine-root (js/process.cwd))
        state-dir (or state-dir (path/resolve root ".." "state"))
        prims-file (path/join root "js" "primitives.mjs")
        cfg (when agent (load-agent state-dir agent))
        max-bytes (when-not (:error cfg) (event-cap events-max-bytes (:events-max-bytes cfg)))
        plan (when (and scenario (fs/existsSync scenario)) (scenario/read-file scenario))
        issues (when plan (scenario/problems registry/jobs triggers/all plan))]
    (cond
      (nil? agent) {:error usage}
      (:error cfg) {:error (:text cfg)}
      (not (fs/existsSync prims-file)) {:error (missing-primitives-message prims-file)}
      (nil? max-bytes) {:error "engine: --events-max-bytes and engine.events.maxBytes must be safe integers >= 1024"}
      (and scenario (nil? plan)) {:error (str "no scenario file " scenario)}
      (seq issues) {:error (str "scenario problems: " (pr-str issues))}
      :else {:root root :cfg cfg :plan plan :state-dir state-dir :events-max-bytes max-bytes})))

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

(defn ^:async run
  "Start a body. Resolves to {:engine eng :stop f} or {:error text}."
  [{:keys [fresh?] :as opts}]
  (let [{:keys [error root cfg plan state-dir events-max-bytes]} (preflight opts)]
    (if error
      {:error error}
      (let [engine-file (path/join (:engine-dir cfg) "engine.edn")
            _ (when (and fresh? (fs/existsSync engine-file)) (fs/unlinkSync engine-file))
            restoring? (fs/existsSync engine-file)
            create-primitives (.-createPrimitives ((createRequire (str root "/")) "./js/primitives.mjs"))
            eng-ref (atom nil)
            ;; the view dump's view.stats and view.error go straight to the event stream (docs/view-format.md)
            on-view-event (fn [e]
                            (when-let [eng @eng-ref]
                              (core/emit! eng (-> (js->clj e :keywordize-keys true)
                                                  (update :kind keyword) (update :source keyword) (update :level keyword)))))
            p (await (create-primitives #js {:host (:host cfg) :port (:port cfg) :username (:username cfg)
                                             :view #js {:stateDir state-dir :agent (:agent opts) :world (:world cfg)
                                                        :onEvent on-view-event}}))
            world (world/open {:plans-dir (path/join state-dir "worlds" (:world cfg) "plans")
                               :blueprint-dir (path/resolve root ".." "blueprints")
                               :zones-file (path/join state-dir "worlds" (:world cfg) "zones.edn")
                               :emit (fn [e] (some-> @eng-ref (core/emit! e)))})
            eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (:engine-dir cfg)
                              :body (:username cfg) :max-event-bytes events-max-bytes :world world})
            _ (reset! eng-ref eng)]
        (when (and plan (not restoring?)) (core/load-scenario! eng plan))
        (let [event-socket (event-api/create (path/join (:engine-dir cfg) "events.sock") eng)]
          (try
            (await ((:listen event-socket)))
            (let [lease-opts {:idle-ms (* 1000 (:drive-idle-s opts))}
                  control (await (start-control! root eng cfg lease-opts))
                  stop-ticks (core/start! eng {:tick-ms 250 :before-tick #(takeover/tick! eng lease-opts)})]
              {:engine eng :stop (fn [] (stop-ticks) (takeover/close! eng) (some-> control .close)
                                   ((:close event-socket)) (core/shutdown! eng) (.close p))})
            (catch :default e
              ((:close event-socket))
              (core/shutdown! eng)
              (await (.close p))
              (throw e))))))))

(defn fail! [text]
  (.write js/process.stderr (str text "\n"))
  (js/process.exit 2))

(defn main [& args]
  (-> (run (parse-args args))
      (.then (fn [{:keys [error stop]}]
               (when error (fail! error))
               (let [shutdown (fn [] (stop) (js/setTimeout #(js/process.exit 0) 200))]
                 (.on js/process "SIGINT" shutdown)
                 (.on js/process "SIGTERM" shutdown))))
      (.catch (fn [e] (fail! (str "engine: " (.-stack e)))))))
