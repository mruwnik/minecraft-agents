(ns engine.main
  "Entry point: npm run body -- --agent <name> --world <world> --scenario <file> [--fresh] [--state-dir <dir>]"
  (:require [engine.bodies :as bodies]
            [engine.core :as core]
            [engine.fsutil :as fsu]
            [engine.event-api :as event-api]
            [engine.notes :as notes]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.takeover :as takeover]
            [engine.trigger-api :as trigger-api]
            [engine.triggers :as triggers]
            [engine.world :as world]
            ["fs" :as fs]
            ["path" :as path]
            ["module" :refer [createRequire]]))

(defn parse-args [args]
  (loop [[a b & more :as all] args
         opts {:agent nil :world nil :scenario nil :fresh? false :state-dir nil :drive-idle-s 15 :events-max-bytes nil}]
    (cond
      (empty? all) opts
      (= a "--agent") (recur more (assoc opts :agent b))
      (= a "--world") (recur more (assoc opts :world b))
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
             :events-max-bytes (get-in config [:engine :events :maxBytes])
             :engine-dir (path/join dir "engine")})))

(defn missing-primitives-message [file]
  (str "engine: " file " does not exist. The body needs the real primitives layer"
       " (js/primitives.mjs exporting createPrimitives); refusing to start."))

(defn body-triggers
  "The built-in triggers plus :condition, the trigger of ad hoc :when entries."
  []
  (trigger-api/with-conditions triggers/all trigger-api/compile-condition))

(def usage "usage: npm run body -- --agent <name> --world <world> --scenario <file> [--fresh] [--state-dir <dir>]")

(defn preflight
  "Everything run needs before connecting, or {:error text}."
  [{:keys [agent world scenario state-dir engine-root events-max-bytes]}]
  (let [root (or engine-root (js/process.cwd))
        state-dir (or state-dir (path/resolve root ".." "state"))
        prims-file (path/join root "js" "primitives.mjs")
        names-ok? (and (string? agent) (string? world) (re-matches bodies/name-re agent) (re-matches bodies/name-re world))
        cfg (when names-ok? (load-agent state-dir world agent))
        max-bytes (when-not (:error cfg) (event-cap events-max-bytes (:events-max-bytes cfg)))
        plan (when (and scenario (fs/existsSync scenario)) (scenario/read-file scenario))
        issues (when plan (scenario/problems registry/jobs (body-triggers) plan))]
    (cond
      (nil? agent) {:error usage}
      (nil? world) {:error (str (bodies/missing-world-error "--world") "\n" usage)}
      (not names-ok?) {:error "--agent and --world take names of letters, digits, _ and -"}
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

(defn open-world
  "The body's world (engine.world: plans, blueprints, zones) with its notes store (engine.notes) as :notes, in
  the world folder state/worlds/<world>; the body's notes are written as agent's."
  [{:keys [state-dir world agent root emit]}]
  (let [dir (path/join (bodies/worlds-dir state-dir) world)]
    (assoc (world/open {:plans-dir (path/join dir "plans")
                        :blueprint-dir (path/resolve root ".." "blueprints")
                        :zones-file (path/join dir "zones.edn")
                        :emit emit})
           :notes (notes/open {:world-dir dir :body agent :emit emit}))))

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
            world (open-world {:state-dir state-dir :world (:world cfg) :agent (:agent opts) :root root
                               :emit (fn [e] (some-> @eng-ref (core/emit! e)))})
            eng (core/create {:primitives p :jobs registry/jobs :triggers (body-triggers) :dir (:engine-dir cfg)
                              :body (:username cfg) :max-event-bytes events-max-bytes :world world})
            _ (reset! eng-ref eng)]
        (trigger-api/restore-conditions! eng)
        (when (and plan (not restoring?)) (trigger-api/load-scenario! eng plan))
        (let [event-socket (event-api/create (path/join (:engine-dir cfg) "events.sock") eng)]
          (try
            (await ((:listen event-socket)))
            (let [lease-opts {:idle-ms (* 1000 (:drive-idle-s opts))}
                  control (await (start-control! root eng cfg lease-opts))
                  stop-ticks (core/start! eng {:tick-ms 250 :before-tick #(do (takeover/tick! eng lease-opts) (trigger-api/tick! eng))})]
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
