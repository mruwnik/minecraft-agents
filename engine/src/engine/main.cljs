(ns engine.main
  "Entry point: npm run body -- --agent <name> --scenario <file> [--fresh] [--state-dir <dir>]"
  (:require [engine.core :as core]
            [engine.fsutil :as fsu]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.triggers :as triggers]
            ["fs" :as fs]
            ["path" :as path]
            ["module" :refer [createRequire]]))

(defn parse-args [args]
  (loop [[a b & more :as all] args
         opts {:agent nil :scenario nil :fresh? false :state-dir nil}]
    (cond
      (empty? all) opts
      (= a "--agent") (recur more (assoc opts :agent b))
      (= a "--scenario") (recur more (assoc opts :scenario b))
      (= a "--state-dir") (recur more (assoc opts :state-dir b))
      (= a "--fresh") (recur (rest all) (assoc opts :fresh? true))
      :else (recur (rest all) opts))))

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
             :engine-dir (path/join state-dir "agents" agent "engine")})))

(defn missing-primitives-message [file]
  (str "engine: " file " does not exist. The body needs the real primitives layer"
       " (js/primitives.mjs exporting createPrimitives); refusing to start."))

(def usage "usage: npm run body -- --agent <name> --scenario <file> [--fresh] [--state-dir <dir>]")

(defn preflight
  "Everything run needs before connecting, or {:error text}."
  [{:keys [agent scenario state-dir engine-root]}]
  (let [root (or engine-root (js/process.cwd))
        state-dir (or state-dir (path/resolve root ".." "state"))
        prims-file (path/join root "js" "primitives.mjs")
        cfg (when agent (load-agent state-dir agent))
        plan (when (and scenario (fs/existsSync scenario)) (scenario/read-file scenario))
        issues (when plan (scenario/problems registry/jobs triggers/all plan))]
    (cond
      (nil? agent) {:error usage}
      (:error cfg) {:error (:text cfg)}
      (not (fs/existsSync prims-file)) {:error (missing-primitives-message prims-file)}
      (and scenario (nil? plan)) {:error (str "no scenario file " scenario)}
      (seq issues) {:error (str "scenario problems: " (pr-str issues))}
      :else {:root root :cfg cfg :plan plan :state-dir state-dir})))

(defn ^:async run
  "Start a body. Resolves to {:engine eng :stop f} or {:error text}."
  [{:keys [fresh?] :as opts}]
  (let [{:keys [error root cfg plan state-dir]} (preflight opts)]
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
            eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (:engine-dir cfg)
                              :body (:username cfg)})
            _ (reset! eng-ref eng)]
        (when (and plan (not restoring?)) (core/load-scenario! eng plan))
        (let [stop-ticks (core/start! eng {:tick-ms 250})]
          {:engine eng :stop (fn [] (stop-ticks) (core/shutdown! eng) (.close p))})))))

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
