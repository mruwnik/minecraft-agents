(ns agent-tools.goal
  "goal.mjs: show, set or clear a body's goal (dashboard.goal), the one line the dashboard shows as what the body is
  doing now."
  (:require [engine.bodies :as bodies]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            [dashboard.goal :as goal]
            ["node:fs" :as fs]))

(def usage
  (str "usage: goal.mjs <body> --world <world> [<text> | --clear] [--by <name>] [--worlds <dir> --state <legacy-parent>]\n"
       "With no text, prints the goal ({:ok true :goal {:text :by :since} | nil}). <text> (one shell-quoted line, at most "
       goal/max-text " characters) sets it, by --by (default the body); --clear removes it. The dashboard shows it with its age."))

(defn request-for [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options (vec argv)
                                          {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}
                                           :by {:type "string"} :clear {:type "boolean"}})
          [body text & extra] positionals
          world (:world values)]
      (when-not (and (string? body) (re-matches #"[A-Za-z0-9_-]{1,40}" body))
        (throw (js/Error. "body must be a valid name")))
      (when-not (and (string? world) (re-matches #"[A-Za-z0-9_-]{1,64}" world))
        (throw (js/Error. "missing or invalid --world <world>")))
      (when (seq extra) (throw (js/Error. "the goal must be one shell-quoted argument")))
      (when (and (:clear values) (some? text)) (throw (js/Error. "choose <text> or --clear")))
      (when (and (some? text) (nil? (goal/clean-text text))) (throw (js/Error. "goal text must not be empty")))
      {:op (cond (:clear values) :clear (some? text) :set :else :show)
       :text text :by (or (:by values) body)
       :dir (bodies/body-dir (bodies/storage-root values map-tool/default-state-dir) world body)})
    (catch :default error {:error (.-message error)})))

(defn print-edn! [value] (.write (.-stdout js/process) (str (data/write-edn value) "\n")))

(defn main!
  "argv -> exit code (0 done, 2 refused); prints one EDN map."
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv]
   (let [{:keys [op text by dir error]} (request-for argv)]
     (cond
       error (do (print-edn! {:ok false :reason :bad-args :message error :usage usage}) 2)
       (not (fs/existsSync dir)) (do (print-edn! {:ok false :reason :no-body :message "no such body folder in this world"}) 2)
       :else (do (print-edn! (case op
                               :show {:ok true :goal (goal/read-goal dir)}
                               :set {:ok true :goal (goal/write-goal! dir text by (js/Date.now))}
                               :clear (do (goal/clear-goal! dir) {:ok true :goal nil :cleared true})))
                 0)))))
