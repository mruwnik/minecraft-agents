(ns agent-tools.say
  "Fast control-adjacent public chat and whisper command validation."
  (:require [engine.bodies :as bodies]
            [agent-tools.map :as map-tool]
            [clojure.string :as str]
            ["node:path" :as path]))

(def usage "usage: say.mjs <body> --world <world> <message> [--to <player>] [--worlds <dir> --state <legacy-parent>]")

(defn request-for [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options (vec argv)
          {:state {:type "string"} :worlds {:type "string"}
           :world {:type "string"} :to {:type "string"}})
          [body message & extra] positionals
          world (:world values)
          to (:to values)
          cleaned (some-> message str (str/replace #"[\x00-\x1f\x7f]" " ") (str/replace "§" "") str/trim)
          limit (if to (- 256 (count (str "/tell " to " "))) 256)]
      (when-not (and (string? body) (re-matches #"[A-Za-z0-9_-]{1,40}" body))
        (throw (js/Error. "body must be a valid name")))
      (when-not (and (string? world) (re-matches #"[A-Za-z0-9_-]{1,64}" world))
        (throw (js/Error. "missing or invalid --world <world>")))
      (when (seq extra) (throw (js/Error. "message must be one shell-quoted argument")))
      (when-not (and cleaned (not (empty? cleaned))) (throw (js/Error. "message must not be empty")))
      (when (str/starts-with? cleaned "/") (throw (js/Error. "message cannot start with /")))
      (when (and to (not (re-matches #"[A-Za-z0-9_]{3,16}" to)))
        (throw (js/Error. "--to must be a Minecraft player name (3-16 letters, digits, or _)")))
      (when (> (count cleaned) limit)
        (throw (js/Error. (str "message is longer than " limit " characters"))))
      (let [state (bodies/storage-root values map-tool/default-state-dir)]
        {:body body :world world :message cleaned :to to :state state
         :socketPath (.join path (bodies/worlds-dir state) world "agents" body "engine" "events.sock")}))
    (catch :default error {:error (.-message error)})))
