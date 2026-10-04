(ns agent-tools.jobs
  "Job command validation and native EDN requests; HTTP remains a Node boundary."
  (:require [agent-tools.map :as map-tool]
            ["node:path" :as path]
            ["node:crypto" :as crypto]))

(def usage "usage: jobs.mjs <body> --world <world> list [--limit 8 --offset 0] | show <jID> | submit <EDN-spec> [--hold --front] | interrupt <EDN-spec> | cancel <jID> | cancel-all | retry <jID> [--state DIR] [--request-id ID]\nMutations return immediately; observe.mjs <body> --world <world> --wait --watch jID tracks completion.")

(defn spec-for [text]
  (when (or (not (string? text)) (> (.byteLength js/Buffer text) 12000))
    (throw (js/Error. "spec must be EDN text, at most 12000 bytes")))
  (let [form (map-tool/read-edn text)]
    (when-not (and (seq? form) (symbol? (first form)))
      (throw (js/Error. "spec must be one native EDN job expression list")))
    form))

(defn request-for [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options argv
          {:state {:type "string" :default map-tool/default-state-dir} :world {:type "string"}
           :request-id {:type "string"} :limit {:type "string"} :offset {:type "string"}
           :hold {:type "boolean"} :front {:type "boolean"}})
          [body op arg & extra] positionals
          op (keyword (or op "list"))
          no-argument? (#{:list :cancel-all} op)
          mutating? (boolean (#{:submit :interrupt :cancel :cancel-all :retry} op))
          spec-op? (#{:submit :interrupt} op)]
      (when-not (and (string? body) (re-matches #"[A-Za-z0-9_-]{1,40}" body))
        (throw (js/Error. "body must be a valid name")))
      (when (nil? (:world values))
        (throw (js/Error. "missing --world <world>: the world the body plays in (a folder under state/worlds/)")))
      (when-not (re-matches #"[A-Za-z0-9_-]{1,64}" (:world values))
        (throw (js/Error. "the world must be a name of letters, digits, _ and -")))
      (when-not (#{:list :show :submit :interrupt :cancel :cancel-all :retry} op)
        (throw (js/Error. "unknown operation")))
      (when (or (seq extra) (if no-argument? (some? arg) (nil? arg)))
        (throw (js/Error. (str (name op) (if no-argument? " takes no argument" " needs exactly one argument")))))
      (when (and (not mutating?) (:request-id values))
        (throw (js/Error. "--request-id requires a mutation")))
      (when (and (not= op :list) (or (:limit values) (:offset values)))
        (throw (js/Error. "--limit and --offset require list")))
      (when (and (contains? values :hold) (not spec-op?))
        (throw (js/Error. "--hold requires submit or interrupt")))
      (when (and (contains? values :front) (not= op :submit))
        (throw (js/Error. "--front requires submit")))
      (let [state (.resolve path (:state values))
            base {:body body :state state
                  :socketPath (.join path state "worlds" (:world values) "agents" body "engine" "events.sock")
                  :mutating mutating?}]
        (cond
          (= op :list)
          (let [limit (js/Number (or (:limit values) 8)) offset (js/Number (or (:offset values) 0))]
            (when-not (and (js/Number.isInteger limit) (<= 1 limit 32)
                           (js/Number.isInteger offset) (<= 0 offset 10000))
              (throw (js/Error. "list limit must be1..32 and offset0..10000")))
            (assoc base :path (str "/jobs?limit=" limit "&offset=" offset)))
          :else
          (do
            (when (and (#{:show :cancel :retry} op) (not (re-matches #"j[0-9]+" arg)))
              (throw (js/Error. "job ID must be j<number>")))
            (if (= op :show)
              (assoc base :path (str "/job?id=" arg))
              (let [id (or (:request-id values) (.randomUUID crypto))]
                (when-not (re-matches #"[A-Za-z0-9_.:-]{1,80}" id)
                  (throw (js/Error. "--request-id must be a short identifier")))
                (assoc base :path "/jobs"
                       :request (cond-> {:op op :request-id id}
                                  spec-op? (assoc :spec (spec-for arg))
                                  (#{:cancel :retry} op) (assoc :id arg)
                                  (contains? values :hold) (assoc :hold? (:hold values))
                                  (contains? values :front) (assoc :front? (:front values))))))))))
    (catch :default error {:error (.-message error)})))
