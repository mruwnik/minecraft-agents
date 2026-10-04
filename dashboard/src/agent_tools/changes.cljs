(ns agent-tools.changes
  "Agent change feed with explicit cursors or serialized named checkpoints."
  (:require [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:timers/promises" :refer [setTimeout]]))

(def usage "world-changes.mjs --world WORLD [--cursor EDN | --observer NAME] [--wait --timeout 60s] [--raw] [--type TYPE --owner OWNER --status STATUS --text TEXT --center EDN --place NAME --radius 128 --limit 10 --state DIR]")

(defn options [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options argv
                    (merge (zipmap [:world :repo-root :cursor :observer :timeout :poll-ms :center :place :radius :type :owner :status :text :limit]
                                   (repeat {:type "string"}))
                           {:state {:type "string" :default map-tool/default-state-dir}}
                           (zipmap [:wait :raw] (repeat {:type "boolean"}))))
        v values
        observer (or (:observer v) "agent")
        [_ n unit] (re-matches #"^(\d+(?:\.\d+)?)(ms|s|m)?$" (or (:timeout v) "60s"))
        timeout-ms (if n (* (js/Number n) (get {"ms" 1 "s" 1000 "m" 60000} (or unit "s"))) js/NaN)
        poll-ms (js/Number (or (:poll-ms v) 250))]
    (when (or (seq positionals) (and (:cursor v) (:observer v))) (throw (data/fail :invalid-options usage)))
    (when (and (:cursor v) (> (.byteLength js/Buffer (:cursor v)) 500)) (throw (data/fail :invalid-cursor "cursor too large")))
    (when-not (re-matches #"^[A-Za-z0-9_-]{1,64}$" observer) (throw (data/fail :invalid-observer "invalid observer name")))
    (when (or (not (js/Number.isFinite timeout-ms)) (< timeout-ms 10) (> timeout-ms 3600000)
              (not (js/Number.isInteger poll-ms)) (< poll-ms 50) (> poll-ms 5000))
      (throw (data/fail :invalid-timeout "timeout10ms..60m and poll50..5000ms required")))
    (when (and (not (:wait v)) (or (:timeout v) (:poll-ms v))) (throw (data/fail :invalid-options "timeout/poll need --wait")))
    (let [ctx (data/context (select-keys v [:state :world :repo-root]))]
      {:ctx ctx :filter (map-tool/filters ctx v) :observer observer
       :cursor (when (:cursor v) (map-tool/read-edn (:cursor v))) :explicit-cursor (boolean (:cursor v))
       :wait (boolean (:wait v)) :raw (boolean (:raw v)) :timeout-ms timeout-ms :poll-ms poll-ms})))

(defn checkpoint! [file cursor]
  (.mkdirSync fs (.dirname path file) #js {:recursive true})
  (let [temp (str file "." (.-pid js/process) ".tmp")]
    (try (.writeFileSync fs temp (str (data/write-edn cursor) "\n") #js {:mode 384})
         (.renameSync fs temp file)
         (finally (when (.existsSync fs temp) (.unlinkSync fs temp))))))

(defn output! [result]
  (js/Promise.
   (fn [resolve reject]
     (.write (.-stdout js/process) (str (data/write-edn (data/raw-bound result)) "\n")
             (fn [error] (if error (reject error) (resolve nil)))))))

(defn execute-request! [r {:keys [signal output] :or {output output!}}]
  (let [file (.join path (:metadata-dir (:ctx r)) "observers" (str (:observer r) ".edn"))
        run (fn []
              (let [cursor (if (and (not (:explicit-cursor r)) (.existsSync fs file))
                             (map-tool/read-edn (.readFileSync fs file "utf8"))
                             (:cursor r))
                    baseline (if cursor (js/Promise.resolve cursor)
                                 (.then (data/read-changes (:ctx r))
                                        (fn [result]
                                          (when-not (:explicit-cursor r) (checkpoint! file (:cursor result)))
                                          (:cursor result))))]
                (.then baseline
                       (fn [initial]
                         (let [deadline (+ (js/Date.now) (:timeout-ms r))]
                           (letfn [(poll! [cursor]
                                     (when signal (.throwIfAborted signal))
                                     (.then (data/read-changes (:ctx r) (assoc (:filter r) :cursor cursor))
                                            (fn [result]
                                              (let [next-cursor (:cursor result)]
                                                (if (or (false? (:ok result)) (seq (:items result)) (not (:wait r)) (>= (js/Date.now) deadline))
                                                  (let [answer (cond-> result (and (:wait r) (not (false? (:ok result))) (empty? (:items result))) (assoc :timeout true))]
                                                    (.then (js/Promise.resolve (output answer))
                                                           (fn [_]
                                                             (when-not (:explicit-cursor r) (checkpoint! file next-cursor))
                                                             answer)))
                                                  (.then (setTimeout (min (:poll-ms r) (max 1 (- deadline (js/Date.now))))
                                                                     js/undefined (if signal #js {:signal signal} #js {}))
                                                         (fn [_] (poll! next-cursor))))))))]
                             (poll! initial))))))) ]
    (if (:explicit-cursor r) (run) (data/with-file-lock file run {:timeout-ms 0}))))

(defn execute!
  ([request] (execute! request {}))
  ([request opts]
   (.then (js/Promise.resolve nil) (fn [_] (execute-request! request opts)))))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv]
   (let [controller (js/AbortController.)
         stop (fn [] (.abort controller))]
     (.once js/process "SIGINT" stop)
     (.once js/process "SIGTERM" stop)
     (-> (.then (js/Promise.resolve nil) (fn [_] (execute! (options argv) {:signal (.-signal controller)})))
         (.then (fn [_] 0))
         (.catch (fn [error]
                   (if (.-aborted (.-signal controller)) 130
                       (let [message (or (.-message error) (str error))]
                         (.write (.-stdout js/process)
                                 (str (data/write-edn {:ok false :reason (keyword (data/name (or (map-tool/error-field error :reason) :invalid-request)))
                                               :message (subs message 0 (min 300 (count message)))}) "\n"))
                         1))))
         (.finally (fn [] (.removeListener js/process "SIGINT" stop) (.removeListener js/process "SIGTERM" stop)))))))
