(ns agent-tools.quote
  "quote.mjs: ask any job (or primitive, jobs.primitive.*) for its quote over the body's events socket."
  (:require [engine.bodies :as bodies]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            [agent-tools.world-data :as data]
            ["node:path" :as path]))

(def usage "usage: quote.mjs <body> --world <world> <job-name> [<args-EDN>] [--worlds DIR --state LEGACY_PARENT]
Asks the body's engine what the job expects to use and yield for a call with those args, without running it: {:ok true :job .. :quote {:uses {:job-time [least mean most] ..} :yields ..}}. ##Inf means unbounded. Find a job's target, ways, defaults and args with observe.mjs <body> catalog job <name>.")

(def request-timeout-ms 10000)
(def max-response-bytes 65536)

(defn fail [message] (throw (js/Error. message)))

(defn request-for-unsafe [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options (vec argv) {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}})
        [body job args & extra] positionals]
    (when-not (and body (re-matches bodies/name-re body)) (fail "body must be a valid name"))
    (when (nil? (:world values)) (fail (bodies/missing-world-error "--world")))
    (when-not (re-matches bodies/name-re (:world values)) (fail "world must be a valid name"))
    (when-not (and (string? job) (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)+" job)) (fail "quote needs an exact job name such as jobs.movement.go-to"))
    (when (seq extra) (fail "quote takes a job name and at most one args map"))
    (when (and args (> (.byteLength js/Buffer args) 8000)) (fail "args must be at most 8000 bytes"))
    (let [state (bodies/storage-root values map-tool/default-state-dir)]
      {:socket-path (.join path (bodies/body-dir state (:world values) body) "engine" "events.sock")
       :path (str "/quote?name=" (js/encodeURIComponent job) (when args (str "&args=" (js/encodeURIComponent args))))})))

(defn request-for [argv]
  (try (request-for-unsafe argv) (catch :default error {:error (.-message error)})))

(defn print-text! [text] (.write (.-stdout js/process) text))

(defn main!
  "Print the engine's quote and resolve to the exit code. Options (for tests): :output, :request-fn."
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [output request-fn] :or {output print-text!}}]
   (let [r (request-for argv)
         print! #(js/Promise.resolve (output (str % "\n")))]
     (if (:error r)
       (http/print-bad-args! output (:error r) usage)
       (-> (http/request (cond-> {:socket-path (:socket-path r) :path (:path r) :timeout-ms request-timeout-ms
                                  :max-bytes max-response-bytes :label "quote"}
                           request-fn (assoc :request-fn request-fn)))
           (.then (fn [{:keys [status content-type text] :as response}]
                    (cond
                      (http/unsupported-route? response)
                      (-> (print! (data/write-edn {:ok false :reason :quote-unavailable :action :restart-with-current-build}))
                          (.then (constantly 2)))
                      (not (http/edn-response? content-type))
                      (-> (print! (data/write-edn {:ok false :reason :bad-response})) (.then (constantly 2)))
                      :else (-> (print! text) (.then (constantly (if (<= 200 status 299) 0 1)))))))
           (.catch (fn [error]
                     (-> (print! (data/write-edn {:ok false :reason (http/transport-reason error)})) (.then (constantly 2))))))))))
