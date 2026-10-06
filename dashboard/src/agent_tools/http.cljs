(ns agent-tools.http
  "HTTP over a unix socket for the agent tools: one request with a deadline, a response byte cap and an abort
  signal, rejecting with the error codes the tools report on (ETIMEDOUT, ERESPONSETOOLARGE, ECONNRESET, ABORT_ERR).
  The request function is injectable so tests can fake the socket."
  (:require ["node:http" :as node-http]))

(def edn-content-type (js/RegExp. "^application\\/edn(?:;|$)" "i"))

(defn edn-response?
  "True when the content type is application/edn (parameters allowed)."
  [content-type]
  (.test edn-content-type (or content-type "")))

(def not-found-body #"^\s*\{\s*:ok\s+false\s*,?\s*:reason\s+:not-found\s*\}\s*$")

(defn unsupported-route?
  "True for the bare EDN 404 an engine answers when it has no such route (a build older than the tool)."
  [{:keys [status content-type text]}]
  (boolean (and (= 404 status) (edn-response? content-type) (re-matches not-found-body (or text "")))))

(def transport-reasons
  {"ECONNREFUSED" :no-running-body "ENOENT" :no-running-body "ETIMEDOUT" :timeout
   "ERESPONSETOOLARGE" :response-too-large "EACCES" :socket-access-denied})

(defn transport-reason
  "The tool-facing :reason for a failed socket request, by the error's code; :transport-error when it has no name."
  [error]
  (get transport-reasons (aget error "code") :transport-error))

(defn coded-error [code message]
  (let [error (js/Error. message)]
    (aset error "code" code)
    error))

;; Node request, response and signal objects are foreign (and may be test fakes): their properties are
;; read by name so the advanced compiler never renames them.
(defn invoke [object method & args]
  (.apply (aget object method) object (to-array args)))

(defn content-type-of [res]
  (or (some-> (aget res "headers") (aget "content-type"))
      (when (fn? (aget res "getHeader")) (invoke res "getHeader" "content-type"))
      ""))

(defn default-request-fn [options callback] (.request node-http options callback))

(defn request
  "Send one request; a promise of {:status :content-type :text}. Options: :socket-path :method (default GET) :path
  :headers (map) :body (string) :timeout-ms and :max-bytes (none when absent) :signal :request-fn :label (the word
  error messages start with)."
  [{:keys [socket-path method path headers body timeout-ms max-bytes signal request-fn label]
    :or {method "GET" label "request" request-fn default-request-fn}}]
  (js/Promise.
   (fn [resolve reject]
     (let [settled (volatile! false)
           timer (volatile! nil)
           req (volatile! nil)
           on-abort (volatile! nil)
           finish (fn [settle value]
                    (when-not @settled
                      (vreset! settled true)
                      (some-> @timer js/clearTimeout)
                      (when signal (invoke signal "removeEventListener" "abort" @on-abort))
                      (settle value)))
           fail (fn [error] (finish reject error) (invoke @req "destroy" error))
           bytes (volatile! 0)
           options (clj->js {:socketPath socket-path :method method :path path
                             :headers (cond-> (or headers {}) body (assoc "content-length" (js/Buffer.byteLength body)))})
           respond (fn [res]
                     (let [chunks (array)]
                       (invoke res "setEncoding" "utf8")
                       (invoke res "on" "data"
                            (fn [chunk]
                              (vswap! bytes + (js/Buffer.byteLength chunk))
                              (if (and max-bytes (> @bytes max-bytes))
                                (let [error (coded-error "ERESPONSETOOLARGE" (str label " response exceeded " max-bytes " bytes"))]
                                  (finish reject error)
                                  (invoke res "destroy" error)
                                  (invoke @req "destroy" error))
                                (.push chunks chunk))))
                       (invoke res "on" "aborted" #(finish reject (coded-error "ECONNRESET" (str label " response was aborted"))))
                       (invoke res "on" "error" #(finish reject %))
                       (invoke res "on" "end" #(finish resolve {:status (aget res "statusCode") :content-type (content-type-of res)
                                                       :text (.join chunks "")}))))]
       (vreset! req (request-fn options respond))
       (invoke @req "on" "error" #(finish reject %))
       (when timeout-ms
         (vreset! timer (js/setTimeout
                         #(fail (coded-error "ETIMEDOUT" (str label " request exceeded " timeout-ms " ms")))
                         timeout-ms)))
       (when signal
         (vreset! on-abort #(fail (coded-error "ABORT_ERR" (str label " cancelled"))))
         (invoke signal "addEventListener" "abort" @on-abort #js {:once true}))
       (cond
         (and signal (aget signal "aborted")) (@on-abort)
         body (invoke @req "end" body)
         :else (invoke @req "end"))))))
