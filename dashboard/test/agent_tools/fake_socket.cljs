(ns agent-tools.fake-socket
  "A fake unix socket for tool tests: a request function to inject into agent-tools.http, answering from a handler."
  (:require ["node:events" :refer [EventEmitter]]))

(defn response [{:keys [status content-type] :or {status 200 content-type "application/edn"}}]
  (let [res (EventEmitter.)]
    (set! (.-statusCode res) status)
    (set! (.-headers res) #js {"content-type" content-type})
    (set! (.-setEncoding res) (fn [_]))
    (set! (.-destroy res) (fn [_]))
    res))

(defn request-fn
  "[request-fn seen]. The handler gets {:socket-path :method :path :body} and returns {:status :content-type :text}
  or {:error code}; seen is an atom of every call in order."
  [handler]
  (let [seen (atom [])]
    [(fn [options callback]
       (let [req (EventEmitter.)
             call {:socket-path (.-socketPath options) :method (.-method options) :path (.-path options)}]
         (set! (.-destroy req) (fn [_]))
         (set! (.-end req)
               (fn [payload]
                 (let [call (assoc call :body payload)
                       reply (handler call)]
                   (swap! seen conj call)
                   (if-let [code (:error reply)]
                     (.emit req "error" (doto (js/Error. code) (aset "code" code)))
                     (let [res (response reply)]
                       (callback res)
                       (.emit res "data" (or (:text reply) ""))
                       (.emit res "end"))))))
         req))
     seen]))

(defn pending-request
  "A request that never answers; destroying it reports the destroy error, like a real socket."
  []
  (let [req (EventEmitter.)]
    (set! (.-end req) (fn [& _]))
    (set! (.-destroy req) (fn [error] (.emit req "error" error)))
    req))
