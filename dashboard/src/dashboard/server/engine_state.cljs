(ns dashboard.server.engine-state
  "The engine bodies as the dashboard shows them: event socket reads, engine.edn, pose and hud files, per-body caches."
  (:require [engine.bodies :as bodies]
            [dashboard.engine-events :as ee]
            [dashboard.engine-edn :as engine-edn]
            ["fs" :as fs]
            ["http" :as http]
            ["path" :as path]
            [cljs.reader :as reader]
            [dashboard.view-info :as view-info]
            [dashboard.server.files :refer [file-exists? read-text state-dir]]))

;; ---------------------------------------------------------------- engine bodies
;; A body is addressed by {:world :name} (a name is unique only within a world); agent entries carry both, and every
;; cache below is keyed by (body-key body).
(defn body-key [body] (select-keys body [:world :name]))
(defn body-dir [{:keys [world name]}] (bodies/body-dir state-dir world name))

;; Per body: the parsed engine.edn, and how far into events.edn the chat reader has read.
(def edn-cache (atom {}))
(def tails (atom {}))

(defn engine-dir [body] (.join path (body-dir body) "engine"))
(defn canonical-events-file [body] (.join path (engine-dir body) "events.edn"))
(defn events-socket [body] (.join path (engine-dir body) "events.sock"))
(defn engine-folder? [body]
  (or (file-exists? (.join path (engine-dir body) "engine.edn"))
      (file-exists? (canonical-events-file body))
      (file-exists? (events-socket body))))
(defn canonical-engine? [body]
  (or (file-exists? (canonical-events-file body)) (file-exists? (events-socket body))))

;; The engine owns event ordering, retention and the durable attention inbox. Dashboard caches only the last
;; serialized snapshot and its fold; every request refreshes from the socket before exposing live state.
(def live-engines (atom {}))
(def live-refreshing (atom {}))
(def live-errors (atom {}))
(def event-page-size 1000)
(def event-socket-timeout-ms 1500)
(def event-response-bytes (* 4 1024 1024))
(declare agent-entries)

(defn edn-response [text]
  (try (reader/read-string text) (catch :default e (throw (js/Error. (str "bad EDN from engine: " (ex-message e)))))))

(defn event-socket-request! [target method request-path body]
  (js/Promise.
   (fn [resolve reject]
     (let [text (when (some? body) (pr-str body))
           headers (when text #js {"content-type" "application/edn" "content-length" (js/Buffer.byteLength text)})
           options #js {:socketPath (events-socket target) :method method :path request-path :headers (or headers #js {})}
           req (.request
                http options
                (fn [res]
                  (let [chunks (atom []) size (atom 0)]
                    (.on res "error" reject)
                    (.on res "data"
                         (fn [chunk]
                           (swap! size + (.-length chunk))
                           (if (> @size event-response-bytes)
                             (.destroy res (js/Error. "engine event response exceeds 4 MiB"))
                             (swap! chunks conj chunk))))
                    (.on res "end"
                         (fn []
                           (let [status (.-statusCode res)
                                 response-text (.toString (js/Buffer.concat (to-array @chunks)) "utf8")]
                             (if (and (>= status 200) (< status 300))
                               (try (resolve (edn-response response-text)) (catch :default e (reject e)))
                               (reject (js/Error. (str "engine event API HTTP " status))))))))))]
       (.on req "error" reject)
       (.setTimeout req event-socket-timeout-ms #(.destroy req (js/Error. "engine event API timed out")))
       (.end req text)))))

(defn event-page! [body stream-id after limit]
  (event-socket-request! body "GET"
                         (str "/events?stream-id=" (js/encodeURIComponent stream-id)
                              "&after=" after "&limit=" limit)
                         nil))

(defn state-cursor [snapshot]
  (let [cursor (:cursor snapshot)]
    {:stream-id (:stream-id cursor) :seq (or (:seq cursor) 0)}))

(defn read-live-tail! [body snapshot after]
  (let [{:keys [stream-id seq]} (state-cursor snapshot)
        after (max 0 after)]
    (-> (event-page! body stream-id after event-page-size)
        (.then (fn [page]
                 (if-not (:gap? page)
                   {:page page :after after :snapshot snapshot :reset? false}
                   ;; Retention or a replaced stream: take a fresh snapshot, reconcile its inbox, then read a
                   ;; retained tail from the same stream rather than pretending the missing range was delivered.
                   (-> (event-socket-request! body "GET" "/snapshot" nil)
                       (.then (fn [fresh]
                                (let [{new-stream :stream-id head :seq} (state-cursor fresh)
                                      oldest (or (:oldest-seq page) 1)
                                      newest (or (:latest-seq page) head)
                                      tail-after (max (dec oldest) (- newest (dec event-page-size)))]
                                  (-> (event-page! body new-stream tail-after event-page-size)
                                      (.then (fn [tail] {:page tail :after tail-after :snapshot fresh :reset? true})))))))))))))

(defn last-seq [events fallback]
  (or (:seq (peek (vec events))) fallback 0))

(defn refresh-live-engine! [body]
  (or (get @live-refreshing (body-key body))
      (let [name (body-key body)
            promise
            (-> (event-socket-request! body "GET" "/snapshot" nil)
                (.then (fn [snapshot]
                         (let [previous (get @live-engines name)
                               cursor (state-cursor snapshot)
                               same-stream? (= (:stream-id cursor) (get-in previous [:cursor :stream-id]))
                               same-generation? (= (:generation-id snapshot) (:generation-id previous))
                               local-reset? (not (and same-stream? same-generation?))
                               after (if local-reset?
                                       (max 0 (- (:seq cursor) (dec event-page-size)))
                                       (get-in previous [:cursor :seq] 0))]
                           (-> (read-live-tail! body snapshot after)
                               (.then (fn [{:keys [page snapshot] gap-reset? :reset?}]
                                        (let [events (:events page)
                                              actual-reset? (or local-reset? gap-reset?)
                                              folded (ee/fold-engine (if actual-reset? ee/empty-engine (:folded previous)) events)
                                              combined (if actual-reset? events (into (vec (:events previous)) events))
                                              page-seq (last-seq events after)
                                              next-cursor {:stream-id (:stream-id (state-cursor snapshot))
                                                           :seq page-seq}
                                              cache {:snapshot snapshot :generation-id (:generation-id snapshot) :cursor next-cursor :folded folded
                                                     :events (vec (take-last event-page-size combined))
                                                     :reset? actual-reset?}]
                                          (swap! live-errors dissoc name)
                                          (swap! live-engines assoc name cache)
                                          cache)))))))
                (.catch (fn [e]
                          (swap! live-errors assoc name (ex-message e))
                          (get @live-engines name)))
                (.finally (fn [] (swap! live-refreshing dissoc name))))]
        (swap! live-refreshing assoc name promise)
        promise)))

(def no-socket-error "no events socket")

;; Only a body whose events.sock exists is asked. One without is down: marked once, and asked again the poll its socket appears.
(defn refresh-live-engines-in! [entries]
  (let [targets (->> entries (map body-key) (filter canonical-engine?) vec)
        {asked true down false} (group-by #(file-exists? (events-socket %)) targets)]
    (doseq [body down] (swap! live-errors assoc body no-socket-error))
    (js/Promise.all (clj->js (map refresh-live-engine! asked)))))

;; engine.edn is re-read and re-parsed only when its mtime or size changed: {:text :read} (read = engine-edn/read-text)
(defn read-edn-file [body]
  (let [file (.join path (engine-dir body) "engine.edn")
        k (body-key body)]
    (try
      (let [st (.statSync fs file)
            stamp [(.-mtimeMs st) (.-size st)]
            cached (get @edn-cache k)]
        (if (= stamp (:stamp cached))
          cached
          (let [text (.readFileSync fs file "utf8")
                entry {:stamp stamp :text text :read (engine-edn/read-text text)}]
            (swap! edn-cache assoc k entry)
            entry)))
      (catch :default _ nil))))

(defn read-edn-text [body] (:text (read-edn-file body)))

(defn edn-fields [body now]
  (let [summary (engine-edn/summarize-read (or (:read (read-edn-file body)) (engine-edn/read-text nil)) now)]
    (if (:error summary)
      {:edn-error (:error summary)}
      summary)))

;; ---------------------------------------------------------------- views (pose.json, hud.json)
;; Each file is re-read only when its mtime changed; a missing or unreadable file is nil.
(def view-cache (atom {}))

(defn view-file [body file] (.join path (body-dir body) "view" file))

(defn parse-js [text] (try (js/JSON.parse text) (catch :default _ nil)))

;; pose.json carries every nearby entity and changes often: only the few fields we show are converted
(defn pose-fields [o]
  (when o
    {:t (.-t o) :status (.-status o) :dimension (.-dimension o) :pos (js->clj (.-pos o) :keywordize-keys true)
     :villagers (view-info/villagers (js->clj (.-entities o) :keywordize-keys true))}))

(defn read-view-file [body file convert]
  (let [full (view-file body file)
        k (body-key body)]
    (try
      (let [mtime (.-mtimeMs (.statSync fs full))
            cached (get-in @view-cache [k file])]
        (if (= mtime (:mtime cached))
          cached
          (let [entry {:mtime mtime :value (convert (parse-js (.readFileSync fs full "utf8")))}]
            (swap! view-cache assoc-in [k file] entry)
            entry)))
      (catch :default _ nil))))

(defn read-view [body]
  (let [pose (read-view-file body "pose.json" pose-fields)
        hud (read-view-file body "hud.json" #(some-> % (js->clj :keywordize-keys true)))]
    (view-info/summarize (:value pose) (:value hud) (:mtime pose))))

(defn build-engine-body [agent now]
  (let [body (body-key agent)
        live (get @live-engines body)
        canonical? (canonical-engine? body)
        live-error (get @live-errors body)
        persisted-state (when canonical?
                          (let [{:keys [value]} (:read (read-edn-file body))]
                            (when (map? value) value)))
        folded (or (:folded live) ee/empty-engine)
        engine-view (ee/engine-view folded now)
        pose-view (read-view body)
        snap (:snapshot live)
        authoritative-state (if (and snap (not live-error)) (:state snap) persisted-state)
        scheduler-summary (if (and canonical? (map? authoritative-state))
                            (engine-edn/summarize-read {:value authoritative-state} now)
                            (edn-fields body now))
        outstanding (if (and snap (not live-error))
                      (or (:outstanding snap) {})
                      (or (:attention persisted-state) {}))
        position (or (:position snap) (:pos engine-view))
        offline? (or (and canonical? live-error)
                     (true? (:offline snap))
                     (and canonical? (nil? snap)))
        view (ee/body-view engine-view {:position position
                                        :cursor (:cursor live)
                                        :generation-id (:generation-id snap)
                                        :outstanding outstanding
                                        :scheduler-summary scheduler-summary
                                        :offline? offline?
                                        :snap-present? (some? snap)
                                        :settling? (boolean (:settling snap))})]
    (assoc (ee/engine-body agent (ee/with-view-status view pose-view))
           :state (when position {:pos position})
           :outstanding outstanding
           :view pose-view)))

;; A body with no events.sock is built again only when an input changed: the agent, its live state, a stamp of each file
;; the build reads, or the minute (ages and cooldowns are shown coarse for an offline body).
(def body-cache (atom {}))
(def offline-cache-ms 60000)

(defn file-stamp [file]
  (let [st (.statSync fs file #js {:throwIfNoEntry false})]
    (when st [(.-mtimeMs st) (.-size st)])))

(defn offline-key [agent body now sock-stamp]
  (let [dir (engine-dir body)]
    [agent (quot now offline-cache-ms) (get @live-engines body) (get @live-errors body)
     (mapv #(file-stamp (.join path dir %)) ["engine.edn" "events.edn"])
     sock-stamp
     (mapv #(file-stamp (view-file body %)) ["pose.json" "hud.json"])]))

(defn engine-body [agent now]
  (let [body (body-key agent)
        sock-stamp (file-stamp (events-socket body))]
    (if sock-stamp
      (build-engine-body agent now)
      (let [k (offline-key agent body now sock-stamp)
            cached (get @body-cache body)]
        (if (= k (:key cached))
          (:value cached)
          (let [value (build-engine-body agent now)]
            (swap! body-cache assoc body {:key k :value value})
            value))))))

(defn prune-cache
  "The cache without the bodies that no longer have a folder (entries: one listing of the bodies)."
  [cache entries]
  (let [live (set (map body-key entries))]
    (into {} (filter (fn [[key _]] (contains? live key))) cache)))

;; every body folder of every world: {:world :name :text raw config.json}
(defn agent-entries []
  (mapv (fn [{:keys [world name dir]}] {:world world :name name :text (read-text (.join path dir "config.json"))})
        (bodies/list-bodies state-dir)))

(defn bodies-in [now entries]
  (let [engine? (filter engine-folder? entries)
         other (remove engine-folder? entries)]
     (swap! body-cache #(prune-cache % entries))
     (vec (concat (map #(engine-body % now) (ee/parse-engine-agents engine?))
                  (map ee/unsupported-body (ee/parse-engine-agents other))))))

(defn bodies [now] (bodies-in now (agent-entries)))

(defn agent-names [bodies]
  (vec (distinct (mapcat (juxt :name :username) bodies))))
