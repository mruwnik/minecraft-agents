(ns dashboard.server.entities
  "Background refresh of each body's entity observations over its control socket, and the per-world snapshot."
  (:require [dashboard.edn :as edn]
            [dashboard.entity-observations :as entities]
            ["http" :as http]
            ["path" :as path]
            [dashboard.server.files :refer [file-exists?]]
            [dashboard.server.engine-state :refer [agent-entries body-key engine-dir engine-folder? no-socket-error prune-cache]]))

;; Entity socket refreshes run in the background when dashboard requests arrive. Socket failures
;; preserve only the remaining lifetime of the previously observed entities.
(def entity-refresh-ms 2000)
(def entity-request-ms 1000)
(def entity-response-bytes (* 4 1024 1024))
(def entity-source-cap 128)
(def entity-request-cap 4)
(defonce entity-cache (atom {}))
(defonce entity-in-flight (atom #{}))

(defn entity-socket [body] (.join path (engine-dir body) "control.sock"))

(defn entity-request! [body]
  (js/Promise.
   (fn [resolve reject]
     (let [req (.request http #js {:socketPath (entity-socket body) :method "GET" :path "/entities"}
                         (fn [res]
                           (let [chunks (atom []) size (atom 0)]
                             (.on res "error" reject)
                             (.on res "data"
                                  (fn [chunk]
                                    (swap! size + (.-length chunk))
                                    (if (> @size entity-response-bytes)
                                      (.destroy res (js/Error. "engine entity response exceeds 4 MiB"))
                                      (swap! chunks conj chunk))))
                             (.on res "end"
                                  (fn []
                                    (if (= 200 (.-statusCode res))
                                      (try (resolve (edn/one-form (.toString (js/Buffer.concat (to-array @chunks)) "utf8")))
                                           (catch :default e (reject e)))
                                      (reject (ex-info (str "engine entity API HTTP " (.-statusCode res))
                                                       {:status (.-statusCode res)}))))))))
           timer (js/setTimeout #(.destroy req (js/Error. "engine entity API timed out")) entity-request-ms)]
       (.once req "close" #(js/clearTimeout timer))
       (.on req "error" reject)
       (.end req)))))

(def unsupported-error "This body has no /entities endpoint. Restart it with the current engine build to enable entity observations.")

(defn entity-failure
  "The cache entry after a failed request: previous entities stay until they expire. reason: :unsupported, or a short text."
  [entry reason]
  (assoc entry :online? nil
         :status (if (= :unsupported reason) :unsupported :unavailable)
         :error (if (= :unsupported reason) unsupported-error (str reason))))

(defn refresh-entities-in! [world-name entries]
  (let [now (js/Date.now)
        targets (->> entries (filter #(and (= world-name (:world %)) (engine-folder? %)))
                     (sort-by (fn [b] [(get-in @entity-cache [(body-key b) :requested-at] 0) (:name b)])))
        room (max 0 (- entity-request-cap (count @entity-in-flight)))]
    (swap! entity-cache #(entities/bound (prune-cache % entries) now))
    ;; a body without a socket is settled at once and uses no request slot; only the asked ones are capped
    (let [due (filterv #(and (not (contains? @entity-in-flight (body-key %)))
                             (>= (- now (get-in @entity-cache [(body-key %) :requested-at] 0)) entity-refresh-ms)) targets)
          {asked true silent false} (group-by #(file-exists? (entity-socket %)) due)]
      (doseq [body silent]
        (swap! entity-cache update (body-key body)
               #(entity-failure (assoc % :body (body-key body) :requested-at now) no-socket-error)))
      (doseq [body (take room asked)
              :let [key (body-key body)]]
        (swap! entity-cache update key #(assoc % :body key :requested-at now :status (or (:status %) :loading)))
        (do
          (swap! entity-in-flight conj key)
          (-> (entity-request! body)
              (.then (fn [payload]
                       (swap! entity-cache assoc key (entities/body-snapshot body payload (js/Date.now)))
                       (swap! entity-cache #(entities/bound % (js/Date.now)))))
              (.catch (fn [e]
                        (swap! entity-cache update key entity-failure
                               (if (= 404 (:status (ex-data e)))
                                 :unsupported
                                 (or (.-code e) (subs (str (ex-message e)) 0 (min 80 (count (str (ex-message e))))))))))
              (.finally #(swap! entity-in-flight disj key))))))
    (when (> (count @entity-cache) entity-source-cap)
      (swap! entity-cache #(into {} (take entity-source-cap (sort-by (comp - :requested-at val) %)))))))

(defn refresh-entities! [world-name] (refresh-entities-in! world-name (agent-entries)))

(defn entity-snapshot-in [world-name dimension entries]
  (let [targets (filter #(and (= world-name (:world %)) (engine-folder? %)) entries)
        scoped (into {} (for [body (take entity-source-cap (sort-by :name targets))
                              :let [key (body-key body)]]
                          [key (or (get @entity-cache key) {:body key :status :loading :entities []})]))
        snapshot (entities/merge-world scoped world-name dimension (js/Date.now))
        truncated? (> (count targets) entity-source-cap)]
    (assoc snapshot :source-count (count targets) :source-cap entity-source-cap
           :sources-truncated? truncated? :truncated? (or truncated? (:truncated? snapshot)))))

(defn entity-snapshot [world-name dimension] (entity-snapshot-in world-name dimension (agent-entries)))
