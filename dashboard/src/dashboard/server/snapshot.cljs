(ns dashboard.server.snapshot
  "The /api/state snapshot: saved village observations joined with the worlds and bodies."
  (:require ["path" :as path]
            [dashboard.restart :as restart]
            [dashboard.village-data :as village-data]
            [dashboard.worlds :as worlds]
            [dashboard.server.files :refer [read-world-list read-worlds repo-root root worlds-dir]]
            [dashboard.server.responses :refer [send-edn!]]
            [dashboard.server.engine-state :refer [agent-entries agent-names bodies-in refresh-live-engines-in!]]
            [dashboard.server.entities :refer [entity-snapshot-in refresh-entities-in!]]))

;; ---------------------------------------------------------------- saved village observations


(defn village-snapshot [worlds]
  (village-data/snapshot root
                         (vec (mapcat (fn [world]
                                        (map #(assoc % :world (:name world)) (:places world)))
                                      worlds))
                         {:worlds (mapv :name worlds) :worlds-dir worlds-dir :blueprint-dir (.join path repo-root "blueprints")}))

;; ---------------------------------------------------------------- state
(defn world-entry [entries world]
  (let [observations (entity-snapshot-in (:name world) "overworld" entries)]
    (assoc world :humans []
           :entities (:entities observations) :entity-sources (:sources observations)
           :entity-truncated? (:truncated? observations))))

(defn attach-villages [worlds villages]
  (mapv (fn [world]
          (assoc world :places
                 (worlds/readable-places
                  (village-data/attach-status (mapv #(assoc % :world (:name world)) (:places world))
                                              (filterv #(= (:name world) (:world %)) villages)))))
        worlds))

(defn snapshot [world-name entries]
  (let [now (js/Date.now)
        all-bodies (bodies-in now entries)
        names (agent-names all-bodies)
        all-worlds (read-worlds)
        {:keys [villages error]} (village-snapshot all-worlds)
        with-villages (attach-villages all-worlds villages)
        full {:at now
              :agents names
              :bodies all-bodies
              :worlds (mapv #(world-entry entries %) with-villages)
              :villageError error}]
    (assoc (worlds/scope-snapshot full world-name)
           :worldList (read-world-list)
           :selected world-name)))

(defn send-state! [res world-name]
  (let [entries (agent-entries)]
  (refresh-entities-in! world-name entries)
  (-> (refresh-live-engines-in! entries)
      (.then (fn [_] (send-edn! res 200 (assoc (snapshot world-name entries) :build-id restart/build-id))))
      (.catch (fn [e]
                (when-not (.-headersSent res)
                  (send-edn! res 500 {:error (str (ex-message e))})))))))
