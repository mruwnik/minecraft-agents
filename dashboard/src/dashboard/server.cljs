(ns dashboard.server
  "The dashboard server: plain node http on 127.0.0.1; this file routes requests. Engine bodies are read from their local event service
  and engine.edn."
  (:require [dashboard.blueprint-library :as blueprint-lib]
            [dashboard.chat :as chat]
            [dashboard.chat-send :as chat-send]
            [dashboard.entity-observations :as entities]
            [dashboard.guard :as guard]
            ["http" :as http]
            [dashboard.jobs-registry :as jobs-registry]
            ["path" :as path]
            [dashboard.restart :as restart]
            [dashboard.routes :as routes]
            [dashboard.server.files :refer [choose-world port public-dir read-world-list read-worlds repo-root root]]
            [dashboard.server.responses :refer [send-edn! send-file! send-json! serve-static!]]
            [dashboard.server.engine-state :refer [bodies body-key]]
            [dashboard.server.entities :refer [entity-snapshot refresh-entities!]]
            [dashboard.server.snapshot :refer [send-state! village-snapshot]]
            [dashboard.server.chat-log :refer [chat-log chat-sender]]
            [dashboard.server.esm :refer [load-view-mount view-mount view-request?]]
            [dashboard.server.pictures :refer [send-item-icon! send-thumb! send-thumbs-stats! thumbnailer]]
            [dashboard.server.events :refer [resolve-attention! send-events!]]
            [dashboard.server.map :refer [send-plans! send-tile! send-tiles! tile-stats-json]]
            [dashboard.server.posts :refer [preview! request-restart! send-chat! send-whisper!]]))

(def route-list
  "try /, /villagers, /villages, /blueprints, /api/worlds, /api/state, /api/villagers, /api/villages, /api/chat?limit=200, POST /api/chat/send, /api/jobs, /api/plans, /api/plan/<name>, /api/blueprints, /api/blueprint/<name>, POST /api/blueprint-preview (state, chat, world and villages take ?world=<name>, default the first world)")

;; ---------------------------------------------------------------- jobs (GET /api/jobs)
;; The job and trigger namespaces as compiled into this build (dashboard.jobs-registry), joined with usage.
(defn read-jobs [now]
  (jobs-registry/attach-usage jobs-registry/entries (jobs-registry/usage (bodies now))))

(defn blueprint-library []
  (blueprint-lib/library (.join path repo-root "blueprints")))

(defn handle-world-scoped! [res kind world-name query plan-name]
  (case kind
    :state (send-state! res world-name)
    :plans-api (send-plans! res world-name nil)
    :plan-api (send-plans! res world-name plan-name)
    :chat (send-json! res 200 (chat-log (chat/chat-limit (.get query "limit")) world-name))
    :world (send-json! res 501 {:error "unsupported for engine bodies: world scan (needs a body's HTTP API)"})
    :entities-api (do (refresh-entities! world-name)
                      (send-edn! res 200 (entity-snapshot world-name (.get query "dimension"))))
    :villagers-api (do (refresh-entities! world-name)
                       (send-edn! res 200 (entities/villagers (entity-snapshot world-name (.get query "dimension")))))
    :villages-api (let [snap (village-snapshot (filterv #(= world-name (:name %)) (read-worlds)))]
                    (send-edn! res 200 (assoc snap :readOnly true)))))

(def world-kinds #{:state :chat :world :villages-api :villagers-api :entities-api :plans-api :plan-api})
(def edn-library-kinds #{:villages-api :villagers-api :entities-api :blueprints :blueprint :blueprint-preview})

(defn handle! [req res]
  (let [{:keys [kind] blueprint-name :name request-path :path :as route} (routes/route (.-url req))
        query (.-searchParams (js/URL. (.-url req) "http://dashboard"))
        choice (when (world-kinds kind) (choose-world query))]
    (cond
      (:error choice) ((if (edn-library-kinds kind) send-edn! send-json!) res 400 choice)
      (world-kinds kind) (handle-world-scoped! res kind (:name choice) query blueprint-name)
      :else
      (case kind
        :page (send-file! res (.join path public-dir "index.html"))
        :static (serve-static! res request-path)
        :thumb (send-thumb! res (body-key route))
        :item-icon (send-item-icon! res blueprint-name)
        :events (send-events! res (body-key route) query)
        :attention-resolve (resolve-attention! req res (body-key route))
        :chat-send (send-chat! req res)
        :whisper-send (send-whisper! req res (body-key route))
        :restart (request-restart! req res)
        :build-id (send-json! res 200 {:build-id restart/build-id})
        :jobs-api (send-json! res 200 {:at (js/Date.now) :jobs (read-jobs (js/Date.now))})
        :thumbs-stats (send-thumbs-stats! res)
        :tile (send-tile! res (:world route) (:cx route) (:cz route))
        :tiles (send-tiles! res (:world route) query)
        :tile-stats (send-json! res 200 (tile-stats-json))
        :worlds (send-json! res 200 {:worlds (read-world-list)})
        :blueprints (send-edn! res 200 (blueprint-library))
        :blueprint (let [found (first (filter #(= blueprint-name (:name %)) (:blueprints (blueprint-library))))]
                     (if found
                       (send-edn! res 200 found)
                       (send-edn! res 404 {:error (str "no blueprint called " blueprint-name ": /api/blueprints lists them")})))
        :blueprint-preview (preview! req res)
        :unsupported (send-json! res 404 {:error "unsupported for engine bodies"})
        (send-json! res 404 {:error route-list})))))

(defn request-url
  "The parsed request target, or nil when it is malformed."
  [req]
  (try (js/URL. (.-url req) "http://dashboard") (catch :default _ nil)))

(defn dispatch! [req res]
  (try
    (if (view-request? req)
      (.handle @view-mount req res)
      (handle! req res))
    (catch :default e
      (if (.-headersSent res)
        (.end res)
        ;; the target parsed in handler, so routing it again cannot throw
        ((if (edn-library-kinds (:kind (routes/route (.-url req)))) send-edn! send-json!)
         res 500 {:error (str (ex-message e))})))))

(defn handler
  "Every request needs a loopback Host (DNS rebinding) and a parseable target before any route sees it."
  [req res]
  (cond
    (not (guard/local-host? (some-> (.-headers req) (.-host)) port)) (send-json! res 403 {:error "bad Host"})
    (nil? (request-url req)) (send-json! res 400 {:error "bad request target"})
    :else (dispatch! req res)))

(defn close-all!
  "Ends the thumbnail worker and the view server's scan worker (those that were started); resolves when done."
  []
  (js/Promise.all
   #js [(if (realized? thumbnailer)
          (-> @thumbnailer (.then (fn [t] ((:close t)))) (.catch (fn [_])))
          (js/Promise.resolve))
        (if-let [m @view-mount]
          (-> (js/Promise.resolve (.close m)) (.catch (fn [_])))
          (js/Promise.resolve))]))

(defn shutdown-on-signals!
  "SIGTERM and SIGINT close the http server and the workers, then exit (after at most 3 s)."
  [server]
  (let [done (atom false)
        stop (fn [_]
               (when-not @done
                 (reset! done true)
                 (.unref (js/setTimeout #(.exit js/process 1) 3000))
                 (.close server)
                 (-> (close-all!) (.then #(.exit js/process 0)))))]
    (.on js/process "SIGTERM" stop)
    (.on js/process "SIGINT" stop)))

(defn main []
  (when-not (chat-send/valid-sender? chat-sender)
    (js/console.error (str "DASHBOARD_CHAT_AS must match " chat-send/sender-re ", got " (pr-str chat-sender)))
    (.exit js/process 1))
  (let [server (.createServer http handler)]
    (shutdown-on-signals! server)
    (-> (load-view-mount)
        (.then (fn [_]
                 (.listen server port "127.0.0.1"
                          #(println (str "dashboard on http://127.0.0.1:" port " (root " root ")"))))))))
