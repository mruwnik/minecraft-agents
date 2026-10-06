(ns dashboard.server.map
  "The plan and terrain-tile endpoints over the dumped chunk columns (js/worldblocks.mjs)."
  (:require ["fs" :as fs]
            ["path" :as path]
            [dashboard.plan-api :as plan-api]
            [dashboard.tiles :as tiles]
            ["url" :as url]
            [dashboard.server.files :refer [dashboard-dir repo-root state-dir world-names worlds-dir]]
            [dashboard.server.responses :refer [send-json! to-js]]
            [dashboard.server.esm :refer [import-esm]]))

;; ---------------------------------------------------------------- plans (dashboard.plan-api)
;; Plans are worlds/<world>/plans/<id>.edn, the blueprints they place blueprints/<id>.edn at the repo root (next to
;; the legacy .blueprint.json library). The block lookup over the dumped chunk columns is the JS glue js/worldblocks.mjs
;; (the view's column decoders); everything else is ClojureScript.
(def worldblocks-module (or (.-WORLDBLOCKS_MODULE js/process.env) (.join path dashboard-dir "js" "worldblocks.mjs")))
(def plan-blueprints-dir (.join path repo-root "blueprints"))
(def plans-ttl-ms 10000)

(defonce worldblocks-loaded (delay (import-esm (.-href (.pathToFileURL url worldblocks-module)))))
(defonce blocks-by-world (atom {}))
(defonce plan-summaries (atom {}))

(defn blocks-for [module world-name]
  (or (get @blocks-by-world world-name)
      (let [blocks ((.-createWorldBlocks module) #js {:stateDir state-dir :world world-name})]
        (swap! blocks-by-world assoc world-name blocks)
        blocks)))

(defn column-mtime [world-name cx cz]
  (try (js/Math.floor (.-mtimeMs (.statSync fs (.join path worlds-dir world-name "chunks" (str cx "." cz ".bin")))))
       (catch :default _ nil)))

(defn plan-opts [module world-name]
  (let [blocks (blocks-for module world-name)]
    {:dir (.join path worlds-dir world-name "plans")
     :blueprint-dir plan-blueprints-dir
     :block-at (fn [x y z] (.blockAt blocks x y z))
     :column-mtime (fn [cx cz] (column-mtime world-name cx cz))}))

(defn plan-list [module world-name]
  (let [cached (get @plan-summaries world-name)]
    (if (and cached (< (- (js/Date.now) (:at cached)) plans-ttl-ms))
      (:value cached)
      (let [value (assoc (plan-api/summaries (plan-opts module world-name)) :world world-name)]
        (swap! plan-summaries assoc world-name {:at (js/Date.now) :value value})
        value))))

(defn send-plans! [res world-name plan-id]
  (-> @worldblocks-loaded
      (.then (fn [module]
               (if-not plan-id
                 (send-json! res 200 (plan-list module world-name))
                 (if-let [found (plan-api/detail (plan-opts module world-name) plan-id)]
                   (send-json! res 200 found)
                   (send-json! res 404 {:error (str "no plan called " plan-id " in " world-name)})))))
      (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))

;; ---------------------------------------------------------------- terrain tiles (dashboard.tiles)
;; GET /api/tile/<world>/<cx>.<cz>.png: the top-down picture of a dumped column. The JS glue decodes the column (a few ms),
;; dashboard.tiles colours it, the PNG is cached by the column file's mtime in a bounded LRU. A tile is ~100-600 bytes.
(def tile-cache-cap 2048)
(def tile-list-ttl-ms 5000)
(defonce tile-cache (tiles/lru))
(defonce tile-lists (atom {}))
(defonce world-tiles (atom {}))
(defonce tile-stats (atom {:renders 0 :render-ms 0 :hits 0}))

(defn tiles-for [module world-name]
  (or (get @world-tiles world-name)
      (let [made ((.-createWorldTiles module) #js {:stateDir state-dir :world world-name})]
        (swap! world-tiles assoc world-name made)
        made)))

(defn render-tile
  "The PNG of the column, or nil when it is not dumped."
  [module world-name cx cz]
  (let [started (js/performance.now)]
    (when-let [col (.column (tiles-for module world-name) cx cz (to-js tiles/skipped-blocks))]
      (let [png ((.-encodeTile module) tiles/size tiles/size
                 (tiles/tile-rgba {:palette (vec (.-palette col)) :top (.-top col) :floor (.-floor col) :y (.-y col) :depth (.-depth col)}))]
        (swap! tile-stats #(-> % (update :renders inc) (update :render-ms + (- (js/performance.now) started))))
        png))))

(defn cached-tile [module world-name cx cz mtime]
  (let [k (str world-name "/" cx "." cz)
        held (tiles/lru-get! tile-cache k)]
    (if (and held (= mtime (:mtime held)))
      (do (swap! tile-stats update :hits inc) (:png held))
      (when-let [png (render-tile module world-name cx cz)]
        (tiles/lru-put! tile-cache tile-cache-cap k {:mtime mtime :png png})
        png))))

(defn send-tile! [res world-name cx cz]
  (let [mtime (when (some #{world-name} (world-names)) (column-mtime world-name cx cz))]
    (if-not mtime
      (send-json! res 404 {:error (str "column " cx "." cz " of " world-name " is not dumped")})
      (-> @worldblocks-loaded
          (.then (fn [module]
                   (if-let [png (cached-tile module world-name cx cz mtime)]
                     (do (.writeHead res 200 #js {"content-type" "image/png" "cache-control" "public, max-age=3600"
                                                  "x-column-mtime" (str mtime)})
                         (.end res png))
                     (send-json! res 404 {:error (str "column " cx "." cz " of " world-name " is not dumped")}))))
          (.catch (fn [e] (when-not (.-headersSent res) (send-json! res 500 {:error (str (ex-message e))}))))))))

(defn column-entries [world-name]
  (let [dir (.join path worlds-dir world-name "chunks")]
    (vec (for [file (try (.readdirSync fs dir) (catch :default _ #js []))
               :let [mtime (try (.-mtimeMs (.statSync fs (.join path dir file))) (catch :default _ nil))]
               :when mtime]
           [file mtime]))))

(defn tile-entries
  "[[cx cz mtime]] of the world's dumped columns, re-read from the directory at most every 5 s."
  [world-name]
  (let [{:keys [at value]} (get @tile-lists world-name)]
    (if (and at (< (- (js/Date.now) at) tile-list-ttl-ms))
      value
      (let [value (tiles/index-entries (column-entries world-name))]
        (swap! tile-lists assoc world-name {:at (js/Date.now) :value value})
        value))))

(defn send-tiles! [res world-name query]
  (if-not (some #{world-name} (world-names))
    (send-json! res 404 {:error (str "no world called " world-name)})
    (let [since (some-> (.get query "since") (js/parseInt 10))]
      (send-json! res 200 {:world world-name :at (js/Date.now)
                           :tiles (tiles/newer-than (tile-entries world-name) (when-not (js/isNaN since) since))}))))

(defn tile-stats-json []
  (let [{:keys [renders render-ms hits]} @tile-stats]
    {:renders renders :renders-ms-avg (when (pos? renders) (/ render-ms renders)) :hits hits :cached (.-size tile-cache)}))
