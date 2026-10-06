(ns view.scene
  "One scene of the browser view: follows an agent's pose (its own /pose stream, or events fed by the hub's shared one), keeps
   the toroidal window of chunk columns (view.window) filled (fetch, decode on the shared pool, upload into its GL world)
   within the fetch and decode limits, and says where the camera is. It does not draw: the page (app.mjs) or the hub
   (view.hub) calls frame(now) and draws the returned params with renderer.draw(scene.world, params).

   The GL world, the decoder pool, the block tables and the camera and shading maths are JS; scene.mjs hands them in.
   frame, drew and the upload step run every animation frame, so they are written in the tuned style (see view.interp):
   JS Maps, Sets, arrays and objects mutated in place, no persistent data.

     createScene({agent, radius, fov, interp, world, decoder, tables, baseUrl, maxDist, urlParams, finishForLatency,
                  ownStream, finish, cameraBasis, sceneTime, skyDarken, fetch})
       -> {id, agent, radius, fov, world, metrics, interp, frame, drew, stats, close, pose, hud, counts, isReady, feed}

   options: interp (boolean, default true); ownStream (default true: the scene opens /pose/<world>/<name> itself; false: whoever
   owns a shared /poses stream passes each event of this agent to feed(event, data), as the hub does, because a browser allows
   only 6 HTTP/1.1 connections per origin); finishForLatency (finish(), gl.finish, before stamping latencies: the single-view
   page's measurement; off for many scenes); urlParams (?time / ?rain overrides for sceneTime); fetch (default js/fetch)."
  (:require [view.interp :as interp]
            [view.schedule :as s]
            [view.window :as w]))

(set! *warn-on-infer* true)

(def ^:const MAX-IN-FLIGHT 8) ; column fetches at once, per scene
(def ^:const FETCH-RETRIES 4) ; retries of a failed column fetch before the column counts as missing
(def ^:const MAX-DECODING 12) ; columns fetched and waiting for a decode, per scene
(def ^:const UPLOAD-BUDGET-MS 4) ; GPU uploads per frame, at least one
(def ^:const COLUMN-DRAWN-KEEP 500)
(def ^:const LATENCY-KEEP 200)
(def ^:const TRACE-KEEP 4000)
(def ^:const DECODE-KEEP 2000)
(def ^:const TIMING-KEEP 2000)
(def ^:const OFFLINE-LATENCY-MS 60000) ; an older file is an offline pose, not a live write
(def ^:const MAX-ENTITIES 64)

(defn perf-now [] (js/performance.now))
(defn round-to ^number [^number v ^number scale] (/ (js/Math.round (* v scale)) scale))

;; ---- the decoder pool is shared, so its job keys carry the scene id; the pool asks here which job is nearest ----
(defonce live (js/Map.)) ; scene id -> () => the scene's columns Map
(defonce next-id (volatile! 1))

(defn decode-key [id k] (str id "|" k))

(defn decode-priority
  "The dist of the column a decoder job key (\"<scene id>|<column key>\") is for; Infinity when it is no longer wanted."
  [job-key]
  (let [at (.indexOf job-key "|")
        columns-of (.get live (.slice job-key 0 at))
        ^js column (when (some? columns-of) (.get ^js (columns-of) (.slice job-key (inc at))))]
    (if (some? column) (.-dist column) js/Infinity)))

;; ---- entities as boxes ----

(defn entity-color [^js e]
  (let [type (.-type e)
        kind (.-kind e)]
    (cond
      (or (= type "player") (= kind "player") (.-username e)) #js [0.2 0.4 0.95]
      (or (= type "hostile") (= kind "Hostile mobs")) #js [0.88 0.14 0.14]
      (or (= type "animal") (= type "passive") (= kind "Passive mobs") (= kind "Animals")) #js [0.55 0.38 0.22]
      (or (= type "item") (= (.-name e) "item") (= kind "Drops")) #js [1 0.88 0.16]
      :else #js [0.5 0.5 0.5])))

(defn entity-kind
  "What the views write and colour a mob by: \"player\", \"hostile\", \"item\", else its type (nil when it has none)."
  [^js e]
  (let [type (.-type e)
        kind (.-kind e)]
    (cond
      (or (= type "player") (= kind "player") (.-username e)) "player"
      (or (= type "hostile") (= kind "Hostile mobs")) "hostile"
      (or (= type "item") (= (.-name e) "item") (= kind "Drops")) "item"
      :else type)))

(defn or-default [v fallback] (if (nil? v) fallback v))

(defn by-d ^number [^js a ^js b] (- (.-d a) (.-d b)))

(defn entity-boxes
  "The nearest 64 entities to the eye as {min, max, color, name, label, item, kind} boxes relative to origin (name, label, item and kind are what
   the browser view's species colours and name labels go by)."
  [^js entities ^js origin ^js eye]
  (if (nil? entities)
    #js []
    (let [near (.sort (.map entities (fn [^js e]
                                       (let [^js p (.-pos e)]
                                         #js {:e e :d (js/Math.hypot (- (.-x p) (.-x eye)) (- (.-y p) (.-y eye)) (- (.-z p) (.-z eye)))})))
                      by-d)]
      (.map (.slice near 0 MAX-ENTITIES)
            (fn [^js item]
              (let [^js e (.-e item)
                    ^js p (.-pos e)
                    half (/ (or-default (.-width e) 0.6) 2)
                    x (- (.-x p) (.-x origin))
                    y (- (.-y p) (.-y origin))
                    z (- (.-z p) (.-z origin))]
                #js {:min #js [(- x half) y (- z half)]
                     :max #js [(+ x half) (+ y (or-default (.-height e) 1.8)) (+ z half)]
                     :color (entity-color e)
                     :name (.-name e)
                     :label (.-username e)
                     :item (.-item e)
                     :kind (entity-kind e)}))))))

(defn new-metrics []
  #js {:fps 0 :frames 0 :latencies #js [] :shownLatencies #js [] :camTrace #js [] :underruns 0 :decodeMs #js [] :lightMs #js []
       :uploadMs #js [] :columnDrawn #js [] :retargetMs #js [] :slowUploads #js [] :mainDecode #js {:ms 0 :columns 0}
       :loaded 0 :wanted 0 :ready false})

(defn option [^js options k fallback]
  (let [v (unchecked-get options k)]
    (if (nil? v) fallback v)))

(defn keep-timing! [^js xs ms] (s/keep! xs (round-to ms 10) TIMING-KEEP))
(defn keep-latency! [^js xs v] (s/keep! xs (js/Math.round v) LATENCY-KEEP))

(defn agent-path
  "agent is <world>/<name>: each part is encoded, the slash between them is a path separator."
  [agent]
  (.join (.map (.split agent "/") js/encodeURIComponent) "/"))

(defn loaded-count
  "How many columns of the Map are \"loaded\" (runs every frame: an iterator loop, no seq)."
  [^js columns]
  (let [it (.values columns)]
    (loop [n 0]
      (let [^js r (.next it)]
        (if (.-done r)
          n
          (recur (if (= "loaded" (.-status ^js (.-value r))) (inc n) n)))))))

(defn nearest-upload
  "The entry [key, {column, result, mtime}] of uploads whose column is nearest the eye; nil when there is none."
  [^js uploads]
  (let [it (.entries uploads)]
    (loop [best nil best-dist js/Infinity]
      (let [^js step (.next it)]
        (if (.-done step)
          best
          (let [^js entry (.-value step)
                d (.-dist ^js (.-column ^js (aget entry 1)))]
            (if (< d best-dist) (recur entry d) (recur best best-dist))))))))

(defn nearest-need
  "The key in needs, not in flight and still wanted, of the column nearest the eye; nil when there is none."
  [^js needs ^js in-flight ^js columns]
  (let [it (.keys needs)]
    (loop [best nil best-dist js/Infinity]
      (let [^js step (.next it)]
        (if (.-done step)
          best
          (let [k (.-value step)
                ^js column (.get columns k)]
            (if (and (some? column) (not (.has in-flight k)) (< (.-dist column) best-dist))
              (recur k (.-dist column))
              (recur best best-dist))))))))

(defn create-scene [^js options]
  (let [agent (.-agent options)
        radius (option options "radius" 2)
        fov (option options "fov" 70)
        interp-on (option options "interp" true)
        ^js world (.-world options)
        ^js decoder (.-decoder options)
        ^js tables (.-tables options)
        base-url (option options "baseUrl" "")
        max-dist (option options "maxDist" (* radius 16))
        url-params (option options "urlParams" (js/URLSearchParams.))
        finish-for-latency (option options "finishForLatency" false)
        own-stream (option options "ownStream" true)
        finish (option options "finish" (fn []))
        camera-basis (.-cameraBasis options)
        scene-time (.-sceneTime options)
        sky-darken (.-skyDarken options)
        fetch-url (option options "fetch" (fn [url] (js/fetch url))) ; not `fetch`: that local would shadow js/fetch
        id (str @next-id)
        _ (vswap! next-id inc)
        N (inc (* 2 radius))
        ^js interp (interp/pose-interpolator)
        ^js metrics (new-metrics)
        ;; the scene's state, mutated in place
        ^js st #js {:pose nil
                    :hud nil
                    :table nil ; the version's block table; null until loaded, and for a scene whose version the renderer refused
                    :dims nil ; {height, minY}
                    :ccx nil
                    :ccz nil
                    :owners (js/Map.) ; slot "sx.sz" -> column key
                    :columns (js/Map.) ; wanted column key -> column (view.window)
                    :needs (js/Map.) ; column key -> sequence of the request that would satisfy it
                    :inFlight (js/Set.) ; columns being fetched
                    :decoding (js/Map.) ; column key -> sequence of the fetch whose bytes are queued or being decoded
                    :uploads (js/Map.) ; column key -> {column, result, mtime} decoded and waiting for the GPU
                    :eventMtimes (js/Map.) ; column key -> mtime of the latest column event not yet fetched
                    :seq 0
                    :pendingMtime nil
                    :pendingShown #js [] ; {mtime, t} of poses not yet displayed
                    :uploaded #js [] ; {key, mtime} uploaded by the current frame
                    :uploadCount 0
                    :closed false
                    :source nil}
        next-seq! (fn [] (set! (.-seq st) (inc (.-seq st))) (.-seq st))
        owns? (fn [k ^js column] (= k (.get (.-owners st) (w/slot-key (.-cx column) (.-cz column) N))))

        fetch-column (fn [world-name cx cz]
                       (.then (fetch-url (str base-url "/columns/" world-name "/" cx "." cz ".bin"))
                              (fn [^js res]
                                (cond
                                  (= 404 (.-status res)) nil
                                  (not (.-ok res)) (throw (js/Error. (str "column " cx "." cz ": HTTP " (.-status res))))
                                  :else (.then (.arrayBuffer res) #(js/Uint8Array. %))))))

        ensure-dims! (fn [^js header]
                       (when (nil? (.-dims st))
                         (set! (.-dims st) #js {:height (.-worldHeight header) :minY (.-minY header)})
                         (.allocate world N (.-worldHeight header))))

        ;; a decoded column waits here for the frame's upload step
        settle! (fn [k ^js column result mtime]
                  (when (owns? k column) ; else the slot changed hands while fetching
                    (if (nil? result)
                      (set! (.-status column) "missing")
                      (.set (.-uploads st) k #js {:column column :result result :mtime mtime}))))

        ;; uploads the nearest decoded columns until the frame's budget is spent; returns the {key, mtime} drawn by this frame
        upload-step! (fn []
                       (let [started (perf-now)
                             uploaded #js []
                             ^js uploads (.-uploads st)]
                         (loop [n 0] ; uploads this frame
                           (let [^js next (when (or (zero? n) (< (- (perf-now) started) UPLOAD-BUDGET-MS)) (nearest-upload uploads))]
                             (when (some? next)
                               (let [k (aget next 0)
                                     ^js item (aget next 1)
                                     ^js column (.-column item)
                                     ^js result (.-result item)
                                     mtime (.-mtime item)]
                                 (.delete uploads k)
                                 (if-not (and (identical? (.get (.-columns st) k) column) (owns? k column))
                                   (recur n)
                                   (let [_ (ensure-dims! (.-header result))
                                         upload-started (perf-now)
                                         _ (.uploadColumn world (w/modulo (.-cx column) N) (w/modulo (.-cz column) N)
                                                          (.-mats result) (.-flags result) (.-light result) (.-biomes result))
                                         upload-ms (- (perf-now) upload-started)]
                                     (set! (.-uploadCount st) (inc (.-uploadCount st)))
                                     (keep-timing! (.-uploadMs metrics) upload-ms)
                                     (when (> upload-ms UPLOAD-BUDGET-MS)
                                       (.push (.-slowUploads metrics) #js {:ms (round-to upload-ms 10) :nthInFrame n
                                                                           :queued (.-size uploads) :total (.-uploadCount st)}))
                                     (set! (.-status column) "loaded")
                                     (when (some? mtime) (.push uploaded #js {:key k :mtime mtime}))
                                     (recur (inc n))))))))
                         uploaded))

        pump-ref (volatile! nil)
        pump! (fn [] (@pump-ref))

        record-decode! (fn [^js result]
                         (let [^js main (.-mainDecode metrics)]
                           (s/keep! (.-decodeMs metrics) (round-to (.-ms result) 10) DECODE-KEEP)
                           (keep-timing! (.-lightMs metrics) (.-lightMs result))
                           (set! (.-ms main) (+ (.-ms main) (.-mainMs result)))
                           (set! (.-columns main) (inc (.-columns main)))))

        decode! (fn [k ^js column bytes sq mtime]
                  (.set (.-decoding st) k sq)
                  (pump!)
                  (.then (.decode decoder (decode-key id k) bytes)
                         (fn [result]
                           ;; cancelled, or replaced by a newer fetch of the same column
                           (when-not (or (.-closed st) (not= sq (.get (.-decoding st) k)))
                             (.delete (.-decoding st) k)
                             (when (some? result) (record-decode! result))
                             (when (identical? (.get (.-columns st) k) column) (settle! k column result mtime))
                             (pump!)))))

        ;; a failed fetch (network, HTTP 5xx) is retried after a doubling delay, FETCH-RETRIES times; a 404 is not an error
        retries (volatile! {})
        retry-ms (option options "retryMs" 500)
        retry-later! (fn [k ^js column sq mtime]
                       (let [n (get @retries k 0)]
                         (if (>= n FETCH-RETRIES)
                           (do (vswap! retries dissoc k)
                               (settle! k column nil mtime))
                           (do (vswap! retries assoc k (inc n))
                               (js/setTimeout
                                (fn []
                                  (when (and (not (.-closed st)) (identical? (.get (.-columns st) k) column) (not (.has (.-needs st) k)))
                                    (when (some? mtime) (.set (.-eventMtimes st) k mtime))
                                    (.set (.-needs st) k (next-seq!))
                                    (pump!)))
                                (* retry-ms (js/Math.pow 2 n)))))))

        start-fetch! (fn [k]
                       (let [^js column (.get (.-columns st) k)
                             sq (.get (.-needs st) k)
                             mtime (.get (.-eventMtimes st) k)]
                         (.delete (.-eventMtimes st) k)
                         (.add (.-inFlight st) k)
                         (-> (fetch-column (.-world ^js (.-pose st)) (.-cx column) (.-cz column))
                             (.catch (fn [error] (js/console.error (str "column " k ":") error) ::failed))
                             (.then (fn [bytes]
                                      (.delete (.-inFlight st) k)
                                      (when-not (.-closed st)
                                        (when (= sq (.get (.-needs st) k)) (.delete (.-needs st) k))
                                        (cond
                                          (not (identical? (.get (.-columns st) k) column)) (pump!)
                                          (= ::failed bytes) (do (retry-later! k column sq mtime) (pump!))
                                          (nil? bytes) (do (vswap! retries dissoc k) (settle! k column nil mtime) (pump!))
                                          :else (do (vswap! retries dissoc k) (decode! k column bytes sq mtime)))))))))

        ;; fetches the nearest needed columns while the fetch and decode limits allow
        pump (fn []
               (loop []
                 (when (and (not (.-closed st)) (< (.-size (.-inFlight st)) MAX-IN-FLIGHT)
                            (< (.-size (.-decoding st)) MAX-DECODING) (some? (.-table st)))
                   (let [k (nearest-need (.-needs st) (.-inFlight st) (.-columns st))]
                     (when (some? k)
                       (start-fetch! k)
                       (recur))))))
        _ (vreset! pump-ref pump)

        drop-unwanted! (fn [^js m ^js columns on-drop]
                         (doseq [k (vec (es6-iterator-seq (.keys m)))]
                           (when-not (.has columns k)
                             (.delete m k)
                             (on-drop k))))

        ;; the window follows the eye's chunk; a slot whose owner changes is zeroed at once and refilled when its fetch lands
        retarget! (fn [ccx ccz]
                    (set! (.-ccx st) ccx)
                    (set! (.-ccz st) ccz)
                    (let [^js moved (w/move-window ccx ccz radius (.-columns st) (.-owners st))
                          ^js columns (.-columns moved)]
                      (doseq [^js column (.-fresh moved)]
                        (when (some? (.-dims st)) (.clearSlot world (w/modulo (.-cx column) N) (w/modulo (.-cz column) N)))
                        (.set (.-needs st) (w/key-of (.-cx column) (.-cz column)) (next-seq!)))
                      (drop-unwanted! (.-needs st) columns (fn [_]))
                      (drop-unwanted! (.-decoding st) columns (fn [k] (.cancel decoder (decode-key id k))))
                      (drop-unwanted! (.-uploads st) columns (fn [_]))
                      (drop-unwanted! (.-eventMtimes st) columns (fn [_]))
                      (set! (.-columns st) columns)
                      (pump)))

        on-column-event! (fn [^js data]
                           (let [k (w/key-of (.-cx data) (.-cz data))]
                             (when (.has (.-columns st) k)
                               (.set (.-eventMtimes st) k (.-mtime data))
                               (.set (.-needs st) k (next-seq!))
                               (pump))))

        ;; the world's biome colours, once per scene; without them the world keeps the fixed group colours
        load-biomes! (fn [world-name]
                       (-> (fetch-url (str base-url "/biomes/" world-name ".json"))
                           (.then (fn [^js r] (when (.-ok r) (.json r))))
                           (.catch (fn [_] nil))
                           (.then (fn [^js body]
                                    (when-not (or (.-closed st) (.setBiomes world body))
                                      (js/console.warn (str "biomes: no colour table for world " world-name
                                                            (if (some? (some-> body .-reason)) (str " (" (.-reason body) ")") "")
                                                            ": fixed tint colours")))))))

        follow! (fn [^js pose]
                  (let [ccx (js/Math.floor (/ (.. pose -eye -x) 16))
                        ccz (js/Math.floor (/ (.. pose -eye -z) 16))]
                    (when-not (and (= ccx (.-ccx st)) (= ccz (.-ccz st)))
                      (let [started (perf-now)]
                        (retarget! ccx ccz)
                        (keep-timing! (.-retargetMs metrics) (- (perf-now) started))))))

        on-pose! (fn [^js data]
                   (let [^js pose (.-pose data)
                         mtime (.-mtime data)]
                     (set! (.-pose st) pose)
                     (set! (.-pendingMtime st) mtime)
                     (.push interp pose (js/Date.now))
                     (when (some? (.-eye pose))
                       (.push (.-pendingShown st) #js {:mtime mtime :t (.-t pose)})
                       (if (some? (.-table st))
                         (do (follow! pose) nil)
                         (.then (.ensure tables (.-mcVersion pose))
                                (fn [table]
                                  (when-not (or (.-closed st) (nil? table))
                                    (when (nil? (.-table st))
                                      (set! (.-table st) table)
                                      (load-biomes! (.-world pose)))
                                    (follow! pose))))))))

        ;; one decoded event of the agent's stream: "pose", "hud" or "column"
        feed (fn [event ^js data]
               (when-not (.-closed st)
                 (case event
                   "pose" (try
                            (when-let [^js loading (on-pose! data)] (.catch loading #(js/console.error "pose:" %)))
                            (catch :default error (js/console.error "pose:" error)))
                   "hud" (set! (.-hud st) (.-hud data))
                   "column" (on-column-event! data)
                   nil)))

        connect! (fn []
                   (let [^js source (js/EventSource. (str base-url "/pose/" (agent-path agent) "?radius=" radius))]
                     (set! (.-source st) source)
                     (doseq [event ["pose" "hud" "column"]]
                       (.addEventListener source event (fn [^js e] (feed event (js/JSON.parse (.-data e))))))
                     (set! (.-onerror source) #(js/console.warn (str "event stream of " agent " interrupted; the browser will retry")))))

        settled? (fn []
                   (and (some? (.-pose st)) (some? (.-dims st))
                        (zero? (.-size (.-inFlight st))) (zero? (.-size (.-needs st)))
                        (zero? (.-size (.-decoding st))) (zero? (.-size (.-uploads st)))
                        (pos? (.-size (.-columns st)))))

        refresh-counts! (fn []
                          (set! (.-loaded metrics) (loaded-count (.-columns st)))
                          (set! (.-wanted metrics) (.-size (.-columns st)))
                          (set! (.-ready metrics) (settled?)))

        ;; Uploads what is decoded, samples the pose and returns the params for renderer.draw(world, params) (add width and
        ;; height), or null when there is nothing to draw yet. camera: {eye, yaw, pitch} replaces the pose's camera (the free camera).
        frame (fn [now-ms ^js opts]
                   (set! (.-uploaded st) (upload-step!))
                   (let [^js pose (.-pose st)
                         ^js dims (.-dims st)
                         ^js camera (when (some? opts) (.-camera opts))]
                     (when (and (some? pose) (some? (.-eye pose)) (some? dims) (some? (.-ccx st)))
                       (let [^js shown (or (and interp-on (.sample interp (js/Date.now))) pose)
                             _ (set! (.-underruns metrics) (.underruns interp))
                             _ (set! (.-delay metrics) (.delay interp))
                             ^js cam (or camera #js {:eye (.-eye shown) :yaw (.-yaw shown) :pitch (.-pitch shown)})
                             ^js eye (.-eye cam)
                             _ (when (nil? camera)
                                 (s/keep! (.-camTrace metrics) #js {:ts now-ms :x (.-x eye) :y (.-y eye) :z (.-z eye) :yaw (.-yaw cam)} TRACE-KEEP))
                             ox (* (- (.-ccx st) radius) 16)
                             oy (.-minY dims)
                             oz (* (- (.-ccz st) radius) 16)
                             ^js t (scene-time #js {:timeOfDay (or-default (.-timeOfDay shown) (.-timeOfDay pose))
                                                    :rain (or-default (.-rain shown) (.-rain pose))}
                                               url-params)]
                         #js {:eye #js {:x (- (.-x eye) ox) :y (- (.-y eye) oy) :z (- (.-z eye) oz)}
                              :basis (camera-basis #js {:yaw (.-yaw cam) :pitch (.-pitch cam) :fov fov})
                              :dist max-dist
                              :darken (sky-darken (.-time t) (.-rain t))
                              :slotOff #js {:x (* 16 (w/modulo (- (.-ccx st) radius) N)) :z (* 16 (w/modulo (- (.-ccz st) radius) N))}
                              :entities (entity-boxes (or-default (.-entities shown) (.-entities pose)) #js {:x ox :y oy :z oz} eye)}))))

        ;; columns refetched after a file change count as drawn once the frame that uploaded them has been drawn; a pose counts as
        ;; displayed once the playback body time has reached its t (at once with interpolation off). Call after the frame was drawn.
        drew (fn []
               (refresh-counts!)
               (let [drawn-at (js/Date.now)
                     ^js drawn (.-columnDrawn metrics)
                     ^js uploaded (.-uploaded st)]
                 (dotimes [i (alength uploaded)]
                   (let [^js u (aget uploaded i)]
                     (.push drawn #js {:key (.-key u) :mtime (.-mtime u) :drawnAt drawn-at})))
                 (when (> (alength drawn) COLUMN-DRAWN-KEEP) (.splice drawn 0 (- (alength drawn) COLUMN-DRAWN-KEEP)))
                 (set! (.-uploaded st) #js [])
                 (when (or (some? (.-pendingMtime st)) (pos? (alength (.-pendingShown st))))
                   (when finish-for-latency (finish))
                   (let [wall (js/Date.now)]
                     (when (some? (.-pendingMtime st))
                       (let [latency (- wall (.-pendingMtime st))]
                         (set! (.-pendingMtime st) nil)
                         (when (<= latency OFFLINE-LATENCY-MS) (keep-latency! (.-latencies metrics) latency))))
                     (let [playhead (if interp-on (.playhead interp wall) js/Infinity)
                           ^js pending (.-pendingShown st)
                           waiting #js []]
                       (dotimes [i (alength pending)]
                         (let [^js p (aget pending i)]
                           (cond
                             (> (.-t p) playhead) (.push waiting p)
                             (<= (- wall (.-mtime p)) OFFLINE-LATENCY-MS) (keep-latency! (.-shownLatencies metrics) (- wall (.-mtime p))))))
                       (set! (.-pendingShown st) waiting))))))

        stats (fn []
                (refresh-counts!)
                (let [^js pose (.-pose st)]
                  #js {:loaded (.-loaded metrics) :wanted (.-wanted metrics)
                       :poseAge (when (some? pose) (- (js/Date.now) (.-t pose)))
                       :status (if (some? pose) (or-default (.-status pose) "connecting") "connecting")}))

        close (fn []
                (when-not (.-closed st)
                  (set! (.-closed st) true)
                  (when-let [^js source (.-source st)] (.close source))
                  (doseq [k (es6-iterator-seq (.keys (.-decoding st)))] (.cancel decoder (decode-key id k)))
                  (.delete live id)
                  (.dispose world)))]
    (.set live id (fn [] (.-columns st)))
    (when own-stream (connect!))
    #js {:id id :agent agent :radius radius :fov fov :world world :metrics metrics :interp interp
         :frame frame :drew drew :stats stats :close close :feed feed
         :pose (fn [] (.-pose st))
         :hud (fn [] (.-hud st))
         :counts (fn [] #js {:inFlight (.-size (.-inFlight st)) :decoding (.-size (.-decoding st))
                             :uploads (.-size (.-uploads st)) :needs (.-size (.-needs st))})
         :isReady (fn [] (and (some? (.-dims st)) (some? (.-ccx st))))}))
