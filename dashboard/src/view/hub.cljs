(ns view.hub
  "Many scenes, one WebGL2 context: the dashboard's body cards. Every scene (view.scene) has its own block window (GL world)
   but they share one renderer, one decoder pool and one hidden canvas; a scene is rendered into the hidden canvas at its
   card's size and copied to the card's 2D canvas, round-robin at `fps` within a per-frame time budget (view.schedule).

   The GL side is a surface hub.mjs makes (the shared context, the hidden canvas, GPU timer queries, the drawImage copy):
     surface.createScene(options) -> a scene (scene.mjs over view.scene, on the surface's renderer and decoder)
     surface.draw(world, params, width, height, onGpuMs)   into the hidden canvas' bottom-left; onGpuMs(ms) once timed
     surface.poll()                                       delivers the finished GPU timings
     surface.copy(canvas, width, height) -> ms            the hidden canvas' bottom-left corner onto a 2D canvas
     surface.placeholder(canvas, width, height); surface.fit(width, height); surface.bitmap(width, height) -> Promise
     surface.finish(); surface.memory(); surface.close(); surface.rendererName; surface.gpuTimer

     const hub = createViewHub({ maxScenes: 12, fps: 6, baseUrl: '' })            (hub.mjs)
     const scene = hub.addScene({ agent, radius: 2, fov: 70, interp: true })
     scene.attach(canvas2d, { width: 320, height: 180, fps })   // redrawn while the canvas is visible (IntersectionObserver);
       fps: a number (default: the hub's fps) or 'raf' (every animation frame, drawn before the cards and outside their budget)
     scene.snapshot({ width, height }) -> Promise<ImageBitmap>
     scene.stats() -> { fps, loaded, wanted, poseAge, status, gpuMs?, attaches: [{ width, height, fps, measuredFps, copyMs, copyMsP95, gpuMs? }] }
     scene.detach(canvas2d); scene.close(); hub.close()

   tick (every animation frame) is written in the tuned style (see view.interp): the hub's entries and targets are JS objects
   and JS Maps, Sets and arrays mutated in place; no persistent data per frame."
  (:require [view.schedule :as s]))

(set! *warn-on-infer* true)

(def ^:const FPS-WINDOW-MS 1000)
(def ^:const FRAME-KEEP 600)
(def ^:const GPU-SMOOTHING 0.2) ; weight of the newest timer-query result in the running gpuMs
(def ^:const SNAPSHOT-UPLOAD-ROUNDS 40)
(def ^:const STREAM-DEBOUNCE-MS 500) ; scenes added or closed within this window share one reopening of the /poses stream

(defn perf-now [] (js/performance.now))
(defn round-to [v ^number scale] (when (some? v) (/ (js/Math.round (* v scale)) scale)))

(defn option [^js options k fallback]
  (let [v (when (some? options) (unchecked-get options k))]
    (if (nil? v) fallback v)))

(defn valid-fps? [v] (or (= v "raf") (and (number? v) (js/isFinite v) (pos? v))))

(defn smooth-gpu!
  "Folds a timer-query result into x.gpuMs (a running average)."
  [^js x ms]
  (set! (.-gpuMs x) (if (nil? (.-gpuMs x)) ms (+ (.-gpuMs x) (* GPU-SMOOTHING (- ms (.-gpuMs x)))))))

(defn with-gpu-ms
  "o with gpuMs (rounded to 0.01) when x has one."
  [^js o ^js x]
  (when (some? (.-gpuMs x)) (set! (.-gpuMs o) (round-to (.-gpuMs x) 100)))
  o)

(defn attach-stats [^js target now]
  (with-gpu-ms #js {:width (.-width target) :height (.-height target) :fps (.-fps target)
                    :measuredFps (s/count-since (.-renders target) now FPS-WINDOW-MS)
                    :copyMs (round-to (s/percentile (.-copyMs target) 0.5) 1000)
                    :copyMsP95 (round-to (s/percentile (.-copyMs target) 0.95) 1000)}
               target))

(defn unsupported-hub
  "Without WebGL2: a hub that reports supported: false and whose scenes do nothing (attach leaves the canvas as it is)."
  []
  #js {:supported false
       :addScene (fn [^js options]
                   #js {:id nil
                        :agent (.-agent options)
                        :attach (fn [_ _])
                        :detach (fn [_])
                        :snapshot (fn [_] (js/Promise.reject (js/Error. "WebGL2 is not available")))
                        :stats (fn [] #js {:fps 0 :loaded 0 :wanted 0 :poseAge nil :status "unsupported" :attaches #js []})
                        :ready (fn [] false)
                        :close (fn [])})
       :stats (fn [] #js {:supported false :scenes #js {}})
       :probeGpu (fn [_] #js {})
       :close (fn [])})

(defn values [^js m] (.from js/Array (.values m)))

(defn hub
  "The hub over a surface, without its animation-frame loop: (.tick hub now) renders one frame (view-hub runs it every frame).
   options: maxScenes 12, fps 6, baseUrl \"\", frameBudgetMs 8; also openStream(url) -> EventSource-like (default: EventSource)."
  [^js options ^js surface]
  (let [max-scenes (option options "maxScenes" 12)
        fps (option options "fps" 6)
        base-url (option options "baseUrl" "")
        budget-ms (option options "frameBudgetMs" 8)
        debounce-ms (option options "streamDebounceMs" STREAM-DEBOUNCE-MS)
        open-stream (option options "openStream" (fn [url] (js/EventSource. url)))
        entries (js/Map.) ; scene id -> entry #js {scene, targets (Map canvas -> target), renders, renderCount, renderMs, gpuMs}
        frame-costs #js [] ; main-thread ms of the animation frames that rendered something
        targets-of (js/WeakMap.) ; canvas -> target, for the observer
        closed (volatile! false)
        attach-count (volatile! 0)
        observer (when (exists? js/IntersectionObserver)
                   (js/IntersectionObserver.
                    (fn [^js changes]
                      (.forEach changes (fn [^js change]
                                          (when-let [^js target (.get targets-of (.-target change))]
                                            (set! (.-visible target) (.-isIntersecting change))))))))

        all-targets (fn []
                      (let [out #js []]
                        (.forEach entries (fn [^js entry] (.forEach (.-targets entry) (fn [t] (.push out t)))))
                        out))
        ;; the hidden canvas is as big as the largest attach, resized only when the set of attach sizes changes (a resize
        ;; reallocates the drawing buffer and clears it); smaller targets are drawn into its bottom-left corner
        fit-hidden! (fn []
                      (let [targets (all-targets)]
                        (when (pos? (alength targets))
                          (.fit surface
                                (.reduce targets (fn [m ^js t] (max m (.-width t))) 0)
                                (.reduce targets (fn [m ^js t] (max m (.-height t))) 0)))))

        ;; draws the scene's params at width x height into the hidden canvas, its GPU time to the entry (and target)
        draw-into! (fn [^js entry params width height ^js target]
                     (.draw surface (.. entry -scene -world) params width height
                            (fn [ms]
                              (smooth-gpu! entry ms)
                              (when (some? target) (smooth-gpu! target ms)))))

        ;; renders one target now (its scene's params are sampled once per animation frame); returns the ms it cost
        render-target! (fn [^js target now ^js frame-params ^js touched]
                         (let [started (perf-now)
                               ^js entry (.-entry target)]
                           (when-not (.has frame-params entry) (.set frame-params entry (.frame ^js (.-scene entry) now)))
                           (let [params (.get frame-params entry)]
                             (if (some? params)
                               (do (draw-into! entry params (.-width target) (.-height target) target)
                                   (s/keep! (.-copyMs target) (.copy surface (.-canvas target) (.-width target) (.-height target)) FRAME-KEEP))
                               (.placeholder surface (.-canvas target) (.-width target) (.-height target))))
                           (.add touched entry)
                           (s/keep! (.-renders target) now FRAME-KEEP)
                           (- (perf-now) started)))

        tick (fn [now]
               (when-not @closed
                 (.poll surface)
                 (let [started (perf-now)
                       targets (all-targets)
                       frame-params (js/Map.)
                       touched (js/Set.)
                       rendered (s/plan-frame targets now budget-ms
                                              (fn [^js target]
                                                (try
                                                  (render-target! target now frame-params touched)
                                                  (catch :default error
                                                    (js/console.error (str "render of " (.. target -entry -scene -agent) " failed:") error)
                                                    0)
                                                  (finally
                                                    (set! (.-dueAt target) (s/next-due-at (.-dueAt target) now (.-fps target)))))))]
                   (.forEach touched
                             (fn [^js entry]
                               (.drew ^js (.-scene entry))
                               (s/keep! (.-renders entry) now FRAME-KEEP)
                               (set! (.-renderCount entry) (inc (.-renderCount entry)))
                               (s/keep! (.-renderMs entry) (- (perf-now) started) FRAME-KEEP)))
                   (when (pos? (alength rendered)) (s/keep! frame-costs (- (perf-now) started) FRAME-KEEP)))))

        ;; ONE /poses stream for all scenes (a browser holds only 6 HTTP/1.1 connections per origin); reopened, debounced, when
        ;; the set of agents or the radius changes. Its events carry `agent` and go to every scene of that agent.
        ^js replay (s/event-cache)
        stream (volatile! nil) ; #js {key, source}
        stream-timer (volatile! nil)
        sync-stream! (fn []
                       (let [scenes (map #(.-scene ^js %) (values entries))
                             ^js want (s/stream-key (map #(.-agent ^js %) scenes) (map #(.-radius ^js %) scenes))
                             ^js agents (.-agents want)
                             ^js current @stream]
                         (when-not (or (and (some? current) (= (.-key current) (.-key want)))
                                       (and (zero? (alength agents)) (nil? current)))
                           (when (some? current) (.close ^js (.-source current)))
                           (vreset! stream nil)
                           (.keepOnly replay agents)
                           (when (pos? (alength agents))
                             (let [^js source (open-stream (str base-url "/poses?agents=" (.join (.map agents js/encodeURIComponent) ",")
                                                                "&radius=" (.-radius want)))]
                               (vreset! stream #js {:key (.-key want) :source source})
                               (doseq [event ["pose" "hud" "column"]]
                                 (.addEventListener source event
                                                    (fn [^js e]
                                                      (let [^js data (js/JSON.parse (.-data e))]
                                                        (.record replay event data)
                                                        (.forEach entries (fn [^js entry]
                                                                            (when (= (.. entry -scene -agent) (.-agent data))
                                                                              (.feed ^js (.-scene entry) event data))))))))
                               (set! (.-onerror source) #(js/console.warn "the /poses stream was interrupted; the browser will retry")))))))
        schedule-stream! (fn []
                           (js/clearTimeout @stream-timer)
                           (vreset! stream-timer (js/setTimeout sync-stream! debounce-ms)))

        fps-of (fn [^js entry now] (s/count-since (.-renders entry) now FPS-WINDOW-MS))

        add-scene (fn [^js scene-options]
                    (when (>= (.-size entries) max-scenes) (throw (js/Error. (str "the hub holds at most " max-scenes " scenes"))))
                    (let [agent (.-agent scene-options)
                          ^js scene (.createScene surface #js {:agent agent
                                                               :radius (option scene-options "radius" 2)
                                                               :fov (option scene-options "fov" 70)
                                                               :interp (option scene-options "interp" true)})
                          ^js entry #js {:scene scene :targets (js/Map.) :renders #js [] :renderCount 0 :renderMs #js [] :gpuMs nil}
                          detach (fn [canvas]
                                   (when (some? observer) (.unobserve observer canvas))
                                   (.delete targets-of canvas)
                                   (when (.delete (.-targets entry) canvas) (fit-hidden!)))
                          attach (fn [^js canvas ^js attach-options]
                                   (let [width (option attach-options "width" 320)
                                         height (option attach-options "height" 180)
                                         target-fps (option attach-options "fps" fps)]
                                     (when-not (valid-fps? target-fps)
                                       (throw (js/Error. (str "attach fps must be a positive number or 'raf', got " target-fps))))
                                     (detach canvas)
                                     (set! (.-width canvas) width)
                                     (set! (.-height canvas) height)
                                     (let [stagger (/ (* (mod @attach-count max-scenes) 1000) fps max-scenes)
                                           _ (vswap! attach-count inc)
                                           target #js {:id (str (.-id scene) ":" @attach-count) :entry entry :canvas canvas
                                                       :width width :height height :fps target-fps :raf (= target-fps "raf")
                                                       :visible (nil? observer) :dueAt (+ (perf-now) stagger)
                                                       :renders #js [] :copyMs #js [] :gpuMs nil}]
                                       (.set targets-of canvas target)
                                       (.set (.-targets entry) canvas target)
                                       (when (some? observer) (.observe observer canvas))
                                       (fit-hidden!)
                                       (.placeholder surface canvas width height))))
                          ;; renders the scene now at width x height; waits no longer than the columns already decoded
                          ;; a failure comes back as a rejected promise, never a throw
                          snapshot (fn [^js snapshot-options]
                                     (try
                                       (let [width (option snapshot-options "width" 320)
                                             height (option snapshot-options "height" 180)
                                             now (perf-now)
                                             params (loop [round 0]
                                                      (let [params (.frame scene now)]
                                                        (if (or (>= (inc round) SNAPSHOT-UPLOAD-ROUNDS)
                                                                (zero? (.-uploads ^js (.counts scene))))
                                                          params
                                                          (recur (inc round)))))]
                                         (if (nil? params)
                                           (js/Promise.reject (js/Error. (str "scene of " agent " has nothing to draw yet")))
                                           (do (draw-into! entry params width height nil)
                                               (.then (.bitmap surface width height) (fn [bitmap] (.drew scene) bitmap)))))
                                       (catch :default e (js/Promise.reject e))))
                          attaches (fn [now] (.map (values (.-targets entry)) #(attach-stats % now)))
                          stats (fn []
                                  (let [now (perf-now)]
                                    (with-gpu-ms (js/Object.assign #js {:fps (fps-of entry now)} (.stats scene) #js {:attaches (attaches now)})
                                                 entry)))
                          close (fn []
                                  (doseq [canvas (vec (es6-iterator-seq (.keys (.-targets entry))))] (detach canvas))
                                  (.delete entries (.-id scene))
                                  (fit-hidden!)
                                  (.close scene)
                                  (schedule-stream!))
                          ready (fn []
                                  (.stats scene)
                                  (.. scene -metrics -ready))]
                      (.set entries (.-id scene) entry)
                      (.replay replay agent (.-feed scene))
                      (schedule-stream!)
                      #js {:id (.-id scene) :agent agent :attach attach :detach detach :snapshot snapshot :stats stats :ready ready :close close}))

        ;; for measurement: each scene drawn `rounds` times at width x height with a finish after every draw; median ms per
        ;; scene (agent -> ms)
        probe-gpu (fn [^js probe-options]
                    (let [width (option probe-options "width" 320)
                          height (option probe-options "height" 180)
                          rounds (option probe-options "rounds" 7)
                          out #js {}]
                      (.forEach entries
                                (fn [^js entry]
                                  (let [^js scene (.-scene entry)
                                        params (.frame scene (perf-now))
                                        times #js []]
                                    (when (some? params)
                                      (dotimes [_ rounds]
                                        (let [started (perf-now)]
                                          (draw-into! entry params width height nil)
                                          (.finish surface)
                                          (.push times (- (perf-now) started)))))
                                    (unchecked-set out (.-agent scene) (round-to (s/percentile times 0.5) 100)))))
                      out))

        ;; measurement numbers of the whole hub: per-scene stats, main-thread cost of the frames that rendered, GPU memory
        stats (fn []
                (let [now (perf-now)
                      scenes #js {}]
                  (.forEach entries
                            (fn [^js entry]
                              (let [^js scene (.-scene entry)
                                    o (js/Object.assign #js {} (.stats scene)
                                                        #js {:fps (fps-of entry now)
                                                             :attaches (.map (values (.-targets entry)) #(attach-stats % now))
                                                             :renderMsP50 (s/percentile (.-renderMs entry) 0.5)
                                                             :renders (.-renderCount entry)})]
                                (when (some? (.-gpuMs entry)) (set! (.-gpuMs o) (.-gpuMs entry)))
                                (unchecked-set scenes (.-agent scene) o))))
                  #js {:renderer (.-rendererName surface)
                       :gpuTimer (.-gpuTimer surface)
                       :scenes scenes
                       :frameCostMs #js {:n (alength frame-costs)
                                         :p50 (s/percentile frame-costs 0.5)
                                         :p95 (s/percentile frame-costs 0.95)
                                         :max (when (pos? (alength frame-costs)) (.apply js/Math.max nil frame-costs))}
                       :memory (.memory surface)}))

        close (fn []
                (vreset! closed true)
                (js/clearTimeout @stream-timer)
                (when-let [^js current @stream] (.close ^js (.-source current)))
                (vreset! stream nil)
                (doseq [^js entry (values entries)]
                  (when (some? observer) (.forEach (.-targets entry) (fn [_ canvas] (.unobserve observer canvas))))
                  (.close ^js (.-scene entry)))
                (.clear entries)
                (when (some? observer) (.disconnect observer))
                (.close surface))]
    #js {:supported true :addScene add-scene :stats stats :probeGpu probe-gpu :close close :tick tick :closed (fn [] @closed)}))

(defn view-hub
  "The hub (see the namespace doc) over surface, drawing every animation frame; without a surface (no WebGL2), the
   unsupported hub."
  [^js options ^js surface]
  (if (nil? surface)
    (unsupported-hub)
    (let [^js h (hub options surface)
          raf (volatile! 0)
          loop! (fn loop! [now]
                  (when-not (.closed h)
                    (vreset! raf (js/requestAnimationFrame loop!))
                    (.tick h now)))
          close (.-close h)]
      (vreset! raf (js/requestAnimationFrame loop!))
      (set! (.-close h) (fn [] (close) (js/cancelAnimationFrame @raf)))
      h)))
