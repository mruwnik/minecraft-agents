(ns dashboard.thumbs
  "The server's still-image fallback for browsers without WebGL2 (with the hub, offline cards show the server's still, online ones draw live): decides
  what to render and when; js/thumbs.mjs only renders one still on request.
  Rules: a body's still is cached by its pose mtime; a newer pose is re-rendered only when the cached one is at least
  min-interval-ms old; renders run one at a time; the render worker is replaced once it holds more than column-cap columns, and ended after
  idle-ms without a render (the next render starts a new one).")

(def column-cap 400)
(def min-interval-ms 2000)
(def idle-ms
  "Idle time after which the render worker is ended; env THUMB_IDLE_MS overrides (0 = never)."
  (let [v (js/parseInt (some-> js/process .-env .-THUMB_IDLE_MS) 10)]
    (if (js/isNaN v) (* 3 60 1000) v)))

(defn recycle?
  "The worker's column cache never evicts, so it is replaced (a fresh, empty cache) once it holds more than `cap`."
  [loaded cap]
  (and (number? loaded) (> loaded cap)))

(defn fresh?
  "Is the cached `entry` good enough for a body whose pose file has `mtime`? Same pose, or a render younger than min-interval."
  [entry mtime now min-interval]
  (boolean (and entry
                (or (= mtime (:pose-mtime-ms entry))
                    (< (- now (:rendered-at entry)) min-interval)))))

(defn make
  "{:render name -> Promise<{:png :ms :loaded} | nil>, :pose-mtime name -> ms | nil, :recycle fn, :now fn,
   optional :idle-ms (> 0), :set-timer (f ms) -> handle, :clear-timer handle}
  -> {:get name -> Promise<{:png :pose-mtime-ms :rendered-at} | nil>, :stats fn}."
  [{:keys [render pose-mtime recycle now min-interval-ms column-cap idle-ms set-timer clear-timer]}]
  (let [cache (atom {})
        idle-timer (atom nil)
        inflight (atom {})
        chain (atom (js/Promise.resolve nil))
        queue (atom 0)
        totals (atom {:renders 0 :ms 0 :last-ms nil})
        render-one (fn [name mtime]
                     (-> (render name)
                         (.then (fn [result]
                                  (when result
                                    (swap! totals #(-> % (update :renders inc) (update :ms + (:ms result)) (assoc :last-ms (:ms result))))
                                    (when (recycle? (:loaded result) column-cap) (recycle))
                                    (let [entry {:png (:png result) :pose-mtime-ms mtime :rendered-at (now)}]
                                      (swap! cache assoc name entry)
                                      entry))))))
        disarm (fn []
                 (when-let [h @idle-timer] (clear-timer h) (reset! idle-timer nil)))
        arm (fn []
              (when (and idle-ms (pos? idle-ms) (zero? @queue))
                (disarm)
                (reset! idle-timer
                        (set-timer (fn []
                                     (reset! idle-timer nil)
                                     (when (zero? @queue) (recycle)))
                                   idle-ms))))
        enqueue (fn [name mtime]
                  (disarm)
                  (let [run (-> @chain
                                (.then (fn [_] (render-one name mtime)))
                                (.finally (fn []
                                            (swap! queue dec)
                                            (swap! inflight dissoc name)
                                            (arm))))]
                    (swap! queue inc)
                    (reset! chain (.catch run (fn [_] nil)))
                    (swap! inflight assoc name run)
                    run))
        get-thumb (fn [name]
                    (let [mtime (pose-mtime name)
                          entry (get @cache name)]
                      (cond
                        (nil? mtime) (js/Promise.resolve nil)
                        (fresh? entry mtime (now) min-interval-ms) (js/Promise.resolve entry)
                        (contains? @inflight name) (get @inflight name)
                        :else (enqueue name mtime))))]
    {:get get-thumb
     :stats (fn []
              (let [{:keys [renders ms last-ms]} @totals]
                {:bodies (count @cache) :renders renders :last-ms last-ms
                 :mean-ms (when (pos? renders) (/ ms renders)) :queue @queue}))}))
