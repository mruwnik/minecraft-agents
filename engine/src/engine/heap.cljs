(ns engine.heap
  "The body's heap statistics as a periodic body.heap event, next to perception.stats: V8 heap used and limit, rss, external
  and array buffers (bytes), and the gauges registered by code that keeps state outside job memory (gauge!)."
  (:require ["v8" :as v8]
            ["perf_hooks" :as perf]
            [engine.core.base :as base]))

(def stall-ms
  "A late timer or a longest loop delay over this is reported as a stall. One second: past a player's notice, and the
  owner's search budget is 100 ms, so anything here is a long block, not jitter."
  1000)

(defonce ^{:doc "Event-loop delay histogram (ns); read and reset by every sample, enabled by start!."}
  loop-delay (.monitorEventLoopDelay perf))

(defonce ^{:doc "{key (fn [] number)}: the sizes sample reports beside the heap."} gauges (atom {}))

(defn gauge! "Report (f) under key k in every sample." [k f] (swap! gauges assoc k f))

(defn sample
  "{:heap-used :heap-limit :heap-total :external :array-buffers :rss} and one key per gauge."
  []
  (let [h (v8/getHeapStatistics)
        m (.memoryUsage js/process)]
    (into {:heap-used (.-used_heap_size h) :heap-limit (.-heap_size_limit h) :heap-total (.-total_heap_size h)
           :external (.-external m) :array-buffers (.-arrayBuffers m) :rss (.-rss m)}
          (map (fn [[k f]] [k (f)]))
          @gauges)))

(defn loop-max-ms!
  "The longest event-loop delay (ms) since the last call, and starts a fresh window."
  []
  (let [m (js/Math.round (/ (.-max loop-delay) 1e6))]
    (.reset loop-delay)
    m))

(defn running-context
  "What the engine was running: {:running id :reflex id :job [leaf job symbols] :round n}, nil parts left out."
  [eng]
  (let [{:keys [id reflex round]} (base/running eng)
        spec (get-in (base/state eng) [:instances id :spec])
        leaves (->> (tree-seq map? #(cond-> (vec (:children %)) (:child %) (conj (:child %))) spec) (keep :job) vec)]
    (cond-> {}
      id (assoc :running id)
      reflex (assoc :reflex reflex)
      (seq leaves) (assoc :job leaves)
      (number? round) (assoc :round round))))

(defn stall-event
  "A :body.stall event when a timer of period-ms, due at last-ms, fired at now-ms more than stall-ms late; else nil.
  ctx (running-context) and the gauges say what was in flight."
  [now-ms last-ms period-ms ctx]
  (let [late (- now-ms last-ms period-ms)]
    (when (> late stall-ms)
      (merge {:kind :body.stall :source :body :level :warn :late-ms (js/Math.round late)} ctx (select-keys (sample) (keys @gauges))))))

(defn start!
  "Emit a body.heap event through emit! every ms (with :loop-max-ms, the longest loop delay since the last one), and a
  :body.stall event from a stall-ms timer that fires late; context-fn gives the running context. Returns stop."
  [emit! ms context-fn]
  (.enable loop-delay)
  (let [t (doto (js/setInterval #(emit! (merge {:kind :body.heap :source :body :level :info :loop-max-ms (loop-max-ms!)} (sample))) ms) (.unref))
        prev (atom (js/performance.now))
        w (doto (js/setInterval (fn []
                                  (let [now (js/performance.now)]
                                    (some-> (stall-event now @prev stall-ms (context-fn)) emit!)
                                    (reset! prev now)))
                                stall-ms)
            (.unref))]
    #(do (js/clearInterval t) (js/clearInterval w) (.disable loop-delay))))
