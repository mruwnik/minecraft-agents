(ns engine.heap
  "The body's heap statistics as a periodic body.heap event, next to perception.stats: V8 heap used and limit, rss, external
  and array buffers (bytes), and the sizes of the planner state kept outside job memory (jobs.lib.walk.search)."
  (:require ["v8" :as v8]
            [jobs.lib.walk.search :as wsearch]))

(defn set-size [^js s] (if s (.-size s) 0))

(defn sample
  "{:heap-used :heap-limit :heap-total :external :array-buffers :rss :known-cells :known-edges :goal-floods :searches}"
  []
  (let [h (v8/getHeapStatistics)
        m (.memoryUsage js/process)
        land (vals @wsearch/known-land)]
    {:heap-used (.-used_heap_size h) :heap-limit (.-heap_size_limit h) :heap-total (.-total_heap_size h)
     :external (.-external m) :array-buffers (.-arrayBuffers m) :rss (.-rss m)
     :known-cells (transduce (map #(set-size (:cells %))) + land)
     :known-edges (transduce (map #(set-size (:edges %))) + land)
     :goal-floods (count @wsearch/goal-floods)
     :searches (count @wsearch/searches)}))

(defn start!
  "Emit a body.heap event through emit! every ms; returns stop."
  [emit! ms]
  (let [t (doto (js/setInterval #(emit! (merge {:kind :body.heap :source :body :level :info} (sample))) ms) (.unref))]
    #(js/clearInterval t)))
