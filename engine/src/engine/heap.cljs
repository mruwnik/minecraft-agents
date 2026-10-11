(ns engine.heap
  "The body's heap statistics as a periodic body.heap event, next to perception.stats: V8 heap used and limit, rss, external
  and array buffers (bytes), and the gauges registered by code that keeps state outside job memory (gauge!)."
  (:require ["v8" :as v8]))

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

(defn start!
  "Emit a body.heap event through emit! every ms; returns stop."
  [emit! ms]
  (let [t (doto (js/setInterval #(emit! (merge {:kind :body.heap :source :body :level :info} (sample))) ms) (.unref))]
    #(js/clearInterval t)))
