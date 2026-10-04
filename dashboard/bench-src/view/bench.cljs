(ns view.bench
  "JS-facing exports of the viewer's hot functions for tools/view/bench-cljs-vs-js.mjs (the :viewer-bench build). Only what the
   :viewer build does not already export, and only as thin adapters; the logic benchmarked is view.schedule and view.window."
  (:require [view.schedule :as s]))

(defn event-cache
  "view.schedule's EventCache as a JS object of methods (the deftype's methods are renamed in an advanced build)."
  []
  (let [c (s/event-cache)]
    #js {:record (fn [event data] (.record c event data))
         :replay (fn [agent feed] (.replay c agent feed))
         :keepOnly (fn [agents] (.keepOnly c agents))}))
