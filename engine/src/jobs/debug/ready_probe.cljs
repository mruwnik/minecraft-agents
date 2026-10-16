(ns jobs.debug.ready-probe
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.ready :as ready]))

(def doc
  "For testing readiness live: binds a readiness checker and does nothing else. Its safe? is ready/safe-at the cell
  :at (here without it), or ready/safe-if-lit with :lit?; it waits with the checker's reason until that holds. Once
  started it emits an info job.ready-probe and ends done.")

(a/defargs args
  {:at {:doc "the place the job would go to, [x y z] or {:x :y :z}" :spec a/position? :default nil}
   :lit? {:doc "bind safe-if-lit (a roaming job) instead of safe-at" :spec boolean? :default false}})

(def target [])

(def defaults {})

(defn checker [c]
  (if (:lit? (:args c))
    ready/safe-if-lit
    (ready/safe-at (fn [c] (a/cell (:at (:args c)))))))

(def safe?
  (with-meta (fn [c] ((checker c) c))
    {:jobs.lib.ready/exposure (fn [c] ((:jobs.lib.ready/exposure (meta (checker c))) c))}))

(def fit? ready/ok)

(defn quote-of [_c] {:uses {}})

(defn ^:async round [c]
  (ctx/emit! c :job.ready-probe :info {:text "ready-probe started" :attention :notice})
  (ctx/end c :done nil))
