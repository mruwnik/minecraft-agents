(ns jobs.debug.ready-probe
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]))

(def doc
  "For testing readiness live: binds a readiness checker and does nothing else. Its safe? is ready/safe-at the cell
  :at (here without it), or ready/safe-if-lit with :lit?; it waits with the checker's reason until that holds. Once
  started it emits an info job.ready-probe with the checker's status (:status) and ends done.")

(a/defargs args
  {:at {:doc "the place the job would go to, [x y z] or {:x :y :z}" :spec a/position? :default nil}
   :stay-s {:doc "seconds the probe counts as staying at :at (price/stays); none: the place-stay default" :spec number? :default nil}
   :lit? {:doc "bind safe-if-lit (a roaming job) instead of safe-at" :spec boolean? :default false}})

(def target [])

(def defaults {})

(defn checker [c]
  (let [{:keys [lit? stay-s]} (:args c)]
    (cond lit? ready/safe-if-lit
          stay-s (price/stays (ready/safe-at (fn [c] (a/cell (:at (:args c))))) (constantly stay-s))
          :else (ready/safe-at (fn [c] (a/cell (:at (:args c))))))))

(def safe?
  (with-meta (fn [c] ((checker c) c))
    {:jobs.lib.ready/exposure (fn [c] ((:jobs.lib.ready/exposure (meta (checker c))) c))}))

(def fit? ready/ok)

(defn quote-of [_c] {:uses {}})

(defn ^:async round [c]
  (ctx/emit! c :job.ready-probe :info {:text "ready-probe started" :attention :notice :status (safe? c)})
  (ctx/end c :done nil))
