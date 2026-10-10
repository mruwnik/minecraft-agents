(ns jobs.lib.standing
  "Targets a standing job (one kept in the queue under repeat) rests after a fruitless pass, so a stuck crop or refused
  ground never keeps its check true for ever and starves the other jobs. Body memory :standing/rested
  {:job :key :until :why}; a target rests ::rest-ms, then is tried once more."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(a/defargs settings
  {::rest-ms {:default 600000 :doc "How long a target rests after a pass that left it undone, in ms." :spec (a/int-in 1 nil)}})
(defn rest-ms [] (settings/get settings ::rest-ms))

(def kind :standing/rested)

(defn rested
  "{key {:until ms :why}} of the targets of job (symbol) resting now."
  [c job]
  (let [view (ctx/view c)]
    (into {} (keep (fn [{:keys [data]}]
                     (when (and (= job (:job data)) (< (:now view) (:until data)))
                       [(:key data) (select-keys data [:until :why])])))
          (mem/entries view kind))))

(defn rest!
  "Rest target key of job for ::rest-ms, with why (a keyword) kept for the status text."
  [c job key why]
  (ctx/forget-where! c kind #(and (= job (:job %)) (= key (:key %))))
  (ctx/remember! c kind {:job job :key key :why why :until (+ (ctx/now c) (rest-ms))} {:cap 32 :ttl (rest-ms)}))
