(ns jobs.lib.walk.way-log
  "The way-on inputs of a priced decision and what became of each way, for the measured prior (node tools/event-counts.mjs way-on).
  :way-on.priced {:set :lit|:dark|nil :k :n :ways [[x y z d] ...]}: the counts jobs.lib.walk.way-on/scan gave at the feet, the light
  there as the body perceives it, and the first ways into cells it had not seen. :way-on.seen {:way [x y z d] :on bool}: the
  first time the body's own memory holds the way's cells, whether the step goes on. Nothing is read but the body's memory."
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.walk.way-on :as way-on]
            [jobs.lib.walk.world :as wworld]))

(def pending-policy "How many unseen ways wait for their cells and for how long." {:cap 24 :ttl (* 20 60 1000)})

(defn priced
  "{:set :k :n :ways} the body of primitives p prices a way on with now, nil without a memory."
  [p]
  (when-let [known (wworld/memory-sections p)]
    (let [feet (sh-sense/feet p)
          counts (way-on/scan known [(:x feet) (:y feet) (:z feet)])]
      (assoc counts :set (case (sh-sense/lit-here? p feet) true :lit false :dark nil)))))

(defn resolve!
  "Emits :way-on.seen for each waiting way whose cells the body's memory holds now, and forgets it."
  [c]
  (let [p (:primitives c)]
    (when-let [known (wworld/memory-sections p)]
      (doseq [{way :way} (map :data (ctx/entries c :way-pending))
              :let [on (way-on/way-outcome known way)]
              :when (some? on)]
        (ctx/emit! c :way-on.seen :info {:way way :on on})
        (ctx/forget-where! c :way-pending #(= way (:way %)))))))

(defn log-priced!
  "Emits :way-on.priced for the decision the body is pricing at its feet and keeps its ways to resolve!; resolves the earlier ones first."
  [c]
  (resolve! c)
  (when-let [{:keys [ways] :as v} (priced (:primitives c))]
    (ctx/emit! c :way-on.priced :info v)
    (doseq [w ways] (ctx/remember! c :way-pending {:way w} pending-policy))))
