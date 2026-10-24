(ns jobs.lib.walk.way-log
  "The way-on inputs of a priced decision and what became of each way, for the measured prior (node tools/event-counts.mjs way-on).
  :way-on.priced {:set :lit|:dark|:unknown :k :n :ways [[x y z d] ...]}: the counts the decision priced the flight with
  (wworld/way-scan at the feet), the light there as the body perceives it (:unknown: no light data at the feet), and the first
  ways into cells it had not seen. :way-on.seen {:way [x y z d] :on bool}: whether the step goes on, read from the body's memory
  at the first resolve! that finds the way's cells there; resolve! runs every respond-to-hostile tick, every decision
  logged and every go-to step, so a way seen in any job is read. :way-on.censored {:way :why :evicted}: a way dropped
  unread to keep at most pending-cap waiting; one still waiting when the logs are counted is censored too.
  Nothing is read but the body's memory."
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.walk.way-on :as way-on]
            [jobs.lib.walk.world :as wworld]))

(def pending-cap "Most unseen ways that wait for their cells; log-priced! logs each one it drops past it." 64)

(def ^:private pending-policy {:cap pending-cap :ttl :forever})

(defn setting
  "The light of cell as the body of primitives p perceives it: :lit, :dark, or :unknown without light data there."
  [p cell]
  (cond (nil? (sh-sense/cell-light p cell)) :unknown
        (sh-sense/lit-here? p cell) :lit
        :else :dark))

(defn resolve!
  "Emits :way-on.seen for each waiting way whose cells the body's memory holds now, and forgets it."
  [c]
  (when-let [pending (seq (ctx/entries c :way-pending))]
    (when-let [known (wworld/memory-sections (:primitives c))]
      (doseq [{way :way} (map :data pending)
              :let [on (way-on/way-outcome known way)]
              :when (some? on)]
        (ctx/emit! c :way-on.seen :info {:way way :on on})
        (ctx/forget-where! c :way-pending #(= way (:way %)))))))

(defn log-priced!
  "Emits :way-on.priced of scan (wworld/way-scan's value the decision priced with; nil: it priced none, nothing is logged) and
  keeps its ways not already waiting for resolve!, dropping the oldest past pending-cap as :way-on.censored; resolves the
  earlier ones first."
  [c scan]
  (resolve! c)
  (when scan
    (let [p (:primitives c)
          v (assoc (select-keys scan [:k :n :ways]) :set (setting p (sh-sense/feet p)))
          waiting (mapv (comp :way :data) (ctx/entries c :way-pending))
          fresh (remove (set waiting) (distinct (:ways v)))]
      (ctx/emit! c :way-on.priced :info v)
      (doseq [w (take (- (+ (count waiting) (count fresh)) pending-cap) waiting)]
        (ctx/emit! c :way-on.censored :info {:way w :why :evicted})
        (ctx/forget-where! c :way-pending #(= w (:way %))))
      (doseq [w fresh] (ctx/remember! c :way-pending {:way w} pending-policy)))))
