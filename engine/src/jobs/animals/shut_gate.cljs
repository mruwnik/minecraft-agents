(ns jobs.animals.shut-gate
  (:require [engine.ctx :as ctx]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.util :as u]
            [jobs.lib.pass :as pass]
            [jobs.lib.result :as res]
            [jobs.lib.pen-gate :as pg]
            [jobs.lib.world :as known]))

(def doc
  "Shut the planned fence gates that stand open. A planned gate is a cell of a plan whose want is a fence gate
  (jobs.lib.pen-gate/gate-cells). A gate in no plan is never touched.

  - Without :plan: the open planned gates within :radius of the body. This is the job of the pen-gate trigger,
    which fires for a gate seen open with the body more than 2 blocks away, so a job that holds a gate open on
    purpose is not fought. When one stands open the run first waits :open-s (4 s), so a gate a player just opened
    is not shut on them, and reads the gates again.
  - With :plan: every open gate of that plan, wherever it is.

  One run shuts them all, nearest first. For each it walks within :reach (a jobs.movement.go-to child, doors :never),
  waits a few seconds when an animal stands in the gate (never pushes it), clicks it with an empty hand and reads
  the block again. A gate the body stands in is left (reason :standing-in), as is one an animal never left
  (:animal-in-the-way), or one the walk to it was interrupted for (:walk-interrupted); both get a 30 s :gate-gave-up
  entry, no warn. A gate that could not be reached or did not shut after :tries clicks is given up with one warn
  shut-gate.gave-up and a :gate-gave-up memory entry, which keeps the trigger away from it for 10 minutes.

  Ends done with info shut-gate.done {:shut n :left []} when every open gate was shut (or none stood open); with any
  left, stopped :left with warn shut-gate.stopped and {:shut n :left [{:cell [x y z] :reason r}]}. A :plan that is
  missing or unreadable gives one warn shut-gate.declined and {:shut 0 :left [] :declined text}.")

(def args
  {:plan {:doc "id of a plan whose open gates are all shut; nil: the open planned gates of every active plan within :radius" :default nil}
   :radius {:doc "without :plan, how far from the body a gate is looked for, in blocks" :default 8}
   :open-s {:doc "without :plan, seconds to wait before shutting a gate that stands open (0: shut at once)" :default 4}
   :reach {:doc "walk until within this many cells of the gate (the click reaches 4.5 from the eye)" :default 3}
   :tries {:doc "failed clicks on one gate before it is given up" :default 3}})

(def quiet-ttl-ms (* 1000 (:quiet-s pg/defaults)))

(def pause-ttl-ms 30000)

(defn cell-pos [[x y z]] {:x x :y y :z z})

(defn world-of [c] (:world (:engine c)))

(defn plan-gates
  "[cells trouble] for plan id: its gate cells, or nil and why the plan cannot be worked."
  [c id]
  (let [answer (known/plan c id)]
    (cond
      (nil? answer) [nil "no such plan"]
      (:broken answer) [nil (str "the plan cannot be read: " (:broken answer))]
      :else [(keys (pg/gate-cells [answer])) nil])))

(defn watched-cells
  "[cells trouble]: the gate cells this run looks at. Without :plan, the planned gates within :radius that the
  trigger has not been told to leave alone."
  [c]
  (let [{:keys [plan radius]} (:args c)]
    (if plan
      (plan-gates c plan)
      (let [quiet (pg/quiet-cells (ctx/view c) quiet-ttl-ms)
            cells (remove quiet (keys (pg/gate-index (world-of c))))]
        [(pg/candidates (u/self-pos c) cells {:radius radius :min-dist -1}) nil]))))

(defn open-now
  "The open gates among cells, minus the cells in left ({:cell [x y z]})."
  [c cells left]
  (let [done (set (map :cell left))]
    (pg/open-cells (apiary/seen-block-at-fn (:primitives c)) (remove done cells))))

(defn nearest [c cells]
  (let [here (u/self-pos c)]
    (first (sort-by #(pg/distance here %) cells))))

(defn finish!
  "End the job: the event and the result; stopped when any gate is left."
  [c shut left]
  (let [result {:shut shut :left left}
        text (str shut " gate(s) shut" (when (seq left) (str ", " (count left) " left open")))]
    (if (empty? left)
      (do (ctx/emit! c :shut-gate.done :info (assoc result :text text))
          (res/finish! c result))
      (do (ctx/emit! c :shut-gate.stopped :warn (assoc result :text text))
          (res/stop! c :left text :shut shut :left left)))))

(defn decline!
  [c text]
  (ctx/emit! c :shut-gate.declined :warn {:plan (:plan (:args c)) :text (str "shut-gate: " text)})
  (ctx/result! c {:shut 0 :left [] :declined text})
  :done)

(defn give-up!
  "Give up the gate: one warn, one memory entry the trigger reads; the entry for the left list."
  [c cell reason]
  (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl quiet-ttl-ms})
  (ctx/emit! c :shut-gate.gave-up :warn {:cell cell :reason reason :plan (:plan (:args c))
                                        :text (str "gate " cell " stays open: " (name reason))})
  {:cell cell :reason reason})

(defn leave!
  "Leave the gate for now, with no verdict on it: a short :gate-gave-up entry keeps the trigger away for a while."
  [c cell reason]
  (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl pause-ttl-ms})
  {:cell cell :reason reason})

(defn ^:async walk!
  "nil when the body stands within reach, else why not: :walk-interrupted (the walk ended :continue) or :unreachable."
  [c cell]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (cell-pos cell) :range (:reach (:args c)) :doors :never :escalate false}))]
    (cond
      (= :continue r) :walk-interrupted
      (and (= :done r) (:arrived (ctx/child-result c :walk))) nil
      :else :unreachable)))

(defn ^:async clear-of-animals?
  "Whether no animal stands in the gate's cell, after waiting out one for pass/shut-waits times pass/shut-wait-ms."
  [c cell]
  (let [col (pass/column-of (cell-pos cell) {})]
    (loop [n 0]
      (cond
        (empty? (pass/animals-in c col)) true
        (>= n pass/shut-waits) false
        :else (do (await (ctx/act c :wait #js {:ms pass/shut-wait-ms})) (recur (inc n)))))))

(defn ^:async click!
  "Click the gate at cell once the body is in reach, then read it again: nil when shut, else the reason."
  [c cell]
  (let [r (await (ctx/act c :useOn (clj->js {:pos (cell-pos cell)})))]
    (cond
      (empty? (pg/open-cells (apiary/seen-block-at-fn (:primitives c)) [cell])) nil
      (= "unreachable" (.-status r)) :unreachable
      :else :refused)))

(defn ^:async shut-once!
  "Shut the gate: nil when shut, else the reason it stays open."
  [c cell]
  (if-let [why (await (walk! c cell))]
    why
    (if-not (await (clear-of-animals? c cell))
      :animal-in-the-way
      (loop [n 1]
        (let [reason (await (click! c cell))]
          (cond
            (nil? reason) nil
            (< n (:tries (:args c))) (recur (inc n))
            :else reason))))))

(defn check
  "Always: a run that finds nothing open ends at once with {:shut 0 :left []}, and a started run owes its result
  after the gate it shut no longer reads open."
  [_c]
  true)

(defn ^:async grace!
  "Without :plan, wait :open-s seconds once when a watched gate stands open now."
  [c cells]
  (let [{:keys [plan open-s]} (:args c)]
    (when (and (not plan) (pos? open-s) (seq (open-now c cells [])))
      (await (ctx/act c :wait #js {:ms (* 1000 open-s)})))))

(defn ^:async round [c]
  (let [[cells trouble] (watched-cells c)]
    (if trouble
      (decline! c trouble)
      (do
        (await (grace! c cells))
        (loop [shut 0 left []]
          (let [here (u/self-pos c)
                open (open-now c cells left)
                under (filter #(pg/standing-in? here %) open)
                todo (remove (set under) open)
                left (into left (map (fn [cell] {:cell cell :reason :standing-in})) under)]
            (if-let [cell (nearest c todo)]
              (let [reason (await (shut-once! c cell))]
                (cond
                  (nil? reason) (recur (inc shut) left)
                  (#{:animal-in-the-way :walk-interrupted} reason) (recur shut (conj left (leave! c cell reason)))
                  :else (recur shut (conj left (give-up! c cell reason)))))
              (finish! c shut left))))))))
