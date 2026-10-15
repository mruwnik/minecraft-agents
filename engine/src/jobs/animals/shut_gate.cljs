(ns jobs.animals.shut-gate
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.animals.herd-run :as hr]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.pass :as pass]
            [jobs.lib.pen-gate :as pg]
            [jobs.lib.world :as known]))

(def doc
  "Shut the planned fence gates that stand open (info gate.shut {:cell} for each one shut). A planned gate is a cell of a plan whose want is a fence gate
  (jobs.lib.pen-gate/gate-cells). A gate in no plan is never touched.

  - Without :plan: the open planned gates within :radius of the body. This is the job of the pen-gate trigger,
    which fires for a gate seen open with the body more than 2 blocks away, so a job that holds a gate open on
    purpose is not fought. When one stands open the run first pauses :open-s (4 s, a jobs.time.noop child), so a gate a player just opened
    is not shut on them, and reads the gates again.
  - With :plan: every open gate of that plan, wherever it is. A gate the body has not seen lately is walked to and
    looked at, never taken for shut; one that still cannot be read is left (:unseen).

  One run shuts them all, nearest first. For each it walks within :reach (a jobs.movement.go-to child, doors :never, :ways #{:walk}),
  waits a few seconds when an animal stands in the gate (never pushes it), clicks it with an empty hand and reads
  the block again. A gate the body stands in is left (reason :standing-in), as is one an animal never left
  (:animal-in-the-way), or one the walk to it was interrupted for (:walk-interrupted); both get a 30 s :gate-gave-up
  entry, no warn. A gate that could not be reached or did not shut after :tries clicks is given up with one warn
  shut-gate.gave-up and a :gate-gave-up memory entry, which keeps the trigger away from it for 10 minutes.

  Ends :done with info shut-gate.done and the data {:shut n :left []} when every open gate was shut; a call that finds none open (or unread) at a round start ends done at once, with no event (done?); with any left, :nothing-possible :left with warn shut-gate.stopped and {:shut n :left [{:cell [x y z] :reason r}]}.
  A call whose :plan is missing or unreadable waits (:plan-trouble); one that goes bad after the start ends :condition-failed :plan-trouble with one warn
  shut-gate.declined and {:shut 0 :left [] :declined text}.")

(a/defargs args
  {:plan {:doc "id of a plan whose open gates are all shut; nil: the open planned gates of every active plan within :radius" :spec a/name? :default nil}
   :radius {:doc "without :plan, how far from the body a gate is looked for, in blocks" :spec (a/num-in 0 nil) :default 64}
   :open-s {:doc "without :plan, seconds to wait before shutting a gate that stands open (0: shut at once)" :spec (a/num-in 0 nil) :default 4}
   :reach {:doc "walk until within this many cells of the gate (the click reaches 4.5 from the eye)" :spec (a/num-in 0 nil) :default 3}
   :tries {:doc "failed clicks on one gate before it is given up" :spec (a/int-in 1 nil) :default 3}})

(def quiet-ttl-ms (* 1000 (:quiet-s pg/defaults)))

(a/defargs settings
  {::time-s {:default 120 :doc "Seconds a shut-gate run may take." :spec (a/num-in 0 nil)}
   ::pause-ttl-ms {:default 30000 :doc "How long a gate stays paused after a failed shut, in ms." :spec (a/int-in 0 nil)}
   ::unreachable-ttl-ms {:default 120000 :doc "How long the trigger leaves a gate the walk could not reach, in ms (the body may be shut in; it tries again then)." :spec (a/int-in 0 nil)}})
(defn unreachable-ttl-ms [] (settings/get settings ::unreachable-ttl-ms))
(defn pause-ttl-ms [] (settings/get settings ::pause-ttl-ms))

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

(defn unread-now
  "With :plan, the gates among cells (minus the cells in skip and left) the body has not seen lately: it goes and looks."
  [c cells left skip]
  (if-not (:plan (:args c))
    []
    (let [done (into (set skip) (map :cell left))]
      (filterv #(= :unknown (hr/gate-state (:primitives c) %)) (remove done cells)))))

(defn nearest [c cells]
  (let [here (u/self-pos c)]
    (first (sort-by #(pg/distance here %) cells))))

(defn finish!
  "End the call: the event and the ending; nothing-possible when any gate is left."
  [c shut left]
  (let [result {:shut shut :left left}
        _ (ctx/update-mem! c assoc :shut shut)
        text (str shut " gate(s) shut" (when (seq left) (str ", " (count left) " left open")))]
    (if (empty? left)
      (do (ctx/emit! c :shut-gate.done :info (assoc result :text text))
          (ctx/end c :done nil result))
      (do (ctx/emit! c :shut-gate.stopped :warn (assoc result :text text))
          (ctx/end c :nothing-possible :left (assoc result :text text))))))

(defn decline!
  [c text]
  (ctx/emit! c :shut-gate.declined :warn {:plan (:plan (:args c)) :text (str "shut-gate: " text)})
  (ctx/end c :condition-failed :plan-trouble {:shut 0 :left [] :declined text}))

(defn give-up!
  "Give up the gate: one warn, one memory entry the trigger reads; the entry for the left list."
  [c cell reason]
  (let [blocker (:blocker (ctx/mem c))]
    (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl (if blocker (unreachable-ttl-ms) quiet-ttl-ms)})
    (ctx/emit! c :shut-gate.gave-up :warn {:cell cell :reason reason :plan (:plan (:args c)) :blocker blocker
                                          :text (str "gate " cell " stays open: " (name reason) (when blocker (str " (" blocker ")")))}))
  {:cell cell :reason reason})

(defn leave!
  "Leave the gate for now, with no verdict on it: a short :gate-gave-up entry keeps the trigger away for a while."
  [c cell reason]
  (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl (pause-ttl-ms)})
  {:cell cell :reason reason})

(defn ^:async walk!
  "nil when the body stands within reach, else why not: :walk-interrupted (the walk ended :continue) or :unreachable."
  [c cell]
  (let [e (await (child/call-walk* c :walk {:pos (u/cell-pos cell) :range (:reach (:args c)) :doors :never :ways #{:walk}}))
        res (:data e)]
    (ctx/update-mem! c dissoc :blocker)
    (cond
      (= :continue e) :walk-interrupted
      (and (= :done (:kind e)) (:arrived res)) nil
      :else (do (ctx/update-mem! c assoc :blocker (str (or (:hint res) (some-> (:why res) name) "no walk leads there")))
                :unreachable))))

(defn ^:async clear-of-animals?
  "Whether no animal stands in the gate's cell, after waiting out one for pass/shut-waits pauses of pass/shut-wait-ms
  (jobs.time.noop children, the count kept in memory): :continue while a pause is on."
  [c cell]
  (let [col (pass/column-of (u/cell-pos cell) {})]
    (loop []
      (let [n (:gate-waits (ctx/mem c) 0)]
        (cond
          (empty? (pass/animals-in c col)) (do (ctx/update-mem! c dissoc :gate-waits) true)
          (>= n (pass/shut-waits)) (do (ctx/update-mem! c dissoc :gate-waits) false)
          :else (if (= :continue (await (child/pause! c (/ (pass/shut-wait-ms) 1000) :animal-in-gate)))
                  :continue
                  (do (ctx/update-mem! c assoc :gate-waits (inc n)) (recur))))))))

(defn ^:async click!
  "Click the gate at cell once the body is in reach, then read it again: nil when shut, else the reason."
  [c cell]
  (let [r (await (ctx/act c :useOn (clj->js {:pos (u/cell-pos cell)})))]
    (cond
      (empty? (pg/open-cells (apiary/seen-block-at-fn (:primitives c)) [cell])) nil
      (= "unreachable" (.-status r)) :unreachable
      :else :refused)))

(defn ^:async read-gate!
  "The gate's state from beside it: a look at it first when it is not known."
  [c cell]
  (when (= :unknown (hr/gate-state (:primitives c) cell))
    (await (ctx/act c :look (clj->js {:pos {:x (+ (first cell) 0.5) :y (+ (second cell) 0.5) :z (+ (nth cell 2) 0.5)}})))
    (look/see! c))
  (hr/gate-state (:primitives c) cell))

(defn ^:async shut-once!
  "Shut the gate: nil when shut, :was-shut when it read shut on arrival, else the reason it stays open."
  [c cell]
  (if-let [why (await (walk! c cell))]
    why
    (case (await (read-gate! c cell))
      :shut :was-shut
      :unknown :unseen
      (let [clear (await (clear-of-animals? c cell))]
        (cond
          (= :continue clear) :continue
          (not clear) :animal-in-the-way
          :else (loop [n 1]
                  (let [reason (await (click! c cell))]
                    (cond
                      (nil? reason) (do (ctx/emit! c :gate.shut :info {:cell cell :text (str "gate " cell " shut")}) nil)
                      (< n (:tries (:args c))) (recur (inc n))
                      :else reason))))))))

(def target [:plan])

(def defaults {:spend {:time ::time-s}})

(defn plan-fact
  "The :plan-trouble wait while :plan is missing or unreadable, else nil."
  [c]
  (when-let [id (:plan (:args c))]
    (when-let [why (second (plan-gates c id))]
      {:reason :plan-trouble :plan id :text (str "shut-gate waits: " why)})))

(def conditions "Waits while :plan is missing or unreadable (plan-fact)." [[:fn plan-fact]])

(defn done?
  "The gates shut and the watched gates still open or unread: done once none is (the goal held: no event, the call just ends done)."
  [c]
  (let [[cells trouble] (watched-cells c)
        open (if trouble [] (into (open-now c cells []) (unread-now c cells [] #{})))]
    {:done? (and (not trouble) (empty? open)) :units :gates :done (:shut (ctx/mem c) 0) :left (count open)}))

(defn quote-of [_c]
  {:uses {:time [0 15 120]}})

(defn ^:async grace!
  "Without :plan, pause :open-s seconds once (a jobs.time.noop child; a cut goes on with it) when a watched gate stands
  open now: :continue while the pause is on."
  [c cells]
  (let [{:keys [plan open-s]} (:args c)]
    (when (and (not plan) (pos? open-s) (not (:grace-done (ctx/mem c))) (seq (open-now c cells [])))
      (if (= :continue (await (child/pause! c open-s :gate-grace)))
        :continue
        (ctx/update-mem! c assoc :grace-done true)))))

(defn ^:async round [c]
  (let [[cells trouble] (watched-cells c)]
    (if trouble
      (decline! c trouble)
      (if (= :continue (await (grace! c cells)))
        :continue
        (loop [shut (:shut (ctx/mem c) 0) left (:left (ctx/mem c) []) skip #{}]
          (let [here (u/self-pos c)
                open (into (open-now c cells left) (unread-now c cells left skip))
                under (filter #(pg/standing-in? here %) open)
                todo (remove (set under) open)
                left (into left (map (fn [cell] {:cell cell :reason :standing-in})) under)]
            (if-let [cell (nearest c todo)]
              (let [reason (await (shut-once! c cell))]
                (cond
                  (= :continue reason) (do (ctx/update-mem! c assoc :shut shut :left left) :continue)
                  (nil? reason) (recur (inc shut) left skip)
                  (= :was-shut reason) (recur shut left (conj skip cell))
                  (#{:animal-in-the-way :walk-interrupted} reason) (recur shut (conj left (leave! c cell reason)) skip)
                  :else (recur shut (conj left (give-up! c cell reason)) skip)))
              (finish! c shut left))))))))
