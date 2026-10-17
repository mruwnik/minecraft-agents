(ns jobs.animals.shut-gate
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
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
    purpose is not fought. A gate seen open for the first time is given :open-s (4 s) of grace, so a gate a player just opened
    is not shut on them: the run waits (later :gate-grace) :open-s, then shuts it if it still stands open.
  - With :plan: every open gate of that plan, wherever it is. A gate the body has not seen lately is walked to and
    looked at, never taken for shut; one that still cannot be read is left (:unseen).

  One run shuts them all, nearest first. For each it walks within :reach (a jobs.movement.go-to child, doors :never, :ways #{:walk}),
  waits a few moments (pass/shut-waits) when an animal stands in the gate (never pushes it), clicks it with an empty hand and reads
  the block again. A gate the body stands in is left (reason :standing-in), as is one an animal never left
  (:animal-in-the-way), or one the walk to it was interrupted for (:walk-interrupted); both get a 30 s :gate-gave-up
  entry, no warn. A gate that could not be reached or did not shut after :tries clicks is given up with one warn
  shut-gate.gave-up and a :gate-gave-up memory entry, which keeps the trigger away from it for 10 minutes.

  Ends :done with the data {:shut n :left []} when every open gate was shut; a call that finds none open (or unread) at its start ends done at once, with no event (done?); with any left, :nothing-possible :left with warn shut-gate.stopped and {:shut n :left [{:cell [x y z] :reason r}]}.
  A call whose :plan is missing or unreadable waits (:plan-trouble), at the start and later.")

(a/defargs args
  {:plan {:doc "id of a plan whose open gates are all shut; nil: the open planned gates of every active plan within :radius" :spec a/name? :default nil}
   :radius {:doc "without :plan, how far from the body a gate is looked for, in blocks" :spec (a/num-in 0 nil) :default 64}
   :open-s {:doc "without :plan, seconds to wait before shutting a gate that stands open (0: shut at once)" :spec (a/num-in 0 nil) :default 4}
   :reach {:doc "walk until within this many cells of the gate (the click reaches 4.5 from the eye)" :spec (a/num-in 0 nil) :default 3}
   :tries {:doc "failed clicks on one gate before it is given up" :spec (a/int-in 1 nil) :default 3}})

(def quiet-ttl-ms (* 1000 (:quiet-s pg/defaults)))

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a shut-gate run may take." :spec (a/num-in 0 nil)}
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

(defn shut-text
  "\"2 gate(s) shut\", or \"no gate open\" when this call shut none."
  [shut]
  (if (pos? shut) (str shut " gate(s) shut") "no gate open"))

(defn end-text
  "The call's closing text: what was shut and what stays open, never \"no gate open\" beside a gate left open."
  [shut n-left]
  (cond (zero? n-left) (shut-text shut)
        (zero? shut) (str n-left " left open")
        :else (str (shut-text shut) ", " n-left " left open")))

(defn nearest [c cells]
  (let [here (u/self-pos c)]
    (first (sort-by #(pg/distance here %) cells))))

(defn finish!
  "End the call: the event and the ending; nothing-possible when any gate is left."
  [c shut left]
  (let [result {:shut shut :left left}
        _ (ctx/update-mem! c assoc :shut shut)
        text (end-text shut (count left))]
    (if (empty? left)
      (ctx/end c :done nil (assoc result :text text))
      (do (ctx/emit! c :shut-gate.stopped :warn (assoc result :text text))
          (ctx/end c :nothing-possible :left (assoc result :text text))))))

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
  "Whether no animal stands in the gate's cell, after waiting out one for pass/shut-waits waits of pass/shut-wait-ms."
  [c cell]
  (let [col (pass/column-of (u/cell-pos cell) {})]
    (loop [n 0]
      (cond
        (empty? (pass/animals-in c col)) true
        (>= n (pass/shut-waits)) false
        :else (do (await (ctx/act c :wait #js {:ms (pass/shut-wait-ms)})) (recur (inc n)))))))

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
      (if-not (await (clear-of-animals? c cell))
        :animal-in-the-way
        (loop [n 1]
          (let [reason (await (click! c cell))]
            (cond
              (nil? reason) (do (ctx/emit! c :gate.shut :info {:cell cell :text (str "gate " cell " shut")}) nil)
              (< n (:tries (:args c))) (recur (inc n))
              :else reason)))))))

(def target [:plan])

(def defaults {:spend {:job-time ::job-time-s}})

(defn grace-cells
  "Without :plan, the open watched gates still inside their :open-s of grace (graced by an earlier round of this call)."
  [c]
  (let [{:keys [plan open-s]} (:args c)
        [cells trouble] (watched-cells c)
        since (- (ctx/now c) (* 1000 open-s))
        graced (into #{} (comp (filter #(> (:t %) since)) (keep #(some-> % :data :cell vec))) (ctx/entries c :gate-graced))]
    (if (or plan trouble) [] (filterv graced (open-now c cells [])))))

(defn can-run?
  "Later :plan-trouble while :plan is missing or unreadable; later :gate-grace while an open gate is inside its grace."
  [c]
  (let [id (:plan (:args c))
        why (when id (second (plan-gates c id)))
        graced (when-not why (seq (grace-cells c)))]
    (cond
      why (contract/later :plan-trouble (str "shut-gate waits: " why) :plan id)
      graced (contract/later :gate-grace "a gate just opened stays open for its grace" :cells (vec graced))
      :else (contract/ok :plan "the plan is readable"))))

(def can-start? can-run?)

(def meta {:category :reflex})

(def safe?
  "Not exposed at the gate of the plan nearest to the body."
  (ready/safe-at-by-night (fn [c] (some-> (nearest c (first (watched-cells c))) a/cell))))
(def fit? ready/ok)


(defn done?
  "Ok once no watched gate is open or unread (the gates shut so far and none left as its result); before the start the
  call just ends done."
  [c]
  (let [[cells trouble] (watched-cells c)
        open (if trouble [] (into (open-now c cells []) (unread-now c cells [] #{})))
        shut (:shut (ctx/mem c) 0)
        data [:units :gates :done shut :left (count open) :result {:shut shut :left []}]]
    (if (and (not trouble) (empty? open))
      (apply contract/ok :shut (shut-text shut) data)
      (apply contract/later :gates-open (str (count open) " gate(s) open") data))))

(defn quote-of [_c]
  {:uses {:job-time [0 15 120]}})

(def graced-ttl-ms
  "How long a gate's grace is remembered: the round that finds it open after the grace shuts it."
  60000)

(defn grace!
  "Without :plan, a watched gate that stands open and was not graced yet is remembered as graced; can-run? then waits
  :open-s seconds before the round shuts it. The cells newly graced, or nil."
  [c cells]
  (let [{:keys [plan open-s]} (:args c)
        graced (into #{} (keep #(some-> % :data :cell vec)) (ctx/entries c :gate-graced))
        fresh (when (and (not plan) (pos? open-s)) (vec (remove graced (open-now c cells []))))]
    (when (seq fresh)
      (doseq [cell fresh]
        (ctx/remember! c :gate-graced {:cell cell} {:cap 50 :ttl graced-ttl-ms}))
      fresh)))

(defn ^:async round [c]
  (let [[cells trouble] (watched-cells c)]
    (if trouble
      :continue
      (if (grace! c cells)
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
                  (nil? reason) (recur (inc shut) left skip)
                  (= :was-shut reason) (recur shut left (conj skip cell))
                  (#{:animal-in-the-way :walk-interrupted} reason) (recur shut (conj left (leave! c cell reason)) skip)
                  :else (recur shut (conj left (give-up! c cell reason)) skip)))
              (finish! c shut left))))))))
