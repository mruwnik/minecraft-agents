(ns jobs.animals.shut-gate
  (:require [engine.ctx :as ctx]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [engine.triggers.pen-gate :as pg]))

(def doc
  "Shut the planned fence gates that stand open. A planned gate is a cell of a plan whose want is a fence gate
  (engine.triggers.pen-gate/gate-cells). A gate in no plan is never touched.

  - Without :plan: the open planned gates within :radius of the body. This is the job of the pen-gate trigger,
    which waits until a gate has stood open for 4 s with the body more than 2 blocks away, so a job that holds a
    gate open on purpose is not fought.
  - With :plan: every open gate of that plan, wherever it is.

  Each round takes the nearest open gate, walks within :reach of it, clicks it with an empty hand and reads the
  block again. A gate the body stands in is left (reason :standing-in). A gate that could not be reached or did
  not shut after :tries rounds is given up with one warn shut-gate.gave-up and a :gate-gave-up memory entry,
  which keeps the trigger away from it for 10 minutes.

  Ends with info shut-gate.done and {:shut n :left [{:cell [x y z] :reason r}]}. A :plan that is missing or
  unreadable gives one warn shut-gate.declined and {:shut 0 :left [] :declined text}.")

(def args
  {:plan {:doc "id of a plan whose open gates are all shut; nil: the open planned gates of every active plan within :radius" :default nil}
   :radius {:doc "without :plan, how far from the body a gate is looked for, in blocks" :default 8}
   :reach {:doc "walk until within this many cells of the gate (the click reaches 4.5 from the eye)" :default 3}
   :tries {:doc "failed rounds on one gate before it is given up" :default 3}})

;; Two unreachable gates are six fruitless rounds in a row; the default backoff of 3 would cut that short.
(def backoff {:after 9})

(def quiet-ttl-ms (* 1000 (:quiet-s pg/defaults)))

(defn cell-pos [[x y z]] {:x x :y y :z z})

(defn world-of [c] (:world (:engine c)))

(defn plan-gates
  "[cells trouble] for plan id: its gate cells, or nil and why the plan cannot be worked."
  [c id]
  (let [answer (ctx/plan c id)]
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
  "The open gates among cells that this job has not given up on."
  [c cells]
  (let [given-up (set (map first (:given-up (ctx/mem c))))]
    (pg/open-cells (apiary/block-at-fn (:primitives c)) (remove given-up cells))))

(defn nearest [c cells]
  (let [here (u/self-pos c)]
    (first (sort-by #(pg/distance here %) cells))))

(defn finish!
  "End the job: the event and the result."
  [c]
  (let [m (ctx/mem c)
        left (into (vec (:standing m)) (map (fn [[cell reason]] {:cell cell :reason reason})) (:given-up m))
        result {:shut (:shut m 0) :left left}]
    (ctx/emit! c :shut-gate.done :info
               (assoc result :text (str (:shut result) " gate(s) shut" (when (seq left) (str ", " (count left) " left open")))))
    (ctx/result! c result)
    :done))

(defn decline!
  [c text]
  (ctx/emit! c :shut-gate.declined :warn {:plan (:plan (:args c)) :text (str "shut-gate: " text)})
  (ctx/result! c {:shut 0 :left [] :declined text})
  :done)

(defn give-up!
  "Give up the gate: one warn, one memory entry the trigger reads, and the gate is not looked at again by this job."
  [c cell reason]
  (ctx/update-mem! c update :given-up (fnil conj []) [cell reason])
  (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl quiet-ttl-ms})
  (ctx/emit! c :shut-gate.gave-up :warn {:cell cell :reason reason :plan (:plan (:args c))
                                        :text (str "gate " cell " stays open: " (name reason))})
  :continue)

(defn fail-gate!
  "Count a failed round on the gate; give it up on the last allowed try."
  [c cell reason]
  (let [tries (inc (get-in (ctx/mem c) [:tries cell] 0))]
    (ctx/update-mem! c assoc-in [:tries cell] tries)
    (if (>= tries (:tries (:args c)))
      (give-up! c cell reason)
      :continue)))

(defn ^:async click!
  "Click the gate at cell once the body is in reach, then read it again."
  [c cell]
  (let [r (await (ctx/act c :useOn (clj->js {:pos (cell-pos cell)})))]
    (if (empty? (pg/open-cells (apiary/block-at-fn (:primitives c)) [cell]))
      (do (ctx/update-mem! c update :shut (fnil inc 0)) :continue)
      (fail-gate! c cell (if (= "unreachable" (.-status r)) :unreachable :refused)))))

(defn ^:async walk-and-click! [c cell]
  (case (await (near/walk-near! c (cell-pos cell) (:reach (:args c)) {:doors :never}))
    :there (await (click! c cell))
    :partial :continue
    (fail-gate! c cell :unreachable)))

(defn check
  "Always: a run that finds nothing open ends at once with {:shut 0 :left []}, and a started run owes its result
  after the gate it shut no longer reads open."
  [_c]
  true)

(defn ^:async round [c]
  (let [[cells trouble] (watched-cells c)]
    (if trouble
      (decline! c trouble)
      (let [here (u/self-pos c)
            open (open-now c cells)
            under (filter #(pg/standing-in? here %) open)
            todo (remove (set under) open)]
        (ctx/update-mem! c assoc :standing (mapv (fn [cell] {:cell cell :reason :standing-in}) under))
        (if (empty? todo)
          (finish! c)
          (await (walk-and-click! c (nearest c todo))))))))
