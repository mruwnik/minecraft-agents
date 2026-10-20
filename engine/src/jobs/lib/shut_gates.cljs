(ns jobs.lib.shut-gates
  "The :gates kind of jobs.maintenance.shut: open planned fence gates (see that job's doc). `ops` is what the job's loop calls."
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.pass :as pass]
            [jobs.lib.pen-gate :as pg]
            [jobs.lib.shut-walk :as walk]
            [jobs.lib.world :as known]))

(def quiet-ttl-ms (* 1000 (:quiet-s pg/defaults)))

(a/defargs settings
  {::open-s {:default 4 :doc "Gates without :plan, seconds to wait before shutting a gate that stands open (0: shut at once)." :spec (a/num-in 0 nil)}
   ::graced-ttl-ms {:default 60000 :doc "How long a gate's grace is remembered, in ms: the round that finds it open after the grace shuts it." :spec (a/int-in 0 nil)}
   ::pause-ttl-ms {:default 30000 :doc "How long a gate stays paused after a failed shut, in ms." :spec (a/int-in 0 nil)}
   ::unreachable-ttl-ms {:default 120000 :doc "How long the trigger leaves a gate the walk could not reach, in ms (the body may be shut in; it tries again then)." :spec (a/int-in 0 nil)}})
(defn graced-ttl-ms [] (settings/get settings ::graced-ttl-ms))
(defn open-s [] (settings/get settings ::open-s))
(defn unreachable-ttl-ms [] (settings/get settings ::unreachable-ttl-ms))
(defn pause-ttl-ms [] (settings/get settings ::pause-ttl-ms))

(defn radius [c] (or (:radius (:args c)) 64))

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
  (if-let [plan (:plan (:args c))]
    (plan-gates c plan)
    (let [quiet (pg/quiet-cells (ctx/view c) quiet-ttl-ms)
          cells (remove quiet (keys (pg/gate-index (world-of c))))]
      [(pg/candidates (u/self-pos c) cells {:radius (radius c) :min-dist -1}) nil])))

(defn open-now
  "The open gates among cells, minus the cells in left ({:cell [x y z]})."
  [c cells left]
  (let [done (set (map :cell left))]
    (pg/open-cells (u/seen-block-at-fn (:primitives c)) (remove done cells))))

(defn unread-now
  "With :plan, the gates among cells (minus the cells in skip and left) the body has not seen lately: it goes and looks."
  [c cells left skip]
  (if-not (:plan (:args c))
    []
    (let [done (into (set skip) (map :cell left))]
      (filterv #(= :unknown (pg/gate-state (:primitives c) %)) (remove done cells)))))

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

(defn targets [c left skip]
  (let [[cells] (watched-cells c)]
    (map (fn [cell] {:cell cell})
         (sort-by #(pg/distance (u/self-pos c) %) (into (open-now c cells left) (unread-now c cells left skip))))))

(defn give-up-gate!
  "One warn, one memory entry the trigger reads; the entry for the left list."
  [c cell reason]
  (let [blocker (:blocker (ctx/mem c))]
    (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl (if blocker (unreachable-ttl-ms) quiet-ttl-ms)})
    (ctx/emit! c :shut.gave-up :warn {:openings :gates :cell cell :reason reason :plan (:plan (:args c)) :blocker blocker
                                      :text (str "gate " cell " stays open: " (name reason) (when blocker (str " (" blocker ")")))}))
  {:cell cell :reason reason})

(defn leave-gate!
  "Leave the gate for now, with no verdict on it: a short :gate-gave-up entry keeps the trigger away for a while."
  [c cell reason]
  (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl (pause-ttl-ms)})
  {:cell cell :reason reason})

(defn give-up!
  "The gate stays open: the entry for the left list."
  [c {:keys [cell]} reason]
  (case reason
    :standing-in {:cell cell :reason reason}
    :animal-in-the-way (leave-gate! c cell reason)
    (give-up-gate! c cell reason)))

(defn ^:async clear-of-animals?
  "Whether no animal stands in the gate's cell, after waiting out one for pass/shut-waits waits of pass/shut-wait-ms."
  [c cell]
  (let [col (pass/column-of (u/cell-pos cell) {})
        r (await (child/poll! c #(empty? (pass/animals-in c col)) (pass/shut-wait-ms) (pass/shut-waits) :animal-in-the-gate))]
    (if (= :continue r) :continue (boolean r))))

(defn ^:async click!
  "Shut the gate at cell with a jobs.access.toggle child (the body is in reach): nil when shut, :continue while it waits
  on the world, else the reason (:unreachable or :refused)."
  [c cell]
  (let [e (await (ctx/call c :gate 'jobs.access.toggle {:pos (u/cell-pos cell) :state :closed}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) nil
      (= :unreachable (contract/cause-key e)) :unreachable
      :else :refused)))

(defn ^:async read-gate!
  "The gate's state from beside it: a look at it first when it is not known."
  [c cell]
  (when (= :unknown (pg/gate-state (:primitives c) cell))
    (await (ctx/act c :look (clj->js {:pos {:x (+ (first cell) 0.5) :y (+ (second cell) 0.5) :z (+ (nth cell 2) 0.5)}})))
    (look/see! c))
  (pg/gate-state (:primitives c) cell))

(defn ^:async shut!
  "Shut the gate: nil when shut, :was-shut when it read shut on arrival, else the reason it stays open."
  [c {:keys [cell]}]
  (if (pg/standing-in? (u/self-pos c) cell)
    :standing-in
    (if-let [why (await (walk/walk! c (u/cell-pos cell) #{:walk}))]
      why
      (case (await (read-gate! c cell))
        :shut :was-shut
        :unknown :unseen
        (case (await (clear-of-animals? c cell))
          :continue :continue
          false :animal-in-the-way
          (loop [n 1]
            (let [reason (await (click! c cell))]
              (cond
                (= :continue reason) :continue
                (nil? reason) (do (ctx/emit! c :gate.shut :info {:cell cell :text (str "gate " cell " shut")}) nil)
                (< n (walk/tries)) (recur (inc n))
                :else reason))))))))

(def safe?
  "Not exposed at the gate of the plan nearest to the body."
  (ready/safe-at-by-night (fn [c] (some-> (nearest c (first (watched-cells c))) a/cell))))

(defn grace-cells
  "Gates without :plan: the open watched gates still inside their open-s grace (graced by an earlier round of this call)."
  [c]
  (let [plan (:plan (:args c))
        [cells trouble] (watched-cells c)
        since (- (ctx/now c) (* 1000 (open-s)))
        graced (into #{} (comp (filter #(> (:t %) since)) (keep #(some-> % :data :cell vec))) (ctx/entries c :gate-graced))]
    (if (or plan trouble) [] (filterv graced (open-now c cells [])))))

(defn can-run?
  "Later :plan-trouble while :plan is missing or unreadable; later :gate-grace while an open gate is inside its grace."
  [c]
  (let [id (:plan (:args c))
        why (when id (second (plan-gates c id)))
        graced (when-not why (seq (grace-cells c)))]
    (cond
      why (contract/later :plan-trouble (str "shut waits: " why) :plan id)
      graced (contract/later :gate-grace "a gate just opened stays open for its grace" :cells (vec graced))
      :else (contract/ok :plan "the plan is readable"))))

(defn done?
  "Ok once no watched gate is open or unread (the gates shut so far and none left as its result)."
  [c]
  (let [[cells trouble] (watched-cells c)
        open (if trouble [] (into (open-now c cells []) (unread-now c cells [] #{})))
        shut (:shut (ctx/mem c) 0)
        data [:units :gates :done shut :left (count open) :result {:shut shut :left []}]]
    (if (and (not trouble) (empty? open) (empty? (:left (ctx/mem c))))
      (apply contract/ok :shut (shut-text shut) data)
      (apply contract/later :gates-open (str (count open) " gate(s) open") data))))

(defn quote-of [_c] {:uses {:job-time [0 15 120]}})

(defn grace!
  "Without :plan: a watched gate that stands open and was not graced yet is remembered as graced; can-run? then waits
  open-s seconds before the round shuts it. The cells newly graced, or nil."
  [c cells]
  (let [plan (:plan (:args c))
        graced (into #{} (keep #(some-> % :data :cell vec)) (ctx/entries c :gate-graced))
        fresh (when (and (not plan) (pos? (open-s))) (vec (remove graced (open-now c cells []))))]
    (when (seq fresh)
      (doseq [cell fresh]
        (ctx/remember! c :gate-graced {:cell cell} {:cap 50 :ttl (graced-ttl-ms)}))
      fresh)))

(defn wait-first?
  "A plan in trouble, or a gate just seen open that waits out its grace: the round yields."
  [c]
  (let [[cells trouble] (watched-cells c)]
    (boolean (or trouble (grace! c cells)))))

(defn finish
  "[left text] for the ending."
  [_c shut left]
  [left (end-text shut (count left))])

(def ops
  {:targets targets :shut! shut! :give-up! give-up! :safe? safe? :can-run? can-run? :done? done? :quote quote-of
   :wait-first? wait-first? :finish finish})
