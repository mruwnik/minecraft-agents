(ns jobs.maintenance.shut
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.pass :as pass]
            [jobs.lib.pen-gate :as pg]
            [jobs.lib.doors :as doors]
            [jobs.lib.access :as access]
            [jobs.lib.world :as known]))

(def doc
  "Shut what stands open that should be shut, nearest first. Two kinds of openings (the :openings arg; a :plan means :gates). The pen-gate
  trigger passes :gates, the door-left trigger :walked.

  :gates - the open planned fence gates (info gate.shut {:cell} for each one shut). A planned gate is a cell of a plan whose want is a
  fence gate (jobs.lib.pen-gate/gate-cells); a gate in no plan is never touched.
  - Without :plan: the open planned gates within :radius (64) of the body, which the pen-gate trigger fires for (a gate seen open with the
    body more than 2 blocks away, so a job that holds a gate open on purpose is not fought). A gate seen open for the first time is given the
    open-s setting (4 s) of grace, so a gate a player just opened is not shut on them: the run waits (later :gate-grace), then shuts it if it
    still stands open.
  - With :plan: every open gate of that plan, wherever it is. A gate the body has not seen lately is walked to and looked at, never taken for
    shut; one that still cannot be read is left (:unseen). A missing or unreadable :plan waits (:plan-trouble), at the start and later.
  - A gate the body stands in is left (:standing-in), as is one an animal never left (:animal-in-the-way, waited out first, never pushed); both
    get a 30 s :gate-gave-up entry, no warn. A gate that could not be reached or did not shut after the tries setting (3) toggles is given up
    with one warn shut-gate.gave-up and a :gate-gave-up memory entry, which keeps the trigger away from it for 10 minutes.

  :walked - the doors, gates and trapdoors a walk opened and left open (jobs.lib.doors), within :radius (16). Blocks with an :opened entry count
  (written by jobs.lib.pass when a walk opens a block, dropped when it shuts it), whatever their age. With the unrecorded? setting (default off)
  wooden doors (never gates or trapdoors) seen open without an entry count too, but only those of the body's own building: inside the footprint
  of a plan it made, or within the home-radius setting (8) of its :home place. Blocks a :leave-open walk left open on purpose are left alone
  while that walk's job lives. The block is shut as the walker does (pass/shut-column!). A recorded block the body still has not seen after the
  look is left as :unseen (entry untouched). A block that cannot be reached is dropped at once; one that is not shut after the tries setting (3)
  clicks, or that the body stands in the column of, fails this run (shut-doors.gave-up, warn the first time) and keeps its entry with a fail
  counted (the trigger waits :open-s per fail) until the attempts setting (5) runs have failed: then it is dropped with a last warn. A block an
  animal stands in (:animal-in-the-way, info only) keeps its entry untouched and counts no failure.

  Both: one run shuts them all. Each is walked to within the reach setting (3) (a jobs.movement.go-to child, doors :never: it opens nothing on
  the way). A walk that goes on or waits (go-to :continue) yields the round with :continue; the block is not given up.
  Ends :done with {:shut n :left []} when every opening was shut (a call that finds none open ends done at once); with any left,
  :nothing-possible :left with {:shut n :left [{:cell :reason}]}. Spends :job-time.")

(a/defargs args
  {:openings {:doc ":gates (open planned gates) or :walked (blocks a walk left open); :gates when :plan is given" :spec #{:gates :walked} :default :walked}
   :plan {:doc "gates only: id of a plan whose open gates are all shut; nil: the open planned gates of every active plan within :radius" :spec a/name? :default nil}
   :radius {:doc "how far from the body openings are looked for, in blocks; nil: 64 for :gates, 16 for :walked" :spec (a/num-in 0 nil) :default nil}})

(def quiet-ttl-ms (* 1000 (:quiet-s pg/defaults)))

(a/defargs settings
  {::open-s {:default 4 :doc "Gates without :plan, seconds to wait before shutting a gate that stands open (0: shut at once)." :spec (a/num-in 0 nil)}
   ::tries {:default 3 :doc "Failed clicks on one opening before it is given up or left." :spec (a/int-in 1 nil)}
   ::reach {:default 3 :doc "Walk until within this many cells of the opening (the click reaches 4.5 from the eye)." :spec (a/num-in 0 nil)}
   ::graced-ttl-ms {:default 60000 :doc "How long a gate's grace is remembered, in ms: the round that finds it open after the grace shuts it." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds a run may take." :spec (a/num-in 0 nil)}
   ::pause-ttl-ms {:default 30000 :doc "How long a gate stays paused after a failed shut, in ms." :spec (a/int-in 0 nil)}
   ::unreachable-ttl-ms {:default 120000 :doc "How long the trigger leaves a gate the walk could not reach, in ms (the body may be shut in; it tries again then)." :spec (a/int-in 0 nil)}
   ::unrecorded? {:default false :doc "Walked: also shut wooden doors of the body's own building seen standing open that no walk recorded opening." :spec boolean?}
   ::home-radius {:default 8 :doc "With ::unrecorded?, a door this close to the :home place is the body's own, in blocks." :spec (a/num-in 0 nil)}
   ::attempts {:default 5 :doc "Walked: runs whose clicks failed before a block is dropped for good." :spec (a/int-in 1 nil)}
   ::s-per-door {:default 6 :doc "Walked: seconds a quote counts for one door: the walk and the clicks." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn graced-ttl-ms [] (settings/get settings ::graced-ttl-ms))
(defn open-s [] (settings/get settings ::open-s))
(defn tries [] (settings/get settings ::tries))
(defn unreachable-ttl-ms [] (settings/get settings ::unreachable-ttl-ms))
(defn pause-ttl-ms [] (settings/get settings ::pause-ttl-ms))
(defn home-radius [] (settings/get settings ::home-radius))
(defn attempts [] (settings/get settings ::attempts))

(defn gates? [c] (boolean (or (:plan (:args c)) (= :gates (:openings (:args c))))))

(defn radius [c] (or (:radius (:args c)) (if (gates? c) 64 16)))

(defn world-of [c] (:world (:engine c)))

(defn cell-key [{:keys [x y z]}] [x y z])

;; --- gates

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

(defn gate-targets [c left skip]
  (let [[cells] (watched-cells c)]
    (map (fn [cell] {:cell cell})
         (sort-by #(pg/distance (u/self-pos c) %) (into (open-now c cells left) (unread-now c cells left skip))))))

(defn give-up-gate!
  "One warn, one memory entry the trigger reads; the entry for the left list."
  [c cell reason]
  (let [blocker (:blocker (ctx/mem c))]
    (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl (if blocker (unreachable-ttl-ms) quiet-ttl-ms)})
    (ctx/emit! c :shut-gate.gave-up :warn {:cell cell :reason reason :plan (:plan (:args c)) :blocker blocker
                                          :text (str "gate " cell " stays open: " (name reason) (when blocker (str " (" blocker ")")))}))
  {:cell cell :reason reason})

(defn leave-gate!
  "Leave the gate for now, with no verdict on it: a short :gate-gave-up entry keeps the trigger away for a while."
  [c cell reason]
  (ctx/remember! c :gate-gave-up {:cell cell :reason reason} {:cap 50 :ttl (pause-ttl-ms)})
  {:cell cell :reason reason})

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

;; --- walked

(defn own-door?
  "Whether the door cell is in the footprint of a plan this body made, or within the home-radius setting (8) of its :home place."
  [c {:keys [x y z] :as cell}]
  (let [home (mem/place (ctx/view c) :home)]
    (boolean (or (some (access/own-plans c) [(get (known/footprints c) [x y z])])
                 (and home (<= (doors/distance {:x (:x home) :y (:y home) :z (:z home)} cell) (home-radius)))))))

(defn door-args [c] (assoc (:args c) :radius (radius c)))

(defn walked-targets
  "The left-open blocks (doors/left-open as the body last saw them, any age; then doors/unrecorded-open) within :radius, minus the cells in left ({:cell [x y z]})."
  [c left]
  (let [given-up (set (map :cell left))
        args (door-args c)]
    (remove #(given-up (cell-key (:cell %)))
            (concat (doors/left-open (:primitives c) (ctx/view c) args 0 doors/seen-open-block)
                    (when (settings/get settings ::unrecorded?) (doors/unrecorded-open (:primitives c) (ctx/view c) (assoc args :own? #(own-door? c %))))))))

(defn give-up-walked!
  "Record why the target stays open. :standing-in and a first few :shut-failed keep the entry,
  with its opening time (a failed shut counts in :fails; the trigger waits :open-s per fail before it tries again). A repeat failure that keeps its entry only gets an info."
  [c {:keys [cell by fails t]} reason]
  (let [k (cell-key cell)
        fails' (if (= :shut-failed reason) (inc fails) fails)
        keep? (case reason
                :standing-in true
                :shut-failed (< fails' (attempts))
                false)]
    (when-not (= :animal-in-the-way reason)
      (ctx/forget-where! c :opened #(= cell (:cell %)))
      (when keep?
        (ctx/remember! c :opened {:cell cell :by by :t (or t (ctx/now c)) :shut? true :fails fails'} pass/opened-policy)))
    (ctx/emit! c :shut-doors.gave-up (if (or (= :animal-in-the-way reason) (and keep? (pos? fails))) :info :warn)
               {:cell k :reason reason :text (str "the block at " k " stays open: " (name reason)
                                                  (when (and (= :shut-failed reason) keep?) ", will try again later"))})
    {:cell k :reason reason}))

;; --- both

(defn targets
  "The openings still to shut, nearest first, as maps with a :cell; none in left (the ones given up) or skip."
  [c left skip]
  (if (gates? c) (gate-targets c left skip) (walked-targets c left)))

(defn ^:async walk!
  "nil when the body stands within reach of pos, else why not: :continue (the walk goes on or waits) or :unreachable."
  [c pos ways]
  (let [e (await (child/call-walk* c :walk {:pos pos :range (reach) :doors :never :ways ways}))
        res (:data e)]
    (ctx/update-mem! c dissoc :blocker)
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (:arrived res)) nil
      :else (do (ctx/update-mem! c assoc :blocker (str (or (:hint res) (some-> (:why res) name) "no walk leads there")))
                :unreachable))))

(defn ^:async shut-gate!
  "Shut the gate: nil when shut, :was-shut when it read shut on arrival, else the reason it stays open."
  [c {:keys [cell]}]
  (if (pg/standing-in? (u/self-pos c) cell)
    :standing-in
    (if-let [why (await (walk! c (u/cell-pos cell) #{:walk}))]
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
                (< n (tries)) (recur (inc n))
                :else reason))))))))

(defn ^:async shut-walked!
  "Shut the block: nil when shut, else the reason it stays open."
  [c {:keys [cell column dist]}]
  (if (pass/in-column? column (doors/feet-cell (u/self-pos c)))
    :standing-in
    (if-let [why (when (> dist (pass/leftover-reach)) (await (walk! c cell #{:walk :clear-seal})))]
      why
      (loop [n 1]
        (await (pass/shut-column! c column))
        (cond
          (not (doors/open-block (:primitives c) cell)) nil
          (< n (tries)) (recur (inc n))
          (seq (pass/animals-in c column)) :animal-in-the-way
          :else :shut-failed)))))

(defn give-up!
  "The gate or block stays open: the entry for the left list."
  [c {:keys [cell] :as t} reason]
  (cond
    (not (gates? c)) (give-up-walked! c t reason)
    (= :standing-in reason) {:cell cell :reason reason}
    (= :animal-in-the-way reason) (leave-gate! c cell reason)
    :else (give-up-gate! c cell reason)))

(defn finish!
  "End the call; nothing-possible when any opening is left."
  [c shut left]
  (let [result {:shut shut :left left}
        _ (ctx/update-mem! c assoc :shut shut)
        gates (gates? c)
        left (if gates left (into left (map (fn [cell] {:cell (cell-key cell) :reason :unseen})
                                            (doors/unseen-recorded (:primitives c) (ctx/view c) (door-args c)))))
        result (assoc result :left left)
        text (if gates
               (end-text shut (count left))
               (str "shut " shut " left open by a walk" (when (seq left) (str ", gave up on " (count left)))))]
    (if (empty? left)
      (ctx/end c :done nil (assoc result :text text))
      (ctx/end c :nothing-possible :left (assoc result :text text)))))

(def target [:openings :plan])

(def defaults {:spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def gate-safe?
  "Not exposed at the gate of the plan nearest to the body."
  (ready/safe-at-by-night (fn [c] (some-> (nearest c (first (watched-cells c))) a/cell))))

(defn safe? [c] (if (gates? c) (gate-safe? c) (ready/threatened c)))
(def fit? ready/ok)

(defn grace-cells
  "Gates without :plan: the open watched gates still inside their open-s grace (graced by an earlier round of this call)."
  [c]
  (let [plan (:plan (:args c))
        [cells trouble] (watched-cells c)
        since (- (ctx/now c) (* 1000 (open-s)))
        graced (into #{} (comp (filter #(> (:t %) since)) (keep #(some-> % :data :cell vec))) (ctx/entries c :gate-graced))]
    (if (or plan trouble) [] (filterv graced (open-now c cells [])))))

(defn can-run?
  "Gates: later :plan-trouble while :plan is missing or unreadable; later :gate-grace while an open gate is inside its grace."
  [c]
  (if-not (gates? c)
    (contract/ok :openings "nothing to wait for")
    (let [id (:plan (:args c))
          why (when id (second (plan-gates c id)))
          graced (when-not why (seq (grace-cells c)))]
      (cond
        why (contract/later :plan-trouble (str "shut waits: " why) :plan id)
        graced (contract/later :gate-grace "a gate just opened stays open for its grace" :cells (vec graced))
        :else (contract/ok :plan "the plan is readable")))))

(def can-start? can-run?)

(defn gates-done?
  "Ok once no watched gate is open or unread (the gates shut so far and none left as its result)."
  [c]
  (let [[cells trouble] (watched-cells c)
        open (if trouble [] (into (open-now c cells []) (unread-now c cells [] #{})))
        shut (:shut (ctx/mem c) 0)
        data [:units :gates :done shut :left (count open) :result {:shut shut :left []}]]
    (if (and (not trouble) (empty? open) (empty? (:left (ctx/mem c))))
      (apply contract/ok :shut (shut-text shut) data)
      (apply contract/later :gates-open (str (count open) " gate(s) open") data))))

(defn walked-done?
  "Ok when no block seen left open stands within :radius and none was given up on; later with the recorded blocks never
  seen; else later, with the doors shut so far and the blocks left."
  [c]
  (let [open (walked-targets c [])
        given-up (count (:left (ctx/mem c)))
        unseen (when (and (empty? open) (zero? given-up)) (doors/unseen-recorded (:primitives c) (ctx/view c) (door-args c)))
        shut (:shut (ctx/mem c) 0)]
    (if (seq unseen)
      (contract/later :gate-unseen "a block a walk opened was never seen" :look-at (vec unseen))
      (checks/goal (and (empty? open) (zero? given-up)) [:shut (str "shut " shut " left open by a walk")]
                   [:open (str (+ (count open) given-up) " blocks stand open")]
                   {:units :doors :done shut :left (+ (count open) given-up) :result {:shut shut :left []}}))))

(defn done? [c] (if (gates? c) (gates-done? c) (walked-done? c)))

(defn quote-of [c]
  (if (gates? c)
    {:uses {:job-time [0 15 120]}}
    (let [t (* (settings/get settings ::s-per-door) (count (walked-targets c [])))]
      {:uses {:job-time [0 t (* 3 t)]}})))

(defn grace!
  "Gates without :plan: a watched gate that stands open and was not graced yet is remembered as graced; can-run? then waits
  open-s seconds before the round shuts it. The cells newly graced, or nil."
  [c cells]
  (let [plan (:plan (:args c))
        graced (into #{} (keep #(some-> % :data :cell vec)) (ctx/entries c :gate-graced))
        fresh (when (and (not plan) (pos? (open-s))) (vec (remove graced (open-now c cells []))))]
    (when (seq fresh)
      (doseq [cell fresh]
        (ctx/remember! c :gate-graced {:cell cell} {:cap 50 :ttl (graced-ttl-ms)}))
      fresh)))

(defn ^:async step
  "The nearest opening is walked to and shut or given up (the counts and the openings left are kept in job memory);
  :again while one is left to try, else the ending."
  [c]
  (let [{:keys [shut left skip] :or {shut 0 left [] skip []}} (ctx/mem c)
        keep! (fn [shut left skip] (ctx/update-mem! c assoc :shut shut :left left :skip skip) :again)]
    (if-let [t (first (targets c left skip))]
      (let [reason (await (if (gates? c) (shut-gate! c t) (shut-walked! c t)))]
        (cond
          (nil? reason) (keep! (inc shut) left skip)
          (= :was-shut reason) (keep! shut left (conj skip (:cell t)))
          (= :continue reason) :continue
          :else (keep! shut (conj left (give-up! c t reason)) skip)))
      (finish! c shut left))))

(defn ^:async round
  "The openings one by one (pace/steps!: done? and can-run? are asked between them, so a cancel or an interrupt cuts
  between them); a gate just seen open without :plan waits out its grace first."
  [c]
  (let [[cells trouble] (when (gates? c) (watched-cells c))]
    (if (and (gates? c) (or trouble (grace! c cells)))
      :continue
      (await (pace/steps! c #(step c))))))
