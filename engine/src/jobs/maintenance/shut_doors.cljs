(ns jobs.maintenance.shut-doors
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.pass :as pass]
            [jobs.lib.doors :as doors]
            [jobs.lib.access :as access]
            [jobs.lib.world :as known]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(def doc
  "Shut the doors, gates and trapdoors a walk opened and left open (the job of the door-left trigger,
  jobs.lib.doors).
  Blocks with an :opened entry count (written by jobs.lib.pass when a walk opens a block, dropped when it
  shuts it), whatever their age. With :unrecorded? (default off; the door-left reflex and lead-to never set it) wooden
  doors (never gates or trapdoors) seen open without an entry count too, but only those of the body's own building:
  inside the footprint of a plan it made, or within :home-radius of its :home place. Any other block the walker did not open is never touched. Blocks a :leave-open walk left
  open on purpose are left alone while that walk's job lives (once it is gone, they count).
  One run shuts them all, nearest first. For each it walks within :reach (a jobs.movement.go-to child, doors :never:
  it opens nothing on the way), then shuts it as the walker does (pass/shut-column!). An animal in the cell is waited
  out, never pushed. A recorded block the body still has not seen after the look is left as :unseen (entry untouched). A block that cannot be reached, that the body stands in the column of, or that is not shut after
  :tries clicks fails this run (shut-doors.gave-up, warn the first time); its entry is kept with a fail counted (the
  trigger waits :open-s per fail) and a later run tries again, until :attempts runs have failed: then it is dropped with a last
  warn. A block that cannot be reached is dropped at once. A block the body stands in the column of, or whose walk was
  interrupted (:walk-interrupted, info only), is left and keeps its entry the same way. A block an animal stands in
  (:animal-in-the-way, info only) keeps its entry untouched and no failure counts, so the trigger tries again at once.
  Needs nothing. Ends :done with {:shut n :left []} when every block was shut (or none stood
  open); with any left, nothing-possible :left with warn shut-doors.stopped, data {:shut n :left [{:cell :reason}]}.
  Spends :time (default ::job-time-s).")

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a run may take before it ends." :spec (a/num-in 0 nil)}
   ::s-per-door {:default 6 :doc "Seconds a quote counts for one door: the walk and the clicks." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def safe? ready/threatened)

(def fit? ready/ok)


(a/defargs args
  {:radius {:doc "how far from the body a left block is looked for, in blocks" :spec (a/num-in 0 nil) :default 16}
   :reach {:doc "walk until within this many cells of the block (a click reaches 4.5 from the eye)" :spec (a/num-in 0 nil) :default 3}
   :unrecorded? {:doc "also shut wooden doors of the body's own building seen standing open that no walk recorded opening" :spec boolean? :default false}
   :home-radius {:doc "with :unrecorded?, a door this close to the :home place is the body's own, in blocks" :spec (a/num-in 0 nil) :default 8}
   :tries {:doc "clicks on one block before the run leaves it" :spec (a/int-in 1 nil) :default 3}
   :attempts {:doc "runs whose clicks failed before a block is dropped for good" :spec (a/int-in 1 nil) :default 5}})

;; the blocks given up in this run are skipped; a later run starts afresh from the world and the :opened entries
(defn cell-key [{:keys [x y z]}] [x y z])

(defn own-door?
  "Whether the door cell is in the footprint of a plan this body made, or within :home-radius of its :home place."
  [c {:keys [x y z] :as cell}]
  (let [home (mem/place (ctx/view c) :home)]
    (boolean (or (some (access/own-plans c) [(get (known/footprints c) [x y z])])
                 (and home (<= (doors/distance {:x (:x home) :y (:y home) :z (:z home)} cell) (:home-radius (:args c))))))))

(defn targets
  "The left-open blocks (doors/left-open as the body last saw them, any age; then doors/unrecorded-open) within :radius, minus the cells in left ({:cell [x y z]})."
  [c left]
  (let [given-up (set (map :cell left))]
    (remove #(given-up (cell-key (:cell %)))
            (concat (doors/left-open (:primitives c) (ctx/view c) (:args c) 0 doors/seen-open-block)
                    (when (:unrecorded? (:args c)) (doors/unrecorded-open (:primitives c) (ctx/view c) (assoc (:args c) :own? #(own-door? c %))))))))

(defn ^:async walk!
  "nil when the body stands within reach, else why not: :walk-interrupted (the walk ended :continue) or :unreachable."
  [c {:keys [cell]}]
  (let [r (await (child/call-walk* c :walk {:pos cell :range (:reach (:args c)) :doors :never :ways #{:walk :clear-seal}}))]
    (cond
      (= :continue r) :walk-interrupted
      (and (= :done (:kind r)) (:arrived (:data r))) nil
      :else :unreachable)))

(defn ^:async shut-once!
  "Shut the target: nil when shut, else the reason it stays open."
  [c {:keys [cell column dist]}]
  (if (pass/in-column? column (doors/feet-cell (u/self-pos c)))
    :standing-in
    (if-let [why (when (> dist (pass/leftover-reach)) (await (walk! c {:cell cell})))]
      why
      (loop [n 1]
        (await (pass/shut-column! c column))
        (cond
          (not (doors/open-block (:primitives c) cell)) nil
          (< n (:tries (:args c))) (recur (inc n))
          (seq (pass/animals-in c column)) :animal-in-the-way
          :else :shut-failed)))))

(defn give-up!
  "Record why the target stays open. :standing-in, :walk-interrupted and a first few :shut-failed keep the entry,
  with its opening time (a failed shut counts in :fails; the trigger waits :open-s per fail before it tries again). A walk that was interrupted is not a
  failure, so it only gets an info, as does a repeat failure that keeps its entry."
  [c {:keys [cell by fails t]} reason]
  (let [k (cell-key cell)
        fails' (if (= :shut-failed reason) (inc fails) fails)
        keep? (case reason
                (:standing-in :walk-interrupted) true
                :shut-failed (< fails' (:attempts (:args c)))
                false)]
    (when-not (= :animal-in-the-way reason)
      (ctx/forget-where! c :opened #(= cell (:cell %)))
      (when keep?
        (ctx/remember! c :opened {:cell cell :by by :t (or t (ctx/now c)) :shut? true :fails fails'} pass/opened-policy)))
    (ctx/emit! c :shut-doors.gave-up (if (or (#{:walk-interrupted :animal-in-the-way} reason) (and keep? (pos? fails))) :info :warn)
               {:cell k :reason reason :text (str "the block at " k " stays open: " (name reason)
                                                  (when (and (= :shut-failed reason) keep?) ", will try again later"))})
    {:cell k :reason reason}))

(defn finish! [c shut left]
  (let [result {:shut shut :left left}]
    (if (empty? left)
      (ctx/end c :done nil (assoc result :text (str "shut " shut " left open by a walk")))
      (let [text (str "shut " shut " left open by a walk, gave up on " (count left))
            level (if (every? #(= :walk-interrupted (:reason %)) left) :info :warn)]
        (ctx/emit! c :shut-doors.stopped level (assoc result :text text))
        (ctx/end c :nothing-possible :left (assoc result :text text))))))

(defn done?
  "Ok when no block seen left open stands within :radius and none was given up on; a :look at the recorded blocks never
  seen; else later, with the doors shut so far and the blocks left."
  [c]
  (let [open (targets c [])
        given-up (count (:left (ctx/mem c)))
        unseen (when (and (empty? open) (zero? given-up)) (doors/unseen-recorded (:primitives c) (ctx/view c) (:args c)))
        shut (:shut (ctx/mem c) 0)]
    (if (seq unseen)
      (contract/look :gate-unseen "a block a walk opened was never seen" :look-at (vec unseen))
      (checks/goal (and (empty? open) (zero? given-up)) [:shut (str "shut " shut " left open by a walk")]
                   [:open (str (+ (count open) given-up) " blocks stand open")]
                   {:units :doors :done shut :left (+ (count open) given-up) :result {:shut shut :left []}}))))

(defn quote-of [c]
  (let [t (* (settings/get settings ::s-per-door) (count (targets c [])))]
    {:uses {:job-time [0 t (* 3 t)]}}))

(defn ^:async step!
  "Shut the nearest target not given up on, or end the run when none is left."
  [c]
  (let [m (ctx/mem c)
        left (:left m [])]
    (if-let [target (first (targets c left))]
      (if-let [reason (await (shut-once! c target))]
        (do (ctx/update-mem! c assoc :left (conj left (give-up! c target reason))) :again)
        (do (ctx/update-mem! c update :shut (fnil inc 0)) :again))
      (finish! c (:shut m 0) (into left (map (fn [cell] {:cell (cell-key cell) :reason :unseen})
                                             (doors/unseen-recorded (:primitives c) (ctx/view c) (:args c))))))))

(defn ^:async round
  "One door per step, a pace between, so a cancel or an interrupt cuts between doors."
  [c]
  (await (pace/steps! c #(step! c))))
