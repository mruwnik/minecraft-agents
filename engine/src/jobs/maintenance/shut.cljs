(ns jobs.maintenance.shut
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.pace :as pace]
            [jobs.lib.shut-gates :as gates]
            [jobs.lib.shut-walked :as walked]))

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
    with one warn shut.gave-up (data :openings :gates) and a :gate-gave-up memory entry, which keeps the trigger away from it for 10 minutes.

  :walked - the doors, gates and trapdoors a walk opened and left open (jobs.lib.doors), within :radius (16). Blocks with an :opened entry count
  (written by jobs.lib.pass when a walk opens a block, dropped when it shuts it), whatever their age. With the unrecorded? setting (default off)
  wooden doors (never gates or trapdoors) seen open without an entry count too, but only those of the body's own building: inside the footprint
  of a plan it made, or within the home-radius setting (8) of its :home place. Blocks a :leave-open walk left open on purpose are left alone
  while that walk's job lives. The block is shut as the walker does (pass/shut-column!). A recorded block the body still has not seen after the
  look is left as :unseen (entry untouched). A block that cannot be reached is dropped at once; one that is not shut after the tries setting (3)
  clicks, or that the body stands in the column of, fails this run (shut.gave-up, warn the first time) and keeps its entry with a fail
  counted (the trigger waits :open-s per fail) until the attempts setting (5) runs have failed: then it is dropped with a last warn. A block an
  animal stands in (:animal-in-the-way, info only) keeps its entry untouched and counts no failure.

  Both: one run shuts them all. Each is walked to within the reach setting (3) (a jobs.movement.go-to child, doors :never: it opens nothing on
  the way). A walk that goes on or waits (go-to :continue) is another step; the block is not given up.
  Ends :done with {:shut n :left []} when every opening was shut (a call that finds none open ends done at once); with any left,
  :nothing-possible :left with {:shut n :left [{:cell :reason}]}. Spends :job-time.")

(a/defargs args
  {:openings {:doc ":gates (open planned gates) or :walked (blocks a walk left open); :gates when :plan is given" :spec #{:gates :walked} :default :walked}
   :plan {:doc "gates only: id of a plan whose open gates are all shut; nil: the open planned gates of every active plan within :radius" :spec a/name? :default nil}
   :radius {:doc "how far from the body openings are looked for, in blocks; nil: 64 for :gates, 16 for :walked" :spec (a/num-in 0 nil) :default nil}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a run may take." :spec (a/num-in 0 nil)}})

(defn gates? [c] (boolean (or (:plan (:args c)) (= :gates (:openings (:args c))))))

(defn ops
  "The functions of the kind of opening this call shuts (jobs.lib.shut-gates or jobs.lib.shut-walked); the loop below is shared."
  [c]
  (if (gates? c) gates/ops walked/ops))

(defn ^:async step
  "The nearest opening is walked to and shut or given up (the counts and the openings left are kept in job memory);
  :again while one is left to try, else the ending. A gate just seen open without :plan, or a plan in trouble, is
  :again too: steps! then asks can-run?, which is later while the grace or the trouble lasts."
  [c]
  (let [{:keys [shut left skip] :or {shut 0 left [] skip []}} (ctx/mem c)
        {:keys [targets shut! give-up! finish wait-first?]} (ops c)
        keep! (fn [shut left skip] (ctx/update-mem! c assoc :shut shut :left left :skip skip) :again)]
    (if (wait-first? c)
      :again
    (if-let [t (first (targets c left skip))]
      (let [reason (await (shut! c t))]
        (cond
          (nil? reason) (keep! (inc shut) left skip)
          (= :was-shut reason) (keep! shut left (conj skip (:cell t)))
          (= :continue reason) :continue
          :else (keep! shut (conj left (give-up! c t reason)) skip)))
      (let [_ (ctx/update-mem! c assoc :shut shut)
            [left text] (finish c shut left)
            result {:shut shut :left left :text text}]
        (if (empty? left)
          (ctx/end c :done nil result)
          (ctx/end c :nothing-possible :left result)))))))

(defn ^:async round
  "The openings one by one (pace/steps!: done? and can-run? are asked between them, so a cancel or an interrupt cuts
  between them)."
  [c]
  (await (pace/steps! c #(step c))))

(def target [:openings :plan])

(def defaults {:spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(defn safe? [c] ((:safe? (ops c)) c))
(def fit? ready/ok)

(defn can-run? [c] ((:can-run? (ops c)) c))
(def can-start? can-run?)
(defn done? [c] ((:done? (ops c)) c))
(defn quote-of [c] ((:quote (ops c)) c))
