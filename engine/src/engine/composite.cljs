(ns engine.composite
  "Job expressions as jobs: a parsed node (engine.expr) becomes [def args],
  where a combinator's def is a generic job whose children run through
  call-child. Child slots come from position: :c0, :c1, ... See README.md,
  Job expressions."
  (:require [engine.args :as a]
            [engine.core.legacy :as legacy]
            [engine.ctx :as ctx]
            [engine.expr :as expr]
            [engine.settings :as settings]))

(a/defargs settings
  {::rest-s {:default 30 :spec (a/num-in 0.001 nil) :doc "How long repeat or until rests after a run of its child that could not finish; each such run in a row doubles it."}
   ::rest-max-s {:default 600 :spec (a/num-in 0.001 nil) :doc "The longest rest of repeat or until, s."}})

(defn slot [i] (keyword (str "c" i)))

(declare job)

(defn refusal
  "The result slot's child handed over when it ended :done but gave up, else nil."
  [c i]
  (let [res (ctx/child-result c (slot i))]
    (when (= :stopped (:status res)) res)))

(defn stop!
  "Ends the combinator :done with a stopped result carrying the child's cause."
  [c res]
  (ctx/result! c (cond-> {:status :stopped} (:reason res) (assoc :reason (:reason res))))
  :done)

(defn passes?
  "Whether child i of kids would run now, checked against its own sub-map."
  [c kids i]
  (let [[def args] (kids i)]
    (boolean (ctx/check-child c (slot i) def args))))

(defn seq-def
  "Children in order, one call per round; :at in memory is the next
  unfinished child. Check: that child's check. Done when the last is done;
  stopped with the cause when a child ends refused. Declined when that child declines in its round (a reflex job is then dropped)."
  [kids]
  (let [at (fn [c] (:at (ctx/mem c) 0))
        last-i (dec (count kids))]
    {:check (fn [c] (passes? c kids (at c)))
     :round (fn ^:async seq-round [c]
              (let [i (at c)
                    [def args] (kids i)
                    r (await (ctx/call-child c (slot i) def args))]
                (cond
                  (= :declined r) :declined
                  (not= :done r) :continue
                  (refusal c i) (stop! c (refusal c i))
                  (= i last-i) :done
                  :else (do (ctx/update-mem! c assoc :at (inc i)) :continue))))}))

(defn any-def
  "Each round, the first child whose check passes gets one call; one that ends
  refused is skipped from then on (:refused in memory). Check: any child's check,
  or a refusal to report. Done when the child that ran is done; stopped with the
  last cause once every passing child refused. Declined when no child's check
  passes, so a reflex job whose children all decline is dropped."
  [kids]
  (let [refused (fn [c] (:refused (ctx/mem c) {}))
        first-passing (fn [c] (first (filter #(and (not (contains? (refused c) (slot %))) (passes? c kids %)) (range (count kids)))))]
    {:check (fn [c] (or (some? (first-passing c)) (seq (refused c)) false))
     :round (fn ^:async any-round [c]
              (let [i (first-passing c)]
                (if (nil? i)
                  (if-let [last-res (last (vals (refused c)))] (stop! c last-res) :declined)
                  (let [[def args] (kids i)
                        r (await (ctx/call-child c (slot i) def args))]
                    (cond
                      (not= :done r) :continue
                      (refusal c i) (do (ctx/update-mem! c assoc-in [:refused (slot i)] (refusal c i)) :continue)
                      :else :done)))))}))

(defn kind-of
  "The ending kind of a refused child's result: its contract ending's, else an old job's stop reason read as one."
  [res]
  (or (get-in res [:ending :kind]) (get legacy/reason-kinds (:reason res) :nothing-possible)))

(defn resting
  "repeat's or until's rest {:until :after :kind} while it lasts, else nil."
  [c]
  (let [r (:rest (ctx/mem c))]
    (when (and r (< (ctx/now c) (:until r))) r)))

(defn rest-check
  "A repeating check: the child's answer kid?, declined while the rest lasts."
  [c kid?]
  (cond
    (not kid?) false
    (resting c) (ctx/wait c (assoc (resting c) :reason :resting))
    :else true))

(defn ran!
  "After the child in slot i ended :done: stopped with its cause when it was impossible or cut off (any refusal, in a
  reflex job, which runs one round); a rest when it could not finish (each in a row doubles it); else a run counted.
  :done after times runs, else :continue."
  [c i times]
  (let [res (refusal c i)
        kind (some-> res kind-of)]
    (cond
      (and res (or (:reflex c) (#{:impossible :cut-off} kind))) (stop! c res)
      (and res (not= :done kind))
      (let [fails (inc (:fails (ctx/mem c) 0))
            s (min (settings/get settings ::rest-max-s) (* (settings/get settings ::rest-s) (js/Math.pow 2 (dec fails))))]
        (ctx/update-mem! c assoc :fails fails :rest {:until (+ (ctx/now c) (* 1000 s)) :after (:reason res) :kind kind})
        :continue)
      :else
      (do (ctx/update-mem! c #(-> % (update :runs (fnil inc 0)) (dissoc :fails :rest)))
          (if (and times (>= (:runs (ctx/mem c)) times)) :done :continue)))))

(defn repeat-def
  "The child in slot :c0; when it is done call-child drops its memory, so the
  next call starts it fresh, and :runs counts the runs. Never done, or done
  after times runs. A child that ends refused: see ran!. Check: the child's
  check, while not resting. Declined when the child declines, so it never spins."
  [kid times]
  (let [[def args] kid]
    {:check (fn [c] (rest-check c (ctx/check-child c (slot 0) def args)))
     :round (fn ^:async repeat-round [c]
              (let [r (await (ctx/call-child c (slot 0) def args))]
                (case r
                  :declined :declined
                  :done (ran! c 0 times)
                  :continue)))}))

(defn until-def
  "Guard in slot :c0, child in :c1, which repeats like repeat's (rests too). Done at the
  top of a round once the guard's check passes (the guard itself never runs).
  Check: the guard's or the child's. Declined when the child declines."
  [guard kid]
  (let [[gdef gargs] guard
        [def args] kid
        reached? (fn [c] (boolean (ctx/check-child c (slot 0) gdef gargs)))]
    {:check (fn [c] (or (reached? c) (rest-check c (ctx/check-child c (slot 1) def args))))
     :round (fn ^:async until-round [c]
              (if (reached? c)
                :done
                (let [r (await (ctx/call-child c (slot 1) def args))]
                  (case r
                    :declined :declined
                    :done (ran! c 1 nil)
                    :continue))))}))

(defn job
  "[def args] for node: a registry job with its args, or a combinator."
  [registry {:keys [op] :as node}]
  (let [kids #(mapv (fn [n] (job registry n)) %)]
    (case op
      :leaf (expr/leaf registry (:job node) (:args node))
      :seq [(seq-def (kids (:children node))) {}]
      :any [(any-def (kids (:children node))) {}]
      :repeat [(repeat-def (job registry (:child node)) (:times node)) {}]
      :until (let [[g k] (kids (:children node))] [(until-def g k) {}]))))
