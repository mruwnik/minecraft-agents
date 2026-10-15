(ns engine.composite
  "Job expressions as jobs: a parsed node (engine.expr) becomes [def args],
  where a combinator's def is a contract job whose children run through
  ctx/call. Child slots come from position: :c0, :c1, ... See README.md,
  Job expressions."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.expr :as expr]
            [engine.settings :as settings]))

(a/defargs settings
  {::rest-s {:default 30 :spec (a/num-in 0.001 nil) :doc "How long repeat or until rests after a run of its child that could not finish; each such run in a row doubles it."}
   ::rest-max-s {:default 600 :spec (a/num-in 0.001 nil) :doc "The longest rest of repeat or until, s."}})

(defn slot [i] (keyword (str "c" i)))

(defn ending? [r] (not= :continue r))

(defn refused? [e] (and (ending? e) (not= :done (:kind e))))

(defn already? [e] (and (= :done (:kind e)) (= :already (:cause e))))

(defn stop
  "The combinator's ending for a child's ending e that was not :done: its kind and cause, e as :because."
  [c e]
  (contract/with-because (ctx/end c (:kind e) (:cause e) (:data e)) (dissoc e :report)))

(defn kid-check
  "Whether the child in slot would start now; when it would wait, its reason is the combinator's too."
  [c slot def args]
  (if-let [w (ctx/child-wait c slot def args)]
    (ctx/wait c w)
    true))

(defn passes?
  "Whether child i of kids would run now, checked against its own sub-map."
  [c kids i]
  (let [[def args] (kids i)]
    (kid-check c (slot i) def args)))

(defn held
  "What a round returns when it cannot call a child that would wait with w: a reflex job runs one round, so it ends
  condition-failed with the wait as cause; a listed one yields (its check waits)."
  [c w]
  (if (:reflex c)
    (ctx/end c :condition-failed (if (= [:reason] (keys w)) (:reason w) w))
    :continue))

(defn ^:async call
  "One ctx/call round of child i of kids, or held when its check would wait."
  [c kids i]
  (let [[def args] (kids i)]
    (if-let [w (ctx/child-wait c (slot i) def args)]
      (held c w)
      (await (ctx/call c (slot i) def args)))))

(defn contract-def
  "A combinator as a contract job: no conditions of its own (its children have theirs) and a check that waits with
  its children's reasons, which the scheduler asks of a listed one."
  [check round]
  {:conditions [] :check check :round round})

(defn seq-def
  "Children in order, one call per round; :at in memory is the next
  unfinished child. Check: that child's check. Done when the last is done
  (:already when every child's goal held before it started); a child that
  ends otherwise ends it the same way (held: see held)."
  [kids]
  (let [at (fn [c] (:at (ctx/mem c) 0))
        last-i (dec (count kids))]
    (contract-def
     (fn [c] (passes? c kids (at c)))
     (fn ^:async seq-round [c]
       (let [i (at c)
             e (await (call c kids i))]
         (cond
           (not (ending? e)) :continue
           (refused? e) (stop c e)
           (= i last-i) (ctx/end c :done (when (and (already? e) (not (:worked (ctx/mem c)))) :already))
           :else (do (ctx/update-mem! c #(cond-> (assoc % :at (inc i)) (not (already? e)) (assoc :worked true)))
                     :continue)))))))

(defn any-def
  "Each round, the first child whose check passes gets one call; one that ends
  other than :done is skipped from then on (:refused in memory; the latest ending in :last-refusal). Check: any child's check,
  or a refusal to report. Done as the child that ran is done; once every passing child refused, ends as the last
  refusal. No child passing and none refused: held."
  [kids]
  (let [refused (fn [c] (:refused (ctx/mem c) {}))
        first-passing (fn [c] (first (filter #(and (not (contains? (refused c) (slot %))) (passes? c kids %)) (range (count kids)))))]
    (contract-def
     (fn [c] (or (some? (first-passing c)) (seq (refused c)) false))
     (fn ^:async any-round [c]
       (let [i (first-passing c)]
         (if (nil? i)
           (if-let [e (:last-refusal (ctx/mem c))]
             (stop c e)
             (held c (or (ctx/child-wait c (slot 0) (first (kids 0)) (second (kids 0))) {:reason :not-ready})))
           (let [e (await (call c kids i))]
             (cond
               (not (ending? e)) :continue
               (refused? e) (do (ctx/update-mem! c #(-> % (assoc-in [:refused (slot i)] true) (assoc :last-refusal (dissoc e :report))))
                                :continue)
               :else (ctx/end c :done (when (already? e) :already))))))))))

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
  "After the child's ending e: ends as e when it was impossible or cut off (any refusal, in a reflex job, which runs
  one round); a rest when it could not finish (each in a row doubles it), or of ::rest-s, not counted, when its goal
  held before it started (it would only end again at once); else a run counted. :done after times runs, else
  :continue."
  [c e times]
  (cond
    (and (refused? e) (or (:reflex c) (#{:impossible :cut-off} (:kind e)))) (stop c e)
    (already? e)
    (do (ctx/update-mem! c assoc :rest {:until (+ (ctx/now c) (* 1000 (settings/get settings ::rest-s))) :after :already :kind :done})
        :continue)
    (refused? e)
    (let [fails (inc (:fails (ctx/mem c) 0))
          s (min (settings/get settings ::rest-max-s) (* (settings/get settings ::rest-s) (js/Math.pow 2 (dec fails))))]
      (ctx/update-mem! c assoc :fails fails :rest {:until (+ (ctx/now c) (* 1000 s)) :after (contract/cause-key e) :kind (:kind e)})
      :continue)
    :else
    (do (ctx/update-mem! c #(-> % (update :runs (fnil inc 0)) (dissoc :fails :rest)))
        (if (and times (>= (:runs (ctx/mem c)) times)) (ctx/end c :done nil) :continue))))

(defn repeat-def
  "The child in slot :c0; any ending drops its memory, so the next call starts it fresh, and :runs counts the runs.
  Never done, or done after times runs. Each ending: see ran!. Check: the child's check, while not resting. Held
  when the child's check would wait, so it never spins."
  [kid times]
  (let [kids [kid]
        [def args] kid]
    (contract-def
     (fn [c] (rest-check c (kid-check c (slot 0) def args)))
     (fn ^:async repeat-round [c]
       (let [e (await (call c kids 0))]
         (if (ending? e) (ran! c e times) :continue))))))

(defn until-def
  "Guard in slot :c0, child in :c1, which repeats like repeat's (rests too). Done at the
  top of a round once the guard's check passes (the guard itself never runs).
  Check: the guard's or the child's. Held when the child's check would wait."
  [guard kid]
  (let [kids [guard kid]
        [gdef gargs] guard
        [def args] kid
        reached? (fn [c] (nil? (ctx/child-wait c (slot 0) gdef gargs)))]
    (contract-def
     (fn [c] (or (reached? c) (rest-check c (kid-check c (slot 1) def args))))
     (fn ^:async until-round [c]
       (if (reached? c)
         (ctx/end c :done nil)
         (let [e (await (call c kids 1))]
           (if (ending? e) (ran! c e nil) :continue)))))))

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
