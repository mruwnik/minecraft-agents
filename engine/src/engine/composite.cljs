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

(defn kid-wait
  "Why child i of kids would wait now (ctx/child-wait, against its own sub-map), or nil."
  [c kids i]
  (let [[def args] (kids i)]
    (ctx/child-wait c (slot i) def args)))

(defn held
  "What a round returns when it cannot call a child that would wait with w: a reflex job runs one round, so it ends
  condition-failed with the wait as cause; a listed one yields (its condition waits)."
  [c w]
  (if (:reflex c)
    (ctx/end c :condition-failed (if (= [:reason] (keys w)) (:reason w) w))
    :continue))

(defn ^:async call
  "One ctx/call round of child i of kids, or held when it would wait."
  [c kids i]
  (if-let [w (kid-wait c kids i)]
    (held c w)
    (let [[def args] (kids i)]
      (await (ctx/call c (slot i) def args)))))

(defn contract-def
  "A combinator as a contract job: one condition, waits, giving its children's wait reasons (a map), which the
  scheduler asks of a listed one (ctx/check-wait) and a parent combinator of a child one."
  [waits round]
  {:conditions [[:fn waits]] :round round})

(defn seq-def
  "Children in order, one call per round; :at in memory is the next
  unfinished child. Waits: as that child. Done when the last is done
  (:already when every child's goal held before it started); a child that
  ends otherwise ends it the same way (held: see held)."
  [kids]
  (let [at (fn [c] (:at (ctx/mem c) 0))
        last-i (dec (count kids))]
    (contract-def
     (fn seq-waits [c] (kid-wait c kids (at c)))
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
  "Each round, the first child that would not wait gets one call; one that ends
  other than :done is skipped from then on (:refused in memory; the latest ending in :last-refusal). Waits: as the
  first child, while every child would wait and none refused. Done as the child that ran is done; once every passing child refused, ends as the last
  refusal. No child passing and none refused: held."
  [kids]
  (let [refused (fn [c] (:refused (ctx/mem c) {}))
        first-passing (fn [c] (first (filter #(and (not (contains? (refused c) (slot %))) (nil? (kid-wait c kids %))) (range (count kids)))))]
    (contract-def
     (fn any-waits [c] (when-not (or (some? (first-passing c)) (seq (refused c))) (kid-wait c kids 0)))
     (fn ^:async any-round [c]
       (let [i (first-passing c)]
         (if (nil? i)
           (if-let [e (:last-refusal (ctx/mem c))]
             (stop c e)
             (held c (or (kid-wait c kids 0) {:reason :not-ready})))
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

(defn rest-wait
  "A repeating combinator's wait: the child's, else :resting while the rest lasts."
  [c kids i]
  (or (kid-wait c kids i)
      (some-> (resting c) (assoc :reason :resting))))

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
  Never done, or done after times runs. Each ending: see ran!. Waits: rest-wait. Held when the child would wait, so
  it never spins."
  [kid times]
  (let [kids [kid]]
    (contract-def
     (fn repeat-waits [c] (rest-wait c kids 0))
     (fn ^:async repeat-round [c]
       (let [e (await (call c kids 0))]
         (if (ending? e) (ran! c e times) :continue))))))

(defn until-def
  "Guard in slot :c0, child in :c1, which repeats like repeat's (rests too). Done at the
  top of a round once the guard would not wait (the guard itself never runs).
  Waits: rest-wait, until the guard would not. Held when the child would wait."
  [guard kid]
  (let [kids [guard kid]
        reached? (fn [c] (nil? (kid-wait c kids 0)))]
    (contract-def
     (fn until-waits [c] (when-not (reached? c) (rest-wait c kids 1)))
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
