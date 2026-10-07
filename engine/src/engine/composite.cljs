(ns engine.composite
  "Job expressions as jobs: a parsed node (engine.expr) becomes [def args],
  where a combinator's def is a generic job whose children run through
  call-child. Child slots come from position: :c0, :c1, ... See README.md,
  Job expressions."
  (:require [engine.ctx :as ctx]
            [engine.expr :as expr]))

(defn slot [i] (keyword (str "c" i)))

(declare job)

(defn passes?
  "Whether child i of kids would run now, checked against its own sub-map."
  [c kids i]
  (let [[def args] (kids i)]
    (boolean (ctx/check-child c (slot i) def args))))

(defn seq-def
  "Children in order, one call per round; :at in memory is the next
  unfinished child. Check: that child's check. Done when the last is done.
  Declined when that child declines in its round (a reflex job is then dropped)."
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
                  (= i last-i) :done
                  :else (do (ctx/update-mem! c assoc :at (inc i)) :continue))))}))

(defn any-def
  "Each round, the first child whose check passes gets one call. Check: any
  child's check. Done when the child that ran is done. Declined when no
  child's check passes, so a reflex job whose children all decline is dropped."
  [kids]
  (let [first-passing (fn [c] (first (filter #(passes? c kids %) (range (count kids)))))]
    {:check (fn [c] (some? (first-passing c)))
     :round (fn ^:async any-round [c]
              (let [i (first-passing c)]
                (if (nil? i)
                  :declined
                  (let [[def args] (kids i)
                        r (await (ctx/call-child c (slot i) def args))]
                    (if (= :done r) :done :continue)))))}))

(defn repeat-def
  "The child in slot :c0; when it is done call-child drops its memory, so the
  next call starts it fresh, and :runs counts the runs. Never done, or done
  after times runs. Check: the child's check. Declined when the child declines,
  so it never spins."
  [kid times]
  (let [[def args] kid]
    {:check (fn [c] (boolean (ctx/check-child c (slot 0) def args)))
     :round (fn ^:async repeat-round [c]
              (let [r (await (ctx/call-child c (slot 0) def args))]
                (when (= :done r)
                  (ctx/update-mem! c update :runs (fnil inc 0)))
                (cond
                  (= :declined r) :declined
                  (and times (= :done r) (>= (:runs (ctx/mem c)) times)) :done
                  :else :continue)))}))

(defn until-def
  "Guard in slot :c0, child in :c1, which repeats like repeat's. Done at the
  top of a round once the guard's check passes (the guard itself never runs).
  Check: the guard's or the child's. Declined when the child declines."
  [guard kid]
  (let [[gdef gargs] guard
        [def args] kid
        reached? (fn [c] (boolean (ctx/check-child c (slot 0) gdef gargs)))]
    {:check (fn [c] (or (reached? c) (boolean (ctx/check-child c (slot 1) def args))))
     :round (fn ^:async until-round [c]
              (if (reached? c)
                :done
                (let [r (await (ctx/call-child c (slot 1) def args))]
                  (if (= :declined r) :declined :continue))))}))

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
