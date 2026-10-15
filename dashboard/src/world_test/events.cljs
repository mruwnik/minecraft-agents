(ns world-test.events
  "The live-tests @@test lines of a world-test run (TEST_EVENTS=1): plan, phase, one result per case and run, progress per fixture. Lines are built by engine.test-events."
  (:require [engine.test-events :as ev]))

(def enabled? ev/enabled?)

(defn emit! [m] (when enabled? (ev/emit! m)))

(defn plan [cases n-repeat] {:event "plan" :total (* (count cases) n-repeat)})

(defn phase [k] {:event "phase" :name (name k)})

(defn progress [done total] {:event "progress" :done done :total total :unit "fixtures"})

(defn- unmet-text [{:keys [expect evidence]}]
  (str (if (:event expect) (str "no event " (pr-str (:event expect)) " within " (:within-s expect) " s")
           (str "event " (pr-str (:no-event expect)) " seen within " (:for-s expect) " s"))
       (when evidence (str ": " evidence))))

(defn message
  "The first unmet expectation (one the run stopped before judging only after the failed :after checks), else the first failed :after check, else the run's :why; nil for a pass."
  [{:keys [expects afters why]}]
  (let [unmet (remove #(= :pass (:status %)) expects)]
    (or (some-> (first (remove :not-judged unmet)) unmet-text)
        (some #(when-not (:pass? %) (str "after " (pr-str (:check %)) ": " (:evidence %))) afters)
        (some-> (first unmet) unmet-text)
        why)))

(defn with-why
  "A failed case r with its first failure as :why when it has none, so no failed result says nothing."
  [{:keys [status why] :as r}]
  (if (and (= :fail status) (not why))
    (cond-> r (message r) (assoc :why (message r)))
    r))

(def outcomes {:pass "passed" :fail "failed" :error "error" :skipped "skipped" :flaky "flaky"})

(defn result [{:keys [id run status] :as r}]
  (let [m (when-not (= :pass status) (message r))]
    (cond-> {:event "result" :name (str id "#" run) :outcome (outcomes status "failed")}
      m (assoc :message (subs m 0 (min 2000 (count m)))))))

(defn fixtures-done
  "Fixture stems whose every run (expected: {stem runs}) is among the reported results ({:file :run})."
  [expected reported]
  (let [n (frequencies (map :file reported))]
    (vec (for [[stem runs] expected :when (>= (n stem 0) runs)] stem))))
