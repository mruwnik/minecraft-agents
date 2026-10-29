(ns world-test.retry-wins
  "tools/event-counts.mjs retry-wins: counts the measured priors behind jobs.lib.cost.value ::retry-counts from the bodies'
  logs alone (no fold): per retry kind, the tries that followed a failed try of the same kind in the same job (a generation's
  job id), won or failed again, fitted as a Beta prior over the calls' shares (field-passes/beta-fit; the uniform [1 1]
  with none), and :try-s, the mean seconds from a failed try's end to the next try's end (with no retry, the mean seconds of
  one try, start to end). Kinds (`kinds`): :craft the craft action (a timeout fails), :furnace the furnace action (a busy
  slot fails), :dig the dig action (a timeout, a failure or an unreachable block fails), :place the place action (an occupied cell, no support, a failure or a timeout fails), :walk a go-to's result (not arrived
  fails), :respond respond-to-hostile's :hostile.call (a call that changed nothing fails), :risky-stay jobs.lib.near/risky-wait!'s
  events (:near.risky-wait a refusal, :near.risky-over the stay allowed after one), :comb-look jobs.apiary.harvest's look
  for comb (:apiary.comb-wait a look that took none, :apiary.comb-over a look that took some after one); :pooled all kinds' tries together,
  the prior of a kind with none counted.
  Pure parts: scan-line, retries, report."
  (:require [clojure.string :as str]
            [world-test.field-passes :as passes]))

(def kinds "Action name -> [kind, the statuses that fail a try]." {"craft" [:craft #{"timeout"}] "furnace" [:furnace #{"busy"}] "dig" [:dig #{"timeout" "failed" "unreachable"}]
                                                 "place" [:place #{"occupied" "no-support" "failed" "timeout"}]})

(defn- field [line re] (second (re-find re line)))

(defn scan-line
  "One log line -> {:kind :key [generation job-id] :t ms :fail? bool :edge :start|:end :action id} of a try of a retry kind, else nil."
  [line]
  (let [t #(js/parseFloat (field line #":time-ms (\d+)"))
        key #(vector (field line #":generation-id \"([^\"]*)\"") (field line #":job-id \"([^\"]*)\""))]
    (cond
      (and (str/includes? line ":source :action") (or (str/includes? line ":kind :done") (str/includes? line ":kind :started")))
      (when-let [[kind fails] (kinds (field line #":name \"([^\"]*)\""))]
        (let [done? (str/includes? line ":kind :done")]
          {:kind kind :key (key) :t (t) :edge (if done? :end :start) :action (field line #":action-id \"([^\"]*)\"")
           :fail? (and done? (contains? fails (field line #":status \"([^\"]*)\"")))}))

      (or (str/includes? line ":kind :near.risky-wait") (str/includes? line ":kind :near.risky-over"))
      {:kind :risky-stay :key (key) :t (t) :edge :end :fail? (str/includes? line ":kind :near.risky-wait")}

      (or (str/includes? line ":kind :apiary.comb-wait") (str/includes? line ":kind :apiary.comb-over"))
      {:kind :comb-look :key (key) :t (t) :edge :end :fail? (str/includes? line ":kind :apiary.comb-wait")}

      (str/includes? line ":kind :hostile.call")
      {:kind :respond :key (key) :t (t) :edge :end :fail? (str/includes? line ":changed false")}

      (and (str/includes? line ":kind :result") (re-find #":arrived (true|false)" line))
      {:kind :walk :key (key) :t (t) :edge :end :fail? (= "false" (field line #":arrived (true|false)"))})))

(defn retries
  "{:key :won? :s} of each try that followed a failed one, from the scanned ends of one kind."
  [rows]
  (for [[_ ends] (group-by :key (filter #(= :end (:edge %)) rows))
        [a b] (partition 2 1 (sort-by :t ends))
        :when (:fail? a)]
    {:key (:key a) :won? (not (:fail? b)) :s (/ (- (:t b) (:t a)) 1000)}))

(defn- try-s
  "The mean seconds of one try: start to end of each action's pair."
  [rows]
  (let [starts (into {} (keep #(when (= :start (:edge %)) [(:action %) (:t %)])) rows)
        ss (keep #(when-let [s (and (= :end (:edge %)) (starts (:action %)))] (/ (- (:t %) s) 1000)) rows)]
    (when (seq ss) (/ (reduce + ss) (count ss)))))

(defn- r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn- fit
  "{:kind {:tries :won :shares :s :spread}} of the scanned rows, :spread the moments' pseudo-count total or nil."
  [rows]
  (for [[kind rs] (group-by :kind rows)
        :let [tries (retries rs)
              shares (for [[_ ts] (group-by :key tries)] (/ (count (filter :won? ts)) (count ts)))]]
    {:kind kind :rs rs :tries tries :won (count (filter :won? tries)) :shares shares
     :spread (when (seq tries) (passes/beta-spread shares))}))

(defn report
  "{kind {:win :fail :try-s :won :retries :calls}} of the scanned rows (a kind with no retry and no timed try is left out). A
  kind with retries but no fitted spread (too few calls) keeps its pooled mean at the mean pseudo-count total of the kinds
  that did fit; with none fitted, its pooled counts. :pooled fits all kinds' tries together."
  [rows]
  (let [fits (fit rows)
        all (mapcat (fn [{:keys [kind tries]}] (map #(update % :key conj kind) tries)) fits)
        all-shares (for [[_ ts] (group-by :key all)] (/ (count (filter :won? ts)) (count ts)))
        spreads (keep :spread fits)
        pooled (when (seq spreads) (/ (reduce + spreads) (count spreads)))]
    (into (sorted-map)
          (for [{:keys [kind rs tries won shares spread]} (cond-> fits
                                                            (seq all) (concat [{:kind :pooled :rs rows :tries all :won (count (filter :won? all))
                                                                                :shares all-shares :spread (passes/beta-spread all-shares)}]))
                :let [[win fail] (cond (empty? tries) [1 1]
                                       spread (passes/beta-fit shares won (count tries))
                                       pooled (let [m (/ won (count tries))] [(r3 (* m pooled)) (r3 (* (- 1 m) pooled))])
                                       :else (passes/beta-fit shares won (count tries)))
                      s (if (seq tries) (/ (reduce + (map :s tries)) (count tries)) (try-s rs))]
                :when s]
            [kind {:win win :fail fail :try-s (r3 s) :won won :retries (count tries) :calls (count shares)}]))))
