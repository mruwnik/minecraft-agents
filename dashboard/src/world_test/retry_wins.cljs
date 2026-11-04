(ns world-test.retry-wins
  "tools/event-counts.mjs retry-wins: counts the measured priors behind jobs.lib.cost.value ::retry-counts from the bodies'
  logs, folded into the counts file's :retry-wins (per body, kind and call: tries, won, seconds; a rotated log's calls stay; probe bodies are left out by the caller): per retry kind, the tries that followed a failed try of the same kind in the same job (a generation's
  job id), won or failed again, fitted as a Beta prior over the calls' shares (field-passes/beta-fit; the uniform [1 1]
  with none), and :try-s, the mean seconds from a failed try's end to the next try's end (with no retry, the mean seconds of
  one try, start to end). Kinds (`kinds`): :craft the craft action (a timeout fails), :furnace the furnace action (a busy
  slot fails), :dig the dig action (a timeout, a failure or an unreachable block fails), :place the place action (an occupied cell, no support, a failure or a timeout fails) and jumpPlace (no block raised, or part of them, fails), :walk a go-to's end (not arrived
  fails; read from the :result of logs before 2026-10-19, after it from the root go-to's :completed/:stopped and from a
  job.child_ended of a job id that has any go-to.* event, in the debug log: `covered` drops a body's go-to rows between its last
  :result and its first child_ended, which no log shows an end of), :respond respond-to-hostile's :hostile.call (a call that changed nothing fails), :risky-stay jobs.lib.near/risky-wait!'s
  events (:near.risky-wait a refusal, :near.risky-over the stay allowed after one), :comb-look jobs.apiary.harvest's look
  for comb (:apiary.comb-wait a look that took none, :apiary.comb-over a look that took some after one), :kit-later jobs.items.get-kit's bill after a :later stop (:kit.later-wait a stop rested on, :kit.later-over a bill round that ended otherwise after one); :pooled all kinds' tries together,
  the prior of a kind with none counted. :walk-round is one round inside a go-to: a go-to.retrying (a fruitless round that plans again) fails, and a go-to result that follows one is the call's last round (won when it arrived); a call is a run of retrying tries ending at a result (a job id holds many); the pooled fit leaves it out.
  Pure parts: scan-line, retries, report."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [world-test.field-passes :as passes]))

(def kinds "Action name -> [kind, the statuses that fail a try]." {"craft" [:craft #{"timeout"}] "furnace" [:furnace #{"busy"}] "dig" [:dig #{"timeout" "failed" "unreachable"}]
                                                 "place" [:place #{"occupied" "no-support" "failed" "timeout"}]
                                                 "jumpPlace" [:place #{"partial" "failed"}]})

(defn- field [line re] (second (re-find re line)))

(defn- ending-arrived
  "The :arrived of a child_ended line's ending data, or nil (a cut-off child has none)."
  [line]
  (when (str/includes? line ":arrived")
    (let [ev (try (reader/read-string line) (catch :default _ nil))]
      (get-in ev [:data :ending :data :arrived]))))

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

      (or (str/includes? line ":kind :kit.later-wait") (str/includes? line ":kind :kit.later-over"))
      {:kind :kit-later :key (key) :t (t) :edge :end :fail? (str/includes? line ":kind :kit.later-wait")}

      (str/includes? line ":kind :hostile.call")
      {:kind :respond :key (key) :t (t) :edge :end :fail? (str/includes? line ":changed false")}

      (str/includes? line ":kind :go-to.retrying")
      {:kind :walk-round :key (key) :t (t) :edge :end :fail? true :try (js/parseInt (field line #":try (\d+)"))}

      (and (str/includes? line ":kind :result") (re-find #":arrived (true|false)" line))
      {:kind :walk :key (key) :t (t) :edge :end :fail? (= "false" (field line #":arrived (true|false)")) :result? true}

      (and (re-find #":kind :(completed|stopped)" line) (str/includes? line ":name \"jobs.movement.go-to\"") (re-find #":arrived (true|false)" line))
      {:kind :walk :key (key) :t (t) :edge :end :fail? (= "false" (field line #":arrived (true|false)"))}

      (str/includes? line ":kind :child_ended")
      (when-some [arrived (ending-arrived line)]
        {:kind :walk :key (key) :t (t) :edge :end :fail? (false? arrived) :child? true})

      (str/includes? line ":kind :go-to.")
      {:kind :go-to-mark :key (key) :t (t) :edge :mark})))

(defn retries
  "{:key :won? :s} of each try that followed a failed one, from the scanned ends of one kind."
  [rows]
  (for [[_ ends] (group-by :key (filter #(= :end (:edge %)) rows))
        [a b] (partition 2 1 (sort-by :t ends))
        :when (:fail? a)]
    {:key (:key a) :won? (not (:fail? b)) :s (/ (- (:t b) (:t a)) 1000)}))

(defn covered
  "One body's rows without the go-to rows between its last :result and its first child_ended: no log shows a go-to child's end there."
  [rows]
  (let [last-result (reduce max ##-Inf (keep #(when (:result? %) (:t %)) rows))
        first-child (reduce min ##Inf (keep #(when (:child? %) (:t %)) rows))]
    (remove #(and (#{:walk :walk-round} (:kind %)) (< last-result (:t %) first-child)) rows)))

(defn- go-to-ends
  "The :walk rows of the scanned rows once: an end from the root go-to or a child_ended duplicates a :result within a second
  of it (logs of 2026-10-14 to 19 hold both); a child_ended counts only for a job id that has any go-to.* event (other jobs'
  endings carry :arrived too; go-to.retrying alone would keep only the calls that planned again). The marks are dropped."
  [rows]
  (let [retrying (into #{} (comp (filter #(#{:walk-round :go-to-mark} (:kind %))) (map :key)) rows)
        results (group-by :key (filter :result? rows))]
    (remove #(or (= :go-to-mark (:kind %))
                 (and (= :walk (:kind %)) (not (:result? %))
                  (or (and (:child? %) (not (retrying (:key %))))
                      (some (fn [r] (< (abs (- (:t r) (:t %))) 1000)) (results (:key %))))))
            rows)))

(defn- rounds
  "The :walk-round rows of the scanned rows, keyed per go-to call (a job id holds many): a go-to.retrying is a failed round,
  and a call's go-to result that follows a retrying is its last round, won when it arrived. A call ends at its result, or when
  a retrying's :try does not rise (the next call began)."
  [rows]
  (mapcat (fn [[key rs]]
            (:out (reduce (fn [{:keys [n open? last-try out] :as st} r]
                            (if (= :walk-round (:kind r))
                              (let [n (if (and open? (<= (:try r) last-try)) (inc n) n)]
                                (assoc st :n n :open? true :last-try (:try r) :out (conj out (assoc r :key (conj key n)))))
                              (if open?
                                {:n (inc n) :open? false :last-try 0 :out (conj out (assoc r :kind :walk-round :key (conj key n)))}
                                st)))
                          {:n 0 :open? false :last-try 0 :out []}
                          (sort-by :t rs))))
          (group-by :key (filter #(and (#{:walk :walk-round} (:kind %)) (= :end (:edge %))) rows))))

;; a try-s part is [seconds tries]
(defn- timed
  "[seconds n] of the tries start to end of each action's pair."
  [rows]
  (let [starts (into {} (keep #(when (= :start (:edge %)) [(:action %) (:t %)])) rows)
        ss (keep #(when-let [s (and (= :end (:edge %)) (starts (:action %)))] (/ (- (:t %) s) 1000)) rows)]
    [(reduce + 0 ss) (count ss)]))

(defn summarize
  "One body's scanned rows -> {kind {:calls {key {:tries :won :s}} :timed [s n]}}: what report needs of them, small enough to keep
  in the counts file (the key is [generation job-id ...], a call). The walk-round calls are kept apart from the pooled fit by kind."
  [rows]
  (let [rows (go-to-ends rows)
        rows (concat (remove #(= :walk-round (:kind %)) rows) (rounds rows))]
    (into {} (for [[kind rs] (group-by :kind rows)]
               [kind {:calls (into {} (for [[key ts] (group-by :key (retries rs))]
                                        [key {:tries (count ts) :won (count (filter :won? ts)) :s (reduce + (map :s ts))}]))
                      :timed (timed rs)}]))))

(defn fold
  "counts with each body's summary (body -> summarize) merged into :retry-wins; of a call or timed part read twice the longer
  stays (a rotated log may have lost the start of a call)."
  [counts summaries]
  (let [longer (fn [x y] (if (> (:tries y 0) (:tries x 0)) y x))
        kind-merge (fn [a b] {:calls (merge-with longer (:calls a) (:calls b))
                              :timed (if (> (second (:timed b)) (second (:timed a))) (:timed b) (:timed a))})]
    (update counts :retry-wins #(merge-with (fn [a b] (merge-with kind-merge a b)) % summaries))))

(defn- r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn- merge-summaries
  "One summary of the bodies' summaries (body -> summary): calls keyed by body too."
  [by-body]
  (apply merge-with (fn [a b] {:calls (merge (:calls a) (:calls b)) :timed (mapv + (:timed a) (:timed b))})
         (for [[body sm] by-body
               [kind {:keys [calls timed]}] sm]
           {kind {:calls (update-keys calls #(into [body] %)) :timed timed}})))

(defn- fit-calls
  "{:win :fail :s-total :tries :won :calls :spread} of calls ({key {:tries :won :s}}), the fit of their shares."
  [calls]
  (let [cs (vals calls)
        shares (map #(/ (:won %) (:tries %)) cs)]
    {:tries (reduce + (map :tries cs)) :won (reduce + (map :won cs)) :s (reduce + (map :s cs)) :shares shares
     :spread (when (seq cs) (passes/beta-spread shares))}))

(defn report
  "{kind {:win :fail :try-s :won :retries :calls}} of rows (one body's, scanned) or of a summary map (body -> summary, as kept in the counts file).
  A kind with no retry and no timed try is left out. A kind with retries but no fitted spread (too few calls) keeps its pooled mean at the mean
  pseudo-count total of the kinds that did fit; with none fitted, its pooled counts. :pooled fits all kinds' tries together, :walk-round apart."
  [x]
  (let [summary (merge-summaries (if (map? x) x {"" (summarize x)}))
        fits (for [[kind {:keys [calls timed]}] summary] (assoc (fit-calls calls) :kind kind :timed timed))
        pooled-calls (into {} (for [[kind {:keys [calls]}] summary :when (not= :walk-round kind)
                                    [k c] calls]
                                [(conj k kind) c]))
        spreads (keep :spread fits)
        pooled (when (seq spreads) (/ (reduce + spreads) (count spreads)))]
    (into (sorted-map)
          (for [{:keys [kind tries won shares spread s timed]} (cond-> (vec fits)
                                                                 (seq pooled-calls) (conj (assoc (fit-calls pooled-calls) :kind :pooled
                                                                                                 :timed (reduce #(mapv + %1 %2) [0 0] (map :timed fits)))))
                :let [[win fail] (cond (zero? tries) [1 1]
                                       spread (passes/beta-fit shares won tries)
                                       pooled (let [m (/ won tries)] [(r3 (* m pooled)) (r3 (* (- 1 m) pooled))])
                                       :else (passes/beta-fit shares won tries))
                      mean-s (cond (pos? tries) (/ s tries) (pos? (second timed)) (/ (first timed) (second timed)))]
                :when mean-s]
            [kind {:win win :fail fail :try-s (r3 mean-s) :won won :retries tries :calls (count shares)}]))))
