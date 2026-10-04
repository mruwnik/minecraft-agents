(ns engine.planner-options-test
  "The non-default-options cases of bench-lang/fixtures-equal.test.mjs against the ClojureScript planner: one query
  under several settings (weight, maxNodes, the box, the goal flood, costs), goals in an unloaded column or ignoring y,
  and create-search in slices matching plan. The JS planner's answers are recorded in test/planner-bench.json (see
  engine.planner-bench-test)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.planner-bench-test :as bench :refer [recorded record disagreements]]
            [engine.planner-fixture :as pf]))

(defn samples [] (:samples @recorded))

(defn course-query
  "{:snapshot :query} of a named course, keyword-keyed"
  [name]
  (let [^js c (.courseSnapshot ^js @pf/courses name)]
    {:snapshot (.-snapshot c)
     :query {:from (js->clj (.-from c) :keywordize-keys true) :goal (js->clj (.-goal c) :keywordize-keys true)}}))

(defn plan-with [name options]
  (let [{:keys [snapshot query]} (course-query name)] (pf/plan snapshot query options)))

(deftest options-agree-with-the-js-planner-on-every-sample-course
  (doseq [[what options] (:optionSets @recorded)
          :let [expected (get-in @recorded [:options (keyword what)])
                options (js->clj (clj->js options) :keywordize-keys true)]]
    (is (= [] (disagreements (samples) #(plan-with % options) #(expected (keyword %)))) what)))

(defn reasons-under [options] (set (map #(:reason (plan-with % options)) (samples))))

(deftest options-change-the-reasons-they-should
  (is (contains? (reasons-under {:maxNodes 40}) "budget"))
  (is (contains? (reasons-under {:floodAfter 5}) "goal-enclosed")))

(defn shift-east [dx {:keys [snapshot query]}]
  {:snapshot snapshot :query (update-in query [:goal :x] + dx)})
(defn planar [{:keys [snapshot query]}]
  {:snapshot snapshot :query (assoc-in query [:goal :kind] "xz")})

(def goal-variants
  {"east400" (partial shift-east 400)
   "east48" (partial shift-east 48)
   "xz" planar})

(deftest goal-variants-agree-with-the-js-planner
  (doseq [[variant make] goal-variants]
    (is (= [] (disagreements (samples)
                             (fn [name] (let [{:keys [snapshot query]} (make (course-query name))] (pf/plan snapshot query)))
                             (fn [name] (get-in @recorded [:goals (keyword (str name "|" variant))]))))
        variant)))

(deftest a-goal-in-an-unloaded-column-ends-in-goal-unloaded-on-some-courses
  (let [reasons (set (map (fn [name] (let [{:keys [snapshot query]} (shift-east 400 (course-query name))]
                                       (:reason (pf/plan snapshot query))))
                          (samples)))]
    (is (contains? reasons "goal-unloaded"))))

(deftest a-start-that-is-not-standable-and-a-goal-that-is-not-standable
  (let [{:keys [snapshot query]} (course-query "full-open")
        in-stone (assoc-in query [:from :y] (- (get-in query [:from :y]) 3))
        buried (assoc query :goal (pf/near (get-in query [:goal :x]) (- (get-in query [:goal :y]) 3) (get-in query [:goal :z]) 0))
        answers (mapv #(record (pf/plan snapshot %)) [in-stone buried])]
    (is (= [["none" "start-not-standable"] ["none" "goal-not-standable"]] (mapv (juxt first second) answers)))
    (is (every? true? (map bench/same-answer? answers (:standable @recorded))))))

;; ---- create-search in slices ----

(defn slice-run
  "step a search in slices until it is done: {:steps [bool ...] :result record}"
  [name options slice]
  (let [{:keys [snapshot query]} (course-query name)
        {:keys [step result]} (pf/create-search snapshot query options)
        steps (loop [steps []]
                (let [done (step slice)
                      steps (conj steps (boolean done))]
                  (cond done steps
                        (> (count steps) 100000) steps
                        :else (recur steps))))]
    {:steps steps :result (record (result))}))

(deftest create-search-in-slices-matches-plan-and-the-js-planner
  (doseq [slice [100 1 7]
          :let [names (take (case slice 1 6 (count (samples))) (samples))
                runs (map #(vector % (slice-run % {:floodAfter 5} slice)) names)
                expected (get-in @recorded [:options (keyword "the flood after 5 expansions")])]]
    (is (= [] (vec (keep (fn [[name {:keys [result]}]]
                           (let [want (expected (keyword name))]
                             (when-not (bench/same-answer? result want) [name result want])))
                         runs)))
        (str "slices of " slice))
    (is (every? (fn [[_ {:keys [steps]}]] (and (true? (peek steps)) (not-any? true? (pop steps)))) runs)
        (str "slices of " slice ": only the last step is done"))))

(deftest create-search-result-before-the-search-is-done-ends-with-reason-budget
  (let [{:keys [snapshot query]} (course-query "rand50-we")
        {:keys [step result]} (pf/create-search snapshot query)]
    (step 20)
    (is (= "budget" (:reason (result))))))
