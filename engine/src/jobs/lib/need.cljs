(ns jobs.lib.need
  "A contract job's way to get what it lacks: call the fetch job as a child under the caller's remainder (ctx/call) and
  read its ending. tool!, item! and blocks! resolve to :continue while the child waits on the world, else to the ending
  to return or read (call! is the call alone): :done as the child gave it; any other with the cause {:reason :need|:tool|:free-slots ..} naming
  what was wanted, the child's ending as its :because, the kind kept (a limit stays a limit). A duplicate call is
  the engine's refusal. Options {:slot :how :explore} pass on to the fetch job.

  The :fetch arg of a job: false/nil (off), true (all kinds, all sources), a set of kinds (#{:tool :item}), or a map of
  limits {:what :how :minutes :fail-minutes :explore}. Limits merge, later wins: built-in, the job's code
  default, the call's arg.

  A fetch that ended :done, asked again with nothing of what it got used up since, ends condition-failed
  {:why :not-solved}: the child does not cure the caller's need. :return? true keeps the cell the body stood on when
  the first fetch began; return! walks back to it once a fetch has ended, done or not. Both live in the caller's job memory, so a cut
  goes on with them."
  (:require [clojure.set :as set]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.waits :as waits]
            [jobs.lib.util :as u]))

;; ------------------------------------------------------------------ the :fetch arg and its limits

(def kinds #{:tool :item})
(def fetch-ways "The ways of a job whose missing things are fetched: :carried is the job's own stock." [:carried :chest :craft :gather])
(def sources #{:chest :craft :gather})

(def built-in
  "The limits of :fetch true."
  {:what kinds :how sources :minutes 10 :fail-minutes 10})

(defn child-limits
  "The call keys of limits o for a fetch child job: :how as :ways (an obtain's :craft brings :smelt), :minutes as
  :spend {:job-time}."
  [job {:keys [how minutes]}]
  (cond-> {}
    how (assoc :ways (set (cond-> how (and (= job 'jobs.items.obtain) (contains? how :craft)) (conj :smelt))))
    minutes (assoc :spend {:job-time (* 60 minutes)})))

(def limit-keys [:what :how :minutes :fail-minutes :explore])

(defn limit-error
  "Why a limits map is bad, or nil."
  [m]
  (let [{:keys [what how minutes fail-minutes explore]} m
        pos-num? #(and (number? %) (js/isFinite %) (pos? %))]
    (cond
      (not (map? m)) "limits are a map"
      (seq (remove (set limit-keys) (keys m))) (str "unknown keys " (pr-str (vec (remove (set limit-keys) (keys m)))))
      (and (some? what) (not (and (set? what) (every? kinds what)))) (str ":what is a set of " (pr-str kinds))
      (and (some? how) (not (and (set? how) (every? sources how)))) (str ":how is a set of " (pr-str sources))
      (and (some? minutes) (not (pos-num? minutes))) ":minutes is a positive number"
      (and (some? fail-minutes) (not (pos-num? fail-minutes))) ":fail-minutes is a positive number"
      (and (some? explore) (not (boolean? explore))) ":explore is true or false")))

(defn parse-arg
  "The :fetch arg as a limits map ({} for true), nil when off, or {:error text}."
  [v]
  (cond
    (or (nil? v) (false? v)) nil
    (true? v) {}
    (set? v) (if (every? kinds v) {:what v} {:error (str ":fetch set holds kinds of " (pr-str kinds))})
    (map? v) (if-let [e (limit-error v)] {:error (str ":fetch " e)} v)
    :else {:error ":fetch is true, false, a set of kinds or a map of limits"}))

(def option?
  "The spec of a job's :fetch arg."
  (a/pred "true, false, a set of kinds (:tool :item) or a map of limits {:what :how :minutes :fail-minutes :explore}"
          #(nil? (:error (parse-arg %)))))

(defn call-ways
  "The ways the call of the job running in c allows: the frozen call's, before it starts the args', else all."
  [c]
  (set (or (:ways (ctx/frozen-call c)) (:ways (:args c)) fetch-ways)))

(defn fetch-how
  "The sources a fetch of the job running in c may use: its call's ways but :carried."
  [c]
  (disj (call-ways c) :carried))

(defn ways-of-fetch
  "The call ways for a job still on the old :fetch arg v: :carried alone when it is off, else its :how sources too."
  [v]
  (let [arg (parse-arg v)]
    (if (:error arg) (set fetch-ways) (into #{:carried} (when arg (or (:how arg) sources))))))

(defn merge-limits
  "Limits: built-in, then job-default, then call."
  [job-default call]
  (let [clean #(into {} (remove (comp nil? val)) (select-keys % limit-keys))]
    (merge built-in (clean job-default) (clean call))))

(defn opts
  "The fetch limits of the job running in c (job: its namespace symbol), nil when :fetch is off."
  ([c job] (opts c job nil))
  ([c job job-default]
   (let [arg (parse-arg (:fetch (:args c)))]
     (cond
       (nil? arg) nil
       :else (merge-limits job-default arg)))))

(defn allows?
  "Whether a job's :fetch arg v lets it fetch things of kind (:tool or :item)."
  [v kind]
  (let [arg (parse-arg v)]
    (boolean (and arg (not (:error arg)) (contains? (:what (merge-limits nil arg)) kind)))))

(defn fetch-allowed?
  "Whether the job running in c may fetch things of kind (:tool or :item): its call's ways when it has some, else its
  old :fetch arg."
  [c kind]
  (if (contains? (ctx/frozen-call c) :ways)
    (boolean (seq (fetch-how c)))
    (allows? (:fetch (:args c)) kind)))

(defn tighten
  "Limits o tightened by a wait's limits l: the sources both allow, the shorter :minutes; other keys of l fill in."
  [o l]
  (cond-> (merge l o)
    (and (:how l) (:how o)) (assoc :how (set/intersection (:how l) (:how o)))
    (and (:minutes l) (:minutes o)) (assoc :minutes (min (:minutes l) (:minutes o)))))

(defn plan-for
  "The fetch for wait reason w under limits o: {:kind :job :args :key}, or nil when w is not fetchable or its kind
  is not in (:what o). A :no-tool wait names :block, :needs (an item) or :kind (a tool kind such as \"hoe\"). A :need wait's :count is how many obtain gets (default 1)."
  [w o]
  (let [w (merge (when (map? (:cause w)) (:cause w)) w)
        {:keys [reason item any-of block needs]} w
        explore (or (:explore o) false)
        with-explore #(assoc % :explore explore)
        pl (case reason
             :no-tool (cond
                        block {:kind :tool :job 'jobs.items.get-tool :args {:block block :explore explore} :key [:tool block]}
                        needs {:kind :tool :job 'jobs.items.get-tool :args {:item needs :explore explore} :key [:tool needs]}
                        (:kind w) {:kind :tool :job 'jobs.items.get-tool :args {:kind (:kind w) :explore explore} :key [:tool (:kind w)]})
             :need (cond
                     item {:kind :item :job 'jobs.items.obtain :args (with-explore {:item item :count (or (:count w) 1)}) :key [:item [item]]}
                     (seq any-of) {:kind :item :job 'jobs.items.obtain :args (with-explore {:any-of (vec any-of) :count (or (:count w) 1)})
                                   :key [:item (vec any-of)]})
             nil)]
    (when (and pl (contains? (:what o) (:kind pl)))
      pl)))

(def full-reasons "Fetch endings that mean the bag has no room." #{:inventory-full :bag-full})

(defn ending-for
  "Child ending e of a fetch for wanted (a map of what was asked, e.g. {:item .. :count ..}) as this job's ending:
  :done and :continue pass; a bag-full ending has the cause :free-slots, else cause-reason (:need or :tool)."
  [e cause-reason wanted]
  (if (or (= :continue e) (= :done (:kind e)))
    e
    (let [why (contract/cause-key e)
          reason (if (full-reasons why) :free-slots cause-reason)]
      (-> (contract/ending (:kind e) (assoc wanted :reason reason :why why) (:data e))
          (contract/with-because e)))))

(defn counts
  "{name n} carried of names (0 when none)."
  [c names]
  (let [inv (u/inventory (:primitives c))]
    (into {} (map (fn [n] [n (transduce (comp (filter #(= n (:name %))) (map :count)) + 0 inv)])) names)))

(defn got-names
  "The names a done fetch's ending e is about: what was wanted, or the tool it found."
  [wanted e]
  (or (some-> (:item wanted) vector) (:any-of wanted) (some-> (get-in e [:data :tool]) vector) []))

(defn repeat-done?
  "Whether the done fetch booked in the caller's memory was for want, and none of what it got has been used up."
  [c want]
  (when-let [{w :want have :have} (:need/done (ctx/mem c))]
    (and (= want w) (seq have) (let [now (counts c (keys have))] (every? (fn [[n k]] (>= (now n) k)) have)))))

(defn feet [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn ^:async call!
  "ctx/call of a fetch child, but a child not yet started whose start check says :later for a readiness reason
  (jobs.lib.waits/wait-reasons: night, exposure, a hostile near, low health) is not called: the caller waits with that
  reason (:continue), where the child would end condition-failed and read as a failed fetch."
  [c slot job args]
  (let [s (when (nil? (get-in (ctx/mem c) [:children slot])) (ctx/child-status c slot job args))]
    (if (and (= :later (first s)) (contains? waits/wait-reasons (:reason (nth s 2))))
      (do (some-> (:child-wait c) (reset! (contract/status-cause s))) :continue)
      (waits/waiting-out c (await (ctx/call c slot job args))))))

(defn booked-total
  "The count to carry at least for n more of names: n and what is carried now, booked in the caller's memory when the child
  of slot is fresh, so the items the child brings in do not move the goal (ctx/call evaluates the args every round)."
  [c slot names n fresh?]
  (if fresh?
    (let [t (+ n (reduce + 0 (vals (counts c names))))]
      (ctx/update-mem! c assoc-in [:need/total slot] t)
      t)
    (get-in (ctx/mem c) [:need/total slot])))

(defn carry-count
  "booked-total for the child about to be called (or already running) in slot: the :count of an item goal (\"carry at least\")
  that asks for n more of names."
  [c slot names n]
  (booked-total c slot names n (nil? (get-in (ctx/mem c) [:children slot]))))

(defn total-of
  "The count to carry at least for a fetch of args' :count more of its item(s), booked once per child."
  [c slot job args fresh?]
  (when (and (= 'jobs.items.obtain job) (:count args))
    (booked-total c slot (if (:item args) [(:item args)] (:any-of args)) (:count args) fresh?)))

(defn cause-why
  "The reason of the get that failed (its own, which need keeps as :why), else the cause's."
  [e]
  (or (get-in e [:cause :why]) (contract/cause-key e)))

(defn ^:async fetch!
  "Get :count more of what args name (jobs.items.obtain's :count is the amount to carry, so the carried is added here)."
  [c job args cause-reason wanted {:keys [slot how explore return? minutes for-plan]}]
  (let [slot (or slot :need)
        args (cond-> (merge (assoc args :explore (boolean explore)) (child-limits job {:how how :minutes minutes}))
                for-plan (assoc :for-plan for-plan))
        want [job args]
        identity-of [job (dissoc args :count :avoid :spend)]
        _ (when (not= identity-of (get-in (ctx/mem c) [:need/slot-want slot]))
            (ctx/update-mem! c #(-> (update % :children dissoc slot) (assoc-in [:need/slot-want slot] identity-of))))
        fresh? (nil? (get-in (ctx/mem c) [:children slot]))]
    (if (and fresh? (repeat-done? c want))
      (contract/ending :condition-failed (assoc wanted :reason cause-reason :why :not-solved) nil)
      (do
        (when (and fresh? return? (not (:need/return (ctx/mem c))))
          (ctx/update-mem! c assoc :need/return {:cell (feet c)}))
        (let [total (total-of c slot job args fresh?)
              r (await (call! c slot job (cond-> args total (assoc :count total))))
              e (if (= :continue r) :continue (ending-for r cause-reason wanted))]
          (when-not (= :continue e)
            (ctx/update-mem! c #(cond-> (cond-> % (= :done (:kind e)) (assoc :need/done {:want want :have (counts c (got-names wanted e))}))
                                  (:need/return %) (assoc-in [:need/return :back] true))))
          e)))))

(defn ^:async return!
  "For a caller using :return?: nil while nothing is to walk back (no mark, no done fetch yet, or there); else one
  round of a go-to child (slot :need-back, range 0) to the kept cell: :continue while it walks, nil once it ended
  (arrived or not: the caller judges where it stands). Call it first in each round."
  [c]
  (let [{:keys [cell back]} (:need/return (ctx/mem c))]
    (cond
      (not back) nil
      (= cell (feet c)) (do (ctx/update-mem! c dissoc :need/return) nil)
      :else (let [[x y z] cell
                  r (await (child/call-walk* c :need-back {:pos {:x x :y y :z z} :range 0 :ways #{:walk :clear-seal}
                                                            :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
              (when-not (= :continue r)
                (ctx/update-mem! c dissoc :need/return))
              (when (= :continue r) :continue)))))

(defn item!
  "Get n (default 1) of item (a name) with jobs.items.obtain."
  ([c item] (item! c item 1 nil))
  ([c item n] (item! c item n nil))
  ([c item n opts]
   (fetch! c 'jobs.items.obtain {:item item :count n} :need {:item item :count n} opts)))

(defn ^:async table!
  "A crafting table to craft at: nil when one is in reach (in-reach?) or carried, else item! of one (:continue or its
  ending); a crafting job then puts the carried table down."
  [c in-reach?]
  (when-not (or in-reach? (pos? (get (counts c ["crafting_table"]) "crafting_table")))
    (await (item! c "crafting_table" 1 {:slot :need-table}))))

(defn blocks!
  "Get n of any of the names (building blocks) with jobs.items.obtain."
  ([c names n] (blocks! c names n nil))
  ([c names n opts]
   (fetch! c 'jobs.items.obtain {:any-of (vec names) :count n} :need {:any-of (vec names) :count n} opts)))

(defn tool!
  "Get a tool with jobs.items.get-tool; what is {:block b}, {:item i} or {:kind k}."
  ([c what] (tool! c what nil))
  ([c what opts]
   (fetch! c 'jobs.items.get-tool what :tool what opts)))
