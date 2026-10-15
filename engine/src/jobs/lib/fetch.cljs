(ns jobs.lib.fetch
  "The :fetch option of a job that can wait for a missing tool or item (design rule 10: getting it is an option of the
  job; without it the job waits with the reason).

  A job with :fetch reads its own wait reason (or the booked declined child's, jobs.lib.declined) and, when the
  reason is one a fetch job can cure, its check passes and its round runs that fetch job as the child in slot :fetch
  (jobs.items.get-tool for :no-tool, with the :explore limit; jobs.items.obtain for :need). It does not pass :fetch to its children: their waits
  come up to it. A fetch that ends without the thing is remembered as :fetch/failed for :fail-minutes (or until more logs, coal or wool are carried than at the failure); meanwhile the
  job waits with its reason plus {:fetch {:failed reason ...}} and nothing is tried again.

  The :fetch arg: false/nil (off), true (all kinds, all sources), a set of kinds (#{:tool :item}), or a map of limits
  {:what :how :depth :minutes :fail-minutes :explore}; :explore (default false: reflexes must not walk off) lets the
  fetch jobs walk and look for a raw need not seen. Limits merge, later wins: built-in, the job's code default, the body's
  the call's own arg.

  Chest stock (body memory :fetch/stock) is jobs.lib.stock."
  (:require [jobs.lib.child :as child]
            [clojure.set :as set]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.declined :as declined]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-fetch-calls {:default 400 :doc "Fetch rounds of one fetch! before it gives the round back with :continue." :spec (a/int-in 1 nil)}})

;; ------------------------------------------------------------------ the option and its limits

(def kinds #{:tool :item})
(def sources #{:chest :craft :gather})

(def built-in
  "The limits of :fetch true."
  {:what kinds :how sources :depth 7 :minutes 10 :fail-minutes 10})

(defn child-limits
  "The call keys of limits o for a fetch child job: :how as :ways (an obtain's :craft brings :smelt), :minutes as
  :spend {:time}. :depth is no child key (the engine refuses a duplicate call)."
  [job {:keys [how minutes]}]
  (cond-> {}
    how (assoc :ways (set (cond-> how (and (= job 'jobs.items.obtain) (contains? how :craft)) (conj :smelt))))
    minutes (assoc :spend {:time (* 60 minutes)})))

(def limit-keys [:what :how :depth :minutes :fail-minutes :explore])

(defn limit-error
  "Why a limits map is bad, or nil."
  [m]
  (let [{:keys [what how depth minutes fail-minutes explore]} m
        pos-num? #(and (number? %) (js/isFinite %) (pos? %))]
    (cond
      (not (map? m)) "limits are a map"
      (seq (remove (set limit-keys) (keys m))) (str "unknown keys " (pr-str (vec (remove (set limit-keys) (keys m)))))
      (and (some? what) (not (and (set? what) (every? kinds what)))) (str ":what is a set of " (pr-str kinds))
      (and (some? how) (not (and (set? how) (every? sources how)))) (str ":how is a set of " (pr-str sources))
      (and (some? depth) (not (and (int? depth) (>= depth 0)))) ":depth is a whole number, 0 or more"
      (and (some? minutes) (not (pos-num? minutes))) ":minutes is a positive number"
      (and (some? fail-minutes) (not (pos-num? fail-minutes))) ":fail-minutes is a positive number"
      (and (some? explore) (not (or (boolean? explore) (map? explore)))) ":explore is true, false or jobs.explore.search options")))

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
  (a/pred "true, false, a set of kinds (:tool :item) or a map of limits {:what :how :depth :minutes :fail-minutes :explore}"
          #(nil? (:error (parse-arg %)))))

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

;; ------------------------------------------------------------------ waits to fetch jobs

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

;; ------------------------------------------------------------------ failures

(def failed-kind :fetch/failed)
(def failed-policy {:cap 32 :ttl (* 24 60 60 1000)})

(defn raw-carried
  "Carried counts of the raw materials (facts/gatherable?) a failed fetch could have crafted from."
  [c]
  (reduce (fn [m {:keys [name count]}] (cond-> m (facts/gatherable? name) (update name (fnil + 0) count)))
          {} (u/inventory (:primitives c))))

(defn failure
  "The live failure booked for key, or nil: not once the time is up or more of a raw material is carried than when it
  was booked (a fetch that found nothing to craft from may succeed now)."
  [c key]
  (let [now (ctx/now c)
        have (raw-carried c)]
    (some #(let [d (:data %)]
             (when (and (= key (:key d)) (< now (:until d))
                        (every? (fn [[k n]] (<= n (get (:raw d) k 0))) have))
               d))
          (rseq (ctx/entries c failed-kind)))))

(defn shown
  "The :fetch field of a wait whose fetch failed."
  [f]
  (into {} (remove (comp nil? val)) (select-keys f [:failed :why :item :any-of :block :tried :chain])))

;; ------------------------------------------------------------------ check and round

(defn check
  "For the check of the job (symbol) whose problem is wait reason w (non-nil): true when the round will fetch for it,
  else ctx/wait with w, plus {:fetch {:failed ...}} when a fetch for it failed lately."
  [c job w]
  (let [o (opts c job)
        pl (when o (plan-for w o))
        f (when pl (failure c (:key pl)))]
    (cond
      (nil? pl) (ctx/wait c w)
      f (ctx/wait c (assoc w :fetch (shown f)))
      :else true)))

(defn failed?
  "Whether the fetch for wait reason w of job already failed lately (nothing left to wait for)."
  [c job w]
  (boolean (when-let [pl (some->> (opts c job) (plan-for w))]
             (failure c (:key pl)))))

(defn tighten
  "Limits o tightened by a wait's limits l: the sources both allow, the shorter :minutes; other keys of l fill in."
  [o l]
  (cond-> (merge l o)
    (and (:how l) (:how o)) (assoc :how (set/intersection (:how l) (:how o)))
    (and (:minutes l) (:minutes o)) (assoc :minutes (min (:minutes l) (:minutes o)))))

(defn due
  "What the round of job should fetch for wait reason w (nil when none): the plan with the limits as :opts (w's :limits
  tighten them, for an optional fetch)."
  [c job w]
  (when w
    (let [o (some-> (opts c job) (tighten (:limits w)))]
      (cond
        (nil? o) nil
        :else (when-let [pl (plan-for w o)]
                (when-not (failure c (:key pl))
                  (assoc pl :opts o :wait w)))))))

;; :fetched [key wait] in job memory: the last fetch that ended :done and the wait it was for. The same wait
;; straight after means the child did not solve it.
(defn repeat-done? [c key wait]
  (= [key wait] (:fetched (ctx/mem c))))

(defn mark-done! [c key wait]
  (ctx/update-mem! c assoc :fetched [key wait]))

(defn settle!
  "For a round with nothing to fetch: a fetch under way whose need went away (the thing arrived before the fetch job
  ended, e.g. in its withdraw) is told done (info fetch.done) and its child memory dropped, so a later fetch starts
  fresh."
  [c]
  (ctx/update-mem! c dissoc :fetched)
  (when-let [key (:fetching (ctx/mem c))]
    (ctx/update-mem! c #(-> % (dissoc :fetching) (update :children dissoc :fetch)))
    (ctx/emit! c :fetch.done :info {:key key :text (str "fetched " (pr-str (second key)))})))

(defn feet [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn ^:async walk-back!
  "One go-to round (child :fetch-back, range 0) back to the cell the body left to fetch; the mark is dropped once
  there or when the walk ends (arrived or not: the job judges where it stands). :continue while go-to waits, else :again."
  [c cell]
  (let [[x y z] cell
        r (await (child/call-walk c :fetch-back {:pos {:x x :y y :z z} :range 0 :escalate false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (when-not (= :continue r)
      (ctx/update-mem! c dissoc :fetch-return))
    (if (= :continue r) :continue :again)))

(defn booked-wait
  "The wait reason of the child booked by jobs.lib.declined, nil when none is booked or its check passes now."
  [c]
  (when-let [{:keys [slot job args]} (get (ctx/mem c) declined/key*)]
    (b/child-wait c slot job args)))

(defn declined-check
  "jobs.lib.declined/check for a job with :fetch: the booked child's wait is fetched for when it can be."
  [c job]
  (if-let [w (booked-wait c)]
    (check c job w)
    true))

(defn need-name
  "What a fetch's args name: the item, a tool for a block, a tool kind or the any-of list."
  [args]
  (or (:item args)
      (some->> (:block args) (str "a tool for "))
      (some->> (:kind args) (str "a "))
      (pr-str (:any-of args))))

(defn book-failed!
  "Book the failed fetch (until now + :fail-minutes), warn fetch.failed, :again (the check now waits)."
  [c {:keys [key opts wait args]} res]
  (let [f (merge (select-keys args [:item :any-of :block :kind])
                 {:key key :until (+ (ctx/now c) (* 60000 (:fail-minutes opts))) :failed (:reason res :failed) :raw (raw-carried c)}
                 (select-keys res [:tried :chain :why]))]
    (ctx/remember! c failed-kind f failed-policy)
    (ctx/update-mem! c #(-> % (dissoc :fetching :fetched :fetch-return) (update :children dissoc :fetch)))
    (ctx/emit! c :fetch.failed :warn (assoc (shown f) :for (:reason wait)
                                            :text (str "could not fetch " (need-name args)
                                                       ": " (name (:failed f)))))
    :again))

(defn ^:async round!
  "One round of the fetch in slot :fetch for plan pl (from due). :continue while the child waits on the world (a child
  that declines while its furnace cooks too), :again after it ended or failed."
  [c pl]
  (let [{:keys [job key opts]} pl
        args (merge (:args pl) (child-limits job opts) (select-keys opts [:fail-minutes]))
        pl (assoc pl :args args)]
    (when-not (= key (:fetching (ctx/mem c)))
      (ctx/update-mem! c #(cond-> (assoc % :fetching key)
                            (not= key (first (:fetched %))) (dissoc :fetched)
                            (and (:return? pl) (not (:fetch-return %))) (assoc :fetch-return (feet c))))
      (ctx/emit! c :fetch.started :info {:for (:reason (:wait pl)) :job job :args (:args pl)
                                         :text (str "fetching " (need-name args)
                                                    " for " (name (:reason (:wait pl))))}))
    (let [r (await (ctx/call-child c :fetch job args))]
      (case r
        :continue :continue
        :declined (let [w (or (b/child-wait c :fetch job args) {:reason :not-ready})]
                    (if (#{:cooking :night-sheltered} (:reason w)) :continue (book-failed! c pl w)))
        (let [res (ctx/child-result c :fetch)]
          (if (= :done (:status res))
            (do (ctx/update-mem! c dissoc :fetching)
                (mark-done! c key (:wait pl))
                (ctx/emit! c :fetch.done :info (merge {:for (:reason (:wait pl)) :text (str "fetched for " (name (:reason (:wait pl))))}
                                                      (select-keys res [:got :item :tool])))
                :again)
            (book-failed! c pl res)))))))

(defn ^:async step!
  "One round's fetch part for job with wait reason w: runs the due fetch (round!) and returns its round result
  (:continue: a child waits on the world, yield; :again: go round again), or settles a finished one and returns nil
  (the job does its own work).
  opts {:return? true}: a job whose work depends on where the body stands (a stair) first walks back to the cell it
  stood on when the fetch began (child :fetch-back), then goes on."
  ([c job w] (step! c job w nil))
  ([c job w {:keys [return?]}]
   (if-let [pl (due c job w)]
     (if (repeat-done? c (:key pl) (:wait pl))
       (book-failed! c pl {:reason :not-solved
                    :why (str "fetched " (pr-str (second (:key pl))) " but the need is unchanged")})
       (await (round! c (assoc pl :return? return?))))
     (do (settle! c)
         (let [back (:fetch-return (ctx/mem c))]
           (cond
             (nil? back) nil
             (= back (feet c)) (do (ctx/update-mem! c dissoc :fetch-return) nil)
             :else (await (walk-back! c back))))))))

(defn max-fetch-calls [] (settings/get settings ::max-fetch-calls))

(defn ^:async fetch!
  "The fetch part of a whole attempt: step! (problem c) again, a pace! between, until nothing is due any more (the
  thing arrived, or the fetch failed and is booked: nil, the job judges where it stands). Resolves to :done for a bad
  :fetch arg (the job has ended), to :continue while a fetch child waits on the world (state kept: the next round
  resumes it) and after max-fetch-calls rounds or a cut."
  [c job problem]
  (loop [n 1]
    (let [w (problem c)
          r (await (step! c job w))]
      (cond
        (nil? r) nil
        (#{:done :continue} r) r
        (and (< n (max-fetch-calls)) (ctx/alive? c)) (do (await (pace/pace!)) (recur (inc n)))
        :else :continue))))

(defn ^:async fetch-untimed!
  "fetch!, then mem :started (the job's :timeout-s start) moves on by the time the fetch took: :timeout-s bounds the
  work the job does, not the fetch (which has its own limits)."
  [c job problem]
  (u/untimed! c #(fetch! c job problem)))
