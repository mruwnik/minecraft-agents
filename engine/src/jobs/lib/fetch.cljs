(ns jobs.lib.fetch
  "The :fetch option of a job that can wait for a missing tool or item (design rule 10: getting it is an option of the
  job; without it the job waits with the reason).

  A job with :fetch reads its own wait reason (or the booked declined child's, jobs.lib.declined) and, when the
  reason is one a fetch job can cure, its check passes and its round runs that fetch job as the child in slot :fetch
  (jobs.items.get-tool for :no-tool, jobs.items.obtain for :need). It does not pass :fetch to its children: their waits
  come up to it. A fetch that ends without the thing is remembered as :fetch/failed for :fail-minutes; meanwhile the
  job waits with its reason plus {:fetch {:failed reason ...}} and nothing is tried again.

  The :fetch arg: false/nil (off), true (all kinds, all sources), a set of kinds (#{:tool :item}), or a map of limits
  {:what :how :depth :minutes :fail-minutes}. Limits merge, later wins: built-in, the job's code default, the body's
  default for all jobs, the body's default for this job (both set by jobs.items.fetch-limits, body memory
  :fetch/limits), the call's own arg.

  Also body memory :fetch/stock: what a chest held when last looked into or moved from (withdraw, deposit, kit,
  obtain), so obtain goes first to a chest known to hold the item and skips one known not to."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.declined :as declined]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [engine.memory :as mem]))

;; ------------------------------------------------------------------ the option and its limits

(def kinds #{:tool :item :station})
(def sources #{:chest :craft :gather})

(def built-in
  "The limits of :fetch true."
  {:what kinds :how sources :depth 7 :minutes 10 :fail-minutes 10})

(def limit-keys [:what :how :depth :minutes :fail-minutes])

(defn limit-error
  "Why a limits map is bad, or nil."
  [m]
  (let [{:keys [what how depth minutes fail-minutes]} m
        pos-num? #(and (number? %) (js/isFinite %) (pos? %))]
    (cond
      (not (map? m)) "limits are a map"
      (seq (remove (set limit-keys) (keys m))) (str "unknown keys " (pr-str (vec (remove (set limit-keys) (keys m)))))
      (and (some? what) (not (and (set? what) (every? kinds what)))) (str ":what is a set of " (pr-str kinds))
      (and (some? how) (not (and (set? how) (every? sources how)))) (str ":how is a set of " (pr-str sources))
      (and (some? depth) (not (and (int? depth) (>= depth 0)))) ":depth is a whole number, 0 or more"
      (and (some? minutes) (not (pos-num? minutes))) ":minutes is a positive number"
      (and (some? fail-minutes) (not (pos-num? fail-minutes))) ":fail-minutes is a positive number")))

(defn parse-arg
  "The :fetch arg as a limits map ({} for true), nil when off, or {:error text}."
  [v]
  (cond
    (or (nil? v) (false? v)) nil
    (true? v) {}
    (set? v) (if (every? kinds v) {:what v} {:error (str ":fetch set holds kinds of " (pr-str kinds))})
    (map? v) (if-let [e (limit-error v)] {:error (str ":fetch " e)} v)
    :else {:error ":fetch is true, false, a set of kinds or a map of limits"}))

(def limits-kind :fetch/limits)

(defn body-limits
  "The body's defaults {:all {..} :jobs {job-sym {..}}} from memory view, {} when none were set."
  [view]
  (or (:data (peek (mem/entries view limits-kind))) {}))

(defn merge-limits
  "Limits for job: built-in, then job-default, then the body's :all and the body's entry for job, then call."
  [job job-default body call]
  (let [clean #(into {} (remove (comp nil? val)) (select-keys % limit-keys))]
    (merge built-in (clean job-default) (clean (:all body)) (clean (get-in body [:jobs job])) (clean call))))

(defn opts
  "The fetch limits of the job running in c (job: its namespace symbol), nil when :fetch is off, {:error text} when
  the arg is bad."
  ([c job] (opts c job nil))
  ([c job job-default]
   (let [arg (parse-arg (:fetch (:args c)))]
     (cond
       (nil? arg) nil
       (:error arg) arg
       :else (merge-limits job job-default (body-limits (ctx/view c)) arg)))))

;; ------------------------------------------------------------------ waits to fetch jobs

(defn plan-for
  "The fetch for wait reason w under limits o: {:kind :job :args :key}, or nil when w is not fetchable or its kind
  is not in (:what o). A :need wait's :count is how many obtain gets (default 1)."
  [w o]
  (let [{:keys [reason item any-of block needs]} w
        pl (case reason
             :no-tool (cond
                        block {:kind :tool :job 'jobs.items.get-tool :args {:block block} :key [:tool block]}
                        needs {:kind :tool :job 'jobs.items.get-tool :args {:item needs} :key [:tool needs]})
             :need (cond
                     item {:kind :item :job 'jobs.items.obtain :args {:item item :count (or (:count w) 1)} :key [:item [item]]}
                     (seq any-of) {:kind :item :job 'jobs.items.obtain :args {:any-of (vec any-of) :count (or (:count w) 1)}
                                   :key [:item (vec any-of)]})
             nil)]
    (when (and pl (contains? (:what o) (:kind pl)))
      pl)))

;; ------------------------------------------------------------------ failures

(def failed-kind :fetch/failed)
(def failed-policy {:cap 32 :ttl (* 24 60 60 1000)})

(defn failure
  "The live failure booked for key, or nil."
  [c key]
  (let [now (ctx/now c)]
    (some #(let [d (:data %)] (when (and (= key (:key d)) (< now (:until d))) d))
          (rseq (ctx/entries c failed-kind)))))

(defn shown
  "The :fetch field of a wait whose fetch failed."
  [f]
  (into {} (remove (comp nil? val)) (select-keys f [:failed :item :any-of :block :tried :chain])))

;; ------------------------------------------------------------------ check and round

(defn check
  "For the check of the job (symbol) whose problem is wait reason w (non-nil): true when the round will fetch for it
  (or end a bad :fetch arg), else ctx/wait with w, plus {:fetch {:failed ...}} when a fetch for it failed lately."
  [c job w]
  (let [o (opts c job)
        pl (when (and o (not (:error o))) (plan-for w o))
        f (when pl (failure c (:key pl)))]
    (cond
      (:error o) true
      (nil? pl) (ctx/wait c w)
      f (ctx/wait c (assoc w :fetch (shown f)))
      :else true)))

(defn due
  "What the round of job should fetch for wait reason w (nil when none): the plan with the limits as :opts, or {:bad
  text} for a bad :fetch arg."
  [c job w]
  (when w
    (let [o (opts c job)]
      (cond
        (nil? o) nil
        (:error o) {:bad (:error o)}
        :else (when-let [pl (plan-for w o)]
                (when-not (failure c (:key pl))
                  (assoc pl :opts o :wait w)))))))

(defn settle!
  "For a round with nothing to fetch: a fetch under way whose need went away (the thing arrived before the fetch job
  ended, e.g. in its withdraw) is told done (info fetch.done) and its child memory dropped, so a later fetch starts
  fresh."
  [c]
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
        r (await (ctx/call-child c :fetch-back 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 0 :escalate false}))]
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

(defn fail!
  "Book the failed fetch (until now + :fail-minutes), warn fetch.failed, :again (the check now waits)."
  [c {:keys [key opts wait args]} res]
  (let [f (merge (select-keys args [:item :any-of :block])
                 {:key key :until (+ (ctx/now c) (* 60000 (:fail-minutes opts))) :failed (:reason res :failed)}
                 (select-keys res [:tried :chain]))]
    (ctx/remember! c failed-kind f failed-policy)
    (ctx/update-mem! c #(-> % (dissoc :fetching :fetch-return) (update :children dissoc :fetch)))
    (ctx/emit! c :fetch.failed :warn (assoc (shown f) :for (:reason wait)
                                            :text (str "could not fetch " (or (:item args) (:block args) (pr-str (:any-of args)))
                                                       ": " (name (:failed f)))))
    :again))

(defn bad!
  "End the job for a bad :fetch arg."
  [c text]
  (ctx/emit! c :fetch.bad-args :warn {:text text})
  (ctx/result! c {:status :stopped :reason :bad-args :why text :text text})
  :done)

(defn ^:async round!
  "One round of the fetch in slot :fetch for plan pl (from due). :continue while the child waits on the world, :again
  after it ended or failed; :done only for a bad :fetch arg."
  [c pl]
  (if (:bad pl)
    (bad! c (:bad pl))
    (let [{:keys [job key opts]} pl
          args (merge (:args pl) (select-keys opts [:depth :minutes :how :fail-minutes]))
          pl (assoc pl :args args)]
      (when-not (= key (:fetching (ctx/mem c)))
        (ctx/update-mem! c #(cond-> (assoc % :fetching key)
                              (and (:return? pl) (not (:fetch-return %))) (assoc :fetch-return (feet c))))
        (ctx/emit! c :fetch.started :info {:for (:reason (:wait pl)) :job job :args (:args pl)
                                           :text (str "fetching " (or (:item args) (:block args) (pr-str (:any-of args)))
                                                      " for " (name (:reason (:wait pl))))}))
      (let [r (await (ctx/call-child c :fetch job args))]
        (case r
          :continue :continue
          :declined (fail! c pl (or (b/child-wait c :fetch job args) {:reason :not-ready}))
          (let [res (ctx/child-result c :fetch)]
            (if (= :done (:status res))
              (do (ctx/update-mem! c dissoc :fetching)
                  (ctx/emit! c :fetch.done :info (merge {:for (:reason (:wait pl)) :text (str "fetched for " (name (:reason (:wait pl))))}
                                                        (select-keys res [:got :item :tool])))
                  :again)
              (fail! c pl res))))))))

(defn ^:async step!
  "One round's fetch part for job with wait reason w: runs the due fetch (round!) and returns its round result
  (:continue: a child waits on the world, yield; :again: go round again), or settles a finished one and returns nil
  (the job does its own work).
  opts {:return? true}: a job whose work depends on where the body stands (a stair) first walks back to the cell it
  stood on when the fetch began (child :fetch-back), then goes on."
  ([c job w] (step! c job w nil))
  ([c job w {:keys [return?]}]
   (if-let [pl (due c job w)]
     (await (round! c (assoc pl :return? return?)))
     (do (settle! c)
         (let [back (:fetch-return (ctx/mem c))]
           (cond
             (nil? back) nil
             (= back (feet c)) (do (ctx/update-mem! c dissoc :fetch-return) nil)
             :else (await (walk-back! c back))))))))

(def max-fetch-calls "Fetch rounds of one fetch! before it gives the round back with :continue." 400)

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
        (and (< n max-fetch-calls) (ctx/alive? c)) (do (await (pace/pace!)) (recur (inc n)))
        :else :continue))))

(defn ^:async fetch-untimed!
  "fetch!, then mem :started (the job's :timeout-s start) moves on by the time the fetch took: :timeout-s bounds the
  work the job does, not the fetch (which has its own limits)."
  [c job problem]
  (let [t0 (ctx/now c)
        r (await (fetch! c job problem))]
    (ctx/update-mem! c update :started #(when % (+ % (- (ctx/now c) t0))))
    r))

;; ------------------------------------------------------------------ chests: seen ones and their stock

(def container-names ["chest" "trapped_chest" "barrel"])
(def chest-radius 32)
(def stock-kind :fetch/stock)
(def stock-policy {:cap 64 :ttl (* 30 60 1000)})

(defn cell-of [pos] (if (vector? pos) pos [(:x pos) (:y pos) (:z pos)]))

(defn stock-of
  "{[x y z] {name n}}: the latest stock booked per chest, from memory view."
  [view]
  (reduce (fn [m e] (assoc m (:pos (:data e)) (:items (:data e)))) {} (mem/entries view stock-kind)))

(defn stacks->items
  "{name n} of inspected container items (JS array of {name count}) or cljs maps."
  [stacks]
  (reduce (fn [m s] (let [[n k] (if (map? s) [(:name s) (:count s)] [(.-name s) (.-count s)])]
                      (update m n (fnil + 0) k)))
          {} (if (array? stacks) (array-seq stacks) stacks)))

(defn note-stock!
  "Book what the chest at pos holds now (stacks as inspected)."
  [c pos stacks]
  (ctx/remember! c stock-kind {:pos (cell-of pos) :items (stacks->items stacks)} stock-policy))

(defn note-moved!
  "Book delta of name moved into (+) or out of (-) the chest at pos, when its stock is known."
  [c pos name delta]
  (let [cell (cell-of pos)]
    (when-let [items (get (stock-of (ctx/view c)) cell)]
      (let [n (+ (get items name 0) delta)]
        (ctx/remember! c stock-kind {:pos cell :items (if (pos? n) (assoc items name n) (dissoc items name))} stock-policy)))))

(def view-age-ms 10000)

(defn seen-chests
  "The containers the body has seen (perception's seenBlocks, never x-ray) within chest-radius, as {:x :y :z},
  nearest first; empty without perception. A cell seen in the last view-age-ms is checked against the live block
  (gone: dropped); an older memory is trusted, a failed open or a later sight corrects it."
  [c]
  (let [p (:primitives c)
        me (u/self-pos c)]
    (->> (look/seen-blocks p {:names container-names :radius chest-radius :max 32 :live? true :live-within-ms view-age-ms})
         (map :pos)
         (sort-by #(u/dist me %)))))

(defn usable-chests
  "seen-chests whose zone or claim allows :take (an open, unzoned chest does)."
  [c]
  (remove #(access/container-refusal c :take %) (seen-chests c)))
