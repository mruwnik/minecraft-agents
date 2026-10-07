(ns engine.job-fuzz-test
  "Seeded fuzz of every registered job on the fake world: generated args (missing, odd values, out-of-range positions),
  small random worlds (no ground, empty bags, absent targets). A job is honest when no round throws, no check throws,
  a declining check or a stop carries a :reason, and the job ends or waits within a bounded number of rounds and calls.
  A failure prints job, seed and args; FUZZ_JOB=<ns> FUZZ_SEED=<n> FUZZ_CASES=<n> replay or widen a run, FUZZ_TRACE=1 names
  every case as it starts (a sync hang names its case last), FUZZ_CASE=<k> runs only case k of a job, FUZZ_EXTREME=1 adds huge numbers.
  A case seed is FUZZ_SEED + a hash of the job name + 104729 * k, so it does not depend on the other jobs or on FUZZ_JOB.
  FUZZ_WRONG_TYPES=1 also feeds wrong-typed values to args with no :type (a report of type gaps, not part of the default)."
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.expr :as expr]
            [engine.fake :as fake]))

;; ---- seeded random numbers (mulberry32)

(defn rng
  "A fn of no args returning floats in [0, 1), deterministic from seed."
  [seed]
  (let [s (atom (bit-or seed 0))]
    (fn []
      (swap! s #(bit-or (+ % 0x6D2B79F5) 0))
      (let [t (js/Math.imul (bit-xor @s (unsigned-bit-shift-right @s 15)) (bit-or @s 1))
            t (bit-xor t (+ t (js/Math.imul (bit-xor t (unsigned-bit-shift-right t 7)) (bit-or t 61))))]
        (/ (unsigned-bit-shift-right (bit-xor t (unsigned-bit-shift-right t 14)) 0) 4294967296)))))

(defn pick [r coll] (nth coll (js/Math.floor (* (r) (count coll)))))
(defn chance? [r p] (< (r) p))
(defn between [r lo hi] (+ lo (js/Math.floor (* (r) (inc (- hi lo))))))

;; ---- generated values

(def names ["" "cow" "zombie" "oak_log" "stone" "wheat" "wheat_seeds" "iron_pickaxe" "nope" "Steve" "no-such-plan" "chest" "oak_sapling" "bread"])

(def wrong-values ["text" 3 -7 :kw [] {} true false [1 2] ["a"] {:x "a"}])

(defn gen-pos [r]
  (pick r [[0 64 0] [3 64 2] [-5 64 4] [10 64 -8] [0 63 0] [0 -300 0] [0 400 0] [1e7 64 1e7] [-1e6 64 3] [0.5 64.5 0.5] [4 64 4]
           {:x 2 :y 64 :z 0}]))

(defn gen-box [r]
  (let [x (between r -6 6) z (between r -6 6) y (pick r [60 63 64 -40])]
    (pick r [{:min {:x x :y y :z z} :max {:x (+ x (between r 0 8)) :y (+ y (between r 0 5)) :z (+ z (between r 0 8))}}
             {:min {:x 5 :y 70 :z 5} :max {:x -5 :y 60 :z -5}}
             {:min {:x 0 :y 64 :z 0} :max {:x 0 :y 64 :z 0}}])))

(def extreme? (= "1" (aget js/process.env "FUZZ_EXTREME")))

(defn gen-number
  "Small and edge numbers; FUZZ_EXTREME=1 adds huge ones (a job that loops over its own radius or depth hangs the sync run)."
  [r default]
  (pick r (cond-> [0 1 -1 2 5 40 1000 0.5]
            extreme? (into [1e6 -1e6])
            (number? default) (into [default (* 2 default) (inc default)]))))

(defn gen-value
  "A plausible value for arg k of spec, typed by its :type, else by its default and name. wrong? picks a wrong-typed one."
  [r k {:keys [default type values] :as spec} wrong?]
  (let [kn (name k)]
    (cond
      wrong? (pick r (if type (filterv #(if (= :pos type) (nil? (expr/cell %)) (some? (expr/type-problem spec %))) wrong-values) wrong-values))
      (= :pos type) (gen-pos r)
      (= :enum type) (pick r (vec values))
      (= :keyword type) (pick r [:a :b default])
      (= :int type) (pick r [0 1 -1 7 1000])
      (number? default) (gen-number r default)
      (boolean? default) (pick r [true false])
      (string? default) (pick r names)
      (keyword? default) (pick r [default :wait :other])
      (vector? default) (pick r [[] [(pick r names)] [(pick r names) (pick r names)]])
      (map? default) (pick r [{} nil])
      (re-find #"box|area|zone" kn) (gen-box r)
      (re-find #"pos|at$|cell|spot|target$|from|to$" kn) (pick r [nil (gen-pos r)])
      (re-find #"^(count|n)$|radius|range|max|min|limit|reach|-s$|-ms$" kn) (pick r [nil (gen-number r 3)])
      (re-find #"(item|mob|player|plan|name|block|kind|tool|food|seed|crop)s$" kn) (pick r [nil [] [(pick r names)]])
      (re-find #"item|mob|player|plan|name|block|kind|tool|food|seed|crop" kn) (pick r [nil (pick r names)])
      :else nil)))

(defn gen-args
  "Args for entry, each left out with some chance (the default stands). A wrong-typed value goes into args with a :type
  (the metadata :wrong-typed names them: submit must refuse), and into others only when wrong-untyped?."
  [r entry wrong-untyped?]
  (reduce (fn [acc [k spec]]
            (if (chance? r 0.35)
              acc
              (let [wrong? (and (chance? r 0.25) (or (:type spec) wrong-untyped?))]
                (cond-> (assoc acc k (gen-value r k spec wrong?))
                  (and wrong? (:type spec)) (vary-meta update :wrong-typed (fnil conj #{}) k)))))
          {} (:args entry)))

(def ore-ish ["oak_log" "stone" "dirt" "wheat" "water" "grass_block" "iron_ore" "chest" "oak_leaves" "farmland" "sand"])

(defn gen-world [r]
  (let [ground (pick r [:none :floor :floor :floor])
        n (if (= :none ground) (between r 0 5) (between r 0 25))
        blocks (into {} (map (fn [_] [(str (between r -7 7) "," (pick r [62 63 64 65]) "," (between r -7 7)) (pick r ore-ish)])) (range n))
        ents (mapv (fn [i] (pick r [{:id (+ 10 i) :name "zombie" :kind "hostile" :pos {:x (between r -9 9) :y 64 :z (between r -9 9)}}
                                    {:id (+ 10 i) :name "cow" :uuid (str "c" i) :kind "passive" :pos {:x (between r -9 9) :y 64 :z (between r -9 9)}}
                                    {:id (+ 10 i) :name "Steve" :username "Steve" :kind "player" :pos {:x (between r -9 9) :y 64 :z (between r -9 9)}}]))
                   (range (between r 0 3)))
        inv (pick r [[] [] [{:name "iron_pickaxe" :count 1} {:name "oak_log" :count 12}] [{:name "wheat_seeds" :count 5} {:name "bread" :count 3} {:name "iron_sword" :count 1}]])]
    {:spec (cond-> {:self {:pos [0 64 0] :health (between r 1 20) :food (between r 0 20)} :time (pick r [1000 14000]) :inventory inv :entities ents}
             (= :floor ground) (assoc :floor [-8 -8 8 8])
             (seq blocks) (assoc :blocks blocks))
     :ground ground}))

;; ---- running one case

(def call-cap "Acts allowed per round (the fake clock moves 50 ms with each)." 1500)
(def long-running-cap "Acts allowed per round for a job in long-running: its cut-off is expected, so stop early." 200)
(def read-cap "Calls allowed since the last act: a loop over reads never awaits, so only a count stops it." 60000)
(def max-rounds 12)

(def current-case "The case run-all is running: acts started now belong to it." (atom nil))

(def owners "Act promise -> the case that started it, so a late stray rejection is blamed on that case." (js/WeakMap.))

(defn own!
  "Tag promise r with the current case; returns r."
  [r]
  (when-let [c @current-case] (.set owners r c))
  r)

(defn capped-primitives
  "p with every act (a call returning a promise) counted in calls (reset before each round); past the cap one throws, so a loop that never ends fails instead of hanging. The act it drops is handled, so its late rejection (a cut) is not a stray one."
  [p calls hit act-cap]
  (js/Proxy. p #js {:get (fn [t k]
                           (let [v (aget t k)]
                             (if (fn? v)
                               (fn [& args]
                                 (when (> (:all (swap! calls update :all inc)) read-cap)
                                   (reset! hit :read)
                                   (throw (js/Error. "fuzz: call cap (runaway job)")))
                                 (let [r (.apply v t (to-array args))]
                                   (when (and r (fn? (.-then r)))
                                     (own! r)
                                     (swap! calls assoc :all 0))
                                   (when (and r (fn? (.-then r)) (> (:acts (swap! calls update :acts inc)) act-cap))
                                     (reset! hit :act)
                                     (.catch r (fn [_] nil))
                                     (throw (js/Error. "fuzz: act cap (runaway job)")))
                                   r))
                               v)))}))

(defn event-reason [e] (or (:reason e) (get-in e [:data :reason])))

(defn declared-failure?
  "A round that threw one of the jobs' own ex-info on purpose (README: a throw is a failure): the messages of clear-box/cells, till/cells and herd-run, not a library error or a stray JS error."
  [text]
  (boolean (re-find #"^#error \{:message \"(clear-box (needs|covers)|till (needs|covers)|herd brought none)" (str text))))

(defn act-cap? [text] (str/includes? (str text) "(runaway job)"))

(defn declared-wait?
  "True when the job said why it waits (a :waiting event with a :reason): a yield, not a runaway."
  [events]
  (boolean (some #(and (= :job (:source %)) (= :waiting (:kind %)) (event-reason %)) events)))

(def long-running
  "{job reason}: jobs whose rounds legitimately outlast the fuzzer's caps (the fake world's time is frozen), so a cut-off is no defect."
  {'jobs.survival.night "a night round waits for dawn, which the fake world's frozen clock never brings"
   'jobs.survival.retreat "flight keeps moving while the danger lasts, and the fake danger never goes"})

(def progress-window "Rounds over which a change in the fake world counts as progress, not a spin." 3)

(defn running-ids
  "The listed job ids that are not parked after a failure (a parked job has stopped with its reason)."
  [state]
  (remove #(contains? (:failed state) %) (:list state)))

(defn world-sig
  "What a job can change in the fake world p: blocks, bag, containers, drops, entities, the body's position (block states and ages, furnaces, enchant tables, unloaded cells, rain, worn gear). Job memory, chat and self stats are left out: retry counters and timestamps change on a spin. :time is left out: every wait or sleep moves it, so a job that only waits would look like progress."
  [p]
  (let [w @(fake/state p)]
    {:blocks (:blocks w) :inventory (:inventory w) :containers (:containers w) :drops (:drops w)
     :entities (:entities w) :pos (get-in w [:self :pos])
     :states (:states w) :ages (:ages w) :furnaces (:furnaces w) :equipment (:equipment w)
     :enchant-tables (:enchant-tables w) :unloaded (:unloaded w) :raining (:raining w)}))

(defn progressing?
  "True when a world signature in the last progress-window rounds is new: not seen before in sigs (the world before round 1, then one per round, oldest first). A job that cycles through signatures it has already had is spinning."
  [sigs]
  (let [n (count sigs)]
    (boolean (some #(not (contains? (set (take % sigs)) (nth sigs %))) (range (max 1 (- n progress-window)) n)))))

(defn runaway
  "[[:runaway detail]] when job hit the act or call cap (cap is :act, :read or nil) or still ran after max-rounds, unless job is in long-running."
  [job cap still-listed?]
  (when-not (contains? long-running job)
    (cond
      (= :act cap) [[:runaway "act cap hit"]]
      (= :read cap) [[:runaway "call cap hit"]]
      still-listed? [[:runaway (str "still running after " max-rounds " rounds")]])))

(defn defects
  "Defect keywords (with detail) for the events and final state of a case; runaway cut-offs are reported by runaway."
  [events state]
  (concat
   (for [[_ f] (:failed state) :when (not (or (act-cap? (:error f)) (declared-failure? (:error f))))] [:round-threw (str (:error f))])
   (for [e events :when (and (= :system (:source e)) (= :error (:kind e)) (not (act-cap? (:text e))))] [:check-threw (:text e)])
   (for [e events :when (and (= :job (:source e)) (= :waiting (:kind e)) (= :not-ready (event-reason e))
                             (not-any? #(= :error (:kind %)) events))] [:waiting-without-reason (:text e)])
   (for [e events :when (and (= :job (:source e)) (= :stopped (:kind e)) (nil? (event-reason e)))] [:stopped-without-reason (:text e)])))

(defn ^:async run-case
  "Run one case ({:job :entry :seed :wrong-untyped?}): {:job :seed :args :outcome (:refused/:ran) :defects [[kind detail]]}."
  [{:keys [job entry seed wrong-untyped?]}]
  (let [r (rng seed)
        args (gen-args r entry wrong-untyped?)
        {:keys [spec ground]} (gen-world r)
        clock (atom 1000000)
        calls (atom {:all 0 :acts 0})
        capped (atom nil)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake spec)
        now (tu/act-clock clock raw 50)
        eng (core/create {:primitives (capped-primitives raw calls capped (if (contains? long-running job) long-running-cap call-cap)) :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})
        base {:job job :seed seed :args args :ground ground}
        id (try (core/submit! eng (list job args) {}) (catch :default e e))]
    (if (instance? js/Error id)
      (assoc base :outcome :refused :defects (when (str/blank? (ex-message id)) [[:blank-refusal ""]]))
      (let [thrown (atom nil)
            sigs (atom [(world-sig raw)])
            unrefused (when (seq (:wrong-typed (meta args))) [[:wrong-type-accepted (pr-str (:wrong-typed (meta args)))]])]
        (loop [i 0]
          (when (and (< i max-rounds) (seq (running-ids (core/state eng))) (not @thrown) (not @capped))
            (swap! clock + 1500)
            (reset! calls {:all 0 :acts 0})
            (let [ok? (try (await (core/tick! eng)) true (catch :default e (reset! thrown (str e)) false))]
              (swap! sigs conj (world-sig raw))
              (when ok? (recur (inc i))))))
        (assoc base :outcome :ran
               :defects (concat unrefused (when (and @thrown (not (act-cap? @thrown))) [[:tick-threw @thrown]])
                                (runaway job @capped (and (not @thrown) (seq (running-ids (core/state eng))) (not (declared-wait? @seen)) (not (progressing? @sigs))))
                                (defects @seen (core/state eng))))))))

;; ---- the run

(defn env [k default]
  (let [v (aget js/process.env k)] (if (str/blank? v) default v)))

(def skipped
  "Debug tools: not jobs a body runs for itself."
  #{'jobs.debug.access-check 'jobs.debug.notify 'jobs.debug.walk-plan})

(def known
  "{[job kind] card}: defects already carded, so the suite stays green and a new one fails it. A default run fails when an entry no longer occurs: delete it with its fix."
  {["jobs.access.tunnel" :runaway] "d7c8ff99"
   ["jobs.explore.search" :runaway] "7ef2db8f"
   ["jobs.farm.find-spot" :runaway] "bc44edcd"
   ["jobs.gather.mine" :runaway] "cd74fd83"
   ["jobs.items.enchant" :round-threw] "1b671b73"
   ["jobs.movement.linger-near" :runaway] "df540718"
   ["jobs.survival.respond-to-hostile" :runaway] "e7ed9147"})

(defn case-seed
  "Seed of case k of job: stable under the job filter and the other jobs."
  [base job k]
  (+ base (mod (hash (str job)) 1000003) (* 104729 k)))

(defn job-cases
  "Cases [{:job :entry :seed :k :base :n :wrong-untyped?}] for the jobs matching :only (nil = all), :n per job or just case :case."
  [{:keys [only n base wrong-untyped?] kase :case}]
  (for [[job entry] (sort-by (comp str key) registry/jobs)
        :when (and (not (skipped job)) (or (nil? only) (= only (str job))))
        k (if kase [kase] (range n))]
    {:job job :entry entry :seed (case-seed base job k) :k k :base base :n n :wrong-untyped? wrong-untyped?}))

(defn env-opts
  "Run options from the FUZZ_* variables; getenv is (fn [k default]), the process env by default."
  ([wrong-untyped?] (env-opts wrong-untyped? env))
  ([wrong-untyped? getenv]
   {:only (getenv "FUZZ_JOB" nil)
    :n (js/parseInt (getenv "FUZZ_CASES" "10"))
    :base (js/parseInt (getenv "FUZZ_SEED" "1"))
    :case (some-> (getenv "FUZZ_CASE" nil) js/parseInt)
    :wrong-untyped? wrong-untyped?}))

(defn repro-getenv
  "A getenv fn over the K=V pairs of a printed repro line."
  [line]
  (let [vars (into {} (map #(str/split % #"=" 2)) (str/split line #" "))]
    (fn [k default] (get vars k default))))

(defn repro-line
  "The env vars that replay case c alone."
  [{:keys [job base n k wrong-untyped?]}]
  (str "FUZZ_JOB=" job " FUZZ_SEED=" base " FUZZ_CASES=" n " FUZZ_CASE=" k
       (when wrong-untyped? " FUZZ_WRONG_TYPES=1") (when extreme? " FUZZ_EXTREME=1")))

(defn stale-known
  "The keys of known that no defect in found has: their cards are fixed."
  [known found]
  (let [seen (into #{} (map (juxt (comp str :job) :kind)) found)]
    (vec (remove seen (keys known)))))

(defn tick!
  "A promise resolving after the event loop has run what was waiting: a rejection nobody handled is reported by then."
  []
  (js/Promise. (fn [resolve] (js/setTimeout resolve 0))))

(defn ^:async run-all
  "The defects of every case ([{:job :kind :detail :repro ...}]). A case that throws, or leaves a promise rejected with no handler, is that case's defect (with its repro line), never a crash of the run. A stray rejection is blamed on the case that started its act (see own!), else on the case running when it fired."
  ([opts] (run-all opts (job-cases opts) run-case))
  ([_opts cases run]
   (let [strays (atom [])
         handler (fn [reason promise] (swap! strays conj [(or (.get owners promise) @current-case) (str reason)]))]
     (.on js/process "unhandledRejection" handler)
     (try
       (let [results (loop [todo cases done []]
                       (if-let [c (first todo)]
                         (let [_ (when (env "FUZZ_TRACE" nil) (println "fuzz" (:job c) (:seed c)))
                               _ (reset! current-case c)
                               res (try (await (run c))
                                        (catch :default e {:job (:job c) :defects [[:harness-threw (str e)]]}))]
                           (recur (rest todo) (conj done [c res])))
                         done))
             _ (await (tick!))
             stray-of (group-by first @strays)]
         (vec (for [[c res] results
                    [kind detail] (concat (:defects res) (map (fn [[_ s]] [:stray-rejection s]) (stray-of c)))]
                (assoc (select-keys res [:job :args :ground]) :repro (repro-line c) :kind kind :detail detail))))
       (finally
         (reset! current-case nil)
         (.off js/process "unhandledRejection" handler))))))

(defn summary [found]
  (->> (group-by (juxt (comp str :job) :kind) found)
       (sort-by key)
       (map (fn [[[job kind] ds]]
              (let [d (first ds)]
                (str job " " kind " x" (count ds) ": " (pr-str (:detail d))
                     "\n    repro " (:repro d) "  args " (pr-str (:args d)) " ground " (:ground d)))))
       (str/join "\n")))

(defn full-run?
  "True when opts and env make the run cover the default cases of every job, so a known entry that did not occur is stale."
  [{:keys [only n base wrong-untyped?] kase :case}]
  (and (nil? only) (nil? kase) (>= n 10) (= 1 base) (not wrong-untyped?) (not extreme?)))

(deftest every-job-handles-generated-inputs-honestly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts (env-opts (= "1" (env "FUZZ_WRONG_TYPES" "0")))
              found (await (run-all opts))
              fresh (remove #(contains? known [(str (:job %)) (:kind %)]) found)
              stale (when (full-run? opts) (stale-known known found))]
          (doseq [[[job kind] ds] (sort-by key (group-by (juxt (comp str :job) :kind) (filter #(contains? known [(str (:job %)) (:kind %)]) found)))]
            (println "known" job kind (count ds) (pr-str (:detail (first ds)))))
          (is (empty? fresh) (str (count fresh) " defect(s) across " (count (group-by (juxt :job :kind) fresh)) " job/kind pairs:\n" (summary fresh)))
          (is (empty? stale) (str "known entries that no longer occur (card fixed? delete them): " (pr-str (map (juxt identity known) stale)))))))))

(deftest rng-is-deterministic-per-seed
  (is (= (repeatedly 5 (rng 42)) (repeatedly 5 (rng 42))))
  (is (not= (repeatedly 5 (rng 42)) (repeatedly 5 (rng 43)))))

(deftest generated-args-repeat-per-seed-and-wrong-types-hit-typed-args
  (let [entry (get registry/jobs 'jobs.movement.go-to)
        gen #(gen-args (rng %) entry false)]
    (is (= (gen 7) (gen 7)))
    (is (some #(seq (:wrong-typed (meta (gen %)))) (range 60)) "some seed puts a wrong value in the typed :pos arg")))

(deftest case-seeds-do-not-depend-on-the-job-filter
  (let [all (job-cases {:n 3 :base 1})
        one (job-cases {:n 3 :base 1 :only "jobs.movement.go-to"})]
    (is (= 3 (count one)))
    (is (= one (filter #(= 'jobs.movement.go-to (:job %)) all)))))

(deftest repro-line-names-every-env-var-and-replays-the-case
  (let [c (first (job-cases {:n 4 :base 5 :case 2 :only "jobs.movement.go-to" :wrong-untyped? true}))
        line (repro-line c)]
    (is (= 2 (:k c)))
    (is (str/includes? line "FUZZ_JOB=jobs.movement.go-to FUZZ_SEED=5 FUZZ_CASES=4 FUZZ_CASE=2 FUZZ_WRONG_TYPES=1"))
    (is (= (:seed c) (:seed (nth (job-cases {:n 4 :base 5 :only "jobs.movement.go-to"}) 2))))
    (is (= (gen-args (rng (:seed c)) (:entry c) true) (gen-args (rng (:seed c)) (:entry c) true)))))

(deftest stale-known-entries-are-the-ones-not-found
  (is (= [["a" :x]] (stale-known {["a" :x] "c1" ["b" :y] "c2"} [{:job 'b :kind :y}]))))

(deftest cut-offs-are-runaway-unless-the-job-is-listed-long-running
  (is (= [[:runaway "act cap hit"]] (runaway 'jobs.farm.till :act false)))
  (is (= [[:runaway "still running after 12 rounds"]] (runaway 'jobs.farm.till nil true)))
  (is (empty? (runaway 'jobs.farm.till nil false)))
  (is (empty? (runaway (first (keys long-running)) :act true))))

(deftest only-the-jobs-own-ex-info-shapes-are-declared-failures
  (is (declared-failure? "#error {:message \"clear-box needs :from and :to\", :data {}}"))
  (is (declared-failure? "#error {:message \"till covers 9 cells, at most 4\", :data {:cells 9}}"))
  (is (not (declared-failure? "#error {:message \"No matching clause: :x\", :data {}}")))
  (is (not (declared-failure? "TypeError: x is not a function"))))

(deftest a-printed-repro-replays-the-same-case
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts {:n 10 :base 1 :only "jobs.movement.go-to"}
              c (nth (job-cases opts) 1)
              getenv (repro-getenv (repro-line c))
              replay-opts (env-opts false getenv)
              replay (first (job-cases replay-opts))
              a (await (run-case c))
              b (await (run-case replay))]
          (is (= 1 (:case replay-opts)))
          (is (= (:seed c) (:seed replay)))
          (is (= (select-keys a [:args :ground :defects]) (select-keys b [:args :ground :defects]))))))))

(deftest runaway-ignores-parked-jobs-and-jobs-whose-world-changes
  (is (= ["j2"] (running-ids {:list ["j1" "j2"] :failed {"j1" {}}})))
  (is (progressing? [{:a 0} {:a 0} {:a 1} {:a 2}]))
  (is (progressing? [{:a 0} {:a 1} {:a 2} {:a 0}]) "a walk A,B,C,A met new states in the window")
  (is (not (progressing? [{:a 1} {:a 1} {:a 1} {:a 1} {:a 1}])))
  (is (not (progressing? [{:a 0} {:a 1} {:a 0} {:a 1} {:a 0} {:a 1} {:a 0}])) "a 2-cycle already seen is a spin, whatever its parity")
  (is (not (progressing? [{:a 1}]))))

(defn sigs-over
  "The world-sig before and after each of rounds rounds on a fake world, where (change w round) is the new world state that round."
  [rounds change]
  (let [p (tu/fake {:self {:pos [0 64 0]}})]
    (into [(world-sig p)]
          (map (fn [i] (swap! (fake/state p) change i) (world-sig p)))
          (range rounds))))

(deftest a-job-whose-only-change-is-a-retry-counter-is-a-runaway
  (let [memory (atom {})
        sigs (sigs-over 8 (fn [w _] (swap! memory update :tries (fnil inc 0)) w))]
    (is (= 8 (:tries @memory)))
    (is (apply = sigs) "the world never changed")
    (is (not (progressing? sigs)))
    (is (= [[:runaway "still running after 12 rounds"]] (runaway 'jobs.farm.till nil (not (progressing? sigs)))))))

(def every-third-round
  "Change functions by round mod 3: round 0 places a block, rounds 1 and 2 leave the world as it is."
  [(fn [w i] (assoc-in w [:blocks [1 64 i]] "dirt")) (fn [w _] w) (fn [w _] w)])

(deftest a-job-that-changes-a-block-every-third-round-is-progress
  (let [sigs (sigs-over 12 (fn [w i] ((nth every-third-round (mod i 3)) w i)))]
    (is (= 1 (count (distinct (take 3 (drop 1 sigs))))) "the two repeat rounds leave the signature as it was")
    (is (every? progressing? (map #(take % sigs) (range 5 14))))))

(deftest the-same-job-changing-nothing-is-a-runaway
  (let [sigs (sigs-over 12 (fn [w _] w))]
    (is (not (progressing? sigs)))
    (is (= [[:runaway "still running after 12 rounds"]] (runaway 'jobs.farm.till nil (not (progressing? sigs)))))))

(deftest a-stray-rejection-is-blamed-on-the-case-that-started-its-act
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [late (fn [c] (own! (js/Promise. (fn [_ reject] (js/setTimeout #(reject (str "late " (:job c))) 5)))))
              run (fn ^:async r [c] (when (= 'a (:job c)) (late c)) (await (js/Promise. (fn [r] (js/setTimeout r (if (= 'a (:job c)) 0 40))))) {:job (:job c) :defects []})
              cases [{:job 'a :seed 1 :k 0 :base 1 :n 2} {:job 'b :seed 2 :k 1 :base 1 :n 2}]
              found (await (run-all {} cases run))]
          (is (= [['a :stray-rejection "late a"]] (map (juxt :job :kind :detail) found))))))))

(def world-parts
  "[part path value]: a change a job can make to each part of the fake world that world-sig must see."
  [[:blocks [:blocks [1 64 1]] "dirt"]
   [:inventory [:inventory] [{:name "oak_log" :count 1}]]
   [:containers [:containers [2 64 2]] [{:name "bread" :count 1}]]
   [:drops [:drops] [{:id 99 :name "oak_log" :count 1 :pos [1 64 1]}]]
   [:entities [:entities] [{:id 5 :name "cow" :pos {:x 1 :y 64 :z 1}}]]
   [:pos [:self :pos] [3 64 3]]
   [:states [:states [1 64 1]] {:open true}]
   [:ages [:ages [1 64 1]] 2]
   [:furnaces [:furnaces [1 64 1]] {:smelted 1}]
   [:equipment [:equipment "head"] {:name "iron_helmet" :count 1}]
   [:enchant-tables [:enchant-tables [1 64 1]] {:lapis 1}]
   [:unloaded [:unloaded] #{[1 64 1]}]
   [:raining [:raining] true]])

(deftest every-part-of-the-world-a-job-can-change-shows-in-the-signature
  (doseq [[part path value] world-parts]
    (let [sigs (sigs-over 1 (fn [w _] (assoc-in w path value)))]
      (is (not= (first sigs) (last sigs)) (str "world-sig misses " part)))))

(deftest chat-and-self-state-are-not-progress
  (let [sigs (sigs-over 1 (fn [w _] (-> w (update :chat (fnil conj []) {:message "hi"}) (assoc-in [:self :health] 3))))]
    (is (= (first sigs) (last sigs)))))
