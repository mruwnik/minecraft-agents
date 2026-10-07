(ns engine.job-fuzz-test
  "Seeded fuzz of every registered job on the fake world: generated args (missing, odd values, out-of-range positions),
  small random worlds (no ground, empty bags, absent targets). A job is honest when no round throws, no check throws,
  a declining check or a stop carries a :reason, and the job ends or waits within a bounded number of rounds and calls.
  A failure prints job, seed and args; FUZZ_JOB=<ns> FUZZ_SEED=<n> FUZZ_CASES=<n> replay or widen a run, FUZZ_TRACE=1 names
  every case as it starts (a sync hang names its case last), FUZZ_EXTREME=1 adds huge numbers.
  FUZZ_WRONG_TYPES=1 also feeds wrong-typed values to args with no :type (a report of type gaps, not part of the default)."
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.expr :as expr]))

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
      wrong? (pick r wrong-values)
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
(def read-cap "Calls allowed since the last act: a loop over reads never awaits, so only a count stops it." 150000)
(def round-ms "Wall time one round may run before its next act ends it: a harness cut-off, not a verdict (a whole-night round never ends on the fake world frozen time)." 500)
(def max-rounds 12)

(defn capped-primitives
  "p with every act (a call returning a promise) counted in calls (reset before each round); past the cap one throws, so a loop that never ends fails instead of hanging."
  [p calls hit]
  (js/Proxy. p #js {:get (fn [t k]
                           (let [v (aget t k)]
                             (if (fn? v)
                               (fn [& args]
                                 (when (> (:all (swap! calls update :all inc)) read-cap)
                                   (reset! hit true)
                                   (throw (js/Error. "fuzz: call cap (runaway job)")))
                                 (let [r (.apply v t (to-array args))]
                                   (when (and r (fn? (.-then r)))
                                     (swap! calls assoc :all 0))
                                   (when (and r (fn? (.-then r)) (or (> (:acts (swap! calls update :acts inc)) call-cap)
                                                                     (> (- (js/Date.now) (:t0 @calls)) round-ms)))
                                     (throw (js/Error. "fuzz: act cap (runaway job)")))
                                   r))
                               v)))}))

(defn event-reason [e] (or (:reason e) (get-in e [:data :reason])))

(defn declared-failure?
  "A round that threw an ex-info on purpose (the job says why it failed, README: a throw is a failure), not a stray JS error."
  [text]
  (str/starts-with? (str text) "#error {:message"))

(defn act-cap? [text] (str/includes? (str text) "fuzz: act cap"))

(defn defects
  "Defect keywords (with detail) for the events and final state of a case. Hitting the act cap is not one: a round may be a
  whole night or flight, and the fake world's time does not pass for it."
  [events state capped?]
  (concat
   (when capped? [[:runaway "call cap"]])
   (for [[_ f] (:failed state) :when (not (or (act-cap? (:error f)) (declared-failure? (:error f))))] [:round-threw (str (:error f))])
   (for [e events :when (and (= :system (:source e)) (= :error (:kind e)) (not (act-cap? (:text e))))] [:check-threw (:text e)])
   (for [e events :when (and (= :job (:source e)) (= :waiting (:kind e)) (= :not-ready (event-reason e))
                             (not-any? #(= :error (:kind %)) events))] [:waiting-without-reason (:text e)])
   (for [e events :when (and (= :job (:source e)) (= :stopped (:kind e)) (nil? (event-reason e)))] [:stopped-without-reason (:text e)])))

(defn ^:async run-case
  "Run job once for seed: {:job :seed :args :world :outcome (:refused/:ran) :defects [[kind detail]]}."
  [job entry seed wrong-untyped?]
  (let [r (rng seed)
        args (gen-args r entry wrong-untyped?)
        {:keys [spec ground]} (gen-world r)
        clock (atom 1000000)
        calls (atom {:all 0 :acts 0 :t0 (js/Date.now)})
        capped (atom false)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake spec)
        now (tu/act-clock clock raw 50)
        eng (core/create {:primitives (capped-primitives raw calls capped) :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})
        base {:job job :seed seed :args args :ground ground}
        id (try (core/submit! eng (list job args) {}) (catch :default e e))]
    (if (instance? js/Error id)
      (assoc base :outcome :refused :defects (when (str/blank? (ex-message id)) [[:blank-refusal ""]]))
      (let [thrown (atom nil)
            unrefused (when (seq (:wrong-typed (meta args))) [[:wrong-type-accepted (pr-str (:wrong-typed (meta args)))]])]
        (loop [i 0]
          (when (and (< i max-rounds) (seq (:list (core/state eng))) (not @thrown))
            (swap! clock + 1500)
            (reset! calls {:all 0 :acts 0 :t0 (js/Date.now)})
            (let [ok? (try (await (core/tick! eng)) true (catch :default e (reset! thrown (str e)) false))]
              (when ok? (recur (inc i))))))
        (assoc base :outcome :ran
               :defects (concat unrefused (when (and @thrown (not (act-cap? @thrown))) [[:tick-threw @thrown]])
                                (defects @seen (core/state eng) @capped)))))))

;; ---- the run

(defn env [k default]
  (let [v (aget js/process.env k)] (if (str/blank? v) default v)))

(def skipped
  "Debug tools: not jobs a body runs for itself."
  #{'jobs.debug.access-check 'jobs.debug.notify 'jobs.debug.walk-plan})

(def known
  "{[job kind] card}: defects already carded, so the suite stays green and a new one fails it. Delete an entry with its fix."
  {["jobs.farm.find-spot" :runaway] "a9fb6a61"
          ["jobs.farm.find-spot" :tick-threw] "a9fb6a61"
          ["jobs.survival.dig-niche" :runaway] "a9fb6a61"
          ["jobs.survival.dig-niche" :tick-threw] "a9fb6a61"
          ["jobs.movement.go-to" :round-threw] "a9fb6a61"})

(defn job-cases [wrong-untyped?]
  (let [only (env "FUZZ_JOB" nil)
        n (js/parseInt (env "FUZZ_CASES" "10"))
        base (js/parseInt (env "FUZZ_SEED" "1"))]
    (for [[i [job entry]] (map-indexed vector (sort-by (comp str key) registry/jobs))
          :when (and (not (skipped job)) (or (nil? only) (= only (str job))))
          k (range n)]
      [job entry (+ base (* 7919 i) (* 104729 k)) wrong-untyped?])))

(defn ^:async run-all [wrong-untyped?]
  (loop [todo (job-cases wrong-untyped?) found []]
    (if-let [[job entry seed w] (first todo)]
      (let [_ (when (env "FUZZ_TRACE" nil) (println "fuzz" job seed))
            res (try (await (run-case job entry seed w))
                     (catch :default e {:job job :seed seed :defects [[:harness-threw (str e)]]}))]
        (recur (rest todo) (into found (map (fn [[kind detail]] (assoc (select-keys res [:job :seed :args :ground]) :kind kind :detail detail))) (:defects res))))
      found)))

(defn summary [found]
  (->> (group-by (juxt (comp str :job) :kind) found)
       (sort-by key)
       (map (fn [[[job kind] ds]]
              (let [d (first ds)]
                (str job " " kind " x" (count ds) ": " (pr-str (:detail d))
                     "\n    repro FUZZ_JOB=" job " FUZZ_SEED=" (:seed d) " FUZZ_CASES=1  args " (pr-str (:args d)) " ground " (:ground d)))))
       (str/join "\n")))

(deftest every-job-handles-generated-inputs-honestly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [found (await (run-all (= "1" (env "FUZZ_WRONG_TYPES" "0"))))
              fresh (remove #(contains? known [(str (:job %)) (:kind %)]) found)]
          (is (empty? fresh) (str (count fresh) " defect(s) across " (count (group-by (juxt :job :kind) fresh)) " job/kind pairs:\n" (summary fresh))))))))

(deftest rng-is-deterministic-per-seed
  (is (= (repeatedly 5 (rng 42)) (repeatedly 5 (rng 42))))
  (is (not= (repeatedly 5 (rng 42)) (repeatedly 5 (rng 43)))))

(deftest generated-args-repeat-per-seed-and-wrong-types-hit-typed-args
  (let [entry (get registry/jobs 'jobs.movement.go-to)
        gen #(gen-args (rng %) entry false)]
    (is (= (gen 7) (gen 7)))
    (is (some #(seq (:wrong-typed (meta (gen %)))) (range 60)) "some seed puts a wrong value in the typed :pos arg")))
