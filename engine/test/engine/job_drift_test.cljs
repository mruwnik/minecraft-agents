(ns engine.job-drift-test
  "Static drift guards over the job sources, each with a shrink-only allow-list (plain data beside this file): a new
  offender fails, and a listed one that is now clean fails too, so the list only shrinks.
  - quote-of: a job's quote-of must reach ctx/quote, jobs.lib.quote or jobs.lib.cost* (quote_allow.edn).
  - callers: an arg or :settings key that fixtures and tests pass to a job needs a caller in src, a trigger or the
    ./bin text (arg_allow.edn). Calls are read as `jobs.x.y {map}` in the text, so a call built another way is not seen."
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is]]
            [clojure.set :as set]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.layering-test :as layering]
            ["fs" :as fs]
            ["path" :as path]))

;; ----------------------------------------------------------------- reading

(defn read-text [file] (.readFileSync fs file "utf8"))

(defn scan-all
  "Every file under dir whose name matches re."
  [dir re]
  (mapcat (fn [e]
            (let [p (path/join dir (.-name e))]
              (cond (.isDirectory e) (scan-all p re)
                    (re-find re (.-name e)) [p])))
          (.readdirSync fs dir #js {:withFileTypes true})))

(defn strip
  "The text without string literals and comments."
  [text]
  (str/replace text #"\"(?:[^\"\\]|\\.)*\"|;[^\n]*" ""))

(defn chunks
  "{def-name chunk-text} for the top-level forms of text (strings and comments cut)."
  [text]
  (into {} (keep (fn [c] (when-let [[_ n] (re-find #"^\(def[a-z-]*\s+(?:\^\S+\s+)*([^\s()\[\]]+)" c)] [n c])))
        (str/split (strip text) #"(?m)^(?=\()")))

(defn tokens [chunk] (re-seq #"[^\s()\[\]{}'`~@^#,]+" chunk))

(defn requires-info
  "{:aliases {alias ns} :refers {sym [ns name]}} (strings) of an ns form's :require clause."
  [ns-form]
  (let [entries (for [clause ns-form
                      :when (and (seq? clause) (= :require (first clause)))
                      e (rest clause)
                      :when (vector? e)]
                  [(str (first e)) (apply hash-map (rest e))])]
    {:aliases (into {} (for [[n m] entries :when (:as m)] [(str (:as m)) n]))
     :refers (into {} (for [[n m] entries r (:refer m)] [(str r) [n (str r)]]))}))

(defn module
  [file]
  (let [text (read-text file)
        ns-form (reader/read-string text)]
    (assoc (requires-info ns-form) :ns (str (second ns-form)) :defs (chunks text) :job? (layering/job-text? text))))

(defn modules
  "{ns-string module} for src/jobs and src/triggers."
  []
  (into {} (map (fn [f] (let [m (module f)] [(:ns m) m]))) (concat (layering/scan "src/jobs") (layering/scan "src/triggers"))))

;; ---------------------------------------------------------------- quote-of

(defn reaching?
  "Is [ns name] one of the quote makers a quote-of may rest on."
  [[n d]]
  (or (= n "jobs.lib.quote") (str/starts-with? n "jobs.lib.cost") (and (= n "engine.ctx") (= d "quote"))))

(defn targets
  "The [ns name] that the symbols of a def's chunk point to, through the module's aliases, refers and own defs."
  [{:keys [aliases refers defs] module-ns :ns} chunk]
  (for [t (tokens chunk)
        :when (not (str/starts-with? t ":"))
        :let [[a b] (str/split t #"/" 2)
              target (cond (and a b (seq b)) [(get aliases a a) b]
                           (contains? refers t) (refers t)
                           (contains? defs t) [module-ns t])]
        :when target]
    target))

(defn reaches-quote?
  "Does the def start ([ns name]) reach a quote maker through the defs it calls, in any module?"
  [mods start]
  (loop [todo [start] seen #{start}]
    (if-let [[n d] (first todo)]
      (let [mod (get mods n)
            ts (targets mod (get-in mod [:defs d] ""))]
        (if (some reaching? ts)
          true
          (let [more (remove seen (filter (fn [[tn td]] (get-in mods [tn :defs td])) ts))]
            (recur (into (rest todo) more) (into seen more)))))
      false)))

(defn quote-offenders
  "Sorted job namespaces whose quote-of reaches no quote maker."
  [mods]
  (vec (sort (for [[n m] mods
                   :when (and (:job? m) (contains? (:defs m) "quote-of"))
                   :when (not (reaches-quote? mods [n "quote-of"]))]
               (symbol n)))))

(def fabricated-quote-mods
  (let [mod (fn [ns-str aliases defs] (merge {:ns ns-str :aliases aliases :refers {} :defs defs} {:job? true}))]
    {"jobs.a.direct" (mod "jobs.a.direct" {"ctx" "engine.ctx"} {"quote-of" "(defn quote-of [c] (ctx/quote c 'jobs.b.x {}))"})
     "jobs.a.via-helper" (mod "jobs.a.via-helper" {"h" "jobs.a.help"} {"quote-of" "(defn quote-of [c] (h/price c))"})
     "jobs.a.help" (mod "jobs.a.help" {"cost" "jobs.lib.cost.ways"} {"price" "(defn price [c] (cost/ways c))"})
     "jobs.a.local" (mod "jobs.a.local" {} {"quote-of" "(defn quote-of [c] (own c))" "own" "(defn own [c] (cost/x c))"})
     "jobs.a.flat" (mod "jobs.a.flat" {"ctx" "engine.ctx"} {"quote-of" "(defn quote-of [_] {:uses {:job-time [0 1 2]}})"})
     "jobs.a.loop" (mod "jobs.a.loop" {} {"quote-of" "(defn quote-of [c] (a c))" "a" "(defn a [c] (quote-of c))"})}))

(deftest quote-of-offenders-are-those-reaching-no-quote-maker
  (is (= '[jobs.a.flat jobs.a.local jobs.a.loop] (quote-offenders fabricated-quote-mods))))

(defn read-allow [file] (reader/read-string (read-text (str "test/engine/" file))))

(defn same-set
  [found allowed what]
  (is (= #{} (set/difference found allowed)) (str "new " what ": fix it, do not list it"))
  (is (= #{} (set/difference allowed found)) (str what " that are now clean: delete them from the allow-list")))

(deftest quote-of-reaches-the-cost-functions
  (same-set (set (quote-offenders (modules))) (set (read-allow "quote_allow.edn")) "quote-of that reach no quote maker"))

;; ------------------------------------------------------------ call-site args

(defn map-end
  "The index just after the map that opens at text[start], or nil when it never closes."
  [text start]
  (loop [j start depth 0 in-str? false]
    (when (< j (count text))
      (let [ch (.charAt text j)]
        (cond
          (and in-str? (= ch "\\")) (recur (+ j 2) depth true)
          (= ch "\"") (recur (inc j) depth (not in-str?))
          in-str? (recur (inc j) depth true)
          (= ch "{") (recur (inc j) (inc depth) false)
          (and (= ch "}") (= depth 1)) (inc j)
          (= ch "}") (recur (inc j) (dec depth) false)
          :else (recur (inc j) depth false))))))

(defn read-map
  "The map in s, or nil when it does not read as plain data (fn literals, regexes). ::k becomes :tmp/k."
  [s]
  (try (reader/read-string (-> s (str/replace "::" ":tmp/") (str/replace "#(" "(") (str/replace #"[@'`~]" "")))
       (catch :default _ nil)))

(defn job-calls
  "[job-ns-string args-map] for each `jobs.x.y {map}` in text whose map reads as data."
  [text]
  (let [re (js/RegExp. "(jobs\\.[a-z0-9.\\-]+)\\s*\\{" "g")]
    (loop [acc []]
      (if-let [m (.exec re text)]
        (let [start (+ (.-index m) (count (aget m 0)) -1)
              end (map-end text start)
              args (when end (read-map (subs text start end)))]
          (recur (cond-> acc (map? args) (conj [(aget m 1) args]))))
        acc))))

(def not-job-args
  "The keys every call takes, and the ones tests pass on purpose to see a job refuse them."
  (into #{:typo :bogus} (keys contract/call-keys)))

(defn plain-key? [k] (and (keyword? k) (nil? (namespace k)) (not (not-job-args k))))
(defn settings-key? [k] (and (keyword? k) (namespace k) (not= "tmp" (namespace k))))

(defn passes
  "{:args #{[job-sym key]} :settings #{key}} that the `jobs.x.y {map}` calls in text pass, for the jobs in mods."
  [mods text]
  (let [calls (filter (fn [[j]] (get-in mods [j :job?])) (job-calls text))]
    {:args (set (for [[j m] calls k (keys m) :when (plain-key? k)] [(symbol j) k]))
     :settings (set (for [[_ m] calls
                          :let [st (:settings m)]
                          :when (map? st)
                          k (keys st)
                          :when (settings-key? k)]
                      k))}))

(defn union-passes [ps] (apply merge-with set/union {:args #{} :settings #{}} ps))

(defn caller-texts
  "[file text] of what documents or performs a call outside the tests: src, the dashboard src (the ./bin tool text)."
  []
  (for [dir ["src" "../dashboard/src"]
        f (scan-all dir #"\.(cljs?|edn)$")]
    [f (str/replace (read-text f) "\\\"" "\"")]))

(defn test-texts
  "[file text] of what passes args in tests and fixtures."
  []
  (for [[dir re] [["test" #"\.cljs$"] ["fixtures/world" #"\.edn$"] ["scenarios" #"\.edn$"]]
        f (scan-all dir re)]
    [f (read-text f)]))

(defn mentions
  "{:args #{[job key]} :settings #{key}} that a src chunk names without a literal call: a chunk that mentions a job
  namespace and a plain keyword (args builders), or a :settings word and a namespaced keyword of another namespace.
  The job's own namespace is no caller."
  [mods [file text]]
  (let [module-ns (some-> text reader/read-string second str)
        cs (vals (chunks text))]
    {:args (set (for [c cs
                      :let [ts (tokens c)
                            jobs (filter #(and (get-in mods [% :job?]) (not= % module-ns)) ts)
                            ks (filter #(and (str/starts-with? % ":") (not (str/starts-with? % "::"))
                                             (not (str/includes? % "/")))
                                       ts)]
                      j jobs k ks]
                  [(symbol j) (keyword (subs k 1))]))
     :settings (set (for [c cs
                          :let [ts (tokens c)]
                          :when (some #{":settings"} ts)
                          t ts
                          :when (re-find #"^:[^:/]+/" t)
                          :let [k (keyword (subs t 1))]
                          :when (not= (namespace k) module-ns)]
                      k))}))

(defn caller-passes [mods]
  (union-passes (mapcat (fn [[f t :as ft]]
                          [(passes mods t) (if (str/ends-with? f ".edn") {} (mentions mods ft))])
                        (caller-texts))))

(defn test-passes [mods] (union-passes (map (fn [[_ t]] (passes mods t)) (test-texts))))

(defn uncalled
  "{:args [[job key]...] :settings [key...]} sorted: passed by the tests and fixtures, by no caller."
  [mods]
  (let [t (test-passes mods) c (caller-passes mods)]
    {:args (vec (sort (set/difference (:args t) (:args c))))
     :settings (vec (sort (set/difference (:settings t) (:settings c))))}))

(deftest passes-reads-literal-calls
  (let [mods {"jobs.a.b" {:job? true}}]
    (is (= {:args #{['jobs.a.b :n] ['jobs.a.b :limit]} :settings #{:jobs.a.b/k}}
           (passes mods "[:job (jobs.a.b {:n 3 :limit #{:x} :ways #{:y} :settings {:jobs.a.b/k 1 ::own 2}})] (jobs.c.d {:z 1})")))
    (is (= {:args #{['jobs.a.b :s]} :settings #{}} (passes mods "'jobs.a.b {:s \"a } b\"}")))))

(deftest a-builder-chunk-counts-as-a-caller
  (let [mods {"jobs.a.b" {:job? true}}
        text "(ns jobs.c.d)\n(defn args [c]\n  (assoc {:x 1} :limit 2 :settings {:jobs.e.f/g 1}))\n(defn go [c] (call c 'jobs.a.b (args c)))\n(defn one [c] (call c 'jobs.a.b {:y 1}))"]
    (is (= #{['jobs.a.b :y]} (:args (mentions mods ["x.cljs" text]))) "a keyword beside the job symbol, not in another chunk")
    (is (= #{:jobs.e.f/g} (:settings (mentions mods ["x.cljs" text]))))))

(deftest arg-keys-with-no-caller
  (let [{:keys [args settings]} (uncalled (modules))
        allow (read-allow "arg_allow.edn")]
    (same-set (set args) (set (:args allow)) "fixture/test args that no caller passes")
    (same-set (set settings) (set (:settings allow)) "fixture/test :settings keys that no caller passes")))

