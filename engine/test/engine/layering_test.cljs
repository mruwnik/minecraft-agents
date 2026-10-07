(ns engine.layering-test
  "Layering guard: jobs call other jobs as children, shared helpers live in jobs.lib, and jobs and libs never reach into
  triggers. Rules over the ns forms of src/jobs and src/triggers; the known leftovers are the shrink-only
  allow-list layering_allow.edn (one reason per edge)."
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is]]
            [clojure.set :as set]
            [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]))

;; ------------------------------------------------------------------ rules

(defn segments [ns-sym] (str/split (str ns-sym) #"\."))

(defn kind
  "What a namespace is: :trigger, :lib (jobs.lib.*), :job, :helper (a non-lib jobs.* file without check+round), or
  nil for anything else."
  [files ns-sym]
  (let [[root second-seg] (segments ns-sym)]
    (cond
      (= root "triggers") :trigger
      (not= root "jobs") nil
      (= second-seg "lib") :lib
      (get-in files [ns-sym :job?]) :job
      (contains? files ns-sym) :helper)))

(defn family [ns-sym] (second (segments ns-sym)))

(defn edge-violation
  "The rule broken by `from` requiring `to`, or nil."
  [files from to]
  (let [fk (kind files from)
        tk (kind files to)]
    (cond
      (and (= fk :job) (= tk :trigger)) :job-requires-trigger
      (and (= fk :helper) (= tk :trigger)) :job-requires-trigger
      (and (= fk :lib) (= tk :trigger)) :lib-requires-trigger
      (and (#{:lib :trigger} fk) (#{:job :helper} tk)) :lib-requires-job
      (and (#{:job :helper} fk) (#{:job :helper} tk) (not= (family from) (family to)))
      (if (= tk :job) :job-requires-other-family-job :helper-required-cross-family))))

(defn violations
  "Sorted [from to rule] for every require edge that breaks a rule. files: {ns-sym {:requires #{sym} :job? bool}}."
  [files]
  (vec (sort (for [[from {:keys [requires]}] files
                   to requires
                   :let [rule (edge-violation files from to)]
                   :when rule]
               [from to rule]))))

;; ----------------------------------------------------------------- reading

(defn requires-of
  "The namespaces in the ns form's :require clause."
  [ns-form]
  (set (for [clause ns-form
             :when (and (seq? clause) (= :require (first clause)))
             entry (rest clause)]
         (if (coll? entry) (first entry) entry))))

(defn scan [dir]
  (mapcat (fn [e]
            (let [p (path/join dir (.-name e))]
              (cond (.isDirectory e) (scan p)
                    (re-find #"\.cljs$" (.-name e)) [p])))
          (.readdirSync fs dir #js {:withFileTypes true})))

(defn defines? [text name]
  (boolean (re-find (re-pattern (str "(?m)^\\(def[a-z-]*\\s+(?:\\^\\S+\\s+)*" name "(?:\\s|\\)|$)")) text)))

(defn job-text? [text] (and (defines? text "check") (defines? text "round")))

(defn read-file [file]
  (let [text (.readFileSync fs file "utf8")
        ns-form (reader/read-string text)]
    [(second ns-form) {:requires (requires-of ns-form) :job? (job-text? text)}]))

(defn source-files []
  (into {} (map read-file) (concat (scan "src/jobs") (scan "src/triggers"))))

(defn allow-list []
  (reader/read-string (.readFileSync fs "test/engine/layering_allow.edn" "utf8")))

;; ------------------------------------------------------------------- tests

(def fabricated
  {'jobs.a.job {:requires '#{triggers.a.cond jobs.lib.x jobs.a.split jobs.b.job jobs.b.help} :job? true}
   'jobs.a.split {:requires #{} :job? false}
   'jobs.b.job {:requires #{} :job? true}
   'jobs.b.help {:requires #{} :job? false}
   'jobs.lib.x {:requires '#{jobs.a.job triggers.a.cond} :job? false}
   'triggers.a.cond {:requires '#{jobs.lib.x jobs.b.help} :job? false}})

(deftest guard-finds-fabricated-violations
  (is (= '[[jobs.a.job jobs.b.help :helper-required-cross-family]
           [jobs.a.job jobs.b.job :job-requires-other-family-job]
           [jobs.a.job triggers.a.cond :job-requires-trigger]
           [jobs.lib.x jobs.a.job :lib-requires-job]
           [jobs.lib.x triggers.a.cond :lib-requires-trigger]
           [triggers.a.cond jobs.b.help :lib-requires-job]]
         (violations fabricated))))

(deftest job-text-needs-check-and-round
  (is (job-text? "(ns x)\n(defn check [ctx] 1)\n(defn round [ctx] 2)"))
  (is (not (job-text? "(ns x)\n(defn check [ctx] 1)\n(defn rounds [ctx] 2)"))))

(deftest requires-of-reads-vectors-and-symbols
  (is (= '#{a.b c.d e.f} (requires-of '(ns x "doc" (:require [a.b :as b] [c.d :refer [y]] e.f))))))

(deftest layering-matches-the-allow-list
  (let [found (set (violations (source-files)))
        allowed (set (map (juxt :from :to :rule) (allow-list)))]
    (is (= #{} (set/difference found allowed)) "new layering violations: move the helper into jobs.lib")
    (is (= #{} (set/difference allowed found)) "allow-list entries that no longer break a rule: delete them")))

(deftest allow-list-gives-a-reason-for-every-edge
  (is (every? (comp seq :why) (allow-list))))
