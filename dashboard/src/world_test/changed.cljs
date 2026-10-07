(ns world-test.changed
  "Pure part of --changed-since-pass: which fixture files may skip a run because the job and trigger code they use is
  unchanged since their last recorded pass. Any change to shared engine code selects every fixture."
  (:require [clojure.string :as str]))

(def ns-token #"\bjobs\.[a-z0-9\-]+(?:\.[a-z0-9\-]+)*")
(def trigger-token #":([a-z0-9\-]+)")

(defn ns-file [ns-name]
  (str "engine/src/" (str/replace (str/replace ns-name "-" "_") "." "/") ".cljs"))

(defn trigger-file
  "The file of trigger keyword name among the known source paths, or nil."
  [name known]
  (let [tail (str "/" (str/replace name "-" "_") ".cljs")]
    (first (filter #(and (str/starts-with? % "engine/src/triggers/") (str/ends-with? % tail)) known))))

(defn mentioned
  "Source files the text names: job namespaces by symbol, triggers by keyword."
  [text known]
  (let [known? (set known)]
    (into (set (filter known? (map ns-file (re-seq ns-token text))))
          (keep #(trigger-file (second %) known))
          (re-seq trigger-token text))))

(defn closure
  "The source files fixture text uses, transitively through the names in their own source. src: {path text} of every
  job and trigger source."
  [fixture-text src]
  (let [known (keys src)]
    (loop [seen #{} todo (mentioned fixture-text known)]
      (if (empty? todo)
        seen
        (let [p (first todo)]
          (recur (conj seen p)
                 (into (disj todo p) (remove seen) (mentioned (src p) known))))))))

(defn leaf-source? [p]
  (and (str/ends-with? p ".cljs")
       (or (str/starts-with? p "engine/src/jobs/")
           (re-find #"^engine/src/triggers/[^/]+/" p))))

(defn shared-code?
  "A changed path that can affect every fixture: engine code, js, trigger defaults, the runner itself."
  [p]
  (and (not (leaf-source? p))
       (or (str/starts-with? p "engine/src/")
           (str/starts-with? p "engine/js/")
           (str/starts-with? p "dashboard/src/world_test/")
           (#{"engine/shadow-cljs.edn" "engine/package.json"} p))))

(defn stale?
  "True when a changed path is the fixture's own file, in its closure, or shared code. Other paths do not matter."
  [closure fixture-path changed]
  (boolean (some #(or (= % fixture-path) (closure %) (shared-code? %)) changed)))

(defn stale-stems
  "The fixture stems to run: no recorded pass, a rev git cannot diff (changed-since gives nil), or a change since it.
  fixtures {stem text}, record {stem rev}, src {path text}, dir the fixtures' repo-relative directory,
  changed-since (rev -> changed repo paths or nil)."
  [{:keys [fixtures record src dir changed-since]}]
  (set (for [[stem text] fixtures
             :let [rev (get record stem)
                   changed (when rev (changed-since rev))]
             :when (or (nil? changed) (stale? (closure text src) (str dir "/" stem ".edn") changed))]
         stem)))

(defn passed-stems
  "The stems whose every expected run (expected {stem n}) passed in results ({:file :status})."
  [expected results]
  (let [by-file (group-by :file results)]
    (set (for [[stem n] expected
               :let [rs (by-file stem)]
               :when (and (= n (count rs)) (every? #(= :pass (:status %)) rs))]
           stem))))
