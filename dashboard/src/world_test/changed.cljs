(ns world-test.changed
  "Pure part of --changed-since-pass: which fixture files may skip a run because the job and trigger code they use is
  unchanged since their last recorded pass. Any change to shared engine code selects every fixture."
  (:require [clojure.string :as str]))

(def ns-token #"\b[a-z][a-z0-9\-]*(?:\.[a-z0-9\-]+)+")
(def trigger-token #":([a-z0-9\-]+)")

(defn ns-file [ns-name]
  (str "engine/src/" (str/replace (str/replace ns-name "-" "_") "." "/") ".cljs"))

(defn trigger-index
  "{trigger keyword name file} for the trigger sources among the known paths."
  [known]
  (into {} (for [p known
                 :when (and (str/starts-with? p "engine/src/triggers/") (str/ends-with? p ".cljs"))]
             [(str/replace (last (str/split p #"/")) #"\.cljs$" "") p])))

(defn mentions
  "A memoised fn text-key -> source files the text names: namespaces by symbol (any dotted name that is a known file),
  triggers by keyword. Built once per source set, so a run indexes the sources once."
  [known]
  (let [known? (set known)
        idx (trigger-index known)
        scan (fn [text]
               (into (set (filter known? (map ns-file (re-seq ns-token text))))
                     (keep #(idx (str/replace (second %) "-" "_")))
                     (re-seq trigger-token text)))]
    scan))

(defn scanner
  "{:scan text -> files, :of path -> files its source names (memoised)} over src {path text}."
  [src]
  (let [scan (mentions (keys src))]
    {:scan scan :of (memoize (fn [p] (scan (src p))))}))

(defn closure
  "The source files fixture text uses, transitively through the names in their own source. src: {path text} of every
  job and trigger source; sc: (scanner src), shared across fixtures."
  ([fixture-text src] (closure fixture-text src (scanner src)))
  ([fixture-text _src {:keys [scan of]}]
   (loop [seen #{} todo (scan fixture-text)]
     (if (empty? todo)
       seen
       (let [p (first todo)]
         (recur (conj seen p) (into (disj todo p) (remove seen) (of p))))))))

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
  "The fixture paths to run: no recorded pass, a rev git cannot diff (changed-since gives nil), or a change since it.
  fixtures {repo-relative path text}, record {path rev}, src {path text} of every job and trigger source,
  changed-since (rev -> changed repo paths or nil)."
  [{:keys [fixtures record src changed-since]}]
  (set (for [sc [(scanner src)]
             [path text] fixtures
             :let [rev (get record path)
                   changed (when rev (changed-since rev))]
             :when (or (nil? changed) (stale? (closure text src sc) path changed))]
         path)))

(defn recordable
  "The passed fixture paths whose pass may be recorded: HEAD and the changed set held from the run's start (the body
  build) to its end, and none of their code differs from HEAD. start/end: {:rev :changed}."
  [{:keys [passed fixtures src start end]}]
  (if (or (empty? passed) (nil? (:rev start)) (nil? (:changed end)) (not= (:rev start) (:rev end)) (not= (set (:changed start)) (set (:changed end))))
    #{}
    (set (remove (stale-stems {:fixtures (select-keys fixtures passed) :record (zipmap passed (repeat (:rev end))) :src (force src)
                               :changed-since (constantly (:changed end))})
                 passed))))

(defn next-record
  "The pass record after a run: fixtures that ran without passing lose their record, clean ones get rev."
  [record rev {:keys [clean ran]}]
  (merge (apply dissoc record (remove (set clean) ran)) (zipmap clean (repeat rev))))

(defn passed-stems
  "The stems whose every expected run (expected {stem n}) passed in results ({:file :status})."
  [expected results]
  (let [by-file (group-by :file results)]
    (set (for [[stem n] expected
               :let [rs (by-file stem)]
               :when (and (= n (count rs)) (every? #(= :pass (:status %)) rs))]
           stem))))
