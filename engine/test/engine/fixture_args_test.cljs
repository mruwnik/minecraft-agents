(ns engine.fixture-args-test
  "World fixtures only submit job args the job declares: a refused :bad-spec is caught here, not in a sweep."
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            [engine.expr :as expr]
            [engine.registry :as registry]
            ["fs" :as fs]))

(def dir "fixtures/world")

(defn job-forms
  "Every (jobs.x.y {...}) list in a fixture's text, at any depth."
  [text]
  (->> (tree-seq coll? seq (reader/read-string {:default (fn [_ v] v)} text))
       (filter #(and (seq? %) (symbol? (first %)) (str/starts-with? (str (first %)) "jobs.")))))

(defn bad-args [file]
  (keep (fn [[sym args]] (expr/unknown-keys-message sym (get registry/jobs sym) args))
        (job-forms (.readFileSync fs (str dir "/" file) "utf8"))))

(deftest every-fixture-job-arg-is-declared-by-its-job
  (is (= {} (into {} (keep (fn [f] (when-let [bad (seq (bad-args f))] [f (vec (distinct bad))])))
                  (filter #(str/ends-with? % ".edn") (.readdirSync fs dir))))))
