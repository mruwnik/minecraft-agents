(ns engine.args-test
  "engine.args: defargs specs with a nested group, submit errors by key path, defaults merged per group, conform
  idempotent, and every real job arg and setting: a want text and a default that fits."
  (:require [cljs.test :refer [deftest is are testing]]
            [cljs.spec.alpha :as s]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.core.restart :as restart]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.settings-registry :as engine-settings]
            [engine.trigger-api :as trigger-api]
            [engine.triggers :as triggers]
            ["fs" :as fs]))

(defn ^:async noop-round [_] :done)

(a/defargs args
  {:pos {:doc "a cell" :spec ::a/pos :default nil}
   :range {:doc "how close" :spec (a/num-in 0 nil) :default 1}
   :doors {:doc "doors" :spec #{:shut :leave-open :never} :default :shut}
   :risk {:doc "what the walk may cost"
          :keys {:min-health {:doc "hp floor" :spec (a/int-in 1 20) :default nil}
                 :max-damage {:doc "hp spent" :spec (a/int-in 0 20) :default 4}
                 :at {:doc "a cell in the group" :spec ::a/pos :default nil}}}})

(def job 'engine.args-test)

(def reg {job {:check (constantly true) :round noop-round :args args}})

(defn problem [form] (expr/problem reg form))

(deftest a-group-takes-its-defaults-and-a-caller-overrides-key-by-key
  (is (= {:pos nil :range 1 :doors :shut :risk {:min-health nil :max-damage 4 :at nil}}
         (:args (expr/parse reg (list job)))))
  (is (= {:pos nil :range 1 :doors :shut :risk {:min-health 12 :max-damage 4 :at nil}}
         (:args (expr/parse reg (list job {:risk {:min-health 12}}))))))

(deftest positions-conform-at-any-depth
  (is (= {:x 1 :y 2 :z 3} (:pos (:args (expr/parse reg (list job {:pos [1 2 3]}))))))
  (is (= {:x 1 :y 2 :z 3} (get-in (expr/parse reg (list job {:risk {:at [1 2 3]}})) [:args :risk :at]))))

(deftest a-bad-value-is-refused-naming-the-job-the-key-path-the-want-and-the-value
  (are [form re] (re-find re (problem form))
    (list job {:risk {:min-health 30}}) #"engine.args-test :risk :min-health must be a whole number from 1 to 20, got 30, in "
    (list job {:range -1}) #"engine.args-test :range must be a number >= 0, got -1"
    (list job {:doors :open}) #"engine.args-test :doors must be one of :leave-open, :never, :shut, got :open"
    (list job {:risk 5}) #"engine.args-test :risk must be a map, got 5"
    (list job {:pos [1 2]}) #"engine.args-test :pos must be \[x y z\] or \{:x :y :z\} of numbers, got \[1 2\]"
    (list job {:risk {:at "x"}}) #"engine.args-test :risk :at must be \[x y z\]"))

(deftest unknown-keys-are-refused-at-every-level
  (is (re-find #"engine.args-test has no arg :bogus; its args are :doors, :pos, :range, :risk" (problem (list job {:bogus 1}))))
  (is (re-find #"engine.args-test :risk has no key :min-hp; its keys are :at, :max-damage, :min-health"
               (problem (list job {:risk {:min-hp 3}})))))

(deftest nil-is-unset-for-a-leaf-and-a-group
  (is (nil? (problem (list job {:range nil :risk nil})))))

(deftest restore-strips-a-stale-key-at-any-level
  (is (= [:bogus [:risk :min-hp]] (vec (restart/stale-keys reg job {:bogus 1 :risk {:min-hp 3 :max-damage 2}}))))
  (is (= {:risk {:max-damage 2}} (restart/strip-keys {:bogus 1 :risk {:min-hp 3 :max-damage 2}} [:bogus [:risk :min-hp]]))))

(deftest merge-args-merges-layers-per-group
  (is (= {:range 3 :risk {:min-health 5 :max-damage 1}}
         (a/merge-args args {:range 1 :risk {:min-health 5 :max-damage 4}} {:range 3} {:risk {:max-damage 1}}))))

(deftest a-hand-built-job-declares-its-specs-at-run-time
  (let [data (a/declare! 'engine.args-test.hand {:n {:spec (a/int-in 0 5) :default 1 :form '(a/int-in 0 5)}})
        r {'engine.args-test.hand {:check (constantly true) :round noop-round :args data}}]
    (is (= '(a/int-in 0 5) (get-in data [:n :spec])) "the data map keeps the form")
    (is (re-find #"engine.args-test.hand :n must be a whole number from 0 to 5, got 9" (expr/problem r '(engine.args-test.hand {:n 9}))))))

(deftest predicates-say-what-they-want
  (are [p text] (= text (a/want p))
    (a/int-in 1 20) "a whole number from 1 to 20"
    (a/num-in 0.1 nil) "a number >= 0.1"
    (a/int-in nil nil) "a whole number"
    #{:b :a} "one of :a, :b"
    (a/coll-of a/item?) "a list, vector or set of an item name (non-empty string)"
    (a/map-of string? number?) "a map of a string to a number"
    (a/or-of (a/num-in 0 nil) false?) "a number >= 0 or false"
    boolean? "true or false"
    ::a/pos "[x y z] or {:x :y :z} of numbers")
  (is (thrown? js/Error (a/coll-of (fn [_] true))) "a pred without a want text cannot be built into another"))

(defn job-leaves
  "[[job path spec-key value] ...] of every leaf of every registered job, with its default."
  []
  (for [[sym entry] registry/jobs
        :let [walk (fn walk [data path]
                     (mapcat (fn [[k e]]
                               (if (:keys e)
                                 (walk (:keys e) (conj path k))
                                 [[sym (conj path k) (keyword (a/level-ns sym path) (name k)) (:default e)]]))
                             data))]
        leaf (walk (:args entry) [])]
    leaf))

(deftest every-job-arg-has-a-spec-a-want-text-and-a-default-that-fits
  (is (< 600 (count (job-leaves))) "the registry is loaded")
  (doseq [[sym path k default] (job-leaves)]
    (is (s/get-spec k) (str sym " " path " has no spec"))
    (is (string? (get @a/wants k)) (str sym " " path " has no want text"))
    (is (s/valid? k default) (str sym " " path " default " (pr-str default) " does not fit " (get @a/wants k)))))

(deftest every-setting-has-a-spec-and-a-default-that-fits
  (doseq [[k spec] (merge registry/settings engine-settings/settings)]
    (is (s/get-spec k) (str k " has no spec"))
    (is (nil? (a/problem k (:default spec))) (str k " default " (pr-str (:default spec))))))

(deftest conform-is-idempotent-for-every-job
  (testing "stored args are already conformed and go through leaf again on every build and restore"
    (doseq [sym (cons job (keys registry/jobs))
            :let [once (second (expr/leaf (merge registry/jobs reg) sym {}))]]
      (is (= once (second (expr/leaf (merge registry/jobs reg) sym once))) (str sym))))
  (let [once (:args (expr/parse reg (list job {:pos [1 2 3] :risk {:at [4 5 6]}})))]
    (is (= once (second (expr/leaf reg job once))))))

(deftest a-setting-refuses-a-bad-value-with-its-want
  (is (= "a whole number >= 1000" (a/problem :engine.perception/save-ms 5)))
  (is (= "a value, not nil" (a/problem :engine.perception/save-ms nil)))
  (is (nil? (a/problem :engine.perception/save-ms 60000))))

(deftest the-catalog-gives-each-spec-as-its-want
  (let [d (a/described job args)]
    (is (= "a number >= 0" (get-in d [:range :spec])))
    (is (= "a whole number from 1 to 20" (get-in d [:risk :keys :min-health :spec])))
    (is (not-any? #(str/includes? (str %) "#object") (tree-seq coll? seq d)))))

(deftest every-shipped-scenario-still-parses
  (let [ts (trigger-api/with-conditions triggers/all trigger-api/compile-condition)]
    (doseq [f (sort (.readdirSync fs "scenarios"))
            :when (str/ends-with? f ".edn")]
      (is (= [] (scenario/problems registry/jobs ts (scenario/with-defaults (scenario/read-file (str "scenarios/" f))))) f))))
