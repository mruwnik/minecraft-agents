(ns dashboard.jobs-registry-test
  (:require [cljs.test :refer [deftest is are]]
            [clojure.string :as str]
            [dashboard.jobs-registry :as reg]))

(defn entry [id] (first (filter #(= id (:id %)) reg/entries)))

(deftest compiled-in-job-entry
  (let [job (entry "jobs.movement.go-to")]
    (are [k v] (= v (get job k))
      :kind :job
      :category "movement"
      :name "go-to"
      :file "engine/src/jobs/movement/go_to.cljs")
    (are [k] (string? (get job k))
      :doc :args)
    (is (boolean? (:backoff job)))))

(deftest every-entry-is-well-formed
  (is (every? #(and (string? (:id %)) (string? (:category %)) (string? (:file %))
                    (str/ends-with? (:file %) ".cljs") (#{:job :trigger} (:kind %)))
              reg/entries))
  (is (= (count reg/entries) (count (set (map :id reg/entries))))))

(deftest triggers-are-listed
  (let [t (entry "stuck")]
    (are [k v] (= v (get t k))
      :kind :trigger
      :category "triggers"
      :name "stuck"
      :file "engine/src/engine/triggers/stuck.cljs"
      :job "(jobs.maintenance.unstick)")
    (is (string? (:doc t)))
    (is (string? (:ns-doc t)))))

(deftest every-default-trigger-is-listed
  (is (= #{"suffocating" "burning" "wedged" "hostile-near" "hungry" "night" "stuck" "died" "pen-gate" "door-left"
           "inventory-nearly-full" "scaffold-left" "tidy-pending" "mounted" "player-joined"}
         (set (map :id (filter #(= :trigger (:kind %)) reg/entries))))))

(deftest pretty-args-one-entry-per-line
  (are [value out] (= out (reg/pretty value))
    {:a 1 :b "x"} "{:a 1\n :b \"x\"}"
    nil "nil"
    [1 2] "[1 2]"))

(deftest usage-join
  (let [bodies [{:name "A" :engine {:jobs [{:label "jobs.movement.pace"} {:label "repeat jobs.movement.go-to"}]
                                    :reflexes [{:job "jobs.survival.eat"}]}}
                {:name "B" :engine {:jobs [{:label "seq(jobs.movement.pace, jobs.time.wait-for-day)"}]
                                    :reflexes []}}
                {:name "C" :engine nil}]]
    (is (= {"jobs.movement.pace" {:running ["A" "B"] :reflex []}
            "jobs.movement.go-to" {:running ["A"] :reflex []}
            "jobs.time.wait-for-day" {:running ["B"] :reflex []}
            "jobs.survival.eat" {:running [] :reflex ["A"]}}
           (reg/usage bodies)))))

(deftest attach-usage-defaults
  (is (= [{:id "jobs.a.b" :running [] :reflex []} {:id "jobs.a.c" :running ["X"] :reflex ["Y"]}]
         (reg/attach-usage [{:id "jobs.a.b"} {:id "jobs.a.c"}] {"jobs.a.c" {:running ["X"] :reflex ["Y"]}}))))
