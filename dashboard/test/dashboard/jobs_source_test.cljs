(ns dashboard.jobs-source-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.jobs-source :as src]))

(def sample
  "(ns jobs.movement.go-to
  \"Walks somewhere.\"
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  \"Walk to :pos.

  Second paragraph.\")

(def args
  {:pos {:doc \"target\" :default nil}
   :range {:doc \"how close\" :default default-range}})

(defn check [_c] true)
(defn ^:async round [c] #js {:a ::local :b #inst \"2020-01-01\"})
(defn backoff [c] 100)")

(def bare
  "(ns jobs.debug.notify (:require [engine.ctx :as ctx]))
(def doc \"Only a doc.\")
(defn check [_c] true)
(defn round [c] nil)")

(deftest parses-a-job-file
  (let [job (src/parse-job "engine/src/jobs/movement/go_to.cljs" sample)]
    (are [k v] (= v (get job k))
      :id "jobs.movement.go-to"
      :category "movement"
      :name "go-to"
      :file "engine/src/jobs/movement/go_to.cljs"
      :ns-doc "Walks somewhere."
      :doc "Walk to :pos.\n\n  Second paragraph."
      :backoff true
      :error nil)
    (is (= "{:pos {:doc \"target\", :default nil}\n :range {:doc \"how close\", :default default-range}}" (:args job)))))

(deftest parses-a-bare-job-file
  (let [job (src/parse-job "x/jobs/debug/notify.cljs" bare)]
    (are [k v] (= v (get job k))
      :id "jobs.debug.notify"
      :ns-doc nil
      :doc "Only a doc."
      :args nil
      :backoff false)))

(deftest unreadable-file-is-an-error-entry
  (let [job (src/parse-job "x/jobs/a/b.cljs" "(ns jobs.a.b")]
    (are [k v] (= v (get job k))
      :id "jobs.a.b"
      :category "a")
    (is (string? (:error job)))))

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
           (src/usage bodies)))))

(deftest attach-usage-defaults
  (is (= [{:id "jobs.a.b" :running [] :reflex []} {:id "jobs.a.c" :running ["X"] :reflex ["Y"]}]
         (src/attach-usage [{:id "jobs.a.b"} {:id "jobs.a.c"}] {"jobs.a.c" {:running ["X"] :reflex ["Y"]}}))))
