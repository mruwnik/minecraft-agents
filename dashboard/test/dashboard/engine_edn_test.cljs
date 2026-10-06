(ns dashboard.engine-edn-test
  (:require [cljs.test :refer [deftest is testing]]
            [dashboard.engine-edn :as edn]))

(def fight
  "{:next-id 10, :reflex-state {:health-low {:cooldown-until 1791035169236}}, :cursor 1, :instances {\"j1\" {:id \"j1\", :spec {:op :repeat, :child {:op :leaf, :job jobs.movement.look-around, :args {:every-ms 2000}}}, :round 50, :hold? false}}, :list [\"j1\"], :register [{:id :health-low, :trigger :health-low, :job (jobs.survival.recover {:health 7, :healed 16, :sight 16}), :persistence :cooldown, :cooldown-s 10, :builtin? false, :args {:health 7}} {:id :hostile-near, :trigger :hostile-near, :job (jobs.survival.respond-to-hostile {:radius 8}), :persistence :cooldown, :cooldown-s 5, :builtin? false}], :current \"j1\", :changes {}, :resume nil, :failed {}}")

(def seq-edn
  "{:reflex-state {}, :instances {\"j1\" {:id \"j1\", :spec {:op :seq, :children [{:op :leaf, :job jobs.a, :args {}} {:op :repeat, :child {:op :any, :children [{:op :leaf, :job jobs.b.c, :args {}}]}}]}, :round 3, :hold? true} \"j2\" {:id \"j2\", :spec {:op :leaf, :job jobs.movement.pace, :args {}}, :round 1, :hold? false}}, :list [\"j2\" \"j1\"], :register [], :current \"j2\", :failed {\"j9\" {:error \"x\"} \"j8\" {}}}")

(deftest summarize-a-live-engine
  (is (= {:jobs [{:id "j1" :label "repeat jobs.movement.look-around" :current? true :hold? false :round 50}]
          :reflexes [{:id :health-low :trigger :health-low :job "jobs.survival.recover" :persistence :cooldown :cooldown-s 10 :cooling? true}
                     {:id :hostile-near :trigger :hostile-near :job "jobs.survival.respond-to-hostile" :persistence :cooldown :cooldown-s 5 :cooling? false}]
          :current "j1"
          :failed-count 0}
         (edn/summarize fight 1791035000000))))

(deftest cooling-compares-with-now
  (doseq [[now cooling] [[1791035169235 true] [1791035169236 false] [1791035169237 false]]]
    (testing (str now)
      (is (= cooling (:cooling? (first (:reflexes (edn/summarize fight now)))))))))

(deftest combinator-labels-list-order-and-counts
  (let [s (edn/summarize seq-edn 0)]
    (is (= [{:id "j2" :label "jobs.movement.pace" :current? true :hold? false :round 1}
            {:id "j1" :label "seq(jobs.a, repeat any(jobs.b.c))" :current? false :hold? true :round 3}]
           (mapv #(select-keys % [:id :label :current? :hold? :round]) (:jobs s))))
    (is (= 2 (:failed-count s)))))

(deftest unreadable-text-is-an-error
  (doseq [[why text] [["empty" ""] ["nil" nil] ["whitespace" "  \n"] ["unbalanced" "{:list ["] ["not a map" "[1 2]"]]]
    (testing why
      (is (string? (:error (edn/summarize text 0)))))))

(deftest older-register-entries-name-the-job-with-a-keyword
  (let [s (edn/summarize "{:instances {}, :list [], :register [{:id :hungry, :trigger :hungry, :job :eat, :persistence :cooldown, :cooldown-s 90}]}" 0)]
    (is (= ["eat"] (mapv :job (:reflexes s))))))
