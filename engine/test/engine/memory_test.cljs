(ns engine.memory-test
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(deftest scopes-start-empty
  (let [store (mem/open (tu/tmp-dir))]
    (is (= {} (mem/scope store :common)))
    (is (= {:records []} (mem/scope store :body)))
    (is (= {} (mem/job store ["j1"])))))

(deftest commits-write-json-and-reload
  (let [dir (tu/tmp-dir)
        store (mem/open dir)]
    (mem/commit! store :body {:records [] :home {:x 1 :y 2 :z 3}})
    (mem/commit! store :common #(assoc % :places {:bed [{:pos {:x 0 :y 64 :z 0}}]}))
    (mem/commit-job! store ["j1"] {:planted 3})
    (mem/commit-job! store ["j1" :fell] {:logs 2})
    (mem/commit-job! store ["j1" :fell :walk] {:tries 1})
    (is (= {:planted 3} (:mem (tu/read-json (path/join dir "jobs" "j1.json")))))
    (let [again (mem/open dir)]
      (is (= {:x 1 :y 2 :z 3} (:home (mem/scope again :body))))
      (is (= [{:pos {:x 0 :y 64 :z 0}}] (mem/places (mem/snapshot again) :bed)))
      (is (= {:planted 3} (mem/job again ["j1"])))
      (is (= {:logs 2} (mem/job again ["j1" :fell])))
      (is (= {:tries 1} (mem/job again ["j1" :fell :walk]))))))

(deftest a-commit-function-gets-the-current-value
  (let [store (mem/open (tu/tmp-dir))]
    (mem/commit-job! store ["j2"] {:n 1})
    (mem/commit-job! store ["j2"] #(update % :n inc))
    (is (= {:n 2} (mem/job store ["j2"])))))

(deftest children-can-be-marked-done-and-jobs-deleted
  (let [dir (tu/tmp-dir)
        store (mem/open dir)]
    (mem/commit-job! store ["j3" :a] {:x 1})
    (mem/mark-done! store ["j3" :a])
    (is (true? (mem/done? store ["j3" :a])))
    (is (false? (mem/done? store ["j3" :b])))
    (is (= {} (mem/job store ["j3" :a])))
    (mem/delete-job! store "j3")
    (is (false? (fs/existsSync (path/join dir "jobs" "j3.json"))))
    (is (false? (mem/done? store ["j3" :a])))))

(deftest records-are-added-read-and-dropped
  (let [store (mem/open (tu/tmp-dir))]
    (mem/add-record! store {:kind "hurt" :t 1 :health 5})
    (mem/add-record! store {:kind "chat" :t 2})
    (mem/add-record! store {:kind "hurt" :t 3 :health 4})
    (is (= [1 3] (mapv :t (mem/records (mem/snapshot store) "hurt"))))
    (is (= [{:kind "chat" :t 2}] (:records (mem/drop-records (mem/scope store :body) "hurt"))))))

(deftest ids-render-paths
  (is (= "j1" (mem/path->id ["j1"])))
  (is (= "j1/fell/walk" (mem/path->id ["j1" :fell :walk]))))
