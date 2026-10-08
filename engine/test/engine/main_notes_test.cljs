(ns engine.main-notes-test
  "engine.main opens the body's world with its notes store in state/worlds/<world>/notes/<agent>.edn."
  (:require [cljs.test :refer [deftest is]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.main :as main]
            [engine.notes :as notes]
            [engine.test-util :as tu]))

(deftest the-world-carries-the-body-notes-store-in-its-world-folder
  (let [state (tu/tmp-dir)
        w (main/open-world {:state-dir state :world "test" :agent "ProbeView" :root "/engine" :emit (fn [_])})
        file (path/join state "worlds" "test" "notes" "ProbeView.edn")]
    (is (= "ProbeView" (notes/body (:notes w))))
    (is (= :ok (:status (notes/add! (:notes w) 1000 [{:kind :seen :what "oak_log" :pos [1 64 1] :by "ProbeView"
                                                       :t 1000 :until 2000}]))))
    (is (fs/existsSync file))
    (is (= (path/join state "worlds" "test" "plans") (get-in w [:opts :plans-dir])))))

(deftest the-world-reads-blueprints-from-the-given-folder-else-beside-the-engine
  (let [open #(main/open-world (merge {:state-dir (tu/tmp-dir) :world "test" :agent "ProbeView" :root "/repo/engine" :emit (fn [_])} %))]
    (is (= "/repo/blueprints" (get-in (open {}) [:opts :blueprint-dir])))
    (is (= "/main/blueprints" (get-in (open {:blueprints-dir "/main/blueprints"}) [:opts :blueprint-dir])))))

(deftest parse-args-takes-the-blueprints-flag
  (is (= "/main/blueprints" (:blueprints (main/parse-args ["--agent" "A" "--blueprints" "/main/blueprints"])))))
