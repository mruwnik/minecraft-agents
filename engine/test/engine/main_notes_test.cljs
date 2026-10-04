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
        w (main/open-world {:state-dir state :world "claude" :agent "ProbeView" :root "/engine" :emit (fn [_])})
        file (path/join state "worlds" "claude" "notes" "ProbeView.edn")]
    (is (= "ProbeView" (notes/body (:notes w))))
    (is (= :ok (:status (notes/add! (:notes w) 1000 [{:kind :seen :what "oak_log" :pos [1 64 1] :by "ProbeView"
                                                       :t 1000 :until 2000}]))))
    (is (fs/existsSync file))
    (is (= (path/join state "worlds" "claude" "plans") (get-in w [:opts :plans-dir])))))
