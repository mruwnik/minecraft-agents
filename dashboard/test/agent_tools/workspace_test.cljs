(ns agent-tools.workspace-test
  (:require [cljs.test :refer [deftest is are]]
            [agent-tools.drive :as drive]
            [agent-tools.world :as world]
            [agent-tools.workspace :as workspace]))

(def ctx {:body "Probe" :world "w" :worlds "/x" :repo "/r"})

(defn who-of [command argv]
  (let [routed (workspace/route ctx command argv)]
    (case command
      "drive" (get-in (drive/request-for routed) [:body :who])
      "world" (get-in (world/request-for routed) [:who]))))

(deftest drive-and-world-share-the-body-identity-by-default
  (are [command argv] (= "Probe" (who-of command argv))
    "drive" ["take" "--why" "x" "--idle-s" "30"]
    "drive" ["release"]
    "world" ["inventory"]
    "world" ["submit" "dig" "1" "2" "3"]))

(deftest an-explicit-who-is-kept
  (are [command argv] (= "Other" (who-of command argv))
    "drive" ["take" "--who" "Other"]
    "world" ["inventory" "--who" "Other"]))
