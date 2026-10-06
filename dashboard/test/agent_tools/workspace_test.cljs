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
      "world" (get-in (world/request-for routed) [:request :by]))))

(deftest drive-and-world-share-the-body-identity-by-default
  (are [command argv] (= "Probe" (who-of command argv))
    "drive" ["take" "--why" "x" "--idle-s" "30"]
    "drive" ["release"]
    "world" ["submit" "dig" "1" "2" "3"]))

(deftest an-explicit-who-is-kept
  (are [command argv] (= "Other" (who-of command argv))
    "drive" ["take" "--who" "Other"]
    "world" ["submit" "dig" "1" "2" "3" "--who" "Other"]))

(deftest plans-check-gets-the-workspace-body
  (is (= ["--world" "w" "--worlds" "/x" "--repo" "/r" "--body" "Probe" "check" "p1"]
         (workspace/route ctx "plans" ["check" "p1"])))
  (is (= ["--world" "w" "--worlds" "/x" "--repo" "/r" "list"]
         (workspace/route ctx "plans" ["list"]))))

(deftest plans-check-cannot-read-another-body
  (is (thrown? js/Error (workspace/route ctx "plans" ["check" "p1" "--body" "Other"]))))

(deftest player-help-hides-plans-check-body
  (is (not (re-find #"--body" (workspace/player-usage "  check <id> --body <name> [--inventory X]")))))

(deftest player-error-cuts-only-usage-lines
  (is (= "failed at /repo/engine/tools/foo.mjs:12\n    at x (/repo/engine/tools/bar.mjs:3:4)"
         (workspace/player-error "failed at /repo/engine/tools/foo.mjs:12\n    at x (/repo/engine/tools/bar.mjs:3:4)")))
  (is (= "usage: ./bin/check <id>\nbad /repo/a.mjs"
         (workspace/player-error "usage: check.mjs <id> --body <name>\nbad /repo/a.mjs"))))
