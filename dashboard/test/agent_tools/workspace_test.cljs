(ns agent-tools.workspace-test
  (:require [cljs.test :refer [deftest is are]]
            [clojure.string :as str]
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

(deftest world-changes-defaults-to-the-body-as-observer
  (are [argv expected] (= expected (some #{"--observer" "--cursor"} (workspace/route ctx "world-changes" argv)))
    [] "--observer"
    ["--wait"] "--observer"
    ["--observer" "mine"] "--observer"
    ["--cursor" "{:seq 1}"] "--cursor")
  (is (= ["--world" "w" "--worlds" "/x" "--repo-root" "/r" "--observer" "Probe" "--wait"]
         (workspace/route ctx "world-changes" ["--wait"])))
  (is (not-any? #{"--observer"} (workspace/route ctx "world-changes" ["--cursor" "{:seq 1}"]))))

(deftest player-edn-cuts-plumbing-from-usage-and-message
  (let [text (str "{:ok false :reason :bad-args :message \"bad\" "
                  ":usage \"usage: observe.mjs <agent> --world <world> [--worlds <dir>] x\"}")
        out (workspace/player-edn text)]
    (is (not (re-find #"<agent>|--world|--worlds|\.mjs" out)))
    (is (re-find #"usage: \./bin/observe" out))
    (is (re-find #":reason :bad-args" out))))

(deftest player-edn-keeps-later-forms
  (let [text (str "{:ok false :reason :bad-args :usage \"usage: observe.mjs <agent> x\"}\n"
                  "{:ok true :n 1}\n")
        out (workspace/player-edn text)]
    (is (not (re-find #"<agent>|\.mjs" out)))
    (is (re-find #"usage: \./bin/observe" out))
    (is (str/ends-with? out "{:ok true :n 1}\n"))))

(deftest player-edn-leaves-other-output-alone
  (are [text] (= text (workspace/player-edn text))
    "{:ok true :message \"fine\"}"
    "not edn at all {"
    "{:ok false :reason :x :message \"nothing to cut\"}"))
