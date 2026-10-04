(ns plan.drop-status-test
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest are is]]
            [plan.drop-status :as drop-status]
            [plan.drop-status-cli :as cli]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]))

(def part {:id "a" :cells [[0 64 0]] :want "stone"})

(deftest retired-plans-are-deleted-and-the-rest-lose-the-key
  (let [plans {"live" {:id "live" :status :active :parts [part]}
               "idea" {:id "idea" :status :proposed :parts [part]}
               "done" {:id "done" :status :completed :parts [part]}
               "old" {:id "old" :status :retired :parts [part]}
               "bare" {:id "bare" :parts [part]}}]
    (is (= {:plans {"live" {:id "live" :parts [part]}
                    "idea" {:id "idea" :parts [part]}
                    "done" {:id "done" :parts [part]}
                    "bare" {:id "bare" :parts [part]}}
            :deleted ["old"]
            :stripped ["done" "idea" "live"]}
           (drop-status/migrate plans)))))

(deftest nothing-to-do-for-plans-without-status
  (is (= {:plans {"p" {:id "p" :parts []}} :deleted [] :stripped []}
         (drop-status/migrate {"p" {:id "p" :parts []}}))))

(deftest the-text-loses-only-the-status-pair
  (are [text expected] (= expected (drop-status/strip-status-text text))
    "{:id \"p\" :status :active\n :parts []}" "{:id \"p\"\n :parts []}"
    "{:id \"p\", :status :proposed, :note \"n\"}" "{:id \"p\", :note \"n\"}"
    "{:id \"p\" :parts [] :status :completed}" "{:id \"p\" :parts []}"
    "{:id \"p\" :parts []}" "{:id \"p\" :parts []}"
    "{:status :active :id \"p\"}" "{:id \"p\"}"
    "{:id \"p\" :metadata {:status :nested}}" "{:id \"p\" :metadata {:status :nested}}"
    ";; keep :status in comments?\n{:id \"p\" :status :active :note \":status :active\"}"
    ";; keep :status in comments?\n{:id \"p\" :note \":status :active\"}"))

(deftest the-stripped-text-reads-as-the-plan-without-the-key
  (let [text "{:id \"p\" :status :active\n :note \"x\" :parts [{:id \"a\" :cells [[0 64 0]] :want \"stone\"}]}"]
    (is (= (dissoc (reader/read-string text) :status)
           (reader/read-string (drop-status/strip-status-text text))))))

(defn temp-dir-with [files]
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "drop-status-"))]
    (doseq [[name text] files] (fs/writeFileSync (path/join dir name) text))
    dir))

(defn dir-texts [dir]
  (into {} (map (fn [f] [f (fs/readFileSync (path/join dir f) "utf8")])) (cli/edn-files dir)))

(def migrate-files
  {"live.edn" "{:id \"live\" :status :active\n :parts []}"
   "old.edn" "{:id \"old\" :status :retired :parts []}"
   "bare.edn" "{:id \"bare\" :parts []}"
   "broken.edn" "{:id"})

(deftest migrate-dir-deletes-retired-strips-the-rest-and-a-rerun-changes-nothing
  (let [dir (temp-dir-with migrate-files)
        first-run (cli/migrate-dir! dir false)
        after (dir-texts dir)
        second-run (cli/migrate-dir! dir false)]
    (is (= {:deleted ["old"] :stripped ["live"] :skipped ["broken.edn"]} first-run))
    (is (= {"live.edn" "{:id \"live\"\n :parts []}"
            "bare.edn" "{:id \"bare\" :parts []}"
            "broken.edn" "{:id"}
           after))
    (is (= {:deleted [] :stripped [] :skipped ["broken.edn"]} second-run))
    (is (= after (dir-texts dir)))))

(deftest migrate-dir-dry-run-touches-nothing
  (let [dir (temp-dir-with migrate-files)]
    (is (= {:deleted ["old"] :stripped ["live"] :skipped ["broken.edn"]} (cli/migrate-dir! dir true)))
    (is (= migrate-files (dir-texts dir)))))

(deftest migrate-dir-checks-every-file-before-it-deletes-or-writes
  (let [files {"live.edn" "{:id \"live\" :status :active :parts []}"
               "old.edn" "{:id \"old\" :status :retired :parts []}"
               "odd.edn" "{:id \"odd\" :status \"text\" :parts []}"}
        dir (temp-dir-with files)]
    (is (thrown? js/Error (cli/migrate-dir! dir false)))
    (is (= files (dir-texts dir)))))
