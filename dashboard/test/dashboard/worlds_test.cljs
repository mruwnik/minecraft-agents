(ns dashboard.worlds-test
  (:require [cljs.test :refer [deftest is testing]]
            [dashboard.worlds :as worlds]))

(deftest parse-world-list-sorted-with-host-and-port
  (is (= [{:name "broken" :host nil :port nil}
          {:name "claude" :host "localhost" :port 25565}
          {:name "empty" :host nil :port nil}
          {:name "test" :host "h2" :port 25566}]
         (worlds/parse-world-list
          [{:name "test" :text "{\"host\":\"h2\",\"port\":25566}"}
           {:name "claude" :text "{\"host\":\"localhost\",\"port\":25565}"}
           {:name "broken" :text "{nope"}
           {:name "empty" :text ""}]))))

(deftest resolve-world-cases
  (doseq [[why names requested expected]
          [["nil picks the first" ["claude" "test"] nil "claude"]
           ["empty string picks the first" ["claude" "test"] "" "claude"]
           ["no worlds and nothing asked is nil" [] nil nil]
           ["an exact name" ["claude" "test"] "test" "test"]
           ["a case difference is invalid" ["claude"] "Claude" :unknown]
           ["a traversal is invalid" ["claude"] "../x" :unknown]
           ["a sibling directory is invalid" ["claude"] "../agents" :unknown]
           ["a slash is invalid" ["claude"] "a/b" :unknown]
           ["a nested traversal is invalid" ["claude"] "claude/../claude" :unknown]
           ["a name when no worlds exist is invalid" [] "claude" :unknown]
           ["an absolute path is invalid" ["claude"] "/etc/passwd" :unknown]]]
    (testing why
      (is (= expected (worlds/resolve-world names requested))))))

(deftest world-choice-cases
  (doseq [[why names requested expected]
          [["valid" ["claude" "test"] "test" {:name "test"}]
           ["default" ["claude" "test"] nil {:name "claude"}]
           ["no worlds" [] nil {:name nil}]
           ["invalid" ["claude"] "nope" {:error "no world called nope" :worlds ["claude"]}]]]
    (testing why
      (is (= expected (worlds/world-choice names requested))))))

(deftest scope-snapshot-keeps-only-the-chosen-world
  (let [snapshot {:at 5
                  :agents ["Claude" "Claude2" "Chani" "Chani2"]
                  :bodies [{:name "Claude" :username "Claude2" :world "main"}
                           {:name "Chani" :username "Chani2" :world "test"}]
                  :worlds [{:name "main" :bodies []} {:name "test" :bodies []}]
                  :villageError nil}]
    (is (= {:at 5
            :agents ["Chani" "Chani2"]
            :bodies [(second (:bodies snapshot))]
            :worlds [(second (:worlds snapshot))]
            :villageError nil}
           (worlds/scope-snapshot snapshot "test")))))

(deftest agent-in-world-cases
  (let [agents [{:name "Claude" :world "main"} {:name "Free" :world nil}]]
    (doseq [[why name world expected]
            [["no world asked keeps the agent" "Claude" nil true]
             ["the agent's world" "Claude" "main" true]
             ["another world" "Claude" "test" false]
             ["an unknown agent" "Nobody" "main" false]
             ["an agent with no world" "Free" "main" false]]]
      (testing why
        (is (= expected (worlds/agent-in-world? agents name world)))))))
