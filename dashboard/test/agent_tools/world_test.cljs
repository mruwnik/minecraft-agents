(ns agent-tools.world-test
  (:require [cljs.test :refer [deftest is]]
            [agent-tools.world :as world]))

(defn fails-with [pattern argv]
  (let [error (:error (world/request-for argv))]
    (and (string? error) (boolean (re-find pattern error)))))

(deftest builds-a-bounded-move-to-submission-with-a-request-id
  (let [request (world/request-for ["--world" "w" "Probe" "submit" "move-to" "1" "64" "-2" "--timeout-s" "4" "--who" "claude"])]
    (is (nil? (:error request)))
    (is (= :submit (get-in request [:body :op])))
    (is (= :move-to (get-in request [:body :action])))
    (is (re-matches #"[0-9a-f-]{36}" (get-in request [:body :request-id])))
    (is (= {:pos {:x 1 :y 64 :z -2} :timeoutS 4} (get-in request [:body :args])))))

(deftest a-submission-can-be-retried-with-the-same-request-id
  (is (= "retry-1" (get-in (world/request-for ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--request-id" "retry-1"])
                           [:body :request-id]))))

(deftest rejects-options-that-do-not-fit-the-command
  (doseq [[pattern argv] [[#"body name" ["--world" "w" "../etc" "inventory"]]
                          [#"only valid for submit" ["--world" "w" "Probe" "status" "op-1" "--request-id" "ignored"]]
                          [#"not valid for dig" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--range" "-1"]]
                          [#"only valid with submit" ["--world" "w" "Probe" "inventory" "--item" "bread"]]
                          [#"--request-id must be" ["--world" "w" "Probe" "submit" "dig" "1" "64" "2" "--request-id" "bad id"]]
                          [#"--who must be" ["--world" "w" "Probe" "inventory" "--who" ""]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(deftest maps-status-cancel-inventory-and-interactions-to-fixed-operations
  (doseq [[argv op] [[["status" "op-1"] :status] [["cancel" "op-1"] :cancel] [["inventory"] :inventory]]]
    (is (= op (get-in (world/request-for (into ["--world" "w" "Probe"] argv)) [:body :op])) (pr-str argv)))
  (let [place (world/request-for ["--world" "w" "Probe" "submit" "place" "1" "64" "2" "oak_planks"])]
    (is (= :place (get-in place [:body :action])))
    (is (= {:pos {:x 1 :y 64 :z 2} :item "oak_planks"} (get-in place [:body :args]))))
  (is (= 17 (get-in (world/request-for ["--world" "w" "Probe" "submit" "interact" "17"]) [:body :args :id])))
  (is (= {:action :wear :args {:item "iron_helmet"}}
         (select-keys (:body (world/request-for ["--world" "w" "Probe" "submit" "wear" "iron_helmet"])) [:action :args])))
  (is (= {} (get-in (world/request-for ["--world" "w" "Probe" "submit" "wear"]) [:body :args])))
  (is (= {:pos {:x 1 :y 64 :z 2} :item "bread" :face "up"}
         (get-in (world/request-for ["--world" "w" "Probe" "submit" "use-on" "1" "64" "2" "--item" "bread" "--face" "up"])
                 [:body :args]))))

(deftest rejects-malformed-commands
  (doseq [[pattern argv] [[#"unknown action delete-file" ["--world" "w" "Probe" "submit" "delete-file"]]
                          [#"unknown action undefined" ["--world" "w" "Probe" "submit"]]
                          [#"needs x y z" ["--world" "w" "Probe" "submit" "dig" "1" "64"]]
                          [#"needs x y z item" ["--world" "w" "Probe" "submit" "place" "1" "64" "2"]]
                          [#"x must be a finite number" ["--world" "w" "Probe" "submit" "dig" "x" "64" "2"]]
                          [#"request-id" ["--world" "w" "Probe" "status"]]
                          [#"unknown command" ["--world" "w" "Probe" "dance"]]]]
    (is (fails-with pattern argv) (pr-str argv))))

(deftest a-body-is-addressed-in-its-world
  (is (fails-with #"missing --world <world>" ["Probe" "inventory"]))
  (is (fails-with #"world" ["Probe" "inventory" "--world" "../x"]))
  (let [r (world/request-for ["Probe" "inventory" "--world" "w" "--state" "/s"])]
    (is (= ["w" "Probe" "/s"] [(:world r) (:agent r) (:state r)]))))

(deftest the-body-is-written-as-edn-in-the-engine-s-key-order
  (is (= "{:op :submit :who \"claude\" :request-id \"r1\" :action :move-to :args {:pos {:x 1 :y 64 :z -2} :timeoutS 4}}"
         (world/body-edn (:body (world/request-for ["--world" "w" "Probe" "submit" "move-to" "1" "64" "-2" "--timeout-s" "4" "--request-id" "r1"]))))))
