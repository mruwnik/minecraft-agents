(ns agent-tools.drive-test
  (:require [cljs.test :refer [deftest is]]
            [agent-tools.drive :as drive]
            [agent-tools.map :as map-tool]
            ["node:path" :as path]))

(defn post [body] {:method "POST" :path "/drive" :body body})

(deftest request-for-builds-the-fixed-drive-requests
  (doseq [[argv expected]
          [[["Bob" "take" "--why" "poking"] (post {:op "take" :who "claude" :why "poking"})]
           [["Bob" "take"] (post {:op "take" :who "claude" :why ""})]
           [["Bob" "take" "--idle-s" "120" "--why" "x"] (post {:op "take" :who "claude" :why "x" :idleS 120})]
           [["Bob" "hold" "forward" "500" "--who" "view"] (post {:op "set" :who "view" :controls {:forward true} :ms 500})]
           [["Bob" "hold" "forward,sprint" "1000"] (post {:op "set" :who "claude" :controls {:forward true :sprint true} :ms 1000})]
           [["Bob" "look" "180" "-10"] (post {:op "set" :who "claude" :look {:yaw 180 :pitch -10}})]
           [["Bob" "turn" "15"] (post {:op "set" :who "claude" :look {:dyaw 15 :dpitch 0}})]
           [["Bob" "turn" "-15" "5"] (post {:op "set" :who "claude" :look {:dyaw -15 :dpitch 5}})]
           [["Bob" "jump"] (post {:op "set" :who "claude" :controls {:jump true} :ms 300})]
           [["Bob" "stop"] (post {:op "stop" :who "claude"})]
           [["Bob" "ping"] (post {:op "ping" :who "claude"})]
           [["Bob" "release"] (post {:op "release" :who "claude"})]
           [["Bob" "release" "--force"] (post {:op "release" :who "claude" :force true})]
           [["Bob" "state"] {:method "GET" :path "/drive" :body nil}]]]
    (let [r (drive/request-for (into argv ["--world" "w"]))]
      (is (nil? (:error r)) (pr-str argv))
      (is (= "Bob" (:agent r)))
      (is (= expected (select-keys r [:method :path :body])) (pr-str argv)))))

(deftest request-for-rejects-bad-usage
  (doseq [argv [[] ["Bob"] ["Bob" "dance"] ["Bob" "hold"] ["Bob" "hold" "forward"] ["Bob" "hold" "forward" "abc"]
                ["Bob" "hold" "forward" "1.5"] ["Bob" "hold" "fly" "100"] ["Bob" "look" "10"] ["Bob" "look" "a" "b"]
                ["Bob" "turn"] ["Bob" "turn" "x"] ["Bob" "take" "--bogus"]]]
    (is (string? (:error (drive/request-for (into argv ["--world" "w"])))) (pr-str argv))))

(deftest request-for-addresses-a-body-in-its-world
  (is (= "w2" (:world (drive/request-for ["Bob" "state" "--world" "w2"]))))
  (is (re-find #"missing --world <world>" (:error (drive/request-for ["Bob" "state"]))))
  (is (re-find #"world" (:error (drive/request-for ["Bob" "state" "--world" "../x"]))))
  (is (= "/x/y" (:state (drive/request-for ["Bob" "state" "--world" "w" "--state" "/x/y"]))))
  (is (= {:worldsDir (.join path map-tool/default-state-dir "worlds")}
         (:state (drive/request-for ["Bob" "state" "--world" "w"])))))

(deftest request-for-names-a-bad-idle-seconds
  (doseq [argv [["Bob" "take" "--idle-s" "abc"] ["Bob" "take" "--idle-s" ""]]]
    (is (re-find #"idle-s" (:error (drive/request-for (into argv ["--world" "w"])))) (pr-str argv))))

(deftest socket-path-for-builds-the-control-socket-path
  (is (= "/s/worlds/w/agents/Bob/engine/control.sock" (drive/socket-path-for {:state "/s" :world "w" :agent "Bob"}))))

(deftest exit-code-is-zero-only-for-an-ok-2xx-reply
  (doseq [[reply code] [[{:status 200 :json {:ok true}} 0]
                        [{:status 200 :json {:ok false :reason "offline"}} 1]
                        [{:status 400 :json {:ok false}} 1]
                        [{:status 404 :json {}} 1]
                        [{:status 200 :json {}} 1]
                        [{:status 200 :json nil} 1]]]
    (is (= code (drive/exit-code-for reply)) (pr-str reply))))
