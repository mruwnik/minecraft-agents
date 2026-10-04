(ns engine.bodies-test
  (:require [cljs.test :refer [deftest is are]]
            [engine.bodies :as bodies]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(deftest body-dir-is-under-its-world
  (is (= (path/join "/s" "worlds" "claude" "agents" "Bob") (bodies/body-dir "/s" "claude" "Bob")))
  (is (not= (bodies/body-dir "/s" "a" "Bob") (bodies/body-dir "/s" "b" "Bob"))))

(deftest body-dir-refuses-a-missing-or-unsafe-part
  (are [world name pattern] (re-find pattern (try (bodies/body-dir "/s" world name) "" (catch :default e (ex-message e))))
    nil "Bob" #"world"
    "" "Bob" #"world"
    "w" nil #"name"
    "a/b" "Bob" #"world"
    "w" ".." #"name"
    ".." "Bob" #"world"))

(deftest account-dir-is-outside-every-world
  (is (= (path/join "/s" "accounts" "Bob") (bodies/account-dir "/s" "Bob")))
  (is (re-find #"name" (try (bodies/account-dir "/s" "../x") "" (catch :default e (ex-message e))))))

(deftest world-of-body-dir-and-missing-world-text
  (is (= "claude" (bodies/world-of-body-dir (bodies/body-dir "/s" "claude" "Bob"))))
  (is (re-find #"--world <world>" (bodies/missing-world-error "--world"))))

(deftest list-bodies-reads-every-world
  (let [dir (tu/tmp-dir)]
    (doseq [[w n] [["w2" "Bob"] ["w1" "Bob"] ["w1" "Ann"]]]
      (fs/mkdirSync (bodies/body-dir dir w n) #js {:recursive true}))
    (fs/mkdirSync (path/join dir "worlds" "empty") #js {:recursive true})
    (fs/writeFileSync (path/join dir "worlds" "w1" "agents" "stray.txt") "")
    (is (= [{:world "w1" :name "Ann" :dir (bodies/body-dir dir "w1" "Ann")}
            {:world "w1" :name "Bob" :dir (bodies/body-dir dir "w1" "Bob")}
            {:world "w2" :name "Bob" :dir (bodies/body-dir dir "w2" "Bob")}]
           (bodies/list-bodies dir)))
    (is (= ["w2"] (mapv :world (bodies/list-bodies dir "w2"))))
    (is (= [] (bodies/list-bodies (path/join dir "nowhere"))))))
