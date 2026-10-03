(ns dashboard.ui.driving-fx-test
  (:require [cljs.test :refer [deftest are is]]
            [dashboard.ui.driving-fx :as fx]))

(deftest drive-url-encodes-the-name
  (are [name url] (= url (fx/drive-url name))
    "Bob" "/drive/Bob"
    "a b/c" "/drive/a%20b%2Fc"))

(deftest post-init-cases
  (are [keepalive? expected-keepalive] (= expected-keepalive (.-keepalive (fx/post-init {:op "stop" :who "w"} keepalive?)))
    true true
    false false)
  (let [init (fx/post-init {:op "set" :who "w" :controls {:forward true}} false)]
    (is (= "POST" (.-method init)))
    (is (= "{\"op\":\"set\",\"who\":\"w\",\"controls\":{\"forward\":true}}" (.-body init)))))
