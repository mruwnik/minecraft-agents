(ns dashboard.restart-test
  (:require [cljs.test :refer [deftest are]]
            [dashboard.restart :as restart]))

(deftest loopback-address
  (are [addr expected] (= expected (restart/loopback-address? addr))
    "127.0.0.1" true
    "127.1.2.3" true
    "::1" true
    "::ffff:127.0.0.1" true
    "::ffff:127.9.9.9" true
    "192.168.1.5" false
    "10.0.0.1" false
    "::ffff:10.0.0.1" false
    "::2" false
    "127.0.0.1.evil.com" false
    "" false
    nil false))
