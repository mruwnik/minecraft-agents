(ns dashboard.rcon-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.rcon :as rcon]))

(deftest encode-packet-layout-is-little-endian-length-id-type-body-two-zeros
  (is (= [12 0 0 0 7 0 0 0 2 0 0 0 104 105 0 0]
         (vec (rcon/encode-packet 7 2 "hi")))))

(deftest decode-packet-round-trips-and-reports-bytes-used
  (let [body "Added Aviendha to the whitelist"
        buf (js/Buffer.concat #js [(rcon/encode-packet 3 0 body) (js/Buffer.from #js [9 9])])]
    (is (= {:id 3 :type 0 :body body :size (- (.-length buf) 2)}
           (rcon/decode-packet buf)))))

(deftest decode-packet-incomplete-data-is-nil
  (is (nil? (rcon/decode-packet (.subarray (rcon/encode-packet 1 2 "list") 0 9))))
  (is (nil? (rcon/decode-packet (js/Buffer.alloc 3)))))
