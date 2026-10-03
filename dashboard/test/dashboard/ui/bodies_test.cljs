(ns dashboard.ui.bodies-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.bodies :as bodies]))

(defn button-of [hiccup] (second (nth hiccup 2)))

(deftest show-on-map-is-enabled-with-a-position
  (let [props (button-of (bodies/show-on-map "Ann" {:x 1 :z 2} "claude"))]
    (is (false? (:disabled props)))
    (is (= "show Ann on the map" (:title props)))))

(deftest show-on-map-is-disabled-with-a-reason-without-a-position
  (let [props (button-of (bodies/show-on-map "Ann" nil "claude"))]
    (is (true? (:disabled props)))
    (is (= "no known position yet" (:title props)))))
