(ns engine.smoke-test
  (:require [cljs.test :refer [deftest is async]]))

(defn ^:async twice [x]
  (* 2 (await (js/Promise.resolve x))))

(deftest async-fn-works
  (async done
    (-> (twice 21)
        (.then (fn [v] (is (= 42 v))))
        (.finally done))))
