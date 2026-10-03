(ns engine.smoke-test
  (:require [cljs.test :refer [deftest is async]]))

(def a (fn ^:async named [] (await (js/Promise.resolve 1))))

(deftest anonymous-async-forms
  (async done
    (-> (js/Promise.all #js [(a)])
        (.then (fn [v] (is (= [1] (vec v)))))
        (.catch (fn [e] (is (nil? e) (str e))))
        (.finally done))))
