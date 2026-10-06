(ns world-test.runner-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.runner :as r]))

(deftest the-body-launch-argv-caps-new-space
  (let [argv (vec (r/body-argv {:body "B" :world "w"} "/tmp/s.edn"))]
    (is (some #{"--max-semi-space-size=4"} argv))
    (is (< (.indexOf argv "--max-semi-space-size=4") (.indexOf argv "out/body.cjs")))
    (is (= ["--agent" "B" "--world" "w" "--scenario" "/tmp/s.edn" "--fresh"]
           (vec (drop (inc (.indexOf argv "out/body.cjs")) argv))))))
