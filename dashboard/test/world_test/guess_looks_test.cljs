(ns world-test.guess-looks-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.guess-looks :as g]))

(defn line [data] (pr-str {:kind :go-to.guess-look :data data}))

(deftest a-guess-look-says-its-class-and-whether-the-guess-held
  (is (= {:class :beside :ok? false} (g/scan-line (line {:class :beside :ok? false}))))
  (is (nil? (g/scan-line (pr-str {:kind :go-to.escalated :data {:class :beside}}))))
  (is (= {:beside {:k 1 :n 2} :apart {:k 0 :n 1}}
         (g/report [{:class :beside :ok? false} {:class :beside :ok? true} {:class :apart :ok? true}]))))
