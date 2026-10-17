(ns agent-tools.map-test
  (:require [cljs.test :refer [deftest is]]
            [agent-tools.map :as map-tool]))

(defn failure [f]
  (try (f) nil (catch :default e (.-message e))))

(deftest zone-error-names-the-bad-keys-and-the-accepted-ones
  (let [message (failure #(map-tool/validate-zone {:name "z" :owner "b" :box [[0 0 0] [1 1 1]]}))]
    (is (re-find #":box" message))
    (is (re-find #":min" message))
    (is (re-find #":max" message))))

(deftest find-center-takes-a-vector-or-an-xyz-map
  (let [center #(:center (map-tool/filters nil {:center %}))]
    (is (= [1 2 3] (center "[1 2 3]")))
    (is (= [1 2 3] (center "{:x 1 :y 2 :z 3}")))
    (is (re-find #"\[x y z\] or \{:x :y :z\}" (failure #(center "[1 2]"))))))

(deftest usage-documents-the-zone-corners
  (is (re-find #":min" map-tool/usage)))

(deftest whose-needs-the-asker-and-takes-no-kind
  (is (re-find #"--by" (failure #(map-tool/options ["whose" "--center" "[1 2 3]" "--world" "w"]))))
  (is (re-find #"map.mjs" (failure #(map-tool/options ["whose" "zone" "--by" "Bot" "--world" "w"])))))
