(ns agent-tools.map-test
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            [agent-tools.map :as map-tool]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]))

(defn world-args
  "Argv tail naming an empty temp world; the dir is left to the OS tmp cleaner."
  []
  (let [dir (.mkdtempSync fs (.join path (os/tmpdir) "map-test-"))]
    (.mkdirSync fs (.join path dir "worlds" "test") #js {:recursive true})
    (.writeFileSync fs (.join path dir "worlds" "test" "world.json") "{}")
    ["--world" "test" "--state" dir "--repo-root" dir]))

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

(deftest find-takes-the-kind-alone-as-the-documented-usage-says
  (is (= "zone" (get-in (map-tool/options (into ["find" "zone"] (world-args))) [:values :type])))
  (is (re-find #"not both" (failure #(map-tool/options (into ["find" "zone" "--type" "claim"] (world-args))))))
  (is (re-find #"map.mjs" (failure #(map-tool/options (into ["find" "door"] (world-args)))))))

(deftest usage-says-the-zone-owner-must-equal-by
  (is (re-find #"must equal --by" map-tool/usage)))

(deftest remove-without-a-revision-asks-for-one
  (is (re-find #"--revision" (failure #(map-tool/options (into ["remove" "zone" "nope" "--by" "Bot"] (world-args)))))))

(deftest box-gives-a-zones-corners-in-order
  (is (= {:min [1 2 3] :max [4 5 6]} (map-tool/box-corners "4,2,3:1,5,6")))
  (is (re-find #"--box needs" (failure #(map-tool/box-corners "1,2:3,4"))))
  (is (= "9,9,9" (str/join "," (:max (map-tool/box-corners "9,9,9:0,0,0"))))))
