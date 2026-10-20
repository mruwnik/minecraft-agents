(ns engine.edn-fast-test
  (:require [cljs.test :refer [deftest is testing]]
            [cljs.reader :as reader]
            [engine.edn-fast :as fast]
            [engine.events :as events]
            [engine.test-util :as tu]
            ["path" :as path]
            ["fs" :as fs]))

(def readable
  ["{:seq 1, :kind :done, :data {:pos {:x 20004.5, :y -150, :z 2e3}, :ok? true, :none nil, :v [1 [2 :a/b] \"s p\"]}}"
   "{}" "[]" "{:a #{:x :y}, :b #{}}" "{:a \"\"}" "{:a \"x\\\"y\\\\z\\n\"}" "  {:a   1 , :b -0.5}  " "{:ns/k :ns/v}"])

(def unsupported
  ["{:a \"\\u0041\"}" "{:a foo}" "{:a #inst \"2020-01-01\"}" "{:a \\c}" "{:a ##NaN}" "{:a (1 2)}" "{:a 1N}" "{:a" "{:a 1} extra" "{:a}"])

(deftest it-reads-what-the-reader-reads
  (doseq [text readable]
    (is (= (reader/read-string text) (fast/read-string text)) text)))

(deftest it-throws-on-what-it-does-not-cover
  (doseq [text unsupported]
    (is (thrown? js/Error (fast/read-string text)) text)))

(deftest records-the-stream-wrote-read-back-equal
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})]
    (doseq [i (range 50)]
      (events/emit! stream {:source :job :kind :done :level :info :job "j" :chain ["a" "b"] :round i :cause (if (even? i) "lava" i)
                            :data {:i i :s "text, with {braces} and :colons" :f (* i 0.25) :nested {:k [i nil true]}}}))
    (doseq [line (remove empty? (.split (fs/readFileSync file "utf8") "\n"))]
      (is (= (reader/read-string line) (fast/read-string line))))))
