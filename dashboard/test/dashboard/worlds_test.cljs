(ns dashboard.worlds-test
  (:require [cljs.test :refer [deftest is testing]]
            [cljs.reader :as reader]
            [dashboard.worlds :as worlds]))

(deftest parse-world-list-sorted-with-host-and-port
  (is (= [{:name "broken" :host nil :port nil}
          {:name "empty" :host nil :port nil}
          {:name "main" :host "localhost" :port 25565}
          {:name "test" :host "h2" :port 25566}]
         (worlds/parse-world-list
          [{:name "test" :text "{\"host\":\"h2\",\"port\":25566}"}
           {:name "main" :text "{\"host\":\"localhost\",\"port\":25565}"}
           {:name "broken" :text "{nope"}
           {:name "empty" :text ""}]))))

(deftest resolve-world-cases
  (doseq [[why names requested expected]
          [["nil picks the first" ["main" "test"] nil "main"]
           ["empty string picks the first" ["main" "test"] "" "main"]
           ["no worlds and nothing asked is nil" [] nil nil]
           ["an exact name" ["main" "test"] "test" "test"]
           ["a case difference is invalid" ["main"] "Main" :unknown]
           ["a traversal is invalid" ["main"] "../x" :unknown]
           ["a sibling directory is invalid" ["main"] "../agents" :unknown]
           ["a slash is invalid" ["main"] "a/b" :unknown]
           ["a nested traversal is invalid" ["main"] "main/../main" :unknown]
           ["a name when no worlds exist is invalid" [] "main" :unknown]
           ["an absolute path is invalid" ["main"] "/etc/passwd" :unknown]]]
    (testing why
      (is (= expected (worlds/resolve-world names requested))))))

(deftest world-choice-cases
  (doseq [[why names requested expected]
          [["valid" ["main" "test"] "test" {:name "test"}]
           ["default" ["main" "test"] nil {:name "main"}]
           ["no worlds" [] nil {:name nil}]
           ["invalid" ["main"] "nope" {:error "no world called nope" :worlds ["main"]}]]]
    (testing why
      (is (= expected (worlds/world-choice names requested))))))

(deftest scope-snapshot-keeps-only-the-chosen-world
  (let [snapshot {:at 5
                  :agents ["Claude" "Claude2" "Chani" "Chani2"]
                  :bodies [{:name "Claude" :username "Claude2" :world "main"}
                           {:name "Chani" :username "Chani2" :world "test"}]
                  :worlds [{:name "main" :bodies []} {:name "test" :bodies []}]
                  :villageError nil}]
    (is (= {:at 5
            :agents ["Chani" "Chani2"]
            :bodies [(second (:bodies snapshot))]
            :worlds [(second (:worlds snapshot))]
            :villageError nil}
           (worlds/scope-snapshot snapshot "test")))))

(def tilde-place {:name "hut" :structure {:legend {(keyword "~") "water" (keyword "#") "wall" :w "wood"} :rows ["#~w"]}})

(deftest readable-places-stringifies-legend-keys
  (is (= [{:name "hut" :structure {:legend {"~" "water" "#" "wall" "w" "wood"} :rows ["#~w"]}}]
         (worlds/readable-places [tilde-place]))))

(deftest readable-places-leaves-other-places-alone
  (let [places [{:name "a"}
                {:name "b" :structure {:rows ["x"]}}
                {:name "c" :structure {:legend "text"}}
                {:name "d" :structure nil}]]
    (is (= places (worlds/readable-places places)))))

(deftest readable-places-survive-the-edn-reader
  (is (thrown? js/Error (reader/read-string (pr-str [tilde-place]))))
  (is (= [{:name "hut" :structure {:legend {"~" "water" "#" "wall" "w" "wood"} :rows ["#~w"]}}]
         (reader/read-string (pr-str (worlds/readable-places [tilde-place]))))))
