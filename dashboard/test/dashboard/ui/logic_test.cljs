(ns dashboard.ui.logic-test
  (:require [cljs.test :refer [deftest is testing]]
            [dashboard.ui.logic :as l]))

(deftest time-ago-text
  (doseq [[ms expected] [[0 "0s ago"] [-5 "0s ago"] [8292 "8s ago"] [59999 "59s ago"] [60000 "1m ago"]
                         [3599000 "59m ago"] [3600000 "1h ago"] [90000000 "1d ago"] [nil "never"]]]
    (testing (str ms)
      (is (= expected (l/time-ago-text ms))))))

(def up-b {:name "Zed" :up true :engine {:up true}})
(def up-a {:name "Amy" :up true :engine {:up true}})
(def down-b {:name "Bob" :up false :engine {:up false :age-ms 5000}})
(def foreign {:name "Old" :up false :engine nil :error "not an engine body (unsupported)"})

(deftest engine-body?
  (doseq [[body expected] [[up-a true] [down-b true] [foreign false]]]
    (is (= expected (l/engine-body? body)))))

(deftest sort-bodies-up-first
  (is (= ["Amy" "Zed" "Bob"] (map :name (l/sort-bodies [down-b up-b up-a])))))

(deftest split-bodies
  (is (= {:engine [up-a up-b down-b] :foreign [foreign]}
         (l/split-bodies [foreign down-b up-b up-a]))))

(deftest world-from-search
  (doseq [[search expected] [["?world=claude" "claude"] ["?a=1&world=a%20b" "a b"] ["" nil] ["?world=" nil]]]
    (is (= expected (l/world-from-search search)))))

(deftest with-world
  (doseq [[path world expected] [["/villages" "claude" "/villages?world=claude"]
                                 ["/villages" nil "/villages"]
                                 ["/" "a b" "/?world=a%20b"]]]
    (is (= expected (l/with-world path world)))))

(deftest api-url
  (doseq [[path world params expected]
          [["/api/state" "claude" {} "/api/state?world=claude"]
           ["/api/chat" "claude" {:limit 300} "/api/chat?world=claude&limit=300"]
           ["/api/worlds" nil {} "/api/worlds"]]]
    (is (= expected (l/api-url path world params)))))

(deftest page-for-path
  (doseq [[path expected] [["/" :bodies] ["/map" :map] ["/map/" :map] ["/plans" :plans] ["/jobs" :jobs] ["/villages" :villages]
                           ["/villagers" :villagers] ["/blueprints" :blueprints] ["/villages/" :villages] ["/zzz" :bodies]]]
    (is (= expected (l/page-for-path path)))))

(def msgs [{:t 1 :from "Steve" :to nil :message "hello there"}
           {:t 2 :from "Amy" :to "Zed" :message "psst"}
           {:t 3 :from "Zed" :to nil :message "STEVE go"}])

(deftest filter-chat
  (doseq [[needle hide expected] [["" false [1 2 3]] [nil false [1 2 3]] ["steve" false [1 3]] ["ZED" false [2 3]]
                                  ["" true [1 3]] ["psst" true []] ["hello" true [1]]]]
    (testing (str needle hide)
      (is (= expected (map :t (l/filter-chat msgs needle hide)))))))

(deftest zoom-view
  (let [view {:scale 2 :origin-x 0 :origin-z 0}
        z (l/zoom-view view 2 100 50)]
    (is (= 4 (:scale z)))
    (is (= {:x 50 :z 25} {:x (+ (:origin-x z) (/ 100 4)) :z (+ (:origin-z z) (/ 50 4))})
        "the world point under the cursor stays under the cursor")
    (is (= 25 (:origin-x z)))))

(deftest zoom-view-clamps
  (doseq [[factor expected] [[1000000 200] [0.0000001 0.05]]]
    (is (= expected (:scale (l/zoom-view {:scale 2 :origin-x 0 :origin-z 0} factor 0 0))))))

(deftest pan-view
  (is (= {:scale 2 :origin-x -5 :origin-z 10} (l/pan-view {:scale 2 :origin-x 0 :origin-z 0} 10 -20))))

(deftest center-view
  (is (= {:scale 2 :origin-x 70 :origin-z -30}
         (l/center-view {:scale 2 :origin-x 0 :origin-z 0} 100 -20 120 40))))

(deftest clock-text
  (doseq [[clock expected] [[{:day 3 :timeOfDay 0} "day 3 06:00 (day)"]
                            [{:day 3 :time-of-day 18000} "day 3 00:00 (night)"]
                            [{:day 1 :timeOfDay 13000} "day 1 19:00 (night)"]
                            [{:day true :timeOfDay 7277} "13:16 (day)"]
                            [{:day false :timeOfDay 100} "06:06 (night)"]
                            [nil "no clock"]]]
    (is (= expected (l/clock-text clock)))))

(deftest pick-nearest
  (let [items [{:id :a :px 10 :py 10} {:id :b :px 14 :py 10} {:id :c :px 100 :py 100}]]
    (doseq [[x y r expected] [[13 10 8 :b] [10 10 8 :a] [50 50 8 nil] [100 104 8 :c]]]
      (is (= expected (:id (l/pick-nearest items x y r)))))))

(deftest cooldown-text
  (doseq [[reflex expected] [[{:cooldown-s 30 :cooling? true} "cooling (30s)"] [{:cooldown-s 30 :cooling? false} "ready (30s)"]
                             [{:cooling? true} "cooling"] [{} "ready"]]]
    (is (= expected (l/cooldown-text reflex)))))

(deftest clock-ms-text
  (is (= "03:04:05" (l/clock-ms-text (.getTime (js/Date. 2020 0 1 3 4 5)))))
  (is (= "x" (l/clock-ms-text "x"))))


(deftest with-body-cases
  (doseq [[pathname search body expected]
          [["/" "" "Bob" "/?body=Bob"]
           ["/" "?world=w1" "Bob" "/?world=w1&body=Bob"]
           ["/" "?world=w1&body=Al" "Bob" "/?world=w1&body=Bob"]
           ["/" "?world=w1&body=Bob" nil "/?world=w1"]
           ["/map" "?body=Bob" nil "/map"]]]
    (is (= expected (l/with-body pathname search body)))))

(deftest body-from-search-cases
  (doseq [[search expected] [["" nil] ["?body=" nil] ["?body=Bob" "Bob"] ["?world=x&body=Probe_1" "Probe_1"]]]
    (is (= expected (l/body-from-search search)))))
