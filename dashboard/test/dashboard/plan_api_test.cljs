(ns dashboard.plan-api-test
  (:require ["path" :as path]
            [cljs.test :refer [deftest are is]]
            [dashboard.plan-api :as api]))

(def dir (.resolve path js/__dirname ".." "test" "fixtures" "plans"))
(def blueprint-dir (.resolve path js/__dirname ".." "test" "fixtures" "blueprints"))

(defn opts [block-at] {:dir dir :blueprint-dir blueprint-dir :block-at block-at})

(def wheat-everywhere (fn [_ _ _] #js {:name "wheat" :state #js {}}))
(def nothing-dumped (fn [_ _ _] nil))
(deftest the-world-lookup-keeps-the-block-state
  (let [lookup (api/world-blocks (fn [x _ _] (when (zero? x) #js {:name "oak_door" :state #js {:facing "east" :half "lower"}})))]
    (is (= {:name "oak_door" :state {"facing" "east" "half" "lower"}} (lookup [0 64 0])))
    (is (nil? (lookup [1 64 0])))))

(defn door-only [facing]
  (fn [x y z] (when (= [x y z] [101 65 204]) #js {:name "oak_door" :state #js {:facing facing :half "lower"}})))

(deftest a-door-is-judged-by-its-dumped-state
  (are [facing counts] (= counts (select-keys (get-in (api/detail (opts (door-only facing)) "claude-village") [:counts])
                                              [:match :wrong :unknown]))
    "east" {:match 1 :wrong 0 :unknown 218}      ; the hut is turned 90, so its north door faces east
    "north" {:match 0 :wrong 1 :unknown 218}))

(defn by-id [items id] (first (filter #(= id (:id %)) items)))

(deftest summaries-list-every-plan-with-totals
  (let [{:keys [plans errors]} (api/summaries (opts nothing-dumped))
        farm (by-id plans "jizo-farm")]
    (is (= [] errors))
    (is (= ["claude-village" "jizo-farm" "spawn-clear"] (map :id plans)))
    (are [path expected] (= expected (get-in farm path))
      [:name] "jizo-farm"
      [:status] :active
      [:children] []
      [:counts :total] 307
      [:counts :match] 0
      [:counts :unknown] 307
      [:percent] 0
      [:region] {:min [-17 62 -97] :max [14 63 -71]}
      [:elements 0 :id] "potatoes"
      [:elements 0 :kind] :box
      [:elements 0 :bounds] {:min [-11 63 -97] :max [-10 63 -96]})))   ; the farmland below is not part of it

(deftest a-village-places-its-blueprints
  (let [village (by-id (:plans (api/summaries (opts nothing-dumped))) "claude-village")]
    (are [path expected] (= expected (get-in village path))
      [:counts :total] 219                               ; two huts of 100 cells, 17 of path, a lectern, a composter
      [:region] {:min [95 64 200] :max [105 67 216]}
      [:elements 0 :bounds] {:min [101 64 202] :max [105 67 206]}
      [:elements 1 :bounds] {:min [95 64 208] :max [99 67 212]})))

(deftest detail-has-elements-layers-and-errors
  (let [d (api/detail (opts wheat-everywhere) "jizo-farm")]
    (are [path expected] (= expected (get-in d path))
      [:id] "jizo-farm"
      [:elements 0 :content] "crop potatoes"
      [:elements 0 :counts :wrong] 4
      [:elements 8 :id] "wheat-a"
      [:elements 8 :counts :percent] 100
      [:errors] []
      [:grid :min-x] -17
      [:grid :cols] 32
      [:layers 0 :y] 62
      [:layers 1 :y] 63)))

(deftest detail-of-a-village-turns-the-door-and-lists-assignments
  (let [d (api/detail (opts nothing-dumped) "claude-village")]
    (are [path expected] (= expected (get-in d path))
      [:layers 1 :y] 65
      [:layers 1 :rows 4 6 :e] "oak_door[facing=east,half=lower]"   ; the door of jizo-hut, turned 90, at 101 65 204
      [:assign 2] {:spot "jizo-hut/bed" :body "Jizo" :use :bed :answer :unknown}
      [:spots "jizo-hut/bed"] [104 65 203])))

(deftest detail-of-a-village-without-blueprints-lists-what-it-cannot-place
  (let [d (api/detail {:dir dir :blueprint-dir (.join path dir "missing") :block-at nothing-dumped} "claude-village")]
    (is (= ["jizo-hut" "west-hut" "jizo-hut/bed" "jizo-hut/chest"] (map :element (:errors d))))))

(deftest detail-of-an-unknown-plan-is-nil
  (is (nil? (api/detail (opts nothing-dumped) "nope"))))
