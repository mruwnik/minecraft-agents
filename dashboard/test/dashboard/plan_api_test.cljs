(ns dashboard.plan-api-test
  (:require ["path" :as path]
            [cljs.test :refer [deftest are is]]
            [dashboard.plan-api :as api]))

(def dir (.resolve path js/__dirname ".." "test" "fixtures" "plans"))
(def blueprint-dir (.resolve path js/__dirname ".." "test" "fixtures" "blueprints"))

(defn opts [block-at] {:dir dir :blueprint-dir blueprint-dir :block-at block-at})

(deftest a-village-anchor-has-no-invented-completion
  (let [p {:id "marker" :kind :village :at [4 65 -2]
           :parts [] :metadata {:geometry :incomplete :population {:target 8}}}
        item (api/list-item {} (fn [& _] (throw (js/Error. "marker must not read blocks"))) {} ["marker" p])]
    (is (= :village (:kind item)))
    (is (= [4 65 -2] (:at item)))
    (is (= {:geometry :incomplete} (:metadata item)))
    (is (= 0 (get-in item [:counts :total])))
    (is (= 0 (:percent item)))
    (is (nil? (:region item)))
    (is (= [] (:elements item)))))

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

(deftest detail-says-when-the-world-was-last-checked
  (let [d (api/detail (assoc (opts nothing-dumped) :column-mtime (fn [cx _] (when (= cx -2) 5000))) "jizo-farm")]
    (are [path expected] (= expected (get-in d path))
      [:checked :oldest] 5000
      [:checked :newest] 5000
      [:checked :dumped] 1)
    (is (number? (get-in d [:checked :now])))
    (is (<= 1 (get-in d [:checked :chunks])))))

(deftest detail-without-a-dump-clock-has-no-checked
  (is (nil? (:checked (api/detail (opts nothing-dumped) "jizo-farm")))))

(deftest detail-of-an-unknown-plan-is-nil
  (is (nil? (api/detail (opts nothing-dumped) "nope"))))

;; ---------------------------------------------------------------- plans that want different things of one cell
(def conflict-dir (.resolve path js/__dirname ".." "test" "fixtures" "plans-conflict"))
(defn conflict-opts [] {:dir conflict-dir :blueprint-dir blueprint-dir :block-at nothing-dumped})

(deftest summaries-list-the-conflicts-between-plans
  (let [{:keys [plans conflicts]} (api/summaries (conflict-opts))]
    (is (= [{:plans ["north-field" "south-field"] :count 4 :same 0 :shown 4
             :box {:min [2 64 2] :max [3 64 3]} :cells [[2 64 2] [2 64 3] [3 64 2] [3 64 3]]}]
           conflicts))
    (are [id expected] (= expected (:conflicts (by-id plans id)))
      "north-field" [{:with "south-field" :count 4 :box {:min [2 64 2] :max [3 64 3]}}]
      "south-field" [{:with "north-field" :count 4 :box {:min [2 64 2] :max [3 64 3]}}]
      "agreeing" [])))     ; the same want on a shared cell is no conflict

(deftest summaries-without-conflicts-say-so
  (is (= [] (:conflicts (api/summaries (opts nothing-dumped))))))

(deftest the-cells-sent-to-the-browser-are-capped
  (are [limit shown cells] (= {:count 4 :shown shown :cells cells}
                              (select-keys (api/capped limit {:count 4 :cells [[0 0 0] [1 0 0] [2 0 0] [3 0 0]]}) [:count :shown :cells]))
    10 4 [[0 0 0] [1 0 0] [2 0 0] [3 0 0]]
    2 2 [[0 0 0] [1 0 0]]
    0 0 []))

(deftest detail-lists-its-conflicts-and-flags-the-cells-in-its-grid
  (let [d (api/detail (conflict-opts) "north-field")
        row (fn [z] (get-in d [:layers 0 :rows z]))]
    (is (= [{:with "south-field" :count 4 :box {:min [2 64 2] :max [3 64 3]}}] (:conflicts d)))
    (is (= [nil nil true true] (mapv :x (row 2))))
    (is (= [nil nil nil nil] (mapv :x (row 1))))
    (is (= [] (:conflicts (api/detail (conflict-opts) "agreeing"))))
    (is (nil? (get-in (api/detail (conflict-opts) "agreeing") [:layers 0 :rows 0 0 :x])))))
