(ns dashboard.plan-test
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [deftest are is]]
            [clojure.string :as str]
            [dashboard.plan :as plan]))

(def fixtures-dir (.resolve path js/__dirname ".." "test" "fixtures" "plans"))

(def region {:min [0 64 0] :max [1 64 1]})
(def good {:id "p" :region region :elements [{:id "a" :kind :plot :region region :content {:crop "wheat"}}]})

(defn errors-of [plan] (plan/plan-errors plan "p"))

(deftest valid-plans-have-no-errors
  (are [p] (empty? (errors-of p))
    good
    (assoc good :status :done :kind :farm :name "N" :owner "O")
    (assoc good :elements [])
    (dissoc good :elements)
    (assoc good :elements [{:id "w" :kind :structure :at [1 2 3] :rotation 90 :content {:blueprint "well"}}])
    (assoc good :elements [{:id "c" :kind :plan :ref "other"}])
    (assoc good :elements [{:id "b" :kind :border :region region :content {:palette ["a" "b"]}}
                           {:id "k" :kind :area :region region :content {:air true}}])))

(deftest invalid-plans-name-their-problem
  (are [p fragment] (some #(str/includes? % fragment) (errors-of p))
    (assoc good :id "other") "must equal the file name"
    (dissoc good :region) "region must be"
    (assoc good :region {:min [0 0 0] :max [-1 0 0]}) "must not exceed"
    (assoc good :region {:min [0 0] :max [1 1 1]}) ":min and :max"
    (assoc good :status :bogus) ":status must be one of"
    (assoc good :elements {:a 1}) ":elements must be a vector"
    (assoc good :elements [(first (:elements good)) (first (:elements good))]) "unique"
    (assoc good :elements [{:id "a" :kind :plot :content {:crop "wheat"}}]) "region must be"
    (assoc good :elements [{:id "a" :kind :plot :region region}]) "content must be a map"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:crop "wheat" :block "x"}}]) "exactly one"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:palette []}}]) ":palette must be"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:air false}}]) ":air must be true"
    (assoc good :elements [{:id "a" :kind :plot :region region :content {:blueprint "x"}}]) "belongs to a :structure"
    (assoc good :elements [{:id "a" :kind :structure :content {:blueprint "x"}}]) "needs :at"
    (assoc good :elements [{:id "a" :kind :structure :at [0 0 0] :rotation 45 :content {:blueprint "x"}}]) ":rotation must be"
    (assoc good :elements [{:id "a" :kind :structure :at [0 0 0] :content {:crop "x"}}]) "{:blueprint name}"
    (assoc good :elements [{:id "a" :kind :plan}]) "needs :ref"
    (assoc good :elements [{:id "a b" :kind :plot}]) ":id must be"
    (assoc good :elements [{:kind :plot}]) ":id must be"))

(deftest parse-never-throws
  (are [text expected-fragment] (some #(str/includes? % expected-fragment) (:errors (plan/parse text "p")))
    "{:id " "unreadable EDN"
    "[1 2]" "must hold one map"
    "{:id \"q\"}" "must equal the file name"
    "" "must hold one map"))

(deftest parse-reads-a-plan
  (is (= good (:plan (plan/parse (pr-str good) "p")))))

(deftest read-dir-lists-valid-plans-and-the-errors-of-the-rest
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "plans-"))]
    (.writeFileSync fs (.join path dir "p.edn") (pr-str good))
    (.writeFileSync fs (.join path dir "broken.edn") "{:id ")
    (.writeFileSync fs (.join path dir "wrongid.edn") (pr-str good))
    (.writeFileSync fs (.join path dir "notes.txt") "ignored")
    (let [{:keys [plans errors]} (plan/read-dir dir)]
      (is (= ["p"] (keys plans)))
      (is (= ["broken.edn" "wrongid.edn"] (map :file errors)))
      (is (every? seq (map :errors errors))))
    (is (= {:plans {} :errors []} (plan/read-dir (.join path dir "missing"))))))

(deftest the-fixture-plans-are-valid
  (let [{:keys [plans errors]} (plan/read-dir fixtures-dir)]
    (is (= [] errors))
    (is (= ["claude-village" "jizo-farm" "spawn-clear"] (sort (keys plans))))))

(deftest border-keeps-the-ring-of-every-layer
  (are [region border? n] (= n (count (plan/region-cells region border?)))
    {:min [0 0 0] :max [4 0 3]} false 20
    {:min [0 0 0] :max [4 0 3]} true 14
    {:min [0 0 0] :max [4 1 3]} true 28
    {:min [0 0 0] :max [0 0 0]} true 1
    {:min [0 0 0] :max [2 0 0]} true 3
    {:min [0 0 0] :max [2 0 2]} true 8))

(deftest rotation-is-clockwise-seen-from-above
  (are [rotation offset expected] (= expected (plan/rotate rotation offset))
    0 [3 1] [3 1]
    90 [3 1] [-1 3]
    180 [3 1] [-3 -1]
    270 [3 1] [1 -3]))

(deftest blueprint-cells-are-placed-at-the-anchor
  (let [cells [{:dx 0 :dy 0 :dz 0 :names ["cobblestone"]} {:dx 2 :dy 1 :dz 0 :names ["oak_planks" "birch_planks"]}
               {:dx 1 :dy 0 :dz 0 :air true} {:dx 1 :dy 1 :dz 0 :names ["@solid"]}]]
    (are [rotation expected] (= expected (map (juxt :pos :want) (plan/structure-cells {:at [10 70 20] :rotation rotation} cells)))
      0 [[[10 70 20] {:kind :block :block "cobblestone"}] [[12 71 20] {:kind :palette :blocks ["oak_planks" "birch_planks"]}]
         [[11 70 20] {:kind :air}] [[11 71 20] {:kind :solid}]]
      90 [[[10 70 20] {:kind :block :block "cobblestone"}] [[10 71 22] {:kind :palette :blocks ["oak_planks" "birch_planks"]}]
          [[10 70 21] {:kind :air}] [[10 71 21] {:kind :solid}]])))

(def blueprints {"hut" [{:dx 0 :dy 0 :dz 0 :names ["stone"]} {:dx 1 :dy 0 :dz 0 :names ["stone"]}]})
(def plans
  {"farm" {:id "farm" :region region
           :elements [{:id "plot" :kind :plot :region {:min [0 64 0] :max [1 64 0]} :content {:crop "wheat"}}
                      {:id "fence" :kind :border :region {:min [0 64 0] :max [2 64 2]} :content {:block "oak_fence"}}]}
   "town" {:id "town" :region region
           :elements [{:id "hut" :kind :structure :at [5 64 5] :content {:blueprint "hut"}}
                      {:id "farm" :kind :plan :ref "farm"}]}
   "bad" {:id "bad" :region region
          :elements [{:id "ghost" :kind :structure :at [0 0 0] :content {:blueprint "nope"}}
                     {:id "lost" :kind :plan :ref "nowhere"}
                     {:id "huge" :kind :plot :region {:min [0 0 0] :max [999 999 999]} :content {:air true}}]}
   "a" {:id "a" :region region :elements [{:id "to-b" :kind :plan :ref "b"}]}
   "b" {:id "b" :region region :elements [{:id "to-a" :kind :plan :ref "a"}]}})

(defn expand [id] (plan/expand-plan plans id blueprints))

(deftest expansion-of-a-plan
  (let [{:keys [cells elements errors]} (expand "farm")]
    (is (= [2 8] (map :count elements)))
    (is (= ["plot" "fence"] (map :id elements)))
    (is (= [] errors))
    (is (= 10 (count cells)))
    (is (= #{"plot" "fence"} (set (map :element cells))))))

(deftest children-roll-up-into-one-element
  (let [{:keys [cells elements]} (expand "town")]
    (is (= [2 10] (map :count elements)))
    (is (= "crop wheat" (:content (first (:elements (expand "farm"))))))
    (is (= "plan farm" (:content (second elements))))
    (is (= 12 (count cells)))
    (is (= {"hut" 2 "farm" 10} (frequencies (map :element cells))))))

(deftest broken-elements-report-errors-and-keep-the-rest
  (let [{:keys [elements errors]} (expand "bad")]
    (are [i fragment] (str/includes? (get-in elements [i :error]) fragment)
      0 "unknown blueprint"
      1 "unknown plan"
      2 "too large")
    (is (= ["ghost" "lost" "huge"] (map :element errors)))
    (is (= [0 0 0] (map :count elements)))))

(deftest cycles-are-an-error-not-a-hang
  (let [{:keys [elements errors]} (expand "a")]
    (is (= ["to-b/to-a"] (map :element errors)))
    (is (str/includes? (:error (first errors)) "cycle: a -> b -> a"))
    (is (str/includes? (get-in elements [0 :error]) "inside plan b"))
    (is (= [0] (map :count elements)))))

(deftest child-errors-surface-under-the-parent-element
  (let [p (assoc plans "top" {:id "top" :region region :elements [{:id "inner" :kind :plan :ref "bad"}]})
        {:keys [errors]} (plan/expand-plan p "top" blueprints)]
    (is (= ["inner/ghost" "inner/lost" "inner/huge"] (map :element errors)))))

(deftest children-lists-the-nested-plans
  (are [id expected] (= expected (plan/children (get plans id)))
    "town" ["farm"]
    "farm" []
    "bad" ["nowhere"]))
