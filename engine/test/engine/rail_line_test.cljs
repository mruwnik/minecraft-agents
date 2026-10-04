(ns engine.rail-line-test
  "jobs.build.rail-line: build a rail plan with the from-plan builder and prove the line, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.build-from-plan-test :as b]
            [engine.core :as core]
            [engine.harvest-test :as h]
            [engine.jobs.rail :as builder]
            [engine.takeover :as takeover]
            [jobs.build.rail-line :as rail-line]
            [engine.test-util :as tu]
            [plan.rail :as rail]
            [plan.shape :as shape]))

(def job 'jobs.build.rail-line)

(def from [0 64 0])
(def to [29 64 0])

(defn ground
  "Ground of block at height y under a 40 x 9 area around the line."
  [block y]
  (into {} (for [x (range -5 35) z (range -4 5)] [(h/cell-key x y z) block])))

(defn line-plan [opts]
  {:id "line" :parts (:parts (rail/layout from to opts))})

(defn kit
  "Exactly the items the layout counts, the fill as cobblestone, plus extra {item n}."
  [opts & [extra]]
  (let [{:keys [items fill]} (:materials (rail/layout from to opts))]
    (mapv (fn [[name n]] {:name name :count n}) (merge-with + items {"cobblestone" fill} extra))))

(defn spec
  "The fake world: ground at y 63 (the bed is natural ground) or y 62 (the bed is built), the body beside the line."
  [ground-block ground-y inventory]
  {:inventory inventory :blocks (ground ground-block ground-y) :self {:pos {:x 0 :y (inc ground-y) :z 3}}})

(defn ^:async build!
  "Run the job as a child over the world spec and plans: [result seen p]."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen]} (b/start world-spec plans (or zones []))
        result (await (h/child-outcome eng job (merge {:plan "line"} args) 400))]
    [result seen p]))

(defn props [p [x y z]] (js->clj (.-properties (.blockAt p #js {:x x :y y :z z})) :keywordize-keys true))

;; ---------------------------------------------------------------- building and proving

(deftest every-style-and-power-source-builds-a-line-that-passes-the-proof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[opts ground-block ground-y] [[{} "stone" 63]
                                              [{} "grass_block" 63]
                                              [{:power :lever} "stone" 63]
                                              [{:power :block} "stone" 62]
                                              [{:style :all-powered} "stone" 62]
                                              [{:style :all-powered :power :torch} "dirt" 63]]]
          (let [[result seen p] (await (build! (spec ground-block ground-y (kit opts)) {"line" (line-plan opts)} {}))
                label (pr-str opts ground-block)]
            (is (true? (:ok? result)) label)
            (is (= [] (:breaks result)) label)
            (is (= [] (get-in result [:built :wrong])) (str label ": a bed of other sturdy ground is no fault"))
            (is (= "powered_rail" (h/block-at p 3 64 0)) label)
            (is (true? (:powered (props p [3 64 0]))) label)
            (is (= "east_west" (:shape (props p [0 64 0]))) label)
            (is (= 1 (count (h/events-of seen :rail-build.done))) label)
            (is (empty? (h/events-of seen :rail-build.broken)) label)))))))

(deftest a-lever-is-switched-on-after-the-build
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ _ p] (await (build! (spec "stone" 63 (kit {:power :lever})) {"line" (line-plan {:power :lever})} {}))]
          (is (= "lever" (h/block-at p 3 64 -1)))
          (is (true? (:powered (props p [3 64 -1])))))))))

;; ---------------------------------------------------------------- the check

(defn ^:async declines
  "Submit the job as a top-level job, tick a few times: the places made and the events of each kind named."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen]} (b/start world-spec plans (if (= :none zones) nil (or zones [])))]
    (core/submit! eng (list job args) {})
    (dotimes [_ 4] (swap! h/clock + 700) (await (core/tick! eng)))
    {:places (count (h/calls p "place"))
     :declined (mapv #(select-keys % [:plan :reason :why :refused :cells]) (h/events-of seen :rail-build.declined))
     :texts (mapv :text (h/events-of seen :rail-build.declined))
     :short (mapv :short (h/events-of seen :rail-build.short))
     :warns (count (filter #(re-find #"^(rail-)?build\." (name (:kind %))) @seen))}))

(def holed-plan
  {:id "line"
   :parts [{:id "bed" :box [[-1 63 0] [5 63 0]] :want [:any "stone"]}
           {:id "rails" :cells [[0 64 0] [1 64 0] [3 64 0] [4 64 0]] :want {:block "rail" :shape :east_west}}]})

(deftest a-plan-it-cannot-build-declines-with-one-warn-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[plans args zones declined]
                [[{} {:plan "line"} [] [{:plan "line" :reason :plan}]]
                 [{"line" holed-plan} {:plan "line"} [] [{:plan "line" :reason :not-a-line :why :gap}]]
                 [{"line" (line-plan {})} {:plan "line"} :none [{:plan "line" :reason :no-zones}]]
                 [{"line" (line-plan {}) "other" {:id "other" :parts [{:id "o" :cells [[7 64 0]] :want "stone"}]}} {:plan "line"} []
                  [{:plan "line" :reason :refused :refused [{:pos [7 64 0] :reason :footprint :plan "other"}]}]]]]
          (let [r (await (declines (spec "stone" 63 (kit {})) plans args zones))]
            (is (= declined (:declined r)) (pr-str declined))
            (is (= 1 (:warns r)) (pr-str declined))
            (is (= 0 (:places r)) (pr-str declined))))))))

(deftest short-of-powered-rails-declines-naming-the-shortage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (declines (spec "stone" 63 (kit {} {"powered_rail" -2})) {"line" (line-plan {})} {:plan "line"}))]
          (is (= [{"powered_rail" 2}] (:short r)))
          (is (= 1 (:warns r)))
          (is (= 0 (:places r))))))))

(defn built-world
  "The fake world with every cell of the plan already as it wants (names only: the fake works the rail states out)."
  [plan ground-block]
  (merge (ground ground-block 63)
         (into {} (keep (fn [{:keys [pos want]}]
                          (when-not (= :clear want) [(apply h/cell-key pos) (shape/want-block want)])))
               (:cells (shape/expand plan {})))))

(deftest a-sound-line-resubmitted-finishes-at-once-with-one-done-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (line-plan {})
              {:keys [eng p seen]} (b/start {:inventory (kit {}) :blocks (built-world plan "stone")
                                             :self {:pos {:x 0 :y 64 :z 3}}}
                                            {"line" plan} [])
              _ (core/submit! eng (list job {:plan "line"}) {})]
          (dotimes [_ 6] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (= 1 (count (h/events-of seen :rail-build.done))))
          (is (= 0 (count (h/events-of seen :rail-build.declined))))
          (is (= 0 (count (h/calls p "place")))))))))

;; ---------------------------------------------------------------- redstone blocks on ground that is there

(deftest a-redstone-block-bed-on-natural-ground-declines-once-naming-the-cells-and-what-would-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts {:power :block}
              r (await (declines (spec "stone" 63 (kit opts)) {"line" (line-plan opts)} {:plan "line"}))]
          (is (= [{:plan "line" :reason :source-blocked :cells [[3 63 0] [25 63 0]]}] (:declined r)))
          (is (= 1 (:warns r)))
          (is (= 0 (:places r)))
          (is (every? #(re-find % (first (:texts r))) [#"\[3 63 0\]" #"\[25 63 0\]" #":torch" #":lever" #"raised"])))))))

(deftest a-redstone-block-bed-on-a-raised-line-is-not-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts {:power :block}
              r (await (declines (spec "stone" 62 (kit opts)) {"line" (line-plan opts)} {:plan "line"}))]
          (is (= [] (:declined r)))
          (is (pos? (:places r))))))))

;; ---------------------------------------------------------------- build.wrong

(deftest sturdy-natural-ground-under-the-line-is-no-wrong-block-of-the-builder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (ground "grass_block" 63) (h/cell-key 5 63 0) "farmland")
              [result seen] (await (build! {:inventory (kit {}) :blocks world :self {:pos {:x 0 :y 64 :z 3}}}
                                           {"line" (line-plan {})} {}))
              [wrong] (h/events-of seen :build.wrong)]
          (is (= [[5 63 0]] (mapv :pos (:cells wrong))) "only the bed cell that is not sturdy is wrong")
          (is (= 1 (count (h/events-of seen :build.wrong))))
          (is (= [[5 63 0]] (mapv :pos (get-in result [:built :wrong])))))))))

(deftest ground-that-is-sturdy-everywhere-gives-no-build-wrong-notice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ seen] (await (build! (spec "grass_block" 63 (kit {})) {"line" (line-plan {})} {}))]
          (is (empty? (h/events-of seen :build.wrong))))))))

;; ---------------------------------------------------------------- cells nobody has seen

(defn missing-cell [want item] {:answer :missing :want want :item item :found "air"})
(def unseen-cell (fn [want item] {:answer :unknown :want want :item item :found nil}))
(def fill [:any "cobblestone" "dirt"])

(deftest unseen-cells-are-owed-fill-only-up-to
  (let [cells [(missing-cell fill "cobblestone") (missing-cell fill "dirt")
               (unseen-cell fill "cobblestone") (unseen-cell fill "cobblestone")]]
    (is (= {"cobblestone | dirt" 1} (rail-line/short-of cells {"cobblestone" 1})) "seen cells are owed for sure")
    (is (= {"cobblestone | dirt" 3} (rail-line/up-to-of cells {"cobblestone" 1})) "with the unseen ones: up to")
    (is (= {} (rail-line/short-of cells {"cobblestone" 2})))
    (is (= {"cobblestone | dirt" 2} (rail-line/up-to-of cells {"cobblestone" 2})))
    (is (= {} (rail-line/up-to-of cells {"cobblestone" 4})))))

;; the two buffer blocks are seen empty and carried; the 11 bed cells at the far end are not seen
(deftest a-far-end-nobody-has-seen-does-not-hold-the-line-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (mapv #(str % ",63,0") (range 20 31))
              world {:inventory (kit {} {"cobblestone" (- 2 (:fill (:materials (rail/layout from to {}))))}) :blocks (ground "stone" 63) :unloaded far
                     :self {:pos {:x 0 :y 64 :z 3}}}
              r (await (declines world {"line" (line-plan {})} {:plan "line"}))]
          (is (= [] (:short r)))
          (is (pos? (:places r))))))))

;; ---------------------------------------------------------------- a line broken after the build

(defn ^:async broken-run
  "Build the line, break the rail at x 7, then submit the job with args: [seen p]."
  [args]
  (let [{:keys [eng p seen]} (b/start (spec "stone" 63 (kit {})) {"line" (line-plan {})} [])]
    (await (h/child-outcome eng job {:plan "line"} 400))
    (.delete (.-blocks (.-state (.-world p))) "7,64,0")
    (core/submit! eng (list job (merge {:plan "line"} args)) {})
    (dotimes [_ 6] (swap! h/clock + 700) (await (core/tick! eng)))
    [seen p]))

(deftest a-rail-broken-after-the-build-is-a-gap-in-the-next-proof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen p] (await (broken-run {:all-carried false}))
              [warn] (h/events-of seen :rail-build.broken)]
          (is (= "air" (h/block-at p 7 64 0)))
          (is (= [{:pos [7 64 0] :why :gap}] (:breaks warn)))
          (is (= {"rail" 1} (:short warn)))
          (is (= 1 (count (h/events-of seen :rail-build.broken)))))))))

(deftest a-broken-line-without-the-rail-carried-declines-short-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen] (await (broken-run {}))]
          (is (= [{"rail" 1}] (mapv :short (h/events-of seen :rail-build.short))))
          (is (empty? (h/events-of seen :rail-build.broken))))))))

;; ---------------------------------------------------------------- resuming

(deftest a-cut-mid-build-resumes-and-finishes-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (spec "stone" 63 (kit {})) {"line" (line-plan {})} [])
              release (.hold (.-world p) "place")]
          (core/submit! eng (list job {:plan "line"}) {})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= {:ok true} (takeover/take! eng {:who "claude" :why "test"})))
            (await round)
            (release))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (dotimes [_ 120] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (every? #(= "rail" (h/block-at p % 64 0)) [0 1 28 29]))
          (is (every? #(= "powered_rail" (h/block-at p % 64 0)) [2 3 4 5 24 25 26 27]))
          (is (= 1 (count (h/events-of seen :rail-build.done))))
          (is (empty? (:list (core/state eng)))))))))

;; ---------------------------------------------------------------- corners and slopes: the head-first builder

(def l-route [[0 64 0] [19 64 0] [19 64 12]])
(def u-route [[0 64 0] [19 64 0] [19 64 3] [0 64 3]])
(def slope-route [[0 64 0] [19 64 0] [25 70 0] [45 70 0]])
(def down-route [[0 70 0] [14 70 0] [20 64 0] [40 64 0]])

(defn route-plan [waypoints opts] {:id "line" :parts (:parts (rail/layout waypoints opts))})

(defn route-kit
  "Exactly the items the layout of the waypoints counts, the fill as cobblestone, plus extra {item n}."
  [waypoints opts & [extra]]
  (let [{:keys [items fill]} (:materials (rail/layout waypoints opts))]
    (mapv (fn [[name n]] {:name name :count n}) (merge-with + items {"cobblestone" fill} extra))))

(defn terrain
  "Stone from y (top - 1) to top under every column x in xs, z in zs, where top is (top-of x)."
  [top-of xs zs]
  (into {} (for [x xs z zs y [(dec (top-of x)) (top-of x)]] [(h/cell-key x y z) "stone"])))

(defn up-top
  "The ground under the slope-route: the bed of the rail one block up for each cell along the climb (x 19 to 25)."
  [x]
  (cond (<= x 19) 63 (<= x 25) (+ 63 (- x 19)) :else 69))

(defn down-top [x] (cond (<= x 14) 69 (<= x 20) (- 69 (- x 14)) :else 63))

(def flat-top (constantly 63))

(defn route-world
  [waypoints opts top-of & [inventory]]
  {:inventory (or inventory (route-kit waypoints opts))
   :blocks (terrain top-of (range -5 60) (range -4 18))
   :self {:pos {:x 0 :y (inc (top-of 0)) :z 3}}})

(defn ^:async build-route!
  "Build the plan of the waypoints over the world: [result seen p eng]."
  [waypoints opts world args & [zones]]
  (let [{:keys [eng p seen]} (b/start world {"line" (route-plan waypoints opts)} (or zones []))
        result (await (h/child-outcome eng job (merge {:plan "line"} args) 900))]
    [result seen p eng]))

(defn shape-of [p [x y z]] (:shape (props p [x y z])))

(deftest the-work-goes-station-by-station-the-bed-first-then-the-rail-then-what-stands-beside-it
  (let [ps [[0 64 0] [1 64 0] [2 65 0]]
        rail {:block "rail"}
        cells [{:pos [2 65 0] :want rail} {:pos [1 64 -1] :want "redstone_torch"} {:pos [1 63 -1] :want "stone"}
               {:pos [1 64 0] :want rail} {:pos [0 64 0] :want rail} {:pos [2 64 0] :want "stone"}
               {:pos [0 63 0] :want "stone"} {:pos [1 63 0] :want "stone"}]
        order (fn [from] (mapv :pos (builder/work-order ps from cells)))
        before (fn [order a b] (< (.indexOf order a) (.indexOf order b)))]
    (are [from pairs] (every? (fn [[a b]] (before (order from) a b)) pairs)
      :first [[[0 63 0] [0 64 0]] [[0 64 0] [1 63 0]] [[1 63 0] [1 64 0]] [[1 63 -1] [1 64 0]] [[1 64 0] [1 64 -1]]
              [[1 64 -1] [2 64 0]] [[2 64 0] [2 65 0]]]
      :last [[[2 64 0] [2 65 0]] [[2 65 0] [1 63 0]] [[1 64 0] [1 64 -1]] [[1 64 -1] [0 63 0]] [[0 63 0] [0 64 0]]])))

(deftest corners-and-slopes-are-built-in-line-order-and-pass-the-proof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label waypoints opts top-of]
                [["L" l-route {} flat-top]
                 ["U-turn" u-route {} flat-top]
                 ["S-bend" [[0 64 0] [19 64 0] [19 64 3] [38 64 3]] {} flat-top]
                 ["L all powered" l-route {:style :all-powered :power :torch} flat-top]
                 ["6 up" slope-route {} up-top]
                 ["6 down" down-route {} down-top]]]
          (let [[result seen] (await (build-route! waypoints opts (route-world waypoints opts top-of) {}))]
            (is (true? (:ok? result)) label)
            (is (= [] (:breaks result)) label)
            (is (= {} (get-in result [:built :given-up])) label)
            (is (= 1 (count (h/events-of seen :rail-build.done))) label)
            (is (empty? (h/events-of seen :rail-build.broken)) label)))))))

(deftest the-corner-and-the-slope-come-out-as-the-layout-wants-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ _ p] (await (build-route! l-route {} (route-world l-route {} flat-top) {}))]
          (is (= ["east_west" "south_west" "north_south"] (mapv #(shape-of p %) [[18 64 0] [19 64 0] [19 64 1]])))
          (is (true? (:powered (props p [18 64 0]))))
          (is (true? (:powered (props p [19 64 1])))))
        (let [[_ _ p] (await (build-route! slope-route {} (route-world slope-route {} up-top) {}))]
          (is (= ["east_west" "ascending_east" "ascending_east" "ascending_east" "east_west"]
                 (mapv #(shape-of p %) [[18 64 0] [19 64 0] [20 65 0] [24 69 0] [25 70 0]])))
          (is (= [true true true] (mapv #(:powered (props p %)) [[19 64 0] [21 66 0] [23 68 0]])))
          (is (= "powered_rail" (h/block-at p 21 66 0))))))))

(deftest a-slope-over-ground-that-lacks-its-bed-gets-the-bed-placed-from-the-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lower (fn [x] (dec (up-top x)))
              [result _ p] (await (build-route! slope-route {} (route-world slope-route {} lower) {}))]
          (is (true? (:ok? result)))
          (is (every? #(= "cobblestone" (h/block-at p % (+ 62 (- % 19) 1) 0)) [21 22 23 24]) "the bed under each climbing rail"))))))

(deftest the-body-stands-on-the-line-it-builds-not-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ _ _ eng] (await (build-route! slope-route {} (route-world slope-route {} up-top) {}))
              stands (set (map (juxt :x :y :z) (tu/walked-to eng)))
              rails (set (map :pos (rail/line (:cells (shape/expand (route-plan slope-route {}) {})))))]
          (is (some #(and (<= 19 (first %) 25) (rails %)) stands) "a stand on a climbing rail")
          (is (some #(and (<= 5 (first %) 15) (rails %)) stands) "a stand on a flat rail"))))))

;; ---------------------------------------------------------------- shapes that settled wrong

(defn built-line-world
  "A world with the whole plan of the waypoints already placed (names only) over the terrain, plus extra blocks and
  stored states; the body holds extra items only."
  [waypoints opts top-of extra-blocks states & [inventory]]
  (assoc (merge-with merge
                     {:blocks (terrain top-of (range -5 60) (range -4 18))}
                     {:blocks (into {} (keep (fn [{:keys [pos want]}]
                                               (when-not (= :clear want) [(apply h/cell-key pos) (shape/want-block want)])))
                                    (:cells (shape/expand (route-plan waypoints opts) {})))}
                     {:blocks extra-blocks})
         :inventory (or inventory [])
         :states states
         :self {:pos {:x 3 :y 64 :z 3}}))

(deftest a-rail-that-settled-in-the-wrong-shape-is-dug-and-placed-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (built-line-world l-route {} flat-top {} {"10,64,0" {:shape "north_south"}})
              [result seen p] (await (build-route! l-route {} world {:all-carried false}))]
          (is (true? (:ok? result)))
          (is (= "east_west" (shape-of p [10 64 0])))
          (is (= 1 (count (filter #(= {:x 10 :y 64 :z 0} (js->clj (.-pos (.-args %)) :keywordize-keys true)) (h/calls p "dig")))))
          (is (= [] (h/events-of seen :rail-build.broken))))))))

(deftest a-rail-that-cannot-come-out-right-is-given-up-as-shape-after-the-fixes-are-spent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[fix digs] [[1 1] [2 2] [0 0]]]
          (let [world (built-line-world l-route {} flat-top {"0,64,1" "rail"} {})
                [result seen p] (await (build-route! l-route {} world {:all-carried false :fix fix}))
                [warn] (h/events-of seen :rail-build.broken)]
            (is (false? (:ok? result)) (str fix))
            (is (= {[0 64 0] :shape} (get-in result [:built :given-up])) (str fix))
            (is (= [{:pos [0 64 0] :why :shape}] (:breaks warn)) (str fix))
            (is (= {[0 64 0] :shape} (:given-up warn)) (str fix))
            (is (= digs (count (h/calls p "dig"))) (str fix))))))))

(deftest a-corner-rail-broken-after-the-build-is-a-gap-in-the-next-proof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (route-world l-route {} flat-top) {"line" (route-plan l-route {})} [])]
          (await (h/child-outcome eng job {:plan "line"} 900))
          (.delete (.-blocks (.-state (.-world p))) "19,64,0")
          (core/submit! eng (list job {:plan "line" :all-carried false}) {})
          (dotimes [_ 6] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (= [{:pos [19 64 0] :why :gap}] (:breaks (first (h/events-of seen :rail-build.broken))))))))))

(deftest a-corner-in-another-plans-footprint-declines-before-anything-is-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (declines (route-world l-route {} flat-top)
                                 {"line" (route-plan l-route {}) "other" {:id "other" :parts [{:id "o" :cells [[19 64 0]] :want "stone"}]}}
                                 {:plan "line"} []))]
          (is (= [{:plan "line" :reason :refused :refused [{:pos [19 64 0] :reason :footprint :plan "other"}]}] (:declined r)))
          (is (= 0 (:places r))))))))

(deftest a-cut-mid-build-of-a-corner-line-resumes-and-finishes-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (route-world l-route {} flat-top) {"line" (route-plan l-route {})} [])
              release (.hold (.-world p) "place")]
          (core/submit! eng (list job {:plan "line"}) {})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= {:ok true} (takeover/take! eng {:who "claude" :why "test"})))
            (await round)
            (release))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (dotimes [_ 160] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (= ["east_west" "south_west" "north_south"] (mapv #(shape-of p %) [[18 64 0] [19 64 0] [19 64 1]])))
          (is (= 1 (count (h/events-of seen :rail-build.done))))
          (is (empty? (:list (core/state eng)))))))))
