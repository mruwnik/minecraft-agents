(ns engine.farm-tend-test
  "jobs.farm.tend against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.farm.tend :as tend]
            [jobs.lib.world-files :as world]))

(def box {:min {:x 2 :y 63 :z 2} :max {:x 4 :y 64 :z 4}})

(def all-cells (for [x (range 2 5) z (range 2 5)] [x z]))

(defn k [x y z] (str x "," y "," z))

(defn world
  "Merge world parts: :blocks and :ages maps are merged, the rest is last wins."
  [& parts]
  (apply merge-with (fn [a b] (if (map? a) (merge a b) b)) parts))

(defn ground [name cells]
  {:blocks (into {} (map (fn [[x z]] [(k x 63 z) name])) cells)})

(defn crops [name age cells]
  {:blocks (into {} (map (fn [[x z]] [(k x 64 z) name])) cells)
   :ages (into {} (map (fn [[x z]] [(k x 64 z) age])) cells)})

(defn farm
  "Farmland under every cell and crops of name at age over the cells (none when cells is empty)."
  [crop age ground-cells crop-cells]
  (world (ground "farmland" ground-cells) (crops crop age crop-cells)))

(def wheat-drops {:drops {"wheat" ["wheat" "wheat_seeds"]}})

(defn item [name n] {:name name :count n})

(defn setup
  "An engine over a fake world; every result of tend's check is logged in :checks."
  ([spec] (setup spec nil))
  ([spec zones]
  (let [clock (atom 1000000)
        checks (atom [])
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor spec))
        logged (assoc-in registry/jobs ['jobs.farm.tend :check] (fn [c] (let [r (tend/check c)] (swap! checks conj r) r)))
        eng (core/create (cond-> {:primitives p :jobs logged :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           zones (assoc :world (world/of-data {} {} zones))))]
    {:eng eng :p p :seen seen :clock clock :checks checks})))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args spec n]
  (let [s (setup spec)]
    (core/submit! (:eng s) (list 'jobs.farm.tend (merge {:box box} args)) {})
    (dotimes [_ n]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    s))

(defn calls [{:keys [p]} name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :farm-tend.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn block-at [{:keys [p]} x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-name)))
(defn age-at [{:keys [p]} x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-age)))
(defn xz-of [call] (let [pos (.-pos (.-args call))] [(.-x pos) (.-z pos)]))
(defn dug [s] (set (map xz-of (calls s "dig"))))
(defn step [s name] (get-in (done-event s) [:steps name]))

(def chest {:x 10 :y 64 :z 0})

(defn chest-items [{:keys [p]}]
  (into {} (map (juxt :name :count)) (get-in @(fake/state p) [:containers [10 64 0]])))

(defn declines?
  "True when the job is submitted, the first tick does nothing and the body is never asked to do anything."
  [args spec]
  (let [{:keys [eng p]} (setup spec)]
    (core/submit! eng (list 'jobs.farm.tend args) {})
    (and (nil? (core/tick! eng))
         (empty? (.-calls (.-world p))))))

;; ------------------------------------------------------------------ scenarios

(deftest only-the-ripe-crops-are-cut-and-replanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (farm "wheat" 7 all-cells [[2 2] [3 2]]) (crops "wheat" 3 [[4 2]]) wheat-drops) 80))]
          (is (= #{[2 2] [3 2]} (dug s)))
          (is (= [0 0 3] (mapv #(age-at s % 64 2) [2 3 4])))
          (is (= "wheat" (block-at s 2 64 2)))
          (is (= 2 (:cut (step s :harvest))))
          (is (= 2 (:replanted (step s :harvest))))
          (is (= {:skipped :no-hoe} (step s :till)))
          (is (true? (finished? s))))))))

(deftest bare-farmland-is-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "farmland" [[2 2] [3 2]])
                                           {:inventory [(item "wheat_seeds" 4)]}) 40))]
          (is (= 2 (:planted (step s :plant))))
          (is (= ["wheat" "wheat"] (mapv #(block-at s % 64 2) [2 3])))
          (is (= {"wheat_seeds" 2} (inv s)))
          (is (= {:crops 2 :bare 0 :untilled 0} (:field (done-event s))))
          (is (= {:skipped :no-ripe} (step s :harvest)))
          (is (true? (finished? s))))))))

(deftest untilled-dirt-is-tilled-then-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "dirt" [[2 2] [3 2]])
                                           {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 5)]}) 80))]
          (is (= {:tilled 2} (step s :till)))
          (is (= 2 (:planted (step s :plant))))
          (is (= ["farmland" "farmland"] (mapv #(block-at s % 63 2) [2 3])))
          (is (= ["wheat" "wheat"] (mapv #(block-at s % 64 2) [2 3])))
          (is (= 2 (count (calls s "useOn"))))
          (is (true? (finished? s))))))))

(deftest without-a-hoe-the-dirt-is-left-and-the-bare-farmland-still-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "dirt" [[2 2]]) (ground "farmland" [[3 2]])
                                           {:inventory [(item "wheat_seeds" 4)]}) 40))]
          (is (= {:skipped :no-hoe} (step s :till)))
          (is (= 1 (:planted (step s :plant))))
          (is (= "dirt" (block-at s 2 63 2)))
          (is (= "wheat" (block-at s 3 64 2)))
          (is (empty? (calls s "useOn"))))))))

(deftest without-seed-nothing-is-tilled-or-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (farm "wheat" 7 [[2 2] [3 2]] [[2 2]]) (ground "dirt" [[4 2]])
                                           wheat-drops {:inventory [(item "stone_hoe" 1)]}) 80))]
          (is (= {:skipped :no-seed} (step s :till)))
          (is (= {:skipped :no-seed} (step s :plant)))
          (is (= 1 (count (calls s "place"))) "the replant of the cut crop only")
          (is (empty? (calls s "useOn")))
          (is (= "dirt" (block-at s 4 63 2)))
          (is (= "air" (block-at s 3 64 2))))))))

(deftest seed-caps-the-beds-that-are-made
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "dirt" [[2 2] [3 2] [4 2]])
                                           {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 2)]}) 120))]
          (is (= {:tilled 2} (step s :till)) "the report keeps the tilled count after the later skip")
          (is (= 2 (:planted (step s :plant))))
          (is (= 1 (get-in (done-event s) [:field :untilled])))
          (is (= 2 (count (calls s "useOn"))))
          (is (true? (finished? s))))))))

(deftest cells-outside-the-box-are-untouched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [outside (world (ground "dirt" [[9 3]]) (ground "farmland" [[9 4] [9 2]]) (crops "wheat" 7 [[9 2]]))
              s (await (scenario {} (world (farm "wheat" 7 [[2 2] [3 2]] [[2 2]]) (ground "dirt" [[4 2]]) outside wheat-drops
                                           {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 5)]}) 120))
              touched (concat (calls s "dig") (calls s "place") (calls s "useOn"))]
          (is (seq touched))
          (is (empty? (filter #(= 9 (first (xz-of %))) touched)))
          (is (= ["dirt" "farmland" "air"] [(block-at s 9 63 3) (block-at s 9 63 4) (block-at s 9 64 4)]))
          (is (= 7 (age-at s 9 64 2))))))))

(deftest nothing-to-do-declines-and-asks-the-body-nothing
  (let [mature (world (farm "wheat" 3 all-cells all-cells) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 4) (item "bone_meal" 3)]})
        huge {:min {:x 0 :y 63 :z 0} :max {:x 60 :y 64 :z 20}}]
    (is (declines? {:box box} mature) "unripe crops, fertilize off by default")
    (is (declines? {:box box :fertilize true :till false} (world (farm "wheat" 3 all-cells all-cells))) "bone meal wanted but none carried")
    (is (declines? {:box box :till false} (world (ground "dirt" [[2 2]]) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 4)]})) "till off")
    (is (declines? {} (world (farm "wheat" 7 all-cells all-cells))) "no box")
    (is (declines? {:box huge} (world (farm "wheat" 7 all-cells all-cells))) "box over the cell cap")
    (is (declines? {:box {:min {:x 2 :y 63 :z 2} :max {:x 4 :y 63 :z 4}}} (world (farm "wheat" 7 all-cells all-cells))) "no crop layer")))

(deftest without-a-chest-the-goods-stay-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (farm "wheat" 7 [[2 2]] [[2 2]]) wheat-drops {:inventory [(item "wheat" 5)]}) 40))]
          (is (= {:skipped :no-chest} (step s :deposit)))
          (is (= 6 (get (inv s) "wheat"))))))))

(def goods-world
  (world (farm "wheat" 3 [[2 2] [3 2]] [[2 2] [3 2]])
         {:inventory [(item "wheat" 5) (item "wheat_seeds" 10) (item "iron_hoe" 1) (item "bread" 2)]
          :containers {"10,64,0" []}}))

(deftest goods-above-the-seed-reserve-go-to-the-chest-and-tools-and-food-stay
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest} goods-world 40))]
          (is (= {"wheat" 5 "wheat_seeds" 6} (chest-items s)))
          (is (= {"wheat_seeds" 4 "iron_hoe" 1 "bread" 2} (inv s)))
          (is (= {:gave-up false} (select-keys (step s :deposit) [:gave-up])))
          (is (true? (finished? s))))))))

(deftest keep-is-honoured-above-the-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest :keep {"wheat" 2 "wheat_seeds" 1}} goods-world 40))]
          (is (= {"wheat" 3 "wheat_seeds" 6} (chest-items s)))
          (is (= 2 (get (inv s) "wheat")))
          (is (= 4 (get (inv s) "wheat_seeds"))))))))

(deftest only-seed-above-the-reserve-is-composted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:composter {:x 6 :y 64 :z 2}}
                                 (world goods-world {:blocks {"6,64,2" "composter"}}) 60))]
          (is (= {"wheat_seeds" 6} (:fed (step s :compost))))
          (is (= 4 (get (inv s) "wheat_seeds")))
          (is (= 5 (get (inv s) "wheat")) "crops are not fed")
          (is (= {:skipped :no-chest} (step s :deposit))))))))

(deftest without-a-composter-or-surplus-seed-the-step-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest} goods-world 40))]
          (is (= {:skipped :no-composter} (step s :compost))))
        (let [s (await (scenario {:composter {:x 6 :y 64 :z 2} :chest chest}
                                 (world (farm "wheat" 3 [[2 2] [3 2]] [[2 2] [3 2]]) {:blocks {"6,64,2" "composter"}}
                                        {:inventory [(item "wheat_seeds" 4) (item "wheat" 2)] :containers {"10,64,0" []}}) 40))]
          (is (= {:skipped :no-surplus-seed} (step s :compost)))
          (is (= {"wheat" 2} (chest-items s))))))))

(deftest bone-meal-is-used-on-unripe-crops-when-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:fertilize true} (world (farm "wheat" 3 [[2 2] [3 2]] [[2 2] [3 2]]) {:inventory [(item "bone_meal" 4)]}) 60))]
          (is (pos? (:used (step s :fertilize))))
          (is (> (age-at s 2 64 2) 3))
          (is (true? (finished? s))))))))

(deftest an-unreachable-ripe-crop-is-given-up-and-the-run-ends-with-a-result
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (farm "wheat" 7 [[4 4]] [[4 4]]) {:unreachable [(k 4 64 4)]}) 80))]
          (is (empty? (calls s "dig")))
          (is (some? (step s :harvest)))
          (is (= {:crops 1 :bare 0 :untilled 0} (:field (done-event s))))
          (is (true? (finished? s))))))))

(deftest a-started-run-passes-its-check-on-every-tick-until-it-hands-over-its-result
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (farm "wheat" 7 [[2 2] [3 2]] [[2 2] [3 2]]) wheat-drops) 80))]
          (is (> (count @(:checks s)) 3))
          (is (every? true? @(:checks s)) "also after the harvest left nothing that decide would call")
          (is (some? (done-event s)))
          (is (true? (finished? s))))))))

(deftest the-nearest-untilled-cell-is-tilled-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "dirt" [[2 2] [4 4]]) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 1)]}) 60))]
          (is (= ["farmland" "dirt"] [(block-at s 2 63 2) (block-at s 4 63 4)])))))))

(deftest a-cell-the-hoe-cannot-till-is-tried-once-and-the-run-still-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (ground "dirt" [[2 2]]) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 3)]}))]
          (.override (.-world (:p s)) "useOn" (fn [_ _ _] (js/Promise.resolve #js {:status "unchanged"})))
          (core/submit! (:eng s) (list 'jobs.farm.tend {:box box}) {})
          (dotimes [_ 60]
            (swap! (:clock s) + 700)
            (await (core/tick! (:eng s))))
          (is (true? (finished? s)))
          (is (<= (count (calls s "useOn")) 2) "one till child per cell, which tries twice")
          (is (= "dirt" (block-at s 2 63 2))))))))

(deftest bare-farmland-just-outside-the-box-is-not-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "farmland" [[2 2] [5 2] [6 3] [7 4]]) {:inventory [(item "wheat_seeds" 9)]}) 60))]
          (is (= "wheat" (block-at s 2 64 2)))
          (is (= ["air" "air" "air"] [(block-at s 5 64 2) (block-at s 6 64 3) (block-at s 7 64 4)])))))))

(deftest crops-just-outside-the-box-do-not-start-a-run
  (is (declines? {:box box} (world (farm "wheat" 7 [[5 2]] [[5 2]]) wheat-drops)) "ripe outside")
  (is (declines? {:box box :fertilize true} (world (farm "wheat" 3 [[5 2]] [[5 2]]) {:inventory [(item "bone_meal" 3)]})) "unripe outside"))

;; ------------------------------------------------------------------ pure pieces

(deftest reserve-sows-the-field-twice-over-one-seed-at-a-time
  (are [expected beds inventory] (= expected (tend/reserve beds inventory))
    {} 0 [(item "wheat_seeds" 9)]
    {"wheat_seeds" 4} 2 [(item "wheat_seeds" 9)]
    {"wheat_seeds" 3} 2 [(item "wheat_seeds" 3)]
    {"wheat_seeds" 2 "carrot" 2} 2 [(item "wheat_seeds" 9) (item "carrot" 9)]
    {"wheat_seeds" 2 "carrot" 1 "potato" 1} 2 [(item "wheat_seeds" 9) (item "carrot" 1) (item "potato" 5)]
    {"wheat_seeds" 5 "carrot" 1} 3 [(item "wheat_seeds" 5) (item "carrot" 1)]
    {"wheat_seeds" 3} 5 [(item "wheat_seeds" 1) (item "wheat_seeds" 2)]
    {} 4 [(item "bread" 5) (item "melon_seeds" 3)]))

(deftest tools-food-and-meal-are-never-farm-goods
  (are [name] (not (some #{name} tend/farm-goods))
    "stone_hoe" "iron_hoe" "shears" "bread" "bone_meal" "water_bucket"))

(deftest surplus-lists-names-above-their-keep
  (are [expected inventory keep names] (= expected (tend/surplus inventory keep names))
    ["wheat"] [(item "wheat" 2) (item "bread" 4)] {} ["wheat" "carrot"]
    [] [(item "wheat" 2)] {"wheat" 2} ["wheat"]
    ["wheat"] [(item "wheat" 2) (item "wheat" 1)] {"wheat" 2} ["wheat"]
    ["carrot" "wheat"] [(item "wheat" 2) (item "carrot" 1)] {} ["carrot" "wheat"]))

(deftest the-box-geometry
  (are [expected actual] (= expected actual)
    {:x 3 :y 64 :z 3} (tend/centre box)
    3 (tend/radius box (tend/centre box))
    9 (tend/box-cells {:min {:x 0 :y 0 :z 0} :max {:x 2 :y 0 :z 2}})
    true (tend/in-box? box {:x 4 :y 64 :z 2})
    false (tend/in-box? box {:x 5 :y 64 :z 2})
    false (tend/in-box? box {:x 3 :y 65 :z 2})
    true (tend/usable-box? box)
    false (tend/usable-box? nil)
    false (tend/usable-box? {:min {:x 0 :y 63 :z 0} :max {:x 4 :y 63 :z 4}})
    false (tend/usable-box? {:min {:x 0 :y 63 :z 0} :max {:x 60 :y 64 :z 20}})))

(deftest beds-are-farmland-or-uncovered-dirt
  (are [expected ground] (= expected (tend/beds ground))
    2 [{:name "farmland" :above "wheat"} {:name "dirt" :above "air"}]
    1 [{:name "grass_block" :above "short_grass"} {:name "grass_block" :above "stone"}]
    0 [{:name "stone" :above "air"} {:name "dirt" :above nil}]))

(deftest each-step-decides-from-its-facts
  (are [expected step args facts] (= expected (:skip (tend/decide step args facts)))
    :no-ripe :harvest {} {:ripe 0}
    nil :harvest {} {:ripe 1 :mid {:x 0 :y 0 :z 0} :radius 2}
    :till-off :till {:till false} {:hoe true :untilled [{}] :bare 0 :seeds 3}
    :no-hoe :till {:till true} {:hoe false :untilled [{}] :bare 0 :seeds 3}
    :nothing-to-till :till {:till true} {:hoe true :untilled [] :bare 0 :seeds 3}
    :no-seed :till {:till true} {:hoe true :untilled [{}] :bare 2 :seeds 2}
    nil :till {:till true} {:hoe true :untilled [{:x 1 :y 63 :z 1}] :bare 2 :seeds 3}
    :no-bare :plant {} {:bare 0 :seed true}
    :no-seed :plant {} {:bare 1 :seed false}
    nil :plant {:box box} {:bare 1 :seed true}
    :fertilize-off :fertilize {:fertilize false} {:meal true :unripe 2}
    :no-bone-meal :fertilize {:fertilize true} {:meal false :unripe 2}
    :none-unripe :fertilize {:fertilize true} {:meal true :unripe 0}
    nil :fertilize {:fertilize true} {:meal true :unripe 1 :mid {} :radius 2}
    :no-composter :compost {} {:waste ["wheat_seeds"]}
    :no-surplus-seed :compost {:composter {:x 1 :y 1 :z 1}} {:waste []}
    nil :compost {:composter {:x 1 :y 1 :z 1}} {:waste ["wheat_seeds"] :keep {}}
    :no-chest :deposit {} {:stored ["wheat"]}
    :nothing-to-store :deposit {:chest {:x 1 :y 1 :z 1}} {:stored []}
    nil :deposit {:chest {:x 1 :y 1 :z 1}} {:stored ["wheat"] :keep {}}))

(deftest the-till-call-names-the-nearest-cell-and-the-plan-keeps-a-tilled-report
  (let [cell {:x 3 :y 63 :z 2}
        facts {:hoe true :untilled [cell {:x 4 :y 63 :z 2}] :bare 0 :seeds 3}]
    (is (= {:from cell :to cell} (:args (:call (tend/decide :till {:till true} facts)))))
    (is (= {:till {:tilled 2} :plant {:skipped :no-bare}}
           (:report (tend/plan [:till :plant] {:till true} {:hoe true :untilled [] :bare 0 :seeds 3} {:till {:tilled 2}}))))
    (is (= {:till {:skipped :nothing-to-till}}
           (:report (tend/plan [:till] {:till true} {:hoe true :untilled [] :bare 0 :seeds 3} {}))))))

;; ------------------------------------------------------------------ zones

(defn ^:async first-check
  "tend's check on the first tick, in a world whose zone list is zones."
  [args spec zones]
  (let [{:keys [eng checks]} (setup spec zones)]
    (core/submit! eng (list 'jobs.farm.tend (merge {:box box} args)) {})
    (await (core/tick! eng))
    (first @checks)))

(def foreign-zone {:name "keep-out" :min [2 60 2] :max [4 70 4] :owner "Miles" :allow #{}})

(def ripe-world (world (farm "wheat" 7 all-cells [[2 2]]) wheat-drops))

(def bare-world (world (farm "wheat" 7 all-cells []) {:inventory [(item "wheat_seeds" 4)]}))

(deftest a-field-under-a-foreign-zone-does-not-start-a-run-and-an-own-zone-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spec owner start?] [[ripe-world "Miles" false] [bare-world "Miles" false]
                                     [ripe-world "Fake" true] [bare-world "Fake" true]]]
          (is (= start? (await (first-check {} spec [(assoc foreign-zone :owner owner)]))) (pr-str owner)))
        (is (true? (await (first-check {} ripe-world []))) "no zones")))))
