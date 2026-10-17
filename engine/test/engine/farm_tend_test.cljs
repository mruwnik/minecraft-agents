(ns engine.farm-tend-test
  "jobs.farm.tend against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.farm.tend :as tend]
            [jobs.farm.tend-decide :as decide]
            [jobs.farm.tend-stock :as stock]
            [jobs.lib.look :as look]
            [jobs.lib.steps :as steps]
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
  "An engine over a fake world."
  ([spec] (setup spec nil))
  ([spec zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor spec))
        eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           zones (assoc :world (world/of-data {} {} zones))))]
    {:eng eng :p p :seen seen :clock clock})))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args spec n]
  (let [s (setup spec)]
    (core/submit! (:eng s) (list 'jobs.farm.tend (merge {:box box} args)) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(defn calls [{:keys [p]} name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-events [s] (filterv #(#{:completed :stopped} (:kind %)) @(:seen s)))
(defn done-event [s] (first (done-events s)))
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

(def under {:blocks (into {} (map (fn [[x z]] [(k x 62 z) "dirt"])) all-cells)})

(deftest the-irrigate-call-covers-the-ground-layer-of-the-box
  (is (= {:from (:min box) :to {:x 4 :y 63 :z 4}}
         (:args (:call (decide/decide :irrigate {:irrigate true :box box} {:dry 2}))))))

(deftest a-dry-field-is-wetted-before-it-is-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "farmland" all-cells) under
                                           {:inventory [(item "water_bucket" 1) (item "wheat_seeds" 9)]}) 120))]
          (is (= {:poured 1} (step s :irrigate)))
          (is (= 1 (count (filter #(= "water" (block-at s (first %) 63 (second %))) all-cells))))
          (is (= 8 (:planted (step s :plant)))))))))

(deftest a-field-with-irrigate-off-is-left-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:irrigate false} (world (ground "farmland" all-cells)
                                                          {:inventory [(item "water_bucket" 1) (item "wheat_seeds" 9)]}) 80))]
          (is (= {:skipped :irrigate-off} (step s :irrigate)))
          (is (= 9 (:planted (step s :plant)))))))))

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

(deftest one-call-tills-plants-and-reports
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "dirt" [[2 2] [3 2]])
                                           {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 5)]}) 1))]
          (is (= {:tilled 2} (step s :till)))
          (is (= 2 (:planted (step s :plant))))
          (is (true? (finished? s))))))))

(deftest food-crops-below-the-reserve-are-neither-tilled-nor-sown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (ground "dirt" [[2 2] [3 2]]) (ground "farmland" [[4 2]])
                                           {:inventory [(item "stone_hoe" 1) (item "carrot" 15)]}) 40))]
          (is (empty? (calls s "useOn")))
          (is (empty? (calls s "place")))
          (is (= "dirt" (block-at s 2 63 2)))
          (is (= {"stone_hoe" 1 "carrot" 15} (inv s))))))))

(deftest a-harvested-food-cell-is-replanted-below-the-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (farm "carrots" 7 [[2 2] [3 2]] [[2 2]])
                                           {:drops {"carrots" ["carrot"]}}) 80))]
          (is (= 1 (count (calls s "place"))))
          (is (= "carrots" (block-at s 2 64 2)))
          (is (= "air" (block-at s 3 64 2))))))))

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
  (let [mature (world (farm "wheat" 3 all-cells all-cells) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 4) (item "bone_meal" 3)]})]
    (is (declines? {:box box} mature) "unripe crops, fertilize off by default")
    (is (declines? {:box box :fertilize true :till false} (world (farm "wheat" 3 all-cells all-cells))) "bone meal wanted but none carried")
    (is (declines? {:box box :till false} (world (ground "dirt" [[2 2]]) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 4)]})) "till off")))

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
          (is (= {"wheat" 5} (chest-items s)) "a stack of backup seed and the 3-day food stay carried")
          (is (= {"wheat_seeds" 10 "iron_hoe" 1 "bread" 2} (inv s)))
          (is (= {:gave-up false} (select-keys (step s :deposit) [:gave-up])))
          (is (true? (finished? s))))))))

(deftest keep-is-honoured-above-the-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest :keep {"wheat" 2 "wheat_seeds" 1}} goods-world 40))]
          (is (= {"wheat" 3} (chest-items s)))
          (is (= 2 (get (inv s) "wheat")))
          (is (= 10 (get (inv s) "wheat_seeds"))))))))

(deftest only-seed-above-the-backup-stack-is-composted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:composter {:x 6 :y 64 :z 2}}
                                 (world goods-world {:blocks {"6,64,2" "composter"}}
                                        {:inventory [(item "wheat" 5) (item "wheat_seeds" 70) (item "bread" 2)]}) 60))]
          (is (= {"wheat_seeds" 6} (:fed (step s :compost))))
          (is (= 64 (get (inv s) "wheat_seeds")))
          (is (= 2 (get (inv s) "bread")))
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

(deftest bones-are-crafted-into-bone-meal-when-fertilizing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:fertilize true} (world (farm "wheat" 3 [[2 2] [3 2]] [[2 2] [3 2]]) {:inventory [(item "bone" 2)] :recipes {"bone_meal" {:count 3 :needs {"bone" 1}}}}) 80))]
          (is (pos? (:used (step s :fertilize))))
          (is (> (age-at s 2 64 2) 3)))))))

(deftest an-unreachable-ripe-crop-is-given-up-and-the-run-ends-with-a-result
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world (farm "wheat" 7 [[4 4]] [[4 4]]) {:unreachable [(k 4 64 4)]}) 80))]
          (is (empty? (calls s "dig")))
          (is (some? (step s :harvest)))
          (is (= {:crops 1 :bare 0 :untilled 0} (:field (done-event s))))
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
          (await (tu/run-ticks (:eng s) (:clock s) 60 700))
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

(deftest the-keep-holds-sowing-seed-a-backup-stack-per-seed-and-the-food-reserve
  (are [expected sow inventory keep] (= expected (stock/keeps sow inventory keep))
    {"wheat_seeds" 64 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 64} {} [] {}
    {"wheat_seeds" 100 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 64} {"wheat_seeds" 100} [] {}
    {"wheat_seeds" 64 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 64 "bread" 4 "carrot" 9}
    {"carrot" 2} [(item "bread" 4) (item "carrot" 9)] {}
    {"wheat_seeds" 64 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 70} {} [] {"pumpkin_seeds" 70}))

(deftest crops-are-stored-down-to-the-food-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest}
                                 (world (farm "wheat" 3 [[2 2]] [[2 2]])
                                        {:inventory [(item "carrot" 20) (item "bread" 3)] :containers {"10,64,0" []}}) 40))]
          (is (= {"carrot" 5} (chest-items s)) "bread 3 is 15 of the 60 points, so 15 carrots (45 points) stay")
          (is (= {"carrot" 15 "bread" 3} (inv s))))))))

(deftest surplus-lists-names-above-their-keep
  (are [expected inventory keep names] (= expected (stock/surplus inventory keep names))
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
  (are [expected step args facts] (= expected (:skip (decide/decide step args facts)))
    :no-ripe :harvest {} {:ripe 0}
    nil :harvest {} {:ripe 1 :mid {:x 0 :y 0 :z 0} :radius 2}
    :till-off :till {:till false} {:hoe true :untilled [{}] :bare 0 :seeds 3}
    :no-hoe :till {:till true} {:hoe false :untilled [{}] :bare 0 :seeds 3}
    :nothing-to-till :till {:till true} {:hoe true :untilled [] :bare 0 :seeds 3}
    :no-seed :till {:till true} {:hoe true :untilled [{}] :bare 2 :seeds 2}
    nil :till {:till true} {:hoe true :untilled [{:x 1 :y 63 :z 1}] :bare 2 :seeds 3}
    :irrigate-off :irrigate {:irrigate false :box box} {:dry 3}
    :not-dry :irrigate {:irrigate true :box box} {:dry 0}
    :not-dry :irrigate {:irrigate true} {}
    nil :irrigate {:irrigate true :box box} {:dry 3}
    :no-bare :plant {} {:bare 0 :seed true}
    :no-seed :plant {} {:bare 1 :seed false}
    nil :plant {:box box} {:bare 1 :seed true}
    :fertilize-off :fertilize {:fertilize false} {:meal true :unripe 2}
    :no-bone-meal :fertilize {:fertilize true} {:meal false :unripe 2}
    nil :fertilize {:fertilize true} {:meal false :bones true :unripe 2 :mid {} :radius 2}
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
    (is (= {:from cell :to cell} (:args (:call (decide/decide :till {:till true} facts)))))
    (is (= {:till {:tilled 2} :plant {:skipped :no-bare}}
           (:report (steps/plan decide/decide [:till :plant] {:till true} {:hoe true :untilled [] :bare 0 :seeds 3} {:till {:tilled 2}}))))
    (is (= {:till {:skipped :nothing-to-till}}
           (:report (steps/plan decide/decide [:till] {:till true} {:hoe true :untilled [] :bare 0 :seeds 3} {}))))))

(deftest a-contract-child-refused-before-it-started-counts-as-declined
  (are [e refused] (= refused (tend/refused? e))
    {:kind :condition-failed :cause {:reason :no-zones}} true
    {:kind :condition-failed :cause :no-composter :data {:fed {}}} false
    {:kind :nothing-possible :cause :unreachable} false
    {:kind :done :data {:tilled 2}} false)
  (is (= {:till {:skipped :declined}}
         (:report (tend/after-child {:report {}} :till {:from {:x 1 :y 63 :z 1}} {:kind :condition-failed :cause {:reason :no-zones}})))))

(deftest a-child-that-hit-a-limit-ends-the-step-and-the-run-with-its-cause
  (let [m (tend/after-child {:todo [:harvest :till] :report {}} :harvest {} {:kind :limit-reached :cause {:limit :job-time} :data {:cut 1}})]
    (is (= {:limit :job-time} (:limit m)))
    (is (= {:harvest {:cut 1}} (:report m)))
    (is (= [:till] (:todo m)))))

(deftest a-step-counts-as-worked-only-when-it-changed-something
  (are [step summary worked] (= worked (tend/worked? step summary))
    :harvest {:cut 0 :replanted 0 :gave-up true} false
    :harvest {:cut 1} true
    :irrigate {:poured 1} true
    :irrigate {:poured 0} false
    :till {:tilled 0} false
    :plant {:planted 2} true
    :plant {:skipped :no-bare} false
    :fertilize {:used 0} false
    :compost {:fed {"wheat_seeds" 6}} true
    :compost {:fed {}} false
    :deposit {:gave-up false} true
    :deposit {:gave-up true} false
    :deposit {:skipped :no-chest} false))

;; ------------------------------------------------------------------ zones

(defn ^:async first-check
  "Whether tend starts a round on the first tick, in a world whose zone list is zones."
  [args spec zones]
  (let [s (setup spec zones)]
    (core/submit! (:eng s) (list 'jobs.farm.tend (merge {:box box} args)) {})
    (await (core/tick! (:eng s)))
    (boolean (seq (events-of s :round_started)))))

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

(deftest a-field-never-seen-is-looked-at-before-the-run-decides
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 7 all-cells [[2 2] [3 2]]) wheat-drops))
              _ (tu/seeing-after-look (:p s))]
          (core/submit! (:eng s) (list 'jobs.farm.tend {:box box}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 80 700))
          (is (seq (calls s "look")))
          (is (= #{[2 2] [3 2]} (dug s)))
          (is (true? (finished? s))))))))

(deftest a-seen-field-with-nothing-to-do-still-waits-after-the-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 3 all-cells all-cells)))
              _ (tu/seeing-after-look (:p s))]
          (core/submit! (:eng s) (list 'jobs.farm.tend {:box box}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 6 700))
          (is (seq (calls s "look")))
          (is (= [:nothing-to-do] (distinct (map :reason (events-of s :waiting)))))
          (is (empty? (dug s))))))))

(deftest nothing-to-do-says-what-blocks-the-tilling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [terrain (world (ground "dirt" all-cells) (crops "grass_block" 0 all-cells)
                             {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 4)]})
              s (setup terrain)
              _ (tu/seeing-after-look (:p s))]
          (core/submit! (:eng s) (list 'jobs.farm.tend {:box box :till true}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 6 700))
          (let [w (last (events-of s :waiting))]
            (is (= :nothing-to-do (:reason w)))
            (is (= 9 (:covered w)) "9 tillable cells have a block over them: the box sits too low")))))))

(def far-box {:min {:x 202 :y 63 :z 2} :max {:x 204 :y 64 :z 4}})
(def far-ripe
  (world {:blocks (into {} (map (fn [[x z]] [(k (+ x 200) 63 z) "farmland"])) all-cells)}
         {:blocks (into {} (map (fn [[x z]] [(k (+ x 200) 64 z) "wheat"])) all-cells)
          :ages (into {} (map (fn [[x z]] [(k (+ x 200) 64 z) 7])) all-cells)}))

(defn ^:async far-run
  "Ripe wheat 200 blocks off; wait-for-dusk (listed, check false by day) first when listed?."
  [listed?]
  (let [s (setup far-ripe)]
    (when listed? (core/submit! (:eng s) '(jobs.time.wait-for-dusk) {}))
    (core/submit! (:eng s) (list 'jobs.farm.tend {:box far-box}) {})
    (await (tu/run-ticks (:eng s) (:clock s) 3 700))
    s))

(deftest a-tend-far-from-its-field-declines-while-another-job-is-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (far-run true))]
          (is (empty? (events-of s :round_started)))
          (is (some #(= :away (:reason %)) (events-of s :waiting))))))))

(deftest a-tend-far-from-its-field-runs-when-nothing-else-is-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (seq (events-of (await (far-run false)) :round_started)))))))

(def high-box {:min {:x 2 :y 93 :z 2} :max {:x 4 :y 94 :z 4}})

(deftest a-tend-far-below-its-field-declines-though-nothing-else-is-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup far-ripe)]
          (core/submit! (:eng s) (list 'jobs.farm.tend {:box high-box}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 3 700))
          (is (empty? (events-of s :round_started)))
          (is (some #(= :away (:reason %)) (events-of s :waiting))))))))

;; ------------------------------------------------------------------ contract

(defn ^:async ending-of
  "Run tend with args as a ctx/call child of a recording parent in setup s, at most n ticks: the child's {:kind :cause :data}."
  [s args n]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid 'jobs.farm.tend args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) (ctx/end c :done nil)))))}
        eng (assoc-in (:eng s) [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-ticks eng (:clock s) n 700))
    @out))

(deftest tend-exports-the-contract-and-quotes-validly
  (let [job (get registry/jobs 'jobs.farm.tend)]
    (is (fn? (:can-start? job)))
    (is (vector? (:target job)))
    (is (fn? (:done? job)))
    (is (nil? (contract/quote-problem ((:quote-of job) {:primitives (tu/fake {}) :args {:box box}}))))))

(deftest a-run-that-worked-is-done-with-its-steps-and-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 7 all-cells [[2 2] [3 2]]) wheat-drops))
              e (await (ending-of s {:box box} 80))]
          (is (= :done (:kind e)))
          (is (= 2 (get-in e [:data :steps :harvest :cut])))
          (is (= {:crops 2 :bare 7 :untilled 0} (get-in e [:data :field]))))))))

(deftest a-call-with-nothing-to-do-fails-its-condition-and-a-box-that-cannot-hold-a-field-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 3 all-cells all-cells)))]
          (is (= {:kind :condition-failed :cause {:reason :nothing-to-do}} (update (select-keys (await (ending-of s {:box box} 20)) [:kind :cause]) :cause dissoc :text))))
        (let [s (setup (world (farm "wheat" 7 all-cells all-cells)))]
          (is (= {:kind :impossible :cause {:reason :no-box}} (update (select-keys (await (ending-of s {} 20)) [:kind :cause]) :cause dissoc :text)))
          (is (= :impossible (:kind (await (ending-of s {:box {:min {:x 2 :y 63 :z 2} :max {:x 4 :y 63 :z 4}}} 20)))) "no crop layer")
          (is (= :impossible (:kind (await (ending-of s {:box {:min {:x 0 :y 63 :z 0} :max {:x 60 :y 64 :z 20}}} 20)))) "over the cell cap"))))))

(deftest a-run-that-gave-up-on-everything-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 7 [[4 4]] [[4 4]]) {:unreachable [(k 4 64 4)]}))
              e (await (ending-of s {:box box} 80))]
          (is (= {:kind :nothing-possible :cause :gave-up} (select-keys e [:kind :cause])))
          (is (some? (get-in e [:data :steps :harvest]))))))))

(deftest tend-hands-its-chest-argument-to-deposit
  (let [given [-545 64 324]]
    (is (= given (get-in (decide/decide :deposit {:chest given} {:stored ["wheat"] :keep {}}) [:call :args :chest])))))

(deftest a-part-with-no-step-is-a-look-until-the-body-has-looked-round
  (let [c {:primitives (tu/fake {}) :args {:box box}}
        status (fn [surveyed?] (with-redefs [ctx/mem (constantly {}) tend/check-facts (constantly {}) tend/would-run? (constantly false)
                                             tend/needs-look? (constantly true) look/surveyed? (fn ([_] surveyed?) ([_ _] surveyed?))]
                                 (tend/part-status c)))]
    (is (= [:look :nothing-to-do] (tu/kind-reason (status false))))
    (is (= [:later :nothing-to-do] (tu/kind-reason (status true))))))
