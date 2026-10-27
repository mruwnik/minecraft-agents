(ns engine.farm-tend-test
  "jobs.farm.tend against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.farm.tend :as tend]
            [jobs.farm.tend-box :as tend-box]
            [jobs.farm.tend-decide :as decide]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.field-area :as fa]
            [jobs.lib.look :as look]
            [jobs.lib.steps :as steps]
            [jobs.lib.walk-pick :as pick]
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
    (core/submit! (:eng s) (list 'jobs.farm.tend (merge {:area box} args)) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(defn calls [{:keys [p]} name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-events [s] (filterv #(#{:completed :stopped} (:kind %)) @(:seen s)))
(defn done-event [s] (first (done-events s)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))
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
          (is (= {:skipped :nothing-to-till} (step s :till)))
          (is (true? (finished? s))))))))

(deftest a-box-with-vector-corners-works-the-same-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:area {:min [2 63 2] :max [4 64 4]}} (world (farm "wheat" 7 all-cells [[2 2] [3 2]]) (crops "wheat" 3 [[4 2]]) wheat-drops) 80))]
          (is (= #{[2 2] [3 2]} (dug s)))
          (is (true? (finished? s))))))))

(def under {:blocks (into {} (map (fn [[x z]] [(k x 62 z) "dirt"])) all-cells)})

(deftest the-irrigate-call-covers-the-ground-layer-of-the-box
  (is (= {:area {:min (:min box) :max {:x 4 :y 63 :z 4}}}
         (:args (:call (decide/decide :irrigate {:irrigate true :area box} {:dry 2 :ground-box {:min (:min box) :max {:x 4 :y 63 :z 4}}}))))))

(deftest the-irrigate-call-of-a-plan-names-the-plan
  (let [gb {:min {:x 2 :y 63 :z 2} :max {:x 3 :y 63 :z 3}}]
    (is (= {:area {:min (:min gb) :max (:max gb)} :for-plan "mix"}
           (:args (:call (decide/decide :irrigate {:irrigate true :area {:plan "mix"}} {:dry 2 :ground-box gb})))))))

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

(deftest without-a-hoe-and-carried-only-ways-the-dirt-is-left-and-the-bare-farmland-still-planted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:ways #{:carried}} (world (ground "dirt" [[2 2]]) (ground "farmland" [[3 2]])
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
    (is (declines? {:area box} mature) "unripe crops, fertilize off by default")
    (is (declines? {:area box :fertilize true :till false} (world (farm "wheat" 3 all-cells all-cells))) "bone meal wanted but none carried")
    (is (declines? {:area box :till false} (world (ground "dirt" [[2 2]]) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 4)]})) "till off")))

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
          (core/submit! (:eng s) (list 'jobs.farm.tend {:area box}) {})
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
  (is (declines? {:area box} (world (farm "wheat" 7 [[5 2]] [[5 2]]) wheat-drops)) "ripe outside")
  (is (declines? {:area box :fertilize true} (world (farm "wheat" 3 [[5 2]] [[5 2]]) {:inventory [(item "bone_meal" 3)]})) "unripe outside"))

;; ------------------------------------------------------------------ pure pieces

(deftest the-box-geometry
  (are [expected actual] (= expected actual)
    {:x 3 :y 64 :z 3} (fa/centre box)
    3 (fa/cover-radius box (fa/centre box))
    9 (fa/cell-count {:min {:x 0 :y 0 :z 0} :max {:x 2 :y 0 :z 2}})
    true (fa/contains-pos? box {:x 4 :y 64 :z 2})
    false (fa/contains-pos? box {:x 5 :y 64 :z 2})
    false (fa/contains-pos? box {:x 3 :y 65 :z 2})
    true (tend/usable-box? box)
    false (tend/usable-box? nil)
    false (tend/usable-box? {:min {:x 0 :y 63 :z 0} :max {:x 4 :y 63 :z 4}})
    false (tend/usable-box? {:min {:x 0 :y 63 :z 0} :max {:x 60 :y 64 :z 20}})))

(deftest each-step-decides-from-its-facts
  (are [expected step args facts] (= expected (:skip (decide/decide step args facts)))
    :no-ripe :harvest {} {:ripe 0}
    nil :harvest {} {:ripe 1}
    :till-off :till {:till false} {:hoe true :untilled [{}] :bare 0 :seeds 3}
    :no-hoe :till {:till true :ways #{:carried}} {:hoe false :untilled [{}] :bare 0 :seeds 3}
    nil :till {:till true :ways #{:carried :craft}} {:hoe false :untilled [{}] :bare 0 :seeds 3}
    nil :till {:till true} {:hoe false :untilled [{}] :bare 0 :seeds 3}
    :nothing-to-till :till {:till true} {:hoe true :untilled [] :bare 0 :seeds 3}
    :no-seed :till {:till true} {:hoe true :untilled [{}] :bare 2 :seeds 2}
    nil :till {:till true} {:hoe true :untilled [{:x 1 :y 63 :z 1}] :bare 2 :seeds 3}
    :irrigate-off :irrigate {:irrigate false :area box} {:dry 3}
    :not-dry :irrigate {:irrigate true :area box} {:dry 0}
    :not-dry :irrigate {:irrigate true} {}
    nil :irrigate {:irrigate true :area box} {:dry 3}
    :no-bare :plant {} {:bare 0 :seed true}
    :no-seed :plant {} {:bare 1 :seed false}
    nil :plant {:area box} {:bare 1 :seed true}
    :fertilize-off :fertilize {:fertilize false} {:meal true :unripe 2}
    :no-bone-meal :fertilize {:fertilize true} {:meal false :unripe 2}
    nil :fertilize {:fertilize true} {:meal false :bones true :unripe 2}
    :none-unripe :fertilize {:fertilize true} {:meal true :unripe 0}
    nil :fertilize {:fertilize true} {:meal true :unripe 1}))

(deftest the-till-call-names-the-nearest-cell-and-the-plan-keeps-a-tilled-report
  (let [cell {:x 3 :y 63 :z 2}
        facts {:hoe true :untilled [cell {:x 4 :y 63 :z 2}] :bare 0 :seeds 3}]
    (is (= {:area {:min cell :max cell}} (:args (:call (decide/decide :till {:till true} facts)))))
    (is (= {:till {:tilled 2} :plant {:skipped :no-bare}}
           (:report (steps/plan decide/decide [:till :plant] {:till true} {:hoe true :untilled [] :bare 0 :seeds 3} {:till {:tilled 2}}))))
    (is (= {:till {:skipped :nothing-to-till}}
           (:report (steps/plan decide/decide [:till] {:till true} {:hoe true :untilled [] :bare 0 :seeds 3} {}))))))

(deftest a-contract-child-refused-before-it-started-counts-as-declined
  (are [e refused] (= refused (tend/refused? e))
    {:kind :condition-failed :cause {:reason :no-zones}} true
    {:kind :condition-failed :cause :no-hoe :data {:tilled 0}} false
    {:kind :nothing-possible :cause :unreachable} false
    {:kind :done :data {:tilled 2}} false)
  (is (= {:till {:skipped :declined}}
         (:report (tend/after-child {:report {}} :till {:area {:min {:x 1 :y 63 :z 1}}} {:kind :condition-failed :cause {:reason :no-zones}})))))

(deftest a-child-that-hit-its-own-limit-is-booked-and-the-run-goes-on
  (let [m (tend/after-child {:todo [:harvest :till] :report {}} :harvest {} {:kind :limit-reached :cause {:limit :job-time} :data {:cut 1}})]
    (is (not (contains? m :limit)))
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
    :fertilize {:used 0} false))

;; ------------------------------------------------------------------ zones

(defn ^:async first-check
  "Whether tend starts a round on the first tick, in a world whose zone list is zones."
  [args spec zones]
  (let [s (setup spec zones)]
    (core/submit! (:eng s) (list 'jobs.farm.tend (merge {:area box} args)) {})
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
          (core/submit! (:eng s) (list 'jobs.farm.tend {:area box}) {})
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
          (core/submit! (:eng s) (list 'jobs.farm.tend {:area box}) {})
          (await (tu/with-looks-showing #(tu/run-ticks (:eng s) (:clock s) 6 700)))
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
          (core/submit! (:eng s) (list 'jobs.farm.tend {:area box :till true}) {})
          (await (tu/with-looks-showing #(tu/run-ticks (:eng s) (:clock s) 6 700)))
          (let [w (last (events-of s :waiting))]
            (is (= :nothing-to-do (:reason w)))
            (is (= 9 (:covered w)) "9 tillable cells have a block over them: the box sits too low")))))))

(deftest nothing-to-do-over-a-box-names-who-refuses-the-tilling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (ground "dirt" all-cells) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 4)]}) [foreign-zone])
              _ (tu/seeing-after-look (:p s))]
          (core/submit! (:eng s) (list 'jobs.farm.tend {:area box :till true}) {})
          (await (tu/with-looks-showing #(tu/run-ticks (:eng s) (:clock s) 6 700)))
          (let [w (last (events-of s :waiting))]
            (is (= :nothing-to-do (:reason w)))
            (is (= 9 (:refused w)))
            (is (= ["Miles"] (:owners w)))))))))

(defn far-box [dx] {:min {:x (+ 2 dx) :y 63 :z 2} :max {:x (+ 4 dx) :y 64 :z 4}})
(defn far-ripe [dx]
  (world {:blocks (into {} (map (fn [[x z]] [(k (+ x dx) 63 z) "farmland"])) all-cells)}
         {:blocks (into {} (map (fn [[x z]] [(k (+ x dx) 64 z) "wheat"])) all-cells)
          :ages (into {} (map (fn [[x z]] [(k (+ x dx) 64 z) 7])) all-cells)}))

(defn ^:async far-run
  "Nine ripe wheat seen dx blocks off; safe? always, so the price alone decides."
  [dx]
  (let [s (setup (assoc (far-ripe dx) :time 1000))]
    (core/submit! (:eng s) (list 'jobs.farm.tend {:area (far-box dx) :checks {:safe? '(always)}}) {})
    (await (tu/run-ticks (:eng s) (:clock s) 3 700))
    s))

(deftest a-tend-runs-when-its-ripe-crops-are-worth-the-least-walk-to-the-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (seq (events-of (await (far-run 150)) :round_started)) "nine ripe wheat are worth more than 150 blocks' walk")))))

(deftest a-tend-waits-not-worth-while-the-least-walk-to-its-seen-field-costs-more-than-its-crops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (far-run 400))]
          (is (empty? (events-of s :round_started)))
          (is (some #(= :not-worth (:reason %)) (events-of s :waiting))))))))

(def high-box {:min {:x 202 :y 93 :z 2} :max {:x 204 :y 94 :z 4}})

(def high-ripe
  (world {:blocks (into {} (map (fn [[x z]] [(k (+ x 200) 93 z) "farmland"])) all-cells)}
         {:blocks (into {} (map (fn [[x z]] [(k (+ x 200) 94 z) "wheat"])) all-cells)
          :ages (into {} (map (fn [[x z]] [(k (+ x 200) 94 z) 7])) all-cells)}))

(defn ^:async below-run
  "A tend far below its field with nothing else listed, kept under repeat or alone."
  [repeat?]
  (let [s (setup high-ripe)
        job (list 'jobs.farm.tend {:area high-box})]
    (core/submit! (:eng s) (if repeat? (list 'repeat job) job) {})
    (await (tu/run-ticks (:eng s) (:clock s) 3 700))
    s))

(deftest a-tend-under-repeat-far-below-its-ripe-field-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (seq (events-of (await (below-run true)) :round_started)))))))

(def unseen-cells (for [x (range 30 35) z (range 2 7)] [x z]))

(deftest a-tend-under-repeat-walks-to-a-field-it-has-not-seen-when-its-share-of-ripe-crops-pays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (assoc (world (farm "wheat" 7 unseen-cells unseen-cells) wheat-drops) :floor [-5 -5 60 15]))]
          (tu/seeing-within (:p s) 24)
          (core/submit! (:eng s) (list 'repeat (list 'jobs.farm.tend {:area {:min {:x 30 :y 63 :z 2} :max {:x 34 :y 64 :z 6}}})) {})
          (await (tu/run-ticks (:eng s) (:clock s) 60 700))
          (is (seq (tu/walk-calls (:p s))) "the body goes to the field")
          (is (= (set (map (fn [[x z]] [x z]) unseen-cells)) (dug s)) "and cuts the wheat there"))))))

(def hidden-cells (for [x (range 12 15) z (range 2 5)] [x z]))

(deftest a-tend-under-repeat-walks-to-a-near-field-no-look-from-here-shows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (assoc (world (farm "wheat" 7 hidden-cells hidden-cells) wheat-drops) :floor [-5 -5 60 15]))]
          (tu/seeing-within (:p s) 10)
          (core/submit! (:eng s) (list 'repeat (list 'jobs.farm.tend {:area {:min {:x 12 :y 63 :z 2} :max {:x 14 :y 64 :z 4}} :irrigate false})) {})
          (await (tu/run-ticks (:eng s) (:clock s) 60 700))
          (is (seq (tu/walk-calls (:p s))) "13 blocks off and unseen: the body goes to the field")
          (is (= (set hidden-cells) (dug s)) "and cuts the wheat there"))))))

(deftest a-tend-waits-not-worth-while-the-least-walk-to-its-unseen-field-costs-more-than-the-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 7 [[300 2]] [[300 2]]) wheat-drops))]
          (tu/seeing-within (:p s) 24)
          (core/submit! (:eng s) (list 'repeat (list 'jobs.farm.tend {:area {:min {:x 300 :y 63 :z 2} :max {:x 300 :y 64 :z 2}} :irrigate false})) {})
          (await (tu/run-ticks (:eng s) (:clock s) 5 700))
          (is (empty? (tu/walk-calls (:p s))) "one crop 300 blocks off is not worth the walk")
          (is (some #(= :not-worth (:reason %)) (events-of s :waiting))))))))

(deftest a-walk-quote-over-the-worth-is-remembered-not-asked-again-each-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (assoc (world (farm "wheat" 7 hidden-cells hidden-cells) wheat-drops) :floor [-5 -5 60 15]))
              asked (atom 0)
              quoted pick/quoted-s]
          (tu/seeing-within (:p s) 10)
          (set! pick/quoted-s (fn [_c _q] (swap! asked inc) 1000))
          (try
            (core/submit! (:eng s) (list 'repeat (list 'jobs.farm.tend {:area {:min {:x 12 :y 63 :z 2} :max {:x 14 :y 64 :z 4}} :irrigate false})) {})
            (await (tu/run-ticks (:eng s) (:clock s) 400 700))
            (finally (set! pick/quoted-s quoted)))
          (is (= 1 @asked) "the walk is quoted once in 280 s, not again after each rest")
          (is (empty? (tu/walk-calls (:p s))))
          (is (some #(and (= :not-worth (:reason %)) (:quoted %)) (events-of s :waiting))))))))

(defn pass-ctx
  "A check ctx of tend over the box from x0 to x1 (z 2) in setup s."
  [s x0 x1]
  {:args {:area {:min {:x x0 :y 63 :z 2} :max {:x x1 :y 64 :z 2}} :till true :irrigate false} :primitives (:p s)
   :id "t" :root "t" :slots [] :view (fn [] {:now 0 :data {}}) :engine {:warned (atom #{})}})

(deftest a-walk-quote-is-keyed-on-the-site-not-its-cell-above-and-on-the-light
  (let [pos {:x 12 :y 63 :z 2}
        key-at (fn [time] (tend/quote-key (pass-ctx (setup (assoc (world (farm "wheat" 7 [[2 2]] [[2 2]])) :time time)) 2 3) pos))]
    (is (= (key-at 1000) (tend/quote-key (pass-ctx (setup (assoc (world (farm "wheat" 7 [[2 2]] [[2 2]])) :time 1000)) 2 3) (update pos :y inc)))
        "the check's walk and the run's walk share one key")
    (is (not= (key-at 1000) (key-at 18000)) "a quote taken at night is not the day's quote")))

(def dry-rate (crops/stages-per-s false nil))

(deftest a-pass-is-worth-the-crops-it-expects-ripe-and-their-replanting
  (let [s (setup (world (farm "wheat" 7 [[2 2]] [[2 2]]) (farm "wheat" 3 [[3 2]] [[3 2]])))
        c (pass-ctx s 2 3)]
    (is (= (+ (cost/crop-worth "wheat") (cost/sown-worth "wheat" dry-rate)) (tend/pass-worth c))
        "a ripe crop in view counts whole and is sown again, an unripe one in view not at all")))

(deftest a-cell-never-seen-is-worth-the-measured-share-of-ripe-crops-not-a-ripe-crop
  (let [s (setup (world (farm "wheat" 7 [[2 2] [3 2]] [[2 2] [3 2]])))
        c (pass-ctx s 2 3)
        least (apply min (map cost/crop-worth (keys crops/ripe-age)))]
    (tu/seeing-within (:p s) 0)
    (is (< 0 (cost/field-share :ripe 0 0) 0.5))
    (is (= (* 2 (cost/field-share :ripe 0 0) least) (tend/pass-worth c)))))

(deftest a-pass-that-sows-bare-farmland-is-worth-the-crops-it-sets-up
  (let [s (setup (world (farm "wheat" 0 [[2 2] [3 2] [4 2]] []) {:inventory [(item "wheat_seeds" 2)]}))
        c (pass-ctx s 2 4)]
    (is (pos? (cost/sown-worth "wheat" dry-rate)))
    (is (= (* 2 (cost/sown-worth "wheat" dry-rate)) (tend/pass-worth c)) "two seeds sow two of the three bare cells")))

(deftest a-pass-quotes-the-measured-work-of-its-steps
  (let [s (setup (world (farm "wheat" 7 [[2 2]] [[2 2]]) (farm "wheat" 3 [[3 2]] [[3 2]])))]
    (is (= [0 (cost/field-step-s :harvest 1) ##Inf] (get-in (tend/quote-of (pass-ctx s 2 3)) [:uses :job-time])))))

(deftest a-crop-seen-unripe-ripens-by-the-games-growth-rate
  (let [wet (crops/stages-per-s true nil)]
    (is (= 1 (crops/ripe-chance "wheat" 7 0 wet)))
    (is (= 0 (crops/ripe-chance "wheat" 0 0 wet)))
    (is (< 0.4 (crops/ripe-chance "wheat" 0 (/ 7 wet) wet) 0.6) "about even odds after the mean time of the stages left")
    (is (> (crops/ripe-chance "wheat" 6 600 wet) (crops/ripe-chance "wheat" 0 600 wet)) "fewer stages left ripen sooner")
    (is (> (crops/ripe-chance "wheat" 4 600 wet) (crops/ripe-chance "wheat" 4 600 dry-rate)) "dry farmland grows slower")
    (is (= 0 (crops/ripe-chance "wheat" 4 6000 (crops/stages-per-s true 8))) "under light 9 nothing grows")))

(deftest a-sown-crop-is-worth-its-share-ripe-within-the-food-horizon
  (let [wet (crops/stages-per-s true nil)]
    (is (= 1 (cost/grown-share "wheat" 7 wet)))
    (is (< 0 (cost/grown-share "wheat" 0 dry-rate) (cost/grown-share "wheat" 0 wet) 1))
    (is (= 0 (cost/sown-worth "wheat" 0)) "a crop that never grows is not worth its seed")))

(deftest a-lone-tend-far-below-its-field-walks-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (seq (events-of (await (below-run false)) :round_started)))))))

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

(deftest blocked-cells-at-ground-level-leave-the-field-in-order
  (with-redefs [tend/part-status (constantly [:later :x {:reason :nothing-to-do :blocked 2 :ground-level 2 :text "t"}])]
    (is (tend/in-order? nil))))

(deftest tend-exports-the-contract-and-quotes-validly
  (let [job (get registry/jobs 'jobs.farm.tend)]
    (is (fn? (:can-start? job)))
    (is (vector? (:target job)))
    (is (fn? (:done? job)))
    (is (nil? (contract/quote-problem ((:quote-of job) {:primitives (tu/fake {}) :args {:area box} :view (fn [] {:now 0 :data {}}) :engine {:warned (atom #{})}}))))))

(deftest a-run-that-worked-is-done-with-its-steps-and-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 7 all-cells [[2 2] [3 2]]) wheat-drops))
              e (await (ending-of s {:area box} 80))]
          (is (= :done (:kind e)))
          (is (= 2 (get-in e [:data :steps :harvest :cut])))
          (is (= {:crops 2 :bare 7 :untilled 0} (get-in e [:data :field]))))))))

(deftest a-call-with-nothing-to-do-fails-its-condition-and-a-box-that-cannot-hold-a-field-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 3 all-cells all-cells)))]
          (is (= {:kind :condition-failed :cause {:reason :nothing-to-do}} (update (select-keys (await (ending-of s {:area box} 20)) [:kind :cause]) :cause dissoc :text))))
        (let [s (setup (world (farm "wheat" 7 all-cells all-cells)))]
          (is (= {:kind :impossible :cause {:reason :no-box}} (update (select-keys (await (ending-of s {} 20)) [:kind :cause]) :cause dissoc :text)))
          (is (= :impossible (:kind (await (ending-of s {:area {:min {:x 2 :y 63 :z 2} :max {:x 4 :y 63 :z 4}}} 20)))) "no crop layer")
          (is (= :impossible (:kind (await (ending-of s {:area {:min {:x 0 :y 63 :z 0} :max {:x 60 :y 64 :z 20}}} 20)))) "over the cell cap"))))))

(deftest a-run-that-gave-up-on-everything-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world (farm "wheat" 7 [[4 4]] [[4 4]]) {:unreachable [(k 4 64 4)]}))
              e (await (ending-of s {:area box} 80))]
          (is (= {:kind :nothing-possible :cause :gave-up} (select-keys e [:kind :cause])))
          (is (some? (get-in e [:data :steps :harvest]))))))))

(deftest a-part-with-no-step-is-a-look-until-the-body-has-looked-round
  (let [c {:primitives (tu/fake {}) :args {:area box}}
        status (fn [surveyed?] (with-redefs [ctx/mem (constantly {}) tend/check-facts (constantly {}) tend/would-run? (constantly false)
                                             tend/needs-look? (constantly true) look/surveyed? (fn ([_] surveyed?) ([_ _] surveyed?))]
                                 (tend/part-status c)))]
    (is (= [:look :nothing-to-do] (tu/kind-reason (status false))))
    (is (= [:later :nothing-to-do] (tu/kind-reason (status true))))))
