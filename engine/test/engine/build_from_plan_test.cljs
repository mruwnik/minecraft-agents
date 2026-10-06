(ns engine.build-from-plan-test
  "jobs.build.from-plan placing what a plan wants against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.harvest-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.build.from-plan :as build]))

(def job 'jobs.build.from-plan)

(deftest left-text-names-what-a-build-left
  (are [built text] (= text (build/left-text built))
    {} ""
    {:refused [{:pos [1 2 3] :reason :zone}]} "refused [1 2 3] zone"
    {:given-up {[1 2 3] :shape}} "gave up [1 2 3] shape"
    {:short {"stone" 2 "dirt" 1}} "short of stone 2, dirt 1"
    {:refused [{:pos [1 2 3] :reason :zone}] :short {"stone" 2}} "refused [1 2 3] zone; short of stone 2"))

(defn start
  "An engine over the fake world spec with the plans {id plan} and the zones (default none; nil: never read) as its
  world data."
  ([spec plans] (start spec plans []))
  ([spec plans zones]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor spec)
        w (world/of-data plans {} zones)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                          :world w})]
    {:eng eng :p p :seen seen :w w})))

(defn pen-plan
  "A 3x3 fence ring at y 64 over x 2..4, z 2..4, a gate at 3 64 2 and a torch on the post at 2 64 2."
  []
  {:id "pen"
   :parts [{:id "fence" :outline [[2 64 2] [4 64 4]] :want "oak_fence"}
           {:id "gate" :cells [[3 64 2]] :want {:block "oak_fence_gate" :facing :north}}
           {:id "light" :cells [[2 65 2]] :want "torch"}]})

(def kit [{:name "oak_fence" :count 16} {:name "oak_fence_gate" :count 1} {:name "torch" :count 4}])

(def ring [[2 64 2] [4 64 2] [2 64 3] [4 64 3] [2 64 4] [3 64 4] [4 64 4]])

(defn places [p] (mapv #(let [a (.-args %)] [[(.-x (.-pos a)) (.-y (.-pos a)) (.-z (.-pos a))] (.-item a)]) (h/calls p "place")))

(defn block [p [x y z]] (h/block-at p x y z))

(deftest an-empty-site-is-built-post-before-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory kit} {"pen" (pen-plan)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))
              order (mapv first (places p))]
          (is (every? #(= "oak_fence" (block p %)) ring))
          (is (= "oak_fence_gate" (block p [3 64 2])))
          (is (= "torch" (block p [2 65 2])))
          (is (< (.indexOf order [2 64 2]) (.indexOf order [2 65 2])))
          (is (= {:placed 9 :missing [] :short {} :given-up {} :wrong [] :refused []} result))
          (is (= 1 (count (h/events-of seen :build.done))))
          (is (empty? (h/events-of seen :build.short))))))))

(deftest a-facing-no-click-gives-is-placed-from-the-side-it-faces-away-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (update (pen-plan) :parts conj {:id "eye" :cells [[3 65 2]] :want {:block "observer" :facing :north}})
              ;; the body starts south of the pen, the side the observer is placed from (a walk round the ring would
              ;; hug a fence side, which the fake's block-wise steer cannot walk: card aef2106d)
              {:keys [eng p]} (start {:inventory (conj kit {:name "observer" :count 1}) :self {:pos {:x 3 :y 64 :z 7}}} {"pen" plan})
              body-z (atom nil)]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (when (= "observer" (.-item args)) (reset! body-z (.-z (.-pos (.self p)))))
                       (await (impl token args))))
          (await (h/child-outcome eng job {:plan "pen"} 200))
          (is (> @body-z 2.5)))))))

(defn state-at [p pos] (js->clj (.-properties (.blockAt p (clj->js (zipmap [:x :y :z] pos)))) :keywordize-keys true))

(defn clicks [p item] (mapv #(js->clj (.-click (.-args %)) :keywordize-keys true) (filter #(= item (.-item (.-args %))) (h/calls p "place"))))

(deftest a-gate-is-placed-facing-its-way-from-wherever-the-body-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= "north" (:facing (state-at p [3 64 2]))))
          (is (= [0] (mapv :yaw (clicks p "oak_fence_gate"))))
          (is (= [] (:wrong result))))))))

(deftest a-short-material-builds-what-it-can-and-says-what-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory [{:name "oak_fence" :count 4} {:name "oak_fence_gate" :count 1}
                                                       {:name "torch" :count 1}]}
                                          {"pen" (pen-plan)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= 4 (count (filter #(= "oak_fence" (block p %)) ring))))
          (is (= {"oak_fence" 3} (:short result)))
          (is (= 3 (count (filter (set ring) (:missing result)))))
          (is (= [{"oak_fence" 3}] (mapv :short (h/events-of seen :build.short)))))))))

(deftest a-removed-block-is-put-back-and-nothing-else-is-touched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [built (into {"3,64,2" "oak_fence_gate" "2,65,2" "torch"}
                          (map (fn [[x y z]] [(h/cell-key x y z) "oak_fence"]) (remove #{[4 64 3]} ring)))
              {:keys [eng p]} (start {:blocks built :inventory kit} {"pen" (pen-plan)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= [[[4 64 3] "oak_fence"]] (places p)))
          (is (= 1 (:placed result))))))))

(deftest a-cell-refused-three-times-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory kit} {"pen" (pen-plan)})]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (if (= [4 3] [(.-x (.-pos args)) (.-z (.-pos args))])
                         #js {:status "no-support"}
                         (await (impl token args)))))
          (let [result (await (h/child-outcome eng job {:plan "pen"} 200))]
            (is (= 3 (count (filter #(= [4 64 3] (first %)) (places p)))))
            (is (= {[4 64 3] :refused} (:given-up result)))
            (is (= 8 (:placed result)))
            (is (= 1 (count (h/events-of seen :build.gave-up))))))))))

(deftest a-cell-it-cannot-walk-to-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              {:keys [eng]} (start {:inventory kit :unreachable (map #(apply h/cell-key %) (build/stand-cells [40 64 0] 64 nil #{}))} {"far" plan})
              result (await (h/child-outcome eng job {:plan "far"} 200))]
          (is (= {[40 64 0] :unreachable} (:given-up result)))
          (is (= 0 (:placed result))))))))

(deftest an-unloaded-cell-is-walked-to-not-taken-as-built
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              {:keys [eng p]} (start {:inventory kit :unloaded ["40,64,0"]} {"far" plan})
              result (await (h/child-outcome eng job {:plan "far"} 200))]
          (is (seq (tu/walk-calls p)))
          (is (= {[40 64 0] :unloaded} (:given-up result)))
          (is (= [[40 64 0]] (:missing result))))))))

(defn declines
  "Submit the job with args over the world spec and plans, tick a few times: [places, build.declined warns]."
  ([spec plans args] (declines spec plans args []))
  ([spec plans args zones]
  (let [{:keys [eng p seen]} (start spec plans zones)]
    (core/submit! eng (list job args) {})
    (dotimes [_ 4] (swap! h/clock + 700) (core/tick! eng))
    [(count (h/calls p "place")) (mapv #(select-keys % [:plan :reason]) (h/events-of seen :build.declined))])))

(deftest the-check-declines-a-plan-it-cannot-build-with-one-warn
  (let [kit-spec {:inventory kit}]
    (is (= [0 [{:plan "nope" :reason "no such plan"}]] (declines kit-spec {} {:plan "nope"})))
    (is (= [0 [{:plan "pen" :reason "no cells to build"}]] (declines kit-spec {"pen" (pen-plan)} {:plan "pen" :part "nope"})))
    (is (= [0 [{:plan "pen" :reason "nothing carried to build with: oak_fence 7, oak_fence_gate 1, torch 1"}]]
           (declines {:inventory []} {"pen" (pen-plan)} {:plan "pen"})))))

(deftest a-broken-plan-declines-naming-the-error
  (let [{:keys [eng p seen w]} (start {:inventory kit} {})]
    (reset! (:state w) (assoc @(:state w) :plans {"pen" {:error "unreadable EDN: eof"}}))
    (core/submit! eng (list job {:plan "pen"}) {})
    (dotimes [_ 3] (swap! h/clock + 700) (core/tick! eng))
    (is (empty? (h/calls p "place")))
    (is (= [{:plan "pen" :reason "the plan cannot be read: unreadable EDN: eof"}]
           (mapv #(select-keys % [:plan :reason]) (h/events-of seen :build.declined))))))

(deftest item-for-picks-the-block-to-place
  (are [want carried item] (= item (build/item-for want carried))
    "oak_fence" #{} "oak_fence"
    {:block "oak_fence_gate" :facing :north} #{} "oak_fence_gate"
    [:any "oak_fence" "spruce_fence"] #{"spruce_fence"} "spruce_fence"
    [:any "oak_fence" {:block "spruce_fence"}] #{} "oak_fence"
    {:block "wall_torch" :facing :north} #{} "torch"
    [:any {:block "wall_torch" :facing :east} "lantern"] #{"torch"} "torch"
    {:crop "wheat"} #{"wheat_seeds"} nil
    :clear #{} nil))

(deftest facing-ok-wants-the-body-on-the-far-side
  (are [facing body ok] (= ok (build/facing-ok? facing [3 64 2] body))
    nil {:x 0.5 :y 64 :z 0.5} true
    "north" {:x 3.5 :y 64 :z 4.5} true
    "north" {:x 3.5 :y 64 :z 0.5} false
    "south" {:x 3.5 :y 64 :z 0.5} true
    "east" {:x 0.5 :y 64 :z 2.5} true
    "east" {:x 3.5 :y 64 :z 4.5} false))

(deftest a-corner-can-be-placed-from-inside-the-ring
  (let [planned (set (map first (map vector (conj ring [3 64 2]))))]
    (is (some #{[3 64 3]} (build/stand-cells [2 64 2] 64 nil planned)))))

;; ---------------------------------------------------------------- access

(defn zone [name [x0 y0 z0] [x1 y1 z1] allow] {:name name :min [x0 y0 z0] :max [x1 y1 z1] :owner "someone" :allow allow})

(def other-plan {:id "other" :parts [{:id "o" :cells [[4 64 3]] :want "stone"}]})

(deftest a-plan-is-built-over-a-foreign-zone-but-not-another-plans-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shrine (zone "shrine" [4 64 2] [4 64 4] #{})
              {:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan) "other" other-plan} [shrine])
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= [{:pos [4 64 3] :reason :footprint :plan "other"}] (:refused result)))
          (is (= "air" (block p [4 64 3])))
          (is (= "oak_fence" (block p [4 64 2])) "the pen's own cell in the shrine is built")
          (is (= 8 (:placed result))))))))

(deftest a-zone-allowing-placing-is-no-obstacle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:inventory kit} {"pen" (pen-plan)} [(zone "yard" [2 64 2] [4 65 4] #{:place})])
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= 9 (:placed result)))
          (is (= [] (:refused result))))))))

(deftest a-cell-in-another-active-plans-footprint-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan) "other" other-plan})
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= [{:pos [4 64 3] :reason :footprint :plan "other"}] (:refused result)))
          (is (= "air" (block p [4 64 3])))
          (is (= 8 (:placed result))))))))

(deftest no-zone-list-declines-and-never-means-no-zones
  (is (= [0 [{:plan "pen" :reason "no zone list has been read"}]]
         (declines {:inventory kit} {"pen" (pen-plan)} {:plan "pen"} nil))))

(def late-plan {:id "other" :parts [{:id "o" :cells (conj ring [3 64 2] [2 65 2]) :want "stone"}]})

(deftest the-access-is-checked-again-right-before-each-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p w]} (start {:inventory kit} {"pen" (pen-plan)})]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (world/set-data! w {"pen" (pen-plan) "other" late-plan} {})
                         r)))
          (let [result (await (h/child-outcome eng job {:plan "pen"} 200))]
            (is (= 1 (count (places p))) "the first place goes through and the other plan closes the rest")
            (is (= 1 (:placed result)))
            (is (= 8 (count (:refused result))))
            (is (= #{:footprint} (set (map :reason (:refused result)))))))))))

(defn ^:async run-beside
  "Build the pen with the fluid at 5 64 3 (beside the ring cell 4 64 3) and args: [placed, refused]."
  [fluid args]
  (let [{:keys [eng]} (start {:inventory kit :blocks {"5,64,3" fluid}} {"pen" (pen-plan)})
        result (await (h/child-outcome eng job (assoc args :plan "pen") 200))]
    [(:placed result) (:refused result)]))

(deftest water-beside-is-accepted-by-default-and-lava-beside-is-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [9 []] (await (run-beside "water" {}))))
        (is (= [8 [{:pos [4 64 3] :reason :hazard :hazards [:lava-adjacent]}]] (await (run-beside "lava" {}))))))))

(deftest accept-names-the-fluid-hazards-a-builder-takes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [9 []] (await (run-beside "lava" {:accept [:fluid-adjacent :lava-adjacent]}))))
        (is (= [8 [{:pos [4 64 3] :reason :hazard :hazards [:fluid-adjacent]}]] (await (run-beside "water" {:accept []}))))))))

;; ---------------------------------------------------------------- block state (jobs.lib.placement)

(defn ^:async build
  "Build plan parts {:id .. :cells .. :want ..} over the blocks with the inventory: [result p seen]."
  [parts blocks inventory]
  (let [{:keys [eng p seen]} (start {:blocks blocks :inventory inventory} {"house" {:id "house" :parts parts}})
        result (await (h/child-outcome eng job {:plan "house"} 200))]
    [result p seen]))

(def ground (into {} (for [x (range 0 6) z (range 0 6)] [(h/cell-key x 63 z) "stone"])))

(deftest stairs-slabs-and-logs-get-the-state-the-plan-wants
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "step" :cells [[3 64 3]] :want {:block "oak_stairs" :facing :east :half :bottom}}
                                        {:id "roof" :cells [[1 64 3]] :want {:block "oak_stairs" :facing :west :half :top}}
                                        {:id "cover" :cells [[3 64 1]] :want {:block "oak_slab" :type :top}}
                                        {:id "post" :cells [[1 64 1]] :want {:block "oak_log" :axis :y}}]
                                       (assoc ground "1,65,3" "stone" "4,64,1" "stone")
                                       [{:name "oak_stairs" :count 2} {:name "oak_slab" :count 1} {:name "oak_log" :count 1}]))]
          (is (= [["east" "bottom"] ["west" "top"]] (mapv #(vals (select-keys (state-at p %) [:facing :half])) [[3 64 3] [1 64 3]])))
          (is (= "top" (:type (state-at p [3 64 1]))))
          (is (= "y" (:axis (state-at p [1 64 1]))))
          (is (= {:placed 4 :missing [] :wrong [] :given-up {}} (select-keys result [:placed :missing :wrong :given-up]))))))))

(deftest a-door-and-a-bed-are-placed-once-their-other-half-comes-with-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "door" :cells [[2 64 2]] :want {:block "oak_door" :facing :north :half :lower}}
                                        {:id "door-top" :cells [[2 65 2]] :want {:block "oak_door" :facing :north :half :upper}}
                                        {:id "bed-head" :cells [[4 64 4]] :want {:block "white_bed" :facing :south :part :head}}
                                        {:id "bed-foot" :cells [[4 64 3]] :want {:block "white_bed" :facing :south :part :foot}}]
                                       ground [{:name "oak_door" :count 2} {:name "white_bed" :count 2}]))]
          (is (= [[[2 64 2] "oak_door"] [[4 64 3] "white_bed"]] (sort (places p))))
          (is (= ["upper" "head"] [(:half (state-at p [2 65 2])) (:part (state-at p [4 64 4]))]))
          (is (= {:placed 2 :missing [] :wrong [] :given-up {}} (select-keys result [:placed :missing :wrong :given-up]))))))))

(deftest a-wall-torch-is-placed-with-a-torch-on-its-wall-and-a-floor-torch-looking-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "wall" :cells [[2 65 2]] :want {:block "wall_torch" :facing :north}}
                                        {:id "floor" :cells [[4 64 4]] :want "torch"}]
                                       (assoc ground "2,65,3" "stone" "4,64,5" "stone" "5,64,4" "stone")
                                       [{:name "torch" :count 2}]))]
          (is (= ["wall_torch" "north"] [(h/block-at p 2 65 2) (:facing (state-at p [2 65 2]))]))
          (is (= "torch" (h/block-at p 4 64 4)))
          (is (= {:placed 2 :wrong []} (select-keys result [:placed :wrong]))))))))

(deftest a-state-no-neighbour-gives-is-given-up-with-its-reason-and-not-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (build [{:id "beam" :cells [[2 64 2]] :want {:block "oak_log" :axis :x}}
                                             {:id "post" :cells [[4 64 4]] :want "oak_log"}]
                                            ground [{:name "oak_log" :count 2}]))]
          (is (= [[[4 64 4] "oak_log"]] (places p)))
          (is (= {[2 64 2] :no-support} (:given-up result)))
          (is (= [[2 64 2]] (:missing result)))
          (is (= 1 (count (h/events-of seen :build.gave-up)))))))))

(deftest a-block-that-comes-out-another-way-is-reported-wrong-never-dug-or-retried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:blocks ground :inventory [{:name "oak_stairs" :count 3}]}
                                          {"house" {:id "house"
                                                    :parts [{:id "step" :cells [[3 64 3]] :want {:block "oak_stairs" :facing :east}}]}})]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (set! (.-yaw (.-click args)) 0)
                       (await (impl token args))))
          (let [result (await (h/child-outcome eng job {:plan "house"} 200))]
            (is (= 1 (count (places p))))
            (is (empty? (h/calls p "dig")))
            (is (= [{:pos [3 64 3] :found "oak_stairs[facing=north]" :want "oak_stairs[facing=east]" :placed true}] (:wrong result)))
            (is (= 1 (count (h/events-of seen :build.wrong))))))))))

(deftest the-opt-out-needs-no-zone-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:inventory kit} {"pen" (pen-plan)} nil)
              result (await (h/child-outcome eng job {:plan "pen" :ignore-zones? true} 200))]
          (is (= 9 (:placed result))))))))

;; ---------------------------------------------------------------- wrong blocks in wanted cells

(defn ^:async fence-run
  "Build the pen with the blocks laid and the extra plans, items and args: [result p seen]."
  [blocks inventory plans args]
  (let [{:keys [eng p seen]} (start {:blocks blocks :inventory inventory} (merge {"pen" (pen-plan)} plans))
        result (await (h/child-outcome eng job (merge {:plan "pen"} args) 300))]
    [result p seen]))

(deftest a-replaceable-block-in-a-wanted-cell-is-placed-into-directly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (fence-run {"4,64,3" "leaf_litter" "5,64,3" "fern"} kit {} {}))]
          (is (empty? (h/calls p "dig")))
          (is (= "oak_fence" (block p [4 64 3])))
          (is (= {:placed 9 :wrong [] :missing []} (select-keys result [:placed :wrong :missing]))))))))

(deftest a-wrong-solid-block-is-dug-by-hand-when-no-tool-is-needed-then-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (fence-run {"4,64,3" "dirt" "2,64,3" "oak_leaves"} kit {} {}))]
          (is (= 2 (count (h/calls p "dig"))))
          (is (= ["oak_fence" "oak_fence"] (mapv #(block p %) [[4 64 3] [2 64 3]])))
          (is (= {:placed 9 :wrong [] :missing [] :given-up {}} (select-keys result [:placed :wrong :missing :given-up]))))))))

(deftest a-wrong-stone-is-dug-with-the-carried-pickaxe-and-without-one-is-given-up-as-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[with p] (await (fence-run {"4,64,3" "stone"} (conj kit {:name "stone_pickaxe" :count 1}) {} {}))
              [without q] (await (fence-run {"4,64,3" "stone"} kit {} {}))]
          (is (= "oak_fence" (block p [4 64 3])))
          (is (= [] (:wrong with)))
          (is (empty? (h/calls q "dig")))
          (is (= "stone" (block q [4 64 3])))
          (is (= {[4 64 3] :no-tool} (:given-up without)))
          (is (= [{:pos [4 64 3] :found "stone" :want "oak_fence"}] (:wrong without))))))))

(deftest a-wrong-block-in-another-plans-cell-is-listed-and-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (fence-run {"4,64,3" "dirt"} kit {"other" other-plan} {}))]
          (is (empty? (h/calls p "dig")))
          (is (= "dirt" (block p [4 64 3])))
          (is (= [{:pos [4 64 3] :found "dirt" :want "oak_fence"}] (:wrong result)))
          (is (= [{:pos [4 64 3] :reason :footprint :plan "other"}] (:refused result))))))))

(deftest a-door-is-placed-once-at-its-lower-cell-over-wrong-blocks-and-never-gives-up-the-upper-half
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [door [{:id "door" :cells [[2 64 2]] :want {:block "oak_door" :facing :north :half :lower}}
                    {:id "door-top" :cells [[2 65 2]] :want {:block "oak_door" :facing :north :half :upper}}]
              [result p seen] (await (build door (assoc ground "2,64,2" "dirt" "2,65,2" "oak_leaves") [{:name "oak_door" :count 1}]))
              [litter q] (await (build door (assoc ground "2,64,2" "leaf_litter") [{:name "oak_door" :count 1}]))]
          (is (= [[[2 64 2] "oak_door"]] (places p)))
          (is (= 2 (count (h/calls p "dig"))))
          (is (= ["lower" "upper"] (mapv #(:half (state-at p %)) [[2 64 2] [2 65 2]])))
          (is (= {:placed 1 :missing [] :wrong [] :given-up {}} (select-keys result [:placed :missing :wrong :given-up])))
          (is (empty? (h/events-of seen :build.gave-up)))
          (is (= [[[2 64 2] "oak_door"]] (places q)))
          (is (empty? (h/calls q "dig")))
          (is (= {:placed 1 :wrong [] :given-up {}} (select-keys litter [:placed :wrong :given-up]))))))))

(deftest every-wrong-cell-is-listed-and-the-event-text-is-never-clipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (mapv (fn [x] [x 64 0]) (range 10 40))
              wall {"wall" {:id "wall" :parts [{:id "w" :cells cells :want "oak_planks"}]}}
              guard {"other" {:id "other" :parts [{:id "o" :cells cells :want "stone"}]}}
              {:keys [eng seen]} (start {:blocks (into {} (map (fn [[x y z]] [(h/cell-key x y z) "dirt"])) cells)
                                         :inventory [{:name "oak_planks" :count 64}]}
                                        (merge wall guard))
              result (await (h/child-outcome eng job {:plan "wall"} 300))
              wrong (first (h/events-of seen :build.wrong))]
          (is (= 30 (count (:wrong result))))
          (is (= 30 (count (:cells wrong))))
          (is (re-find #"30 wrong" (:text wrong)))
          (is (not (re-find #"…|\.\.\." (:text wrong))))
          (is (< (count (:text wrong)) 240)))))))
