(ns engine.build-from-plan-test
  "jobs.build.from-plan placing what a plan wants against the fake world."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [engine.fake]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.events :as events]
            [jobs.lib.ledger :as ledger]
            [engine.blocks-dig-test :as bd]
            [engine.fetch-test :as f]
            [engine.harvest-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [engine.triggers :as triggers]
            [jobs.lib.escape :as escape]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.world-files :as world]
            [jobs.build.from-plan :as build]
            [jobs.build.from-plan.book :as fp-book]
            [jobs.build.from-plan.cells :as fp-cells]
            [jobs.build.from-plan.light :as fp-light]
            [jobs.build.from-plan.walk :as fp-walk]
            [jobs.items.obtain.price :as obtain-price]
            [jobs.lib.need :as need]
            [jobs.lib.stock :as stock]
            [jobs.lib.shelter.sense :as sense]
            [jobs.lib.torch :as torch]
            [engine.contract :as contract]
            [jobs.build.from-plan.walk :as fp-walk]
            [jobs.lib.near :as near]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.build.text :as text]
            [plan.surface :as surface]))

(def job 'jobs.build.from-plan)

(defn ^:async child-outcome-exploring
  "tu/child-outcome without the status the adapter adds (the contract test reads the ending). The job's default fetch
  explores for what is short, which on the empty fake floor is a long search."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (if (map? r) (cond-> r (= :done (:status r)) (dissoc :status)) r)))

(defn child-outcome
  "child-outcome-exploring with :explore false unless args say otherwise: most tests are about what a shortage does, not the search."
  [eng job args n]
  (child-outcome-exploring eng job (merge {:explore false} args) n))

(deftest left-text-names-what-a-build-left
  (are [built text] (= text (plan/left-text built))
    {} ""
    {:refused [{:pos [1 2 3] :reason :zone}]} "refused [1 2 3] zone"
    {:given-up {[1 2 3] :shape}} "gave up [1 2 3] shape"
    {:short {"stone" 2 "dirt" 1}} "short of stone 2, dirt 1"
    {:refused [{:pos [1 2 3] :reason :zone}] :short {"stone" 2}} "refused [1 2 3] zone; short of stone 2"))

(defn start
  "An engine over the fake world spec with the plans {id plan} and the zones (default none; nil: never read) as its
  world data."
  ([spec plans] (start spec plans []))
  ([spec plans zones] (start spec plans zones nil))
  ([spec plans zones jobs]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (cond-> (tu/seeing-all (tu/fake-on-floor spec)) (:sight spec) (tu/seeing-within (:sight spec)))
        w (world/of-data plans {} zones)
        eng (core/create {:primitives p :jobs (merge registry/jobs jobs) :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
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

(def missing-blueprint-plan {:id "huts" :parts [{:id "h1" :blueprint "hut" :at [5 64 5]}]})

(def kit [{:name "oak_fence" :count 16} {:name "oak_fence_gate" :count 1} {:name "torch" :count 4}])

(def ring [[2 64 2] [4 64 2] [2 64 3] [4 64 3] [2 64 4] [3 64 4] [4 64 4]])

(defn places [p] (mapv #(let [a (.-args %)] [[(.-x (.-pos a)) (.-y (.-pos a)) (.-z (.-pos a))] (.-item a)]) (h/calls p "place")))

(defn block [p [x y z]] (h/block-at p x y z))

(deftest an-empty-site-is-built-post-before-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory kit} {"pen" (pen-plan)})
              result (await (child-outcome eng job {:plan "pen"} 200))
              order (mapv first (places p))]
          (is (every? #(= "oak_fence" (block p %)) ring))
          (is (= "oak_fence_gate" (block p [3 64 2])))
          (is (= "torch" (block p [2 65 2])))
          (is (< (.indexOf order [2 64 2]) (.indexOf order [2 65 2])))
          (is (= {:placed 9 :missing [] :short {} :given-up {} :wrong [] :refused [] :text "build of pen done: placed 9"}
                 result))
          (is (empty? (h/events-of seen :build.done)) "the ending tells it"))))))

(deftest a-water-cell-is-poured-from-the-carried-water-bucket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "pool" :parts [{:id "water" :cells [[5 64 5]] :want "water"}]}
              {:keys [eng p]} (start {:inventory [{:name "water_bucket" :count 1}]} {"pool" plan})
              result (await (child-outcome eng job {:plan "pool"} 200))]
          (is (= "water" (block p [5 64 5])))
          (is (= [1 {} {}] ((juxt :placed :short :given-up) result)) (pr-str result)))))))

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
          (await (child-outcome eng job {:plan "pen"} 200))
          (is (> @body-z 2.5)))))))

(defn state-at [p pos] (js->clj (.-properties (.blockAt p (clj->js (zipmap [:x :y :z] pos)))) :keywordize-keys true))

(defn clicks [p item] (mapv #(js->clj (.-click (.-args %)) :keywordize-keys true) (filter #(= item (.-item (.-args %))) (h/calls p "place"))))

(deftest a-gate-is-placed-facing-its-way-from-wherever-the-body-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan)})
              result (await (child-outcome eng job {:plan "pen"} 200))]
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
              result (await (child-outcome eng job {:plan "pen"} 200))]
          (is (= 4 (count (filter #(= "oak_fence" (block p %)) ring))))
          (is (= {"oak_fence" 3} (:short result)))
          (is (= 3 (count (filter (set ring) (:missing result))))))))))

(deftest a-removed-block-is-put-back-and-nothing-else-is-touched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [built (into {"3,64,2" "oak_fence_gate" "2,65,2" "torch"}
                          (map (fn [[x y z]] [(h/cell-key x y z) "oak_fence"]) (remove #{[4 64 3]} ring)))
              {:keys [eng p]} (start {:blocks built :inventory kit} {"pen" (pen-plan)})
              result (await (child-outcome eng job {:plan "pen"} 200))]
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
          (let [result (await (child-outcome eng job {:plan "pen"} 200))]
            (is (= 3 (count (filter #(= [4 64 3] (first %)) (places p)))))
            (is (= {[4 64 3] :no-support} (:given-up result)))
            (is (= 8 (:placed result)))))))))

(deftest the-give-up-setting-sets-how-often-a-refused-cell-is-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan)})]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (if (= [4 3] [(.-x (.-pos args)) (.-z (.-pos args))])
                         #js {:status "no-support"}
                         (await (impl token args)))))
          (let [result (await (child-outcome eng job {:plan "pen" :settings {:jobs.build.from-plan.walk/give-up 1}} 200))]
            (is (= 1 (count (filter #(= [4 64 3] (first %)) (places p)))))
            (is (= {[4 64 3] :no-support} (:given-up result)))))))))

(deftest a-fence-placed-over-air-warns-of-the-gap-under-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory kit :blocks {"4,63,3" "air"}} {"pen" (pen-plan)})
              _ (await (child-outcome eng job {:plan "pen"} 200))
              gaps (h/events-of seen :build.gap)]
          (is (= [[[4 64 3]]] (mapv :cells gaps)) "only the fence over the dip, once"))))))

(deftest cells-the-body-stood-in-are-warned-of-when-filled-without-a-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "wall" :cells [[3 64 2] [3 65 2] [3 66 2]] :want "stone"}]}
              {:keys [eng seen]} (start {:inventory [{:name "stone" :count 8}] :blocks {"3,64,2" "stone"}
                                         :self {:pos {:x 3.5 :y 65 :z 2.5}}} {"hut" plan})
              _ (await (child-outcome eng job {:plan "hut"} 200))
              warns (h/events-of seen :build.way-in)]
          (is (= [[3 65 2] [3 66 2]] (mapcat :cells warns)) "the doorway cells, not the floor cell, once each"))))))

(deftest a-cell-it-cannot-walk-to-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              {:keys [eng]} (start {:inventory kit :unreachable (map #(apply h/cell-key %) (plan/stand-cells [40 64 0] 64 nil #{}))} {"far" plan})
              result (await (child-outcome eng job {:plan "far"} 200))]
          (is (= {[40 64 0] :unreachable} (:given-up result)))
          (is (= 0 (:placed result))))))))

(deftest a-dig-it-cannot-walk-to-is-given-up-unreachable-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "floor" :cells [[40 64 0]] :want "stone"}]}
              {:keys [eng]} (start {:inventory [{:name "stone" :count 2}] :blocks {"40,64,0" "dirt"}
                                    :unreachable (map #(apply h/cell-key %) (plan/stand-cells [40 64 0] 64 nil #{}))} {"far" plan})
              result (await (child-outcome eng job {:plan "far"} 200))]
          (is (= {[40 64 0] :unreachable} (:given-up result)) "not :refused after three tries"))))))

(defn rock-start
  "A dirt cell [3 64 2] inside a solid block of stone with no air beside it, the body outside; kit carried."
  [kit tunnel]
  (let [plan {:id "ring" :parts [{:id "p" :cells [[3 64 2]] :want "dirt"}]}
        rock (into {} (for [x (range -2 9) z (range -4 9) y [62 63 64 65 66]] [(str x "," y "," z) "stone"]))]
    (start {:inventory kit :blocks rock :self {:pos {:x 12.5 :y 64 :z 2.5}}} {"ring" plan} [] {'jobs.access.tunnel tunnel})))

(deftest a-cell-in-rock-with-no-stand-is-tunnelled-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [targets (atom [])
              tunnel {:round (fn ^:async w [c] (swap! targets conj (:target (:args c)) (:through (:args c))) (ctx/end c :nothing-possible :dig-failed {}))}
              {:keys [eng]} (rock-start [{:name "stone_pickaxe" :count 1} {:name "dirt" :count 4}] tunnel)
              result (await (child-outcome eng job {:plan "ring"} 200))]
          (is (= [[3 64 2] "ring"] (distinct @targets)) "the target, and the plan whose footprint the tunnel may cut")
          (is (= {[3 64 2] :unreachable} (:given-up result)) "a tunnel that fails gives the cell up"))))))

(deftest a-cell-never-seen-beyond-a-pocket-wall-is-walked-to-not-tunnelled-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [targets (atom [])
              tunnel {:round (fn ^:async w [c] (swap! targets conj (:target (:args c))) (ctx/end c :nothing-possible :dig-failed {}))}
              plan {:id "ring" :parts [{:id "p" :cells [[12 64 8]] :want "dirt"}]}
              shell (into {} (for [x (range 11 14) z (range 1 4) y (range 62 67)
                                   :when (not (and (= x 12) (= z 2) (#{64 65} y)))]
                               [(str x "," y "," z) "stone"]))
              {:keys [eng p]} (start {:inventory [{:name "stone_pickaxe" :count 1} {:name "dirt" :count 4}]
                                    :blocks (assoc shell "12,63,8" "stone") :sight 5
                                    :self {:pos {:x 12.5 :y 64 :z 2.5}}}
                                   {"ring" plan} [] {'jobs.access.tunnel tunnel})
              _ (await (child-outcome eng job {:plan "ring"} 200))]
          (is (empty? @targets) "the cell was never seen in rock: no tunnel is begun")
          (is (>= (.-z (.-pos (.self p))) 4) "the body dug out of the shell and walked toward the cell")
          (is (= "dirt" (block p [12 64 8])) "the cell is placed: open ground at the edge of sight is not a wall"))))))

(deftest a-place-beside-a-body-that-sees-only-a-little-open-ground-does-not-seal-it-in
  (let [{:keys [p]} (start {:self {:pos {:x 4.5 :y 64 :z 4.5}} :sight 3} {} [])]
    (is (not (fp-cells/seals? {:primitives p} [5 64 4])))
    (is (not (fp-cells/seals? {:primitives p} [4 64 5])))))

(defn gap-room
  "A 3x3x3 stone room around [4 64 4] with a two-high gap at x 5 z 4; the body inside with kit."
  [kit]
  (let [room (into {} (for [x (range 3 6) z (range 3 6) y [63 64 65 66] :when (not (and (= x 4) (= z 4) (#{64 65} y)))]
                        [(h/cell-key x y z) "stone"]))
        gap (dissoc room (h/cell-key 5 64 4) (h/cell-key 5 65 4))]
    (start {:blocks gap :floor [-30 -30 30 30] :self {:pos {:x 4.5 :y 64 :z 4.5}} :inventory kit}
           {"gap" {:id "gap" :parts [{:id "p" :cells [[5 64 4] [5 65 4]] :want "oak_planks"}]}})))

(def planks {:name "oak_planks" :count 4})
(def pick {:name "stone_pickaxe" :count 1})
(def gap-cells [{:pos [5 64 4] :part "p" :answer :missing :item "oak_planks"} {:pos [5 65 4] :part "p" :answer :missing :item "oak_planks"}])

(deftest a-cell-that-would-seal-the-body-in-is-left-by-the-gap-it-closes
  (let [c {:primitives (:p (gap-room [planks]))}]
    (is (fp-cells/seals? c [5 64 4]))
    (is (= [6 64 4] (fp-cells/seal-exit c [5 64 4])) "the nearest cell beyond the gap, outside the cell's column")
    (is (not (fp-cells/seals? c [5 66 4])))))

(deftest a-body-walks-out-before-it-places-the-cell-that-would-seal-it-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [kit [[planks pick] [planks]]]
          (let [{:keys [eng p]} (gap-room kit)
                result (await (child-outcome eng job {:plan "gap"} 300))]
            (is (= ["oak_planks" "oak_planks"] (mapv #(block p %) [[5 64 4] [5 65 4]])) (pr-str kit))
            (is (empty? (:given-up result)))
            (is (= "stone" (block p [3 64 4])) "no wall dug")
            (is (not (reach-room/enclosed? p :raw)) "placed from outside")))))))

(defn with-seal-fns
  "Run async f with the fp-cells seals?/seal-exit and the walk exit-of replaced, restored when it ends."
  [{:keys [seals seal-exit exit-of]} f]
  (let [old [fp-cells/seals? fp-cells/seal-exit fp-walk/exit-of]]
    (set! fp-cells/seals? seals)
    (set! fp-cells/seal-exit seal-exit)
    (set! fp-walk/exit-of exit-of)
    (-> (f) (.finally (fn [] (set! fp-cells/seals? (nth old 0)) (set! fp-cells/seal-exit (nth old 1)) (set! fp-walk/exit-of (nth old 2)))))))

(deftest a-cell-that-seals-the-body-in-with-no-way-out-is-given-up-not-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (gap-room [planks])
              result (await (with-seal-fns {:seals (fn [_ cell] (= cell [5 64 4])) :seal-exit (fn [_ _] nil) :exit-of (fn [_ _] nil)}
                              #(child-outcome eng job {:plan "gap"} 300)))]
          (is (not= "oak_planks" (block p [5 64 4])))
          (is (= :seals-in (get (:given-up result) [5 64 4]))))))))

(deftest a-cell-that-needs-three-successful-walk-outs-is-still-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (gap-room [planks])
              n (atom 0)
              real-exit fp-cells/seal-exit
              result (await (with-seal-fns {:seals (fn [_ cell] (and (= cell [5 64 4]) (< (swap! n inc) 4)))
                                            :seal-exit (fn [c cell] (real-exit c cell))
                                            :exit-of fp-walk/exit-of}
                              #(child-outcome eng job {:plan "gap"} 300)))]
          (is (= "oak_planks" (block p [5 64 4])))
          (is (not (contains? (:given-up result) [5 64 4]))))))))

(deftest a-seal-that-places-nothing-does-not-dig-the-body-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (gap-room [pick])]
          (await (child-outcome eng job {:plan "gap"} 300))
          (is (not= "oak_planks" (block p [5 64 4])))
          (is (= "stone" (block p [3 64 4])) "no seal placed, so no dig-out"))))))

(deftest a-dig-in-rock-with-no-pickaxe-is-given-up-no-tool-without-a-tunnel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [targets (atom [])
              tunnel {:round (fn ^:async w [c] (swap! targets conj (:target (:args c))) (ctx/end c :nothing-possible :dig-failed {}))}
              {:keys [eng]} (rock-start [{:name "dirt" :count 4}] tunnel)
              result (await (child-outcome eng job {:plan "ring"} 200))]
          (is (empty? @targets))
          (is (= {[3 64 2] :no-tool} (:given-up result))))))))

(defn corridor
  "A fake world: a stone mass with an air corridor along x 0..5 at y 64, z 0."
  [& {:keys [wall inventory]}]
  (tu/seeing-all
   (tu/fake-on-floor
    {:inventory (or inventory [])
     :blocks (merge (into {} (for [x (range -2 9) y [62 63 64 65 66] z (range -3 4)] [(str x "," y "," z) (or wall "stone")]))
                    (into {} (for [x (range 0 6)] [(str x ",64,0") "air"])))})))

(deftest a-block-cut-into-rock-is-filled-farthest-from-the-mouth-first
  (let [p (corridor)
        cells (map (fn [x] {:pos [x 64 0]}) [3 4 5])
        dists (fp-cells/retreat-dists p [0 64 0] cells)]
    (is (= 5 (get dists [5 64 0])) "steps through open cells from the mouth")
    (is (= [[5 64 0] [4 64 0] [3 64 0]] (map :pos (fp-cells/fill-order dists [0 64 0] false cells))) "deepest first")
    (is (= [[5 64 0] [4 64 0]] (map :pos (fp-cells/fill-order dists [3 64 0] true cells))) "standing in a plan cell: none nearer the mouth than it")))

(deftest the-body-steps-back-one-cell-toward-the-mouth-to-fill-the-cell-it-stood-in
  (let [dists (fp-cells/retreat-dists (corridor) [0 64 0] (map (fn [x] {:pos [x 64 0]}) [3 4 5]))]
    (is (= [4 64 0] (fp-cells/step-back dists [5 64 0])))
    (is (nil? (fp-cells/step-back dists [0 64 0])) "at the mouth")))

(deftest a-cell-found-out-of-sight-is-tunnelled-to-again-only-up-to-the-tunnel-tries-setting
  (let [e {:cause {:why :out-of-sight}}]
    (is (fp-book/sightless? {:mouth [0 64 0] :tunnels {[1 1 1] (dec (plan/tunnel-tries))}} :unreachable e [1 1 1]))
    (is (not (fp-book/sightless? {:mouth [0 64 0] :tunnels {[1 1 1] (plan/tunnel-tries)}} :unreachable e [1 1 1])))
    (is (not (fp-book/sightless? {:tunnels {}} :unreachable e [1 1 1])) "no tunnel cut by this build: given up as before")))

(deftest breaking-the-rock-round-a-cell-takes-a-tool-only-when-the-block-needs-one
  (let [cell {:pos [3 64 0]}]
    (is (fp-cells/needs-tool? (corridor) cell) "stone, no pickaxe")
    (is (not (fp-cells/needs-tool? (corridor :wall "dirt") cell)) "dirt goes by hand")
    (is (not (fp-cells/needs-tool? (corridor :inventory [{:name "stone_pickaxe" :count 1}]) cell)))))

(deftest a-wall-waits-for-the-dirt-floor-under-it-though-the-floor-item-is-not-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "floor" :cells [[3 64 2]] :want "stone"}
                                      {:id "wall" :cells [[3 65 2]] :want "cobblestone"}]}
              {:keys [eng p]} (start {:inventory [{:name "cobblestone" :count 4}] :blocks {"3,64,2" "dirt"}} {"hut" plan})
              _ (await (child-outcome eng job {:plan "hut"} 200))]
          (is (empty? (places p)) "the wall cell is not placed over the dirt that is still to dig"))))))

(deftest a-part-builds-the-cells-of-other-parts-it-stands-on-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "table" :cells [[3 64 2]] :want "crafting_table"}
                                      {:id "torch" :cells [[3 65 2]] :want "torch"}]}
              {:keys [eng]} (start {:inventory [{:name "crafting_table" :count 1} {:name "torch" :count 1}]
                                    :blocks {"3,64,2" "air" "3,65,2" "air" "3,63,2" "stone"}} {"hut" plan})
              result (await (child-outcome eng job {:plan "hut" :part "torch"} 200))]
          (is (= 2 (:placed result)) "the table under the torch is placed first")
          (is (empty? (:given-up result))))))))

(deftest a-herd-only-part-ends-done-saying-nothing-was-to-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "farm" :parts [{:id "pen" :box [[2 64 2] [3 64 3]] :want "air" :herd {:mob "sheep" :target 2}}]}
              {:keys [eng]} (start {:inventory [] :blocks {}} {"farm" plan})
              result (await (child-outcome eng job {:plan "farm" :part "pen"} 200))]
          (is (= 0 (:placed result)))
          (is (re-find #"nothing to place in part pen.*animals\.maintain" (:text result)) (pr-str result)))))))

(deftest wood-hint-names-carried-other-wood-and-the-generic-name
  (is (= "carries oak_log 16; the blueprint can use generic planks" (text/wood-hint "spruce_planks" {"oak_log" 16 "spruce_log" 1 "stone" 3})))
  (is (nil? (text/wood-hint "spruce_planks" {"spruce_log" 4})))
  (is (nil? (text/wood-hint "stone" {"oak_log" 16}))))

(deftest a-floor-cell-under-a-built-wall-is-placed-from-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "floor" :cells [[3 64 2]] :want "stone"}
                                      {:id "wall" :cells [[3 65 2]] :want "stone"}]}
              {:keys [eng]} (start {:inventory [{:name "stone" :count 8}] :blocks {"3,65,2" "stone" "3,64,2" "air" "3,63,2" "stone"}} {"hut" plan})
              result (await (child-outcome eng job {:plan "hut"} 200))]
          (is (= 1 (:placed result)))
          (is (empty? (:given-up result))))))))

(deftest a-floor-cell-shut-in-under-a-built-block-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "floor" :cells [[3 64 2]] :want "stone"}
                                      {:id "chest" :cells [[3 65 2]] :want "stone"}]}
              ring (into {} (for [x (range 0 7) z (range -1 6) y [63 64 65]] [(str x "," y "," z) "stone"]))
              {:keys [eng]} (start {:inventory [{:name "stone" :count 8}] :blocks (merge ring {"3,65,2" "stone" "3,64,2" "air" "3,63,2" "stone"}) :self {:pos {:x 9.5 :y 64 :z 2.5}}} {"hut" plan})
              result (await (child-outcome eng job {:plan "hut"} 200))]
          (is (= {[3 64 2] :no-tool} (:given-up result)))
          (is (= 0 (:placed result))))))))

(deftest walks-that-keep-ending-budget-stop-the-build-instead-of-trying-every-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              budget-walk {:ways [:walk :clear-seal]
                           :round (fn ^:async w [c] (swap! walks inc) (ctx/end c :nothing-possible :unreachable {:why :budget :arrived false}))}
              plan {:id "far" :parts [{:id "posts" :cells (mapv (fn [i] [(+ 40 (* 2 i)) 64 0]) (range 8)) :want "oak_fence"}]}
              {:keys [eng]} (start {:inventory kit :floor [-5 -5 60 5]} {"far" plan} [] {'jobs.movement.go-to budget-walk})
              result (await (child-outcome eng job {:plan "far"} 400))]
          (is (= [:nothing-possible :unreachable] [(get-in result [:ending :kind]) (get-in result [:ending :cause])]) (pr-str result))
          (is (<= @walks 12) (str @walks " walks")))))))

(deftest an-unloaded-cell-is-walked-to-and-waited-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              {:keys [eng p]} (start {:inventory kit :unloaded ["40,64,0"]} {"far" plan})
              result (await (child-outcome eng job {:plan "far"} 200))]
          (is (seq (tu/walk-calls p)))
          (is (= :not-done result) "a chunk that has not loaded is waited for, never given up"))))))

(deftest a-place-child-that-waits-ends-the-round-instead-of-being-called-again-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "near" :parts [{:id "post" :cells [[3 64 0]] :want "oak_fence"}]}
              {:keys [eng seen]} (start {:inventory kit} {"near" plan})
              eng (assoc-in eng [:jobs 'jobs.blocks.place] {:round (fn [_] :continue)})
              result (await (child-outcome eng job {:plan "near"} 30))
              placed (filter #(= :place (:slot %)) (h/events-of seen :child_started))]
          (is (= :not-done result))
          (is (<= (count placed) 30) "at most one place call per tick"))))))

(deftest a-cell-in-reach-that-place-cannot-walk-to-gets-a-scaffold-stand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [no-walk {:ways [:carried] :round (fn ^:async w [c] (ctx/end c :nothing-possible :unreachable {:why :budget :placed false :reason :unreachable}))}
              plan {:id "roof" :parts [{:id "r" :cells [[3 64 4]] :want "stone"}]}
              {:keys [eng seen]} (start {:inventory [{:name "stone" :count 4} {:name "dirt" :count 10}] :self {:pos {:x 3.5 :y 64 :z 2.5}}} {"roof" plan} [] {'jobs.blocks.place no-walk})
              _ (await (child-outcome eng job {:plan "roof"} 300))]
          (is (some #(= :swalk (:slot %)) (h/events-of seen :child_started)) "the scaffold stand is walked to instead of the place walking again"))))))

(deftest a-roof-corner-hidden-from-inside-the-hut-is-placed-from-a-stand-outside-that-sees-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hut (merge (into {} (for [x (range 2 7) y (range 63 68) z (range 4 9)] [(h/cell-key x y z) "stone"]))
                         (into {} (for [x (range 3 6) y (range 64 67) z (range 5 8)] [(h/cell-key x y z) "air"]))
                         {(h/cell-key 4 64 8) "air" (h/cell-key 4 65 8) "air" (h/cell-key 2 67 4) "air"})
              plan {:id "roof" :parts [{:id "r" :cells (vec (for [x (range 2 7) z (range 4 9)] [x 67 z])) :want "stone"}]}
              {:keys [eng]} (start {:inventory [{:name "stone" :count 4}] :blocks hut :self {:pos {:x 4.5 :y 64 :z 6.5}}} {"roof" plan})
              result (await (child-outcome eng job {:plan "roof"} 300))]
          (is (= 1 (:placed result)) (pr-str result))
          (is (empty? (:given-up result))))))))

(deftest the-last-roof-cell-of-a-hut-beside-a-cliff-is-placed-from-the-ground-that-sees-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hill (into {} (for [x (range -1 5) y (range 64 67) z (range -5 7)] [(h/cell-key x y z) "dirt"]))
              hut (into {} (for [x (range 5 8) y (range 64 68) z (range 0 3)
                                 :when (or (#{64 67} y) (not= [x z] [6 1]))
                                 :when (not= [x y z] [7 67 1])]
                             [(h/cell-key x y z) "stone"]))
              plan {:id "hut" :parts [{:id "roof" :box [[5 67 0] [7 67 2]] :want "stone"}]}
              {:keys [eng]} (start {:inventory [{:name "stone" :count 4} {:name "dirt" :count 10}] :blocks (merge hill hut) :self {:pos {:x 7.5 :y 64 :z -1.5}}} {"hut" plan})
              result (await (child-outcome eng job {:plan "hut"} 300))]
          (is (= 1 (:placed result)) (pr-str result))
          (is (empty? (:given-up result))))))))

(deftest a-place-that-says-already-for-a-cell-the-plan-reads-missing-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [already {:ways [:carried] :round (fn ^:async w [c] (ctx/end c :done nil {:reason :already}))}
              plan {:id "post" :parts [{:id "p" :cells [[3 64 3]] :want "oak_fence"}]}
              {:keys [eng]} (start {:inventory kit :self {:pos {:x 3.5 :y 64 :z 1.5}}} {"post" plan} [] {'jobs.blocks.place already})
              result (await (child-outcome eng job {:plan "post"} 300))]
          (is (= {[3 64 3] :already} (:given-up result)) (pr-str result))
          (is (= [[3 64 3]] (:missing result))))))))

(deftest the-hole-in-the-roof-of-a-sealed-room-gets-a-scaffold-stand-when-the-walks-end-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [budget-walk {:ways [:walk :clear-seal]
                           :round (fn ^:async w [c] (ctx/end c :nothing-possible :unreachable {:why :budget :arrived false}))}
              hut (merge (into {} (for [x (range 2 7) y (range 64 69) z (range 4 9)] [(h/cell-key x y z) "stone"]))
                         (into {} (for [x (range 3 6) y (range 65 68) z (range 5 8)] [(h/cell-key x y z) "air"]))
                         {(h/cell-key 4 68 6) "air"})
              plan {:id "roof" :parts [{:id "r" :cells [[4 68 6]] :want "stone"}]}
              {:keys [eng seen]} (start {:inventory [{:name "stone" :count 4} {:name "dirt" :count 10}] :blocks hut :self {:pos {:x 4.5 :y 64 :z 1.5}}}
                                        {"roof" plan} [] {'jobs.movement.go-to budget-walk})
              _ (await (child-outcome eng job {:plan "roof"} 300))]
          (is (some #(= :swalk (:slot %)) (h/events-of seen :child_started))))))))

(deftest a-cell-walled-in-on-every-side-gets-a-stand-in-reach-when-nothing-sees-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [budget-walk {:ways [:walk :clear-seal]
                           :round (fn ^:async w [c] (ctx/end c :nothing-possible :unreachable {:why :budget :arrived false}))}
              rock (merge (into {} (for [x (range 2 7) y (range 64 69) z (range 4 9)] [(h/cell-key x y z) "stone"]))
                          {(h/cell-key 4 66 6) "air"})
              plan {:id "cell" :parts [{:id "r" :cells [[4 66 6]] :want "stone"}]}
              {:keys [eng seen]} (start {:inventory [{:name "stone" :count 4} {:name "dirt" :count 10}] :blocks rock :self {:pos {:x 4.5 :y 64 :z 1.5}}}
                                        {"cell" plan} [] {'jobs.movement.go-to budget-walk})
              result (await (child-outcome eng job {:plan "cell"} 300))]
          (is (some #(= :swalk (:slot %)) (h/events-of seen :child_started)) "a stand in reach is chosen though none sees the cell")
          (is (not= :not-done result) "the build ends instead of looping"))))))

(deftest a-hole-in-the-middle-of-a-flat-roof-gets-a-pillar-base-walk-after-a-failed-walk-to-a-roof-stand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [targets (atom [])
              failing-walk {:ways [:walk :clear-seal]
                            :round (fn ^:async w [c]
                                     (swap! targets conj (:y (:pos (:args c))))
                                     (ctx/end c :nothing-possible :unreachable {:why :exhausted :arrived false :shut-in true}))}
              hut (merge (into {} (for [x (range 2 7) y (range 64 69) z (range 4 9)] [(h/cell-key x y z) "stone"]))
                         (into {} (for [x (range 3 6) y (range 65 68) z (range 5 8)] [(h/cell-key x y z) "air"]))
                         {(h/cell-key 4 68 6) "air"})
              plan {:id "roof" :parts [{:id "r" :cells [[4 68 6]] :want "stone"}]}
              {:keys [eng]} (start {:inventory [{:name "stone" :count 4} {:name "dirt" :count 10}] :blocks hut :self {:pos {:x 4.5 :y 64 :z 1.5}}}
                                   {"roof" plan} [] {'jobs.movement.go-to failing-walk})
              _ (await (child-outcome eng job {:plan "roof"} 300))]
          (is (some #(< % 68) (rest @targets)) (pr-str @targets)))))))

(deftest an-unseen-cell-is-looked-at-before-it-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "near" :parts [{:id "post" :cells [[3 64 0]] :want "oak_fence"}]}
              [seen sink] (tu/legacy-capture-sink)
              p (tu/seeing-after-look (tu/fake-on-floor {:inventory kit}))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                                :world (world/of-data {"near" plan} {} [])})
              result (await (child-outcome eng job {:plan "near"} 200))]
          (is (seq (h/calls p "look")))
          (is (= {} (:given-up result)))
          (is (= 1 (:placed result))))))))

(deftest an-unseen-floor-with-nothing-carried-is-looked-at-then-the-check-says-no-items
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen sink] (tu/legacy-capture-sink)
              p (tu/seeing-after-look (tu/fake-on-floor {:inventory []}))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                                :world (world/of-data {"pen" (pen-plan)} {} [])})
              result (await (child-outcome eng job {:plan "pen" :ways #{:carried}} 60))]
          (is (seq (h/calls p "look")))
          (is (= :no-items (:reason result)))
          (is (= 0 (count (h/calls p "place")))))))))

(deftest a-cell-still-unseen-after-the-look-does-not-stop-the-start-while-a-fetch-is-due
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen sink] (tu/legacy-capture-sink)
              p (tu/seeing-after-look (tu/fake-on-floor {:inventory []}))
              hidden? (fn [pos] (let [[x y z] (if (array? pos) pos [(.-x pos) (.-y pos) (.-z pos)])] (= [2 65 2] [x y z])))
              seen-at (.-seenBlockAt p)
              sensed-at (.-sensedAt p)
              _ (aset p "seenBlockAt" (fn [pos] (if (hidden? pos) #js {:unknown true :pos pos} (seen-at pos))))
              _ (tu/set-sensed! p (fn [pos] (if (hidden? pos) #js {:unknown true} (sensed-at pos))))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                                :world (world/of-data {"pen" (pen-plan)} {} [])})
              result (await (child-outcome eng job {:plan "pen"} 200))]
          (is (seq (:fetch-failed result)) (pr-str result)))))))

(deftest unseen-cells-the-body-carries-the-items-for-are-not-a-no-items-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen sink] (tu/legacy-capture-sink)
              p (doto (tu/seeing-after-look (tu/fake-on-floor {:inventory kit}))
                  (aset "seenBlocks" (fn [_] #js []))
                  (aset "seenBlockAt" (fn [pos] #js {:unknown true :pos pos}))
                  (tu/set-sensed! (fn [_] #js {:unknown true})))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                                :world (world/of-data {"pen" (pen-plan)} {} [])})
              result (await (child-outcome eng job {:plan "pen" :ways #{:carried}} 60))]
          (is (not= :no-items (:reason result))))))))

(defn declines
  "Submit the job with args over the world spec and plans, tick a few times: [places, build.declined warns]."
  ([spec plans args] (declines spec plans args []))
  ([spec plans args zones]
   (let [{:keys [eng p seen]} (start spec plans zones)]
     (core/submit! eng (list job args) {})
     (dotimes [_ 4] (swap! h/clock + 700) (core/tick! eng))
     [(count (h/calls p "place")) (count (h/events-of seen :build.declined))])))

(deftest the-check-declines-a-plan-it-cannot-build-without-a-warn
  (let [kit-spec {:inventory kit}]
    (is (= [0 0] (declines kit-spec {} {:plan "nope"})))
    (is (= [0 0] (declines kit-spec {"pen" (pen-plan)} {:plan "pen" :part "nope"})))
    (is (= [0 0] (declines kit-spec {"huts" missing-blueprint-plan} {:plan "huts"})))
    (is (= [0 0] (declines {:inventory []} {"pen" (pen-plan)} {:plan "pen" :ways #{:carried}})))))

(deftest a-plan-in-terminal-trouble-ends-the-job-stopped-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[plans args reason] [[{"pen" (pen-plan)} {:plan "pen" :part "nope"} "no cells to build"]]]
          (let [{:keys [eng seen]} (start {:inventory kit} plans)
                result (await (child-outcome eng job args 200))]
            (is (= :stopped (:status result)) reason)
            (is (= :plan-trouble (:reason result)) reason)
            (is (re-find (re-pattern reason) (get-in result [:ending :cause :why])) reason)))))))

(deftest the-check-without-items-waits-naming-the-shortage
  (let [{:keys [eng seen]} (start {:inventory []} {"pen" (pen-plan)})]
    (core/submit! eng (list job {:plan "pen" :ways #{:carried}}) {})
    (dotimes [_ 4] (swap! h/clock + 700) (core/tick! eng))
    (is (= [[:no-items "pen" {"oak_fence" 7 "oak_fence_gate" 1 "torch" 1}]]
           (distinct (map (juxt :reason :plan :needs) (h/events-of seen :waiting)))))))

(deftest the-check-judges-the-plan-once-per-window
  (let [n (atom 0)
        real plan/judged
        {:keys [eng seen p]} (start {:inventory []} {"pen" (pen-plan)})]
    (with-redefs [plan/judged (fn [& args] (swap! n inc) (apply real args))]
      (core/submit! eng (list job {:plan "pen" :ways #{:carried}}) {})
      (dotimes [_ 8] (swap! h/clock + 700) (core/tick! eng)))
    (is (< 0 @n 8) "eight ticks, fewer than eight judgements")))

(deftest a-broken-plan-declines-without-a-warn
  (let [{:keys [eng p seen w]} (start {:inventory kit} {})]
    (reset! (:state w) (assoc @(:state w) :plans {"pen" {:error "unreadable EDN: eof"}}))
    (core/submit! eng (list job {:plan "pen"}) {})
    (dotimes [_ 3] (swap! h/clock + 700) (core/tick! eng))
    (is (empty? (h/calls p "place")))
    (is (empty? (h/events-of seen :build.declined)))))

(deftest item-for-picks-the-block-to-place
  (are [want carried item] (= item (plan/item-for want carried))
    "oak_fence" #{} "oak_fence"
    {:block "oak_fence_gate" :facing :north} #{} "oak_fence_gate"
    [:any "oak_fence" "spruce_fence"] #{"spruce_fence"} "spruce_fence"
    [:any "oak_fence" {:block "spruce_fence"}] #{} "oak_fence"
    {:block "wall_torch" :facing :north} #{} "torch"
    [:any {:block "wall_torch" :facing :east} "lantern"] #{"torch"} "torch"
    {:crop "wheat"} #{"wheat_seeds"} nil
    :clear #{} nil))

(deftest facing-ok-wants-the-body-on-the-far-side
  (are [facing body ok] (= ok (plan/facing-ok? facing [3 64 2] body))
    nil {:x 0.5 :y 64 :z 0.5} true
    "north" {:x 3.5 :y 64 :z 4.5} true
    "north" {:x 3.5 :y 64 :z 0.5} false
    "south" {:x 3.5 :y 64 :z 0.5} true
    "east" {:x 0.5 :y 64 :z 2.5} true
    "east" {:x 3.5 :y 64 :z 4.5} false))

(deftest a-corner-can-be-placed-from-inside-the-ring
  (let [planned (set (map first (map vector (conj ring [3 64 2]))))]
    (is (some #{[3 64 3]} (plan/stand-cells [2 64 2] 64 nil planned)))))

;; ---------------------------------------------------------------- access

(defn zone [name [x0 y0 z0] [x1 y1 z1] allow] {:name name :min [x0 y0 z0] :max [x1 y1 z1] :owner "someone" :allow allow})

(def other-plan {:id "other" :parts [{:id "o" :cells [[4 64 3]] :want "stone"}]})

(deftest a-plan-is-built-over-a-foreign-zone-but-not-another-plans-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shrine (zone "shrine" [4 64 2] [4 64 4] #{})
              {:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan) "other" other-plan} [shrine])
              result (await (child-outcome eng job {:plan "pen"} 200))]
          (is (= [{:pos [4 64 3] :reason :footprint :plan "other"}] (:refused result)))
          (is (= "air" (block p [4 64 3])))
          (is (= "oak_fence" (block p [4 64 2])) "the pen's own cell in the shrine is built")
          (is (= 8 (:placed result))))))))

(deftest a-zone-allowing-placing-is-no-obstacle
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:inventory kit} {"pen" (pen-plan)} [(zone "yard" [2 64 2] [4 65 4] #{:place})])
              result (await (child-outcome eng job {:plan "pen"} 200))]
          (is (= 9 (:placed result)))
          (is (= [] (:refused result))))))))

(deftest a-cell-in-another-active-plans-footprint-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan) "other" other-plan})
              result (await (child-outcome eng job {:plan "pen"} 200))]
          (is (= [{:pos [4 64 3] :reason :footprint :plan "other"}] (:refused result)))
          (is (= "air" (block p [4 64 3])))
          (is (= 8 (:placed result))))))))

(deftest no-zone-list-declines-without-a-warn
  (is (= [0 0]
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
          (let [result (await (child-outcome eng job {:plan "pen"} 200))]
            (is (= 1 (count (places p))) "the first place goes through and the other plan closes the rest")
            (is (= 1 (:placed result)))
            (is (= 8 (count (:refused result))))
            (is (= #{:footprint} (set (map :reason (:refused result)))))))))))

(defn ^:async run-beside
  "Build the pen with the fluid at 5 64 3 (beside the ring cell 4 64 3) and args: [placed, refused]."
  [fluid args]
  (let [{:keys [eng]} (start {:inventory kit :blocks {"5,64,3" fluid}} {"pen" (pen-plan)})
        result (await (child-outcome eng job (assoc args :plan "pen") 200))]
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
        result (await (child-outcome eng job {:plan "house"} 200))]
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

(deftest a-door-fits-a-gap-under-a-built-lintel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "door" :cells [[2 64 2]] :want {:block "oak_door" :facing :north :half :lower}}
                                        {:id "door-top" :cells [[2 65 2]] :want {:block "oak_door" :facing :north :half :upper}}]
                                       (assoc ground "2,66,2" "stone" "1,64,2" "stone" "1,65,2" "stone" "3,64,2" "stone" "3,65,2" "stone")
                                       [{:name "oak_door" :count 1}]))]
          (is (= [[[2 64 2] "oak_door"]] (places p)))
          (is (= {:placed 1 :missing [] :wrong [] :given-up {}} (select-keys result [:placed :missing :wrong :given-up]))))))))

(deftest a-grass-floor-where-the-plan-says-dirt-is-built-and-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "floor" :cells [[1 63 1] [2 63 1] [1 63 2]] :want "dirt"}
                                        {:id "chest" :cells [[1 64 1]] :want "chest"}]
                                       (assoc ground "1,63,1" "grass_block" "2,63,1" "grass_block" "1,63,2" "grass_block" "1,64,1" "chest")
                                       [{:name "dirt" :count 4}]))]
          (is (empty? (h/calls p "dig")))
          (is (= {:placed 0 :missing [] :wrong [] :given-up {}} (select-keys result [:placed :missing :wrong :given-up]))))))))

(deftest a-wrong-cell-under-a-standing-door-of-the-plan-is-not-dug
  (are [above dig?] (= dig? (boolean (:dig? (first (plan/prepared [{:pos [0 0 0] :answer :wrong :found "stone" :item "cobblestone" :want "cobblestone"}
                                                                   above]
                                                                  #{"cobblestone"} {} #{})))))
    {:pos [0 1 0] :answer :match :found "oak_door" :want {:block "oak_door" :half :lower}} false
    {:pos [0 1 0] :answer :missing :found "air" :want {:block "oak_door" :half :lower}} true
    {:pos [0 1 0] :answer :match :found "oak_planks" :want "oak_planks"} true))

(deftest a-bed-want-takes-any-colour-of-bed-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "bed-head" :cells [[4 64 4]] :want {:block "bed" :facing :south :part :head}}
                                        {:id "bed-foot" :cells [[4 64 3]] :want {:block "bed" :facing :south :part :foot}}]
                                       ground [{:name "brown_bed" :count 1}]))]
          (is (= [[[4 64 3] "brown_bed"]] (sort (places p))))
          (is (= {:placed 1 :missing [] :wrong [] :given-up {}} (select-keys result [:placed :missing :wrong :given-up]))))))))

(deftest a-body-standing-on-a-bed-steps-off-it-before-it-builds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (assoc ground "2,64,2" "white_bed") :self {:pos {:x 2.5 :y 64 :z 2.5}}
                                      :inventory [{:name "stone" :count 4}]}
                                     {"house" {:id "house" :parts [{:id "wall" :cells [[3 64 3]] :want "stone"}]}})
              result (await (child-outcome eng job {:plan "house"} 200))
              {:keys [x z]} (core/self-pos p)]
          (is (= [] (:missing result)))
          (is (not= [2 2] [(js/Math.floor x) (js/Math.floor z)]) "the body ended off the bed"))))))

(def bed-parts [{:id "bed-head" :cells [[4 64 4]] :want {:block "white_bed" :facing :south :part :head}}
                {:id "bed-foot" :cells [[4 64 3]] :want {:block "white_bed" :facing :south :part :foot}}])

(defn ^:async build-bed
  "Build a bed from a plan at time of day: [beds used, spawn-bed entries]."
  [time]
  (let [{:keys [eng p]} (start {:blocks ground :time time :inventory [{:name "white_bed" :count 1}]}
                               {"house" {:id "house" :parts bed-parts}})]
    (await (child-outcome eng job {:plan "house"} 200))
    [(mapv (fn [c] (let [a (.-args c)] [(.-x (.-pos a)) (.-y (.-pos a)) (.-z (.-pos a))])) (h/calls p "useOn"))
     (mapv :data (mem/entries (mem/view (:store eng)) :spawn-bed))]))

(deftest a-built-bed-leaves-the-spawn-alone-by-day-and-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [[] []] (await (build-bed 1000))))
        (is (= [[] []] (await (build-bed 14000))))))))

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
          (is (= [[2 64 2]] (:missing result))))))))

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
          (let [result (await (child-outcome eng job {:plan "house"} 200))]
            (is (= 1 (count (places p))))
            (is (empty? (h/calls p "dig")))
            (is (= [{:pos [3 64 3] :found "oak_stairs[facing=north]" :want "oak_stairs[facing=east]" :placed true}] (:wrong result)))
            (is (= [:stopped :wrong-blocks] ((juxt :status :reason) result)) "done? says not done, so the build does not end :done")))))))

(deftest the-opt-out-needs-no-zone-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:inventory kit} {"pen" (pen-plan)} nil)
              result (await (child-outcome eng job {:plan "pen" :ignore-zones? true} 200))]
          (is (= 9 (:placed result))))))))

;; ---------------------------------------------------------------- wrong blocks in wanted cells

(defn ^:async fence-run
  "Build the pen with the blocks laid and the extra plans, items and args: [result p seen]."
  [blocks inventory plans args]
  (let [{:keys [eng p seen]} (start {:blocks blocks :inventory inventory} (merge {"pen" (pen-plan)} plans))
        result (await (child-outcome eng job (merge {:plan "pen"} args) 300))]
    [result p seen]))

(deftest a-replaceable-block-in-a-wanted-cell-is-placed-into-directly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (fence-run {"4,64,3" "leaf_litter" "5,64,3" "fern" "2,64,3" "light"} kit {} {}))]
          (is (empty? (h/calls p "dig")))
          (is (= "oak_fence" (block p [4 64 3])))
          (is (= {:placed 9 :wrong [] :missing []} (select-keys result [:placed :wrong :missing]))))))))

(deftest a-clear-cell-holding-a-fluid-stays-extra-but-a-wanted-cell-holding-one-is-missing
  (is (= [:extra :missing :match]
         (mapv :answer (plan/loosened [{:answer :extra :found "water"} {:answer :wrong :found "lava"} {:answer :extra :found "fern"}])))))

(deftest a-wrong-solid-block-is-dug-by-hand-when-no-tool-is-needed-then-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (fence-run {"4,64,3" "dirt" "2,64,3" "oak_leaves"} kit {} {}))]
          (is (= 2 (count (h/calls p "dig"))))
          (is (= ["oak_fence" "oak_fence"] (mapv #(block p %) [[4 64 3] [2 64 3]])))
          (is (= {:placed 9 :wrong [] :missing [] :given-up {}} (select-keys result [:placed :wrong :missing :given-up]))))))))

(deftest every-place-goes-through-the-place-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ p seen] (await (fence-run {} kit {} {}))]
          (is (= 9 (count (h/calls p "place"))))
          (is (= 9 (count (ce/ends seen :placed)))))))))

(deftest every-dig-goes-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ p seen] (await (fence-run {"4,64,3" "dirt" "2,64,3" "oak_leaves"} kit {} {}))]
          (is (= 2 (count (h/calls p "dig"))))
          (is (= 2 (count (ce/ends seen :dug)))))))))

(deftest a-dig-with-the-last-worn-pickaxe-warns-that-no-spare-was-had
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [worn {:name "iron_pickaxe" :count 1 :durability 4 :max-durability 250}
              [_ _ seen] (await (fence-run {"4,64,3" "stone"} (conj kit worn) {} {}))]
          (is (= 1 (count (h/events-of seen :tool.no-spare)))))))))

(deftest a-wrong-hand-block-outside-the-soft-names-is-dug-by-hand-not-given-up-as-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (fence-run {"4,64,3" "ice"} kit {} {}))]
          (is (= "oak_fence" (block p [4 64 3])))
          (is (= {} (:given-up result))))))))

(deftest a-wrong-snow-block-is-dug-by-hand-with-or-without-a-shovel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[with p] (await (fence-run {"4,64,3" "snow_block"} (conj kit {:name "iron_shovel" :count 1}) {} {}))
              [without q] (await (fence-run {"4,64,3" "snow_block"} kit {} {}))]
          (is (= "oak_fence" (block p [4 64 3])))
          (is (= {} (:given-up with)))
          (is (= "oak_fence" (block q [4 64 3])))
          (is (= {} (:given-up without))))))))

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

(deftest a-door-or-gate-in-a-wanted-cell-is-kept-and-listed-wrong-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [found ["oak_door" "oak_fence_gate" "oak_trapdoor"]]
          (let [[result p] (await (fence-run {"4,64,3" found} (conj kit {:name "iron_shovel" :count 1}) {} {}))]
            (is (empty? (h/calls p "dig")) found)
            (is (= found (block p [4 64 3])))
            (is (= [4 64 3] (:pos (first (:wrong result)))))))))))

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
          (is (= [[[2 64 2] "oak_door"]] (places q)))
          (is (empty? (h/calls q "dig")))
          (is (= {:placed 1 :wrong [] :given-up {}} (select-keys litter [:placed :wrong :given-up]))))))))

(deftest a-wrong-block-where-the-item-is-not-carried-counts-as-short-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (fence-run {"4,64,3" "dirt"}
                                                [{:name "oak_fence_gate" :count 1} {:name "torch" :count 1}]
                                                {} {}))]
          (is (= "dirt" (block p [4 64 3])))
          (is (= {"oak_fence" 7} (:short result)) "the dirt cell counts with the empty ones")
          (is (some #{[4 64 3]} (:missing result)))
          (is (= [{:pos [4 64 3] :found "dirt" :want "oak_fence" :short true}] (:wrong result))))))))

(deftest every-wrong-cell-is-listed-in-the-result
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (mapv (fn [x] [x 64 0]) (range 10 40))
              wall {"wall" {:id "wall" :parts [{:id "w" :cells cells :want "oak_planks"}]}}
              guard {"other" {:id "other" :parts [{:id "o" :cells cells :want "stone"}]}}
              {:keys [eng seen]} (start {:blocks (into {} (map (fn [[x y z]] [(h/cell-key x y z) "dirt"])) cells)
                                         :inventory [{:name "oak_planks" :count 64}]}
                                        (merge wall guard))
              result (await (child-outcome eng job {:plan "wall"} 300))]
          (is (= 30 (count (:wrong result)))))))))

(def wide-ground (into {} (for [x (range -4 12) z (range -4 12)] [(h/cell-key x 63 z) "stone"])))

(deftest a-body-inside-a-closed-plan-is-never-sealed-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "box" :parts [{:id "walls" :outline [[1 64 1] [4 65 4]] :want "dirt"}
                                      {:id "roof" :box [[1 66 1] [4 66 4]] :want "dirt"}]}
              {:keys [eng p]} (start {:blocks wide-ground :inventory [{:name "dirt" :count 64}]
                                      :self {:pos {:x 2.5 :y 64 :z 2.5}}}
                                     {"box" plan} [])]
          (let [result (await (child-outcome eng job {:plan "box"} 400))
                pos (.-pos (.self p))]
            (is (= 40 (:placed result)))
            (is (< (.indexOf (mapv first (places p)) [2 66 2]) (.indexOf (mapv first (places p)) [3 65 1])) "the interior roof goes up from inside before the last wall cell shuts the room")
            (is (not (reach-room/enclosed? p :raw)))
            (is (not (and (<= 1 (js/Math.floor (.-x pos)) 4) (<= 1 (js/Math.floor (.-z pos)) 4))))))))))

(deftest a-box-built-in-a-pit-gets-its-interior-roof-before-the-last-wall-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "box" :parts [{:id "walls" :outline [[10 64 10] [13 65 13]] :want "dirt"}
                                      {:id "roof" :box [[10 66 10] [13 66 13]] :want "dirt"}]}
              pit (into {} (for [x (range 4 20) z (range 4 20) y (range 63 69)
                                 :when (or (= y 63) (not (and (<= 8 x 15) (<= 8 z 15))))]
                             [(h/cell-key x y z) "stone"]))
              {:keys [eng]} (start {:blocks pit :inventory [{:name "dirt" :count 64}]
                                    :self {:pos {:x 11.5 :y 64 :z 11.5}}}
                                   {"box" plan} [])
              result (await (child-outcome eng job {:plan "box"} 600))]
          (is (= 40 (:placed result))))))))

(deftest a-solid-block-in-a-clear-cell-is-dug-and-listed-wrong-only-when-it-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "wall" :cells [[1 64 1]] :want "dirt"}
                                        {:id "door" :cells [[3 64 3]] :want :clear}]
                                       (assoc ground "3,64,3" "oak_leaves")
                                       [{:name "dirt" :count 2}]))]
          (is (= 1 (count (h/calls p "dig"))))
          (is (= "air" (h/block-at p 3 64 3)))
          (is (= {:wrong [] :missing []} (select-keys result [:wrong :missing]))))))))

(deftest a-want-of-air-clears-the-cell-and-fetches-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (build [{:id "wall" :cells [[1 64 1]] :want "dirt"}
                                        {:id "door" :cells [[3 64 3]] :want "air"}]
                                       (assoc ground "3,64,3" "oak_leaves")
                                       [{:name "dirt" :count 2}]))]
          (is (= "air" (h/block-at p 3 64 3)))
          (is (= "dirt" (h/block-at p 1 64 1)))
          (is (not (contains? (:short result) "air")))
          (is (empty? (:fetch-failed result)))
          (is (= [] (:missing result))))))))

(defn no-support-world!
  "Make place answer no-support for a cell with no block beside, above or below it, as the real primitive does."
  [p]
  (.override (.-world p) "place"
             (fn ^:async f [token args impl]
               (let [x (.-x (.-pos args)) y (.-y (.-pos args)) z (.-z (.-pos args))
                     held? (some #(not= "air" (h/block-at p (+ x (nth % 0)) (+ y (nth % 1)) (+ z (nth % 2))))
                                 [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])]
                 (if held? (await (impl token args)) #js {:status "no-support"})))))

(deftest a-cell-with-nothing-beside-it-waits-for-its-neighbour-and-does-not-starve-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "arm" :parts [{:id "arm" :cells [[2 66 3] [3 66 3]] :want "dirt"}]}
              {:keys [eng p]} (start {:blocks (assoc wide-ground "1,66,3" "stone") :inventory [{:name "dirt" :count 8}]
                                      :self {:pos {:x 7.3 :y 64 :z 3.5}}}
                                     {"arm" plan} [])]
          (no-support-world! p)
          (let [result (await (child-outcome eng job {:plan "arm"} 400))]
            (is (= [] (:missing result)))
            (is (= {} (:given-up result)))
            (is (= 2 (:placed result)))
            (is (= "dirt" (block p [3 66 3])))))))))

(deftest a-floor-over-a-dip-with-nothing-to-click-starts-from-one-fill-block-the-body-places-under-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[2 65 2] [3 65 2] [2 65 3] [3 65 3]]
              plan {:id "floor" :parts [{:id "f" :cells cells :want "oak_planks"}]}
              {:keys [eng p]} (start {:blocks wide-ground :inventory [{:name "oak_planks" :count 4} {:name "dirt" :count 4}]
                                      :self {:pos {:x 7.5 :y 64 :z 3.5}}}
                                     {"floor" plan} [])]
          (no-support-world! p)
          (let [result (await (child-outcome eng job {:plan "floor"} 400))]
            (is (= [] (:missing result)))
            (is (= {} (:given-up result)))
            (is (every? #(= "oak_planks" (block p %)) cells))
            (is (= 1 (count (filter #(= "dirt" (block p (update % 1 dec))) cells))) "one support column, the rest of the floor grows from it")))))))

(deftest a-cell-whose-floor-was-never-seen-is-looked-at-before-it-is-refused-no-support
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "rails" :parts [{:id "r" :cells [[3 64 3]] :want "dirt"}]}
              {:keys [eng p]} (start {:inventory [{:name "dirt" :count 4}] :self {:pos {:x 3.5 :y 64 :z 5.5}}} {"rails" plan} [])
              looked? #(seq (h/calls p "look"))
              hide (fn [f unseen] (fn [pos] (if (and (< (.-y pos) 64) (not (looked?))) (unseen pos) (f pos))))]
          (aset p "seenBlockAt" (hide (.-seenBlockAt p) (fn [pos] #js {:unknown true :pos pos})))
          (aset p "sensedAt" (hide (.-sensedAt p) (fn [_] #js {:unknown true})))
          (no-support-world! p)
          (let [result (await (child-outcome eng job {:plan "rails"} 200))]
            (is (seq (places p)))
            (is (= {} (:given-up result)))
            (is (= 1 (:placed result)))))))))

(deftest a-cell-no-neighbour-ever-supports-is-given-up-as-no-support-and-never-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "float" :parts [{:id "f" :cells [[3 66 3]] :want "dirt"}]}
              {:keys [eng p seen]} (start {:blocks wide-ground :inventory [{:name "dirt" :count 8}]
                                           :self {:pos {:x 3.5 :y 64 :z 6.5}}}
                                          {"float" plan} [])]
          (no-support-world! p)
          (let [result (await (child-outcome eng job {:plan "float"} 200))]
            (is (empty? (places p)))
            (is (= {[3 66 3] :no-support} (:given-up result)))
            (is (= :stopped (:status result)))
            (is (= :incomplete (:reason result)))
            (is (re-find #"still missing 1" (:text result)))
            (is (re-find #"no-support" (:text result)))))))))

(deftest a-build-that-leaves-cells-ends-stopped-and-a-complete-one-does-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:inventory [{:name "oak_fence" :count 4} {:name "oak_fence_gate" :count 1}
                                                {:name "torch" :count 1}]}
                                   {"pen" (pen-plan)})
              short (await (child-outcome eng job {:plan "pen"} 200))
              {eng2 :eng} (start {:inventory kit} {"pen" (pen-plan)})
              full (await (child-outcome eng2 job {:plan "pen"} 200))]
          (is (= :stopped (:status short)))
          (is (re-find #"still missing 3" (:text short)))
          (is (re-find #"short of oak_fence 3" (:text short)))
          (is (nil? (:status full))))))))

(deftest a-body-in-a-pit-it-cannot-climb-is-already-shut-in-so-no-place-is-held-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pit (into {} (for [x (range 0 10) z (range 0 10) y (range 63 69)
                                 :when (or (= y 63) (not (and (<= 3 x 6) (<= 3 z 6))))]
                             [(h/cell-key x y z) "stone"]))
              {:keys [p]} (start {:blocks pit :self {:pos {:x 4.5 :y 64 :z 4.5}}} {} [])]
          (is (reach-room/enclosed? p :raw))
          (is (not (fp-cells/seals? {:primitives p} [4 65 4])))
          (is (not (fp-cells/seals? {:primitives p} [4 64 3]))))))))

(deftest a-box-on-a-slope-is-built-complete-with-the-body-starting-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ground (into {} (concat (for [x (range 5 23) z (range 5 23)] [(h/cell-key x 63 z) "stone"])
                                      (for [x (range 15 23) z (range 5 23)] [(h/cell-key x 64 z) "stone"])
                                      (for [c [[11 11] [12 11] [11 12]]] [(h/cell-key (first c) 64 (second c)) "stone"])))
              plan {:id "box" :parts [{:id "walls" :outline [[10 64 10] [13 65 13]] :want "dirt"}]}
              {:keys [eng]} (start {:blocks ground :inventory [{:name "dirt" :count 64}]
                                    :self {:pos {:x 11.5 :y 65 :z 11.5}}}
                                   {"box" plan} [])
              result (await (child-outcome eng job {:plan "box"} 600))]
          (is (= [] (:missing result)))
          (is (= {} (:given-up result)))
          (is (= 24 (:placed result))))))))

(deftest stand-cells-are-put-on-the-ground-where-it-rises-or-falls
  (let [ground (into {} (concat (for [x (range 5 23) z (range 5 23)] [(h/cell-key x 63 z) "stone"])
                                (for [x (range 15 23) z (range 5 23)] [(h/cell-key x 64 z) "stone"])))
        p (tu/fake-on-floor {:blocks ground :self {:pos {:x 12.5 :y 64 :z 14.5}}})
        stands (plan/ground-stands p [[15 64 10] [14 64 10] [13 64 8] [16 64 12]])]
    (is (= [[15 65 10] [14 64 10] [13 64 8] [16 65 12]] (vec stands)))))

(deftest a-bed-is-no-floor-for-a-stand-above-it
  (let [ground (into {(h/cell-key 14 64 10) "white_bed"} (for [x (range 5 23) z (range 5 23)] [(h/cell-key x 63 z) "stone"]))
        p (tu/seeing-all (tu/fake-on-floor {:blocks ground :self {:pos {:x 12.5 :y 64 :z 14.5}}}))]
    (is (= [[13 64 8]] (vec (plan/ground-stands p [[14 64 10] [13 64 8]]))))))

(deftest only-a-low-floor-is-no-stand
  (doseq [[block state stand] [["white_bed" {} [13 64 8]] ["oak_slab" {:type "bottom"} [13 64 8]] ["oak_slab" {:type "top"} [14 65 10]]
                               ["oak_slab" {:type "double"} [14 65 10]]]]
    (let [ground (into {(h/cell-key 14 64 10) block} (for [x (range 5 23) z (range 5 23)] [(h/cell-key x 63 z) "stone"]))
          p (tu/seeing-all (tu/fake-on-floor {:blocks ground :states {(h/cell-key 14 64 10) state} :self {:pos {:x 12.5 :y 64 :z 14.5}}}))]
      (is (= [stand] (take 1 (plan/ground-stands p [[14 64 10] [13 64 8]]))) (str block state)))))

(deftest a-stand-cell-holding-a-bed-is-no-stand
  (let [ground (into {(h/cell-key 14 64 10) "white_bed"} (for [x (range 5 23) z (range 5 23)] [(h/cell-key x 63 z) "stone"]))
        p (tu/seeing-all (tu/fake-on-floor {:blocks ground :self {:pos {:x 12.5 :y 64 :z 14.5}}}))]
    (is (not-any? #{[14 64 10]} (plan/ground-stands p [[14 63 10] [13 64 8]])))))

(def fence-chest "-2,64,3")

(defn chest-fences [p] (some #(when (= "oak_fence" (:name %)) (:count %)) (get-in @(engine.fake/state p) [:containers [-2 64 3]])))

(defn ^:async fence-fetch
  "The pen plan with the gate and torches carried and seven fences in a seen chest; the result and the body."
  [args]
  (let [spec {:inventory [{:name "oak_fence_gate" :count 1} {:name "torch" :count 4}]
              :blocks {fence-chest "chest"} :containers {fence-chest [{:name "oak_fence" :count 7}]}}
        {:keys [eng p]} (start spec {"pen" (pen-plan)})]
    (tu/seeing-all p)
    {:result (await (child-outcome eng job (merge {:plan "pen"} args) 300)) :p p}))

(deftest a-cell-inside-the-ground-is-buried-not-unloaded
  (let [block (fn [x y z] [(h/cell-key x y z) "stone"])
        around (into {} (map (fn [[x y z]] (block x y z))) [[3 65 3] [3 63 3] [4 64 3] [2 64 3] [3 64 4] [3 64 2]])
        p (tu/seeing-all (tu/fake-on-floor {:blocks around}))]
    (is (true? (fp-cells/buried? p [3 64 3])))
    (is (false? (fp-cells/buried? p [8 64 3])) "open air round it")
    (is (false? (fp-cells/buried? (tu/seeing-all (tu/fake-on-floor {:blocks (dissoc around (h/cell-key 3 65 3))})) [3 64 3]))
        "air above it")))

(deftest surface-follow-moves-each-column-onto-its-seen-ground
  (let [cells [{:pos [1 64 1] :want "oak_fence"} {:pos [1 65 1] :want "torch"} {:pos [4 64 1] :want "oak_fence"} {:pos [7 64 1] :want "oak_fence"}]
        tops {[1 1] 62 [4 1] 70}
        moved (surface/follow cells (fn [x z _] (tops [x z])))]
    (is (= [[1 63 1] [1 64 1] [4 71 1] [7 64 1]] (mapv :pos moved)) "a column moves as a whole; an unseen one stays")))

(deftest surface-follow-plan-keeps-blueprint-parts-flat
  (let [plan {:parts [{:id "hut" :blueprint "h" :at [1 64 1]} {:id "fence" :cells [[1 64 1]] :want "oak_fence"}]}
        cells [{:pos [1 64 1] :want "dirt" :part "hut"} {:pos [1 65 1] :want "dirt" :part "hut"} {:pos [4 64 1] :want "oak_fence" :part "fence"}]
        moved (surface/follow-plan plan cells (fn [& _] 70))]
    (is (= [[1 64 1] [1 65 1] [4 71 1]] (mapv :pos moved)) "only the area part follows the ground")))

(deftest a-surface-build-puts-the-fence-on-the-ground-of-every-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hill (into {} (for [x (range 0 8) z (range 0 4) y (range 62 (if (<= x 2) 64 65))] [(h/cell-key x y z) "stone"]))
              plan {:id "slope" :surface true :parts [{:id "fence" :cells [[1 64 1] [4 64 1]] :want "oak_fence"}]}
              {:keys [eng p]} (start {:blocks hill :inventory [{:name "oak_fence" :count 4}] :self {:pos {:x 2.5 :y 64 :z 2.5}}} {"slope" plan})
              result (await (child-outcome eng job {:plan "slope"} 200))]
          (is (= "oak_fence" (block p [1 64 1])))
          (is (= "oak_fence" (block p [4 65 1])) "the column on the higher ground")
          (is (= [] (:missing result))))))))

(deftest a-surface-build-ignores-the-canopy-over-the-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hill (into {} (for [x (range 0 8) z (range 0 4) y (range 62 64)] [(h/cell-key x y z) "stone"]))
              canopy (into {} (for [z (range 0 4) y [66 67]] [(h/cell-key 4 y z) "birch_leaves"]))
              plan {:id "wood" :surface true :parts [{:id "fence" :cells [[4 64 1]] :want "oak_fence"}]}
              {:keys [eng p]} (start {:blocks (merge hill canopy) :inventory [{:name "oak_fence" :count 2}] :self {:pos {:x 2.5 :y 64 :z 2.5}}} {"wood" plan})
              _ (await (child-outcome eng job {:plan "wood"} 200))]
          (is (= "oak_fence" (block p [4 64 1])) "on the ground under the leaves, not on them"))))))

(deftest what-is-carried-is-placed-before-the-rest-is-fetched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:inventory [{:name "oak_fence_gate" :count 1} {:name "torch" :count 4} {:name "oak_fence" :count 3}]
                    :blocks {fence-chest "chest"} :containers {fence-chest [{:name "oak_fence" :count 4}]}}
              {:keys [eng p]} (start spec {"pen" (pen-plan)})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "pen"} 300))
              names (mapv #(.-name %) (.-calls (.-world p)))
              first-place (.indexOf names "place")
              first-take (.indexOf names "transfer")]
          (is (= [] (:missing result)))
          (is (pos? first-take))
          (is (< first-place first-take) "the carried fences go in before the body walks to the chest"))))))

(deftest the-walls-are-fetched-before-a-roof-cell-with-nothing-below-it-is-laid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "walls" :cells [[2 64 2] [4 64 2]] :want "cobblestone"}
                                      {:id "roof" :cells [[3 65 2]] :want "oak_planks"}]}
              spec {:inventory [{:name "oak_planks" :count 2}]
                    :blocks (assoc ground fence-chest "chest" (h/cell-key 3 65 1) "stone")
                    :containers {fence-chest [{:name "cobblestone" :count 2}]}}
              {:keys [eng p]} (start spec {"hut" plan})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "hut"} 300))]
          (is (= [] (:missing result)))
          (is (= ["cobblestone" "cobblestone" "oak_planks"] (mapv second (places p))) "the walls first, then the roof"))))))

(deftest a-wall-on-a-laid-ring-goes-up-before-the-floor-inside-is-fetched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "ring" :cells [[2 64 2] [4 64 2]] :want "cobblestone"}
                                      {:id "floor" :cells [[3 64 2]] :want "oak_planks"}
                                      {:id "walls" :cells [[2 65 2] [4 65 2]] :want "cobblestone"}]}
              spec {:inventory [{:name "cobblestone" :count 2}]
                    :blocks (assoc ground fence-chest "chest" (h/cell-key 2 64 2) "cobblestone" (h/cell-key 4 64 2) "cobblestone")
                    :containers {fence-chest [{:name "oak_planks" :count 2}]}}
              {:keys [eng p]} (start spec {"hut" plan})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "hut"} 300))
              names (mapv #(.-name %) (.-calls (.-world p)))]
          (is (= [] (:missing result)))
          (is (< (.indexOf names "place") (.indexOf names "transfer")) "the carried walls go up before the body walks to the chest for the floor"))))))

(deftest a-missing-bed-is-fetched-after-the-carried-cells-above-its-level
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "house" :parts [{:id "bed-head" :cells [[4 64 4]] :want {:block "bed" :facing :south :part :head}}
                                        {:id "bed-foot" :cells [[4 64 3]] :want {:block "bed" :facing :south :part :foot}}
                                        {:id "roof" :cells [[3 65 2]] :want "oak_planks"}]}
              spec {:inventory [{:name "oak_planks" :count 1}]
                    :blocks (assoc ground fence-chest "chest" (h/cell-key 3 65 1) "stone")
                    :containers {fence-chest [{:name "red_bed" :count 1}]}}
              {:keys [eng p]} (start spec {"house" plan})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "house"} 300))
              names (mapv #(.-name %) (.-calls (.-world p)))]
          (is (= [] (:missing result)))
          (is (< (.indexOf names "place") (.indexOf names "transfer")) "the carried planks go in before the body goes for the bed"))))))

(deftest a-plan-bed-is-the-bed-the-body-put-down-itself-taken-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "house" :parts [{:id "bed-head" :cells [[4 64 4]] :want {:block "bed" :facing :south :part :head}}
                                        {:id "bed-foot" :cells [[4 64 3]] :want {:block "bed" :facing :south :part :foot}}]}
              {:keys [eng p]} (start {:blocks (assoc ground "1,64,1" "red_bed")} {"house" plan})
              _ (ledger/write! (:store eng) [{:cell [1 64 1] :item "red_bed" :before "air" :job "j9" :purpose :bed :state :placed}])
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "house"} 300))]
          (is (= [] (:missing result)))
          (is (= "red_bed" (block p [4 64 3])) "the plan's bed is the one taken up")
          (is (= "air" (block p [1 64 1])) "from where the night put it down"))))))

(deftest a-scaffold-cell-the-plan-wants-as-its-wall-leaves-the-ledger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "wall" :parts [{:id "w" :cells [[1 64 1] [2 64 1]] :want "dirt"}]}
              {:keys [eng p]} (start {:blocks (assoc ground "1,64,1" "dirt") :inventory [{:name "dirt" :count 1}]} {"wall" plan})
              _ (ledger/write! (:store eng) [{:cell [1 64 1] :item "dirt" :before "air" :job "j9/pillar" :purpose :pillar :state :placed}])
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "wall"} 300))]
          (is (= [] (:missing result)))
          (is (= "dirt" (block p [1 64 1])))
          (is (empty? (ledger/open-entries (mem/view (:store eng)))) "a cleanup would dig the wall out otherwise"))))))

(deftest a-scaffold-block-landing-on-a-plan-cell-in-the-final-step-is-adopted-by-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "wall" :parts [{:id "w" :cells [[1 64 1]] :want "dirt"}]}
              {:keys [eng p]} (start {:blocks ground :inventory [{:name "dirt" :count 1}]} {"wall" plan})
              _ (ledger/write! (:store eng) [{:cell [1 64 1] :item "dirt" :before "air" :job "j9/pillar" :purpose :pillar :state :placed}])
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "wall"} 300))]
          (is (= [] (:missing result)))
          (is (= "dirt" (block p [1 64 1])) "the wall stays after cleanup")
          (is (empty? (ledger/open-entries (mem/view (:store eng))))))))))

(deftest missing-material-is-fetched-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (fence-fetch {}))]
          (is (every? #(= "oak_fence" (block p %)) ring))
          (is (= {:placed 9 :missing []} (select-keys result [:placed :missing])))
          (is (nil? (chest-fences p))))))))

(deftest a-bed-want-fetches-a-bed-of-any-colour-from-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (assoc ground fence-chest "chest") :containers {fence-chest [{:name "red_bed" :count 1}]}}
                                     {"house" {:id "house" :parts [{:id "bed-head" :cells [[4 64 4]] :want {:block "bed" :facing :south :part :head}}
                                                                   {:id "bed-foot" :cells [[4 64 3]] :want {:block "bed" :facing :south :part :foot}}]}})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "house"} 300))]
          (is (= [] (:missing result)))
          (is (= "red_bed" (block p [4 64 3]))))))))

(deftest fetch-false-builds-what-it-can-and-leaves-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [result p]} (await (fence-fetch {:ways #{:carried}}))]
          (is (= {"oak_fence" 7} (:short result)))
          (is (= 7 (chest-fences p))))))))

(deftest a-missing-pickaxe-is-fetched-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks {"4,64,3" "stone" fence-chest "chest"} :inventory kit
                    :containers {fence-chest [{:name "stone_pickaxe" :count 1}]}}
              {:keys [eng p]} (start spec {"pen" (pen-plan)})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "pen"} 300))]
          (is (= "oak_fence" (block p [4 64 3])))
          (is (= [] (:wrong result)))
          (is (= {} (:given-up result))))))))

(deftest fetch-false-gives-up-a-pickaxe-block-as-no-tool-and-leaves-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks {"4,64,3" "stone" fence-chest "chest"} :inventory kit
                    :containers {fence-chest [{:name "stone_pickaxe" :count 1}]}}
              {:keys [eng p]} (start spec {"pen" (pen-plan)})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "pen" :ways #{:carried}} 300))]
          (is (= {[4 64 3] :no-tool} (:given-up result)))
          (is (= "stone" (block p [4 64 3]))))))))

(deftest fetch-false-gives-up-a-cobweb-and-stone-clear-with-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "row" :parts [{:id "clear" :cells [[4 64 3] [5 64 3]] :want :clear}]}
              {:keys [eng p]} (start {:blocks {"4,64,3" "cobweb" "5,64,3" "stone"} :inventory []} {"row" plan})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "row" :ways #{:carried}} 300))]
          (is (= {[4 64 3] :no-tool [5 64 3] :no-tool} (:given-up result)))
          (is (= "cobweb" (block p [4 64 3]))))))))

(deftest the-walk-to-a-stand-never-escalates-and-a-waiting-walk-counts-no-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              opts (atom [])
              {:keys [eng]} (start {:inventory kit} {"far" plan})
              result (await (tu/with-near-stub (fn ^:async f [_ _ _ o] (swap! opts conj o) :partial)
            (fn ^:async b [] (await (child-outcome eng job {:plan "far"} 6)))))]
          (is (seq @opts))
          (is (every? #(= #{:walk :clear-seal} (:ways %)) @opts) "a walk to a stand never digs through the build")
          (is (not= :unreachable (get-in result [:given-up [40 64 0]]))))))))

(deftest a-body-in-a-hole-walks-with-escalation-so-it-can-climb-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              hole (into {} (for [x [3 4 5] z [3 4 5] y [64 65 66] :when (not= [4 4] [x z])] [(h/cell-key x y z) "dirt"]))
              opts (atom [])
              {:keys [eng]} (start {:inventory kit :blocks hole :self {:pos {:x 4.5 :y 64 :z 4.5}}} {"far" plan})
              _ (await (tu/with-near-stub (fn ^:async f [_ _ _ o] (swap! opts conj o) :partial)
                         (fn ^:async b [] (await (child-outcome eng job {:plan "far"} 6)))))]
          (is (seq @opts))
          (is (every? (comp nil? :ways) @opts) "shut in by its own dig: go-to may pillar or stair out"))))))

(deftest a-body-go-to-finds-no-way-out-of-its-hole-for-stops-shut-in-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "posts" :cells [[40 64 0] [41 64 0] [42 64 0] [43 64 0]] :want "oak_fence"}]}
              hole (into {} (for [x [3 4 5] z [3 4 5] y [64 65 66] :when (not= [4 4] [x z])] [(h/cell-key x y z) "dirt"]))
              walks (atom 0)
              {:keys [eng]} (start {:inventory kit :blocks hole :self {:pos {:x 4.5 :y 64 :z 4.5}}} {"far" plan})
              result (await (tu/with-near-stub (fn ^:async f [c _ _ _]
                                                 (swap! walks inc)
                                                 (ctx/update-mem! c assoc :near-walk {:data {:why :exhausted :shut-in true}})
                                                 :blocked)
                              (fn ^:async b [] (await (child-outcome eng job {:plan "far"} 30)))))]
          (is (= :stopped (:status result)))
          (is (= :shut-in (:reason result)))
          (is (re-find #"no way out" (:text result)))
          (is (= 1 @walks) "go-to's verdict on the one escalating walk decides, no count of walks"))))))

(deftest blocked-walks-from-a-hole-without-a-shut-in-verdict-give-the-cells-up-and-build-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [posts [[40 64 0] [41 64 0] [42 64 0] [43 64 0]]
              plan {:id "far" :parts [{:id "posts" :cells posts :want "oak_fence"}]}
              hole (into {} (for [x [3 4 5] z [3 4 5] y [64 65 66] :when (not= [4 4] [x z])] [(h/cell-key x y z) "dirt"]))]
          (doseq [why [:exhausted :stuck]]
            (let [{:keys [eng]} (start {:inventory kit :blocks hole :self {:pos {:x 4.5 :y 64 :z 4.5}}} {"far" plan})
                  result (await (tu/with-near-stub (fn ^:async f [c _ _ _]
                                                     (ctx/update-mem! c assoc :near-walk {:data {:why why}})
                                                     :blocked)
                                  (fn ^:async b [] (await (child-outcome eng job {:plan "far"} 60)))))]
              (is (= :incomplete (:reason result)) (pr-str why result))
              (is (= (zipmap posts (repeat :unreachable)) (:given-up result)) "each cell given up for its own failed walks"))))))))

(deftest a-walk-that-gives-up-on-its-node-budget-is-no-hole-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "posts" :cells [[40 64 0] [41 64 0] [42 64 0] [43 64 0]] :want "oak_fence"}]}
              hole (into {} (for [x [3 4 5] z [3 4 5] y [64 65 66] :when (not= [4 4] [x z])] [(h/cell-key x y z) "dirt"]))
              {:keys [eng]} (start {:inventory kit :blocks hole :self {:pos {:x 4.5 :y 64 :z 4.5}}} {"far" plan})
              result (await (tu/with-near-stub (fn ^:async f [c _ _ _]
                                                 (ctx/update-mem! c assoc :near-walk {:data {:why :budget}})
                                                 :blocked)
                              (fn ^:async b [] (await (child-outcome eng job {:plan "far"} 30)))))]
          (is (= :unreachable (:reason result)) "a node-budget give-up is no verdict on a way out: the budget streak ends it"))))))

(deftest a-body-shut-in-by-a-walk-stops-shut-in-instead-of-walking-to-every-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "posts" :cells [[40 64 0] [41 64 0] [42 64 0] [43 64 0]] :want "oak_fence"}]}
              walks (atom [])
              walled (apply dissoc (tu/box 2 64 2 6 65 6 "stone") (for [x (range 3 6) y [64 65] z (range 3 6)] (str x "," y "," z)))
              {:keys [eng]} (start {:inventory kit :blocks walled :self {:pos {:x 4.5 :y 64 :z 4.5}}} {"far" plan})
              result (await (tu/with-near-stub (fn ^:async f [c _ _ o]
                                                 (swap! walks conj o)
                                                 (ctx/update-mem! c assoc :near-walk {:data {:why :exhausted :shut-in true}})
                                                 :blocked)
                              (fn ^:async b [] (await (child-outcome eng job {:plan "far" :settings {:jobs.build.from-plan.walk/pocket-cells 1000000}} 30)))))]
          (is (= :shut-in (:reason result)))
          (is (<= (count @walks) 3) "not one failed walk per planned cell")
          (is (every? (comp nil? :ways) (rest @walks)) "after a shut-in verdict go-to may dig or build out"))))))

(deftest a-shut-in-verdict-in-a-room-larger-than-a-pocket-is-not-a-stop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "posts" :cells [[40 64 0]] :want "oak_fence"}]}
              {:keys [eng]} (start {:inventory kit :self {:pos {:x 4.5 :y 64 :z 4.5}}} {"far" plan})
              result (await (tu/with-near-stub (fn ^:async f [c _ _ _]
                                                 (ctx/update-mem! c assoc :near-walk {:data {:why :exhausted :shut-in true}})
                                                 :blocked)
                              (fn ^:async b [] (await (child-outcome eng job {:plan "far" :settings {:jobs.build.from-plan.walk/pocket-cells 1}} 30)))))]
          (is (not= :shut-in (:reason result))))))))

(deftest a-stand-is-priced-by-the-walk-there-and-the-walks-on-to-the-cells-it-leaves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:floor [-30 -30 30 30] :self {:pos [4.5 64 0.5]}})
              c {:primitives p :view (constantly {:data {} :now 0}) :update-mem (constantly nil)}
              row (mapv (fn [x] {:pos [x 64 3] :want "dirt"}) (range 5 12))
              walked (atom nil)]
          ;; [4 64 2] is nearest but reaches only the row's west half; [8 64 2] reaches all of it
          (with-redefs [fp-walk/stand-choices (constantly {:all [[4 64 2] [8 64 2]]})
                        fp-cells/seeing (fn [_ stands _] stands)
                        fp-walk/walk-stand! (fn [_ _ stand _] (reset! walked stand) (js/Promise.resolve :there))
                        near/last-walk (constantly nil)]
            (await (fp-walk/walk-to-stand! c row (nth row 1))))
          (is (= [8 64 2] @walked)))))))

(deftest a-shut-in-verdict-counts-a-pocket-only-over-walls-the-body-has-seen
  (let [p (tu/fake {:blocks (apply dissoc (tu/box -1 64 -1 3 66 1 "stone") (for [x (range 0 3) y [64 65]] (str x "," y ",0")))
                    :floor [-30 -30 30 30] :self {:pos [1 64 0]}})
        sensed (.-sensedAt p)
        c {:primitives p :view (constantly {:data {} :now 0})}
        shut-in? #(with-redefs [near/last-walk (constantly {:data {:shut-in true}}) near/last-goal (constantly nil)] (fp-walk/shut-in-walk? c))]
    (is (true? (shut-in?)) "walls all seen: a pocket")
    (aset p "sensedAt" (fn [q] (if (#{[1 64 1] [1 65 1]} [(.-x q) (.-y q) (.-z q)]) #js {:unknown true} (sensed q))))
    (is (false? (shut-in?)) "a wall never looked at may be a gap: not a pocket")))

(deftest a-body-sealed-in-its-own-shelter-waits-instead-of-stopping-shut-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :parts [{:id "posts" :cells [[40 64 0] [41 64 0] [42 64 0] [43 64 0]] :want "oak_fence"}]}
              walks (atom 0)
              {:keys [eng]} (start {:inventory kit :self {:pos {:x 4.5 :y 64 :z 4.5}}} {"far" plan})
              result (await (tu/with-near-stub (fn ^:async f [c _ _ _]
                                                 (swap! walks inc)
                                                 (ctx/update-mem! c assoc :near-walk {:data {:why :exhausted :inside-own-shelter {:x 4 :y 64 :z 4}}})
                                                 :blocked)
                              (fn ^:async b [] (await (child-outcome eng job {:plan "far"} 12)))))]
          (is (not= :shut-in (:reason result)))
          (is (empty? (:given-up result)) "the shelter is no verdict on the cells")
          (is (< 3 @walks) "the build keeps trying while the shelter is shut"))))))

(deftest a-whole-plan-is-built-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:inventory kit} {"pen" (pen-plan)})
              result (await (child-outcome eng job {:plan "pen"} 1))]
          (is (= {:placed 9 :missing [] :short {} :given-up {} :wrong [] :refused [] :text "build of pen done: placed 9"} (dissoc result :spent :spent-at))))))))

(deftest a-place-that-finds-no-item-or-an-occupied-cell-is-given-up-after-the-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [status ["no-item" "occupied"]]
          (let [{:keys [eng p]} (start {:inventory kit} {"post" {:id "post" :parts [{:id "p" :cells [[3 64 3]] :want "oak_fence"}]}})
                _ (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status status}))
                result (await (child-outcome eng job {:plan "post"} 1))]
            (is (= {[3 64 3] (keyword status)} (:given-up result)) status)
            (is (= 3 (count (places p))) status)))))))

(deftest a-cut-right-after-a-place-keeps-the-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan)})
              out (atom nil)
              cut? (atom false)
              parent (tu/recording-parent job {:plan "pen"} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (when-not @cut?
                           (reset! cut? true)
                           (core/cut! eng (core/holder eng) :test nil))
                         r)))
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (await (h/run-until-empty eng 40))
          (is (= 9 (:placed @out))))))))

(deftest a-rail-shape-in-the-place-result-is-no-misplacement
  (are [want block text] (= text (plan/misplaced want "rail" block))
    {:block "rail" :shape "south_east"} {:name "rail" :state {:shape "north_south"}} nil
    {:block "powered_rail" :shape "north_south" :powered true} {:name "powered_rail" :state {:shape "north_south" :powered "false"}} nil
    {:block "rail" :shape "south_east"} {:name "stone"} "stone"
    {:block "lever" :facing "north"} {:name "lever" :state {:facing "south"}} "lever[facing=south]"))

(deftest a-rail-of-the-wanted-kind-in-another-shape-is-left-to-settle-not-dug
  (are [found dig?] (= dig? (boolean (:dig? (first (plan/prepared [{:pos [0 0 0] :answer :wrong :found found :item "rail"
                                                                    :want {:block "rail" :shape "east_west"}}]
                                                                  {"rail" 3} {} #{})))))
    "rail" false
    "stone" true))

(deftest an-extra-chest-in-a-clear-cell-is-dug-only-when-the-body-placed-it
  (are [found own dig?] (= dig? (boolean (:dig? (first (plan/prepared [{:pos [0 0 0] :answer :extra :found found :want :clear}] #{} {} own)))))
    "chest" #{[[0 0 0] "chest"]} true
    "crafting_table" #{[[0 0 0] "crafting_table"]} true
    "chest" #{} false
    "chest" #{[[1 0 0] "chest"]} false
    "dirt" #{} true))

(deftest a-plan-cell-under-the-feet-is-dug-from-beside-not-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "floor" :parts [{:id "f" :cells [[0 63 0]] :want "oak_planks"}]}
              {:keys [eng p]} (start {:self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,63,0" "dirt"}
                                      :inventory [{:name "oak_planks" :count 4}]}
                                     {"floor" plan})
              result (await (child-outcome eng job {:plan "floor"} 200))]
          (is (= [] (:refused result)) "the floor under it is unseen: not a hazard to refuse, a dig to make from beside")
          (is (= "oak_planks" (block p [0 63 0]))))))))

(deftest a-body-standing-on-the-wall-it-built-steps-off-to-place-its-own-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[2 64 2] [2 65 2] [3 65 2]]
              {:keys [eng p]} (start {:self {:pos {:x 2.5 :y 65 :z 2.5}} :blocks {"2,64,2" "dirt"}
                                      :inventory [{:name "dirt" :count 4}]}
                                     {"wall" {:id "wall" :parts [{:id "w" :cells cells :want "dirt"}]}})
              result (await (child-outcome eng job {:plan "wall"} 200))]
          (is (= [] (:missing result)) "the cell it stood in is placed once it stepped off")
          (is (every? #(= "dirt" (block p %)) cells)))))))

;; ---------------------------------------------------------------- lava a dig lays open

(deftest a-clear-cell-over-hidden-lava-is-dug-and-the-lava-under-it-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (assoc ground "3,63,3" "dirt" "3,62,3" "lava" "3,61,3" "stone")
                                      :inventory [{:name "cobblestone" :count 2}] :self {:pos {:x 4 :y 64 :z 3}}}
                                     {"house" {:id "house" :parts [{:id "door" :cells [[3 63 3]] :want :clear}]}})]
          (bd/hiding p #(= [3 62 3] %))
          (await (child-outcome eng job {:plan "house"} 200))
          (is (= "air" (block p [3 63 3])) "the plan's cell stays clear")
          (is (= "cobblestone" (block p [3 62 3])) "the lava under it is sealed"))))))

(deftest an-unseen-clear-cell-is-looked-at-and-dug-not-taken-as-clear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "near" :parts [{:id "clear" :cells [[3 64 0]] :want :clear}]}
              [seen sink] (tu/legacy-capture-sink)
              p (tu/seeing-after-look (tu/fake-on-floor {:inventory [] :blocks {"3,64,0" "dirt"}}))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                                :world (world/of-data {"near" plan} {} [])})
              result (await (child-outcome eng job {:plan "near"} 200))]
          (is (seq (h/calls p "look")))
          (is (= {} (:given-up result)))
          (is (= [] (:missing result)))
          (is (= "air" (block p [3 64 0]))))))))

(deftest an-item-that-cannot-be-fetched-does-not-stop-the-fetch-of-the-next-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:inventory [{:name "oak_fence_gate" :count 1}]
                    :blocks {fence-chest "chest"} :containers {fence-chest [{:name "torch" :count 4}]}}
              {:keys [eng p]} (start spec {"pen" (pen-plan)})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "pen"} 300))]
          (is (= {"oak_fence" 7} (:short result)) "the torch was fetched though the fences could not be")
          (is (= #{"oak_fence"} (set (keys (:fetch-failed result)))))
          (is (= :stopped (:status result)))
          (is (re-find #"could not fetch oak_fence: no-source" (:text result)) (:text result)))))))

(deftest a-no-source-exact-wood-ending-carries-the-generic-name-hint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:inventory [{:name "oak_fence_gate" :count 1} {:name "torch" :count 4} {:name "birch_fence" :count 9}]
                    :blocks {fence-chest "chest"} :containers {fence-chest [{:name "torch" :count 4}]}}
              {:keys [eng p]} (start spec {"pen" (pen-plan)})
              _ (tu/seeing-all p)
              result (await (child-outcome eng job {:plan "pen"} 300))
              hint "carries birch_fence 9; the blueprint can use generic wooden_fences"]
          (is (= hint (get-in result [:fetch-failed "oak_fence" :hint])) (pr-str (:fetch-failed result)))
          (is (re-find (re-pattern (str "could not fetch oak_fence: no-source.* - " hint)) (:text result)) (:text result)))))))

(deftest a-failed-item-fetch-does-not-hold-back-the-pickaxe-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:inventory [{:name "oak_fence_gate" :count 1} {:name "oak_fence" :count 2}]
                    :blocks {"4,64,3" "stone" fence-chest "chest"}
                    :containers {fence-chest [{:name "stone_pickaxe" :count 1}]}}
              {:keys [eng p seen]} (start spec {"pen" (pen-plan)})
              _ (tu/seeing-all p)
              orig ctx/call
              tool-calls (atom [])
              _ (set! ctx/call (fn [c slot j args]
                                 (when (= 'jobs.items.get-tool j) (swap! tool-calls conj args))
                                 (orig c slot j args)))
              result (try (await (child-outcome eng job {:plan "pen"} 300))
                          (finally (set! ctx/call orig)))]
          (is (contains? (:fetch-failed result) "oak_fence") (pr-str result))
          (is (some #(= {:block "stone"} (select-keys % [:block])) @tool-calls) "the pickaxe fetch is not held back")
          (is (= {} (:given-up result))))))))

(deftest cobblestone-with-no-pickaxe-is-mined-with-a-pickaxe-crafted-from-carried-logs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "step" :parts [{:id "s" :cells [[3 64 3] [4 64 3]] :want "cobblestone"}]}
              stone (into {} (for [x [-1 0] z [5 6] [y b] [[63 "andesite"] [64 "stone"]]] [(str x "," y "," z) b]))
              spec {:blocks stone :drops {"stone" "cobblestone"} :inventory [{:name "oak_log" :count 4}]}
              {:keys [eng p]} (start spec {"step" plan})
              result (await (child-outcome eng job {:plan "step" :explore false} 600))]
          (is (= "cobblestone" (block p [3 64 3])) (pr-str (select-keys result [:short :fetch-failed :text])))
          (is (= "cobblestone" (block p [4 64 3])))
          (is (= [] (:missing result))))))))

(deftest the-default-fetch-explores-for-what-is-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig ctx/call
              seen (atom [])
              spec {:inventory [{:name "oak_fence_gate" :count 1}]}
              {:keys [eng p]} (start spec {"pen" (pen-plan)})]
          (tu/seeing-all p)
          (set! ctx/call (fn [c slot j args]
                           (when (= 'jobs.items.obtain j) (swap! seen conj args))
                           (orig c slot j args)))
          (try
            (await (child-outcome-exploring eng job {:plan "pen"} 20))
            (finally (set! ctx/call orig)))
          (is (some #(and (= "oak_fence" (:item %)) (true? (:explore %)) (= "pen" (:for-plan %)) (vector? (:center %))) @seen) (pr-str @seen)))))))

(deftest stand-levels-are-the-cells-first-then-the-bodys-when-near
  (are [body cell levels] (= levels (plan/stand-levels body cell))
    64 64 [64]
    62 64 [64 62]
    67 64 [64 67]
    68 64 [64]
    48 64 [64]
    80 64 [64]))

(deftest a-body-near-the-threshold-above-the-cell-aims-at-the-cells-ground-not-its-own-level
  (let [p (tu/seeing-all (tu/fake-on-floor {:blocks {}}))
        stands (plan/stands-for p [40 64 0] 67 nil #{} #{})]
    (is (seq stands))
    (is (every? #(= 64 (second %)) stands) (pr-str stands))))

(deftest a-floor-cell-out-of-reach-of-the-ground-is-aimed-at-from-the-floor-already-placed
  (let [floor (for [x (range 36 45) z (range -4 5)] [x 64 z])
        planned (set floor)
        placed (into {} (for [[x y z] floor :when (not= [40 0] [x z])] [(str x "," y "," z) "cobblestone"]))
        stands (fn [blocks] (plan/stands-for (tu/seeing-all (tu/fake-on-floor {:blocks blocks})) [40 64 0] 64 nil planned #{}))]
    (is (seq (stands placed)))
    (is (every? #(= 65 (second %)) (stands placed)) (pr-str (stands placed)))
    (is (not-any? #(= 65 (second %)) (stands {})) "no placed floor: no stand on it")))

(deftest a-pit-cell-is-aimed-at-from-the-rim-at-the-bodys-level
  (let [rim (into {} (for [x (range 37 44) z (range -3 4) y (range 63 67)
                           :when (not= [40 0] [x z])] [(str x "," y "," z) "stone"]))
        p (tu/seeing-all (tu/fake-on-floor {:blocks rim}))
        stands (plan/stands-for p [40 64 0] 67 nil #{} #{})]
    (is (seq stands))
    (is (every? #(= 67 (second %)) stands) (pr-str stands))))

(deftest walk-given-up-cells-are-retried-when-the-body-moved-or-the-job-is-finishing
  (let [m {:given-up {[1 64 1] :unreachable [2 64 2] :refused [3 64 3] :unreachable}
           :walk-given {[1 64 1] {:x 0 :y 64 :z 0} [3 64 3] {:x 0 :y 64 :z 0}}
           :retries {[3 64 3] 2}}]
    (is (= m (fp-book/retry-walks m {:x 2 :y 64 :z 0} false)) "not moved: nothing")
    (is (= {[2 64 2] :refused [3 64 3] :unreachable}
           (:given-up (fp-book/retry-walks m {:x 20 :y 64 :z 0} false))) "moved: the walk one is open, the refused stays, the twice-retried stays")
    (is (= 1 (get-in (fp-book/retry-walks m {:x 20 :y 64 :z 0} false) [:retries [1 64 1]])))
    (is (not (contains? (:walk-given (fp-book/retry-walks m {:x 20 :y 64 :z 0} false)) [1 64 1])))
    (is (= {[2 64 2] :refused [3 64 3] :unreachable}
           (:given-up (fp-book/retry-walks m {:x 0 :y 64 :z 0} true))) "finishing: retried in place")))

(deftest a-cell-whose-walk-keeps-failing-is-reopened-max-retries-times-then-stays-given-up
  (let [pos [1 64 1]
        fail-round (fn [m at] (reduce (fn [m _] (fp-book/walk-fail m pos :unreachable 2 at)) m (range 2)))
        step (fn [m] (fp-book/retry-walks (fail-round m {:x 0 :y 64 :z 0}) {:x 0 :y 64 :z 0} true))
        runs (iterate step {})]
    (is (= {pos :unreachable} (:given-up (fail-round {} {:x 0 :y 64 :z 0}))))
    (is (= {:x 5 :y 64 :z 5} (get-in (fail-round {} {:x 5 :y 64 :z 5}) [:walk-given pos])) "noted where the body stood at the first give-up")
    (is (= {} (:given-up (nth runs 1))) "reopened once")
    (is (= {} (:given-up (nth runs 2))) "reopened twice")
    (is (= {pos :unreachable} (:given-up (nth runs 3))) "the third give-up stays")
    (is (= (:walk-given (nth runs 3)) {pos {:x 0 :y 64 :z 0}}))))

(deftest walk-fail-notes-only-newly-given-up-cells
  (let [at1 {:x 0 :y 64 :z 0}
        m (fp-book/walk-fail {:given-up {[1 64 1] :unreachable}} [1 64 1] :unreachable 1 at1)]
    (is (not (contains? (:walk-given m) [1 64 1])) "already given up: no note")
    (is (= at1 (get-in (fp-book/walk-fail {} [2 64 2] :unreachable 1 at1) [:walk-given [2 64 2]])))))

;; ---------------------------------------------------------------- wood species

(def wood-ground (into {} (for [x (range 8 15) z (range -3 4)] [(h/cell-key x 63 z) "stone"])))

(defn wall-plan [planks stairs]
  {:id "wall" :parts [{:id "w" :cells [[10 64 0] [11 64 0]] :want planks}
                      {:id "s" :cells [[12 64 0]] :want {:block stairs :facing :north}}]})

(deftest generic-wood-names-are-built-in-the-species-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks wood-ground :inventory [{:name "birch_planks" :count 8} {:name "birch_stairs" :count 2}]}
                                     {"wall" (wall-plan "planks" "wooden_stairs")})
              result (await (child-outcome eng job {:plan "wall" :ways #{:carried}} 300))]
          (is (= "birch_planks" (block p [10 64 0])))
          (is (= "birch_stairs" (block p [12 64 0])))
          (is (= "birch" (:wood result)))
          (is (= [] (:missing result))))))))

(deftest a-species-name-stays-exact
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks wood-ground :inventory [{:name "birch_planks" :count 8}]}
                                     {"wall" (wall-plan "oak_planks" "oak_stairs")})
              result (await (child-outcome eng job {:plan "wall" :ways #{:carried}} 300))]
          (is (= "air" (block p [10 64 0])))
          (is (nil? (:wood result))))))))

(deftest generic-wood-with-nothing-carried-wants-the-nearest-log-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:blocks (assoc wood-ground "9,64,2" "spruce_log") :inventory []}
                                        {"wall" (wall-plan "planks" "wooden_stairs")})
              r (await (child-outcome eng job {:plan "wall" :ways #{:carried}} 100))]
          (is (= {"spruce_planks" 2 "spruce_stairs" 1} (:needs (:cause (:ending r))))))))))

(deftest a-generic-want-matches-another-species-standing-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks (assoc wood-ground "10,64,0" "oak_planks") :inventory [{:name "birch_planks" :count 8} {:name "birch_stairs" :count 2}]}
                                     {"wall" (wall-plan "planks" "wooden_stairs")})
              result (await (child-outcome eng job {:plan "wall" :ways #{:carried}} 300))]
          (is (= "oak_planks" (block p [10 64 0])) "left, not dug")
          (is (= "birch_planks" (block p [11 64 0]))))))))

(deftest an-any-want-uses-the-next-choice-carried-when-the-first-runs-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "row" :parts [{:id "fence" :cells [[2 64 2] [3 64 2] [4 64 2] [5 64 2] [6 64 2] [7 64 2]]
                                       :want [:any "oak_fence" "spruce_fence"]}]}
              {:keys [eng p seen]} (start {:inventory [{:name "oak_fence" :count 2} {:name "spruce_fence" :count 10}]} {"row" plan})
              result (await (child-outcome eng job {:plan "row"} 200))]
          (is (empty? (filter #(= :need (:slot %)) (h/events-of seen :child_started))) "nothing is fetched: a listed choice is carried")
          (is (= 6 (:placed result)))
          (is (= {} (:short result))))))))

(deftest a-box-on-an-island-the-walk-out-cannot-reach-never-seals-the-body-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "box" :parts [{:id "walls" :outline [[10 64 10] [13 65 13]] :want "dirt"}
                                      {:id "roof" :box [[10 66 10] [13 66 13]] :want "dirt"}]}
              island (into {} (concat (for [x (range 0 48) z (range 0 48)] [(h/cell-key x 60 z) "stone"])
                                      (for [x (range 9 15) z (range 9 15)] [(h/cell-key x 63 z) "stone"])))
              {:keys [eng p]} (start {:blocks island :inventory [{:name "dirt" :count 64}]
                                      :self {:pos {:x 11.5 :y 64 :z 11.5}}}
                                     {"box" plan} [])
              result (await (child-outcome eng job {:plan "box"} 400))
              open? (fn [x y z] (not= "dirt" (block p [x y z])))]
          (is (= :incomplete (:reason result)))
          (is (re-find #"unreachable" (str (:text result))) "the standing cell is given up unreachable, and the text says so")
          (is (some (fn [[x y z]] (open? x y z))
                    (concat (for [x (range 10 14) z (range 10 14)] [x 66 z])
                            (for [x (range 10 14) y [64 65] z [10 13]] [x y z])
                            (for [z (range 11 13) y [64 65] x [10 13]] [x y z]))))
          (is (not (reach-room/enclosed? p :raw))))))))

(def tall-wall-plan
  "A stone wall 3 wide and 5 high at z 6, its lowest row at y 64 over the floor."
  {:id "wall" :parts [{:id "w" :cells (vec (for [x [2 3 4] y (range 64 69)] [x y 6])) :want "stone"}]})

(defn tall-wall-start [inventory zones]
  (start {:inventory inventory :self {:pos {:x 3.5 :y 64 :z 2.5}}} {"wall" tall-wall-plan} zones))

(defn tall-wall-built [p] (every? #(= "stone" (block p %)) (for [x [2 3 4] y (range 64 69)] [x y 6])))

(defn scaffold-left [p]
  (->> (:blocks @(engine.fake/state p)) (filter (fn [[[x y z] n]] (and (>= y 64) (not= "stone" n) (not= "air" n)))) vec))

(deftest a-layer-is-finished-before-the-next-though-its-far-cells-are-out-of-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "slab" :parts [{:id "floor" :cells (vec (for [x (range 2 12)] [x 64 6])) :want "stone"}
                                       {:id "wall" :cells (vec (for [x [2 3 4]] [x 65 6])) :want "stone"}]}
              {:keys [eng p]} (start {:inventory [{:name "stone" :count 20}] :self {:pos {:x 3.5 :y 64 :z 2.5}}} {"slab" plan} [])
              result (await (child-outcome eng job {:plan "slab" :settings {:jobs.lib.build.plan/reach 3}} 600))
              ys (map (comp second first) (places p))]
          (is (= 13 (:placed result)))
          (is (= ys (sort ys)) "no cell of a higher layer is placed while a lower one is open"))))))

(deftest a-layer-waits-for-a-far-wrong-block-of-a-lower-layer-to-be-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "slab" :parts [{:id "floor" :cells (vec (for [x (range 2 12)] [x 64 6])) :want "stone"}
                                       {:id "wall" :cells (vec (for [x [2 3 4]] [x 65 6])) :want "stone"}]}
              dirt (into {} (map (fn [x] [(h/cell-key x 64 6) "dirt"])) (range 2 12))
              {:keys [eng p]} (start {:blocks dirt :inventory [{:name "stone" :count 20}] :self {:pos {:x 3.5 :y 64 :z 2.5}}} {"slab" plan} [])
              result (await (child-outcome eng job {:plan "slab" :settings {:jobs.lib.build.plan/reach 3}} 600))
              ys (map (comp second first) (places p))]
          (is (= 13 (:placed result)))
          (is (= ys (sort ys)) "no wall cell is placed while a floor cell is still dirt"))))))

(deftest a-wall-above-reach-is-built-from-a-pillar-that-is-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (tall-wall-start [{:name "stone" :count 15} {:name "dirt" :count 10}] [])
              result (await (child-outcome eng job {:plan "wall" :settings {:jobs.lib.build.plan/reach 3}} 600))]
          (is (= 15 (:placed result)))
          (is (tall-wall-built p))
          (is (seq (h/calls p "jumpPlace")) "a pillar was raised")
          (is (empty? (scaffold-left p)) "no scaffold block stands afterwards")
          (is (empty? (ledger/open-entries (mem/view (:store eng))))))))))

(def long-wall-plan
  "A stone wall 14 wide and 5 high at z 6, its lowest row at y 64 over the floor."
  {:id "wall" :parts [{:id "w" :cells (vec (for [x (range 2 16) y (range 64 69)] [x y 6])) :want "stone"}]})

(deftest a-long-wall-above-reach-is-built-along-a-bridge-row-that-is-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory [{:name "stone" :count 70} {:name "dirt" :count 40}] :self {:pos {:x 3.5 :y 64 :z 2.5}}}
                                     {"wall" long-wall-plan} [])
              result (await (child-outcome eng job {:plan "wall" :settings {:jobs.lib.build.plan/reach 3}} 900))]
          (is (= 70 (:placed result)))
          (is (every? #(= "stone" (block p %)) (for [x (range 2 16) y (range 64 69)] [x y 6])))
          (is (seq (filter (comp #{"dirt"} second) (places p))) "a bridge row was laid beside the pillar")
          (is (empty? (scaffold-left p)) "no scaffold block stands afterwards")
          (is (empty? (ledger/open-entries (mem/view (:store eng))))))))))

(deftest a-wall-on-an-island-is-built-along-a-bridge-row-from-the-bank-and-the-row-is-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[2 64 10] [3 64 10] [4 64 10]]
              moat (into {} (for [x (range -3 10) z (range 6 16)] [(h/cell-key x 63 z) "air"]))
              island (into {} (for [x [2 3 4] y [63 65]] [(h/cell-key x y 10) "stone"]))
              plan {:id "wall" :parts [{:id "w" :cells cells :want "stone"}]}
              {:keys [eng p]} (start {:blocks (merge moat island) :inventory [{:name "stone" :count 6} {:name "dirt" :count 10}]
                                      :self {:pos {:x 3.5 :y 64 :z 2.5}}}
                                     {"wall" plan} [])
              result (await (child-outcome eng job {:plan "wall" :settings {:jobs.lib.build.plan/reach 3}} 600))]
          (is (= 3 (:placed result)))
          (is (every? #(= "stone" (block p %)) cells))
          (is (seq (filter (comp #{"dirt"} second) (places p))) "a bridge row was laid from the bank")
          (is (empty? (h/calls p "jumpPlace")) "no pillar was raised")
          (is (not-any? #(= "dirt" (block p [3 63 %])) (range 6 10)) "the row is taken back")
          (is (empty? (ledger/open-entries (mem/view (:store eng))))))))))

(deftest a-zone-over-every-pillar-base-refuses-the-cells-above-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "keep-out" :min [-30 60 -10] :max [40 80 10]}]
              {:keys [eng p]} (tall-wall-start [{:name "stone" :count 15} {:name "dirt" :count 10}] zones)
              result (await (child-outcome eng job {:plan "wall" :settings {:jobs.lib.build.plan/reach 3}} 600))]
          (is (= :refused (:reason result)))
          (is (= #{[:zone :scaffold]} (set (map (juxt :reason :why) (:refused result)))))
          (is (empty? (h/calls p "jumpPlace")))
          (is (not (tall-wall-built p))))))))

(deftest nothing-carried-for-a-pillar-waits-for-a-scaffold-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (tall-wall-start [{:name "stone" :count 15}] [])
              result (await (child-outcome eng job {:plan "wall" :ways #{:carried} :settings {:jobs.lib.build.plan/reach 3}} 600))]
          (is (= :needs (:reason result)))
          (is (empty? (h/calls p "jumpPlace"))))))))

(deftest roof-cells-over-the-way-out-wait-for-the-rest-of-the-interior-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "box" :parts [{:id "walls" :outline [[1 64 1] [4 65 4]] :want "dirt"}
                                      {:id "roof" :box [[1 66 1] [4 66 4]] :want "dirt"}]}
              open #{[3 65 1] [3 66 1] [3 66 2] [2 66 2] [2 66 3] [3 66 3]}
              built (into {} (for [x (range 1 5) z (range 1 5) y [64 65 66]
                                   :when (and (or (= y 66) (#{1 4} x) (#{1 4} z)) (not (open [x y z])))]
                               [(h/cell-key x y z) "dirt"]))
              {:keys [eng p]} (start {:blocks (merge wide-ground built) :inventory [{:name "dirt" :count 64}]
                                      :self {:pos {:x 3.5 :y 64 :z 2.5}}}
                                     {"box" plan} [])
              result (await (child-outcome eng job {:plan "box"} 400))
              order (mapv first (places p))]
          (is (= 6 (:placed result)) (pr-str order (:given-up result)))
          (is (not (reach-room/enclosed? p :raw))))))))

(deftest a-dark-site-is-lit-with-carried-torches-before-it-is-built
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (update (pen-plan) :parts #(vec (butlast %)))
              {:keys [eng p]} (start {:inventory (conj (vec (butlast kit)) {:name "torch" :count 4})
                                      :time 18000 :light {} :light-default [0 0]}
                                     {"pen" plan})
              result (await (child-outcome eng job {:plan "pen"} 400))
              torches (for [x (range 0 7) y [64 65] z (range 0 7) :when (= "torch" (block p [x y z]))] [x y z])]
          (is (seq torches) "a torch hung in the dark site")
          (is (every? #(= "oak_fence" (block p %)) ring))
          (is (= [] (:missing result)) (pr-str result)))))))

(deftest a-body-standing-in-light-beside-a-dark-site-still-lights-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (update (pen-plan) :parts #(vec (butlast %)))
              {:keys [eng p]} (start {:inventory (conj (vec (butlast kit)) {:name "torch" :count 4})
                                      :self {:pos {:x 8.5 :y 64 :z 8.5}}
                                      :time 18000 :light {[8 64 8] [15 0] [8 65 8] [15 0]} :light-default [0 0]}
                                     {"pen" plan})
              result (await (child-outcome eng job {:plan "pen"} 400))
              torches (for [x (range 0 7) y [64 65] z (range 0 7) :when (= "torch" (block p [x y z]))] [x y z])]
          (is (seq torches) "a torch hung at the dark site")
          (is (= [] (:missing result)) (pr-str result)))))))

(deftest a-pocket-is-judged-over-the-world-the-last-walk-planned-toward-its-goal
  (let [p (tu/fake {:blocks (tu/box -8 50 -8 8 60 8 "stone") :floor [-30 -30 30 30] :self {:pos [0 55 0]}})
        c {:primitives p :view (constantly {:data {} :now 0})}
        goals (atom [])]
    (with-redefs [near/last-goal (constantly [6 55 0])
                  escape/walk-spec (fn [_ goal] (swap! goals conj goal) nil)]
      (fp-walk/in-pocket? c))
    (is (= [[6 55 0]] @goals))))

(defn dark-wait [s] (contract/later :exposed "dark at 1 65 1 until light" :lowered-by #{:light} :chance 0.005 :recheck-after-ms (* 1000 s) :wait-ms (* 1000 s)))
(def no-light (contract/never :exposed "dark at 1 65 1, a hall of dark floor" :lowered-by #{:torch} :chance 0.005))

(defn lit-check-status
  "The start status lights-itself makes of status for a body with torches? and a fetch allowed when fetch?, whose torch obtain for
  the 2 torches owed starts with `obtain` (its worth booked in worths)."
  [status torches? fetch? obtain mem worths]
  (with-redefs [torch/torches-held (constantly (if torches? 4 0)) need/fetch-opts (constantly (when fetch? {:how #{:gather}})) ctx/mem (constantly mem)
                fp-light/torches-owed (constantly 2)
                need/item-status (fn [_ item n _ worth] (swap! worths conj [item n worth]) obtain)]
    ((fp-light/lights-itself (constantly status) (constantly [])) {:primitives nil})))

(defn lit-check [& args] (first (apply lit-check-status args)))

(def not-worth (contract/later :not-worth "obtain: costs 400 s, worth 297 s"))

(deftest a-torchless-build-fetches-torches-when-the-obtain-takes-what-the-wait-saves
  (with-redefs [sense/sky-in (constantly nil)]
   (are [status torches? fetch? obtain mem kind] (= kind (lit-check status torches? fetch? obtain mem (atom [])))
    (dark-wait 3000) false true (contract/ok :ready "ready") {} :ok
    (dark-wait 3000) true false nil {} :ok
    (dark-wait 3000) false false nil {} :later
    (dark-wait 3000) false true (contract/ok :ready "ready") {:torches-tried true} :later
    (dark-wait 300) false true not-worth {} :later
    no-light false true (contract/ok :ready "ready") {} :ok
    no-light false true not-worth {} :never
    no-light false false nil {} :never)
   (let [worths (atom [])]
     (lit-check (dark-wait 300) false true not-worth {} worths)
     (lit-check no-light false true not-worth {} worths)
     (is (= [["torch" 2 (cost/light-wait-worth 300 2)] ["torch" 2 nil]] @worths) "the wait less the placing; no night left to wait out, any price"))))

(deftest a-never-lit-cell-at-night-fetches-torches-only-for-what-waiting-for-dawn-would-cost
  (let [worths (atom [])
        check (fn [night-s obtain] (with-redefs [sense/sky-in (constantly night-s) obtain-price/item-price (constantly 100)] (lit-check no-light false true obtain {} worths)))]
    (is (= [:later :ok :never] [(check 300 not-worth) (check 300 (contract/ok :ready "ready")) (check 0 not-worth)]))
    (is (= 300000 (:wait-ms (nth (with-redefs [sense/sky-in (constantly 300) obtain-price/item-price (constantly 100)] (lit-check-status no-light false true not-worth {} (atom []))) 2))) "deferred to dawn, not failed")
    (is (= [["torch" 2 400] ["torch" 2 400] ["torch" 2 nil]] @worths)
        "at night the night left and the torches' price by day, by day any price")))

(defn ^:async never-lit-status
  "The start status lights-itself gives a never-lit build cell for a torchless body whose ways fetch from chests, 2 torches
  owed, a chest 40 blocks east booked as holding torches, at time of day t, the ground's light [sky block] light."
  [t light]
  (let [s (f/start (-> (f/bare []) (assoc :time t :light {} :light-default light) (update :blocks assoc "40,64,0" "chest")) [(assoc f/own-zone :max [50 70 6])])
        out (atom nil)
        parent {:round (fn ^:async lit-round [c]
                         (reset! out (with-redefs [fp-light/torches-owed (constantly 2) need/fetch-opts (constantly {:how #{:chest}})]
                                       ((fp-light/lights-itself (constantly no-light) (constantly [])) c)))
                         (ctx/end c :done nil))}
        eng (assoc-in (:eng s) [:jobs 'lit-parent] parent)]
    (mem/write! (:store eng) stock/stock-kind {:pos [40 64 0] :items {"torch" 8}} stock/stock-policy)
    (core/submit! eng '(lit-parent) {})
    (await (tu/run-until-empty eng 5))
    (first @out)))

(deftest a-never-lit-cell-near-dawn-waits-when-the-dark-walk-to-the-torches-costs-more-than-the-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :later (await (never-lit-status 22800 [15 0]))) "near dawn, the spawns on 80 blocks of dark ground cost more than the night left")
        (is (= :ok (await (never-lit-status 22800 [15 15]))) "the same walk over torch-lit ground risks nothing more by night: fetched now")
        (is (= :ok (await (never-lit-status 18000 [15 0]))) "at midnight the night left costs more than the dark walk: fetched now")))))

(deftest a-torchless-build-plans-its-torches-only-when-it-could-fetch-them
  (let [planned (atom 0)
        check (fn [fetch? mem]
                (with-redefs [torch/torches-held (constantly 0) need/fetch-opts (constantly (when fetch? {:how #{:gather}})) ctx/mem (constantly mem)
                              fp-light/torches-owed (fn [_ _] (swap! planned inc) 2)
                              need/item-status (constantly (contract/ok :ready "ready"))]
                  (first ((fp-light/lights-itself (constantly (dark-wait 3000)) (constantly [])) {:primitives nil}))))]
    (is (= [:later :later] [(check false {}) (check true {:torches-tried true})]))
    (is (zero? @planned) "no fetch allowed or one tried already: the lighting is not planned")
    (is (= :ok (check true {})))
    (is (= 1 @planned))))

(defn wall-slot-hut
  "Teasel j134's hut (a 5x5 box, walls y 64..66, roof y 67, a door gap south) built but for the body's own cells (a slot in
  the top course of the north wall, head in the roof) and the roof cell south of its head; leaves outside the slot."
  [kit]
  (let [cells (for [x (range 5) y (range 63 68) z (range 5)
                    :when (or (#{63 67} y) (#{0 4} x) (#{0 4} z))
                    :when (not (#{[2 64 4] [2 65 4]} [x y z]))]
                [x y z])
        open #{[2 66 0] [2 67 0] [2 67 1]}
        built (into {} (for [c cells :when (not (open c))] [(apply h/cell-key c) "oak_planks"]))
        leaves (into {} (for [y [64 65 66 67] x [1 2 3]] [(h/cell-key x y -1) "jungle_leaves"]))]
    (start {:blocks (merge leaves built) :floor [-30 -30 30 30] :self {:pos {:x 2.5 :y 66 :z 0.5}} :inventory kit}
           {"hut" {:id "hut" :parts [{:id "house" :cells (vec cells) :want "oak_planks"}
                                     {:id "door" :cells [[2 64 4] [2 65 4]] :want "air"}]}})))

(deftest a-body-in-a-wall-slot-of-its-own-hut-is-never-sealed-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (wall-slot-hut [{:name "oak_planks" :count 8}])
              result (await (child-outcome eng job {:plan "hut"} 300))]
          (is (not= :shut-in (:reason result)) (pr-str result))
          (is (not (reach-room/enclosed? p :raw)) (str "body at " (js->clj (.-pos (.self p)))))
          (is (every? #(= "oak_planks" (block p %)) [[2 66 0] [2 67 0] [2 67 1]]) (pr-str (places p))))))))

(deftest a-body-shut-in-its-own-closed-room-gets-out-and-leaves-the-room-whole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (for [x (range 3 8) y (range 64 67) z (range 3 8)
                         :when (or (= y 66) (#{3 7} x) (#{3 7} z))]
                     [x y z])
              plan {:id "own" :metadata {:by "Fake"} :parts [{:id "room" :cells (vec room) :want "dirt"}
                                      {:id "post" :cells [[12 64 5]] :want "oak_fence"}]}
              {:keys [eng p]} (start {:blocks (merge (tu/floor 3 3 7 7) (into {} (for [c room] [(apply h/cell-key c) "dirt"])))
                                      :floor [-30 -30 30 30] :self {:pos {:x 5.5 :y 64 :z 5.5}}
                                      :inventory [{:name "oak_fence" :count 1} {:name "dirt" :count 8} {:name "stone_shovel" :count 1}]}
                                     {"own" plan})
              result (await (child-outcome eng job {:plan "own"} 400))]
          (is (= "oak_fence" (block p [12 64 5])))
          (is (every? #(= "dirt" (block p %)) room) (pr-str (remove #(= "dirt" (block p %)) room)))
          (is (empty? (:missing result)) (pr-str result)))))))

(deftest a-cell-tunnelled-to-from-a-closed-room-of-the-plan-leaves-the-room-wall-refilled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shell (for [x (range 3 8) y (range 63 68) z (range 3 8)
                          :when (or (#{63 67} y) (#{3 7} x) (#{3 7} z))]
                      [x y z])
              inside (set (for [x (range 4 7) y (range 64 67) z (range 4 7)] [x y z]))
              rock (into {} (for [x (range -2 15) y (range 62 70) z (range -2 13)
                                  :when (not (inside [x y z])) :when (not= [x y z] [11 64 5])]
                              [(h/cell-key x y z) "stone"]))
              plan {:id "own" :metadata {:by "Fake"} :parts [{:id "room" :cells (vec shell) :want "dirt"}
                                                            {:id "post" :cells [[11 64 5]] :want "dirt"}]}
              {:keys [eng p]} (start {:blocks (merge rock (into {} (for [c shell] [(apply h/cell-key c) "dirt"])))
                                      :floor [-30 -30 30 30] :self {:pos {:x 5.5 :y 64 :z 5.5}}
                                      :inventory [{:name "dirt" :count 16} {:name "stone_pickaxe" :count 1} {:name "stone_shovel" :count 1}]}
                                     {"own" plan})
              result (await (child-outcome eng job {:plan "own"} 600))]
          (is (= "dirt" (block p [11 64 5])))
          (is (every? #(= "dirt" (block p %)) shell) (pr-str (remove #(= "dirt" (block p %)) shell)))
          (is (empty? (:missing result)) (pr-str result)))))))
