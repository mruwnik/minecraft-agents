(ns engine.strip-corridor-test
  "jobs.strip.corridor: one mineshaft corridor segment stripped, in the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [jobs.lib.strip :as strip]
            [jobs.strip.corridor :as corridor]))

(def job 'jobs.strip.corridor)
(def seg {:y 64 :axis :x :line 0 :from 0 :to 12 :open-ends #{}})
(def args {:segment (assoc seg :id (strip/seg-id seg)) :entry [-3 64 0]})
(def kit [{:name "iron_pickaxe" :count 1} {:name "iron_axe" :count 1} {:name "shears" :count 1}])
(def drops {"rail" "rail" "cobweb" "string" "oak_fence" "oak_fence" "oak_planks" "oak_planks" "torch" "torch" "wall_torch" "torch"})
(def bridge [8 63 0])
(def web [4 65 0])
(def wood-cells
  (concat (for [x [2 6 10] z [-1 1] y [64 65]] [x y z])
          (for [x [2 6 10] z [-1 0 1]] [x 66 z])))
(def rail-cells (for [x (range 0 13)] [x 64 0]))

(def corridor
  "A corridor along x in rock, 3 wide and 3 high, supports at x 2 6 10, rails on the centre line, a cobweb, and one floor plank
  with air under it (a bridge)."
  (let [air (set (for [x (range -5 16) y [64 65 66] z [-1 0 1]] (str x "," y "," z)))
        key (fn [[x y z]] (str x "," y "," z))]
    (merge (apply dissoc (tu/box -6 63 -2 16 67 2 "stone") air)
           (into {} (map (fn [c] [(key c) (if (= 66 (second c)) "oak_planks" "oak_fence")])) wood-cells)
           (into {} (map (fn [c] [(key c) "rail"])) rail-cells)
           {(key web) "cobweb" (key bridge) "oak_planks"})))

(defn world [& {:keys [inventory entities zones blocks] :or {inventory kit blocks corridor}}]
  (bd/setup {:self {:pos {:x -3 :y 64 :z 0}} :blocks blocks :inventory inventory :drops drops :entities (or entities [])
             :zones (or zones [])}))

(defn dark-world
  "A seeing world whose floor level in the corridor is dark (light 0) after the first sight pass; the rest stays lit so the corridor is seen."
  ([inventory] (dark-world inventory corridor (range -5 16)))
  ([inventory blocks xs]
   (let [env (bd/setup-seeing {:self {:pos {:x -3 :y 64 :z 0}} :blocks blocks :drops drops :inventory inventory})]
     (swap! (.. (:p env) -world -state) merge {:light (into {} (for [x xs y [64 65] z [-1 0 1]] [[x y z] [0 0]]))})
     env)))

(def spawner-cell [10 65 2])
(def spawner-corridor
  "The corridor with nothing to take but a spawner in its wall."
  (-> (apply dissoc corridor (map (fn [[x y z]] (str x "," y "," z)) (concat wood-cells rail-cells [web bridge])))
      (assoc "8,63,0" "stone" "10,65,2" "spawner")))
(def spider {:id 9 :name "cave_spider" :kind "hostile" :pos {:x 3 :y 64 :z 0} :health 12})
(def pick-torches (into kit [{:name "torch" :count 8}]))

(defn run! [{:keys [eng]} a] (tu/child-outcome eng job a 400))
(defn cause "The cause of a stopped run r: what its can-run? said." [r] (:cause (:ending r)))
(defn gone? [p cells] (every? #(#{"air" "cave_air"} (bd/block-at p %)) cells))
(defn cart [id pos] {:id id :name "chest_minecart" :kind "other" :pos pos :health 5
                     :items [{:name "iron_ingot" :count 3}] :drops [{:name "chest_minecart" :count 1}]})

(deftest supports-rails-and-web-go-the-bridge-plank-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              p (:p env)
              r (await (run! env args))]
          (is (= {:status :done :taken {:wood 21 :rail 13 :cobweb 1}} (select-keys r [:status :taken])))
          (is (gone? p (concat wood-cells rail-cells [web])))
          (is (= "oak_planks" (bd/block-at p bridge)) "the plank that floors a gap stays")
          (is (= [{:pos {:x 8 :y 63 :z 0} :why :bridge}] (:kept r))))))))

(deftest torches-in-the-segment-are-taken-back-after-the-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self {:pos {:x -3 :y 64 :z 0}} :blocks (assoc corridor "3,64,1" "torch" "9,65,-1" "wall_torch")
                             :inventory kit :drops drops})
              r (await (run! env args))]
          (is (= {:status :done :taken {:wood 21 :rail 13 :cobweb 1 :torch 2}} (select-keys r [:status :taken])))
          (is (= 2 (bd/carried (:p env) "torch"))))))))

(deftest a-zombie-in-the-corridor-holds-the-job-back-with-nothing-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0} :health 20}])
              why (await (bd/waiting-after env (list job args) 3))]
          (is (= :hostile-near (:reason why)))
          (is (empty? (bd/calls (:p env) "dig"))))))))

(deftest a-long-corridor-is-one-round-not-cut-at-a-step-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (await (bd/ended-in-one-tick? (world) (list job args)))))))))

(deftest a-bag-at-want-free-stops-bag-full-after-the-first-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "filler_" i) :count 1}) (range 21))
              env (world :inventory (into kit filler))
              r (await (run! env args))]
          (is (= [:stopped :bag-full {:rail 1}] [(:status r) (:reason r) (:taken (cause r))]))
          (is (= 1 (count (bd/calls (:p env) "dig")))))))))

(deftest a-chest-minecart-in-the-segment-is-looted-by-the-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :entities [(cart 50 [5 64 1])])
              p (:p env)
              r (await (run! env args))]
          (is (= :done (:status r)))
          (is (empty? (filter #(= "chest_minecart" (:name %)) (fake/entities p))))
          (is (= 3 (bd/carried p "iron_ingot")))
          (is (= 1 (bd/carried p "chest_minecart"))))))))

(deftest a-cart-that-cannot-be-emptied-into-a-full-bag-stops-bag-full-for-the-trip
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :entities [(assoc (cart 50 [5 64 1]) :drops [] :items [])])
              p (:p env)
              _ (.override (.-world p) "inspectContainer" (fn [_ _ _] #js {:status "ok" :items #js [#js {:name "iron_ingot" :count 3}]}))
              _ (.override (.-world p) "transfer" (fn [_ _ _] #js {:status "full" :moved 0}))
              r (await (run! env (assoc args :want-free 0)))]
          (is (= [:stopped :bag-full] ((juxt :status :reason) r))))))))

(deftest without-shears-the-web-stops-no-tool-naming-the-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :inventory (vec (take 2 kit)))
              r (await (run! env args))]
          (is (= {:reason :no-tool :tool "shears"} (select-keys (cause r) [:reason :tool])))
          (is (= "cobweb" (bd/block-at (:p env) web))))))))

(deftest a-refused-dig-ends-the-trip-with-the-childs-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :zones [{:name "farm" :min [0 60 -3] :max [12 70 3] :owner "Miles"}])
              r (await (run! env args))]
          (is (= {:status :stopped :reason :refused} (select-keys r [:status :reason])))
          (is (empty? (bd/calls (:p env) "dig"))))))))

(deftest a-dark-corridor-gets-a-torch-before-the-first-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world (into kit [{:name "torch" :count 8}]))
              r (await (run! env args))]
          (is (= {:status :done} (select-keys r [:status :reason :child :cell])))
          (is (pos? (count (filter #(= "torch" (.-item (.-args %))) (bd/calls p "place")))))
          (is (= 8 (bd/carried p "torch")) "the torches it hung are taken back at the end"))))))

(deftest no-torch-in-the-dark-stops-no-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world kit)
              r (await (run! env args))]
          (is (= {:status :stopped :reason :no-torches} (select-keys r [:status :reason])))
          (is (empty? (bd/calls p "dig"))))))))

(deftest the-segment-may-be-given-as-its-id
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              r (await (run! env (assoc args :segment (:id (:segment args)))))]
          (is (= {:status :done :taken {:wood 21 :rail 13 :cobweb 1}} (select-keys r [:status :taken]))))))))

(deftest no-segment-stops-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! (world) {:segment "nonsense"}))]
          (is (= {:status :stopped :reason :bad-args} (select-keys r [:status :reason]))))))))

(deftest spawner-break-with-a-cave-spider-near-waits-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :entities [spider] :blocks spawner-corridor)
              why (await (bd/waiting-after env (list job (assoc args :spawner :break)) 3))]
          (is (= :hostile-near (:reason why)))
          (is (empty? (bd/calls (:p env) "dig")))
          (is (= "spawner" (bd/block-at (:p env) spawner-cell))))))))

(deftest spawner-break-lights-the-cells-round-it-then-digs-it-counting-it-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world pick-torches spawner-corridor (range 6 13))
              r (await (run! env (assoc args :spawner :break)))
              torches (map #(js->clj (.-pos (.-args %)) :keywordize-keys true) (filter #(= "torch" (.-item (.-args %))) (bd/calls p "place")))]
          (is (= {:status :done :taken {:spawner 1 :torch (count torches)}} (select-keys r [:status :taken])) "the torches hung are taken back")
          (is (gone? p [spawner-cell]))
          (is (seq torches))
          (is (every? (fn [{:keys [x y z]}] (and (<= (js/Math.abs (- x 10)) 4) (<= (js/Math.abs (- y 65)) 1) (<= (js/Math.abs (- z 2)) 4))) torches)
              "every torch within the spawner's spawn box"))))))

(deftest spawner-break-in-the-dark-without-torches-stops-no-torches-leaving-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world kit spawner-corridor (range 6 13))
              r (await (run! env (assoc args :spawner :break)))]
          (is (= {:status :stopped :reason :no-torches} (select-keys r [:status :reason])))
          (is (= "spawner" (bd/block-at p spawner-cell))))))))

(deftest spawner-seal-stops-at-once-with-the-spawner-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :blocks (assoc corridor "10,65,2" "spawner"))
              r (await (run! env (assoc args :spawner :seal)))]
          (is (= [:nothing-possible :spawner [10 65 2]] [(:kind (:ending r)) (:reason r) (:cell (:data (:ending r)))]))
          (is (empty? (bd/calls (:p env) "dig")))
          (is (= "spawner" (bd/block-at (:p env) spawner-cell))))))))

(defn torch-poses [p]
  (map #(js->clj (.-pos (.-args %)) :keywordize-keys true) (filter #(= "torch" (.-item (.-args %))) (bd/calls p "place"))))

(deftest spawner-light-is-the-default-it-lights-the-box-and-leaves-spawner-and-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world pick-torches spawner-corridor (range 6 13))
              r (await (run! env args))
              torches (torch-poses p)]
          (is (= {:status :done :taken {}} (select-keys r [:status :taken])) "the torches stay")
          (is (= [{:pos {:x 10 :y 65 :z 2} :why :spawner :lit true}] (:kept r)))
          (is (= "spawner" (bd/block-at p spawner-cell)))
          (is (seq torches))
          (is (every? (fn [{:keys [x y z]}] (and (<= (js/Math.abs (- x 10)) 4) (<= (js/Math.abs (- y 65)) 1) (<= (js/Math.abs (- z 2)) 4))) torches))
          (is (every? #(= "torch" (bd/block-at p [(:x %) (:y %) (:z %)])) torches) "none taken back"))))))

(deftest spawner-light-reaches-the-far-corner-of-the-spawn-box
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world pick-torches spawner-corridor (range 6 13))
              _ (await (run! env args))]
          (is (some #(<= (js/Math.abs (- (:x %) 6)) 3) (torch-poses p)) "x 6 is 4 along, 2 across and 1 below the spawner: a torch stands within torch-gap of it"))))))

(deftest spawner-light-without-torches-in-the-dark-stops-no-torches-leaving-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as env} (dark-world kit spawner-corridor (range 6 13))
              r (await (run! env args))]
          (is (= {:status :stopped :reason :no-torches} (select-keys r [:status :reason])))
          (is (= "spawner" (bd/block-at p spawner-cell))))))))

(deftest spawner-skip-strips-the-rest-and-lists-it-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :blocks (assoc corridor "10,65,2" "spawner"))
              r (await (run! env (assoc args :spawner :skip)))]
          (is (= {:status :done :taken {:wood 21 :rail 13 :cobweb 1}} (select-keys r [:status :taken])))
          (is (some #{{:pos {:x 10 :y 65 :z 2} :why :spawner}} (:kept r)))
          (is (= "spawner" (bd/block-at (:p env) spawner-cell))))))))

(deftest spawner-skip-lists-a-spawner-seen-at-the-start-though-the-far-end-is-out-of-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [air (set (for [x (range -5 56) y [64 65 66] z [-1 0 1]] (str x "," y "," z)))
              blocks (-> (apply dissoc (tu/box -6 63 -2 56 67 2 "stone") air) (assoc "10,65,2" "spawner"))
              long-seg (assoc seg :to 50 :id (strip/seg-id (assoc seg :to 50)))
              env (bd/setup-seeing {:self {:pos {:x -3 :y 64 :z 0}} :blocks blocks :drops drops :inventory kit})
              r (await (run! env (assoc args :segment long-seg :spawner :skip)))]
          (is (= :done (:status r)))
          (is (some #{{:pos {:x 10 :y 65 :z 2} :why :spawner}} (:kept r))))))))

(deftest spawner-skip-lists-a-spawner-in-a-dark-side-wall-seen-only-from-the-side
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [air (set (for [x (range -5 56) y [64 65 66] z [-1 0 1]] (str x "," y "," z)))
              blocks (-> (apply dissoc (tu/box -6 63 -2 56 67 2 "stone") air) (assoc "30,65,2" "spawner"))
              long-seg (assoc seg :to 50 :id (strip/seg-id (assoc seg :to 50)))
              env (bd/setup-seeing {:self {:pos {:x -3 :y 64 :z 0}} :blocks blocks :drops drops :inventory kit})
              _ (swap! (.. (:p env) -world -state) merge {:light (into {} (for [x (range -5 56) y [64 65 66] z [-1 0 1 2]] [[x y z] [0 0]]))})
              r (await (run! env (assoc args :segment long-seg :spawner :skip)))]
          (is (= :done (:status r)))
          (is (some #{{:pos {:x 30 :y 65 :z 2} :why :spawner}} (:kept r))
              "in the dark a block is seen within 4 cells and inside the view cone: a stop must look to the walls"))))))

(deftest a-stopped-trip-ends-with-the-kind-of-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "filler_" i) :count 1}) (range 21))
              ends (fn ^:async ends [env a] (let [e (:ending (await (run! env a)))] [(:kind e) (if (map? (:cause e)) (:reason (:cause e)) (:cause e))]))]
          (is (= [:impossible :bad-args] (await (ends (world) {:segment "nonsense"})))
              "a segment from the args that cannot be read is never possible")
          (is (= [:condition-failed :bag-full] (await (ends (world :inventory (into kit filler)) args))))
          (is (= [:condition-failed :no-tool] (await (ends (world :inventory (vec (take 2 kit))) args))))
          (is (= [:condition-failed :refused]
                 (await (ends (world :zones [{:name "farm" :min [0 60 -3] :max [12 70 3] :owner "Miles"}]) args)))))))))

(deftest the-walk-stops-at-the-farthest-cell-whose-floor-is-seen
  (is (= [3 3 -9 7 -3]
         [(corridor/walk-stop #{1 2 3} 0 20)
          (corridor/walk-stop #{1 2 3 11} 0 20)
          (corridor/walk-stop #{-1 -9} -3 -20)
          (corridor/walk-stop #{} 0 7)
          (corridor/walk-stop #{} 0 -3)])))

(deftest a-hop-stop-is-walked-to-exactly-the-end-within-one
  (is (= [0 1] [(corridor/walk-range 5 12) (corridor/walk-range 12 12)])))
