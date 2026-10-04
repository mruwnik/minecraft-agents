(ns plan.rail-test
  (:require [clojure.test :refer [deftest are is]]
            [plan.rail :as rail]
            [plan.shape :as shape]))

;; ---------------------------------------------------------------- the chain

(def ew {:block "rail" :shape :east_west})

(defn rails [& ps] (mapv (fn [p] {:pos p :want ew :part "r"}) ps))

(deftest rail-cells-in-any-order-make-one-chain-from-an-end
  (are [cells chain] (= chain (mapv :pos (rail/line cells)))
    (rails [3 64 0] [1 64 0] [4 64 0] [2 64 0]) [[1 64 0] [2 64 0] [3 64 0] [4 64 0]]
    (rails [5 64 2] [5 64 0] [5 64 1]) [[5 64 0] [5 64 1] [5 64 2]]
    (rails [0 64 0] [1 64 0] [1 64 1] [1 65 2]) [[0 64 0] [1 64 0] [1 64 1] [1 65 2]]
    (rails [7 64 7]) [[7 64 7]]
    (conj (rails [2 64 0] [1 64 0]) {:pos [1 63 0] :want "stone" :part "bed"}) [[1 64 0] [2 64 0]]))

(deftest cells-that-are-not-one-chain-say-why
  (are [cells error] (= error (select-keys (rail/line cells) [:error :at]))
    (rails [1 64 0] [2 64 0] [4 64 0] [5 64 0]) {:error :gap :at [3 64 0]}
    (rails [1 64 0] [2 64 0] [3 64 0] [2 64 1]) {:error :branch :at [2 64 0]}
    (rails [1 64 0] [2 64 0] [1 64 5] [2 64 5]) {:error :two-chains}
    (rails [0 64 0] [1 64 0] [1 64 1] [0 64 1]) {:error :loop}
    (rails [1 64 0] [2 66 0]) {:error :two-chains}
    [{:pos [0 63 0] :want "stone" :part "bed"}] {:error :no-rails}))

;; ---------------------------------------------------------------- the proof

(def bed [:any "cobblestone" "stone" "dirt"])

(def plan-cells
  "Six rails along x at y 64 (x 1..6): normal 1 2, lit powered 3 4, normal 5 6; buffers at x 0 and 7; a bed under
  every rail with a redstone block under 3; the cell above every rail clear."
  (vec (concat (for [x (range 1 7)] {:pos [x 63 0] :want bed :part "bed"})
               [{:pos [3 63 0] :want "redstone_block" :part "power"}
                {:pos [0 64 0] :want bed :part "buffers"} {:pos [7 64 0] :want bed :part "buffers"}]
               (for [x [1 2 5 6]] {:pos [x 64 0] :want ew :part "rails"})
               (for [x [3 4]] {:pos [x 64 0] :want {:block "powered_rail" :shape :east_west :powered true} :part "powered"})
               (for [x (range 1 7)] {:pos [x 65 0] :want :clear :part "head"}))))

(defn ew-rail [name & [state]] {:name name :state (merge {:shape "east_west" :waterlogged "false"} state)})

(def sound-world
  (merge (into {} (for [x (range 1 7)] [[x 63 0] {:name "stone"}]))
         {[3 63 0] {:name "redstone_block"} [0 64 0] {:name "stone"} [7 64 0] {:name "stone"}}
         (into {} (for [x [1 2 5 6]] [[x 64 0] (ew-rail "rail")]))
         (into {} (for [x [3 4]] [[x 64 0] (ew-rail "powered_rail" {:powered "true"})]))))

(defn block-at [world] (fn [pos] (get world pos {:name "air"})))

(defn judged [world & [cells]] (rail/judge-line (or cells plan-cells) (block-at world)))

(def brake-cells
  (mapv #(if (= [4 64 0] (:pos %)) (assoc-in % [:want :powered] false) %) plan-cells))

(deftest a-sound-line-passes
  (is (= {:ok? true :breaks [] :hazards []} (judged sound-world))))

(deftest each-break-is-named-at-its-cell
  (are [world cells breaks] (= {:ok? false :breaks breaks} (select-keys (judged world cells) [:ok? :breaks]))
    (dissoc sound-world [2 64 0]) plan-cells [{:pos [2 64 0] :why :gap}]
    (assoc sound-world [2 64 0] {:name "stone"}) plan-cells [{:pos [2 64 0] :why :gap}]
    (assoc sound-world [5 64 0] (ew-rail "rail" {:shape "north_south"})) plan-cells [{:pos [5 64 0] :why :shape}]
    (assoc sound-world [4 64 0] (ew-rail "powered_rail" {:powered "false"})) plan-cells [{:pos [4 64 0] :why :unlit}]
    sound-world brake-cells [{:pos [4 64 0] :why :lit-brake}]
    (dissoc sound-world [2 63 0]) plan-cells [{:pos [2 64 0] :why :no-bed}]
    (assoc sound-world [2 63 0] {:name "oak_slab" :state {:type "bottom"}}) plan-cells [{:pos [2 64 0] :why :no-bed}]
    (assoc sound-world [2 63 0] {:name "oak_leaves"}) plan-cells [{:pos [2 64 0] :why :no-bed}]
    (assoc sound-world [2 65 0] {:name "stone"}) plan-cells [{:pos [2 64 0] :why :blocked}]
    (assoc sound-world [2 64 0] (ew-rail "rail" {:waterlogged "true"})) plan-cells [{:pos [2 64 0] :why :wet}]
    (assoc sound-world [2 65 0] {:name "water"}) plan-cells [{:pos [2 64 0] :why :wet}]
    (dissoc sound-world [7 64 0]) plan-cells [{:pos [6 64 0] :why :no-buffer}]
    (assoc sound-world [6 64 0] (ew-rail "powered_rail" {:powered "true"})) plan-cells [{:pos [6 64 0] :why :launch-trap}]
    (assoc sound-world [2 64 0] nil) plan-cells [{:pos [2 64 0] :why :unloaded}]))

(deftest a-bed-of-any-sturdy-block-is-no-fault-and-a-falling-one-is-a-hazard
  (are [bed-block hazards] (= {:ok? true :breaks [] :hazards hazards} (judged (assoc sound-world [2 63 0] bed-block)))
    {:name "grass_block"} []
    {:name "oak_planks"} []
    {:name "oak_slab" :state {:type "top"}} []
    {:name "gravel"} [{:pos [2 63 0] :why :falling-bed}]
    {:name "sand"} [{:pos [2 63 0] :why :falling-bed}]))

(deftest ground-is-under-the-rails-and-the-buffers-and-the-buffers-but-never-a-source
  (is (= (disj (set (concat (for [x (range 1 7)] [x 63 0]) [[0 64 0] [7 64 0] [0 63 0] [7 63 0]])) [3 63 0])
         (rail/ground plan-cells)))
  (is (contains? (rail/ground (:cells (shape/expand {:id "p" :status :active
                                                     :parts (:parts (rail/layout [0 64 0] [29 64 0] {}))} {})))
                 [3 63 -1])
      "under a torch"))

(deftest a-plan-that-is-no-line-is-not-judged
  (is (= {:ok? false :error :no-rails} (select-keys (judged sound-world [{:pos [0 63 0] :want "stone"}]) [:ok? :error]))))

;; ---------------------------------------------------------------- layout

(defn expanded [{:keys [parts]}] (:cells (shape/expand {:id "p" :status :active :parts parts} {})))

(defn chain-of [laid] (rail/line (expanded laid)))

(defn indices-of [laid block]
  (set (keep-indexed (fn [i cell] (when (= block (shape/want-block (:want cell))) i)) (chain-of laid))))

(defn cells-wanting [laid block] (set (map :pos (filter #(= block (shape/want-block (:want %))) (expanded laid)))))

(defn part-cells [laid id] (set (map :pos (filter #(= id (:part %)) (expanded laid)))))

(def line-x [[0 64 0] [29 64 0]])

(defn laid [opts] (apply rail/layout (conj line-x opts)))

(deftest the-spaced-line-has-a-launch-group-at-each-end-then-one-lit-rail-every-power-every-cells
  (are [opts powered] (= powered (indices-of (laid opts) "powered_rail"))
    {} #{2 3 4 5 19 24 25 26 27}
    {:power-every 6} #{2 3 4 5 11 17 23 24 25 26 27}
    {:launch 3} #{2 3 4 18 25 26 27}
    {:launch 5} #{2 3 4 5 6 20 23 24 25 26 27}
    {:launch-ends :first} #{2 3 4 5 19}
    {:style :all-powered} (set (range 2 28))
    {:style :all-powered :launch-ends :first} (set (range 2 28))))

(deftest every-end-is-a-buffer-and-two-normal-rails
  (are [opts] (let [chain (mapv :pos (chain-of (laid opts)))
                    ends (into (subvec chain 0 2) (subvec chain 28))]
                (and (= #{[-1 64 0] [30 64 0]} (part-cells (laid opts) "buffers"))
                     (= #{} (set (filter (indices-of (laid opts) "powered_rail") [0 1 28 29])))
                     (= ends [[0 64 0] [1 64 0] [28 64 0] [29 64 0]])))
    {} {:style :all-powered} {:launch-ends :first} {:launch 3} {:launch 5} {:power :lever}))

(deftest the-power-source-takes-the-place-the-option-names
  (are [opts block cells] (= cells (cells-wanting (laid opts) block))
    {} "redstone_torch" #{[3 64 -1] [19 64 -1] [25 64 -1]}
    {:power-side :right} "redstone_torch" #{[3 64 1] [19 64 1] [25 64 1]}
    {:power :lever} "lever" #{[3 64 -1] [19 64 -1] [25 64 -1]}
    {:power :block} "redstone_block" #{[3 63 0] [19 63 0] [25 63 0]}
    {:style :all-powered} "redstone_block" #{[10 63 0] [23 63 0]}
    {:style :all-powered :power :torch} "redstone_torch" #{[10 64 -1] [23 64 -1]}))

(deftest a-torch-or-lever-stands-on-a-bed-cell-of-its-own
  (are [opts] (every? (cells-wanting (laid opts) (shape/want-block bed))
                      (map #(update % 1 dec) (into (cells-wanting (laid opts) "redstone_torch") (cells-wanting (laid opts) "lever"))))
    {} {:power :lever} {:style :all-powered :power :torch}))

(defn reach-ok?
  "Every powered rail has a source within 8 rails along its own unbroken run of powered rails, and no run has more
  sources than it needs (one per 17 rails)."
  [laid]
  (let [chain (vec (chain-of laid))
        powered (indices-of laid "powered_rail")
        sources (into (cells-wanting laid "redstone_block") (map #(update % 1 dec)) (cells-wanting laid "redstone_torch"))
        source? (fn [i] (let [[x y z] (:pos (chain i))]
                          (some sources [[x (dec y) z] [x (dec y) (dec z)] [x (dec y) (inc z)]])))
        runs (partition-by #(contains? powered %) (range (count chain)))
        powered-runs (filter #(contains? powered (first %)) runs)]
    (every? (fn [run]
              (let [lit (filter source? run)]
                (and (= (count lit) (quot (+ (count run) 16) 17))
                     (every? (fn [i] (some #(<= (abs (- i %)) 8) lit)) run))))
            powered-runs)))

(deftest one-source-lights-at-most-17-powered-rails-and-every-powered-rail-is-reached
  (are [from to opts] (reach-ok? (rail/layout from to opts))
    [0 64 0] [29 64 0] {}
    [0 64 0] [29 64 0] {:style :all-powered}
    [0 64 0] [29 64 0] {:style :all-powered :power :torch}
    [0 64 0] [59 64 0] {:style :all-powered}
    [0 64 0] [40 64 0] {:style :all-powered :launch-ends :first}
    [0 64 0] [99 64 0] {:power :block}))

(deftest the-bed-want-is-the-fill-set-and-the-head-is-cleared
  (are [opts want] (= want (:want (first (filter #(= [5 63 0] (:pos %)) (expanded (laid opts))))))
    {} [:any "cobblestone" "dirt" "stone"]
    {:fill #{"andesite"}} [:any "andesite"]
    {:fill #{"oak_planks" "stone"}} [:any "oak_planks" "stone"])
  (is (= (set (for [x (range 30)] [x 65 0])) (set (map :pos (filter #(= :clear (:want %)) (expanded (laid {}))))))))

(deftest a-line-along-z-runs-north-south-and-its-left-is-west-going-north
  (let [l (rail/layout [5 70 10] [5 70 -19] {})]
    (is (= #{:north_south} (set (map #(get-in % [:want :shape]) (chain-of l)))))
    (is (= #{[4 70 7] [4 70 -9] [4 70 -15]} (cells-wanting l "redstone_torch")))
    (is (= #{[5 70 11] [5 70 -20]} (part-cells l "buffers")))))

(deftest the-materials-are-counted
  (are [opts materials] (= materials (:materials (laid opts)))
    {} {:items {"rail" 21 "powered_rail" 9 "redstone_torch" 3} :fill 37}
    {:power :lever} {:items {"rail" 21 "powered_rail" 9 "lever" 3} :fill 37}
    {:power :block} {:items {"rail" 21 "powered_rail" 9 "redstone_block" 3} :fill 31}
    {:style :all-powered} {:items {"rail" 4 "powered_rail" 26 "redstone_block" 2} :fill 32}))

(deftest a-layout-is-a-good-plan-whose-planned-world-passes-the-proof
  (are [opts] (let [parts (:parts (laid opts))
                    cells (expanded (laid opts))
                    world (into {} (keep (fn [{:keys [pos want]}]
                                           (when-not (= :clear want)
                                             (let [b (shape/want-block want)]
                                               [pos {:name b :state (into {} (map (fn [[k v]] [k (shape/state-text v)]))
                                                                         (when (map? want) (dissoc want :block)))}]))))
                                cells)]
                (and (= [] (shape/plan-errors {:id "p" :status :active :parts parts} "p"))
                     (= 30 (count (rail/line cells)))
                     (= {:ok? true :breaks [] :hazards []} (rail/judge-line cells (block-at world)))))
    {} {:style :all-powered} {:power :lever} {:launch-ends :first}))

(deftest a-layout-it-cannot-make-says-why
  (are [from to opts error] (= {:error error} (rail/layout from to opts))
    [0 64 0] [29 65 0] {} :not-flat
    [0 64 0] [29 64 3] {} :not-straight
    [0 64 0] [0 64 0] {} :not-straight
    [0 64 0] [10 64 0] {} :too-short
    [0 64 0] [5 64 0] {:launch-ends :first :launch 3} :too-short
    [0 64 0] [29 64 0] {:launch 6} :launch
    [0 64 0] [29 64 0] {:launch 2} :launch
    [0 64 0] [29 64 0] {:power :piston} :power
    [0 64 0] [29 64 0] {:style :zigzag} :style
    [0 64 0] [29 64 0] {:fill #{}} :fill))
