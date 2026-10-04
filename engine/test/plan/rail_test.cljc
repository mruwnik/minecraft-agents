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
    {} #{2 3 4 5 24 25 26 27}
    {:power-every 6} #{2 3 4 5 11 17 23 24 25 26 27}
    {:launch 3} #{2 3 4 25 26 27}
    {:launch 5} #{2 3 4 5 6 23 24 25 26 27}
    {:launch-ends :first} #{2 3 4 5}
    {:style :all-powered} (set (range 2 28))
    {:style :all-powered :launch-ends :first} (set (range 2 28))))

(deftest the-default-spacing-is-the-measured-30-cells
  (is (= 30 rail/power-every))
  (is (= #{2 3 4 5 35 65 94 95 96 97}
         (indices-of (rail/layout [0 64 0] [99 64 0] {}) "powered_rail"))))

(deftest every-end-is-a-buffer-and-two-normal-rails
  (are [opts] (let [chain (mapv :pos (chain-of (laid opts)))
                    ends (into (subvec chain 0 2) (subvec chain 28))]
                (and (= #{[-1 64 0] [30 64 0]} (part-cells (laid opts) "buffers"))
                     (= #{} (set (filter (indices-of (laid opts) "powered_rail") [0 1 28 29])))
                     (= ends [[0 64 0] [1 64 0] [28 64 0] [29 64 0]])))
    {} {:style :all-powered} {:launch-ends :first} {:launch 3} {:launch 5} {:power :lever}))

(deftest the-power-source-takes-the-place-the-option-names
  (are [opts block cells] (= cells (cells-wanting (laid opts) block))
    {} "redstone_torch" #{[3 64 -1] [25 64 -1]}
    {:power-side :right} "redstone_torch" #{[3 64 1] [25 64 1]}
    {:power :lever} "lever" #{[3 64 -1] [25 64 -1]}
    {:power :block} "redstone_block" #{[3 63 0] [25 63 0]}
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
    (is (= #{[4 70 7] [4 70 -15]} (cells-wanting l "redstone_torch")))
    (is (= #{[5 70 11] [5 70 -20]} (part-cells l "buffers")))))

(deftest the-materials-are-counted
  (are [opts materials] (= materials (:materials (laid opts)))
    {} {:items {"rail" 22 "powered_rail" 8 "redstone_torch" 2} :fill 36}
    {:power :lever} {:items {"rail" 22 "powered_rail" 8 "lever" 2} :fill 36}
    {:power :block} {:items {"rail" 22 "powered_rail" 8 "redstone_block" 2} :fill 32}
    {:power-every 14} {:items {"rail" 21 "powered_rail" 9 "redstone_torch" 3} :fill 37}
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

;; ---------------------------------------------------------------- layout: corners and slopes

(defn route [waypoints & [opts]] (rail/layout waypoints (or opts {})))

(defn shape-at [laid i] (get-in (nth (chain-of laid) i) [:want :shape]))

(def l-route [[0 64 0] [19 64 0] [19 64 12]])

(deftest a-corner-is-a-normal-rail-of-the-corner-shape-whichever-way-the-line-turns
  (are [waypoints shape] (= shape (shape-at (route waypoints) 15))
    [[0 64 0] [15 64 0] [15 64 15]] :south_west
    [[0 64 0] [15 64 0] [15 64 -15]] :north_west
    [[15 64 0] [0 64 0] [0 64 15]] :south_east
    [[15 64 0] [0 64 0] [0 64 -15]] :north_east
    [[0 64 0] [0 64 15] [15 64 15]] :north_east
    [[0 64 15] [0 64 0] [-15 64 0]] :south_west))

(deftest each-corner-gets-a-lit-powered-rail-on-both-sides-and-itself-stays-a-normal-rail
  (let [l (route l-route)]
    (is (= #{2 3 4 5 18 20 26 27 28 29} (indices-of l "powered_rail")))
    (is (= "rail" (shape/want-block (:want (nth (chain-of l) 19)))))
    (is (= [:east_west :south_west :north_south]
           (mapv #(shape-at l %) [18 19 20])))))

(deftest the-spacing-count-restarts-at-each-corner
  (let [powered (indices-of (route [[0 64 0] [60 64 0] [60 64 50]] {:power-every 10}) "powered_rail")]
    (is (= #{2 3 4 5 15 25 35 45 55 59 61 71 81 91 101 105 106 107 108} powered))))

(deftest a-u-turn-and-an-s-bend-are-lines-of-their-own
  (are [waypoints corners] (= corners (set (keep-indexed (fn [i c] (when (#{:south_west :south_east :north_west :north_east} (get-in c [:want :shape])) i))
                                                          (chain-of (route waypoints)))))
    [[0 64 0] [19 64 0] [19 64 3] [0 64 3]] #{19 22}
    [[0 64 0] [19 64 0] [19 64 3] [38 64 3]] #{19 22}))

(def slope-up [[0 64 0] [19 64 0] [25 70 0] [45 70 0]])
(def slope-down [[0 70 0] [14 70 0] [20 64 0] [40 64 0]])

(deftest a-slope-has-a-climbing-rail-every-cell-and-every-other-one-is-lit-from-its-lower-end
  (are [waypoints shapes lit] (let [l (route waypoints)]
                                (and (= shapes (set (map #(shape-at l %) (range (count (chain-of l))))))
                                     (= lit (set (filter (indices-of l "powered_rail") (range 15 30))))))
    slope-up #{:east_west :ascending_east} #{19 21 23}
    slope-down #{:east_west :ascending_west} #{16 18 20}))

(deftest a-slope-is-a-run-of-climbing-cells-whose-top-is-flat
  (let [l (route slope-up)]
    (is (= [:east_west :ascending_east :ascending_east :ascending_east :ascending_east :ascending_east :ascending_east :east_west]
           (mapv #(shape-at l %) (range 18 26)) ))
    (is (= (mapv #(vector % (+ 64 (max 0 (- % 19)))  0) (range 0 26)) (mapv :pos (take 26 (chain-of l)))))))

(deftest every-lit-slope-rail-has-a-source-of-its-own
  (let [l (route slope-up)]
    (is (= #{[19 64 -1] [21 66 -1] [23 68 -1]}
           (set (filter #(< 15 (first %) 25) (cells-wanting l "redstone_torch")))))))

(deftest a-slope-cell-needs-headroom-two-above-as-well-as-one
  (let [l (route slope-up)
        clear (part-cells l "head")]
    (is (every? clear [[19 65 0] [19 66 0] [24 70 0] [24 71 0] [25 71 0]]))
    (is (not (clear [25 72 0])))
    (is (not (clear [10 66 0])))))

(deftest a-corner-may-follow-a-slope-after-one-flat-cell-a-crest-is-fine-and-a-line-without-a-far-launch-group-may-end-short
  (are [waypoints opts] (nil? (:error (route waypoints opts)))
    [[0 64 0] [19 64 0] [25 70 0] [26 70 0] [26 70 20]] {}
    [[0 64 0] [19 64 0] [25 70 0] [31 64 0] [50 64 0]] {}
    [[0 64 0] [19 64 0] [19 64 4]] {:launch-ends :first}))

(defn world-of
  "The world a plan's cells describe: every wanted block with its wanted state, :clear cells left as air."
  [cells]
  (into {} (keep (fn [{:keys [pos want]}]
                   (when-not (= :clear want)
                     [pos {:name (shape/want-block want)
                           :state (into {} (map (fn [[k v]] [k (shape/state-text v)]))
                                        (when (map? want) (dissoc want :block)))}])))
        cells))

(deftest a-layout-of-corners-and-slopes-is-a-good-plan-whose-world-passes-the-proof
  (are [waypoints opts] (let [l (route waypoints opts)
                              cells (expanded l)]
                          (and (= [] (shape/plan-errors {:id "p" :status :active :parts (:parts l)} "p"))
                               (= {:ok? true :breaks [] :hazards []} (rail/judge-line cells (block-at (world-of cells))))))
    l-route {}
    l-route {:style :all-powered}
    l-route {:power :lever :power-side :right}
    [[0 64 0] [19 64 0] [19 64 3] [0 64 3]] {}
    [[0 64 0] [19 64 0] [19 64 3] [38 64 3]] {:power :block}
    slope-up {}
    slope-up {:style :all-powered}
    slope-down {}
    [[0 64 0] [19 64 0] [25 70 0] [26 70 0] [26 70 20]] {}
    [[0 64 0] [19 64 0] [25 70 0] [31 64 0] [50 64 0]] {:power :lever}))

(deftest the-proof-names-a-corner-or-slope-built-wrong
  (let [corner-cells (expanded (route l-route))
        slope-cells (expanded (route slope-up))
        corner-world (world-of corner-cells)
        slope-world (world-of slope-cells)
        judge (fn [cells world] (select-keys (rail/judge-line cells (block-at world)) [:ok? :breaks]))]
    (are [cells world breaks] (= {:ok? false :breaks breaks} (judge cells world))
      corner-cells (assoc corner-world [19 64 0] {:name "rail" :state {:shape "east_west"}}) [{:pos [19 64 0] :why :shape}]
      corner-cells (assoc corner-world [19 64 0] {:name "rail" :state {:shape "north_west"}}) [{:pos [19 64 0] :why :shape}]
      corner-cells (assoc corner-world [18 64 0] {:name "powered_rail" :state {:shape "east_west" :powered "false"}}) [{:pos [18 64 0] :why :unlit}]
      slope-cells (assoc slope-world [21 66 0] {:name "powered_rail" :state {:shape "east_west" :powered "true"}}) [{:pos [21 66 0] :why :shape}]
      slope-cells (assoc slope-world [22 67 0] {:name "rail" :state {:shape "ascending_west"}}) [{:pos [22 67 0] :why :shape}]
      slope-cells (assoc slope-world [21 68 0] {:name "stone"}) [{:pos [21 66 0] :why :blocked}])))

(deftest corner-and-slope-materials-are-counted
  (are [waypoints opts materials] (= materials (:materials (route waypoints opts)))
    l-route {} {:items {"rail" 22 "powered_rail" 10 "redstone_torch" 4} :fill 40}
    slope-up {} {:items {"rail" 35 "powered_rail" 11 "redstone_torch" 5} :fill 55}))

(deftest the-torch-stands-outside-a-corner-unless-the-caller-names-a-side-and-two-rails-may-share-one
  (are [opts torches] (= torches (set (filter #(and (<= 15 (first %) 22) (<= (nth % 2) 1)) (cells-wanting (route l-route opts) "redstone_torch"))))
    {} #{[18 64 -1] [20 64 1]}
    {:power-side :left} #{[18 64 -1] [20 64 1]}
    {:power-side :right} #{[18 64 1]}))

(deftest a-lines-that-cannot-be-made-says-why-and-where
  (are [waypoints opts error] (= error (select-keys (route waypoints opts) [:error :leg :at]))
    [[0 64 0] [5 64 0] [5 64 20]] {} {:error :leg-too-short :leg 0}
    [[0 64 0] [19 64 0] [19 64 4]] {} {:error :leg-too-short :leg 1}
    [[0 64 0] [19 64 0] [19 64 2] [5 64 2]] {} {:error :leg-too-short :leg 1}
    [[0 64 0] [19 64 0] [25 70 0] [25 70 20]] {} {:error :slope-into-corner :at [25 70 0] :leg 1}
    [[0 64 0] [19 64 0] [19 64 20] [25 64 26]] {} {:error :not-straight :leg 2}
    [[0 64 0] [6 64 0] [12 70 0] [40 70 0]] {} {:error :slope-into-launch :at [6 64 0] :leg 0}
    [[0 64 0] [19 64 0] [25 67 0] [45 67 0]] {} {:error :bad-slope :leg 1}
    [[0 64 0] [19 64 0] [10 64 0]] {} {:error :reversal :leg 1 :at [19 64 0]}
    [[0 64 0] [10 64 0] [12 62 0] [14 64 0] [30 64 0]] {} {:error :valley :at [12 62 0] :leg 1}
    [[0 64 0] [30 64 0] [30 64 12] [10 64 12] [10 64 1]] {} {:error :touching :at [10 64 1] :leg 3}
    [[0 64 0] [19 64 0] [19 64 0]] {} {:error :not-straight :leg 1}
    [[0 64 0] [19 64 0]] {:launch 6} {:error :launch}))

(deftest a-power-side-the-caller-asked-for-that-is-taken-by-the-line-is-an-error-naming-the-cell
  (are [occupied opts result] (= result (rail/power-cell [[18 64 0] [19 64 0]] occupied opts 0))
    #{[18 64 1]} {:caller-side :right} {:error :power-side :at [18 64 0] :side :right}
    #{[18 64 1]} {:caller-side :left} {:cell [18 64 -1]}
    #{[18 64 -1]} {} {:cell [18 64 1]}
    #{[18 64 -1]} {:outer :right} {:cell [18 64 1]}
    #{} {:outer :right} {:cell [18 64 1]}
    #{[18 62 -1]} {} {:cell [18 64 1]}
    #{[18 61 -1]} {} {:cell [18 64 -1]}
    #{[18 64 1] [18 64 -1]} {} {:error :power-side :at [18 64 0]}))
