(ns engine.breathe-test
  "The suffocating trigger and the breathe job against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.seek-contract-test :as sc]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.world-files :as world-files]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost.planner :as planner]
            [jobs.lib.shore :as shore]
            [jobs.lib.swim :as swim]
            [jobs.survival.breathe :as b]
            [jobs.survival.breathe.air :as air]
            [jobs.survival.breathe.fight :as fight]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def breathe 'jobs.survival.breathe)

(defn setup
  "An engine over a fake body; raw (an atom) also collects the canonical events, with their levels."
  ([world] (setup world (atom [])))
  ([world raw]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/seeing-all (tu/fake world))
         eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink #(swap! raw conj %)] :now #(deref clock)})})]
     {:eng eng :p p :seen seen :clock clock})))

(defn holds? [p args]
  ((:when (get triggers/all :suffocating)) p nil args))

(defn check? [p args]
  (b/needs-air? {:primitives p :args args}))

(def defaults {})

(def knobs
  {:radius :jobs.survival.breathe.air/radius :air-radius :jobs.lib.swim/air-radius :reach :jobs.lib.swim/reach
   :shore-radius :jobs.survival.breathe/shore-radius :search-radius :jobs.survival.breathe/search-radius
   :leg-length :jobs.survival.breathe/leg-length :swim-range :jobs.survival.breathe/swim-range
   :max-legs :jobs.survival.breathe/max-legs})

(defn tune
  "args with the call's :settings set for the breathe knobs given as key/value pairs."
  [args & kvs]
  (update args :settings merge (into {} (map (fn [[k v]] [(knobs k) v])) (partition 2 kvs))))

(defn knob [args k] (get-in args [:settings (knobs k)]))

(def water-column
  "Feet at y 64 in water up to y 67; air from y 68."
  {"0,64,0" "water" "0,65,0" "water" "0,66,0" "water" "0,67,0" "water"})

(def walls
  "Stone around the water column, except the +x side, so only air above is open."
  (into {} (for [y (range 64 68) [x z] [[-1 0] [0 1] [0 -1]]] [(str x "," y "," z) "stone"])))

(def boxed
  "water-column with stone on every side and a ceiling beyond the cap dig."
  (merge water-column walls {"0,70,0" "stone"} (into {} (for [y (range 64 68)] [(str "1," y ",0") "stone"]))))

(def cases
  [["drowning: low oxygen, head under water"
    {:self {:inWater true :oxygen 4} :blocks water-column} true]
   ["swimming at the surface with low oxygen: head in air"
    {:self {:inWater true :oxygen 5} :blocks {"0,64,0" "water"}} false]
   ["in water with plenty of oxygen"
    {:self {:inWater true :oxygen 18} :blocks water-column} false]
   ["low oxygen but not in water"
    {:self {:inWater false :oxygen 5}} false]
   ["head inside stone"
    {:blocks {"0,65,0" "stone"}} true]
   ["head inside gravel"
    {:blocks {"0,65,0" "gravel"}} true]
   ["head in a torch"
    {:blocks {"0,65,0" "wall_torch"}} false]
   ["head in tall grass"
    {:blocks {"0,65,0" "tall_grass"}} false]
   ["head in air"
    {} false]
   ;; feet y is not an integer on farmland, slabs, paths: the eyes are 1.62 above the feet
   ["on farmland inside ripe wheat"
    {:self {:pos {:x 0 :y 64.9375 :z 0}} :blocks {"0,64,0" "farmland" "0,65,0" "wheat"}} false]
   ["on farmland inside freshly seeded wheat"
    {:self {:pos {:x 0 :y 64.9375 :z 0}} :blocks {"0,64,0" "farmland" "0,65,0" "wheat"}
     :ages {"0,65,0" 0}} false]
   ["on a bottom slab"
    {:self {:pos {:x 0 :y 64.5 :z 0}} :blocks {"0,64,0" "oak_slab"}} false]
   ["on a dirt path"
    {:self {:pos {:x 0 :y 64.9375 :z 0}} :blocks {"0,64,0" "dirt_path"}} false]
   ["in tall grass"
    {:blocks {"0,64,0" "tall_grass" "0,65,0" "tall_grass"}} false]
   ["in water one deep"
    {:self {:inWater true :oxygen 20} :blocks {"0,64,0" "water"}} false]
   ["wheat at the eyes"
    {:blocks {"0,65,0" "wheat"}} false]
   ["a bottom slab at the eyes"
    {:blocks {"0,65,0" "oak_slab"}} false]
   ["a flower the list never heard of at the eyes"
    {:blocks {"0,65,0" "wither_rose"}} false]
   ["stone on a body standing on farmland"
    {:self {:pos {:x 0 :y 64.9375 :z 0}} :blocks {"0,64,0" "farmland" "0,66,0" "stone"}} true]
   ["sand fallen on the head"
    {:blocks {"0,65,0" "sand"}} true]
   ["sunk into a solid block"
    {:blocks {"0,64,0" "dirt" "0,65,0" "dirt"}} true]])

(deftest trigger-and-check-agree-on-every-branch
  (doseq [[label world expected] cases]
    (let [p (tu/fake world)]
      (is (= expected (holds? p defaults)) (str "trigger: " label))
      (is (= expected (check? p defaults)) (str "check: " label)))))

(defn shaft
  "Water from y 64 to top in a stone shaft on a stone floor, air above."
  [top]
  (into {"0,63,0" "stone"} (for [y (range 64 (inc top)) [x z kind] [[0 0 "water"] [1 0 "stone"] [-1 0 "stone"] [0 1 "stone"] [0 -1 "stone"]]]
             [(str x "," y "," z) kind])))

(def short-shaft "Air from y 68: 3 blocks (1.5 s) up." (shaft 67))
(def long-shaft "Air from y 81: 16 blocks (8 s) up." (shaft 80))

(defn fires?
  "Whether the trigger's holds? and check? fire, with a swim of half a second a block (the shafts' geometry) unless kvs say otherwise."
  [world & kvs]
  (let [p (tu/fake world)]
    (settings/with-settings (apply hash-map :jobs.lib.breath/swim-ms 500 kvs) (fn [] [(holds? p {}) (check? p {})]))))

(deftest fires-when-air-left-is-short-of-the-swim-to-air-plus-margin
  (are [oxygen blocks expected] (= expected (fires? {:self {:inWater true :oxygen oxygen} :blocks blocks}))
    ;; 1.5 s to air, margin 3: air left 11.25 s (oxygen 15) or 3.75 s (oxygen 5) at 0.75 s a bubble
    15 short-shaft [false false]
    5 short-shaft [true true]
    4 short-shaft [true true]
    ;; 8 s to air: oxygen 15 leaves 11.25 s, over 8 + 3; oxygen 14 leaves 10.5 s, under it
    15 long-shaft [false false]
    14 long-shaft [true true]))

(deftest the-swim-price-is-the-planners-dive-price
  (is (= planner/swim-sub-ms (breath/swim-ms))))

(deftest the-drown-allowance-and-margin-move-the-fire-point
  (let [world {:self {:inWater true :oxygen 12} :blocks long-shaft}]
    (is (= [true true] (fires? world)))
    (is (= [false false] (fires? world :jobs.lib.breath/drown-hp 10)))
    (is (= [false false] (fires? world :jobs.lib.breath/margin-s 0)))))

(deftest no-air-seen-fires-at-once
  (is (= [true true] (fires? {:self {:inWater true :oxygen 20} :blocks (assoc (shaft 69) "0,70,0" "stone")}))))

(deftest water-breathing-never-fires
  (is (= [false false] (fires? {:self {:inWater true :oxygen 0 :effects [{:name "water_breathing" :left 600}]} :blocks long-shaft}))))

(defn climb-ctx
  "A ctx over p whose job memory holds data."
  [p data]
  {:primitives p :root "j1" :slots []
   :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data data}]}}})})

(deftest a-climb-that-rose-goes-on-while-the-head-is-under-though-the-air-left-fits
  (let [p (tu/fake {:self {:inWater true :oxygen 20} :blocks short-shaft})]
    (is (nil? (b/situation-of (climb-ctx p {}))) "no swim yet: plenty of air, no run")
    (is (= :drowning (b/situation-of (climb-ctx p {:best-y 65}))) "a swim rose without surfacing: swim on")
    (is (nil? (b/situation-of (climb-ctx (tu/fake {:self {:inWater true :oxygen 20} :blocks {"0,64,0" "water"}}) {:best-y 65})))
        "head out of the water: the climb is over")))

(deftest suffocating-is-registered-with-breathe
  (let [t (get triggers/all :suffocating)]
    (is (= '(jobs.survival.breathe) (:job t)))
    (is (= {} (:args t)))))

(defn call-names [p] (mapv #(.-name %) (array-seq (.. p -world -calls))))

(defn ended
  "The end events of jobs, [kind reason] in order: [:completed nil] or [:stopped reason]."
  [seen]
  (->> @seen
       (filter #(and (= :job (:source %)) (#{:stopped :completed} (:kind %))))
       (mapv (fn [e] [(:kind e) (or (:reason e) (:reason (:data e)))]))))

(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn ^:async one-run!
  "Submit breathe with args and run exactly one tick: one round is the whole attempt."
  [eng args]
  (core/submit! eng (list breathe args) {})
  (await (core/tick! eng)))

(deftest drowning-run-swims-up-and-stops-no-land-in-range-without-holding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (await (one-run! eng defaults))
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)) "feet at the top water block, head in air")
          (is (= [] (:list (core/state eng))) "one run")
          (is (= [[:stopped :no-land-in-range]] (ended seen)) "no land and no water to swim on: stopped, not completed")
          (is (= ["swim" "swim"] (call-names p)) "the swim surfaces; the last way is treading water before the stop")
          (is (true? (.-tread (.-args (last (array-seq (.. p -world -calls)))))) "the body treads, its head stays out while it stops")
          (is (empty? (of-kind seen :holding)) "no hold")
          (is (empty? (of-kind seen :afloat))))))))

(deftest drowning-with-a-failing-swim-tries-the-other-ways-then-is-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :swimFails true :blocks boxed})]
          (await (one-run! eng defaults))
          (is (= [] (:list (core/state eng))))
          (is (= [[:stopped :no-blocks]] (ended seen)) "a go-to and a footing block were tried; none carried")
          (is (empty? (of-kind seen :no_air)) "the ending carries it"))))))

(def side-args (tune defaults :radius 1))

(deftest a-column-seen-open-whose-swim-rises-no-higher-is-left-for-the-next-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:inWater true :oxygen 4} :swimFails true
                     :blocks (merge water-column
                                    {"1,64,0" "water" "1,65,0" "water" "1,66,0" "water" "1,67,0" "water"})}
              {:keys [eng p]} (setup world)]
          (await (one-run! eng side-args))
          (is (= ["swim" "moveTo"] (take 2 (call-names p))) "the failed column is not swum again; the body steps to the next"))))))

(deftest drowning-in-a-capped-column-moves-sideways-then-swims-up-in-one-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:inWater true :oxygen 4}
                     :blocks (merge water-column walls
                                    {"0,68,0" "stone"
                                     "1,64,0" "water" "1,65,0" "water" "1,66,0" "water" "1,67,0" "water"})}
              {:keys [eng p seen]} (setup world)]
          (await (one-run! eng side-args))
          (is (= ["moveTo" "swim"] (take 2 (call-names p))) "sideways at feet height, then up the adjacent column")
          (is (= {:x 0 :y 69 :z 0} (core/self-pos p)) "then walked onto the cap, the one land in reach")
          (is (= [[:completed nil]] (ended seen))))))))

(deftest drowning-with-no-air-in-reach-is-stopped-no-air
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4}
                                         :blocks (merge water-column walls {"0,68,0" "oak_planks" "1,64,0" "stone"})})]
          (await (one-run! eng side-args))
          (is (= [] (:list (core/state eng))))
          (is (= [[:stopped :no-air]] (ended seen)))
          (is (empty? (of-kind seen :no_air)) "the ending carries it")
          (is (every? some? ((juxt :air-radius :cap :pocket) (first (of-kind seen :stopped))))))))))

(deftest enclosed-run-digs-the-head-block-then-the-one-above-and-steps-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "stone" "0,66,0" "stone"}})]
          (await (one-run! eng defaults))
          (is (= "air" (.-name (.blockAt p (tu/pos 0 65 0)))) "head block dug")
          (is (= "air" (.-name (.blockAt p (tu/pos 0 66 0)))) "block above dug")
          (is (= {:x 0 :y 65 :z 0} (core/self-pos p)) "stepped up into the shaft")
          (is (= [] (:list (core/state eng))))
          (is (= [[:completed nil]] (ended seen))))))))

(deftest enclosed-with-a-dig-that-times-out-is-stopped-without-moving
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "obsidian"}})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (await (one-run! eng defaults))
          (is (= [] (:list (core/state eng))))
          (is (= ["dig" "dig" "dig"] (call-names p)) "three tries in the one run, no moveTo")
          (is (= [[:stopped :no-way-out]] (ended seen)))
          (is (empty? (of-kind seen :no-way-out)) "the ending carries it"))))))

(deftest enclosed-digs-that-work-but-leave-the-head-enclosed-are-progress-not-failures
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "stone" "0,66,0" "stone"}})
              digs (atom 0)]
          (.override (.-world p) "dig" (fn ^:async f [token a impl]
                                         (if (<= (swap! digs inc) 5)
                                           #js {:status "dug"}
                                           (await (impl token a)))))
          (await (one-run! eng defaults))
          (is (= [[:completed nil]] (ended seen)) "five refilled digs are not three failures in a row"))))))

(deftest enclosed-run-leaves-the-block-above-when-it-is-already-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "dirt"}})]
          (await (one-run! eng defaults))
          (is (= ["dig" "moveTo"] (call-names p)) "no second dig"))))))

(deftest a-clear-body-declines-and-its-round-is-done-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {})
              round (:round (get registry/jobs breathe))]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= :done (:kind (await (round {:primitives p :args defaults})))))
          (is (zero? (count (.. p -world -calls)))))))))

(deftest a-cut-round-yields-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {:self {:oxygenLevel 0} :blocks {"0,64,0" "water" "0,65,0" "water"}})]
          (is (= :continue (await (b/round {:primitives p :args defaults :alive? (constantly false)}))))
          (is (zero? (count (.. p -world -calls)))))))))

(deftest an-enclosed-run-remembers-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks {"0,65,0" "stone"}})]
          (await (one-run! eng defaults))
          (is (= :enclosed (:why (:data (mem/latest (mem/view (:store eng)) :breathe))))))))))

(deftest a-run-writes-one-breathe-memory-entry-with-where-and-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (await (one-run! eng defaults))
          (let [view (mem/view (:store eng))
                entry (mem/latest view :breathe)]
            (is (= 1 (count (mem/entries view :breathe))))
            (is (= :drowning (:why (:data entry))))
            (is (= {:x 0 :y 64 :z 0} (:pos (:data entry))) "where it started")
            (is (= {:cap 20 :ttl (* 60 60 1000)} (mem/policy view :breathe)))))))))

(def pool
  "Water at y 64 and 65 around the origin column; air above."
  (into {} (for [y [64 65] x [-1 0 1] z [-1 0 1]] [(str x "," y "," z) "water"])))

(deftest surfaced-in-a-pool-it-swims-onto-a-ledge-two-blocks-away-in-one-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})]
          (await (one-run! eng defaults))
          (is (= {:x 2 :y 65 :z 0} (core/self-pos p)) "stands on the ledge")
          (is (not (.-inWater (.self p))))
          (is (= ["swim" "swim"] (call-names p)) "the shore step swims, no moveTo")
          (is (= {:x 2 :y 65 :z 0} (js->clj (.-toward (.-args (last (array-seq (.. p -world -calls))))) :keywordize-keys true)))
          (is (= [] (:list (core/state eng))))
          (is (= [[:completed nil]] (ended seen))))))))

(deftest surfaced-beside-a-grass-covered-bank-it-lands-on-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"2,63,0" "dirt" "2,64,0" "grass_block" "2,65,0" "short_grass"})})]
          (await (one-run! eng defaults))
          (is (= {:x 2 :y 65 :z 0} (core/self-pos p)) "stands in the grass on the bank")
          (is (not (.-inWater (.self p))))
          (is (= [[:completed nil]] (ended seen))))))))

(deftest surfaced-with-the-rim-two-blocks-above-the-feet-it-still-finds-the-ledge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"2,64,0" "stone" "2,65,0" "stone" "2,66,0" "stone"})})]
          (await (one-run! eng defaults))
          (is (= {:x 2 :y 67 :z 0} (core/self-pos p)) "stands on the ledge, feet two above the own")
          (is (= [[:completed nil]] (ended seen))))))))

(defn set-self!
  "Set fields of the fake body's self sensing (a bob above the surface, standing on the ground)."
  [p fields]
  (fake/swap-self! p into (map (fn [[k v]] [(keyword k) v]) fields)))

(defn pond
  "Water y 62..65 over a stone floor at y 61, for x and z within r of the origin; air above."
  [r]
  (into {} (for [x (range (- r) (inc r)) z (range (- r) (inc r))
                 [y n] (cons [61 "stone"] (map (fn [y] [y "water"]) (range 62 66)))]
             [(str x "," y "," z) n])))

(defn bank
  "Stone at y 61..64 for x from x0 to x1, z within r: land level with the water surface."
  [x0 x1 r]
  (into {} (for [x (range x0 (inc x1)) z (range (- r) (inc r)) y (range 61 65)] [(str x "," y "," z) "stone"])))

(deftest a-body-sunk-below-the-surface-still-finds-a-bank-flush-with-the-surface
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 62 :z 0}}
                                           :blocks (merge (pond 3) (bank 4 5 3))})]
          (.override (.-world p) "swim"
                     (fn ^:async f [_ args impl]
                       (if (.-toward args)
                         (await (impl _ args))
                         (do (set-self! p {"oxygen" 20}) #js {:status "surfaced"}))))
          (await (one-run! eng defaults))
          (is (= {:x 4 :y 65 :z 0} (core/self-pos p)) "swam onto the ledge from three blocks below the surface")
          (is (not (.-inWater (.self p))))
          (is (= [[:completed nil]] (ended seen))))))))

(deftest surface-pos-raises-the-body-to-the-top-of-its-water-column
  (let [p (tu/seeing-all (tu/fake {:blocks (pond 3)}))]
    (doseq [[y expected] [[62 65] [64 65] [65 65] [66 66]]]
      (is (= {:x 0 :y expected :z 0} (b/surface-pos p {:x 0 :y y :z 0} 10)) (str "feet at " y)))))


(deftest a-walled-nearer-shore-is-skipped-for-the-open-one-beyond
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"-2,64,0" "stone" "-2,63,0" "stone"
                                                                "3,64,0" "stone" "3,63,0" "stone"
                                                                "2,64,0" "water" "2,65,0" "water"})})]
          (.override (.-world p) "swim"
                     (fn ^:async f [_ args impl]
                       (if (and (.-toward args) (neg? (.-x (.-toward args))))
                         #js {:status "timeout"}
                         (await (impl _ args)))))
          (await (one-run! eng defaults))
          (is (= {:x 3 :y 65 :z 0} (core/self-pos p)) "the east ledge is reached after the west swim timed out")
          (is (= [[:completed nil]] (ended seen)))
          (is (empty? (of-kind seen :no_shore))))))))

;; live (card 95752610): land behind a wall two blocks above the water is nearer than the open ledge; a swim can only
;; climb out onto a rim at the water, so it heads for the ledge first and never swims at the wall
(deftest land-behind-a-high-wall-is-not-a-shore-the-farther-ledge-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [y (range 64 68) z [-1 0 1]] [(str "-2," y "," z) "stone"]))
              {:keys [eng p]} (setup {:self {:inWater true :oxygen 4}
                                      :blocks (merge pool wall
                                                     {"-3,64,0" "stone" "-3,63,0" "stone"
                                                      "2,64,0" "water" "2,65,0" "water" "3,64,0" "water" "3,65,0" "water"
                                                      "4,64,0" "stone" "4,63,0" "stone"})})]
          (await (one-run! eng defaults))
          (is (= [{:x 4 :y 65 :z 0}]
                 (->> (array-seq (.. p -world -calls))
                      (keep #(some-> (.-args %) .-toward (js->clj :keywordize-keys true)))))
               "one shore swim, toward the ledge")
          (is (= {:x 4 :y 65 :z 0} (core/self-pos p))))))))

;; a pond with a ledge in every direction: the swims west, north and south time out, then the east ledge is tried
(deftest every-shore-direction-is-tried-before-no-shore
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"-2,64,0" "stone" "-2,63,0" "stone"
                                                                "0,64,-2" "stone" "0,63,-2" "stone"
                                                                "0,64,2" "stone" "0,63,2" "stone"
                                                                "3,64,0" "stone" "3,63,0" "stone"
                                                                "2,64,0" "water" "2,65,0" "water"})})]
          (.override (.-world p) "swim"
                     (fn ^:async f [_ args impl]
                       (if (and (.-toward args) (< (.-x (.-toward args)) 3))
                         #js {:status "timeout"}
                         (await (impl _ args)))))
          (await (one-run! eng defaults))
          (is (= {:x 3 :y 65 :z 0} (core/self-pos p)) "reached the east ledge after three failed shores")
          (is (= [[:completed nil]] (ended seen)))
          (is (empty? (of-kind seen :no_shore))))))))

;; live (card 95752610): a swim toward a walled shore times out with the body on a jump crest against the wall,
;; out of the water for that moment (inWater false, onGround true or false) but over water
;; a go-to swims over a floor: the fake's walker needs ground under the water
(def pool-floor (into {} (for [x (range -1 2) z (range -1 2)] [(str x ",63," z) "stone"])))

(defn ^:async failed-shore-swim-run!
  "A shore swim that times out with the body out of the water (on-ground as given), then the go-to onto the ledge."
  [on-ground]
  (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                     :blocks (merge pool pool-floor {"2,64,0" "stone" "2,63,0" "stone"})})]
    (.override (.-world p) "swim"
               (fn ^:async f [_ args impl]
                 (if (.-toward args)
                   (do (set-self! p {"inWater" false "onGround" on-ground}) #js {:status "timeout"})
                   (await (impl _ args)))))
    (await (one-run! eng defaults))
    {:ended (ended seen) :pos (core/self-pos p) :holding (of-kind seen :holding)}))

(deftest a-failed-shore-swim-falls-through-to-a-go-to-and-standing-on-the-ledge-is-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (failed-shore-swim-run! true))]
          (is (= [[:completed nil]] (:ended r)))
          (is (= {:x 2 :y 65 :z 0} (:pos r)) "stands on the ledge")
          (is (empty? (:holding r)) "no hold"))))))

(deftest a-go-to-arriving-without-standing-on-land-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (failed-shore-swim-run! false))]
          (is (= [[:stopped :no-land-in-range]] (:ended r)) "the go-to arrives on the ledge, but not onGround")
          (is (= {:x 2 :y 65 :z 0} (:pos r)))
          (is (empty? (:holding r)) "no hold"))))))

(deftest enclosed-with-a-free-neighbour-steps-sideways-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "obsidian" "1,63,0" "stone"}})]
          (await (one-run! eng defaults))
          (is (= {:x 1 :y 64 :z 0} (core/self-pos p)) "stepped to the free neighbour")
          (is (= ["moveTo"] (call-names p)) "no dig")
          (is (= [[:completed nil]] (ended seen))))))))

(deftest enclosed-with-a-failed-side-step-digs-in-the-same-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "dirt" "1,63,0" "stone"}})]
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "blocked"}))
          (await (one-run! eng defaults))
          (is (= ["moveTo" "dig" "moveTo"] (take 3 (call-names p))) "the side step once, then the dig")
          (is (= "air" (.-name (.blockAt p (tu/pos 0 65 0)))))
          (is (= [] (:list (core/state eng)))))))))

(deftest breathe-never-digs-a-block-that-cannot-suffocate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos {:x 0 :y 64.9375 :z 0}}
                                      :blocks {"0,64,0" "farmland" "0,65,0" "wheat" "0,66,0" "wheat"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= [] (call-names p)) "no dig, no move")
          (is (= "wheat" (.-name (.blockAt p (tu/pos 0 65 0)))) "the crop is still there"))))))

;; ---------------------------------------------------------------- the afloat ladder: shore, go-to land, swim legs, stop

(def small-args
  "Shrunk bounds so the fake planner stays small."
  (tune defaults :shore-radius 2 :search-radius 12 :leg-length 10 :swim-range 24 :max-legs 4))

(def no-shore-args "No shore swims: only the go-to child and the legs move the body." (tune small-args :shore-radius 0))

(defn lake
  "Water y 62..65 over a stone floor at y 61, x from x0 to x1, z within r; air above."
  [x0 x1 r]
  (into {} (for [x (range x0 (inc x1)) z (range (- r) (inc r))
                 [y n] (cons [61 "stone"] (map (fn [y] [y "water"]) (range 62 66)))]
             [(str x "," y "," z) n])))

(defn dist-from-origin [p] (let [{:keys [x z]} (core/self-pos p)] (js/Math.hypot x z)))

(defn stopped-data
  "The data of the first job.stopped event of the run."
  [seen]
  (let [e (first (filter #(and (= :job (:source %)) (= :stopped (:kind %))) @seen))]
    (merge e (:data e))))

(defn unreachable-count [seen] (count (filter #(= :unreachable (get-in % [:ending :cause :reason])) (of-kind seen :child_ended))))

(deftest a-bank-beyond-the-shore-radius-is-reached-with-a-go-to-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -6 6 6) (bank 7 9 6))})]
          (await (one-run! eng no-shore-args))
          (is (= [[:completed nil]] (ended seen)))
          (is (>= (:x (core/self-pos p)) 7) "stands on the bank")
          (is (not (.-inWater (.self p))))
          (is (empty? (of-kind seen :holding))))))))

(deftest a-farmland-bank-beyond-the-shore-radius-is-reached-with-a-go-to-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [farm (into {} (for [x (range 7 10) z (range -6 7)] [(str x ",64," z) "farmland"]))
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -6 6 6) (bank 7 9 6) farm)})]
          (await (one-run! eng no-shore-args))
          (is (= [[:completed nil]] (ended seen)) "the go-to target over farmland is a cell the planner stands in")
          (is (>= (:x (core/self-pos p)) 7) "stands on the farmland bank")
          (is (not (.-inWater (.self p)))))))))

(def wall-all-round
  (into {} (for [x (range -7 8) z (range -7 8) y [65 66]
                 :when (= 7 (max (js/Math.abs x) (js/Math.abs z)))]
             [(str x "," y "," z) "stone"])))

(deftest walled-all-round-stops-no_land_in_range-with-its-fields-and-never-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -6 6 6) (bank 7 7 6) (bank -7 -7 6) wall-all-round)})]
          (await (one-run! eng no-shore-args))
          (is (= [[:stopped :no-land-in-range]] (ended seen)))
          (is (< (dist-from-origin p) 7) "never left the pond")
          (is (empty? (of-kind seen :holding)) "no job.holding event")
          (is (empty? (of-kind seen :afloat)))
          (let [d (stopped-data seen)]
            (is (= 12 (:searched d)))
            (is (= 0 (:legs d)) "the pond is narrower than a leg")
            (is (number? (:swum d)))
            (is (number? (:headings-failed d)))))))))

(deftest at-most-land-tries-go-tos-per-spot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                         :blocks (merge (lake -6 6 6) (bank 7 7 6) (bank -7 -7 6) wall-all-round)})]
          (await (one-run! eng no-shore-args))
          (is (= [[:stopped :no-land-in-range]] (ended seen)))
          (is (<= 1 (unreachable-count seen) (b/land-tries))))))))

(deftest open-water-with-no-land-swims-legs-inside-the-bounds-then-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (lake -30 30 12)})]
          (await (one-run! eng small-args))
          (is (= [[:stopped :no-land-in-range]] (ended seen)))
          (let [d (stopped-data seen)]
            (is (pos? (:legs d)) "it swam outward before it gave up")
            (is (<= (:legs d) (knob small-args :max-legs))))
          (is (<= (dist-from-origin p) (+ 2 (knob small-args :swim-range))) "never beyond the swim range"))))))

(deftest the-leg-count-is-capped-by-max-legs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                         :blocks (lake -50 50 12)})]
          (await (one-run! eng (tune small-args :max-legs 2 :swim-range 60 :search-radius 5)))
          (is (= [[:stopped :no-land-in-range]] (ended seen)))
          (is (= 2 (:legs (stopped-data seen)))))))))

(deftest a-bank-beyond-the-search-radius-is-found-after-a-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -30 19 8) (bank 20 24 8))})]
          (await (one-run! eng small-args))
          (is (= [[:completed nil]] (ended seen)))
          (is (>= (:x (core/self-pos p)) 20) "on the east bank: the first leg went east, then the go-to landed"))))))

(deftest a-walled-target-is-skipped-for-the-open-bank-in-another-direction
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [east-wall (into {} (for [y (range 65 69) z (range -6 7)] [(str "6," y "," z) "bedrock"]))
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -6 5 6) (bank 6 8 6) east-wall (bank -10 -7 6))})]
          (await (one-run! eng small-args))
          (is (= [[:completed nil]] (ended seen)))
          (is (neg? (:x (core/self-pos p))) "the walled east bank is no shore (a wall two above the water is climbable, higher is not): it landed west")
          (is (zero? (unreachable-count seen)) "no go-to was spent on the walled bank"))))))

(deftest legal-headings-go-outward-only-and-skip-failed-ones
  (let [h (fn [pos failed] (b/legal-headings pos {:x 0 :z 0} failed))]
    (is (= 8 (count (h {:x 0 :z 0} #{}))) "all eight at the start")
    (is (= [1 0] (first (h {:x 0 :z 0} #{}))) "east first")
    (is (not-any? #{[-1 0] [-1 1] [-1 -1]} (h {:x 10 :z 0} #{})) "none back toward the start")
    (is (not-any? #{[1 0]} (h {:x 0 :z 0} #{[1 0]})) "a failed one is skipped")))

(deftest a-leg-that-moved-clears-the-failed-ways-a-leg-that-did-not-fails-its-heading
  (let [m {:failed-shores [1] :failed-land [2] :land-tries 3 :failed-headings #{[1 0]} :legs 1}
        moved (b/after-leg m [0 1] true)
        stuck (b/after-leg m [0 1] false)]
    (is (= 2 (:legs moved)))
    (is (nil? (:failed-shores moved)))
    (is (nil? (:failed-land moved)))
    (is (nil? (:land-tries moved)))
    (is (empty? (:failed-headings moved)) "east is tried again from the new spot")
    (is (= #{[1 0] [0 1]} (:failed-headings stuck)))
    (is (= 1 (:legs stuck)))))

(deftest a-cut-during-the-go-to-ends-the-run-and-the-refire-lands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -6 6 6) (bank 7 9 6))})
              steers (atom 0)]
          (core/register-reflex! eng {:trigger :suffocating})
          (.override (.-world p) "steer"
                     (fn ^:async f [token args impl]
                       (when (= 1 (swap! steers inc)) (core/cut! eng (core/holder eng) :test nil))
                       (await (impl token args))))
          (await (core/tick! eng))
          (is (not-any? #{[:completed nil]} (ended seen)) "the cut run did not complete")
          (set-self! p {"oxygen" 4 "pos" [0 62 0] "inWater" true})
          (await (core/tick! eng))
          (is (>= (:x (core/self-pos p)) 7) "the refire starts from the world and lands"))))))

;; ---------------------------------------------------------------- follow-ups: travel bound, child results, refire, warns

(deftest the-travel-bound-stops-a-run-whose-go-tos-used-it-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ring (into {} (for [x (range -15 16) z (range -15 16) y [65 66]
                                  :when (= 15 (max (js/Math.abs x) (js/Math.abs z)))]
                              [(str x "," y "," z) "stone"]))
              {:keys [eng seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                         :blocks (merge (lake -14 14 14) (bank 15 15 14) (bank -15 -15 14) ring)})
              args (tune no-shore-args :swim-range 9 :leg-length 9 :search-radius 16)]
          (await (one-run! eng args))
          (is (= [[:stopped :no-land-in-range]] (ended seen)))
          (is (<= 1 (unreachable-count seen) (b/land-tries)) "go-tos tried, each walks the body toward the wall so later tries are short"))))))

(defn stub-child-ctx
  "A ctx whose go-to child call resolves to the ending e, with a go-to registry entry."
  [e]
  {:engine {:jobs {'jobs.movement.go-to {:ways [:walk :clear-seal] :round (fn [_] nil)}}}
   :call (fn [_slot _def _args] (js/Promise.resolve e))})

(deftest a-go-to-child-that-continues-or-does-not-end-done-is-a-failed-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [e [:continue (contract/ending :condition-failed :declined nil)
                   (contract/ending :done nil {:arrived false})]]
          (is (false? (await (air/go! (stub-child-ctx e) :leg {:x 0 :y 64 :z 0} 2))) (str e)))
        (is (true? (await (air/go! (stub-child-ctx (contract/ending :done nil {:arrived true})) :leg {:x 0 :y 64 :z 0} 2))))))))

(deftest the-go-to-child-may-drown-the-body-when-no-other-way-exists
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sent (atom nil)
              c (assoc (stub-child-ctx nil) :call (fn [_slot _def args] (reset! sent args) (js/Promise.resolve (contract/ending :done nil {:arrived true}))))]
          (await (air/go! c :leg {:x 0 :y 64 :z 0} 2))
          (is (= {:jobs.lib.cost.planner/lethal-air true :jobs.lib.walk.world/dive-pause-s 0} (:settings @sent)))
          (is (= #{:walk :clear-seal} (:ways @sent)) "changes nothing on the way but its own seal round the air"))))))

(deftest a-refire-after-a-stop-keeps-the-searched-area
  (let [entries [{:data {:start {:x 0 :y 64 :z 0} :pos {:x 100 :y 65 :z 100} :headings []}}
                 {:data {:start {:x 5 :y 64 :z 5} :pos {:x 8 :y 65 :z 8} :headings [[1 0]]}}]]
    (is (= {:start {:x 5 :y 64 :z 5} :failed #{[1 0]}} (b/initial-search entries {:x 10 :z 10})) "the start is the earlier run's")
    (is (= {:start nil :failed #{}} (b/initial-search entries {:x 50 :z 50})))))

(deftest a-refire-in-the-searched-lake-stops-at-once-without-new-legs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (lake -60 60 14)})]
          (await (one-run! eng small-args))
          (is (pos? (:legs (stopped-data seen))) "the first run swam legs")
          (let [{:keys [x z]} (core/self-pos p)]
            (reset! seen [])
            (set-self! p {"oxygen" 4 "pos" [x 62 z] "inWater" true})
            (await (one-run! eng small-args))
            (is (= [[:stopped :no-land-in-range]] (ended seen)))
            (is (= 0 (:legs (stopped-data seen))) "no leg repeats the first run's")))))))

(deftest a-walled-in-breathe-run-ends-after-its-go-to-children-gave-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (atom [])
              {:keys [eng seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                         :blocks (merge (lake -6 6 6) (bank 7 7 6) (bank -7 -7 6) wall-all-round)}
                                        raw)
              gave-up? #(= :unreachable (get-in % [:data :ending :cause :reason]))]
          (await (one-run! eng no-shore-args))
          (is (= [[:stopped :no-land-in-range]] (ended seen)))
          (is (pos? (count (filter gave-up? @raw))) "the children gave up"))))))

;; ---------------------------------------------------------------- as a reflex: one run, cut, refire

(deftest the-reflex-run-ends-in-one-tick-and-never-yields
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})]
          (core/register-reflex! eng {:trigger :suffocating})
          (await (core/tick! eng))
          (is (= {:x 2 :y 65 :z 0} (core/self-pos p)) "on the ledge after one tick")
          (is (empty? (:instances (core/state eng))) "the reflex job has ended")
          (is (empty? (of-kind seen :yielded)) "no :continue"))))))

(deftest a-cut-mid-swim-ends-the-run-and-the-refire-starts-from-the-world
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})
              swims (atom 0)]
          (core/register-reflex! eng {:trigger :suffocating})
          (.override (.-world p) "swim"
                     (fn ^:async f [token args impl]
                       (if (= 1 (swap! swims inc))
                         (do (core/cut! eng (core/holder eng) :test nil) #js {:status "timeout"})
                         (await (impl token args)))))
          (await (core/tick! eng))
          (is (= 1 @swims) "the cut round acts no more")
          (is (= {:x 0 :y 64 :z 0} (core/self-pos p)) "still under water")
          (is (not-any? #{[:stopped :no-air]} (ended seen)) "a cut is not a failed try")
          (await (core/tick! eng))
          (is (= 3 @swims) "the refire swims up, then treads before it stops")
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p))))))))

(deftest a-run-cut-mid-swim-by-a-higher-reflex-is-ended-cut-and-the-refire-surfaces-from-the-world
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})
              reflex-events (fn [kind] (filterv #(and (= :reflex (:source %)) (= kind (:kind %))) @seen))]
          (core/register-reflex! eng {:trigger :burning})
          (core/register-reflex! eng {:trigger :suffocating})
          (let [release (.hold (.-world p) "swim")
                run (core/tick! eng)]
            (await (js/Promise. (fn [r] (js/setTimeout r 50))))
            (is (= [:suffocating] (mapv :reflex (reflex-events :fired))) "breathe is swimming")
            (fake/swap-self! p assoc :onFire true)
            (let [cutter (core/tick! eng)]
              (release)
              (await run)
              (await cutter)))
          (is (some #(and (= :suffocating (:reflex %)) (= :dropped (:outcome %)) (= :burning (:by %))) (reflex-events :ended)) "the swim's run was dropped by the fire reflex")
          (is (empty? (of-kind seen :afloat)) "a cut is not an afloat give-up")
          (fake/swap-self! p assoc :onFire false)
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (>= (count (filter #(= :suffocating (:reflex %)) (reflex-events :fired))) 2) "breathe fired again")
          (is (= {:x 2 :y 65 :z 0} (core/self-pos p)) "the refire re-read the world and landed on the ledge")
          (is (not (.-inWater (.self p)))))))))

(def step-args "Shore swim and go-to look 2 blocks out; no leg fits the pond." small-args)

(defn walled-pond
  "Water y 64 and 65 over a stone floor at y 63, x and z within 2, a stone ring at distance 3 up to y 66; extra overrides."
  [extra]
  (merge (into {} (for [x (range -2 3) z (range -2 3)
                        [y n] [[63 "stone"] [64 "water"] [65 "water"]]]
                    [(str x "," y "," z) n]))
         (into {} (for [x (range -3 4) z (range -3 4) y (range 63 68)
                        :when (= 3 (max (js/Math.abs x) (js/Math.abs z)))]
                    [(str x "," y "," z) "stone"]))
         extra))

(defn ^:async step-run!
  "One breathe run in the walled pond with inventory and zones; the ending, the ledger and the body."
  [{:keys [inventory zones blocks args] :or {inventory [] zones [] args step-args}}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}} :blocks (walled-pond blocks) :inventory inventory}))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (world-files/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (await (one-run! eng args))
    {:ended (ended seen) :seen seen :p p :data (stopped-data seen)
     :ledger (ledger/open-entries (mem/view (:store eng)))
     :block (fn [cell] (get (:blocks @(fake/state p)) cell))}))

(deftest a-walled-pond-is-left-on-a-placed-block-in-the-ledger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended p ledger block]} (await (step-run! {:inventory [{:name "dirt" :count 4}]}))
              [e & more] ledger
              [x y z] (:cell e)]
          (is (= [[:completed nil]] ended))
          (is (empty? more) "one step block")
          (is (= [:breathe-step :placed "dirt"] ((juxt :purpose :state :item) e)))
          (is (= "dirt" (block [x y z])))
          (is (< (max (js/Math.abs x) (js/Math.abs z)) 3) "inside the wall ring")
          (is (not (.-inWater (.self p))))
          (is (= (inc y) (js/Math.floor (:y (core/self-pos p)))) "standing on the block"))))))

(deftest a-walled-pond-without-blocks-stops-no_blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended ledger data]} (await (step-run! {}))]
          (is (= [[:stopped :no-blocks]] ended))
          (is (= :no-blocks (:step data)))
          (is (empty? ledger)))))))

(deftest every-step-cell-in-a-zone-is-refused-and-nothing-is-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended ledger data p]} (await (step-run! {:inventory [{:name "dirt" :count 4}]
                                                              :zones [{:name "keep-out" :min [-3 60 -3] :max [3 70 3]}]}))]
          (is (= [[:stopped :no-land-in-range]] ended))
          (is (= :refused (:step data)))
          (is (empty? ledger))
          (is (empty? (filterv #(= "place" (.-name %)) (array-seq (.. p -world -calls))))))))))

(deftest reachable-land-in-range-never-places-a-step-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended ledger]} (await (step-run! {:inventory [{:name "dirt" :count 4}]
                                                       :blocks {"3,66,0" "air" "3,67,0" "air"}}))]
          (is (= [[:completed nil]] ended))
          (is (empty? ledger)))))))

(deftest a-cell-under-an-overhang-is-never-a-step-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [roof (into {} (for [x (range 0 3) z (range -2 3)] [(str x ",67," z) "stone"]))
              {:keys [ended ledger]} (await (step-run! {:inventory [{:name "dirt" :count 4}] :blocks roof}))
              [x] (:cell (first ledger))]
          (is (= [[:completed nil]] ended))
          (is (< x 0) "only a cell with two air above is chosen"))))))

;; ---------------------------------------------------------------- drowning under a ceiling: go-to air, dig a natural cap

(def cap-args "One column out is searched, so the open fake world beyond the walls is not a target." (tune small-args :radius 1 :air-radius 1))

(defn capped-world
  "The walled water column capped at y 68 by block, stone beside it up to y 67 with the +x top as land."
  [block]
  (merge water-column walls {"0,68,0" block}
         (into {} (for [y (range 64 68)] [(str "1," y ",0") "stone"]))))

(defn ^:async cap-run!
  "One breathe run in the capped column; the ending, the fake body, its events and the block reader."
  [{:keys [block zones args dig-ms inventory] :or {block "dirt" zones [] args cap-args dig-ms 0 inventory []}}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake {:self {:inWater true :oxygen 4} :blocks (capped-world block) :inventory inventory}))
        _ (swap! (fake/state p) assoc :dig-ms dig-ms)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (world-files/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (await (one-run! eng args))
    {:ended (ended seen) :seen seen :p p :data (stopped-data seen)
     :block (fn [cell] (.-name (.blockAt p (apply tu/pos cell))))}))

(deftest drowning-under-a-ceiling-goes-to-air-further-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tunnel (merge (into {} (for [x (range -9 19) z (range -8 9) y [64 65 66]] [(str x "," y "," z) "stone"]))
                            (into {} (for [x (range 0 10) y [64 65]] [(str x "," y ",0") "water"]))
                            {"6,66,0" "water"})
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}} :blocks tunnel})]
          (await (one-run! eng defaults))
          (is (= [[:completed nil]] (ended seen)))
          (is (>= (:y (core/self-pos p)) 67) "out through the pocket, on top of the ceiling")
          (is (not (.-inWater (.self p)))))))))

(deftest drowning-in-a-column-capped-by-dirt-digs-up-and-gets-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended block p]} (await (cap-run! {}))]
          (is (= [[:completed nil]] ended))
          (is (= "air" (block [0 68 0])) "the cap is dug")
          (is (not (.-inWater (.self p)))))))))

(deftest drowning-far-below-a-cap-swims-up-to-it-then-digs-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deep (merge (into {} (for [y (range 64 73)] [(str "0," y ",0") "water"]))
                          (into {} (for [y (range 64 73) [x z] [[-1 0] [0 1] [0 -1] [1 0]]] [(str x "," y "," z) "stone"]))
                          {"0,73,0" "dirt"})
              clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/seeing-all (tu/fake {:self {:inWater true :oxygen 4} :blocks deep}))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :world (world-files/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (swap! (fake/state p) assoc :dig-ms 0)
          (await (one-run! eng cap-args))
          (is (= [[:completed nil]] (ended seen)) (pr-str (stopped-data seen)))
          (is (= "air" (.-name (.blockAt p (apply tu/pos [0 73 0])))) "the cap is dug once the body is under it"))))))

(deftest a-swim-that-rises-and-stops-under-a-cap-digs-it-in-the-same-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deep (merge (into {} (for [y (range 64 73)] [(str "0," y ",0") "water"]))
                          (into {} (for [y (range 64 73) [x z] [[-1 0] [0 1] [0 -1] [1 0]]] [(str x "," y "," z) "stone"]))
                          {"0,73,0" "dirt"})
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks deep})]
          (swap! (fake/state p) assoc :dig-ms 0)
          (await (one-run! eng cap-args))
          (is (= [[:completed nil]] (ended seen)) (pr-str (stopped-data seen)))
          (is (= "air" (.-name (.blockAt p (tu/pos 0 73 0)))) "the cap is dug")
          (is (= 1 (count (filter #{"swim"} (call-names p)))) (pr-str (call-names p))))))))

(defn ^:async never-dug!
  "A cap of block: stopped :no-air with :cap why, the block still there, no dig call."
  [done block why]
  (tu/run-async done
    (fn ^:async t []
      (let [{:keys [ended data p] at :block} (await (cap-run! {:block block}))]
        (is (= [[:stopped :no-air]] ended))
        (is (= why (:cap data)))
        (is (= 1 (:air-radius data)) "the stop says how far it looked")
        (is (= block (at [0 68 0])))
        (is (not (some #{"dig"} (call-names p))))))))

(deftest a-cap-that-is-not-natural-is-never-dug
  (async done (never-dug! done "oak_planks" :not-natural)))

(deftest a-protected-cap-is-never-dug
  (async done (never-dug! done "chest" :protected)))

(deftest a-cap-in-another-zone-is-dug-as-a-last-resort-with-a-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended block seen]} (await (cap-run! {:zones [{:name "keep-out" :min [0 68 0] :max [0 68 0]}]}))]
          (is (= [[:completed nil]] ended))
          (is (= "air" (block [0 68 0])))
          (is (= 1 (count (of-kind seen :breathe.trespass-last-resort)))))))))

;; ---------------------------------------------------------------- sensing: only seen cells are targets, the cap is read by digging

(defn stub-seen!
  "Gives the fake body a perception: seen? (cell vector -> bool) cells read as the world holds them, the rest as never seen."
  [p seen?]
  (aset p "sensedAt"
        (fn [a]
          (let [cell [(.-x a) (.-y a) (.-z a)]
                b (.blockAt p a)]
            (if (seen? cell)
              b
              #js {:unknown true :name (.-name b)})))))

(def pocket-tunnel
  "A stone block with a water tunnel at y 64..65 from x 0 to 9 and a one-cell shaft to air at x 6; the own column has a stone cap at y 66."
  (merge (into {} (for [x (range -9 19) z (range -8 9) y [64 65 66]] [(str x "," y "," z) "stone"]))
         (into {} (for [x (range 0 10) y [64 65]] [(str x "," y ",0") "water"]))
         {"6,66,0" "water"}))

(defn ^:async pocket-run!
  "One breathe run in pocket-tunnel with seen? (cell -> bool) stubbed; the ending, the body, its reader and the dig count."
  [seen? blocks]
  (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}} :blocks blocks})]
    (stub-seen! p seen?)
    (await (one-run! eng defaults))
    {:ended (ended seen) :p p :data (stopped-data seen)
     :digs (count (filter #{"dig"} (call-names p)))
     :block (fn [cell] (.-name (.blockAt p (apply tu/pos cell))))}))

(deftest an-air-pocket-behind-stone-that-was-never-seen-is-not-a-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended p block]} (await (pocket-run! (fn [[x]] (zero? x)) pocket-tunnel))]
          (is (= [[:completed nil]] ended))
          (is (= "air" (block [0 66 0])) "dug up through the own cap instead")
          (is (< (:x (core/self-pos p)) 3) "never went to the unseen pocket"))))))

(deftest an-air-pocket-that-was-seen-is-a-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended p block]} (await (pocket-run! (constantly true) pocket-tunnel))]
          (is (= [[:completed nil]] ended))
          (is (= "stone" (block [0 66 0])) "no dig")
          (is (>= (:x (core/self-pos p)) 5) "went to the pocket"))))))

(deftest a-seen-pocket-that-go-to-cannot-reach-falls-back-to-the-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walled (assoc pocket-tunnel "3,64,0" "stone" "3,65,0" "stone")
              {:keys [ended block]} (await (pocket-run! (constantly true) walled))]
          (is (= [[:completed nil]] ended))
          (is (= "air" (block [0 66 0])) "dug the cap once the walk failed"))))))

(defn ^:async refused-dig-run!
  "A dirt cap over a thick stack whose dig reports dug but changes nothing, so the body keeps drowning."
  []
  (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (capped-world "dirt")})]
    (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "dug"}))
    (await (one-run! eng cap-args))
    {:ended (ended seen) :data (stopped-data seen) :digs (count (filter #{"dig"} (call-names p)))}))

(deftest a-cap-that-keeps-drowning-the-body-stops-after-three-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended data digs]} (await (refused-dig-run!))]
          (is (= [[:stopped :no-air]] ended))
          (is (= :dig-failed (:cap data)))
          (is (= 3 digs)))))))

;; ---------------------------------------------------------------- the cap dig fits the air left; seen air below

(deftest a-cap-dig-slower-than-the-air-left-is-never-started
  ;; oxygen 4 and health 20 leave 13 s; a floating dig under water is 25 times slower than on land
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended data p] at :block} (await (cap-run! {:block "stone" :dig-ms 60000}))]
          (is (= [[:stopped :no-air]] ended))
          (is (= :too-slow (:cap data)))
          (is (= "stone" (at [0 68 0])))
          (is (not (some #{"dig"} (call-names p)))))))))

(defn slow-cap-run!
  "A drowning body under a stone cap it cannot dig in time; the column floor is floor, a still pool with a stone floor
  lies beside it at x 1."
  [floor level]
  (let [world (merge (capped-world "stone") {"0,63,0" floor "1,64,0" "water" "1,65,0" "water" "1,63,0" "stone"})
        {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0.5 :y 66 :z 0.5}} :blocks world
                                     :states (when level {"0,64,0" {:level level} "0,65,0" {:level level} "0,66,0" {:level level} "0,67,0" {:level level}})})]
    (swap! (fake/state p) assoc :dig-ms 60000)
    (-> (one-run! eng cap-args) (.then (fn [_] {:p p :ended (ended seen)})))))

(deftest a-body-too-slow-for-the-cap-moves-off-a-lava-floor-to-a-still-pool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (slow-cap-run! "lava" nil))]
          (is (<= 1 (:x (core/self-pos p))) "moved to the pool"))))))

(deftest a-body-too-slow-for-the-cap-moves-out-of-flowing-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (slow-cap-run! "stone" 1))]
          (is (<= 1 (:x (core/self-pos p))) "moved to the pool"))))))

(deftest a-body-too-slow-for-the-cap-stays-over-a-still-stone-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p ended]} (await (slow-cap-run! "stone" nil))]
          (is (= [[:stopped :no-air]] ended))
          (is (< (:x (core/self-pos p)) 1) "no better spot, stays"))))))

(deftest a-cap-dig-holds-the-best-tool-and-is-bounded-by-the-air-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended p]} (await (cap-run! {:block "stone" :dig-ms 1000 :inventory [{:name "stone_pickaxe" :count 1}]}))
              calls (array-seq (.. p -world -calls))
              dig (first (filter #(= "dig" (.-name %)) calls))
              before-dig (take-while #(not= "dig" (.-name %)) calls)]
          (is (= [[:completed nil]] ended))
          (is (some #(and (= "equip" (.-name %)) (= "stone_pickaxe" (.. % -args -item))) before-dig) "the pickaxe is held first")
          (is (< 1 (.. dig -args -timeoutS) 13) "the dig gives up within the air left"))))))

(def flooded-shaft
  "A stone block: a water shaft at x 0 from y 64 to 73 capped by stone at y 74, and at its foot a tunnel with air
  from x 1 to 4 (y 64 and 65), the way the body came."
  (merge (into {} (for [x (range -10 12) z (range -10 11) y (range 63 76)] [(str x "," y "," z) "stone"]))
         (into {} (for [y (range 64 74)] [(str "0," y ",0") "water"]))
         (into {} (for [x (range 1 5) y [64 65]] [(str x "," y ",0") "air"]))))

(deftest drowning-under-a-slow-cap-goes-back-down-to-seen-air
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 72 :z 0}} :blocks flooded-shaft})]
          (swap! (fake/state p) assoc :dig-ms 60000)
          (await (one-run! eng defaults))
          (is (= [[:completed nil]] (ended seen)))
          (is (= "stone" (.-name (.blockAt p (tu/pos 0 74 0)))) "the cap is left")
          (is (<= 1 (:x (core/self-pos p))) "in the tunnel")
          (is (> 66 (:y (core/self-pos p))) "down at its floor"))))))

(def shaft-top {:x 0 :y 72 :z 0})

(deftest a-seen-pocket-joined-to-the-body-by-water-is-a-target-with-its-swim-length
  (let [p (:p (setup {:self {:inWater true :oxygen 4 :pos shaft-top} :blocks flooded-shaft}))
        {:keys [cell steps]} (breath/nearest-pocket p shaft-top 8 24 nil)]
    (is (= {:x 1 :y 64 :z 0} (select-keys cell [:x :y :z])))
    (is (<= 8 steps 12) "down the shaft and one across")))

(deftest a-seen-pocket-walled-off-from-the-body-is-no-target
  (let [walled (assoc flooded-shaft "1,64,0" "stone" "1,65,0" "stone")
        p (:p (setup {:self {:inWater true :oxygen 4 :pos shaft-top} :blocks walled}))]
    (is (nil? (breath/nearest-pocket p shaft-top 8 24 nil)))))

(deftest a-cheap-cap-dig-comes-before-a-swim-down-to-a-pocket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4 :pos shaft-top} :blocks flooded-shaft})]
          (swap! (fake/state p) assoc :dig-ms 100)
          (await (one-run! eng defaults))
          (is (= "air" (.-name (.blockAt p (tu/pos 0 74 0)))) "the cap is dug")
          (is (<= 72 (:y (core/self-pos p))) "not down in the tunnel"))))))

(deftest a-two-cell-cap-is-dug-one-cell-and-the-air-over-it-is-not-peeked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (:p (setup {:self {:inWater true :oxygen 4}
                              :blocks (merge (capped-world "dirt") {"0,69,0" "dirt" "0,70,0" "air"})}))
              head {:x 0 :y 65 :z 0}]
          (is (= {:cells [{:x 0 :y 68 :z 0}]} (air/cap-above p head)))
          (is (= {:cells [{:x 0 :y 68 :z 0}]}
                 (air/cap-above (:p (setup {:blocks (merge (capped-world "dirt") {"0,69,0" "dirt" "0,70,0" "dirt"})})) head))
              "the same whatever lies over the cap"))))))

(deftest a-sand-cap-is-never-dug-it-would-fall-into-the-water
  (async done (never-dug! done "sand" :gravity)))

(deftest a-cap-cell-never-seen-is-unseen-not-read
  (let [p (:p (setup {:self {:inWater true :oxygen 4} :blocks (capped-world "dirt")}))]
    (stub-seen! p (fn [[_ y]] (< y 68)))
    (is (= {:why :unseen} (air/cap-above p {:x 0 :y 65 :z 0})))))

(deftest drowning-under-a-cap-looks-up-before-digging-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (capped-world "dirt")})
              looked (atom false)]
          (stub-seen! p (fn [[_ y]] (or @looked (< y 68))))
          (.override (.-world p) "look" (fn [_ _ _] (reset! looked true) #js {:status "ok"}))
          (await (one-run! eng cap-args))
          (is @looked)
          (is (= "air" (.-name (.blockAt p (tu/pos 0 68 0)))) "dug once seen")
          (is (= [[:completed nil]] (ended seen))))))))

;; ---------------------------------------------------------------- sensing: look up the own column before the air search

(deftest drowning-in-a-dark-column-looks-up-and-swims-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (merge water-column walls)})
              looked (atom false)]
          (stub-seen! p (fn [[_ y]] (or @looked (<= y 65))))
          (.override (.-world p) "look" (fn [_ _ _] (reset! looked true) #js {:status "ok"}))
          (await (one-run! eng cap-args))
          (is @looked)
          (is (some #{"swim"} (call-names p)) "swam up the lit-by-looking column")
          (is (= [[:completed nil]] (ended seen))))))))

(deftest drowning-in-a-column-that-stays-unseen-swims-up-anyway
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (merge water-column walls (into {} (for [y (range 64 68)] [(str "1," y ",0") "stone"])))})]
          (stub-seen! p (fn [[_ y]] (<= y 65)))
          (await (one-run! eng cap-args))
          (is (some #{"swim"} (call-names p)) "an unseen column is swum up, as a player does")
          (is (= [[:stopped :no-land-in-range]] (ended seen)) "surfaced, then no land in the sealed world: not :no-air"))))))

(deftest a-seen-cap-is-not-swum-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (capped-world "obsidian")})]
          (await (one-run! eng cap-args))
          (is (not-any? #{"swim"} (call-names p)) "a seen cap blocks the swim fallback")
          (is (= [[:stopped :no-air]] (ended seen))))))))

(deftest step-cell-and-leg-target-skip-cells-never-seen
  (let [world (merge {"0,64,0" "water" "0,65,0" "air" "0,66,0" "air" "1,64,0" "water" "1,65,0" "air" "1,66,0" "air"
                      "1,63,0" "stone"}
                     (into {} (for [x (range 8 20)] [(str x ",64,0") "water"])))
        p (:p (setup {:self {:inWater true :oxygen 20} :blocks world}))
        pos {:x 0 :y 64 :z 0}]
    (is (some? (b/step-cell p pos 2 nil)) "seen: a step cell is found")
    (stub-seen! p (constantly false))
    (is (nil? (b/step-cell p pos 2 nil)) "never seen: none")
    (is (nil? (settings/with-settings {:jobs.survival.breathe/leg-length 12} #(b/leg-target p pos pos [1 0]))))))

;; ---------------------------------------------------------------- an open own column is swum first; unseen cells are not read

(deftest a-dark-open-own-column-is-swum-without-looking-at-other-columns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (capped-world "air")})
              looks (atom [])]
          (stub-seen! p (fn [[x y z]] (and (zero? x) (zero? z) (<= y 65))))
          (.override (.-world p) "look" (fn [_ a _]
                                          (swap! looks conj [[(js/Math.floor (.. a -pos -x)) (js/Math.floor (.. a -pos -z))]
                                                             (boolean (some #{"swim"} (call-names p)))])
                                          #js {:status "ok"}))
          (await (one-run! eng (tune small-args :radius 2 :air-radius 2)))
          (is (some #{"swim"} (call-names p)) "swam up")
          (is (every? #{[0 0]} (map first (remove second @looks))) "before the swim up only the own column was looked at (after it the land search may look)"))))))

(deftest a-blocked-own-column-looks-at-the-neighbour-columns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4} :blocks (capped-world "obsidian")})
              looks (atom [])]
          (stub-seen! p (fn [[x y z]] (and (zero? x) (zero? z) (or (<= y 65) (= y 68)))))
          (.override (.-world p) "look" (fn [_ a _]
                                          (swap! looks conj [(js/Math.floor (.. a -pos -x)) (js/Math.floor (.. a -pos -z))])
                                          #js {:status "ok"}))
          (await (one-run! eng (tune defaults :radius 2 :air-radius 1)))
          (is (some #(not= [0 0] %) @looks) "a seen cap sends the look sideways"))))))

(deftest a-kelp-column-is-swum-up-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (capped-world "air") {"0,65,0" "kelp_plant" "0,66,0" "seagrass" "0,67,0" "kelp"})
              {:keys [eng p]} (setup {:self {:inWater true :oxygen 4} :blocks blocks})]
          (await (one-run! eng cap-args))
          (is (some #{"swim"} (call-names p)) "swam up through the plants"))))))

(deftest a-bubble-column-is-water-for-the-air-search
  (let [blocks (merge water-column {"0,65,0" "bubble_column" "0,66,0" "tall_seagrass"})
        p (:p (setup {:self {:inWater true :oxygen 4} :blocks blocks}))]
    (is (= {:x 0 :y 67 :z 0} (breath/surface-in-column p 0 0 64 10)))))

(deftest unseen-cells-are-not-side-land-or-shore-cells
  (let [world {"1,64,0" "air" "1,65,0" "air" "1,63,0" "stone"
               "0,64,0" "water" "0,65,0" "air" "0,63,0" "water" "0,62,0" "water"}
        {:keys [p]} (setup {:self {:inWater true :oxygen 20 :pos {:x 0.5 :y 64 :z 0.5}} :blocks world})
        self (.self p)
        cell {:x 1 :y 64 :z 0}
        shore {:x 1 :y 65 :z 0}]
    (is (some? (breath/side-cell p self)) "seen: a side cell")
    (is (shore/standable? p cell) "seen: land")
    (is (some? (b/shore-cell? p shore)) "seen: shore")
    (stub-seen! p (constantly false))
    (is (nil? (breath/side-cell p self)) "unseen: no side cell")
    (is (not (shore/standable? p cell)) "unseen: no land")
    (is (not (b/shore-cell? p shore)) "unseen: no shore")))

(defn neighbour-column
  "water-column capped over the body, and an adjacent +x column of bubble_column cells (drag as given) open to air."
  [drag]
  {:self {:inWater true :oxygen 4}
   :blocks (merge water-column walls {"0,68,0" "stone"}
                  (into {} (for [y (range 64 68)] [(str "1," y ",0") "bubble_column"])))
   :states (into {} (for [y (range 64 68)] [(str "1," y ",0") {:drag drag}]))})

(deftest a-down-bubble-column-is-never-swum-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (neighbour-column true))]
          (await (one-run! eng side-args))
          (is (not-any? #(= 1 (some-> % .-args .-pos .-x)) (array-seq (.. p -world -calls))) "no step into the dragging column")
          (is (not= 1 (js/Math.floor (:x (core/self-pos p))))))))))

(deftest an-up-bubble-column-is-swum-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (neighbour-column false))]
          (await (one-run! eng side-args))
          (is (= ["moveTo"] (call-names p)))
          (is (= 1 (js/Math.floor (:x (core/self-pos p))))))))))

(deftest a-column-deeper-than-one-swim-is-swum-to-the-top
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [top 113
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (into {} (concat (for [y (range 64 (inc top))] [(str "0," y ",0") "water"])
                                                                    (for [y (range 64 (inc top)) x (range -2 3) z (range -2 3) :when (not= 0 x z)]
                                                                      [(str x "," y "," z) "stone"])))})]
          (.override (.-world p) "swim"
                     (fn ^:async f [_ _ _]
                       (fake/swap-self! p (fn [s] (update-in s [:pos 1] #(min top (+ % 10)))))
                       (if (= top (js/Math.floor (:y (core/self-pos p))))
                         (do (set-self! p {"oxygen" 20}) #js {:status "surfaced"})
                         #js {:status "timeout"})))
          (await (one-run! eng defaults))
          (is (<= top (js/Math.floor (:y (core/self-pos p)))) "swam to the top")
          (is (not-any? #{[:stopped :no-air]} (ended seen)) "a swim that rose is not a failed try")
          (is (>= (count (filter #{"swim"} (call-names p))) 5)))))))

(defn crossing-world
  "Stone y 64..67 around; water at x 0 capped at y 68, a bubble column (drag as given) at x 1, and water open to air at x 2."
  [drag]
  (let [column (fn [x] (for [y (range 64 68)] (str x "," y ",0")))]
    {:self {:inWater true :oxygen 4}
     :blocks (merge (into {} (for [x (range -2 4) y (range 64 68) z (range -2 3)] [(str x "," y "," z) "stone"]))
                    {"0,68,0" "stone"}
                    (into {} (for [c (column 0)] [c "water"]))
                    (into {} (for [c (column 1)] [c "bubble_column"]))
                    (into {} (for [c (column 2)] [c "water"])))
     :states (into {} (for [c (column 1)] [c {:drag drag}]))}))

(deftest a-down-bubble-column-between-the-body-and-the-air-is-never-crossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (crossing-world true))]
          (await (one-run! eng (tune defaults :radius 2 :air-radius 1)))
          (is (not-any? #(some-> % .-args .-pos .-x (>= 1)) (array-seq (.. p -world -calls))) "no move past x=0")
          (is (> 1 (js/Math.floor (:x (core/self-pos p))))))))))

(deftest side-cell-refuses-a-down-bubble-column-in-feet-head-or-below
  (doseq [[cells drag? expect?] [[["1,64,0" "1,65,0"] true false]
                                 [["1,64,0" "1,65,0"] false true]
                                 [["1,63,0"] true false]]]
    (let [world {:blocks (merge {"0,65,0" "stone" "1,63,0" "stone"} (into {} (for [c cells] [c "bubble_column"])))
                 :states (into {} (for [c cells] [c {:drag drag?}]))}
          {:keys [p]} (setup (assoc world :self {:inWater true :oxygen 20 :pos {:x 0.5 :y 64 :z 0.5}}))]
      (is (= expect? (some? (breath/side-cell p (.self p)))) (str cells " drag " drag?)))))

(deftest side-cell-never-stands-on-an-upward-bubble-column-or-a-water-plant
  (doseq [below ["bubble_column" "kelp" "seagrass"]]
    (let [world {:blocks {"0,65,0" "stone" "1,63,0" below} :states {"1,63,0" {:drag false}}}
          {:keys [p]} (setup (assoc world :self {:inWater true :oxygen 20 :pos {:x 0.5 :y 64 :z 0.5}}))]
      (is (nil? (breath/side-cell p (.self p))) below))))

(deftest a-swim-is-progress-only-above-the-best-height-reached
  (is (swim/rose? nil 64 65) "first rise")
  (is (not (swim/rose? 65 64 65)) "pushed back down, back to the best: no progress")
  (is (swim/rose? 65 64 66) "above the best")
  (is (swim/rose? 65 66 67) "carried up between passes, then a block more")
  (is (not (swim/rose? nil 64 64)) "no rise"))

(defn drag-tunnel
  "A water tunnel x 0..9, z -1..1 (feet y 64, head y 65) under a stone ceiling at y 66, a glass cap over the body (never
  dug), a dragging column across the middle row at x 3, and the only opening at (6, 66, 0)."
  []
  (merge (into {} (for [x (range -9 19) z (range -8 9) y [64 65 66]] [(str x "," y "," z) "stone"]))
         (into {} (for [x (range 0 10) z [-1 0 1] y [64 65]] [(str x "," y "," z) "water"]))
         {"0,66,0" "glass" "6,66,0" "water" "3,63,0" "magma_block" "3,64,0" "bubble_column" "3,65,0" "bubble_column"}))

(deftest air-behind-a-drag-column-is-gone-to-round-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}} :blocks (drag-tunnel)
                                           :states {"3,64,0" {:drag true} "3,65,0" {:drag true}}})]
          (await (one-run! eng defaults))
          (is (not-any? #{[:stopped :no-air]} (ended seen)) "the go-to leg routes round the column")
          (is (= [[:completed nil]] (ended seen)))
          (is (not (.-inWater (.self p)))))))))

(defn drag-corner
  "Water at x 0 capped at y 68, stone at (0, z 1), a dragging column at (1, z 0) and water open to air at (1, z 1): the
  straight walk to (1, 1) brushes the dragging column's corner."
  []
  (let [column (fn [x z] (for [y (range 64 68)] (str x "," y "," z)))]
    {:self {:inWater true :oxygen 4}
     :blocks (merge (into {} (for [x (range -2 4) y (range 64 68) z (range -2 3)] [(str x "," y "," z) "stone"]))
                    {"0,68,0" "stone" "1,63,0" "magma_block"}
                    (into {} (for [c (column 0 0)] [c "water"]))
                    (into {} (for [c (column 1 0)] [c "bubble_column"]))
                    (into {} (for [c (column 1 1)] [c "water"])))
     :states (into {} (for [c (column 1 0)] [c {:drag true}]))}))

(deftest a-diagonal-step-past-a-drag-column-corner-is-never-a-raw-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (drag-corner))]
          (is (breath/drag-between? p 0 0 1 1 64) "the corner cell counts")
          (await (one-run! eng (tune defaults :radius 2 :air-radius 1)))
          (is (not-any? #(and (= "moveTo" (.-name %)) (= [1 64 1] (some-> % .-args .-pos (as-> q [(.-x q) (.-y q) (.-z q)]))))
                        (array-seq (.. p -world -calls)))
              "no raw moveTo past the corner"))))))

(deftest drag-between-checks-both-cells-beside-each-diagonal-step
  (let [{:keys [p]} (setup {:blocks {"1,64,0" "bubble_column"} :states {"1,64,0" {:drag true}}})]
    (is (breath/drag-between? p 0 0 2 1 64) "(0,0) to (2,1) passes the corner (1,0)")
    (is (breath/drag-between? p 0 0 1 1 64))
    (is (not (breath/drag-between? p 0 0 0 2 64)))))

(deftest a-second-drowning-in-the-same-run-counts-rises-afresh
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [top 80
              n (atom 0)
              pulled (atom false)
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (into {} (concat (for [y (range 64 (inc top))] [(str "0," y ",0") "water"])
                                                                    (for [y (range 64 (inc top)) x (range -2 3) z (range -2 3) :when (not= 0 x z)]
                                                                      [(str x "," y "," z) "stone"])))})]
          ;; the first swim surfaces at the top; the first swim toward the shore ends with the body pulled back to the
          ;; floor, drowning again; later swims up rise 3 blocks each (timed out) until the top
          (.override (.-world p) "swim"
                     (fn ^:async f [token a impl]
                       (let [k (swap! n inc)
                             y #(js/Math.floor (:y (core/self-pos p)))]
                         (cond
                           (= 1 k) (do (fake/swap-self! p (fn [s] (assoc-in s [:pos 1] top))) (set-self! p {"oxygen" 20})
                                       #js {:status "surfaced"})
                           (and (some-> a .-toward) (not @pulled))
                           (do (reset! pulled true) (fake/swap-self! p (fn [s] (assoc-in s [:pos 1] 64))) (set-self! p {"oxygen" 4})
                               #js {:status "timeout"})
                           (some-> a .-toward) (await (impl token a))
                           :else (do (fake/swap-self! p (fn [s] (update-in s [:pos 1] #(min top (+ % 3)))))
                                     (if (= top (y))
                                       (do (set-self! p {"oxygen" 20}) #js {:status "surfaced"})
                                       #js {:status "timeout"}))))))
          (await (one-run! eng (tune defaults :reach 20)))
          (is @pulled "drowned a second time")
          (is (not-any? #{[:stopped :no-air]} (ended seen)) "rises below the first surfacing are progress")
          (is (< 4 @n) "swam up again"))))))

(deftest a-bank-never-seen-is-looked-for-before-the-run-stops-afloat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -30 19 8) (bank 20 24 8))})
              looks (atom [])]
          ;; a look shows what lies within 45 degrees of its direction (a flat view cone), the rest stays unseen
          (stub-seen! p (fn [[x _ z]]
                          (or (<= (js/Math.abs x) 4)
                              (some (fn [[lx lz]] (let [d (js/Math.hypot lx lz)]
                                                    (> (+ (* x lx) (* z lz)) (* 0.7 d (js/Math.hypot x z)))))
                                    @looks))))
          (.override (.-world p) "look" (fn [_ a _]
                                          (swap! looks conj [(- (.. a -pos -x) 0.5) (- (.. a -pos -z) 0.5)])
                                          #js {:status "ok"}))
          (await (one-run! eng small-args))
          (is (seq @looks) "looked before giving up")
          (is (some (fn [[lx lz]] (and (pos? lx) (< (js/Math.abs lz) lx))) @looks) "one look faced the east bank")
          (is (= [[:completed nil]] (ended seen)))
          (is (>= (:x (core/self-pos p)) 20) "on the east bank"))))))

(deftest an-unseen-ledge-behind-a-high-wall-is-looked-at-and-the-ledge-is-swum-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [y (range 64 68) z [-1 0 1]] [(str "-2," y "," z) "stone"]))
              {:keys [eng p]} (setup {:self {:inWater true :oxygen 4}
                                      :blocks (merge pool wall
                                                     {"-3,64,0" "stone" "-3,63,0" "stone"
                                                      "2,64,0" "water" "2,65,0" "water" "3,64,0" "water" "3,65,0" "water"
                                                      "4,64,0" "stone" "4,63,0" "stone"})})
              looked (atom false)]
          (stub-seen! p (fn [[x]] (or @looked (<= (js/Math.abs x) 1))))
          (.override (.-world p) "look" (fn [_ _ _] (reset! looked true) #js {:status "ok"}))
          (await (one-run! eng defaults))
          (is @looked)
          (is (= {:x 4 :y 65 :z 0} (core/self-pos p)) "the ledge, not the wall"))))))

;; ---------------------------------------------------------------- no air seen: a dry pocket, a footing, the cap as a last resort

(defn flooded-cut
  "Solid stone with a flooded 1-wide cut: water in the own column from y 64 up to top, cap at top + 1, no air anywhere."
  [top cap]
  (merge (into {} (for [x (range -3 4) z (range -3 4) y (range 62 (+ top 3))] [(str x "," y "," z) "stone"]))
         (into {} (for [y (range 64 (inc top))] [(str "0," y ",0") "water"]))
         {(str "0," (inc top) ",0") cap}))

(def kit [{:name "cobblestone" :count 64} {:name "iron_pickaxe" :count 1}])

(defn ^:async cut-run!
  "One breathe run of a body at feet y fy in (flooded-cut top cap) carrying inventory, a dig taking 1.5 s."
  [top cap fy inventory]
  (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y fy :z 0}} :blocks (flooded-cut top cap)
                                     :inventory inventory})]
    (swap! (fake/state p) assoc :dig-ms 1500)
    (await (one-run! eng cap-args))
    {:ended (ended seen) :p p :eng eng :data (stopped-data seen) :pos (core/self-pos p)
     :block (fn [cell] (.-name (.blockAt p (apply tu/pos cell))))}))

(deftest a-flooded-cut-under-deep-water-is-left-for-a-dry-pocket-in-the-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended p pos block]} (await (cut-run! 82 "stone" 64 kit))]
          (is (= [[:completed nil]] ended))
          (is (nil? (breath/situation p)) "breathing")
          (is (pos? (.-health (.self p))))
          (is (= {:x 2.5 :y 64 :z 0.5} pos) "in the pocket beside the cut, not up the water column")
          (is (= ["cobblestone" "cobblestone"] [(block [1 64 0]) (block [1 65 0])]) "the opening is plugged behind it")
          (is (= "air" (block [2 65 0])) "the head cell is dry")
          (is (not (some #{"swim"} (call-names p)))))))))

(deftest drowning-again-near-a-pocket-just-made-swims-up-instead-of-making-another
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p block]} (await (cut-run! 82 "stone" 64 kit))]
          (set-self! p {:pos [0 66 0] :inWater true :oxygen 4})
          (await (one-run! eng cap-args))
          (is (= "stone" (block [1 66 0])) "no second pocket")
          (is (= 82 (:y (core/self-pos p))) "up the column"))))))

(deftest a-pocket-needs-a-carried-block-to-plug-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p block]} (await (cut-run! 82 "stone" 64 [{:name "iron_pickaxe" :count 1}]))]
          (is (= "stone" (block [1 64 0])) "no pocket is dug")
          (is (some #{"swim"} (call-names p)) "the column is swum up as before"))))))

(deftest afloat-under-a-cap-it-may-not-dig-it-stands-on-a-placed-block-and-makes-the-pocket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended p pos block]} (await (cut-run! 72 "oak_planks" 70 kit))]
          (is (= [[:completed nil]] ended))
          (is (nil? (breath/situation p)) "breathing")
          (is (= "cobblestone" (block [0 69 0])) "the footing under the feet")
          (is (= {:x 2.5 :y 70 :z 0.5} pos) "in the pocket")
          (is (= "oak_planks" (block [0 73 0])) "the cap is left"))))))

(deftest a-stop-with-no-air-says-why-no-pocket-was-made
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended data]} (await (cut-run! 72 "oak_planks" 70 []))]
          (is (= [[:stopped :no-air]] ended))
          (is (= [:not-natural :no-block] [(:cap data) (:pocket data)]))
          (is (re-find #"pocket: no-block.*cap: not-natural" (:text data))))))))

(deftest a-cap-dig-over-its-share-of-the-air-is-started-when-nothing-else-is-left
  ;; oxygen 4 and health 20 leave 13 s: 9 s is over the share (6.5 s) and within the air left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [ended] at :block} (await (cap-run! {:block "stone" :dig-ms 9000}))]
          (is (= [[:completed nil]] ended))
          (is (= "air" (at [0 68 0]))))))))


(deftest breathe-exports-the-contract
  (let [job (get registry/jobs breathe)]
    (is (vector? (:target job)))
    (is (nil? (contract/quote-problem ((:quote-of job) {:primitives (tu/fake {}) :args {} :quote-call {}}))))))

(defn ^:async ending [world args]
  (await (sc/ending-of #(tu/seeing-all (tu/fake world)) (list breathe args))))

(defn glass-shaft
  "A water shaft (feet y 64..67) under a one-cell dirt cap (y 68) with air over it, in stone; a seen air pocket two columns
  east behind a glass window."
  []
  (merge (into {} (for [x (range -9 9) z (range -8 9) y (range 63 72)] [(str x "," y "," z) "stone"]))
         (into {} (for [y (range 64 68)] [(str "0," y ",0") "water"]))
         {"0,68,0" "dirt" "1,64,0" "glass" "1,65,0" "glass" "2,64,0" "air" "2,65,0" "air" "3,64,0" "air" "3,65,0" "air"}
         (into {} (for [y (range 69 72)] [(str "0," y ",0") "air"]))))

(deftest a-body-with-air-is-done-already
  (is (= [:ok :air] (tu/kind-reason (b/done? {:primitives (tu/fake {}) :args defaults})))))

(deftest a-body-that-found-air-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :completed (:event (await (ending {:self {:inWater true :oxygen 4}
                                                  :blocks (merge water-column walls
                                                                 {"0,68,0" "stone"
                                                                  "1,64,0" "water" "1,65,0" "water" "1,66,0" "water" "1,67,0" "water"})}
                                                 side-args)))))))))

(deftest a-pocket-behind-glass-is-not-a-target-the-cap-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}} :blocks (glass-shaft)})]
          (await (one-run! eng defaults))
          (is (not-any? #{[:stopped :no-air]} (ended seen)))
          (is (= "air" (.-name (.blockAt p (tu/pos 0 68 0)))) "the cap is dug"))))))

(deftest a-down-bubble-column-cutting-the-only-way-to-air-is-not-gone-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (-> (crossing-world true) (assoc-in [:blocks "0,68,0"] "oak_planks") (assoc-in [:self :pos] {:x 0 :y 64 :z 0})
                          (update :blocks merge (into {} (for [x (range -2 4) z (range -2 3)] [(str x ",63," z) "stone"])))))]
          (await (one-run! eng (tune defaults :radius 2 :air-radius 4)))
          (is (= [[:stopped :no-air]] (ended seen)))
          (is (> 1 (js/Math.floor (:x (core/self-pos p)))) "never past the dragging column")
          (is (not-any? #{"steer"} (call-names p)) "no go-to child was sent through it"))))))

(deftest the-walk-to-air-may-go-over-the-damage-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen (atom nil)
              c (assoc (stub-child-ctx (contract/ending :done nil {:arrived true})) :call (fn [_ _ args] (reset! seen args) (js/Promise.resolve :continue)))]
          (await (air/go! c :leg {:x 0 :y 64 :z 0} 2))
          (is (= {:health 1} (:floor @seen)) "go-to no longer heals: a drowning body must not end :needs-health"))))))

(deftest a-stop-before-the-cap-was-reached-says-not-tried-not-no-cap
  (is (= {:air-radius 24 :cap :not-tried :pocket :not-tried} (b/air-stop-fields {})))
  (is (= :too-slow (:cap (b/air-stop-fields {:cap :too-slow})))))

(deftest a-swim-up-that-rises-no-higher-while-attacked-is-stalled
  (settings/with-settings {:jobs.survival.breathe.fight/stall-s 4 :jobs.survival.breathe.fight/rise 0.5}
    (fn []
      (are [rise y now expected] (= expected (fight/step-rise rise y now))
        nil 64 1000 [{:y 64 :t 1000} false]
        {:y 64 :t 1000} 65 3000 [{:y 65 :t 3000} false]
        {:y 64 :t 1000} 64.3 4000 [{:y 64 :t 1000} false]
        {:y 64 :t 1000} 64.3 5100 [{:y 64 :t 1000} true]))))

(deftest surfaced-in-a-pool-with-a-mob-hitting-it-the-body-fights-before-it-swims-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:self {:inWater true :oxygen 4}
                                      :entities [{:id 7 :name "zombie" :kind "hostile" :pos {:x 1.5 :y 64 :z 0.5} :health 20}]
                                      :blocks (merge pool {"3,64,0" "stone" "3,63,0" "stone"})})]
          (.override (.-world p) "attack"
                     (fn [token a impl]
                       (swap! clock + 700)
                       (swap! (fake/state p) assoc :entities [])
                       (impl token a)))
          (.emit (.-world p) (clj->js {:kind "hurt" :food 20 :health 17 :amount 3 :attacker {:id 7 :name "zombie"}}))
          (await (one-run! eng {:settings {:jobs.survival.breathe/max-passes 8}}))
          (let [names (call-names p)]
            (is (some #{"attack"} names) (pr-str names))
            (is (< 0 (.indexOf names "attack")) "after the swim up, the body that surfaced fights")))))))

(defn flooded-stair
  "A 1-wide stair up along x from (0,64,0), three cells high per step over stone: water in the first flooded steps, air
  after them, rock all round."
  [flooded steps]
  (into {} (for [x (range -1 (+ steps 2)) y (range 62 (+ 70 steps)) z [-1 0 1]
                 :let [i x
                       open? (and (= z 0) (<= 0 i (dec steps)) (<= (+ 64 i) y (+ 66 i)))
                       floor? (and (= z 0) (<= 0 i (dec steps)) (= y (+ 63 i)))]]
             [(str x "," y "," z) (cond open? (if (< i flooded) "water" "air") floor? "stone" :else "stone")])))

(deftest dry-steps-high-up-a-flooded-stair-shaft-are-land-the-body-can-reach
  (let [p (tu/seeing-all (tu/fake {:self {:inWater true :pos {:x 0.5 :y 64 :z 0.5}} :blocks (flooded-stair 6 9)}))
        pos {:x 0.5 :y 64 :z 0.5}]
    (is (nil? (b/nearest-land p pos 48 nil (constantly true))) "the column search reads only a few cells above the feet")
    (is (= {:x 6 :y 70 :z 0} (b/reachable-land p pos 48 nil)) "the first dry step, found through the flooded ones")
    (is (nil? (b/reachable-land p pos 48 [{:x 6 :y 70 :z 0} {:x 7 :y 71 :z 0} {:x 8 :y 72 :z 0}])) "failed steps are skipped")
    (is (nil? (b/reachable-land p pos 3 nil)) "bounded by the radius")))

(deftest the-unseen-cells-beside-the-water-joined-to-the-body-are-where-a-flooded-stair-goes-on
  (let [p (tu/seeing-all (tu/fake {:self {:inWater true :pos {:x 0.5 :y 64 :z 0.5}} :blocks (flooded-stair 6 9)}))
        seen (tu/sensed-by p)
        hidden? (fn [pos] (and (= 3 (.-x pos)) (= 0 (.-z pos)) (<= 67 (.-y pos) 69)))
        pos {:x 0.5 :y 64 :z 0.5}]
    (is (empty? (breath/unseen-beside-joined p 0 64 0 24 24 4 nil)) "all seen: nothing to look at")
    (tu/set-sensed! p (fn [q] (if (hidden? q) #js {:unknown true :pos q} (seen q))))
    (is (nil? (b/reachable-land p pos 48 nil)) "the stair is cut where it is not seen")
    (is (= [{:x 3 :y 67 :z 0} {:x 3 :y 68 :z 0}] (breath/unseen-beside-joined p 0 64 0 24 24 4 nil)) "the unseen cells touching the water, nearest first, not the one behind them")))

(deftest drowning-at-the-foot-of-a-flooded-stair-swims-up-it-to-the-dry-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0.5 :y 64 :z 0.5}} :blocks (flooded-stair 6 9)})]
          (await (one-run! eng defaults))
          (is (>= (:x (core/self-pos p)) 6) "up the stair, on a dry step")
          (is (not (.-inWater (.self p))))
          (is (= [[:completed nil]] (ended seen))))))))
