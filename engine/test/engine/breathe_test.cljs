(ns engine.breathe-test
  "The suffocating trigger and the breathe job against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [jobs.survival.breathe :as b]
            [engine.fake :as fake]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def breathe 'jobs.survival.breathe)

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn holds? [p args]
  ((:when (get triggers/all :suffocating)) p nil args))

(defn check? [p args]
  ((:check (get registry/jobs breathe)) {:primitives p :args args}))

(def defaults {:min-oxygen 12})

(def water-column
  "Feet at y 64 in water up to y 67; air from y 68."
  {"0,64,0" "water" "0,65,0" "water" "0,66,0" "water" "0,67,0" "water"})

(def walls
  "Stone around the water column, except the +x side, so only air above is open."
  (into {} (for [y (range 64 68) [x z] [[-1 0] [0 1] [0 -1]]] [(str x "," y "," z) "stone"])))

(def cases
  [["drowning: low oxygen, head under water"
    {:self {:inWater true :oxygen 5} :blocks water-column} true]
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

(deftest min-oxygen-is-an-arg
  (let [p (tu/fake {:self {:inWater true :oxygen 15} :blocks water-column})]
    (is (false? (holds? p defaults)))
    (is (true? (holds? p {:min-oxygen 16})))
    (is (true? (check? p {:min-oxygen 16})))))

(deftest suffocating-is-registered-with-breathe
  (let [t (get triggers/all :suffocating)]
    (is (= '(jobs.survival.breathe) (:job t)))
    (is (= 12 (:min-oxygen (:args t))))))

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

(deftest drowning-run-swims-up-holds-afloat-with-no-shore-and-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (await (one-run! eng defaults))
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)) "feet at the top water block, head in air")
          (is (= [] (:list (core/state eng))) "one run")
          (is (= [[:stopped :no_land]] (ended seen)) "afloat with no land is stopped, not completed")
          (is (= (into ["swim"] (repeat b/afloat-holds "steer")) (call-names p))
              "the swim surfaces, then afloat-holds holds keep it up, no walk")
          (is (= [:afloat] (mapv :reason (of-kind seen :holding))) "the hold is declared")
          (is (= 1 (count (of-kind seen :afloat)))))))))

(deftest drowning-with-a-failing-swim-is-stopped-no-air-with-one-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :swimFails true :blocks water-column})]
          (await (one-run! eng defaults))
          (is (= [] (:list (core/state eng))))
          (is (= ["swim" "swim" "swim"] (call-names p)) "three tries in the one run")
          (is (= [[:stopped :no_air]] (ended seen)))
          (is (= 1 (count (of-kind seen :no_air)))))))))

(def side-args (assoc defaults :radius 1))

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
                                         :blocks (merge water-column walls {"0,68,0" "stone" "1,64,0" "stone"})})]
          (await (one-run! eng side-args))
          (is (= [] (:list (core/state eng))))
          (is (= [[:stopped :no_air]] (ended seen)))
          (is (= 1 (count (of-kind seen :no_air)))))))))

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
          (is (= [[:stopped :no_way_out]] (ended seen)))
          (is (= 1 (count (of-kind seen :no_way_out)))))))))

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
          (is (= :done (await (round {:primitives p :args defaults}))))
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

(defn on-hold!
  "Make the nth steer act (1-based) set the body's self fields before it holds."
  [p n fields]
  (let [steers (atom 0)]
    (.override (.-world p) "steer"
               (fn ^:async f [token args impl]
                 (when (= n (swap! steers inc)) (set-self! p fields))
                 (await (impl token args))))))

(deftest a-second-run-afloat-at-the-same-spot-does-not-warn-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (await (one-run! eng defaults))
          (set-self! p {"oxygen" 4 "pos" [0 64 0]})
          (await (one-run! eng defaults))
          (is (= [[:stopped :no_land] [:stopped :no_land]] (ended seen)))
          (is (= 1 (count (of-kind seen :afloat))) "one notice for the spot, however many runs"))))))

(deftest an-afloat-body-that-bobs-out-of-the-water-is-still-afloat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (on-hold! p 2 {"inWater" false "onGround" false})
          (await (one-run! eng defaults))
          (is (= [[:stopped :no_land]] (ended seen)) "a crest above the water does not end the run")
          (is (= b/afloat-holds (count (filter #{"steer"} (call-names p))))))))))

(deftest an-afloat-body-that-stands-on-land-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (assoc water-column "0,63,0" "stone")})]
          (on-hold! p 2 {"inWater" false "onGround" true})
          (await (one-run! eng defaults))
          (is (= [[:completed nil]] (ended seen)))
          (is (= 2 (count (filter #{"steer"} (call-names p)))) "no hold after it stands"))))))

(deftest a-body-pressed-against-a-wall-over-water-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (assoc water-column "0,63,0" "water")})]
          (on-hold! p 2 {"inWater" false "onGround" true})
          (await (one-run! eng defaults))
          (is (= [[:stopped :no_land]] (ended seen)) "water below: not a footing"))))))

(deftest an-afloat-body-that-sinks-swims-up-again-in-the-same-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (.override (.-world p) "steer" (let [steers (atom 0)]
                                           (fn ^:async f [token args impl]
                                             (when (= 1 (swap! steers inc)) (set-self! p {"oxygen" 4 "pos" [0 64 0]}))
                                             (await (impl token args)))))
          (await (one-run! eng defaults))
          (is (= ["swim" "steer" "swim"] (take 3 (call-names p))) "sunk during the first hold: swims up again")
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)))
          (is (= [[:stopped :no_land]] (ended seen))))))))

(deftest hold-presses-jump-until-its-ticks-are-spent-or-the-body-stands-out-of-the-water
  (doseq [[label pose ticks done?] [["in water, first tick" {:onGround false :inWater true} 1 false]
                                    ["in water, last tick" {:onGround false :inWater true} 100 true]
                                    ["crest above the water" {:onGround false :inWater false} 1 false]
                                    ["standing on the bank" {:onGround true :inWater false} 1 true]
                                    ["on the pond floor" {:onGround true :inWater true} 1 false]]]
    (let [decide (b/hold-decider)
          out (last (repeatedly ticks #(decide (clj->js pose))))]
      (is (= done? (some? (.-done out))) label)
      (is (= (not done?) (true? (some-> (.-controls out) .-jump))) label))))

(def far-args (assoc defaults :shore-radius 2 :far-radius 10))

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
  (let [p (tu/fake {:blocks (pond 3)})]
    (doseq [[y expected] [[62 65] [64 65] [65 65] [66 66]]]
      (is (= {:x 0 :y expected :z 0} (b/surface-pos p {:x 0 :y y :z 0} 10)) (str "feet at " y)))))

(defn swim-to-bank!
  "A steer that does what a working walk does, since the fake's walker cannot swim: the body ends on the bank cell
  (7 65 0) and the act's decide function is asked at that pose until it is done."
  [p args]
  (let [pose #js {:x 7.5 :y 65 :z 0.5 :vy 0 :onGround true :onClimbable false :inWater false :collided false :yaw 0 :t 0}
        done (->> (repeatedly #((.-decide args) pose)) (take 200) (some #(.-done %)))]
    (fake/swap-self! p assoc :pos [7 65 0] :inWater false)
    (if done #js {:status "done" :result done} #js {:status "timeout" :pose pose})))

(deftest surfaced-with-a-bank-beyond-the-shore-radius-walks-out-with-the-walk-driver
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (pond 6) (bank 7 9 6))})]
          (.override (.-world p) "steer" (fn [_ args _] (js/Promise.resolve (swim-to-bank! p args))))
          (await (one-run! eng far-args))
          (is (= [] (:list (core/state eng))) "done once out of the water")
          (is (= [[:completed nil]] (ended seen)))
          (is (>= (:x (core/self-pos p)) 7) "stands on the bank")
          (is (not (.-inWater (.self p))))
          (is (= ["steer"] (distinct (rest (call-names p)))) "walked with steer, no moveTo")
          (is (empty? (of-kind seen :afloat))))))))

(deftest surfaced-with-a-wall-all-round-holds-afloat-reports-once-and-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [x (range -7 8) z (range -7 8) y [65 66]
                                  :when (= 7 (max (js/Math.abs x) (js/Math.abs z)))]
                              [(str x "," y "," z) "stone"]))
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (pond 6) (bank 7 7 6) (bank -7 -7 6) wall)})]
          (await (one-run! eng far-args))
          (is (= [[:stopped :no_land]] (ended seen)))
          (is (< (:x (core/self-pos p)) 7) "never left the pond")
          (is (= 1 (count (of-kind seen :afloat)))))))))

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

(deftest surfaced-with-a-failing-shore-swim-warns-no-shore-once-holds-afloat-and-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})]
          (.override (.-world p) "swim"
                     (fn ^:async f [_ args impl]
                       (if (.-toward args) #js {:status "timeout"} (await (impl _ args)))))
          (await (one-run! eng defaults))
          (is (= [[:stopped :no_land]] (ended seen)))
          (is (= 1 (count (of-kind seen :no_shore))))
          (is (empty? (of-kind seen :afloat)) "the no_shore warn is the one notice")
          (is (= b/afloat-holds (count (filter #{"steer"} (call-names p)))))
          (is (not (some #{"moveTo"} (call-names p)))))))))

;; live (card 95752610): a swim toward a walled shore times out with the body on a jump crest against the wall,
;; out of the water for that moment (inWater false, onGround true or false) but over water
(deftest a-failed-shore-swim-ending-on-a-crest-over-water-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [on-ground [false true]]
          (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                             :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})]
            (.override (.-world p) "swim"
                       (fn ^:async f [_ args impl]
                         (if (.-toward args)
                           (do (set-self! p {"inWater" false "onGround" on-ground}) #js {:status "timeout"})
                           (await (impl _ args)))))
            (await (one-run! eng defaults))
            (is (= [[:stopped :no_land]] (ended seen)) (str "not done on a crest, onGround " on-ground))
            (is (= 1 (count (of-kind seen :no_shore))) (str "no_shore once, onGround " on-ground))))))))

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
                         (do (takeover/take! eng {:who "claude" :why "cut"}) #js {:status "timeout"})
                         (await (impl token args)))))
          (await (core/tick! eng))
          (is (= 1 @swims) "the cut round acts no more")
          (is (= {:x 0 :y 64 :z 0} (core/self-pos p)) "still under water")
          (is (empty? (of-kind seen :no_air)) "a cut is not a failed try")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (core/tick! eng))
          (is (= 2 @swims) "the refire swims up")
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p))))))))
