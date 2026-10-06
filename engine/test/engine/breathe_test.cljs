(ns engine.breathe-test
  "The suffocating trigger and the breathe job against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [jobs.survival.breathe :as b]
            [engine.fake :as fake]
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

(deftest drowning-round-swims-up-to-the-surface-and-finishes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)) "feet at the top water block, head in air")
          (await (core/tick! eng))
          (is (= 1 (count (:list (core/state eng)))) "still listed: afloat, no shore in reach"))))))

(defn call-names [p] (mapv #(.-name %) (array-seq (.. p -world -calls))))

(deftest drowning-in-an-open-column-swims-and-does-not-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)))
          (is (= 1 (count (:list (core/state eng)))) "afloat, not done")
          (is (= ["swim" "steer"] (call-names p)) "the swim surfaces, a steer hold keeps it up, no walk"))))))

(deftest drowning-with-a-failing-swim-gives-up-with-one-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4} :swimFails true :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :no_air (:kind %)) @seen)))))))))

(def side-args (assoc defaults :radius 1))

(deftest drowning-round-swims-sideways-when-the-column-is-capped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world {:self {:inWater true :oxygen 4}
                     :blocks (merge water-column walls
                                    {"0,68,0" "stone"
                                     "1,64,0" "water" "1,65,0" "water" "1,66,0" "water" "1,67,0" "water"})}
              {:keys [eng p]} (setup world)]
          (core/submit! eng (list breathe side-args) {})
          (await (core/tick! eng))
          (is (= {:x 1 :y 64 :z 0} (core/self-pos p)) "first round: sideways at feet height")
          (await (core/tick! eng))
          (is (= {:x 1 :y 67 :z 0} (core/self-pos p)) "second round: up the adjacent column")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest drowning-with-no-air-in-reach-gives-up-after-bounded-rounds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4}
                                         :blocks (merge water-column walls {"0,68,0" "stone" "1,64,0" "stone"})})]
          (core/submit! eng (list breathe side-args) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (some #(= :no_air (:kind %)) @seen)))))))

(deftest enclosed-round-digs-the-head-block-then-the-one-above-and-steps-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "stone" "0,66,0" "stone"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= "air" (.-name (.blockAt p (tu/pos 0 65 0)))) "head block dug")
          (is (= "air" (.-name (.blockAt p (tu/pos 0 66 0)))) "block above dug")
          (is (= {:x 0 :y 65 :z 0} (core/self-pos p)) "stepped up into the shaft")
          (is (= [] (:list (core/state eng)))))))))

(deftest enclosed-with-a-dig-that-times-out-gives-up-without-moving
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"0,65,0" "obsidian"}})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "timeout"}))
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (filter #(= :no_way_out (:kind %)) @seen))))
          (is (not (some #{"moveTo"} (call-names p)))))))))

(deftest enclosed-round-leaves-the-block-above-when-it-is-already-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "dirt"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= ["dig" "moveTo"] (mapv #(.-name %) (array-seq (.. p -world -calls)))) "no second dig"))))))

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

(deftest an-enclosed-round-remembers-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks {"0,65,0" "stone"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= :enclosed (:why (:data (mem/latest (mem/view (:store eng)) :breathe))))))))))

(deftest a-round-writes-one-breathe-memory-entry-with-where-and-why
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (let [view (mem/view (:store eng))
                entry (mem/latest view :breathe)]
            (is (= 1 (count (mem/entries view :breathe))))
            (is (= :drowning (:why (:data entry))))
            (is (= {:x 0 :y 64 :z 0} (:pos (:data entry))) "where it started")
            (is (= {:cap 20 :ttl (* 60 60 1000)} (mem/policy view :breathe)))))))))

(def pool
  "Water at y 64 and 65 around the origin column; air above."
  (into {} (for [y [64 65] x [-1 0 1] z [-1 0 1]] [(str x "," y "," z) "water"])))

(deftest surfaced-in-a-pool-it-swims-onto-a-ledge-two-blocks-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4}
                                      :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 0 :y 65 :z 0} (core/self-pos p)) "first round surfaces")
          (is (= 1 (count (:list (core/state eng)))) "still listed: the body is in water")
          (await (core/tick! eng))
          (is (= {:x 2 :y 65 :z 0} (core/self-pos p)) "second round stands on the ledge")
          (is (not (.-inWater (.self p))))
          (is (= ["swim" "swim"] (call-names p)) "the shore step swims, no moveTo")
          (is (= {:x 2 :y 65 :z 0} (js->clj (.-toward (.-args (last (array-seq (.. p -world -calls))))) :keywordize-keys true)))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest surfaced-with-the-rim-two-blocks-above-the-feet-it-still-finds-the-ledge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4}
                                      :blocks (merge pool {"2,64,0" "stone" "2,65,0" "stone" "2,66,0" "stone"})})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 0 :y 65 :z 0} (core/self-pos p)) "the swim surfaced at the top water block")
          (await (core/tick! eng))
          (is (= {:x 2 :y 67 :z 0} (core/self-pos p)) "stands on the ledge, feet two above the own")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest surfaced-in-open-water-with-no-shore-stays-afloat-and-reports-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (= 1 (count (:list (core/state eng)))) "the job does not end while the body is in water")
          (is (= 1 (count (filter #(= :afloat (:kind %)) @seen))) "one notice, however many rounds")
          (is (= ["swim"] (distinct (take 1 (call-names p)))))
          (is (= ["steer"] (distinct (rest (call-names p)))) "after the surfacing swim every act is a hold")
          (is (= 5 (count (rest (call-names p)))) "one hold per round, no second swim"))))))

(defn set-self!
  "Set fields of the fake body's self sensing (a bob above the surface, standing on the ground)."
  [p fields]
  (fake/swap-self! p into (map (fn [[k v]] [(keyword k) v]) fields)))

(deftest an-afloat-body-that-bobs-out-of-the-water-is-still-afloat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (core/register-reflex! eng {:trigger :suffocating})
          (dotimes [_ 4] (await (core/tick! eng)))
          (set-self! p {"inWater" false "onGround" false})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (empty? (filter #(= :ended (:kind %)) @seen)) "a crest above the water does not end the reflex")
          (is (= 1 (count (filter #(= :afloat (:kind %)) @seen))))
          (is (= "steer" (last (call-names p)))))))))

(deftest an-afloat-body-that-stands-on-land-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (assoc water-column "0,63,0" "stone")})]
          (core/register-reflex! eng {:trigger :suffocating})
          (dotimes [_ 4] (await (core/tick! eng)))
          (set-self! p {"inWater" false "onGround" true})
          (await (core/tick! eng))
          (is (= 1 (count (filter #(= :ended (:kind %)) @seen)))))))))

(deftest a-body-pressed-against-a-wall-over-water-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks (assoc water-column "0,63,0" "water")})]
          (core/register-reflex! eng {:trigger :suffocating})
          (dotimes [_ 4] (await (core/tick! eng)))
          (set-self! p {"inWater" false "onGround" true})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (empty? (filter #(= :ended (:kind %)) @seen)) "water below: not a footing"))))))

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
        (let [{:keys [eng p]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 62 :z 0}}
                                      :blocks (merge (pond 3) (bank 4 5 3))})]
          (.override (.-world p) "swim"
                     (fn ^:async f [_ args impl]
                       (if (.-toward args)
                         (await (impl _ args))
                         (do (set-self! p {"oxygen" 20}) #js {:status "surfaced"}))))
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (= {:x 4 :y 65 :z 0} (core/self-pos p)) "swam onto the ledge from three blocks below the surface")
          (is (not (.-inWater (.self p)))))))))

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
          (core/submit! eng (list breathe far-args) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (= [] (:list (core/state eng))) "done once out of the water")
          (is (>= (:x (core/self-pos p)) 7) "stands on the bank")
          (is (not (.-inWater (.self p))))
          (is (= ["steer"] (distinct (rest (call-names p)))) "walked with steer, no moveTo")
          (is (empty? (filter #(= :afloat (:kind %)) @seen))))))))

(deftest surfaced-with-a-wall-all-round-stays-afloat-and-reports-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (into {} (for [x (range -7 8) z (range -7 8) y [65 66]
                                  :when (= 7 (max (js/Math.abs x) (js/Math.abs z)))]
                              [(str x "," y "," z) "stone"]))
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (pond 6) (bank 7 7 6) (bank -7 -7 6) wall)})]
          (core/submit! eng (list breathe far-args) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (= 1 (count (:list (core/state eng)))))
          (is (< (:x (core/self-pos p)) 7) "never left the pond")
          (is (= 1 (count (filter #(= :afloat (:kind %)) @seen)))))))))

(deftest surfaced-with-a-failing-shore-swim-warns-no-shore-after-three-rounds-and-stays-afloat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4}
                                           :blocks (merge pool {"2,64,0" "stone" "2,63,0" "stone"})})]
          (.override (.-world p) "swim"
                     (fn ^:async f [_ args impl]
                       (if (.-toward args) #js {:status "timeout"} (await (impl _ args)))))
          (core/submit! eng (list breathe defaults) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (= 1 (count (:list (core/state eng)))) "afloat, not done: it would sink and fire again")
          (is (= 1 (count (filter #(= :no_shore (:kind %)) @seen))))
          (is (empty? (filter #(= :afloat (:kind %)) @seen)) "the no_shore warn is the one notice")
          (is (= "steer" (last (call-names p))))
          (is (not (some #{"moveTo"} (call-names p)))))))))

(deftest enclosed-with-a-free-neighbour-steps-sideways-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "obsidian" "1,63,0" "stone"}})]
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= {:x 1 :y 64 :z 0} (core/self-pos p)) "stepped to the free neighbour")
          (is (= ["moveTo"] (call-names p)) "no dig")
          (is (= [] (:list (core/state eng)))))))))

(deftest enclosed-with-a-failed-side-step-digs-the-next-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"0,65,0" "dirt" "1,63,0" "stone"}})]
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "blocked"}))
          (core/submit! eng (list breathe defaults) {})
          (await (core/tick! eng))
          (is (= ["moveTo"] (call-names p)) "first round only tries the step")
          (is (= 1 (count (:list (core/state eng)))) "still listed, no failure counted")
          (await (core/tick! eng))
          (is (= ["moveTo" "dig" "moveTo"] (call-names p)) "second round digs"))))))

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
