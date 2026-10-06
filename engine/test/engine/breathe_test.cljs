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

(defn setup
  "An engine over a fake body; raw (an atom) also collects the canonical events, with their levels."
  ([world] (setup world (atom [])))
  ([world raw]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/fake world)
         eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks [sink #(swap! raw conj %)] :now #(deref clock)})})]
     {:eng eng :p p :seen seen})))

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

(deftest drowning-run-swims-up-and-stops-no-land-in-range-without-holding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4} :blocks water-column})]
          (await (one-run! eng defaults))
          (is (= {:x 0 :y 67 :z 0} (core/self-pos p)) "feet at the top water block, head in air")
          (is (= [] (:list (core/state eng))) "one run")
          (is (= [[:stopped :no_land_in_range]] (ended seen)) "no land and no water to swim on: stopped, not completed")
          (is (= ["swim"] (call-names p)) "the swim surfaces; nothing else to try")
          (is (empty? (of-kind seen :holding)) "no hold")
          (is (empty? (of-kind seen :afloat))))))))

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
          (is (= [[:stopped :no_land_in_range]] (:ended r)) "the go-to arrives on the ledge, but not onGround")
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
  (assoc defaults :shore-radius 2 :search-radius 12 :leg-length 10 :swim-range 24 :max-legs 4))

(def no-shore-args "No shore swims: only the go-to child and the legs move the body." (assoc small-args :shore-radius 0))

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

(defn unreachable-count [seen] (count (of-kind seen :unreachable)))

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
          (await (one-run! eng small-args))
          (is (= [[:stopped :no_land_in_range]] (ended seen)))
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
          (await (one-run! eng small-args))
          (is (= [[:stopped :no_land_in_range]] (ended seen)))
          (is (<= 1 (unreachable-count seen) b/land-tries)))))))

(deftest open-water-with-no-land-swims-legs-inside-the-bounds-then-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (lake -30 30 12)})]
          (await (one-run! eng small-args))
          (is (= [[:stopped :no_land_in_range]] (ended seen)))
          (let [d (stopped-data seen)]
            (is (pos? (:legs d)) "it swam outward before it gave up")
            (is (<= (:legs d) (:max-legs small-args))))
          (is (<= (dist-from-origin p) (+ 2 (:swim-range small-args))) "never beyond the swim range"))))))

(deftest the-leg-count-is-capped-by-max-legs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                         :blocks (lake -50 50 12)})]
          (await (one-run! eng (assoc small-args :max-legs 2 :swim-range 60 :search-radius 5)))
          (is (= [[:stopped :no_land_in_range]] (ended seen)))
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
        (let [east-wall (into {} (for [y [65 66] z (range -6 7)] [(str "6," y "," z) "stone"]))
              {:keys [eng p seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                           :blocks (merge (lake -6 5 6) (bank 6 8 6) east-wall (bank -10 -7 6))})]
          (await (one-run! eng small-args))
          (is (= [[:completed nil]] (ended seen)))
          (is (neg? (:x (core/self-pos p))) "the failed east target excluded that way: it landed west")
          (is (pos? (unreachable-count seen)) "the east try failed first"))))))

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
                       (when (= 1 (swap! steers inc)) (takeover/take! eng {:who "claude" :why "cut"}))
                       (await (impl token args))))
          (await (core/tick! eng))
          (is (not-any? #{[:completed nil]} (ended seen)) "the cut run did not complete")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
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
              args (assoc no-shore-args :swim-range 9 :leg-length 9 :search-radius 16)]
          (await (one-run! eng args))
          (is (= [[:stopped :no_land_in_range]] (ended seen)))
          (is (= 2 (unreachable-count seen)) "two walled banks tried (2 x 15 blocks, beyond 3 x swim-range), not a third try"))))))

(defn stub-child-ctx
  "A ctx whose go-to child call resolves to r, with a go-to registry entry; child-result gives res."
  [r res]
  {:engine {:jobs {'jobs.movement.go-to {:check (fn [_] true) :round (fn [_] nil) :args nil}}}
   :call-child (fn [_slot _def _args] (js/Promise.resolve r))
   :child-result (fn [_slot] res)})

(deftest a-go-to-child-that-continues-or-is-declined-is-a-failed-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [r [:continue :declined]]
          (is (false? (await (b/go! (stub-child-ctx r {:arrived true}) :leg {:x 0 :y 64 :z 0} 2))) (str r)))
        (is (true? (await (b/go! (stub-child-ctx :done {:arrived true}) :leg {:x 0 :y 64 :z 0} 2))))
        (is (false? (await (b/go! (stub-child-ctx :done {:arrived false}) :leg {:x 0 :y 64 :z 0} 2))))))))

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
            (is (= [[:stopped :no_land_in_range]] (ended seen)))
            (is (= 0 (:legs (stopped-data seen))) "no leg repeats the first run's")))))))

(deftest children-of-a-breathe-run-leave-no-unreachable-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (atom [])
              {:keys [eng seen]} (setup {:self {:inWater true :oxygen 4 :pos {:x 0 :y 64 :z 0}}
                                         :blocks (merge (lake -6 6 6) (bank 7 7 6) (bank -7 -7 6) wall-all-round)}
                                        raw)
              unreachable #(= :unreachable (:kind %))]
          (await (one-run! eng small-args))
          (is (= [[:stopped :no_land_in_range]] (ended seen)))
          (is (pos? (count (filter unreachable @raw))) "the children still report, quietly")
          (is (empty? (filter #(and (unreachable %) (= :warn (:level %))) @raw))))))))

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
