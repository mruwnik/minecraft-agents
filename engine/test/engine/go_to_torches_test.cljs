(ns engine.go-to-torches-test
  "go-to :torches (with :unknown): a dark floor the body cannot see from the takeoff is lit by a torch it places
  there (carried, or fetched once), then walked once seen; the planner prices the torch (jobs.lib.cost/light-cost, a
  caller's :unknown :light-cost overrides). Without the option nothing is placed or fetched."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.fake :as fake]
            [engine.go-to-sensed-test :as gs]
            [engine.memory :as mem]
            [engine.path.blocks :as blocks]
            [engine.path.planner.base :as base]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [engine.walk-sight-test :as wst]
            [jobs.lib.cost :as cost]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.sight :as ws]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.torch :as gtorch]
            [jobs.movement.go-to.cut :as gcut]))

;; ---------------------------------------------------------------- the gate hands a :light step to the walk

(deftest a-light-step-given-up-unseen-asks-for-a-torch
  (let [steps (-> (ws/mark-steps (subvec wst/road 2) (fn [x y z] (= [4 61 0] [x y z]))) (assoc-in [1 :light] true))
        e (assoc (wst/env (constantly nil)) :steps steps)
        s0 (walk/start-state steps {:x 3.5 :z 0.5})
        [_ out] (wst/run-ticks e s0 (assoc wst/still :x 3.5) 200)]
    (is (= {:status :light :step 1 :cells [[4 61 0]]} (select-keys (:done out) [:status :step :cells])) (pr-str out))))

(deftest a-takeoff-where-no-torch-went-joins-the-masks-no-light-set
  (let [pw (wst/masked-pw [[0 63 0 9 63 0 "stone" {}]] #{[5 63 0]})
        u (.-unknown (.-snapshot pw))]
    (set! (.-noLight u) (js/Set.))
    (ws/note-missed! pw {:status :replan :why :no-light :cells [[4 64 0]]})
    (is (= #{(base/cell-key 4 64 0)} (set (.-noLight u))))
    (is (= 0 (.-size (.-missed u))) "not a missed floor")))

;; ---------------------------------------------------------------- the price and the planner's inputs

(deftest a-torch-costs-the-torch-the-placing-and-a-trip-when-none-is-carried
  (let [{carried :cost cp :parts} (cost/light-cost {:carried 3})
        {none :cost np :parts} (cost/light-cost {:carried 0})]
    (is (= #{:torch :place} (set (keys cp))))
    (is (= #{:torch :place :trip} (set (keys np))))
    (is (pos? (:torch cp)))
    (is (= (+ carried (cost/trip)) none))))

(deftest a-torch-stands-on-a-sturdy-floor-or-a-fence-top
  (let [t (blocks/default-state-table)
        base (wworld/torch-base)
        on? (fn [n] (aget base (first (blocks/state-ids t n))))]
    (is (= [1 1 1 0 0 0] (mapv on? ["stone" "oak_fence" "glass" "air" "oak_slab" "water"])))))

;; ---------------------------------------------------------------- go-to down a dark ledge

(def ledge
  "Rock (stone x -4..10, y 59..67, z -2..2) with a corridor (x -2..0, feet y 64, z 0, the body's) whose end is a ledge over
  a low corridor 3 below (x 1..9, feet y 61, open up to y 65)."
  (apply dissoc (tu/box -4 59 -2 10 67 2 "stone")
         (concat (for [x (range -2 1) y [64 65]] (str x "," y ",0"))
                 (for [x (range 1 10) y (range 61 66)] (str x "," y ",0")))))

(defn torch-light
  "Lights the fake world as a torch does once one stands in it: block light 14 less the blocks (x+y+z) from it, round
  the ledge."
  [w]
  (let [torches (keep (fn [[pos n]] (when (#{"torch" "wall_torch"} n) pos)) (:blocks w))
        light (into {} (for [x (range -4 11) y (range 59 68) z (range -2 3)
                             :let [l (apply max 0 (map (fn [[tx ty tz]] (- 14 (+ (js/Math.abs (- x tx)) (js/Math.abs (- y ty)) (js/Math.abs (- z tz))))) torches))]
                             :when (pos? l)]
                         [[x y z] [0 l]]))]
    (if (= light (:light w {})) w (assoc w :light light))))

(defn dark-with-torch-light
  "gs/dark-and-looked (the low corridor's floor is past near, 4, and unseen), and a torch placed lights round it (torch-light)."
  [{:keys [p] :as world}]
  (gs/dark-and-looked world)
  (add-watch (fake/state p) :torch-light (fn [_ ref _ w] (let [w' (torch-light w)] (when-not (identical? w w') (reset! ref w'))))))

(def shielded
  "The body on the ledge, one block west of its rim, a shield in the off hand (no off-hand torch to see deeper), torches carried."
  {:self {:pos {:x -0.5 :y 64 :z 0.5}} :blocks ledge
   :inventory [{:name "torch" :count 4}] :equipment {"offHand" {:name "shield" :count 1}}})

(def below "The goal on the low ground." [9 61 0])

(defn torches-left [p] (reduce + (map :count (filter #(= "torch" (:name %)) (:inventory @(fake/state p))))))
(defn torches-held
  "The torches carried and in the off hand."
  [p]
  (+ (torches-left p) (let [o (get-in @(fake/state p) [:equipment "offHand"])] (if (= "torch" (:name o)) (:count o) 0))))
(defn torch-cells [p] (keep (fn [[pos n]] (when (#{"torch" "wall_torch"} n) pos)) (:blocks @(fake/state p))))

(deftest go-to-lights-the-dark-drop-with-a-carried-torch-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to shielded {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                                  :torches :fetch :ignore-zones? true}
                                                        dark-with-torch-light))]
          (is (= {:arrived true} (select-keys out [:arrived])) (pr-str out))
          (is (= {"torch" 1} (:spent out)) (str "the torch is reported: " (pr-str out)))
          (is (= (mapv vec (torch-cells p)) (:spent-at out)))
          (is (= [[0 65 0]] (torch-cells p)) "one torch on the takeoff, on the wall at the head")
          (is (= 3 (torches-left p)))
          (is (= 1 (count (filter #(= :walk.torch-placed (:kind %)) seen))))
          (is (empty? (filter #(= :gate-breach (:kind %)) seen))))))))

(deftest go-to-lights-the-dark-drop-by-default-when-a-torch-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gs/run-go-to shielded {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true :ignore-zones? true}
                                                   dark-with-torch-light))]
          (is (true? (:arrived out)) (pr-str out))
          (is (= [[0 65 0]] (torch-cells p))))))))

(deftest go-to-with-torches-never-places-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gs/run-go-to shielded {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true :torches :never :ignore-zones? true}
                                                   dark-with-torch-light))]
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (empty? (torch-cells p)))
          (is (= 4 (torches-left p))))))))

(deftest go-to-with-no-torch-and-no-fetch-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to (assoc shielded :inventory [])
                                                        {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                         :torches :carried :ignore-zones? true}
                                                        dark-with-torch-light))]
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (re-find #"no torch" (str (:text out))) (str (:text out)))
          (is (empty? (torch-cells p))))))))

(deftest go-to-with-no-torch-fetches-torches-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (gs/run-go-to (assoc shielded :inventory [])
                                                      {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                       :torches :fetch :ignore-zones? true}
                                                      dark-with-torch-light))]
          (is (= :unseen-floor-dark (:why out)) (str "nothing to fetch torches from " (pr-str out)))
          (is (re-find #"could not fetch" (str (:text out))) (str (:text out))))))))

(deftest go-to-takes-a-torches-arg
  (is (contains? go-to/args :torches)))

;; ---------------------------------------------------------------- the off hand

(def bare "The body on the ledge with torches carried and the off hand empty (the off-hand torch aid may take them)."
  (dissoc shielded :equipment))

(deftest go-to-lights-the-dark-drop-with-an-empty-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to bare {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                              :torches :fetch :ignore-zones? true}
                                                        dark-with-torch-light))]
          (is (= {:arrived true} (select-keys out [:arrived])) (pr-str out))
          (is (= 1 (count (torch-cells p))) (pr-str (torch-cells p)))
          (is (= 3 (torches-held p))))))))

(deftest go-to-places-a-torch-it-holds-in-the-off-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to (assoc shielded :inventory [] :equipment {"offHand" {:name "torch" :count 4}})
                                                        {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                         :torches :carried :ignore-zones? true}
                                                        dark-with-torch-light))]
          (is (= {:arrived true} (select-keys out [:arrived])) (pr-str out))
          (is (= 1 (count (torch-cells p))))
          (is (= 3 (torches-held p))))))))

;; ---------------------------------------------------------------- zones, prices, failures

(deftest go-to-hangs-no-wall-torch-in-another-owners-zone-and-uses-the-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gs/run-go-to (assoc shielded :zones [{:name "theirs" :min [0 65 0] :max [0 65 0] :owner "Other"}])
                                                   {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true :torches :fetch}
                                                   dark-with-torch-light))]
          (is (= {:arrived true} (select-keys out [:arrived])) (pr-str out))
          (is (= [[0 64 0]] (torch-cells p)) "a floor torch at the feet, not a wall torch in the zone"))))))

(deftest go-to-places-no-torch-at-a-takeoff-in-another-owners-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (gs/run-go-to (assoc shielded :zones [{:name "theirs" :min [0 64 0] :max [0 65 0] :owner "Other"}])
                                                   {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true :torches :fetch}
                                                   dark-with-torch-light))]
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (empty? (torch-cells p))))))))

(deftest a-torch-that-will-not-go-ends-dark-and-is-not-tried-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [places (atom 0)
              refuse! (fn [{:keys [w] :as world}]
                        (dark-with-torch-light world)
                        (aset w "place" (fn [& _] (swap! places inc) (js/Promise.resolve #js {:status "failed" :reason "refused"}))))
              {:keys [out seen p]} (await (gs/run-go-to shielded {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                                  :torches :fetch :ignore-zones? true}
                                                        refuse!))]
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (= 1 @places) "the takeoff joins noLight: one try")
          (is (empty? (torch-cells p)))
          (is (empty? (filter #(= :walk.torch-placed (:kind %)) seen))))))))

(deftest a-torch-already-there-is-not-booked-as-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [occupied! (fn [{:keys [w p] :as world}]
                          (dark-with-torch-light world)
                          (aset w "place" (fn [& _]
                                            (swap! (fake/state p) assoc-in [:blocks "0,65,0"] "wall_torch")
                                            (js/Promise.resolve #js {:status "occupied" :block "wall_torch"}))))
              {:keys [seen]} (await (gs/run-go-to shielded {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                            :torches :fetch :ignore-zones? true}
                                                  occupied!))]
          (is (empty? (filter #(= :walk.torch-placed (:kind %)) seen)) "not its torch"))))))

(deftest a-floor-still-unseen-after-the-torch-ends-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to shielded {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                                  :torches :fetch :ignore-zones? true}
                                                        gs/dark-and-looked))]
          (is (= :unseen-floor-dark (:why out)) (pr-str out))
          (is (= 1 (count (torch-cells p))) "one torch, not one per round")
          (is (= 1 (count (filter #(= :walk.torch-placed (:kind %)) seen)))))))))

;; ---------------------------------------------------------------- the torch fetch's memory

(defn stub-ctx
  "A go-to ctx over primitives p whose job memory is the atom mem*, events into emitted, children waiting."
  [p mem* emitted args]
  {:primitives p :args args :id "j1" :root "j1" :slots [] :engine {:jobs registry/jobs}
   :view (fn [] {:now 1000000 :data {:entries {(mem/job-kind "j1") [{:t 1000000 :data @mem*}]}
                                     :policies {(mem/job-kind "j1") mem/job-policy}}})
   :update-mem (fn [f more] (apply swap! mem* f more))
   :remember (fn [_ _ _] nil)
   :emit (fn [kind _ _] (swap! emitted conj kind))})

(defn fail-entry [mem*] {(mem/job-kind "j1") [{:t 1000000 :data @mem*}]})

(deftest a-torch-fetch-is-a-need-child-that-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (gs/sensed-only (assoc shielded :inventory []))
              mem* (atom {})
              calls (atom [])
              c (assoc (stub-ctx p mem* (atom []) {:torches :fetch :unknown true})
                       :call (fn [slot def args] (swap! calls conj [slot (:item args) (:count args)]) (js/Promise.resolve :continue)))]
          (is (= :continue (await (gtorch/fetch-torches! c))))
          (is (= [[:need-torch "torch" 4]] @calls))
          (is (true? (:torch-fetching @mem*)) "step! re-enters it while the child waits"))))))

(deftest a-failed-torch-fetch-is-booked-in-body-memory
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (gs/sensed-only (assoc shielded :inventory []))
              mem* (atom {})
              booked (atom [])
              c (assoc (stub-ctx p mem* (atom []) {:torches :fetch :unknown true})
                       :call (fn [_ _ _] (js/Promise.resolve (contract/ending :condition-failed {:reason :need} nil)))
                       :remember (fn [kind _ policy] (swap! booked conj [kind (:ttl policy)])))]
          (is (= :again (await (gtorch/fetch-torches! c))))
          (is (= [[gtorch/fetch-failed-kind gtorch/fetch-failed-ttl-ms]] @booked))
          (is (not (:torch-fetching @mem*))))))))

(deftest a-call-fetches-torches-once-and-not-after-a-recent-failure
  (let [{:keys [p]} (gs/sensed-only (assoc shielded :inventory []))
        args {:torches :fetch :unknown true}
        open? #(gtorch/torch-fetch-open? %)]
    (is (false? (open? (stub-ctx p (atom {:torch-fetched true}) (atom []) args))))
    (is (true? (open? (stub-ctx p (atom {}) (atom []) args))))
    (is (false? (open? (stub-ctx p (atom {}) (atom []) (assoc args :torches :carried)))))
    (is (false? (open? (assoc (stub-ctx p (atom {}) (atom []) args)
                              :view (fn [] {:now 1000000 :data {:entries {gtorch/fetch-failed-kind [{:t 1000000 :data {}}]}
                                                                 :policies {gtorch/fetch-failed-kind {:cap 1 :ttl gtorch/fetch-failed-ttl-ms}}}})))))))

(deftest the-torch-setting-prices-the-torch
  (let [{:keys [p]} (gs/sensed-only shielded)
        round (fn [args] (.-lightCost (gtorch/round-unknown (assoc (stub-ctx p (atom {}) (atom []) (merge {:torches :fetch :ignore-zones? true} args))
                                                                  :unknown (wworld/unknown-spec)))))]
    (is (= (:cost (cost/light-cost {:carried 4})) (round {:unknown true})))
    (is (= (+ 7.5 (:cost (cost/light-cost {:carried 4})))
           (settings/with-settings {:jobs.lib.cost.value/torch-place-s 9} #(round {:unknown true}))))))

(deftest go-to-with-no-torch-fetches-torches-from-a-seen-chest-lights-the-drop-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen p]} (await (gs/run-go-to (assoc shielded :inventory []
                                                                :blocks (assoc ledge "-2,64,0" "chest")
                                                                :containers {"-2,64,0" [{:name "torch" :count 8}]})
                                                        {:pos below :range 1 :ways #{:walk :dig} :may-dig #{:own} :unknown true
                                                         :torches :fetch :ignore-zones? true}
                                                        dark-with-torch-light))]
          (is (= {:arrived true} (select-keys out [:arrived])) (pr-str out))
          (is (= 1 (count (torch-cells p))) "one torch on the takeoff")
          (is (= 3 (torches-held p)) "4 torches taken from the chest, one placed")
          (is (= 4 (get-in @(fake/state p) [:containers [-2 64 0] 0 :count])) "the other four stay in the chest")
          (is (= 1 (count (filter #(= :walk.torch-placed (:kind %)) seen)))))))))
