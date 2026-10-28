(ns engine.strip-seal-test
  "jobs.strip.seal: a 3x3 corridor cross-section blocked off from the base side, in the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.test-util :as tu]
            [jobs.lib.child :as child]
            [jobs.lib.cost.risk :as risk]
            [jobs.strip.seal :as seal]))

(def job 'jobs.strip.seal)
(def args {:at [0 64 0] :axis :x :side :from})
(def cobble [{:name "cobblestone" :count 12}])
(def tools [{:name "iron_sword" :count 1}])
(def drops {"rail" "rail" "cobweb" "string" "torch" "torch"})
(def plane (for [y [64 65 66] z [-1 0 1]] [0 y z]))

(def corridor
  "A corridor along x, 3 wide and 3 high, in rock: a rail in the centre of the section, a cobweb and a torch in it."
  (merge (tu/box -10 63 -2 10 63 2 "stone") (tu/box -10 64 -2 10 66 -2 "stone") (tu/box -10 64 2 10 66 2 "stone") (tu/box -10 67 -2 10 67 2 "stone")
         {"0,64,0" "rail" "0,65,1" "cobweb" "0,65,-1" "torch"}))

(defn world [& {:keys [at inventory extra zones] :or {at -3 inventory (into cobble tools)}}]
  (bd/setup {:self {:pos {:x at :y 64 :z 0}} :blocks (merge corridor extra) :inventory inventory :drops drops :zones (or zones [])}))

(defn run! [{:keys [eng]} a] (tu/child-outcome eng job a 120))
(defn solid? [p cell] (= "cobblestone" (bd/block-at p cell)))
(defn feet-x [p] (js/Math.floor (.-x (.-pos (.self p)))))
(defn placed-cells [p] (mapv #(let [pos (.-pos (.-args %))] [(.-x pos) (.-y pos) (.-z pos)]) (bd/calls p "place")))

(deftest the-section-is-cleared-and-sealed-from-the-base-side
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              p (:p env)
              r (await (run! env args))]
          (is (= {:status :done :placed 9} (select-keys r [:status :placed])))
          (is (every? #(solid? p %) plane))
          (is (neg? (feet-x p)) "the body ends on the side it was asked to")
          (is (= 1 (bd/carried p "torch")) "the torch in the section is taken")
          (is (= 3 (bd/carried p "cobblestone")))
          (is (= [0 64 0] (last (placed-cells p))) "the centre bottom cell is closed last")
          (is (= [64 64 64 65 65 65 66 66 66] (sort (mapv second (placed-cells p)))) "nine cells, three rows"))))))

(deftest a-pickup-that-takes-nothing-does-not-hold-the-seal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              p (:p env)
              _ (.override (.-world p) "collect" (fn [_ _ _] #js {:status "unreachable" :reason "no-path" :gained #js []}))
              r (await (run! env args))]
          (is (= {:status :done :placed 9} (select-keys r [:status :placed])))
          (is (every? #(solid? p %) plane)))))))

(deftest the-lowest-row-is-placed-before-the-rows-above
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              _ (await (run! env args))
              ys (mapv second (placed-cells (:p env)))]
          (is (= (sort (butlast ys)) (butlast ys)) "rows go up, apart from the centre bottom cell last"))))))

(deftest a-sealed-section-is-done-with-nothing-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :extra (into {} (map (fn [[x y z]] [(str x "," y "," z) "cobblestone"]) plane)))
              r (await (run! env args))]
          (is (= :done (:status r)) "done at once: nothing is open")
          (is (empty? (bd/calls (:p env) "place"))))))))

(deftest short-of-blocks-stops-no-blocks-naming-the-shortfall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :inventory (into [{:name "cobblestone" :count 4}] tools))
              r (await (run! env (assoc args :ways #{:carried})))]
          (is (= {:status :stopped :reason :no-blocks :placed 4 :short 5} (select-keys r [:status :reason :placed :short]))))))))

(deftest with-no-blocks-carried-they-are-fetched-by-default-before-any-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :inventory tools)
              _ (await (run! env args))
              started (filter #(= :child_started (:kind %)) @(:seen env))]
          (is (some #(= :need (:slot %)) started) "the blocks are fetched through a need child")
          (is (not-any? #(= :walk (:slot %)) started) "no walk to the stand first")
          (is (empty? (bd/calls (:p env) "dig"))))))))

(deftest with-no-blocks-carried-and-only-carried-ways-it-stops-no-blocks-before-any-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :inventory tools)
              r (await (run! env (assoc args :ways #{:carried})))]
          (is (= {:status :stopped :reason :no-blocks :placed 0 :short 9} (select-keys r [:status :reason :placed :short])))
          (is (empty? (bd/calls (:p env) "dig")) "nothing dug")
          (is (= -3 (feet-x (:p env))) "the body did not walk to the stand"))))))

(deftest a-body-on-the-far-side-walks-round-to-the-base-side
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :at 4)
              r (await (run! env args))]
          (is (= :done (:status r)))
          (is (every? #(solid? (:p env) %) plane))
          (is (neg? (feet-x (:p env)))))))))

(deftest a-far-side-body-with-no-way-round-stops-wrong-side
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :at 4 :extra (merge (tu/box 2 64 -1 2 66 1 "stone") (tu/box -11 64 -2 -11 66 2 "stone") (tu/box 11 64 -2 11 66 2 "stone")))
              r (await (run! env args))]
          (is (= {:status :stopped :reason :wrong-side :placed 0} (select-keys r [:status :reason :placed])))
          (is (empty? (bd/calls (:p env) "place"))))))))

(deftest a-cell-in-anothers-zone-stops-refused-naming-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :zones [{:name "farm" :min [0 60 0] :max [0 70 0] :owner "Miles"}])
              r (await (run! env args))]
          (is (= {:status :stopped :reason :refused} (select-keys r [:status :reason])))
          (is (= [0 65 0] (vec (:cell r))) "the centre cells are the zone's; the first refused is reported")
          (is (not (solid? (:p env) [0 64 0]))))))))

(deftest a-container-in-the-section-is-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :extra {"0,64,1" "chest"})
              r (await (run! env args))]
          (is (= {:status :stopped :reason :blocked :cell [0 64 1]} (select-keys r [:status :reason :cell])))
          (is (= "chest" (bd/block-at (:p env) [0 64 1]))))))))


(deftest section-cells-run-lowest-row-first-with-the-centre-bottom-last
  (let [cells (seal/section [0 64 0] :x 3 3)]
    (is (= 9 (count (distinct cells))))
    (is (= [0 64 0] (last cells)))
    (is (= (sort (map second (butlast cells))) (map second (butlast cells))))))

(deftest the-stand-cell-is-one-step-back-on-the-side
  (are [axis side expected] (= expected (seal/stand [0 64 0] axis side))
    :x :from [-1 64 0]
    :x :to [1 64 0]
    :z :from [0 64 -1]
    :z :to [0 64 1]))

(deftest a-failed-block-fetch-ends-no-blocks-with-the-fetch-as-because
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world :inventory tools)
              r (await (run! env args))]
          (is (= {:status :stopped :reason :no-blocks} (select-keys r [:status :reason])) (pr-str r))
          (is (some? (:because (:ending r))) "the fetch's own ending rides as the because"))))))

(deftest the-walk-to-the-stand-is-priced-over-a-few-placed-blocks-not-a-strips-stay
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world)
              stays (atom [])
              orig child/call-walk*]
          (set! child/call-walk* (fn [c slot a] (swap! stays conj (:stay-s a)) (orig c slot a)))
          (try (await (run! env args))
               (finally (set! child/call-walk* orig)))
          (is (seq @stays))
          (is (every? #(= (risk/work-s :work) %) @stays)))))))
