(ns engine.blocks-place-test
  "jobs.blocks.place against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [plan.shape :as shape]))

(def job 'jobs.blocks.place)
(def at {:x 2 :y 64 :z 0})
(def body {:pos {:x 0 :y 64 :z 0}})
(def cobble [{:name "cobblestone" :count 4}])

(deftest a-carried-block-is-placed-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :inventory cobble})
              result (await (bd/child-outcome eng job {:pos [2 64 0] :item "cobblestone"} 5))]
          (is (= {:placed true :pos at :item "cobblestone" :reason :placed} result))
          (is (= "cobblestone" (bd/block-at p at)))
          (is (= 3 (bd/carried p "cobblestone")))
          (is (= 1 (count (filter #(= :blocks.place.done (:kind %)) @seen)))))))))

(deftest any-of-places-the-first-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :inventory [{:name "dirt" :count 1}]})
              result (await (bd/child-outcome eng job {:pos at :any-of ["cobblestone" "dirt"]} 5))]
          (is (= {:placed true :item "dirt"} (select-keys result [:placed :item])))
          (is (= "dirt" (bd/block-at p at))))))))

(deftest a-cell-out-of-reach-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble})
              result (await (bd/child-outcome eng job {:pos {:x 12 :y 64 :z 0} :item "cobblestone"} 20))]
          (is (:placed result))
          (is (= "cobblestone" (bd/block-at p {:x 12 :y 64 :z 0}))))))))

(deftest a-cell-holding-the-block-already-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks {"2,64,0" "cobblestone"} :inventory cobble})
              result (await (bd/child-outcome eng job {:pos at :item "cobblestone"} 5))]
          (is (= {:placed false :reason :already} (select-keys result [:placed :reason])))
          (is (empty? (bd/calls p "place"))))))))

(deftest grass-or-a-flower-in-the-cell-is-dug-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [plant ["short_grass" "poppy" "snow"]]
          (let [{:keys [eng p]} (bd/setup {:self body :blocks {"2,64,0" plant "2,63,0" "grass_block"} :inventory cobble})
                result (await (bd/child-outcome eng job {:pos at :item "cobblestone"} 8))]
            (is (:placed result) plant)
            (is (= [[2 64 0]] (mapv #(let [a (.-pos (.-args %))] [(.-x a) (.-y a) (.-z a)]) (bd/calls p "dig"))) plant)
            (is (= "cobblestone" (bd/block-at p at)) plant)))))))

(deftest without-the-item-it-waits-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :need :item "cobblestone" :pos at}
               (await (bd/waiting-after (bd/setup {:self body}) (list job {:pos at :item "cobblestone"}) 3))))
        (is (= {:reason :need :any-of ["cobblestone" "dirt"] :pos at}
               (await (bd/waiting-after (bd/setup {:self body}) (list job {:pos at :any-of ["cobblestone" "dirt"]}) 3))))))))

(deftest a-solid-block-in-the-cell-ends-occupied
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})]
          (let [r (await (bd/child-outcome (:eng env) job {:pos at :item "cobblestone"} 5))]
            (is (= {:placed false :reason :occupied :block "stone"} (select-keys r [:placed :reason :block]))))
          (is (empty? (bd/calls (:p env) "dig"))))))))

(defn done-text [seen]
  (:text (first (filter #(= :blocks.place.done (:kind %)) @seen))))

(deftest done-texts-name-the-item-and-the-blocking-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [a (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})
              _ (await (bd/child-outcome (:eng a) job {:pos at :item "cobblestone"} 5))
              b (bd/setup {:self body :blocks {"2,64,0" "cobblestone"} :inventory cobble})
              _ (await (bd/child-outcome (:eng b) job {:pos at :item "cobblestone"} 5))
              c (bd/setup {:self body :inventory cobble})
              _ (await (bd/child-outcome (:eng c) job {:pos [2 64 0] :item "cobblestone"} 5))]
          (is (= "did not place cobblestone at [2 64 0]: occupied by stone" (done-text (:seen a))))
          (is (= "did not place cobblestone at [2 64 0]: already" (done-text (:seen b))))
          (is (= "placed cobblestone at [2 64 0]" (done-text (:seen c)))))))))

(deftest a-cell-in-anothers-zone-waits-not-allowed-and-ignore-zones-records-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble :zones [bd/farm-zone]})]
          (is (= {:reason :not-allowed :pos at :by :zone :zone "farm" :owner "Miles"}
                 (await (bd/waiting-after env (list job {:pos at :item "cobblestone"}) 3)))))
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble :zones [bd/farm-zone]})
              result (await (bd/child-outcome eng job {:pos at :item "cobblestone" :ignore-zones? true} 5))]
          (is (:placed result))
          (is (= [{:cell [2 64 0] :action :place :was "air" :now "cobblestone"}]
                 (mapv #(select-keys (:data %) [:cell :action :was :now]) (mem/entries (mem/view (:store eng)) :tidy))))
          (is (= "cobblestone" (bd/block-at p at))))))))

(deftest the-bodys-own-cell-waits-own-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :own-body :pos {:x 0 :y 64 :z 0}}
               (await (bd/waiting-after (bd/setup {:self body :inventory cobble}) (list job {:pos [0 64 0] :item "cobblestone"}) 3))))))))

(deftest a-cell-with-nothing-to-place-against-waits-no-support
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :no-support :pos {:x 2 :y 67 :z 0}}
               (await (bd/waiting-after (bd/setup {:self body :inventory cobble}) (list job {:pos [2 67 0] :item "cobblestone"}) 3))))))))

(deftest no-item-named-ends-with-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (bd/setup {:self body :inventory cobble})]
          (is (= {:placed false :reason :bad-args} (select-keys (await (bd/child-outcome eng job {:pos at} 3)) [:placed :reason]))))))))

(deftest a-cell-of-the-bodys-own-plan-is-placed-another-bodys-plan-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "w" :cells [[2 64 0]] :want "cobblestone"}]}
              own (bd/setup {:self body :inventory cobble :plans {"hut" (shape/with-author plan "Fake")}})
              other (bd/setup {:self body :inventory cobble :plans {"hut" (shape/with-author plan "Miles")}})]
          (is (:placed (await (bd/child-outcome (:eng own) job {:pos at :item "cobblestone"} 5))))
          (is (= {:reason :not-allowed :by :footprint :plan "hut"}
                 (select-keys (await (bd/waiting-after other (list job {:pos at :item "cobblestone"}) 3)) [:reason :by :plan]))))))))

(deftest a-far-cell-is-walked-to-and-placed-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble})]
          (is (await (bd/ended-in-one-tick? env (list job {:pos [12 64 0] :item "cobblestone"}))))
          (is (= "cobblestone" (bd/block-at (:p env) {:x 12 :y 64 :z 0}))))))))

(deftest a-plant-is-cleared-and-the-cell-filled-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :blocks {"2,64,0" "short_grass" "2,63,0" "grass_block"} :inventory cobble})]
          (is (await (bd/ended-in-one-tick? env (list job {:pos [2 64 0] :item "cobblestone"}))))
          (is (= "cobblestone" (bd/block-at (:p env) at))))))))
