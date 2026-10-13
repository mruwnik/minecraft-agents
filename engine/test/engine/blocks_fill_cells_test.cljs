(ns engine.blocks-fill-cells-test
  "jobs.blocks.fill-cells against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.test-util :as tu]))

(def job 'jobs.blocks.fill-cells)
(def body {:pos {:x 0 :y 64 :z 0}})
(def cells [{:x 2 :y 64 :z 0} {:x 3 :y 64 :z 0}])

(defn ^:async outcome [inventory args & [override]]
  (let [env (bd/setup {:self body :inventory inventory})]
    (when override (.override (.-world (:p env)) "place" override))
    [env (await (bd/child-outcome (:eng env) job args 20))]))

(deftest every-cell-is-filled-with-the-first-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (outcome [{:name "dirt" :count 5}] {:cells cells :any-of ["cobblestone" "dirt"]}))]
          (is (= {:mended 2 :why nil} (select-keys r [:mended :why])))
          (is (= ["dirt" "dirt"] (map #(bd/block-at p %) cells))))))))

(deftest no-block-carried-ends-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (outcome [] {:cells cells :any-of ["dirt"]}))]
          (is (= {:mended 0 :why :short} (select-keys r [:mended :why])))
          (is (= "air" (bd/block-at p (first cells)))))))))

(deftest refused-places-end-failed-after-the-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (outcome [{:name "dirt" :count 9}] {:cells cells :any-of ["dirt"] :max-failures 3}
                                              (fn ^:async f [_ _ _] #js {:status "no-support"})))]
          (is (= {:mended 0 :why :failed} (select-keys r [:mended :why])))
          (is (= 3 (count (bd/calls p "place")))))))))
