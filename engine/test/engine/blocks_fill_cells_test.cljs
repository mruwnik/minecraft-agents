(ns engine.blocks-fill-cells-test
  "jobs.blocks.fill-cells against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.test-util :as tu]))

(def job 'jobs.blocks.fill-cells)
(def body {:pos {:x 0 :y 64 :z 0}})
(def three-failures {:jobs.blocks.fill-cells/max-failures 3})
(def cells [{:x 2 :y 64 :z 0} {:x 3 :y 64 :z 0}])

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent: its ending {:kind :cause :data}."
  [env args]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) (ctx/end c :done nil)))))}
        eng (assoc-in (:eng env) [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng 20))
    @out))

(defn ^:async outcome [inventory args & [override]]
  (let [env (bd/setup {:self body :inventory inventory})]
    (when override (.override (.-world (:p env)) "place" override))
    [env (await (ending-of env args))]))

(deftest every-cell-is-filled-with-the-first-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} e] (await (outcome [{:name "dirt" :count 5}] {:cells cells :use {:blocks ["cobblestone" "dirt"]}}))]
          (is (= :done (:kind e)))
          (is (= {:mended 2 :left []} (select-keys (:data e) [:mended :left])))
          (is (= ["dirt" "dirt"] (map #(bd/block-at p %) cells))))))))

(deftest cells-already-filled-are-done-with-nothing-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory [{:name "dirt" :count 5}] :blocks {"2,64,0" "dirt"}})
              e (await (ending-of env {:cells [(first cells)] :use {:blocks ["dirt"]}}))]
          (is (= :done (:kind e)))
          (is (empty? (bd/calls (:p env) "place"))))))))

(deftest no-block-carried-is-a-failed-need-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} e] (await (outcome [] {:cells cells :use {:blocks ["dirt"]}}))]
          (is (= :condition-failed (:kind e)))
          (is (= :need (get-in e [:cause :reason])))
          (is (= "air" (bd/block-at p (first cells)))))))))

(deftest a-partial-fill-is-not-done-unless-least-allows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ all] (await (outcome [{:name "dirt" :count 1}] {:cells cells :use {:blocks ["dirt"]}}))
              [_ one] (await (outcome [{:name "dirt" :count 1}] {:cells cells :use {:blocks ["dirt"]} :least 1}))]
          (is (= :condition-failed (:kind all)))
          (is (= {:mended 1 :left [(second cells)]} (select-keys (:data all) [:mended :left])))
          (is (= :done (:kind one)))
          (is (= 1 (:mended (:data one)))))))))

(deftest refused-places-end-nothing-possible-after-the-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} e] (await (settings/with-settings three-failures
                                                #(outcome [{:name "dirt" :count 9}] {:cells cells :use {:blocks ["dirt"]}}
                                                          (fn ^:async f [_ _ _] #js {:status "no-support"}))))]
          (is (= :nothing-possible (:kind e)))
          (is (= :failed (get-in e [:cause :reason])))
          (is (= {:mended 0 :left cells} (select-keys (:data e) [:mended :left])))
          (is (= 3 (count (bd/calls p "place")))))))))

(deftest a-jump-place-does-not-clear-the-failures-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              env (bd/setup {:self body :inventory [{:name "dirt" :count 9}]})
              _ (.override (.-world (:p env)) "jumpPlace" (fn [_ _ _] #js {:status (if (= 2 (swap! n inc)) "done" "failed")}))
              e (await (settings/with-settings three-failures #(ending-of env {:cells [(:pos body)] :use {:blocks ["dirt"]}})))]
          (is (= :nothing-possible (:kind e)))
          (is (= 4 (count (bd/calls (:p env) "jumpPlace")))))))))
