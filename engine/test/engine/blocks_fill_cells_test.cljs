(ns engine.blocks-fill-cells-test
  "jobs.blocks.fill-cells against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.fake :as fake]
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
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data :because])) (ctx/end c :done nil)))))}
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

(deftest order-given-fills-in-the-callers-order-and-lowest-goes-nearest-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far-first [{:x 3 :y 64 :z 0} {:x 2 :y 64 :z 0}]
              [{:keys [p]} _] (await (outcome [{:name "dirt" :count 5}] {:cells far-first :order :given :use {:blocks ["dirt"]}}))
              [{p2 :p} _] (await (outcome [{:name "dirt" :count 5}] {:cells far-first :use {:blocks ["dirt"]}}))
              ys #(mapv (fn [call] (.-x (.-pos (.-args call)))) (bd/calls % "place"))]
          (is (= [3 2] (ys p)))
          (is (= [2 3] (ys p2))))))))

(deftest clear-loose-digs-a-rail-first-and-never-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory [{:name "dirt" :count 5}] :blocks {"2,64,0" "rail" "3,64,0" "chest"} :drops {"rail" "rail"}})
              e (await (ending-of env {:cells cells :use {:blocks ["dirt"]} :clear-loose true}))
              plain (await (ending-of (bd/setup {:self body :inventory [{:name "dirt" :count 5}] :blocks {"2,64,0" "rail"}})
                                      {:cells [(first cells)] :use {:blocks ["dirt"]}}))]
          (is (= :condition-failed (:kind e)))
          (is (= {:reason :blocked :cell {:x 3 :y 64 :z 0}} (select-keys (:cause e) [:reason :cell])))
          (is (= :done (:kind plain)) "without :clear-loose a rail is no owed cell")
          (is (= 0 (:mended (:data plain)))))))))

(deftest clear-loose-replaces-the-rail-with-a-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory [{:name "dirt" :count 5}] :blocks {"2,64,0" "rail" "2,63,0" "stone"} :drops {"rail" "rail"}})
              e (await (ending-of env {:cells [(first cells)] :use {:blocks ["dirt"]} :clear-loose true}))]
          (is (= :done (:kind e)))
          (is (= "dirt" (bd/block-at (:p env) (first cells)))))))))

(deftest a-cell-in-anothers-zone-ends-refused-unless-zones-are-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone [{:name "farm" :min [2 60 0] :max [2 70 0] :owner "Miles"}]
              mk #(bd/setup {:self body :inventory [{:name "dirt" :count 5}] :zones zone})
              e (await (ending-of (mk) {:cells [(first cells)] :use {:blocks ["dirt"]}}))
              ok (await (ending-of (mk) {:cells [(first cells)] :use {:blocks ["dirt"]} :ignore-zones? true}))]
          (is (= :condition-failed (:kind e)))
          (is (= :refused (get-in e [:cause :reason])))
          (is (= :done (:kind ok))))))))

(deftest with-fetching-ways-and-no-block-a-need-child-is-started
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory []})
              e (await (ending-of env {:cells cells :use {:blocks ["dirt"]} :ways #{:carried :chest}}))
              started (filter #(= :child_started (:kind %)) @(:seen env))]
          (is (some #(= :need (:slot %)) started))
          (is (= :need (get-in e [:cause :reason])))
          (is (some? (:because e))))))))

(deftest a-mob-standing-in-the-cell-is-waited-out-not-counted-as-a-failed-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cow {:id 7 :name "cow" :kind "passive" :width 0.9 :height 1.4 :pos {:x 2.5 :y 64 :z 0.5}}
              env (bd/setup {:self body :inventory [{:name "dirt" :count 5}] :entities [cow]})
              p (:p env)
              waits (atom 0)]
          (.override (.-world p) "place"
                     (fn [token args impl]
                       (if (seq (fake/entities p))
                         (js/Promise.resolve #js {:status "refused"})
                         (impl token args))))
          (.override (.-world p) "wait"
                     (fn [token args impl]
                       (when (= 3 (swap! waits inc)) (swap! (fake/state p) assoc :entities []))
                       (impl token args)))
          (let [e (await (settings/with-settings three-failures #(ending-of env {:cells [(first cells)] :use {:blocks ["dirt"]}})))]
            (is (= :done (:kind e)))
            (is (= 1 (count (bd/calls p "place"))) "no place was tried while the cow stood there")))))))
