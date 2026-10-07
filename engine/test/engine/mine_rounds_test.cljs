(ns engine.mine-rounds-test
  "mine and get-seeds run as one whole attempt: a single tick of the engine digs, tunnels, mends and walks home."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.get-seeds-test :as gs]
            [engine.mine-test :as m]
            [engine.test-util :as tu]))

(deftest mine-digs-a-patch-mends-and-ends-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (m/scenario {:block "sand" :count 6} {:blocks (merge m/floor m/sand-patch)} 1))]
          (is (m/finished? s) "ended after one tick")
          (is (= :count (:reason (m/done-event s))))
          (is (>= (get (m/inv s) "sand") 6)))))))

(deftest mine-runs-the-strip-tunnel-and-walks-home-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (m/scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4}
                                   (m/rock-world {"3,64,1" "iron_ore"}) 1))]
          (is (m/finished? s) "ended after one tick")
          (is (= 1 (get (m/inv s) "raw_iron")))
          (is (= [0 64 0] (m/feet s))))))))

(deftest mine-stairs-down-to-stone-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (m/start {:world {:blocks (m/soil-over-stone 63 59) :drops m/cobble :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
          (core/submit! (:eng s) (m/spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :mend false}) {})
          (await (m/run-ticks s 1))
          (is (m/finished? s) "ended after one tick")
          (is (= :count (:reason (m/done-event s)))))))))

(deftest mine-hangs-torches-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (m/scenario {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4 :torch-interval 2}
                                   (assoc (m/rock-world {}) :inventory (conj m/pickaxe {:name "torch" :count 3})) 1))]
          (is (m/finished? s) "ended after one tick")
          (is (some #(= "torch" (.-item (.-args %))) (m/calls s "place")) "a torch was placed"))))))

(deftest get-seeds-breaks-grass-until-the-count-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (gs/scenario {:count 2} {:blocks (gs/patch "short_grass" (range 2 6) (range 0 4)) :drops gs/seed-drops} 1))]
          (is (gs/finished? s) "ended after one tick")
          (is (= :count (:reason (gs/done-event s)))))))))

(deftest a-mend-place-answered-occupied-for-ever-ends-mend-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (m/start {:world {:blocks m/floor}})]
          (.override (.-world (:p s)) "place" (fn [_ _ _] (js/Promise.resolve #js {:status "occupied"})))
          (.override (.-world (:p s)) "dig"
                     (fn [token args impl]
                       (.then (impl token args)
                              (fn [r]
                                (fake/add-item! (:p s) "dirt" 1)
                                (swap! (fake/state (:p s)) assoc :entities [])
                                (m/set-pos! s 3 64 0)
                                r))))
          (core/submit! (:eng s) (m/spec {:block "dirt" :count 1}) {})
          (await (m/run-ticks s 1))
          (is (m/finished? s) "ended in one tick")
          (is (= 1 (count (m/events-of s :mine.mend-failed))))
          (is (>= 6 (count (m/calls s "place"))) "bounded"))))))
