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

(deftest mine-stairs-down-to-stone-once-the-lava-flow-delay-has-passed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (m/start {:world {:blocks (m/soil-over-stone 63 59) :drops m/cobble :inventory [{:name "stone_pickaxe" :count 1}]}})]
          (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
          (core/submit! (:eng s) (m/spec {:block "stone" :count 2 :direction "east" :tunnel-length 4 :mend false}) {})
          ;; after a dig the stair yields :continue until a flow delay has passed (the fake clock moves 700 ms a tick)
          (await (m/run-ticks s 14))
          (is (m/finished? s) "ended once the flow delay passed")
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

(def three-dirt
  "Stone with exactly three dirt cells beside the start: whichever the job digs first, count 3 needs all three, so the cut one again."
  (merge (m/cells "stone" (range -2 3) [61 62 63] (range -2 3))
         {"1,63,0" "dirt" "-1,63,0" "dirt" "0,63,-1" "dirt"}))

(defn ^:async resume-after-cut
  "Submit a dirt job, cut it with cut!, restart over the same dir and run on; the restarted setup."
  [cut! blocks]
  (let [dir (tu/tmp-dir)
        s (m/start {:world {:blocks blocks} :dir dir})]
    (cut! s)
    (core/submit! (:eng s) (m/spec {:block "dirt" :count 3 :mend false}) {})
    (await (m/run-ticks s 1))
    (let [again (m/start {:p (:p s) :dir dir :clock (:clock s)})]
      (await (m/run-ticks again 40))
      {:cut s :again again})))

(deftest a-cut-after-the-dig-books-it-once-and-does-not-dig-the-cell-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              s (m/start {:world {:blocks m/floor} :dir dir})
              k (atom 0)
              live (atom (:eng s))
              cut! (fn [] (core/shutdown! @live) (js/Promise.reject (core/cut-error)))]
          (.override (.-world (:p s)) "dig"
                     (fn [token a impl]
                       (case (swap! k inc)
                         1 (.then (impl token a) (fn [_] (cut!)))
                         2 (cut!)
                         (impl token a))))
          (core/submit! (:eng s) (m/spec {:block "dirt" :count 3 :mend false}) {})
          (await (m/run-ticks s 1))
          (is (some? (:digging (m/job-mem s))) "the cut left the dig written")
          (let [first-cell (first (m/dug-cells s))
                again (m/start {:p (:p s) :dir dir :clock (:clock s)})]
            (reset! live (:eng again))
            (await (m/run-ticks again 1))
            (let [mem (m/job-mem again)]
              (is (= first-cell (:dug-at mem)) "the dig of the first cell is booked"))
            (await (m/run-ticks again 1))
            (let [more (m/dug-cells again)]
              (is (= 1 (count (filter #(= first-cell %) more))) "the cell is not dug again"))))))))

(deftest a-cut-before-the-dig-clears-the-flag-and-digs-the-cell-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [again]} (await (resume-after-cut #(tu/shutdown-at! % "dig" 1) three-dirt))
              cells (m/dug-cells again)]
          (is (m/finished? again))
          (is (= :count (:reason (m/done-event again))))
          (is (= 4 (count cells)) "the undone dig was repeated")
          (is (= 2 (count (filter #(= (first cells) %) cells))) "the cut one and its repeat")
          (is (= 3 (get (m/inv again) "dirt"))))))))

(def torch-tunnel {:block "iron_ore" :count 1 :direction "east" :tunnel-length 12 :mend false})
(def coal-stick [{:name "iron_pickaxe" :count 1} {:name "coal" :count 1} {:name "stick" :count 1}])

(deftest a-torch-craft-that-makes-nothing-is-not-retried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (m/start {:world (m/torch-world coal-stick)})]
          (.override (.-world (:p s)) "craft" (fn [_ _ _] (js/Promise.resolve #js {:status "failed" :reason "no-recipe"})))
          (core/submit! (:eng s) (m/spec torch-tunnel) {})
          (await (m/run-ticks s 60))
          (is (m/finished? s))
          (is (= :tunnel-length (:reason (m/done-event s))))
          (is (>= 3 (count (m/calls s "craft"))) "bounded: the craft is given up, not retried each round"))))))

(deftest a-collect-that-ends-with-nothing-gained-is-booked-and-does-not-loop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (m/start {:world {:blocks (m/cells "stone" [4 5 6 7 8 9] [64] [0]) :drops {"stone" "cobblestone"}
                                  :inventory m/pickaxe :unreachable ["5,64,2"]}})]
          (m/drop-away! (:p s) [5 64 2])
          (core/submit! (:eng s) (m/spec {:block "stone" :count 4 :dry-digs 2 :mend false}) {})
          (await (m/run-ticks s 60))
          (is (m/finished? s))
          (is (= :no-drops (:reason (m/done-event s))) "nothing came in, so the digs ran dry")
          (is (= 2 (m/dig-count s))))))))
