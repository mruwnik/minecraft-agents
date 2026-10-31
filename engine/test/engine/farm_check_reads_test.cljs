(ns engine.farm-check-reads-test
  "A listed repeat of farm.maintain is checked every scheduler tick: its check reads the seen water once per cache window, not once per cell or per price."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.core.schedule :as schedule]
            [engine.farm-tend-plan-test :as fp]
            [engine.test-util :as tu]))

(def cells (for [x (range 2 12) z (range 2 12)] [x z]))

(def spec
  (fp/world-of
   (fp/ground "farmland" cells)
   (fp/crops "wheat" 3 cells)
   {:blocks (into {} (for [x (range -16 17) z (range -16 17) y [58 59 60 61 62]
                           :when (not (and (<= 2 x 11) (<= 2 z 11) (= y 62)))]
                       [(fp/k x y z) "water"]))
    :inventory [(fp/item "wheat_seeds" 20) (fp/item "bread" 12)]}))

(def plans {"w" {:id "w" :parts [{:id "wheat" :cells (mapv (fn [[x z]] [x 64 z]) cells) :want {:crop "wheat"}}]}})

(deftest a-check-reads-the-seen-water-once-per-window
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fp/setup spec plans [])
              eng (:eng s)
              reads (atom 0)
              p (:p s)
              seen-blocks (aget p "seenBlocks")]
          (aset p "seenBlocks" (fn [q] (swap! reads inc) (.call seen-blocks p q)))
          (core/submit! eng '(repeat (seq (jobs.farm.maintain {:area {:plan "w"}}) (jobs.time.noop))) {})
          (await (tu/run-ticks eng (:clock s) 1 700))
          (let [id (first (:list (core/state eng)))
                before @reads]
            (dotimes [_ 3] (swap! (:clock s) + 100) (schedule/check-passes? eng id))
            (is (<= (- @reads before) 1))))))))
