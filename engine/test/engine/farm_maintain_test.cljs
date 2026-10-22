(ns engine.farm-maintain-test
  "jobs.farm.maintain against the fake world: the stock steps (compost, deposit) after tend, and the keep they share."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.farm-tend-test :as ft]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.farm.maintain :as maintain]
            [jobs.farm.maintain-stock :as stock]))

(defn ^:async scenario
  "Submit farm.maintain with args in a world; run n ticks 700 ms apart; the setup map."
  [args spec n]
  (let [s (ft/setup spec)]
    (core/submit! (:eng s) (list 'jobs.farm.maintain (merge {:area ft/box} args)) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(deftest without-a-chest-the-goods-stay-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (ft/world (ft/farm "wheat" 7 [[2 2]] [[2 2]]) ft/wheat-drops {:inventory [(ft/item "wheat" 5)]}) 40))]
          (is (= {:skipped :no-chest} (ft/step s :deposit)))
          (is (= 6 (get (ft/inv s) "wheat"))))))))

(def goods-world
  (ft/world (ft/farm "wheat" 3 [[2 2] [3 2]] [[2 2] [3 2]])
         {:inventory [(ft/item "wheat" 5) (ft/item "wheat_seeds" 10) (ft/item "iron_hoe" 1) (ft/item "bread" 2)]
          :containers {"10,64,0" []}}))

(deftest goods-above-the-seed-reserve-go-to-the-chest-and-tools-and-food-stay
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest ft/chest} goods-world 40))]
          (is (= {"wheat" 3} (ft/chest-items s)) "a stack of backup seed, the 3-day food and two wheat of bait stay carried")
          (is (= {"wheat" 2 "wheat_seeds" 10 "iron_hoe" 1 "bread" 2} (ft/inv s)))
          (is (= {:gave-up false} (select-keys (ft/step s :deposit) [:gave-up])))
          (is (true? (ft/finished? s))))))))

(deftest goods-an-agent-reserve-entry-holds-are-not-stored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ft/setup goods-world)
              _ (mem/write! (:store (:eng s)) :reserve {:id "wheat" :item "wheat" :count :all} {:cap 100 :ttl :forever})
              _ (core/submit! (:eng s) (list 'jobs.farm.maintain {:area ft/box :chest ft/chest}) {})
              _ (await (tu/run-ticks (:eng s) (:clock s) 40 700))]
          (is (= {} (ft/chest-items s)))
          (is (= 5 (get (ft/inv s) "wheat"))))))))

(deftest keep-is-honoured-above-the-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest ft/chest :carry {"wheat" 2 "wheat_seeds" 1}} goods-world 40))]
          (is (= {"wheat" 3} (ft/chest-items s)))
          (is (= 2 (get (ft/inv s) "wheat")))
          (is (= 10 (get (ft/inv s) "wheat_seeds"))))))))

(deftest only-seed-above-the-backup-stack-is-composted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:composter {:x 6 :y 64 :z 2}}
                                 (ft/world goods-world {:blocks {"6,64,2" "composter"}}
                                        {:inventory [(ft/item "wheat" 5) (ft/item "wheat_seeds" 70) (ft/item "bread" 2)]}) 60))]
          (is (= {"wheat_seeds" 6} (:fed (ft/step s :compost))))
          (is (= 64 (get (ft/inv s) "wheat_seeds")))
          (is (= 2 (get (ft/inv s) "bread")))
          (is (= 5 (get (ft/inv s) "wheat")) "crops are not fed")
          (is (= {:skipped :no-chest} (ft/step s :deposit))))))))

(deftest without-a-composter-or-surplus-seed-the-step-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest ft/chest} goods-world 40))]
          (is (= {:skipped :no-composter} (ft/step s :compost))))
        (let [s (await (scenario {:composter {:x 6 :y 64 :z 2} :chest ft/chest}
                                 (ft/world (ft/farm "wheat" 3 [[2 2] [3 2]] [[2 2] [3 2]]) {:blocks {"6,64,2" "composter"}}
                                        {:inventory [(ft/item "wheat_seeds" 4) (ft/item "wheat" 3)] :containers {"10,64,0" []}}) 40))]
          (is (= {:skipped :no-surplus-seed} (ft/step s :compost)))
          (is (= {"wheat" 1} (ft/chest-items s)) "two wheat stay as bait"))))))

(deftest reserve-sows-the-field-twice-over-one-seed-at-a-time
  (are [expected beds inventory] (= expected (stock/box-reserve beds inventory))
    {} 0 [(ft/item "wheat_seeds" 9)]
    {"wheat_seeds" 4} 2 [(ft/item "wheat_seeds" 9)]
    {"wheat_seeds" 3} 2 [(ft/item "wheat_seeds" 3)]
    {"wheat_seeds" 2 "carrot" 2} 2 [(ft/item "wheat_seeds" 9) (ft/item "carrot" 9)]
    {"wheat_seeds" 2 "carrot" 1 "potato" 1} 2 [(ft/item "wheat_seeds" 9) (ft/item "carrot" 1) (ft/item "potato" 5)]
    {"wheat_seeds" 5 "carrot" 1} 3 [(ft/item "wheat_seeds" 5) (ft/item "carrot" 1)]
    {"wheat_seeds" 3} 5 [(ft/item "wheat_seeds" 1) (ft/item "wheat_seeds" 2)]
    {} 4 [(ft/item "bread" 5) (ft/item "melon_seeds" 3)]))

(deftest the-keep-holds-sowing-seed-a-backup-stack-per-seed-and-the-held-units
  (are [expected sow held keep] (= expected (stock/keeps sow held keep))
    {"wheat" 2 "wheat_seeds" 64 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 64} {} {} {}
    {"wheat" 2 "wheat_seeds" 100 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 64} {"wheat_seeds" 100} {} {}
    {"wheat" 2 "wheat_seeds" 64 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 64 "bread" 4 "carrot" 9}
    {"carrot" 2} {"bread" 4 "carrot" 9} {}
    {"wheat" 2 "wheat_seeds" 64 "beetroot_seeds" 64 "melon_seeds" 64 "pumpkin_seeds" 70} {} {} {"pumpkin_seeds" 70}))

(deftest crops-are-stored-down-to-the-food-reserve
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest ft/chest}
                                 (ft/world (ft/farm "wheat" 3 [[2 2]] [[2 2]])
                                        {:inventory [(ft/item "carrot" 20) (ft/item "bread" 3)] :containers {"10,64,0" []}}) 40))]
          (is (= {"carrot" 5} (ft/chest-items s)) "bread 3 is 15 of the 60 points, so 15 carrots (45 points) stay")
          (is (= {"carrot" 15 "bread" 3} (ft/inv s))))))))

(deftest surplus-lists-names-above-their-keep
  (are [expected inventory keep names] (= expected (stock/surplus inventory keep names))
    ["wheat"] [(ft/item "wheat" 2) (ft/item "bread" 4)] {} ["wheat" "carrot"]
    [] [(ft/item "wheat" 2)] {"wheat" 2} ["wheat"]
    ["wheat"] [(ft/item "wheat" 2) (ft/item "wheat" 1)] {"wheat" 2} ["wheat"]
    ["carrot" "wheat"] [(ft/item "wheat" 2) (ft/item "carrot" 1)] {} ["carrot" "wheat"]))

(deftest beds-are-farmland-or-uncovered-dirt
  (are [expected ground] (= expected (stock/beds ground))
    2 [{:name "farmland" :above "wheat"} {:name "dirt" :above "air"}]
    1 [{:name "grass_block" :above "short_grass"} {:name "grass_block" :above "stone"}]
    0 [{:name "stone" :above "air"} {:name "dirt" :above nil}]))

(deftest each-step-decides-from-its-facts
  (are [expected step args facts] (= expected (:skip (maintain/decide step args facts)))
    nil :tend {:area ft/box} {}
    :nothing-blocked :tidy {:area {:plan "p"}} {:blocked 0}
    nil :tidy {:area {:plan "p"}} {:blocked 3}
    :no-composter :compost {} {:waste ["wheat_seeds"]}
    :no-surplus-seed :compost {:composter {:x 1 :y 1 :z 1}} {:waste []}
    nil :compost {:composter {:x 1 :y 1 :z 1}} {:waste ["wheat_seeds"] :keep {}}
    :no-chest :deposit {} {:stored ["wheat"]}
    :nothing-to-store :deposit {:chest {:x 1 :y 1 :z 1}} {:stored []}
    nil :deposit {:chest {:x 1 :y 1 :z 1}} {:stored ["wheat"] :keep {}}))

(deftest the-children-get-the-field-and-the-chest-and-the-farm-goods-chooser
  (let [given [-545 64 324]
        keep {"wheat_seeds" 64}]
    (is (= {:area ft/box :till true :irrigate false :fertilize true}
           (:args (:call (maintain/decide :tend {:area ft/box :till true :irrigate false :fertilize true} {})))))
    (is (= {:chest given :items :farm-goods :carry keep}
           (:args (:call (maintain/decide :deposit {:chest given} {:stored ["wheat"] :keep keep})))))))

(deftest tidy-gets-the-plan-and-its-part
  (is (= {:plan "p" :part "wheat"} (:args (:call (maintain/decide :tidy {:area {:plan "p" :part "wheat"}} {:blocked 2}))))))

(deftest a-plan-reserves-twice-its-cells-per-crop
  (is (= {"wheat_seeds" 4 "carrot" 2}
         (stock/plan-reserve {{:x 0 :y 64 :z 0} "wheat" {:x 1 :y 64 :z 0} "wheat" {:x 2 :y 64 :z 0} "carrots"}))))

(deftest a-step-counts-as-ran-only-with-an-entry-that-was-not-skipped-or-stopped
  (are [e entry] (= entry (maintain/entry-of :compost e))
    {:kind :done :data {:fed {"wheat_seeds" 6} :bone-meal 1}} {:fed {"wheat_seeds" 6} :bone-meal 1}
    {:kind :condition-failed :cause {:reason :no-zones}} {:skipped :declined :reason :no-zones}
    {:kind :nothing-possible :cause :gave-up :data {:fed {}}} {:fed {} :reason :gave-up :stopped :nothing-possible}))

(def field-60 {:min {:x 62 :y 63 :z 2} :max {:x 64 :y 64 :z 4}})

(deftest a-maintain-far-from-its-field-walks-there-and-tends-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ft/setup (ft/world {:floor [-30 -10 80 10]}
                                    {:blocks (into {} (map (fn [[x z]] [(ft/k (+ x 60) 63 z) "farmland"])) ft/all-cells)}
                                    {:blocks (into {} (map (fn [[x z]] [(ft/k (+ x 60) 64 z) "wheat"])) ft/all-cells)
                                     :ages (into {} (map (fn [[x z]] [(ft/k (+ x 60) 64 z) 7])) ft/all-cells)}
                                    ft/wheat-drops))
              _ (core/submit! (:eng s) (list 'jobs.farm.maintain {:area field-60}) {})
              _ (await (tu/run-ticks (:eng s) (:clock s) 60 700))]
          (is (pos? (get (ft/inv s) "wheat" 0)) "the ripe wheat 60 blocks off was cut")
          (is (some #(re-find #"^[^/]+/walk$" (str (:job %))) @(:seen s)) "maintain's own go-to child walked first")
          (is (true? (ft/finished? s))))))))
