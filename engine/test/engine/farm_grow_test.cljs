(ns engine.farm-grow-test
  "jobs.farm.grow against the fake world: a standing field is used, never started again; no field is made."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [engine.farm-tend-test :as ft]
            [engine.test-util :as tu]
            [jobs.farm.grow :as grow]
            [jobs.lib.cost :as cost]
            [jobs.lib.world-files :as ew]))

(defn setup
  "As ft/setup, the clock moving 5 s with every primitive call, so a wait ends by time; plans {id plan} in its world."
  ([spec] (setup spec nil))
  ([spec plans]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor spec))
        now (tu/act-clock clock p 5000)
        eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                                  :events (events/make {:body "Fake" :sinks [sink] :now now})}
                           plans (assoc :world (ew/of-data plans {} []))))]
    {:eng eng :p p :seen seen :clock clock})))

(defn ^:async scenario
  ([args spec n] (scenario args spec n nil))
  ([args spec n plans]
  (let [s (setup spec plans)]
    (core/submit! (:eng s) (tu/order (list 'jobs.farm.grow args)) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s)))

(def kit [(ft/item "iron_hoe" 1) (ft/item "wheat_seeds" 9)])

(deftest a-ripe-crop-in-sight-is-cut-and-the-count-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2} (ft/world (ft/farm "wheat" 7 [[2 2] [3 2]] [[2 2] [3 2]]) ft/wheat-drops {:inventory kit}) 40))]
          (is (<= 2 (get (ft/inv s) "wheat" 0)))
          (is (true? (ft/finished? s))))))))

(deftest a-ripe-crop-beyond-the-harvest-radius-of-the-body-is-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 1} (ft/world (ft/farm "wheat" 7 [[25 2]] [[25 2]]) ft/wheat-drops {:inventory kit}) 60))]
          (is (<= 1 (get (ft/inv s) "wheat" 0))))))))

(deftest the-default-job-time-is-the-usual-cost-of-the-grow-plus-one-sd
  (let [{:keys [value sd]} (cost/field-grow "wheat" 6 3 2)
        one (grow/job-time-of {:crop "wheat" :count 6 :w 3 :h 2})
        two (grow/job-time-of {:crop "wheat" :count 12 :w 3 :h 2})]
    (is (= (+ value sd) one) "Q184: the expected cost plus one sd")
    (is (< (+ (cost/ripen-s "wheat" 6 6 0) (cost/ripen-sd "wheat" 6 6 0) (:value (cost/goal-value [{:name "wheat_seeds" :count 6}]))) one js/Infinity)
        "the ripening's mean and spread and the seed fetch, all within it")
    (is (< (:value (cost/field-grow "wheat" 6 3 2 {:hoe? true :seeds 6})) value) "a hoe and seeds carried are not fetched")
    (is (< one two) "a second harvest of the field is a second growth to wait for")))

(deftest a-growing-field-is-waited-for-and-no-field-is-started
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :spend {:job-time 120}} (ft/world (ft/farm "wheat" 2 [[2 2]] [[2 2]]) ft/wheat-drops {:inventory kit}) 60))]
          (is (some #(= :limit_reached (:kind %)) @(:seen s)) "waited for it to ripen until its time was up")
          (is (= 9 (get (ft/inv s) "wheat_seeds")) "no seed was sown")
          (is (empty? (ft/calls s "dig"))))))))

(deftest a-growing-wait-logs-one-holding-event-a-pause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :spend {:job-time 120}} (ft/world (ft/farm "wheat" 2 [[2 2]] [[2 2]]) ft/wheat-drops {:inventory kit}) 60))
              holds (filter #(= :holding (:kind %)) @(:seen s))]
          (is (= (count holds) (count (distinct (map :since holds)))) (pr-str (map #(select-keys % [:kind :since :round :reason]) holds))))))))

(deftest a-growing-wait-looks-again-at-crops-remembered-from-long-ago
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (ft/world (ft/farm "wheat" 2 [[2 2]] [[2 2]]) ft/wheat-drops {:inventory kit}))
              p (:p s)
              seen-blocks (aget p "seenBlocks")]
          (aset p "seenBlocks" (fn [q] (let [bs (seen-blocks q)] (doseq [b bs] (aset b "age-ms" 2400000)) bs)))
          (core/submit! (:eng s) (tu/order (list 'jobs.farm.grow {:count 2 :spend {:job-time 120}})) {})
          (await (tu/run-ticks (:eng s) (:clock s) 8 700))
          (is (some #(= [2.5 2.5] [(.-x (.-pos (.-args %))) (.-z (.-pos (.-args %)))]) (ft/calls s "look"))
              "it turned to the crop it only remembered"))))))

(defn ^:async relook-calls
  "The look calls a growing wait makes over a field of crops at cells (default two side by side) whose memory is age-ms old."
  [age-ms & [cells]]
  (let [cells (or cells [[2 2] [3 2]])
        s (setup (ft/world (ft/farm "wheat" 2 cells cells) ft/wheat-drops {:inventory kit}))
        p (:p s)
        seen-blocks (aget p "seenBlocks")]
    (aset p "seenBlocks" (fn [q] (let [bs (seen-blocks q)] (doseq [b bs] (aset b "age-ms" age-ms)) bs)))
    (core/submit! (:eng s) (tu/order (list 'jobs.farm.grow {:count 2 :spend {:job-time 120}})) {})
    (await (tu/run-ticks (:eng s) (:clock s) 8 700))
    (mapv #(let [pos (.-pos (.-args %))] [(.-x pos) (.-z pos)]) (ft/calls s "look"))))

(deftest memory-too-young-for-the-crop-to-have-ripened-is-not-looked-at-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [looks (await (relook-calls 60000))]
          (is (not-any? #(#{[2.5 2.5] [3.5 2.5] [3.0 2.5]} %) looks) "a minute-old memory of a wheat stage 2 is still good"))))))

(deftest stale-crops-are-looked-at-in-one-glance-at-the-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [looks (await (relook-calls 2400000))]
          (is (some #{[3.0 2.5]} looks) "one look at the middle of the two crops")
          (is (not-any? #{[2.5 2.5] [3.5 2.5]} looks) "not one look a crop"))))))

(deftest stale-crops-wider-apart-than-one-look-are-looked-at-once-per-side
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [looks (await (relook-calls 2400000 [[8 0] [-8 0]]))]
          (is (some #{[8.5 0.5]} looks) "a look at the crop east of the body")
          (is (some #{[-7.5 0.5]} looks) "and one at the crop west of it, behind the first look"))))))

(deftest a-growing-field-in-a-plan-of-the-bodys-own-is-waited-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plans {"farm" {:id "farm" :metadata {:by "Fake"} :parts [{:id "wheat" :cells [[2 64 2]] :want "wheat"}]}}
              s (await (scenario {:count 2 :spend {:job-time 120}} (ft/world (ft/farm "wheat" 2 [[2 2]] [[2 2]]) ft/wheat-drops {:inventory kit}) 60 plans))]
          (is (= 9 (get (ft/inv s) "wheat_seeds")) "no field of its own was sown beside it"))))))

(deftest a-growing-wait-ends-the-round-so-a-queued-job-takes-its-turn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2} (ft/world (ft/farm "wheat" 2 [[2 2]] [[2 2]]) ft/wheat-drops {:inventory kit}) 40))]
          (is (< 2 (apply max (keep #(when (= "jobs.farm.grow" (:name %)) (:round %)) @(:seen s))))
              "grow yields after each wait: one endless round would hold the turn from a queued job"))))))

(deftest with-no-crop-in-sight-a-field-is-made-on-flat-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :spend {:job-time 600}}
                                 (ft/world (ft/ground "dirt" (for [x (range -2 8) z (range -2 8)] [x z]))
                                           {:inventory kit}) 120))]
          (is (some #(= "wheat" (apply ft/block-at s %)) (for [x (range -2 8) z (range -2 8)] [x 64 z])) "seeds were sown"))))))

(deftest a-dark-floor-is-lit-with-carried-torches-and-then-sown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (for [x (range -2 8) z (range -2 8)] [x z])
              s (await (scenario {:count 2 :spend {:job-time 600}}
                                 (ft/world (ft/ground "dirt" cells)
                                           {:light {} :light-default [0 0]
                                            :inventory (conj kit (ft/item "torch" 8))}) 160))]
          (is (seq (ft/calls s "place")) "a torch was placed")
          (is (some #(= "wheat" (apply ft/block-at s %)) (for [[x z] cells] [x 64 z])) "seeds were sown"))))))

(deftest a-ripe-crop-the-harvest-cannot-reach-is-given-up-but-its-reachable-neighbour-is-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :spend {:job-time 600}}
                                 (ft/world (ft/farm "wheat" 7 [[12 4] [3 1]] [[12 4] [3 1]])
                                           {:unreachable [(ft/k 12 64 4)]} ft/wheat-drops {:inventory kit}) 200))]
          (is (= 7 (ft/age-at s 12 64 4)) "the unreachable crop stands")
          (is (< (ft/age-at s 3 64 1) 7) "the reachable crop beside it was cut and replanted"))))))

(deftest with-only-an-unreachable-ripe-crop-a-field-is-made-elsewhere
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (for [x (range -2 8) z (range -2 8)] [x z])
              s (await (scenario {:count 2 :spend {:job-time 600}}
                                 (ft/world (ft/farm "wheat" 7 [[12 4]] [[12 4]]) (ft/ground "dirt" cells)
                                           {:unreachable [(ft/k 12 64 4)]} ft/wheat-drops {:inventory kit}) 200))]
          (is (some #(= "wheat" (apply ft/block-at s %)) (for [[x z] cells] [x 64 z])) "seeds were sown elsewhere"))))))

(deftest a-field-is-sown-with-fewer-seeds-than-cells-when-the-count-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[crop seed] [["wheat" "wheat_seeds"] ["beetroots" "beetroot_seeds"]]]
          (let [s (await (scenario {:crop crop :count 4 :w 3 :h 2 :spend {:job-time 600}}
                                   (ft/world (ft/ground "dirt" (for [x (range -2 8) z (range -2 8)] [x z]))
                                             {:inventory [(ft/item "iron_hoe" 1) (ft/item seed 5)]}) 120))]
            (is (not-any? #(= :nothing-possible (:kind %)) @(:seen s)) (str crop ": not refused for the missing sixth seed"))
            (is (> 5 (get (ft/inv s) seed 0)) (str crop ": seeds were sown"))))))))
