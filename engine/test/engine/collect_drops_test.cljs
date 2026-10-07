(ns engine.collect-drops-test
  "jobs.forestry.collect-drops against the fake world: a drop down a seen slope is walked to (go-to), a drop in a hole the
  caller dug only from a seen solid safe floor."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.fake :as fake]
            [engine.test-util :as tu]))

(def job 'jobs.forestry.collect-drops)
(def body {:pos {:x 0 :y 64 :z 0}})

(defn drop! [p pos item] (swap! (fake/state p) fake/spawn pos item 1))

(defn seen-from-above!
  "Mark every item seen: the step lips hide a drop low down the slope from the body standing at its top."
  [p]
  (swap! (fake/state p) update :entities (fn [es] (mapv #(cond-> % (= "item" (:kind %)) (assoc :visible true)) es))))

(defn collect-args [p] (mapv #(js->clj (.-args %) :keywordize-keys true) (bd/calls p "collect")))

(def steps
  "Stone steps down along +x from the floor at y 63: feet cells (1,63,0), (2,62,0), (3,61,0)."
  {"1,62,0" "stone" "2,61,0" "stone" "3,60,0" "stone"})

(deftest a-drop-three-blocks-down-a-seen-slope-is-walked-to-and-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks steps})]
          (drop! p [3 61 0] "porkchop")
          (seen-from-above! p)
          (let [result (await (bd/child-outcome eng job {:radius 6 :filter ["porkchop"]} 30))]
            (is (= 1 (:collected result)))
            (is (= 1 (bd/carried p "porkchop")))
            (is (empty? (bd/left-drops seen)))
            (is (some #(and (= :child_started (:kind %)) (= :walk (:slot %))) @seen) "a go-to walk took it down the slope")
            (is (every? #(nil? (:maxDropDown %)) (collect-args p)) "no drop cap off a dug hole")))))))

(deftest a-drop-in-the-dug-hole-is-picked-up-with-the-walk-capped-to-one-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,62,0" "stone"}})]
          (drop! p [0 63 0] "dirt")
          (let [result (await (bd/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 1 (:collected result)))
            (is (empty? (bd/left-drops seen)))
            (is (= [1] (mapv :maxDropDown (collect-args p))))))))))

(deftest a-drop-two-down-in-the-dug-hole-is-left-too-deep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,63,0" "air" "0,61,0" "stone"}})]
          (drop! p [0 62 0] "dirt")
          (let [result (await (bd/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 0 (:collected result)))
            (is (= [:too-deep] (mapv :reason (bd/left-drops seen))))
            (is (empty? (bd/calls p "collect")))))))))

(deftest a-dug-hole-with-a-side-cell-never-seen-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,62,0" "stone" "1,63,0" "lava"}})]
          (aset p "sensedAt" (fn [pos] (if (= [1 63 0] [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
          (drop! p [0 63 0] "dirt")
          (let [result (await (bd/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 0 (:collected result)))
            (is (= [:unseen-cell] (mapv :reason (bd/left-drops seen))))
            (is (seq (filter #(= :sense.looked (:kind %)) @seen)) "it looked before it left the drop")
            (is (empty? (bd/calls p "collect")))))))))
