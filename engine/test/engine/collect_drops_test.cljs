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

(deftest a-drop-one-column-over-from-the-dug-hole-is-in-the-hole-and-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,63,0" "air" "1,59,0" "stone"}})]
          (drop! p [1 60 0] "dirt")
          (seen-from-above! p)
          (let [result (await (bd/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 0 (:collected result)))
            (is (= [:too-deep] (mapv :reason (bd/left-drops seen))))
            (is (not-any? #(and (= :child_started (:kind %)) (= :walk (:slot %))) @seen) "no go-to walk down the hole")
            (is (empty? (bd/calls p "collect")))))))))

(deftest a-deep-drop-go-to-cannot-reach-is-left-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pocket (into {} (for [x [4 5 6] y [56 57 58 59] z [-1 0 1]
                                    :when (not= [5 58 0] [x y z])]
                                [(str x "," y "," z) "stone"]))
              {:keys [eng p seen]} (bd/setup {:self body :blocks pocket})]
          (drop! p [5 58 0] "dirt")
          (seen-from-above! p)
          (let [result (await (bd/child-outcome eng job {:radius 8} 30))]
            (is (= 0 (:collected result)))
            (is (= [:unreachable] (mapv :reason (:left result))))
            (is (= [:unreachable] (mapv :reason (bd/left-drops seen))))))))))

(defn falling-first!
  "The first entities() answer shows every item one block higher, as a drop still falling into the hole."
  [p]
  (let [entities (.bind (.-entities p) p)
        calls (atom 0)]
    (aset p "entities"
          (fn [opts]
            (let [es (entities opts)]
              (when (= 1 (swap! calls inc))
                (doseq [e (array-seq es) :when (.-pos e)] (aset (.-pos e) "y" (inc (.-y (.-pos e))))))
              es)))))

(deftest a-drop-still-falling-into-a-cave-opening-is-judged-where-it-comes-to-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,63,0" "air" "0,62,0" "air" "0,61,0" "stone"}})]
          (drop! p [0 62 0] "dirt")
          (falling-first! p)
          (let [result (await (bd/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 0 (:collected result)))
            (is (= [:too-deep] (mapv :reason (bd/left-drops seen))))
            (is (empty? (bd/calls p "collect")))))))))

(def leaf-wall
  "A wall of leaves at x 2, z -2..2, two high: a drop at (4,64,0) behind it is out of the body's sight."
  (into {} (for [z (range -2 3) y [64 65]] [(str "2," y "," z) "oak_leaves"])))

(deftest a-drop-behind-a-leaf-wall-is-found-by-walking-round-it-when-a-search-is-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks leaf-wall})]
          (drop! p [4 64 0] "oak_log")
          (let [result (await (bd/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0} :search 3} 60))]
            (is (= 1 (:collected result)))
            (is (= 1 (bd/carried p "oak_log")))))))))

(deftest a-drop-behind-a-leaf-wall-stays-unseen-without-a-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks leaf-wall})]
          (drop! p [4 64 0] "oak_log")
          (let [result (await (bd/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0}} 30))]
            (is (= 0 (:collected result)))
            (is (= 0 (bd/carried p "oak_log")))))))))
