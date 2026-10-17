(ns engine.collect-drops-test
  "jobs.forestry.collect-drops against the fake world: a drop down a seen slope is walked to (go-to), a drop in a hole the
  caller dug only from a seen solid safe floor."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.memory :as mem]
            [engine.event-api :as event-api]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [jobs.forestry.collect-drops :as job-ns]
            [jobs.lib.loose :as loose]
            [jobs.lib.shelter :as sh]))

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
          (let [result (await (tu/child-outcome eng job {:radius 6 :filter ["porkchop"]} 30))]
            (is (= 1 (:collected result)))
            (is (= :done (:status result)))
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
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 1 (:collected result)))
            (is (empty? (bd/left-drops seen)))
            (is (= [1] (mapv :maxDropDown (collect-args p))))))))))

(deftest a-drop-two-down-in-the-dug-hole-is-left-too-deep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,63,0" "air" "0,61,0" "stone"}})]
          (drop! p [0 62 0] "dirt")
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
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
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
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
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
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
          (let [result (await (tu/child-outcome eng job {:radius 8} 30))]
            (is (= 0 (:collected result)))
            (is (= :nothing-possible (get-in result [:ending :kind])) "drops seen, none collected: never :done")
            (is (= :none-collected (:reason result)))
            (is (= [:unreachable] (mapv :reason (:left result))))
            (is (= [:unreachable] (mapv :reason (bd/left-drops seen))))))))))

(deftest no-drop-in-sight-is-done-with-nothing-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (bd/setup {:self body})
              result (await (tu/child-outcome eng job {:radius 6} 10))]
          (is (= 0 (:collected result)))
          (is (= :done (:status result))))))))

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
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 0 (:collected result)))
            (is (= [:too-deep] (mapv :reason (bd/left-drops seen))))
            (is (empty? (bd/calls p "collect")))))))))

(def wall
  "A stone wall at x 2, z -2..2, two high: a drop at (4,64,0) behind it is out of the body's sight."
  (into {} (for [z (range -2 3) y [64 65]] [(str "2," y "," z) "stone"])))

(deftest a-drop-behind-a-wall-is-found-by-walking-round-it-when-a-search-is-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks wall})]
          (drop! p [4 64 0] "oak_log")
          (let [result (await (tu/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0} :search 3} 60))]
            (is (= 1 (:collected result)))
            (is (= 1 (bd/carried p "oak_log")))))))))

(deftest a-drop-behind-a-wall-stays-unseen-without-a-search
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks wall})]
          (drop! p [4 64 0] "oak_log")
          (let [result (await (tu/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0}} 30))]
            (is (= 0 (:collected result)))
            (is (= 0 (bd/carried p "oak_log")))))))))

(deftest a-search-that-runs-out-of-cells-reports-what-was-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body})]
          (drop! p [1 64 0] "oak_log")
          (let [result (await (tu/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0} :search 3 :want 3} 80))]
            (is (= 1 (:collected result)))
            (is (= 1 (bd/carried p "oak_log")))))))))

(deftest a-search-that-finds-nothing-ends-none-collected-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (bd/setup {:self body})
              result (await (tu/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0} :search 3} 80))]
          (is (= [:stopped :none-collected 0] ((juxt :status :reason :collected) result))))))))

(def night-ms 14000)

(deftest a-search-underground-at-night-walks-on-in-its-own-tunnel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :time night-ms :blocks (assoc wall "0,66,0" "stone")})]
          (drop! p [4 64 0] "oak_log")
          (let [result (with-redefs [sh/buried? (constantly true)]
                         (await (tu/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0} :search 3} 60)))]
            (is (= 1 (:collected result)))))))))

(deftest a-buried-walk-forces-its-dark-form-only-where-the-caller-has-none
  (with-redefs [sh/buried? (constantly true)]
    (is (= {:safe? '(not (threatened))} (:checks (job-ns/walk-args {:quote-call {}} {}))))
    (is (nil? (:checks (job-ns/walk-args {:quote-call {:checks {:safe? '(always)}}} {}))))))

(deftest only-the-want-items-count-towards-want
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks wall})]
          (drop! p [-1 64 0] "oak_sapling")
          (drop! p [4 64 0] "oak_log")
          (let [result (await (tu/child-outcome eng job {:radius 6 :filter ["oak_log" "oak_sapling"] :want-items ["oak_log"]
                                                         :near {:x 4 :y 64 :z 0} :search 3 :want 1} 80))]
            (is (= 2 (:collected result)))
            (is (= 1 (bd/carried p "oak_log")))))))))

(deftest ids-not-in-sight-are-left-unseen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body})]
          (drop! p [4 64 0] "oak_log")
          (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :visible false) es)))
          (let [result (await (tu/child-outcome eng job {:radius 6 :ids [999]} 30))]
            (is (= 0 (:collected result)))
            (is (= [{:reason :unseen :ids [999]}] (:left result)))
            (is (= [:unseen] (mapv :reason (bd/left-drops seen))))))))))

(deftest ids-all-picked-up-end-done-with-no-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body})]
          (drop! p [2 64 0] "oak_log")
          (drop! p [1 64 1] "oak_sapling")
          (let [ids (mapv :id (:entities @(fake/state p)))
                result (await (tu/child-outcome eng job {:radius 8 :ids ids} 30))]
            (is (= 2 (:collected result)))
            (is (nil? (:left result)))))))))

(deftest an-id-that-goes-out-of-sight-is-left-unseen-with-the-cell-it-was-last-seen-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body})]
          (drop! p [2 64 0] "oak_log")
          (drop! p [4 64 1] "iron_sword")
          (let [[a b] (mapv :id (:entities @(fake/state p)))]
            (.override (.-world p) "collect"
                       (fn [token args impl]
                         (swap! (fake/state p) update :entities (fn [es] (mapv #(cond-> % (= b (:id %)) (assoc :visible false)) es)))
                         (impl token args)))
            (let [result (await (tu/child-outcome eng job {:radius 8 :ids [a b]} 30))]
              (is (= [{:reason :unseen :ids [b] :last {b [4 64 1]}}] (:left result))))))))))

(deftest a-drop-in-a-sealed-pocket-is-dug-to-with-dig-true
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pocket (into {} (for [x (range -1 9) y (range 56 64) z [-2 -1 0 1 2]
                                    :when (not= [5 58 0] [x y z])]
                                [(str x "," y "," z) "stone"]))
              {:keys [eng p seen]} (bd/setup {:self body :inventory [{:name "wooden_pickaxe" :count 1}] :blocks pocket})]
          (drop! p [5 58 0] "dirt")
          (seen-from-above! p)
          (let [result (await (tu/child-outcome eng job {:radius 8 :dig true} 80))]
            (is (pos? (:collected result)) "the dug stone came in with it")
            (is (= 1 (bd/carried p "dirt")))
            (is (empty? (:left result)))))))))

(defn collect-unreachable-first!
  "Make the first n collect calls answer unreachable without moving; later ones run for real."
  [p n]
  (let [left (atom n)]
    (.override (.-world p) "collect"
               (fn [token args impl]
                 (if (pos? @left)
                   (do (swap! left dec) #js {:status "unreachable" :reason "no-path" :gained #js []})
                   (impl token args))))))

(deftest a-hole-drop-the-collect-action-calls-unreachable-is-left-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,62,0" "stone"}})]
          (drop! p [0 63 0] "dirt")
          (collect-unreachable-first! p 5)
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 0 (:collected result)))
            (is (= [:unreachable] (mapv :reason (:left result))))
            (is (= [:unreachable] (mapv :reason (bd/left-drops seen))))))))))

(deftest a-drop-picked-up-while-the-collect-action-answers-unreachable-is-collected-not-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks {"0,62,0" "stone"}})]
          (drop! p [0 63 0] "dirt")
          (.override (.-world p) "collect"
                     (fn [token args impl]
                       (impl token args)
                       #js {:status "unreachable" :reason "no-path" :gained #js []}))
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]]} 20))]
            (is (= 1 (bd/carried p "dirt")))
            (is (= 1 (:collected result)))
            (is (empty? (:left result)))))))))

(deftest a-drop-that-entered-the-inventory-without-the-collect-act-reporting-it-is-counted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks steps})]
          (drop! p [3 61 0] "porkchop")
          (seen-from-above! p)
          (.override (.-world p) "collect"
                     (fn [token args impl]
                       (impl token args)
                       #js {:status "ok" :gained #js []}))
          (let [result (await (tu/child-outcome eng job {:radius 6 :filter ["porkchop"]} 30))]
            (is (= 1 (bd/carried p "porkchop")))
            (is (= 1 (:collected result)))
            (is (= :done (:status result)))))))))

(deftest a-hole-drop-the-collect-action-calls-unreachable-is-dug-to-and-collected-again-with-dig-true
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :blocks {"0,62,0" "stone"}})]
          (drop! p [0 63 0] "dirt")
          (collect-unreachable-first! p 1)
          (let [result (await (tu/child-outcome eng job {:radius 6 :holes [[0 63 0]] :dig true} 20))]
            (is (= 1 (:collected result)))
            (is (= 1 (bd/carried p "dirt")))
            (is (empty? (:left result)))
            (is (empty? (bd/left-drops seen)))))))))

(deftest the-job-is-a-contract-job-naming-what-it-collects
  (let [{:keys [eng]} (bd/setup {:self body})
        entry (get (:jobs eng) job)]
    (is (fn? (:round entry)))
    (is (= [:ids :near] (:target entry)))
    (is (fn? (:quote-of entry)))
    (is (nil? (:ways entry)) "one method: no ways")))

(deftest a-quote-yields-the-drops-in-sight-and-spends-time-to-walk-to-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body})]
          (drop! p [3 64 0] "porkchop")
          (drop! p [4 64 0] "porkchop")
          (drop! p [90 64 0] "dirt")
          (let [{:keys [ok quote]} (await (event-api/quote-view eng (str job) "{:radius 6}"))
                [least mean most] (get-in quote [:uses :job-time])]
            (is (true? ok))
            (is (= {"porkchop" [0 2 2]} (get-in quote [:yields :items])) "only what is in the radius")
            (is (= [0 2 2] (:done quote)))
            (is (and (zero? least) (pos? mean) (<= mean most)))))))))

(deftest a-collect-cut-while-it-waits-on-a-settling-drop-releases-its-hold-in-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks {"0,62,0" "stone"}})
              held #(mem/entries (mem/view (:store eng)) loose/hold-kind)]
          (drop! p [0 63 0] "dirt")
          (core/submit! eng '(jobs.forestry.collect-drops {:radius 6 :holes [[0 63 0]]}) {})
          (await (core/tick! eng))
          (is (seq (held)) "held while it waits")
          (core/cancel! eng (first (:list (core/state eng))))
          (await (tu/run-until-empty eng 20))
          (is (empty? (held)) "the cleanup released it"))))))

(deftest a-search-with-want-zero-finding-nothing-ends-done-zero
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (bd/setup {:self body})
              result (await (tu/child-outcome eng job {:radius 6 :filter ["oak_log"] :near {:x 4 :y 64 :z 0} :search 3 :want 0} 80))]
          (is (= [:done 0] ((juxt :status :collected) result))))))))
