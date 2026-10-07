(ns engine.pen-build-test
  "jobs.build.pen: build a plan's fence and prove it holds, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.build-from-plan-test :as b]
            [engine.core :as core]
            [engine.harvest-test :as h]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]
            [jobs.build.pen :as pen]))

(def job 'jobs.build.pen)

(def ground (into {} (for [x (range -2 9) z (range -2 9)] [(h/cell-key x 63 z) "stone"])))

(defn spec [inventory & [blocks]]
  {:inventory inventory :blocks (merge ground blocks)})

(def fenced-ring
  "Every cell of the 3x3 ring except the gate cell, which the plan gives as a gate."
  [[2 64 2] [4 64 2] [2 64 3] [4 64 3] [2 64 4] [3 64 4] [4 64 4]])

(defn ring-plan
  "The 3x3 fence ring at y 64 over x 2..4, z 2..4 with a gate at 3 64 2; without cells in skip."
  [& [skip]]
  {:id "pen"
   :parts [{:id "fence" :cells (vec (remove (set skip) fenced-ring)) :want "oak_fence"}
           {:id "gate" :cells [[3 64 2]] :want {:block "oak_fence_gate" :facing :north}}]})

(def kit [{:name "oak_fence" :count 16} {:name "oak_fence_gate" :count 1}])

(def built-ring
  (into {} (map (fn [[x y z]] [(h/cell-key x y z) "oak_fence"])) fenced-ring))

(def built-pen (assoc built-ring "3,64,2" "oak_fence_gate"))

(defn ^:async build!
  "Run the job as a child over the world spec and plans: [result seen p]."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen]} (b/start world-spec plans (or zones []))
        result (await (h/child-outcome eng job (merge {:plan "pen"} args) 300))]
    [result seen p]))

(defn kinds-of [seen kind] (h/events-of seen kind))

;; ---------------------------------------------------------------- the box

(deftest the-box-is-the-bounding-box-of-the-barrier-cells
  (are [cells box] (= box (pen/barrier-box cells))
    [{:pos [2 64 2] :want "oak_fence"} {:pos [4 64 4] :want "oak_fence"}]
    {:min {:x 2 :y 64 :z 2} :max {:x 4 :y 64 :z 4}}
    [{:pos [2 64 2] :want "oak_fence"} {:pos [5 65 3] :want {:block "oak_fence_gate" :facing :north}}
     {:pos [9 70 9] :want "torch"}]
    {:min {:x 2 :y 64 :z 2} :max {:x 5 :y 65 :z 3}}
    [{:pos [1 64 1] :want "cobblestone_wall"} {:pos [3 64 1] :want ["any" "oak_fence" "birch_fence"]}]
    {:min {:x 1 :y 64 :z 1} :max {:x 3 :y 64 :z 1}}
    [{:pos [2 64 2] :want "torch"} {:pos [3 64 2] :want "stone"}] nil
    [] nil))

;; ---------------------------------------------------------------- success

(deftest a-complete-plan-builds-and-passes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result seen p] (await (build! (spec kit) {"pen" (ring-plan)} {}))]
          (is (true? (:closed? result)))
          (is (nil? (:reason result)))
          (is (= [] (:leaks result)))
          (is (= 8 (get-in result [:built :placed])))
          (is (= "oak_fence_gate" (h/block-at p 3 64 2)))
          (is (= 1 (count (kinds-of seen :pen-build.done))))
          (is (empty? (kinds-of seen :pen-build.leaky))))))))

;; ---------------------------------------------------------------- leaks

(deftest a-fence-with-a-gap-builds-fails-the-check-and-names-the-leak
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result seen p] (await (build! (spec kit) {"pen" (ring-plan [[3 64 4]])} {}))
              warns (kinds-of seen :pen-build.leaky)]
          (is (= 7 (get-in result [:built :placed])) "everything the plan asks for is placed")
          (is (false? (:closed? result)))
          (is (= :leak (:reason result)))
          (is (= [{:pos {:x 3 :y 64 :z 4} :why :gap}] (:leaks result)))
          (is (= "air" (h/block-at p 3 64 4)) "nothing is placed that the plan does not ask for")
          (is (= 1 (count warns)))
          (is (= (:leaks result) (:leaks (first warns))))
          (is (empty? (kinds-of seen :pen-build.done))))))))

(deftest an-open-gate-is-a-leak
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world-spec (assoc (spec kit built-pen) :states {"3,64,2" {:open true :facing "north"}})
              [result seen] (await (build! world-spec {"pen" (ring-plan)} {}))]
          (is (= :leak (:reason result)))
          (is (= [:open-gate] (mapv :why (:leaks result))))
          (is (= 1 (count (kinds-of seen :pen-build.leaky)))))))))

;; ---------------------------------------------------------------- access and materials

(def zone b/zone)

(deftest a-cell-in-a-foreign-zone-is-built-the-plan-is-the-permission
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result seen p] (await (build! (spec kit) {"pen" (ring-plan)} {}
                                             [(zone "shrine" [4 64 3] [4 64 3] #{})]))]
          (is (= "oak_fence" (h/block-at p 4 64 3)))
          (is (true? (:closed? result)))
          (is (empty? (kinds-of seen :pen-build.leaky))))))))

(deftest ignore-zones-builds-without-a-zone-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (b/start (spec kit) {"pen" (ring-plan)} nil)
              result (await (h/child-outcome eng job {:plan "pen" :ignore-zones? true} 300))]
          (is (true? (:closed? result)))
          (is (= "oak_fence" (h/block-at p 4 64 3))))))))

(deftest a-cell-in-another-plans-footprint-is-reported-not-forced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (spec kit) {"pen" (ring-plan) "other" b/other-plan} [])
              result (await (h/child-outcome eng job {:plan "pen"} 300))
              [warn] (kinds-of seen :pen-build.leaky)]
          (is (= "air" (h/block-at p 4 64 3)))
          (is (= :leak (:reason result)))
          (is (= [{:pos [4 64 3] :reason :footprint :plan "other"}] (:refused warn))))))))

(deftest too-little-fence-gives-up-naming-what-is-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result seen] (await (build! (spec [{:name "oak_fence" :count 4} {:name "oak_fence_gate" :count 1}])
                                           {"pen" (ring-plan)} {}))
              [warn] (kinds-of seen :pen-build.leaky)]
          (is (= {"oak_fence" 3} (get-in result [:built :short])))
          (is (= {"oak_fence" 3} (:short warn)))
          (is (= :leak (:reason result)))
          (is (= 1 (count (kinds-of seen :pen-build.leaky)))))))))

;; ---------------------------------------------------------------- the check

(defn ^:async declines
  "Submit the job as a top-level job in the world, tick a few times: [places, pen-build.declined reasons, other events, list]."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen]} (b/start world-spec plans (if (= :none zones) nil (or zones [])))]
    (core/submit! eng (list job args) {})
    (dotimes [_ 4] (swap! h/clock + 700) (await (core/tick! eng)))
    {:places (count (h/calls p "place"))
     :declined (mapv #(select-keys % [:plan :reason]) (kinds-of seen :pen-build.declined))
     :done (count (kinds-of seen :pen-build.done))
     :listed (:list (core/state eng))}))

(deftest a-sound-pen-declines-and-nothing-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (declines (spec kit built-pen) {"pen" (ring-plan)} {:plan "pen"}))]
          (is (= 0 (:places r)))
          (is (= [] (:declined r)))
          (is (= 0 (:done r))))))))

(deftest a-built-but-leaky-pen-is-worked-so-the-leak-is-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (b/start (spec kit (dissoc built-pen "3,64,4")) {"pen" (ring-plan [[3 64 4]])} [])]
          (core/submit! eng (list job {:plan "pen"}) {})
          (dotimes [_ 6] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (= 1 (count (kinds-of seen :pen-build.leaky))))
          (is (= [{:pos {:x 3 :y 64 :z 4} :why :gap}] (:leaks (first (kinds-of seen :pen-build.leaky))))))))))

(deftest a-plan-it-cannot-work-declines-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone-plan {:id "pen" :parts [{:id "s" :cells [[2 64 2]] :want "stone"}]}]
          (doseq [[plans args zones reason]
                  [[{} {:plan "pen"} [] "no such plan"]
                   [{"pen" (ring-plan)} {:plan "pen" :part "nope"} [] "no cells to build"]
                   [{"pen" stone-plan} {:plan "pen"} [] "the plan has no fence, wall or gate cells"]
                   [{"pen" (ring-plan)} {:plan "pen"} :none "no zone list has been read"]]]
            (let [r (await (declines (spec kit) plans args zones))]
              (is (= [{:plan "pen" :reason reason}] (:declined r)))
              (is (= 0 (:places r))))))))))

(deftest a-broken-plan-declines-naming-the-error
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen w]} (b/start (spec kit) {} [])]
          (reset! (:state w) (assoc @(:state w) :plans {"pen" {:error "unreadable EDN: eof"}}))
          (core/submit! eng (list job {:plan "pen"}) {})
          (dotimes [_ 3] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (= [{:plan "pen" :reason "the plan cannot be read: unreadable EDN: eof"}]
                 (mapv #(select-keys % [:plan :reason]) (kinds-of seen :pen-build.declined)))))))))

(deftest nothing-carried-declines-through-the-builder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (spec []) {"pen" (ring-plan)} [])]
          (core/submit! eng (list job {:plan "pen" :fetch false}) {})
          (dotimes [_ 4] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (empty? (h/calls p "place")))
          (is (= 1 (count (kinds-of seen :build.declined)))))))))

;; ---------------------------------------------------------------- resuming

(deftest a-cut-mid-build-resumes-and-finishes-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (spec kit) {"pen" (ring-plan)} [])
              release (.hold (.-world p) "place")]
          (core/submit! eng (list job {:plan "pen"}) {})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= {:ok true} (takeover/take! eng {:who "claude" :why "test"})))
            (await round)
            (release))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (dotimes [_ 40] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (every? #(= "oak_fence" (h/block-at p (first %) (second %) (nth % 2))) fenced-ring))
          (is (= "oak_fence_gate" (h/block-at p 3 64 2)))
          (is (= 1 (count (kinds-of seen :pen-build.done))))
          (is (empty? (:list (core/state eng)))))))))
