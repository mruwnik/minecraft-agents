(ns engine.pen-build-test
  "jobs.build.pen: build a plan's fence and prove it holds, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.build-from-plan-test :as b]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.farm-contract-test :refer [ending-of kc]]
            [engine.harvest-test :as h]
            [engine.test-util :as tu]
            [jobs.build.pen :as pen]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pen-read :as pen-read]))

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
  "Run the job as a child over the world spec and plans: [data seen p ending]."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen] :as w} (b/start world-spec plans (or zones []))
        e (await (ending-of w job (merge {:plan "pen"} args) 300))]
    [(:data e) seen p e]))

(defn kinds-of [seen kind] (h/events-of seen kind))

;; ---------------------------------------------------------------- the box

(deftest the-box-is-the-bounding-box-of-the-barrier-cells
  (are [cells box] (= box (some-> (pen-read/barrier-box cells) (dissoc :cols)))
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
          (is (= 1 (count (kinds-of seen :completed)))))))))

;; ---------------------------------------------------------------- the contract

(deftest a-closed-pen-is-done-and-a-leaky-one-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ _ _ closed] (await (build! (spec kit) {"pen" (ring-plan)} {}))
              [_ _ _ leaky] (await (build! (spec kit) {"pen" (ring-plan [[3 64 4]])} {:ways #{:build}}))]
          (is (= {:kind :done :cause nil} (kc closed)))
          (is (true? (get-in closed [:data :closed?])))
          (is (= {:kind :nothing-possible :cause :leak} (kc leaky)))
          (is (= :leak (get-in leaky [:data :reason]))))))))

(deftest a-plan-without-fence-cells-is-impossible-and-no-zone-list-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone {:id "pen" :parts [{:id "s" :cells [[2 64 2]] :want "stone"}]}]
          (doseq [[plans zones kind reason]
                  [[{"pen" stone} [] :impossible :plan-trouble]
                   [{"pen" (ring-plan)} nil :condition-failed :no-zones]]]
            (let [e (await (ending-of (b/start (spec kit) plans zones) job {:plan "pen"} 10))]
              (is (= [kind reason] [(:kind e) (get-in e [:cause :reason])])))))))))

;; ---------------------------------------------------------------- the body ends outside

(defn body-cell [p] (mapv #(int (js/Math.floor %)) (get-in @(fake/state p) [:self :pos])))

(deftest a-body-inside-the-finished-pen-leaves-by-the-gate-and-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [world [(assoc (spec kit built-pen) :self {:pos [3.5 64 3.5]})
                       (assoc (spec kit) :self {:pos [3.5 64 3.5]})]]
          (let [{:keys [p] :as w} (b/start world {"pen" (ring-plan)} [])
                e (await (ending-of w job {:plan "pen"} 300))
                [x _ z] (body-cell p)]
            (is (= :done (:kind e)) (pr-str (kc e)))
            (is (not (and (<= 2 x 4) (<= 2 z 4))) (str "body at " (body-cell p)))
            (is (= "oak_fence_gate" (h/block-at p 3 64 2)))
            (is (true? (get-in e [:data :closed?])) "the gate is shut: an open gate is a leak")))))))

;; ---------------------------------------------------------------- leaks

(deftest a-fence-with-a-gap-builds-fails-the-check-and-names-the-leak
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ p] (await (build! (spec kit) {"pen" (ring-plan [[3 64 4]])} {:ways #{:build}}))]
          (is (= 7 (get-in result [:built :placed])) "everything the plan asks for is placed")
          (is (false? (:closed? result)))
          (is (= :leak (:reason result)))
          (is (= [{:pos {:x 3 :y 64 :z 4} :why :gap}] (:leaks result)))
          (is (= "air" (h/block-at p 3 64 4)) "nothing is placed that the plan does not ask for"))))))

;; Card 3f51b63a: without :ways #{} the same gap is patched.
(deftest a-gap-in-the-ring-is-patched-with-a-fence-of-the-plans-kind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ p] (await (build! (spec kit) {"pen" (ring-plan [[3 64 4]])} {}))]
          (is (true? (:closed? result)) (pr-str (:leaks result)))
          (is (= "oak_fence" (h/block-at p 3 64 4))))))))

;; The pen check lists at most 12 leaks: more gaps than that need more than one patch pass.
(deftest more-gaps-than-one-check-lists-are-patched-in-further-passes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ring (concat (for [x (range 1 12)] [x 64 11]) (for [z (range 10 0 -1)] [11 64 z]) (for [x (range 10 0 -1)] [x 64 1]) (for [z (range 2 11)] [1 64 z]))
              gate [6 64 1]
              gaps (take 16 (remove #{gate} (take-nth 2 (rest ring))))
              wide (into {} (for [x (range -2 14) z (range -2 14)] [(h/cell-key x 63 z) "stone"]))
              plan {:id "pen" :parts [{:id "fence" :cells (vec (remove (conj (set gaps) gate) ring)) :want "oak_fence"}
                                      {:id "gate" :cells [gate] :want {:block "oak_fence_gate" :facing :north}}]}
              [result _ p] (await (build! {:inventory [{:name "oak_fence" :count 50} {:name "oak_fence_gate" :count 1}] :blocks wide} {"pen" plan} {}))]
          (is (true? (:closed? result)) (pr-str (:leaks result)))
          (is (every? #(= "oak_fence" (h/block-at p (first %) 64 (nth % 2))) gaps)))))))

(deftest an-open-gate-is-a-leak
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world-spec (assoc (spec kit built-pen) :states {"3,64,2" {:open true :facing "north"}})
              [result] (await (build! world-spec {"pen" (ring-plan)} {}))]
          (is (= :leak (:reason result)))
          (is (= [:open-gate] (mapv :why (:leaks result)))))))))

;; ---------------------------------------------------------------- access and materials

(def zone b/zone)

(deftest a-cell-in-a-foreign-zone-is-built-the-plan-is-the-permission
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ p] (await (build! (spec kit) {"pen" (ring-plan)} {}
                                          [(zone "shrine" [4 64 3] [4 64 3] #{})]))]
          (is (= "oak_fence" (h/block-at p 4 64 3)))
          (is (true? (:closed? result))))))))

(deftest ignore-zones-builds-without-a-zone-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as w} (b/start (spec kit) {"pen" (ring-plan)} nil)
              result (:data (await (ending-of w job {:plan "pen" :ignore-zones? true} 300)))]
          (is (true? (:closed? result)))
          (is (= "oak_fence" (h/block-at p 4 64 3))))))))

(deftest a-cell-in-another-plans-footprint-is-reported-not-forced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as w} (b/start (spec kit) {"pen" (ring-plan) "other" b/other-plan} [])
              result (:data (await (ending-of w job {:plan "pen"} 300)))]
          (is (= "air" (h/block-at p 4 64 3)))
          (is (= :leak (:reason result)))
          (is (= [{:pos [4 64 3] :reason :footprint :plan "other"}] (get-in result [:built :refused]))))))))

(deftest too-little-fence-gives-up-naming-what-is-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (build! (spec [{:name "oak_fence" :count 4} {:name "oak_fence_gate" :count 1}])
                                      {"pen" (ring-plan)} {}))]
          (is (= {"oak_fence" 3} (get-in result [:built :short])))
          (is (= :leak (:reason result))))))))

;; ---------------------------------------------------------------- the check

(defn ^:async declines
  "Submit the job as a top-level job in the world, tick a few times: [places, pen-build.declined reasons, other events, list]."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen]} (b/start world-spec plans (if (= :none zones) nil (or zones [])))]
    (core/submit! eng (list job args) {})
    (await (tu/run-ticks eng h/clock 4 700))
    {:places (count (h/calls p "place"))
     :declined (mapv #(select-keys % [:plan :reason]) (kinds-of seen :pen-build.declined))
     :waits (mapv #(select-keys % [:reason :why]) (kinds-of seen :waiting))
     :done (count (kinds-of seen :completed))
     :listed (:list (core/state eng))}))

(deftest a-sound-pen-places-nothing-and-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (declines (spec kit built-pen) {"pen" (ring-plan)} {:plan "pen"}))]
          (is (= 0 (:places r)))
          (is (= [] (:declined r)))
          (is (= 1 (:done r)) "ended by done? before any round: one completion, nothing placed")
          (is (empty? (:listed r))))))))

(deftest a-built-but-leaky-pen-is-worked-so-the-leak-is-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (b/start (spec kit (dissoc built-pen "3,64,4")) {"pen" (ring-plan [[3 64 4]])} [])
              e (await (ending-of w job {:plan "pen" :ways #{:build}} 300))]
          (is (= :nothing-possible (:kind e)))
          (is (= [{:pos {:x 3 :y 64 :z 4} :why :gap}] (get-in e [:data :leaks]))))))))

(deftest a-plan-it-cannot-work-declines-without-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone-plan {:id "pen" :parts [{:id "s" :cells [[2 64 2]] :want "stone"}]}]
          (doseq [[plans args zones]
                  [[{} {:plan "pen"} []]
                   [{"pen" (ring-plan)} {:plan "pen" :part "nope"} []]
                   [{"pen" stone-plan} {:plan "pen"} []]
                   [{"pen" (ring-plan)} {:plan "pen"} :none]]]
            (let [r (await (declines (spec kit) plans args zones))]
              (is (empty? (:declined r)))
              (is (= 0 (:places r))))))))))

(deftest a-broken-plan-declines-without-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen w]} (b/start (spec kit) {} [])]
          (reset! (:state w) (assoc @(:state w) :plans {"pen" {:error "unreadable EDN: eof"}}))
          (core/submit! eng (list job {:plan "pen"}) {})
          (await (tu/run-ticks eng h/clock 3 700))
          (is (empty? (kinds-of seen :pen-build.declined))))))))

(deftest nothing-carried-and-no-fetch-ends-condition-failed-without-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as w} (b/start (spec []) {"pen" (ring-plan)} [])
              e (await (ending-of w job {:plan "pen" :ways #{:carried :patch}} 40))]
          (is (empty? (h/calls p "place")))
          (is (= [:condition-failed :no-items] [(:kind e) (get-in e [:cause :reason])]))
          (is (empty? (kinds-of seen :build.declined))))))))

(deftest a-builder-short-of-items-after-its-look-round-ends-the-pen-the-same
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as w} (b/start (spec []) {"pen" (ring-plan)} [])
              _ (tu/seeing-after-look p)
              e (await (ending-of w job {:plan "pen" :ways #{:carried :patch}} 100))]
          (is (empty? (h/calls p "place")))
          (is (= [:condition-failed :no-items {"oak_fence" 7 "oak_fence_gate" 1}] [(:kind e) (get-in e [:cause :reason]) (get-in e [:cause :needs])])))))))

(deftest the-default-fetch-explores-for-fences-that-are-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig ctx/call
              asked (atom [])
              {:keys [p] :as w} (b/start (spec [{:name "oak_fence_gate" :count 1}]) {"pen" (ring-plan)} [])]
          (tu/seeing-all p)
          (set! ctx/call (fn [c slot j args]
                           (when (= 'jobs.items.obtain j) (swap! asked conj args))
                           (orig c slot j args)))
          (try
            (await (ending-of w job {:plan "pen"} 300))
            (finally (set! ctx/call orig)))
          (is (some #(and (= "oak_fence" (:item %)) (true? (:explore %))) @asked) (pr-str @asked)))))))

(deftest an-unseen-gate-is-walked-to-not-waited-for-as-an-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "pen" :parts [{:id "gate" :cells [[3 64 2]] :want [:any "oak_fence_gate" "birch_fence_gate"]}]}
              {:keys [eng p seen]} (b/start {:inventory [{:name "birch_fence" :count 5}] :blocks ground :unloaded ["3,64,2"]} {"pen" plan} [])]
          (core/submit! eng (list job {:plan "pen" :ways #{:carried :patch}}) {})
          (await (tu/run-ticks eng h/clock 4 700))
          (is (seq (tu/walk-calls p)))
          (is (empty? (filter #(= :no-items (:reason %)) (kinds-of seen :waiting)))))))))

;; ---------------------------------------------------------------- resuming

(deftest an-unsensed-pen-is-walked-back-to-before-it-is-judged
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig pen-read/read!
              reads (atom 0)
              unseen {:closed? false :reason :unloaded :inside #{} :leaks [{:pos {:x 2 :y 64 :z 2} :why :unloaded}]}]
          (set! pen-read/read! (fn ([c opts] (if (< (swap! reads inc) 3) (js/Promise.resolve unseen) (orig c opts))) ([c opts r] (orig c opts r))))
          (try
            (let [[_ _ _ e] (await (build! (spec kit (dissoc built-pen "3,64,4")) {"pen" (ring-plan)} {}))]
              (is (= {:kind :done :cause nil} (kc e)) "once back in sight the pen is read, not judged unloaded"))
            (finally (set! pen-read/read! orig))))))))

(deftest a-cut-mid-build-resumes-and-finishes-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (spec kit) {"pen" (ring-plan)} [])
              release (.hold (.-world p) "place")]
          (core/submit! eng (list job {:plan "pen"}) {})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (core/cut! eng (core/holder eng) :test nil)
            (await round)
            (release))
          (await (tu/run-ticks eng h/clock 40 700))
          (is (every? #(= "oak_fence" (h/block-at p (first %) (second %) (nth % 2))) fenced-ring))
          (is (= "oak_fence_gate" (h/block-at p 3 64 2)))
          (is (= 1 (count (kinds-of seen :completed))))
          (is (empty? (:list (core/state eng)))))))))

(deftest a-pen-is-built-and-checked-in-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (b/start (spec kit) {"pen" (ring-plan)} [])
              result (:data (await (ending-of w job {:plan "pen"} 1)))]
          (is (true? (:closed? result))))))))

;; ---------------------------------------------------------------- uneven

(def two-wide-ring
  [{:pos [2 64 2] :want "oak_fence"} {:pos [3 64 2] :want "oak_fence"} {:pos [4 66 2] :want "oak_fence"}
   {:pos [3 64 4] :want {:block "oak_fence_gate" :facing :north}}])

(deftest steps-of-more-than-one-between-neighbouring-fences-are-uneven
  (are [cells top-of expected] (= expected (pen/uneven cells top-of))
    two-wide-ring (constantly nil) [{:why :step :at [3 64 2] :to [4 66 2] :step 2}]
    [{:pos [2 64 2] :want "oak_fence"} {:pos [3 65 2] :want "oak_fence"}] (constantly nil) []
    [{:pos [2 64 2] :want "oak_fence"} {:pos [3 70 2] :want "oak_fence"}] (constantly nil) [{:why :step :at [2 64 2] :to [3 70 2] :step 6}]
    (conj (mapv (fn [pos] {:pos pos :want "oak_fence"}) fenced-ring) {:pos [3 64 2] :want {:block "oak_fence_gate"}})
    (fn [_ z _] (if (= 3 z) 60 63))
    [{:why :gate-drop :gate [3 64 2] :inside [3 3] :drop 3}]
    (conj (mapv (fn [pos] {:pos pos :want "oak_fence"}) fenced-ring) {:pos [3 64 2] :want {:block "oak_fence_gate"}})
    (fn [_ _ _] 63) []))

(deftest a-gate-onto-a-trunk-is-uneven
  (let [cells (conj (mapv (fn [pos] {:pos pos :want "oak_fence"}) fenced-ring) {:pos [3 64 2] :want {:block "oak_fence_gate"}})
        problems (fn [blocks] (pen/uneven cells (fn [_ _ _] 63) (fn [x y z] (blocks [x y z]))))]
    (is (= [{:why :gate-blocked :gate [3 64 2] :outside [3 1]}] (problems {[3 64 1] "air" [3 65 1] "oak_log"})) "a log over the cell outside")
    (is (= [] (problems {})) "open ground")
    (is (= [] (problems {[3 64 1] "dirt" [3 65 1] "air" [3 66 1] "air"})) "ground one higher is a step up")))

(deftest a-pen-with-an-uneven-ring-waits-naming-the-steps-unless-allowed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "pen" :parts [{:id "fence" :cells [[2 64 2] [3 64 2] [4 66 2]] :want "oak_fence"}]}
              r (await (declines (spec kit) {"pen" plan} {:plan "pen"}))
              ok (await (declines (spec kit) {"pen" plan} {:plan "pen" :uneven? true}))]
          (is (empty? (:declined r)) "the check does not warn")
          (is (= [:uneven] (distinct (map :reason (:waits r)))) "the job waits listed")
          (is (= ["the pen is uneven (steps over 1): [3 64 2] to [4 66 2] steps 2"] (distinct (map :why (:waits r)))))
          (is (= 0 (:places r)))
          (is (= [] (:declined ok))))))))

(deftest trees-inside-the-box-are-cleared-before-the-fence-and-the-ground-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trees {"3,64,3" "oak_log" "3,65,3" "oak_log" "3,65,4" "oak_leaves"}
              [result seen p] (await (build! (spec kit trees) {"pen" (ring-plan)} {}))]
          (is (true? (:closed? result)))
          (is (= 0 (count (filter #(= :forestry/replant (:memory %)) (kinds-of seen :memory_written)))) "no sapling is owed inside the pen")
          (is (some #(= :fell-3-64-3 (:slot %)) (kinds-of seen :child_started)) "the trunk was felled by fell-tree")
          (is (every? #{nil "air"} (map #(h/block-at p 3 % 3) [64 65])))
          (is (contains? #{nil "air"} (h/block-at p 3 65 4)))
          (is (= "stone" (h/block-at p 3 63 3))))))))

(deftest the-bodys-own-leftover-shelter-block-beside-the-fence-is-taken-up-before-the-build
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as w} (b/start (spec (conj kit {:name "iron_pickaxe" :count 1}) {"5,64,3" "cobblestone" "5,65,3" "cobblestone" "3,64,1" "cobblestone" "3,65,1" "cobblestone"})
                                                  {"pen" (ring-plan)} [])
              entry (fn [cell] {:cell cell :item "cobblestone" :before "air" :job "j9" :purpose :shelter :state :placed})]
          (ledger/write! (:store eng) [(entry [5 64 3]) (entry [5 65 3]) (entry [3 64 1]) (entry [3 65 1])])
          (let [e (await (ending-of w job {:plan "pen"} 300))]
            (is (true? (:closed? (:data e))))
            (is (every? #{nil "air"} (concat (map #(h/block-at p 5 % 3) [64 65]) (map #(h/block-at p 3 % 1) [64 65]))) "the shelter blocks beside the fence and in front of the gate are gone, with no weapon carried")
            (is (some #(= :tidy (:slot %)) (kinds-of seen :child_started)))))))))
