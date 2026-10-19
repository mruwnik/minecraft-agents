(ns engine.farm-tend-plan-test
  "jobs.farm.tend working a plan (:plan id, optional :part) against the fake world: two crops in one plan, each sown
  where the plan names it, till then sow, wrong crops left and reported, zones and footprints, declines."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.farm.tend-plan :as tend-plan]
            [jobs.lib.world-files :as world]))

(def job 'jobs.farm.tend)

(defn k [x y z] (str x "," y "," z))

(defn world-of [& parts]
  (apply merge-with (fn [a b] (if (map? a) (merge a b) b)) parts))

(defn ground [name cells] {:blocks (into {} (map (fn [[x z]] [(k x 63 z) name])) cells)})

(defn crops [name age cells]
  {:blocks (into {} (map (fn [[x z]] [(k x 64 z) name])) cells)
   :ages (into {} (map (fn [[x z]] [(k x 64 z) age])) cells)})

(defn item [name n] {:name name :count n})

(def wheat-cells [[2 2] [3 2]])
(def carrot-cells [[2 3] [3 3]])

(defn plan-of
  "A plan with wheat over wheat-cells and carrots over carrot-cells at y 64."
  []
  {:id "mix"
   :parts [{:id "wheat" :cells (mapv (fn [[x z]] [x 64 z]) wheat-cells) :want {:crop "wheat"}}
           {:id "carrots" :cells (mapv (fn [[x z]] [x 64 z]) carrot-cells) :want {:crop "carrots"}}]})

(def farmland-all (ground "farmland" (concat wheat-cells carrot-cells)))

(def all-seed {:inventory [(item "wheat_seeds" 6) (item "carrot" 6) (item "bread" 12)]})

(defn setup
  ([spec plans zones] (setup spec plans zones registry/jobs))
  ([spec plans zones jobs]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor spec))
        w (world/of-data plans {} zones)
        eng (core/create {:primitives p :jobs jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                          :world w})]
    {:eng eng :p p :seen seen :clock clock :w w})))

(defn ^:async run
  "Submit tend with args over a world and run n ticks 700 ms apart; the setup map."
  ([args spec plans n] (run args spec plans [] n))
  ([args spec plans zones n]
   (let [s (setup spec plans zones)]
     (core/submit! (:eng s) (list job args) {})
     (await (tu/run-ticks (:eng s) (:clock s) n 700))
     s)))

(defn calls [{:keys [p]} name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn pos-of [call] (let [pos (.-pos (.-args call))] [(.-x pos) (.-y pos) (.-z pos)]))
(defn placed [s] (into {} (map (fn [c] [(pos-of c) (.-item (.-args c))])) (calls s "place")))
(defn dug [s] (set (map pos-of (calls s "dig"))))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-events [s] (filterv #(#{:completed :stopped} (:kind %)) @(:seen s)))
(defn done-event [s] (first (done-events s)))
(defn step [s name] (get-in (done-event s) [:steps name]))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn block-at [{:keys [p]} x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-name)))
(defn age-at [{:keys [p]} x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-age)))

(defn untouched?
  "True when the job is submitted, ticks a few times and the body is never asked to do anything."
  ([args spec plans] (untouched? args spec plans []))
  ([args spec plans zones]
   (let [{:keys [eng p]} (setup spec plans zones)]
     (core/submit! eng (list job args) {})
     (core/tick! eng)
     (empty? (.-calls (.-world p))))))

;; ---------------------------------------------------------------- scenarios

(deftest two-crops-in-one-plan-are-each-sown-in-their-own-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"} (world-of farmland-all all-seed) {"mix" (plan-of)} 60))]
          (is (= {[2 64 2] "wheat_seeds" [3 64 2] "wheat_seeds" [2 64 3] "carrot" [3 64 3] "carrot"} (placed s)))
          (is (= ["wheat" "wheat" "carrots" "carrots"] (mapv (fn [[x z]] (block-at s x 64 z)) (concat wheat-cells carrot-cells))))
          (is (= {:crops 4 :bare 0 :untilled 0 :wrong []} (:field (done-event s))))
          (is (true? (finished? s))))))))

(deftest parts-work-several-parts-of-the-plan-in-one-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix" :parts ["wheat" "carrots"]} (world-of farmland-all all-seed) {"mix" (plan-of)} 80))
              ev (done-event s)]
          (is (= {[2 64 2] "wheat_seeds" [3 64 2] "wheat_seeds" [2 64 3] "carrot" [3 64 3] "carrot"} (placed s)))
          (is (= ["wheat" "carrots"] (keys (:parts ev))) "one report per part, in order")
          (is (= 1 (count (done-events s))))
          (is (true? (finished? s))))))))

(deftest parts-limit-the-job-to-the-named-parts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix" :parts ["carrots"]} (world-of farmland-all all-seed) {"mix" (plan-of)} 60))]
          (is (= #{[2 64 3] [3 64 3]} (set (keys (placed s))))))))))

(defn pass-ends
  "The child_ended events of finished tend passes (their ending data has :steps)."
  [s]
  (filterv #(contains? (ce/data %) :steps) (filter #(= :child_ended (:kind %)) @(:seen s))))

(defn ^:async run-standing
  "Submit (repeat (tend args)) over the world, tick n times 700 ms apart; the setup map."
  [args spec plans n]
  (let [s (setup spec plans [])]
    (core/submit! (:eng s) (list 'repeat (list job args)) {})
    (dotimes [_ n]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    s))

(def stuck-wheat
  (world-of farmland-all (crops "wheat" 7 wheat-cells) {:unreachable ["2,64,2" "3,64,2"] :inventory [(item "wheat_seeds" 6) (item "bread" 12)]}))

(deftest a-part-whose-pass-leaves-its-check-true-is-tried-once-per-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-standing {:plan "mix" :part "wheat"} stuck-wheat {"mix" (plan-of)} 80))]
          (is (<= 2 (count (pass-ends s))) "no rest: the next run tries the part again")
          (is (empty? (filter :backed-off (events-of s :waiting))) "no wait names a back-off"))))))

(deftest a-missing-seed-leaves-its-cells-bare-with-one-note-and-the-other-crop-is-still-sown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"} (world-of farmland-all {:inventory [(item "wheat_seeds" 6)]}) {"mix" (plan-of)} 60))]
          (is (= #{[2 64 2] [3 64 2]} (set (keys (placed s)))))
          (is (= ["air" "air"] (mapv #(block-at s % 64 3) [2 3])))
          (let [notes (events-of s :farm-tend.short-seed)]
            (is (= 1 (count notes)))
            (is (= "carrot" (:seed (first notes)))))
          (is (true? (finished? s))))))))

(deftest a-missing-seed-alone-starts-no-run-and-the-wait-says-so-without-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"} (world-of (ground "farmland" carrot-cells) {:inventory [(item "wheat_seeds" 6)]})
                            {"mix" (plan-of)} 6))]
          (is (empty? (.-calls (.-world (:p s)))))
          (is (= [] (events-of s :farm-tend.short-seed)) "the check writes no warn")
          (is (= [[:short-seed ["carrot"]]] (distinct (map (juxt :reason :seeds) (events-of s :waiting))))))))))

(deftest the-check-reads-the-field-once-per-window
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              real tend-plan/plan-facts]
          (with-redefs [tend-plan/plan-facts (fn [c] (swap! n inc) (real c))]
            (await (run {:plan "mix"} (world-of (ground "farmland" carrot-cells) {:inventory [(item "wheat_seeds" 6)]})
                        {"mix" (plan-of)} 8)))
          (is (< 0 @n 8) "eight ticks, fewer than eight readings"))))))

(deftest planned-dirt-is-tilled-then-sown-with-the-planned-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"}
                            (world-of (ground "farmland" [[3 2]]) (ground "dirt" [[2 2] [2 3]]) (ground "farmland" [[3 3]])
                                      {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 6) (item "carrot" 6) (item "bread" 12)]})
                            {"mix" (plan-of)} 100))]
          (is (= {:tilled 2} (step s :till)))
          (is (= ["farmland" "farmland"] (mapv #(block-at s 2 63 %) [2 3])))
          (is (= {[2 64 2] "wheat_seeds" [3 64 2] "wheat_seeds" [2 64 3] "carrot" [3 64 3] "carrot"} (placed s)))
          (is (= 2 (count (calls s "useOn"))))
          (is (true? (finished? s))))))))

(deftest ground-the-plan-names-as-something-else-is-not-tilled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (update (plan-of) :parts conj {:id "path" :cells [[2 63 2]] :want "dirt"})
              s (await (run {:plan "mix"} (world-of (ground "dirt" [[2 2]]) (ground "farmland" [[3 2]])
                                                    {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 6)]})
                            {"mix" plan} 60))]
          (is (empty? (calls s "useOn")))
          (is (= "dirt" (block-at s 2 63 2)))
          (is (= #{[3 64 2]} (set (keys (placed s))))))))))

(deftest a-ripe-crop-is-cut-and-the-cell-sown-again-with-the-planned-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"}
                            (world-of farmland-all (crops "wheat" 7 [[2 2]]) (crops "carrots" 7 [[2 3]])
                                      {:inventory [(item "wheat_seeds" 6) (item "carrot" 6)] :drops {"wheat" ["wheat" "wheat_seeds"] "carrots" ["carrot"]}})
                            {"mix" (plan-of)} 100))]
          (is (= #{[2 64 2] [2 64 3]} (dug s)))
          (is (= ["wheat" "carrots"] [(block-at s 2 64 2) (block-at s 2 64 3)]))
          (is (= 2 (:cut (step s :harvest))))
          (is (= {[2 64 2] "wheat_seeds" [3 64 2] "wheat_seeds" [2 64 3] "carrot"} (placed s)))
          (is (true? (finished? s))))))))

(deftest a-crop-of-another-kind-in-a-crop-cell-is-left-standing-and-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"}
                            (world-of (ground "farmland" [[3 2] [2 3] [3 3]]) (crops "carrots" 7 [[2 2]])
                                      {:inventory [(item "wheat_seeds" 6) (item "carrot" 6) (item "bread" 12)] :drops {"carrots" ["carrot"]}})
                            {"mix" (plan-of)} 60))]
          (is (empty? (dug s)))
          (is (= ["carrots" 7] [(block-at s 2 64 2) (age-at s 2 64 2)]))
          (is (not (contains? (placed s) [2 64 2])))
          (is (= [{:pos [2 64 2] :found "carrots" :want "crop wheat"}] (:wrong (:field (done-event s)))))
          (is (= 1 (count (events-of s :farm-tend.wrong))))
          (is (true? (finished? s))))))))

(deftest cells-outside-the-plan-are-untouched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [outside (world-of (ground "dirt" [[8 2]]) (ground "farmland" [[8 3] [9 3]]) (crops "wheat" 7 [[9 3]]))
              s (await (run {:plan "mix"}
                            (world-of (ground "farmland" [[2 2]]) outside
                                      {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 9)] :drops {"wheat" ["wheat"]}})
                            {"mix" (plan-of)} 60))
              touched (concat (calls s "dig") (calls s "place") (calls s "useOn"))]
          (is (seq touched))
          (is (empty? (filter #(= 8 (first (pos-of %))) touched)))
          (is (empty? (filter #(= 9 (first (pos-of %))) touched)))
          (is (= ["dirt" "farmland" "air"] [(block-at s 8 63 2) (block-at s 8 63 3) (block-at s 8 64 3)]))
          (is (= 7 (age-at s 9 64 3))))))))

(deftest a-part-limits-the-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix" :part "carrots"} (world-of farmland-all all-seed) {"mix" (plan-of)} 60))]
          (is (= #{[2 64 3] [3 64 3]} (set (keys (placed s)))))
          (is (= ["air" "air"] (mapv #(block-at s % 64 2) [2 3]))))))))

(defn foreign-plan
  "Another plan claiming cells, a footprint that refuses every builder of a different plan."
  [cells]
  {:id "other" :parts [{:id "o" :cells (vec cells) :want "stone"}]})

(deftest another-plan-over-part-of-the-field-keeps-the-body-from-sowing-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"} (world-of farmland-all all-seed)
                            {"mix" (plan-of) "other" (foreign-plan [[3 64 2] [3 64 3]])} 60))]
          (is (= #{[2 64 2] [2 64 3]} (set (keys (placed s)))))
          (is (= ["air" "air"] (mapv #(block-at s 3 64 %) [2 3])))
          (is (true? (finished? s))))))))

(deftest a-foreign-zone-over-the-field-is-no-obstacle-the-plan-is-the-permission
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep-out" :min [3 60 2] :max [3 70 3] :owner "x" :allow #{}}
              s (await (run {:plan "mix"} (world-of farmland-all all-seed) {"mix" (plan-of)} [zone] 60))]
          (is (= #{[2 64 2] [3 64 2] [2 64 3] [3 64 3]} (set (keys (placed s))))))))))

(deftest a-zone-allowing-place-and-dig-lets-the-field-be-worked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "farm" :min [0 60 0] :max [9 70 9] :owner "x" :allow #{:place :dig}}
              s (await (run {:plan "mix"} (world-of farmland-all all-seed) {"mix" (plan-of)} [zone] 60))]
          (is (= 4 (count (placed s)))))))))

(deftest another-active-plans-footprint-is-not-sown-over-but-its-own-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [other {:id "other" :parts [{:id "hut" :cells [[3 64 2]] :want "oak_planks"}]}
              s (await (run {:plan "mix"} (world-of farmland-all all-seed) {"mix" (plan-of) "other" other} 60))]
          (is (= #{[2 64 2] [2 64 3] [3 64 3]} (set (keys (placed s)))))
          (is (= "air" (block-at s 3 64 2))))))))

(deftest a-zone-over-planned-dirt-keeps-the-hoe-off-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep-out" :min [2 60 2] :max [2 70 2] :owner "x" :allow #{}}
              s (await (run {:plan "mix"} (world-of (ground "dirt" [[2 2]]) (ground "farmland" [[3 2]])
                                                    {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 6)]})
                            {"mix" (plan-of)} [zone] 60))]
          (is (empty? (calls s "useOn")))
          (is (= "dirt" (block-at s 2 63 2)))
          (is (= #{[3 64 2]} (set (keys (placed s))))))))))

(deftest a-zone-over-the-only-ground-to-till-is-named-in-the-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep-out" :min [2 60 2] :max [2 70 2] :owner "x" :allow #{}}
              s (await (run {:plan "mix"} (world-of (ground "dirt" [[2 2]])
                                                    {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 6)]})
                            {"mix" (plan-of)} [zone] 6))
              waits (events-of s :waiting)]
          (is (empty? (calls s "useOn")))
          (is (= [{:reason :nothing-to-do :refused 1 :zones ["keep-out"] :owners ["x"]}]
                 (distinct (map #(select-keys % [:reason :refused :zones :owners]) waits)))))))))

(deftest tend-passes-the-zone-opt-out-to-its-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[extra hoed] [[{} "dirt"] [{:ignore-zones? true} "farmland"]]]
          (let [zone {:name "keep-out" :min [2 60 2] :max [2 70 2] :owner "x" :allow #{}}
                s (await (run (merge {:plan "mix"} extra)
                              (world-of (ground "dirt" [[2 2]]) (ground "farmland" [[3 2]])
                                        {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 6)]})
                              {"mix" (plan-of)} [zone] 60))]
            (is (= hoed (block-at s 2 63 2)) (pr-str extra))))))))

(deftest a-plan-deleted-mid-job-is-not-worked-in-the-next-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup (world-of farmland-all {:inventory [(item "wheat_seeds" 6) (item "carrot" 6) (item "bread" 12)]}) {"mix" (plan-of)} [])
              deleted (atom false)]
          (.override (.-world (:p s)) "place"
                     (fn ^:async f [token args impl]
                       (when-not @deleted
                         (reset! deleted true)
                         (world/set-data! (:w s) {} {}))
                       (await (impl token args))))
          (core/submit! (:eng s) (list job {:plan "mix"}) {})
          (swap! (:clock s) + 700)
          (await (core/tick! (:eng s)))
          (let [before (count (calls s "place"))]
            (await (tu/run-ticks (:eng s) (:clock s) 20 700))
            (is (pos? before))
            (is (= before (count (calls s "place"))) "nothing is placed after the plan was deleted")
            (is (empty? (filterv #(= :completed (:kind %)) (done-events s))))))))))

;; ---------------------------------------------------------------- declines

(deftest the-job-declines-and-touches-nothing-when-the-plan-cannot-be-worked
  (let [spec (world-of farmland-all all-seed)]
    (is (untouched? {:plan "mix"} spec {}) "no such plan")
    (is (untouched? {:plan "mix" :part "nope"} spec {"mix" (plan-of)}) "no cells in the part")
    (is (untouched? {:plan "mix"} spec {"mix" {:id "mix" :parts [{:id "hut" :cells [[2 64 2]] :want "oak_planks"}]}})
        "no crop cells")
    (is (untouched? {:plan "mix"} spec {"mix" (plan-of)} nil) "no zone list has been read")))

(deftest a-decline-is-warned-once-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"} (world-of farmland-all all-seed) {} 5))
              warns (events-of s :farm-tend.declined)]
          (is (= 1 (count warns)))
          (is (= "mix" (:plan (first warns))))
          (is (= "no such plan" (:reason (first warns)))))
        (let [s (await (run {:plan "mix"} (world-of farmland-all all-seed) {"mix" (plan-of)} nil 5))
              warns (events-of s :farm-tend.declined)]
          (is (= 1 (count warns)))
          (is (re-find #"zone" (:reason (first warns)))))))))

(deftest nothing-to-do-over-a-plan-declines
  (is (untouched? {:plan "mix"} (world-of (ground "farmland" wheat-cells) (crops "wheat" 3 wheat-cells) {:inventory [(item "wheat_seeds" 6)]})
                  {"mix" (plan-of)}) "young crops only")
  (is (untouched? {:plan "mix"} (world-of (ground "dirt" wheat-cells) {:inventory [(item "wheat_seeds" 6)]}) {"mix" (plan-of)})
      "dirt without a hoe"))

;; ---------------------------------------------------------------- the children over a plan

(defn ^:async child-result
  "Run job with args as the child of a recording parent over a world; [result calls-of-body setup]."
  [job-ns args spec plans zones n]
  (let [s (setup spec plans zones)
        out (atom :not-done)
        parent (tu/recording-parent job-ns args out)
        eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-ticks eng (:clock s) n 700))
    [@out s]))

(deftest plant-over-a-plan-names-what-it-refused-and-what-it-lacked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r s] (await (child-result 'jobs.farm.plant {:plan "mix"} (world-of farmland-all {:inventory [(item "wheat_seeds" 6)]})
                                         {"mix" (plan-of) "other" (foreign-plan [[3 64 2]])} [] 20))]
          (is (= {:planted 1 :skipped [] :refused [{:pos {:x 3 :y 64 :z 2} :reason :footprint}] :short ["carrot"] :status :done} r))
          (is (= #{[2 64 2]} (set (keys (placed s))))))))))

(deftest plant-over-a-plan-asks-the-rules-again-right-before-the-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "mix" :parts [{:id "far" :cells [[8 64 0]] :want {:crop "wheat"}}]}
              s (setup (world-of (ground "farmland" [[8 0]]) {:inventory [(item "wheat_seeds" 3)]}) {"mix" plan} [])
              late (foreign-plan [[8 64 0]])]
          (.override (.-world (:p s)) "steer" (fn [token args impl] (world/set-data! (:w s) {"mix" plan "other" late} {}) (impl token args)))
          (core/submit! (:eng s) (list 'jobs.farm.plant {:plan "mix"}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 6 700))
          (is (seq (tu/walk-calls (:p s))))
          (is (empty? (calls s "place")))
          (is (= "air" (block-at s 8 64 0))))))))

(deftest till-for-a-plan-skips-cells-the-rules-refuse-and-checks-again-before-the-hoe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep-out" :min [3 60 2] :max [3 70 2] :owner "x" :allow #{}}
              [r s] (await (child-result 'jobs.farm.till {:from {:x 2 :y 63 :z 2} :to {:x 3 :y 63 :z 2} :for-plan "mix"}
                                         (world-of (ground "dirt" wheat-cells) {:inventory [(item "stone_hoe" 1)]})
                                         {"mix" (plan-of)} [zone] 20))]
          (is (= {:tilled 1 :skipped {{:x 3 :y 63 :z 2} :not-permitted}} (dissoc r :status :text)))
          (is (= ["farmland" "dirt"] [(block-at s 2 63 2) (block-at s 3 63 2)])))
        (let [plan {:id "mix" :parts [{:id "far" :cells [[8 64 0]] :want {:crop "wheat"}}]}
              s (setup (world-of (ground "dirt" [[8 0]]) {:inventory [(item "stone_hoe" 1)]}) {"mix" plan} [])
              zone {:name "late" :min [8 60 0] :max [8 70 0] :owner "x" :allow #{}}]
          (.override (.-world (:p s)) "steer" (fn [token args impl] (world/set-zones! (:w s) [zone]) (impl token args)))
          (core/submit! (:eng s) (list 'jobs.farm.till {:from {:x 8 :y 63 :z 0} :to {:x 8 :y 63 :z 0} :for-plan "mix"}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 6 700))
          (is (seq (tu/walk-calls (:p s))))
          (is (empty? (calls s "useOn")))
          (is (= "dirt" (block-at s 8 63 0))))))))

(deftest till-for-a-plan-declines-without-a-zone-list
  (let [{:keys [eng p seen]} (setup (world-of (ground "dirt" [[2 2]]) {:inventory [(item "stone_hoe" 1)]}) {"mix" (plan-of)} nil)]
    (core/submit! eng (list 'jobs.farm.till {:from {:x 2 :y 63 :z 2} :to {:x 2 :y 63 :z 2} :for-plan "mix"}) {})
    (core/tick! eng)
    (is (empty? (.-calls (.-world p))))
    (is (empty? (filter #(= :till.skipped (:kind %)) @seen)) "the round never ran")))

(deftest tend-asks-the-rules-again-before-it-tills-or-sows-a-far-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "mix" :parts [{:id "far" :cells [[8 64 0] [8 64 1]] :want {:crop "wheat"}}]}
              late (foreign-plan [[8 64 0] [8 64 1]])
              ground-zone {:name "late" :min [8 63 0] :max [8 63 1] :owner "x" :allow #{}}
              s (setup (world-of (ground "dirt" [[8 0]]) (ground "farmland" [[8 1]])
                                 {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 5)]}) {"mix" plan} [])]
          (.override (.-world (:p s)) "steer" (fn [token args impl] (world/set-data! (:w s) {"mix" plan "other" late} {})
                                                       (world/set-zones! (:w s) [ground-zone])
                                                       (impl token args)))
          (core/submit! (:eng s) (list job {:plan "mix"}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 30 700))
          (is (seq (tu/walk-calls (:p s))))
          (is (empty? (calls s "useOn")))
          (is (empty? (calls s "place")))
          (is (= ["dirt" "air" "air"] [(block-at s 8 63 0) (block-at s 8 64 0) (block-at s 8 64 1)])))))))

(deftest tend-asks-the-rules-before-the-till-child-is-called
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "keep-out" :min [2 60 2] :max [2 70 2] :owner "x" :allow #{}}
              s (await (run {:plan "mix"} (world-of (ground "dirt" [[2 2]]) (ground "farmland" [[3 2]])
                                                    {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 6)]})
                            {"mix" (plan-of)} [zone] 60))]
          (is (empty? (events-of s :till.skipped)) "the till child never saw the cell"))))))

(deftest a-cut-cell-in-a-zone-that-forbids-placing-is-sown-again-the-plan-is-the-permission
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "no-sowing" :min [3 60 2] :max [3 70 2] :owner "x" :allow #{:dig}}
              s (await (run {:plan "mix" :part "wheat"}
                            (world-of farmland-all (crops "wheat" 7 [[3 2]])
                                      {:inventory [(item "wheat_seeds" 6)] :drops {"wheat" ["wheat" "wheat_seeds"]}})
                            {"mix" (plan-of)} [zone] 60))]
          (is (= #{[3 64 2]} (dug s)))
          (is (= ["wheat" "wheat"] [(block-at s 2 64 2) (block-at s 3 64 2)]))
          (is (= #{[2 64 2] [3 64 2]} (set (keys (placed s))))))))))

(deftest the-seed-of-a-crop-caps-the-beds-made-for-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix"} (world-of (ground "dirt" wheat-cells) {:inventory [(item "stone_hoe" 1) (item "wheat_seeds" 1)]})
                            {"mix" (plan-of)} 80))]
          (is (= {:tilled 1} (step s :till)))
          (is (= 1 (count (calls s "useOn"))))
          (is (= 1 (count (placed s))))
          (is (= 1 (get-in (done-event s) [:field :untilled]))))))))

(deftest a-ripe-crop-outside-the-part-is-left-standing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix" :part "carrots"}
                            (world-of farmland-all (crops "wheat" 7 [[2 2]]) (crops "carrots" 7 [[2 3]])
                                      {:inventory [(item "carrot" 6) (item "bread" 12)] :drops {"wheat" ["wheat"] "carrots" ["carrot"]}})
                            {"mix" (plan-of)} 60))]
          (is (= #{[2 64 3]} (dug s)))
          (is (= ["wheat" 7] [(block-at s 2 64 2) (age-at s 2 64 2)])))))))

(deftest plant-over-a-plan-sows-a-food-crop-only-above-the-reserve-within-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r s] (await (child-result 'jobs.farm.plant {:plan "mix" :part "carrots"} (world-of farmland-all {:inventory [(item "carrot" 21)]})
                                         {"mix" (plan-of)} [] 20))]
          (is (= 1 (:planted r)))
          (is (= 1 (count (placed s)))))))))

(def reserve-only {:inventory [(item "stone_hoe" 1) (item "carrot" 3)]})

(deftest dry-cells-whose-crop-the-reserve-keeps-are-not-tilled-and-the-report-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix" :part "carrots"} (world-of (ground "dirt" [[2 3]]) (ground "farmland" [[3 3]]) (crops "carrots" 7 [[3 3]])
                                      {:inventory (:inventory reserve-only) :drops {"carrots" ["carrot"]}})
                            {"mix" (plan-of)} 60))]
          (is (empty? (calls s "useOn")))
          (is (= {:skipped :food-reserve :seed "carrot" :held 3 :kept 3 :reserve-points 60}
                 (select-keys (step s :till) [:skipped :seed :held :kept :reserve-points]))))))))

(deftest wet-cells-are-tilled-though-the-reserve-keeps-their-seed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run {:plan "mix" :part "carrots"}
                            (world-of (ground "dirt" carrot-cells) {:blocks {(k 7 64 3) "water"}} reserve-only)
                            {"mix" (plan-of)} 80))]
          (is (= {:tilled 1} (step s :till)) "only the cell within 4 blocks of the water")
          (is (= ["farmland" "dirt"] (mapv #(block-at s % 63 3) [3 2]))))))))
