(ns engine.get-seeds-test
  "jobs.gather.get-seeds against the fake world."
  (:require
            [engine.child-ends :as ce]
            [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup-seeing
  "h/setup over a body that has seen every block in range."
  [world]
  (update (h/setup world) :p tu/seeing-all))

(defn spec [args] (list 'jobs.gather.get-seeds args))

(defn patch
  "{\"x,64,z\" block} for the grass cells x in xs, z in zs."
  [block xs zs]
  (into {} (for [x xs z zs] [(str x ",64," z) block])))

(def seed-drops {"short_grass" "wheat_seeds"})

(defn ^:async run-ticks
  [{:keys [eng clock]} n step]
  (tu/run-ticks eng clock n step))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (setup-seeing world)]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event "The root ending's event." [{:keys [seen]}] (first (filter #(#{:completed :stopped} (:kind %)) @seen)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))
(defn job-mem [{:keys [eng]}] (core/job-memory eng "j1"))
(defn ending-of
  "{:kind :completed|:stopped :reason cause} of the job's end event."
  [{:keys [seen]}]
  (select-keys (first (filter #(#{:completed :stopped} (:kind %)) @seen)) [:kind :reason]))
(defn dig-count [{:keys [p]}] (count (h/calls p "dig")))

;; ------------------------------------------------------------------ the job

(deftest the-end-is-done-only-with-the-goal-else-nothing-possible-with-the-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args world expected] [[{:count 2} {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops} {:kind :completed :reason :count}]
                                       [{} {:blocks {"2,64,0" "dirt"}} {:kind :stopped :reason :none}]
                                       [{:settings {:jobs.gather.get-seeds/dry-digs 6}} {:blocks (patch "short_grass" (range 2 12) [0]) :drops {"short_grass" nil}} {:kind :stopped :reason :dry}]
                                       [{:count 64} {:blocks (patch "short_grass" [2 3 4] [0]) :drops seed-drops} {:kind :stopped :reason :none}]
                                       ]]
          (let [s (await (scenario args world 40))]
            (is (= expected (ending-of s)) (pr-str args))))))))

(deftest breaks-grass-until-the-seeds-are-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2} {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops} 40))]
          (is (>= (get (inv s) "wheat_seeds") 2))
          (is (= :count (:reason (done-event s))))
          (is (>= (:got (done-event s)) 2))
          (is (finished? s)))))))

(deftest a-round-has-no-step-cap-of-its-own
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2} {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops} 1))]
          (is (finished? s)))))))

(deftest every-dig-goes-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2} {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops} 40))]
          (is (= (dig-count s) (count (ce/ends s :dug)))))))))

(deftest the-count-is-what-is-carried-in-all
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {:inventory [{:name "wheat_seeds" :count 5}]
                          :blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops})]
          (tu/shutdown-at! s "dig" 1)
          (core/submit! (:eng s) (spec {:count 7}) {})
          (await (run-ticks s 1 700))
          (is (= 7 (:goal (job-mem s))))
          (await (run-ticks s 40 700))
          (is (>= (get (inv s) "wheat_seeds") 7))
          (is (= :count (:reason (done-event s))))
          (is (finished? s)))))))

(deftest the-check-declines-without-a-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async [[{} {} "nothing at all"]
                               [{} {:blocks {"2,64,0" "dirt"}} "other blocks"]
                               [{:radius 5} {:blocks {"90,64,0" "short_grass"}} "grass beyond the far radius"]]
                              (fn ^:async one [[args world note]]
                                (let [s (setup-seeing world)]
                                  (core/submit! (:eng s) (spec args) {})
                                  (await (run-ticks s 10 700))
                                  (is (zero? (count (tu/walked-to (:eng s)))) note)
                                  (is (zero? (dig-count s)) note)))))))))

(deftest grass-never-seen-is-not-a-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (update (setup-seeing {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops}) :p tu/blind)]
          (core/submit! (:eng s) (spec {:count 2}) {})
          (await (run-ticks s 10 700))
          (is (zero? (dig-count s))))))))

(deftest no-source-looks-once-and-ends-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:blocks {"2,64,0" "dirt"}} 10))]
          (is (= :none (:reason (done-event s))))
          (is (= 8 (count (h/calls (:p s) "look"))) "exactly one look-around (8 glances) before concluding there is nothing")
          (is (zero? (dig-count s))))))))

(deftest grass-seen-beyond-the-radius-is-walked-to-and-broken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:radius 10 :count 2} {:blocks (patch "short_grass" [30 31 32] [0 1]) :drops seed-drops} 60))]
          (is (= {:kind :completed :reason :count} (ending-of s)))
          (is (pos? (dig-count s))))))))

(deftest grass-beyond-a-radius-under-the-reach-is-walked-to-until-it-is-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:radius 1 :count 1} {:blocks (patch "short_grass" [30 31 32] [0 1]) :drops seed-drops} 60))]
          (is (= {:kind :completed :reason :count} (ending-of s)))
          (is (pos? (dig-count s))))))))

(deftest the-none-text-without-any-grass-seen-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:blocks {"2,64,0" "dirt"}} 10))
              text (:text (done-event s))]
          (is (re-find #"no short_grass/tall_grass within 16" text) text)
          (is (not (re-find #"nearest" text)) text))))))

(deftest grass-that-drops-nothing-ends-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:settings {:jobs.gather.get-seeds/dry-digs 6}} {:blocks (patch "short_grass" (range 2 12) [0]) :drops {"short_grass" nil}} 60))]
          (is (= :dry (:reason (done-event s))))
          (is (<= 6 (dig-count s) 10))
          (is (= 0 (:got (done-event s))))
          (is (finished? s)))))))

;; Changed from ending :barren: a batch whose targets were all skipped is not barren;
;; the job ends :none once every source in radius is skipped.
(deftest unreachable-grass-is-skipped-and-the-job-ends-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (patch "short_grass" (range 6 12) [0])
              s (await (scenario {:settings {:jobs.gather.get-seeds/per-round 3}} {:blocks cells :drops seed-drops :unreachable (vec (keys cells))} 40))]
          (is (= :none (:reason (done-event s))))
          (is (<= (count (tu/walked-to (:eng s))) 6) "each cell is tried once")
          (is (zero? (dig-count s)))
          (is (finished? s)))))))

(deftest a-body-shut-in-ends-shut-in-after-one-walk-instead-of-skipping-every-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (patch "short_grass" (range 6 12) (range 0 10))
              walks (atom 0)
              s (await (tu/with-near-stub (fn ^:async f [c _ _ _]
                                            (swap! walks inc)
                                            (ctx/update-mem! c assoc :near-walk {:data {:why :exhausted :shut-in true}})
                                            :blocked)
                         (fn ^:async b [] (await (scenario {} {:blocks cells :drops seed-drops} 20)))))]
          (is (= :shut-in (:reason (done-event s))))
          (is (= 1 @walks) "not one failed walk per grass block")
          (is (empty? (:skipped (job-mem s))) "no block is blamed for the body's trouble")
          (is (finished? s)))))))

(deftest too-few-blocks-end-none-with-the-partial-got
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 64} {:blocks (patch "short_grass" [2 3 4] [0]) :drops seed-drops} 40))]
          (is (= 3 (dig-count s)))
          (is (= :none (:reason (done-event s))))
          (is (= 3 (:got (done-event s))))
          (is (= 3 (get (inv s) "wheat_seeds")))
          (is (finished? s)))))))

(deftest only-the-item-is-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {:blocks (patch "short_grass" [2 3 4] [0]) :drops seed-drops})]
          (core/submit! (:eng s) (spec {:count 1}) {})
          (await (run-ticks s 1 700))
          (fake/add-entity! (:p s) {:id 50 :kind "item" :name "item" :item {:name "dirt" :count 1}
                                    :pos [3 64 1]})
          (await (run-ticks s 40 700))
          (is (>= (get (inv s) "wheat_seeds") 1))
          (is (nil? (get (inv s) "dirt")) "dirt stays on the ground")
          (is (finished? s)))))))

(deftest the-result-is-handed-to-a-parent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (setup-seeing {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops})
              out (atom nil)
              parent (tu/recording-parent 'jobs.gather.get-seeds {:count 2} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'seeding-parent parent))]
          (core/submit! eng '(seeding-parent) {})
          (await (tu/run-ticks eng clock 40 700))
          (is (= :count (:reason @out)))
          (is (>= (:got @out) 2)))))))

(deftest a-partial-walk-goes-on-in-the-same-call-and-the-source-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (setup-seeing {:blocks {"6,64,0" "short_grass" "9,64,0" "short_grass"} :drops seed-drops})]
          (tu/short-walks! p 8 1) ; the first walk, to the nearer cell at x 6, ends partial: go-to walks on
          (core/submit! eng (spec {:count 1}) {})
          (await (run-ticks s 40 700))
          (is (= [6 9] (mapv #(.-x (.-pos (.-args %))) (h/calls p "dig"))) "go-to walks on after a short leg, both cells are dug")
          (is (= :count (:reason (done-event s))))
          (is (finished? s)))))))

(defn setup-with-jobs
  "setup-seeing with the registry's jobs overridden by stubs."
  [world stubs]
  (update (setup-seeing world) :eng #(update % :jobs merge stubs)))

(deftest a-walk-waiting-on-the-world-is-a-yield-not-a-skipped-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (setup-with-jobs {:blocks {"6,64,0" "short_grass"} :drops seed-drops}
                                                    {'jobs.movement.go-to {:ways [:walk :clear-seal] :round (fn [_] :continue)}})]
          (core/submit! eng (spec {:count 1}) {})
          (await (run-ticks s 6 700))
          (is (zero? (dig-count s)))
          (is (not (finished? s)) "the job waits on the walk, it does not end")
          (is (zero? (:barren (job-mem s) 0)) "no barren round counted")
          (is (empty? (:skipped (job-mem s))) "the cell is not skipped"))))))

(deftest the-scan-sees-past-skipped-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (patch "short_grass" (range 5 16) (range 0 6))
        ;; every patch cell is unreachable (the fake walls in the cells round each); the one beyond them, at z 9, is not
              far "11,64,9"
              s (await (scenario {:count 1 :settings {:jobs.gather.get-seeds/per-round 8}}
                                 {:blocks (assoc cells far "short_grass") :drops seed-drops :unreachable (vec (keys cells))} 200))]
          (is (= 1 (dig-count s)) "the one reachable cell, past the skipped ones, is dug")
          (is (= :count (:reason (done-event s)))))))))

;; ------------------------------------------------------------------ worlds with zones and plans

(defn start
  "An engine over primitives p on dir, sharing the jobs.lib.world-files w."
  [{:keys [p dir shared]}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (or dir (tu/tmp-dir))
                          :now #(deref clock) :world shared
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async in-world
  "Submit the job with args over a fake of world sharing w; run n ticks; the setup map."
  [args world w n]
  (let [s (start {:p (tu/seeing-all (tu/fake-on-floor world)) :shared w})]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn dug-cells [{:keys [p]}] (mapv #(let [q (.-pos (.-args %))] [(.-x q) (.-y q) (.-z q)]) (h/calls p "dig")))
(defn block-at [{:keys [p]} x y z] (.-name (.blockAt p (tu/pos x y z))))
(defn gave-up-fields [s] (map #(select-keys % [:reason :zones :plans]) (events-of s :stopped)))
(defn declined-reasons [s] (vec (distinct (map :reason (events-of s :waiting)))))

(defn stand
  "{\"x,y,z\" block} for a stand of height h of block on x z, base at y 64."
  [block x z h]
  (into {} (for [y (range 64 (+ 64 h))] [(str x "," y "," z) block])))

(def farm-zone {:name "farm" :min [2 60 -2] :max [4 70 2] :owner "Miles"})
(defn plan-over [id [x y z] want] {:id id :parts [{:id "p" :box [[x y z] [x y z]] :want want}]})

;; ------------------------------------------------------------------ stalks: cut above the base

(deftest a-stand-is-cut-at-its-second-segment-and-the-base-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[item height] [["sugar_cane" 3] ["bamboo" 3] ["sugar_cane" 2]]]
          (let [s (await (scenario {:item item :count 1} {:blocks (stand item 3 0 height)} 20))]
            (is (= [[3 65 0]] (dug-cells s)) (str item " " height))
            (is (= item (block-at s 3 64 0)) "the base stands")
            (is (= :count (:reason (done-event s))))
            (is (<= 1 (get (inv s) item)))
            (is (finished? s))))))))

(deftest a-tall-stand-is-found-beyond-many-single-canes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "sugar_cane" :count 1}
                                 {:blocks (merge (patch "sugar_cane" (range 2 12) (range 1 8)) (stand "sugar_cane" 14 0 3))} 20))]
          (is (= [[14 65 0]] (dug-cells s)))
          (is (= :count (:reason (done-event s)))))))))

(deftest a-stand-of-one-is-never-cut-and-the-job-declines-once-naming-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "sugar_cane"} {:blocks (stand "sugar_cane" 3 0 1)} 6))]
          (is (zero? (dig-count s)))
          (is (not (finished? s)))
          (is (= [:too-short] (declined-reasons s))))))))

(deftest two-stands-and-a-goal-of-more-end-none-with-what-was-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "sugar_cane" :count 64}
                                 {:blocks (merge (stand "sugar_cane" 3 0 3) (stand "sugar_cane" 5 2 3))} 40))]
          (is (= #{[3 65 0] [5 65 2]} (set (dug-cells s))))
          (is (= :none (:reason (done-event s))))
          (is (= 2 (:got (done-event s))))
          (is (finished? s)))))))

(deftest no-stand-in-radius-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async [[{:item "bamboo"} {}]
                               [{:item "bamboo"} {:blocks (stand "sugar_cane" 3 0 3)}]
                               [{:item "bamboo" :radius 5} {:blocks (stand "bamboo" 90 0 3)}]]
                              (fn ^:async one [[args world]]
                                (let [s (setup-seeing world)]
                                  (core/submit! (:eng s) (spec args) {})
                                  (await (run-ticks s 10 700))
                                  (is (zero? (dig-count s)))))))))))

(deftest a-cut-in-a-zone-or-a-plan-is-refused-and-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[w expected] [[(ew/of-data {} {} [(assoc farm-zone :min [8 60 -2] :max [10 70 2])]) {:zones ["farm"] :plans []}]
                              [(ew/of-data {"pad" (plan-over "pad" [9 65 0] "sugar_cane")} {} []) {:zones [] :plans ["pad"]}]]]
          (let [s (start {:p (tu/seeing-all (tu/fake-on-floor {:blocks (stand "sugar_cane" 9 0 3)})) :shared w})
                out (await (tu/child-outcome (:eng s) 'jobs.gather.get-seeds {:item "sugar_cane"} 40))]
            (is (empty? (dug-cells s)))
            (is (zero? (count (tu/walked-to (:eng s)))) "no walk to a refused cell")
            (is (= (assoc expected :reason :refused) (select-keys out [:reason :zones :plans])))
            (is (= :refused (:reason out)))
            (is (= :condition-failed (get-in out [:ending :kind])))
            (is (= 0 (:got out)))
            (is (finished? s))))))))

(deftest a-source-gone-after-the-start-is-a-failed-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {:blocks {"2,64,0" "dirt"}})
              out (await (tu/child-outcome (:eng s) 'jobs.gather.get-seeds {:count 1} 40))]
          (is (= :none (:reason out)))
          (is (= :condition-failed (get-in out [:ending :kind]))))))))

(deftest a-cut-in-a-claim-gives-up-naming-the-claim
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [])
              _ (ew/set-area-claims! w [{:id "c9" :owner "Miles" :status :active :until 9999999999999 :min [8 60 -2] :max [10 70 2]}])
              s (await (in-world {:item "sugar_cane"} {:blocks (stand "sugar_cane" 9 0 3)} w 20))]
          (is (empty? (dug-cells s)))
          (is (= [["c9"]] (map :claims (events-of s :stopped)))))))))

(deftest the-free-stand-is-cut-and-the-refused-one-ends-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (in-world {:item "sugar_cane" :count 64}
                                 {:blocks (merge (stand "sugar_cane" 3 0 3) (stand "sugar_cane" 9 0 3))}
                                 (ew/of-data {} {} [farm-zone]) 40))]
          (is (= [[9 65 0]] (dug-cells s)))
          (is (= :refused (:reason (done-event s))))
          (is (= 1 (:got (done-event s))))
          (is (= [{:reason :refused :zones ["farm"] :plans []}] (gave-up-fields s))))))))

(deftest a-zone-that-allows-digging-is-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (in-world {:item "sugar_cane" :count 1} {:blocks (stand "sugar_cane" 3 0 3)}
                                 (ew/of-data {} {} [(assoc farm-zone :allow #{:dig})]) 20))]
          (is (= [[3 65 0]] (dug-cells s)))
          (is (= :count (:reason (done-event s)))))))))

(deftest no-zone-list-declines-a-dig-source-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:item "sugar_cane"} {}]]
          (let [s (await (in-world args {:blocks (merge (stand "sugar_cane" 3 0 3) (patch "short_grass" [4] [0]))}
                                   (ew/of-data {} {} nil) 5))]
            (is (empty? (dug-cells s)))
            (is (not (finished? s)))
            (is (= [:no-zones] (declined-reasons s)))))))))

(deftest only-the-first-failing-check-warns-with-no-zone-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args world reason] [[{:item "coffee"} {:blocks (patch "short_grass" [3] [0])} :no-source]
                                     [{:item "sugar_cane"} {:blocks (stand "sugar_cane" 3 0 1)} :no-zones]]]
          (let [s (await (in-world args world (ew/of-data {} {} nil) 5))]
            (is (= [reason] (declined-reasons s)) (pr-str args))))))))

(deftest a-zone-added-between-the-choice-and-the-dig-stops-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args world] [[{:item "sugar_cane" :count 1} {:blocks (stand "sugar_cane" 6 0 3)}]
                              [{:count 1} {:blocks (patch "short_grass" [6] [0])}]]]
          (let [w (ew/of-data {} {} [])
                p (tu/seeing-all (tu/fake-on-floor world))
                _ (.override (.-world p) "steer"
                             (fn [token a impl] (ew/set-zones! w [(assoc farm-zone :min [5 60 -2] :max [7 70 2])])
                               (impl token a)))
                s (start {:p p :shared w})]
            (core/submit! (:eng s) (spec args) {})
            (await (run-ticks s 20 700))
            (is (empty? (dug-cells s)))
            (is (= :refused (:reason (done-event s))))))))))

(deftest a-stalk-cut-resumes-after-a-restart
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              p (tu/seeing-all (tu/fake-on-floor {:blocks (merge (stand "sugar_cane" 3 0 3) (stand "sugar_cane" 5 2 3) (stand "sugar_cane" 7 4 3))}))
              s (start {:p p :dir dir})]
          (tu/shutdown-at! s "dig" 2)
          (core/submit! (:eng s) (spec {:item "sugar_cane" :count 3 :settings {:jobs.gather.get-seeds/per-round 1}}) {})
          (await (run-ticks s 1 700))
          (is (not (finished? s)))
          (is (= 3 (:goal (job-mem s))))
          (let [again (start {:p p :dir dir})]
            (is (= 3 (:goal (job-mem again))))
            (await (run-ticks again 60 700))
            (is (finished? again))
            (is (= #{[3 65 0] [5 65 2] [7 65 4]} (set (dug-cells again))))
            (is (= 3 (get (inv again) "sugar_cane")))))))))

;; ------------------------------------------------------------------ grass is judged by the rules too

(deftest grass-in-a-zone-or-a-plan-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[w expected] [[(ew/of-data {} {} [farm-zone]) {:zones ["farm"] :plans []}]
                              [(ew/of-data {"pad" (plan-over "pad" [3 64 0] "short_grass")} {} []) {:zones [] :plans ["pad"]}]]]
          (let [s (await (in-world {:count 1} {:blocks {"3,64,0" "short_grass"} :drops seed-drops} w 20))]
            (is (empty? (dug-cells s)))
            (is (= [(assoc expected :reason :refused)] (gave-up-fields s)))
            (is (= :refused (:reason (done-event s))))))))))

(deftest grass-beside-lava-is-left-unless-the-hazard-is-accepted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args expected walks?] [[{:count 1} [] zero?]
                                        [{:count 1 :accept #{:fluid-adjacent}} [] zero?]
                                        [{:count 1 :accept #{:lava-adjacent}} [[9 64 0]] pos?]]]
          (let [s (await (scenario args {:blocks {"9,64,0" "short_grass" "9,64,1" "lava"} :drops seed-drops} 20))]
            (is (= expected (dug-cells s)))
            (is (walks? (count (tu/walked-to (:eng s)))))))))))

;; ------------------------------------------------------------------ roots are not gathered here

(deftest roots-are-never-dug-from-the-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [item ["carrot" "potato" "beetroot_seeds"]]
          (let [{:keys [eng p]} (setup-seeing {:blocks (merge (patch "short_grass" [3] [0]) (patch "carrots" [4] [0]))})]
            (core/submit! eng (spec {:item item}) {})
            (is (nil? (core/tick! eng)) item)
            (is (zero? (dig-count {:p p})))))))))

(def farm-plan
  {:id "farm"
   :parts [{:id "beds" :box [[2 64 0] [4 64 0]] :want {:crop "carrots"}}
           {:id "store" :box [[10 64 0] [10 64 0]] :want "chest"}]})

(deftest a-material-nobody-gathers-this-way-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {:blocks (patch "short_grass" [3] [0])})]
          (core/submit! (:eng s) (spec {:item "coffee"}) {})
          (await (run-ticks s 5 700))
          (is (= [:no-source] (declined-reasons s)))
          (is (zero? (dig-count s))))))))

(deftest grass-behind-the-body-is-found-after-a-look-around
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops})
              _ (tu/seeing-after-look (:p s))]
          (core/submit! (:eng s) (spec {:count 2}) {})
          (await (run-ticks s 60 700))
          (is (seq (h/calls (:p s) "look")))
          (is (pos? (dig-count s))))))))

(deftest the-fertilize-way-uses-bone-meal-on-open-grass-when-none-is-left-to-break
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async [[nil true] [#{:break} false]]
                              (fn ^:async one [[ways used?]]
                                (let [s (setup-seeing {:blocks (patch "grass_block" [2 3] [0]) :inventory [{:name "bone_meal" :count 2}]})]
                                  (core/submit! (:eng s) (spec (cond-> {:count 2} ways (assoc :ways ways))) {})
                                  (await (run-ticks s 30 700))
                                  (is (= used? (boolean (seq (h/calls (:p s) "useOn")))) (pr-str ways))))))))))

(deftest the-fertilize-way-leaves-reserved-bone-meal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async [[2 false] [1 true]]
                              (fn ^:async one [[reserved used?]]
                                (let [s (setup-seeing {:blocks (patch "grass_block" [2 3] [0]) :inventory [{:name "bone_meal" :count 2}]})]
                                  (mem/write! (:store (:eng s)) :reserve {:id "meal" :item "bone_meal" :count reserved} {:cap 100 :ttl :forever})
                                  (core/submit! (:eng s) (spec {:count 2}) {})
                                  (await (run-ticks s 30 700))
                                  (is (= used? (boolean (seq (h/calls (:p s) "useOn")))) (str reserved " reserved of 2"))))))))))
