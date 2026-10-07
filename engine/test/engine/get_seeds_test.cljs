(ns engine.get-seeds-test
  "jobs.gather.get-seeds against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.near :as near]
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
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (setup-seeing world)]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :get-seeds.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn job-mem [{:keys [eng]}] (core/job-memory eng "j1"))
(defn dig-count [{:keys [p]}] (count (h/calls p "dig")))

;; ------------------------------------------------------------------ the job

(deftest breaks-grass-until-the-seeds-are-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2} {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops} 40))]
          (is (>= (get (inv s) "wheat_seeds") 2))
          (is (= :count (:reason (done-event s))))
          (is (>= (:got (done-event s)) 2))
          (is (finished? s)))))))

(deftest the-goal-is-relative-to-what-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {:inventory [{:name "wheat_seeds" :count 5}]
                          :blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops})]
          (core/submit! (:eng s) (spec {:count 2}) {})
          (await (run-ticks s 1 700))
          (is (= 7 (:goal (job-mem s))))
          (await (run-ticks s 40 700))
          (is (>= (get (inv s) "wheat_seeds") 7))
          (is (= :count (:reason (done-event s))))
          (is (finished? s)))))))

(deftest the-check-declines-without-a-source-or-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args world note]
                [[{} {} "nothing at all"]
                 [{} {:blocks {"2,64,0" "dirt"}} "other blocks"]
                 [{:radius 5} {:blocks {"9,64,0" "short_grass"}} "grass outside the radius"]]]
          (let [{:keys [eng p]} (setup-seeing world)]
            (core/submit! eng (spec args) {})
            (is (nil? (core/tick! eng)) note)
            (is (zero? (count (tu/walked-to eng))) note)
            (is (zero? (dig-count {:p p})) note)))))))

(deftest grass-never-seen-is-not-a-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (update (setup-seeing {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops}) :p tu/blind)]
          (core/submit! eng (spec {:count 2}) {})
          (is (nil? (core/tick! eng)))
          (is (zero? (dig-count {:p p}))))))))

(deftest the-check-passes-with-only-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup-seeing {})]
          (core/submit! eng (spec {:chest {:x 10 :y 64 :z 0}}) {})
          (await (core/tick! eng))
          (is (some #(= :child_started (:kind %)) @seen) "the job ran, it did not wait"))))))

(deftest grass-that-drops-nothing-ends-dry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:dry-digs 6} {:blocks (patch "short_grass" (range 2 12) [0]) :drops {"short_grass" nil}} 60))
              warns (events-of s :get-seeds.gave-up)]
          (is (= :dry (:reason (done-event s))))
          (is (= 1 (count warns)))
          (is (= :warn (:level (first warns))))
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
              s (await (scenario {:per-round 3} {:blocks cells :drops seed-drops :unreachable (vec (keys cells))} 40))
              warns (events-of s :get-seeds.gave-up)]
          (is (= :none (:reason (done-event s))))
          (is (zero? (count warns)))
          (is (<= (count (tu/walked-to (:eng s))) 6) "each cell is tried once")
          (is (zero? (dig-count s)))
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

(deftest takes-seeds-from-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :chest {:x 10 :y 64 :z 0}}
                                 {:containers {"10,64,0" [{:name "wheat_seeds" :count 10}]}} 20))]
          (is (>= (get (inv s) "wheat_seeds") 2))
          (is (= :count (:reason (done-event s))))
          (is (= 0 (dig-count s)))
          (is (finished? s)))))))

(deftest an-empty-chest-ends-short
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :chest {:x 10 :y 64 :z 0}} {:containers {"10,64,0" []}} 20))]
          (is (= :short (:reason (done-event s))))
          (is (= 0 (:got (done-event s))))
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
              parent {:check (constantly true)
                      :round (fn ^:async seeding-parent [c]
                               (let [r (await (ctx/call-child c :kid 'jobs.gather.get-seeds {:count 2}))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'seeding-parent parent))]
          (core/submit! eng '(seeding-parent) {})
          (dotimes [_ 40]
            (swap! clock + 700)
            (await (core/tick! eng)))
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

(deftest a-walk-waiting-on-the-world-is-a-yield-not-a-skipped-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (setup-seeing {:blocks {"6,64,0" "short_grass"} :drops seed-drops})]
          (await (tu/with-near-stub (fn ^:async f [_ _ _ _] :partial)
            (fn ^:async b []
              (core/submit! eng (spec {:count 1}) {})
              (await (run-ticks s 6 700)))))
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
              s (await (scenario {:count 1 :per-round 8}
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
(defn gave-up-fields [s] (map #(select-keys % [:reason :zones :plans]) (events-of s :get-seeds.gave-up)))
(defn declined-reasons [s] (mapv :reason (events-of s :get-seeds.declined)))

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
        (doseq [[args world] [[{:item "bamboo"} {}]
                              [{:item "bamboo"} {:blocks (stand "sugar_cane" 3 0 3)}]
                              [{:item "bamboo" :radius 5} {:blocks (stand "bamboo" 9 0 3)}]]]
          (let [{:keys [eng p]} (setup-seeing world)]
            (core/submit! eng (spec args) {})
            (is (nil? (core/tick! eng)))
            (is (zero? (dig-count {:p p})))))))))

(deftest a-cut-in-a-zone-or-a-plan-is-refused-and-reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[w expected] [[(ew/of-data {} {} [(assoc farm-zone :min [8 60 -2] :max [10 70 2])]) {:zones ["farm"] :plans []}]
                              [(ew/of-data {"pad" (plan-over "pad" [9 65 0] "sugar_cane")} {} []) {:zones [] :plans ["pad"]}]]]
          (let [s (await (in-world {:item "sugar_cane"} {:blocks (stand "sugar_cane" 9 0 3)} w 20))]
            (is (empty? (dug-cells s)))
            (is (zero? (count (tu/walked-to (:eng s)))) "no walk to a refused cell")
            (is (= [(assoc expected :reason :refused)] (gave-up-fields s)))
            (is (= :refused (:reason (done-event s))))
            (is (= 0 (:got (done-event s))))
            (is (finished? s))))))))

(deftest a-cut-in-a-claim-gives-up-naming-the-claim
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (ew/of-data {} {} [])
              _ (ew/set-area-claims! w [{:id "c9" :owner "Miles" :status :active :until 9999999999999 :min [8 60 -2] :max [10 70 2]}])
              s (await (in-world {:item "sugar_cane"} {:blocks (stand "sugar_cane" 9 0 3)} w 20))]
          (is (empty? (dug-cells s)))
          (is (= [["c9"]] (map :claims (events-of s :get-seeds.gave-up)))))))))

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
          (core/submit! (:eng s) (spec {:item "sugar_cane" :count 3 :per-round 1}) {})
          (await (run-ticks s 3 700))
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
                                        [{:count 1 :accept #{:fluid-adjacent}} [[9 64 0]] pos?]]]
          (let [s (await (scenario args {:blocks {"9,64,0" "short_grass" "9,64,1" "lava"} :drops seed-drops} 20))]
            (is (= expected (dug-cells s)))
            (is (walks? (count (tu/walked-to (:eng s)))))))))))

;; ------------------------------------------------------------------ roots: from a chest only

(def chest-world (fn [stock] {:containers {"10,64,0" stock}}))

(deftest roots-come-out-of-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[item] [["carrot"] ["potato"] ["beetroot_seeds"]]]
          (let [s (await (scenario {:item item :count 4 :chest {:x 10 :y 64 :z 0}}
                                   (chest-world [{:name item :count 10}]) 20))]
            (is (= :count (:reason (done-event s))) item)
            (is (= 4 (get (inv s) item)))
            (is (zero? (dig-count s)))
            (is (finished? s))))))))

(deftest a-chest-with-less-ends-short-with-what-it-gave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "carrot" :count 8 :chest {:x 10 :y 64 :z 0}}
                                 (chest-world [{:name "carrot" :count 3}]) 20))]
          (is (= :short (:reason (done-event s))))
          (is (= 3 (:got (done-event s))))
          (is (= 3 (get (inv s) "carrot")))
          (is (finished? s)))))))

(deftest roots-are-never-dug-from-the-field
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [item ["carrot" "potato" "beetroot_seeds"]]
          (let [{:keys [eng p]} (setup-seeing {:blocks (merge (patch "short_grass" [3] [0]) (patch "carrots" [4] [0]))})]
            (core/submit! eng (spec {:item item}) {})
            (is (nil? (core/tick! eng)) item)
            (is (zero? (dig-count {:p p})))))))))

(deftest roots-without-a-chest-decline-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {})]
          (core/submit! (:eng s) (spec {:item "carrot"}) {})
          (await (run-ticks s 5 700))
          (is (= [:no-chest] (declined-reasons s)))
          (is (not (finished? s))))))))

(def farm-plan
  {:id "farm"
   :parts [{:id "beds" :box [[2 64 0] [4 64 0]] :want {:crop "carrots"}}
           {:id "store" :box [[10 64 0] [10 64 0]] :want "chest"}]})

(deftest the-chest-of-a-plan-is-the-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (in-world {:item "carrot" :count 2 :plan "farm"}
                                 (chest-world [{:name "carrot" :count 5}])
                                 (ew/of-data {"farm" farm-plan} {} []) 20))]
          (is (= :count (:reason (done-event s))))
          (is (= 2 (get (inv s) "carrot")))
          (is (zero? (dig-count s))))))))

(deftest an-unusable-plan-declines-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[plans reason] [[{} :plan-missing]
                                [{"farm" (update farm-plan :parts subvec 0 1)} :no-chest-cell]]]
          (let [s (await (in-world {:item "carrot" :plan "farm"} (chest-world [{:name "carrot" :count 5}])
                                   (ew/of-data plans {} []) 5))]
            (is (= [reason] (declined-reasons s)))
            (is (zero? (count (h/calls (:p s) "transfer"))))
            (is (not (finished? s)))))))))

(deftest a-material-nobody-gathers-this-way-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup-seeing {:blocks (patch "short_grass" [3] [0])})]
          (core/submit! (:eng s) (spec {:item "coffee"}) {})
          (await (run-ticks s 5 700))
          (is (= [:no-source] (declined-reasons s)))
          (is (zero? (dig-count s))))))))
