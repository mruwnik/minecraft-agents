(ns engine.cull-test
  "jobs.animals.cull against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.fake :as fake]
            [engine.look-support :as ls]
            [engine.test-util :as tu]
            [jobs.animals.cull :as cull]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.animals :as animals]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(defn spec [args] (list 'jobs.animals.cull args))

(def cow-drops [{:name "beef" :count 1} {:name "leather" :count 1}])

(defn animal
  ([id name x] (animal id name x 0 {}))
  ([id name x z more] (merge {:id id :name name :kind "passive" :pos {:x x :y 64 :z z}} more)))

(defn cow
  ([id x] (cow id x 0))
  ([id x z] (animal id "cow" x z {:drops cow-drops})))

(defn calf [id x] (animal id "cow" x 0 {:drops cow-drops :baby true}))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world 50)]
    (core/submit! (:eng s) (spec args) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event
  "The cull's ending event (job.completed or job.stopped)."
  [{:keys [seen]}]
  (first (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %))) @seen)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn world-ids [{:keys [p]}] (set (map :id (fake/entities p))))

(defn rounds [{:keys [seen]}] (count (filter #(= :round_started (:kind %)) @seen)))

(defn ^:async too-few-outcome
  "{:finished :acted} of the job submitted in a world: its goal holds, so it ends done at once, attacks and walks nowhere."
  [args entities]
  (let [{:keys [p] :as s} (await (scenario args {:inventory h/sword :entities entities} 20))]
    {:finished (finished? s) :acted (+ (count (h/calls p "attack")) (count (h/calls p "moveTo")))}))

(deftest six-adults-keep-two-kills-four-and-collects
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 2} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) (range 1 7))} 80))]
          (is (= 2 (count (world-ids s))) "two cows live")
          (is (= 4 (get (inv s) "beef")))
          (is (= 4 (get (inv s) "leather")))
          (is (= {:killed 4 :remaining 2 :babies 0 :reason :keep :skipped []}
                 (select-keys (done-event s) [:killed :remaining :babies :reason :skipped])))
          (is (= "cull done: killed 4, 2 cow adults left (keep 2)" (:text (done-event s))))
          (is (empty? (events-of s :cull.done)) "the engine's job.completed tells it")
          (is (finished? s)))))))

(deftest one-call-kills-the-surplus-and-collects
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 2} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) (range 1 5))} 3))]
          (is (finished? s))
          (is (= :keep (:reason (done-event s))))
          (is (= 2 (get (inv s) "beef"))))))))

(deftest without-a-surplus-the-job-is-done-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args entities in-range note]
                [[{:keep 2} [(cow 1 3) (cow 2 4) (calf 3 2) (calf 4 2) (calf 5 2)] "two adults and three calves"]
                 [{:keep 2} [] "no cows"]
                 [{:keep 2} [(animal 1 "sheep" 3) (animal 2 "sheep" 4) (animal 3 "sheep" 5)] "only other kinds"]
                 [{:keep 2} [(cow 1 3) (cow 2 4)] "exactly keep"]
                 [{:keep 0} [] "keep zero and nothing there"]]]
          (is (= {:finished true :acted 0} (await (too-few-outcome args entities))) note))))))

(deftest a-surplus-of-one-with-calves-around-kills-one-adult
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 2} {:inventory h/sword
                                            :entities [(cow 1 3) (cow 2 4) (cow 3 5) (calf 4 2) (calf 5 2) (calf 6 2)]} 40))]
          (is (= #{2 3 4 5 6} (world-ids s)) "the nearest adult died, calves untouched")
          (is (not-any? #{4 5 6} (attacked s)))
          (is (= {:killed 1 :remaining 2 :babies 3 :reason :keep}
                 (select-keys (done-event s) [:killed :remaining :babies :reason]))))))))

(deftest count-caps-the-kills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 1 :count 2} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) (range 1 7))} 80))]
          (is (= 4 (count (world-ids s))))
          (is (= {:killed 2 :remaining 4 :reason :count}
                 (select-keys (done-event s) [:killed :remaining :reason]))))))))

(deftest a-box-bounds-the-herd
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [box {:min {:x 0 :y 60 :z -5} :max {:x 6 :y 70 :z 5}}
              s (await (scenario {:keep 1 :box box :radius 1}
                                 {:inventory h/sword :entities [(cow 1 3) (cow 2 5) (cow 3 6) (cow 4 7) (cow 5 12) (cow 6 3 9)]} 80))]
          (is (= #{4 5 6 3} (world-ids s)) "the cows inside the box are thinned to one, those outside live")
          (is (= {:killed 2 :remaining 1 :reason :keep}
                 (select-keys (done-event s) [:killed :remaining :reason]))))))))

(deftest a-box-counts-animals-standing-in-its-max-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [box {:min {:x 0 :y 64 :z 0} :max {:x 6 :y 64 :z 5}}
              s (await (scenario {:keep 1 :box box :radius 1}
                                 {:inventory h/sword :entities [(cow 1 6.5 0.5) (cow 2 6.9 0.2) (cow 3 6.2 0.8)]} 200))]
          (is (= {:killed 2 :remaining 1 :reason :keep}
                 (select-keys (done-event s) [:killed :remaining :reason]))))))))

(deftest in-box-floors-each-axis-and-includes-the-max-cells
  (let [box {:min {:x 0 :y 64 :z 0} :max {:x 6 :y 64 :z 5}}]
    (are [in? pos] (= in? (animals/in-box? box pos))
      true {:x 6.9 :y 64 :z 0.5}
      false {:x 7.0 :y 64 :z 0.5}
      false {:x -0.1 :y 64 :z 0.5}
      true {:x 0.5 :y 64 :z 5.9}
      false {:x 0.5 :y 64 :z 6.0}
      false {:x 0.5 :y 64 :z -0.1}
      true {:x 0.5 :y 64.9 :z 0.5}
      false {:x 0.5 :y 65 :z 0.5}
      false {:x 0.5 :y 63.5 :z 0.5})))

(deftest a-center-and-radius-bound-the-herd
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 1 :center {:x 10 :z 0} :radius 4}
                                 {:inventory h/sword :entities [(cow 1 3) (cow 2 8) (cow 3 10) (cow 4 12) (cow 5 20) (cow 6 10 9)]} 120))]
          (is (= #{1 5 6 4} (world-ids s)) "of 8, 10 and 12 (in the bound) the two nearest the body die")
          (is (= {:killed 2 :remaining 1 :reason :keep}
                 (select-keys (done-event s) [:killed :remaining :reason]))))))))

(deftest the-body-radius-bounds-the-herd-without-a-centre
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:finished true :acted 0}
               (await (too-few-outcome {:keep 1 :radius 10} [(cow 1 3) (cow 2 30) (cow 3 40)])))
            "one cow in range is not above keep")))))

(defn wall-of [block xs] (into {} (for [x xs y [64 65 66]] [(str x "," y ",0") block])))

(deftest walled-off-cows-are-tried-last-and-the-job-ends-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 1}
                                 {:inventory h/sword
                                  :entities [(cow 1 3) (cow 2 4) (cow 3 0 5) (cow 4 0 6) (cow 5 0 7)]
                                  :blocks (wall-of "glass" [1 2])
                                  :unreachable ["3,64,0" "4,64,0"]} 200))
              {:keys [killed remaining reason skipped]} (done-event s)]
          (is (= #{1 2} (world-ids s)) "the three open cows were killed, the walled two live")
          (is (= #{3 4 5} (set (attacked s))) "the walled cows are never swung at")
          (is (= [3 :unreachable 2] [killed reason remaining]))
          (is (= [1 2] (vec (sort skipped))))
          (is (= [:unreachable] (mapv :reason (events-of s :cull.gave-up))) "one give-up event, the event stream carries no severity")
          (is (finished? s)))))))

(deftest another-mob-is-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 0 :mob "pig"}
                                 {:inventory h/sword
                                  :entities [(cow 1 3) (cow 2 4) (animal 3 "pig" 5 0 {:drops [{:name "porkchop" :count 1}]})]} 40))]
          (is (= #{1 2} (world-ids s)))
          (is (= 1 (:killed (done-event s)))))))))

(deftest keep-is-never-undercut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [keep [0 1 2 3]]
          (let [s (await (scenario {:keep keep} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) (range 1 5))} 80))]
            (is (= keep (count (world-ids s))) (str "keep " keep))
            (is (= (- 4 keep) (:killed (done-event s))) (str "keep " keep))))))))

(deftest cull-follows-zones-and-claims
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra acted declined]
                [["Fake" {} [1 2] []]
                 ["Miles" {} [2] [:refused]]
                 ["Miles" {:ignore-zones? true} [1 2] []]
                 [nil {} [] []]]]
          (let [{:keys [eng] :as s} (h/setup {:inventory h/sword :entities [(cow 1 2) (cow 2 3)]} 50 (h/zone-store 2 owner))]
            (core/submit! eng (list 'jobs.animals.cull (merge {:keep 0} extra)) {})
            (await (tu/run-ticks eng (:clock s) 20 700))
            (is (= acted (vec (sort (distinct (attacked s))))) (pr-str owner extra))
            (is (= declined (mapv :reason (events-of s :cull.declined))) (pr-str owner extra))))))))

(deftest cull-waits-when-every-animal-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner reason] [["Miles" :refused] [nil :no-zones]]]
          (let [{:keys [eng] :as s} (h/setup {:inventory h/sword :entities [(cow 1 2)]} 50 (h/zone-store 2 owner))]
            (core/submit! eng (list 'jobs.animals.cull {:keep 0}) {})
            (await (tu/run-ticks eng (:clock s) 20 700))
            (is (= 0 (rounds s)) (pr-str owner reason))
            (is (empty? (attacked s)) (pr-str owner reason))))))))

(deftest a-refusal-met-below-least-waits-not-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (h/setup {:inventory h/sword :entities [(cow 1 2) (cow 2 3)]} 50 (h/zone-store 2 "Miles"))]
          (core/submit! eng (list 'jobs.animals.cull {:keep 0 :count 2 :least 2}) {})
          (await (tu/run-ticks eng (:clock s) 30 700))
          (is (= [2] (vec (distinct (attacked s)))) "the allowed cow is killed")
          (is (not (finished? s)) "the refused one is waited for")
          (is (= :refused (:reason (first (events-of s :waiting))))))))))

(deftest the-reach-covers-the-far-edge-of-the-boxs-max-cells
  (let [c {:primitives #js {:self (fn [] #js {:pos #js {:x 16.5 :y 0 :z 16.5}})}
           :args {:box {:min {:x 19 :y 0 :z 14} :max {:x 23 :y 1 :z 18}}}}
        far-edge {:x 23.7 :y 0 :z 18.7}]
    (is (>= (hunting/reach c) (js/Math.hypot (- (:x far-edge) 16.5) (- (:z far-edge) 16.5)))
        "a cow at a fractional position in the max row is inside the query")))

(deftest cull-acts-in-the-footprint-of-its-own-plan-but-not-another-bodys
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[author acted] [["Fake" [1]] ["Miles" []]]]
          (let [plan (shape/with-author {:id "pen" :parts [{:id "p" :box [[2 64 0] [2 64 0]] :want "stone"}]} author)
                {:keys [eng] :as s} (h/setup {:inventory h/sword :entities [(cow 1 2)]} 50 (ew/of-data {"pen" plan} {} []))]
            (core/submit! eng (list 'jobs.animals.cull {:keep 0}) {})
            (await (tu/run-ticks eng (:clock s) 20 700))
            (is (= acted (vec (distinct (attacked s)))) author)))))))

(defn sheep [id x color] (animal id "sheep" x 0 {:color color :drops [{:name (str color "_wool") :count 1}]}))

(deftest a-colour-cull-kills-only-sheep-of-that-colour
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "sheep" :color "white" :keep 0 :count 1}
                                 {:inventory h/sword :entities [(sheep 1 3 "brown") (sheep 2 6 "white")]} 40))]
          (is (= #{2} (set (attacked s))) "the nearer brown sheep is left alone")
          (is (= #{1} (world-ids s))))))))

(deftest every-culled-cow-is-a-kill-event-naming-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 2} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) (range 1 5))} 60))
              es (events-of s :animal.killed)]
          (is (= [["cow" "jobs.animals.cull" :cull] ["cow" "jobs.animals.cull" :cull]]
                 (mapv (juxt :animal :by :rule) es)))
          (is (= #{1 2} (set (map :entity es)))))))))

(deftest drops-that-cannot-be-collected-do-not-hold-the-cull-at-collecting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:inventory h/sword :entities [(cow 1 3)]} 50)]
          (.override (.-world p) "collect" (fn [_ _ _] #js {:status "unreachable" :reason "no-path" :gained #js []}))
          (core/submit! eng (spec {:count 1 :keep 0}) {})
          (await (tu/run-ticks eng clock 40 700))
          (is (= 1 (:killed (done-event s))))
          (is (finished? s)))))))

(deftest a-cow-behind-the-body-is-looked-for-before-the-cull-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities [(cow 1 3)]} 50)]
          (ls/entities-after-look (:p s))
          (core/submit! (:eng s) (spec {:keep 0 :count 1}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 40 700))
          (is (= #{1} (set (attacked s))) "it looked round, saw the cow and culled it"))))))

(deftest two-cows-in-view-before-the-look-do-not-end-the-cull-at-keep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) (range 1 5))} 50)]
          (let [p (:p s)
                entities (.-entities p)]
            (aset p "entities" (fn [q] (let [all (array-seq (.call entities p q))]
                                         (if (some #(= "look" (.-name %)) (.-calls (.-world p)))
                                           (to-array all)
                                           (to-array (take 2 all)))))))
          (core/submit! (:eng s) (spec {:keep 2}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 60 700))
          (is (= 2 (:killed (done-event s))) "the four cows were seen before the goal was judged")
          (is (= 2 (count (world-ids s)))))))))
