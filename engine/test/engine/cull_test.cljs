(ns engine.cull-test
  "jobs.animals.cull against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [jobs.animals.cull :as cull]))

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
  (let [s (h/setup world)]
    (core/submit! (:eng s) (spec args) {})
    (dotimes [_ n]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :cull.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn world-ids [{:keys [p]}] (set (map :id (fake/entities p))))

(defn declines?
  "True when the job is submitted, the first tick does nothing and no attack or walk is called."
  [args entities]
  (let [{:keys [eng p]} (h/setup {:inventory h/sword :entities entities})]
    (core/submit! eng (spec args) {})
    (and (nil? (core/tick! eng))
         (zero? (count (h/calls p "attack")))
         (zero? (count (h/calls p "moveTo"))))))

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
          (is (finished? s)))))))

(deftest the-check-declines-without-a-surplus
  (is (declines? {:keep 2} [(cow 1 3) (cow 2 4) (calf 3 2) (calf 4 2) (calf 5 2)]) "two adults and three calves")
  (is (declines? {:keep 2} []) "no cows")
  (is (declines? {:keep 2} [(animal 1 "sheep" 3) (animal 2 "sheep" 4) (animal 3 "sheep" 5)]) "only other kinds")
  (is (declines? {:keep 2} [(cow 1 3) (cow 2 4)]) "exactly keep")
  (is (declines? {:keep 0} []) "keep zero and nothing there"))

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

(deftest a-centre-and-radius-bound-the-herd
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 1 :centre {:x 10 :z 0} :radius 4}
                                 {:inventory h/sword :entities [(cow 1 3) (cow 2 8) (cow 3 10) (cow 4 12) (cow 5 20) (cow 6 10 9)]} 120))]
          (is (= #{1 5 6 4} (world-ids s)) "of 8, 10 and 12 (in the bound) the two nearest the body die")
          (is (= {:killed 2 :remaining 1 :reason :keep}
                 (select-keys (done-event s) [:killed :remaining :reason]))))))))

(deftest the-body-radius-bounds-the-herd-without-a-centre
  (is (declines? {:keep 1 :radius 10} [(cow 1 3) (cow 2 30) (cow 3 40)]) "one cow in range is not above keep"))

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

(deftest check-and-round-are-exported
  (is (fn? cull/check))
  (is (fn? cull/round)))

(deftest cull-follows-zones-and-claims
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra acted declined]
                [["Fake" {} [1 2] []]
                 ["Miles" {} [2] [:refused]]
                 ["Miles" {:ignore-zones? true} [1 2] []]
                 [nil {} [] [:no-zones]]]]
          (let [{:keys [eng] :as s} (h/setup {:inventory h/sword :entities [(cow 1 2) (cow 2 3)]} 0 (h/zone-store 2 owner))]
            (core/submit! eng (list 'jobs.animals.cull (merge {:keep 0} extra)) {})
            (dotimes [_ 20]
              (swap! (:clock s) + 700)
              (await (core/tick! eng)))
            (is (= acted (vec (sort (distinct (attacked s))))) (pr-str owner extra))
            (is (= declined (mapv :reason (events-of s :cull.declined))) (pr-str owner extra))))))))

(deftest cull-ends-refused-when-every-animal-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner reason] [["Miles" :refused] [nil :no-zones]]]
          (let [{:keys [eng] :as s} (h/setup {:inventory h/sword :entities [(cow 1 2)]} 0 (h/zone-store 2 owner))]
            (core/submit! eng (list 'jobs.animals.cull {:keep 0}) {})
            (dotimes [_ 20]
              (swap! (:clock s) + 700)
              (await (core/tick! eng)))
            (is (= reason (:reason (done-event s))) (pr-str owner))))))))
