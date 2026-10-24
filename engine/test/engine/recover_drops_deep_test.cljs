(ns engine.recover-drops-deep-test
  "recover-drops for a pile far below the body: a stair down (jobs.access.tunnel) instead of standing above it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.death-test :as dt :refer [die! job recovered]]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.library-test :refer [setup]]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [run-until-empty]]
            [jobs.movement.go-to.result :as end]))

(def pile-pos {:x 20 :y 50 :z 0})

(def deep-ground
  "Stone from y 40 to the floor at y 63 over x -2..30, z -4..4, with the cell of the pile left as air."
  (into {} (for [x (range -2 31) y (range 40 64) z (range -4 5)
                 :when (not= [x y z] [20 50 0])]
             [(str x "," y "," z) "stone"])))

(def pile [{:name "diamond" :count 5 :slot 0}])

(defn deep-run
  "A body at the surface and the pile 14 below, 20 aside; died ago-ms ago."
  [ago-ms]
  (fn ^:async t []
    (let [{:keys [eng p seen]} (setup {:blocks deep-ground :inventory [{:name "iron_pickaxe" :count 1} {:name "torch" :count 16}]
                                       :entities [{:id 1 :name "item" :kind "item" :pos pile-pos :item {:name "diamond" :count 5}}]})]
      (die! eng {:pos pile-pos :inventory pile} ago-ms)
      (core/submit! eng job {})
      (await (run-until-empty eng 120))
      {:eng eng :p p :seen seen})))

(deftest a-pile-fourteen-below-is-reached-by-a-stair-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((deep-run 5000)))]
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(deftest a-stair-that-cannot-finish-before-the-despawn-gives-up-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await ((deep-run 290000)))
              digs (filter #(= "dig" (.-name %)) (.-calls (.-world p)))
              decided (dt/ending-event seen)]
          (is (= :warn (:level decided)) "an abandoned pile is said to the owner, not logged quietly")
          (is (re-find #"20 50 0" (:text decided)) (str "the text names the pile: " (:text decided)))
          (is (re-find #"window-closed" (:text decided)) (:text decided))
          (is (= {:decision :abandoned :reason :window-closed} (select-keys (recovered eng) [:decision :reason])) (pr-str (recovered eng)))
          (is (empty? digs) "nothing is dug for a stair that ends after the pile is gone"))))))

(deftest a-skeleton-beyond-eight-blocks-with-a-line-of-fire-is-weighed-not-an-unreachable-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks deep-ground :inventory [{:name "iron_pickaxe" :count 1} {:name "torch" :count 16}]
                                           :entities [{:id 1 :name "item" :kind "item" :pos pile-pos :item {:name "diamond" :count 5}}
                                                      {:id 9 :name "skeleton" :kind "hostile" :pos {:x 12.5 :y 64 :z 0.5}}]})]
          (die! eng {:pos pile-pos :inventory pile} 5000)
          (core/submit! eng (list (first job) {:value-overrides {"diamond" 6}}) {})
          (await (run-until-empty eng 200))
          (is (not= :unreachable (:reason (recovered eng))) (pr-str (recovered eng)))
          (is (some? (dt/danger-remembered eng)) "the skeleton is weighed and the trip backs off"))))))

(def slope-pile-pos {:x 20 :y 82 :z 0})

(def open-slope
  "A hillside over x -2..22, z -4..4: the top at y 99 falls one block a step (a flat every tenth) to y 81 at x 20."
  (into {} (for [x (range -2 23) z (range -4 5)
                 :let [top (- 99 (js/Math.floor (/ (* (max x 0) 18) 20)))]
                 y (range (- top 3) (inc top))]
             [(str x "," y "," z) "stone"])))

(deftest a-pile-eighteen-below-on-an-open-slope-is-walked-to-with-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks open-slope :self {:pos [0 100 0]}
                                           :entities [{:id 1 :name "item" :kind "item" :pos slope-pile-pos :item {:name "diamond" :count 5}}]})]
          (die! eng {:pos slope-pile-pos :inventory pile} 5000)
          (core/submit! eng job {})
          (await (run-until-empty eng 120))
          (is (empty? (filter #(= "dig" (.-name %)) (.-calls (.-world p)))))
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(deftest an-agent-retry-walks-to-a-pile-an-earlier-run-abandoned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks open-slope :self {:pos [0 100 0]}
                                    :entities [{:id 1 :name "item" :kind "item" :pos slope-pile-pos :item {:name "diamond" :count 5}}]})]
          (die! eng {:pos slope-pile-pos :inventory pile} 5000)
          (let [store (:store eng)
                died (mem/latest (mem/view store) :died)]
            (swap! store update :data mem/add-entry :recovered {:t (inc (:t died)) :wt ((:world-time @store)) :data {:decision :abandoned :death-t (:t died) :reason :unreachable}} nil))
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (= :abandoned (:decision (recovered eng))) "a reflex run keeps the decision")
          (core/submit! eng '(jobs.survival.recover-drops {:retry true}) {})
          (await (run-until-empty eng 120))
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(defn walk-script
  "A deep-ground run whose first walks to the pile are scripted (later ones and the stair's are real): walk n moves the
  body to (get at n) and ends as a leg that did not arrive."
  ([at pile-at] (walk-script at pile-at #(ctx/end % :limit-reached {:reason :unreachable :why :search-spent :line :job-time} {:arrived false})))
  ([at pile-at finish!]
  (fn ^:async t []
    (let [calls (atom [])
          {:keys [eng p seen]} (setup {:blocks deep-ground :inventory [{:name "iron_pickaxe" :count 1} {:name "torch" :count 16}]
                                       :entities [{:id 1 :name "item" :kind "item" :pos pile-at :item {:name "diamond" :count 5}}]})
          real (get registry/jobs 'jobs.movement.go-to)
          scripted? (fn [c] (and (= pile-at (:pos (:args c))) (< (count @calls) (count at))))
          eng (assoc-in eng [:jobs 'jobs.movement.go-to]
                        (assoc real :round (fn ^:async stub [c]
                                             (if-not (scripted? c)
                                               (await ((:round real) c))
                                               (do (swap! calls conj (:args c))
                                                   (swap! (fake/state p) assoc-in [:self :pos] (get at (dec (count @calls))))
                                                   (finish! c))))))]
      (die! eng {:pos pile-at :inventory pile} 5000)
      (core/submit! eng job {})
      (await (run-until-empty eng 40))
      {:eng eng :p p :seen seen :calls calls}))))

(deftest a-leg-cut-by-the-engine-job-time-limit-is-a-leg-not-a-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await ((walk-script [[10 64 0]] pile-pos #(ctx/end % :limit-reached {:line :job-time}))))]
          (is (not-any? #(re-find #"waiting on the world" (str (:text %))) @seen) "no decline for a leg the limit cut")
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(deftest legs-that-bounce-away-from-a-deep-pile-end-in-the-stair-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p calls]} (await ((walk-script [[20 64 0] [-10 64 0] [20 64 0]] pile-pos)))
              digs (filter #(= "dig" (.-name %)) (.-calls (.-world p)))]
          (is (<= (count @calls) 3) "no more walks once one went away from the pile")
          (is (seq digs) "the stair is cut from above the pile")
          (is (= :collected (:decision (recovered eng))) (pr-str (recovered eng))))))))

(def far-pile {:x 28 :y 64 :z 0})

(deftest legs-that-never-near-a-far-pile-end-stopped-naming-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng calls]} (await ((walk-script (vec (repeat 8 [0 64 0])) far-pile)))
              res (->> @(:results eng) vals (filter #(re-find #"recover-drops" (str (:name %)))) last)]
          (is (<= (count @calls) 4) "bounded by progress, not by the clock")
          (is (= {:decision :abandoned :reason :unreachable} (select-keys (recovered eng) [:decision :reason])))
          (is (= :stopped (:status res)) (pr-str res))
          (is (re-find #"28 64 0" (str (:text res))) (pr-str res)))))))

(deftest go-to-refusing-the-goal-cell-at-once-never-gives-the-pile-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refuse #(ctx/end % :nothing-possible :unreachable {:arrived false :why :goal-not-standable})
              {:keys [eng calls]} (await ((walk-script (vec (repeat 6 [0 64 0])) far-pile refuse)))]
          (dotimes [_ 4]
            (core/submit! eng job {})
            (await (run-until-empty eng 10)))
          (is (<= 5 (count @calls)) "every run walked again")
          (is (nil? (recovered eng)) "refusals that searched nothing are no failed walks"))))))

(deftest a-stair-the-tunnel-cannot-start-abandons-the-pile-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks deep-ground
                                         :entities [{:id 1 :name "item" :kind "item" :pos pile-pos :item {:name "diamond" :count 5}}]})]
          (die! eng {:pos pile-pos :inventory pile} 5000)
          (core/submit! eng job {})
          (await (run-until-empty eng 120))
          (is (= {:decision :abandoned :reason :unreachable} (select-keys (recovered eng) [:decision :reason])) (pr-str (recovered eng)))
          (is (= 1 (count (filter #(and (= :stopped (:kind %)) (= :unreachable (:reason %))) @seen))))
          (is (re-find #":no-tool \(no pickaxe could be got\)" (:detail (recovered eng))) (pr-str (recovered eng))))))))
