(ns engine.get-seeds-test
  "jobs.gather.get-seeds against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]))

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
  (let [s (h/setup world)]
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
        (let [s (h/setup {:inventory [{:name "wheat_seeds" :count 5}]
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
          (let [{:keys [eng p]} (h/setup world)]
            (core/submit! eng (spec args) {})
            (is (nil? (core/tick! eng)) note)
            (is (zero? (count (h/calls p "moveTo"))) note)
            (is (zero? (dig-count {:p p})) note)))))))

(deftest the-check-passes-with-only-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (h/setup {})]
          (core/submit! eng (spec {:chest {:x 10 :y 64 :z 0}}) {})
          (await (core/tick! eng))
          (is (some? (:goal (core/job-memory eng "j1")))))))))

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

(deftest unreachable-grass-is-skipped-and-the-job-ends-barren
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (patch "short_grass" (range 6 12) [0])
              s (await (scenario {:per-round 3} {:blocks cells :drops seed-drops :unreachable (vec (keys cells))} 40))
              warns (events-of s :get-seeds.gave-up)]
          (is (= :barren (:reason (done-event s))))
          (is (= 1 (count warns)))
          (is (<= (count (h/calls (:p s) "moveTo")) 6) "each cell is tried once")
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
        (let [s (h/setup {:blocks (patch "short_grass" [2 3 4] [0]) :drops seed-drops})]
          (core/submit! (:eng s) (spec {:count 1}) {})
          (await (run-ticks s 1 700))
          (.push (.-entities (.-state (.-world (:p s)))) #js {:id 50 :kind "item" :name "item" :item #js {:name "dirt" :count 1}
                                                              :pos (tu/pos 3 64 1)})
          (await (run-ticks s 40 700))
          (is (>= (get (inv s) "wheat_seeds") 1))
          (is (nil? (get (inv s) "dirt")) "dirt stays on the ground")
          (is (finished? s)))))))

(deftest the-result-is-handed-to-a-parent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (h/setup {:blocks (patch "short_grass" (range 2 6) (range 0 4)) :drops seed-drops})
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
