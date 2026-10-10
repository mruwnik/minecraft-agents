(ns engine.pen-check-test
  "jobs.animals.pen-check against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]
            [jobs.lib.pen :as pen]))

(def job 'jobs.animals.pen-check)

(def ground
  (into {} (for [x (range -10 16) z (range -10 16)] [(str x ",63," z) "stone"])))

(def fence-ring
  (into {} (for [x (range -1 6) z (range -1 6) :when (or (#{-1 5} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"])))

(defn world [more & [states]]
  {:self {:pos {:x 2 :y 64 :z 2}}
   :blocks (merge ground fence-ring more)
   :states (or states {})})

(defn ^:async run-job
  "Submit the job with args in a world and run two ticks; the pen-check.done event, or nil."
  [args w]
  (let [s (h/setup w)]
    (core/submit! (:eng s) (list job args) {})
    (dotimes [_ 2]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    {:event (first (filter #(= :pen-check.done (:kind %)) @(:seen s)))
     :listed (:list (core/state (:eng s)))}))

(deftest a-closed-pen-from-a-cell-is-reported-closed-with-its-size
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event listed]} (await (run-job {:at [2 64 2]} (world {})))]
          (is (= {:closed? true :cells 25 :leaks [] :gates []}
                 (select-keys event [:closed? :reason :cells :leaks :gates])))
          (is (empty? listed)))))))

(deftest an-open-gate-is-a-leak-at-the-gate-and-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:at [2 64 2] :max-cells 100}
                                              (world {"2,64,-1" "oak_fence_gate"} {"2,64,-1" {:open true}})))]
          (is (false? (:closed? event)))
          (is (= :leak (:reason event)))
          (is (= [{:pos {:x 2 :y 64 :z -1} :why :open-gate}] (:leaks event)))
          (is (= [{:pos {:x 2 :y 64 :z -1} :open? true}] (:gates event))))))))

(deftest a-box-gives-the-exact-crossings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}}}
                                              (world {"5,64,1" "air"})))]
          (is (false? (:closed? event)))
          (is (= [{:pos {:x 5 :y 64 :z 1} :why :gap}] (:leaks event)))
          (is (= 25 (:cells event))))))))

(deftest a-start-in-the-air-is-no-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:at [2 70 2]} (world {})))]
          (is (= :no-start (:reason event))))))))

(deftest without-a-start-or-a-box-the-job-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {} (world {})))]
          (is (nil? event)))))))

(deftest the-event-and-result-list-the-animals-seen-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [es [{:id 7 :name "cow" :kind "passive" :pos [2.5 64 2.5]}
                  {:id 8 :name "sheep" :kind "passive" :pos [3.5 64 1.5]}
                  {:id 9 :name "cow" :kind "passive" :pos [8.5 64 2.5]}
                  {:id 10 :name "zombie" :kind "hostile" :pos [1.5 64 1.5]}]
              {:keys [event]} (await (run-job {:at [2 64 2]} (assoc (world {}) :entities es)))]
          (is (= 2 (:animals-in event)))
          (is (= [{:kind "cow" :id 7 :pos [2.5 64 2.5]} {:kind "sheep" :id 8 :pos [3.5 64 1.5]}]
                 (sort-by :id (:animals event)))))))))

(deftest dedupe-leaks-keeps-first-seen-order-past-eight-leaks
  (let [leaks (mapv (fn [i] {:pos {:x i :y 64 :z 0} :why :gap}) (range 12))]
    (is (= leaks (pen/dedupe-leaks (concat leaks leaks))))))

(defn ^:async run-seeing
  "run-job with the body seeing through perception and no look taken yet; facing yaw (Minecraft degrees), ticks rounds (default 6)."
  [args w yaw & [ticks]]
  (let [s (h/setup-seeing (assoc w :yaw yaw) nil)]
    (core/submit! (:eng s) (list job args) {})
    (dotimes [_ (or ticks 6)]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    (first (filter #(= :pen-check.done (:kind %)) @(:seen s)))))

(deftest a-pen-the-body-never-looked-at-is-looked-at-before-the-answer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [event (await (run-seeing {:at [2 64 2]} (world {}) 0))]
          (is (= {:closed? true :cells 25} (select-keys event [:closed? :reason :cells]))
              "inside the ring: the floor round the feet and the fence behind are looked at, not :no-start"))
        (let [event (await (run-seeing {:at [2 64 2]} (assoc (world {}) :self {:pos {:x 6 :y 64 :z 2}}) 270))]
          (is (= {:closed? true :cells 25} (select-keys event [:closed? :reason :cells]))
              "the ring behind the body, outside it: no false :unloaded or :climb leak"))
        (let [event (await (run-seeing {:at [2 64 2] :max-cells 100} (world {"2,64,-1" "air"}) 0))]
          (is (= :leak (:reason event)) "the gap in the fence behind the body is seen")
          (is (some #{{:x 2 :y 64 :z -1}} (map :pos (:leaks event)))))))))

(deftest a-shut-gate-behind-the-body-is-looked-at-not-unloaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [yaw [0 90 180 270]]
          (let [event (await (run-seeing {:at [2 64 2]} (world {"2,64,-1" "oak_fence_gate"} {"2,64,-1" {:open false}}) yaw))]
            (is (= {:closed? true :cells 25} (select-keys event [:closed? :reason :cells])) (str "yaw " yaw))
            (is (= [{:pos {:x 2 :y 64 :z -1} :open? false}] (:gates event)) (str "yaw " yaw))))))))

(deftest a-gate-hidden-behind-a-pillar-is-walked-to-and-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (world {"-1,64,2" "oak_fence_gate" "1,64,2" "stone" "1,65,2" "stone" "1,66,2" "stone"} {"-1,64,2" {:open false}})
              event (await (run-seeing {:at [3 64 2]} (assoc-in w [:self :pos] {:x 3 :y 64 :z 2}) 90 60))]
          (is (= {:closed? true :cells 24} (select-keys event [:closed? :cells])) "the body walks round the pillar to see the gate")
          (is (= [{:pos {:x -1 :y 64 :z 2} :open? false}] (:gates event))))))))
