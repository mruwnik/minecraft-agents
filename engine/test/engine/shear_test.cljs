(ns engine.shear-test
  "jobs.animals.shear against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.perception :as perception]
            [engine.fake :as fake]
            [engine.test-util :as tu]))

(defn sheep [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "sheep" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def shears [{:name "shears" :count 1}])

(defn ^:async run-ticks
  [{:keys [eng clock]} n step]
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit (jobs.animals.shear args) in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world)]
    (core/submit! (:eng s) (list 'jobs.animals.shear args) {})
    (await (run-ticks s n 700))
    s))

(defn done-event [{:keys [seen]}] (first (filter #(= :shear.done (:kind %)) @seen)))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls-of [{:keys [p]} name] (h/calls p name))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))
(defn world-sheared [{:keys [p]}]
  (mapv #(boolean (:sheared %)) (filter #(= "sheep" (:name %)) (fake/entities p))))

(deftest shears-all-and-collects-the-wool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory shears :entities [(sheep 1 2) (sheep 2 3)]} 8))]
          (is (finished? s))
          (is (= :shorn (:reason (done-event s))))
          (is (= ["u1" "u2"] (:shorn (done-event s))))
          (is (= [true true] (world-sheared s)))
          (is (= 2 (count-of s "white_wool")))
          (is (= 2 (:collected (done-event s))))
          (is (pos? (count (calls-of s "collect"))))
          (is (empty? (events-of s :shear.gave-up))))))))

(deftest one-call-walks-shears-and-collects
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory shears :entities [(sheep 1 8) (sheep 2 9)]} 1))]
          (is (finished? s))
          (is (= :shorn (:reason (done-event s))))
          (is (= [true true] (world-sheared s))))))))

(deftest count-limits-the-shearing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 1} {:inventory shears :entities [(sheep 1 2) (sheep 2 3)]} 8))]
          (is (finished? s))
          (is (= :shorn (:reason (done-event s))))
          (is (= ["u1"] (:shorn (done-event s))))
          (is (= [true false] (world-sheared s)))
          (is (= 1 (count-of s "white_wool"))))))))

(deftest ends-without-shearing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label args world reason]
                [["no shears" {} {:entities [(sheep 1 2)]} :no-shears]
                 ["all sheared" {} {:inventory shears :entities [(sheep 1 2 {:sheared true}) (sheep 2 3 {:sheared true})]} :all-sheared]
                 ["no sheep" {} {:inventory shears :entities []} :none]
                 ["only babies" {} {:inventory shears :entities [(sheep 1 2 {:baby true})]} :none]]]
          (let [s (await (scenario args world 4))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (empty? (calls-of s "interact")) label)
            (is (empty? (calls-of s "collect")) label)))))))

(deftest gives-up-on-unreachable-sheep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory shears :entities [(sheep 1 8) (sheep 2 9) (sheep 3 10)]
                                     :unreachable ["8,64,0" "9,64,0" "10,64,0"]} 5))]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (empty? (calls-of s "interact")))
          (is (<= (count (calls-of s "moveTo")) 3))
          (is (empty? (events-of s :job.backoff))))))))

(deftest collect-false-skips-the-pickup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:collect false} {:inventory shears :entities [(sheep 1 2) (sheep 2 3)]} 6))]
          (is (finished? s))
          (is (= :shorn (:reason (done-event s))))
          (is (empty? (calls-of s "collect")))
          (is (= 0 (count-of s "white_wool"))))))))

(deftest a-sheep-that-leaves-does-not-stall-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory shears :entities [(sheep 1 2) (sheep 2 3)]})]
          (.override (.-world p) "interact"
                     (fn [token args impl]
                       (let [r (impl token args)]
                         (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "u2" (:uuid e))) %))
                         r)))
          (core/submit! eng '(jobs.animals.shear {}) {})
          (await (run-ticks s 8 700))
          (is (finished? s))
          (is (= :shorn (:reason (done-event s))))
          (is (= ["u1"] (:shorn (done-event s)))))))))

(deftest broken-shears-end-after-collecting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory shears :entities [(sheep 1 2) (sheep 2 3)]})]
          (.override (.-world p) "interact"
                     (fn [token args impl]
                       (let [r (impl token args)]
                         (swap! (fake/state p) assoc :inventory [])
                         r)))
          (core/submit! eng '(jobs.animals.shear {}) {})
          (await (run-ticks s 8 700))
          (is (finished? s))
          (is (= :shears-broke (:reason (done-event s))))
          (is (= ["u1"] (:shorn (done-event s))))
          (is (= 1 (count-of s "white_wool")))
          (is (= 1 (:collected (done-event s))))
          (is (= [:shear.gave-up] (mapv :kind (events-of s :shear.gave-up)))))))))

(deftest shear-follows-zones-and-claims
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra acted declined]
                [["Fake" {} [1 2] []]
                 ["Miles" {} [2] [:refused]]
                 ["Miles" {:ignore-zones? true} [1 2] []]
                 [nil {} [] [:no-zones]]]]
          (let [{:keys [eng] :as s} (h/setup {:inventory shears :entities [(sheep 1 2) (sheep 2 3)]} 0 (h/zone-store 2 owner))]
            (core/submit! eng (list 'jobs.animals.shear (merge {:collect false} extra)) {})
            (dotimes [_ 20]
              (swap! (:clock s) + 700)
              (await (core/tick! eng)))
            (is (= acted (vec (sort (map #(.-id (.-args %)) (calls-of s "interact"))))) (pr-str owner extra))
            (is (= declined (mapv :reason (events-of s :shear.declined))) (pr-str owner extra))))))))

(deftest shear-ends-refused-when-every-animal-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner reason] [["Miles" :refused] [nil :no-zones]]]
          (let [{:keys [eng] :as s} (h/setup {:inventory shears :entities [(sheep 1 2)]} 0 (h/zone-store 2 owner))]
            (core/submit! eng (list 'jobs.animals.shear {:collect false}) {})
            (dotimes [_ 20]
              (swap! (:clock s) + 700)
              (await (core/tick! eng)))
            (is (= reason (:reason (done-event s))) (pr-str owner))))))))

(def chest-at "-2,64,3")

(defn chest-world [stock]
  {:blocks {chest-at "chest"} :containers {chest-at stock} :entities [(sheep 1 2)]})

(defn ^:async seeing-scenario
  "As scenario, the body seeing through perception (a chest must be seen to be fetched from)."
  [args world n]
  (let [s (h/setup-seeing world nil)]
    (perception/pass! (aget (:p s) "perception"))
    (core/submit! (:eng s) (list 'jobs.animals.shear args) {})
    (await (run-ticks s n 700))
    s))

(defn shears-in-chest [{:keys [p]}]
  (some #(when (= "shears" (:name %)) (:count %)) (get-in @(fake/state p) [:containers [-2 64 3]])))

(deftest shears-are-fetched-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (seeing-scenario {} (chest-world [{:name "shears" :count 1}]) 120))]
          (is (finished? s))
          (is (= :shorn (:reason (done-event s))))
          (is (nil? (shears-in-chest s))))))))

(deftest fetch-false-ends-no-shears-and-leaves-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (seeing-scenario {:fetch false} (chest-world [{:name "shears" :count 1}]) 12))]
          (is (finished? s))
          (is (= :no-shears (:reason (done-event s))))
          (is (= 1 (shears-in-chest s))))))))

(deftest a-fetch-longer-than-timeout-s-does-not-end-timeout
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup-seeing (chest-world [{:name "shears" :count 1}]) nil)]
          (perception/pass! (aget (:p s) "perception"))
          (.override (.-world (:p s)) "transfer"
                     (fn [token a impl] (swap! (:clock s) + 10000) (impl token a)))
          (core/submit! (:eng s) (list 'jobs.animals.shear {:timeout-s 3}) {})
          (await (run-ticks s 120 700))
          (is (finished? s))
          (is (= :shorn (:reason (done-event s)))))))))

(deftest no-fetch-when-no-sheep-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (seeing-scenario {} (assoc (chest-world [{:name "shears" :count 1}]) :entities []) 12))]
          (is (finished? s))
          (is (= 1 (shears-in-chest s))))))))

(deftest a-failed-fetch-ends-no-shears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (seeing-scenario {} (chest-world []) 120))]
          (is (finished? s))
          (is (= :no-shears (:reason (done-event s)))))))))
