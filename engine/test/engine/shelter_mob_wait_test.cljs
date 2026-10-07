(ns engine.shelter-mob-wait-test
  "dig-in's walls and the retreat's seal wait a bounded game time for a mob standing in a cell they place into, then stop
  naming the mob (as dig-niche's plug does)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def floor {"0,63,0" "dirt" "0,62,0" "stone" "0,61,0" "stone" "0,60,0" "stone"})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001 :time 14000 :floor tu/walk-floor} world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn mob-in-cell-until
  "A place override refusing with a sheep in the cell while the clock is before until-ms."
  [clock until-ms]
  (fn ^:async f [token a impl]
    (if (< @clock until-ms)
      #js {:status "failed" :reason "Server refused" :refusal #js {:entities #js [#js {:name "sheep" :id 7}]}}
      (await (impl token a)))))

(defn ^:async run-timed!
  "Submit the job; the clock moves 500 ms a tick."
  [{:keys [eng clock]} job]
  (core/submit! eng (list job) {})
  (loop [i 0]
    (when (and (< i 600) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn failed [seen] (some #(when (= :dig_in_failed (:kind %)) %) @seen))

(deftest dig-in-walls-wait-out-a-mob-in-a-wall-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen clock] :as s} (setup {:inventory [{:name "dirt" :count 12}] :blocks floor})]
          (.override (.-world p) "place" (mob-in-cell-until clock (+ @clock 3000)))
          (await (run-timed! s 'jobs.survival.dig-in))
          (is (nil? (failed seen)))
          (is (= "dirt" (.-name (.blockAt p (tu/pos 0 66 0)))) "the roof went on once the mob left")
          (is (= "dirt" (.-name (.blockAt p (tu/pos 1 65 0))))))))))

(deftest dig-in-walls-stop-naming-a-mob-that-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen clock] :as s} (setup {:inventory [{:name "dirt" :count 12}] :blocks floor})]
          (.override (.-world p) "place" (mob-in-cell-until clock (+ @clock 600000)))
          (await (run-timed! s 'jobs.survival.dig-in))
          (is (some? (failed seen)))
          (is (re-find #"sheep" (:text (failed seen)))))))))

;; ------------------------------------------------------------------ the retreat's seal

(defn key-of [x y z] (str x "," y "," z))

(def dead-end
  (let [open (set (for [x (range 0 9) y [64 65]] [x y 0]))]
    (into {} (for [x (range -3 10) y (range 62 68) z [-1 0 1] :when (not (open [x y z]))]
               [(key-of x y z) "stone"]))))

(defn retreat-setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor {:floor [-3 -1 9 1] :blocks dead-end
                                            :inventory [{:name "cobblestone" :count 20}]
                                            :entities [{:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x 4 :y 64 :z 0}}]}))
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn refuse-with-mob!
  "The first n place calls are refused with a sheep in the cell."
  [p n]
  (let [k (atom 0)]
    (.override (.-world p) "place" (fn ^:async f [token a impl]
                                     (if (< (swap! k inc) n)
                                       #js {:status "failed" :refusal #js {:entities #js [#js {:name "sheep" :id 8}]}}
                                       (await (impl token a)))))))

(defn placed [p] (count (filter #(= "place" (.-name %)) (.-calls (.-world p)))))

(deftest a-retreat-seal-waits-out-a-mob-in-a-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (retreat-setup)]
          (refuse-with-mob! p 4)
          (js/setTimeout #(swap! (fake/state p) assoc :entities []) 500)
          (core/submit! eng '(jobs.survival.retreat {}) {})
          (loop [i 0] (when (and (< i 8) (seq (:list (core/state eng)))) (await (core/tick! eng)) (recur (inc i))))
          (is (= "cobblestone" (.-name (.blockAt p (tu/pos 1 64 0)))) "sealed in despite the sheep")
          (is (not-any? #(= :retreat_blocked (:kind %)) @seen)))))))

(deftest a-retreat-seal-gives-up-on-a-mob-that-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (retreat-setup)]
          (refuse-with-mob! p 1000000)
          (core/submit! eng '(jobs.survival.retreat {}) {})
          (loop [i 0] (when (and (< i 8) (seq (:list (core/state eng)))) (await (core/tick! eng)) (recur (inc i))))
          (is (not= "cobblestone" (.-name (.blockAt p (tu/pos 1 64 0)))) "never sealed")
          (is (< 4 (placed p) 60) "waited several tries, then gave up the seal"))))))
