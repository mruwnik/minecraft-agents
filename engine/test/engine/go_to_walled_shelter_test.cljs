(ns engine.go-to-walled-shelter-test
  "go-to and a walled dig-in shelter (live: a go-to was cut by a hostile, the night-unsafe reflex walled the body in with
  carried dirt, the go-to resumed inside the walls and gave up :unreachable :exhausted in three quick rounds). A go-to
  resumed in its own walled shelter at night ends :sheltered, not :exhausted; at dawn a go-to digs the door and arrives."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def night 14000)
(def dawn 0)

(defn ground
  "Dirt at y 63 over stone 58..62, x -14..30, z -6..6: flat ground with its surface feet height 64."
  []
  (into {} (for [x (range -14 31) z (range -6 7) y (range 58 64)]
             [(str x "," y "," z) (if (= y 63) "dirt" "stone")])))

(defn setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:offlineScale 0.0001 :blocks (ground) :time night
                    :inventory [{:name "iron_pickaxe" :count 1} {:name "dirt" :count 32}]})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/register-reflex! eng {:trigger :night-unsafe})
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-until-empty [{:keys [eng clock]} n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))
(defn emitted [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(def goal {:x 25 :y 64 :z 0})

(deftest a-go-to-resumed-in-its-walled-shelter-at-night-ends-sheltered-not-exhausted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)]
          (core/submit! eng (list 'jobs.movement.go-to {:pos goal}) {})
          (await (run-until-empty s 200))
          (is (= [:night-unsafe] (mapv :reflex (emitted seen :fired))))
          (is (<= 8 (count (calls p "place"))) "the night-unsafe reflex walled the body in")
          (is (= [] (emitted seen :unreachable)) "no give-up :exhausted from inside the shelter")
          (is (= [:night] (mapv :why (emitted seen :sheltered))))
          (is (= [0 64 0] (feet p)) "still in the shelter"))))))

(deftest at-dawn-go-to-leaves-its-walled-shelter-and-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)]
          (core/submit! eng (list 'jobs.movement.go-to {:pos goal}) {})
          (await (run-until-empty s 200))
          (.setTime (.-world p) dawn)
          (core/submit! eng (list 'jobs.movement.go-to {:pos goal}) {})
          (await (run-until-empty s 400))
          (is (= [] (emitted seen :unreachable)))
          (is (= 1 (count (emitted seen :leave-shelter.out))))
          (is (<= 24 (first (feet p)) 26) "arrived beside the target")
          (is (= 64 (second (feet p)))))))))
