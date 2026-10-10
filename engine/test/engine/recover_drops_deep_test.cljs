(ns engine.recover-drops-deep-test
  "recover-drops for a pile far below the body: a stair down (jobs.access.tunnel) instead of standing above it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered]]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.library-test :refer [setup]]
            [engine.test-util :as tu :refer [run-until-empty]]))

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
              decided (first (filter #(= :recover-drops.decided (:kind %)) @seen))]
          (is (= :warn (:level decided)) "an abandoned pile is said to the owner, not logged quietly")
          (is (re-find #"20 50 0" (:text decided)) (str "the text names the pile: " (:text decided)))
          (is (re-find #"window-closed" (:text decided)) (:text decided))
          (is (= {:decision :abandoned :reason :window-closed} (select-keys (recovered eng) [:decision :reason])) (pr-str (recovered eng)))
          (is (empty? digs) "nothing is dug for a stair that ends after the pile is gone"))))))

(defn feet-y [p] (js/Math.floor (.-y (.-pos (.self p)))))

(deftest a-zombie-turning-up-mid-stair-ends-the-run-declined-for-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [arrived (atom false)
              {:keys [eng p seen]} (setup {:blocks deep-ground :inventory [{:name "iron_pickaxe" :count 1} {:name "torch" :count 16}]
                                           :entities [{:id 1 :name "item" :kind "item" :pos pile-pos :item {:name "diamond" :count 5}}]})]
          (die! eng {:pos pile-pos :inventory pile} 5000)
          (core/submit! eng job {})
          (add-watch (fake/state p) ::zombie
                     (fn [_ _ _ _]
                       (when (and (<= (feet-y p) 58) (not @arrived))
                         (reset! arrived true)
                         (let [pos (.-pos (.self p))]
                           (fake/add-entity! p
                                  {:id 7 :name "zombie" :kind "hostile" :pos {:x (- (js/Math.floor (.-x pos)) 0.5) :y (inc (js/Math.floor (.-y pos))) :z (+ 0.5 (js/Math.floor (.-z pos)))}})))))
          (await (run-until-empty eng 20))
          (is (some #(= :danger (:reason %)) (filter #(= :recover-drops.declined (:kind %)) @seen)))
          (is (> (feet-y p) 50) "the stair was not carried on to the pile"))))))

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
          (is (empty? (filter #(= :recover-drops.deep (:kind %)) @seen)) "no stair is cut for a pile a walk reaches")
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
