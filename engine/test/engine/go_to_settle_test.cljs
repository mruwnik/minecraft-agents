(ns engine.go-to-settle-test
  "go-to waits out a free fall before it plans, also for a replan inside a walk (jobs.lib.near), and never waits in a fluid."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.go-to-test :as gt]
            [jobs.lib.walk.drive :as drive]
            [engine.test-util :as tu :refer [floor]]))

(defn sinking
  "Starts the body in the air at y 70 with the self flags (a map), then lowers it a tenth with every change of the world,
  down to y 64; it is on the ground there only when the flags leave it out of a fluid."
  [p flags]
  (swap! (fake/state p) update :self #(-> (merge % flags) (assoc :onGround false :pos [3.5 70 0.5])))
  (add-watch (fake/state p) ::sink
             (fn [_ _ old new]
               (let [[x y z] (get-in new [:self :pos])]
                 (when (and (> y 64) (not (get-in new [:self :onGround])) (= (get-in old [:self :pos]) (get-in new [:self :pos])))
                   (let [y' (max 64 (- y 0.1))]
                     (swap! (fake/state p) update :self
                            #(cond-> (assoc % :pos [x y' z]) (and (= 64 y') (not-any? flags [:inLava :inWater])) (assoc :onGround true)))))))))

(defn ^:async replan-in-fall!
  "go-to over a flat floor; the first walk is cut by the body falling in, and its replan (the plan-fn near hands the
  driver) is called at once. The body's [y onGround] when that replan answers."
  [flags]
  (let [{:keys [eng p]} (gt/setup {:blocks (floor -2 -3 40 3)})
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out {:pos [10 64 0] :range 0})))
        follow! drive/follow!
        seen (atom nil)]
    (set! drive/follow!
          (fn ^:async f [c plan opts]
            (when-not @seen
              (sinking p flags)
              (await ((:plan-fn opts) []))
              (let [s (fake/self p)] (reset! seen [(get-in s [:pos 1]) (:onGround s)])))
            (await (follow! c plan opts))))
    (core/submit! eng '(recording-parent) {})
    (await (gt/tick-out! eng 60))
    (set! drive/follow! follow!)
    @seen))

(deftest a-replan-inside-a-walk-waits-for-a-free-fall-to-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [64 true] (await (replan-in-fall! {}))))))))

(deftest a-replan-does-not-wait-in-a-fluid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [flags [{:inLava true} {:inWater true}]]
          (is (= [70 false] (await (replan-in-fall! flags))) (pr-str flags)))))))
