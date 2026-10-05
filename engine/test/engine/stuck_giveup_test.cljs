(ns engine.stuck-giveup-test
  "The stuck trigger counts only moves that show the body held where it stands (a walk that carried the body away, or
  moves made somewhere else, are not); walk-near! does not take a drop it cannot climb back toward a target it cannot
  reach; unstick ends once the body is out (the hop arrived, or a body that was enclosed is not any more); the cells
  unstick digs are noted for restore-broken."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.jobs.reach :as reach]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [engine.unstick-test :as ut]))

(defn stuck-in? [world moves]
  (let [{:keys [eng p]} (ut/setup world)]
    (ut/seed-moved! eng moves)
    ((:when (get triggers/all :stuck)) p (mem/view (:store eng)) {})))

(def target {:x 12 :y 64 :z 0})
(def far-end {:x 30 :y 64 :z 0})

(deftest walks-that-carried-the-body-away-are-not-stuck-evidence
  ;; the moat case: an attack walks 25 blocks off and gives up there (no path), a second attack from the start walks off
  ;; the same way: four "blocked" moves, none of which held the body
  (let [away {:from ut/at5 :to far-end :status "blocked" :target target}
        no-path {:from far-end :to far-end :status "blocked" :target target :no-path true}]
    (is (false? (stuck-in? {:self {:pos far-end} :floor tu/walk-floor} [away no-path no-path away])))))

(deftest bad-moves-made-somewhere-else-do-not-hold-the-body-here
  (doseq [[here expected] [[{:x 20 :y 64 :z 0} false] [ut/at5 true]]]
    (is (= expected (stuck-in? {:self {:pos here} :floor tu/walk-floor} (repeat 4 (ut/bad-move)))) (pr-str here))))

;; ------------------------------------------------------------------ unstick ends once out

(def beside {:x 5 :y 64 :z 2})

(deftest unstick-ends-when-the-hop-arrives-even-a-short-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; open ground (not enclosed), no path sensing (the first round's walk does nothing): the step back is blocked,
        ;; the hop puts the body one block on, within range 1 of the goal, and says arrived
        (let [{:keys [eng p seen]} (ut/setup {:self {:pos ut/at5} :floor tu/walk-floor})
              state (.-state (.-world p))]
          (set! (.-pathWorld p) nil)
          (.override (.-world p) "moveTo"
                     (fn ^:async f [_ args _]
                       (if (zero? (.-range args))
                         #js {:status "blocked" :pos (.-pos (.self p)) :distance 1}
                         (do (swap! state assoc-in [:self :pos] [5 64 1])
                             #js {:status "arrived" :pos (.-pos (.self p)) :distance 1}))))
          (ut/seed-moved! eng (repeat 4 {:from ut/at5 :to ut/at5 :status "blocked" :target beside}))
          (core/submit! eng '(jobs.maintenance.unstick) {})
          (loop [i 0] (when (< i 10) (await (core/tick! eng)) (recur (inc i))))
          (is (= [] (:list (core/state eng))) "the spell is over")
          (is (nil? (ut/failed-event seen)) "no give-up")
          (is (= [] (ut/calls p "dig")) "nothing dug"))))))

(def wide-surface-pit
  "The 3-deep 1x1 dirt pit of the unstick tests in a wide floor (feet 67): out of the pit the body is not enclosed."
  (dissoc (merge (floor 66 -30 -20 40 20) ut/dirt-pit) "5,66,0"))

(deftest unstick-ends-when-an-enclosed-body-is-out-though-the-goal-stays-out-of-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (ut/setup {:self ut/dirt-self :blocks wide-surface-pit})]
          (ut/lifting-moveTo! p 99)
          (await (ut/run-attempts! eng p 12 :free))
          (is (false? (reach/enclosed? p)) "the body can walk out")
          (is (= [] (:list (core/state eng))) "the spell is over")
          (is (nil? (ut/failed-event seen)) "no give-up after the body is out"))))))

;; ------------------------------------------------------------------ dug cells are noted for restore-broken

(deftest unstick-notes-every-cell-it-digs-for-restore-broken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (ut/setup {:self ut/dirt-self :blocks ut/dirt-pit})]
          (await (ut/run-attempts! eng p 1 false))
          (let [dug (set (map (fn [{:keys [x y z]}] [x y z]) (ut/dig-positions p)))
                noted (mapv :data (mem/entries (mem/view (:store eng)) :tidy))]
            (is (seq dug))
            (is (= dug (set (map :cell noted))))
            (is (every? #(= {:action :dig :was "dirt" :now "air" :job "j1"} (select-keys % [:action :was :now :job])) noted))))))))
