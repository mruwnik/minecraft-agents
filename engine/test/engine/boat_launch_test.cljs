(ns engine.boat-launch-test
  "jobs.movement.boat-launch against the fake world: put a boat on water and board it."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.sense-test :as st]
            [engine.test-util :as tu]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.movement.boat-launch :as bl]))

(def pond
  "A pond at x 4..6, z -1..1 in the stone floor (y 63) over a stone bed (a drop lands only in water that holds the fall), air above."
  (into {} (for [x (range 4 7) z (range -1 2) [y b] [[63 "water"] [62 "stone"]]] [(str x "," y "," z) b])))

(def boat-item {:name "oak_boat" :count 1})

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.boat-launch args 40))]
    {:res res :p p :seen seen :eng eng}))

(defn boats [p] (filterv #(= "oak_boat" (:name %)) (fake/entities p)))

(deftest launches-a-boat-and-boards-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p seen]} (await (run {:blocks pond :inventory [boat-item]} {}))]
          (is (= :done (:status res)) (pr-str res))
          (is (true? (:boarded res)))
          (is (= 1 (count (boats p))) "one boat in the world")
          (is (nil? (get (lt/inv p) "oak_boat")) "the item is used up")
          (is (some? (:vehicle (js->clj (.self p) :keywordize-keys true))) "aboard"))))))

(deftest board-false-leaves-the-boat-on-the-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run {:blocks pond :inventory [boat-item]} {:board false}))]
          (is (= :done (:status res)))
          (is (false? (:boarded res)))
          (is (nil? (:vehicle (js->clj (.self p) :keywordize-keys true)))))))))

(deftest a-pos-names-the-water-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond :inventory [boat-item]} {:pos [4 63 0] :board false}))]
          (is (= {:x 4 :y 63 :z 0} (:pos res))))))))

(def big-pond
  "A pond at x 3..7, z -2..2 in the stone floor (y 63) over a stone bed: the ring of cells beside the bank has a bank block within a boat's width."
  (into {} (for [x (range 3 8) z (range -2 3) [y b] [[63 "water"] [62 "stone"]]] [(str x "," y "," z) b])))

(deftest the-boat-goes-where-it-clears-the-bank
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks big-pond :inventory [boat-item]} {:board false}))
              {:keys [x z]} (:pos res)]
          (is (= :done (:status res)) (pr-str res))
          (is (and (<= 4 x 6) (<= -1 z 1)) (str "a cell with water all round it: " (pr-str (:pos res)))))))))

(deftest no-water-in-sight-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:inventory [boat-item]})]
          (core/submit! eng '(jobs.movement.boat-launch {}) {})
          (await (tu/run-until-empty eng 3))
          (is (= 1 (count (:list (core/state eng)))) "still queued")
          (is (= [:no-water] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest a-non-boat-item-is-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond :inventory [{:name "stick" :count 1}]} {:item "stick"}))]
          (is (= {:status :stopped :reason :bad-args} (select-keys res [:status :reason]))))))))

(deftest already-aboard-fails-its-condition-aboard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [other {:id 5 :name "minecart" :uuid "u-5" :kind "other" :pos {:x 0.5 :y 64 :z 0.5}}
              {:keys [res]} (await (run {:blocks pond :inventory [boat-item] :self {:vehicle 5} :entities [other]} {}))]
          (is (= [:condition-failed :aboard] [(:kind (:ending res)) (:reason res)])))))))

(deftest a-remembered-boat-seen-gone-is-replaced-by-a-new-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [m (atom {:placed {:id 99 :pos {:x 5 :y 63 :z 0}}})
              mem ctx/mem
              update-mem! ctx/update-mem!
              _ (set! ctx/mem (fn [_] @m))
              _ (set! ctx/update-mem! (fn [_ f & args] (apply swap! m f args)))
              {:keys [res p]} (try (await (run {:blocks pond :inventory [boat-item]} {}))
                                   (finally (set! ctx/mem mem) (set! ctx/update-mem! update-mem!)))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (count (boats p))) "a new boat is down")
          (is (not= 99 (:id (:placed @m)))))))))

(deftest a-remembered-boat-beyond-the-radius-is-not-replaced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far {:id 99 :name "oak_boat" :uuid "u-99" :kind "other" :pos {:x 25.5 :y 63 :z 0.5}}
              m (atom {:placed {:id 99 :pos {:x 25 :y 63 :z 0}}})
              mem ctx/mem
              update-mem! ctx/update-mem!
              _ (set! ctx/mem (fn [_] @m))
              _ (set! ctx/update-mem! (fn [_ f & args] (apply swap! m f args)))
              {:keys [p]} (try (await (run {:blocks pond :inventory [boat-item] :entities [far]} {:board false}))
                               (finally (set! ctx/mem mem) (set! ctx/update-mem! update-mem!)))]
          (is (= 1 (count (boats p))) "only the remembered boat is on the water")
          (is (some? (get (lt/inv p) "oak_boat")) "no boat item is spent")
          (is (= 99 (:id (:placed @m)))))))))

(deftest boat-names
  (is (every? bl/boat-name? ["oak_boat" "bamboo_raft" "oak_chest_boat"]))
  (is (not-any? bl/boat-name? ["minecart" nil "boat_x"])))

(deftest board-failed-carries-the-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond :inventory [boat-item] :mountFails true} {}))]
          (is (= {:status :stopped :reason :board-failed} (select-keys res [:status :reason])) (pr-str res)))))))

(deftest it-looks-around-before-it-waits-for-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (lt/setup {:blocks pond :inventory [boat-item]})
              _ (tu/seeing-after-look p)]
          (core/submit! eng '(jobs.movement.boat-launch {:board false}) {})
          (await (tu/run-until-empty eng 30))
          (is (some #(= "look" (.-name %)) (.-calls (.-world p))))
          (is (empty? (:list (core/state eng)))))))))

(def channel
  "Water one cell wide at x 4, z -2..2 over a stone bed: every cell has a bank block beside it."
  (into {} (for [z (range -2 3) [y b] [[63 "water"] [62 "stone"]]] [(str "4," y "," z) b])))

(deftest water-without-room-waits-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {:blocks channel :inventory [boat-item]})]
          (core/submit! eng '(jobs.movement.boat-launch {}) {})
          (await (tu/run-until-empty eng 3))
          (is (= [:no-room] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest an-unseen-bank-is-looked-at-before-a-cell-is-chosen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (st/seeing {:blocks pond})
              c (assoc (st/stub-ctx p) :args {})
              cell (await (bl/launch-pos! c))]
          (is (some? cell))
          (is (some #(= :sense.looked (:kind %)) @(:events c)) "it looked at what it had not seen"))))))

(deftest a-non-boat-item-is-impossible
  (is (= [:never :bad-args] ((juxt first #(:reason (nth % 2))) (bl/bad-args {:args {:item "stick"}}))))
  (is (= :ok (first (bl/bad-args {:args {:item "oak_boat"}}))))
  (is (= :ok (first (bl/bad-args {:args {}})))))

(deftest aboard-is-a-start-condition
  (let [other {:id 5 :name "minecart" :uuid "u-5" :kind "other" :pos {:x 0.5 :y 64 :z 0.5}}
        c (fn [args] (assoc (st/stub-ctx (:p (lt/setup {:blocks pond :entities [other] :self {:vehicle 5}}))) :args args))]
    (is (= [:later :aboard] ((juxt first #(:reason (nth % 2))) (bl/aboard (c {})))))))

(deftest no-water-is-a-condition-that-asks-for-a-look
  (let [c (fn [world] (assoc (st/stub-ctx (:p (lt/setup world))) :args {}))]
    (is (= :ok (first (bl/no-water (c {:blocks pond})))))
    (is (= [:look :no-water] ((juxt first #(:reason (nth % 2))) (bl/no-water (c {})))))
    (is (= [:look :no-room] ((juxt first #(:reason (nth % 2))) (bl/no-water (c {:blocks channel})))))))

(defn done-of [world args mem]
  (let [p (:p (lt/setup world))]
    (bl/done? {:primitives p :root "j1" :slots [] :args args
               :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data mem}]}}})})))

(def placed-boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 5.5 :y 63 :z 0.5}})

(deftest done-is-ok-once-the-boat-is-down-and-boarded
  (are [world args mem kind] (= kind (first (done-of world args mem)))
    {:blocks pond} {:board true} {} :later
    {:blocks pond :entities [placed-boat]} {:board true} {:placed {:id 9}} :later
    {:blocks pond :entities [placed-boat] :self {:vehicle 9}} {:board true} {:placed {:id 9}} :ok
    {:blocks pond :entities [placed-boat]} {:board false} {:placed {:id 9}} :ok
    {:blocks pond} {:board false} {:placed {:id 9}} :later))

(deftest a-place-that-fails-is-place-failed-with-its-ending-as-the-because
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond} {:board false :item "oak_boat"}))]
          (is (= :place-failed (:reason res)) (pr-str res))
          (is (some? (:because (:ending res))) "the place's ending is kept")
          (is (not= :place-failed (contract/cause-key (:because (:ending res))))))))))
