(ns engine.boat-launch-test
  "jobs.movement.boat-launch against the fake world: put a boat on water and board it, and take it back."
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
  "A pond at x 4..6, z -1..1 in the stone floor (y 63), air above."
  (into {} (for [x (range 4 7) z (range -1 2)] [(str x ",63," z) "water"])))

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
          (is (some? (:vehicle (js->clj (.self p) :keywordize-keys true))) "aboard")
          (is (some #(= :boat.launched (:kind %)) @seen)))))))

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
  "A pond at x 3..7, z -2..2 in the stone floor (y 63): the ring of cells beside the bank has a bank block within a boat's width."
  (into {} (for [x (range 3 8) z (range -2 3)] [(str x ",63," z) "water"])))

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

(deftest recovers-a-boat-and-picks-up-the-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5} :health 1
                    :drops [{:name "oak_boat" :count 1}]}
              {:keys [res p]} (await (run {:blocks pond :entities [boat]} {:action :recover}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (:collected res)))
          (is (= 1 (get (lt/inv p) "oak_boat")))
          (is (empty? (boats p))))))))

(deftest recover-with-no-boat-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {})]
          (core/submit! eng '(jobs.movement.boat-launch {:action :recover}) {})
          (await (tu/run-until-empty eng 3))
          (is (= [:no-boat] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest recover-gets-off-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5} :health 1
                    :drops [{:name "oak_boat" :count 1}] :passengers [fake/fake-body-id]}
              {:keys [res p]} (await (run {:blocks pond :entities [boat] :self {:vehicle 9}} {:action :recover}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (get (lt/inv p) "oak_boat"))))))))

(deftest boat-names
  (is (every? bl/boat-name? ["oak_boat" "bamboo_raft" "oak_chest_boat"]))
  (is (not-any? bl/boat-name? ["minecart" nil "boat_x"])))

(defn boat-at [id x & {:as more}]
  (merge {:id id :name "oak_boat" :uuid (str "u-" id) :kind "other" :pos {:x x :y 64 :z 0.5} :health 1
          :drops [{:name "oak_boat" :count 1}]}
         more))

(deftest recover-a-boat-that-takes-many-hits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run {:blocks pond :entities [(boat-at 9 1.5 :health 100)]} {:action :recover}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (get (lt/inv p) "oak_boat"))))))))

(deftest recover-walks-to-a-far-boat-once-it-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run {:blocks pond :entities [(boat-at 9 9.5)] :radius 20} {:action :recover :radius 20}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (get (lt/inv p) "oak_boat"))))))))

(deftest recover-an-id-picks-that-boat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond :entities [(boat-at 8 1.5 :drops [{:name "oak_boat" :count 1}]) (boat-at 9 3.5 :name "birch_boat" :drops [{:name "birch_boat" :count 1}])]}
                                        {:action :recover :id 9}))]
          (is (= {:status :done :id 9 :item "birch_boat"} (select-keys res [:status :id :item]))))))))

(deftest recover-a-boat-that-never-breaks-ends-at-the-time-spend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (lt/setup {:blocks pond :entities [(boat-at 9 1.5 :invulnerable true)]})
              _ (.override (.-world p) "attack" (fn [token a impl] (swap! clock + 100) (impl token a)))
              res (await (tu/child-outcome eng 'jobs.movement.boat-launch {:action :recover :spend {:job-time 0.3}} 40))]
          (is (= {:status :stopped :reason :job-time} (select-keys res [:status :reason])) "the engine refuses the hit the time left cannot hold"))))))

(deftest recover-without-a-drop-is-not-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond :entities [(boat-at 9 1.5 :drops [])]} {:action :recover}))]
          (is (= {:status :stopped :reason :not-collected} (select-keys res [:status :reason]))))))))

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
  "Water one cell wide at x 4, z -2..2: every cell has a bank block beside it."
  (into {} (for [z (range -2 3)] [(str "4,63," z) "water"])))

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
              c (assoc (st/stub-ctx p) :args {:radius 12})
              cell (await (bl/launch-pos! c))]
          (is (some? cell))
          (is (some #(= :sense.looked (:kind %)) @(:events c)) "it looked at what it had not seen"))))))

(deftest a-non-boat-item-is-impossible
  (is (= [:never :bad-args] ((juxt first #(:reason (nth % 2))) (bl/bad-args {:args {:item "stick"}}))))
  (is (= :ok (first (bl/bad-args {:args {:item "oak_boat"}}))))
  (is (= :ok (first (bl/bad-args {:args {}})))))

(deftest aboard-is-a-start-condition-for-a-launch-only
  (let [other {:id 5 :name "minecart" :uuid "u-5" :kind "other" :pos {:x 0.5 :y 64 :z 0.5}}
        c (fn [args] (assoc (st/stub-ctx (:p (lt/setup {:blocks pond :entities [other] :self {:vehicle 5}}))) :args args))]
    (is (= [:later :aboard] ((juxt first #(:reason (nth % 2))) (bl/aboard (c {:action :launch})))))
    (is (= :ok (first (bl/aboard (c {:action :recover})))))))

(deftest no-water-is-a-condition-that-asks-for-a-look
  (let [c (fn [world] (assoc (st/stub-ctx (:p (lt/setup world))) :args {:radius 12 :action :launch}))]
    (is (= :ok (first (bl/no-water (c {:blocks pond})))))
    (is (= [:look :no-water] ((juxt first #(:reason (nth % 2))) (bl/no-water (c {})))))
    (is (= [:look :no-room] ((juxt first #(:reason (nth % 2))) (bl/no-water (c {:blocks channel})))))))

(deftest done-carries-the-result-so-far-for-a-limit-ending
  (let [c (fn [m args] {:primitives (tu/fake {}) :root "j1" :slots [] :args args
                        :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data m}]}}})})]
    (is (= {:units :boats :done 1 :left 0 :result {:placed true}}
           (select-keys (nth (bl/done? (c {:placed {:id 7}} {})) 2) [:units :done :left :result])))
    (is (= {:done 0 :left 1 :result {:placed false}}
           (select-keys (nth (bl/done? (c {} {})) 2) [:done :left :result])))
    (is (= {:done 0 :result {:id 7 :hits 2}}
           (select-keys (nth (bl/done? (c {:hits 2} {:action :recover :id 7})) 2) [:done :result])))))
