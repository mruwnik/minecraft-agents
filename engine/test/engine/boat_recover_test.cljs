(ns engine.boat-recover-test
  "jobs.movement.boat-recover against the fake world: break a boat and take the item back."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [jobs.movement.boat-recover :as br]))

(def pond
  "A pond at x 4..6, z -1..1 in the stone floor (y 63), air above."
  (into {} (for [x (range 4 7) z (range -1 2)] [(str x ",63," z) "water"])))

(defn ^:async run [world args]
  (let [{:keys [eng p seen]} (lt/setup world)
        res (await (tu/child-outcome eng 'jobs.movement.boat-recover args 40))]
    {:res res :p p :seen seen :eng eng}))

(defn boats [p] (filterv #(= "oak_boat" (:name %)) (fake/entities p)))

(defn boat-at [id x & {:as more}]
  (merge {:id id :name "oak_boat" :uuid (str "u-" id) :kind "other" :pos {:x x :y 64 :z 0.5} :health 1
          :drops [{:name "oak_boat" :count 1}]}
         more))

(deftest recovers-a-boat-and-picks-up-the-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5} :health 1
                    :drops [{:name "oak_boat" :count 1}]}
              {:keys [res p]} (await (run {:blocks pond :entities [boat]} {}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (:collected res)))
          (is (= 1 (get (lt/inv p) "oak_boat")))
          (is (empty? (boats p))))))))

(deftest recover-with-no-boat-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (lt/setup {})]
          (core/submit! eng '(jobs.movement.boat-recover {}) {})
          (await (tu/run-until-empty eng 3))
          (is (= [:no-boat] (mapv :reason (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest recover-gets-off-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [boat {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos {:x 1.5 :y 64 :z 0.5} :health 1
                    :drops [{:name "oak_boat" :count 1}] :passengers [fake/fake-body-id]}
              {:keys [res p]} (await (run {:blocks pond :entities [boat] :self {:vehicle 9}} {}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (get (lt/inv p) "oak_boat"))))))))

(deftest recover-a-boat-that-takes-many-hits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run {:blocks pond :entities [(boat-at 9 1.5 :health 100)]} {}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (get (lt/inv p) "oak_boat"))))))))

(deftest recover-walks-to-a-far-boat-once-it-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res p]} (await (run {:blocks pond :entities [(boat-at 9 9.5)] :radius 20} {:radius 20}))]
          (is (= :done (:status res)) (pr-str res))
          (is (= 1 (get (lt/inv p) "oak_boat"))))))))

(deftest recover-an-id-picks-that-boat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond :entities [(boat-at 8 1.5 :drops [{:name "oak_boat" :count 1}]) (boat-at 9 3.5 :name "birch_boat" :drops [{:name "birch_boat" :count 1}])]}
                                        {:id 9}))]
          (is (= {:status :done :id 9 :item "birch_boat"} (select-keys res [:status :id :item]))))))))

(deftest recover-a-boat-that-never-breaks-ends-at-the-time-spend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (lt/setup {:blocks pond :entities [(boat-at 9 1.5 :invulnerable true)]})
              _ (.override (.-world p) "attack" (fn [token a impl] (swap! clock + 100) (impl token a)))
              res (await (tu/child-outcome eng 'jobs.movement.boat-recover { :spend {:job-time 0.3}} 40))]
          (is (= {:status :stopped :reason :job-time} (select-keys res [:status :reason])) "the engine refuses the hit the time left cannot hold"))))))

(deftest recover-without-a-drop-is-not-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [res]} (await (run {:blocks pond :entities [(boat-at 9 1.5 :drops [])]} {}))]
          (is (= {:status :stopped :reason :not-collected} (select-keys res [:status :reason]))))))))

(defn done-of [world args mem]
  (br/done? {:primitives (:p (lt/setup world)) :root "j1" :slots [] :args args
             :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data mem}]}}})}))

(deftest done-is-ok-once-the-boat-is-gone-and-its-item-carried
  (are [world mem kind] (= kind (first (done-of world {:id 9 :radius 12} mem)))
    {:blocks pond :entities [(boat-at 9 1.5)]} {} :later
    {:blocks pond :entities [(boat-at 9 1.5)] :inventory [{:name "oak_boat" :count 1}]} {:target {:id 9 :item "oak_boat" :before 0}} :later
    {:blocks pond} {:target {:id 9 :item "oak_boat" :before 0}} :later
    {:blocks pond :inventory [{:name "oak_boat" :count 1}]} {:target {:id 9 :item "oak_boat" :before 0}} :ok
    {:blocks pond :inventory [{:name "oak_boat" :count 1}]} {:target {:id 9 :item "oak_boat" :before 1}} :later))
