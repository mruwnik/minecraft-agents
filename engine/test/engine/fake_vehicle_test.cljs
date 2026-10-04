(ns engine.fake-vehicle-test
  "engine.fake vehicles: mount and dismount (a port of js/fake-vehicle.test.mjs)."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.fake :as fake]
            [engine.test-util :as tu]))

(def body-id fake/fake-body-id)

(defn at [x y z] {:x x :y y :z z})

(defn boat [& {:as extra}]
  (merge {:id 9 :name "oak_boat" :uuid "u-9" :kind "other" :pos (at 1 64 0)} extra))

(defn cow [& {:as extra}]
  (merge {:id 7 :name "cow" :uuid "u-7" :kind "passive" :pos (at 2 64 0)} extra))

(defn rig [spec]
  (doto (fake/create spec) (.setOwner "t")))

(defn got [x] (js->clj x :keywordize-keys true))

(defn async-test [f]
  (async done (tu/run-async done f)))

(defn ^:async for-each
  "Await (f case) for each case, in order."
  [f cases]
  (loop [[c & more] (seq cases)]
    (when c
      (await (f c))
      (recur more))))

(defn vehicle-id [p] (:vehicle (got (.self p))))

(defn entity [p id] (first (filter #(= id (:id %)) (fake/entities p))))

(def boat-vehicle {:id 9 :uuid "u-9" :name "oak_boat"})

(deftest self-vehicle-names-the-vehicle
  (is (nil? (vehicle-id (rig {}))))
  (is (= boat-vehicle (vehicle-id (rig {:self {:vehicle 9} :entities [(boat)]})))))

(deftest entities-carry-passengers-and-vehicle
  (let [p (rig {:entities [(boat :passengers [7]) (cow :vehicle 9)]})]
    (is (= [7] (:passengers (entity p 9))))
    (is (= 9 (:vehicle (entity p 7))))))

(def refusals
  [["already-mounted" {:self {:vehicle 9} :entities [(boat)]} {:status "already-mounted" :vehicle boat-vehicle}]
   ["gone" {} {:status "gone"}]
   ["not-mountable" {:entities [(boat :name "cow")]} {:status "not-mountable"}]
   ["occupied" {:entities [(boat :passengers [7 8])]} {:status "occupied"}]
   ["out-of-reach" {:entities [(boat :pos (at 5 64 0))]} {:status "out-of-reach"}]
   ["timeout" {:entities [(boat)] :mountFails true} {:status "timeout"}]])

(deftest mount-refuses
  (async-test
   (fn ^:async t []
     (await
      (for-each
       (fn ^:async t [[label spec expected]]
         (let [p (rig spec)
               before (vehicle-id p)
               result (got (await (.mount p "t" #js {:id 9})))]
           (is (= expected result) label)
           (is (= before (vehicle-id p)) label)))
       refusals)))))

(deftest mount-seats-the-body
  (async-test
   (fn ^:async t []
     (let [p (rig {:self {:held "lead"} :inventory [{:name "lead" :count 1}] :entities [(boat)]})
           result (got (await (.mount p "t" #js {:id 9})))]
       (is (= {:status "mounted" :vehicle boat-vehicle} result))
       (is (= [1 64 0] (:pos (fake/self p))))
       (is (nil? (:held (fake/self p))))
       (is (= [body-id] (:passengers (entity p 9))))))))

(deftest dismount-not-mounted
  (async-test
   (fn ^:async t []
     (is (= {:status "not-mounted"} (got (await (.dismount (rig {}) "t" #js {}))))))))

(deftest dismount-timeout-keeps-the-body-aboard
  (async-test
   (fn ^:async t []
     (let [p (rig {:self {:vehicle 9} :entities [(boat)] :dismountFails true})]
       (is (= {:status "timeout" :mounted true} (got (await (.dismount p "t" #js {})))))
       (is (= 9 (:id (vehicle-id p))))))))

(deftest dismount-lands-at-dismount-at-and-records-yaw
  (async-test
   (fn ^:async t []
     (let [p (rig {:self {:vehicle 9}
                   :entities [(boat :passengers [body-id] :dismountAt (at 1 65 3))]
                   :blocks {"1,64,3" "stone"}})
           result (got (await (.dismount p "t" #js {:yaw 0})))]
       (is (= {:status "dismounted" :pos (at 1 65 3)} result))
       (is (nil? (vehicle-id p)))
       (is (= [1 65 3] (:pos (fake/self p))))
       (is (not (contains? (entity p 9) :passengers)))
       (is (= 0 (:dismount-yaw @(fake/state p))))))))

(deftest dismount-without-dismount-at-lands-beside-the-vehicle
  (async-test
   (fn ^:async t []
     (let [p (rig {:self {:vehicle 9} :entities [(boat)] :blocks {"2,64,0" "water"}})
           result (got (await (.dismount p "t" #js {})))]
       (is (= {:status "dismounted" :pos (at 2 64 0)} result))
       (is (true? (:inWater (fake/self p))))))))

(deftest move-to-and-steer-answer-mounted-while-aboard
  (async-test
   (fn ^:async t []
     (await
      (for-each
       (fn ^:async t [[method args]]
         (let [p (rig {:self {:vehicle 9} :entities [(boat)]})
               result (got (await (js-invoke p method "t" args)))]
           (is (= {:status "mounted"} result) method)
           (is (= [0 64 0] (:pos (fake/self p))) method)))
       [["moveTo" #js {:pos (at 10 64 0)}]
        ["steer" #js {:timeoutS 1 :decide (fn [] #js {:done true})}]])))))
