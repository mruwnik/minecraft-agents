(ns engine.fake.furnace-test
  "The furnace of the fake world as pure functions over cljs world data; the cases of js/fake-furnace.test.mjs."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.furnace :as furnace]))

(def at [1 64 0])

(defn owned
  ([spec] (owned spec "furnace"))
  ([spec block]
   (furnace/start (merge {:blocks {at block} :self {:pos [0 64 0]} :inventory []} spec))))

(defn carried [w] (into {} (map (juxt (comp keyword :name) :count)) (:inventory w)))
(defn lit [w] (get-in w [:states at :lit]))
(defn load-in [w input fuel]
  (furnace/furnace w (merge {:pos at :op "load"} (when input {:input input}) (when fuel {:fuel fuel}))))
(defn read-at [w] (second (furnace/furnace w {:pos at :op "read"})))
(defn adv [w ticks] (furnace/advance w ticks))
(defn loaded [w input fuel] (first (load-in w input fuel)))

(def stock [{:name "raw_iron" :count 5} {:name "beef" :count 5} {:name "coal" :count 3} {:name "oak_planks" :count 4}])

(deftest load-puts-input-and-fuel-in-and-answers-what-moved
  (let [[w r] (load-in (owned {:inventory stock}) {:item "raw_iron" :count 3} {:item "coal" :count 1})]
    (is (= ["ok" {:input 3 :fuel 1} {:name "raw_iron" :count 3} {:name "coal" :count 1} nil]
           ((juxt :status :moved :input :fuel :output) r)))
    (is (= {:raw_iron 2 :beef 5 :coal 2 :oak_planks 4} (carried w)))))

(deftest cooking-rates
  (doseq [{:keys [block input output ticks]}
          [{:block "furnace" :input "raw_iron" :output "iron_ingot" :ticks 200}
           {:block "blast_furnace" :input "raw_iron" :output "iron_ingot" :ticks 100}
           {:block "smoker" :input "beef" :output "cooked_beef" :ticks 100}
           {:block "furnace" :input "beef" :output "cooked_beef" :ticks 200}]]
    (testing (str block " " input)
      (let [w (loaded (owned {:inventory stock} block) {:item input :count 2} {:item "coal" :count 1})
            early (adv w (dec ticks))
            first-done (adv early 1)
            both (adv first-done ticks)]
        (is (nil? (:output (read-at early))))
        (is (= {:name output :count 1} (:output (read-at first-done))))
        (is (= [{:name output :count 2} nil] ((juxt :output :input) (read-at both))))))))

(deftest lit-while-burning-and-out-with-the-last-fuel
  (let [w (loaded (owned {:inventory stock}) {:item "raw_iron" :count 1} {:item "oak_planks" :count 1})]
    (is (false? (lit (owned {:inventory stock}))))
    (is (true? (lit (adv w 1))))
    (is (false? (lit (adv w 301))))))

(deftest one-coal-burns-for-eight-items
  (let [w (loaded (owned {:inventory [{:name "raw_iron" :count 10} {:name "coal" :count 1}]})
                  {:item "raw_iron" :count 10} {:item "coal" :count 1})
        after (adv w 2000)
        r (read-at after)]
    (is (= [8 2 false false] [(:count (:output r)) (:count (:input r)) (:lit r) (lit after)]))))

(deftest smoker-burns-fuel-twice-as-fast-eight-items-the-same
  (let [[w loaded-r] (load-in (owned {:inventory [{:name "beef" :count 10} {:name "coal" :count 1}]} "smoker")
                              {:item "beef" :count 10} {:item "coal" :count 1})]
    (is (= {:left 800 :total 800} (:burn (read-at (adv w 1)))))
    (is (= [8 "ok"] [(:count (:output (read-at (adv w 2001)))) (:status loaded-r)]))))

(deftest input-slot-takes-anything-what-the-kind-cannot-smelt-sits-there
  (let [[w r] (load-in (owned {:inventory stock} "smoker") {:item "raw_iron" :count 2} {:item "coal" :count 1})
        after (read-at (adv w 1000))]
    (is (= ["ok" {:name "raw_iron" :count 2} nil {:name "coal" :count 1} false]
           [(:status r) (:input after) (:output after) (:fuel after) (:lit after)]))))

(deftest without-fuel-nothing-cooks
  (let [r (read-at (adv (loaded (owned {:inventory stock}) {:item "raw_iron" :count 2} nil) 1000))]
    (is (= [2 nil false] [(:count (:input r)) (:output r) (:lit r)]))))

(deftest read-reports-the-bars
  (let [r (read-at (adv (loaded (owned {:inventory stock}) {:item "raw_iron" :count 2} {:item "coal" :count 1}) 50))]
    (is (= [true {:left 1551 :total 1600} {:done 50 :total 200}] [(:lit r) (:burn r) (:cook r)]))))

(deftest take-brings-the-output-input-and-fuel-only-on-request
  (let [w (adv (loaded (owned {:inventory stock}) {:item "raw_iron" :count 3} {:item "coal" :count 2}) 400)
        [w1 r] (furnace/furnace w {:pos at :op "take"})
        [w2 all] (furnace/furnace w1 {:pos at :op "take" :input true :fuel true})]
    (is (= ["ok" [{:part "output" :name "iron_ingot" :count 2}] {:name "raw_iron" :count 1} {:name "coal" :count 1}]
           [(:status r) (:taken r) (:input r) (:fuel r)]))
    (is (= ["input" "fuel"] (map :part (:taken all))))
    (is (= {:raw_iron 3 :beef 5 :coal 2 :oak_planks 4 :iron_ingot 2} (carried w2)))))

(deftest take-with-no-room-leaves-the-output
  (let [filler (mapv #(hash-map :name (str "item_" %) :count 1) (range 34))
        w (loaded (owned {:inventory (into filler [{:name "raw_iron" :count 1} {:name "coal" :count 2}])})
                  {:item "raw_iron" :count 1} {:item "coal" :count 1})
        w (-> w (update :inventory conj {:name "dirt" :count 1}) (adv 200))
        [_ r] (furnace/furnace w {:pos at :op "take"})]
    (is (= ["full" [] {:name "iron_ingot" :count 1}] [(:status r) (:taken r) (:output r)]))))

(deftest refused-as-data
  (doseq [{:keys [label spec args expect]}
          [{:label "no block there" :args {:pos [2 64 0] :op "read"} :expect {:status "missing" :block "air"}}
           {:label "a chest" :spec {:blocks {at "chest"}} :args {:op "read"} :expect {:status "cannot" :reason "not-a-furnace" :block "chest"}}
           {:label "too far" :spec {:blocks {at "furnace" [9 64 0] "furnace"}} :args {:pos [9 64 0] :op "read"}
            :expect {:status "unreachable" :reason "too-far" :distance 9}}
           {:label "an item not carried" :args {:op "load" :input {:item "cobblestone" :count 1}}
            :expect {:status "no-item" :slot "input" :item "cobblestone"}}
           {:label "beef as fuel" :args {:op "load" :fuel {:item "beef" :count 1}}
            :expect {:status "rejected" :slot "fuel" :item "beef" :reason "not-accepted"}}]]
    (testing label
      (let [w (owned (merge {:inventory stock} spec))
            [w' r] (furnace/furnace w (merge {:pos at} args))]
        (is (= expect r))
        (is (= (carried w) (carried w')))))))

(deftest a-slot-holding-another-item-is-busy
  (let [w (loaded (owned {:inventory stock}) {:item "beef" :count 2} nil)
        [_ r] (load-in w {:item "raw_iron" :count 1} nil)]
    (is (= {:status "busy" :slot "input" :holds {:name "beef" :count 2}} r))))

(deftest a-furnace-can-start-with-stacks-in-it
  (let [w (owned {:furnaces {at {:input {:name "raw_iron" :count 2} :fuel {:name "coal" :count 1}}}})]
    (is (= {:name "iron_ingot" :count 1} (:output (read-at (adv w 200)))))))

(deftest a-furnace-block-that-is-gone-drops-its-stacks
  (let [w (loaded (owned {:inventory stock}) {:item "raw_iron" :count 2} {:item "coal" :count 1})
        gone (adv (update w :blocks dissoc at) 5)]
    (is (empty? (:furnaces gone)))))

(deftest a-full-output-slot-stops-the-cooking
  (let [w (owned {:furnaces {at {:input {:name "raw_iron" :count 2} :fuel {:name "coal" :count 1}
                                 :output {:name "iron_ingot" :count 64}}}})
        r (read-at (adv w 1000))]
    (is (= [{:name "iron_ingot" :count 64} {:name "raw_iron" :count 2} 0 false]
           [(:output r) (:input r) (:done (:cook r)) (:lit r)]))))
