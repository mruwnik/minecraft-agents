(ns engine.fake.enchant-test
  "The enchanting table of the fake world as pure functions over cljs world data; the cases of js/fake-enchant.test.mjs."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.enchant :as enchant]))

(def at [1 64 0])
(def sword {:name "diamond_sword" :count 1})
(defn lapis [n] {:name "lapis_lazuli" :count n})

(defn owned [spec]
  (merge {:blocks {at "enchanting_table"} :self {:pos [0 64 0] :experience {:level 0}} :inventory []} spec))

(defn carried [w] (into {} (map (juxt (comp keyword :name) :count)) (:inventory w)))
(defn level [w] (get-in w [:self :experience :level]))
(defn offers [w] (second (enchant/enchant w {:pos at :op "offers" :item "diamond_sword"})))
(defn enchant-with [w choice extra]
  (enchant/enchant w (merge {:pos at :op "enchant" :item "diamond_sword" :choice choice} extra)))

(deftest offers-with-no-shelves-are-low-with-fifteen-they-reach-30
  (let [bare (offers (owned {:inventory [sword (lapis 3)] :self {:pos [0 64 0] :experience {:level 5}}}))
        shelved (offers (owned {:inventory [sword] :enchant-tables {at {:shelves 15}}}))]
    (is (= [[2 1] [3 2] [5 3]] (map (juxt :levelCost :lapisCost) (:offers bare))))
    (is (= ["ok" 5 3] [(:status bare) (:xpLevel bare) (:lapis bare)]))
    (is (= [10 20 30] (map :levelCost (:offers shelved))))))

(deftest a-table-may-be-given-exact-offers-and-hints
  (let [r (offers (owned {:inventory [sword]
                          :enchant-tables {at {:offers [7 11 13] :hints [["sharpness" 2] nil ["unbreaking" 1]]}}}))]
    (is (= [[7 {:enchant "sharpness" :level 2}] [11 nil] [13 {:enchant "unbreaking" :level 1}]]
           (map (juxt :levelCost :hint) (:offers r))))))

(deftest offers-only-read
  (let [w (owned {:inventory [sword (lapis 3)] :self {:pos [0 64 0] :experience {:level 9}}})
        [w' _] (enchant/enchant w {:pos at :op "offers" :item "diamond_sword"})]
    (is (= {:diamond_sword 1 :lapis_lazuli 3} (carried w')))
    (is (= 9 (level w')))))

(deftest enchant-spends-the-slot-number-in-lapis-and-levels
  (let [w (owned {:inventory [sword (lapis 5)] :self {:pos [0 64 0] :experience {:level 30}}
                  :enchant-tables {at {:shelves 15}}})
        [w' r] (enchant-with w 2 {:levelCost 30})]
    (is (= "enchanted" (:status r)))
    (is (= [3 3 27] [(:lapisSpent r) (:levelsSpent r) (:xpLevel r)]))
    (is (seq (:enchants r)))
    (is (= {:diamond_sword 1 :lapis_lazuli 2} (carried w')))
    (is (= 27 (level w')))
    (is (= (:enchants r) (:enchants (first (filter #(= "diamond_sword" (:name %)) (:inventory w'))))))))

(deftest a-book-comes-out-as-an-enchanted-book-one-at-a-time
  (let [w (owned {:inventory [{:name "book" :count 3} (lapis 3)] :self {:pos [0 64 0] :experience {:level 10}}})
        [w' r] (enchant/enchant w {:pos at :op "enchant" :item "book" :choice 0})]
    (is (= "enchanted" (:status r)))
    (is (= {:book 2 :enchanted_book 1 :lapis_lazuli 2} (carried w')))))

(def lvl30 {:pos [0 64 0] :experience {:level 30}})

(deftest refusals
  (doseq [[label spec op extra expected]
          [["missing" {:blocks {}} "offers" {} {:status "missing"}]
           ["a chest" {:blocks {at "chest"}} "offers" {} {:status "cannot" :reason "not-a-table"}]
           ["too far" {:blocks {[9 64 0] "enchanting_table"}} "offers" {:pos [9 64 0]} {:status "unreachable" :reason "too-far"}]
           ["item not carried" {:inventory [(lapis 3)]} "offers" {} {:status "no-item" :item "diamond_sword"}]
           ["already enchanted" {:inventory [(assoc sword :enchants [{:name "sharpness" :level 1}]) (lapis 3)]} "offers" {}
            {:status "cannot" :reason "already-enchanted" :item "diamond_sword"}]
           ["no lapis" {:inventory [sword] :self lvl30} "enchant" {:choice 0} {:status "no-lapis" :have 0 :need 1}]
           ["too few levels" {:inventory [sword (lapis 3)] :self {:pos [0 64 0] :experience {:level 2}}
                              :enchant-tables {at {:offers [4 9 16]}}} "enchant" {:choice 0}
            {:status "no-levels" :need 4 :have 2}]
           ["not enchantable" {:inventory [{:name "dirt" :count 1} (lapis 3)] :self lvl30} "enchant"
            {:item "dirt" :choice 0} {:status "cannot" :reason "not-enchantable"}]
           ["offer changed" {:inventory [sword (lapis 3)] :self lvl30} "enchant" {:choice 0 :levelCost 99}
            {:status "cannot" :reason "offer-changed" :offers [2 3 5]}]]]
    (testing label
      (let [w (owned (merge {:blocks {at "enchanting_table"}} spec))
            [w' r] (enchant/enchant w (merge {:pos at :op op :item "diamond_sword"} extra))]
        (is (= expected (select-keys r (keys expected))))
        (is (= (:inventory w) (:inventory w')))
        (is (= (level w) (level w')))))))

(deftest a-busy-table-does-not-open-its-window
  (let [[_ r] (enchant/enchant (owned {:inventory [sword] :enchant-tables {at {:busy true}}})
                               {:pos at :op "offers" :item "diamond_sword"})]
    (is (= {:status "failed" :reason "window-did-not-open"} r))))

(deftest bad-args-are-rejected
  (let [w (owned {:inventory [sword]})]
    (doseq [a [{} {:pos at} {:pos at :op "stir" :item "x"} {:pos at :op "offers"}
               {:pos at :op "enchant" :item "x" :choice 3} {:pos at :op "enchant" :item "x"}]]
      (is (= "bad-args" (try (enchant/enchant w a) nil (catch :default e (:code (ex-data e)))))))))
