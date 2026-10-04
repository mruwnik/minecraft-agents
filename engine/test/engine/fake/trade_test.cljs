(ns engine.fake.trade-test
  "A villager's trade in the fake world as pure functions over cljs world data; the cases of js/fake-trade.test.mjs."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.trade :as trade]))

(defn offer [cost gives & [extra]] (merge {:cost cost :gives gives} extra))
(def emerald-for-wheat (offer [{:item "wheat" :count 20}] {:item "emerald" :count 1}))
(def pair (offer [{:item "emerald" :count 5} {:item "book" :count 1}] {:item "enchanted_book" :count 1} {:maxUses 4 :uses 1}))
(def farmer {:name "villager" :uuid "v-1" :pos [1 64 0] :profession "farmer" :level 2 :offers [emerald-for-wheat pair]})

(defn rig [entities & [inventory]] {:self {:pos [0 64 0]} :entities entities :inventory (or inventory [])})
(defn count-of [w name] (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 (:inventory w)))
(def read-args {:villager "v-1" :op "offers"})
(defn buy [offer-idx & [times]] (merge {:villager "v-1" :op "buy" :offer offer-idx} (when times {:times times})))

(deftest refusals
  (doseq [{:keys [label ent args want]}
          [{:label "gone" :ent [] :args read-args :want {:status "gone"}}
           {:label "not a villager" :ent [(-> farmer (assoc :name "cow") (dissoc :offers))] :args read-args
            :want {:status "cannot" :reason "not-villager" :name "cow"}}
           {:label "too far" :ent [(assoc farmer :pos [9 64 0])] :args read-args :want {:status "out-of-reach" :reason "too-far" :distance 9}}
           {:label "unemployed by default" :ent [(-> farmer (dissoc :profession) (assoc :offers []))] :args read-args
            :want {:status "cannot" :reason "no-offers" :why "unemployed" :profession "unemployed" :level 2}}
           {:label "nitwit" :ent [(assoc farmer :profession "nitwit" :offers [])] :args read-args
            :want {:status "cannot" :reason "no-offers" :why "nitwit" :profession "nitwit" :level 2}}
           {:label "baby" :ent [(assoc farmer :baby true)] :args read-args
            :want {:status "cannot" :reason "no-offers" :why "baby" :profession "farmer" :level 2}}
           {:label "busy" :ent [(assoc farmer :busy true)] :args read-args :want {:status "failed" :reason "window-did-not-open"}}
           {:label "busy on a buy" :ent [(assoc farmer :busy true)] :args (buy 0) :want {:status "failed" :reason "window-did-not-open"}}
           {:label "zero offers" :ent [(assoc farmer :offers [])] :args read-args
            :want {:status "cannot" :reason "no-offers" :why "none" :profession "farmer" :level 2}}
           {:label "no offers field" :ent [(dissoc farmer :offers)] :args read-args
            :want {:status "cannot" :reason "no-offers" :why "none" :profession "farmer" :level 2}}]]
    (testing label
      (let [[_ r] (trade/trade (rig ent) args)]
        (is (= want (select-keys r (keys want))))))))

(deftest offers-read-lists-both-cost-stacks-uses-left-disabled-profession-and-level
  (let [sold (offer [{:item "wheat" :count 1}] {:item "bread" :count 2} {:maxUses 3 :uses 3})
        [_ r] (trade/trade (rig [(assoc farmer :offers [emerald-for-wheat pair sold])]) read-args)]
    (is (= {:status "ok" :uuid "v-1" :profession "farmer" :level 2
            :offers [{:index 0 :cost [{:item "wheat" :count 20}] :gives {:item "emerald" :count 1} :uses 0 :maxUses 12 :left 12 :disabled false}
                     {:index 1 :cost [{:item "emerald" :count 5} {:item "book" :count 1}] :gives {:item "enchanted_book" :count 1} :uses 1 :maxUses 4 :left 3 :disabled false}
                     {:index 2 :cost [{:item "wheat" :count 1}] :gives {:item "bread" :count 2} :uses 3 :maxUses 3 :left 0 :disabled true}]}
           r))))

(deftest level-defaults-to-1
  (is (= 1 (:level (second (trade/trade (rig [(dissoc farmer :level)]) read-args))))))

(deftest bad-args-reject
  (let [w (rig [farmer])]
    (doseq [a [{} {:villager "" :op "offers"} {:villager "v-1" :op "sell"} {:villager "v-1" :op "buy"}
               {:villager "v-1" :op "buy" :offer -1} {:villager "v-1" :op "buy" :offer 0 :times 0}
               {:villager "v-1" :op "buy" :offer 0 :times 1.5}]]
      (is (= "bad-args" (try (trade/trade w a) nil (catch :default e (:code (ex-data e)))))))))

(defn wheat [n] [{:name "wheat" :count n}])
(defn fillers [n] (mapv #(hash-map :name (str "item_" %) :count 1) (range n)))
(defn cheap [& [extra]] (offer [{:item "wheat" :count 1}] {:item "emerald" :count 32} extra))
(def two-uses (offer (:cost emerald-for-wheat) (:gives emerald-for-wheat) {:maxUses 2}))
(def spent (offer (:cost emerald-for-wheat) (:gives emerald-for-wheat) {:maxUses 2 :uses 2}))

(deftest buys
  (doseq [{:keys [label ent inv args want have] expected-uses :uses}
          [{:label "no such offer" :ent [farmer] :args (buy 2) :want {:status "cannot" :reason "no-such-offer" :offers 2}}
           {:label "sold out" :ent [(assoc farmer :offers [spent])] :inv (wheat 40) :args (buy 0) :want {:status "cannot" :reason "sold-out"}}
           {:label "sold out before payment" :ent [(assoc farmer :offers [spent])] :args (buy 0) :want {:status "cannot" :reason "sold-out"}}
           {:label "payment short" :ent [farmer] :inv (wheat 5) :args (buy 0) :want {:status "no-item" :short {:wheat 15}}}
           {:label "second stack short" :ent [farmer] :inv [{:name "emerald" :count 5}] :args (buy 1) :want {:status "no-item" :short {:book 1}}}
           {:label "both stacks short" :ent [farmer] :inv [{:name "emerald" :count 2}] :args (buy 1) :want {:status "no-item" :short {:emerald 3 :book 1}}}
           {:label "full inventory" :ent [farmer] :inv (into (wheat 30) (fillers 35)) :args (buy 0) :want {:status "full"}}
           {:label "normal buy" :ent [farmer] :inv (wheat 45) :args (buy 0)
            :want {:status "bought" :times 1 :requested 1 :gained {:emerald 1} :paid {:wheat 20} :stopped nil}
            :have {:wheat 25 :emerald 1} :uses [0 1]}
           {:label "two cost stacks" :ent [farmer] :inv [{:name "emerald" :count 12} {:name "book" :count 3}] :args (buy 1 2)
            :want {:status "bought" :times 2 :requested 2 :gained {:enchanted_book 2} :paid {:emerald 10 :book 2} :stopped nil}
            :have {:emerald 2 :book 1 :enchanted_book 2} :uses [1 3]}
           {:label "clamped by uses" :ent [(assoc farmer :offers [two-uses])] :inv (vec (concat (wheat 64) (wheat 64) (wheat 64))) :args (buy 0 5)
            :want {:status "bought" :times 2 :requested 5 :gained {:emerald 2} :paid {:wheat 40} :stopped "sold-out"}
            :have {:wheat 152 :emerald 2} :uses [0 2]}
           {:label "clamped by payment" :ent [farmer] :inv (wheat 45) :args (buy 0 5)
            :want {:status "bought" :times 2 :requested 5 :gained {:emerald 2} :paid {:wheat 40} :stopped "payment"}
            :have {:wheat 5 :emerald 2} :uses [0 2]}
           {:label "clamped by room" :ent [(assoc farmer :offers [(cheap)])] :inv (into (wheat 64) (fillers 34)) :args (buy 0 5)
            :want {:status "bought" :times 2 :requested 5 :gained {:emerald 64} :paid {:wheat 2} :stopped "room"}
            :have {:wheat 62 :emerald 64} :uses [0 2]}
           {:label "room in a partial stack" :ent [(assoc farmer :offers [(cheap)])]
            :inv (-> (wheat 64) (conj {:name "emerald" :count 60}) (into (fillers 33))) :args (buy 0 5)
            :want {:status "bought" :times 2 :requested 5 :gained {:emerald 64} :paid {:wheat 2} :stopped "room"}
            :have {:wheat 62 :emerald 124} :uses [0 2]}
           {:label "sold-out wins over payment and room" :ent [(assoc farmer :offers [(cheap {:maxUses 1})])] :inv (wheat 1) :args (buy 0 3)
            :want {:status "bought" :times 1 :requested 3 :gained {:emerald 32} :paid {:wheat 1} :stopped "sold-out"}
            :have {:emerald 32} :uses [0 1]}]]
    (testing label
      (let [w (rig ent (or inv []))
            [w' r] (trade/trade w args)
            idx (:offer args)
            used #(get-in % [:entities 0 :offers idx :uses] 0)
            before (used w)]
        (is (= want (select-keys r (keys want))))
        (is (= (or have {}) (into {} (map (fn [k] [k (count-of w' (name k))])) (keys have))))
        (is (= (or expected-uses [before before]) [before (used w')]))))))

(deftest a-further-buy-after-the-clamp-says-sold-out-and-the-read-shows-it
  (let [w (rig [(assoc farmer :offers [two-uses])] (wheat 100))
        [w1 _] (trade/trade w (buy 0 5))
        [w2 again] (trade/trade w1 (buy 0))
        [_ read] (trade/trade w2 read-args)
        o (first (:offers read))]
    (is (= [{:status "cannot" :reason "sold-out"} 0 true 2] [again (:left o) (:disabled o) (count-of w2 "emerald")]))))

(deftest the-stacks-never-pass-64-and-36-slots
  (let [[w _] (trade/trade (rig [(assoc farmer :offers [(cheap)])] (into (wheat 64) (fillers 34))) (buy 0 5))
        stacks (:inventory w)]
    (is (<= (count stacks) 36))
    (is (<= (apply max (map :count stacks)) 64))))
