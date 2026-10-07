(ns engine.village-trade-test
  "jobs.village.trade against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def clock
  "One clock for every engine of a test run; each tick moves it on, so a backoff pause ends."
  (atom 1000000))

(defn start
  "An engine over primitives p (made from world when not given) on dir."
  [{:keys [world p dir]}]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (or p (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (or dir (tu/tmp-dir)) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async run-until
  "Tick while (done? eng) is false, at most n ticks."
  [eng done? n]
  (loop [i 0]
    (if (or (>= i n) (done? eng))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))
(defn kinds [seen] (set (map :kind @seen)))

(def job 'jobs.village.trade)

(def bread (fn [extra] (merge {:cost [{:item "emerald" :count 1}] :gives {:item "bread" :count 1}} extra)))

(defn villager
  "A farmer entity spec with the offers; extra fields merged over it."
  [offers & [extra]]
  (merge {:id 1 :name "villager" :kind "passive" :uuid "v-1" :pos {:x 1 :y 64 :z 0}
          :profession "farmer" :level 2 :offers offers}
         extra))

(defn fillers [n] (mapv (fn [i] {:name (str "item_" i) :count 1}) (range n)))

(defn ^:async trade
  "Run the job on a world of entities and inventory, after (prepare p); [result p seen]."
  ([world args] (trade world args identity))
  ([{:keys [entities inventory]} args prepare]
   (let [{:keys [eng p seen]} (start {:world {:entities entities :inventory inventory}})
         _ (prepare p)
         result (await (child-outcome eng job (merge {:villager "v-1" :buy "bread"} args) 80))]
     [result p seen])))

(defn trade-ops
  "An override of the trade primitive recording each op into the atom ops; the first buy asks for only one trade when first-only."
  [ops first-only]
  (let [once (atom first-only)]
    (fn ^:async f [token args impl]
      (swap! ops conj (.-op args))
      (if (and (= "buy" (.-op args)) @once)
        (do (reset! once false)
            (await (impl token (js/Object.assign #js {} args #js {:times 1}))))
        (await (impl token args))))))

(defn wait-all
  "Run each case {:world :args :want :have :kind :label} and check the result subset, the inventory and the warn kind."
  [cases]
  (js/Promise.all
   (map (fn ^:async f [{:keys [world args want have kind label]}]
          (let [[result p seen] (await (trade world args))]
            (is (= want result) label)
            (is (= have (inv p)) label)
            (is (contains? (kinds seen) kind) label)))
        cases)))

(deftest trade-check-wants-a-villager-and-an-item
  (are [args ok] (= ok ((:check (get registry/jobs job)) {:args args}))
    {:villager "v-1" :buy "bread"} true
    {:villager "v-1" :buy nil} false
    {:villager nil :buy "bread"} false))

(deftest buys-the-wanted-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (trade {:entities [(villager [(bread {})])] :inventory [{:name "emerald" :count 10}]}
                                            {:count 3}))]
          (is (= {:bought 3 :paid {"emerald" 3} :item "bread"} result))
          (is (= {"emerald" 7 "bread" 3} (inv p)))
          (is (contains? (kinds seen) :trade.done)))))))

(deftest an-offer-with-two-cost-stacks-pays-both
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [offer {:cost [{:item "emerald" :count 5} {:item "book" :count 1}] :gives {:item "bread" :count 1}}
              [result p] (await (trade {:entities [(villager [offer])] :inventory [{:name "emerald" :count 12} {:name "book" :count 3}]}
                                       {:count 2}))]
          (is (= {:bought 2 :paid {"emerald" 10 "book" 2} :item "bread"} result))
          (is (= {"emerald" 2 "book" 1 "bread" 2} (inv p))))))))

(deftest the-cheapest-offer-for-the-item-is-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [offers [(bread {:cost [{:item "emerald" :count 3}]})
                      {:cost [{:item "emerald" :count 1}] :gives {:item "apple" :count 1}}
                      (bread {:cost [{:item "emerald" :count 2}]})]
              [result p] (await (trade {:entities [(villager offers)] :inventory [{:name "emerald" :count 10}]} {:count 2}))]
          (is (= {:bought 2 :paid {"emerald" 4} :item "bread"} result))
          (is (= {"emerald" 6 "bread" 2} (inv p))))))))

(deftest an-offer-giving-several-items-rounds-the-trades-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [offer (bread {:gives {:item "bread" :count 6}})
              [result p] (await (trade {:entities [(villager [offer])] :inventory [{:name "emerald" :count 10}]} {:count 7}))]
          (is (= {:bought 12 :paid {"emerald" 2} :item "bread"} result))
          (is (= {"emerald" 8 "bread" 12} (inv p))))))))

(deftest stops-where-the-trade-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (wait-all
                [{:label "sells out mid-buy"
                  :world {:entities [(villager [(bread {:maxUses 2})]) ] :inventory [{:name "emerald" :count 10}]}
                  :args {:count 5} :want {:bought 2 :paid {"emerald" 2} :item "bread" :status :stopped :reason "sold-out"}
                  :have {"emerald" 8 "bread" 2} :kind :trade.gave-up}
                 {:label "sold out from the start"
                  :world {:entities [(villager [(bread {:maxUses 2 :uses 2})])] :inventory [{:name "emerald" :count 10}]}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "sold-out"}
                  :have {"emerald" 10} :kind :trade.gave-up}
                 {:label "no payment"
                  :world {:entities [(villager [(bread {})])]}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "payment-short"}
                  :have {} :kind :trade.gave-up}
                 {:label "partial payment"
                  :world {:entities [(villager [(bread {})])] :inventory [{:name "emerald" :count 2}]}
                  :args {:count 3} :want {:bought 2 :paid {"emerald" 2} :item "bread" :status :stopped :reason "payment-short"}
                  :have {"bread" 2} :kind :trade.gave-up}
                 {:label "no room"
                  :world {:entities [(villager [(bread {})])] :inventory (into [{:name "emerald" :count 5}] (fillers 35))}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "no-room"}
                  :have (into {"emerald" 5} (map (fn [i] [(:name i) 1])) (fillers 35)) :kind :trade.gave-up}]))))))

(deftest the-price-limit-is-the-adjusted-price-of-the-first-stack
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (wait-all
                [{:label "over the limit"
                  :world {:entities [(villager [(bread {:cost [{:item "emerald" :count 4}]}) (bread {:cost [{:item "emerald" :count 3}]})])]
                          :inventory [{:name "emerald" :count 10}]}
                  :args {:max-price 2} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "price" :price 3}
                  :have {"emerald" 10} :kind :trade.gave-up}
                 {:label "at the limit"
                  :world {:entities [(villager [(bread {:cost [{:item "emerald" :count 3}]})])]
                          :inventory [{:name "emerald" :count 10}]}
                  :args {:max-price 3} :want {:bought 1 :paid {"emerald" 3} :item "bread"}
                  :have {"emerald" 7 "bread" 1} :kind :trade.done}]))))))

(deftest gives-up-when-the-villager-offers-nothing-to-buy
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (wait-all
                [{:label "not in the world"
                  :world {:entities []}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "gone"} :have {} :kind :trade.gave-up}
                 {:label "a cow under the uuid is not listed as a villager"
                  :world {:entities [{:id 2 :name "cow" :kind "passive" :uuid "v-1" :pos {:x 1 :y 64 :z 0}}]}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "gone"} :have {} :kind :trade.gave-up}
                 {:label "unemployed"
                  :world {:entities [(villager [] {:profession nil})]}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "no-offers"} :have {} :kind :trade.gave-up}
                 {:label "nothing for the item"
                  :world {:entities [(villager [{:cost [{:item "emerald" :count 1}] :gives {:item "apple" :count 1}}])]}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "no-offer"} :have {} :kind :trade.gave-up}
                 {:label "busy villager, the window never opens"
                  :world {:entities [(villager [(bread {})] {:busy true})] :inventory [{:name "emerald" :count 3}]}
                  :args {} :want {:bought 0 :paid {} :item "bread" :status :stopped :reason "window"} :have {"emerald" 3} :kind :trade.gave-up}]))))))

(deftest a-far-villager-is-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (trade {:entities [(villager [(bread {})] {:pos {:x 30 :y 64 :z 0}})] :inventory [{:name "emerald" :count 3}]}
                                       {:count 2}))]
          (is (= {:bought 2 :paid {"emerald" 2} :item "bread"} result))
          (is (seq (tu/walk-calls p)))
          (is (every? #(<= % 15) (map #(.-timeoutS (.-args %)) (tu/walk-calls p))) "a short walk: it aims again at the villager")
          (is (= {"emerald" 1 "bread" 2} (inv p))))))))

(deftest a-restart-keeps-what-was-bought
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              a (start {:world {:entities [(villager [(bread {})])] :inventory [{:name "emerald" :count 10}]} :dir dir})
              p (:p a)
              once (atom true)
              cut (atom false)]
          (.override (.-world p) "trade"
                     (fn ^:async f [token args impl]
                       (cond
                         (and (= "buy" (.-op args)) @once)
                         (do (reset! once false)
                             (await (impl token (js/Object.assign #js {} args #js {:times 1}))))

                         (and (= "offers" (.-op args)) (not @once) (not @cut))
                         (do (reset! cut true)
                             (core/shutdown! (:eng a))
                             (await (js/Promise. (fn [_ _]))))

                         :else (await (impl token args)))))
          (core/submit! (:eng a) (list job {:villager "v-1" :buy "bread" :count 3}) {})
          (swap! clock + 700)
          (core/tick! (:eng a))
          (await (js/Promise. (fn [ok] (js/setTimeout ok 50))))
          (is (= 1 (:bought (core/job-memory (:eng a) "j1"))) "the first buy was booked before the cut")
          (let [b (start {:p p :dir dir})]
            (await (run-until-empty (:eng b) 40))
            (is (= {"emerald" 7 "bread" 3} (inv p)) "three loaves in all, none bought twice")))))))

(deftest equal-prices-take-the-lowest-index
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [offers [(bread {}) (bread {:cost [{:item "emerald" :count 1} {:item "book" :count 1}]})]
              [result p] (await (trade {:entities [(villager offers)] :inventory [{:name "emerald" :count 5} {:name "book" :count 5}]} {:count 2}))]
          (is (= {:bought 2 :paid {"emerald" 2} :item "bread"} result))
          (is (= {"emerald" 3 "book" 5 "bread" 2} (inv p))))))))

(deftest a-cheaper-sold-out-offer-is-not-chosen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [offers [(bread {:maxUses 1 :uses 1}) (bread {:cost [{:item "emerald" :count 3}]})]
              [result p] (await (trade {:entities [(villager offers)] :inventory [{:name "emerald" :count 10}]} {}))]
          (is (= {:bought 1 :paid {"emerald" 3} :item "bread"} result))
          (is (= {"emerald" 7 "bread" 1} (inv p))))))))

(deftest what-was-paid-adds-up-over-several-buys
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ops (atom [])
              [result p] (await (trade {:entities [(villager [(bread {})])] :inventory [{:name "emerald" :count 10}]} {:count 3}
                                       #(.override (.-world %) "trade" (trade-ops ops true))))]
          (is (= {:bought 3 :paid {"emerald" 3} :item "bread"} result))
          (is (= {"emerald" 7 "bread" 3} (inv p)))
          (is (= ["offers" "buy" "offers" "buy"] @ops) "two buys, the offers read again between"))))))

(deftest a-busy-villager-ends-in-three-reads-and-is-never-bought-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ops (atom [])
              [result p] (await (trade {:entities [(villager [(bread {})] {:busy true})] :inventory [{:name "emerald" :count 3}]} {}
                                       #(.override (.-world %) "trade" (trade-ops ops false))))]
          (is (= {:bought 0 :paid {} :item "bread" :status :stopped :reason "window"} result))
          (is (= ["offers" "offers" "offers"] @ops))
          (is (= {"emerald" 3} (inv p))))))))

(deftest a-whole-purchase-is-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world {:entities [(villager [(bread {})] {:pos {:x 30 :y 64 :z 0}})] :inventory [{:name "emerald" :count 5}]}})]
          (core/submit! eng (list job {:villager "v-1" :buy "bread" :count 2}) {})
          (swap! clock + 700)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "walk, offers and both buys in one round")
          (is (= {"emerald" 3 "bread" 2} (inv p))))))))

(deftest a-busy-villager-gives-up-in-one-round-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (start {:world {:entities [(villager [(bread {})] {:busy true})] :inventory [{:name "emerald" :count 3}]}})]
          (core/submit! eng (list job {:villager "v-1" :buy "bread"}) {})
          (swap! clock + 700)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))
