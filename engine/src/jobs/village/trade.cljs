(ns jobs.village.trade
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]))

(def doc
  "Buy :count of :buy from the villager with uuid :villager. One call is the whole purchase: find the villager
  within 48 blocks, walk within 2 of it (jobs.movement.go-to legs of 15 s), read its offers (prices move between visits),
  choose and buy. It yields (:continue) only while go-to waits on the world.
  Of the offers that give :buy and are not sold out, those whose price (the adjusted count of the first cost
  stack) is within :max-price (nil: no limit) are kept. The cheapest is taken, the lowest index on a tie.
  :count is items wanted. An offer gives several per trade, so it buys ceil(remaining / items per trade) trades
  and may overshoot a little.
  Result: {:bought n :paid {item n} :item :buy}. n counts the items actually gained. Success is info trade.done.
  A give-up ends stopped with :reason and warns trade.gave-up. A partial purchase stays bought and paid.
  :reason is one of:
  - \"gone\" (not listed within 48, also a non-villager entity under the uuid), \"not-villager\".
  - \"unreachable\" (walk blocked, or still out of reach after three tries in the call).
  - \"no-offers\" (unemployed, nitwit, baby, or an empty window), \"window\" (did not open three times: busy
    villager), \"no-offer\" (nothing gives :buy), \"sold-out\".
  - \"price\" (every open offer is dearer than :max-price; :price is the cheapest seen).
  - \"payment-short\", \"no-room\", \"incomplete\" (a buy stopped short for no reason, three times), or the
    primitive's failure reason.
  What was bought and paid is kept across a cut and restart, so a purchase is neither lost nor repeated.")

(def args
  {:villager {:doc "the villager's entity uuid" :default nil}
   :buy {:doc "item name to buy" :default nil}
   :count {:doc "items wanted" :default 1}
   :max-price {:doc "highest price per trade (first cost stack); nil for no limit" :default nil}})

(def search-radius 48)
(def reach 2)

(def chase-timeout-s
  "Bound of one walk toward the villager: it wanders, so the walk aims again at where it is now this often."
  15)

(defn check
  "A villager uuid and an item name are given."
  [c]
  (let [{:keys [villager buy]} (:args c)]
    (or (and (string? villager) (string? buy))
        (ctx/wait c {:reason :bad-args :why "needs a villager uuid and an item name"}))))

(defn finish!
  "Hand the parent the counts so far plus extra and return :done."
  [c extra]
  (let [m (ctx/mem c)]
    (ctx/result! c (merge {:bought (:bought m 0)
                           :paid (into {} (map (fn [[k v]] [(name k) v])) (:paid m))
                           :item (:buy (:args c))}
                          extra))
    :done))

(defn give-up!
  "Warn and end stopped with a reason."
  [c reason text]
  (ctx/emit! c :trade.gave-up :warn {:reason reason :text text})
  (finish! c {:status :stopped :reason reason}))

(defn retry
  "A step that failed but may be tried again: counted per call, the third in a row gives up (warn trade.gave-up)."
  [reason text]
  {:retry [reason text]})

(defn done!
  "Info and finish with the counts."
  [c]
  (let [m (ctx/mem c)]
    (ctx/emit! c :trade.done :info {:bought (:bought m 0) :paid (:paid m) :text (str "bought " (:bought m 0) " " (:buy (:args c)))})
    (finish! c {})))

(defn find-villager
  "The entity named villager with uuid within the search radius, or nil."
  [p uuid]
  (->> (array-seq (.entities p #js {:radius search-radius :names #js ["villager"]}))
       (filter #(= uuid (.-uuid %)))
       first))

(defn choose
  "Pick an offer from the read offers: {:offer o} with :price, or {:reason
  r} (and :price, the cheapest open one, for \"price\")."
  [offers buy max-price]
  (let [mine (filter #(= buy (get-in % [:gives :item])) offers)
        open (map #(assoc % :price (get-in % [:cost 0 :count])) (remove :disabled mine))
        fit (filter #(or (nil? max-price) (<= (:price %) max-price)) open)]
    (cond
      (empty? mine) {:reason "no-offer"}
      (empty? open) {:reason "sold-out"}
      (empty? fit) {:reason "price" :price (apply min (map :price open))}
      :else {:offer (first (sort-by (juxt :price :index) fit))})))

(def stopped-reason {"sold-out" "sold-out" "payment" "payment-short" "room" "no-room"})

(defn bought!
  "Book what a buy gained and paid (measured by the primitive), then finish
  when enough was bought, give up by what stopped it, or :again (a retry when it stopped short for no reason)."
  [c r]
  (let [{:keys [buy count]} (:args c)
        gained (or (aget (.-gained r) buy) 0)
        stopped (.-stopped r)]
    (ctx/update-mem! c (fn [m] (-> m
                                   (update :bought (fnil + 0) gained)
                                   (update :paid #(merge-with + % (js->clj (.-paid r)))))))
    (cond
      (>= (:bought (ctx/mem c) 0) count) (done! c)
      (contains? stopped-reason stopped) (give-up! c (stopped-reason stopped) (str "stopped buying: " stopped))
      (pos? gained) :again
      :else (retry "incomplete" "a buy stopped short"))))

(defn ^:async buy!
  "Buy the trades still wanted of the chosen offer and handle the outcome."
  [c offer]
  (let [{:keys [villager buy count]} (:args c)
        per (get-in offer [:gives :count])
        times (js/Math.ceil (/ (- count (:bought (ctx/mem c) 0)) per))
        r (await (ctx/act c :trade (clj->js {:villager villager :op "buy" :offer (:index offer) :times times})))
        status (.-status r)]
    (case status
      "bought" (bought! c r)
      "no-item" (give-up! c "payment-short" "cannot pay")
      "full" (give-up! c "no-room" "no room for the goods")
      "gone" (give-up! c "gone" "the villager is gone")
      "cannot" (if (= "sold-out" (.-reason r))
                 (give-up! c "sold-out" "sold out")
                 (retry "no-such-offer" "the offers changed"))
      (retry (str (.-reason r)) (str "trade " status " " (.-reason r))))))

(defn ^:async trade!
  "Read the offers and buy from the chosen one: :done, :again or a retry."
  [c]
  (let [{:keys [villager buy max-price]} (:args c)
        r (await (ctx/act c :trade (clj->js {:villager villager :op "offers"})))
        status (.-status r)]
    (case status
      "ok" (let [{:keys [offer reason price]} (choose (js->clj (.-offers r) :keywordize-keys true) buy max-price)]
             (if offer
               (await (buy! c offer))
               (do (ctx/emit! c :trade.gave-up :warn {:reason reason :text (str "no purchase: " reason)})
                   (finish! c (cond-> {:status :stopped :reason reason} price (assoc :price price))))))
      "gone" (give-up! c "gone" "the villager is gone")
      "cannot" (give-up! c (if (= "not-villager" (.-reason r)) "not-villager" "no-offers") (str "cannot trade: " (.-reason r)))
      "out-of-reach" (retry "unreachable" "out of reach of the villager")
      (retry "window" "the trade window did not open"))))

(defn ^:async step!
  "Finish when enough is bought, else find the villager, walk within reach with one go-to leg (it wanders), then read
  the offers and buy. :again, :continue (go-to yields), :done or a retry."
  [c]
  (let [{:keys [villager count]} (:args c)
        p (:primitives c)
        e (when (< (:bought (ctx/mem c) 0) count) (find-villager p villager))]
    (cond
      (>= (:bought (ctx/mem c) 0) count) (done! c)
      (nil? e) (give-up! c "gone" "the villager is not here")
      (u/within? (u/self-pos c) (u/pos-of (.-pos e)) reach) (await (trade! c))
      :else
      (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to
                                     {:pos (u/pos-of (.-pos e)) :range reach :leg-s chase-timeout-s :escalate false :warn false}))]
        (cond
          (= :continue r) :continue
          (let [res (ctx/child-result c :walk)] (and (= :done r) (or (:arrived res) (:leg res)))) :again
          :else (give-up! c "unreachable" "cannot reach the villager"))))))

(defn ^:async round
  "One call is the whole purchase: steps (with a pace between) until it ends; a retry counts failures in a row, reset by
  a buy, and the third gives up. Yields only while go-to waits on the world."
  [c]
  (let [fails (atom 0)]
    (await (pace/steps! c (fn ^:async trade-step []
                            (let [r (await (step! c))]
                              (cond
                                (map? r) (let [[reason text] (:retry r)]
                                           (if (< (swap! fails inc) u/max-failures)
                                             :again
                                             (do (ctx/emit! c :trade.gave-up :warn {:tries u/max-failures :reason reason :text text})
                                                 (finish! c {:status :stopped :reason reason}))))
                                (= :again r) (do (reset! fails 0) :again)
                                :else r)))))))
