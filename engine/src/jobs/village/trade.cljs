(ns jobs.village.trade
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.near :as near]))

(def doc
  "Buy :count of :buy from the villager with uuid :villager. Each round is one
  bounded step: find the villager within 48 blocks, walk within 2 of it, read
  its offers (one short window visit, prices move between visits), choose and
  buy. Of the offers that give :buy and are not sold out, those whose price
  (the adjusted count of the first cost stack) is within :max-price (nil: no
  limit) are kept and the cheapest is taken, the lowest index on a tie. :count
  is items wanted; an offer gives several per trade, so it buys ceil(remaining
  / items per trade) trades and may overshoot a little; the result counts the
  items actually gained. Ends with a result {:bought n :paid {item n} :item
  :buy}, plus :reason when it gave up (warn trade.gave-up): \"gone\" (not
  listed within 48, also a non-villager entity under the uuid), \"unreachable\"
  (walk blocked, or still out of reach after three tries), \"not-villager\",
  \"no-offers\" (unemployed, nitwit, baby, or an empty window), \"window\" (the
  window did not open three times, busy villager), \"no-offer\" (nothing gives
  :buy), \"sold-out\", \"price\" (every open offer is dearer than :max-price;
  :price is the cheapest seen), \"payment-short\", \"no-room\", \"incomplete\"
  (a buy stopped short for no reason, three times), or the primitive's failure
  reason. Success is info trade.done. A partial purchase before a give-up stays
  bought and paid. Memory: :bought, :paid (kept across a cut and restart, so a
  purchase is neither lost nor repeated), :failures.")

(def args
  {:villager {:doc "the villager's entity uuid" :default nil}
   :buy {:doc "item name to buy" :default nil}
   :count {:doc "items wanted" :default 1}
   :max-price {:doc "highest price per trade (first cost stack); nil for no limit" :default nil}})

(def search-radius 48)
(def reach 2)

(defn check
  "A villager uuid and an item name are given."
  [c]
  (let [{:keys [villager buy]} (:args c)]
    (and (string? villager) (string? buy))))

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
  "Warn and finish with a reason."
  [c reason text]
  (ctx/emit! c :trade.gave-up :warn {:reason reason :text text})
  (finish! c {:reason reason}))

(defn fail-up!
  "u/fail!, and when it gives up finish with the reason."
  [c reason text]
  (let [r (u/fail! c :trade.gave-up text)]
    (when (= :done r) (finish! c {:reason reason}))
    r))

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
  when enough was bought, give up by what stopped it, or go round again."
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
      :else (fail-up! c "incomplete" "a buy stopped short"))))

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
                 (fail-up! c "no-such-offer" "the offers changed"))
      (fail-up! c (str (.-reason r)) (str "trade " status " " (.-reason r))))))

(defn ^:async trade!
  "Read the offers and buy from the chosen one, or give up."
  [c]
  (let [{:keys [villager buy max-price]} (:args c)
        r (await (ctx/act c :trade (clj->js {:villager villager :op "offers"})))
        status (.-status r)]
    (case status
      "ok" (let [{:keys [offer reason price]} (choose (js->clj (.-offers r) :keywordize-keys true) buy max-price)]
             (if offer
               (await (buy! c offer))
               (do (ctx/emit! c :trade.gave-up :warn {:reason reason :text (str "no purchase: " reason)})
                   (finish! c (cond-> {:reason reason} price (assoc :price price))))))
      "gone" (give-up! c "gone" "the villager is gone")
      "cannot" (give-up! c (if (= "not-villager" (.-reason r)) "not-villager" "no-offers") (str "cannot trade: " (.-reason r)))
      "out-of-reach" (fail-up! c "unreachable" "out of reach of the villager")
      (fail-up! c "window" "the trade window did not open"))))

(defn ^:async round
  "One bounded step: finish when enough is bought, else find and reach the
  villager, then read the offers and buy."
  [c]
  (let [{:keys [villager count]} (:args c)
        p (:primitives c)
        e (when (< (:bought (ctx/mem c) 0) count) (find-villager p villager))]
    (cond
      (>= (:bought (ctx/mem c) 0) count) (done! c)
      (nil? e) (give-up! c "gone" "the villager is not here")
      :else
      (case (await (near/walk-near! c (u/pos-of (.-pos e)) reach))
        :partial :continue
        :blocked (give-up! c "unreachable" "cannot reach the villager")
        (await (trade! c))))))
