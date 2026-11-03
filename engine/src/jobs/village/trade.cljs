(ns jobs.village.trade
  (:require [clojure.string :as str]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [jobs.lib.cond :as cond]
            [jobs.lib.need :as need]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.trade-window :as tw]
            [jobs.lib.villager :as villager]
            [jobs.lib.util :as u]))

(def meta {:tags [:goal]})

(def doc
  "Buy :count of :buy from the villager :villager (uuid or entity id; nil: the nearest adult in sight that has offers, a villager that shows none is skipped for the next, as a player opens one after another). One call is the whole purchase: find the villager
  within jobs.lib.villager/search-radius (a named one out of view: first to where it was last seen), walk within reach of it (jobs.lib.trade-window ::reach), seeing it (no seen solid block, glass too, between the eye and its cell; jobs.movement.go-to legs of jobs.lib.trade-window ::chase-timeout-s), read its offers (prices move between visits),
  choose and buy. It yields (:continue) only while go-to waits on the world.
  Of the offers that give :buy and are not sold out, those paid with an item of the call's :use {:payment [..]}
  (default emerald plus the items named in :spend {:items {item n}}, e.g. {\"paper\" 24} to sell 24 paper for an emerald; any other item, diamond too, is never spent) and whose price (the adjusted count of the first cost stack) is within what the call's :spend
  {:items {payment n}} has left (none set: no limit) are kept. The cheapest is taken, the lowest index on a tie.
  :count is items wanted. An offer gives several per trade, so it buys ceil(remaining / items per trade) trades
  and may overshoot a little.
  Ends with the data {:bought n :paid {item n} :item :buy} (n counts the items actually gained): :done when :count (or the call's :least, default :all)
  was bought, else with the cause, condition-failed for :gone, :not-villager, :payment-short and :no-room, else
  nothing-possible. A partial purchase stays bought and paid. A missing item name is impossible (:bad-args).
  done? holds once the remembered count reaches :count; the engine then ends it :done (already, before a start). The causes:
  - :gone (not listed within jobs.lib.villager/search-radius and not at the place it was last seen, also a non-villager entity under the uuid), :not-villager.
  - :unreachable (walk blocked, or still out of reach after three tries in the call).
  - :no-offers (unemployed, nitwit, baby, or an empty window), :window (did not open three times: busy
    villager), :no-offer (nothing gives :buy), :sold-out.
  - :price (every open offer is dearer than the payment spend left; :price is the cheapest seen).
  - :payment-short, :no-room, :incomplete (a buy stopped short for no reason, three times), or the
    primitive's failure reason.
  While it runs the payment (the :spend items, and emeralds unless bought) is held in the reserve register. Before a buy the body must carry the price of one trade: missing it is fetched (jobs.lib.need; :payment-short when the ways leave only :carried or the fetch ended), and with a full bag one
  jobs.storage.make-room child (keeping the payment) runs per call. With ways #{:carried}, can-start? waits (later :need)
  while no emerald is carried.
  What was bought and paid is kept across a cut and restart, so a purchase is neither lost nor repeated.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid or numeric entity id; nil: the nearest adult villager in sight, the next one when it has no offers" :spec villager/id? :default nil}
   :buy {:doc "item name to buy" :spec a/item? :default nil}
   :count {:doc "items wanted" :spec (a/int-in 1 nil) :default 1}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one purchase may take." :spec (a/int-in 1 nil)}
})

(def ways need/fetch-ways)

(def target [:buy])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)


(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn bad-args
  "Never :bad-args when no item name is given."
  [c]
  (if (string? (:buy (:args c)))
    (contract/ok :args "an item name")
    (contract/never :bad-args "needs an item name" :why "needs an item name")))

(def job 'jobs.village.trade)

(defn bought [c] (:bought (ctx/mem c) 0))

(defn worth-of
  "Seconds a root purchase is worth: the cost/goal-value of the :buy items still to buy."
  [c]
  (let [{:keys [buy count]} (:args c)]
    (if (string? buy) (:value (cost/goal-value [{:name buy :count (max 0 (- count (bought c)))}])) 0)))

(def currency "The item villagers sell for; the price is known only once the offers are read." "emerald")

(defn carried [c item] (u/carried-count (:primitives c) item))

(defn no-payment
  "Later :need while no currency is carried and the call will not fetch it."
  [c]
  (if (or (need/fetch-opts c) (pos? (carried c currency)))
    (contract/ok :payment "payment carried or fetched")
    (contract/later :need "no payment carried and no fetch for it" :why "no payment carried and no fetch for it" :item currency :count 1)))

(def can-start? (cond/and bad-args no-payment))

(defn result
  "{:bought n :paid {item n} :item :buy} so far; {} for a call with no :buy (village.offers)."
  [c]
  (if (:buy (:args c))
    {:bought (bought c)
     :paid (into {} (map (fn [[k v]] [(name k) v])) (:paid (ctx/mem c)))
     :item (:buy (:args c))}
    {}))

(defn done?
  "Ok once :count items are bought (kept in memory and across a restart), with the purchase as its result."
  [c]
  (let [{:keys [buy count]} (:args c)
        data [:units :items :done (bought c) :left (max 0 (- count (bought c))) :result (result c)]]
    (if (>= (bought c) count)
      (apply contract/ok :bought (str "bought " (bought c) " " buy) data)
      (apply contract/later :buying (str "bought " (bought c) " of " count " " buy) data))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 5 (:count (:args c) 1)) ##Inf]}})

(def failed-conditions "Reasons the body could mend itself: the rest are nothing-possible." #{:gone :not-villager :payment-short :no-room})

(defn finish!
  "End with the counts so far plus extra: :done when the call's :least was bought, else
  condition-failed for a shortage of payment or room, else nothing-possible with reason as the cause."
  [c reason extra]
  (let [least (or (:least (ctx/frozen-call c)) :all)
        need (if (= :all least) (:count (:args c)) least)
        data (merge (result c) extra)]
    (cond
      (and need (>= (bought c) need)) (ctx/end c :done nil data)
      (failed-conditions (:reason reason reason)) (ctx/end c :condition-failed reason data)
      :else (ctx/end c :nothing-possible reason data))))

(defn give-up!
  "End with the ending of reason (a keyword)."
  [c reason]
  (finish! c reason nil))

(def stopped-reason {"sold-out" :sold-out "payment" :payment-short "room" :no-room})

(defn bought!
  "Book what a buy gained and paid (measured by the primitive), then give up by what stopped it, or :again (a retry when it stopped short for no reason)."
  [c r]
  (let [buy (:buy (:args c))
        gained (or (aget (.-gained r) buy) 0)
        stopped (.-stopped r)]
    (ctx/update-mem! c (fn [m] (-> m
                                   (update :bought (fnil + 0) gained)
                                   (update :paid #(merge-with + % (js->clj (.-paid r)))))))
    (cond
      (contains? stopped-reason stopped) (give-up! c (stopped-reason stopped))
      (pos? gained) :again
      :else (tw/retry :incomplete))))

(defn timed-out
  "A buy that hit the act time bound, as a bought result: the goods gained and the rest paid, from its inventory change."
  [c r]
  (let [change (js->clj (.-inventoryChange r))
        buy (:buy (:args c))]
    #js {:gained (doto #js {} (aset buy (max 0 (get change buy 0))))
         :paid (clj->js (into {} (keep (fn [[k v]] (when (and (not= k buy) (neg? v)) [k (- v)]))) change))
         :stopped nil}))

(defn ^:async buy!
  "Buy the trades still wanted of the chosen offer and handle the outcome."
  [c offer]
  (let [{:keys [buy count]} (:args c)
        villager (villager/chosen c)
        per (get-in offer [:gives :count])
        times (js/Math.ceil (/ (- count (:bought (ctx/mem c) 0)) per))
        r (await (ctx/act c :trade (clj->js {:villager villager :op "buy" :offer (:index offer) :times times})))
        status (.-status r)]
    (case status
      "bought" (bought! c r)
      "timeout" (if (.-inventoryChange r)
                  (bought! c (timed-out c r))
                  (tw/retry :timeout))
      "no-item" (give-up! c :payment-short)
      "full" (give-up! c :no-room)
      "gone" (give-up! c (villager/gone-cause c))
      "cannot" (if (= "sold-out" (.-reason r))
                 (give-up! c :sold-out)
                 (tw/retry :no-such-offer))
      (tw/retry (keyword (.-reason r))))))

(defn short-payment
  "The first cost stack of offer the body carries too little of for one trade: {:item i :count n-missing}, else nil."
  [c offer]
  (some (fn [{:keys [item count]}]
          (let [missing (- count (carried c item))]
            (when (pos? missing) {:item item :count missing})))
        (:cost offer)))

(defn ^:async pay!
  "Fetch the payment of one trade when it is not carried: nil when it is, :continue while the fetch waits, :again
  when it ran, else the :payment-short ending."
  [c offer]
  (when-let [{:keys [item count]} (short-payment c offer)]
    (let [opts (need/fetch-opts c)
          e (when opts (await (need/item! c item count opts)))]
      (cond
        (= :continue e) :continue
        (and e (= :done (:kind e))) :again
        :else (give-up! c :payment-short)))))

(defn no-room?
  "Whether the bought item would find no slot: the bag is full and no carried stack of it has space."
  [c buy]
  (and (zero? (u/free-slots (:primitives c)))
       (not-any? #(and (= buy (:name %)) (< (:count %) 64)) (u/inventory (:primitives c)))))

(defn ^:async room!
  "A full bag: once per call a jobs.storage.make-room child keeping the payment. :continue while it waits, :again when
  it ran, nil when it was tried before or there is room."
  [c offer]
  (when (and (not (:room-tried (ctx/mem c))) (no-room? c (:buy (:args c))))
    (let [r (await (ctx/call c :room 'jobs.storage.make-room {:carry-items (mapv :item (:cost offer)) :stuck? true}))]
      (if (= :continue r)
        :continue
        (do (ctx/update-mem! c assoc :room-tried true)
            :again)))))

(defn ^:async trade!
  "Read the offers and buy from the chosen one: :again, an ending or a retry."
  [c]
  (let [{:keys [buy]} (:args c)
        payment (set (or (ctx/use c :payment) (cons currency (keys (get-in (ctx/left c) [:spend :items])))))
        spend-of #(get-in (ctx/left c) [:spend :items %] ##Inf)]
    (await (tw/read! c finish! :trade.offers
                  (fn ^:async buy-from [c _ _ offers]
                    (let [{:keys [offer reason price]} (tw/choose offers {:buy buy :payment payment :max-price spend-of})]
                      (if offer
                        (or (await (pay! c offer))
                            (await (room! c offer))
                            (await (buy! c offer)))
                        (finish! c reason (cond price {:price price}
                                                (= :no-offer reason) {:text (str "no offer for " buy " paid with " (str/join ", " (sort payment)) "; offered: "
                                                                                 (str/join "; " (map tw/offer-text offers)))})))))))))

(defn hold-payment!
  "Reserve the carried units the trade may pay with (the currency unless it is bought, and the call's :spend items) so a reflex's make-room leaves them."
  [c]
  (let [spend (get-in (ctx/left c) [:spend :items])
        items (cond-> (into {} (keep (fn [[i n]] (when (pos? n) [i (if (js/isFinite n) n :all)]))) spend)
                (not= currency (:buy (:args c))) (assoc currency :all))]
    (reserve/set-job-entries! c (vec (for [[i n] items] {:id (str (:id c) "-pay-" i) :item i :count n :strength 1 :by (:id c) :why "kept to pay the trade"})))))

(defn ^:async round
  "One call is the whole purchase."
  [c]
  (await (tw/run! c finish! trade! hold-payment!)))
