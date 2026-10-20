(ns jobs.village.trade
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [jobs.lib.cond :as cond]
            [jobs.lib.need :as need]
            [engine.ctx :as ctx]
            [jobs.lib.aim :as aim]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.villager :as villager]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]))

(def doc
  "Buy :count of :buy from the villager with uuid :villager. One call is the whole purchase: find the villager
  within jobs.lib.villager/search-radius, walk within ::reach of it, seeing it (no seen solid block, glass too, between the eye and its cell; jobs.movement.go-to legs of ::chase-timeout-s), read its offers (prices move between visits),
  choose and buy. It yields (:continue) only while go-to waits on the world.
  Of the offers that give :buy and are not sold out, those whose price (the adjusted count of the first cost
  stack) is within :max-price (nil: no limit) are kept. The cheapest is taken, the lowest index on a tie.
  :count is items wanted. An offer gives several per trade, so it buys ceil(remaining / items per trade) trades
  and may overshoot a little.
  Ends with the data {:bought n :paid {item n} :item :buy} (n counts the items actually gained): :done when :count (or the call's :least, default :all)
  was bought, else with the cause, condition-failed for :gone, :not-villager, :payment-short and :no-room, else
  nothing-possible. A partial purchase stays bought and paid. A missing villager uuid or item name is impossible (:bad-args).
  done? holds once the remembered count reaches :count; the engine then ends it :done (already, before a start). The causes:
  - :gone (not listed within jobs.lib.villager/search-radius, also a non-villager entity under the uuid), :not-villager.
  - :unreachable (walk blocked, or still out of reach after three tries in the call).
  - :no-offers (unemployed, nitwit, baby, or an empty window), :window (did not open three times: busy
    villager), :no-offer (nothing gives :buy), :sold-out.
  - :price (every open offer is dearer than :max-price; :price is the cheapest seen).
  - :payment-short, :no-room, :incomplete (a buy stopped short for no reason, three times), or the
    primitive's failure reason.
  Before a buy the body must carry the price of one trade: missing it is fetched (jobs.lib.need, :fetch true, a set of
  kinds or a map of limits; :payment-short when :fetch is off or the fetch ended), and with a full bag one
  jobs.storage.make-room child (keeping the payment) runs per call. Without :fetch, can-start? waits (later :need)
  while no emerald is carried.
  What was bought and paid is kept across a cut and restart, so a purchase is neither lost nor repeated.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid" :spec string? :default nil}
   :buy {:doc "item name to buy" :spec a/item? :default nil}
   :count {:doc "items wanted" :spec (a/int-in 1 nil) :default 1}
   :fetch {:doc "get the payment when one trade's price is not carried (jobs.lib.need): true, a set of kinds or a map of limits; false ends :payment-short" :spec need/option? :default true}
   :max-price {:doc "highest price per trade (first cost stack); nil for no limit" :spec (a/int-in 0 nil) :default nil}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one purchase may take." :spec (a/int-in 1 nil)}
   ::chase-timeout-s {:default 15 :doc "Seconds one walk toward the villager may take: it wanders, so the walk aims again at where it is now this often." :spec (a/num-in 0 nil)}
   ::reach {:default 2 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))

(defn chase-timeout-s [] (settings/get settings ::chase-timeout-s))

(def target [:villager :buy])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)


(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn bad-args
  "Never :bad-args when no villager uuid or no item name is given."
  [c]
  (let [{:keys [villager buy]} (:args c)]
    (if (and (string? villager) (string? buy))
      (contract/ok :args "a villager uuid and an item name")
      (contract/never :bad-args "needs a villager uuid and an item name" :why "needs a villager uuid and an item name"))))

(def job 'jobs.village.trade)

(defn bought [c] (:bought (ctx/mem c) 0))

(def currency "The item villagers sell for; the price is known only once the offers are read." "emerald")

(defn carried [c item] (u/carried-count (:primitives c) item))

(defn no-payment
  "Later :need while no currency is carried and the call will not fetch it."
  [c]
  (if (or (need/opts c job) (pos? (carried c currency)))
    (contract/ok :payment "payment carried or fetched")
    (contract/later :need "no payment carried and no fetch for it" :why "no payment carried and no fetch for it" :item currency :count 1)))

(def can-start? (cond/and bad-args no-payment))

(defn result
  "{:bought n :paid {item n} :item :buy} so far."
  [c]
  {:bought (bought c)
   :paid (into {} (map (fn [[k v]] [(name k) v])) (:paid (ctx/mem c)))
   :item (:buy (:args c))})

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
      (>= (bought c) need) (ctx/end c :done nil data)
      (failed-conditions reason) (ctx/end c :condition-failed reason data)
      :else (ctx/end c :nothing-possible reason data))))

(defn give-up!
  "End with the ending of reason (a keyword)."
  [c reason]
  (finish! c reason nil))

(defn retry
  "A step that failed but may be tried again: counted per call, the third in a row gives up."
  [reason]
  {:retry reason})

(defn choose
  "Pick an offer from the read offers: {:offer o} with :price, or {:reason
  r} (and :price, the cheapest open one, for \"price\")."
  [offers buy max-price]
  (let [mine (filter #(= buy (get-in % [:gives :item])) offers)
        open (map #(assoc % :price (get-in % [:cost 0 :count])) (remove :disabled mine))
        fit (filter #(or (nil? max-price) (<= (:price %) max-price)) open)]
    (cond
      (empty? mine) {:reason :no-offer}
      (empty? open) {:reason :sold-out}
      (empty? fit) {:reason :price :price (apply min (map :price open))}
      :else {:offer (first (sort-by (juxt :price :index) fit))})))

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
      :else (retry :incomplete))))

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
  (let [{:keys [villager buy count]} (:args c)
        per (get-in offer [:gives :count])
        times (js/Math.ceil (/ (- count (:bought (ctx/mem c) 0)) per))
        r (await (ctx/act c :trade (clj->js {:villager villager :op "buy" :offer (:index offer) :times times})))
        status (.-status r)]
    (case status
      "bought" (bought! c r)
      "timeout" (if (.-inventoryChange r)
                  (bought! c (timed-out c r))
                  (retry :timeout))
      "no-item" (give-up! c :payment-short)
      "full" (give-up! c :no-room)
      "gone" (give-up! c :gone)
      "cannot" (if (= "sold-out" (.-reason r))
                 (give-up! c :sold-out)
                 (retry :no-such-offer))
      (retry (keyword (.-reason r))))))

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
    (let [opts (need/opts c job)
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
    (let [r (await (ctx/call c :room 'jobs.storage.make-room {:keep-items (mapv :item (:cost offer))}))]
      (if (= :continue r)
        :continue
        (do (ctx/update-mem! c assoc :room-tried true)
            :again)))))

(defn ^:async trade!
  "Read the offers and buy from the chosen one: :again, an ending or a retry."
  [c]
  (let [{:keys [villager buy max-price]} (:args c)
        r (await (ctx/act c :trade (clj->js {:villager villager :op "offers"})))
        status (.-status r)]
    (case status
      "ok" (let [{:keys [offer reason price]} (choose (js->clj (.-offers r) :keywordize-keys true) buy max-price)]
             (if offer
               (or (await (pay! c offer))
                   (await (room! c offer))
                   (await (buy! c offer)))
               (finish! c reason (when price {:price price}))))
      "gone" (give-up! c :gone)
      "cannot" (give-up! c (if (= "not-villager" (.-reason r)) :not-villager :no-offers))
      "out-of-reach" (retry :unreachable)
      (retry :window))))

(defn cell-of
  "The cell [x y z] of the entity's feet."
  [e]
  (let [{:keys [x y z]} (u/pos-of (.-pos e))]
    (mapv #(js/Math.floor %) [x y z])))

(defn in-reach?
  "Whether the body is within reach of villager e and sees its cell over the blocks it has seen (a glass wall is solid:
  the click would not get through)."
  [c e]
  (and (u/within? (u/self-pos c) (u/pos-of (.-pos e)) (reach))
       (aim/body-sees? c (cell-of e))))

(defn ^:async step!
  "Find the villager, walk within reach with one go-to leg (it wanders), then read the offers and buy. :again,
  :continue (go-to yields), an ending or a retry."
  [c]
  (let [e (villager/find-by-uuid (:primitives c) (:villager (:args c)))]
    (cond
      (and (nil? e) (not (look/surveyed? c))) (await (look/survey! c)) ; villagers count only while in view
      (nil? e) (give-up! c :gone)
      (in-reach? c e) (await (trade! c))
      :else
      (let [r (await (child/call-walk* c :walk
                                      {:pos (zipmap [:x :y :z] (cell-of e)) :see true :range (reach) :spend {:job-time (chase-timeout-s)} :ways #{:walk :clear-seal}}))]
        (cond
          (= :continue r) :continue
          (let [res (:data r)] (or (:arrived res) (:leg res))) :again
          :else (give-up! c :unreachable))))))

(defn ^:async round
  "One call is the whole purchase: steps (with a pace between) until it ends; a retry counts failures in a row, reset by
  a buy, and the third gives up. Yields only while go-to waits on the world."
  [c]
  (let [fails (atom 0)]
    (await (pace/steps! c (fn ^:async trade-step []
                            (let [r (await (step! c))]
                              (cond
                                (:retry r) (if (< (swap! fails inc) (u/max-failures))
                                           :again
                                           (finish! c (:retry r) nil))
                                (= :again r) (do (reset! fails 0) :again)
                                :else r)))))))
