(ns engine.fake.trade
  "The fake world's `trade` primitive: a villager's offers, and buying from them, as pure functions over world data
  {:self {:pos} :inventory :entities [{:uuid :name :pos :profession :level :baby :busy :offers [{:cost [{:item :count}]
  :gives {:item :count} :uses :maxUses}]}]}. Mirrors js/trade.mjs and js/fake-trade.mjs. (trade w args) answers
  [world' result]; a buy raises the offer's :uses. Test-only."
  (:require [engine.fake.pockets :as pockets]))

(def reach 3.5)

(defn check [{:keys [villager op offer times] :as a}]
  (cond
    (not (and (map? a) (string? villager) (seq villager))) (throw (pockets/bad-args "trade needs villager, a uuid string"))
    (not (#{"offers" "buy"} op)) (throw (pockets/bad-args "trade op must be offers or buy"))
    (and (= op "buy") (not (and (integer? offer) (>= offer 0)))) (throw (pockets/bad-args "trade buy needs offer, an integer >= 0"))
    (and (some? times) (not (and (integer? times) (>= times 1)))) (throw (pockets/bad-args "trade times must be an integer >= 1"))))

(defn row [index o]
  (let [uses (get o :uses 0)
        max-uses (get o :maxUses 12)
        left (max 0 (- max-uses uses))]
    {:index index :cost (mapv #(select-keys % [:item :count]) (:cost o)) :gives (:gives o)
     :uses uses :maxUses max-uses :left left :disabled (zero? left)}))

(defn fits
  "How many of the result fit: free slots plus the unfilled part of the stacks already carried."
  [w name count]
  (let [partial (transduce (comp (filter #(= name (:name %))) (map #(max 0 (- pockets/stack-max (:count %))))) + 0 (:inventory w))]
    (quot (+ (* (- pockets/slots (clojure.core/count (:inventory w))) pockets/stack-max) partial) count)))

(defn buy [w e-index rows {:keys [offer times]}]
  (let [r (nth rows offer nil)
        short (when r
                (into {} (keep (fn [{:keys [item count]}]
                                 (let [n (max 0 (- count (pockets/carried w item)))]
                                   (when (pos? n) [(keyword item) n]))))
                      (:cost r)))]
    (cond
      (nil? r) [w {:status "cannot" :reason "no-such-offer" :offers (count rows)}]
      (:disabled r) [w {:status "cannot" :reason "sold-out"}]
      (seq short) [w {:status "no-item" :short short}]
      :else
      (let [{:keys [item count]} (:gives r)
            room (fits w item count)]
        (if (zero? room)
          [w {:status "full"}]
          (let [times (or times 1)
                payable (apply min (map #(quot (pockets/carried w (:item %)) (:count %)) (:cost r)))
                limits [["sold-out" (:left r)] ["payment" payable] ["room" room]]
                n (apply min times (map second limits))
                paid (into {} (map (fn [c] [(keyword (:item c)) (* (:count c) n)])) (:cost r))
                w' (-> (reduce (fn [w c] (pockets/take-from w (:item c) (* (:count c) n))) w (:cost r))
                       (pockets/give item (* count n))
                       (assoc-in [:entities e-index :offers offer :uses] (+ (:uses r) n)))]
            [w' {:status "bought" :times n :requested times :gained {(keyword item) (* count n)} :paid paid
                 :stopped (when (< n times) (first (first (filter #(= n (second %)) limits))))}]))))))

(defn trade [w {:keys [villager op] :as a}]
  (check a)
  (let [i (first (keep-indexed #(when (= villager (:uuid %2)) %1) (:entities w)))
        e (when i (nth (:entities w) i))
        distance (when e (pockets/distance (get-in w [:self :pos]) (:pos e)))
        profession (get e :profession "unemployed")
        level (get e :level 1)
        why (cond (:baby e) "baby" (#{"unemployed" "nitwit"} profession) profession)
        rows (vec (map-indexed row (:offers e)))]
    (cond
      (nil? e) [w {:status "gone"}]
      (not= "villager" (:name e)) [w {:status "cannot" :reason "not-villager" :name (:name e)}]
      (> distance reach) [w {:status "out-of-reach" :reason "too-far" :distance distance}]
      why [w {:status "cannot" :reason "no-offers" :why why :profession profession :level level}]
      (:busy e) [w {:status "failed" :reason "window-did-not-open"}]
      (empty? rows) [w {:status "cannot" :reason "no-offers" :why "none" :profession profession :level level}]
      (= op "offers") [w {:status "ok" :uuid villager :profession profession :level level :offers rows}]
      :else (buy w i rows a))))
