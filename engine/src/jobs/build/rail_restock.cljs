(ns jobs.build.rail-restock
  "The restock of jobs.build.rail-line: a share of every needed item taken in one jobs.storage.withdraw call from a chest
  the body has already looked into, however far it now is (jobs.items.obtain only looks at chests within a few blocks of
  the body unless they are known to hold the item, and never walks back to one it emptied part of)."
  (:require [engine.ctx :as ctx]
            [jobs.lib.stock :as stock]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]))

(defn plan-from
  "{:chest cell :items {name target}} for shares [[key m names]] from the nearest chest of stocks ({cell {name n}}) holding
  some of them and not in failed, each target what is carried of the name plus the share, at most what the chest holds;
  nil when no chest holds any."
  [stocks failed shares carried here]
  (let [pick (fn [items] (into {} (keep (fn [[_ m names]]
                                          (when-let [name (first (filter #(pos? (get items % 0)) names))]
                                            [name (+ (get carried name 0) (min m (get items name 0)))])))
                               shares))
        d (fn [[x y z]] (+ (abs (- x (:x here))) (abs (- y (:y here))) (abs (- z (:z here)))))]
    (->> stocks
         (remove (fn [[cell _]] (failed cell)))
         (keep (fn [[cell items]] (let [taken (pick items)] (when (seq taken) {:chest cell :items taken}))))
         (sort-by (comp d :chest))
         first)))

(defn ^:async step!
  "Take the shares from a known stocked chest with a withdraw child: :continue while it waits, :again once it ended (a
  chest that gave nothing like it is not asked again), nil when no known chest holds any of them."
  [c shares]
  (let [mem (ctx/mem c)
        carried (storage/carried-totals (u/inventory (:primitives c)))
        plan (plan-from (stock/stock-of (ctx/view c)) (:restock-failed mem #{}) shares carried (u/self-pos c))]
    (when plan
      (let [slot (keyword (str "restock-" (:restocks mem 0)))
            [x y z] (:chest plan)
            e (waits/waiting-out c (await (ctx/call c slot 'jobs.storage.withdraw {:chest {:x x :y y :z z} :items (:items plan)})))]
        (if (or (= :continue e) (= :cut-off (:kind e)))
          :continue
          (do (ctx/update-mem! c #(cond-> (update % :restocks (fnil inc 0))
                                    (not= :done (:kind e)) (update :restock-failed (fnil conj #{}) (:chest plan))))
              :again))))))
