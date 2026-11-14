(ns jobs.lib.deposit-pick
  "What a deposit would put away: shared by jobs.storage.deposit, haul and farm.maintain."
  (:require [engine.ctx :as ctx]
            [jobs.lib.crops :as crops]
            [jobs.lib.found :as found]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(defn to-deposit
  "The next carried stack to put away and how many of it: {:stack s :count n}
  or nil. With wanted names, the first carried stack of the first name (in
  wanted's order) that has spare; with none (nil), the first stack in
  inventory order that is not a tool or armour and has spare. A name has spare
  when its carried total exceeds its keep (a map of name to count, default 0);
  n is the stack count, cut so the keep stays carried."
  ([items wanted] (to-deposit items wanted {}))
  ([items wanted keep]
   (let [spare (fn [stack] (- (storage/carried items (:name stack)) (get keep (:name stack) 0)))
         pick (fn [stack] (when (pos? (spare stack))
                            {:stack stack :count (min (:count stack) (spare stack))}))]
     (if wanted
       (some (fn [n] (some #(when (= n (:name %)) (pick %)) items)) wanted)
       (some #(when-not (storage/tool? (:name %)) (pick %)) items)))))

(defn wanted-names
  "The :items names: the farm goods for :farm-goods, nil for none given. The reserve register applies to a chooser, not to named items."
  [items]
  (if (= :farm-goods items) crops/farm-goods items))

(defn spare-items
  "{name n} the call would put away now: the carried names the pick rules allow, each by its spare over the keep."
  [c]
  (let [{items :items keep :carry} (:args c)
        carried (u/inventory (:primitives c))
        goods (found/goods (ctx/view c))
        inventory (remove #(contains? goods (:name %)) carried)
        keeps (if (and items (not= :farm-goods items)) keep (merge-with max (reserve/held c :deposit) keep))]
    (loop [left inventory, acc {}]
      (if-let [pick (to-deposit left (wanted-names items) keeps)]
        (let [n (:name (:stack pick))]
          (recur (remove #(= n (:name %)) left) (assoc acc n (- (storage/carried left n) (get keeps n 0)))))
        acc))))
