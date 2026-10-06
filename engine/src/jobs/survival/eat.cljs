(ns jobs.survival.eat
  (:require [engine.ctx :as ctx]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]))

(def doc
  "Eat the best carried food, bite after bite in one call, until food reaches :until, :max-bites are eaten or nothing edible is left.
  Best means most hunger points, then most saturation (data from jobs.lib.foods).
  Harmful foods (rotten flesh, spider eyes, pufferfish, poisonous potatoes, raw chicken) need :allow-bad.
  Golden apples are eaten only when named or at low health. Chorus fruit and suspicious stew only when named.
  Declines with :not-hungry or :no-food. A named item that is not food, or is harmful without :allow-bad,
  is refused at once with a refused warning,
  reason :not-food or :bad-food, and {:ate false :reason :item}.
  Hands over {:ate bites} and, when a bite fails, :reason :eat-failed.
  Memory: writes :fed {:item :food} for each meal.")

(def args
  {:item {:doc "the food to eat; the best carried when nil" :default nil}
   :until {:doc "keep eating while food is below this (of 20)" :default 18}
   :max-bites {:doc "eat at most this many bites; unlimited when nil" :default nil}
   :allow-bad {:doc "also eat the harmful foods when nothing else is carried" :default false}})

(def fed-policy {:cap 20 :ttl (* 6 60 60 1000)})

(defn eaten?
  "Is item eaten for these args: the :item when named (precious and named-only foods included), else by the
  unnamed rules: not harmful unless allow-bad, and precious only at low health."
  [allow-bad item health name]
  (and (foods/food? name)
       (or (and item (= item name) (or allow-bad (not (contains? foods/harmful name))))
           (and (nil? item)
                (not (contains? foods/named-only name))
                (or allow-bad (not (contains? foods/harmful name)))
                (or (not (contains? foods/precious name)) (< health foods/low-health))))))

(defn best-food
  "The name of the carried item to eat, or nil: the common foods by most hunger points then most saturation, then
  precious ones, then harmful ones (allow-bad). Ties by carrying order. Only item when given."
  [inventory allow-bad item health]
  (->> inventory
       (map :name)
       (filter #(or (nil? item) (= item %)))
       (filter #(eaten? allow-bad item health %))
       (sort-by (fn [n] [(cond (contains? foods/harmful n) 2 (contains? foods/precious n) 1 :else 0)
                         (- (foods/points n)) (- (foods/saturation n))]))
       first))

(defn carried-best [c]
  (let [{:keys [item allow-bad]} (:args c)
        self (.self (:primitives c))]
    (best-food (u/inventory (:primitives c)) allow-bad item (.-health self))))

(defn refusal
  "[reason text] when the :item is not eaten at all (not a food, or harmful without :allow-bad), else nil."
  [c]
  (let [{:keys [item allow-bad]} (:args c)]
    (cond
      (nil? item) nil
      (not (foods/food? item)) [:not-food (str item " is not food")]
      (and (not allow-bad) (contains? foods/harmful item))
      [:bad-food (str item " is harmful food: pass :allow-bad true to eat it")])))

(defn refuse! [c [reason text]]
  (let [item (:item (:args c))]
    (ctx/emit! c :refused :warn {:reason reason :item item :text text})
    (ctx/result! c {:ate false :reason reason :item item})
    :done))

(defn check
  "A named item that is not eaten (not food, harmful without :allow-bad) passes so the round can refuse it. Else hungrier than :until and food carried;
  else it waits with reason :not-hungry or :no-food."
  [c]
  (cond
    (refusal c) true
    (>= (.-food (.self (:primitives c))) (:until (:args c))) (ctx/wait c :not-hungry)
    (nil? (carried-best c)) (ctx/wait c :no-food)
    :else true))

(defn ^:async round
  "One whole attempt: eats the best carried food bite after bite until food reaches :until, :max-bites are eaten,
  nothing edible is left or a bite fails (reason :eat-failed)."
  [c]
  (if-let [r (refusal c)]
    (refuse! c r)
    (let [{:keys [until max-bites]} (:args c)]
      (loop [bites 0]
        (let [best (carried-best c)]
          (if (or (nil? best) (not (ctx/alive? c)) (>= (.-food (.self (:primitives c))) until)
                  (and max-bites (>= bites max-bites)))
            (do (ctx/result! c {:ate bites}) :done)
            (let [_ (await (ctx/act c :equip #js {:item best}))
                  r (await (ctx/act c :eat #js {:item best}))]
              (if (not= "ate" (.-status r))
                (do (ctx/result! c {:ate bites :reason :eat-failed}) :done)
                (do (ctx/remember! c :fed {:item best :food (.-food r)} fed-policy)
                    (recur (inc bites)))))))))))
