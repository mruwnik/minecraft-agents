(ns jobs.survival.eat
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.foods :as foods]
            [jobs.lib.util :as u]))

(def doc
  "Eat the best carried food, bite after bite in one call, until food reaches :until, :max-bites are eaten or nothing edible is left.
  Best means most hunger points, then most saturation (data from jobs.lib.foods).
  Harmful foods (rotten flesh, spider eyes, pufferfish, poisonous potatoes, raw chicken) need :allow-bad.
  Golden apples are eaten only when named or at low health. Chorus fruit and suspicious stew only when named.
  Needs: hunger below :until and an edible food carried; else condition-failed :not-hungry or :no-food (a listed job
  waits). A named item that is not food, or is harmful without :allow-bad, is refused at once with a refused warning:
  condition-failed :not-food or :bad-food, data {:ate false :item}.
  Ends :done with {:ate bites} when food reached :until, :max-bites were eaten or the food ran out after a bite; a
  failed bite ends nothing-possible :eat-failed with {:ate bites}. Done? means fed (food at :until).
  Quote: time per bite, the bites still needed, the best food as the items used.
  Memory: writes :fed {:item :food} for each meal.")

(a/defargs settings
  {::bite-s {:default 1.9 :doc "Seconds a bite takes, equip included (the quote)." :spec (a/num-in 0 nil)}})

(a/defargs args
  {:item {:doc "the food to eat; the best carried when nil" :spec a/item? :default nil}
   :until {:doc "keep eating while food is below this (of 20)" :spec (a/num-in 0 nil) :default 18}
   :max-bites {:doc "eat at most this many bites; unlimited when nil" :spec (a/int-in 1 nil) :default nil}
   :allow-bad {:doc "also eat the harmful foods when nothing else is carried" :spec boolean? :default false}})

(def target [:item])

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
                (or (not (contains? foods/precious name)) (< health (foods/low-health)))))))

(defn best-food
  "The name of the carried item to eat, or nil: the common foods by most hunger points then most saturation, then
  precious ones, then harmful ones (allow-bad). Ties by carrying order. Only item when given. Reserved bait (foods/reserved) is left when no item is named."
  [inventory allow-bad item health food reserved]
  (->> inventory
       (map :name)
       (remove #(and (nil? item) (foods/spared? food % reserved)))
       (filter #(or (nil? item) (= item %)))
       (filter #(eaten? allow-bad item health %))
       (sort-by (fn [n] [(cond (contains? foods/harmful n) 2 (contains? foods/precious n) 1 :else 0)
                         (- (foods/points n)) (- (foods/saturation n))]))
       first))

(defn carried-best [c]
  (let [{:keys [item allow-bad]} (:args c)
        self (.self (:primitives c))]
    (best-food (u/inventory (:primitives c)) allow-bad item (.-health self) (.-food self) (foods/reserved (ctx/view c)))))

(defn refusal
  "[reason text] when the :item is not eaten at all (not a food, or harmful without :allow-bad), else nil."
  [c]
  (let [{:keys [item allow-bad]} (:args c)]
    (cond
      (nil? item) nil
      (not (foods/food? item)) [:not-food (str item " is not food")]
      (and (not allow-bad) (contains? foods/harmful item))
      [:bad-food (str item " is harmful food: pass :allow-bad true to eat it")])))

(defn food-of [c] (.-food (.self (:primitives c))))

(defn not-hungry
  "A named item the round refuses passes, so the round can refuse it."
  [c]
  (when-not (or (refusal c) (< (food-of c) (:until (:args c)))) :not-hungry))

(defn no-food [c]
  (when-not (or (refusal c) (carried-best c)) :no-food))

(def conditions [[:fn not-hungry] [:fn no-food]])

(defn done? [c]
  (let [left (max 0 (- (:until (:args c)) (food-of c)))]
    {:done? (and (nil? (refusal c)) (zero? left)) :units :food :left left}))

(defn ^:async quote-of
  "The bites still needed (the best food's points, the carried count and :max-bites limiting) at ::bite-s each."
  [c]
  (let [best (carried-best c)
        {:keys [until max-bites]} (:args c)
        carried (transduce (comp (filter #(= best (:name %))) (map :count)) + 0 (u/inventory (:primitives c)))
        wanted (js/Math.ceil (/ (max 0 (- until (food-of c))) (max 1 (if best (foods/points best) 1))))
        bites (cond-> (min wanted carried) max-bites (min max-bites))
        t (* bites (settings/get settings ::bite-s))]
    (cond-> {:uses {:time [(min t (settings/get settings ::bite-s)) t t]}}
      (pos? bites) (assoc-in [:uses :items best] [1 bites bites]))))

(defn refuse! [c [reason text]]
  (let [item (:item (:args c))]
    (ctx/emit! c :refused :warn {:reason reason :item item :text text})
    (ctx/end c :condition-failed reason {:ate false :item item})))

(defn ^:async round
  "One whole attempt: eats the best carried food bite after bite until food reaches :until, :max-bites are eaten,
  nothing edible is left or a bite fails."
  [c]
  (if-let [r (refusal c)]
    (refuse! c r)
    (let [{:keys [until max-bites]} (:args c)]
      (loop [bites 0]
        (let [best (carried-best c)]
          (cond
            (not (ctx/alive? c)) (ctx/end c :cut-off :cut {:ate bites})
            (or (>= (food-of c) until) (and max-bites (>= bites max-bites))) (ctx/end c :done nil {:ate bites})
            (nil? best) (if (pos? bites) (ctx/end c :done nil {:ate bites}) (ctx/end c :condition-failed :no-food {:ate 0}))
            :else
            (let [_ (await (ctx/act c :equip #js {:item best}))
                  r (await (ctx/act c :eat #js {:item best}))]
              (if (not= "ate" (.-status r))
                (ctx/end c :nothing-possible :eat-failed {:ate bites})
                (do (ctx/remember! c :fed {:item best :food (.-food r)} fed-policy)
                    (recur (inc bites)))))))))))
