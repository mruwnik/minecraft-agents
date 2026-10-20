(ns jobs.survival.eat
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.foods :as foods]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]))

(def doc
  "Eat the best carried food, bite after bite in one call, until food reaches :until, :max-bites are eaten or nothing edible is left.
  Best means most hunger points, then most saturation (data from jobs.lib.foods).
  Harmful foods (rotten flesh, spider eyes, pufferfish, poisonous potatoes, raw chicken) need :allow-bad.
  An unnamed meal leaves one of each seed a noted dug crop needs for the tidy-up (jobs.lib.tidy) until food is nearly starving.
  Golden apples are eaten only when named or at low health. Chorus fruit and suspicious stew only when named.
  Done? means fed (food at :until): a fed body ends done without eating. Hungry with no edible food carried, it fetches
  through get-food (a child, :eat false :carry the missing food points; :fetch false: not), else condition-failed :no-food (a listed job waits);
  a fetch that brought none ends condition-failed :no-food with the get-food ending as its because. A named item that is not food, or is harmful without :allow-bad, is
  impossible :not-food or :bad-food (the cause's :text says why).
  Ends :done with {:ate bites} when food reached :until or :max-bites were eaten; nothing-possible :out-of-food when the
  food ran out after a bite short of :until; a failed bite ends nothing-possible :eat-failed with {:ate bites}.
  Quote: time per bite, the bites still needed, the best food as the items used.
  Memory: writes :fed {:item :food} for each meal.")

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a meal may take." :spec (a/num-in 0 nil)}
   ::bite-s {:default 1.9 :doc "Seconds a bite takes, equip included (the quote)." :spec (a/num-in 0 nil)}})

(a/defargs args
  {:item {:doc "the food to eat; the best carried when nil" :spec a/item? :default nil}
   :until {:doc "keep eating while food is below this (of 20)" :spec (a/num-in 0 nil) :default 18}
   :max-bites {:doc "eat at most this many bites; unlimited when nil" :spec (a/int-in 1 nil) :default nil}
   :allow-bad {:doc "also eat the harmful foods when nothing else is carried" :spec boolean? :default false}
   :fetch {:doc "with no edible food carried and the body hungry, get some with jobs.survival.get-food first; false: end :no-food at once (reflexes that only eat what is carried)" :spec boolean? :default true}})

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

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
  precious ones, then harmful ones (allow-bad). Ties by carrying order. Only item when given. Reserved names (jobs.lib.reserve/spared) are left when no item is named."
  [inventory allow-bad item health reserved]
  (->> inventory
       (map :name)
       (remove #(and (nil? item) (contains? reserved %)))
       (filter #(or (nil? item) (= item %)))
       (filter #(eaten? allow-bad item health %))
       (sort-by (fn [n] [(cond (contains? foods/harmful n) 2 (contains? foods/precious n) 1 :else 0)
                         (- (foods/points n)) (- (foods/saturation n))]))
       first))

(defn seeds-owed
  "{seed count} the noted dug crops still need to be sown again (jobs.lib.tidy)."
  [c]
  (->> (tidy/entries c)
       (filter #(= :dig (:action %)))
       (keep (comp first :any-of))
       frequencies))

(defn spare-seeds
  "inventory without the seeds owed, so a meal leaves them for the tidy-up; all of it once food is nearly starving."
  [inventory food owed]
  (if (foods/nearly-starving? food)
    inventory
    (keep (fn [{:keys [name count] :as i}]
            (let [left (- count (get owed name 0))]
              (when (pos? left) (assoc i :count left))))
          inventory)))

(defn carried-best [c]
  (let [{:keys [item allow-bad]} (:args c)
        self (.self (:primitives c))
        inventory (if item (u/inventory (:primitives c)) (spare-seeds (u/inventory (:primitives c)) (.-food self) (seeds-owed c)))]
    (best-food inventory allow-bad item (.-health self) (reserve/spared (ctx/view c) self))))

(defn refusal
  "The cause when the :item is not eaten at all (not a food, or harmful without :allow-bad), else nil."
  [c]
  (let [{:keys [item allow-bad]} (:args c)]
    (cond
      (nil? item) nil
      (not (foods/food? item)) {:reason :not-food :item item :text (str item " is not food")}
      (and (not allow-bad) (contains? foods/harmful item))
      {:reason :bad-food :item item :text (str item " is harmful food: pass :allow-bad true to eat it")})))

(defn food-of [c] (.-food (.self (:primitives c))))

(defn bad-item
  "Never when the :item is refused (not food, or harmful without :allow-bad); done? never holds then, so it ends impossible."
  [c]
  (if-let [r (refusal c)] (checks/cause-status :never r) (contract/ok :item "food to eat")))

(defn done? [c]
  (let [left (max 0 (- (:until (:args c)) (food-of c)))]
    (checks/goal (and (nil? (refusal c)) (zero? left)) [:fed "ate up to the wanted food"] [:hungry (str left " food points to eat")]
                 {:units :food :left left})))

(defn no-food [c]
  (cond (carried-best c) (contract/ok :food "food carried")
        (:fetch (:args c)) (contract/ok :food "no food carried: get-food fetches it")
        :else (contract/later :no-food "no edible food carried")))

(def can-start? (cond/and bad-item no-food))

(defn ^:async quote-of
  "The bites still needed (the best food's points, the carried count and :max-bites limiting) at ::bite-s each."
  [c]
  (let [best (carried-best c)
        {:keys [until max-bites]} (:args c)
        carried (transduce (comp (filter #(= best (:name %))) (map :count)) + 0 (u/inventory (:primitives c)))
        wanted (js/Math.ceil (/ (max 0 (- until (food-of c))) (max 1 (if best (foods/points best) 1))))
        bites (cond-> (min wanted carried) max-bites (min max-bites))
        t (* bites (settings/get settings ::bite-s))]
    (cond-> {:uses {:job-time [(min t (settings/get settings ::bite-s)) t t]}}
      (pos? bites) (assoc-in [:uses :items best] [1 bites bites]))))

(defn ^:async round
  "One whole attempt: eats the best carried food bite after bite until food reaches :until, :max-bites are eaten,
  nothing edible is left or a bite fails."
  [c]
  (let [{:keys [until max-bites]} (:args c)]
    (loop [bites 0]
      (let [best (carried-best c)]
        (cond
          (not (ctx/alive? c)) :continue
          (or (>= (food-of c) until) (and max-bites (>= bites max-bites))) (ctx/end c :done nil {:ate bites})
          (nil? best) (cond
                        (pos? bites) (ctx/end c :nothing-possible :out-of-food {:ate bites})
                        (:fetch (:args c)) (let [e (await (child/run!* c :fetch 'jobs.survival.get-food
                                                         {:eat false :carry (max 1 (- until (food-of c)))}))]
                                             (cond
                                               (= :continue e) :continue
                                               (carried-best c) (recur bites)
                                               :else (contract/with-because (ctx/end c :condition-failed :no-food {:ate 0}) e)))
                        :else (ctx/end c :condition-failed :no-food {:ate 0}))
          :else
          (let [_ (await (ctx/act c :equip #js {:item best}))
                r (await (ctx/act c :eat #js {:item best}))]
            (cond
              (= "ate" (.-status r))
              (do (ctx/remember! c :fed {:item best :food (.-food r)} fed-policy)
                  (recur (inc bites)))
              :else (ctx/end c :nothing-possible :eat-failed {:ate bites :bite (.-status r) :item best}))))))))
