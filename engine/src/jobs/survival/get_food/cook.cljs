(ns jobs.survival.get-food.cook
  "Cooking the raw food carried, for get-food's cook way and the night's hold."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.shelter :as sh]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.get-food.risk :as risk]))

(a/defargs settings
  {::shelter-cook-reach {:default 5 :doc "Blocks within which a furnace counts for cooking while the night handler holds the body in its shelter." :spec (a/int-in 0 nil)}
   ::cook-far-radius {:default 64 :doc "Blocks within which a furnace or smoker the body has seen (perception's memory) or the one it remembers (:furnace place) counts for cooking." :spec (a/int-in 0 nil)}
   ::starve-s-per-hp {:default 4 :doc "Seconds of starvation (food 0) that cost one hp." :spec (a/num-in 0 nil)}
   ::cook-s-per-item {:default 10 :doc "Seconds a furnace takes to cook one raw food, to tell whether health outlasts a cook at food 0." :spec (a/num-in 0 nil)}
   ::cook-margin-hp {:default 3 :doc "Hp health must exceed the starvation over the expected cook by, else the body eats raw at food 0." :spec (a/num-in 0 nil)}
   ::furnace-cost {:default 8 :doc "Hunger points a furnace to make and place is counted to cost when cooking is weighed against eating raw." :spec (a/num-in 0 nil)}
   ::place-cost {:default 2 :doc "Hunger points putting down a furnace already carried is counted to cost." :spec (a/num-in 0 nil)}
   ::walk-cost {:default 0.2 :doc "Hunger points one block of walking to a furnace is counted to cost." :spec (a/num-in 0 nil)}
   ::cook-kinds {:default #{"chicken" "beef" "porkchop" "mutton" "rabbit" "cod" "salmon" "potato"} :doc "Raw foods the cook step puts in a furnace." :spec (a/set-of a/item?)}})

(defn shelter-cook-reach [] (settings/get settings ::shelter-cook-reach))

(defn cook-kinds [] (settings/get settings ::cook-kinds))

(defn cook-costs [] {:furnace-cost (settings/get settings ::furnace-cost)
                     :place-cost (settings/get settings ::place-cost)
                     :walk-cost (settings/get settings ::walk-cost)})

(def no-bake-policy {:cap 1 :ttl (* 10 60 1000)})

(def no-bake-ms (* 10 60 1000))

(defn food-level [c] (.-food (.self (:primitives c))))

(defn night-holds?
  "Whether the night handler would take the body now (jobs.lib.shelter/night-holds?)."
  [c]
  (boolean (sh/night-holds? (:primitives c) (ctx/view c) {} (sh/bed-permit (:primitives c) (:world (:engine c))))))

(defn raw-carried
  "The cookable raw foods carried, as {:name :count}."
  [c]
  (filter #(contains? (cook-kinds) (:name %)) (u/inventory (:primitives c))))

(defn seen-cooker
  "The nearest furnace or smoker the body has seen within radius (perception's memory, still there), as a position, or nil."
  [c radius]
  (->> (look/seen-blocks (:primitives c) {:names #{"furnace" "smoker"} :radius radius :max 8 :live? true})
       first :pos))

(defn nearest-cooker
  "The nearest furnace or smoker seen within ::cook-far-radius, else the furnace the body remembers (the :furnace
  place) within it, as a position, or nil."
  [c]
  (let [far (settings/get settings ::cook-far-radius)]
    (or (seen-cooker c far)
        (let [known (mem/place (ctx/view c) :furnace)]
          (when (and known (<= (u/dist (u/self-pos c) known) far))
            known)))))

(defn starving-too-soon?
  "At food 0, health cannot outlast the starvation over the expected cook (::cook-s-per-item per raw food carried,
  at ::starve-s-per-hp) plus ::cook-margin-hp."
  [c]
  (let [cook-s (* (settings/get settings ::cook-s-per-item) (reduce + (map :count (raw-carried c))))
        lost (/ cook-s (settings/get settings ::starve-s-per-hp))]
    (<= (.-health (.self (:primitives c))) (+ lost (settings/get settings ::cook-margin-hp)))))

(defn rushed?
  "No time to cook: a real danger is sensed near, food is 0 and health would not outlast the cook (starving-too-soon?),
  or a fragile body may not walk to the furnace (jobs.survival.get-food.risk/refusal)."
  [c]
  (boolean (or (seq (threats/sensed-mobs (:primitives c) {}))
               (and (<= (food-level c) 0) (starving-too-soon? c))
               (and (seq (raw-carried c)) (risk/refusal c (or (:furnace (:cooking (ctx/mem c))) (nearest-cooker c)))))))

(defn worth-cooking?
  "Cooking the raw food carried beats eating it now (jobs.lib.cost/cook-worth?): the walk to the furnace known, or
  the one smelt makes (or, carried, only puts down) when none is."
  [c]
  (let [cooker (nearest-cooker c)]
    (cost/cook-worth? (raw-carried c) (food-level c) (when cooker (u/dist (u/self-pos c) cooker))
                      (boolean (some #(contains? #{"furnace" "smoker"} (:name %)) (u/inventory (:primitives c))))
                      (cook-costs))))

(defn can-cook?
  "Mid-cook, or raw food carried and no live :no-cook entry, with a furnace known or cooking worth a furnace of its
  own (worth-cooking?); never at night, while the night handler holds or when rushed? (a cook under way is dropped then)."
  [c]
  (boolean
   (and (not (sh/night? (:primitives c)))
        (not (night-holds? c))
        (not (rushed? c))
        (or (:cooking (ctx/mem c))
            (and (seq (raw-carried c))
                 (zero? (ctx/count-in c :no-cook no-bake-ms))
                 (or (nearest-cooker c) (worth-cooking? c)))))))

(defn cook-first?
  "Cook before eating: can-cook? and the cook worth its time now (a cook under way goes on)."
  [c]
  (boolean (and (can-cook? c) (or (:cooking (ctx/mem c)) (worth-cooking? c)))))

(defn ^:async cook!
  "Smelt the raw food carried (the smelt job, fuel as it takes it) in the furnace known (or the given one); the cooking
  wait is spent in the round. :again when it ended, :continue while it goes on, else nil with :no-cook remembered."
  ([c] (await (cook! c nil)))
  ([c cooker]
  (let [{:keys [name count]} (or (first (raw-carried c)) (:cooking (ctx/mem c)))
        furnace (or (:furnace (:cooking (ctx/mem c))) cooker (nearest-cooker c))
        args (cond-> {:item name :count count} furnace (assoc :furnace furnace))
        _ (ctx/update-mem! c assoc :cooking {:name name :count count :furnace furnace})]
    (loop [n 0]
      (let [st (await (child/run! c :cook 'jobs.items.smelt args))]
        (cond
          (not (ctx/alive? c)) :continue
          (= :continue st) :continue
          (and (= :declined st) (< n (child/default-max-calls))) (do (await (child/pace!)) (recur (inc n)))
          (= :declined st) :continue
          :else (let [res (ctx/child-result c :cook)]
                  (ctx/update-mem! c dissoc :cooking)
                  (if (pos? (:smelted res 0))
                    :again
                    (let [reason (or (:reason res) "short")]
                      (ctx/remember! c :no-cook {:reason reason} no-bake-policy)
                      (ctx/emit! c :food.no-cook :info {:text (str "cannot cook " name ": " reason)})
                      nil)))))))))
