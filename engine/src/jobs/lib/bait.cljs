(ns jobs.lib.bait
  "The food animals follow, and putting it out of the hand: a body that walks out through a gate with it in hand takes
  the penned animals along."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def breeding-food
  "Mob name -> the foods that put it in love, in the order the body prefers them."
  {"cow" ["wheat"]
   "mooshroom" ["wheat"]
   "sheep" ["wheat"]
   "goat" ["wheat"]
   "pig" ["carrot" "potato" "beetroot"]
   "chicken" ["wheat_seeds" "melon_seeds" "pumpkin_seeds" "beetroot_seeds" "torchflower_seeds"]
   "rabbit" ["carrot" "golden_carrot" "dandelion"]
   "bee" ["dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip"
          "white_tulip" "pink_tulip" "oxeye_daisy" "cornflower" "lily_of_the_valley" "sunflower"
          "lilac" "rose_bush" "peony" "torchflower" "pink_petals" "wildflowers"]})

(def near-range "Animals this near that follow the held food make it worth putting away." 16)

(defn held [c] (.-held (.self (:primitives c))))

(defn holds-food?
  "True when the body holds the breeding food of :mob in hand."
  [c]
  (boolean (some #{(held c)} (get breeding-food (:mob (:args c))))))

(defn ^:async put-away!
  "Empty the hand when it holds one of foods; with no free slot to put it in, take another carried item in hand, so no
  animal follows the body out."
  [c foods]
  (when (some #{(held c)} foods)
    (let [r (await (ctx/act c :unequip #js {}))]
      (when (and (some #{(held c)} foods) (not= "ok" (.-status r)))
        (when-let [other (first (remove (set foods) (map :name (u/inventory (:primitives c)))))]
          (await (ctx/act c :equip (clj->js {:item other}))))))))

(defn ^:async put-away-near!
  "put-away! when the hand holds food that animals within near-range follow."
  [c]
  (let [p (:primitives c)
        mobs (vec (for [[mob foods] breeding-food :when (some #{(held c)} foods)] mob))]
    (when (and (seq mobs) (seq (look/seen-objs p {:radius near-range :names mobs :max 1})))
      (await (put-away! c (into [] (mapcat breeding-food) mobs))))))

(defn ^:async put-food-away!
  "Empty the hand of the breeding food of :mob (put-away!)."
  [c]
  (await (put-away! c (get breeding-food (:mob (:args c))))))

(defn food-carried
  "The first food of mob's list the body carries, else nil."
  [p mob]
  (let [have (into #{} (map :name) (u/inventory p))]
    (first (filter have (get breeding-food mob)))))

(defn ^:async food!
  "No breeding food of :mob carried: get n of it with jobs.lib.need, the call's ways allowing. Resolves to
  :continue while the fetch goes on, :ok once food is carried, else the ending to report as the :because of :no-food
  (nil when the ways leave only :carried)."
  [c n]
  (let [mob (:mob (:args c))
        limits (need/fetch-opts c)]
    (if-not limits
      nil
      (let [e (await (need/blocks! c (get breeding-food mob) n (select-keys limits [:how :explore :minutes])))]
        (cond
          (= :continue e) :continue
          (food-carried (:primitives c) mob) :ok
          :else e)))))

(defn ^:async hold!
  "Take food, a carried breeding food, into the hand. Nil when it is held; :no-hold after the shared fruitless tries in
  a row; else :retry after one more equip (the caller checks the hand next round)."
  [c food]
  (cond
    (= food (held c)) nil
    (child/exhausted? c :equip) :no-hold
    :else (do (child/fruitless! c :equip)
              (await (ctx/act c :equip (clj->js {:item food})))
              :retry)))
