(ns jobs.lib.combat
  "Helpers the survival jobs share: reading hostiles off sensing and ranking
  the weapons carried."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]))

(def default-weapons
  "Item name substrings that count as weapons; _axe does not match pickaxes."
  ["_sword" "_axe"])

(defn ranged? [e] (contains? weapon/ranged-mobs (.-name e)))

(defn known-or-raw
  "Hostiles within radius as JS entities: the perception's known mobs (seen or heard) when p has them, else the raw
  entities()."
  [p radius]
  (if-let [known (.-knownMobs p)]
    (filter #(and (= "hostile" (.-kind %)) (<= (.-distance %) radius)) (array-seq (.call known p)))
    (array-seq (.entities p #js {:radius radius :kind "hostile" :max 16}))))

(defn sensed
  "The entities a player would place within radius (opts :radius, :max, :ids and :names filter before the cap), as JS entities: known hostiles (seen or
  heard), players (listed through walls), and every other kind only when not hidden (visible false). Items included. Nearest first."
  [p {:keys [radius max ids] :as opts}]
  (let [others (->> (array-seq (.entities p (clj->js opts)))
                    (remove #(= "hostile" (.-kind %)))
                    (remove #(and (not= "player" (.-kind %)) (false? (.-visible %)))))]
    (vec (sort-by #(.-distance %) (concat others (take (or max 64) (cond->> (known-or-raw p radius) ids (filter #(some #{(.-id %)} ids)))))))))

(defn hostiles
  "The hostile mobs within radius of the body that it knows of (seen or heard), nearest first, as JS entities.
  opts: :sight :only keeps the visible ones; :sight :prefer lists visible ones first, each group nearest first;
  without :sight the known ones all count. :ranged-radius r counts ranged mobs out to r instead of radius."
  ([p radius] (hostiles p radius {}))
  ([p radius {:keys [sight ranged-radius]}]
   (let [rr (or ranged-radius radius)
         all (->> (known-or-raw p (max radius rr))
                  (filter #(<= (.-distance %) (if (ranged? %) rr radius))))]
     (case sight
       :only (filterv #(.-visible %) all)
       :prefer (into (filterv #(.-visible %) all) (remove #(.-visible %)) all)
       (vec all)))))

(defn creeper? [e]
  (or (true? (.-creeper e)) (= "creeper" (.-name e))))

(defn weapon-score
  "Higher is better: the material, and swords over axes of the same material."
  [item-name]
  (+ (weapon/weapon-rank item-name)
     (if (str/ends-with? item-name "_sword") 0.5 0)))

(defn weapon? [weapons item-name]
  (boolean (some #(str/includes? item-name %) weapons)))

(defn best-weapon
  "The name of the best carried item matching one of the weapons substrings,
  or nil."
  [p weapons]
  (->> (u/inventory p)
       (map :name)
       (filter #(weapon? weapons %))
       (sort-by weapon-score >)
       first))

(defn ^:async equip-best!
  "Hold weapon in hand unless it is already held; nil does nothing."
  [c weapon]
  (when (and weapon (not= weapon (.-held (.self (:primitives c)))))
    (await (ctx/act c :equip #js {:item weapon :dest "hand"}))))

(defn ^:async wait-gap!
  "Spend the rest of the attack gap after a swing at last-attack (ms): look round (watch!), then wait out what is left."
  [c last-attack gap]
  (await (watch/watch! c {}))
  (let [left (- (+ last-attack gap) (ctx/now c))]
    (when (pos? left)
      (await (ctx/act c :wait #js {:ms left})))))
