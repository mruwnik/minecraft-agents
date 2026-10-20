(ns jobs.lib.combat
  "Helpers the survival jobs share: reading hostiles off sensing and ranking
  the weapons carried."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.look :as look]
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
    (look/seen-objs p {:radius radius :kind "hostile" :max 16})))

(defn sensed
  "The entities a player would place within radius (opts :radius, :max, :ids and :names filter before the cap), as JS entities: known hostiles (seen or
  heard), players (listed through walls), and every other kind only when not hidden (visible false). Items included. Nearest first."
  [p {:keys [radius max ids] :as opts}]
  (let [others (->> (look/seen-objs p opts)
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

(defn enderman? [e] (= "enderman" (.-name e)))

(defn flyer? [e] (= "phantom" (.-name e)))

(defn climber? [e] (contains? #{"spider" "cave_spider"} (.-name e)))

(defn creeper? [e]
  (or (true? (.-creeper e)) (= "creeper" (.-name e))))

(defn weapons
  "The weapons of the call: its :use :tools, else the default substrings."
  [c]
  (or (ctx/use c :tools) default-weapons))

(defn weapon? [weapons item-name]
  (boolean (some #(str/includes? item-name %) weapons)))

(defn best-weapon
  "The name of the best carried item matching one of the weapons substrings,
  or nil."
  [p weapons]
  (->> (u/inventory p)
       (map :name)
       (filter #(weapon? weapons %))
       (sort-by weapon/weapon-score >)
       first))

(defn ^:async equip-best!
  "Hold weapon in hand unless it is already held; nil does nothing."
  [c weapon]
  (when (and weapon (not= weapon (.-held (.self (:primitives c)))))
    (await (ctx/act c :equip #js {:item weapon :dest "hand"}))))

(a/defargs settings
  {::attack-gap-ms {:default :cooldown :doc "Least time between swings, in ms; :cooldown: the held weapon's, so a swing lands at full strength." :spec (a/or-of #{:cooldown} (a/num-in 0 nil))}})

(defn gap-ms
  "The least time between swings: ::attack-gap-ms, else the held weapon's cooldown."
  [c]
  (let [gap (settings/get settings ::attack-gap-ms)]
    (if (number? gap) gap (weapon/attack-gap-ms (.-held (.self (:primitives c)))))))

(defn within-gap?
  "Whether the job memory's :last-attack swing was less than gap (default gap-ms) ago."
  ([c] (within-gap? c (gap-ms c)))
  ([c gap]
   (let [last-attack (:last-attack (ctx/mem c))]
     (and last-attack (< (- (ctx/now c) last-attack) gap)))))

(defn ^:async wait-gap!
  "Spend the rest of the attack gap after a swing at last-attack (ms): look round (watch!), then wait out what is left."
  [c last-attack gap]
  (await (watch/watch! c {}))
  (let [left (- (+ last-attack gap) (ctx/now c))]
    (when (pos? left)
      (await (ctx/act c :wait #js {:ms left})))))
