(ns jobs.farm.compost
  (:require [engine.ctx :as ctx]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.toll-cells :as tc]))

(def doc
  "Feed a composter what a farm cannot use and collect the bone meal it makes.
  Walks to the composter, feeds it, empties it when full and picks up the bone meal.
  Ends when :times bone meal is taken, nothing is left to feed, there is no composter,
  or three failures in a row.
  Result: {:fed {name n} :bone-meal n}, plus :reason (:no-composter, :nothing-to-feed, :gave-up) when it stopped early.
  Zones: a composter in another owner's zone or claim, or inside a plan's footprint, is not used (it counts as a
  :harvest). The job warns compost.declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.")

(def args
  {:at {:doc "the composter position; the nearest composter within :radius when nil" :type :pos :default nil}
   :radius {:doc "how far to look for a composter, when :at is nil" :default 16}
   :items {:doc "item names to feed; every compostable thing carried except seeds and food when nil" :default nil}
   :keep {:doc "{item count} reserves never fed" :default {}}
   :times {:doc "bone meal to take before done" :default 1}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def compostable
  "Everything a composter accepts, any chance."
  #{"wheat_seeds" "beetroot_seeds" "melon_seeds" "pumpkin_seeds" "torchflower_seeds" "pitcher_pod" "short_grass"
    "tall_grass" "fern" "large_fern" "seagrass" "kelp" "dried_kelp" "oak_leaves" "birch_leaves" "spruce_leaves"
    "jungle_leaves" "acacia_leaves" "dark_oak_leaves" "cherry_leaves" "mangrove_leaves" "azalea_leaves" "oak_sapling"
    "birch_sapling" "spruce_sapling" "jungle_sapling" "acacia_sapling" "dark_oak_sapling" "cherry_sapling"
    "mangrove_propagule" "sweet_berries" "glow_berries" "hanging_roots" "moss_carpet" "small_dripleaf" "dead_bush"
    "sugar_cane" "cactus" "vine" "melon_slice" "pumpkin" "carved_pumpkin" "nether_wart" "glow_lichen" "tall_seagrass"
    "big_dripleaf" "lily_pad" "apple" "beetroot" "carrot" "potato" "wheat" "cocoa_beans" "melon" "brown_mushroom"
    "red_mushroom" "sea_pickle" "moss_block" "pink_petals" "bamboo" "nether_sprouts" "bread" "cookie" "baked_potato"
    "hay_block" "brown_mushroom_block" "red_mushroom_block" "nether_wart_block" "warped_wart_block" "waterlily"
    "cake" "pumpkin_pie"})

(def held-back
  "Kept when no :items is given: seeds to sow again and the body's own food."
  #{"wheat_seeds" "beetroot_seeds" "melon_seeds" "pumpkin_seeds" "carrot" "potato" "sugar_cane" "bamboo"
    "bread" "cookie" "baked_potato" "apple" "sweet_berries" "glow_berries" "melon_slice" "cake" "pumpkin_pie"
    "beetroot"})

(defn feedable
  "What to feed: [[name n] ...] from the inventory ([{:name :count}]) and args
  {:items :keep}. With :items, those names only; else every carried compostable
  not held back. Always minus :keep counts; nothing at zero."
  [inventory {items :items reserve :keep}]
  (let [held (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inventory)
        names (if items
                (distinct items)
                (->> inventory (map :name) distinct (remove held-back)))]
    (->> names
         (filter compostable)
         (keep (fn [n]
                 (let [spare (- (get held n 0) (get reserve n 0))]
                   (when (pos? spare) [n spare])))))))

(defn check [_c] true)

(defn level-of [b]
  (or (some-> b .-properties .-level) 0))

(defn composter-allowed?
  "Whether the job may use the composter at pos (zones, claims, footprints; one warn when refused)."
  [c pos]
  (gate/allowed? c :compost.declined "compost" :harvest pos))

(defn nearest-composter
  "The nearest composter within :radius that the zone rules let the job use."
  [c]
  (let [me (u/self-pos c)]
    (->> (look/seen-blocks (:primitives c) {:names ["composter"] :radius (:radius (:args c)) :live? true})
         (map :pos)
         (gate/allowed c :compost.declined "compost" :harvest)
         (sort-by #(u/dist me %))
         first)))

(defn finish!
  "End the job with result data; extra fields merged in."
  [c extra]
  (let [m (ctx/mem c)]
    (ctx/result! c (merge {:fed (:fed m {}) :bone-meal (:taken m 0)} extra))
    :done))

(defn strike!
  "Count a failure; :again until the third in a row, then warn and :done."
  [c level]
  (let [strikes (inc (:strikes (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :strikes strikes)
    (if (< strikes 3)
      :again
      (do (ctx/emit! c :compost.gave-up :warn {:text "the composter keeps refusing, giving up"})
          (finish! c {:level level :reason :gave-up})))))

(defn meal-near
  "The nearest bone_meal item entity within 3 blocks of the composter, or nil."
  [c at]
  (->> (look/seen-items (:primitives c) {:radius 20})
       (filter #(= "bone_meal" (some-> (.-item %) .-name)))
       (filter #(<= (u/dist at (u/pos-of (.-pos %))) 3))
       first))

(defn ^:async step
  "One step: collect bone meal lying by the composter, finish when enough
  is taken, else walk up, empty a full composter, wait out level 7, or feed it
  the first feedable item. Three failures in a row give up."
  [c]
  (let [p (:primitives c)
        {:keys [times items at] reserve :keep} (:args c)
        mem (ctx/mem c)
        pos (or (:composter mem) (when at (when (composter-allowed? c at) at)) (when-not at (nearest-composter c)))
        block (when pos (u/seen-block p pos))]
    (if-not (and pos block (= "composter" (.-name block)))
      (do (ctx/emit! c :compost.no-composter :warn {:text "no composter to feed"})
          (finish! c {:reason :no-composter}))
      (do
        (when-not (:composter mem) (ctx/update-mem! c assoc :composter pos))
        (let [meal (meal-near c pos)
              taken (:taken mem 0)
              level (level-of block)
              todo (feedable (u/inventory p) {:items items :keep reserve})]
          (cond
            meal
            (let [r (await (ctx/act c :collect #js {:id (.-id meal)}))]
              (if (= "collected" (.-status r)) :again (strike! c level)))

            (>= taken times)
            (let [fed (:fed mem {})]
              (ctx/emit! c :compost.done :info {:fed fed :bone-meal taken :text (str "composted " (apply + (vals fed)) " items, took " taken " bone meal")})
              (finish! c {:level level}))

            :else
            (let [w (await (near/go-near! c pos 3 {:tolls (tc/walk-tolls c (near/cell-of pos))}))]
              (case w
                :partial :continue
                :blocked (strike! c level)
                (cond
                  (not (composter-allowed? c pos))
                  (do (ctx/emit! c :compost.no-composter :warn {:text "no composter to feed"})
                      (finish! c {:reason :no-composter}))

                  (= 8 level)
                  (let [r (await (ctx/act c :useOn #js {:pos (clj->js pos) :face "up"}))]
                    (if (= "used" (.-status r))
                      (do (ctx/update-mem! c #(-> % (update :taken (fnil inc 0)) (assoc :strikes 0)))
                          :again)
                      (strike! c level)))

                  (= 7 level)
                  (let [waits (inc (:waits mem 0))]
                    (await (ctx/act c :wait #js {:ms 500}))
                    (if (>= waits 6)
                      (do (ctx/update-mem! c assoc :waits 0)
                          (strike! c level))
                      (do (ctx/update-mem! c assoc :waits waits)
                          :again)))

                  (empty? todo)
                  (do (ctx/emit! c :compost.done :info {:fed (:fed mem {}) :bone-meal taken :reason :nothing-to-feed
                                                        :text "nothing left to feed the composter"})
                      (finish! c {:level level :reason :nothing-to-feed}))

                  :else
                  (let [[name _] (first todo)
                        r (await (ctx/act c :useOn #js {:pos (clj->js pos) :item name :face "up"}))
                        consumed (or (.-consumed r) 0)]
                    (if (pos? consumed)
                      (do (ctx/update-mem! c #(-> % (update-in [:fed name] (fnil + 0) consumed) (assoc :strikes 0)))
                          :again)
                      (strike! c level))))))))))))

(defn ^:async round
  "The whole attempt: loop the steps until one ends; :continue only while a walk waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
