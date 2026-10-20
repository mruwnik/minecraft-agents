(ns jobs.farm.compost
  (:require [jobs.lib.crops :as crops]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.job-common :as common]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.ready :as ready]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.toll-cells :as tc]))

(def doc
  "Feed a composter what a farm cannot use and collect the bone meal it makes.
  Walks to the composter (a go-to child), feeds it, empties it when full and picks up the bone meal.
  Zones: a composter in another owner's zone or claim, or inside a plan's footprint, is not used (it counts as a
  :harvest). :ignore-zones? true skips the check.
  Waits before it starts (a listed call waits with the reason): {:reason :no-zones} (no zone list read, unless
  :ignore-zones?), :nothing-to-feed (nothing carried to feed), :no-composter (none seen and allowed; a :look first).
  A composter at level 7 is waited out with short pauses (a jobs.time.noop child).
  Ends when :times bone meal is taken (done), nothing is left to feed, there is no composter, or ::max-strikes failures in a row.
  Data {:fed {name n} :bone-meal n :level}, plus :reason when it stopped early. Stopped early with at least :least bone meal
  taken (default 1) it is done; with less it is condition-failed :no-composter or :nothing-to-feed, or nothing-possible
  :gave-up. done? counts the bone meal taken.")

(a/defargs args
  {:at {:doc "the composter position; the nearest composter within :radius when nil" :spec ::a/pos :default nil}
   :items {:doc "item names to feed; every compostable thing carried except seeds and food when nil" :spec (a/coll-of a/item?) :default nil}
   :keep {:doc "{item count} reserves never fed" :spec (a/map-of string? number?) :default {}}
   :times {:doc "bone meal to take before done" :spec (a/int-in 1 nil) :default 1}
   :ignore-zones? :shared})

(def compostable
  "Everything a composter accepts, any chance."
  (into #{"torchflower_seeds" "pitcher_pod" "short_grass"
    "tall_grass" "fern" "large_fern" "seagrass" "kelp" "dried_kelp" "oak_leaves" "birch_leaves" "spruce_leaves"
    "jungle_leaves" "acacia_leaves" "dark_oak_leaves" "cherry_leaves" "mangrove_leaves" "azalea_leaves" "oak_sapling"
    "birch_sapling" "spruce_sapling" "jungle_sapling" "acacia_sapling" "dark_oak_sapling" "cherry_sapling"
    "mangrove_propagule" "sweet_berries" "glow_berries" "hanging_roots" "moss_carpet" "small_dripleaf" "dead_bush"
    "sugar_cane" "cactus" "vine" "melon_slice" "pumpkin" "carved_pumpkin" "nether_wart" "glow_lichen" "tall_seagrass"
    "big_dripleaf" "lily_pad" "apple" "cocoa_beans" "melon" "brown_mushroom"
    "red_mushroom" "sea_pickle" "moss_block" "pink_petals" "bamboo" "nether_sprouts" "bread" "cookie" "baked_potato"
    "hay_block" "brown_mushroom_block" "red_mushroom_block" "nether_wart_block" "warped_wart_block"
    "cake" "pumpkin_pie"}
        (concat crops/field-seeds crops/other-seeds (remove #{"poisonous_potato"} crops/produce-items))))

(def held-back
  "Kept when no :items is given: seeds to sow again and the body's own food."
  (into #{"sugar_cane" "bamboo" "bread" "cookie" "baked_potato" "apple" "sweet_berries" "glow_berries" "melon_slice"
          "cake" "pumpkin_pie" "beetroot"}
        (concat crops/field-seeds crops/other-seeds)))

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

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::max-strikes {:default 3 :doc "Failures in a row before the job gives up." :spec (a/int-in 1 nil)}
   ::meal-looks {:default 5 :doc "Looks at the composter right after a take for the meal it dropped." :spec (a/int-in 0 nil)}
   ::radius {:default 16 :doc "How far to look for a composter when no :at is given, in blocks." :spec (a/num-in 0 nil)}
   ::level-7-pause-s {:default 1 :doc "Seconds paused at a level-7 composter before looking again (it turns 8 within a second)." :spec (a/num-in 0 nil)}
   ::max-waits {:default 6 :doc "Pauses at a level-7 composter that count as one failure." :spec (a/int-in 1 nil)}
   ::meal-look {:default 20 :doc "Bone meal on the ground is looked for this far from the body, in blocks." :spec (a/num-in 0 nil)}
   ::meal-reach {:default 3 :doc "Bone meal this close to the composter counts as its own, in blocks." :spec (a/num-in 0 nil)}})

(def target [:at])

(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(defn done?
  "Ok once :times bone meal is taken; else later with the bone meal taken and the rest of :times."
  [c]
  (let [taken (:taken (ctx/mem c) 0)
        data [:units :bone-meal :done taken :left (max 0 (- (:times (:args c)) taken))
              :result {:fed (:fed (ctx/mem c) {}) :bone-meal taken}]]
    (if (>= taken (:times (:args c)))
      (apply contract/ok :composted (str "composted, " taken " bone meal taken") data)
      (apply contract/later :composting (str "compost: " taken " bone meal taken") data))))

(defn quote-of [c]
  (let [times (:times (:args c))]
    {:yields {:items {"bone_meal" [0 times times]}}
     :uses {:job-time [2 (* 40 times) (* 200 times)]}
     :done [0 times times]}))

(defn level-of
  "The composter's level as a number: perception keeps block properties as strings."
  [b]
  (or (some-> b .-properties .-level js/parseInt) 0))

(defn ^:async level-now!
  "The composter's level after a look at it: it changes with every item fed, and the last sight of it is old by then."
  [c pos]
  (let [{:keys [x y z]} (:pos (places/parse-pos pos))]
    (await (ctx/act c :look #js {:pos #js {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}}))
    (look/see! c)
    (level-of (u/seen-block (:primitives c) pos))))

(defn composter-allowed?
  "Whether the job may use the composter at pos (zones, claims, footprints)."
  [c pos]
  (boolean (seq (:allowed (gate/look c :harvest [pos] {:own-plans-ok? true})))))

(defn nearest-composter
  "The nearest composter within :radius that the zone rules let the job use."
  [c]
  (let [me (u/self-pos c)]
    (->> (:allowed (gate/look c :harvest
                                 (map :pos (look/seen-blocks (:primitives c) {:names ["composter"] :radius (settings/get settings ::radius) :live? true}))
                                 {:own-plans-ok? true}))
         (sort-by #(u/dist me %))
         first)))

(defn composter-at
  "The composter the job works at: the one it settled on, else the call's :at (when allowed) or the nearest allowed
  one, and its block when seen as a composter; else nil."
  [c]
  (let [{:keys [at]} (:args c)
        pos (or (:composter (ctx/mem c)) (if at (when (composter-allowed? c at) at) (nearest-composter c)))
        block (when pos (u/seen-block (:primitives c) pos))]
    (when (and block (= "composter" (.-name block))) pos)))

(defn meal-near
  "The nearest bone_meal item entity within ::meal-reach blocks of the composter, or nil."
  [c at]
  (->> (look/seen-items (:primitives c) {:radius (settings/get settings ::meal-look)})
       (filter #(= "bone_meal" (some-> (.-item %) .-name)))
       (filter #(<= (u/dist at (u/pos-of (.-pos %))) (settings/get settings ::meal-reach)))
       first))

(defn ^:async take-meal!
  "An items.collect-drops child takes the meal: :continue while it waits, :collected or :left."
  [c meal]
  (let [r (await (child/run!* c :collect 'jobs.items.collect-drops {:ids [(.-id meal)] :radius (settings/get settings ::meal-look)}))]
    (cond
      (= :continue r) :continue
      (pos? (or (:collected (:data r)) 0)) :collected
      :else :left)))

(defn ^:async collect-meal!
  "Right after a take, up to ::meal-looks looks at the composter for the dropped meal, collecting it when seen: the round ends
  on done? as soon as the take is counted, and the drop shows a moment after it."
  [c pos]
  (loop [n 0]
    (await (level-now! c pos))
    (if-let [meal (meal-near c pos)]
      (await (take-meal! c meal))
      (when (< n (settings/get settings ::meal-looks)) (recur (inc n))))))

(defn problem
  "The cause the job cannot start: no composter seen or allowed, else nothing to feed to one that has no bone meal
  to give (level 7 or 8, or meal lying by it)."
  [c]
  (if-let [pos (composter-at c)]
    (when (and (empty? (feedable (reserve/free c :compost) (:args c)))
               (< (level-of (u/seen-block (:primitives c) pos)) 7)
               (nil? (meal-near c pos)))
      {:reason :nothing-to-feed})
    {:reason :no-composter}))

(def can-start?
  "Zones read, a composter seen (:look until the body has looked around) and something to feed it."
  (cond/and checks/zones-read
            (checks/refusing #(if (or (look/surveyed? %) (composter-at %)) :later :look) :ready
                             "a composter and something to feed" problem)))

(defn finish!
  "End the call. Taking :times bone meal is done; an early stop (extra's :reason) is done when it took the call's least,
  else the reason is the cause: condition-failed for :no-composter and :nothing-to-feed, nothing-possible :gave-up."
  [c extra]
  (let [m (ctx/mem c)
        taken (:taken m 0)
        fed (:fed m {})
        data (merge {:fed fed :bone-meal taken} extra)
        done (assoc data :text (str "composted " (apply + (vals fed)) " items, took " taken " bone meal"))
        reason (:reason extra)]
    (cond
      (nil? reason) (ctx/end c :done nil done)
      (and (pos? taken) (common/enough? c taken (:times (:args c)))) (ctx/end c :done nil done)
      (= :nothing-to-feed reason) (ctx/end c :condition-failed reason (assoc data :text "nothing left to feed the composter"))
      :else (ctx/end c (if (= :gave-up reason) :nothing-possible :condition-failed) reason data))))

(defn strike!
  "Count a failure; :again until the ::max-strikes-th in a row, then end :gave-up."
  [c level]
  (let [strikes (inc (:strikes (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :strikes strikes)
    (if (< strikes (settings/get settings ::max-strikes))
      :again
      (finish! c {:level level :reason :gave-up}))))

(defn ^:async step
  "One step: collect bone meal lying by the composter, finish when enough
  is taken, else walk up, empty a full composter, wait out level 7, or feed it
  the first feedable item. ::max-strikes failures in a row give up."
  [c]
  (let [p (:primitives c)
        {:keys [times items] kept :keep} (:args c)
        mem (ctx/mem c)
        pos (composter-at c)
        block (when pos (u/seen-block p pos))]
    (cond
      (and (nil? pos) (not (look/surveyed? c)))
      (do (await (look/survey! c)) :again) ; a composter behind the body is not seen until it looks

      (nil? pos)
      (finish! c {:reason :no-composter})

      :else
      (do
        (when-not (:composter mem) (ctx/update-mem! c assoc :composter pos))
        (let [meal (meal-near c pos)
              taken (:taken mem 0)
              level (level-of block)
              todo (feedable (reserve/free c :compost) {:items items :keep kept})]
          (cond
            meal
            (case (await (take-meal! c meal))
              :continue :continue
              :collected :again
              (strike! c level))

            (>= taken times)
            (finish! c {:level level})

            :else
            (let [w (await (common/walk! c pos 3 {:tolls (tc/walk-tolls c (:pos (places/parse-pos pos)))}))]
              (case w
                :continue :continue
                :blocked (strike! c level)
                (let [level (await (level-now! c pos))]
                (cond
                  (not (composter-allowed? c pos))
                  (finish! c {:reason :no-composter})

                  (= 8 level)
                  (let [res (await (child/use-on! c pos nil "up"))]
                    (cond
                      (= :continue res) :continue
                      (= :used (:status res))
                      (do (ctx/update-mem! c #(-> % (update :taken (fnil inc 0)) (assoc :strikes 0)))
                          (if (= :continue (await (collect-meal! c pos))) :continue :again))
                      :else (strike! c level)))

                  (= 7 level)
                  (let [waits (inc (:waits mem 0))]
                    (if (>= waits (settings/get settings ::max-waits))
                      (do (ctx/update-mem! c assoc :waits 0)
                          (strike! c level))
                      (do (ctx/update-mem! c assoc :waits waits)
                          (if (= :continue (await (child/pause! c (settings/get settings ::level-7-pause-s) :composter-filling)))
                            :continue
                            :again))))

                  (empty? todo)
                  (finish! c {:level level :reason :nothing-to-feed})

                  :else
                  (let [[name _] (first todo)
                        res (await (child/use-on! c pos name "up"))
                        consumed (or (some-> (:raw res) .-consumed) 0)]
                    (cond
                      (= :continue res) :continue
                      (pos? consumed)
                      (do (ctx/update-mem! c #(-> % (update-in [:fed name] (fnil + 0) consumed) (assoc :strikes 0)))
                          :again)
                      :else (strike! c level)))))))))))))

(defn ^:async round
  "The whole attempt: loop the steps until one ends; :continue only while a walk waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
