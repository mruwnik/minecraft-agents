(ns jobs.farm.fertilize
  (:require [engine.ctx :as ctx]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.lib.crops :as crops]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.toll-cells :as tc]))

(def doc
  "Use bone meal on unripe crops: the crop at :at, or the unripe crops within :radius, nearest first.
  With :grass true it uses bone meal once on each open grass block (nothing on top) it has seen, instead of on crops.
  Needs bone meal in the inventory; it is fetched (jobs.lib.fetch) unless :fetch is false, then the job waits :need. A crop that refuses bone meal or cannot be reached is skipped.
  Ends when :max uses are spent, the bone meal runs out or no unripe crop is left. Result: {:used n}.
  Zones: a crop in another owner's zone or claim, or inside a plan's footprint, is skipped (it counts as a
  :harvest). The job warns fertilize.declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.")

(def args
  {:at {:doc "one crop position to fertilize; the crops around the body when nil" :type :pos :default nil}
   :radius {:doc "crops within this many blocks of the body count, when :at is nil" :default 8}
   :center {:doc "centre of the radius search; the body's position when nil" :type :pos :default nil}
   :grass {:doc "fertilize open grass blocks instead of crops" :default false}
   :max {:doc "bone meal uses, at most" :default 16}
   :fetch {:doc "get missing bone meal (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits :need instead" :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def ripe-age crops/ripe-age)

(defn age-of
  "The age of a block result from the primitives, or nil."
  [b]
  (or (.-age b) (some-> b .-properties .-age)))

(defn unripe?
  "True when b (a block result) is a crop below its ripe age."
  [b]
  (let [ripe (some-> b .-name ripe-age)
        age (when b (age-of b))]
    (boolean (and ripe age (< age ripe)))))

(defn open-grass
  "The seen grass blocks within radius of mid with air above, as positions."
  [p mid radius reach]
  (->> (look/seen-blocks p {:names ["grass_block"] :radius reach :max 4096 :live? true})
       (map :pos)
       (filter #(<= (u/dist mid %) radius))
       (filter #(= "air" (u/block-name p (update % :y inc))))))

(defn targets
  "The positions to fertilize (unripe crops, or open grass with :grass), nearest first, minus the refused ones."
  [c]
  (let [p (:primitives c)
        {:keys [at radius center grass]} (:args c)
        refused (:refused (ctx/mem c) #{})
        me (u/self-pos c)
        mid (or center me)
        reach (+ radius (u/dist me mid))
        found (cond
                grass (if at
                        (if (and (= "grass_block" (u/block-name p at)) (= "air" (u/block-name p (update at :y inc)))) [at] [])
                        (open-grass p mid radius reach))
                at (let [b (u/block-at p at)]
                     (if (unripe? b) [at] []))
                :else
                (->> (crops/seen-crops p (keys ripe-age) reach 4096)
                     (filter #(some-> (:age %) (< (ripe-age (:name %)))))
                     (map :pos)
                     (filter #(<= (u/dist mid %) radius))))]
    (->> (remove refused found)
         (gate/allowed c :fertilize.declined "fertilize" :harvest)
         (sort-by #(u/dist me %)))))

(defn has-meal? [p]
  (some #(= "bone_meal" (:name %)) (u/inventory p)))

(defn problem
  "The wait reason {:reason :need :item \"bone_meal\"} while targets remain, no bone meal is carried and none was
  used yet; else nil."
  [c]
  (when (and (not (has-meal? (:primitives c)))
             (not (pos? (:used (ctx/mem c) 0)))
             (seq (targets c)))
    {:reason :need :item "bone_meal"}))

(defn check
  "True with bone meal in the pockets, when some was used (the round finishes), when no target remains, or when
  fetch will get the bone meal; else waits :need."
  [c]
  (if-let [w (problem c)]
    (fetch/check c 'jobs.farm.fertilize w)
    true))

(defn ^:async fertilize-one!
  "Finish when nothing is left to fertilize, the budget is spent or the bone meal ran out; else walk to the nearest
  target of todo and use one bone meal."
  [c todo used]
  (if (or (empty? todo) (>= used (:max (:args c))) (not (has-meal? (:primitives c))))
    (do (ctx/emit! c :fertilize.done :info {:used used :text (str "fertilized with " used " bone meal")})
        (ctx/result! c {:used used})
        :done)
    (let [target (first todo)
          refuse! #(ctx/update-mem! c update :refused (fnil conj #{}) target)
          w (await (near/go-near! c target 3 {:tolls (tc/walk-tolls c (near/cell-of target))}))]
      (case w
        :partial :continue
        :blocked (do (refuse!) :again)
        (if-not (gate/allowed? c :fertilize.declined "fertilize" :harvest target)
          (do (refuse!) :again)
          (let [r (await (ctx/act c :useOn #js {:pos (clj->js target) :item "bone_meal" :face "up"}))]
            (if (= "used" (.-status r))
              (do (ctx/update-mem! c update :used (fnil + 0) (max 1 (or (.-consumed r) 1)))
                  (when (:grass (:args c)) (refuse!)))
              (refuse!))
            :again))))))

(defn ^:async step
  "One step: fetch missing bone meal unless :fetch is false (a failed fetch leaves the job waiting), then
  fertilize-one!."
  [c]
  (let [_ (when (and (empty? (targets c)) (not (look/looked-here? c)))
            (await (look/look-around! c)))
        fetched (when (problem c) (await (fetch/fetch! c 'jobs.farm.fertilize problem)))]
    (cond
      fetched fetched
      (problem c) :continue
      :else (await (fertilize-one! c (targets c) (:used (ctx/mem c) 0))))))

(defn ^:async round
  "The whole attempt: loop the steps until the targets or the bone meal are done; :continue only while a walk or fetch
  waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
