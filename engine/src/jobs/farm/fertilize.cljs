(ns jobs.farm.fertilize
  (:require [engine.ctx :as ctx]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]))

(def doc
  "Use bone meal on unripe crops: the crop at :at, or the unripe crops within :radius, nearest first.
  Needs bone meal in the inventory. A crop that refuses bone meal or cannot be reached is skipped.
  Ends when :max uses are spent, the bone meal runs out or no unripe crop is left. Result: {:used n}.
  Zones: a crop in another owner's zone or claim, or inside a plan's footprint, is skipped (it counts as a
  :harvest). The job warns fertilize.declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.")

(def args
  {:at {:doc "one crop position to fertilize; the crops around the body when nil" :type :pos :default nil}
   :radius {:doc "crops within this many blocks of the body count, when :at is nil" :default 8}
   :center {:doc "centre of the radius search; the body's position when nil" :type :pos :default nil}
   :max {:doc "bone meal uses, at most" :default 16}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def ripe-age {"wheat" 7 "carrots" 7 "potatoes" 7 "beetroots" 3})

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

(defn seen-crops
  "The crops of the given names the body has seen within radius, still standing, nearest first, as {:name :pos :age}
  (the age as last seen)."
  [p names radius max]
  (->> (look/seen-blocks p {:names names :radius radius :max max :live? true :properties? true})
       (map (fn [b] (assoc b :age (some-> (get-in b [:properties :age]) js/Number))))))

(defn targets
  "The unripe crop positions to fertilize, nearest first, minus the refused ones."
  [c]
  (let [p (:primitives c)
        {:keys [at radius center]} (:args c)
        refused (:refused (ctx/mem c) #{})
        me (u/self-pos c)
        mid (or center me)
        reach (+ radius (u/dist me mid))
        found (if at
                (let [b (u/block-at p at)]
                  (if (unripe? b) [at] []))
                (->> (seen-crops p (keys ripe-age) reach 4096)
                     (filter #(some-> (:age %) (< (ripe-age (:name %)))))
                     (map :pos)
                     (filter #(<= (u/dist mid %) radius))))]
    (->> (remove refused found)
         (gate/allowed c :fertilize.declined "fertilize" :harvest)
         (sort-by #(u/dist me %)))))

(defn has-meal? [p]
  (some #(= "bone_meal" (:name %)) (u/inventory p)))

(defn check
  "True with bone meal in the pockets, when some was used (the round finishes), or
  when no target remains."
  [c]
  (boolean (or (has-meal? (:primitives c))
               (pos? (:used (ctx/mem c) 0))
               (empty? (targets c)))))

(defn ^:async round
  "One bounded step: finish when nothing is left to fertilize, the budget is
  spent or the bone meal ran out; else walk to the nearest unripe crop and use one."
  [c]
  (let [p (:primitives c)
        used (:used (ctx/mem c) 0)
        _ (when (and (empty? (targets c)) (not (look/looked-here? c)))
            (await (look/look-around! c)))
        todo (targets c)]
    (if (or (empty? todo) (>= used (:max (:args c))) (not (has-meal? p)))
      (do (ctx/emit! c :fertilize.done :info {:used used :text (str "fertilized with " used " bone meal")})
          (ctx/result! c {:used used})
          :done)
      (let [target (first todo)
            refuse! #(ctx/update-mem! c update :refused (fnil conj #{}) target)
            w (await (near/walk-near! c target 3))]
        (case w
          :partial :continue
          :blocked (do (refuse!) :continue)
          (if-not (gate/allowed? c :fertilize.declined "fertilize" :harvest target)
            (do (refuse!) :continue)
            (let [r (await (ctx/act c :useOn #js {:pos (clj->js target) :item "bone_meal" :face "up"}))]
              (if (= "used" (.-status r))
                (ctx/update-mem! c update :used (fnil + 0) (max 1 (or (.-consumed r) 1)))
                (refuse!))
              :continue)))))))
