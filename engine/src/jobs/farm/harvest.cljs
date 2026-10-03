(ns jobs.farm.harvest
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Cut the ripe crops within :radius of a centre (the body's position when the job first runs,
  kept in memory) and replant them. Each round takes the first step that applies: (1) replant:
  every cell in the replant debt that is still bare farmland is seeded from what is carried
  (within :reach, else walk to the nearest); (2) cut the ripe crops within :reach, else walk
  to the nearest, giving up on a crop that cannot be reached and, after :give-up of those,
  on cutting at all (a harvest.gave-up warn; replanting and collecting go on); (3) collect
  the drops once; (4) finish with a result {:cut :replanted :bare :gave-up}, cells that
  could not be replanted named in a harvest.bare warn. The debt {:pos :seed} of a cell is
  written to job memory before its dig, so a cut, a reflex or a restart never loses which
  cells owe a seed (the world only shows bare farmland, not which seed was there).")

(def args
  {:radius {:doc "how far around the centre to harvest, in blocks" :default 12}
   :center {:doc "centre of the field; the body's position when the job first runs when nil" :default nil}
   :replant {:doc "replant what was cut" :default true}
   :crops {:doc "crop block names to cut; all known crops when nil" :default nil}
   :give-up {:doc "unreachable crops after which cutting stops" :default 4}
   :reach {:doc "cells whose centre is this close to the eye (dig and place accept 4.5) are worked without walking, in blocks" :default 4.2}})

(def ripe-age {"wheat" 7 "carrots" 7 "potatoes" 7 "beetroots" 3})

(def seed-of {"wheat" "wheat_seeds" "carrots" "carrot" "potatoes" "potato" "beetroots" "beetroot_seeds"})

(def collect-items ["wheat" "wheat_seeds" "carrot" "potato" "poisonous_potato" "beetroot" "beetroot_seeds"])

;; ------------------------------------------------------------------ pure helpers

(defn ripe-crops
  "Ripe crop cells of a wanted crop within :radius of center, not in skipped, nearest to the body first."
  [p args center skipped]
  (let [crops (or (:crops args) (keys ripe-age))
        skipped (set skipped)
        here (u/pos-of (.-pos (.self p)))
        radius (+ (:radius args) (u/dist here center))]
    (->> (array-seq (.blocks p #js {:radius radius :names (clj->js crops) :max 256}))
         (filter #(some-> (.-age %) (>= (ripe-age (.-name %)))))
         (map #(u/pos-of (.-pos %)))
         (filter #(<= (u/dist % center) (:radius args)))
         (remove skipped))))

(defn center-of
  "The centre: the :center arg, else the remembered one, else the body's position."
  [c]
  (or (:center (:args c)) (:center (ctx/mem c)) (u/self-pos c)))

(defn below [pos] (update pos :y dec))

(defn drop-debt [m pos]
  (update m :replant (fn [debts] (vec (remove #(= pos (:pos %)) debts)))))

(defn bare-debt [m pos]
  (-> (drop-debt m pos)
      (update :bare (fn [cells] (vec (distinct (conj (vec cells) pos)))))))

(defn skip-crop [m pos]
  (-> (drop-debt m pos)
      (update :skipped (fnil conj []) pos)
      (update :unreachable (fnil inc 0))))

(def max-place-fails 3)

(defn fail-debt
  "Count a refused place on the debt at pos; the cell is bare once it has failed max-place-fails times."
  [m pos]
  (let [fails (inc (or (:fails (first (filter #(= pos (:pos %)) (:replant m)))) 0))]
    (if (>= fails max-place-fails)
      (bare-debt m pos)
      (update m :replant (fn [debts] (mapv #(if (= pos (:pos %)) (assoc % :fails fails) %) debts))))))

(def eye-height 1.62)

(defn eye-dist
  "Distance from the eye of a body at feet position here to the centre of cell, the measure dig and place accept up to 4.5."
  [here cell]
  (u/dist {:x (:x here) :y (+ (:y here) eye-height) :z (:z here)}
          {:x (+ (:x cell) 0.5) :y (+ (:y cell) 0.5) :z (+ (:z cell) 0.5)}))

(def failed-reach 3.0)

(defn debt-reach
  "The eye-to-centre distance within which debt is placed without walking: a cell that was refused once needs the body closer."
  [debt reach]
  (if (pos? (:fails debt 0)) failed-reach reach))

(defn walk-range
  "The range walked to when out of reach: 3, or 2 for a debt that was refused."
  [debt]
  (if (pos? (:fails debt 0)) 2 3))

(def max-walk-fails 3)

(defn walk-fail-crop
  "Count a blocked walk to the crop at pos in :walk-fails [{:pos :n}]; the crop is skipped at the max-walk-fails-th."
  [m pos]
  (let [n (inc (or (:n (first (filter #(= pos (:pos %)) (:walk-fails m)))) 0))]
    (if (>= n max-walk-fails)
      (skip-crop m pos)
      (update m :walk-fails (fn [entries] (conj (vec (remove #(= pos (:pos %)) entries)) {:pos pos :n n}))))))

(defn walk-fail-debt
  "Count a blocked walk to the debt at pos; the cell is bare at the max-walk-fails-th."
  [m pos]
  (let [n (inc (or (:walk-fails (first (filter #(= pos (:pos %)) (:replant m)))) 0))]
    (if (>= n max-walk-fails)
      (bare-debt m pos)
      (update m :replant (fn [debts] (mapv #(if (= pos (:pos %)) (assoc % :walk-fails n) %) debts))))))

(defn inc-in [m k] (update m k (fnil inc 0)))

(defn sort-debts
  "{:owed [debts to plant] :gone [pos no longer to plant] :bare [pos with no farmland]} for the debts, read off p."
  [p debts]
  (reduce
   (fn [acc {:keys [pos] :as debt}]
     (let [here (u/block-name p pos)]
       (cond
         (nil? here) (update acc :owed conj debt)
         (not= "air" here) (update acc :gone conj pos)
         (not= "farmland" (u/block-name p (below pos))) (update acc :bare conj pos)
         :else (update acc :owed conj debt))))
   {:owed [] :gone [] :bare []}
   debts))

(defn ^:async walk!
  "Walk until within range of pos: :there, :partial (closer, call again), :no-path (the pathfinder
  found none) or :blocked (no progress, maybe only this time)."
  [c pos range]
  (if (u/within? (u/self-pos c) pos range)
    :there
    (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))
          no-path? (= "noPath" (.-reason r))]
      (case (.-status r)
        "arrived" :there
        "partial" (if no-path? :no-path :partial)
        "blocked" (if no-path? :no-path :blocked)
        :blocked))))

(defn carried-names [p] (set (map :name (u/inventory p))))

(defn nearest [p cells]
  (let [here (u/pos-of (.-pos (.self p)))]
    (first (sort-by #(u/dist here %) cells))))

(defn gave-up? [c]
  (>= (:unreachable (ctx/mem c) 0) (:give-up (:args c))))

(defn cutting? [c]
  (not (gave-up? c)))

(defn check
  "A debt is owed, a collect sweep is owed, or there is a ripe crop and cutting has not been given up.
  A job that has begun (its :center is in memory) always passes, so that the round that finishes it runs."
  [c]
  (let [m (ctx/mem c)]
    (boolean
     (or (:center m)
         (seq (:replant m))
         (:collect m)
         (and (cutting? c)
              (seq (ripe-crops (:primitives c) (:args c) (center-of c) (:skipped m))))))))

;; ------------------------------------------------------------------ steps

(defn drop-settled!
  "Forget the debts whose cell is no longer bare air, move the farmland-less ones to :bare; the owed debts."
  [c]
  (let [{:keys [owed gone bare]} (sort-debts (:primitives c) (:replant (ctx/mem c)))]
    (when (or (seq gone) (seq bare))
      (ctx/update-mem! c (fn [m] (reduce bare-debt (reduce drop-debt m gone) bare))))
    owed))

(defn ^:async plant-one!
  "Place the seed of debt at its cell and settle the debt by the status."
  [c {:keys [pos seed]}]
  (let [r (await (ctx/act c :place (clj->js {:pos pos :item seed})))]
    (case (.-status r)
      ("placed" "occupied") (ctx/update-mem! c (fn [m] (-> (drop-debt m pos)
                                                            (inc-in :replanted)
                                                            (update :planted (fn [cells] (vec (distinct (conj (vec cells) pos))))))))
      ("no-item" "unreachable") nil
      (ctx/update-mem! c fail-debt pos))))

(defn ^:async replant!
  "Step 1: :continue when a seed was planted or a walk made, else nil."
  [c]
  (let [p (:primitives c)
        owed (drop-settled! c)
        have (carried-names p)
        plantable (filterv #(have (:seed %)) owed)]
    (when (seq plantable)
      (let [here (u/self-pos c)
            reach (:reach (:args c))
            near (filterv #(<= (eye-dist here (:pos %)) (debt-reach % reach)) plantable)
            nearest-debt (first (sort-by #(u/dist here (:pos %)) plantable))
            walked (when (empty? near) (await (walk! c (:pos nearest-debt) (walk-range nearest-debt))))
            targets (if (seq near) near [nearest-debt])]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c bare-debt (:pos (first targets))) :continue)
          :blocked (do (ctx/update-mem! c walk-fail-debt (:pos (first targets))) :continue)
          (loop [todo targets]
            (if (empty? todo)
              :continue
              (do (await (plant-one! c (first todo)))
                  (recur (rest todo))))))))))

(defn ^:async cut-cell!
  "Write the debt of the crop at pos, dig it, and settle by the status."
  [c pos]
  (let [seed (get seed-of (u/block-name (:primitives c) pos))]
    (when (and (:replant (:args c)) seed)
      (ctx/update-mem! c update :replant (fnil conj []) {:pos pos :seed seed}))
    (let [r (await (ctx/act c :dig (clj->js {:pos pos})))]
      (case (.-status r)
        "dug" (ctx/update-mem! c inc-in :cut)
        "missing" nil
        (ctx/update-mem! c skip-crop pos)))))

(defn warn-gave-up! [c]
  (let [m (ctx/mem c)]
    (when (and (gave-up? c) (not (:warned m)))
      (ctx/update-mem! c assoc :warned true)
      (ctx/emit! c :harvest.gave-up :warn
                 {:unreachable (:unreachable m) :skipped (:skipped m)
                  :text (str "harvest gave up cutting after " (:unreachable m) " unreachable crops: "
                             (str/join ", " (map pr-str (:skipped m))))}))))

(defn ^:async cut!
  "Step 2: :continue when it cut or walked, else nil."
  [c]
  (let [p (:primitives c)
        m (ctx/mem c)
        ripe (when (cutting? c) (ripe-crops p (:args c) (center-of c) (:skipped m)))]
    (when (seq ripe)
      (let [here (u/self-pos c)
            near (filterv #(<= (eye-dist here %) (:reach (:args c))) ripe)
            walked (when (empty? near) (await (walk! c (first ripe) 3)))
            targets (if (seq near) near [(first ripe)])]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c skip-crop (first ripe))
                       (warn-gave-up! c)
                       :continue)
          :blocked (do (ctx/update-mem! c walk-fail-crop (first ripe))
                       (warn-gave-up! c)
                       :continue)
          (do (ctx/update-mem! c assoc :collect true)
              (loop [todo targets]
                (when (seq todo)
                  (await (cut-cell! c (first todo)))
                  (recur (rest todo))))
              (warn-gave-up! c)
              :continue))))))

(def max-home-fails 3)

(defn ^:async home!
  "Step 2b: a body farther than :radius from the centre with debts or a sweep owed walks back
  (range radius/2): :continue, or :finish after max-home-fails walks that went nowhere; nil when
  there is nothing to do."
  [c]
  (let [m (ctx/mem c)
        center (center-of c)
        radius (:radius (:args c))]
    (when (and (or (seq (:replant m)) (:collect m))
               (> (u/dist (u/self-pos c) center) radius))
      (if (>= (:home-fails m 0) max-home-fails)
        :finish
        (case (await (walk! c center (quot radius 2)))
          (:there :partial) :continue
          (do (ctx/update-mem! c inc-in :home-fails) :continue))))))

(defn ^:async collect!
  "Step 3: one collect-drops round while a sweep is owed."
  [c]
  (when (:collect (ctx/mem c))
    (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                   {:radius (+ (:radius (:args c)) 4) :filter collect-items}))]
      (when (= :done r) (ctx/update-mem! c dissoc :collect))
      :continue)))

(defn lost-cells
  "The planted cells whose block is no longer a crop (nil, an unloaded cell, is not counted)."
  [p planted]
  (filterv #(let [n (u/block-name p %)] (and n (not (ripe-age n)))) planted))

(defn finish!
  "Step 4: the debts still owed and the planted cells that are no longer crops are bare; warn, report and be done."
  [c]
  (let [m (ctx/mem c)
        lost (lost-cells (:primitives c) (:planted m))
        bare (vec (distinct (concat (:bare m) (map :pos (:replant m)) lost)))
        result {:cut (:cut m 0) :replanted (:replanted m 0) :bare bare :lost lost :gave-up (gave-up? c)}]
    (when (seq bare)
      (ctx/emit! c :harvest.bare :warn
                 {:cells bare :lost lost
                  :text (str "harvest could not replant " (count bare) " cells: " (str/join ", " (map pr-str bare))
                             (when (seq lost) (str "; lost after replanting: " (str/join ", " (map pr-str lost)))))}))
    (ctx/emit! c :harvest.done :info
               {:cut (:cut result) :replanted (:replanted result) :bare-count (count bare) :gave-up (:gave-up result)
                :text (str "harvest done: cut " (:cut result) ", replanted " (:replanted result) ", bare " (count bare))})
    (ctx/result! c result)
    :done))

(defn ^:async round [c]
  (when-not (:center (ctx/mem c))
    (ctx/update-mem! c assoc :center (center-of c)))
  (or (await (replant! c))
      (await (cut! c))
      (let [home (await (home! c))]
        (case home
          :finish (finish! c)
          :continue :continue
          (or (await (collect! c))
              (finish! c))))))
