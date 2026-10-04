(ns jobs.farm.plant
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [jobs.farm.harvest :as harvest]))

(def doc
  "Sow the bare farmland of a :box. A cell is bare when the block at (x, min.y, z) is farmland and
  the block above it is air (an unloaded block is not bare). Each round takes the bare cells that
  are not skipped and the seed (the :seed arg, else the carried seed with the largest stack):
  no cell ends the job with reason :none (:done once something was planted, :gave-up when cells
  were skipped), no seed with :no-seed; otherwise the cells within :reach are planted, or the body
  walks to the nearest. A cell whose place is refused or unreachable, or whose walk is blocked,
  three times is skipped (a plant.gave-up warn). Result {:planted n :skipped [cells] :reason r}.")

(def args
  {:box {:doc "the field: {:min {:x :y :z} :max {:x :y :z}}, inclusive; the ground layer is y = (:y :min); required (without it the check declines)" :default nil}
   :seed {:doc "item name to plant; the carried seed with the largest count when nil" :default nil}
   :reach {:doc "cells whose centre is this close to the eye (place accepts 4.5) are planted without walking, in blocks" :default 4.2}})

(def max-fails 3)

;; ------------------------------------------------------------------ pure helpers

(defn bare-cells
  "The bare cells of box (farmland with air above, ground layer y = min y) not in skipped, in x then z order."
  [p box skipped]
  (let [skipped (set skipped)
        {:keys [min max]} box
        y (:y min)]
    (->> (for [x (range (:x min) (inc (:x max))) z (range (:z min) (inc (:z max)))] {:x x :y y :z z})
         (remove skipped)
         (filter #(and (= "farmland" (u/block-name p %))
                       (= "air" (u/block-name p (update % :y inc))))))))

(defn pick-seed
  "The seed to plant given the inventory: seed when it is carried, else the largest carried stack of a known seed; nil when none."
  [seed inventory]
  (let [counts (reduce (fn [acc {:keys [name count]}] (update acc name (fnil + 0) count)) {} inventory)]
    (if seed
      (when (pos? (get counts seed 0)) seed)
      (->> (vals harvest/seed-of)
           (filter #(pos? (get counts % 0)))
           (sort-by #(- (counts %)))
           first))))

(defn count-fail
  "Count a fail of kind k (:fails or :walk-fails) on the cell at pos; the cell is skipped at the max-fails-th."
  [m k pos]
  (let [n (inc (or (:n (first (filter #(= pos (:pos %)) (k m)))) 0))]
    (if (>= n max-fails)
      (update m :skipped (fnil conj []) pos)
      (update m k (fn [entries] (conj (vec (remove #(= pos (:pos %)) entries)) {:pos pos :n n}))))))

(defn owes?
  "A box is given and the run has started, or a bare cell that is not skipped exists and a seed is carried.
  A started run always passes, so that the round that finishes it runs."
  [p args m]
  (boolean
   (and (:box args)
        (or (:started m)
            (and (seq (bare-cells p (:box args) (:skipped m)))
                 (pick-seed (:seed args) (u/inventory p)))))))

(defn check [c] (owes? (:primitives c) (:args c) (ctx/mem c)))

;; ------------------------------------------------------------------ steps

(defn finish!
  [c reason]
  (let [m (ctx/mem c)
        skipped (vec (:skipped m))
        result {:planted (:planted m 0) :skipped skipped :reason reason}]
    (when (seq skipped)
      (ctx/emit! c :plant.gave-up :warn
                 {:cells skipped
                  :text (str "plant gave up on " (count skipped) " cells: " (str/join ", " (map pr-str skipped)))}))
    (ctx/emit! c :plant.done :info
               {:planted (:planted result) :skipped-count (count skipped) :reason reason
                :text (str "plant done: planted " (:planted result) ", skipped " (count skipped) ", reason " (name reason))})
    (ctx/result! c result)
    :done))

(defn ^:async plant-one!
  "Place seed on cell and settle by the status: :no-seed when the seed is gone, else nil."
  [c cell seed]
  (let [r (await (ctx/act c :place (clj->js {:pos (update cell :y inc) :item seed})))]
    (case (.-status r)
      ("placed" "occupied") (do (ctx/update-mem! c harvest/inc-in :planted) nil)
      "no-item" :no-seed
      (do (ctx/update-mem! c count-fail :fails cell) nil))))

(defn ^:async plant-all!
  "Plant each cell in turn; :no-seed when the seeds ran out, else nil."
  [c cells seed]
  (loop [todo cells]
    (when (seq todo)
      (or (await (plant-one! c (first todo) seed))
          (recur (rest todo))))))

(defn ^:async round [c]
  (when-not (:started (ctx/mem c))
    (ctx/update-mem! c assoc :started true))
  (let [p (:primitives c)
        m (ctx/mem c)
        cells (vec (bare-cells p (:box (:args c)) (:skipped m)))
        seed (pick-seed (:seed (:args c)) (u/inventory p))]
    (cond
      (empty? cells) (finish! c (cond (seq (:skipped m)) :gave-up (pos? (:planted m 0)) :done :else :none))
      (nil? seed) (finish! c :no-seed)
      :else
      (let [here (u/self-pos c)
            near (filterv #(<= (harvest/eye-dist here %) (:reach (:args c))) cells)
            target (harvest/nearest p cells)
            walked (when (empty? near) (await (harvest/walk! c target 3)))]
        (case walked
          :partial :continue
          :no-path (do (ctx/update-mem! c count-fail :walk-fails target) :continue)
          :blocked (do (ctx/update-mem! c count-fail :walk-fails target) :continue)
          (if (= :no-seed (await (plant-all! c (if (seq near) near [target]) seed)))
            (finish! c :no-seed)
            :continue))))))
