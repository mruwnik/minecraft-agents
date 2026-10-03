(ns jobs.survival.retreat
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Walk :step blocks directly away from the nearest hostile within :radius,
  one walk per round, at most five walks. Done when no hostile is in radius.")

(def args
  {:radius {:doc "hostiles within this many blocks count" :default 8}
   :step {:doc "blocks per walk" :default 8}})

(def max-moves 5)

(defn hostiles [p radius]
  (array-seq (.entities p #js {:radius radius :kind "hostile" :max 8})))

(defn away-from
  "The point step blocks from from, directly away from threat on the
  horizontal plane; +x when they coincide."
  [from threat step]
  (let [dx (- (:x from) (:x threat))
        dz (- (:z from) (:z threat))
        n (js/Math.hypot dx dz)
        [ux uz] (if (zero? n) [1 0] [(/ dx n) (/ dz n)])]
    {:x (js/Math.round (+ (:x from) (* ux step)))
     :y (:y from)
     :z (js/Math.round (+ (:z from) (* uz step)))}))

(defn check [_c] true)

(defn ^:async round [c]
  (let [{:keys [radius step]} (:args c)
        threat (first (hostiles (:primitives c) radius))
        moves (:moves (ctx/mem c) 0)]
    (cond
      (nil? threat) :done
      (>= moves max-moves) (do (ctx/emit! c :retreat_gave_up :warn {:text "hostile keeps up; stopped retreating"})
                               :done)
      :else
      (let [target (away-from (u/self-pos c) (u/pos-of (.-pos threat)) step)]
        (await (ctx/act c :moveTo (clj->js {:pos target :range 1})))
        (ctx/update-mem! c assoc :moves (inc moves))
        :continue))))
