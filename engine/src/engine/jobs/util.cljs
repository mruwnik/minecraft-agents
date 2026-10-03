(ns engine.jobs.util
  "Helpers the library jobs share: reading positions off JS, walking in reach,
  and bounded failure counting."
  (:require [engine.ctx :as ctx]))

(def max-failures 3)

(defn block-name
  "The block name at cell pos of primitives p, or nil when the chunk is not
  loaded (blockAt returns null there)."
  [p pos]
  (some-> (.blockAt p (clj->js pos)) .-name))

(defn pos-of
  "A JS {x y z} object as a cljs map."
  [o]
  {:x (.-x o) :y (.-y o) :z (.-z o)})

(defn self-pos [c]
  (pos-of (.-pos (.self (:primitives c)))))

(defn dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn inventory
  "The carried items as cljs maps {:name :count :slot}."
  [p]
  (mapv (fn [i] {:name (.-name i) :count (.-count i) :slot (.-slot i)})
        (array-seq (.-inventory (.self p)))))

(defn ^:async walk-near!
  "Walk until within range of pos, skipping the walk when already there.
  Resolves to :there, :partial (closer, call again) or :blocked."
  [c pos range]
  (if (<= (dist (self-pos c) pos) range)
    :there
    (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))]
      (case (.-status r)
        "arrived" :there
        "partial" :partial
        :blocked))))

(defn fail!
  "Count a failed round in job memory. Returns :continue until max-failures,
  then emits a warn of kind and gives up with :done."
  [c kind text]
  (let [tries (inc (:failures (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :failures tries)
    (if (< tries max-failures)
      :continue
      (do (ctx/emit! c kind :warn {:tries tries :text text})
          :done))))
