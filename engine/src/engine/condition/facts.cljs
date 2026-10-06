(ns engine.condition.facts
  "The toolkit facts are built with (see engine.condition): unknown, the online wrapper and generic reads. The
  fact table itself is the one the default trigger set names (engine.triggers/facts)."
  (:require [engine.memory :as mem]))

(def unknown
  "The value of a fact, and of anything built on it, that is not known now."
  ::unknown)

(def max-scan-radius 32)

(def max-scan-count 256)

(defn self
  "The sensed self of p, or nil when the body is offline."
  [p]
  (let [s (.self p)]
    (when-not (= "offline" (.-status s)) s)))

(defn number-or-unknown [x] (if (number? x) x unknown))

(defn boolean-or-unknown [x] (if (boolean? x) x unknown))

(defn position? [x]
  (and (map? x) (every? #(number? (get x %)) [:x :y :z])))

(defn online
  "A fact reader that is unknown while the body is offline and otherwise
  (f p self & args)."
  [f]
  (fn [{:keys [world]} & args]
    (if-let [s (self world)]
      (apply f world s args)
      unknown)))

(defn item-count [s item]
  (transduce (comp (filter #(= item (.-name %))) (map #(.-count %))) + 0
             (array-seq (.-inventory s))))

(defn wearing?
  "Whether an armour slot or the off-hand holds item (the main hand is the held item, not worn)."
  [s item]
  (let [gear (.-equipment s)]
    (boolean (some #(= item (some-> (aget gear %) .-name)) ["head" "torso" "legs" "feet" "offHand"]))))

(defn seconds-since
  "Seconds from the latest unexpired entry of kind to the view's now, or
  unknown when there is none. The clock is the view's :now, the one body memory
  stamps entries with."
  [view kind]
  (if-let [t (:t (mem/latest view kind))]
    (/ (- (:now view) t) 1000)
    unknown))

(defn distance-to [p s pos]
  (if (position? pos)
    (let [here (.-pos s)]
      (js/Math.hypot (- (.-x here) (:x pos)) (- (.-y here) (:y pos)) (- (.-z here) (:z pos))))
    unknown))

(defn blocks-near
  "How many blocks of that name the body has seen (perception's seenBlocks, never x-ray); 0 without perception."
  [p _ item radius]
  (if-let [seen (aget p "seenBlocks")]
    (count (.call seen p #js {:radius (min radius max-scan-radius) :names #js [item] :max max-scan-count}))
    0))
