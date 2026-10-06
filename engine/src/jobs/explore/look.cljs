(ns jobs.explore.look
  (:require [engine.ctx :as ctx] [jobs.lib.places :as places] [jobs.lib.util :as u]))
(def doc
  "Observe nearby loaded blocks and entities once without moving or changing the world. Emits look.observed,
  available through observe --wait --watch or observe result after completion. :at inspects one exact block
  including properties (air is known; an unloaded cell is unknown). Nearby samples are nearest first and bounded;
  :more-blocks?/:more-entities? report extra matches. This is loaded-chunk evidence, not a complete terrain map.")
(def args
  {:radius {:doc "nearby sample radius, 1..32 blocks" :default 16}
   :block-names {:doc "nil, one block name or a vector/set of up to 16 names" :default nil}
   :entity-names {:doc "nil, one entity name or a vector/set of up to 16 names" :default nil}
   :max-blocks {:doc "nearest blocks to return, 0..16; zero skips scan" :default 6}
   :max-entities {:doc "nearest entities to return, 0..8; zero skips scan" :default 2}
   :properties? {:doc "include nearby block state properties" :default false}
   :at {:doc "optional exact block position [x y z] or {:x :y :z}" :type :pos :default nil}})
(defn check [_] true)
(defn names [v]
  (cond (nil? v) nil (string? v) [v] (or (vector? v) (set? v)) (vec v) :else ::bad))
(defn count-bound? [v hi] (and (integer? v) (<= 0 v hi)))
(defn valid-names? [v]
  (or (nil? v) (and (vector? v) (<= 1 (count v) 16)
                    (every? #(and (string? %) (re-matches #"[a-z0-9_]{1,80}" %)) v))))
(defn options [a]
  (let [a (merge (into {} (map (fn [[k v]] [k (:default v)]) args)) a)
        blocks (names (:block-names a)) entities (names (:entity-names a))
        at (when (:at a) (places/parse-pos (:at a)))]
    (cond
      (not (and (number? (:radius a)) (js/isFinite (:radius a)) (<= 1 (:radius a) 32))) {:error "radius must be 1..32"}
      (not (count-bound? (:max-blocks a) 16)) {:error "max-blocks must be an integer 0..16"}
      (not (count-bound? (:max-entities a) 8)) {:error "max-entities must be an integer 0..8"}
      (not (and (valid-names? blocks) (valid-names? entities))) {:error "names must be nil, a name, or up to 16 block/entity names"}
      (not (boolean? (:properties? a))) {:error "properties? must be true or false"}
      (:reason at) {:error (:message at)}
      :else (assoc a :block-names blocks :entity-names entities :at (:pos at)))))
(defn pos [p]
  (mapv #(let [n (aget p %)] (/ (js/Math.round (* 10 n)) 10)) ["x" "y" "z"]))
(defn block [b]
  (cond-> {:name (.-name b) :pos (pos (.-pos b))}
    (some? (.-age b)) (assoc :age (.-age b))
    (.-properties b) (assoc :properties (js->clj (.-properties b) :keywordize-keys true))))
(defn entity [e]
  (cond-> {:name (.-name e) :kind (.-kind e) :pos (pos (.-pos e))}
    (.-uuid e) (assoc :uuid (.-uuid e))
    (.-username e) (assoc :username (.-username e))
    (some? (.-visible e)) (assoc :visible? (.-visible e))))
(defn observe [p {:keys [radius block-names entity-names max-blocks max-entities properties? at]}]
  (let [self (.self p)
        blocks (if (pos? max-blocks) (vec (array-seq (.blocks p #js {:radius radius :names (clj->js block-names) :max (inc max-blocks) :properties properties?}))) [])
        entities (if (pos? max-entities) (vec (array-seq (.entities p #js {:radius radius :names (clj->js entity-names) :max (inc max-entities)}))) [])
        exact (when at (u/block-at p at))]
    (cond-> {:center (pos (.-pos self)) :radius radius :scope :loaded-chunks
             :blocks (mapv block (take max-blocks blocks)) :entities (mapv entity (take max-entities entities))}
      (> (count blocks) max-blocks) (assoc :more-blocks? true)
      (> (count entities) max-entities) (assoc :more-entities? true)
      at (assoc :at (if exact (assoc (block exact) :loaded? true) {:pos (pos (clj->js at)) :loaded? false})))))
(defn round [c]
  (let [a (options (:args c))]
    (if-let [error (:error a)]
      (do (ctx/emit! c :look.refused :warn {:reason :bad-args :text error})
          (ctx/result! c {:observed false :reason :bad-args}))
      (let [result (observe (:primitives c) a)]
        (ctx/emit! c :look.observed :info result)
        (ctx/result! c result)))
    :done))
