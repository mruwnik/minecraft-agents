(ns dashboard.block-height
  "How tall a wanted block stands, from minecraft-data's collision boxes (the state the want names, else the block's default)."
  (:require ["minecraft-data" :as minecraft-data]
            [engine.game :as game]
            [plan.shape :as shape]))

(def data (memoize #(minecraft-data game/default-version)))

(defn state-offset
  "Index of a block's state among its minStateId.. range: the want's values over the block's defaults, first state most significant."
  [block state]
  (let [states (array-seq (.-states block))
        default (- (.-defaultState block) (.-minStateId block))
        strides (reverse (reductions * 1 (reverse (map #(.-num_values %) (rest states)))))
        value-index (fn [s v] (let [vs (.-values s) text (shape/state-text v)]
                                (if vs (.indexOf vs text) (if (= "true" text) 0 1))))]
    (reduce (fn [offset [s stride]]
              (let [want (some (fn [[k v]] (when (= (name k) (.-name s)) v)) state)
                    current (mod (quot default stride) (.-num_values s))
                    i (if (some? want) (value-index s want) current)]
                (+ offset (* stride (- (if (neg? i) current i) current)))))
            default (map vector states strides))))

(defn top-of
  "Top of the collision boxes of a want (a cell's block), in blocks above the cell floor; 0 when it has none or is no block."
  [choice]
  (let [want (if (vector? choice) (second choice) choice)
        name (shape/want-block want)
        d (data)
        block (when name (aget (.-blocksByName d) name))
        ids (when block (aget (.-blocks (.-blockCollisionShapes d)) name))
        id (if (array? ids) (aget ids (state-offset block (when (map? want) (dissoc want :block)))) ids)
        boxes (some->> id (aget (.-shapes (.-blockCollisionShapes d))))]
    (reduce max 0 (map #(aget % 4) (array-seq (or boxes #js []))))))
