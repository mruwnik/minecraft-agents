(ns jobs.survival.dig-in
  (:require [engine.ctx :as ctx]
            [engine.jobs.shelter :as sh]
            [engine.jobs.util :as u]))

(def doc
  "Roof the body in for the night. Check: it is night and nothing solid is
  within :roof-height blocks above. With enough :blocks carried to fill every
  open cell, it walls a 1x1 shelter: the four sides at feet height, the four at
  head height, then one above the head, at most :max-places placements per
  round. With fewer it digs down two blocks (collecting the blocks it digs)
  and places one above, at the cell the body stood in, from a carried or dug
  block; with none it just digs down two. Returns :continue until roofed.
  When it ends, however it ends, it writes a :shelter entry {:pos :roof :state
  :built} (cap 10, kept one in-game day); :roof is absent when nothing was
  placed above. A body that cannot place or dig gives up after three failures
  with a dig_in_failed warn.")

(def building-blocks
  ["dirt" "cobblestone" "cobbled_deepslate" "stone" "andesite" "diorite" "granite" "netherrack"
   "oak_planks" "spruce_planks" "birch_planks" "jungle_planks" "acacia_planks" "dark_oak_planks"
   "mangrove_planks" "cherry_planks"])

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}
   :blocks {:doc "names of the blocks it may place" :default building-blocks}
   :max-places {:doc "placements per round" :default 4}})

(def shelter-policy {:cap 10 :ttl sh/ms-per-day})

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(def hazards #{"lava" "water"})

(defn carried
  "The carried [{:name :count}] whose name is in blocks, in the order of blocks."
  [c blocks]
  (let [have (into {} (map (juxt :name :count)) (u/inventory (:primitives c)))]
    (vec (for [b blocks :let [n (get have b 0)] :when (pos? n)] {:name b :count n}))))

(defn pick [c blocks] (:name (first (carried c blocks))))

(defn open-cells
  "The cells to fill around the feet cell, in placement order: sides at feet
  height, sides at head height, the one above the head; only those not solid."
  [p {:keys [x y z]}]
  (filterv #(not (sh/solid-at? p %))
           (concat (for [dy [0 1] [dx dz] sides] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
                   [{:x x :y (+ y 2) :z z}])))

(defn ^:async place-all!
  "Place item-picked blocks at cells in order. Resolves to :ok, or the first
  status that is not placed or occupied (no-item when none is carried)."
  [c blocks cells]
  (loop [cells cells]
    (let [item (pick c blocks)]
      (cond
        (empty? cells) :ok
        (nil? item) "no-item"
        :else (let [r (await (ctx/act c :place (clj->js {:pos (first cells) :item item})))
                    status (.-status r)]
                (if (#{"placed" "occupied"} status)
                  (recur (rest cells))
                  status))))))

(defn ^:async walls-round [c]
  (let [{:keys [blocks max-places roof-height]} (:args c)
        p (:primitives c)
        status (await (place-all! c blocks (take max-places (open-cells p (sh/feet p)))))]
    (cond
      (not= :ok status) (u/fail! c :dig_in_failed (str "cannot place a block: " status))
      (sh/roofed? p roof-height) :done
      :else :continue)))

(defn ^:async collect-drops!
  "Pick up the placeable blocks a dig dropped."
  [c blocks drops]
  (loop [ds (filter #(some #{(.-name %)} blocks) (array-seq drops))]
    (when (seq ds)
      (await (ctx/act c :collect #js {:id (.-id (first ds))}))
      (recur (rest ds)))))

(defn ^:async descend-round
  "One step down toward the pit: dig the block below the feet, collect what
  it dropped, and step into the hole."
  [c]
  (let [{:keys [blocks]} (:args c)
        p (:primitives c)
        {:keys [x y z]} (sh/feet p)
        below {:x x :y (dec y) :z z}
        name (u/block-name p below)]
    (cond
      (hazards name) (do (ctx/emit! c :dig_in_failed :warn {:text (str name " below the body; not digging down")})
                         :done)
      (not (sh/solid-at? p below)) (do (await (ctx/act c :moveTo (clj->js {:pos below :range 0.5})))
                                       :continue)
      :else (let [r (await (ctx/act c :dig (clj->js {:pos below})))]
              (if (= "dug" (.-status r))
                (do (await (collect-drops! c blocks (.-drops r)))
                    :continue)
                (u/fail! c :dig_in_failed (str "cannot dig down: " (.-status r))))))))

(defn ^:async roof-round
  "In the pit: place one block at the cell the body started in."
  [c]
  (let [{:keys [blocks]} (:args c)
        item (pick c blocks)
        roof (:roof (ctx/mem c))]
    (if (nil? item)
      :done
      (let [r (await (ctx/act c :place (clj->js {:pos roof :item item})))]
        (if (#{"placed" "occupied"} (.-status r))
          :done
          (u/fail! c :dig_in_failed (str "cannot roof the pit: " (.-status r))))))))

(defn choose-mode
  "Record in job memory how this shelter is built, once: :walls when enough
  blocks are carried to fill every open cell, else :dig with the starting cell
  as the roof and the way out."
  [c]
  (when-not (:mode (ctx/mem c))
    (let [p (:primitives c)
          start (sh/feet p)
          needed (count (open-cells p start))
          have (reduce + (map :count (carried c (:blocks (:args c)))))]
      (if (>= have needed)
        (ctx/update-mem! c assoc :mode :walls :roof (update start :y + 2))
        (ctx/update-mem! c assoc :mode :dig :roof start :target-y (- (:y start) 2))))))

(defn check [c]
  (and (sh/night? (:primitives c))
       (not (sh/roofed? (:primitives c) (:roof-height (:args c))))))

(defn ^:async step [c]
  (choose-mode c)
  (let [{:keys [mode target-y]} (ctx/mem c)]
    (cond
      (= :walls mode) (await (walls-round c))
      (> (:y (sh/feet (:primitives c))) target-y) (await (descend-round c))
      :else (await (roof-round c)))))

(defn ^:async round [c]
  (let [r (await (step c))]
    (when (= :done r)
      (let [p (:primitives c)
            roof (:roof (ctx/mem c))
            roofed (and roof (sh/solid-at? p roof))]
        (ctx/remember! c :shelter (cond-> {:pos (sh/feet p) :state :built}
                                    roofed (assoc :roof roof))
                       shelter-policy)))
    r))
