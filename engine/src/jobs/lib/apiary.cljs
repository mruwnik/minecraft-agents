(ns jobs.lib.apiary
  "Helpers for the apiary jobs: reading the smoke and fire under a hive, and
  the fires that stand under the hives."
  (:require [jobs.lib.block-sets :as bs]
            [engine.ctx :as ctx]
            [jobs.lib.game-facts :as facts]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

;; ------------------------------------------------------------------ smoke (vanilla CampfireBlock.isSmokeyPos)

(def passes
  "Blocks smoke goes through: open cells, water and the plants and torches with no full face. Carpets, signs and rails
  are walk-through but stop smoke. Anything not listed counts as in the way, which can only make a hive look unsmoked."
  (into (conj bs/air "water") #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "torch" "wall_torch"}))

(defn campfire? [b] (contains? #{"campfire" "soul_campfire"} (some-> b .-name)))

(defn lit-campfire? [b]
  (and (campfire? b) (true? (some-> b .-properties .-lit))))

(defn smoke-source
  "The position of the lit campfire that smokes the hive at pos, or nil. block-at takes a {:x :y :z} and returns a block or nil (unloaded)."
  [block-at {:keys [x y z]}]
  (let [at (fn [down] {:x x :y (- y down) :z z})]
    (loop [down 1]
      (when (<= down 5)
        (let [b (block-at (at down))]
          (cond
            (nil? b) nil
            (lit-campfire? b) (at down)
            (campfire? b) nil
            (contains? passes (.-name b)) (recur (inc down))
            :else (when (lit-campfire? (block-at (at (inc down)))) (at (inc down)))))))))

(defn covered?
  "True when what sits on the fire at pos stops bees landing in it."
  [block-at pos]
  (let [b (block-at (update pos :y inc))]
    (boolean (and b
                  (not (contains? passes (.-name b)))
                  (not= "moss_carpet" (.-name b))))))

(defn open-fire? [block-at fire]
  (not (covered? block-at fire)))

(defn hive-verdict
  "What may be done with a hive: :ok, :not-smoked or :open-fire."
  [block-at pos]
  (let [fire (smoke-source block-at pos)]
    (cond
      (nil? fire) :not-smoked
      (open-fire? block-at fire) :open-fire
      :else :ok)))

(defn block-at-fn [p]
  (fn [pos] (u/block-at p pos)))

(defn seen-block-at-fn
  "block-at-fn over what the body sees or remembers: nil for a cell it has not seen (look at it or skip it)."
  [p]
  (fn [pos] (u/seen-block p pos)))

;; ------------------------------------------------------------------ guarding

(defn solid?
  "True for a block that stops bees flying through: not in passes, or lava. An unloaded cell (nil) counts as ground: nothing is done on what cannot be read."
  [b]
  (or (nil? b)
      (= "lava" (.-name b))
      (not (contains? passes (.-name b)))))

(defn walled?
  "True when all four horizontal neighbours of pos are solid."
  [block-at {:keys [x y z]}]
  (every? #(solid? (block-at (assoc % :y y)))
          [{:x (inc x) :z z} {:x (dec x) :z z} {:x x :z (inc z)} {:x x :z (dec z)}]))

(defn raised?
  "True when a side of the fire is open, so bees fly in sideways."
  [block-at fire]
  (not (walled? block-at fire)))

(defn sinkable?
  "True when the cell under the fire is real ground with ground on its four sides, so the fire can stand one block lower."
  [block-at fire]
  (let [below (update fire :y dec)
        b (block-at below)]
    (boolean (and b
                  (not (contains? passes (.-name b)))
                  (not (contains? #{"lava" "bedrock"} (.-name b)))
                  (walled? block-at below)))))

(defn carpet?
  "True for a carpet name that does not burn; moss carpet burns."
  [n]
  (and (some? (re-matches #"[a-z_]+_carpet" n)) (not= "moss_carpet" n)))

(defn carpet-in
  "The name of the first carried carpet, or nil."
  [inventory]
  (->> inventory (map :name) (filter carpet?) first))

(defn campfire-in
  "kind when carried, else any carried campfire name, else nil."
  [inventory kind]
  (let [carried (set (map :name inventory))]
    (cond
      (carried kind) kind
      (carried "campfire") "campfire"
      (carried "soul_campfire") "soul_campfire")))

(defn needs
  "What the fire at pos lacks: :sink (raised, with ground to sink onto) and :carpet (open)."
  [block-at fire]
  (cond-> #{}
    (and (raised? block-at fire) (sinkable? block-at fire)) (conj :sink)
    (open-fire? block-at fire) (conj :carpet)))

(def fire-names #js ["campfire" "soul_campfire"])

(defn pos-key [{:keys [x y z]}] (str x "," y "," z))

(defn center-of [c]
  (or (:center (:args c)) (:center (ctx/mem c)) (u/self-pos c)))

(defn corners [{:keys [from to]}]
  (for [x [(:x from) (:x to)] y [(:y from) (:y to)] z [(:z from) (:z to)]]
    {:x x :y y :z z}))

(defn watched-cells
  "The cells whose never having been seen calls for a look: the corners of the box, or the centre."
  [c]
  (if-let [box (:box (:args c))] (corners box) [(center-of c)]))

(defn in-area?
  "True when pos is inside the area {:box {:from :to}} or {:center :radius}."
  [{:keys [box center radius]} pos]
  (if box
    (let [{:keys [from to]} box
          within (fn [k] (<= (min (k from) (k to)) (k pos) (max (k from) (k to))))]
      (and (within :x) (within :y) (within :z)))
    (<= (u/dist center pos) radius)))

(defn fires
  "Every lit campfire the body has seen in the area as {:pos :name}, nearest to the body first."
  [p {:keys [box center radius] :as area}]
  (let [me (u/pos-of (.-pos (.self p)))
        search (if box 64 (+ radius (u/dist me center)))]
    (->> (look/seen-blocks p {:radius search :names fire-names :properties? true :live? true :max 64})
         (filter #(true? (get-in % [:properties :lit])))
         (map #(select-keys % [:pos :name]))
         (filter #(in-area? area (:pos %)))
         (sort-by #(u/dist me (:pos %))))))

;; ------------------------------------------------------------------ what the jobs share

(def tool-item {:shears "shears" :bottle "glass_bottle"})
(def hive-names #js ["beehive" "bee_nest"])

(defn permitted-fires
  "The fires of the area whose cell the job may dig and the cell over it place on (quiet?: no decline warn)."
  [c fires & [quiet?]]
  (let [ok (fn [action poss]
             (set (if quiet?
                    (:allowed (gate/look c action poss {}))
                    (gate/allowed c :apiary.guard-declined "apiary guard" action poss))))
        dig (ok :dig (map :pos fires))
        place (ok :place (map #(update (:pos %) :y inc) fires))]
    (filterv #(and (dig (:pos %)) (place (update (:pos %) :y inc))) fires)))

(defn plan-fire
  "What to do about one fire: nil when it is safe, {:pos :action :kind/:item} when something carried does it,
  else {:pos :left reason}."
  [block-at inventory {:keys [pos name]}]
  (let [ns (needs block-at pos)
        kind (campfire-in inventory name)
        carpet (carpet-in inventory)]
    (cond
      (empty? ns) nil
      (and (:sink ns) kind) {:pos pos :action :sink :kind kind}
      (and (:carpet ns) carpet) {:pos pos :action :carpet :item carpet}
      :else {:pos pos :left (if (:carpet ns) :no-carpet :no-campfire)})))

(defn survey
  "{:fires n :todo [action maps, nearest first] :left {key reason}} of the fires not skipped."
  [c center & [quiet?]]
  (let [p (:primitives c)
        block-at (block-at-fn p)
        inventory (u/inventory p)
        skipped (:skipped (ctx/mem c) {})
        all (permitted-fires c (fires p {:box (:box (:args c)) :center center :radius (:radius (:args c))}) quiet?)
        plans (->> all
                   (remove #(contains? skipped (pos-key (:pos %))))
                   (keep #(plan-fire block-at inventory %)))]
    {:fires (count all)
     :todo (vec (filter :action plans))
     :left (into {} (keep (fn [pl] (when (:left pl) [(pos-key (:pos pl)) (:left pl)])) plans))}))

(defn ripe? [b] (>= (or (get-in b [:properties :honey_level]) 0) facts/ripe-level))

(defn found-hives
  "Every hive the body has seen in the area, as {:pos :ripe}."
  [c center]
  (let [p (:primitives c)
        me (u/self-pos c)
        {:keys [radius box]} (:args c)
        search (if box 64 (+ radius (u/dist me center)))]
    (->> (look/seen-blocks p {:radius search :names hive-names :properties? true :live? true :max 64})
         (map (fn [b] {:pos (:pos b) :ripe (ripe? b)}))
         (filter #(in-area? {:box box :center center :radius radius} (:pos %))))))

(defn hives
  "Every hive the body has seen in the area that the zone rules let the job use, as {:pos :ripe}, nearest to the
  body first (quiet?: no decline warn)."
  [c center & [quiet?]]
  (let [found (found-hives c center)
        poss (map :pos found)
        ok (set (if quiet?
                  (:allowed (gate/look c :harvest poss {:own-plans-ok? true}))
                  (gate/allowed c :apiary.declined "apiary harvest" :harvest poss {:own-plans-ok? true})))]
    (->> found
         (filter #(ok (:pos %)))
         (sort-by #(u/dist (u/self-pos c) (:pos %))))))

(defn tool-for
  "The item name to harvest with under :with, from the inventory, or nil."
  [with inventory]
  (let [carried? (fn [n] (some #(= n (:name %)) inventory))
        wanted (case (keyword with)
                 :shears [:shears]
                 :bottle [:bottle]
                 [:shears :bottle])]
    (some #(when (carried? (tool-item %)) (tool-item %)) wanted)))

(defn classify
  "The hives as {:todo [pos] :declined {key reason} :ripe n :hives n}: ripe hives not skipped, sorted by the verdict."
  [c hs]
  (let [block-at (seen-block-at-fn (:primitives c))
        skipped (:skipped (ctx/mem c) {})
        ripe (->> hs (filter :ripe) (remove #(contains? skipped (pos-key (:pos %)))))
        verdicts (map (fn [h] [(:pos h) (hive-verdict block-at (:pos h))]) ripe)]
    {:hives (count hs)
     :ripe (count (filter :ripe hs))
     :todo (vec (keep (fn [[pos v]] (when (= :ok v) pos)) verdicts))
     :declined (into {} (keep (fn [[pos v]] (when-not (= :ok v) [(pos-key pos) v])) verdicts))}))
