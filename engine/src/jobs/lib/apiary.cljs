(ns jobs.lib.apiary
  "Helpers for the apiary jobs: reading the smoke and fire under a hive, and
  the fires that stand under the hives."
  (:require [jobs.lib.util :as u]))

;; ------------------------------------------------------------------ smoke (vanilla CampfireBlock.isSmokeyPos)

(def passes
  "Blocks without a collision box: smoke goes through them. Anything not listed counts as in the way, which can only make a hive look unsmoked."
  #{"air" "cave_air" "void_air" "water" "short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "torch" "wall_torch"})

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
  (fn [pos] (.blockAt p (clj->js pos))))

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

(defn in-area?
  "True when pos is inside the area {:box {:from :to}} or {:center :radius}."
  [{:keys [box center radius]} pos]
  (if box
    (let [{:keys [from to]} box
          within (fn [k] (<= (min (k from) (k to)) (k pos) (max (k from) (k to))))]
      (and (within :x) (within :y) (within :z)))
    (<= (u/dist center pos) radius)))

(defn fires
  "Every lit campfire in the area as {:pos :name}, nearest to the body first."
  [p {:keys [box center radius] :as area}]
  (let [me (u/pos-of (.-pos (.self p)))
        search (if box 64 (+ radius (u/dist me center)))]
    (->> (array-seq (.blocks p #js {:radius search :names fire-names :properties true :max 64}))
         (filter lit-campfire?)
         (map (fn [b] {:pos (u/pos-of (.-pos b)) :name (.-name b)}))
         (filter #(in-area? area (:pos %)))
         (sort-by #(u/dist me (:pos %))))))
