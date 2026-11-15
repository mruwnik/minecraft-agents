(ns jobs.lib.stock
  "Chests the body has seen and what they held when last looked into or moved from: body memory :fetch/stock
  (booked by withdraw, deposit and obtain, so obtain goes first to a chest known to hold the item and skips one
  known not to)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::chest-radius {:default 32 :doc "Blocks round the body searched for a chest holding the item." :spec (a/int-in 1 nil)}
   ::view-age-ms {:default 10000 :doc "A chest view older than this is read again, in ms." :spec (a/int-in 0 nil)}})

(def container-names ["chest" "trapped_chest" "barrel"])
(defn chest-radius [] (settings/get settings ::chest-radius))
(def stock-kind :fetch/stock)
(def stock-policy {:cap 64 :ttl (* 7 24 60 60 1000)})

(defn cell-of [pos] (if (vector? pos) pos [(:x pos) (:y pos) (:z pos)]))

(defn cells-of
  "The cells one booked chest answers to: its :pos and, for a double chest, the other half."
  [data]
  (cond-> [(:pos data)] (:also data) (conj (:also data))))

(defn stock-of
  "{[x y z] {name n}}: the latest stock booked per container, from memory view. A double chest is one key (the cell last
  booked); read a cell of either half with stock-at."
  [view]
  (reduce (fn [m e] (assoc m (:pos (:data e)) (:items (:data e)))) {} (mem/entries view stock-kind)))

(defn stock-at
  "What the chest at cell (either half of a double chest) held when last booked, nil when not booked."
  [view cell]
  (->> (mem/entries view stock-kind) (filter #(some #{cell} (cells-of (:data %)))) last :data :items))

(defn age-ms
  "Ms since the chest at cell was last booked (looked into or moved from), nil when not booked."
  [view cell]
  (some->> (mem/entries view stock-kind) (filter #(some #{cell} (cells-of (:data %)))) (map :t) seq (apply max) (- (:now view))))

(defn age-text
  "\"seen 3 h ago\" for an age in ms."
  [ms]
  (let [min (quot ms 60000)]
    (str "seen " (cond (< min 1) "just now" (< min 120) (str min " min ago") :else (str (quot min 60) " h ago")))))

(defn stacks->items
  "{name n} of inspected container items (JS array of {name count}) or cljs maps."
  [stacks]
  (reduce (fn [m s] (let [[n k] (if (map? s) [(:name s) (:count s)] [(.-name s) (.-count s)])]
                      (update m n (fnil + 0) k)))
          {} (if (array? stacks) (array-seq stacks) stacks)))

;; Vanilla ChestBlock.getConnectedDirection: a LEFT half's partner is at its facing's clockwise side, a RIGHT half's at the counter-clockwise side.
(def clockwise {"north" [1 0] "east" [0 1] "south" [-1 0] "west" [0 -1]})

(defn partner-of
  "The cell of the other half of the double chest at cell as the body last saw it (the block's type and facing), nil for a
  single chest or when it was not seen."
  [c cell]
  (when-let [p (:primitives c)]
    (let [{:keys [name properties]} (look/seen-block p (zipmap [:x :y :z] cell))
          [dx dz] (get clockwise (:facing properties))
          sign (case (:type properties) "left" 1 "right" -1 nil)]
      (when (and dx sign (contains? (set container-names) name))
        (let [[x y z] cell
              other [(+ x (* sign dx)) y (+ z (* sign dz))]]
          (when (= name (:name (look/seen-block p (zipmap [:x :y :z] other)))) other))))))

(defn book!
  "Replace the container's entry at cell (and its other half), so the cap counts chests, not transfers and a double chest is
  booked once."
  [c cell items]
  (let [also (partner-of c cell)
        mine (set (cons cell (when also [also])))]
    (ctx/forget-where! c stock-kind #(some mine (cells-of %)))
    (ctx/remember! c stock-kind (cond-> {:pos cell :items items} also (assoc :also also)) stock-policy)))

(defn note-stock!
  "Book what the chest at pos holds now (stacks as inspected)."
  [c pos stacks]
  (book! c (cell-of pos) (stacks->items stacks)))

(defn note-moved!
  "Book delta of name moved into (+) or out of (-) the chest at pos, when its stock is known."
  [c pos name delta]
  (let [cell (cell-of pos)]
    (when-let [items (stock-at (ctx/view c) cell)]
      (let [n (+ (get items name 0) delta)]
        (book! c cell (if (pos? n) (assoc items name n) (dissoc items name)))))))

(defn view-age-ms [] (settings/get settings ::view-age-ms))

(defn seen-chests
  "The containers the body has seen (perception's seenBlocks, never x-ray) within chest-radius, as {:x :y :z},
  nearest first; empty without perception. A cell seen in the last view-age-ms is checked against the live block
  (gone: dropped); an older memory is trusted, a failed open or a later sight corrects it."
  [c]
  (let [p (:primitives c)
        me (u/self-pos c)]
    (->> (look/seen-blocks p {:names container-names :radius (chest-radius) :max 32 :live? true :live-within-ms (view-age-ms)})
         (map :pos)
         (sort-by #(u/dist me %)))))

(defn usable-chests
  "seen-chests whose zone or claim allows :take (an open, unzoned chest does)."
  [c]
  (remove #(access/container-refusal c :take %) (seen-chests c)))
