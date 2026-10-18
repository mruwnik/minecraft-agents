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
(def stock-policy {:cap 64 :ttl (* 6 60 60 1000)})

(defn cell-of [pos] (if (vector? pos) pos [(:x pos) (:y pos) (:z pos)]))

(defn stock-of
  "{[x y z] {name n}}: the latest stock booked per chest, from memory view."
  [view]
  (reduce (fn [m e] (assoc m (:pos (:data e)) (:items (:data e)))) {} (mem/entries view stock-kind)))

(defn stacks->items
  "{name n} of inspected container items (JS array of {name count}) or cljs maps."
  [stacks]
  (reduce (fn [m s] (let [[n k] (if (map? s) [(:name s) (:count s)] [(.-name s) (.-count s)])]
                      (update m n (fnil + 0) k)))
          {} (if (array? stacks) (array-seq stacks) stacks)))

(defn note-stock!
  "Book what the chest at pos holds now (stacks as inspected)."
  [c pos stacks]
  (ctx/remember! c stock-kind {:pos (cell-of pos) :items (stacks->items stacks)} stock-policy))

(defn note-moved!
  "Book delta of name moved into (+) or out of (-) the chest at pos, when its stock is known."
  [c pos name delta]
  (let [cell (cell-of pos)]
    (when-let [items (get (stock-of (ctx/view c)) cell)]
      (let [n (+ (get items name 0) delta)]
        (ctx/remember! c stock-kind {:pos cell :items (if (pos? n) (assoc items name n) (dissoc items name))} stock-policy)))))

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
