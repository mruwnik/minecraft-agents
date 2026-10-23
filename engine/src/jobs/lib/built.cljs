(ns jobs.lib.built
  "The cells this body placed a block in, in body memory: a pen it fenced by hand is its own whatever the zones say.
  jobs.lib.tidy/place! writes it on every placed block and dig! takes a dug cell out; the scaffold ledger holds only
  temporary blocks, which a cleanup takes back, so a kept build cannot live there. The cells are kept per 16x16 column
  of the world; past max-columns the column touched longest ago goes, whole, so the size of a pen never costs it its verdict.
  A column is touched by a placement, a re-booked cell and a pen read as the body's own (use!)."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]))

(def kind :built-cells)

(def policy {:cap 1 :ttl :forever})

(def max-columns "Most 16x16 columns kept." 256)

(defn column [[x _ z]] [(bit-shift-right x 4) (bit-shift-right z 4)])

(defn add-cell
  "Book data (as stored: {:columns {column #{cell}} :order [column]}) with cell added; its column becomes the newest."
  [data cell]
  (let [col (column cell)
        order (conj (filterv #(not= col %) (:order data)) col)
        columns (update (:columns data) col (fnil conj #{}) cell)
        stale (take (- (count order) max-columns) order)]
    {:columns (apply dissoc columns stale) :order (vec (drop (count stale) order))}))

(defn drop-cell
  "Book data with cell taken out; an emptied column goes."
  [data cell]
  (let [col (column cell)
        left (disj (get-in data [:columns col]) cell)]
    (if (seq left)
      (assoc-in data [:columns col] left)
      {:columns (dissoc (:columns data) col) :order (filterv #(not= col %) (:order data))})))

(defn booked-in? [data cell] (contains? (get-in data [:columns (column cell)]) cell))

(defn refresh
  "Book data with the column of each booked cell in cells made the newest."
  [data cells]
  (reduce (fn [data col] (update data :order #(conj (filterv (complement #{col}) %) col)))
          data
          (distinct (map column (filter #(booked-in? data %) cells)))))

(defn remember-cell
  "Book data with cell added, or its column refreshed when it is booked already."
  [data cell]
  (if (booked-in? data cell) (refresh data [cell]) (add-cell data cell)))

(defn book [view] (:data (mem/latest view kind)))

(defn cells
  "The set of [x y z] cells placed, from a body-memory view {:data :now} (ctx/view in a job)."
  [view]
  (into #{} cat (vals (:columns (book view)))))

(defn booked? [view cell] (booked-in? (book view) cell))

(defn write!
  "Remember data' when it differs from data."
  [c data data']
  (when (not= data data') (ctx/remember! c kind data' policy)))

(defn remember!
  "Add cell [x y z] to the cells placed, from a job's round."
  [c cell]
  (let [data (book (ctx/view c))]
    (write! c data (remember-cell data cell))))

(defn use!
  "Refresh the columns of the booked cells among cells: a pen read as the body's own is in use and must not age out."
  [c cells]
  (let [data (book (ctx/view c))]
    (write! c data (refresh data cells))))

(defn forget!
  "Take cell [x y z], dug out, from the cells placed."
  [c cell]
  (let [view (ctx/view c)]
    (when (booked? view cell)
      (ctx/remember! c kind (drop-cell (book view) cell) policy))))
