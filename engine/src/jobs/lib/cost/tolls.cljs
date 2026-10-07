(ns jobs.lib.cost.tolls
  "The price of cells a walk would rather not cross, for go-to's :tolls arg (the planner's options.tolls): a farm's planted
  cells, the cells of a zone that is not the body's. The planner knows no zones or crops: the job that does names the cells.")

(def farm-cell-factor
  "Times its own seconds a planted cell costs more to cross: crossed only when the way round is over this many times longer."
  20)

(def zone-factor
  "Times its own seconds a cell of another's zone costs more to cross: a last resort, not a ban."
  10)

(defn tolls
  "The :tolls arg of go-to for cells ([x y z] each, feet cells) each costing factor times its own seconds more."
  [cells factor]
  (mapv (fn [[x y z]] {:x x :y y :z z :factor factor}) cells))

(defn farm-tolls
  "tolls for the planted cells of a farm."
  [cells]
  (tolls cells farm-cell-factor))

(defn zone-tolls
  "tolls for the cells of zones the body has no business in."
  [cells]
  (tolls cells zone-factor))
