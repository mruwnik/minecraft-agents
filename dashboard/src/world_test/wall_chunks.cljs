(ns world-test.wall-chunks
  "tools/event-counts.mjs wall-chunks: counts the measured prior behind jobs.lib.cost.look ::wall-prior from the recorded
  chunk columns (worlds/<world>/chunks): the roofed spaces a body can walk (cells with air at feet and head, a solid floor and a
  block built of (built?) 2..6 above: reach.room's flood, same level, 4 directions, at most room-cells), and of each one's side cells
  (the feet cells beside it not in it) how many are closed (a solid block at feet or head) and how many open (passable: the
  space ends where the roof or floor does). A space of the cap or more, or with a side not recorded, is left out. Each space is
  one row, so the prior's spread is fitted by moments over spaces (a call looks at one room). Every stride-th column file in name
  order is read, the fixture plot grid left out; a space is counted by the chunk of its lowest-x, lowest-z cell at its lowest y.
  Pure parts: spaces, report."
  (:require ["fs" :as fs]
            [jobs.lib.escape :as escape]
            [world-test.field-passes :as field-passes]
            [world-test.water-lags :as water-lags]))

(def bottom-y -64)
(def top-y 319)
(def cap "Cells a space may hold before it is a cave, not a room (reach.room/room-cells)." 1024)
(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(defn built?
  "Whether the block name is one a roof is built of (a room someone made, not a tree's leaves or a cave's rock)."
  [name]
  (boolean (and name (re-find #"planks|slab|brick|glass|wool|terracotta|concrete|cobblestone|thatch" name))))

(defn standable
  "Whether the feet cell x y z is one a walker stands in under a roof: :yes, :no, or :unknown when a cell is not recorded."
  [name-at x y z]
  (let [floor (name-at x (dec y) z) feet (name-at x y z) head (name-at x (inc y) z)]
    (cond (not (and floor feet head)) :unknown
          (and (escape/solid? floor) (not (escape/solid? feet)) (not (escape/solid? head))
               (some #(built? (name-at x (+ y %) z)) (range 2 7))) :yes
          :else :no)))

(defn closed?
  "Whether the feet cell x y z blocks a walker (nil: not recorded)."
  [name-at x y z]
  (let [feet (name-at x y z) head (name-at x (inc y) z)]
    (cond (or (escape/solid? feet) (escape/solid? head)) true
          (and feet head) false)))

(defn flood
  "{:cells set :closed n :open n} or nil (cap reached, or a side not recorded) of the space round [x y z]."
  [name-at start]
  (loop [todo [start] seen #{start} closed #{} open #{}]
    (cond
      (>= (count seen) cap) nil
      (empty? todo) {:cells seen :closed (count closed) :open (count open)}
      :else
      (let [[x y z] (peek todo)
            todo (pop todo)
            next (for [[dx dz] sides :let [c [(+ x dx) y (+ z dz)]] :when (not (seen c))] c)
            step (fn [acc c] (let [[cx cy cz] c]
                               (if (= :yes (standable name-at cx cy cz))
                                 (update acc 0 conj c)
                                 (let [k (closed? name-at cx cy cz)]
                                   (cond (nil? k) (reduced nil)
                                         k (update acc 1 conj c)
                                         :else (update acc 2 conj c))))))
            res (reduce step [[] closed open] next)]
        (when res
          (let [[new closed open] res]
            (recur (into todo new) (into seen new) closed open)))))))

(defn spaces
  "Rows {:closed n :open n} of the spaces in the box x0..x1, y0..y1, z0..z1 (inclusive) whose lowest cell (y, then x, then z) lies in it."
  [name-at x0 x1 y0 y1 z0 z1]
  (loop [cells (for [y (range y0 (inc y1)) x (range x0 (inc x1)) z (range z0 (inc z1))] [x y z])
         done #{} rows []]
    (if-let [[c & more] (seq cells)]
      (if (or (done c) (not= :yes (apply standable name-at c)))
        (recur more done rows)
        (let [f (flood name-at c)
              cs (:cells f (hash-set c))
              low (first (sort (map (fn [[x y z]] [y x z]) cs)))]
          (recur more (into done cs)
                 (if (and f (= low (let [[x y z] c] [y x z])) (> (count cs) 1))
                   (conj rows (select-keys f [:closed :open]))
                   rows))))
      rows)))

(defn report
  "Of rows: the spaces, sides :closed and :open pooled, and :setting {:k :n} for ::wall-prior: a Beta fitted by moments over the
  spaces with at least min-n sides (field-passes/beta-fit), n their sum."
  [rows min-n]
  (let [k (reduce + (map :closed rows))
        open (reduce + (map :open rows))
        sized (filter #(>= (+ (:closed %) (:open %)) min-n) rows)
        shares (map #(/ (:closed %) (+ (:closed %) (:open %))) sized)
        [a b] (if (> (count shares) 1) (field-passes/beta-fit shares k (+ k open)) [k open])]
    {:sealed (count (filter (comp zero? :open) rows)) :rows (frequencies (map (juxt :closed :open) rows))
     :spaces (count rows) :closed k :open open :p-closed (/ k (max 1 (+ k open)))
     :setting {:k (/ (js/Math.round (* 1000 a)) 1000) :n (/ (js/Math.round (* 1000 (+ a b))) 1000)}}))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at (fn [x y z] -> {:name ...} or nil)."
  [dir at stride]
  (let [name-at (fn [x y z] (:name (at x y z)))
        cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    (assoc (report (mapcat (fn [[cx cz]] (spaces name-at (* 16 cx) (+ 15 (* 16 cx)) bottom-y top-y (* 16 cz) (+ 15 (* 16 cz)))) cols) 4)
           :files (count cols) :stride stride)))
