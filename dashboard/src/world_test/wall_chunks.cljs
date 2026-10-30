(ns world-test.wall-chunks
  "tools/event-counts.mjs wall-chunks: counts the measured prior behind jobs.lib.cost.look ::wall-prior from the recorded
  chunk columns (worlds/<world>/chunks): the roofed spaces a body can walk (cells with air at feet and head, a solid floor and a
  block built of (built?) 2..6 above: reach.room's flood, same level, 4 directions, at most room-cells), and of each one's side cells
  (the feet cells beside it not in it) how many are closed (a solid block at feet or head) and how many open (passable: the
  space ends where the roof or floor does; a door or gate is a block, so closed). A space of the cap or more, or with a side not
  recorded, is left out. Each space is one row (a call looks at one room). The prior is fitted over the rows under open sky
  (nothing above the roof but built blocks and leaves: a mineshaft, stronghold or trial chamber lies under rock) whose roof is
  not a natural structure's material (natural-roof?). Every stride-th column file in name order is read, the fixture plot grid
  left out; a space is counted by the chunk of its lowest-x, lowest-z cell at its lowest y. Pure parts: spaces, report."
  (:require ["fs" :as fs]
            [clojure.string :as str]
            [jobs.lib.cost.look :as look]
            [jobs.lib.escape :as escape]
            [world-test.water-lags :as water-lags]
            [world-test.way-prior :as way-prior]))

(def bottom-y -64)
(def top-y 319)
(def cap "Cells a space may hold before it is a cave, not a room (reach.room/room-cells)." 1024)
(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(defn built?
  "Whether the block name is one a roof is built of (a room someone made, not a tree's leaves or a cave's rock)."
  [name]
  (boolean (and name (re-find #"planks|slab|brick|glass|wool|terracotta|concrete|cobblestone|thatch" name))))

(def natural-roof?
  "Whether a roof block is one only a natural structure has: trial chambers (tuff, copper), ancient cities (deepslate), strongholds and
  ruins (infested, cracked, mossy, chiseled, stone bricks), monuments (prismarine), mansions (dark oak)."
  (comp boolean #(and % (re-find #"tuff|deepslate|prismarine|copper|infested|mossy|cracked|chiseled|stone_brick|dark_oak" %))))

(defn sky-above?
  "Whether nothing above the feet cell x y z but built blocks and leaves stands over the roof (y + 2 up)."
  [name-at x y z]
  (every? #(let [n (name-at x % z)] (or (not (escape/solid? n)) (built? n) (str/ends-with? n "_leaves")))
          (range (+ y 2) (inc top-y))))

(defn roof-of
  "The name of the first built block 2..6 above the feet cell x y z, or nil."
  [name-at x y z]
  (first (filter built? (map #(name-at x (+ y %) z) (range 2 7)))))

(defn standable
  "Whether the feet cell x y z is one a walker stands in under a roof: :yes, :no, or :unknown when a cell is not recorded."
  [name-at x y z]
  (let [floor (name-at x (dec y) z) feet (name-at x y z) head (name-at x (inc y) z)]
    (cond (not (and floor feet head)) :unknown
          (and (escape/solid? floor) (not (escape/solid? feet)) (not (escape/solid? head))
               (roof-of name-at x y z)) :yes
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
                   (conj rows (assoc (select-keys f [:closed :open]) :roof (apply roof-of name-at c) :sky (apply sky-above? name-at c)))
                   rows))))
      rows)))

(defn all-closed
  "The chance all l sides of a space are closed under the fit (jobs.lib.cost.look/chance-closed from none seen)."
  [a b w l]
  (look/chance-closed {:k a :n (+ a b) :sealed w} 0 l))

(defn log-likelihood
  "Of rows {:closed :open} given w (the share of spaces sealed), Beta(a, b) among the rest: a sealed space is all closed, any other
  has each side closed by the beta-binomial."
  [rows w a b]
  (reduce + (for [[[c o] times] (frequencies (map (juxt :closed :open) rows))
                  :let [t (- (+ (way-prior/ln-gamma (+ a c)) (way-prior/ln-gamma (+ b o)) (way-prior/ln-gamma (+ a b)))
                             (+ (way-prior/ln-gamma a) (way-prior/ln-gamma b) (way-prior/ln-gamma (+ a b c o))))]]
              (* times (if (zero? o) (js/Math.log (+ w (* (- 1 w) (js/Math.exp t)))) (+ (js/Math.log (- 1 w)) t))))))

(defn fit
  "{:w :a :b} maximising log-likelihood over rows, by a grid over w (0 to 0.5), the mean and ln of a + b; with w 0 allowed, so
  the plain beta-binomial is among the candidates."
  [rows]
  (let [cand (for [w (map #(/ % 40) (range 0 21)) m (map #(/ % 40) (range 1 40)) u (map #(/ % 6) (range -12 36))
                   :let [s (js/Math.exp u)]]
               {:w w :a (* m s) :b (* (- 1 m) s)})]
    (apply max-key #(log-likelihood rows (:w %) (:a %) (:b %)) cand)))

(defn round3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn report
  "Of rows {:closed :open :roof :sky}: the spaces, sides :closed and :open pooled over all, and over the population (:sky rows with
  a roof not natural-roof?) :setting {:k :n :sealed} for ::wall-prior (the fit over the spaces with at least min-n sides, as k/n
  the mean of the Beta times its a + b), :by-size {sides [spaces sealed predicted]} (predicted: spaces x all-closed under the
  fit, to compare) and :pooled {:spaces :sealed :closed :open}; :by-roof {roof-name [spaces sealed closed open sky-spaces]} over
  all rows."
  [rows min-n]
  (let [pop (filter #(and (:sky %) (not (natural-roof? (:roof %)))) rows)
        sized (filter #(>= (+ (:closed %) (:open %)) min-n) pop)
        {:keys [w a b]} (if (> (count sized) 1) (fit sized) {:w 0 :a (reduce + (map :closed pop)) :b (reduce + (map :open pop))})
        sealed #(count (filter (comp zero? :open) %))
        sum #(reduce + (map % %2))]
    {:setting {:k (round3 a) :n (round3 (+ a b)) :sealed w}
     :pooled {:spaces (count pop) :sealed (sealed pop) :closed (sum :closed pop) :open (sum :open pop)}
     :by-size (into (sorted-map)
                    (map (fn [[l rs]] [l [(count rs) (sealed rs) (round3 (* (count rs) (all-closed a b w l)))]]))
                    (group-by #(+ (:closed %) (:open %)) sized))
     :by-roof (into (sorted-map)
                    (map (fn [[r rs]] [r [(count rs) (sealed rs) (sum :closed rs) (sum :open rs) (count (filter :sky rs))]]))
                    (group-by :roof rows))
     :spaces (count rows) :all {:sealed (sealed rows) :closed (sum :closed rows) :open (sum :open rows)}}))

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
