(ns world-test.dark-reach
  "tools/event-counts.mjs dark-reach: counts the measured prior jobs.lib.cost.value/::unseen-edge-dark-counts from the
  recorded chunk columns (worlds/<world>/chunks): from a dark stand (a floor-neighbours floor, feet and head open, block
  light 0: dark at midnight), whether a walker reaches it from a dark stand farther than 24 blocks (the spawn-free
  radius), walking the true blocks (a step level, one up with room over the head, or a fall of up to 3). :far when one
  does, :near when the stands that walk to it are all within 24 or lit. By the stand's own sky light (:sky over 0,
  :roofed 0). A stand whose walk meets a column never recorded is left out. Per chunk the shares are kept so the prior's
  spread is fitted by moments over chunks. Stands of columns 4 and 12 (x and z) of every stride-th column file in name
  order, the fixture plot grid left out. Pure parts: reach, tally, report."
  (:require ["fs" :as fs]
            [world-test.field-passes :as field-passes]
            [world-test.floor-neighbours :as fnb]
            [world-test.water-lags :as water-lags]))

(def radius 24)

(defn reach
  ":far, :near or nil (met a cell never recorded) of the stand s [x y z] read by at (fn [x y z] -> {:name :block} or nil):
  breadth first over the stands a walker steps to s from, ending at the first dark one farther than radius."
  [at [sx sy sz]]
  (let [name-at (memoize (fn [x y z] (if-let [c (at x y z)] (:name c) ::unknown)))
        known? (fn [x y z] (not= ::unknown (name-at x y z)))
        open? (fn [x y z] (fnb/open? (name-at x y z)))
        stand? (fn [x y z] (and (fnb/floor? (name-at x (dec y) z)) (open? x y z) (open? x (inc y) z)))
        far? (fn [x y z] (> (+ (* (- x sx) (- x sx)) (* (- y sy) (- y sy)) (* (- z sz) (- z sz))) (* radius radius)))
        dark? (fn [x y z] (zero? (or (:block (at x y z)) 0)))
        ;; the stands t beside c a walker at t steps from to c: level, one lower (a jump: room over t's head), or 1-3 higher
        ;; (a fall down c's open column)
        froms (fn [[cx cy cz]]
                (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                      :let [tx (+ cx dx) tz (+ cz dz)]
                      ty [(dec cy) cy (inc cy) (+ cy 2) (+ cy 3)]
                      :when (and (stand? tx ty tz)
                                 (cond (= ty (dec cy)) (open? tx (+ ty 2) tz)
                                       (> ty cy) (every? #(open? cx % cz) (range (+ cy 2) (+ ty 2)))
                                       :else true))]
                  [tx ty tz]))]
    (loop [queue #queue [[sx sy sz]] seen #{[sx sy sz]}]
      (if-let [[x y z :as c] (peek queue)]
        (cond
          (not-every? #(apply known? %) (for [[dx dz] [[0 0] [1 0] [-1 0] [0 1] [0 -1]]] [(+ x dx) y (+ z dz)])) nil
          (and (far? x y z) (dark? x y z)) :far
          :else (let [ns (remove seen (froms c))]
                  (recur (into (pop queue) ns) (into seen ns))))
        :near))))

(defn tally
  "{kind {:far n :near n}} of the dark stands of the columns xzs ([x z]) read by at, kind by the stand's sky light."
  [at xzs]
  (reduce (fn [m [x z]]
            (reduce (fn [m y]
                      (let [c (at x y z)]
                        (if-not (and c (zero? (:block c)) (fnb/floor? (:name (at x (dec y) z))) (fnb/open? (:name c)) (fnb/open? (:name (at x (inc y) z))))
                          m
                          (if-let [r (reach at [x y z])]
                            (update-in m [(if (pos? (or (:sky c) 0)) :sky :roofed) r] (fnil inc 0))
                            m))))
                    m (range fnb/bottom-y fnb/top-y)))
          {} xzs))

(defn report
  "Of chunk tallies: per kind the pooled {:far :near}, :p-far pooled, and :prior [far near] Beta pseudo-counts fitted by
  moments over the chunks with at least min-n stands of it (field-passes/beta-fit), with :n their sum."
  [tallies min-n]
  (into {}
        (for [k (distinct (mapcat keys tallies))
              :let [rows (keep #(get % k) tallies)
                    far (reduce + (map #(:far % 0) rows))
                    near (reduce + (map #(:near % 0) rows))
                    shares (keep (fn [{:keys [far near] :or {far 0 near 0}}]
                                   (when (>= (+ far near) min-n) (/ far (+ far near))))
                                 rows)
                    [a b] (if (> (count shares) 1) (field-passes/beta-fit shares far (+ far near)) [far near])]]
          [k {:far far :near near :chunks (count shares)
              :p-far (/ (js/Math.round (* 1000 (/ far (max 1 (+ far near))))) 1000)
              :prior [a b] :n (/ (js/Math.round (* 10 (+ a b))) 10)}])))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    {:stands (report (map (fn [[cx cz]] (tally at (for [x [4 12] z [4 12]] [(+ (* 16 cx) x) (+ (* 16 cz) z)]))) cols) 5)
     :files (count cols) :stride stride}))
