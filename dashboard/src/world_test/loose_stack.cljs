(ns world-test.loose-stack
  "tools/event-counts.mjs loose-stack: counts the measured priors jobs.lib.cost.value/::loose-over-loose-counts (:loose) and
  ::roof-fall-shares (:roofs, tally-roofs) from the
  recorded chunk columns (worlds/<world>/chunks): for each sand, red sand or gravel cell, the cell above it unless that
  is air or a fluid (an unseen cell is neither), is it loose ground too; by the height of the run of loose cells ending at
  the cell, capped at jobs.lib.cost.value/loose-stack-cap. Shares are kept per chunk column so the prior's spread is
  fitted by moments (a call digs in one place). Every stride-th column file in name order is read, the fixture plot grid
  left out. Pure parts: tally, tally-roofs, report."
  (:require ["fs" :as fs]
            [jobs.lib.cost.value :as value]
            [jobs.lib.dig-look :as dig-look]
            [world-test.field-passes :as field-passes]
            [world-test.water-lags :as water-lags]))

(def bottom-y -64)
(def top-y 319)
(def open-above? "Names of cells that are not an unseen cell's possible neighbour." #{"air" "cave_air" "void_air" "water" "lava"})

(defn tally
  "{class {:loose n :all n}} of the loose cells of the box x0..x1, z0..z1 (all y) by at (fn [x y z] -> name or nil)."
  [at x0 x1 z0 z1]
  (reduce (fn [m [x z]]
            (loop [y bottom-y run 0 m m]
              (if (> y top-y)
                m
                (let [n (at x y z)]
                  (if-not (dig-look/loose-ground n)
                    (recur (inc y) 0 m)
                    (let [h (inc run)
                          above (at x (inc y) z)
                          k (keyword (str "h" (min h value/loose-stack-cap)))]
                      (recur (inc y) h
                             (if (or (nil? above) (open-above? above))
                               m
                               (-> m (update-in [k :all] (fnil inc 0))
                                   (update-in [k :loose] (fnil + 0) (if (dig-look/loose-ground above) 1 0)))))))))))
          {} (for [x (range x0 (inc x1)) z (range z0 (inc z1))] [x z])))

(defn tally-roofs
  "{class {:loose n :all n}} of the cells that are neither open (open-above?) nor loose ground, a roof a cut opens, of the
  box x0..x1, z0..z1 by at, with the cell above neither: is that cell loose ground (jobs.lib.cost.value/::roof-fall-shares).
  Class :w<d> when the nearest open cell over the roof is water d cells up, d at most value/roof-fall-water-d, else :base."
  [at x0 x1 z0 z1]
  (reduce (fn [m [x z]]
            (loop [y (dec top-y) above (at x top-y z) open nil open-y top-y m m]
              (if (< y bottom-y)
                m
                (let [n (at x y z)
                      open? (or (nil? above) (open-above? above))
                      open (if open? above open)
                      open-y (if open? (inc y) open-y)
                      roof? (not (or (nil? n) (nil? above) (open-above? n) (open-above? above) (dig-look/loose-ground n)))
                      d (- open-y y)
                      k (if (and (= "water" open) (<= d (value/roof-fall-water-d))) (keyword (str "w" d)) :base)]
                  (recur (dec y) n open open-y
                         (if-not roof?
                           m
                           (-> m (update-in [k :all] (fnil inc 0))
                               (update-in [k :loose] (fnil + 0) (if (dig-look/loose-ground above) 1 0)))))))))
          {} (for [x (range x0 (inc x1)) z (range z0 (inc z1))] [x z])))

(defn report
  "Of column tallies: per class the pooled {:loose :all}, :p-loose pooled, and :prior [loose other] Beta pseudo-counts
  fitted by moments over the columns with at least min-n pairs of it (field-passes/beta-fit)."
  [tallies min-n ks]
  (into {}
        (for [k ks
              :let [rows (keep #(get % k) tallies)
                    all (reduce + (map #(:all % 0) rows))
                    loose (reduce + (map #(:loose % 0) rows))
                    shares (keep (fn [{:keys [all loose] :or {all 0 loose 0}}] (when (>= all min-n) (/ loose all))) rows)
                    [a b] (if (> (count shares) 1) (field-passes/beta-fit shares loose all) [loose (- all loose)])]]
          [k {:loose loose :all all :columns (count shares)
              :p-loose (/ loose (max 1 all))
              :prior [a b]}])))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at (fn [x y z] -> {:name ...} or nil)."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))
        name-at (fn [x y z] (:name (at x y z)))
        box (fn [f] (map (fn [[cx cz]] (f name-at (* 16 cx) (+ 15 (* 16 cx)) (* 16 cz) (+ 15 (* 16 cz)))) cols))]
    {:loose (report (box tally) 20 (map #(keyword (str "h" %)) (range 1 (inc value/loose-stack-cap))))
     :roofs (report (box tally-roofs) 20 (cons :base (map #(keyword (str "w" %)) (range 2 (inc (value/roof-fall-water-d))))))
     :files (count cols) :stride stride}))
