(ns world-test.guess-rock
  "tools/event-counts.mjs guess-rock: counts the measured prior jobs.lib.cost.value/::guess-rock-counts from the recorded
  chunk columns (worlds/<world>/chunks): of the solid cells of a chunk (jobs.lib.escape/solid?), those that are rock an
  escalation never digs (escape/protected?: bedrock and the like) over those that are not, split by whether a face neighbour
  of the cell is such rock (:beside, as go-to's guess class, escalate-choose/beside-undiggable?) or not (:apart). Per 16x16x16
  section the shares are kept so the prior's spread is fitted by moments over sections (a call digs in one place; rock
  never dug comes in pockets). Every stride-th
  column file in name order is read, the fixture plot grid left out. Pure parts: tally, report."
  (:require ["fs" :as fs]
            [jobs.lib.escape :as escape]
            [world-test.field-passes :as field-passes]
            [world-test.water-lags :as water-lags]))

(def bottom-y -64)
(def top-y 319)
(def section "Blocks a section is high." 16)

(defn bad-cell?
  "Whether name is solid rock an escalation never digs."
  [name]
  (boolean (and (escape/solid? name) (escape/protected? name))))

(defn tally
  "{class {:bad n :all n}} of the solid cells of the box x0..x1, y0..y1, z0..z1 (inclusive) by at (fn [x y z] -> name or nil;
  nil: not recorded, its neighbours' reads then count as not rock), class :beside when a face neighbour is rock never dug."
  [at x0 x1 y0 y1 z0 z1]
  (let [rock? (fn [x y z] (bad-cell? (at x y z)))]
    (reduce (fn [m [x y z]]
              (let [n (at x y z)]
                (if-not (escape/solid? n)
                  m
                  (let [k (if (some (fn [[dx dy dz]] (rock? (+ x dx) (+ y dy) (+ z dz)))
                                    [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])
                            :beside :apart)]
                    (-> m (update-in [k :all] (fnil inc 0)) (update-in [k :bad] (fnil + 0) (if (bad-cell? n) 1 0)))))))
            {} (for [x (range x0 (inc x1)) z (range z0 (inc z1)) y (range y0 (inc y1))] [x y z]))))

(defn chunk-tally
  "The tallies of the sections of chunk cx cz by at (fn [x y z] -> {:name ...} or nil)."
  [at cx cz]
  (let [name-at (fn [x y z] (:name (at x y z)))]
    (for [y0 (range bottom-y (inc top-y) section)]
      (tally name-at (* 16 cx) (+ 15 (* 16 cx)) y0 (+ y0 section -1) (* 16 cz) (+ 15 (* 16 cz))))))

(defn report
  "Of section tallies: per class the pooled {:bad :all}, :p-bad pooled, and :prior [bad ok] Beta pseudo-counts fitted by
  moments over the sections with at least min-n cells of it (field-passes/beta-fit), with :n their sum."
  [tallies min-n]
  (into {}
        (for [k [:beside :apart]
              :let [rows (keep #(get % k) tallies)
                    all (reduce + (map #(:all % 0) rows))
                    bad (reduce + (map #(:bad % 0) rows))
                    shares (keep (fn [{:keys [all bad] :or {all 0 bad 0}}] (when (>= all min-n) (/ bad all))) rows)
                    [a b] (if (> (count shares) 1) (field-passes/beta-fit shares bad all) [bad (- all bad)])]]
          [k {:bad bad :all all :sections (count shares)
              :p-bad (/ bad (max 1 all))
              :prior [a b] :n (/ (js/Math.round (* 10 (+ a b))) 10)}])))

(defn count-in
  "report of every stride-th recorded column file in dir, read by at."
  [dir at stride]
  (let [cols (->> (array-seq (fs/readdirSync dir))
                  (keep #(when-let [[_ cx cz] (re-find #"^(-?\d+)\.(-?\d+)\.bin$" %)] [(js/parseInt cx) (js/parseInt cz)]))
                  (remove (fn [[cx cz]] (water-lags/reserved? cx cz)))
                  sort
                  (take-nth stride))]
    {:guesses (report (mapcat (fn [[cx cz]] (chunk-tally at cx cz)) cols) 200) :files (count cols) :stride stride}))
