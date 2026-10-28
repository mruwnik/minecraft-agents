(ns world-test.resight
  "tools/event-counts.mjs resight: counts the re-sightings behind jobs.lib.memory-age's priors. Each body's seen memory
  (engine/seen.bin) holds a cell of an aging kind (memory-age/name-kind: fluid, door, fire) with its seen time; the recorded
  chunk column (worlds/<world>/chunks) holds what the cell held when the column was written, over min-age-s later. Changed:
  a door's state differs (open, shut), a fluid or fire's block (water to kelp, seagrass or a bubble column stays water).
  The fixture grid (x or z from grid-from: its plots, lanes and islands, built and torn down by tests) is left out.
  Rows: [body cx sy cz kind seen-s] -> {:n cells :k changed :age-s}, a row folded once stays (its shortest age).
  report: per kind, each section one unit (its cells weigh 1/n, they change together), the Gamma posterior of the change
  rate under a Jeffreys prior, :a changes over :b-s seconds as memory-age's priors read; the exposure of a changed cell is
  its whole age (the change time is unseen), which at these rates moves nothing."
  (:require [jobs.lib.memory-age :as memory-age]))

(def min-age-s 60)
(def grid-from 19000)
(def kinds {memory-age/FLUID :fluid memory-age/DOOR :door memory-age/FIRE :fire})
(def still-water #{"water" "kelp" "kelp_plant" "seagrass" "tall_seagrass" "bubble_column"})

(defn changed?
  "Whether a cell of kind seen as (seen-name, seen-id) holds (now-name, now-id) changed."
  [kind seen-name seen-id now-name now-id]
  (case kind
    :door (not= seen-id now-id)
    :fluid (if (= "water" seen-name) (not (still-water now-name)) (not= seen-name now-name))
    (not= seen-name now-name)))

(defn rows
  "The rows of body's overworld sections (loadSeen's, JS) by state-name ((fn [id]) -> name) and at ((fn [x y z]) ->
  {:name :id :t} of the chunk record, or nil)."
  [body sections state-name at]
  (let [kind-of (memoize #(kinds (memory-age/name-kind (or (state-name %) ""))))]
    (persistent!
     (reduce
      (fn [m ^js s]
        (if-not (#{"overworld" "minecraft:overworld"} (.-dim s))
          m
          (let [^js ids (.-ids s) ^js times (.-times s)]
            (reduce
             (fn [m i]
               (let [v (aget ids i)
                     id (dec v)
                     kind (when (pos? v) (kind-of id))
                     x (+ (* 16 (.-cx s)) (bit-and i 15))
                     z (+ (* 16 (.-cz s)) (bit-and (bit-shift-right i 4) 15))
                     now (when (and kind (< x grid-from) (< z grid-from)) (at x (+ (* 16 (.-sy s)) (bit-shift-right i 8)) z))
                     seen-s (when now (+ (js/Math.floor (/ (.-base s) 1000)) (aget times i)))
                     age-s (when now (- (/ (:t now) 1000) seen-s))]
                 (if (and age-s (> age-s min-age-s))
                   (let [key [body (.-cx s) (.-sy s) (.-cz s) kind seen-s]
                         r (get m key {:n 0 :k 0 :age-s age-s})]
                     (assoc! m key (-> r (update :n inc)
                                       (update :k + (if (changed? kind (state-name id) id (:name now) (:id now)) 1 0)))))
                   m)))
             m (range 4096)))))
      (transient {}) sections))))

(defn fold
  "counts' :resight rows with rows merged in; a row already there stays."
  [counts rows]
  (update counts :resight #(merge rows %)))

(defn- quantiles
  "The 5%, 50% and 95% points of Gamma(a, b), on a grid of rates."
  [a b]
  (let [rates (mapv #(js/Math.pow 10 (/ % 100)) (range -1400 1))
        ;; density in log-rate: rate^a e^(-b rate)
        lw (mapv #(- (* a (js/Math.log %)) (* b %)) rates)
        top (apply max lw)
        ws (mapv #(js/Math.exp (- % top)) lw)
        total (reduce + ws)
        at (fn [q] (loop [i 0 acc 0] (let [acc (+ acc (/ (nth ws i) total))] (if (or (>= acc q) (= i (dec (count rates)))) (nth rates i) (recur (inc i) acc)))))]
    (mapv at [0.05 0.5 0.95])))

(defn report
  "{kind {:cells :changed :sections :a :b-s :rate-per-s [5% 50% 95%]}} of rows."
  [rows]
  (into (sorted-map)
        (for [[kind rs] (group-by (fn [[[_ _ _ _ kind]]] kind) rows)
              :let [secs (vals (group-by (fn [[[_ cx sy cz]]] [cx sy cz]) rs))
                    unit (fn [f] (reduce + (for [sec secs :let [n (reduce + (map (comp :n val) sec))]]
                                             (/ (reduce + (map (comp f val) sec)) n))))
                    a (+ 0.5 (unit :k))
                    b (unit #(* (:n %) (:age-s %)))]]
          [kind {:cells (reduce + (map (comp :n val) rs)) :changed (reduce + (map (comp :k val) rs)) :sections (count secs)
                 :a a :b-s (js/Math.round b) :rate-per-s (quantiles a b)}])))
