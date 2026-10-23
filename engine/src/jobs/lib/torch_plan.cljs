(ns jobs.lib.torch-plan
  "Where to hang torches so no spawnable cell of an area stays dark: the fewest spots, from light's spread. Pure: the
  caller says which cells matter, which are dark, which let light through and where a torch may hang
  (jobs.lib.torch/perceived-dark? reads darkness from the body's perception).")

(def spread
  "Steps a torch's light travels through open cells and still leaves block light 1 (14 at the torch, one less a step):
  from 1 up a cell is no spawn ground."
  13)

(def neighbours [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn lit-by
  "The set of cells a torch hung at spot lights: those within spread steps of it through cells open? lets light through."
  ([open? spot] (lit-by open? spot spread))
  ([open? spot reach]
  (loop [seen #{spot} edge [spot] depth 0]
    (if (or (empty? edge) (= depth reach))
      seen
      (let [next (into [] (comp (mapcat (fn [c] (map #(mapv + c %) neighbours)))
                                (filter #(and (not (seen %)) (open? %)))
                                (distinct))
                       edge)]
        (recur (into seen next) next (inc depth)))))))

(def y-step 1048576)
(def x-step 4294967296)
(def key-steps #js [x-step (- x-step) y-step (- y-step) 1 -1])

(defn spread-keys
  "lit-by over cells as keys (one number each, so a spread is no vector arithmetic): the js/Set of the keys spread steps reach from k."
  [open-key? k reach]
  (let [seen (js/Set. #js [k])]
    (loop [edge #js [k] depth 0]
      (if (or (zero? (alength edge)) (== depth reach))
        seen
        (let [next #js []]
          (dotimes [i (alength edge)]
            (let [e (aget edge i)]
              (dotimes [j 6]
                (let [n (+ e (aget key-steps j))]
                  (when (and (not (.has seen n)) (open-key? n))
                    (.add seen n)
                    (.push next n))))))
          (recur next (inc depth)))))))

(defn plan
  "Torch spots lighting every dark cell of cells (in the order given):
  {:cells the cells that must not stay dark (spawnable ones) :dark? (fn [cell]) :open? (fn [cell]) light passes
   :spots the cells a torch may hang in, best first :sources torches already hanging :spread steps a torch lights (default spread)}
  -> {:spots [cell ...] :uncovered [cell ...]}. Takes the first dark cell no spot chosen yet lights, then the spot
  lighting it (lit-by) that lights most cells still dark (the earliest listed on a tie): a line gets one torch per 2 x spread
  cells. :uncovered are the cells no spot can reach. open? is asked once per cell."
  [{:keys [cells dark? open? spots sources] reach-steps :spread :or {reach-steps spread}}]
  (let [all (concat cells spots sources)
        lo (when (seq all) (mapv (fn [i] (- (apply min (map #(nth % i) all)) reach-steps 1)) [0 1 2]))
        key-of (fn [[x y z]] (+ (* (- x (lo 0)) x-step) (* (- y (lo 1)) y-step) (- z (lo 2))))
        cell-of (fn [k] [(+ (lo 0) (js/Math.floor (/ k x-step))) (+ (lo 1) (js/Math.floor (/ (mod k x-step) y-step))) (+ (lo 2) (mod k y-step))])
        opens (js/Map.)
        open-key? (fn [k] (let [o (.get opens k)]
                            (if (some? o) o (let [o (boolean (open? (cell-of k)))] (.set opens k o) o))))
        reaches (js/Map.)
        reach (fn [cell] (let [k (key-of cell)]
                           (or (.get reaches k) (let [r (spread-keys open-key? k reach-steps)] (.set reaches k r) r))))
        lit (js/Set.)
        _ (doseq [s sources] (.forEach (reach s) #(.add lit %)))
        near? (fn [a b] (<= (reduce + (map #(js/Math.abs (- %1 %2)) a b)) reach-steps))]
    (loop [todo (filterv #(and (dark? %) (not (.has lit (key-of %)))) cells) chosen [] gone []]
      (if (empty? todo)
        {:spots chosen :uncovered gone}
        (let [c (first todo)
              ck (key-of c)
              todo-keys (js/Set. (into-array (map key-of todo)))
              score (fn [s] (let [r (reach s) n (volatile! 0)] (.forEach todo-keys #(when (.has r %) (vswap! n inc))) @n))
              best (first (reduce (fn [[_ bs :as b] s] (let [ss (score s)] (if (> ss bs) [s ss] b)))
                                  [nil -1]
                                  (filter #(and (near? % c) (.has (reach %) ck)) spots)))]
          (if (nil? best)
            (recur (subvec todo 1) chosen (conj gone c))
            (let [rb (reach best)]
              (recur (filterv #(not (.has rb (key-of %))) todo) (conj chosen best) gone))))))))
