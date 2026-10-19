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
  [open? spot]
  (loop [seen #{spot} edge [spot] depth 0]
    (if (or (empty? edge) (= depth spread))
      seen
      (let [next (into [] (comp (mapcat (fn [c] (map #(mapv + c %) neighbours)))
                                (filter #(and (not (seen %)) (open? %)))
                                (distinct))
                       edge)]
        (recur (into seen next) next (inc depth))))))

(defn plan
  "Torch spots lighting every dark cell of cells (in the order given):
  {:cells the cells that must not stay dark (spawnable ones) :dark? (fn [cell]) :open? (fn [cell]) light passes
   :spots the cells a torch may hang in, best first :sources torches already hanging}
  -> {:spots [cell ...] :uncovered [cell ...]}. Takes the first dark cell no spot chosen yet lights, then the spot
  lighting it that lights most cells still dark (the earliest listed on a tie): a line gets one torch per 2 x spread
  cells. :uncovered are the cells no spot can reach."
  [{:keys [cells dark? open? spots sources]}]
  (let [reach (memoize #(lit-by open? %))
        lit (into #{} (mapcat reach) sources)]
    (loop [todo (vec (remove lit (filter dark? cells))) chosen [] gone []]
      (if (empty? todo)
        {:spots chosen :uncovered gone}
        (let [c (first todo)
              todo-set (set todo)
              score #(count (filter todo-set (reach %)))
              best (reduce (fn [b s] (if (> (score s) (score b)) s b))
                           (filter #((reach %) c) spots))]
          (if (nil? best)
            (recur (subvec todo 1) chosen (conj gone c))
            (recur (vec (remove (reach best) todo)) (conj chosen best) gone)))))))
