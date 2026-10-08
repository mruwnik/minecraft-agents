(ns jobs.lib.spent
  "What a job placed, so its result and done event can say what it used and where. jobs.lib.tidy/place! calls note!
  on every placed block; the job reads log from its memory and puts summary and text in its result and event.
  The log is bounded (counts and a few cells), as job memory persists."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]))

(def max-cells "Cells named in a text before it ends with the last one." 5)

(defn cell-vec [cell] (if (map? cell) [(:x cell) (:y cell) (:z cell)] (vec cell)))

(defn add
  "Log (nil or {:counts {item n} :first [cell ...] :last cell}) with item placed at cell. Bounded: item counts, the
  first max-cells - 1 cells and the last one, however many blocks were placed."
  [log item cell]
  (let [cell (cell-vec cell) log (or log {:counts {} :first []})]
    (cond-> (-> log (update-in [:counts item] (fnil inc 0)) (assoc :last cell))
      (< (count (:first log)) (dec max-cells)) (update :first conj cell))))

(defn note!
  "Job memory: item was placed at cell (a map with :x :y :z or a vector)."
  [c item cell]
  (ctx/update-mem! c update :placed-log add item cell))

(defn absorb
  "Log with a child's result (or event) added: its :spent counts and :spent-at cells. A child that placed nothing adds nothing."
  [log child-result]
  (let [counts (:spent child-result) cells (:spent-at child-result)]
    (if-not (seq counts)
      log
      (let [log (or log {:counts {} :first []})
            room (- (dec max-cells) (count (:first log)))]
        (cond-> (-> log (update :counts #(merge-with + % counts)) (assoc :last (or (last cells) (:last log))))
          (pos? room) (update :first into (take room cells)))))))

(defn absorb!
  "Job memory: add the child's :spent and :spent-at to this job's log (absorb), so its done text sums what its children
  placed."
  [c child-result]
  (ctx/update-mem! c update :placed-log absorb child-result))

(defn log "Placed so far in this job's memory, nil when nothing." [c] (:placed-log (ctx/mem c)))

(defn cells "The cells a log names: all when at most max-cells blocks, else the first ones then the last." [log]
  (let [n (reduce + (vals (:counts log)))
        firsts (:first log)]
    (if (and (> n (count firsts)) (:last log)) (conj firsts (:last log)) firsts)))

(defn summary
  "{:spent {item count} :spent-at [cell ...]} of a log (cells: the first ones and the last)."
  [log]
  {:spent (:counts log) :spent-at (cells log)})

(defn text
  "\"used 7 oak_log (what at x y z ...)\" for a log, nil when nothing was placed."
  [log what]
  (when (seq (:counts log))
    (let [n (reduce + (vals (:counts log)))
          shown (let [cs (cells log)] (if (> n max-cells) (conj (vec (butlast cs)) '... (last cs)) cs))
          counts (sort-by (juxt (comp - val) key) (:counts log))]
      (str "used " (str/join ", " (map (fn [[item k]] (str k " " item)) counts))
           " (" what " at " (str/join " " (map #(if (= '... %) "..." (pr-str %)) shown)) ")"))))
