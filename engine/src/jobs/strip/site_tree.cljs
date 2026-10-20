(ns jobs.strip.site-tree
  "Pure reading of a strip site's segment tree for jobs.strip.site: which segment to seal or strip next, and where its mouth is."
  (:require [engine.args :as a]
            [jobs.lib.strip :as strip]))

(def settled #{:stripped :sealed :skipped})


(defn vec3 [v] (let [{:keys [x y z]} (a/cell v)] [x y z]))

(defn counts [s] (frequencies (map :state (vals (:segments s)))))

(defn tree [s] (strip/order (vals (:segments s)) (vec3 (:at s))))

(defn beyond [t id] (mapcat (fn [k] (cons k (beyond t k))) (get-in t [:children id])))

(defn seal-ready
  "The segment to seal next, deepest first: stripped, not the root, everything beyond it settled."
  [s t]
  (let [state #(get-in s [:segments % :state])]
    (->> (reverse (:order t))
         (filter #(and (= :stripped (state %)) (not= % (:root t)) (every? settled (map state (beyond t %)))))
         first)))

(defn next-open
  "The next segment to strip: open, its parent settled, one with an unseen end first."
  [s t]
  (let [open (filter #(and (= :open (get-in s [:segments % :state]))
                           (settled (get-in s [:segments (get-in t [:parent %]) :state] :stripped)))
                     (:order t))]
    (get (:segments s) (or (first (filter #(seq (get-in s [:segments % :open-ends])) open)) (first open)))))

(defn unlinked
  "Open segments the tree does not reach: no stripped segment touches them, so they are passed on, not stripped."
  [s t]
  (filter #(= :open (get-in s [:segments % :state])) (:unreached t)))

(defn mouth
  "{:at :side :entry} of seg on the side of its parent: the cell one step inside the segment from where the parent meets it."
  [s t seg]
  (let [parent (get (:segments s) (get-in t [:parent (:id seg)]))
        {:keys [axis y line from to]} seg
        j (cond (nil? parent) from
                (= axis (:axis parent)) (if (<= (:to parent) from) from to)
                :else (:line parent))
        side (if (<= j from) :from :to)]
    {:side side :at (strip/at axis y line (if (= side :from) (inc j) (dec j))) :entry (if parent (strip/at axis y line j) (vec3 (:at s)))}))
