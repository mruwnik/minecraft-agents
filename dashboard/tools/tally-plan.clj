;; Plan minus world against a column survey, with plan.shape's own functions, on the JVM:
;;   cd dashboard && clojure -Sdeps '{:paths ["src"]}' -M tools/tally-plan.clj <blueprint-dir> <plan.edn> <survey.edn> ...
;; Prints the plan's answer counts (match missing wrong extra unknown), then each part's counts.
;; A survey file maps [x z] to {:y top-solid :ground :above :state :ground-state :stack [[y name state] ..]}, with a
;; header line ";; survey of <kind> <site>, ... plan origin y <y0>". :stack lists the non-air blocks of a y range that
;; depends on the kind of site (below); inside it a missing y is air. Outside it only :y (ground) and :y + 1 (above)
;; are known, everything else is unseen.
(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[plan.shape :as shape])

(defn file-id [f] (str/replace (.getName (io/file f)) #"\.edn$" ""))

(defn stack-range
  "[lo hi] of the y the survey read into :stack for a column, by the survey's kind and site."
  [{:keys [kind site y0]} {:keys [y]}]
  (cond
    (= kind "blueprint-place") [(- y0 2) (+ y0 14)]
    (= site "treebeard-forest") [(- y 3) (+ y 4)]
    (= kind "old-plan") [(dec y) (+ y 4)]
    (= site "jizo-farm") [(dec y) (+ y 3)]
    :else [y (+ y 24)]))

(defn read-survey [f]
  (let [text (slurp f)
        [_ kind site] (re-find #";; survey of (\S+) ([^,]+)," text)
        y0 (some-> (re-find #"plan origin y (-?\d+)" text) second parse-long)
        header {:kind kind :site site :y0 y0}]
    (into {} (for [[xz col] (edn/read-string text) :when col] [xz (assoc col :range (stack-range header col))]))))

(defn block [name state] (cond-> {:name name} (seq state) (assoc :state state)))

(defn block-at [columns [x y z]]
  (when-let [{:keys [stack range ground ground-state above state] :as col} (get columns [x z])]
    (let [[lo hi] range]
      (cond
        (and stack (<= lo y hi)) (if-let [[_ n s] (first (filter #(= y (first %)) stack))] (block n s) {:name "air"})
        (= y (:y col)) (block ground ground-state)
        (= y (inc (:y col))) (block above state)
        :else nil))))

(def answers [:match :missing :wrong :extra :unknown])

(defn counts [cells] (let [f (frequencies (map :answer cells))] (mapv #(get f % 0) answers)))

(let [[dir plan-file & survey-files] *command-line-args*
      blueprints (into {} (for [f (.listFiles (io/file dir)) :when (str/ends-with? (.getName f) ".edn")]
                            [(file-id f) (edn/read-string (slurp f))]))
      plan (edn/read-string (slurp plan-file))
      columns (apply merge (map read-survey survey-files))
      {:keys [cells errors]} (shape/expand plan blueprints)
      judged (shape/plan-minus-world cells #(block-at columns %))]
  (doseq [e (concat (shape/plan-errors plan (file-id plan-file)) errors)] (println "ERROR" (pr-str e)))
  (println (:id plan) (count judged) "cells" (zipmap answers (counts judged)))
  (doseq [[part cs] (sort-by key (group-by :part judged))]
    (println "  " part (zipmap answers (counts cs))
             "wrong:" (frequencies (keep #(when (= :wrong (:answer %)) (:found %)) cs)))))
