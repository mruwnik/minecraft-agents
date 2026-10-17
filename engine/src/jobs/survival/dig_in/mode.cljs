(ns jobs.survival.dig-in.mode
  "dig-in's choice of mode (plug, walls or pit), once per call, and the rounds of a site that has no mode to run."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.choose :as choose]
            [jobs.lib.tools :as tools]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]
            [jobs.lib.shelter :as sh]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-in.site :as site]
            [jobs.survival.retreat-pit :as pit]))

(defn blocks
  "The blocks it may place: the call's :use :blocks, else the shelter fillers (not a list frozen at the start: a pit
  places the dirt it dug)."
  [c]
  (let [used (ctx/use c :blocks)]
    (if (seq used) used (choose/fillers :shelter))))

(def place-s
  "Seconds one wall block takes to place. The dig back at dawn is daylight work, so only the night's seconds are priced."
  0.5)

(defn recoverable-walls-s
  "Night seconds to wall in cells with a carried block worth more than the call's :wall-value (taken back at dawn, so not
  spent): placing each. nil when fewer than cells are carried."
  [c cells]
  (let [dear (remove (set (choose/cheap-blocks (blocks c) (:wall-value (:args c)))) (blocks c))]
    (when (some #(>= (:count %) (count cells)) (lb/carried c dear))
      (* (count cells) place-s))))

(defn priced-name
  "The block name pit-s prices cell by: what is seen there, else the nearest seen block above it (the ground the pit is
  dug into: dirt stays dirt below the surface), else the rock guess."
  [p cell]
  (or (u/seen-name p cell)
      (->> (range 1 (inc (dig-cells/rock-depth))) (keep #(u/seen-name p (update cell :y + %))) (filter solid/solid?) first)
      (dig-cells/rock-name p cell)))

(defn pit-s
  "Seconds the pit's cells take to dig with the best carried tool or the hand."
  [c pit]
  (let [p (:primitives c)]
    (/ (reduce + (map #(tools/clear-ms p (priced-name p %)) (concat (:prep pit) (mapcat :dig (:steps pit))))) 1000)))

(defn mode-choice
  "[mode refusal] for the shelter from start: the first of :plug (only with a room-plug cell), :walls (only when
  walls-ok?) and :dig whose cells are all permitted, else the first of them with its refusal (nil when permitted).
  pit is the pit plan (retreat-pit/pit-plan), nil when none fits (the start cell's rules are then checked)."
  [c start walls-cells walls-ok? pit plug]
  (let [in (access/rules-input c)
        walls-v (some #(access/trespass-refusal in :place %) walls-cells)
        dig-v (if pit
                (or (some #(access/trespass-refusal in :dig %) (concat (:prep pit) (mapcat :dig (:steps pit))))
                    (access/trespass-refusal (assoc in :feet nil) :place (:roof pit)))
                (or (some #(access/trespass-refusal in :dig %) (map #(update start :y - %) [1 2]))
                    (access/trespass-refusal (assoc in :feet nil) :place start)))
        plug-v (when plug (access/trespass-refusal (assoc in :feet nil) :place plug))
        options (cond-> [] plug (conj [:plug plug-v]) walls-ok? (conj [:walls walls-v]) :always (conj [:dig dig-v]))]
    (or (first (filter (comp nil? second) options)) (first options))))

(defn spared-cell
  "The first cell of the pit that lies in a plan the body made (access/own-keep), as its verdict, else nil."
  [c pit]
  (let [in (access/rules-input c {:spare-own? true})]
    (some #(access/own-keep in %) (concat (:prep pit) (mapcat :dig (:steps pit))))))

(defn ^:async choose-mode
  "Record in job memory how this shelter is built, once. Chosen again only when the body leaves a dig-mode column.
  :walls stores :mode, and :start at the shaft top when the body is at the bottom of a 1x1 shaft.
  :dig stores :roof, :start and :cols. :plug stores the one cell to fill. :pit-stop stores :pit-stop, why no pit fits
  (retreat-pit/stop-of).
  See mode-choice for the zone rule."
  [c]
  (let [p (:primitives c)
        {:keys [mode]} (ctx/mem c)
        {:keys [x z] :as start} (sh/feet p)
        moved (and (= :dig mode) (not (contains? (:cols (ctx/mem c)) [x z])))]
    (when moved (ctx/update-mem! c dissoc :mode :roof :cols :pit-drops))
    (when (or moved (not mode))
      (let [cells (dig-cells/open-cells p start)
            have (reduce + (map :count (lb/carried c (blocks c))))
            cheap (reduce + (map :count (lb/carried c (choose/cheap-blocks (blocks c) (:wall-value (:args c))))))
            enclose (:enclose (:args c))
            found (when-not enclose (await (pit/plan-of! c (blocks c))))
            pit (when-not (:stop found) found)
            recoverable (when pit (recoverable-walls-s c cells))
            take-back-walls? (and recoverable (< recoverable (pit-s c pit)))
            plug (when (and (pos? have) (not enclose)) (dig-cells/room-plug p start (:roof-height (:args c))))
            spared (when pit (spared-cell c pit))
            [chosen refusal] (if enclose
                               [(if (>= have (count cells)) :walls :no-blocks)
                                (some #(access/trespass-refusal (access/rules-input c) :place %) cells)]
                               (mode-choice c start cells (or (>= cheap (count cells)) take-back-walls? (and (nil? pit) (>= have (count cells)))) pit plug))]
        (access/trespass! c "dig-in" refusal)
        (cond
          (= :no-blocks chosen) (ctx/update-mem! c assoc :mode :no-blocks)
          (= :plug chosen) (ctx/update-mem! c assoc :mode :plug :plug plug)
          (= :walls chosen) (ctx/update-mem! c #(cond-> (assoc % :mode :walls)
                                                  (dig-cells/shaft-top p start) (assoc :start {:x x :y (dig-cells/shaft-top p start) :z z})))
          (:stop found) (ctx/update-mem! c assoc :mode :pit-stop :pit-stop found)
          spared (ctx/update-mem! c assoc :mode :spared :spared spared)
          :else (ctx/update-mem! c assoc :mode :dig :roof (:roof pit) :start start
                                 :cols (into #{[x z]} (map (comp (juxt :x :z) :to) (:steps pit)))))))))

(defn spared-round
  "The only way left is a pit through the body's own zone or plan: do not dig, so the night shelter looks elsewhere."
  [c]
  (let [{:keys [zone plan cell]} (:spared (ctx/mem c))]
    (site/remember-failed-site! c :spared)
    (ctx/emit! c :dig_in_failed :warn {:cell cell :zone zone :plan plan
                                       :text (str "the pit would cut into the body's own " (if zone (str "zone " zone) (str "plan " plan))
                                                  " at " (pr-str cell) "; not digging")})
    :done))
