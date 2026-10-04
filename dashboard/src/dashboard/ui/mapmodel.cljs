(ns dashboard.ui.mapmodel
  "Pure rules of the map: where a body is, colours, hit tests, label thresholds."
  (:require [dashboard.ui.logic :as logic]
            [dashboard.ui.plansmodel :as pm]))

(def place-color "#d9b25f")
(def base-color "#e8c468")
(def farm-color "#7fcf6a")
(def mine-color "#9aa7b8")
(def resource-color "#4fc3b0")
(def danger-color "#f26d6d")
(def view-color "#6aa9ff")
(def build-color "#e8955f")

(def kind-colors
  {"base" base-color "farm" farm-color "mine" mine-color "resource" resource-color
   "enemy" danger-color "danger" danger-color "view" view-color "build" build-color})

(def villager-color "#c792ea")

(def status-colors {:manual "#f85149" :working "#3fb950" :idle "#8b949e" :trouble "#d29922" :offline "#6e7681"})

(def place-label-scale 0.5)

(defn kind-color [kind] (get kind-colors kind place-color))
(defn status-color [status] (get status-colors status (:idle status-colors)))
(defn show-place-labels? [scale] (>= scale place-label-scale))

(def villager-label-scale 1.5)
(defn show-villager-labels? [scale] (>= scale villager-label-scale))

(defn coords? [p] (and (number? (:x p)) (number? (:z p))))

(defn body-pos
  "Where the body is now: the view's pose, else the last position the engine reported."
  [b]
  (first (filter coords? [(get-in b [:view :pos]) (get-in b [:state :pos]) (get-in b [:engine :pos])])))

(defn body-points [bodies]
  (vec (for [b bodies :when (:up b) :let [p (body-pos b)] :when p] {:x (:x p) :z (:z p)})))

(defn min-size
  "A rectangle at least s pixels each way, grown around its centre."
  [{:keys [px py w h] :as rect} s]
  (assoc rect :px (- px (/ (max 0 (- s w)) 2)) :py (- py (/ (max 0 (- s h)) 2)) :w (max w s) :h (max h s)))

(defn in-rect? [{:keys [px py w h]} x y]
  (and (<= px x (+ px w)) (<= py y (+ py h))))

(defn pick-plan
  "The smallest plan rectangle under the point, so a plan nested in a bigger one stays clickable."
  [plans x y]
  (->> plans (filter #(in-rect? % x y)) (sort-by #(* (:w %) (:h %))) first))

(def element-scale
  "Pixels per block from which the elements of a plan are drawn inside its outline."
  3)

(defn show-plan-elements? [scale] (>= scale element-scale))

(defn bounds-box
  "Block rectangle of {:min [x y z] :max [x y z]} as a zone-like box; the bounds are inclusive, so the far edge is one
  block further."
  [{[x1 _ z1] :min [x2 _ z2] :max}]
  {:x1 x1 :z1 z1 :x2 (inc x2) :z2 (inc z2)})

(defn conflict-marks
  "The pairs of plans in conflict (/api/plans :conflicts) as what the map draws: the label, the block box, the centre to go
  to and the cells."
  [pairs]
  (vec (for [{:keys [box cells] :as pair} pairs
             :let [{[x1 _ z1] :min [x2 _ z2] :max} box]]
         {:kind :conflict :name (pm/pair-text pair) :box (bounds-box box)
          :wx (/ (+ x1 x2 1) 2) :wz (/ (+ z1 z2 1) 2) :cells cells})))

(defn plan-box
  "The block rectangle of a plan, nil for a plan with no cells."
  [{:keys [region]}]
  (when region (bounds-box region)))

(defn edge-marker
  "Where an arrow for an off-screen point sits: on the viewport's edge (inset pixels in), on the line from the centre to
  the point, and the angle it points at. nil when the point is on screen."
  [{:keys [w h inset]} {:keys [px py]}]
  (when (or (< px 0) (> px w) (< py 0) (> py h))
    (let [cx (/ w 2) cy (/ h 2)
          dx (- px cx) dy (- py cy)
          limit (fn [half d] (if (zero? d) js/Infinity (/ half (js/Math.abs d))))
          t (min (limit (- cx inset) dx) (limit (- cy inset) dy))]
      {:x (+ cx (* dx t)) :y (+ cy (* dy t)) :angle (js/Math.atan2 dy dx)})))

(defn distance-text [blocks]
  (cond
    (not (number? blocks)) ""
    (< blocks 1000) (str (js/Math.round blocks))
    :else (let [k (/ (js/Math.round (/ blocks 100)) 10)] (str k "k"))))

;; ---------------------------------------------------------------- terrain tiles
(def tile-blocks 16)
(def max-terrain-tiles
  "More tiles than this in view: no pictures (too many requests), just a mark where columns are dumped."
  2000)

(defn tile-range
  "The chunk columns (inclusive cx/cz ranges) a view shows in a canvas of {:w :h} pixels."
  [{:keys [origin-x origin-z scale]} {:keys [w h]}]
  (let [col (fn [blocks] (js/Math.floor (/ blocks tile-blocks)))
        last-col (fn [blocks] (dec (js/Math.ceil (/ blocks tile-blocks))))]
    {:cx1 (col origin-x) :cx2 (last-col (+ origin-x (/ w scale)))
     :cz1 (col origin-z) :cz2 (last-col (+ origin-z (/ h scale)))}))

(defn tile-item [{:keys [origin-x origin-z scale]} [cx cz] mtime]
  {:cx cx :cz cz :mtime mtime :size (* tile-blocks scale)
   :px (* (- (* cx tile-blocks) origin-x) scale) :py (* (- (* cz tile-blocks) origin-z) scale)})

(defn terrain-mode [view canvas]
  (let [{:keys [cx1 cx2 cz1 cz2]} (tile-range view canvas)
        count-in-view (* (inc (- cx2 cx1)) (inc (- cz2 cz1)))]
    (if (<= count-in-view max-terrain-tiles) :tiles :coverage)))

(defn visible-terrain
  "{:mode :tiles | :coverage, :items [tile]} for the dumped columns ({[cx cz] mtime}) in view. Up to max-terrain-tiles
  columns in view are :tiles, to be drawn as pictures; more are :coverage, drawn as plain marks."
  [view canvas index]
  (let [{:keys [cx1 cx2 cz1 cz2]} (tile-range view canvas)]
    (if (= :tiles (terrain-mode view canvas))
      {:mode :tiles
       :items (vec (for [cz (range cz1 (inc cz2)) cx (range cx1 (inc cx2))
                         :let [mtime (get index [cx cz])]
                         :when mtime]
                     (tile-item view [cx cz] mtime)))}
      {:mode :coverage
       :items (vec (for [[[cx cz :as k] mtime] index
                         :when (and (<= cx1 cx cx2) (<= cz1 cz cz2))]
                     (tile-item view k mtime)))})))

;; ---------------------------------------------------------------- villagers
;; A villager is placed where a body's last pose saw it (the pose lists every entity within 48 blocks and is rewritten at
;; least every 2 s while the body is up). The sighting is live while a body that is up wrote that pose a moment ago;
;; otherwise it is old (drawn faded, with its age), and it goes once it is older than villager-drop-ms.
(def villager-live-ms 10000)
(def villager-drop-ms 1800000)

(defn body-sightings [now {:keys [name up view]}]
  (let [t (:poseT view)]
    (when (and (number? t) (contains? #{nil "overworld"} (:dimension view)))
      (let [age (max 0 (- now t))]
        (for [{:keys [id x z] :as v} (:villagers view)
              :when (and id (number? x) (number? z) (<= age villager-drop-ms))]
          (assoc v :seen-by name :t t :age-ms age :old? (not (and up (<= age villager-live-ms)))))))))

(defn villager-sightings
  "One sighting per villager, the freshest, from the bodies' poses."
  [bodies now]
  (->> (mapcat #(body-sightings now %) bodies)
       (sort-by :t >)
       (reduce (fn [seen v] (if (contains? seen (:id v)) seen (assoc seen (:id v) v))) {})
       vals
       vec))

(defn villager-tip [{:keys [seen-by age-ms old?]}]
  (str "villager, seen by " seen-by " " (logic/time-ago-text age-ms) (when old? " (old: nobody watching now)")))

;; ---------------------------------------------------------------- name labels
(defn pick-label
  "The body name label under the point, among label boxes ({:kind :px :py :w :h})."
  [boxes x y]
  (first (filter #(and (contains? #{:body :conflict} (:kind %)) (in-rect? % x y)) boxes)))

;; ---------------------------------------------------------------- players list
(defn where-text [{:keys [x y z]}]
  (if (every? number? [x y z]) (str (js/Math.round x) ", " (js/Math.round y) ", " (js/Math.round z)) "no position"))

(defn player-rows
  "A row for every body (status-of gives its status keyword; offline ones last, then by name), then for every human seen
  on the server."
  [bodies humans status-of]
  (vec (concat
        (for [b (sort-by (juxt #(= :offline (status-of %)) :name) bodies) :let [pos (body-pos b)]]
          {:kind :body :name (:name b) :status (status-of b) :pos (some-> pos (select-keys [:x :z]))
           :where (where-text pos)})
        (for [h (sort-by :name humans)]
          {:kind :human :name (:name h) :pos {:x (:x h) :z (:z h)} :where (where-text h) :seen-by (:seenBy h)}))))

(defn player-click-events
  "What clicking a row of the players list does: select it, centre the map on it, and for a body open its popup."
  [{:keys [kind name pos]}]
  (cond-> [[:select {:kind kind :name name}]]
    pos (conj [:center-on (:x pos) (:z pos)])
    (= :body kind) (conj [:open-detail name])))
