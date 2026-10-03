(ns dashboard.ui.mapmodel
  "Pure rules of the map: where a body is, colours, hit tests, label thresholds.")

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

(def status-colors {:working "#3fb950" :idle "#8b949e" :trouble "#d29922" :offline "#6e7681"})

(def place-label-scale 0.5)

(defn kind-color [kind] (get kind-colors kind place-color))
(defn status-color [status] (get status-colors status (:idle status-colors)))
(defn show-place-labels? [scale] (>= scale place-label-scale))

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

(defn plan-box [{:keys [region]}] (bounds-box region))

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
