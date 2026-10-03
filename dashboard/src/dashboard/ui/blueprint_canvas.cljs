(ns dashboard.ui.blueprint-canvas
  "Canvas drawing for the blueprint page: one layer's grid and the isometric preview. Pure data comes from dashboard.blueprint."
  (:require [dashboard.blueprint :as bp]))

(def cell-px 22)

(defn dpr [] (or js/window.devicePixelRatio 1))

(defn hatch! [c px py w h]
  (.save c)
  (.beginPath c)
  (.rect c px py w h)
  (.clip c)
  (set! (.-strokeStyle c) "rgba(139,149,165,0.6)")
  (set! (.-lineWidth c) 1.5)
  (doseq [d (range (- h) w 5)]
    (.beginPath c)
    (.moveTo c (+ px d) (+ py h))
    (.lineTo c (+ px d h) py)
    (.stroke c))
  (.restore c))

(defn draw-layer! [canvas {:keys [width depth]} cells hovered]
  (let [d (dpr) w (* width cell-px) h (* depth cell-px)]
    (set! (.. canvas -style -width) (str w "px"))
    (set! (.. canvas -style -height) (str h "px"))
    (set! (.-width canvas) (js/Math.round (* w d)))
    (set! (.-height canvas) (js/Math.round (* h d)))
    (let [c (.getContext canvas "2d")]
      (.setTransform c d 0 0 d 0 0)
      (.clearRect c 0 0 w h)
      (doseq [{:keys [dx dz air colour name]} cells
              :let [px (* dx cell-px) py (* dz cell-px)]]
        (set! (.-fillStyle c) (if air "#161a20" colour))
        (.fillRect c px py (dec cell-px) (dec cell-px))
        (when (= name "@solid") (hatch! c px py (dec cell-px) (dec cell-px)))
        (when air
          (set! (.-fillStyle c) "#242a33")
          (.fillRect c (- (+ px (/ cell-px 2)) 1) (- (+ py (/ cell-px 2)) 1) 2 2)))
      (when hovered
        (set! (.-strokeStyle c) "#fff")
        (set! (.-lineWidth c) 2)
        (.strokeRect c (inc (* (:dx hovered) cell-px)) (inc (* (:dz hovered) cell-px)) (- cell-px 3) (- cell-px 3))))))

(defn cell-at [cells canvas e]
  (let [r (.getBoundingClientRect canvas)
        dx (js/Math.floor (/ (- (.-clientX e) (.-left r)) cell-px))
        dz (js/Math.floor (/ (- (.-clientY e) (.-top r)) cell-px))]
    (first (filter #(and (= dx (:dx %)) (= dz (:dz %))) cells))))

;; draws the building and returns the polygons in pixels, for hover picking
(defn draw-preview! [canvas cells angle max-y]
  (let [rect (.getBoundingClientRect canvas)
        w (max 1 (.-width rect)) h (.-height rect) d (dpr)]
    (set! (.-width canvas) (js/Math.round (* w d)))
    (set! (.-height canvas) (js/Math.round (* h d)))
    (let [c (.getContext canvas "2d")
          faces (bp/preview-faces cells {:angle angle :max-y max-y})
          ;; fit the whole building, so cutting the roof away does not zoom the room
          bounds (bp/preview-bounds (bp/preview-faces cells {:angle angle}))
          polys (bp/fit-faces faces bounds w h)]
      (.setTransform c d 0 0 d 0 0)
      (.clearRect c 0 0 w h)
      (doseq [{:keys [points colour]} polys]
        (.beginPath c)
        (doseq [[i p] (map-indexed vector points)]
          (if (zero? i) (.moveTo c (:x p) (:y p)) (.lineTo c (:x p) (:y p))))
        (.closePath c)
        (set! (.-fillStyle c) colour)
        (.fill c)
        (set! (.-strokeStyle c) "rgba(0,0,0,.18)")
        (set! (.-lineWidth c) 0.6)
        (.stroke c))
      polys)))

(defn face-at [polys canvas e]
  (let [r (.getBoundingClientRect canvas)
        x (- (.-clientX e) (.-left r)) y (- (.-clientY e) (.-top r))]
    (first (filter #(bp/inside-polygon? (:points %) x y) (reverse polys)))))
