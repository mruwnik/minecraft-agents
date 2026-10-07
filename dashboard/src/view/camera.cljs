(ns view.camera
  "The camera maths of the viewer: the basis a shader takes (tools/view/renderer.mjs cameraFor, non-panorama), the ray of a pixel
   and its exact inverse, the pixel rectangle of a world face. Points and vectors are {:x :y :z}; the js-* functions are the
   JS-object faces of the same maths (the GL page, the Node pixel check).
   mineflayer's convention: yaw 0 faces north (-z) and grows turning left; pitch > 0 looks up.")

(def ^:private MIN-DEPTH 1e-6)

(defn direction-for [yaw pitch]
  {:x (* (- (js/Math.sin yaw)) (js/Math.cos pitch))
   :y (js/Math.sin pitch)
   :z (* (- (js/Math.cos yaw)) (js/Math.cos pitch))})

(defn- dot [a b] (+ (* (:x a) (:x b)) (* (:y a) (:y b)) (* (:z a) (:z b))))

(defn- cross [a b]
  {:x (- (* (:y a) (:z b)) (* (:z a) (:y b)))
   :y (- (* (:z a) (:x b)) (* (:x a) (:z b)))
   :z (- (* (:x a) (:y b)) (* (:y a) (:x b)))})

(defn camera-basis
  "fov is the horizontal field of view in degrees; :half is the tangent of half of it."
  [{:keys [yaw pitch fov] :or {yaw 0 pitch 0 fov 90}}]
  (let [forward (direction-for yaw pitch)
        right (direction-for (- yaw (/ js/Math.PI 2)) 0)]
    {:forward forward :right right :up (cross right forward) :half (js/Math.tan (/ (* fov js/Math.PI) 360))}))

(defn ray-dir
  "The unit direction of pixel (px, py) in a width x height image."
  [{:keys [forward right up half]} px py width height]
  (let [sx (* (dec (* (/ (+ px 0.5) width) 2)) half)
        sy (* (- 1 (* (/ (+ py 0.5) height) 2)) half (/ height width))
        [x y z] (map (fn [k] (+ (k forward) (* (k right) sx) (* (k up) sy))) [:x :y :z])
        len (js/Math.hypot x y z)]
    {:x (/ x len) :y (/ y len) :z (/ z len)}))

(defn project-point
  "The pixel {:px :py} of `offset` (the point minus the eye), the exact inverse of ray-dir; nil when it is not in front of the camera."
  [{:keys [forward right up half]} offset width height]
  (let [depth (dot offset forward)]
    (when (> depth MIN-DEPTH)
      (let [sx (/ (/ (dot offset right) depth) half)
            sy (/ (/ (dot offset up) depth) (* half (/ height width)))]
        {:px (- (* (/ (inc sx) 2) width) 0.5) :py (- (* (/ (- 1 sy) 2) height) 0.5)}))))

(defn- corners
  "The four corners of an axis-aligned face {:axis :at, <other axis> [lo hi], ...}, each range shrunk by `inset` of its extent per side."
  [{:keys [axis at] :as face} inset]
  (let [[a b] (remove #{axis} [:x :y :z])
        shrink (fn [[lo hi]] [(+ lo (* (- hi lo) inset)) (- hi (* (- hi lo) inset))])
        [a-lo a-hi] (shrink (a face))
        [b-lo b-hi] (shrink (b face))]
    (for [u [a-lo a-hi] v [b-lo b-hi]] {axis at a u b v})))

(defn face-region
  "The pixel rectangle {:x0 :y0 :x1 :y1} (inclusive) inside the projected face, or nil if any corner is behind the camera."
  ([basis eye face width height] (face-region basis eye face width height 0.15))
  ([basis eye face width height inset]
   (let [points (map (fn [p] (project-point basis {:x (- (:x p) (:x eye)) :y (- (:y p) (:y eye)) :z (- (:z p) (:z eye))} width height))
                     (corners face inset))]
     (when (every? some? points)
       (let [xs (map :px points) ys (map :py points)]
         {:x0 (js/Math.ceil (apply min xs)) :y0 (js/Math.ceil (apply min ys))
          :x1 (js/Math.floor (apply max xs)) :y1 (js/Math.floor (apply max ys))})))))

(defn- vec3->js [{:keys [x y z]}] #js {:x x :y y :z z})
(defn- basis->js [{:keys [forward right up half]}]
  #js {:forward (vec3->js forward) :right (vec3->js right) :up (vec3->js up) :half half})
(defn- js->basis [^js b]
  {:forward (js->clj (.-forward b) :keywordize-keys true) :right (js->clj (.-right b) :keywordize-keys true)
   :up (js->clj (.-up b) :keywordize-keys true) :half (.-half b)})
(defn- js->vec3 [^js p] {:x (.-x p) :y (.-y p) :z (.-z p)})

(defn js-direction-for [yaw pitch] (vec3->js (direction-for yaw pitch)))

(defn js-camera-basis [^js camera]
  (basis->js (camera-basis (cond-> {}
                             (some? (.-yaw camera)) (assoc :yaw (.-yaw camera))
                             (some? (.-pitch camera)) (assoc :pitch (.-pitch camera))
                             (some? (.-fov camera)) (assoc :fov (.-fov camera))))))

(defn js-ray-dir [basis px py width height] (vec3->js (ray-dir (js->basis basis) px py width height)))

(defn js-project-point [basis offset width height]
  (when-let [{:keys [px py]} (project-point (js->basis basis) (js->vec3 offset) width height)]
    #js {:px px :py py}))

(defn js-face-region
  "face: {axis: 'x'|'y'|'z', at, <other axis>: [lo, hi], ...}; inset defaults to 0.15."
  [basis eye face width height inset]
  (let [^js f face
        axis (keyword (.-axis f))
        face-map (into {:axis axis :at (.-at f)} (map (fn [k] [k (vec (aget f (name k)))])) (remove #{axis} [:x :y :z]))]
    (when-let [{:keys [x0 y0 x1 y1]} (face-region (js->basis basis) (js->vec3 eye) face-map width height (if (some? inset) inset 0.15))]
      #js {:x0 x0 :y0 y0 :x1 x1 :y1 y1})))
