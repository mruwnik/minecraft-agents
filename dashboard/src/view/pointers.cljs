(ns view.pointers
  "Pointers at the hostiles and players a view knows about: a marker over each one on screen, an arrow at the screen edge for each one
   off screen. Pure maths over the entity boxes the scene hands the renderer (positions relative to the eye); the DOM drawing is
   tools/view/web/app.mjs. Pixels have y down; :angle is the arrow's direction in radians (atan2 of pixel dy, dx).")

(defn agent-key?
  "Whether ?agent= names a body: <world>/<name>, each of letters, digits, _ and - (the view server's rule, tools/view/serve.mjs)."
  [value]
  (and (string? value) (boolean (re-matches #"[A-Za-z0-9_-]+/[A-Za-z0-9_-]+" value))))

(def MAX-POINTERS 12)
(def EDGE-MARGIN 28)
(def MIN-DIST 0.5)
(def ^:private SHOWN-KINDS #{"hostile" "player"})
(def ^:private EPS 1e-6)

(defn- dot [a b] (+ (* (:x a) (:x b)) (* (:y a) (:y b)) (* (:z a) (:z b))))

(defn edge-point
  "Where the ray from the picture's centre along pixel direction (dx, dy) leaves the rectangle inset by `margin`: {:px :py :angle}."
  [dx dy width height margin]
  (let [dx (+ 0 (if (and (< (abs dx) EPS) (< (abs dy) EPS)) 0 dx)) ; + 0: no negative zero, which atan2 turns into -pi
        dy (+ 0 (if (and (zero? dx) (< (abs dy) EPS)) 1 dy))
        hw (- (/ width 2) margin)
        hh (- (/ height 2) margin)
        t (min (if (< (abs dx) EPS) js/Infinity (/ hw (abs dx)))
               (if (< (abs dy) EPS) js/Infinity (/ hh (abs dy))))]
    {:px (+ (/ width 2) (* dx t)) :py (+ (/ height 2) (* dy t)) :angle (js/Math.atan2 dy dx)}))

(defn pointer-for
  "The pointer for a point `offset` (target minus eye) seen by `basis` in a width x height picture:
   {:px :py :on-screen? :angle :dist}. On screen the position is the point's pixel; off screen (outside the picture, or behind the
   camera) it is on the inset edge in the direction of the point, :angle pointing at it."
  [basis offset width height margin]
  (let [{:keys [forward right up half]} basis
        f (dot offset forward)
        r (dot offset right)
        u (dot offset up)
        dist (js/Math.hypot (:x offset) (:y offset) (:z offset))
        aspect (/ height width)]
    (if (> f EPS)
      (let [sx (/ (/ r f) half)
            sy (/ (/ u f) (* half aspect))]
        (if (and (<= (abs sx) 1) (<= (abs sy) 1))
          {:px (* (/ (inc sx) 2) width) :py (* (/ (- 1 sy) 2) height) :on-screen? true :dist dist}
          (assoc (edge-point (* sx (/ width 2)) (* (- sy) (/ height 2)) width height margin) :on-screen? false :dist dist)))
      (assoc (edge-point r (- u) width height margin) :on-screen? false :dist dist))))

(defn label
  "\"Zombie 12m\": the player's name or the species with spaces for underscores, and the distance in whole blocks."
  [{:keys [kind name label]} dist]
  (let [text (or label name "")
        text (if (= kind "player") text (.replaceAll text "_" " "))]
    (str text " " (js/Math.round dist) "m")))

(defn pointers
  "Pointers for the hostile and player boxes ({:min [x y z] :max [x y z] :kind :name :label :seen}, relative; one the body does not see (:seen false: behind a wall) gets none, to the eye's frame) as seen from
   `eye` through `basis`: the nearest MAX-POINTERS, nearest first, each {:kind :text :px :py :on-screen? :angle :dist}.
   The target is the box's middle; an on-screen marker sits above its head."
  [basis eye boxes width height]
  (->> boxes
       (filter #(and (contains? SHOWN-KINDS (:kind %)) (not (false? (:seen %)))))
       (keep (fn [{[x0 y0 z0] :min [x1 y1 z1] :max :as box}]
               (let [offset {:x (- (/ (+ x0 x1) 2) (:x eye)) :y (- (/ (+ y0 y1) 2) (:y eye)) :z (- (/ (+ z0 z1) 2) (:z eye))}
                     p (pointer-for basis offset width height EDGE-MARGIN)]
                 (when (>= (:dist p) MIN-DIST)
                   (let [p (if (:on-screen? p)
                             (let [above (pointer-for basis (update offset :y + (/ (- y1 y0) 2) 0.3) width height EDGE-MARGIN)]
                               (if (:on-screen? above) (merge p (select-keys above [:px :py])) p))
                             p)]
                     (assoc p :kind (:kind box) :text (label box (:dist p))))))))
       (sort-by :dist)
       (take MAX-POINTERS)
       vec))

(defn- ->box [^js b]
  {:min (vec (.-min b)) :max (vec (.-max b)) :kind (.-kind b) :name (.-name b) :label (.-label b) :seen (.-seen b)})

(defn js-pointers
  "pointers for the renderer's draw params (eye, basis as view.camera/js-camera-basis gives, entities) as an array of
   {kind, text, px, py, onScreen, angle, dist}."
  [^js params width height]
  (let [^js e (.-eye params)
        ^js b (.-basis params)
        v (fn [^js p] {:x (.-x p) :y (.-y p) :z (.-z p)})
        basis {:forward (v (.-forward b)) :right (v (.-right b)) :up (v (.-up b)) :half (.-half b)}]
    (to-array
     (map (fn [{:keys [kind text px py on-screen? angle dist]}]
            #js {:kind kind :text text :px px :py py :onScreen on-screen? :angle (or angle 0) :dist dist})
          (pointers basis (v e) (map ->box (.-entities params)) width height)))))
