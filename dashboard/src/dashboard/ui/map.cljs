(ns dashboard.ui.map
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.mapview :as mv]
            [dashboard.tiles :as tiles]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.mapmodel :as mm]
            [dashboard.ui.plansmodel :as pm]
            [dashboard.ui.trouble :as trouble]))

(def pick-radius 10)
(def label-font "11px ui-monospace, Menlo, monospace")
(def body-font "bold 12px ui-monospace, Menlo, monospace")
(def zone-label-scale 3)
(def edge-inset 16)
(def edge-pick-radius 14)

(def human-color "#f08ad0")
(def zone-color "#5b8fd6")

(defn plan-rect [view plan]
  (-> (mv/zone-rect view (mm/plan-box plan))
      (mm/min-size 8)
      (assoc :kind :plan :name (:id plan) :title (:name plan) :color (pm/completion-color (:counts plan))
             :percent (:percent plan))))

(defn element-rects
  "The elements of a plan inside its outline, only when zoomed in; a click on one opens its plan."
  [view plan]
  (when (mm/show-plan-elements? (:scale view))
    (for [{:keys [id bounds counts]} (:elements plan) :when bounds]
      (assoc (mv/zone-rect view (mm/bounds-box bounds))
             :kind :plan :name (:id plan) :title id :color (pm/completion-color counts)
             :percent (:percent counts)))))

(defn edge-arrows
  "An arrow on the viewport edge for every body that is up and off screen, named with its distance from the view's centre."
  [{:keys [view canvas now]} bodies]
  (let [{:keys [w h]} canvas
        cx (+ (:origin-x view) (/ w 2 (:scale view)))
        cz (+ (:origin-z view) (/ h 2 (:scale view)))]
    (vec (for [b bodies
               :when (:up b)
               :let [pos (mm/body-pos b)
                     marker (when pos (mm/edge-marker {:w w :h h :inset edge-inset} (mv/project view (:x pos) (:z pos))))]
               :when marker]
           {:kind :edge :name (:name b) :px (:x marker) :py (:y marker) :angle (:angle marker)
            :color (mm/status-color (trouble/status b (or now 0)))
            :wx (:x pos) :wz (:z pos)
            :dist (mm/distance-text (js/Math.hypot (- (:x pos) cx) (- (:z pos) cz)))}))))

(defn layout
  "Everything drawable with pixel positions, bodies first (so their labels win space)."
  [{:keys [view bodies places zones humans selected plans now canvas] :as model}]
  {:scale (:scale view)
   :edges (if (and (:w canvas) (:h canvas)) (edge-arrows model bodies) [])
   :zones (for [z zones] (assoc (mv/zone-rect view z) :name (:name z)))
   :plans (for [p plans :when (:region p)] (plan-rect view p))
   :plan-elements (vec (mapcat #(element-rects view %) plans))
   :bodies (for [b bodies :let [pos (mm/body-pos b)] :when pos]
             (let [status (trouble/status b (or now 0))]
               (assoc (mv/project view (:x pos) (:z pos)) :kind :body :name (:name b) :up (boolean (:up b))
                      :status status :color (mm/status-color status)
                      :selected? (= selected {:kind :body :name (:name b)}))))
   :humans (for [h humans] (assoc (mv/project view (:x h) (:z h)) :kind :human :name (:name h)))
   :places (for [p places]
             (assoc (mv/project view (:x p) (:z p)) :kind :place :name (:name p) :place-kind (:kind p)
                    :color (mm/kind-color (:kind p))
                    :selected? (= selected {:kind :place :name (:name p)})))})

(defn pick [lay x y]
  (or (logic/pick-nearest (:edges lay) x y edge-pick-radius)
      (logic/pick-nearest (:bodies lay) x y pick-radius)
      (logic/pick-nearest (:places lay) x y pick-radius)
      (mm/pick-plan (concat (:plan-elements lay) (:plans lay)) x y)))

(defn dot! [ctx {:keys [px py]} color radius]
  (.beginPath ctx)
  (.arc ctx px py radius 0 (* 2 js/Math.PI))
  (set! (.-fillStyle ctx) color)
  (.fill ctx))

(defn ring! [ctx {:keys [px py]} radius]
  (.beginPath ctx)
  (.arc ctx px py radius 0 (* 2 js/Math.PI))
  (set! (.-strokeStyle ctx) "#ffffff")
  (set! (.-lineWidth ctx) 2)
  (.stroke ctx))

(defn body-dot! [ctx {:keys [up color] :as b}]
  (.save ctx)
  (set! (.-globalAlpha ctx) (if up 1 0.55))
  (dot! ctx b "#0b0d11" (if up 7 5))
  (dot! ctx b color (if up 5.5 3.5))
  (.restore ctx))

(defn diamond! [ctx {:keys [px py]} color]
  (.beginPath ctx)
  (.moveTo ctx px (- py 3.5)) (.lineTo ctx (+ px 3.5) py) (.lineTo ctx px (+ py 3.5)) (.lineTo ctx (- px 3.5) py)
  (.closePath ctx)
  (set! (.-fillStyle ctx) color)
  (.fill ctx))

(defn arrow! [ctx {:keys [px py angle color]}]
  (.save ctx)
  (.translate ctx px py)
  (.rotate ctx angle)
  (.beginPath ctx)
  (.moveTo ctx 9 0) (.lineTo ctx -6 -7) (.lineTo ctx -3 0) (.lineTo ctx -6 7)
  (.closePath ctx)
  (set! (.-fillStyle ctx) color)
  (set! (.-strokeStyle ctx) "#0b0d11")
  (set! (.-lineWidth ctx) 2)
  (.stroke ctx)
  (.fill ctx)
  (.restore ctx))

(defn edge-label
  "The arrow's text sits inward of it: left of the arrow on the right half of the canvas, right of it on the left."
  [ctx {:keys [px py name dist color] :as edge} canvas-w]
  (let [text (str name " " dist)]
    (set! (.-font ctx) body-font)
    (let [width (.-width (.measureText ctx text))
          right? (> px (/ canvas-w 2))]
      (assoc edge :name text :label-color color :label-font body-font
             :px (if right? (- px 14 width) (+ px 14)) :py (- py 6) :w width :h 12))))

(defn label-box [ctx {:keys [px py name] :as item} color font]
  (set! (.-font ctx) font)
  (assoc item :label-color color :label-font font
         :px (+ px 9) :py (- py 6) :w (.-width (.measureText ctx name)) :h 12
         :anchor-x px :anchor-y py))

(defn draw-labels! [ctx boxes]
  (set! (.-textBaseline ctx) "top")
  (doseq [{:keys [px py name label-color label-font]} (mv/fit-labels boxes)]
    (set! (.-font ctx) label-font)
    (set! (.-lineWidth ctx) 3)
    (set! (.-strokeStyle ctx) "rgba(11,13,17,0.85)")
    (.strokeText ctx name px py)
    (set! (.-fillStyle ctx) label-color)
    (.fillText ctx name px py)))

(defn draw-plan! [ctx {:keys [px py w h color]}]
  (set! (.-fillStyle ctx) color)
  (set! (.-globalAlpha ctx) 0.18)
  (.fillRect ctx px py w h)
  (set! (.-globalAlpha ctx) 1)
  (set! (.-strokeStyle ctx) color)
  (set! (.-lineWidth ctx) 1.5)
  (.strokeRect ctx px py w h))

(defn draw-zone! [ctx {:keys [px py w h]}]
  (set! (.-fillStyle ctx) "rgba(91,143,214,0.07)")
  (.fillRect ctx px py w h)
  (set! (.-strokeStyle ctx) "rgba(91,143,214,0.35)")
  (set! (.-lineWidth ctx) 1)
  (.strokeRect ctx px py w h))

(defn draw-element! [ctx {:keys [px py w h color]}]
  (set! (.-fillStyle ctx) color)
  (set! (.-globalAlpha ctx) 0.30)
  (.fillRect ctx px py w h)
  (set! (.-globalAlpha ctx) 1)
  (set! (.-strokeStyle ctx) color)
  (set! (.-lineWidth ctx) 1)
  (.strokeRect ctx px py w h))

(defn plan-label [{:keys [title percent]}] (str title " " percent "%"))

(defn labels [ctx lay canvas-w]
  (let [scale (:scale lay)]
    (concat
     (for [e (:edges lay)] (edge-label ctx e canvas-w))
     (for [b (:bodies lay) :when (:up b)] (label-box ctx b (:color b) body-font))
     (for [p (:plans lay)] (label-box ctx (assoc p :name (plan-label p)) (:color p) label-font))
     (for [e (:plan-elements lay) :when (> (:w e) 40)]
       (assoc (label-box ctx (assoc e :name (:title e)) (:color e) label-font) :px (+ (:px e) 2) :py (+ (:py e) 2)))
     (for [h (:humans lay)] (label-box ctx h human-color label-font))
     (when (mm/show-place-labels? scale)
       (for [p (:places lay)] (label-box ctx p (:color p) label-font)))
     (for [b (:bodies lay) :when (not (:up b))] (label-box ctx b "#6e7681" label-font))
     (when (>= scale zone-label-scale)
       (for [z (:zones lay) :when (:name z)]
         (assoc (label-box ctx z zone-color label-font) :px (+ (:px z) 2) :py (+ (:py z) 2)))))))

;; ---------------------------------------------------------------- terrain
;; The tile pictures are loaded as the viewport asks for them (at most max-loading at a time) and kept in a bounded LRU,
;; larger than the most tiles ever drawn as pictures (mm/max-terrain-tiles), so panning back finds them again.
(def image-cap 2400)
(def max-loading 16)
(def terrain-dim "rgba(11,13,17,0.16)")
(def coverage-color "rgba(130,150,175,0.22)")

(defonce tile-images (tiles/lru))
(defonce loading (atom 0))
(defonce on-tile-loaded (atom nil))
(defonce redraw-queued (atom false))
(defonce coverage-path-cache (atom nil))
(def tile-retry-ms 5000)

(defn release-terrain-cache! []
  (reset! coverage-path-cache nil))

(defn coverage-path-key [world index scale]
  {:world world :index index :scale scale})

(defn coverage-path-cache-matches? [cached key]
  (and cached
       (= (:world cached) (:world key))
       (identical? (:index cached) (:index key))
       (= (:scale cached) (:scale key))))

(defn retry-same-image? [current failed]
  (identical? current failed))

(defn coverage-path [world index scale]
  (let [key (coverage-path-key world index scale)
        cached @coverage-path-cache]
    (if (coverage-path-cache-matches? (:key cached) key)
      (:path cached)
      (let [path (js/Path2D.)
            cell-size (/ (max 1 (- (* mm/tile-blocks scale) 0.5)) scale)]
        (doseq [[[cx cz] _] index]
          (.rect path (* cx mm/tile-blocks) (* cz mm/tile-blocks) cell-size cell-size))
        (reset! coverage-path-cache {:key key :path path})
        path))))

(defn queue-redraw! []
  (when-not @redraw-queued
    (reset! redraw-queued true)
    (js/requestAnimationFrame (fn [] (reset! redraw-queued false) (when-let [f @on-tile-loaded] (f))))))

(defn tile-url [world {:keys [cx cz mtime]}]
  (str "/api/tile/" (js/encodeURIComponent world) "/" cx "." cz ".png?m=" mtime))

(defn start-load! [url]
  (let [img (js/Image.)
        done! (fn [] (swap! loading dec) (queue-redraw!))]
    (swap! loading inc)
    (set! (.-onload img) done!)
    (set! (.-onerror img)
          (fn []
            (aset img "failed" true)
            (done!)
            ;; A broken URL must not stay failed in the LRU forever, but a persistent error should not
            ;; trigger a request on every redraw either. Retry only while this failed image is still current.
            (js/setTimeout (fn []
                             (when (retry-same-image? (.get tile-images url) img)
                               (.delete tile-images url)
                               (queue-redraw!)))
                           tile-retry-ms)))
    (set! (.-src img) url)
    (tiles/lru-put! tile-images image-cap url img)
    nil))

(defn tile-image
  "The loaded picture of a tile, or nil (not asked for yet, loading, or failed); starts the load when it is due."
  [url]
  (let [img (tiles/lru-get! tile-images url)]
    (cond
      (and img (.-complete img) (pos? (.-naturalWidth img))) img
      img nil
      (>= @loading max-loading) nil
      :else (start-load! url))))

(defn draw-terrain! [ctx {:keys [view canvas tile-index tile-world]} dpr]
  (let [mode (mm/terrain-mode view canvas)]
    (.save ctx)
    (set! (.-imageSmoothingEnabled ctx) false)
    (if (= mode :coverage)
      (do (set! (.-fillStyle ctx) coverage-color)
          (.setTransform ctx (* dpr (:scale view)) 0 0 (* dpr (:scale view))
                         (* -1 dpr (:scale view) (:origin-x view))
                         (* -1 dpr (:scale view) (:origin-z view)))
          (.fill ctx (coverage-path tile-world tile-index (:scale view))))
      (do (doseq [{:keys [px py size] :as t} (:items (mm/visible-terrain view canvas tile-index))
                  :let [img (tile-image (tile-url tile-world t))]
                  :when img]
            ;; a hair larger than the cell, so no seam shows between neighbours at fractional scales
            (.drawImage ctx img (js/Math.floor px) (js/Math.floor py) (inc (js/Math.ceil size)) (inc (js/Math.ceil size))))))
    (.restore ctx)
    mode))

(defn canvas-size! [canvas w h dpr]
  (set! (.-width canvas) (* w dpr))
  (set! (.-height canvas) (* h dpr)))

(defn draw! [canvas model]
  (let [{:keys [w h]} (:canvas model)
        ctx (when (and canvas w h (:view model)) (.getContext canvas "2d"))
        dpr (or js/window.devicePixelRatio 1)]
    (when canvas
      (canvas-size! canvas w h dpr))
    (when ctx
      (.setTransform ctx dpr 0 0 dpr 0 0)
      (.clearRect ctx 0 0 w h)
      (set! (.-font ctx) label-font)
      (let [lay (layout model)
            terrain-mode (when (and (:terrain? model) (:tile-index model) (:tile-world model))
                           (draw-terrain! ctx model dpr))]
        (when (= terrain-mode :coverage)
          (set! (.-fillStyle ctx) "rgba(215,220,228,0.75)")
          (set! (.-textBaseline ctx) "top")
          (.fillText ctx "zoom in for terrain (shaded: dumped columns)" 10 8))
        (doseq [z (:zones lay)] (draw-zone! ctx z))
        (doseq [p (:plans lay)] (draw-plan! ctx p))
        (doseq [e (:plan-elements lay)] (draw-element! ctx e))
        (doseq [p (:places lay)] (diamond! ctx p (:color p)))
        (doseq [h (:humans lay)] (dot! ctx h human-color 4))
        (doseq [b (sort-by :up (:bodies lay))] (body-dot! ctx b))
        (doseq [x (concat (:bodies lay) (:places lay)) :when (:selected? x)] (ring! ctx x 10))
        (doseq [e (:edges lay)] (arrow! ctx e))
        (draw-labels! ctx (labels ctx lay w))))))

(defn canvas-pos [e node]
  (let [rect (.getBoundingClientRect node)]
    [(- (.-clientX e) (.-left rect)) (- (.-clientY e) (.-top rect))]))

(def legend-items
  [["body: working" (mm/status-color :working)] ["idle" (mm/status-color :idle)] ["trouble" (mm/status-color :trouble)]
   ["offline" (mm/status-color :offline)] ["plan: 90%+" pm/green] ["50%+" pm/amber] ["less" pm/red] ["unseen" pm/grey]])

(defn legend []
  (into [:div#maplegend
         [:div.maphint "drag to pan · wheel to zoom · click a body for its view, a plan for its page, an arrow to go to its body"]]
        (for [[label color] legend-items]
          ^{:key label} [:span [:i {:style {:background color}}] label])))

(defn map-view []
  (let [wrap (atom nil)
        canvas (atom nil)
        resize-observer (atom nil)
        model (atom nil)
        drag (atom nil)
        redraw! #(draw! @canvas @model)
        measure! (fn []
                   (when-let [w @wrap]
                     (rf/dispatch [:canvas-size (.-clientWidth w) (.-clientHeight w)])))
        on-wheel (fn [e]
                   (.preventDefault e)
                   (let [[x y] (canvas-pos e @canvas)]
                     (rf/dispatch [:zoom (if (neg? (.-deltaY e)) 1.15 (/ 1 1.15)) x y])))]
    (r/create-class
     {:component-did-mount
      (fn []
        (.addEventListener @canvas "wheel" on-wheel #js {:passive false})
        (let [observer (js/ResizeObserver. measure!)]
          (reset! resize-observer observer)
          (.observe observer @wrap))
        (reset! on-tile-loaded redraw!)
        (measure!)
        (redraw!))
      :component-will-unmount (fn []
                                (.removeEventListener @canvas "wheel" on-wheel)
                                (when-let [observer @resize-observer] (.disconnect observer))
                                (reset! resize-observer nil)
                                (reset! on-tile-loaded nil)
                                (release-terrain-cache!))
      :component-did-update redraw!
      :reagent-render
      (fn []
        (reset! model @(rf/subscribe [:map-model]))
        [:div.pane {:ref #(reset! wrap %)}
         [:canvas
          {:ref #(reset! canvas %)
           :on-pointer-down (fn [e]
                              (.setPointerCapture (.-target e) (.-pointerId e))
                              (reset! drag {:x (.-clientX e) :y (.-clientY e) :moved 0}))
           :on-pointer-move (fn [e]
                              (when-let [d @drag]
                                (let [dx (- (.-clientX e) (:x d)) dy (- (.-clientY e) (:y d))]
                                  (reset! drag {:x (.-clientX e) :y (.-clientY e)
                                                :moved (+ (:moved d) (js/Math.abs dx) (js/Math.abs dy))})
                                  (rf/dispatch [:pan dx dy]))))
           :on-pointer-up (fn [e]
                            (let [d @drag
                                  [x y] (canvas-pos e @canvas)]
                              (reset! drag nil)
                              (when (< (:moved d 0) 4)
                                (rf/dispatch [:map-click (some-> (pick (layout @model) x y) (select-keys [:kind :name :wx :wz]))]))))}]
         [legend]])})))
