(ns dashboard.ui.map
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.mapview :as mv]
            [dashboard.ui.logic :as logic]))

(def pick-radius 10)
(def label-font "11px ui-monospace, Menlo, monospace")

(def colors {:up "#3fb950" :down "#6e7681" :human "#f08ad0" :place "#d9b25f" :zone "#5b8fd6" :ink "#dfe4ec"})

(defn body-pos [b] (get-in b [:state :pos]))

(defn projected [view x z] (mv/project view x z))

(defn layout
  "Everything drawable with pixel positions, bodies first (so their labels win space)."
  [{:keys [view bodies places zones humans selected]}]
  {:zones (for [z zones] (assoc (mv/zone-rect view z) :name (:name z)))
   :plans (for [p (mv/plan-rects places)] (assoc (mv/zone-rect view {:x1 (:x p) :z1 (:z p) :x2 (+ (:x p) (:w p)) :z2 (+ (:z p) (:h p))}) :name (:name p)))
   :bodies (for [b bodies :let [pos (body-pos b)] :when pos]
             (assoc (projected view (:x pos) (:z pos)) :kind :body :name (:name b) :up (boolean (:up b))
                    :selected? (= selected {:kind :body :name (:name b)})))
   :humans (for [h humans] (assoc (projected view (:x h) (:z h)) :kind :human :name (:name h)))
   :places (for [p places]
             (assoc (projected view (:x p) (:z p)) :kind :place :name (:name p)
                    :selected? (= selected {:kind :place :name (:name p)})))})

(defn pick [lay x y]
  (or (logic/pick-nearest (:bodies lay) x y pick-radius)
      (logic/pick-nearest (:places lay) x y pick-radius)))

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

(defn diamond! [ctx {:keys [px py]} color]
  (.beginPath ctx)
  (.moveTo ctx px (- py 5)) (.lineTo ctx (+ px 5) py) (.lineTo ctx px (+ py 5)) (.lineTo ctx (- px 5) py)
  (.closePath ctx)
  (set! (.-fillStyle ctx) color)
  (.fill ctx))

(defn label-box [ctx {:keys [px py name] :as item} color]
  (assoc item :label-color color
         :px (+ px 8) :py (- py 6) :w (.-width (.measureText ctx name)) :h 12
         :anchor-x px :anchor-y py))

(defn draw-labels! [ctx boxes]
  (set! (.-textBaseline ctx) "top")
  (doseq [{:keys [px py name label-color]} (mv/fit-labels boxes)]
    (set! (.-fillStyle ctx) label-color)
    (.fillText ctx name px py)))

(defn draw! [canvas model]
  (let [{:keys [w h]} (:canvas model)
        ctx (when (and canvas w h (:view model)) (.getContext canvas "2d"))
        dpr (or js/window.devicePixelRatio 1)]
    (when canvas
      (set! (.-width canvas) (* w dpr))
      (set! (.-height canvas) (* h dpr)))
    (when ctx
      (.setTransform ctx dpr 0 0 dpr 0 0)
      (.clearRect ctx 0 0 w h)
      (set! (.-font ctx) label-font)
      (let [lay (layout model)]
        (doseq [z (concat (:plans lay) (:zones lay))]
          (set! (.-strokeStyle ctx) (:zone colors))
          (set! (.-lineWidth ctx) 1)
          (set! (.-fillStyle ctx) "rgba(91,143,214,0.10)")
          (.fillRect ctx (:px z) (:py z) (:w z) (:h z))
          (.strokeRect ctx (:px z) (:py z) (:w z) (:h z)))
        (doseq [p (:places lay)] (diamond! ctx p (:place colors)))
        (doseq [h (:humans lay)] (dot! ctx h (:human colors) 4))
        (doseq [b (:bodies lay)] (dot! ctx b (if (:up b) (:up colors) (:down colors)) 5))
        (doseq [x (concat (:bodies lay) (:places lay)) :when (:selected? x)] (ring! ctx x 9))
        (draw-labels! ctx (concat
                           (for [b (:bodies lay)] (label-box ctx b (if (:up b) (:up colors) (:down colors))))
                           (for [h (:humans lay)] (label-box ctx h (:human colors)))
                           (for [p (:places lay)] (label-box ctx p (:place colors)))
                           (for [z (:zones lay) :when (:name z)]
                             (assoc (label-box ctx z (:zone colors)) :px (+ (:px z) 2) :py (+ (:py z) 2)))))))))

(defn canvas-pos [e node]
  (let [rect (.getBoundingClientRect node)]
    [(- (.-clientX e) (.-left rect)) (- (.-clientY e) (.-top rect))]))

(defn map-view []
  (let [wrap (atom nil)
        canvas (atom nil)
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
        (.observe (js/ResizeObserver. measure!) @wrap)
        (measure!)
        (redraw!))
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
                                (rf/dispatch [:select (some-> (pick (layout @model) x y) (select-keys [:kind :name]))]))))}]
         [:div#hint "drag to pan · wheel to zoom · click a body or place to select it"]])})))
