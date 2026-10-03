(ns dashboard.ui.livecards
  "Live previews on the body cards: the view hub's textured WebGL scenes drawn over the thumbnails.
  The pure decisions (flags, which cards get a scene, canvas or still, the fps label) come first; the reagent
  component with the hub lifecycle is below. The hub comes from window.getViewHub, defined by the module script in index.html."
  (:require [reagent.core :as r]))

(defn flags
  "The debug and fallback flags of a location.search: ?nogl=1 keeps the stills, ?fps=1 labels each card with its fps."
  [search]
  (let [params (js/URLSearchParams. search)]
    {:nogl? (= "1" (.get params "nogl"))
     :fps? (= "1" (.get params "fps"))}))

(defn wants-scene?
  "Online bodies get a scene when the hub can draw; offline bodies keep the still."
  [supported? {:keys [nogl?]} status]
  (boolean (and supported? (not nogl?) (not= status :offline))))

(defn show-canvas?
  "The canvas replaces the still only once the scene has drawn or loaded something."
  [stats]
  (boolean (and stats (or (pos? (or (:fps stats) 0)) (pos? (or (:loaded stats) 0))))))

(defn fps-label [stats]
  (if-let [fps (:fps stats)]
    (str (.toFixed fps 1) " fps")
    "- fps"))

;; ---------------------------------------------------------------- the component

(def poll-ms 500)
(def canvas-size {:width 320 :height 180})

(defn view-hub
  "The page's one hub, created on first use; nil while the module script has not loaded or when it failed."
  []
  (when-let [get-hub (.-getViewHub js/window)]
    (try (get-hub) (catch :default _ nil))))

(defn live-canvas
  "A canvas for one online body, kept in step with `status` by polling; draws nothing until the hub has a scene for it.
  `on-stats` is called with the scene stats each tick (nil when there is no scene)."
  [_props]
  (let [state (atom {})]
    (r/create-class
     {:display-name "live-canvas"
      :component-did-mount
      (fn [this]
        (let [tick (fn []
                     (let [{:keys [name status nogl? on-stats]} (r/props this)
                           canvas (:canvas @state)
                           hub (when (and canvas (not nogl?)) (view-hub))
                           wanted? (wants-scene? (some-> hub .-supported) {:nogl? nogl?} status)
                           scene (:scene @state)]
                       (when (and scene (not wanted?))
                         (.detach scene canvas)
                         (.close scene)
                         (swap! state dissoc :scene))
                       (when (and wanted? (not scene))
                         (let [s (try (.addScene hub #js {:agent name :radius 2 :fov 70 :interp true})
                                      (catch :default _ nil))]
                           (when s
                             (.attach s canvas (clj->js canvas-size))
                             (swap! state assoc :scene s))))
                       (on-stats (some-> (:scene @state) .stats (js->clj :keywordize-keys true)))))]
          (swap! state assoc :timer (js/setInterval tick poll-ms))
          (tick)))
      :component-will-unmount
      (fn [_]
        (let [{:keys [timer scene canvas]} @state]
          (js/clearInterval timer)
          (when scene (.detach scene canvas) (.close scene))))
      :reagent-render
      (fn [{:keys [shown?]}]
        [:canvas.live {:ref #(swap! state assoc :canvas %)
                       :class (when-not shown? "pending")
                       :width (:width canvas-size) :height (:height canvas-size)}])})))

(defn live-preview
  "The canvas plus the fps label for a card; `stats` is a ratom per card holding the latest scene stats."
  [{:keys [name status flags]}]
  (let [stats (r/atom nil)]
    (fn [{:keys [name status flags]}]
      [:<>
       [live-canvas {:name name :status status :nogl? (:nogl? flags) :shown? (show-canvas? @stats)
                     :on-stats #(reset! stats %)}]
       (when (:fps? flags)
         [:span.fps-tag (fps-label @stats)])])))
