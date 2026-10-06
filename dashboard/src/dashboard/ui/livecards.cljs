(ns dashboard.ui.livecards
  "Previews on the body cards from the view hub's textured WebGL scenes: live scenes for online bodies, the server's still
  image for offline ones (no scene); the server's PNGs for all cards when the hub cannot draw (no WebGL2, ?nogl=1).
  The pure decisions (flags, hub status, mode, plan, what a card shows, the fps label) come first; the
  reagent components with the hub lifecycle are below. The hub comes from window.getViewHub, defined by the module script in index.html."
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn flags
  "The debug and fallback flags of a location.search: ?nogl=1 keeps the server's stills (no hub), ?fps=1 labels each live card
  with its fps, ?allive=1 gives every card a live scene, offline ones too."
  [search]
  (let [params (js/URLSearchParams. search)
        on? #(= "1" (.get params %))]
    {:nogl? (on? "nogl")
     :fps? (on? "fps")
     :allive? (on? "allive")}))

(def hub-wait-ms 8000)

(defn hub-status
  "Can the page's hub draw? :loading while the module script has not defined window.getViewHub (for at most hub-wait-ms),
  then :supported, or :unsupported (no WebGL2, the module failed, or it never came)."
  [present? supported? waited-ms]
  (cond
    (and present? supported?) :supported
    present? :unsupported
    (< waited-ms hub-wait-ms) :loading
    :else :unsupported))

(defn render-mode
  "How cards are drawn: :hub (hub scenes), :still (the server's PNGs, the fallback) or :pending (the hub is not known yet)."
  [status {:keys [nogl?]}]
  (cond
    nogl? :still
    (= status :supported) :hub
    (= status :loading) :pending
    :else :still))

(defn card-plan
  "In hub mode: :live (a scene kept open) for online bodies, :last-image (the server's still, no scene) for offline ones; ?allive=1 makes all live."
  [{:keys [allive?]} status]
  (if (or allive? (not= status :offline)) :live :last-image))

(defn card-view
  "What a card's preview shows: :blank, :img (the server's PNG), :noview or :live (hub canvas). A :last-image card needs no hub,
  so it is not blank while the hub loads."
  [mode plan has-view?]
  (cond
    (= plan :last-image) (if has-view? :img :noview)
    (= mode :pending) :blank
    (= mode :still) (if has-view? :img :noview)
    (not has-view?) :noview
    :else :live))

(defn show-canvas?
  "The canvas replaces the still only once the scene has drawn or loaded something."
  [stats]
  (boolean (and stats (or (pos? (or (:fps stats) 0)) (pos? (or (:loaded stats) 0))))))

(defn no-world-data?
  "True when a scene that has connected (it has a pose: status is neither connecting nor unsupported) has no loaded column,
  so the view would be plain sky. `loaded` and `status` as in scene.stats(); status is nil for the popup's window.__view,
  which only exists once the page has connected, so a nil status counts as connected."
  [{:keys [loaded status]}]
  (boolean (and (number? loaded) (zero? loaded) (not (contains? #{"connecting" "unsupported"} status)))))

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

(defonce hub-state (r/atom :loading))

(defonce hub-poll
  (delay
    (let [started (js/Date.now)
          timer (atom nil)
          check (fn []
                  (let [hub (view-hub)
                        status (hub-status (some? hub) (some-> hub .-supported) (- (js/Date.now) started))]
                    (reset! hub-state status)
                    (when-not (= status :loading) (js/clearInterval @timer))))]
      (reset! timer (js/setInterval check 100))
      (check))))

(defn current-mode
  "The render mode of the page, starting the hub check on first use; reactive."
  [flags]
  (when-not (:nogl? flags) @hub-poll)
  (render-mode @hub-state flags))

(defn add-scene
  "A hub scene for one body, or nil when the hub refuses (it holds at most maxScenes): that card keeps its still image.
  The hub addresses a body as <world>/<name>."
  [hub world name]
  (try (.addScene hub #js {:agent (str world "/" name) :radius 2 :fov 70 :interp true})
       (catch :default e (js/console.warn "no live scene for" name (str e)) nil)))

(defn live-canvas
  "A canvas for one body with a scene in the hub, open while it is mounted; draws nothing until the hub has something for it.
  `on-stats` is called with the scene stats every poll-ms."
  [_props]
  (let [state (atom {})]
    (r/create-class
     {:display-name "live-canvas"
      :component-did-mount
      (fn [this]
        (let [canvas (:canvas @state)
              {:keys [name world]} (r/props this)
              scene (some-> (view-hub) (add-scene world name))]
          (some-> scene (.attach canvas (clj->js canvas-size)))
          (swap! state assoc
                 :canvas canvas
                 :scene scene
                 :timer (js/setInterval
                         (fn []
                           ((:on-stats (r/props this))
                            (some-> scene .stats (js->clj :keywordize-keys true))))
                         poll-ms))))
      :component-will-unmount
      (fn [this]
        (let [{:keys [timer scene canvas]} @state]
          (js/clearInterval timer)
          (when scene (.detach scene canvas) (.close scene))))
      :reagent-render
      (fn [{:keys [shown?]}]
        [:canvas.live {:ref #(when % (swap! state assoc :canvas %))
                       :class (when-not shown? "pending")
                       :width (:width canvas-size) :height (:height canvas-size)}])})))

(defn live-preview
  "The canvas plus the fps label for a card; `stats` is a ratom per card holding the latest scene stats."
  [{:keys [name flags]}]
  (let [stats (r/atom nil)]
    (fn [{:keys [name world flags]}]
      [:<>
       [live-canvas {:name name :world world :shown? (show-canvas? @stats) :on-stats #(reset! stats %)}]
       (when (and @stats (no-world-data? @stats))
         [:div.nodata "no world data yet"])
       (when (:fps? flags)
         [:span.fps-tag (fps-label @stats)])])))
