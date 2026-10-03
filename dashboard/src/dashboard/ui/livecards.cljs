(ns dashboard.ui.livecards
  "Previews on the body cards from the view hub's textured WebGL scenes: live scenes for online bodies, one snapshot
  (dashboard.ui.stills) for offline ones; the server's PNGs only when the hub cannot draw (no WebGL2, ?nogl=1).
  The pure decisions (flags, hub status, mode, plan, what a card shows, the snapshot queue, the fps label) come first; the
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
  "In hub mode: :live (a scene kept open) for online bodies, :snapshot (one frame, scene closed) for offline ones; ?allive=1 makes all live."
  [{:keys [allive?]} status]
  (if (or allive? (not= status :offline)) :live :snapshot))

(defn card-view
  "What a card's preview shows: :blank, :img (the server's PNG), :noview, :live (hub canvas) or :still (canvas holding a snapshot).
  `still` is the snapshot record {:mtime :failed?} of the card, nil before it was taken."
  [mode plan has-view? still]
  (cond
    (= mode :pending) :blank
    (= mode :still) (if has-view? :img :noview)
    (not has-view?) :noview
    (= plan :live) :live
    (:failed? still) :img
    :else :still))

(def snapshot-timeout-ms 20000)

(defn snapshot-state
  "Is the scene ready to snapshot, still loading its columns, or out of time?"
  [ready? waited-ms]
  (cond
    ready? :ready
    (< waited-ms snapshot-timeout-ms) :waiting
    :else :timeout))

(def cooling-off-ms 1000)

(defn cooling-down
  "The names whose scene was closed less than cooling-off-ms ago. The hub reopens its /poses stream only when the set of agents
  changes, and applies a change 500 ms after the last add or close; a scene added for an agent before that has no pose, ever."
  [closed-at now]
  (into #{} (comp (filter (fn [[_ at]] (< (- now at) cooling-off-ms))) (map key)) closed-at))

(defn keep-record?
  "A snapshot record that stays when its card goes away: a failed one (so the card is not retried in a loop)."
  [record]
  (boolean (:failed? record)))

(defn next-snapshot
  "The offline card to snapshot next, or nil. `wanted`: name -> pose mtime (nil: no view); `done`: name -> {:mtime ...} of the last
  attempt (a failed one counts, it is not retried for the same pose); `blocked`: names to leave alone for now. Alphabetical, one at a time."
  [wanted done blocked]
  (->> (sort-by key wanted)
       (filter (fn [[name mtime]] (and mtime (not (contains? blocked name)) (not= mtime (:mtime (get done name))))))
       ffirst))

(defonce closed-at (atom {})) ; name -> ms of the last time a scene of that body was closed

(defn note-closed! [name] (swap! closed-at assoc name (js/Date.now)))

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
              {:keys [name]} (r/props this)
              scene (some-> (view-hub)
                            (.addScene #js {:agent name :radius 2 :fov 70 :interp true}))]
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
          (when scene (.detach scene canvas) (.close scene) (note-closed! (:name (r/props this))))))
      :reagent-render
      (fn [{:keys [shown?]}]
        [:canvas.live {:ref #(when % (swap! state assoc :canvas %))
                       :class (when-not shown? "pending")
                       :width (:width canvas-size) :height (:height canvas-size)}])})))

(defn live-preview
  "The canvas plus the fps label for a card; `stats` is a ratom per card holding the latest scene stats."
  [{:keys [name flags]}]
  (let [stats (r/atom nil)]
    (fn [{:keys [name flags]}]
      [:<>
       [live-canvas {:name name :shown? (show-canvas? @stats) :on-stats #(reset! stats %)}]
       (when (and @stats (no-world-data? @stats))
         [:div.nodata "no world data yet"])
       (when (:fps? flags)
         [:span.fps-tag (fps-label @stats)])])))
