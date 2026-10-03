(ns dashboard.ui.stills
  "One frame from the view hub for each offline body card, at the body's last pose. The queue and its rules are in
  dashboard.ui.livecards (next-snapshot, snapshot-state); this namespace is the loop and the canvas.
  A snapshot opens a scene for the body, retries `scene.snapshot` until the scene reports ready (its columns are loaded;
  the hub uploads columns only while drawing, so each try draws once), keeps the ImageBitmap and closes the scene: no scene
  is held between snapshots, and only one is open at a time."
  (:require [reagent.core :as r]
            [dashboard.ui.livecards :as live]))

(def retry-ms 250)
(def tick-ms 500)

(defonce wanted (r/atom {})) ; name -> pose mtime of the mounted offline cards
(defonce done (r/atom {}))   ; name -> {:mtime, :bitmap | :failed?}
(defonce busy? (atom false))
(defonce timer (atom nil))

(defn record [name] (get @done name))

(defn snapshot!
  "Resolves to {:mtime m :bitmap ImageBitmap} or {:mtime m :failed? true}; the scene is closed either way."
  [hub name mtime]
  (js/Promise.
   (fn [resolve]
     (let [scene (try (.addScene hub #js {:agent name :radius 2 :fov 70 :interp false})
                      (catch :default _ nil))
           started (js/Date.now)
           finish (fn [bitmap]
                    (some-> scene .close)
                    (resolve (if bitmap {:mtime mtime :bitmap bitmap} {:mtime mtime :failed? true})))]
       (if-not scene
         (finish nil)
         (letfn [(waited [] (- (js/Date.now) started))
                 (again [] (js/setTimeout attempt retry-ms))
                 (attempt []
                   (-> (.snapshot scene (clj->js live/canvas-size))
                       (.then (fn [bitmap]
                                (case (live/snapshot-state (.ready scene) (waited))
                                  :ready (finish bitmap)
                                  :waiting (do (.close bitmap) (again))
                                  :timeout (do (.close bitmap) (finish nil)))))
                       (.catch (fn [_]
                                 (if (= :timeout (live/snapshot-state false (waited))) (finish nil) (again))))))]
           (attempt)))))))

(defn tick! []
  (let [hub (live/view-hub)
        name (live/next-snapshot @wanted @done)]
    (when (and hub name (not @busy?))
      (reset! busy? true)
      (let [mtime (get @wanted name)]
        (-> (snapshot! hub name mtime)
            (.then (fn [result]
                     (some-> (:bitmap (get @done name)) .close)
                     (swap! done assoc name result)))
            (.finally #(reset! busy? false)))))))

(defn want! [name mtime]
  (swap! wanted assoc name mtime)
  (when-not @timer (reset! timer (js/setInterval tick! tick-ms))))

(defn unwant! [name]
  (swap! wanted dissoc name)
  (some-> (:bitmap (get @done name)) .close)
  (swap! done dissoc name))

(defn draw! [canvas {:keys [bitmap]}]
  (when bitmap
    (.drawImage (.getContext canvas "2d") bitmap 0 0 (:width live/canvas-size) (:height live/canvas-size))))

(defn still-canvas
  "A card's canvas holding its snapshot; blank (pending) until the snapshot of its current pose is drawn."
  [_props]
  (let [node (atom nil)
        sync! (fn [this]
                (let [{:keys [name pose-mtime]} (r/props this)]
                  (want! name pose-mtime)
                  (some-> @node (draw! (record name)))))]
    (r/create-class
     {:display-name "still-canvas"
      :component-did-mount sync!
      :component-did-update (fn [this _] (sync! this))
      :component-will-unmount (fn [this] (unwant! (:name (r/props this))))
      :reagent-render
      (fn [{:keys [name]}]
        [:canvas.live {:ref #(when % (reset! node %))
                       :class (when-not (:bitmap (record name)) "pending")
                       :width (:width live/canvas-size) :height (:height live/canvas-size)}])})))
