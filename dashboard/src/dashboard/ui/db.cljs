(ns dashboard.ui.db
  (:require [dashboard.mapview :as mv]
            [dashboard.ui.chatsend :as cs]
            [dashboard.ui.mapmodel :as mm]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.statusfilter :as statusfilter]))

(defn initial-db [search who]
  {:who who
   :world (logic/world-from-search search)
   :worlds []
   :state nil
   :chat []
   :status "connecting..."
   :canvas nil
   :user-view nil
   :selected nil
   :status-filter #{}
   :chat-open? true
   :places-open? true
   :players-open? false
   :show-body (logic/show-from-search search)
   :chat-filter ""
   :hide-whispers? false
   :chat-send cs/initial
   :whisper-send {}
   :chat-sender nil
   :detail-body (logic/body-from-search search)
   :detail-events []
   :detail-chip :all
   :detail-text ""
   :drive {}
   :notices {}})

(defn world-of [db] (first (get-in db [:state :worlds])))

(defn all-bodies [db]
  (or (get-in db [:state :bodies]) (:bodies (world-of db)) []))

(defn shown-bodies
  "The bodies the status chips let through, plus the selected one and the one open in the detail."
  [db]
  (let [selected (:selected db)
        keep (cond-> #{}
               (= :body (:kind selected)) (conj (:name selected))
               (:detail-body db) (conj (:detail-body db)))]
    (statusfilter/only-states (all-bodies db) (:status-filter db) (get-in db [:state :at] 0) keep)))

(defn body-outstanding [snapshot body-name]
  (or (:outstanding (some #(when (or (= body-name (:name %)) (= body-name (:username %))) %)
                           (:bodies snapshot)))
      {}))

(defn current-world [db]
  (or (:world db) (get-in db [:state :selected])))

(defn plan-boxes [db] (map mm/plan-box (get-in db [:plans :items])))

(defn bodies-view
  "A view fitted to the bodies that are up, or nil when none has a position."
  [db]
  (let [{:keys [w h]} (:canvas db)]
    (when (and w h (pos? w) (pos? h))
      (-> (mm/body-points (shown-bodies db))
          (mv/world-bounds 48)
          (mv/fit-view w h)))))

(defn fit-points [points {:keys [w h]}]
  (when (and w h (pos? w) (pos? h) (seq points))
    (-> points mv/world-bounds (mv/fit-view w h))))

(defn all-view
  "A view fitted to everything: bodies, places, zones, humans and plans."
  [db]
  (let [world (world-of db)]
    (fit-points (mv/map-points (all-bodies db) (:places world) (:zones world) (:humans world) (plan-boxes db))
                (:canvas db))))

(def home-trim 0.1)

(defn home-view
  "A view fitted to the places and plans, the home cluster: a probe standing thousands of blocks away would squash it,
  and so would the few places far from the rest (the outer tenth is left out), but every plan is kept. Falls back to
  everything when the world has neither."
  [db]
  (let [world (world-of db)
        places (mv/trimmed-points (mv/map-points [] (:places world) [] [] []) home-trim)
        plans (mv/map-points [] [] [] [] (plan-boxes db))]
    (or (fit-points (into places plans) (:canvas db))
        (all-view db))))

;; The user's pan/zoom wins until a fit button clears or replaces it.
(defn effective-view [db]
  (or (:user-view db) (home-view db)))

(def show-scale
  "Pixels per block a body shown from its card is zoomed to (a closer zoom is kept)."
  2)

(defn focus-body
  "Centred and selected on the named body, zoomed in to at least show-scale; db unchanged when the body has no position or
  there is no canvas or view yet."
  [db name]
  (let [{:keys [w h]} (:canvas db)
        pos (some #(when (= name (:name %)) (mm/body-pos %)) (all-bodies db))
        view (effective-view db)]
    (if-not (and pos w h view)
      db
      (assoc db
             :selected {:kind :body :name name}
             :user-view (logic/center-view (update view :scale max show-scale) (:x pos) (:z pos) w h)))))

(defn apply-pending-show
  "Carries out the show request the page was opened with as soon as the body, its position and the canvas are known."
  [db]
  (let [name (:show-body db)
        shown (when name (focus-body db name))]
    (if (and shown (not= shown db)) (dissoc shown :show-body) db)))
