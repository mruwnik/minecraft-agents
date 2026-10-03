(ns dashboard.ui.db
  (:require [dashboard.mapview :as mv]
            [dashboard.ui.logic :as logic]))

(defn initial-db [search]
  {:world (logic/world-from-search search)
   :worlds []
   :state nil
   :chat []
   :status "connecting..."
   :canvas nil
   :user-view nil
   :selected nil
   :chat-open? true
   :chat-filter ""
   :hide-whispers? false
   :detail-body (logic/body-from-search search)
   :detail-events []
   :detail-chip :all
   :detail-text ""
   :drive {}
   :notices {}})

(defn world-of [db] (first (get-in db [:state :worlds])))

(defn all-bodies [db]
  (or (get-in db [:state :bodies]) (:bodies (world-of db)) []))

(defn current-world [db]
  (or (:world db) (get-in db [:state :selected])))

(defn auto-view [db]
  (let [{:keys [w h]} (:canvas db)
        world (world-of db)]
    (when (and w h (pos? w) (pos? h))
      (-> (mv/map-points (all-bodies db) (:places world) (:zones world) (:humans world))
          mv/world-bounds
          (mv/fit-view w h)))))

;; The user's pan/zoom wins until "fit everything" clears it.
(defn effective-view [db]
  (or (:user-view db) (auto-view db)))
