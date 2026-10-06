(ns dashboard.ui.tiles-events
  "Which terrain tiles the server has (/api/tiles/<world>): fetched once, then only what changed since (every 15 s, while
  the map is open), and the terrain on/off switch."
  (:require [re-frame.core :as rf]
            [dashboard.ui.api]
            [dashboard.ui.db :as db]))

(def tiles-ms 15000)
(def overlap-ms 10000)

(rf/reg-event-fx
 :tiles/start
 (fn [_ _]
   {:fx [[:dispatch [:tiles/fetch]]
         [:start-timers [[:tiles tiles-ms [:tiles/fetch]]]]]}))

(defn fetch-fx
  "The tiles request for the current world; nil without one (the 15 s timer asks again)."
  [db]
  (when-let [world (db/current-world db)]
    (let [held (:tiles db)
          since (when (= world (:requested held)) (some-> (:at held) (- overlap-ms) (max 0)))]
      {:fetch-json {:key :tiles
                    :url (str "/api/tiles/" (js/encodeURIComponent world) (when since (str "?since=" since)))
                    :on-ok [:tiles/ok world] :on-err [:tiles/err]}})))

(rf/reg-event-fx :tiles/fetch (fn [{:keys [db]} _] (fetch-fx db)))

(defn merge-index
  "The index {[cx cz] mtime} of the same world with the tiles that changed since; a new world starts empty."
  [held world entries]
  (into (if (= world (:requested held)) (:index held) {})
        (map (fn [[cx cz mtime]] [[cx cz] mtime]))
        entries))

(rf/reg-event-db
 :tiles/ok
 (fn [db [_ requested data]]
   (if (not= requested (db/current-world db))
     db
     (assoc db :tiles {:requested requested :world (:world data) :at (:at data)
                       :index (merge-index (:tiles db) requested (:tiles data))}))))

(rf/reg-event-db :tiles/err (fn [db _] db))

(rf/reg-event-db :toggle-terrain (fn [db _] (update db :terrain? #(not (if (nil? %) true %)))))

(rf/reg-sub :terrain? (fn [db _] (if (nil? (:terrain? db)) true (:terrain? db))))
(rf/reg-sub :current-world (fn [db _] (db/current-world db)))
(rf/reg-sub :tiles (fn [db _] (:tiles db)))
