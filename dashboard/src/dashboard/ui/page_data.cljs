(ns dashboard.ui.page-data
  "Events and subscriptions of the villages, villagers and blueprints pages: fetch, poll, keep the answer."
  (:require [re-frame.core :as rf]
            [dashboard.ui.api]
            [dashboard.ui.logic :as logic]))

(def villages-ms 15000)
(def villagers-ms 10000)

(defn clock-now [] (.toLocaleTimeString (js/Date.)))

;; ---------------------------------------------------------------- villages (per world)
(rf/reg-event-fx
 :villages/start
 (fn [_ _]
   {:fx [[:dispatch [:villages/fetch]]
         [:start-timers [[:villages villages-ms [:villages/fetch]]]]]}))

(rf/reg-event-fx
 :villages/fetch
 (fn [{:keys [db]} _]
   (let [world (:world db)]
     {:fetch-json {:key :villages :url (logic/api-url "/api/villages" world {})
                   :on-ok [:villages/ok world] :on-err [:villages/err world]}})))

(rf/reg-event-db
 :villages/ok
 (fn [db [_ world data]]
   (if (not= world (:world db))
     db
     (update db :villages assoc :items (vec (:villages data)) :error (:error data) :clock (clock-now) :failed nil))))

(rf/reg-event-db
 :villages/err
 (fn [db [_ world message]]
   (if (not= world (:world db)) db (assoc-in db [:villages :failed] message))))

(rf/reg-event-db :villages/filter (fn [db [_ text]] (assoc-in db [:villages :filter] text)))

(rf/reg-sub :villages (fn [db _] (:villages db)))

;; ---------------------------------------------------------------- villagers (global)
(rf/reg-event-fx
 :villagers/start
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:villagers :filter] (or (.get (js/URLSearchParams. (.-search js/location)) "uuid") ""))
    :fx [[:dispatch [:villagers/fetch]]
         [:start-timers [[:villagers villagers-ms [:villagers/fetch]]]]]}))

(rf/reg-event-fx
 :villagers/fetch
 (fn [_ _]
   {:fetch-json {:key :villagers :url "/api/villagers" :on-ok [:villagers/ok] :on-err [:villagers/err]}}))

(rf/reg-event-db
 :villagers/ok
 (fn [db [_ data]]
   (if (:error data)
     (assoc-in db [:villagers :failed] (:error data))
     (update db :villagers assoc :roster data :clock (clock-now) :failed nil))))

(rf/reg-event-db :villagers/err (fn [db [_ message]] (assoc-in db [:villagers :failed] message)))
(rf/reg-event-db :villagers/filter (fn [db [_ text]] (assoc-in db [:villagers :filter] text)))

(rf/reg-sub :villagers (fn [db _] (:villagers db)))
