(ns dashboard.ui.events
  (:require [re-frame.core :as rf]
            [dashboard.ui.api]
            [dashboard.ui.db :as db]
            [dashboard.ui.logic :as logic]))

(def state-ms 2000)
(def chat-ms 3000)
(def chat-limit 300)

(defn unsupported-text [what] (str "unsupported for engine bodies: " what))

(rf/reg-event-fx
 :init
 (fn [_ [_ search page]]
   (let [initial (db/initial-db search)]
     {:db initial
      :fx [[:dispatch [:fetch-worlds]]
           [:dispatch [:poll-state]]
           (when (= page :main) [:dispatch [:poll-chat]])
           [:start-timers (concat [[:state state-ms [:poll-state]]]
                                  (when (= page :main) [[:chat chat-ms [:poll-chat]]]))]]})))

(rf/reg-event-fx
 :fetch-worlds
 (fn [_ _]
   {:fetch-json {:key :worlds :url "/api/worlds" :on-ok [:worlds-ok] :on-err [:worlds-err]}}))

(rf/reg-event-db :worlds-ok (fn [db [_ data]] (assoc db :worlds (vec (:worlds data)))))
(rf/reg-event-db :worlds-err (fn [db _] db))

(rf/reg-event-fx
 :poll-state
 (fn [{:keys [db]} _]
   (let [world (:world db)]
     {:fetch-json {:key :state :url (logic/api-url "/api/state" world {})
                   :on-ok [:state-ok world] :on-err [:state-err world]}})))

(rf/reg-event-fx
 :poll-chat
 (fn [{:keys [db]} _]
   (let [world (:world db)]
     {:fetch-json {:key :chat :url (logic/api-url "/api/chat" world {:limit chat-limit})
                   :on-ok [:chat-ok world] :on-err [:state-err world]}})))

;; a response for a world the user has since left is dropped
(rf/reg-event-db
 :state-ok
 (fn [db [_ world data]]
   (if (not= world (:world db))
     db
     (assoc db :state data :status "connected"))))

(rf/reg-event-db
 :state-err
 (fn [db [_ world message]]
   (if (not= world (:world db))
     db
     (assoc db :status (str "error: " message)))))

(rf/reg-event-db
 :chat-ok
 (fn [db [_ world data]]
   (if (not= world (:world db))
     db
     (assoc db :chat (vec (:messages data))))))

(rf/reg-event-fx
 :set-world
 (fn [{:keys [db]} [_ world]]
   {:db (assoc db :world world :state nil :chat [] :user-view nil :selected nil :status "connecting..."
               :villages nil)
    :push-url (logic/with-world (.-pathname js/location) world)
    :fx [[:dispatch [:poll-state]] [:dispatch [:poll-chat]]
         (when (= :villages (logic/page-for-path (.-pathname js/location))) [:dispatch [:villages/fetch]])]}))

(rf/reg-event-db :canvas-size (fn [db [_ w h]] (assoc db :canvas {:w w :h h})))
(rf/reg-event-db :fit (fn [db _] (assoc db :user-view nil)))

(rf/reg-event-db
 :pan
 (fn [db [_ dx dy]]
   (some->> (db/effective-view db) (#(logic/pan-view % dx dy)) (assoc db :user-view))))

(rf/reg-event-db
 :zoom
 (fn [db [_ factor px py]]
   (some->> (db/effective-view db) (#(logic/zoom-view % factor px py)) (assoc db :user-view))))

(rf/reg-event-db
 :center-on
 (fn [db [_ x z]]
   (let [{:keys [w h]} (:canvas db)]
     (if-let [view (db/effective-view db)]
       (assoc db :user-view (logic/center-view view x z w h))
       db))))

(rf/reg-event-db :select (fn [db [_ selection]] (assoc db :selected selection)))
(rf/reg-event-db :toggle-chat (fn [db _] (update db :chat-open? not)))
(rf/reg-event-db :chat-filter (fn [db [_ text]] (assoc db :chat-filter text)))
(rf/reg-event-db :hide-whispers (fn [db [_ on?]] (assoc db :hide-whispers? on?)))
(rf/reg-event-db :look-body (fn [db [_ name]] (assoc db :look-body name)))
(rf/reg-event-db :open-actions (fn [db [_ name]] (assoc db :actions-body name)))
(rf/reg-event-db :close-actions (fn [db _] (assoc db :actions-body nil)))

;; The one handler for every action engine bodies do not support: log it, show it, call nothing.
(rf/reg-event-fx
 :unsupported
 (fn [{:keys [db]} [_ what]]
   (let [text (unsupported-text what)]
     {:db (assoc-in db [:notices what] text)
      :console-log text})))
