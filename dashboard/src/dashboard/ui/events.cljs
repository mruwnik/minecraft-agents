(ns dashboard.ui.events
  (:require [re-frame.core :as rf]
            [dashboard.ui.api]
            [dashboard.ui.chatsend :as cs]
            [dashboard.ui.db :as db]
            [dashboard.ui.drive :as drive]
            [dashboard.ui.logic :as logic]))

(def state-ms 2000)
(def chat-ms 3000)
(def chat-limit 300)

(defn unsupported-text [what] (str "unsupported for engine bodies: " what))

(rf/reg-event-fx
 :init
 (fn [_ [_ search page]]
   (let [initial (db/initial-db search (drive/new-who js/Math.random))]
     {:db initial
      :fx [[:dispatch [:fetch-worlds]]
           [:dispatch [:poll-state]]
           [:dispatch [:poll-chat]]
           [:start-timers [[:state state-ms [:poll-state]] [:chat chat-ms [:poll-chat]]]]
           (when-let [body (:detail-body initial)] [:dispatch [:open-detail body]])]})))

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
   {:db (assoc db :world world :state nil :chat [] :user-view nil :selected nil :detail-body nil :status "connecting..."
               :villages nil)
    :push-url (logic/with-world (.-pathname js/location) world)
    :fx [[:dispatch [:poll-state]] [:dispatch [:poll-chat]] [:dispatch [:plans/world-changed]]
         (when (= :villages (logic/page-for-path (.-pathname js/location))) [:dispatch [:villages/fetch]])]}))

(rf/reg-event-db :canvas-size (fn [db [_ w h]] (assoc db :canvas {:w w :h h})))
(rf/reg-event-db :fit-home (fn [db _] (assoc db :user-view nil)))
(rf/reg-event-db :fit (fn [db _] (assoc db :user-view (db/all-view db))))
(rf/reg-event-db :fit-bodies (fn [db _] (assoc db :user-view (db/bodies-view db))))

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

;; a body opens its popup, a plan its page, an edge arrow pans to its body, anything else is just selected
(rf/reg-event-fx
 :map-click
 (fn [_ [_ {:keys [kind name wx wz] :as selection}]]
   (case kind
     :edge {:dispatch [:center-on wx wz]}
     :body {:fx [[:dispatch [:select selection]] [:dispatch [:open-detail name]]]}
     :plan {:dispatch [:plans/open name]}
     {:dispatch [:select selection]})))
(rf/reg-event-db :toggle-chat (fn [db _] (update db :chat-open? not)))
(rf/reg-event-db :chat-filter (fn [db [_ text]] (assoc db :chat-filter text)))
;; sending a chat line as the owner: POST /api/chat/send; the line shows in the log once the bodies record it
(def ack-ms 3000)

(rf/reg-event-db :chat-draft (fn [db [_ text]] (update db :chat-send cs/edited text)))

(rf/reg-event-fx
 :chat-send
 (fn [{:keys [db]} _]
   (let [state (:chat-send db)]
     (when (cs/sendable? state)
       {:db (assoc db :chat-send (cs/begin state))
        :post-json {:url "/api/chat/send" :body (clj->js (cs/request-body state))
                    :on-ok [:chat-send-ok] :on-err [:chat-send-err] :on-unsupported [:chat-send-err "unsupported"]}}))))

(rf/reg-event-fx
 :chat-send-ok
 (fn [{:keys [db]} _]
   (let [id (inc (get-in db [:chat-send :ack-id]))]
     {:db (update db :chat-send cs/succeeded id)
      :dispatch-later [{:ms ack-ms :dispatch [:chat-ack-clear id]}]
      :fx [[:dispatch [:poll-chat]]]})))

(rf/reg-event-db :chat-send-err (fn [db [_ message]] (update db :chat-send cs/failed message)))
(rf/reg-event-db :chat-ack-clear (fn [db [_ id]] (update db :chat-send cs/clear-ack id)))

(rf/reg-event-db :hide-whispers (fn [db [_ on?]] (assoc db :hide-whispers? on?)))

;; The one handler for every action engine bodies do not support: log it, show it, call nothing.
(rf/reg-event-fx
 :unsupported
 (fn [{:keys [db]} [_ what]]
   (let [text (unsupported-text what)]
     {:db (assoc-in db [:notices what] text)
      :console-log text})))
