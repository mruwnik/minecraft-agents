(ns dashboard.ui.detail-events
  "The body popup's state: open and close (with ?body= in the URL), the action-log poll, and manual takeover
  (a 1 s poll of /drive/<name>, postMessage from the view page, take and release requests)."
  (:require [re-frame.core :as rf]
            [dashboard.ui.api :as api]
            [dashboard.ui.db :as db]
            [dashboard.ui.detail-model :as detail-model]
            [dashboard.ui.drive :as drive]
            [dashboard.ui.logic :as logic]))

(def log-ms 2000)
(def drive-ms 1000)
(def log-limit 300)
(def frame-id "view-frame")

(defn reconcile-page [prior-stream prior-events data]
  (let [reset? (or (:gap? data)
                   (not= (:stream-id data) (:stream-id prior-stream)))
        prior (if reset? [] prior-events)
        known (set (map :seq prior))
        incoming (remove #(contains? known (:seq %)) (:events data))
        events (vec (take-last 1000 (into prior incoming)))]
    {:events events
     :stream {:stream-id (:stream-id data)
              :generation-id (:generation-id data)
              :seq (get-in data [:cursor :seq] 0)}
     :outstanding (or (:outstanding data) {})
     :notices (->> events (filter #(= :notice (:attention %))) (take-last 5) reverse vec)}))

(defn drive-url [name] (str "/drive/" (js/encodeURIComponent name)))

(defn body-url [body]
  (logic/with-body (.-pathname js/location) (.-search js/location) body))

(rf/reg-event-fx
 :open-detail
 (fn [{:keys [db]} [_ name]]
   {:db (assoc db :detail-body name :detail-events [] :detail-chip :all :detail-text "" :drive {}
               :detail-stream nil :attention-outstanding (db/body-outstanding (:state db) name)
               :attention-error nil :detail-notices [])
    :replace-url (body-url name)
    :fx [[:start-timers [[:detail-log log-ms [:poll-detail-log]] [:detail-drive drive-ms [:poll-drive]]]]
         [:dispatch [:poll-detail-log]]
         [:dispatch [:poll-drive]]]}))

;; a body we drive is released before the popup goes away: through the view page when it can, else straight to the socket
(rf/reg-event-fx
 :close-detail
 (fn [{:keys [db]} _]
   (let [name (:detail-body db)
         driving? (drive/driving-now? (:drive db) (:who db))]
     (cond-> {:db (assoc db :detail-body nil :detail-events [] :drive {})
              :replace-url (body-url nil)
              :stop-timers [:detail-log :detail-drive]}
       (and name driving?) (assoc :drive-invoke {:op :release :name name :request (drive/release-request (:drive db) (:who db)) :both? true})))))

;; Esc: stop driving first, close when nobody is driven by us
(rf/reg-event-fx
 :escape
 (fn [{:keys [db]} _]
   (cond
     (not (:detail-body db)) {}
     (drive/driving-now? (:drive db) (:who db)) {:dispatch [:drive-release]}
     :else {:dispatch [:close-detail]})))

(rf/reg-event-fx
 :poll-detail-log
 (fn [{:keys [db]} _]
   (when-let [name (:detail-body db)]
     (let [{:keys [stream-id seq]} (:detail-stream db)
           query (cond-> {:limit log-limit}
                   stream-id (assoc :stream-id stream-id :after (or seq 0)))]
       {:fetch-edn {:key :detail-log :url (logic/api-url (str "/api/events/" name) nil query)
                    :on-ok [:detail-log-ok name] :on-err [:detail-log-err name]}}))))

(rf/reg-event-fx
 :detail-log-ok
 (fn [{:keys [db]} [_ name data]]
   (if (= name (:detail-body db))
     (let [{:keys [events stream outstanding notices]} (reconcile-page (:detail-stream db) (:detail-events db) data)
           db (assoc db :detail-events events
                        :detail-stream stream
                        :attention-outstanding outstanding
                        :detail-notices notices
                        :attention-error nil)]
       (cond-> {:db db} (:more? data) (assoc :dispatch [:poll-detail-log])))
     {:db db})))

(rf/reg-event-db :detail-log-err (fn [db [_ name error]] (if (= name (:detail-body db)) (assoc db :attention-error (str error)) db)))

(rf/reg-event-fx
 :attention-resolve
 (fn [{:keys [db]} [_ request-id]]
   (when-let [name (:detail-body db)]
     {:post-edn {:url (str "/api/attention/" (js/encodeURIComponent name) "/resolve")
                 :body {:request-id request-id :reason :handled}
                 :on-ok [:attention-resolved name]
                 :on-err [:attention-resolve-error name]}})))

(rf/reg-event-fx
 :attention-resolved
 (fn [{:keys [db]} [_ name _]]
   (if (= name (:detail-body db))
     {:db (assoc db :attention-error nil) :dispatch [:poll-detail-log]}
     {})))

(rf/reg-event-db
 :attention-resolve-error
 (fn [db [_ name error]]
   (if (= name (:detail-body db)) (assoc db :attention-error (str error)) db)))

(rf/reg-event-db :detail-chip (fn [db [_ chip]] (assoc db :detail-chip chip)))
(rf/reg-event-db :detail-text (fn [db [_ text]] (assoc db :detail-text text)))

;; ---------------------------------------------------------------- takeover
(rf/reg-event-fx
 :poll-drive
 (fn [{:keys [db]} _]
   (when-let [name (:detail-body db)]
     {:fetch-json {:key :detail-drive :url (drive-url name)
                   :on-ok [:drive-poll-ok name] :on-err [:drive-poll-err name]}})))

(rf/reg-event-db
 :drive-poll-ok
 (fn [db [_ name data]]
   (if (= name (:detail-body db)) (update db :drive drive/apply-poll (:manual data) (js/Date.now)) db)))

;; no body listening (offline) or no reply: nobody drives it
(rf/reg-event-db
 :drive-poll-err
 (fn [db [_ name _]]
   (if (= name (:detail-body db)) (update db :drive drive/apply-poll nil (js/Date.now)) db)))

(rf/reg-event-db
 :drive-message
 (fn [db [_ msg]] (update db :drive drive/apply-message msg (js/Date.now))))

(rf/reg-event-fx
 :drive-take
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:drive :error] nil)
    :drive-invoke {:op :take :name (:detail-body db) :request (drive/take-request (:who db))}}))

(rf/reg-event-fx
 :drive-release
 (fn [{:keys [db]} _]
   {:drive-invoke {:op :release :name (:detail-body db) :request (drive/release-request (:drive db) (:who db))}}))

(rf/reg-event-fx
 :drive-reply
 (fn [{:keys [db]} [_ data]]
   (let [refused (when (false? (:ok data)) (str "refused: " (:reason data)))]
     {:db (assoc-in db [:drive :error] refused)
      :dispatch [:poll-drive]})))

(rf/reg-event-fx
 :drive-error
 (fn [{:keys [db]} [_ text]]
   {:db (assoc-in db [:drive :error] text)}))

(def embed-style-id "dashboard-embed-style")

(defn apply-embed-style!
  "Hide the view page's own chrome inside our same-origin iframe (one <style> element, replaced when the toggle moves)."
  [stats?]
  (when-let [doc (some-> (js/document.getElementById frame-id) .-contentDocument)]
    (when-let [head (.-head doc)]
      (let [style (or (.getElementById doc embed-style-id) (.createElement doc "style"))]
        (set! (.-id style) embed-style-id)
        (set! (.-textContent style) (detail-model/embed-css stats?))
        (.appendChild head style)))))

(rf/reg-fx :embed-style apply-embed-style!)

(rf/reg-event-fx
 :frame-loaded
 (fn [{:keys [db]} _] {:embed-style (boolean (:detail-stats? db))}))

(rf/reg-event-fx
 :detail-stats
 (fn [{:keys [db]} _]
   (let [on? (not (:detail-stats? db))]
     {:db (assoc db :detail-stats? on?) :embed-style on?})))

(defn frame-drive
  "window.__drive of the view page in our iframe (same origin), when it has one."
  []
  (some-> (js/document.getElementById frame-id) .-contentWindow .-__drive))

(defn post-request! [name request]
  (api/post-json! {:url (drive-url name) :body (clj->js request) :on-ok [:drive-reply] :on-err [:drive-error]}))

;; the view page's own __drive.take/release when it has them (it then knows it is driving), else the socket via the proxy
(rf/reg-fx
 :drive-invoke
 (fn [{:keys [op name request both?]}]
   (let [page (frame-drive)
         f (when page (aget page (clojure.core/name op)))]
     (when (fn? f) (.call f page))
     (when (or both? (not (fn? f))) (post-request! name request)))))

(defn on-message
  "A window message: only the view page in our own iframe is believed."
  [e]
  (let [frame (js/document.getElementById frame-id)]
    (when (and frame (= (.-origin e) (.-origin js/location)) (identical? (.-source e) (.-contentWindow frame)))
      (when-let [msg (drive/parse-message (js->clj (.-data e) :keywordize-keys true))]
        (rf/dispatch [:drive-message msg])))))
