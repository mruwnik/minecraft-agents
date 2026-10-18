(ns dashboard.ui.detail-events
  "The body popup's state: open and close (with ?body= in the URL), the action-log poll and the embedded view's chrome."
  (:require [re-frame.core :as rf]
            [dashboard.ui.api :as api]
            [dashboard.ui.db :as db]
            [dashboard.ui.detail-model :as detail-model]
            [dashboard.ui.logic :as logic]))

(def log-ms 2000)
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

;; a body is <world>/<name>; the popup's body is always of the page's world
(defn body-path [db name] (str (js/encodeURIComponent (db/current-world db)) "/" (js/encodeURIComponent name)))

(defn body-url [body]
  (logic/with-body (.-pathname js/location) (.-search js/location) body))

(rf/reg-event-fx
 :open-detail
 (fn [{:keys [db]} [_ name]]
   {:db (assoc db :detail-body name :detail-events [] :detail-chip :all :detail-text ""
               :detail-stream nil :attention-outstanding (db/body-outstanding (:state db) name)
               :attention-error nil :detail-notices [])
    :replace-url (body-url name)
    :fx [[:start-timers [[:detail-log log-ms [:poll-detail-log]]]]
         [:dispatch [:poll-detail-log]]]}))

;; The effects of dropping the popup (also used when the world changes): db without the body's state, timers stopped.
(defn close-fx [db]
  {:db (assoc db :detail-body nil :detail-events [])
   :stop-timers [:detail-log]})

(rf/reg-event-fx
 :close-detail
 (fn [{:keys [db]} _]
   (assoc (close-fx db) :replace-url (body-url nil))))

;; Esc closes the popup
(rf/reg-event-fx
 :escape
 (fn [{:keys [db]} _]
   (if (:detail-body db) {:dispatch [:close-detail]} {})))

(rf/reg-event-fx
 :poll-detail-log
 (fn [{:keys [db]} _]
   (when-let [name (:detail-body db)]
     (let [{:keys [stream-id seq]} (:detail-stream db)
           query (cond-> {:limit log-limit}
                   stream-id (assoc :stream-id stream-id :after (or seq 0)))]
       {:fetch-edn {:key :detail-log :url (logic/api-url (str "/api/events/" (body-path db name)) nil query)
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
     {:post-edn {:url (str "/api/attention/" (body-path db name) "/resolve")
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
(rf/reg-event-db :detail-debug? (fn [db [_ on?]] (assoc db :detail-debug? on?)))
(rf/reg-event-db :detail-text (fn [db [_ text]] (assoc db :detail-text text)))

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
