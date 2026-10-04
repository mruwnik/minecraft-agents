(ns dashboard.ui.plans-events
  "State of the plans: the list (the Plans page and the map both draw it) polled every 10 s, and the selected plan's
  full comparison. ?plan=<name> in the URL is the selection."
  (:require [re-frame.core :as rf]
            [dashboard.ui.api]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.plansmodel :as pm]))

(def plans-ms 10000)

(defn view-state
  "What the plan detail is drawn as: :view (:bird, or :layer for the chosen y) and :mode (\"plan\", \"world\" or \"diff\")."
  [plans]
  {:view (or (:view plans) pm/default-view) :mode (or (:mode plans) pm/default-mode)})

(defn with-layer [plans y] (assoc plans :layer y :view :layer))
(defn with-view [plans view] (assoc plans :view view))
(defn with-mode [plans mode] (assoc plans :mode mode))

;; the view and the mode are the viewer's, kept while moving between plans and worlds
(defn with-selected [plans name]
  (assoc plans :selected name :detail nil :detail-failed nil :layer nil :element nil))

(defn after-world-change [plans]
  (select-keys plans [:active? :selected-wanted? :view :mode]))

(defn selected-from-search []
  (.get (js/URLSearchParams. (.-search js/location)) "plan"))

(defn url-with-plan [plan]
  (let [url (js/URL. (.-href js/location))]
    (.set (.-searchParams url) "plan" plan)
    (str (.-pathname url) (.-search url))))

(rf/reg-fx :navigate (fn [url] (set! (.-href js/location) url)))

(rf/reg-event-fx
 :plans/start
 (fn [{:keys [db]} [_ with-selection?]]
   {:db (update db :plans assoc :active? true :selected-wanted? (boolean with-selection?)
                    :selected (when with-selection? (selected-from-search)))
    :fx [[:dispatch [:plans/fetch]]
         [:start-timers [[:plans plans-ms [:plans/fetch]]]]]}))

(rf/reg-event-fx
 :plans/fetch
 (fn [{:keys [db]} _]
   (let [world (:world db)
         selected (get-in db [:plans :selected])]
     {:fetch-json {:key :plans :url (logic/api-url "/api/plans" world {})
                   :on-ok [:plans/ok world] :on-err [:plans/err world]}
      :fx [(when selected [:dispatch [:plans/fetch-detail selected]])]})))

(rf/reg-event-fx
 :plans/fetch-detail
 (fn [{:keys [db]} [_ name]]
   (let [world (:world db)]
     {:fetch-json {:key :plan-detail :url (logic/api-url (str "/api/plan/" (js/encodeURIComponent name)) world {})
                   :on-ok [:plans/detail-ok world name] :on-err [:plans/detail-err world name]}})))

(rf/reg-event-fx
 :plans/ok
 (fn [{:keys [db]} [_ world data]]
   (if (not= world (:world db))
     {}
     (let [items (pm/sort-plans (:plans data))
           first-name (:id (:plan (first (pm/plan-tree items))))
           pick? (and (get-in db [:plans :selected-wanted?]) (nil? (get-in db [:plans :selected])) first-name)]
       (cond-> {:db (update db :plans assoc :items items :conflicts (:conflicts data) :file-errors (:errors data) :failed nil)}
         pick? (assoc :dispatch [:plans/select first-name]))))))

(rf/reg-event-db :plans/err (fn [db [_ world message]] (if (not= world (:world db)) db (assoc-in db [:plans :failed] message))))

(rf/reg-event-db
 :plans/detail-ok
 (fn [db [_ world name data]]
   (if (and (= world (:world db)) (= name (get-in db [:plans :selected])))
     (update db :plans assoc :detail data :detail-failed nil)
     db)))

(rf/reg-event-db
 :plans/detail-err
 (fn [db [_ world name message]]
   (if (and (= world (:world db)) (= name (get-in db [:plans :selected])))
     (update db :plans assoc :detail nil :detail-failed message)
     db)))

(rf/reg-event-fx
 :plans/select
 (fn [{:keys [db]} [_ name]]
   {:db (update db :plans with-selected name)
    :replace-url (url-with-plan name)
    :fx [[:dispatch [:plans/fetch-detail name]]]}))

(rf/reg-event-db :plans/layer (fn [db [_ y]] (update db :plans with-layer y)))
(rf/reg-event-db :plans/view (fn [db [_ view]] (update db :plans with-view view)))
(rf/reg-event-db :plans/mode (fn [db [_ mode]] (update db :plans with-mode mode)))

;; clicking the selected element again clears the selection; selecting one shows the layer where it starts
(rf/reg-event-db
 :plans/element
 (fn [db [_ id y]]
   (let [clear? (= id (get-in db [:plans :element]))]
     (update db :plans assoc :element (when-not clear? id) :layer (if (or clear? (nil? y)) (get-in db [:plans :layer]) y)))))

(rf/reg-event-fx
 :plans/open
 (fn [{:keys [db]} [_ name]]
   {:navigate (str (logic/with-world "/plans" (:world db))
                   (if (:world db) "&" "?") "plan=" (js/encodeURIComponent name))}))

;; a new world: the old world's plans are gone; refetch when a page that shows them is open
(rf/reg-event-fx
 :plans/world-changed
 (fn [{:keys [db]} _]
   (let [active? (get-in db [:plans :active?])]
     {:db (update db :plans after-world-change)
      :fx [(when active? [:dispatch [:plans/fetch]])]})))

(rf/reg-sub :plans (fn [db _] (:plans db)))
(rf/reg-sub :plan-view-state :<- [:plans] (fn [plans _] (view-state plans)))
(rf/reg-sub :plan-items :<- [:plans] (fn [p _] (:items p)))
(rf/reg-sub :plan-conflicts :<- [:plans] (fn [p _] (:conflicts p)))

(rf/reg-sub
 :plan-layer-y
 :<- [:plans]
 (fn [{:keys [detail layer]} _]
   (let [ys (set (map :y (:layers detail)))]
     (if (contains? ys layer) layer (pm/default-layer-y (:layers detail))))))
