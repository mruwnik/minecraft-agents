(ns dashboard.ui.page-data
  "Events and subscriptions of the villages, villagers and blueprints pages: fetch, poll, keep the answer."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
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

;; ---------------------------------------------------------------- blueprints (global)
(def blueprints-ms 5000)
(def draft-name "__draft__")

(defn selected-from-search []
  (.get (js/URLSearchParams. (.-search js/location)) "name"))

(defn url-with-name [name]
  (let [url (js/URL. (.-href js/location))]
    (.set (.-searchParams url) "name" name)
    (str (.-pathname url) (.-search url))))

(rf/reg-event-fx
 :blueprints/start
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:blueprints :selected] (selected-from-search))
    :fx [[:dispatch [:blueprints/fetch]]
         [:start-timers [[:blueprints blueprints-ms [:blueprints/fetch]]]]]}))

(rf/reg-event-fx
 :blueprints/fetch
 (fn [_ _]
   {:fetch-json {:key :blueprints :url "/api/blueprints" :on-ok [:blueprints/ok] :on-err [:blueprints/err]}}))

(rf/reg-event-fx
 :blueprints/ok
 (fn [{:keys [db]} [_ data]]
   (let [first-name (:name (first (:blueprints data)))
         pick? (and (nil? (get-in db [:blueprints :selected])) first-name)]
     (cond-> {:db (update db :blueprints assoc :library data :clock (clock-now) :failed nil)}
       pick? (assoc :dispatch [:blueprints/select first-name])))))

(rf/reg-event-db :blueprints/err (fn [db [_ message]] (assoc-in db [:blueprints :failed] message)))

(rf/reg-event-fx
 :blueprints/select
 (fn [{:keys [db]} [_ name]]
   {:db (assoc-in db [:blueprints :selected] name)
    :replace-url (url-with-name name)}))

(rf/reg-event-db :blueprints/draft-plan (fn [db [_ text]] (update db :blueprints assoc :draft-plan text :draft-status "Edited draft: validate to update the preview. Existing builds are unchanged.")))
(rf/reg-event-db :blueprints/draft-stock (fn [db [_ text]] (assoc-in db [:blueprints :draft-stock] text)))

(rf/reg-event-db
 :blueprints/copy-selected
 (fn [db _]
   (let [{:keys [library selected draft]} (:blueprints db)
         detail (if (= selected draft-name) draft (first (filter #(= selected (:name %)) (:blueprints library))))]
     (if-let [document (:document detail)]
       (update db :blueprints assoc
               :draft-plan (js/JSON.stringify (clj->js document) nil 2)
               :draft-status "Editable copy. Saved build manifests remain unchanged.")
       (assoc-in db [:blueprints :draft-status] "No editable v2 source selected")))))

(defn parse-json [text] (js/JSON.parse text))

(rf/reg-event-fx
 :blueprints/preview-draft
 (fn [{:keys [db]} _]
   (let [{:keys [draft-plan draft-stock]} (:blueprints db)]
     (try
       (let [plan (parse-json draft-plan)
             stock (when-not (str/blank? draft-stock) (parse-json draft-stock))]
         {:post-json {:url "/api/blueprint-preview" :body #js {:plan plan :stock stock}
                      :on-ok [:blueprints/draft-ok] :on-err [:blueprints/draft-err]
                      :on-unsupported [:blueprints/draft-unsupported]}})
       (catch :default e
         {:db (assoc-in db [:blueprints :draft-status] (ex-message e))})))))

(rf/reg-event-db
 :blueprints/draft-ok
 (fn [db [_ detail]]
   (update db :blueprints assoc :draft detail :selected draft-name
           :draft-status (if (seq (:errors detail))
                           (str/join " · " (:errors detail))
                           (str "Validated " (:hash detail) "; "
                                (if (= "representative" (:palette detail)) "illustrative palette" "declared stock allocation")
                                ". No build started.")))))

(rf/reg-event-db :blueprints/draft-err (fn [db [_ message]] (assoc-in db [:blueprints :draft-status] message)))

(rf/reg-event-fx
 :blueprints/draft-unsupported
 (fn [{:keys [db]} _]
   {:db (assoc-in db [:blueprints :draft-status] "unsupported for engine bodies: blueprint preview")
    :fx [[:dispatch [:unsupported "blueprint preview"]]]}))

(rf/reg-event-fx
 :blueprints/download-draft
 (fn [{:keys [db]} _]
   (try
     (let [plan (parse-json (get-in db [:blueprints :draft-plan]))
           id (.-id plan)]
       {:download-json {:filename (str (if (and (string? id) (re-matches #"[a-z0-9-]+" id)) id "draft") ".blueprint.json")
                        :text (str (js/JSON.stringify plan nil 2) "\n")}})
     (catch :default e
       {:db (assoc-in db [:blueprints :draft-status] (ex-message e))}))))

(rf/reg-sub :blueprints (fn [db _] (:blueprints db)))
