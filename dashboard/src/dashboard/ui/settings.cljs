(ns dashboard.ui.settings
  "The body popup's settings panel: the keys off their code default with their layer, the game rate, and a field
  to set or unset a key in the body's settings.edn (the engine validates and reloads)."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [dashboard.edn :as edn]
            [dashboard.ui.detail-events :refer [body-path]]))

(defn changed-rows
  "[{:key :value :layer}] of the keys whose layer is not :code, sorted by key."
  [view]
  (->> (:settings view)
       (keep (fn [[k v]] (when (not= :code (:layer v)) {:key k :value (:value v) :layer (:layer v)})))
       (sort-by (comp str :key))
       vec))

(defn rate-text
  "The game rate line: \"20 ticks/s (assumed), physics step 50 ms\", with \", frozen\" when frozen."
  [{:keys [game-rate rate-source physics-ms frozen?]}]
  (when game-rate
    (str game-rate " ticks/s (" (name (or rate-source :assumed)) "), physics step " physics-ms " ms" (when frozen? ", frozen"))))

(defn set-request
  "The request for the key and value drafts (text): {:op :set :key :value}, or {:error message} when either is not EDN."
  [key-text value-text]
  (let [parse (fn [text] (try {:value (edn/one-form text)} (catch :default _ nil)))
        k (parse key-text)
        v (parse value-text)]
    (cond
      (not (keyword? (:value k))) {:error "the key must be a keyword such as :jobs.items.smelt/wait-ms"}
      (not v) {:error "the value must be EDN, such as 5000 or \"text\""}
      :else {:op :set :key (:value k) :value (:value v)})))

(defn url [db name] (str "/api/settings/" (body-path db name)))

(rf/reg-event-fx
 :settings-load
 (fn [{:keys [db]} _]
   (when-let [name (:detail-body db)]
     {:fetch-edn {:key :settings :url (url db name) :on-ok [:settings-loaded name] :on-err [:settings-error name]}})))

(rf/reg-event-db
 :settings-loaded
 (fn [db [_ name view]]
   (if (= name (:detail-body db)) (update db :settings assoc :view view :error nil) db)))

(rf/reg-event-db
 :settings-error
 (fn [db [_ name error]]
   (if (= name (:detail-body db)) (update db :settings assoc :error (str error)) db)))

(rf/reg-event-db :settings-draft (fn [db [_ field text]] (assoc-in db [:settings field] text)))

(rf/reg-event-fx
 :settings-act
 (fn [{:keys [db]} [_ request]]
   (when-let [name (:detail-body db)]
     (if (:error request)
       {:db (assoc-in db [:settings :error] (:error request))}
       {:post-edn {:url (url db name) :body request :on-ok [:settings-acted name] :on-err [:settings-error name]}}))))

(rf/reg-event-fx
 :settings-acted
 (fn [{:keys [db]} [_ name _]]
   (if (= name (:detail-body db))
     {:db (assoc-in db [:settings :error] nil) :dispatch [:settings-load]}
     {})))

(rf/reg-sub :settings (fn [db _] (:settings db)))

(defn view-panel []
  (let [{:keys [view error key value]} @(rf/subscribe [:settings])
        rows (changed-rows view)]
    [:section.dpanel.settings
     [:h3 "Settings"]
     (if-let [rate (rate-text view)] [:div.dim rate] [:div.dim "no settings read yet"])
     (if (empty? rows)
       [:div.dim "every key at its code default"]
       (into [:div.joblist]
             (map (fn [{:keys [key value layer]}]
                    ^{:key key}
                    [:div.job [:span.mono (str key " " (pr-str value))] [:span.dim (name layer)]
                     (when (= :body layer)
                       [:button {:title "remove from the body's settings.edn"
                                 :on-click #(rf/dispatch [:settings-act {:op :unset :key key}])} "unset"])]))
             rows))
     [:div.setrow
      [:input {:type "text" :placeholder ":ns/key" :value (or key "") :spell-check false
               :on-change #(rf/dispatch [:settings-draft :key (.. % -target -value)])}]
      [:input {:type "text" :placeholder "value (EDN)" :value (or value "") :spell-check false
               :on-change #(rf/dispatch [:settings-draft :value (.. % -target -value)])}]
      [:button {:on-click #(rf/dispatch [:settings-act (set-request (or key "") (or value ""))])} "set"]
      [:button {:title "re-read the world and body files" :on-click #(rf/dispatch [:settings-act {:op :reload}])} "reload"]]
     (when error [:div.attention-error error])]))

(defn panel
  "The panel; it reads the engine's settings when the popup opens."
  []
  (r/create-class
   {:component-did-mount #(rf/dispatch [:settings-load])
    :reagent-render view-panel}))
