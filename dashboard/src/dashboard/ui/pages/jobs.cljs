(ns dashboard.ui.pages.jobs
  "The Jobs page: every job namespace of the engine, by category, with its doc, default args and who runs it."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.ui.api]
            [dashboard.ui.jobsmodel :as jm]))

(def jobs-ms 5000)

(rf/reg-event-fx
 :jobs/start
 (fn [_ _]
   {:fx [[:dispatch [:jobs/fetch]]
         [:start-timers [[:jobs jobs-ms [:jobs/fetch]]]]]}))

(rf/reg-event-fx
 :jobs/fetch
 (fn [_ _]
   {:fetch-json {:key :jobs :url "/api/jobs" :on-ok [:jobs/ok] :on-err [:jobs/err]}}))

(rf/reg-event-db
 :jobs/ok
 (fn [db [_ data]] (update db :jobs assoc :items (vec (:jobs data)) :failed nil :loaded? true)))

(rf/reg-event-db :jobs/err (fn [db [_ message]] (assoc-in db [:jobs :failed] message)))
(rf/reg-event-db :jobs/filter (fn [db [_ text]] (assoc-in db [:jobs :filter] text)))

(rf/reg-sub :jobs (fn [db _] (:jobs db)))

(defn job-doc [job]
  (let [open? (r/atom false)]
    (fn [job]
      (let [long? (jm/long-doc? job)]
        [:div.job-docs
         [:div.job-text {:class (when (and long? (not @open?)) "clamped")}
          (for [[i p] (map-indexed vector (jm/body-paragraphs job))]
            ^{:key i} [:p.job-doc p])]
         (when long?
           [:button.linkish {:on-click #(swap! open? not)} (if @open? "show less" "show more")])]))))

(defn job-card [{:keys [id name args file error] :as job}]
  ^{:key id}
  [:article.panel.job-card
   [:div.job-head
    [:h3.job-name name]
    [:span.job-id id]
    (for [{:keys [kind text]} (jm/badges job)]
      ^{:key kind} [:span.pill.jobbadge {:class (clojure.core/name kind)} text])]
   (if error [:div.err error] [job-doc job])
   (when args [:pre.job-args args])
   [:div.job-file file]])

(defn group [{:keys [category jobs]}]
  ^{:key category}
  [:section.job-group
   [:h2 (str category " (" (count jobs) ")")]
   (into [:div.job-grid] (map job-card) jobs)])

(defn page []
  (r/create-class
   {:component-did-mount #(rf/dispatch [:jobs/start])
    :reagent-render
    (fn []
      (let [{:keys [items filter failed loaded?]} @(rf/subscribe [:jobs])
            groups (jm/grouped items filter)
            shown (reduce + (map (comp count :jobs) groups))]
        [:main.page.jobs-page
         [:div.toolbar
          [:input {:type "search" :placeholder "filter by name, doc, args or body" :value (or filter "")
                   :on-change #(rf/dispatch [:jobs/filter (.. % -target -value)])}]
          [:span.spacer]
          [:span.muted (or failed (if loaded? (jm/count-text shown (count items) filter) "loading..."))]]
         (if (empty? groups)
           [:div.empty (if loaded? "No jobs match." "loading...")]
           (into [:div] (map group) groups))]))}))
