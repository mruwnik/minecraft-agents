(ns dashboard.ui.pages.villages
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.ui.page-data]
            [dashboard.villages-view :as vv]
            [dashboard.ui.logic :as logic]))

(defn labelled [{:keys [text muted cls]}]
  [:div {:class (cond muted "muted" cls cls)} text])

(defn housing-box [v]
  (into [:section.box [:h2 "housing"]] (map labelled (vv/housing-lines v))))

(defn roles-box [v]
  (let [rows (vv/role-rows v)]
    (into [:section.box [:h2 "required roles"]]
          (if (empty? rows)
            [[:div.muted "No distinct role requirements in the saved population intent."]]
            (for [{:keys [title count status notes]} rows]
              ^{:key title}
              [:<>
               [:div.role [:span title] [:span count] [:span {:class (str "state " status)} status]]
               (for [[i n] (map-indexed vector notes)] ^{:key i} [:small.muted n])])))))

(defn workspaces-box [v]
  (into [:section.box [:h2 "coordinate-bound workspaces"]]
        (if (empty? (:workspaces v))
          [[:div.muted "No station location constraints are in the saved village intent."]]
          (for [w (:workspaces v) :let [{:keys [title where status status-text association]} (vv/workspace-row w)]]
            ^{:key (:id w)}
            [:div.workspace
             [:strong title] [:div.muted where]
             [:div {:class (str "state " status)} status-text]
             [:small.muted association]]))))

(defn evidence-box [now v]
  (into [:section.box [:h2 "evidence"]] (map labelled (vv/evidence-lines now v))))

(defn village-card [now v]
  [:article.card
   [:div.head
    [:span.name (:name v)]
    [:span.muted (str (:x v) ", " (:y v) ", " (:z v))]
    [:span.state (vv/population-text v)]
    [:span {:class (str "state " (:state v))} (vv/state-text v)]]
   [:div.detail
    (when (= :incomplete (:geometry v)) [:div.muted "Geometry is incomplete; this marker does not prove construction or occupancy."])
    [:div.grid [housing-box v] [roles-box v] [workspaces-box v] [evidence-box now v]]
    [:div.links
     (when-let [id (:planId v)]
       [:a {:href (logic/api-url "/plans" (:world v) {:plan id})} "village plan"])
     [:a {:href (logic/api-url "/" (:world v) {})} (vv/map-link-text v)]
     (when-let [id (get-in v [:population :blueprintId])]
       [:a {:href (str "/blueprints?name=" (js/encodeURIComponent id))} "blueprint"])
     (for [m (:members v)]
       ^{:key (:uuid m)}
       [:a {:href (logic/api-url "/villagers" (:world v) {:uuid (:uuid m)})} (vv/member-text now m)])]]])

(defn page []
  (r/create-class
   {:component-did-mount #(rf/dispatch [:villages/start])
    :reagent-render
    (fn []
      (let [{:keys [items filter error clock failed]} @(rf/subscribe [:villages])
            shown (vv/filter-villages items filter)
            now (js/Date.now)]
        [:main.page
         [:div.toolbar
          [:input {:type "search" :placeholder "filter village, profession or role" :value (or filter "")
                   :on-change #(rf/dispatch [:villages/filter (.. % -target -value)])}]
          [:button {:on-click #(rf/dispatch [:villages/fetch])} "refresh"]
          [:small.muted "Village plans and saved observations. This page does not query or move agents."]
          [:span.spacer]
          [:span.muted (or failed (if clock (vv/status-text (count items) clock error) "loading…"))]]
         (if (empty? shown)
           [:div.empty (vv/empty-text (count items))]
           (into [:div] (for [[i v] (map-indexed vector shown)] ^{:key (str (:name v) i)} [village-card now v])))]))}))
