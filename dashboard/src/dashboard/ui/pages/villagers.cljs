(ns dashboard.ui.pages.villagers
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.ui.page-data]
            [dashboard.ui.logic :as logic]
            [dashboard.villagers-view :as vv]))

(defn offer-row [offer]
  (let [{:keys [input output uses]} (vv/offer-text offer)]
    [:div.offer [:span input] [:span output] [:span uses]]))

(defn detail [now {:keys [offers place world] :as v}]
  [:div.detail
   (for [[i {:keys [text cls]}] (map-indexed vector (vv/detail-lines now v))]
     ^{:key i} [:div {:class cls} text])
   (when (:name place)
     [:div "Explicit place association: " [:a {:href (logic/api-url "/" world {:farm (:name place)})} (:name place)]])
   (if offers
     [:<>
      [:h3 (vv/offers-heading now offers)]
      (for [[i o] (map-indexed vector (:items offers))] ^{:key i} [offer-row o])]
     nil)])

(defn villager-card [now v]
  [:details.card
   [:summary [:span.name (str "villager " (or (:uuid v) (:key v)))]
    [:span (str (:world v) " · " (:dimension v))]
    [:span (str "seen " (vv/ago now (:lastSeenAt v)) " by " (:lastSeenBy v))]
    [:span (str "expires in " (js/Math.ceil (/ (max 0 (- (:until v) now)) 1000)) "s")]]
   [detail now v]])

(defn page []
  (r/create-class
   {:component-did-mount #(rf/dispatch [:villagers/start])
    :reagent-render
    (fn []
      (let [{:keys [roster filter clock failed]} @(rf/subscribe [:villagers])
            now @(rf/subscribe [:entity-now])
            all (vv/live-records roster now)
            shown (vv/filter-records all filter)]
        [:main.page
         [:div.toolbar
          [:input {:type "search" :placeholder "filter UUID or transient identity" :value (or filter "")
                   :on-change #(rf/dispatch [:villagers/filter (.. % -target -value)])}]
          [:button {:on-click #(rf/dispatch [:villagers/fetch])} "refresh"]
          [:small.muted "Villagers observed in this world and dimension during the last 2 minutes. Observations expire; disappearance does not imply death."]
          [:span.spacer]
          [:span.muted (or failed (if clock (vv/status-text (count all) clock) "loading…"))]]
         (for [[i message] (map-indexed vector (vv/capability-text (:sources roster)))]
           ^{:key i} [:div.muted message])
         (when (:truncated? roster) [:div.muted "Entity observations are truncated by the configured limit."])
         (if (empty? shown)
           [:div.empty (if (seq all) "No villagers match this filter." "No villagers have been observed yet.")]
           (into [:div] (for [v shown] ^{:key (or (:uuid v) (:key v))} [villager-card now v])))]))}))
