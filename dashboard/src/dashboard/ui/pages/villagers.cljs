(ns dashboard.ui.pages.villagers
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.ui.page-data]
            [dashboard.villagers-view :as vv]))

(defn offer-row [offer]
  (let [{:keys [input output uses]} (vv/offer-text offer)]
    [:div.offer [:span input] [:span output] [:span uses]]))

(defn detail [now {:keys [offers place] :as v}]
  [:div.detail
   (for [[i {:keys [text cls]}] (map-indexed vector (vv/detail-lines now v))]
     ^{:key i} [:div {:class cls} text])
   (when (:name place)
     [:div "Explicit place association: " [:a {:href (str "/?farm=" (js/encodeURIComponent (:name place)))} (:name place)]])
   (if offers
     [:<>
      [:h3 (vv/offers-heading now offers)]
      (for [[i o] (map-indexed vector (:items offers))] ^{:key i} [offer-row o])]
     [:div "No merchant offers observed yet."])])

(defn villager-card [now v]
  (let [[a b c d] (vv/summary-lines now v)]
    [:details.card
     [:summary [:span.name a] [:span b] [:span c] [:span d]]
     [detail now v]]))

(defn page []
  (r/create-class
   {:component-did-mount #(rf/dispatch [:villagers/start])
    :reagent-render
    (fn []
      (let [{:keys [roster filter clock failed]} @(rf/subscribe [:villagers])
            all (vv/records roster)
            shown (vv/filter-records all filter)
            now (js/Date.now)]
        [:main.page
         [:div.toolbar
          [:input {:type "search" :placeholder "filter UUID, profession, place, item or enchantment" :value (or filter "")
                   :on-change #(rf/dispatch [:villagers/filter (.. % -target -value)])}]
          [:button {:on-click #(rf/dispatch [:villagers/fetch])} "refresh"]
          [:small.muted "Last sightings age naturally; unobserved does not mean dead."]
          [:span.spacer]
          [:span.muted (or failed (if clock (vv/status-text (count all) clock) "loading…"))]]
         (if (empty? shown)
           [:div.empty (if (seq all) "No villagers match this filter." "No villagers have been observed yet.")]
           (into [:div] (for [v shown] ^{:key (:uuid v)} [villager-card now v])))]))}))
