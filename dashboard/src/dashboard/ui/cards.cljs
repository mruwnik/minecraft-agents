(ns dashboard.ui.cards
  (:require [re-frame.core :as rf]
            [dashboard.ui.logic :as logic]))

(defn notice [text]
  (when text [:div.muted.notice text]))

(defn place-row [selected {:keys [name kind x y z by note]}]
  (let [sel? (= selected {:kind :place :name name})]
    ^{:key name}
    [:div.place {:class (when sel? "sel")
                 :on-click #(do (rf/dispatch [:select {:kind :place :name name}])
                                (rf/dispatch [:center-on x z]))}
     [:span.name name] [:span.muted (str " " kind " " x ", " y ", " z)]
     (when by [:span.muted (str " by " by)])
     (when note [:div.muted.note note])]))

(defn places-panel []
  (let [places @(rf/subscribe [:places])
        selected @(rf/subscribe [:selected])]
    [:section
     [:h2 (str "places (" (count places) ")")]
     (into [:div#places] (map #(place-row selected %) places))]))
