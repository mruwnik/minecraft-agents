(ns dashboard.ui
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [re-frame.core :as rf]
            [dashboard.ui.events]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.pages.blueprints :as blueprints]
            [dashboard.ui.pages.villagers :as villagers]
            [dashboard.ui.pages.villages :as villages]
            [dashboard.ui.subs]
            [dashboard.ui.views :as views]))

(defn page-component [page]
  (case page
    :villages villages/page
    :villagers villagers/page
    :blueprints blueprints/page
    views/main-page))

(defn root [page]
  [:<>
   [views/header page]
   [(page-component page)]])

(defn init []
  (let [page (logic/page-for-path (.-pathname js/location))]
    (rf/dispatch-sync [:init (.-search js/location) page])
    (.render (rdom/create-root (js/document.getElementById "app")) (r/as-element [root page]))))
