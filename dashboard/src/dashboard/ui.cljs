(ns dashboard.ui
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [re-frame.core :as rf]
            [dashboard.ui.bodies :as bodies]
            [dashboard.ui.detail-events :as detail-events]
            [dashboard.ui.events]
            [dashboard.ui.logic :as logic]
                        [dashboard.ui.pages.blueprints :as blueprints]
            [dashboard.ui.pages.jobs :as jobs]
            [dashboard.ui.pages.plans :as plans]
            [dashboard.ui.pages.villagers :as villagers]
            [dashboard.ui.pages.villages :as villages]
            [dashboard.ui.subs]
            [dashboard.ui.views :as views]))

(defn page-content [page]
  (case page
    :map [views/map-page]
    :plans [plans/page]
    :jobs [jobs/page]
    :villages [villages/page]
    :villagers [villagers/page]
    :blueprints [blueprints/page]
    [bodies/page]))

(defn root [page]
  [views/shell page [page-content page]])

(defn on-key [e]
  (when (= "Escape" (.-key e)) (rf/dispatch [:escape])))

(defn init []
  (let [page (logic/page-for-path (.-pathname js/location))]
    (.addEventListener js/document "keydown" on-key)
    (.addEventListener js/window "message" detail-events/on-message)
    (rf/dispatch-sync [:init (.-search js/location) page])
    (case page
      :plans (rf/dispatch [:plans/start true])
      :map (rf/dispatch [:plans/start false])
      nil)
    (.render (rdom/create-root (js/document.getElementById "app")) (r/as-element [root page]))))
