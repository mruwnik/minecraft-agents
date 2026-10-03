(ns dashboard.ui.bodies
  "The Bodies page: one card per engine body, trouble first, and the detail modal."
  (:require [re-frame.core :as rf]
            [dashboard.ui.livecards :as live]
            [dashboard.ui.stills :as stills]
            [dashboard.ui.trouble :as trouble]))

(def status-label {:manual "manual" :trouble "in trouble" :working "working" :idle "idle" :offline "offline"})

(defn short-label [s] (trouble/short-name s))

(defn status-pill [status]
  [:span.pill.st {:class (name status)} (status-label status)])

(defn bar [kind value]
  (let [v (when (number? value) (max 0 (min 20 value)))
        low? (and v (<= v 6))]
    [:div.vital {:title (str (name kind) " " (if v (js/Math.round v) "?") " / 20")}
     [:span.vlabel (if (= kind :health) "hp" "food")]
     [:div.bar {:class (when low? "low")}
      [:div.fill {:class (name kind) :style {:width (str (* 5 (or v 0)) "%")}}]]
     [:span.vnum (if v (js/Math.round v) "-")]]))

(defn page-flags [] (live/flags (.-search js/location)))

(defn preview [{:keys [name thumb thumb-age status offline pose-mtime]}]
  (let [flags (page-flags)
        mode (live/current-mode flags)
        view (live/card-view mode (live/card-plan flags status) (some? thumb) (stills/record name))]
    [:div.preview
     (case view
       :blank nil
       :img [:img {:src thumb :alt (str "view of " name) :loading "lazy" :draggable false}]
       :noview [:div.noview "no view"]
       :live [live/live-preview {:name name :flags flags}]
       :still [stills/still-canvas {:name name :pose-mtime pose-mtime}])
     (when (= status :offline) [:span.offline-tag offline])
     (when thumb-age [:span.age-tag {:title "age of this view"} thumb-age])
     [:div.overlay [:span.bname name] (when-not (= status :offline) [status-pill status])]]))

(defn body-card [{:keys [name status reason manual severity mine? health food job event event-age event-attention] :as card}]
  ^{:key name}
  [:div.bcard {:class [(clojure.core/name status) (when mine? "mine") (when reason (str "sev-" (clojure.core/name severity)))]
               :tabIndex 0 :role "button"
               :on-click #(rf/dispatch [:open-detail name])
               :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [:open-detail name]))}
   [preview card]
   [:div.binfo
    [:div.vitals [bar :health health] [bar :food food]]
    [:div.line.job {:title job} (or job [:span.dim "no job"])]
    [:div.line.event {:class (when (#{:notice :required} event-attention) (clojure.core/name event-attention)) :title event}
     (if event [:<> [:span.text event] [:span.age event-age]] [:span.dim "no events"])]
    (when manual [:div.reason.manual manual])
    (when reason [:div.reason {:class (clojure.core/name severity)} reason])]])

(defn page []
  (let [cards @(rf/subscribe [:cards])
        foreign @(rf/subscribe [:foreign-count])]
    [:main.page-bodies
     (if (empty? cards)
       [:div.empty "no engine bodies in this world"]
       (into [:div.cards] (map body-card) cards))
     (when (pos? foreign)
       [:div.footnote (str foreign " folders without an engine are not shown")])]))
