(ns dashboard.ui.bodies
  "The Bodies page: one card per engine body, trouble first, and the detail modal."
  (:require [re-frame.core :as rf]
            [dashboard.ui.livecards :as live]
            [dashboard.ui.logic :as logic]
            [dashboard.ui.statusfilter :as statusfilter]
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

(defn preview [{:keys [name world thumb thumb-age status offline]}]
  (let [flags (page-flags)
        mode (live/current-mode flags)
        view (live/card-view mode (live/card-plan flags status) (some? thumb))]
    [:div.preview
     (case view
       :blank nil
       :img [:img {:src thumb :alt (str "view of " name) :loading "lazy" :draggable false}]
       :noview [:div.noview "no view"]
       :live [live/live-preview {:name name :world world :flags flags}])
     (when (= status :offline) [:span.offline-tag offline])
     (when thumb-age [:span.age-tag {:title "age of this view"} thumb-age])
     [:div.overlay [:span.bname name] (when-not (= status :offline) [status-pill status])]]))

(defn show-on-map
  "Opens the map page centred on the body; disabled, with the reason on hover, while no position is known."
  [name pos world]
  [:span {:on-click #(.stopPropagation %) :on-key-down #(.stopPropagation %)}
   [:button.showmap
    {:disabled (nil? pos)
     :title (if pos (str "show " name " on the map") "no known position yet")
     :on-click #(set! (.-href js/location) (logic/map-show-url world name))}
    "show on map"]])

(defn body-card [{:keys [name status reason manual severity mine? health food job parked goal goal-by goal-age event event-age event-attention] :as card}]
  ^{:key name}
  [:div.bcard {:class [(clojure.core/name status) (when mine? "mine") (when reason (str "sev-" (clojure.core/name severity)))]
               :tabIndex 0 :role "button"
               :on-click #(rf/dispatch [:open-detail name])
               :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [:open-detail name]))}
   [preview card]
   [:div.binfo
    [:div.vitals [bar :health health] [bar :food food]]
    (when goal [:div.line.goal {:title (str goal (when goal-by (str " (set by " goal-by ")")))} [:span.text goal] [:span.age goal-age]])
    [:div.line.job {:title job} (or job [:span.dim "no job"])]
    (when parked [:div.line.parked {:title parked} parked])
    [:div.line.event {:class (when (#{:notice :required} event-attention) (clojure.core/name event-attention)) :title event}
     (if event [:<> [:span.text event] [:span.age event-age]] [:span.dim "no events"])]
    (when manual [:div.reason.manual manual])
    (when reason [:div.reason {:class (clojure.core/name severity)} reason])
    [show-on-map name (get @(rf/subscribe [:body-positions]) name) @(rf/subscribe [:current-world])]]])

(defn page []
  (let [cards @(rf/subscribe [:cards])
        foreign @(rf/subscribe [:foreign-count])
        pressed @(rf/subscribe [:status-filter])
        engine-bodies? (seq (:engine @(rf/subscribe [:split-bodies])))]
    [:main.page-bodies
     (cond
       (seq cards) (into [:div.cards] (map body-card) cards)
       (and (seq pressed) engine-bodies?) [:div.empty (statusfilter/empty-text pressed)]
       :else [:div.empty "no engine bodies in this world"])
     (when (pos? foreign)
       [:div.footnote (str foreign " folders without an engine are not shown")])]))
