(ns dashboard.ui.bodies
  "The Bodies page: one card per engine body, trouble first, and the detail modal."
  (:require [re-frame.core :as rf]))

(def status-label {:trouble "in trouble" :working "working" :idle "idle" :offline "offline"})

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

(defn preview [{:keys [name thumb status offline]}]
  [:div.preview
   (if thumb
     [:img {:src thumb :alt (str "view of " name) :loading "lazy" :draggable false}]
     [:div.noview "no view"])
   (when (= status :offline) [:span.offline-tag offline])
   [:div.overlay [:span.bname name] [status-pill status]]])

(defn body-card [{:keys [name status reason severity health food job event event-age event-level] :as card}]
  ^{:key name}
  [:div.bcard {:class [(clojure.core/name status) (when reason (str "sev-" (clojure.core/name severity)))]
               :tabIndex 0 :role "button"
               :on-click #(rf/dispatch [:open-detail name])
               :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [:open-detail name]))}
   [preview card]
   [:div.binfo
    [:div.vitals [bar :health health] [bar :food food]]
    [:div.line.job {:title job} (or job [:span.dim "no job"])]
    [:div.line.event {:class event-level :title event}
     (if event [:<> [:span.text event] [:span.age event-age]] [:span.dim "no events"])]
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

(defn detail-modal []
  (when-let [name @(rf/subscribe [:detail-body])]
    (let [card (first (filter #(= name (:name %)) @(rf/subscribe [:cards])))]
      [:div.modal-back {:on-click #(rf/dispatch [:close-detail])}
       [:div.modal {:on-click #(.stopPropagation %)}
        [:div.modal-head [:h2 name] (when card [status-pill (:status card)]) [:span.spacer]
         [:button {:on-click #(rf/dispatch [:close-detail])} "close"]]
        (if-let [src (:thumb card)]
          [:img.big {:src src :alt (str "view of " name)}]
          [:div.noview.big "no view"])
        (when-let [r (:reason card)] [:div.reason {:class (clojure.core/name (:severity card))} r])]])))
