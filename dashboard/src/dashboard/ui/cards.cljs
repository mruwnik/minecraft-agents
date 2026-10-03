(ns dashboard.ui.cards
  (:require [re-frame.core :as rf]
            [dashboard.ui.logic :as logic]))

(defn notice [text]
  (when text [:div.muted.notice text]))

(defn event-row [now {:keys [t level text]}]
  [:div {:class (str "ev " level)}
   [:span.when (logic/time-ago-text (when (and now t) (- now t)))] " " text])

(defn job-row [{:keys [id label current? hold? round]}]
  ^{:key id}
  [:div.job {:class (when current? "current")}
   (or label id)
   (when current? [:span.badge.cur "current"])
   (when hold? [:span.badge.hold "hold"])
   (when round [:span.muted (str " round " round)])])

(defn reflex-row [{:keys [id trigger job] :as reflex}]
  ^{:key id}
  [:div.reflex
   [:span id] [:span.muted (str " " trigger " -> " job " ")]
   [:span.muted (logic/cooldown-text reflex)]])

(defn pos-text [pos]
  (when pos (str (js/Math.round (:x pos)) ", " (js/Math.round (:y pos)) ", " (js/Math.round (:z pos)))))

(defn body-card [now selected {:keys [name up error state engine]}]
  (let [{:keys [job reflex jobs reflexes recent warn10m error10m]} engine
        sel? (= selected {:kind :body :name name})]
    ^{:key name}
    [:div.body.engine {:class [(when-not up "off") (when sel? "sel")]
                       :on-click #(rf/dispatch [:select {:kind :body :name name}])}
     [:div.head
      [:span.name name]
      [:span.badge {:class (if up "up" "down")} (if up "up" "down")]
      (when (pos? (or warn10m 0)) [:span.badge.warn (str warn10m " warn")])
      (when (pos? (or error10m 0)) [:span.badge.err (str error10m " err")])
      [:span.spacer]
      [:button {:on-click (fn [e] (.stopPropagation e) (rf/dispatch [:open-actions name]))} "actions"]]
     [:div.meta (or (pos-text (:pos state)) "no position")
      (when-not up (str " · " error))]
     (when-let [j (:name job)] [:div.doing (str "job: " j)])
     (when reflex [:div.doing (str "reflex: " (if (map? reflex) (or (:id reflex) (pr-str reflex)) reflex))])
     (when (or (seq jobs) (seq reflexes))
       [:details {:on-click #(.stopPropagation %)}
        [:summary (str (count jobs) " jobs, " (count reflexes) " reflexes")]
        (into [:div.sub [:h3 "jobs"]] (map job-row jobs))
        (into [:div.sub [:h3 "reflexes"]] (map reflex-row reflexes))])
     (into [:div.events] (map #(event-row now %) (reverse (take-last 10 recent))))]))

(defn foreign-list [bodies]
  (when (seq bodies)
    [:div#down
     [:span.muted (str "down (not engine bodies): ")]
     (interpose ", " (for [b bodies] ^{:key (:name b)} [:span.muted (:name b)]))]))

(defn bodies-panel []
  (let [{:keys [engine foreign]} @(rf/subscribe [:split-bodies])
        now @(rf/subscribe [:now])
        selected @(rf/subscribe [:selected])]
    [:section
     [:h2 "bodies"]
     (into [:div] (map #(body-card now selected %) engine))
     [foreign-list foreign]]))

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

(defn look-panel []
  (let [{:keys [engine]} @(rf/subscribe [:split-bodies])
        picked @(rf/subscribe [:look-body])
        notices @(rf/subscribe [:notices])]
    [:section
     [:h2 "through their eyes"]
     [:div.row
      [:select {:value (or picked "") :on-change #(rf/dispatch [:look-body (not-empty (.. % -target -value))])}
       [:option {:value ""} "pick a body"]
       (for [b engine :when (:up b)] ^{:key (:name b)} [:option {:value (:name b)} (:name b)])]
      [:span.spacer]
      [:button {:on-click #(rf/dispatch [:unsupported "look"])} "watch"]
      [:button {:on-click #(rf/dispatch [:unsupported "screen"])} "screen"]]
     [notice (or (get notices "look") (get notices "screen"))]]))

(defn actions-popup []
  (when-let [name @(rf/subscribe [:actions-body])]
    [:div#actionsOverlay {:on-click #(rf/dispatch [:close-actions])}
     [:div#actionsCard {:on-click #(.stopPropagation %)}
      [:div.row [:h2 (str "actions - " name)] [:span.spacer] [:button {:on-click #(rf/dispatch [:close-actions])} "close"]]
      [:div [:button {:on-click #(rf/dispatch [:unsupported "actions"])} "show actions"]]
      [notice (get @(rf/subscribe [:notices]) "actions")]]]))

(defn side-panel []
  [:aside
   [look-panel]
   [bodies-panel]
   [places-panel]])
