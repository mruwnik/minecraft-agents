(ns dashboard.ui.detail
  "The body popup: header, the live view with takeover chrome, HUD, inventory, job stack and the action log."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [dashboard.ui.livecards :as live]
            [dashboard.ui.bodies :as bodies]
            [dashboard.ui.drive :as drive]
            [dashboard.ui.eventlog :as eventlog]
            [dashboard.ui.hudmodel :as hudmodel]
            [dashboard.ui.logic :as logic]))

(defn header [{:keys [name status reason severity job action pos-text dimension world last-seen]}]
  [:div.dhead
   [:h2 name]
   (when status [bodies/status-pill status])
   (when reason [:span.reason {:class (clojure.core/name severity)} reason])
   [:span.dfact (or job [:span.dim "no job"]) (when action [:span.dim (str " · " action)])]
   [:span.dfact.mono pos-text]
   (when dimension [:span.dfact dimension])
   (when world [:span.dfact.dim world])
   (when last-seen [:span.dfact.dim (str "seen " last-seen)])
   [:span.spacer]
   [:button.dclose {:title "close (Esc)" :on-click #(rf/dispatch [:close-detail])} "close ✕"]])

;; ---------------------------------------------------------------- the view and the takeover chrome
(defn banner []
  (let [{:keys [kind text countdown]} @(rf/subscribe [:drive-banner])]
    (case kind
      :ours [:div.dbanner.ours
             [:b text]
             (when countdown [:span.countdown countdown])
             [:button.release {:on-click #(rf/dispatch [:drive-release])} "Release (Esc)"]]
      :other [:div.dbanner.other [:b text]]
      nil)))

(defn chrome [online?]
  (let [{:keys [kind]} @(rf/subscribe [:drive-banner])
        error (:error @(rf/subscribe [:drive]))]
    [:div.dchrome {:class (when-not online? "empty")}
     (case (if online? kind :none)
       :none (when online? [:<> [:span.hint drive/hint]
                            [:button.take {:on-click #(rf/dispatch [:drive-take])} "Take control"]])
       [banner])
     [:span.spacer]
     (when online?
       [:button.stats {:class (when @(rf/subscribe [:detail-stats?]) "on") :title "show the view page's debug overlay"
                       :on-click #(rf/dispatch [:detail-stats])} "stats"])
     (when error [:span.derr error])]))

(defn frame-loaded
  "window.__view.loaded of the view page in our iframe, nil until it has one."
  []
  (some-> (js/document.getElementById "view-frame") .-contentWindow .-__view .-loaded))

(defn frame-nodata
  "Overlay while the popup's view page has no loaded column; polls the same-origin iframe."
  []
  (let [loaded (r/atom nil)
        timer (atom nil)]
    (r/create-class
     {:component-did-mount #(reset! timer (js/setInterval (fn [] (reset! loaded (try (frame-loaded) (catch :default _ nil)))) 500))
      :component-will-unmount #(js/clearInterval @timer)
      :reagent-render
      (fn []
        (when (live/no-world-data? {:loaded @loaded})
          [:div.nodata "no world data yet"]))})))

(defn view-box [{:keys [name online? iframe-src thumb offline-text]}]
  (let [driven? (and online? (= :ours (:kind @(rf/subscribe [:drive-banner]))))]
    [:div.viewarea
     [:div.viewbox {:class [(when driven? "driven") (when-not online? "offline")]}
      (if online?
        ^{:key name} [:<> [:iframe {:id "view-frame" :src iframe-src :title (str "live view of " name) :allow "pointer-lock; fullscreen"
                                  :on-load #(rf/dispatch [:frame-loaded])}]
        [frame-nodata]]
        [:<>
         (if thumb [:img {:src thumb :alt (str "last view of " name)}] [:div.noview "no view"])
         [:div.offline-note offline-text]])]]))

;; ---------------------------------------------------------------- HUD
(def heart-path "M4.5 8.2 L1.1 4.7 C-.3 3.2 .5 .9 2.5 .9 C3.4 .9 4.1 1.4 4.5 2.1 C4.9 1.4 5.6 .9 6.5 .9 C8.5 .9 9.3 3.2 7.9 4.7 Z")

(defn svg-icon [kind]
  (if (= kind :health)
    [:svg {:viewBox "0 0 9 9" :width 15 :height 15} [:path {:d heart-path}]]
    [:svg {:viewBox "0 0 9 9" :width 15 :height 15}
     [:circle {:cx 5.6 :cy 3.4 :r 2.9}]
     [:path {:d "M3.6 5.4 L1.3 7.7" :stroke-width 1.6 :stroke-linecap "round" :class "stem"}]]))

(defn icon [kind state i]
  ^{:key i}
  [:span.ico {:class (clojure.core/name kind)}
   [:span.ico-base [svg-icon kind]]
   [:span.ico-fill {:style {:width (case state :full "100%" :half "50%" "0")}} [svg-icon kind]]])

(defn icon-row [kind value label]
  [:div.iconrow {:title (str label " " (if (number? value) value "?") " / 20")}
   [:span.vlabel label]
   (into [:span.icons] (map-indexed #(icon kind %2 %1)) (hudmodel/icons value))
   [:span.vnum (if (number? value) (js/Math.round value) "-")]])

(defn hud-panel [hud]
  (let [{:keys [level percent]} (hudmodel/xp-model (:xp hud))]
    [:section.dpanel
     [:h3 "HUD"]
     (if-not hud
       [:div.dim "no HUD yet"]
       [:<>
        [icon-row :health (:health hud) "health"]
        [icon-row :food (:food hud) "food"]
        [:div.xprow {:title (str "level " level ", " percent "%")}
         [:span.vlabel "xp"] [:span.xplevel level]
         [:div.bar [:div.fill.xp {:style {:width (str percent "%")}}]]]
        (when (number? (:oxygen hud))
          [:div.xprow [:span.vlabel "air"]
           [:div.bar [:div.fill.air {:style {:width (str (max 0 (min 100 (* 100 (/ (:oxygen hud) 20)))) "%")}}]]
           [:span.vnum (:oxygen hud)]])
        [:div.heldrow [:span.vlabel "held"] [:span (hudmodel/held-text (:held hud))]]
        (when (seq (:effects hud))
          (into [:div.effects] (map-indexed (fn [i e] ^{:key i} [:span.effect (hudmodel/effect-text e)])) (:effects hud)))])]))

;; ---------------------------------------------------------------- inventory
(defn slot-cell [{:keys [slot count] :as s}]
  ^{:key slot}
  [:div.slot {:class (when (:empty? s) "vacant") :title (or (:title s) (str "slot " slot))}
   (when-not (:empty? s)
     [:<>
      [:span.slabel (:label s)]
      (when (:icon s) [:img {:src (:icon s) :alt "" :draggable false :on-error #(set! (.. % -target -style -display) "none")}])
      (when (> (or count 0) 1) [:span.scount count])])])

(defn inventory-panel [hud]
  (let [{:keys [hotbar main armor offhand]} (hudmodel/slots (:inventory hud))]
    [:section.dpanel
     [:h3 "Inventory"]
     (into [:div.invgrid.hotbar] (map slot-cell) hotbar)
     (into [:div.invgrid.maingrid] (map slot-cell) (apply concat main))
     [:div.armorrow
      (into [:div.armor] (map slot-cell) armor)
      [:span.dim.nowrap "offhand"]
      [slot-cell offhand]]]))

;; ---------------------------------------------------------------- jobs
(defn jobs-panel [{:keys [jobs reflexes]}]
  [:section.dpanel
   [:h3 "Jobs"]
   (if (empty? jobs)
     [:div.dim "no jobs"]
     (into [:div.joblist]
           (map (fn [{:keys [id label current? hold? round]}]
                  ^{:key id}
                  [:div.job {:class (when current? "current")}
                   [:span.mono (bodies/short-label label)]
                   (when round [:span.dim (str "round " round)])
                   (when hold? [:span.pill "hold"])]))
           jobs))
   (when (seq reflexes)
     [:<>
      [:h3 "Reflexes"]
      (into [:div.joblist]
            (map (fn [{:keys [id job trigger cooling?]}]
                  ^{:key id}
                  [:div.job [:span.mono (str job)] (when cooling? [:span.dim "cooling"])]))
            reflexes)])])

;; ---------------------------------------------------------------- action log
(defn chip [current [k label]]
  ^{:key k}
  [:button.chip {:class (when (= k current) "on") :on-click #(rf/dispatch [:detail-chip k])} label])

(defn log-row [{:keys [seq t level category source-kind text]}]
  ^{:key seq}
  [:div.lrow {:class [level (clojure.core/name category)]}
   [:span.when (logic/clock-ms-text t)]
   [:span.skind source-kind]
   [:span.ltext text]])

(defn log-panel []
  (let [rows @(rf/subscribe [:detail-rows])
        current @(rf/subscribe [:detail-chip])
        text @(rf/subscribe [:detail-text])]
    [:section.dlog
     [:div.logbar
      [:h3 "Action log"]
      (into [:div.chips] (map #(chip current %)) eventlog/chips)
      [:input {:type "search" :placeholder "filter text" :value text :on-change #(rf/dispatch [:detail-text (.. % -target -value)])}]
      [:span.dim (str (count rows) " shown")]]
     (into [:div.logbody]
           (if (empty? rows) [[:div.dim.lempty "no events"]] (map log-row rows)))]))

;; ---------------------------------------------------------------- the popup
(defn modal []
  (when @(rf/subscribe [:detail-body])
    (let [m @(rf/subscribe [:detail-model])]
      [:div.modal-back {:on-click #(rf/dispatch [:close-detail])}
       [:div.dmodal {:on-click #(.stopPropagation %)}
        [header m]
        [:div.dleft [chrome (:online? m)] [view-box m]]
        [:div.dright [hud-panel (:hud m)] [inventory-panel (:hud m)] [jobs-panel m]]
        [log-panel]]])))
