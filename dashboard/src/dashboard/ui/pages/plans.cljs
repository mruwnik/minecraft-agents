(ns dashboard.ui.pages.plans
  "The Plans page: every plan of the world with its completion, and one plan's layers drawn cell by cell."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.ui.plans-events]
            [dashboard.ui.plansmodel :as pm]))

(defn completion-bar [summary]
  (let [w (pm/bar-widths summary)]
    [:div.cbar {:title (pm/counts-text summary)}
     (for [k [:match :wrong :missing :unknown]]
       ^{:key k} [:span {:class (name k) :style {:width (str (get w k) "%")}}])]))

(defn plan-row [selected {:keys [name by kind summary]}]
  ^{:key name}
  [:div.plan-row {:class (when (= name selected) "sel") :on-click #(rf/dispatch [:plans/select name])}
   [:div.prow1 [:span.pname name] [:span.ppct (pm/percent-text summary)]]
   [completion-bar summary]
   [:div.prow2 [:span kind] (when by [:span (str "by " by)])
    [:span.dim (str (:total summary) " cells")]]])

(defn plan-list []
  (let [{:keys [items failed selected]} @(rf/subscribe [:plans])]
    [:aside.plan-list
     [:h2 (str "plans (" (count items) ")")]
     (when failed [:div.err failed])
     (if (empty? items)
       [:div.dim.pempty "no plans in this world"]
       (into [:div] (map #(plan-row selected %)) items))]))

;; ---------------------------------------------------------------- the layer grid
(def label-font "bold 12px ui-monospace, Menlo, monospace")

(defn paint-grid! [canvas rows size]
  (let [cols (count (first rows))
        dpr (or js/window.devicePixelRatio 1)
        ctx (.getContext canvas "2d")]
    (set! (.-width canvas) (* cols size dpr))
    (set! (.-height canvas) (* (count rows) size dpr))
    (set! (.-width (.-style canvas)) (str (* cols size) "px"))
    (set! (.-height (.-style canvas)) (str (* (count rows) size) "px"))
    (.setTransform ctx dpr 0 0 dpr 0 0)
    (set! (.-font ctx) label-font)
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (doseq [[dz row] (map-indexed vector rows)
            [dx {:keys [status ch]}] (map-indexed vector row)
            :let [color (get pm/status-colors status)
                  x (* dx size) y (* dz size)]]
      (set! (.-fillStyle ctx) (if color color "#12151b"))
      (.fillRect ctx x y size size)
      (set! (.-fillStyle ctx) "rgba(0,0,0,0.30)")
      (.fillRect ctx (+ x (dec size)) y 1 size)
      (.fillRect ctx x (+ y (dec size)) size 1)
      (when (and color (>= size 12))
        (set! (.-fillStyle ctx) "rgba(10,12,16,0.85)")
        (.fillText ctx (pm/shown-char ch) (+ x (/ size 2)) (+ y (/ size 2) 1))))))

(defn layer-grid [rows]
  (let [canvas (atom nil)
        wrap (atom nil)
        hover (r/atom nil)
        rows* (atom rows)
        size (atom pm/max-cell)
        draw! (fn []
                (when (and @canvas (seq @rows*))
                  (let [cols (count (first @rows*))
                        avail (max 200 (- (.-clientWidth @wrap) 24))]
                    (reset! size (pm/cell-size cols (count @rows*) avail 760))
                    (paint-grid! @canvas @rows* @size))))]
    (r/create-class
     {:component-did-mount (fn [] (.observe (js/ResizeObserver. draw!) @wrap) (draw!))
      :component-did-update (fn [this] (reset! rows* (second (r/argv this))) (draw!))
      :reagent-render
      (fn [rows]
        (reset! rows* rows)
        [:div.gridwrap {:ref #(reset! wrap %)}
         [:canvas.plan-grid
          {:ref #(reset! canvas %)
           :on-mouse-leave #(reset! hover nil)
           :on-mouse-move (fn [e]
                            (let [rect (.getBoundingClientRect @canvas)
                                  [c rr] (pm/cell-at @size (count (first rows)) (count rows)
                                                     (- (.-clientX e) (.-left rect)) (- (.-clientY e) (.-top rect)))]
                              (reset! hover (when c {:at [c rr] :cell (get-in (vec (map vec rows)) [rr c])}))))}]
         [:div.cellinfo
          (if-let [{:keys [at cell]} @hover]
            [:span [:span.mono (str "[" (first at) "," (second at) "] ")] (pm/cell-text cell)]
            [:span.dim "hover a cell"])]])})))

(defn legend []
  (into [:div.plan-legend]
        (for [[k label] [["match" "match"] ["wrong" "wrong block"] ["missing" "missing (air)"] ["unknown" "not dumped"]]]
          ^{:key k} [:span [:i {:style {:background (get pm/status-colors k)}}] label])))

(defn layer-selector [layers place-y current]
  (into [:div.layers [:span.dim "layer"]]
        (for [{:keys [y]} layers]
          ^{:key y}
          [:button.chip {:class (when (= y current) "on") :title (str "block y " (pm/world-y place-y y))
                         :on-click #(rf/dispatch [:plans/layer y])}
           (str "y" (pm/world-y place-y y))])))

(defn bill-panel [bill]
  (let [rows (pm/bill-rows bill)]
    [:section.plan-bill
     [:h3 "bill of materials"]
     (if (empty? rows)
       [:div.dim "nothing to buy"]
       (into [:div.bill] (mapcat (fn [[item n]] [[:span.n n] [:span item]])) rows))]))

(defn plan-detail []
  (let [{:keys [detail detail-failed selected]} @(rf/subscribe [:plans])
        current @(rf/subscribe [:plan-layer-y])]
    (cond
      (nil? selected) [:main.plan-detail [:div.dim.pempty "pick a plan"]]
      detail-failed [:main.plan-detail [:div.err detail-failed]]
      (nil? detail) [:main.plan-detail [:div.dim.pempty "loading..."]]
      :else
      (let [{:keys [name kind by note x y z bounds layers bill]} detail
            layer (first (filter #(= current (:y %)) layers))]
        [:main.plan-detail
         [:div.phead
          [:h2 name] [:span.pill kind] (when by [:span.dim (str "by " by)])
          [:span.ppct.big (pm/percent-text detail)]]
         [completion-bar detail]
         [:div.pfacts
          [:span.dim (pm/counts-text detail)]]
         (when note [:div.pnote note])
         [:div.pfacts [:span.dim "origin"] [:span.mono (str x ", " y ", " z)]
          [:span.dim "bounds"] [:span.mono (pm/bounds-text bounds)]]
         [layer-selector layers y current]
         [legend]
         (when layer ^{:key [name current]} [layer-grid (:rows layer)])
         [bill-panel bill]]))))

(defn page []
  [:main.plans-page [plan-list] [plan-detail]])
