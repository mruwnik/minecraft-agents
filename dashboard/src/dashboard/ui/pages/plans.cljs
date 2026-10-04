(ns dashboard.ui.pages.plans
  "The Plans page: the plans of the world as a tree with their completion, and one plan's elements and layers drawn
  cell by cell."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.ui.plans-events]
            [dashboard.ui.plansmodel :as pm]))

(defn completion-bar [counts]
  (let [w (pm/bar-widths counts)]
    [:div.cbar {:title (pm/counts-text counts)}
     (for [k [:match :wrong :missing :extra :unknown]]
       ^{:key k} [:span {:class (name k) :style {:width (str (get w k) "%")}}])]))

(defn plan-row [selected {:keys [plan depth]}]
  (let [{:keys [id name owner kind status counts percent conflicts]} plan]
    ^{:key id}
    [:div.plan-row {:class (when (= id selected) "sel") :style {:margin-left (str (* depth 16) "px")}
                    :on-click #(rf/dispatch [:plans/select id])}
     [:div.prow1 [:span.pname (when (pos? depth) [:span.dim "└ "]) name] [:span.ppct (pm/percent-text {:percent percent})]]
     [completion-bar counts]
     [:div.prow2 [:span kind] [:span {:class (str "pstatus " status)} status] (when owner [:span (str "by " owner)])
      [:span.dim (str (:total counts) " blocks")]]
     (for [{:keys [with] :as conflict} conflicts]
       ^{:key with}
       [:div.pconflict {:title "open the other plan" :on-click (fn [e] (.stopPropagation e) (rf/dispatch [:plans/select with]))}
        (pm/conflict-text conflict)])]))

(defn plan-list []
  (let [{:keys [items file-errors failed selected]} @(rf/subscribe [:plans])]
    [:aside.plan-list
     [:h2 (str "plans (" (count items) ")")]
     (when failed [:div.err failed])
     (if (empty? items)
       [:div.dim.pempty "no plans in this world"]
       (into [:div] (map #(plan-row selected %)) (pm/plan-tree items)))
     (when (seq file-errors)
       (into [:div.plan-file-errors [:h2 "files with problems"]]
             (for [{:keys [file errors]} file-errors]
               ^{:key file} [:div.err [:b file] (into [:ul] (map (fn [e] [:li e])) errors)])))]))

;; ---------------------------------------------------------------- the layer grid
(defn hatch! [ctx x y size]
  (set! (.-strokeStyle ctx) "#8b949e")
  (set! (.-lineWidth ctx) 1)
  (.beginPath ctx)
  (.moveTo ctx x (+ y size))
  (.lineTo ctx (+ x size) y)
  (.stroke ctx))

(defn paint-grid! [canvas rows size selected-element mode]
  (let [cols (count (first rows))
        dpr (or js/window.devicePixelRatio 1)
        ctx (.getContext canvas "2d")]
    (set! (.-width canvas) (* cols size dpr))
    (set! (.-height canvas) (* (count rows) size dpr))
    (set! (.-width (.-style canvas)) (str (* cols size) "px"))
    (set! (.-height (.-style canvas)) (str (* (count rows) size) "px"))
    (.setTransform ctx dpr 0 0 dpr 0 0)
    (set! (.-fillStyle ctx) "#12151b")
    (.fillRect ctx 0 0 (* cols size) (* (count rows) size))
    (doseq [[dz row] (map-indexed vector rows)
            [dx cell] (map-indexed vector row)
            :when cell
            :let [x (* dx size) y (* dz size)]]
      (set! (.-globalAlpha ctx) (if (pm/dimmed? selected-element cell) 0.22 1))
      (when-let [fill (pm/cell-fill mode cell)]
        (set! (.-fillStyle ctx) fill)
        (.fillRect ctx x y size size))
      (when (pm/hatched? mode cell) (hatch! ctx x y size))
      (when (:x cell)
        (set! (.-strokeStyle ctx) "#ff3da5")
        (set! (.-lineWidth ctx) 2)
        (.strokeRect ctx (+ x 1) (+ y 1) (- size 2) (- size 2)))
      (set! (.-fillStyle ctx) "rgba(0,0,0,0.30)")
      (.fillRect ctx (+ x (dec size)) y 1 size)
      (.fillRect ctx x (+ y (dec size)) size 1))
    (set! (.-globalAlpha ctx) 1)))

(defn layer-grid [_rows _grid _y _element _mode]
  (let [canvas (atom nil)
        wrap (atom nil)
        hover (r/atom nil)
        props (atom nil)
        size (atom pm/max-cell)
        draw! (fn []
                (when-let [{:keys [rows element mode]} (and @canvas @props)]
                  (when (seq rows)
                    (let [avail (max 200 (- (.-clientWidth @wrap) 24))]
                      (reset! size (pm/cell-size (count (first rows)) (count rows) avail 640))
                      (paint-grid! @canvas rows @size element mode)))))]
    (r/create-class
     {:component-did-mount (fn [] (.observe (js/ResizeObserver. draw!) @wrap) (draw!))
      :component-did-update (fn [this]
                              (let [[_ rows grid y element mode] (r/argv this)]
                                (reset! props {:rows rows :grid grid :y y :element element :mode mode}))
                              (draw!))
      :reagent-render
      (fn [rows grid y element mode]
        (reset! props {:rows rows :grid grid :y y :element element :mode mode})
        [:div.gridwrap {:ref #(reset! wrap %)}
         [:canvas.plan-grid
          {:ref #(reset! canvas %)
           :on-mouse-leave #(reset! hover nil)
           :on-mouse-move (fn [e]
                            (let [rect (.getBoundingClientRect @canvas)
                                  [c rr] (pm/cell-at @size (count (first rows)) (count rows)
                                                     (- (.-clientX e) (.-left rect)) (- (.-clientY e) (.-top rect)))]
                              (reset! hover (when c (let [cell (get-in rows [rr c])] {:pos (pm/world-pos grid (or (:y cell) y) [c rr]) :cell cell})))))}]
         [:div.cellinfo
          (if-let [{:keys [pos cell]} @hover]
            [:span.mono (pm/cell-text pos cell)]
            [:span.dim "hover a cell"])]])})))

(defn legend [mode view]
  [:div.plan-legend
   (case mode
     "plan" [:span [:i.blank] "clear (nothing wanted)"]
     "world" [:span [:i.blank] "air"]
     nil)
   (when (= "world" mode) [:span [:i.hatch] "not dumped"])
   (when (= "diff" mode)
     (for [[k label] [["match" "match"] ["missing" "missing (air)"] ["wrong" "wrong block"] ["extra" "extra (should be air)"]
                      ["unknown" "not dumped"]]]
       ^{:key k} [:span [:i {:style {:background (get pm/status-colors k)}}] label]))
   (when (not= "diff" mode)
     [:span.dim (if (= "plan" mode) "colours are the blocks the plan wants" "colours are the blocks found at the last check")])
   [:span [:i.conflict] "conflict with another active plan"]
   (when (= :bird view) [:span.dim (pm/bird-rule mode)])])

(defn switch [label options current event]
  (into [:div.layers [:span.dim label]]
        (for [[value text] options]
          ^{:key (str value)}
          [:button.chip {:class (when (= value current) "on") :on-click #(rf/dispatch [event value])} text])))

(defn mode-switch [mode checked]
  [:div.mode-switch
   [switch "show" [["plan" "plan"] ["world" "world"] ["diff" "diff"]] mode :plans/mode]
   [:span.dim.checked (pm/checked-text checked)]])

(defn layer-selector [layers view current]
  (into [:div.layers [:span.dim "view"]
         [:button.chip {:class (when (= :bird view) "on") :on-click #(rf/dispatch [:plans/view :bird])} "bird's eye"]
         [:span.dim "layer y"]]
        (for [{:keys [y]} layers]
          ^{:key y}
          [:button.chip {:class (when (and (= :layer view) (= y current)) "on") :on-click #(rf/dispatch [:plans/layer y])} (str y)])))

;; ---------------------------------------------------------------- elements
(defn element-row [selected {:keys [id kind content counts cells error ref where bounds]}]
  ^{:key id}
  [:tr.el-row {:class (when (= id selected) "sel") :on-click #(rf/dispatch [:plans/element id (get-in bounds [:min 1])])}
   [:td.mono id]
   [:td [:span.pill kind]]
   [:td content (when where [:span.dim (str "  at " where)])
    (when ref [:button.chip {:on-click (fn [e] (.stopPropagation e) (rf/dispatch [:plans/select ref]))} "open"])
    (when error [:div.err error])]
   [:td.barcell [completion-bar counts]]
   [:td.num (pm/percent-text counts)]
   [:td.num.dim cells]])

(defn elements-table [detail selected]
  [:table.plan-elements
   [:thead [:tr [:th "element"] [:th "kind"] [:th "content"] [:th "completion"] [:th "%"] [:th "blocks"]]]
   (into [:tbody] (map #(element-row selected %)) (pm/element-rows detail))])

(defn spots-table [spots]
  (when (seq spots)
    [:section.plan-spots
     [:h3 "spots"]
     [:table.plan-elements
      [:thead [:tr [:th "spot"] [:th "position"]]]
      (into [:tbody]
            (for [{:keys [name pos]} (pm/spot-rows spots)]
              ^{:key name} [:tr [:td.mono name] [:td.mono pos]]))]]))

(defn assign-table [assign]
  (when (seq assign)
    [:section.plan-assign
     [:h3 "assignments"]
     [:table.plan-elements
      [:thead [:tr [:th "spot or part"] [:th "who"] [:th "use"] [:th "answer"]]]
      (into [:tbody]
            (map-indexed
             (fn [i {:keys [spot who use answer]}]
               ^{:key i} [:tr [:td.mono spot] [:td who] [:td use] [:td [:span.pill answer]]])
             (pm/assign-rows assign)))]]))

(defn conflicts-section [conflicts]
  (when (seq conflicts)
    (into [:section.plan-conflicts [:h3 "conflicts"]]
          (for [{:keys [with box] :as conflict} conflicts]
            ^{:key with}
            [:div.pconflict {:on-click #(rf/dispatch [:plans/select with])}
             (pm/conflict-text conflict) [:span.dim.mono (str "  " (pm/region-text box))]]))))

(defn plan-errors [errors]
  (when (seq errors)
    (into [:section.plan-errors [:h3 "problems"]]
          (for [{:keys [element error]} errors]
            ^{:key (str element error)} [:div.err [:b.mono element] " " error]))))

(defn plan-detail []
  (let [{:keys [detail detail-failed selected element]} @(rf/subscribe [:plans])
        current @(rf/subscribe [:plan-layer-y])
        {:keys [view mode]} @(rf/subscribe [:plan-view-state])]
    (cond
      (nil? selected) [:main.plan-detail [:div.dim.pempty "pick a plan"]]
      detail-failed [:main.plan-detail [:div.err detail-failed]]
      (nil? detail) [:main.plan-detail [:div.dim.pempty "loading..."]]
      :else
      (let [{:keys [name kind status owner note region counts layers grid errors spots assign checked conflicts]} detail
            layer (first (filter #(= current (:y %)) layers))
            rows (if (= :bird view) (pm/bird-rows layers mode) (:rows layer))]
        [:main.plan-detail
         [:div.phead
          [:h2 name] [:span.pill kind] [:span {:class (str "pill pstatus " status)} status]
          (when owner [:span.dim (str "by " owner)])
          [:span.ppct.big (pm/percent-text counts)]]
         [completion-bar counts]
         [:div.pfacts [:span.dim (pm/counts-text counts)]]
         (when note [:div.pnote note])
         [:div.pfacts [:span.dim "region"] [:span.mono (pm/region-text region)]
          [:span.dim (str "(" (pm/region-size region) ")")]]
         [plan-errors errors]
         [conflicts-section conflicts]
         [:div.plan-body
          [:div.plan-els [elements-table detail element] [spots-table spots] [assign-table assign]]
          (when (seq layers)
            [:section.plan-layers
             [mode-switch mode checked]
             [layer-selector layers view current]
             [legend mode view]
             (when (seq rows)
               (let [cropped (pm/crop-grid rows grid (:bounds (first (filter #(= element (:id %)) (:elements detail)))))]
                 ^{:key [selected view current element]} [layer-grid (:rows cropped) (:grid cropped) current element mode]))])]]))))

(defn page []
  [:main.plans-page [plan-list] [plan-detail]])
