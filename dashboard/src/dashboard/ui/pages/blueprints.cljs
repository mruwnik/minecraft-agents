(ns dashboard.ui.pages.blueprints
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]
            [dashboard.blueprint :as bp]
            [dashboard.ui.blueprint-canvas :as canvas]
            [dashboard.ui.page-data :as data]))

;; ---------------------------------------------------------------- the list
(defn list-item [selected detail]
  (let [{:keys [name title kind footprint layers blocks status builds]} (bp/blueprint-row detail)]
    ^{:key name}
    [:div.bp-item {:class (when (= name selected) "sel") :on-click #(rf/dispatch [:blueprints/select name])}
     [:div.head [:span.name name] [:span.title title]]
     [:div.meta
      (when (seq kind) [:span.kind kind])
      (when (seq footprint) [:span footprint])
      (when (pos? layers) [:span (str layers " layers")])
      (when (pos? blocks) [:span (str blocks " items")])
      [:span {:class (str "pill " (bp/status-level status))} status]
      (when (pos? builds) [:span (str "built " builds "×")])]]))

(defn draft-section [{:keys [draft-plan draft-stock draft-status]}]
  [:section
   [:h2 "draft blueprint"]
   [:button {:on-click #(rf/dispatch [:blueprints/copy-selected])} "copy selected into editor"]
   [:textarea {:aria-label "Blueprint v2 JSON draft" :placeholder "Paste a schemaVersion 2 plan"
               :value (or draft-plan "") :on-change #(rf/dispatch [:blueprints/draft-plan (.. % -target -value)])}]
   [:label "optional declared item stock (JSON counts)"
    [:textarea.stock {:aria-label "Declared stock" :placeholder "{\"oak_planks\": 64}"
                      :value (or draft-stock "") :on-change #(rf/dispatch [:blueprints/draft-stock (.. % -target -value)])}]]
   [:button {:on-click #(rf/dispatch [:blueprints/preview-draft])} "validate and preview"]
   [:button {:on-click #(rf/dispatch [:blueprints/download-draft])} "download JSON"]
   [:div.draft-status (or draft-status "Draft preview never starts or modifies a build.")]])

(defn sidebar [{:keys [library selected draft-status] :as state}]
  (let [details (:blueprints library)]
    [:aside
     [:section
      [:h2 "the library"]
      (into [:div] (map #(list-item selected %) details))
      (when (= selected data/draft-name) [:div.muted "showing the draft preview"])]
     [draft-section state]]))

;; ---------------------------------------------------------------- the 3D preview
(defn preview-block [detail]
  (let [node (atom nil)
        polys (atom [])
        {:keys [min max]} (bp/layer-range (:bp detail))
        angle (r/atom (/ js/Math.PI 4))
        cut (r/atom max)
        hover (r/atom "")
        drag (atom nil)
        draw! #(when @node (reset! polys (canvas/draw-preview! @node (:preview detail) @angle @cut)))]
    (r/create-class
     {:component-did-mount (fn [] (.observe (js/ResizeObserver. draw!) @node) (draw!))
      :component-did-update draw!
      :reagent-render
      (fn [detail]
        @angle @cut
        [:div.block
         [:h2 "building preview"]
         [:canvas#previewCanvas
          {:aria-label "Rotatable building preview" :ref #(reset! node %)
           :on-pointer-down (fn [e]
                              (.setPointerCapture (.-currentTarget e) (.-pointerId e))
                              (reset! drag {:x (.-clientX e) :angle @angle}))
           :on-pointer-up #(reset! drag nil)
           :on-pointer-cancel #(reset! drag nil)
           :on-pointer-move (fn [e]
                              (if-let [d @drag]
                                (reset! angle (+ (:angle d) (/ (- (.-clientX e) (:x d)) 140)))
                                (reset! hover (if-let [f (canvas/face-at @polys @node e)] (bp/face-hover-text f) ""))))
           :on-pointer-leave #(reset! hover "")}]
         [:div.previewControls
          [:button {:aria-label "Rotate left" :on-click #(swap! angle - (/ js/Math.PI 2))} "↶"]
          [:button {:aria-label "Rotate right" :on-click #(swap! angle + (/ js/Math.PI 2))} "↷"]
          [:label "show through "
           [:input {:type "range" :aria-label "Highest visible layer" :min min :max max :value @cut
                    :on-change #(reset! cut (js/Number (.. % -target -value)))}]
           [:span (bp/preview-layer-text @cut max)]]
          [:span (bp/palette-label detail)]]
         [:div#previewHover @hover]])})))

;; ---------------------------------------------------------------- layers, legend, bill, lint
(defn layer-canvas [bp-data cells hover]
  (let [node (atom nil)
        hovered (r/atom nil)
        draw! #(when @node (canvas/draw-layer! @node bp-data cells @hovered))]
    (r/create-class
     {:component-did-mount draw!
      :component-did-update draw!
      :reagent-render
      (fn [_ cells hover]
        @hovered
        [:canvas {:ref #(reset! node %)
                  :on-mouse-move (fn [e]
                                   (let [cell (canvas/cell-at cells @node e)]
                                     (reset! hovered cell)
                                     (reset! hover (when cell (bp/hover-text cell)))))
                  :on-mouse-leave (fn [] (reset! hovered nil) (reset! hover nil))}])})))

(defn layer-box [bp-data hover y]
  (let [cells (bp/layer-cells bp-data y)
        solid (count (remove :air cells))]
    ^{:key y}
    [:div.layer
     [:div.cap [:b (str "y" y)] [:span (str solid " block" (when-not (= 1 solid) "s"))]]
     [layer-canvas bp-data cells hover]]))

(defn legend-item [{:keys [token name label colour count]}]
  ^{:key token}
  [:div.legendItem
   [:span {:class (str "swatch" (when (= name "@solid") " solid"))
           :style (when (and colour (not= name "@solid")) {:background colour})}]
   [:span.tok token] [:span label] [:span.n (str "×" count)]])

(defn layers-block [bp-data]
  (let [hover (r/atom nil)]
    (fn [bp-data]
      [:div.block
       [:h2 "layers, from the ground up"]
       [:div#gridRow
        (into [:div#layers] (map #(layer-box bp-data hover %) (map :y (:layers bp-data))))
        (into [:div#legend] (map legend-item (bp/legend-rows bp-data)))]
       [:div#hover (when-let [[where what] (some-> @hover (str/split #" · "))]
                     [:<> where [:span.muted " · "] what])]])))

(defn bill-block [detail]
  [:div.block
   [:h2 "bill of materials"]
   (into [:div#bill]
         (for [{:keys [item count share]} (bp/bill-rows (get-in detail [:bill :total]))]
           ^{:key item}
           [:<> [:span.item item] [:span.n (str count)] [:div.bar {:style {:width (str (js/Math.round (* share 100)) "%")}}]]))
   [:div#billNote (bp/bill-note detail)]])

(defn lint-block [detail]
  [:div.block
   [:h2 "what lint says"]
   (into [:div#lint] (for [[i {:keys [level text]}] (map-indexed vector (bp/lint-lines detail))]
                       ^{:key i} [:div {:class level} text]))])

(defn materials-block [detail]
  [:<>
   [:h2 "material roles · requirements and preferences"]
   (into [:div#materials]
         (for [{:keys [role required preferences palette]} (bp/material-role-rows detail)]
           ^{:key role}
           [:section.materialRole
            [:h3 (str/replace role "_" " ")]
            (for [[i t] (map-indexed vector required)] ^{:key (str "r" i)} [:p (str "Required · " t)])
            (for [[i t] (map-indexed vector preferences)] ^{:key (str "p" i)} [:p.preference (str "Preference · " t)])
            (when (seq palette) [:p (str "Allocated from declared stock · " (str/join ", " palette))])]))])

(defn builds-line [builds]
  (if (empty? builds)
    [:div#builds "not yet built anywhere: mark one with blueprint.build place=<name>"]
    [:div#builds "standing at "
     (for [[i b] (map-indexed vector builds)]
       ^{:key i}
       [:<> (when (pos? i) ", ") [:span.bplace (:place b)]
        (str " " (subs (bp/build-text b) (count (:place b))))
        (when (bp/older-version? b) [:<> " " [:span.stale "from an older version of the file"]])])]))

;; ---------------------------------------------------------------- one blueprint
(defn detail-card [detail]
  (let [{:keys [footprint layers status]} (bp/blueprint-row detail)
        bp-data (:bp detail)]
    [:div
     [:div.titleRow
      [:h3 (:name detail)]
      [:span (when bp-data (str (:title bp-data) " · " footprint " · " layers " layers"))]
      [:span {:class (str "pill " (bp/status-level status))} status]]
     [:div.desc (:description bp-data)]
     (when bp-data
       [:<>
        (into [:div#meta] (for [[k v] (bp/meta-facts detail)] ^{:key k} [:span (str k " ") [:b v]]))
        [materials-block detail]
        [builds-line (:builds detail)]
        ^{:key (str "preview:" (:name detail) ":" (:hash detail))} [preview-block detail]
        ^{:key (str "layers:" (:name detail) ":" (:hash detail))} [layers-block bp-data]
        [bill-block detail]])
     [lint-block detail]]))

(defn current-detail [{:keys [library selected draft]}]
  (if (= selected data/draft-name) draft (bp/find-detail (:blueprints library) selected)))

(defn page []
  (r/create-class
   {:component-did-mount #(rf/dispatch [:blueprints/start])
    :reagent-render
    (fn []
      (let [{:keys [library selected clock failed] :as state} @(rf/subscribe [:blueprints])
            detail (current-detail state)
            n (count (:blueprints library))]
        [:main.bp-page
         [sidebar state]
         [:div#detail
          [:div.muted.bpstatus
           (str n " blueprint" (when-not (= 1 n) "s") " · ")
           (or failed (if clock (str "read " clock) "connecting…"))]
          (cond
            detail [detail-card detail]
            selected [:div#empty (str "no blueprint called " selected)]
            :else [:div#empty "pick a blueprint"])]]))}))
