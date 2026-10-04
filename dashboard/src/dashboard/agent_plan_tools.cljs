(ns dashboard.agent-plan-tools
  "Small Node bridge over the shared CLJS plan model. File IO and the chunk reader stay in the CLI; plan meaning,
  validation, expansion, conflicts and answers stay in plan.shape/dashboard.plan-api."
  (:require [cljs.reader :as reader]
            [engine.zones :as zones]
            [plan.conflicts :as conflicts]
            [plan.parse :as parse]
            [plan.shape :as shape]
            [dashboard.plan-compare :as cmp]))

(defn json-value [text fallback]
  (try (js->clj (.parse js/JSON (or text fallback)) :keywordize-keys true)
       (catch :default _ [])))

(defn json-object [text fallback]
  (try (js->clj (.parse js/JSON (or text fallback)))
       (catch :default _ {})))

(defn parse-docs [docs parser key]
  (reduce (fn [{:keys [values errors]} {:keys [id text]}]
            (let [result (parser text id)]
              (if (seq (:errors result))
                {:values values :errors (conj errors {:id id :errors (:errors result)})}
                {:values (assoc values id (get result key)) :errors errors})))
          {:values {} :errors []}
          docs))

(defn parse-zone-text [text]
  (if (nil? text)
    {:missing true}
    (let [result (zones/parse text)]
      (if (seq (:errors result))
        {:errors (:errors result)}
        {:zones (:value result)}))))

(defn inside? [[x y z] {:keys [min max]}]
  (and (vector? min) (vector? max)
       (<= (nth min 0) x (nth max 0))
       (<= (nth min 1) y (nth max 1))
       (<= (nth min 2) z (nth max 2))))

(defn zone-overlaps [cells zones]
  (mapv (fn [{:keys [name owner allow] :as zone}]
          (let [hits (filter #(inside? (:pos %) zone) cells)]
            {:name name :owner owner :allow (vec (sort allow)) :count (count hits)
             :cells (vec (take 10 (map :pos hits)))
             :more? (> (count hits) 10)}))
        (filter #(some (fn [cell] (inside? (:pos cell) %)) cells) zones)))

(defn claim-overlaps [cells claims now]
  (mapv (fn [{:keys [id owner until status note min max]}]
          (let [claim {:min min :max max}
                hits (filter #(inside? (:pos %) claim) cells)]
            {:id id :owner owner :status status :until until :note note
             :count (count hits) :cells (vec (take 10 (map :pos hits)))
             :more? (> (count hits) 10)}))
        (filter (fn [{:keys [until status min max]}]
                  (and (= "active" (if (keyword? status) (name status) status))
                       (or (nil? until) (> until now))
                       (some #(inside? (:pos %) {:min min :max max}) cells)))
                claims)))

(defn conflict-view [pairs id]
  (mapv (fn [{:keys [plans count same box cells]}]
          (let [other (first (remove #{id} plans))]
            {:with other :count count :same same :box box
             :cells (vec (take 10 cells)) :more? (> count 10)}))
        (filter #(some #{id} (:plans %)) pairs)))

(defn candidate-conflicts [id plan plans blueprints]
  (let [candidate (:cells (shape/expand plan blueprints))
        by-pos (into {} (map (juxt :pos :want) candidate))]
    (->> plans
         (keep (fn [[other other-plan]]
                 (when (and (not= id other) (= :active (:status other-plan)))
                   (let [expanded (shape/expand other-plan blueprints)
                         other-cells (into {} (map (juxt :pos :want) (:cells expanded)))
                         [different same] (reduce-kv (fn [[different same] pos want]
                                                       (if (contains? by-pos pos)
                                                         (if (conflicts/agree? want (get by-pos pos))
                                                           [different (inc same)]
                                                           [(conj different pos) same])
                                                         [different same]))
                                                     [[] 0] other-cells)]
                     (when (and (empty? (:errors expanded)) (seq different))
                       {:plans (vec (sort [id other])) :count (count different) :same same
                        :box (conflicts/box-of different) :cells (vec (sort different))})))))
         (sort-by (juxt (comp - :count) :plans))
         vec)))

(defn plan-cell-estimate [plan blueprints]
  (reduce + 0
          (map (fn [part]
                 (cond
                   (:cells part) (count (:cells part))
                   (:blueprint part) (let [bp (get blueprints (:blueprint part))]
                                       (if bp (* (count (first (first (:layers bp))))
                                                 (count (first (:layers bp)))
                                                 (count (:layers bp))) 0))
                   :else (let [[[x1 y1 z1] [x2 y2 z2]] (or (:box part) (:outline part))]
                           (if (and x1 x2 y1 y2 z1 z2)
                             (let [width (inc (abs (- x2 x1)))
                                   depth (inc (abs (- z2 z1)))
                                   height (inc (abs (- y2 y1)))]
                               (if (:outline part)
                                 (* height (if (or (= width 1) (= depth 1))
                                             (* width depth)
                                             (- (+ (* 2 width) (* 2 depth)) 4)))
                                 (* width depth height)))
                             0))))
               (:parts plan))))

(defn prepare
  "Accept EDN source strings plus JSON arrays [{id,text}] and return parsed expansion and geometry checks.
  zonesText nil means zones have never been saved; claimsJSON is the independent world-data social claim projection."
  [plan-text id blueprints-json plans-json zones-text claims-json]
  (let [plan-result (parse/parse plan-text id)
        blueprints-result (parse-docs (json-value blueprints-json "[]") parse/parse-blueprint :blueprint)
        saved-result (parse-docs (json-value plans-json "[]") parse/parse :plan)
        plan (:plan plan-result)
        bps (:values blueprints-result)
        plans (assoc (:values saved-result) id plan)
        total-cells (when plan (plan-cell-estimate plan bps))
        active-cell-estimate (reduce + 0 (for [[_ active-plan] plans
                                                :when (= :active (:status active-plan))]
                                            (plan-cell-estimate active-plan bps)))
        over-budget? (or (> (or total-cells 0) 100000) (> active-cell-estimate 100000))
        expansion (when (and plan (empty? (:errors plan-result)) (not over-budget?)) (shape/expand plan bps))
        cells (:cells expansion)
        zone-result (parse-zone-text zones-text)
        parsed-claims (json-value claims-json "[]")
        pairs (when expansion (candidate-conflicts id plan plans bps))
        errors (vec (concat (:errors plan-result)
                            (when over-budget? ["plan geometry or aggregate active-plan conflict index exceeds 100000 estimated cells; reduce geometry or active plans"])
                            (map :error (:errors expansion))))]
    (clj->js
     (cond-> {:ok (empty? errors) :id id :errors errors
              :blueprint-errors (:errors blueprints-result)
              :plan-errors (:errors saved-result)
              :expansion-edn (when expansion (pr-str expansion))
              :cells (mapv :pos cells)
              :region (cmp/bounds cells)
              :spots (:spots expansion)
              :parts (mapv #(select-keys % [:id :where :count :error]) (:parts expansion))
              :conflicts (if plan (conflict-view (or pairs []) id) [])
              :zones (cond
                       (:missing zone-result) {:state :unknown :reason :missing-zone-file}
                       (:errors zone-result) {:state :invalid :errors (:errors zone-result)}
                       :else {:state :saved :overlaps (zone-overlaps cells (:zones zone-result))})
              :claims (claim-overlaps cells parsed-claims (js/Date.now))}
       (seq errors) (dissoc :expansion-edn)))))

(defn pos-key [[x y z]] (str x "," y "," z))
(defn chunk-key [[x _ z]] (str (bit-shift-right x 4) "," (bit-shift-right z 4)))

(defn exact-material [want]
  (cond
    (string? want) want
    (and (map? want) (string? (:block want))) (:block want)
    :else nil))

(defn material-summary [judged inventory supplied?]
  (let [remaining-cells (filter #(#{:missing :wrong} (:answer %)) judged)
        required (frequencies (keep #(exact-material (:want %)) judged))
        remaining (frequencies (keep #(exact-material (:want %)) remaining-cells))
        unresolved (frequencies (map #(cond
                                       (vector? (:want %)) :choice
                                       (contains? (:want %) :crop) :crop
                                       (contains? (:want %) :tree) :tree
                                       :else :other)
                                     (remove #(or (exact-material (:want %)) (= :clear (:want %))) remaining-cells)))
        unknown (count (filter #(= :unknown (:answer %)) judged))
        materials (into {} (map (fn [[block needed]]
                                  [block {:needed needed
                                          :available (when supplied? (get inventory block 0))
                                          :shortage (when supplied? (max 0 (- needed (get inventory block 0))))}])) remaining)]
    {:availability (if supplied? :provided-name-counts :unknown)
     :basis :block-names
     :required required
     :remaining materials
     :unresolved (into {} (map (fn [[kind n]] [kind n]) unresolved))
     :unknown-cells unknown}))

(defn score
  "Compare an EDN expansion to a JSON map keyed x,y,z of known blocks and x,z of dumped-column mtimes."
  [expansion-edn blocks-json mtimes-json inventory-json inventory-supplied offset limit]
  (let [expansion (reader/read-string expansion-edn)
        blocks (json-object blocks-json "{}")
        mtimes (json-object mtimes-json "{}")
        inventory (json-object inventory-json "{}")
        block-at (fn [pos]
                   (when-let [block (get blocks (pos-key pos))]
                     {:name (get block "name") :state (get block "state")}))
        judged (shape/plan-minus-world (:cells expansion) block-at)
        result (cmp/compare-plan expansion block-at)
        checked (assoc (cmp/checked (:cells expansion) (fn [cx cz] (get mtimes (str cx "," cz))))
                       :now (js/Date.now))]
    (clj->js {:counts (:counts result)
              :elements (mapv #(select-keys % [:id :kind :content :count :bounds :counts :error]) (take limit (drop offset (:elements result))))
              :elements-total (count (:elements result))
              :elements-next-offset (when (< (+ offset limit) (count (:elements result))) (+ offset limit))
              :region (cmp/bounds (:cells expansion))
              :checked checked
              :materials (material-summary judged inventory (true? inventory-supplied))})))

(defn validate-plan [text id]
  (clj->js (parse/parse text id)))

(defn validate-blueprint [text id]
  (clj->js (parse/parse-blueprint text id)))
