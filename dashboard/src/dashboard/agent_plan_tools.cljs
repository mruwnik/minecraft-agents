(ns dashboard.agent-plan-tools
  "Native plan checks: geometry, zone and claim overlaps, conflicts, and scoring against the world."
  (:require [jobs.lib.zone-file :as zones]
            [plan.conflicts :as conflicts]
            [plan.parse :as parse]
            [plan.shape :as shape]
            [dashboard.plan-compare :as cmp]))

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
                 (when (not= id other)
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

(defn prepare-native
  "Accept plan source and native document/claim maps; return expansion and geometry checks.
  A nil zone source means zones have never been saved."
  [plan-text id blueprint-docs plan-docs zones-text claims]
  (let [plan-result (parse/parse plan-text id)
        blueprints-result (parse-docs blueprint-docs parse/parse-blueprint :blueprint)
        saved-result (parse-docs plan-docs parse/parse :plan)
        plan (:plan plan-result)
        bps (:values blueprints-result)
        plans (assoc (:values saved-result) id plan)
        total-cells (when plan (plan-cell-estimate plan bps))
        stored-cell-estimate (reduce + 0 (for [[_ stored-plan] plans]
                                            (plan-cell-estimate stored-plan bps)))
        over-budget? (or (> (or total-cells 0) 100000) (> stored-cell-estimate 100000))
        expansion (when (and plan (empty? (:errors plan-result)) (not over-budget?)) (shape/expand plan bps))
        cells (:cells expansion)
        zone-result (parse-zone-text zones-text)
        parsed-claims claims
        pairs (when expansion (candidate-conflicts id plan plans bps))
        errors (vec (concat (:errors plan-result)
                            (when over-budget? ["plan geometry or aggregate plan conflict index exceeds 100000 estimated cells; reduce geometry or remove plans"])
                            (map :error (:errors expansion))))]
    (cond-> {:ok (empty? errors) :id id :errors errors
              :blueprint-errors (:errors blueprints-result)
              :plan-errors (:errors saved-result)
              :expansion expansion
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
       (seq errors) (dissoc :expansion))))

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

(defn score-native
  "Compare a native expansion using block and column mtime accessors."
  [expansion block-at mtime-of inventory inventory-supplied offset limit]
  (let [judged (shape/plan-minus-world (:cells expansion) block-at)
        result (cmp/compare-plan expansion block-at)
        checked (assoc (cmp/checked (:cells expansion) mtime-of)
                       :now (js/Date.now))]
    {:counts (:counts result)
              :elements (mapv #(select-keys % [:id :kind :content :count :bounds :counts :error]) (take limit (drop offset (:elements result))))
              :elements-total (count (:elements result))
              :elements-next-offset (when (< (+ offset limit) (count (:elements result))) (+ offset limit))
              :region (cmp/bounds (:cells expansion))
              :checked checked
              :materials (material-summary judged inventory (true? inventory-supplied))}))
