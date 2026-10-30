(ns jobs.lib.home-food
  "The food a body has at home, from what it sensed: chests it booked food in (memory :fetch/stock) that zones and
  claims let it take from, ripe crops it saw in its own zones (within the seen-block memory's 64-block reach) and on
  the crop cells of its own plans (anywhere), and the surplus of the food animals it saw or
  noted there (animal-spare's keep rule: the breeders stay). (sources c far) [{:kind :pos :points :walk-s}], walk-s
  from far; (carry-points c trip) what the body must carry on trip (jobs.lib.cost.food/carry-points over them)."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.notes :as notes]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.food :as food]
            [jobs.lib.crops :as crops]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.stock :as stock]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(def crop-food
  "Per field crop: the food one ripe crop gives (at least one item, of the dropped item; wheat as a third of a loaf)."
  {"wheat" ["bread" (/ 1 3)] "carrots" ["carrot" 1] "potatoes" ["potato" 1] "beetroots" ["beetroot" 1]})

(def meat-of (into {} (map (fn [[m animal]] [animal m])) game/meat-animal))

(defn item-points
  "Hunger points count of name give: reserve food at its points, wheat as loaves."
  [name count]
  (cond (food/reserve-food? name) (* count (foods/points name))
        (= "wheat" name) (* (quot count 3) (foods/points "bread"))
        :else 0))

(defn chest-sources [c]
  (for [[cell items] (stock/stock-of (ctx/view c))
        :let [pos (zipmap [:x :y :z] cell)
              points (reduce + 0 (map (fn [[n k]] (item-points n k)) items))]
        :when (and (pos? points) (not (access/container-refusal c :take pos)))]
    {:kind :chest :pos pos :points points}))

(defn crop-sources [c]
  (let [p (:primitives c)
        planned (keep (fn [cell]
                        (let [{:keys [name properties]} (look/seen-block p (zipmap [:x :y :z] cell))]
                          (when (crop-food name)
                            {:name name :pos (zipmap [:x :y :z] cell) :age (some-> (:age properties) js/Number)})))
                      (known/crop-cells c))
        near (crops/seen-crops p (keys crop-food) 64 256)]
    (for [{:keys [name pos age]} (vals (into {} (map (juxt (comp u/cell-vec :pos) identity)) (concat near planned)))
          :when (and age (>= age (crops/ripe-age name)) (access/in-own-area? c pos))
          :let [[item share] (crop-food name)]]
      {:kind :crop :pos pos :points (* share (foods/points item))})))

(defn pen-sources [c]
  (let [p (:primitives c)
        seen (remove #(true? (.-baby %)) (look/seen-objs p {:radius 64 :kind "passive" :names (vec (keys meat-of)) :max 64}))
        ids (set (map #(.-uuid %) seen))
        noted (for [n (notes/notes c)
                    :when (and (= :seen (:kind n)) (meat-of (:what n)) (:id n) (not (:baby n)) (not (ids (:id n)))
                               (not (spare/gone? c n 64 ids)))]
                {:kind (:what n) :pos (zipmap [:x :y :z] (:pos n))})
        own (filter #(access/in-own-area? c (:pos %))
                    (concat (map (fn [e] {:kind (.-name e) :pos (u/pos-of (.-pos e))}) seen) noted))]
    (for [[kind as] (group-by :kind own)
          :let [meat (meat-of kind)
                drops (or (cost/kill-drops kind meat :version (game/version-of p)) cost/expected-drops)
                spare-n (spare/takeable-adults (spare/keep-adults) (count as))]
          :when (pos? spare-n)]
      {:kind :pen :pos (:pos (first as)) :points (* spare-n drops (foods/points meat))})))

(def round-fields
  "The crop and pen sources of the last round asked: {:engine :root :round :sources}. Their seen-block scan costs 5-16 ms
  a call over 100-600 remembered sections, and choosers ask the reserve many times a round."
  (atom nil))

(defn field-sources
  "crop-sources and pen-sources, read once per round of c's root job (every call without an engine round)."
  [c]
  (let [{:keys [engine root round]} c
        kept @round-fields]
    (if (and round (identical? engine (:engine kept)) (= root (:root kept)) (= round (:round kept)))
      (:sources kept)
      (let [found (vec (concat (crop-sources c) (pen-sources c)))]
        (when round (reset! round-fields {:engine engine :root root :round round :sources found}))
        found))))

(defn sources
  "See the ns doc: each with the seconds of the walk to it from far."
  [c far]
  (let [per-s (cost/walk-blocks-per-s)]
    (for [s (concat (chest-sources c) (field-sources c))]
      (assoc s :walk-s (/ (u/dist far (:pos s)) per-s)))))

(defn chain-trip
  "The trip c's call chain is on when the caller names none: as long as its job-time spend left (ctx/left), ending
  where the body stands; nil (no trip ahead known: the walk back alone) when no line bounds it yet, as in a check
  before the job's first round, or with no engine behind c."
  [c]
  {:s (when (:left c) (get-in (ctx/left c) [:spend :job-time])) :far (u/self-pos c)})

(defn quote-trip
  "The trip of c's own job, by its quote q: food/trip-s of q's job-time, its most held to the spend left, ending at q's
  :moves-to, else where the body stands."
  [c q]
  (let [[least mean most] (get-in q [:uses :job-time] [0 0 ##Inf])
        most (min most (or (:s (chain-trip c)) ##Inf))]
    {:s (food/trip-s [(min least most) (min mean most) most])
     :far (if-let [[x y z] (:moves-to q)] {:x x :y y :z z} (u/self-pos c))}))

(defn carry-points
  "Food points the body must carry on trip {:s :far} (default chain-trip): jobs.lib.cost.food/carry-points of its home
  food (sources from far) and the trip's seconds."
  ([c] (carry-points c (chain-trip c)))
  ([c {:keys [s far]}] (food/carry-points (sources c far) s)))
