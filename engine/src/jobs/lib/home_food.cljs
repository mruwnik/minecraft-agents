(ns jobs.lib.home-food
  "The food a body has at home, from what it sensed: chests it booked food in (memory :fetch/stock) that zones and
  claims let it take from, ripe crops it saw in its own zones and on the crop
  cells of its own plans (anywhere), and the surplus of the food animals it saw or
  noted there (animal-spare's keep rule: the breeders stay). (sources c far) [{:kind :pos :points :blocks :walk-s}], the
  walk from far; (carry-points c trip) what the body must carry on trip (jobs.lib.cost.food/carry-points over them)."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.notes :as notes]
            [engine.args :as a]
            [jobs.lib.access :as access]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.food :as food]
            [jobs.lib.crops :as crops]
            [jobs.lib.foods :as foods]
            [jobs.lib.game-facts :as facts]
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

(defn own-zone-cells
  "The cells of the zones the body owns."
  [c]
  (let [self (ctx/self-name c)]
    (for [{[x0 y0 z0] :min [x1 y1 z1] :max :as zn} (known/zones c)
          :when (zones/same-owner? (:owner zn) self)
          x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))]
      [x y z])))

(defn crop-sources
  "Ripe crops of the body's own zones and plans' crop cells, read from memory (seenBlockAt): a cell's name first, its
  age only for a crop."
  [c]
  (when-let [at (aget (:primitives c) "seenBlockAt")]
    (for [[x y z] (distinct (concat (own-zone-cells c) (known/crop-cells c)))
          :let [^js b (.call at (:primitives c) #js {:x x :y y :z z})
                food (some-> b .-name crop-food)
                age (when food (some-> b .-properties (aget "age") js/Number))
                pos {:x x :y y :z z}]
          :when (and age (>= age (crops/ripe-age (.-name b))) (access/in-own-area? c pos))
          :let [[item share] food]]
      {:kind :crop :pos pos :name (.-name b) :points (* share (foods/points item))})))

(defn kill-points
  "Food points a kill of a food animal kind gives: its expected meat drops (cost/kill-drops) at the raw meat's points."
  [p kind]
  (let [meat (meat-of kind)]
    (* (or (cost/kill-drops kind meat :version (game/version-of p)) cost/expected-drops) (foods/points meat))))

(defn best-kill-points "The most food points one kill of a food animal gives (kill-points)." [p] (apply max (map #(kill-points p %) (keys meat-of))))

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
          :let [spare-n (spare/takeable-adults (spare/keep-adults) (count as))]
          :when (pos? spare-n)]
      {:kind :pen :pos (:pos (first as)) :points (* spare-n (kill-points p kind))})))

(def kept-crops
  "The crop sources of the last ask: {:key :sources}. Their seen-block reads cost ms over a field's cells, and choosers
  ask the reserve many times a round."
  (atom nil))

(defn crops-key
  "What the crop sources depend on: the perception's remembered blocks (its write count moves when a dug or sensed cell
  changes), the zones, the plans' crop cells and the body; nil without a perception (no memo)."
  [c]
  (when-let [^js st (some-> (aget (:primitives c) "perception") :st)]
    [st (.-writes st) (known/zones c) (known/crop-cells c) (ctx/self-name c)]))

(defn field-sources
  "crop-sources, read again whenever crops-key moves, and pen-sources."
  [c]
  (let [k (crops-key c)
        kept @kept-crops
        crops (if (and k (= k (:key kept)))
                (:sources kept)
                (let [found (vec (crop-sources c))]
                  (when k (reset! kept-crops {:key k :sources found}))
                  found))]
    (concat crops (pen-sources c))))

(defn sources
  "See the ns doc, the ripe crops of a 16-block column as one field (its :box of cells and their :crops): each with :blocks and the seconds (:walk-s) of the
  walk to it from far, and the seconds of fetching it from here (:fetch-s: a trip and the walk, cost.value/trip and
  walk-cost) and of each point's work (:point-s: a crop's harvest step, cost/field-step-s; a pen animal's kill, at the
  field's price, cost.food/field-point-s; a chest's none)."
  ([c far] (sources c far (u/self-pos c)))
  ([c far here]
   (let [per-s (cost/walk-blocks-per-s)
         crop-s (cost/field-step-s :harvest 1)
         {crops true others false} (group-by #(= :crop (:kind %)) (concat (chest-sources c) (field-sources c)))
         fields (for [[_ cells] (group-by (fn [{{:keys [x z]} :pos}] [(js/Math.floor (/ x 16)) (js/Math.floor (/ z 16))]) crops)
                      :let [points (reduce + (map :points cells))]]
                  {:kind :crop :pos (:pos (first cells)) :points points :point-s (/ (* crop-s (count cells)) points)
                   :box {:min (update (apply mapv min (map (comp (juxt :x :y :z) :pos) cells)) 1 dec)
                         :max (apply mapv max (map (comp (juxt :x :y :z) :pos) cells))}
                   :crops (vec (distinct (map :name cells)))})]
     (for [s (concat others fields)
           :let [blocks (u/dist far (:pos s))]]
       (assoc s :blocks blocks :walk-s (/ blocks per-s)
              :fetch-s (+ (cost/trip) (:cost (cost/walk-cost {:distance (u/dist here (:pos s))})))
              :point-s (case (:kind s) :chest 0 :crop (:point-s s) (food/field-point-s)))))))

(defn chain-trip
  "The trip c's call chain is on when the caller names none: its job-time spend left (ctx/left), taken whole, ending
  where the body stands; no trip (the walk back alone) when no line bounds it yet, as in a check before the job's first
  round, or with no engine behind c."
  [c]
  (let [s (when (:left c) (get-in (ctx/left c) [:spend :job-time]))]
    {:figure (when s [s s s]) :far (u/self-pos c) :far? true}))

(defn loot-slots
  "{:free :worths :stack-points} of the pack on a trip that yields items {name [least mean most]} (its loot and the spoil
  it digs up): its free slots, the worth of each slot the items' means fill (cost.value/item-value of a stack's share),
  cheapest first, and the food points a stack of the best carried food holds (bread's with none); nil without items."
  [c items]
  (let [p (:primitives c)
        worths (sort (for [[n [_ mean]] items :when (pos? mean)
                           :let [k (js/Math.ceil (/ mean facts/stack-size))
                                 w (/ (:value (cost/item-value [{:name n :count mean}])) k)]
                           _ (range k)]
                       w))
        best (apply max (foods/points "bread") (keep #(when (food/reserve-food? (:name %)) (foods/points (:name %))) (u/inventory p)))]
    (when (seq worths)
      {:free (u/free-slots p) :worths (vec worths) :stack-points (* facts/stack-size best)})))

(defn quote-trip
  "The trip of c's own job, by its quote q: q's job-time figure, its most held to the spend left, ending at q's
  :moves-to, else at place (a fn of c, the job's work place) when given, else unknown (far? false: the walk is
  from where the body stands); :slots, the pack's room for q's yield and its :spoil {name fig} (the blocks the work
  digs up and picks up beside its yield, a trip quote's own key) (loot-slots)."
  ([c q] (quote-trip c q nil))
  ([c q place]
   (let [[least mean most] (get-in q [:uses :job-time] [0 0 ##Inf])
         most (min most (or (get-in (chain-trip c) [:figure 2]) ##Inf))
         to (or (when-let [[x y z] (:moves-to q)] {:x x :y y :z z}) (when place (some-> (place c) a/cell)))
         items (merge-with #(mapv + %1 %2) (get-in q [:yields :items]) (:spoil q))]
     (cond-> {:figure [(min least most) (min mean most) most] :far (or to (u/self-pos c)) :far? (some? to)}
       (seq items) (assoc :slots (loot-slots c items))))))

(defn priced
  "trip {:figure :far :far? :slots} with what cost.food/priced-trip reads of the body: its health, the carried food points
  (:had), a death at far (cost/death-s), and field? (food is got in the field past the home food)."
  [c {:keys [far] :as trip} field?]
  (let [p (:primitives c)]
    (assoc trip
           :health (or (some-> (.self p) .-health) 20)
           :had (foods/stock-points (u/inventory p))
           :death-s (cost/death-s p (ctx/view c) far)
           :field? field?)))

(defn carry-points
  "Food points the body must carry on trip {:figure :far :far?} (default chain-trip): jobs.lib.cost.food/carry-points of
  its home food (sources from far) and the trip priced, food got in the field past it."
  ([c] (carry-points c (chain-trip c)))
  ([c trip] (food/carry-points (sources c (:far trip)) (priced c trip true))))

(defn trip-fetch
  "jobs.lib.cost.food/trip-fetch of the home food (sources from the trip's far end) and trip priced, no field food: what
  to fill the pack to from home before trip, or nil."
  [c trip]
  (food/trip-fetch (sources c (:far trip)) (priced c trip false)))
