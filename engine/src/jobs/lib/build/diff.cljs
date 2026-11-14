(ns jobs.lib.build.diff
  "The cells of a plan the body has seen differ from their want, and what mending them is worth."
  (:require [clojure.string :as str]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.build.wood :as wood]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.crops :as crops]
            [jobs.lib.farm :as farm]
            [jobs.lib.look :as look]
            [jobs.lib.price :as price]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.torch :as torch]
            [jobs.lib.torch-plan :as tp]
            [jobs.lib.world :as known]
            [plan.shape :as shape]))

(defn plan-cells
  "{:parts :cells} of plan id: its parts named in parts (nil: every part) and their cells, put on the ground as
  from-plan builds them (plan/surfaced); {:trouble text :later? b} for a plan missing or unreadable (later?: it may be
  saved later) or a part it lacks."
  [c id parts]
  (let [answer (known/plan c id)
        lacking (seq (remove (set (map :id (:parts (:plan answer)))) parts))
        wanted (if (seq parts) (comp (set parts) :part) any?)]
    (cond
      (known/unreadable? answer) {:trouble (str "plan " id (if answer (str " cannot be read: " (:broken answer)) " not found")) :later? true}
      lacking {:trouble (str "plan " id " has no part " (str/join ", " lacking))}
      :else {:parts (filterv #(or (empty? parts) ((set parts) (:id %))) (:parts (:plan answer)))
             :cells (filterv wanted (:cells (plan/surfaced c answer)))})))

(defn others-own?
  "Whether another job keeps a cell with this want: crops and farmland (the farm jobs: tend tills trampled farmland, nothing places it), trees (forestry), and :any (any block will do)."
  [want]
  (or (= :any want) (= "farmland" want)
      (and (map? want) (or (contains? want :crop) (contains? want :tree) (= "farmland" (:block want))))
      (and (vector? want) (some others-own? (rest want)))))

(defn light-loss
  "Seconds a plan light ([x y z] wanting light block `block`) missing costs, as light-area's worth-of prices a dark stay: a
  job's work (risk/work-s :work) at the mean spawn chance under a clear midnight's sky (spawn-light/chance-at, by the light
  the body knows) of the cells of its floor its light reaches (torch-plan/reach), every spawn-light ::step blocks; its
  hurt and death (price/stay-risk-s). nil for a block that gives no light."
  [c [x y z] block]
  (when (or (torch/torch-blocks block) (torch/light-items block))
    (let [reach (tp/reach (str/replace block "wall_" ""))
          step (settings/get spawn-light/settings ::spawn-light/step)
          offs (range (- (* step (quot reach step))) (inc reach) step)
          night (look/sky-subtract-at 18000 0 0)
          chances (for [dx offs dz offs :when (<= (+ (abs dx) (abs dz)) reach)]
                    (spawn-light/chance-at (:primitives c) {:x (+ x dx) :y y :z (+ z dz)} false night))]
      (price/stay-risk-s c (/ (reduce + chances) (count chances)) (risk/work-s :work)))))

(defn diff
  "The cells of `cells` (plan cells) the body has seen differ from their want, judged as from-plan judges them
  (plan/judged, plan/loosened): :missing, :wrong or :extra. A cell never seen, or remembered so long it is likelier
  changed than not, is :unknown and never listed. Never a cell others-own?, nor the upper half of a two-block item (its
  lower half places it). A door, gate or trapdoor where the plan wants another block is wrong; a wanted one's swung state is no difference
  (jobs.maintenance.shut and the pen-gate trigger own it). A light cell carries its light-loss as :loss."
  [c cells]
  (->> (plan/judged (:primitives c) {:cells (remove (comp others-own? :want) cells)} nil {} (first wood/species))
       plan/loosened
       (filterv #(and (#{:missing :wrong :extra} (:answer %))
                      (not (plan/companion? (:want %)))
                      (not (plan/swung? %))))
       (mapv #(if-let [loss (light-loss c (:pos %) (shape/want-block (:want %)))] (assoc % :loss loss) %))))

(defn herd-gaps
  "{:mob :target :gaps n} of each part's :herd with broken cells in its :box or one column round it: a fence or wall
  missing, or a block where the box wants clear, lets an animal out or climbs it over the fence."
  [parts broken]
  (for [{:keys [box herd]} parts
        :when (and box herd)
        :let [[[x1 _ z1] [x2 _ z2]] box
              beside? (fn [[x _ z]] (and (<= (dec (min x1 x2)) x (inc (max x1 x2))) (<= (dec (min z1 z2)) z (inc (max z1 z2)))))
              gaps (count (filter (comp beside? :pos) broken))]
        :when (pos? gaps)]
    (assoc herd :gaps gaps)))

(defn dried
  "The planned field crops (crops/ripe-age) whose farmland only broken water cells of the plan hydrate (farm/hydrated?)."
  [cells broken]
  (let [water (fn [cs] (set (keep #(when (= "water" (shape/want-block (:want %))) (:pos %)) cs)))
        lost (water broken)
        left (reduce disj (water cells) lost)]
    (for [{[x y z] :pos want :want} cells
          :let [crop (when (map? want) (:crop want))]
          :when (and (contains? crops/ripe-age crop) (farm/hydrated? lost x (dec y) z) (not (farm/hydrated? left x (dec y) z)))]
      crop)))

(defn worth
  "cost/repair-worth of mending broken (diff cells) of a plan, given the plan-cells of all its parts: a herd or a crop
  of a part not mended loses by it too."
  [{:keys [parts cells]} broken]
  (cost/repair-worth {:cells broken :herds (herd-gaps parts broken) :dried (dried cells broken)}))
