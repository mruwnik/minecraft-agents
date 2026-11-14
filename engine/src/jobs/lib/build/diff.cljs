(ns jobs.lib.build.diff
  "The cells of a plan the body has seen differ from their want, and what mending them is worth."
  (:require [clojure.string :as str]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.build.wood :as wood]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]
            [jobs.lib.farm :as farm]
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

(defn diff
  "The cells of `cells` (plan cells) the body has seen differ from their want, judged as from-plan judges them
  (plan/judged, plan/loosened): :missing, :wrong or :extra. A cell never seen, or remembered so long it is likelier
  changed than not, is :unknown and never listed. Never a cell others-own?, the upper half of a two-block item (its
  lower half places it), nor a door, gate or trapdoor standing where it should not or swung wrong: from-plan never digs
  one, and shutting a gate is jobs.maintenance.shut's and the pen-gate trigger's."
  [c cells]
  (->> (plan/judged (:primitives c) {:cells (remove (comp others-own? :want) cells)} nil {} (first wood/species))
       plan/loosened
       (filterv #(and (#{:missing :wrong :extra} (:answer %))
                      (not (plan/companion? (:want %)))
                      (not (and (#{:wrong :extra} (:answer %)) (some->> (:found %) (re-find plan/opening-pattern))))))))

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
