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

(defn night-chance-fn
  "(fn [cell lit]) the spawn chance per second at a cell under a clear midnight's sky (spawn-light/chance-at, by the light the
  body knows, or by block light `lit` where given), remembered per cell and light, with one unseen-solid-at-fn: the
  floors of lights close together share cells."
  [c]
  (let [p (:primitives c)
        night (look/sky-subtract-at 18000 0 0)
        solid-at (look/unseen-solid-at-fn p)]
    (memoize (fn [cell lit] (spawn-light/chance-at p cell false night false solid-at lit)))))

(defn- light-of [block] (tp/light-of (str/replace block "wall_" "")))

(defn- lit-by
  "The block light a light at `pos` of `block` gives `cell` by its spread through open air (a step less a block)."
  [[x y z] block {cx :x cy :y cz :z}]
  (max 0 (- (light-of block) (+ (abs (- x cx)) (abs (- y cy)) (abs (- z cz))))))

(defn light-loss
  "Seconds a plan light ([x y z] wanting light block `block`) missing costs, as light-area's worth-of prices a dark stay
  (risk/dark-stay-s) at the `chance-at` ((night-chance-fn c), as given) of the cells of its floor its light reaches
  (torch-plan/reach), every spawn-light ::step blocks. A cell remembered lit by the light is judged by what it would
  keep without it: the light remembered there where more than the light gave, else the most `others` ([pos block] of
  the plan's lights standing) give it, no more than remembered. nil for a block that gives no light."
  ([c pos block] (light-loss c pos block (night-chance-fn c) nil))
  ([c [x y z :as pos] block chance-at others]
   (when (or (torch/torch-blocks block) (torch/light-items block))
     (let [p (:primitives c)
           reach (tp/reach (str/replace block "wall_" ""))
           step (settings/get spawn-light/settings ::spawn-light/step)
           offs (range (- (* step (quot reach step))) (inc reach) step)
           without (fn [cell]
                     (let [seen (spawn-light/seen-light p (spawn-light/standing p cell))]
                       (when seen
                         (if (> seen (lit-by pos block cell))
                           seen
                           (min seen (reduce max 0 (map (fn [[o b]] (lit-by o b cell)) others)))))))
           chances (for [dx offs dz offs :when (<= (+ (abs dx) (abs dz)) reach)
                         :let [cell {:x (+ x dx) :y y :z (+ z dz)}]]
                     (chance-at cell (without cell)))]
       (risk/dark-stay-s #(price/stay-risk-s c %1 %2) chances)))))

(defn diff
  "The cells of `cells` (plan cells) the body has seen differ from their want, judged as from-plan judges them
  (plan/judged, plan/loosened): :missing, :wrong or :extra. A cell never seen, or remembered so long it is likelier
  changed than not, is :unknown and never listed. Never a cell others-own?, nor the upper half of a two-block item (its
  lower half places it). A door, gate or trapdoor where the plan wants another block is wrong; a wanted one's swung state is no difference
  (jobs.maintenance.shut and the pen-gate trigger own it). A light cell carries its light-loss as :loss."
  [c cells]
  (let [chance-at (delay (night-chance-fn c))
        judged (->> (plan/judged (:primitives c) {:cells (remove (comp others-own? :want) cells)} nil {} (first wood/species))
                    plan/loosened)
        gives-light? #(let [b (shape/want-block (:want %))] (or (torch/torch-blocks b) (torch/light-items b)))
        standing (vec (for [cell judged :when (and (= :match (:answer cell)) (gives-light? cell))]
                        [(:pos cell) (shape/want-block (:want cell))]))]
    (->> judged
         (filterv #(and (#{:missing :wrong :extra} (:answer %))
                        (not (plan/companion? (:want %)))
                        (not (plan/swung? %))))
         (mapv #(if-let [loss (light-loss c (:pos %) (shape/want-block (:want %)) (fn [cell lit] (@chance-at cell lit)) standing)]
                  (assoc % :loss loss)
                  %)))))

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
