(ns jobs.items.obtain.plan
  "The craft chain and the gather plan for the names wanted."
  (:require [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.notes :as notes]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.items.obtain.base :refer [craft-radius explore-opts plan-allowed? search-names search-scan-radius search-worth seen-radius way?]]
            [jobs.items.obtain.seen :refer [seen-of sheep-in-range shears-source]]
            [jobs.items.obtain.table :refer [gather-needs grass-seeds hand-dug leaves-of material-blocks need-way searchable? source-opts stalks wool-color wool?]]
            [jobs.items.recipes :as recipes]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.stair-time :as stair-time]
            [jobs.lib.storage :as storage]
            [jobs.lib.threats :as threats]
            [jobs.lib.torch :as torch]
            [jobs.lib.trees :as trees]
            [jobs.lib.util :as u]))

(defn furnace-free
  "Of plans in order of preference, the first with no smelt step (a furnace needs fuel and time), else the first."
  [plans]
  (or (first (remove (fn [pl] (some #(= :smelt (:op %)) (:steps pl))) plans)) (first plans)))

(defn craft-plan
  "{:item name :steps [..]} for the first of names a craft chain of allowed steps (a smelt step needs the :smelt way,
  any other :craft) makes n more of from what is carried, else nil."
  [c names n]
  (let [p (:primitives c)
        have (storage/usable-totals (u/inventory p))
        mem (ctx/mem c)
        table? (and (not (get-in mem [:craft :table-unreachable]))
                    (boolean (or (:table mem) (crafting-table/nearest p (craft-radius)))))
        version (game/version-of p)]
    (furnace-free
     (keep (fn [name]
             (when-let [pl (recipes/plan version have name n (assoc (source-opts) :table? table?))]
               (when (plan-allowed? c pl) (assoc pl :item name))))
           names))))

(defn pick-plan
  "Of the gather plans (one per wanted name) the one that gathers the least wool, so the colour the body carries
  most of; a plan whose sheep are not in range comes after one whose are. {:wool-fallback {:wanted :taking}} on
  the plan when the colour the body carries most of has no sheep in range."
  [c plans]
  (let [wool-of (fn [pl] (into {} (filter (comp wool? key)) (:gather pl)))]
    (if-not (some (comp seq wool-of) plans)
      (furnace-free plans)
      (let [sorted (sort-by #(reduce + 0 (vals (wool-of %))) plans)
            keep-n (if (shears-source c) 0 spare/wool-keep)
            colors (set (keep (fn [[k n]] (when (pos? (spare/takeable-adults keep-n n)) k)) (frequencies (keep #(.-color %) (sheep-in-range c)))))
            colors-of (fn [pl] (map wool-color (keys (wool-of pl))))
            ok? (fn [pl] (every? colors (colors-of pl)))
            chosen (or (first (filter ok? sorted)) (first sorted))
            wanted (first sorted)]
        (cond-> chosen
          (and (not= chosen wanted) (seq (wool-of chosen)) (seq (wool-of wanted)))
          (assoc :wool-fallback {:wanted (first (colors-of wanted)) :taking (first (colors-of chosen))}))))))

(defn raw-seen?
  "Whether every raw need of a gather plan (but wool, left to pick-plan) has a block seen: of several names, the plans
  whose blocks are seen come before one that would have to look for them (coal unseen, logs seen: charcoal). Fish
  seen at water count only with a rod carried: a rod costs more than a search for another food."
  [c plan]
  (let [rod? (pos? (storage/carried (u/inventory (:primitives c)) "fishing_rod"))]
    (every? #(or (:sheep %) (and (seen-of c %) (or rod? (not= 'jobs.food.fish (:job %)))))
            (some-> plan :gather (gather-needs (game/version-of (:primitives c)))))))

(defn any-log
  "The gather map with the logs any species would do as \"log\" (summed); a log only its own species makes (oak_log
  for oak_planks) keeps its name: it stays the plan's even with plenty of another species carried."
  [gather version have name n opts]
  (let [free? (fn [k] (let [other (first (remove #{k} trees/log-names))
                            pl (recipes/plan version (assoc have other 1000) name n opts)]
                        (not (pos? (get-in pl [:gather k] 0)))))
        free (filter (fn [[k _]] (and (trees/log-name? k) (free? k))) gather)]
    (cond-> (apply dissoc gather (map key free))
      (seq free) (update "log" (fnil + 0) (reduce + 0 (vals free))))))

(defn any-of-logs
  "The picked plan with its logs as \"log\" when the plans of the other names gather a log of another species: any
  species then does (planks of any wood), not only the first name's."
  [pl plans]
  (let [log-keys (fn [p] (filter trees/log-name? (keys (:gather p))))]
    (if (< 1 (count (set (mapcat log-keys plans))))
      (update pl :gather (fn [g] (-> (apply dissoc g (log-keys pl))
                                     (update "log" (fnil + 0) (reduce + 0 (map #(get g %) (log-keys pl)))))))
      pl)))

(defn below?
  "Whether the seen block of need lies lower than the body's feet: reaching it means digging down."
  [c need]
  (let [b (seen-of c need)]
    (and b (< (:y (:pos b)) (dec (js/Math.floor (:y (u/self-pos c))))))))

(defn unlit?
  "Whether no seen open cell next to the block at pos is lit as the body perceives it: the way to it is dark."
  [p {:keys [x y z]}]
  (let [open? (fn [[x y z]] (let [cell {:x x :y y :z z}] (and (sh-sense/seen-cell? p cell) (not (sh-sense/solid-at? p cell)))))]
    (->> [[(inc x) y z] [(dec x) y z] [x (inc y) z] [x (dec y) z] [x y (inc z)] [x y (dec z)]]
         (filter open?)
         (every? #(torch/perceived-dark? p %)))))

(defn route-class
  "The class of the body's walks as death-chance reads it: underground while it is under rock (an ore dug down to from
  the surface is priced by its dark way), else over the surface at night while the open sky is dark."
  [p]
  {:underground? (sh-sense/buried? p) :night? (sh-sense/dark-outside? p)})

(defn need-cost
  "Seconds to gather a need whose nearest block is seen: the walk there with the death it risks (threats/route-death-cost),
  the digs, a hostile mob's kills (cost/kill-price, as many as cost/kill-drops says the count takes; Infinity when its
  loot holds none), and for an ore seen
  below the feet a dark way (cost/dark-way-s) unless its cell is lit; Infinity when it
  is dark and no torch is carried: the way down is lit by the torches it is wanted for."
  [c need]
  (let [p (:primitives c)
        src (seen-of c need)
        {:keys [pos name]} (if (map? src) src {:pos (u/pos-of (.-pos src))})
        self (u/self-pos c)
        d (u/dist self pos)
        walk (:cost (cost/walk-cost {:distance d}))
        risk (threats/route-death-cost c (route-class p) pos)
        mob (:mob need)
        kills (when (and mob (cost/hostile? (game/version-of p) {:name mob}))
                (if-let [drops (cost/kill-drops mob (:key need) :version (game/version-of p))]
                  (* (js/Math.ceil (/ (:count need) drops)) (cost/kill-price (threats/death-body c) mob self))
                  js/Infinity))
        dig (+ (or kills 0) (* (:count need) (or (cost/dig-cost name) 0)))
        torches (storage/carried (u/inventory p) "torch")
        dark? (and (:ore? need) (below? c need) (unlit? p pos))]
    (cond
      (not dark?) (+ walk risk dig)
      (not (pos? torches)) js/Infinity
      :else (+ dig risk (cost/dark-way-s walk d torches (threats/body-of p))))))

(defn look-cost
  "Seconds per find of a search for an unseen need (cost/search-s): legs of the search's spacing, each the walk and the
  death it risks now, a look of the gather's scan radius (cost/look-leg-chance), within the call's search-worth, then
  the walk to what it saw. Infinity when the call may not search, its search for the need ran already (it found none:
  the seen way or the tunnel is next) or no chance is priced (logs and other blocks)."
  [c need]
  (if-not (and (explore-opts c) (searchable? need) (zero? (get-in (ctx/mem c) [:explored (:key need)] 0)))
    js/Infinity
    (let [p (:primitives c)
          self (u/self-pos c)
          r (search-scan-radius)
          leg (+ (:cost (cost/walk-cost {:distance (cost/leg-spacing)}))
                 (cost/route-death-cost (threats/death-body c) (route-class p) self))]
      (+ (cost/search-s (cost/look-leg-chance p (notes/notes c) (search-names need) [(:x self) (:z self)] r) leg (search-worth c))
         (:cost (cost/walk-cost {:distance r}))))))

(defn tunnel-way
  "{:way :tunnel :cost :y} of a tunnelled ore need as jobs.gather.mine gets it unseen: a stair down from the feet to
  its cost/ore-level (where the body stands when that is higher or none is set), then a strip tunnel there, legs of the cost/torch-gap
  one torch lights finding by cost/tunnel-chance within the call's search-worth (cost/search-s). Stair and legs are dark ways
  (cost/dark-way-s over stair-time's seconds a step), each risking a death under rock. :cost Infinity for a need not
  tunnelled for or not an ore."
  [c need]
  (let [block (:block (:args need))
        ore (cost/ore-of [block])
        level (cost/ore-level block)]
    (if-not (and (:tunnel? need) ore)
      {:way :tunnel :cost js/Infinity}
      (let [self (u/self-pos c)
            feet (js/Math.floor (:y self))
            y (min feet (or level feet))
            at (assoc self :y y)
            p (:primitives c)
            torches (storage/carried (u/inventory p) "torch")
            kit (threats/body-of p)
            risk (threats/route-death-cost c {:underground? true :night? false} at)
            n cost/torch-gap
            leg (+ risk (cost/dark-way-s (stair-time/steps-s :level n) n torches kit))
            down (+ risk (cost/dark-way-s (stair-time/steps-s :down (- feet y)) (- feet y) torches kit))]
        {:way :tunnel :y y
         :cost (+ down (cost/search-s (cost/tunnel-chance ore y n) leg (search-worth c)))}))))

(defn search-way
  "How an unseen need is best looked for: {:way :look :cost} (look-cost), or for a tunnelled one its tunnel-way when that
  costs less."
  [c need]
  (let [look {:way :look :cost (look-cost c need)}
        tunnel (when (:tunnel? need) (tunnel-way c need))]
    (if (and tunnel (<= (:cost tunnel) (:cost look))) tunnel look)))

(defn search-cost "Seconds per find of the cheaper search-way for an unseen need." [c need] (:cost (search-way c need)))

(defn plan-cost
  "Seconds to gather a plan's raw needs (need-cost, search-cost when a need has no block seen) and smelt what it smelts."
  [c version pl]
  (+ (reduce + 0 (map #(if (seen-of c %) (need-cost c %) (search-cost c %)) (some-> pl :gather (gather-needs version))))
     (* cost/smelt-step (count (filter #(= :smelt (:op %)) (:steps pl))))))

(defn preferred
  "The gather plan pl, or when it gathers a need with no source seen, or mines an ore seen below the feet, and the same
  item is made another way from raw needs all seen (white wool from a seen spider's string, charcoal from logs for
  torches) that costs less (plan-cost: an unseen need its search per find, Infinity for logs; the dark way down to coal,
  with no torch to light it, costs more than any walk to logs), that plan. A way whose own needs are unseen (wool dyed
  from wool) is passed over for the next while that leaves out more items; none is left once it leaves out none."
  [c version pl have name n opts]
  (let [needs #(some-> % :gather (gather-needs version))
        unseen #(set (map :key (remove (fn [need] (seen-of c need)) (needs %))))
        hard (some->> (needs pl) (filter #(or (nil? (seen-of c %)) (and (:ore? %) (below? c %)))) (map :key) set not-empty)
        alt (loop [off hard]
              (when off
                (when-let [alt (recipes/plan version have name n (assoc opts :ungathered off))]
                  (let [more (unseen alt)]
                    (cond (empty? more) alt
                          (every? off more) nil
                          :else (recur (into off more)))))))]
    (if (and alt (plan-allowed? c alt) (< (plan-cost c version alt) (plan-cost c version pl)))
      alt
      pl)))

(defn gather-plan
  "The craft chain for the first of names that has one once the raw items it lacks are gathered, with :gather
  {name n} when it lacks any, else nil. A sapling has no recipe: its plan is just :gather {name n}, when its leaves are seen (or its child is running).
  Of several names that lack wool, the one gathering the least (pick-plan)."
  [c names n]
  (let [p (:primitives c)
        have (storage/usable-totals (u/inventory p))
        version (game/version-of p)
        seen? #(seq (look/seen-blocks p {:names (vec (material-blocks version %)) :radius (seen-radius) :max 1}))
        seen (set (filter seen? (recipes/stone-materials version)))
        materials (if (seq seen) seen (recipes/stone-materials version))
        allowed (fn [pl] (when (and pl (every? #(way? c (need-way %)) (some-> pl :gather (gather-needs version)))) pl))
        plan-of (fn [name]
                  (if-let [seen-of-name (cond (hand-dug name) (hand-dug name)
                                              (leaves-of name) #{(leaves-of name)}
                                              (grass-seeds name) (grass-seeds name)
                                              (stalks name) #{name}
                                              (= "water_bucket" name) #{"water"})]
                    (when (or (= "water_bucket" name)
                              (= name (get-in (ctx/mem c) [:gather :item]))
                              (seq (look/seen-blocks p {:names (vec seen-of-name) :radius (seen-radius) :max 1})))
                      {:steps [] :gather {name n} :item name})
                    (let [opts (merge (source-opts)
                                      {:table? (boolean (or (:table (ctx/mem c)) (crafting-table/nearest p (craft-radius))))
                                       :gather? true :materials materials})]
                      (when-let [pl (preferred c version (recipes/plan version have name n opts) have name n opts)]
                        (when (plan-allowed? c pl)
                          (-> pl
                              (update :gather any-log version have name n opts)
                              (assoc :item name)))))))]
    (if (= 1 (count names))
      (allowed (plan-of (first names)))
      (let [plans (keep (comp allowed plan-of) names)]
        (some-> (pick-plan c (or (not-empty (filter #(raw-seen? c %) plans)) plans)) (any-of-logs plans))))))
