(ns engine.contract
  "The job contract (docs/goals-and-actions.md), pure parts. A frame's :contract state in its job memory is
  {:tally {:uses {:items {name n} :wear {tool uses} :health hp} :yields {:items {name n}}} :ran-ms n}.")

(def health-causes
  "engine.hurt causes charged to the act they happen in; a mob's hit is the mob's doing, not the act's."
  #{"fall" "cactus" "berry bush" "drowning"})

(def report-policy {:cap 256 :ttl :forever})

(def worn-parts
  "The equipment parts not also in the bag (mainHand is a hotbar slot)."
  [:head :torso :legs :feet :offHand])

(defn carried-stacks [{:keys [inventory equipment]}]
  (concat inventory (keep #(get equipment %) worn-parts)))

(defn counts [stacks] (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} stacks))

(defn durabilities [stacks]
  (reduce (fn [m {:keys [name durability]}] (cond-> m durability (update name (fnil + 0) durability))) {} stacks))

(defn positive [m] (into {} (filter (comp pos? val)) m))

(defn minus
  "a - b key by key, a missing key counting 0."
  [a b]
  (into {} (map (fn [k] [k (- (get a k 0) (get b k 0))])) (distinct (concat (keys a) (keys b)))))

(defn prune
  "m without nil, zero and empty-map values, nested."
  [m]
  (into {} (keep (fn [[k v]]
                   (let [v (if (map? v) (prune v) v)]
                     (when-not (or (nil? v) (and (number? v) (zero? v)) (and (map? v) (empty? v))) [k v]))))
        m))

(defn act-delta
  "What one act used and yielded, from body snapshots {:inventory :equipment :hurt {cause total}} before and after it,
  and change, the act's own {item delta} where its result gives one. Uses and yields are not netted."
  [before after change]
  (if-not (and (:inventory before) (:inventory after))
    {}
    (let [stacks-before (carried-stacks before)
          stacks-after (carried-stacks after)
          count-before (counts stacks-before)
          count-after (counts stacks-after)
          items (or change (minus count-after count-before))
          kept? #(= (get count-after %) (get count-before %))
          wear (into {} (filter (comp kept? key))
                     (positive (minus (durabilities stacks-before) (durabilities stacks-after))))
          hurt (minus (:hurt after) (:hurt before))]
      (prune {:uses {:items (positive (update-vals items -))
                     :wear wear
                     :health (reduce + (vals (select-keys hurt health-causes)))}
              :yields {:items (positive items)}}))))

(defn add [a b] (merge-with (fn [x y] (if (map? x) (add x y) (+ x y))) a b))

(defn report
  "The report of a frame's :contract state: its tally with the time its rounds ran, in seconds, among the uses."
  [{:keys [tally ran-ms]}]
  (assoc-in tally [:uses :time] (/ (or ran-ms 0) 1000)))
