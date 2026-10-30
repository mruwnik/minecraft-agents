(ns jobs.lib.cost.food
  "The food a body keeps carried: three days of it. A body loses about 20 hunger points a day (a Minecraft day is
  20 minutes; sprinting, jumping, fighting and healing drain hunger, plain walking and mining hardly do; an
  estimate, not a measure, kept on the safe side), so the reserve is 60 points. Pure over an inventory
  ([{:name :count}]); food is what jobs.lib.foods calls edible, except precious food (golden apples), which is kept
  for emergencies and never counts; precious-worth, eat-precious?: what one gives, whether it is worth eating."
  (:require [engine.game :as game]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.precious :as precious]
            [jobs.lib.cost.value :as value]
            [jobs.lib.foods :as foods]))

(def days 3)

(def points-per-day 20)

(def reserve-points (* days points-per-day))

(def day-s "Seconds a Minecraft day lasts." 1200)

(def points-per-s "Hunger points a second: points-per-day over day-s." (/ points-per-day day-s))

(defn trip-s
  "Seconds a trip whose time is the figure [least mean most] keeps food for: its mean plus one standard deviation of a
  triangular spread over [least most] peaking at the mean, never past most; ##Inf when most is."
  [[least mean most]]
  (if (js/isFinite most)
    (min most (+ mean (js/Math.sqrt (/ (- (+ (* least least) (* mean mean) (* most most)) (* least mean) (* least most) (* mean most))
                                       18))))
    ##Inf))

(defn carry-points
  "Food points the body must carry, given its home food [{:points :walk-s}] (seen stocked chests, ripe crops, pen
  surplus; walk-s the walk to it from the trip's far end) and the trip's seconds (trip-s; nil: no trip): the hunger of
  the trip and the walk back to the nearest home food, and at least the 3-day reserve less that food (each source less
  the hunger of its walk). No home food, or a trip with no bound (##Inf): the trip's hunger, never under the reserve."
  [home trip]
  (let [home (filter (comp pos? :points) home)
        trip (or trip 0)]
    (if (or (empty? home) (not (js/isFinite trip)))
      (max reserve-points (if (js/isFinite trip) (* trip points-per-s) 0))
      (let [back (apply min (map :walk-s home))
            stored (reduce + (map #(max 0 (- (:points %) (* (:walk-s %) points-per-s))) home))]
        (max (* (+ trip back) points-per-s) (- reserve-points stored))))))

(defn reserve-food?
  "Food that counts toward the reserve: edible and not precious."
  [name]
  (and (foods/edible? name) (not (contains? foods/precious name))))

(defn food-reserve
  "{name count}: the carried food to keep, best (most hunger points) first, until it holds points (default
  reserve-points); the last name keeps only what is still needed."
  ([inventory] (food-reserve inventory reserve-points))
  ([inventory points]
  (let [totals (reduce (fn [m {:keys [name count]}] (cond-> m (reserve-food? name) (update name (fnil + 0) count)))
                       {} inventory)]
    (loop [names (sort-by (juxt #(- (foods/points %)) identity) (keys totals))
           need points
           kept {}]
      (if (or (empty? names) (<= need 0))
        kept
        (let [n (first names)
              per (foods/points n)
              take-n (min (totals n) (js/Math.ceil (/ need (max per 1))))]
          (recur (rest names) (- need (* per take-n)) (assoc kept n take-n))))))))

(defn precious-worth
  "Seconds what precious food item gives body is worth, one sum: the hp it adds (precious/hp-worth: its regeneration up
  to 20, each at most what waiting costs an hp while the body heals by waiting, heals?, then its absorption over that)
  paid only by the chance the body lives, plus, against a known answer met at `at` with death the seconds a death costs
  (answer/decide's :death), the death it averts: the fall in answer/answer-death-chance with the item eaten, at death."
  ([body item heals?] (precious-worth body item heals? nil nil nil))
  ([body item heals? known at death]
   (let [eaten (precious/fed body item)
         hp (precious/hp-worth body item heals?)]
     (if known
       (let [before (answer/answer-death-chance body known at death)
             after (answer/answer-death-chance eaten known at death)]
         (+ (* (- 1 after) hp) (* (- before after) death)))
       hp))))

(defn eat-precious?
  "Whether precious food item is worth eating: what it gives (precious-worth, the same args) is worth more than the item
  (cost.value/item-value)."
  [body item & more]
  (> (apply precious-worth body item more) (:value (value/item-value [{:name item :count 1}]))))

(defn cooked-name
  "The cooked food a raw one gives, or nil."
  [name]
  (when-let [cooked (foods/cooked-of name)]
    (when (foods/food? cooked) cooked)))

(def meat-animal "The animal a raw meat comes from; nil for any other food." game/meat-animal)

(defn meat-factor
  "What a raw meat is worth against its cooking gain, for an animal kind seen {:wild n :penned n} (adults near the
  body outside and inside pens it knows): 1 when a working pen (opts :pen-adults penned adults) breeds more;
  otherwise 1 + :wild-premium, less as more wild adults (:wild-relief of them halve the premium) are near, since
  every hunt leaves fewer."
  [{:keys [wild penned]} {:keys [pen-adults wild-premium wild-relief]}]
  (if (>= penned pen-adults)
    1
    (+ 1 (/ wild-premium (+ 1 (/ wild wild-relief))))))

(defn cook-gain
  "Hunger points cooking the raw food in inventory ([{:name :count}]) adds over eating it raw, each raw food's gain
  times (factor-of name), its replacement cost (1 when omitted)."
  ([inventory] (cook-gain inventory (constantly 1)))
  ([inventory factor-of]
   (reduce (fn [n {:keys [name count]}]
             (if-let [cooked (cooked-name name)]
               (+ n (* (factor-of name) count (max 0 (- (foods/points cooked) (or (foods/points name) 0)))))
               n))
           0 inventory)))

(defn cook-worth?
  "Whether cooking the raw food in inventory beats eating it now: the points gained, each raw food valued by its
  replacement cost (costs :meat-factor, name -> number, default 1: wild meat is dear, see meat-factor), cover the walk
  to a furnace (distance, nil when none is known: one is made, :furnace-cost, or only put down, :place-cost, when
  carried-cooker? says one is in the pack; :walk-cost a block) with the cost growing as food falls. costs:
  {:furnace-cost :place-cost :walk-cost :meat-factor}. Whether time allows (starvation ticking, a hostile near) is
  the caller's."
  [inventory food distance carried-cooker? {:keys [furnace-cost place-cost walk-cost meat-factor]}]
  (let [cost (cond distance (* distance walk-cost) carried-cooker? place-cost :else furnace-cost)]
    (>= (cook-gain inventory (or meat-factor (constantly 1))) (* cost (+ 1 (/ (- 20 food) 20))))))
