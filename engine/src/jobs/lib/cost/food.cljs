(ns jobs.lib.cost.food
  "The food a body keeps carried: three days of it. A body loses about 20 hunger points a day (a Minecraft day is
  20 minutes; sprinting, jumping, fighting and healing drain hunger, plain walking and mining hardly do; an
  estimate, not a measure, kept on the safe side), so the reserve is 60 points. Pure over an inventory
  ([{:name :count}]); food is what jobs.lib.foods calls edible, except precious food (golden apples), which is kept
  for emergencies and never counts; precious-worth, eat-precious?: what one gives, whether it is worth eating."
  (:require [engine.args :as a]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.precious :as precious]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.cost.value :as value]
            [jobs.lib.foods :as foods]))

(def days 3)

(def points-per-day 20)

(def reserve-points (* days points-per-day))

(def day-s "Seconds a Minecraft day lasts." 1200)

(def points-per-s "Hunger points a second: points-per-day over day-s." (/ points-per-day day-s))

(a/defargs settings
  {::field-point-s {:default 13.61 :spec (a/num-in 0 nil)
                    :doc "Seconds a hunger point of food got in the field takes (a hunt, crops, a cook): 15783 s of 746 hungry get-food calls of the trial bodies that took nothing from a chest, cut and fruitless ones counted, over the 1160 points their food gave, 2026-10-30. Recount: node tools/event-counts.mjs food-gets (:point-s)."}})

(defn field-point-s [] (settings/get settings ::field-point-s))

(defn trip-spread
  "{:tail :beyond} of a trip whose time is the figure [least mean most]: (tail t) the chance it lasts more than t seconds,
  (beyond t) the seconds it is expected to last past t, under the spread of most entropy over [least most] with that mean (a
  truncated exponential; past least a plain one when most is ##Inf), so the spread's mean is the figure's."
  [[least mean most]]
  (let [span (- most least)
        m (- mean least)
        [tail beyond]
        (cond
          (<= m 0) [(constantly 0) (constantly 0)]
          (>= m span) [(constantly 1) #(- span %)]
          (not (js/isFinite span)) [#(js/Math.exp (- (/ % m))) #(* m (js/Math.exp (- (/ % m))))]
          :else
          (let [r (/ m span)
                share #(if (< (abs %) 1e-6) (- 0.5 (/ % 12)) (- (/ 1 %) (/ 1 (js/Math.expm1 %))))
                x (loop [lo -700 hi 700 i 0]
                    (let [mid (/ (+ lo hi) 2)]
                      (cond (= i 60) mid
                            (> (share mid) r) (recur mid hi (inc i))
                            :else (recur lo mid (inc i)))))]
            (if (< (abs x) 1e-6)
              [#(- 1 (/ % span)) #(/ (* (- span %) (- span %)) (* 2 span))]
              [#(/ (js/Math.expm1 (* x (- 1 (/ % span)))) (js/Math.expm1 x))
               #(/ (- (* (/ span x) (js/Math.expm1 (* x (- 1 (/ % span))))) (- span %)) (js/Math.expm1 x))])))]
    {:tail (fn [t] (cond (< t least) 1 (>= t most) 0 :else (tail (- t least))))
     :beyond (fn [t] (cond (>= t most) 0
                           (< t least) (+ (- least t) (beyond 0))
                           :else (max 0 (beyond (- t least)))))}))

(defn outlasts "trip-spread's chance that a trip of figure lasts more than t seconds." [figure t] ((:tail (trip-spread figure)) t))

(defn death-loss
  "Seconds a carried food point costs by the chance the trip kills the body: food-point-worth times the chance a :long
  work of mean seconds kills it at health (cost.risk/work-death-chance)."
  [health mean]
  (* (value/food-point-worth) (risk/work-death-chance :long health (or mean 0))))

(defn acquire-s
  "Seconds getting the food points past had up to k takes, by lots [{:points :fixed :per}] taken in order (a lot's :fixed
  once any of it is taken, :per a point), then field-point-s a point while field?, else ##Inf; 0 up to had."
  [{:keys [had lots field?]} k]
  (loop [need (- k (or had 0)) [{:keys [points fixed per]} & more :as ls] lots s 0]
    (cond (<= need 0) s
          (empty? ls) (if field? (+ s (* need (field-point-s))) ##Inf)
          :else (let [n (min need points)] (recur (- need n) more (+ s fixed (* per n)))))))

(defn slot-s
  "Seconds k food points cost in pack slots: each slot more than had's that the food needs (a stack of :stack-points), past
  the free slots the trip's loot leaves, costs the loot a slot holds (:worth); 0 without :slots."
  [{:keys [had slots]} k]
  (let [{:keys [free loot worth stack-points]} slots]
    (if (and slots (pos? stack-points))
      (* worth (max 0 (- (js/Math.ceil (/ (max 0 (- k (or had 0))) stack-points)) (max 0 (- free loot)))))
      0)))

(defn run-out-s
  "Seconds running out of k carried points costs on trip, at the cheapest way: a death (death-s), the walk home and out
  again (:walk-out, with a known far end), each by the chance the trip and its walk back (:back-s) outlast the food, or
  getting the food short on the way at field-point-s a point."
  [{:keys [death-s walk-out back-s]} {:keys [tail beyond]} k]
  (let [t (- (/ k points-per-s) back-s)
        p (tail t)]
    (min (value/chance-times p death-s) (value/chance-times p walk-out) (* (field-point-s) points-per-s (beyond t)))))

(defn trip-cost
  "Expected seconds carrying k food points on trip (priced-trip) costs: getting them (acquire-s), their slots (slot-s), the
  ones a death loses (:point-cost each) and running out (run-out-s)."
  ([trip k] (trip-cost trip (trip-spread (:figure trip)) k))
  ([trip sp k]
   (+ (acquire-s trip k) (slot-s trip k) (* (:point-cost trip) k) (run-out-s trip sp k))))

(defn trip-points
  "The food points whose trip-cost is least on trip; 0 without a trip (nil figure). The search stops once running out
  costs less than a point's death loss, or a microsecond: more points cannot pay."
  [{:keys [figure point-cost] :as trip}]
  (if-not figure
    0
    (let [sp (trip-spread figure)]
      (loop [k 0 best 0 best-cost (trip-cost trip sp 0)]
        (let [out (run-out-s trip sp k)]
          (if (or (zero? out) (< out (max point-cost 1e-6)) (not (js/isFinite (acquire-s trip (inc k)))))
            best
            (let [k (inc k) ck (trip-cost trip sp k)]
              (if (< ck best-cost) (recur k k ck) (recur k best best-cost)))))))))

(defn point-price
  "Seconds a point of home food source {:points :fetch-s :point-s} costs, taken whole: the order the trip's lots and
  get-food take home food in."
  [{:keys [points fetch-s point-s]}]
  (/ (+ (or fetch-s 0) (* (or point-s 0) points)) points))

(defn priced-trip
  "Trip {:figure :health :death-s :far? :had :field? :slots} over home food [{:points :walk-s :blocks :fetch-s :point-s}]
  (seen stocked chests, ripe crops, pen surplus; walk-s and blocks the walk to it from the trip's far end, fetch-s the
  fetch of it from here, point-s each point's work) with :point-cost (death-loss over the figure's mean), :back-s (the walk
  to the nearest), :walk-out (with a known far end, far?: that walk and back, cost.value/trip and walk-cost) and :lots
  (the home food, cheapest a point first)."
  [home {:keys [figure health far?] :as trip}]
  (let [home (filter (comp pos? :points) home)
        near (first (sort-by :walk-s home))]
    (assoc trip
           :point-cost (death-loss (or health 20) (second figure))
           :back-s (or (:walk-s near) 0)
           :walk-out (if (and near far?) (+ (value/trip) (:cost (value/walk-cost {:distance (:blocks near)}))) ##Inf)
           :lots (->> (sort-by point-price home)
                      (map (fn [{:keys [points fetch-s point-s]}] {:points points :fixed (or fetch-s 0) :per (or point-s 0)}))))))

(defn carry-points
  "Food points the body must carry on trip (priced-trip's args) with home food: trip-points, and at least the 3-day
  reserve less the home food (each source less the hunger of its walk)."
  [home trip]
  (let [stored (reduce + (map #(max 0 (- (:points %) (* (:walk-s %) points-per-s))) (filter (comp pos? :points) home)))]
    (max (trip-points (priced-trip home trip)) (- reserve-points stored))))

(defn worth-fetching?
  "Whether home food source {:points :fetch-s :point-s} pays its fetch for want points: its fetch and work for the points
  it gives toward them cost less than getting them in the field (field-point-s a point)."
  [{:keys [points fetch-s point-s]} want]
  (let [n (min points want)]
    (and (pos? n) (< (+ fetch-s (* point-s n)) (* n (field-point-s))))))

(defn fetchable
  "The home food sources get-food takes for want points, in its order: cheapest a point first (point-price), each while
  it is worth-fetching? for the points still wanted."
  [home want]
  (loop [[s & more :as ss] (sort-by point-price (filter (comp pos? :points) home)) want want out []]
    (cond (or (empty? ss) (<= want 0)) out
          (worth-fetching? s want) (recur more (- want (:points s)) (conj out s))
          :else (recur more want out))))

(defn fetch-points
  "The food points to fill the pack to before trip (priced-trip's args, :had the carried points), or nil: carry-points,
  at most the carried food and the home food get-food takes for the rest (fetchable), when that is more than is carried."
  [home trip]
  (let [had (or (:had trip) 0)
        want (carry-points home trip)
        to (min want (+ had (reduce + (map :points (fetchable home (- want had))))))]
    (when (> to had) to)))

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
