(ns jobs.movement.go-to.aim
  "Where a go-to call walks to: the goal from :pos or :place, a column's ground aimed from what the body has seen, the looks
  and waypoints toward a guessed one, and whether the body is there."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.sight :as sight]
            [jobs.lib.aim :as aim]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.sense :as sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.walk.one-way :as one-way]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.world :as known]
            [jobs.lib.column-kinds :as kinds]
            [jobs.movement.go-to.column :as column]
            [jobs.movement.go-to.tuning :as tuning :refer [max-waypoint-misses waypoint-gain way-looks]]))

(defn see-cell
  "The cell [x y z] the eye must see under :see: the :pos arg's block (not the cell the walk aims at, which may stand for
  it: a crop's farmland), else nil (no :see, a column or a place)."
  [c]
  (when (:see (:args c))
    (let [{:keys [x y z]} (:pos (places/parse-pos (:pos (:args c))))]
      (when (and x y z) [x y z]))))

(defn here?
  "Whether the body is within range of pos, by its position or by the cell it stands on, and under :see sees pos's block."
  [c pos range]
  (and (or (u/within? (u/self-pos c) pos range) (u/within? (reach/standing-cell (:primitives c)) pos range))
       (if-let [cell (see-cell c)] (aim/body-sees? c cell) true)))

(defn sight-end
  "A fruitless round's result as go-to gives it up under :see: a search that ran out of land (:exhausted) found no stand
  in range that sees the block: :out-of-sight."
  [c pos result]
  (if (and (see-cell c) (= :exhausted (some-> (:reason result) keyword)))
    (assoc result :reason :out-of-sight)
    result))

(defn ^:async look-at-goal-air!
  "Under water, a plan refused for air whose goal head cell the body has not seen (the planner counts only seen air as a
  way to air): one look at that cell a call (mem :air-looked); true when it looked."
  [c pos]
  (let [p (:primitives c)
        cell (mapv #(js/Math.floor %) [(:x pos) (inc (:y pos)) (:z pos)])]
    (when (and (not (:air-looked (ctx/mem c))) (breath/head-under? p) (sense/unknown? p cell))
      (ctx/update-mem! c assoc :air-looked true)
      (await (sense/look-at! c cell))
      true)))

(defn aimed
  "{:pos cell :resolved bool} to walk to for goal (target's :pos; no :y for a column), jobs.movement.go-to.column/aim."
  [c goal]
  (let [{:keys [unknown]} (:args c)]
    (column/aim (:primitives c) goal (tuning/y-slack) (js/Math.floor (:y (u/self-pos c))) (not unknown))))

(defn void-guess?
  "Whether a column goal's ground is only guessed (mem :column-open :guess) under a void: the body within approach-range of
  the column, whose guess (mem :aimed) lies so far below the feet that the drop would hurt, the column seen open from the
  feet down to it with no ground in view (a hurting drop onto ground never seen is none: it may lie deeper still)."
  [c]
  (let [{:keys [column-open aimed]} (ctx/mem c)
        me (u/self-pos c)
        feet (js/Math.floor (:y me))
        ka (kinds/seen-kind-at (:primitives c))]
    (boolean (and (= :guess column-open) aimed
                  (<= (js/Math.hypot (- (:x aimed) (:x me)) (- (:z aimed) (:z me))) column/approach-range)
                  (pos? (cost/fall-damage (cost/equipment-of (.-equipment (.self (:primitives c)))) (- feet (:y aimed))))
                  (every? #(kinds/free? (ka (:x aimed) % (:z aimed))) (range (:y aimed) (inc feet)))))))

(defn not-standable?
  "Whether the planner answers goal-not-standable for pos alone (no search, wplan/goal-stand-problem)."
  [c pos]
  (let [pw (wworld/path-world (:primitives c) (:unknown c))]
    (boolean (and pw (= :goal-not-standable (wplan/goal-stand-problem c pw [(:x pos) (:y pos) (:z pos)] (:range (:args c) 1)))))))

(defn seen-stand-within?
  "Whether a cell within r0 (at most column/approach-range) of the cell [x y z] stands by the cells the body has seen
  (kind-at: kinds/seen-kind-at)."
  [kind-at [x y z] r0]
  (let [reach (min r0 column/approach-range)
        r (js/Math.floor reach)]
    (boolean (some (fn [[dx dy dz]] (and (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* reach reach))
                                         (column/stands? kind-at (+ x dx) (+ y dy) (+ z dz))))
                   (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))] [dx dy dz])))))

(defn in-seen-ground?
  "Under :unknown, whether the y goal pos is a position in the ground with no seen stand within the :range arg of it: a
  seen cell that is not free, or an unseen one under a seen solid cell of its column within :y-slack (the roof of a
  plateau: its inside is never shown), with the body within approach-range of the column. A door or gate cell is no ground: the
  walk opens it."
  [c pos]
  (boolean (and (:unknown c) (:y pos)
                (let [ka (kinds/seen-kind-at (:primitives c))
                      cell (mapv #(js/Math.floor (% pos)) [:x :y :z])
                      [x y z] cell
                      k (apply ka cell)
                      door? (some-> (u/seen-block (:primitives c) {:x x :y y :z z}) .-name reach/openable?)
                      me (u/self-pos c)
                      roofed? (and (keyword-identical? :unknown k)
                                   (<= (js/Math.hypot (- x (:x me)) (- z (:z me))) column/approach-range)
                                   (some #(keyword-identical? :solid (ka x (+ y %) z)) (range 1 (inc (tuning/y-slack)))))]
                  (and (or roofed? (not (or (keyword-identical? :unknown k) (kinds/free? k) door?)))
                       (not (seen-stand-within? ka cell (:range (:args c) 1))))))))

(def lookout-reach "Blocks from a y goal hidden in seen ground (hidden-in-ground?) a stand go-to looks at it from may be." 4)

(def max-lookouts "Stands a call looks at a y goal hidden in seen ground from (lookout) before it plans to it as it is." 3)

(defn hidden-in-ground?
  "Under :unknown, whether the y goal pos is a cell the body never saw behind a seen solid: the line from the eye to
  its centre crosses one (a cell inside a mound, seen only from outside)."
  [c pos]
  (boolean (and (:unknown c) (:y pos)
                (let [p (:primitives c)
                      [x y z :as cell] (mapv #(js/Math.floor (% pos)) [:x :y :z])
                      ka (aim/seen-kind-at p)
                      [ex ey ez] (aim/eye-at (u/self-pos c))]
                  (and (sense/unknown? p cell)
                       (not (sight/line-clear ex ey ez (+ x 0.5) (+ y 0.5) (+ z 0.5) #(keyword-identical? :solid (ka %1 %2 %3)))))))))

(defn lookout
  "For an unseen y goal pos under :unknown, not re-aimed, with fewer than max-lookouts stands tried (mem :lookouts): the
  seen stand [x y z] within lookout-reach of its cell, none tried, whose eye sees the cell over seen cells
  (jobs.lib.aim), nearest the body; else nil."
  [c pos]
  (let [{:keys [re-aim column-open] :as m} (ctx/mem c)
        tried (:lookouts m [])
        p (:primitives c)
        [x y z :as cell] (mapv #(js/Math.floor (% pos)) [:x :y :z])]
    (when (and (:unknown c) (not re-aim) (nil? column-open) (< (count tried) max-lookouts) (sense/unknown? p cell))
      (let [ka (kinds/seen-kind-at p)
            sees? (aim/stand-sees-fn p cell)
            r lookout-reach
            from (u/self-pos c)]
        (->> (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))
                   :when (<= (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))]
               [(+ x dx) (+ y dy) (+ z dz)])
             (remove (set tried))
             (filter #(and (apply column/stands? ka %) (apply sees? %)))
             (sort-by (fn [[sx sy sz]] (u/dist from {:x (+ sx 0.5) :y sy :z (+ sz 0.5)})))
             first)))))

(def max-column-looks "Looks a call takes at a column goal whose ground it has not made out, one after a fruitless walk." 3)

(defn ^:async look-at-column!
  "A column goal with no ground made out yet (mem :column-open), or under :unknown a y goal whose cell it has not seen,
  after a fruitless walk or within reach of its guess: look at its cell at the body's feet y, then one above (a look
  glances the cell and its neighbours: the ground nearest the feet), then its aimed cell (a y goal: its cell first), at most max-column-looks a
  call; true when it looked (goal-pos aims again from what it saw, the planner proves a seen solid goal cell not standable)."
  [c pos]
  (let [n (:column-looks (ctx/mem c) 0)
        feet (js/Math.floor (:y (u/self-pos c)))
        cell (mapv #(js/Math.floor (% pos)) [:x :y :z])]
    (when (and (or (:column-open (ctx/mem c)) (and (:unknown c) (sense/unknown? (:primitives c) cell)))
               (< n max-column-looks))
      (ctx/update-mem! c assoc :column-looks (inc n))
      (await (sense/look-at! c (assoc cell 1 (nth (vec (distinct (if (:column-open (ctx/mem c)) [feet (inc feet)] [(nth cell 1) feet (inc feet)]))) n (nth cell 1)))))
      true)))

(defn waypoint-due?
  "Whether a column goal is only guessed (mem :column-open :guess) and fewer than ::max-waypoint-misses waypoint walks
  were fruitless."
  [c]
  (and (= :guess (:column-open (ctx/mem c))) (< (:waypoint-misses (ctx/mem c) 0) (max-waypoint-misses))))

(defn guess-waypoint
  "For a column goal whose guess has no stand in reach (waypoint-due?): a seen stand to walk to toward it
  (column/waypoint), else nil."
  [c pos]
  (when (waypoint-due? c)
    (let [{:keys [x y z]} (u/self-pos c)]
      (column/waypoint (kinds/seen-kind-at (:primitives c)) (:x pos) (:z pos) (mapv #(js/Math.floor %) [x y z]) (waypoint-gain)
                       (:waypoints-tried (ctx/mem c) #{})))))

(defn ^:async look-along!
  "Before a waypoint walk (waypoint-due?): the next of ::way-looks not yet taken since the last waypoint walk (mem
  :way-looks), at the cell that far along the line to pos and that far under the feet; true when it looked."
  [c pos]
  (let [n (:way-looks (ctx/mem c) 0)
        {:keys [x y z]} (u/self-pos c)
        h (js/Math.hypot (- (:x pos) x) (- (:z pos) z))
        looks (vec (way-looks))]
    (when (and (waypoint-due? c) (< n (count looks)) (> h 2))
      (let [[ahead down] (nth looks n)
            k (/ (min ahead (- h 1)) h)]
        (ctx/update-mem! c assoc :way-looks (inc n))
        (await (sense/look-at! c (mapv #(js/Math.floor %) [(+ x (* k (- (:x pos) x))) (- y down) (+ z (* k (- (:z pos) z)))])))
        true))))

(defn shared-marker
  "The shared marker called s: the exact name first, else the first whose name matches in any case; nil when none."
  [c s]
  (or (known/marker c s)
      (let [low (str/lower-case s)]
        (first (filter #(= low (some-> (:name %) str/lower-case)) (known/markers c))))))

(defn target
  "{:pos {:x :y :z}} to walk to: the recorded position of :place when given (the body's own place of that name, else the
  shared marker of that name, any 1 to 100 characters, exact then in any case), else :pos; else a refusal."
  [c]
  (let [{:keys [place pos]} (:args c)]
    (if (nil? place)
      (or (column/parse pos) (places/parse-pos pos))
      (let [named (places/parse-name place)
            there (when-not (:reason named) (mem/place (ctx/view c) (:name named)))
            s (cond (keyword? place) (when (nil? (namespace place)) (name place)) (string? place) place)
            bad (when-not (and s (<= 1 (count s) 100))
                  (places/refusal :bad-name (str "a place name is 1 to 100 characters; got " (pr-str place))))
            shared (when-not (or there bad) (shared-marker c s))]
        (cond
          there (places/parse-pos there)
          bad bad
          shared (places/parse-pos [(:x shared) (:y shared) (:z shared)])
          :else (places/refusal :unknown-place (str "no place called " s " is recorded in memory or among the shared markers")))))))

(defn args-refusal
  "The refusal {:reason :message} of the target go-to and its dry run share, nil when it is usable (the arg shapes are
  the spec's: refused at submit). The target's parse is {:pos ...} when there is no refusal."
  [c]
  (let [parsed (target c)]
    (when (:reason parsed) parsed)))

(defn goal-pos
  "The cell go-to plans to this step for goal: aimed when a column, or once the planner found no cell in range of its y
  stands (mem :re-aim), then as the planner stands (wworld/stand-goal). A y chosen for the
  goal that differs from the last one chosen (or firms up an estimate) is an info goal-resolved {:goal :to} (:provisional true for a column's
  estimate; mem :said); the cell aimed at, a
  column's stand-in too, is mem :aimed (the result's :resolved). A column not resolved from seen cells is :column-open
  (:guess for an estimate, :stand-in for no ground known)."
  [c goal]
  (when goal
    (let [column? (nil? (:y goal))
          {:keys [pos resolved provisional why]} (if (or column? (:re-aim (ctx/mem c))) (aimed c goal) {:pos goal})
          open (when column? (cond provisional :guess (not resolved) :stand-in))
          aimed-at (when (or resolved column?) pos)]
      (when (not= open (:column-open (ctx/mem c)))
        (ctx/update-mem! c assoc :column-open open))
      (when (not= aimed-at (:aimed (ctx/mem c)))
        (ctx/update-mem! c assoc :aimed aimed-at))
      (when (not= why (:aimed-why (ctx/mem c)))
        (ctx/update-mem! c assoc :aimed-why why))
      (when (and resolved (not= [pos provisional] (:said (ctx/mem c))))
        (ctx/update-mem! c assoc :said [pos provisional])
        (ctx/emit! c :goal-resolved :info (cond-> {:goal goal :to pos} provisional (assoc :provisional true) why (assoc :why why))))
      (wworld/stand-goal (wworld/path-world (:primitives c) (:unknown c)) pos))))

(defn goal-danger
  "The nearest known mob the body would flee that has the cell pos within its full-cost radius (threats/danger-near-fn),
  with :stay-s and :worth, when the toll there over the stay (:stay-s, else cost.risk/work-s of a click act) costs more
  than giving up loses, what the goal is worth (one-way/worth: :worth, else a tour's); else nil."
  [c pos]
  (when-let [{:keys [toll] :as d} ((threats/danger-near-fn c) pos)]
    (let [{:keys [stay-s worth]} (:args c)
          stay (if stay-s stay-s (risk/work-s :click))
          w (one-way/worth (u/dist (u/self-pos c) pos) worth)]
      (when (> (* toll stay) w) (assoc d :stay-s stay :worth w)))))

(defn pre-aim!
  "Before the first walk: a y goal pos (goal-pos) that the planner answers goal-not-standable for alone (no search,
  wplan/goal-stand-problem), or under :unknown seen in the ground, and whose column aims it elsewhere is re-aimed now
  (mem :re-aim), not after a walk."
  [c goal pos]
  (when (and (:y goal) (not (:re-aim (ctx/mem c))) (pos? (tuning/y-slack))
             (or (not-standable? c pos) (in-seen-ground? c pos))
             (:resolved (aimed c goal)))
    (ctx/update-mem! c assoc :re-aim true)))
