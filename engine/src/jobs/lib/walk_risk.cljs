(ns jobs.lib.walk-risk
  "go-to's walk risk: the risk of the walk to the goal, in hit points, against what the body can afford, priced on the way
  go-to plans (way): its plan's cells, and past a plan that is not found (partial, or none yet) the rest to the goal on the
  straight line. It replaces any wait for dawn: a night walk is judged by the light the body perceives along
  the way (Q334), a walk underground by the hostiles it can reach and the dark halls it crosses (a roofed dark cell prices
  spawns only where dark floor beyond the spawn-free radius is reachable: ready/hall-at?, Q345).
  Risk = the hurt of the mobs expected to spawn on the exposed stretches (jobs.lib.cost/spawn-chance per second of walking,
  jobs.lib.cost/spawns-hurt) plus what the known threats take off the body over
  the way (jobs.lib.cost/route-hurt, each over its field, jobs.lib.threats): every mob sensed now that catches
  the walk (live-catchers), and every death, near-death or hide spot the walk goes for (remembered-near-way), each
  by the chance its mob or a like one is there now (death-spot/present-share: one a sun-burning mob left seen sky-lit by day, or one
  the body went far from, is near free).
  Seen lava within jobs.lib.lava-risk/radius of a cell the way steps into adds jobs.lib.lava-risk/hp, scaled by its closeness (a slip beside it is lethal); a body already at the goal, or a way clear of lava, adds none.
  Acceptable when it stays within jobs.lib.price/::risk-share of the health the body has now, or, when the caller says what the goal is worth (:worth), within the hp that worth buys (price/allowed)."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.lava-risk :as lava-risk]
            [jobs.lib.look :as look]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::surface-band {:default 4 :spec (a/num-in 0 nil)
                   :doc "Blocks below the body's feet that unseen ground still counts as open surface; deeper is underground, where only hostiles count."}
   ::exit-blocks {:default 8 :spec (a/num-in 0 nil)
                  :doc "Blocks of open ground a walk out of a night shelter crosses before it can go under, even to a goal underground."}
   ::heal-s-per-hp {:default 4 :spec (a/num-in 0 nil)
                    :doc "Seconds a body that can heal takes to regain an hp, for the wait that would make a risky walk affordable (stay-estimate)."}
   ::step {:default 4 :spec (a/num-in 1 nil)
           :doc "Blocks between the cells of the way whose light is judged; a way longer than ::max-cells steps is judged at that many cells, spaced wider."}
   ::max-cells {:default 256 :spec (a/int-in 1 nil)
                :doc "Most cells of the way whose light is judged, so a far goal costs a bounded number of reads."}})

(defn- cells-along
  "The cells {:x :y :z} every step blocks along the straight way from `from` to `to`, floored, the goal last."
  [from to step]
  (let [len (u/dist from to)
        n (max 1 (js/Math.ceil (/ len step)))]
    (for [i (range 1 (inc n))
          :let [t (/ i n)]]
      (update-vals (merge-with #(+ %1 (* t (- %2 %1))) from to) js/Math.floor))))

(defn- chance-at
  "The spawn chance per second at `cell` for a body at feet: a seen cell by its light (sky reaching it down its column; a roofed cell is dark only in a dark hall), an unseen one
  at the worse case (Q16): by the open-sky guess when it lies within the surface band below the feet and the body is not
  underground, else as a dark cave (a goal above the body is no sign that the ground at its own level is a cave), none
  under caves-free?."
  [p cell feet-y underground? caves-free?]
  (let [night (not (look/lit-light? 0 15 (sh-sense/sky-subtract-now p)))
        sky (if night 0 15)
        cell (if (sh-sense/solid-at? p cell) (update cell :y inc) cell)
        block (when (or (sh-sense/seen-cell? p cell) (look/glimpse? p cell)) (sh-sense/block-light-at p cell))]
    (cond
      block (cost/spawn-chance [(:x cell) (:y cell) (:z cell)]
                               {:light (constantly {:block block :sky (cond (sh-sense/sky-reaches-from-above? p cell) sky (and (< block (look/dark-light)) (ready/hall-at? p cell)) 0 :else 15)})})
      (or underground? (< (:y cell) (- feet-y (settings/get settings ::surface-band)))) (if caves-free? 0 (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block 0 :sky 0})}))
      :else (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block 0 :sky sky})}))))

(def lava-scan-radius 128)

(defn route-lava-hp
  "The most lava-hp of any cell the way's cells step into (the first, the cell the body stands in, is no step): lava the walk keeps clear of, or a body already at its goal, costs nothing. Reads the seen lava once, within the way's reach (at most lava-scan-radius) of the body."
  [p cells]
  (let [start (first cells)
        steps (rest cells)
        lavas (when (seq steps)
                (mapv (comp (juxt :x :y :z) :pos)
                      (look/seen-blocks p {:names ["lava"] :radius (min lava-scan-radius (+ (reduce max (map #(u/dist start %) steps)) (lava-risk/radius))) :max 256})))]
    (reduce max 0 (for [cell steps] (lava-risk/hp-from lavas [(:x cell) (:y cell) (:z cell)])))))

(defn live-catchers
  "The known threats (threats/known-threats of c) sensed now (neither :fled nor :remembered) that catch the walk over
  timed cells route ([{:x :y :z :t}]): a ranged one, or a melee one cost/caught-on? (one the walk leaves behind is no cost)."
  [known route]
  (filterv (fn [{:keys [mob chaser pos fled remembered]}]
             (and (not (or fled remembered))
                  (or (= :projectile (threat/damage-type mob))
                      (cost/caught-on? {:chaser (or chaser mob) :pos pos :route route}))))
           known))

(defn remembered-near-way
  "The remembered threats of c (known, from threats/known-threats, :remembered, and threats/hide-threats) a walk from feet to goal
  goes for: the walk ends nearer the spot than it starts (a body standing, or walking away, is no closer to it)."
  [c known feet goal]
  (filterv #(< (u/dist goal (:pos %)) (u/dist feet (:pos %)))
           (concat (filter :remembered known) (threats/hide-threats c))))

(defn- floor-cell [pos] {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})

(defn way
  "The cells {:x :y :z} a walk from feet to goal is priced on: from the body's cell along the steps of plan (go-to's,
  {:status :steps}), then, unless the plan is \"found\" (a partial one, or none), the rest to the goal a block apart on
  the straight line, which the planner has not said it avoids: each priced by what the body knows of it (an unseen cell
  as unseen)."
  [feet goal {:keys [status steps]}]
  (let [planned (into [] (dedupe) (cons (floor-cell feet) (map floor-cell steps)))
        end (peek planned)]
    (into planned (when-not (= "found" status) (remove #{end} (cells-along end goal 1))))))

(defn- along
  "The blocks walked from the first of cells to each of them."
  [cells]
  (vec (reductions + 0 (map u/dist cells (rest cells)))))

(defn- dark-seconds
  "Seconds of the walk over cells where a mob could spawn (chance-at): every k-th cell and the last stand for the stretch
  walked since the one before, k the larger of ::step and the cells over ::max-cells."
  [p cells feet-y underground? caves-free?]
  (let [n (count cells)
        k (js/Math.ceil (max (settings/get settings ::step) (/ n (settings/get settings ::max-cells))))
        blocks (along cells)
        ends (distinct (concat (range k n k) [(dec n)]))]
    (/ (reduce + (for [[a b] (map vector (cons 0 ends) ends)
                       :when (pos? (chance-at p (nth cells b) feet-y underground? caves-free?))]
                   (- (blocks b) (blocks a))))
       (cost/walk-blocks-per-s))))

(defn timed-way
  "The cells of the walk of c's body to goal on plan (way), each with :t the seconds from the start the body is there at its pace by its food."
  [c goal plan]
  (let [p (:primitives c)
        cells (way (sh-sense/feet p) (update goal :y #(or % (:y (sh-sense/feet p)))) plan)
        speed (cost/body-speed (or (.-food (.self p)) 20))]
    (mapv #(assoc %1 :t (/ %2 speed)) cells (along cells))))

(defn estimate
  "{:hp :walk-hp :spawn-hp :hostile-hp :death-hp :lethal-hp :lava-hp :dark-s :hostiles :threats} of the walk of c's body to `goal` ({:x :y :z})
  on the way of :plan (go-to's plan of it, {:status :steps}; none: all of it unplanned): hp the expected damage and lethal-hp,
  the death the spawns and known threats risk (cost/spawns-chance, cost/threats-chance) in hp (cost/lethal-hp), a death losing
  :stake seconds of worth besides (go-to's :stake, as death-cost prices it, lain :stake-age-ms); walk-hp the
  same leaving out the sensed mobs that catch the walk, their hurt and their death share both (a fight with them prices
  them); dark-s the seconds walked where a mob could spawn (caves-free?: no unseen cell guessed a dark cave); threats [{:mob :pos}] the known threats priced."
  [c goal & [{:keys [plan stake stake-age-ms caves-free?]}]]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        goal (update goal :y #(or % (:y feet)))
        underground? (sh-sense/buried? p)
        leaves? (sh-inside/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) goal 1)
        cells (way feet goal plan)
        exit (if leaves? (settings/get settings ::exit-blocks) 0)
        exposed-s (+ (/ exit (cost/walk-blocks-per-s)) (dark-seconds p cells (:y feet) underground? caves-free?))
        spawns (* exposed-s (cost/spawn-chance [0 0 0]))
        body (assoc (threats/death-body c) :stake stake :stake-age-ms (when stake-age-ms (+ stake-age-ms (* 1000 (/ (u/dist feet goal) (cost/walk-blocks-per-s))))))
        threat-hp (fn [ts] (if (seq ts) (cost/route-hurt body {:cells cells :threats (threats/with-reach c ts)}) 0))
        known (threats/known-threats c)
        catchers (live-catchers known (timed-way c goal plan))
        remembered (remembered-near-way c known feet goal)
        death-hp (threat-hp remembered)
        spawn-dies (cost/spawns-chance body spawns feet)
        dies (fn [met] (- 1 (* (- 1 (if (seq met) (cost/threats-chance body {:cells cells :threats (threats/with-reach c met)}) 0))
                               (- 1 spawn-dies))))
        lethal (cost/lethal-hp body (dies (concat catchers remembered)) goal)
        lava (route-lava-hp p cells)
        spawn-hp (cost/spawns-hurt body spawns feet)
        hostile-hp (threat-hp catchers)
        unfought (if (seq catchers) (cost/lethal-hp body (dies remembered) goal) lethal)]
    {:hp (+ spawn-hp hostile-hp death-hp lethal lava)
     :walk-hp (+ spawn-hp death-hp unfought lava) :lava-hp lava :spawn-hp spawn-hp :hostile-hp hostile-hp :death-hp death-hp :lethal-hp lethal
     :dark-s exposed-s :hostiles (count catchers) :threats (mapv #(select-keys % [:mob :pos]) (concat catchers remembered))}))

(def recheck-ms "How long a walk held :risky waits before status prices it again." 10000)

(defn note-plan!
  "Keep plan ({:status :steps}), the one go-to's round held its walk to goal on, for status to price while the body
  stands where it planned from and knows the world as it did (sense/known-writes)."
  [c goal plan]
  (let [p (:primitives c)]
    (ctx/update-mem! c assoc ::plan {:from (floor-cell (sh-sense/feet p)) :goal goal :writes (sense/known-writes p)
                                     :status (:status plan) :steps (mapv floor-cell (:steps plan))})))

(defn planned
  "The plan note-plan! kept, with its :goal, nil once the body moved from where it planned from or a block it knows
  changed (a plan made now might differ)."
  [c]
  (let [p (:primitives c)
        {:keys [from writes] :as kept} (::plan (ctx/mem c))]
    (when (and kept (= from (floor-cell (sh-sense/feet p))) (= writes (sense/known-writes p)))
      kept)))

(defn stay-estimate
  "hp the body expects to lose staying at its cell instead of walking `route-hp` to `goal`: what the known threats
  sensed now whose field holds the body's cell take off it there (cost/route-hurt, each at full weight over the stay: a
  sensed mob within its field gets to a body that stays; one near the far end of the way does not reach it) plus the spawns met at
  its cell by the light seen there (price/stay-parts), and the death both risk in hp (cost/threats-chance,
  cost/lethal-hp), for the time the walk takes, or, for a body that can heal, for the wait until its health would afford
  the route (::heal-s-per-hp an hp)."
  [c goal route-hp]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        health (.-health (.self p))
        heal-s (if (price/cannot-heal? c) 0 (* (settings/get settings ::heal-s-per-hp) (max 0 (- (min 20 (/ route-hp (settings/get price/settings ::price/risk-share))) health))))
        secs (max heal-s (/ (u/dist feet goal) (cost/walk-blocks-per-s)))
        here [(assoc feet :s secs)]
        live (mapv (fn [{:keys [reach] :as t}] (assoc t :reach (fn [x y z] (when (reach x y z) 1))))
                   (threats/with-reach c (remove #(or (:fled %) (:remembered %)) (threats/known-threats c))))
        chance (chance-at p (update-vals feet js/Math.floor) (:y feet) false false)
        body (threats/death-body c)]
    {:hp (let [{:keys [hurt dies]} (price/stay-parts c chance secs)
               met (if (seq live) (cost/threats-chance body {:cells here :threats live}) 0)]
           (+ (cost/route-hurt body {:cells here :threats live}) hurt
              (cost/lethal-hp body (- 1 (* (- 1 met) (- 1 dies))) feet)))
     :hostiles (count (filter #((:reach %) (:x feet) (:y feet) (:z feet)) live))}))

(defn status
  "Ok while the estimated damage of the walk to `goal` on plan (go-to's, else the one its round held on, planned) is within
  (price/allowed c) or within what staying put would cost (a risk staying does not avoid is no reason to wait). Over both:
  later :risky with the risk and its parts, or never :stranded when the body cannot heal, so waiting only repeats the same
  exposure and the caller must seal in or go to a nearer refuge. Ok :unplanned with no plan: go-to's round prices its plan
  before it walks a step of it (jobs.movement.go-to.reprice)."
  ([c goal] (let [kept (planned c)] (status c (or (:goal kept) goal) kept)))
  ([c goal plan]
   (if-not plan
     (contract/ok :unplanned "the walk is priced on go-to's plan before a step of it")
     (let [{:keys [hp spawn-hp hostile-hp death-hp lethal-hp lava-hp dark-s hostiles]} (check-cache/cached c [::risk goal (floor-cell (sh-sense/feet (:primitives c))) (hash (select-keys plan [:status :steps])) (select-keys (:args c) [:stake :stake-age-ms]) (sense/round-looked c)] #(estimate c goal (assoc (select-keys (:args c) [:stake :stake-age-ms]) :plan plan)))
           allowed (price/allowed c)]
       (if (<= hp allowed)
         (contract/ok :acceptable (str "route risk " (price/round1 hp) " hp, within " (price/round1 allowed)))
         (let [stay (check-cache/cached c [::stay goal (floor-cell (sh-sense/feet (:primitives c))) hp (.-health (.self (:primitives c))) (.-food (.self (:primitives c))) (sense/round-looked c)] #(stay-estimate c goal hp))
               over (str "route risk " (price/round1 hp) " hp over the " (price/round1 allowed) " the body can afford: "
                         (price/round1 dark-s) " s on dark open ground costs " (price/round1 spawn-hp) " hp, "
                         hostiles " hostile(s) by the way that catch it " (price/round1 hostile-hp) " hp, " (price/round1 death-hp) " hp for recent deaths or hides near it, " (price/round1 lethal-hp) " hp for the death they risk, " (price/round1 lava-hp) " hp for seen lava near the way")]
           (cond
             (<= hp (:hp stay)) (contract/ok :cheaper-than-staying (str "route risk " (price/round1 hp) " hp, over the " (price/round1 allowed) " affordable but staying here costs " (price/round1 (:hp stay)) " hp (" (:hostiles stay) " hostile(s) near the body)"))
             (and (pos? (:hp stay)) (price/cannot-heal? c)) (contract/never :stranded (str over "; staying here costs " (price/round1 (:hp stay)) " hp and the body cannot heal, so waiting does not help: seal in or go to a nearer refuge") :risk hp :allowed allowed :stay-hp (:hp stay))
             :else (contract/later :risky over :risk hp :allowed allowed :recheck-after-ms recheck-ms))))))))
