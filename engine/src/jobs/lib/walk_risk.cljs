(ns jobs.lib.walk-risk
  "go-to's walk risk: the risk of the walk to the goal, in hit points, against what the body can afford, priced on the way
  go-to plans (way): its plan's cells, and past a plan that is not found (partial, or none yet) the rest to the goal on the
  straight line. It replaces any wait for dawn: a night walk is judged by the light the body perceives along
  the way (Q334), a walk underground by the hostiles it can reach and the dark halls it crosses (a roofed dark cell prices
  spawns only where dark floor beyond the spawn-free radius is reachable: spawn-light/hall-at?, Q345).
  Risk = the hurt of the mobs expected to spawn on the exposed stretches (jobs.lib.cost/spawn-chance per second of walking,
  jobs.lib.cost/spawns-hurt) plus what the known threats take off the body over
  the way (jobs.lib.cost/route-parts, each over its field, jobs.lib.threats, in meetings): every mob sensed now that catches
  the walk (live-catchers), and every death, near-death or hide spot the walk goes for (remembered-near-way), each
  by the chance its mob or a like one is there now (death-spot/present-share: one a sun-burning mob left seen sky-lit by day, or one
  the body went far from, is near free).
  Seen lava within jobs.lib.lava-risk/radius of a cell the way steps into adds jobs.lib.lava-risk/hp, scaled by its closeness (a slip beside it is lethal); a body already at the goal, or a way clear of lava, adds none.
  Acceptable when it stays within jobs.lib.cost.risk/::risk-share of the health the body has now, or, when the caller says what the goal is worth (:worth), within the hp that worth buys (price/allowed)."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.perception.rays :as rays]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.lava-risk :as lava-risk]
            [jobs.lib.look :as look]
            [jobs.lib.price :as price]
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.sense :as sense]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threat-field :as field]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::exit-blocks {:default 8 :spec (a/num-in 0 nil)
                  :doc "Blocks of open ground a walk out of a night shelter crosses before it can go under, even to a goal underground, priced by the light of the way out (exit-dark)."}
   ::heal-s-per-hp {:default 4 :spec (a/num-in 0 nil)
                    :doc "Seconds a body that can heal takes to regain an hp, for the wait that would make a risky walk affordable (stay-estimate)."}})

(def lava-scan-radius 128)

(defn- cells-along
  "The cells {:x :y :z} along the straight way from `from` to `to`, floored, the goal last: a block apart for the first
  `near` blocks, then at most spawn-light's ::max-cells more, spaced evenly (a goal a million blocks off is no million cells)."
  [from to near]
  (let [len (u/dist from to)
        a (js/Math.floor (min len near))
        m (max 1 (js/Math.ceil (min (- len a) (settings/get spawn-light/settings ::spawn-light/max-cells))))]
    (for [d (concat (range 1 a) (map #(+ a (* (- len a) (/ % m))) (range 1 (inc m))))
          :let [t (if (zero? len) 1 (/ d len))]]
      (update-vals (merge-with #(+ %1 (* t (- %2 %1))) from to) js/Math.floor))))

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
  "The remembered threats of known (threats/known-threats, :remembered) and hides (threats/hide-threats) a walk from feet to goal
  goes for: the walk ends nearer the spot than it starts (a body standing, or walking away, is no closer to it)."
  [known hides feet goal]
  (filterv #(< (u/dist goal (:pos %)) (u/dist feet (:pos %)))
           (concat (filter :remembered known) hides)))

(defn- floor-cell [pos] {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})

(defn way
  "The cells {:x :y :z} a walk from feet to goal is priced on: from the body's cell along the steps of plan (go-to's,
  {:status :steps}), then, unless the plan is \"found\" (a partial one, or none), the rest to the goal on the straight
  line, which the planner has not said it avoids (cells-along: a block apart while seen lava could be near): each priced
  by what the body knows of it (an unseen cell as unseen)."
  [feet goal {:keys [status steps]}]
  (let [start (floor-cell feet)
        planned (into [] (dedupe) (cons start (map floor-cell steps)))
        end (peek planned)
        near (+ lava-scan-radius (lava-risk/radius) (u/dist start end))]
    (into planned (when-not (= "found" status) (remove #{end} (cells-along end goal near))))))

(defn- along
  "The blocks walked from the first of cells to each of them."
  [cells]
  (vec (reductions + 0 (map u/dist cells (rest cells)))))

(defn timed-way
  "The cells of the walk of c's body to goal on plan (way), each with :t the seconds from the start the body is there at cost/walk-speed."
  [c goal plan]
  (let [p (:primitives c)
        cells (way (sh-sense/feet p) (update goal :y #(or % (:y (sh-sense/feet p)))) plan)
        speed cost/walk-speed]
    (mapv #(assoc %1 :t (/ %2 speed)) cells (along cells))))

(defn- exit-dark
  "{:share :dawn?} of the way out of the room round the feet for a walk over cells: the mean spawn-light/dark-share of
  the walk's first ::exit-blocks cells past the room not seen solid, else of the room's sh-inside/way-out-cells (1 with
  neither), under the sky's darkening sky (nil: now), and whether dawn lowers that of one now (spawn-light/dawn-lowers?)."
  [p cells caves-free? sky]
  (let [feet (sh-sense/feet p)
        room (or (reach-room/room p feet) {:cells #{[(:x feet) (:y feet) (:z feet)]}})
        inside (set (:cells room))
        way (or (seq (take (js/Math.ceil (settings/get settings ::exit-blocks))
                           (remove #(or (inside [(:x %) (:y %) (:z %)]) (sh-sense/solid-at? p %)) cells)))
                (seq (sh-inside/way-out-cells p room)))]
    (if way
      {:share (/ (transduce (map #(spawn-light/dark-share p % caves-free? sky)) + 0 way) (count way))
       :dawn? (boolean (some #(spawn-light/dawn-lowers? p %) way))}
      {:share 1 :dawn? false})))

(defn estimate-inputs
  "What estimate reads of c's body and world for the walk to `goal` (estimate's opts), as values: the body's feet,
  its shelter, death-body and food, the known threats and hides, the memory epoch of what it knows, the sky's
  darkening the light is judged by (:sky, default now; 0 prices the walk by full day's light), the cells its look round has looked at, and what the way is priced by as perceived now: its cells, the
  seconds of it spent where a mob could spawn (:exit-s those of leaving a shelter at night by the way out's light, :exit-dawn? whether dawn lowers them) and the hp of the lava seen near it, and the reach of the threats it prices
  (:weights, their fields' cells, which change as cells they read are seen anew). :cells, :catchers and :remembered are derived, left out of a key."
  [c goal & [{:keys [plan caves-free? back? stay-s sky]}]]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        goal (update goal :y #(or % (:y feet)))
        shelter (:data (ctx/latest c :shelter))
        there (way feet goal plan)
        stay-s (or stay-s 0)
        stay-at (dec (count there))
        there (cond-> there (pos? stay-s) (update stay-at assoc :s (+ stay-s (peek (death/cell-seconds there)))))
        cells (cond-> there back? (#(into % (rest (rseq %)))))
        exit (if (sh-inside/walk-leaves-shelter? p shelter goal 1) (exit-dark p there caves-free? sky) {:share 0 :dawn? false})
        exit-s (* (/ (settings/get settings ::exit-blocks) cost/walk-speed) (:share exit))
        food (or (.-food (.self p)) 20)
        known (threats/known-threats c)
        hides (threats/hide-threats c)
        speed cost/walk-speed
        later (fn [i] (if (and (pos? stay-s) (> i stay-at)) stay-s 0))
        catchers (live-catchers known (mapv (fn [i cell d] (assoc cell :t (+ (/ d speed) (later i)))) (range) cells (along cells)))
        remembered (remembered-near-way known hides feet goal)]
    {:goal goal :plan (select-keys plan [:status :steps]) :back? back? :stay-at (when (pos? stay-s) stay-at) :stay-s stay-s
     :feet feet :cells cells :body (threats/death-body c) :food food
     :known known :hides hides :catchers catchers :remembered remembered
     :weights (mapv #(:weights (field/field-of c %)) (concat catchers remembered))
     :memory (sense/known-memory p) :sky (or sky (sh-sense/sky-subtract-now p)) :looked (sense/round-looked c)
     :exit-s exit-s :exit-dawn? (and (pos? exit-s) (:dawn? exit))
     :exposed-s (+ exit-s
                   (spawn-light/dark-seconds p cells (mapv #(/ % cost/walk-speed) (along cells)) caves-free? sky)
                   (* stay-s (spawn-light/dark-share p (nth cells stay-at) caves-free? sky)))
     :lava (route-lava-hp p cells)}))

(defn estimate-of
  "estimate of its inputs (estimate-inputs)."
  [c {:keys [goal feet cells body catchers remembered exposed-s lava]}]
  (let [spawns (* exposed-s (cost/spawn-chance [0 0 0]))
        parts (fn [ts] (if (seq ts) (cost/route-parts body {:cells cells :threats ts :at feet}) {:chance 0 :hurt 0}))
        catchers (threats/with-reach c catchers)
        remembered (threats/with-reach c remembered)
        met-remembered (parts remembered)
        met-all (if (seq catchers) (parts (concat catchers remembered)) met-remembered)
        death-hp (:hurt met-remembered)
        spawn-dies (cost/death-chance body {:spawns spawns :at feet})
        dies (fn [{:keys [chance]}] (- 1 (* (- 1 chance) (- 1 spawn-dies))))
        lethal (cost/lethal-hp body (dies met-all) goal)
        spawn-hp (cost/spawns-hurt body spawns feet)
        hostile-hp (max 0 (- (:hurt met-all) death-hp))
        unfought (if (seq catchers) (cost/lethal-hp body (dies met-remembered) goal) lethal)]
    {:hp (+ spawn-hp hostile-hp death-hp lethal lava)
     :walk-hp (+ spawn-hp death-hp unfought lava) :lava-hp lava :spawn-hp spawn-hp :hostile-hp hostile-hp :death-hp death-hp :lethal-hp lethal
     :death-chance (dies met-all) :dark-s exposed-s :hostiles (count catchers) :threats (mapv #(select-keys % [:mob :pos]) (concat catchers remembered))}))

(defn estimate
  "{:hp :walk-hp :spawn-hp :hostile-hp :death-hp :lethal-hp :death-chance :lava-hp :dark-s :hostiles :threats} of the walk of c's body to `goal` ({:x :y :z})
  on the way of :plan (go-to's plan of it, {:status :steps}; none: all of it unplanned), and with :back? back on the same way
  as one walk (a mob that noticed the body going still has it coming back), and with :stay-s standing that many seconds at the
  goal first (its cell's seconds in the route, its spawns by the light there), and with :sky under that darkening of the sky (estimate-inputs): hp the expected damage and lethal-hp,
  the death the spawns and known threats risk (cost/death-chance) in hp (cost/lethal-hp), walk-hp the
  same leaving out the sensed mobs that catch the walk, their hurt and their death share both (a fight with them prices
  them); death-hp the remembered threats' hurt, hostile-hp what the catchers add to it, all met in one route-parts; dark-s the seconds walked where a mob could spawn (caves-free?: no unseen cell guessed dark); threats [{:mob :pos}] the known threats priced."
  [c goal & [opts]]
  (estimate-of c (estimate-inputs c goal opts)))

(def recheck-ms "How long a walk held :risky waits before status prices it again." 10000)

(defn- raw-epoch [p] (some-> (look/raw-of p) rays/epoch-of))

(defn note-plan!
  "Keep plan ({:status :steps}), the one go-to's round held its walk to goal on, for status to price while the body
  stands where it planned from and the world is as it was (sense/known-memory, the raw world's epoch)."
  [c goal plan]
  (let [p (:primitives c)]
    (ctx/update-mem! c assoc ::plan {:from (floor-cell (sh-sense/feet p)) :goal goal :memory (sense/known-memory p) :epoch (raw-epoch p)
                                     :status (:status plan) :steps (mapv floor-cell (:steps plan))})))

(defn planned
  "The plan note-plan! kept, with its :goal, nil once the body moved from where it planned from or a block it knows
  changed or the raw world did (a plan made now might differ)."
  [c]
  (let [p (:primitives c)
        {:keys [from memory epoch] :as kept} (::plan (ctx/mem c))]
    (when (and kept (= from (floor-cell (sh-sense/feet p))) (= memory (sense/known-memory p)) (= epoch (raw-epoch p)))
      kept)))

(defn stay-inputs
  "What stay-estimate reads of c's body and world for a stay instead of walking `route-hp` to `goal`, as values: its
  feet, health, whether it can heal, its death-body, the known threats sensed now, the spawn chance at its cell and the
  memory epoch of what it knows and the reach of the threats (:weights, their fields' cells)."
  [c goal route-hp]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        live (vec (remove #(or (:fled %) (:remembered %)) (threats/known-threats c)))]
    {:goal goal :route-hp route-hp :feet feet :health (.-health (.self p)) :cannot-heal? (price/cannot-heal? c)
     :body (threats/death-body c) :memory (sense/known-memory p)
     :live live :weights (mapv #(:weights (field/field-of c %)) live)
     :chance (spawn-light/chance-at p (update-vals feet js/Math.floor) false)}))

(defn stay-estimate
  "hp the body expects to lose staying at its cell instead of walking `route-hp` to `goal` (stay-inputs): what the known threats
  sensed now whose field holds the body's cell take off it there (cost/route-parts, each at full weight over the stay: a
  sensed mob within its field gets to a body that stays; one near the far end of the way does not reach it) plus the spawns met at
  its cell by the light seen there (price/stay-parts), and the death both risk in hp (cost/death-chance,
  cost/lethal-hp), for the time the walk takes, or, for a body that can heal, for the wait until its health would afford
  the route (::heal-s-per-hp an hp); for :secs when given. :lethal-hp the death's part of :hp; :heal-s and :secs the wait."
  [c {:keys [goal route-hp feet health cannot-heal? body live chance secs]}]
  (let [heal-s (if cannot-heal? 0 (* (settings/get settings ::heal-s-per-hp) (max 0 (- (min 20 (/ route-hp (settings/get risk/settings ::risk/risk-share))) health))))
        secs (or secs (max heal-s (/ (u/dist feet goal) (cost/walk-blocks-per-s))))
        here [(assoc feet :s secs)]
        live (mapv (fn [{:keys [reach] :as t}] (assoc t :reach (fn [x y z] (when (reach x y z) 1)))) (threats/with-reach c live))
        hurt (+ (if (seq live) (:hurt (cost/route-parts body {:cells here :threats live :at feet})) 0) (:hurt (price/stay-parts c chance secs)))
        lethal (cost/lethal-hp body (cost/death-chance body {:cells here :threats live :spawns (* chance secs) :at feet}) feet)]
    {:hp (+ hurt lethal) :lethal-hp lethal :heal-s heal-s :secs secs
     :hostiles (count (filter #((:reach %) (:x feet) (:y feet) (:z feet)) live))}))

(defn- wait-lowers?
  "Whether waiting may lower the price of the walk over cells (estimate-inputs' :cells) whose estimate is est: the body
  heals toward affording it (heal-s), dawn lights a cell of it or the way out of the shelter it leaves (exit-dawn?), or a
  threat it prices moves or fades."
  [c cells exit-dawn? {:keys [threats]} heal-s]
  (let [p (:primitives c)]
    (or (pos? heal-s) (seq threats) exit-dawn?
        (boolean (some #(spawn-light/dawn-lowers? p %) cells)))))

(defn status
  "Ok while the estimated damage of the walk to `goal` on plan (go-to's, else the one its round held on, planned) is within
  (price/allowed c) or within what staying put would cost (a risk staying does not avoid is no reason to wait). Over both:
  later :risky with the risk and its parts, or never :stranded when the body cannot heal, so waiting only repeats the same
  exposure and the caller must seal in or go to a nearer refuge. A wait that lowers nothing the walk is priced by
  (wait-lowers?) never ends: ok :cheaper-than-staying when staying costs anything, else ok :no-wait-helps within the hp
  allowed when waiting never lowers the price (price/allowed's waits? false), never :risky over it. Ok :unplanned with
  no plan: go-to's round prices its plan before it walks a step of it (jobs.movement.go-to.reprice)."
  ([c goal] (let [kept (planned c)] (status c (or (:goal kept) goal) kept)))
  ([c goal plan]
   (if-not plan
     (contract/ok :unplanned "the walk is priced on go-to's plan before a step of it")
     (let [in (estimate-inputs c goal {:plan plan})
           {:keys [hp spawn-hp hostile-hp death-hp lethal-hp lava-hp dark-s hostiles] :as est} (check-cache/keyed c ::risk (dissoc in :cells :catchers :remembered) #(estimate-of c in))
           allowed (price/allowed c)]
       (if (<= hp allowed)
         (contract/ok :acceptable (str "route risk " (price/round1 hp) " hp, within " (price/round1 allowed)))
         (let [stay (let [in (stay-inputs c goal hp)] (check-cache/keyed c ::stay in #(stay-estimate c in)))
               over (str "route risk " (price/round1 hp) " hp over the " (price/round1 allowed) " the body can afford: "
                         (price/round1 dark-s) " s on dark open ground costs " (price/round1 spawn-hp) " hp, "
                         hostiles " hostile(s) by the way that catch it " (price/round1 hostile-hp) " hp, " (price/round1 death-hp) " hp for recent deaths or hides near it, " (price/round1 lethal-hp) " hp for the death they risk, " (price/round1 lava-hp) " hp for seen lava near the way")]
           (cond
             (<= hp (:hp stay)) (contract/ok :cheaper-than-staying (str "route risk " (price/round1 hp) " hp, over the " (price/round1 allowed) " affordable but staying here costs " (price/round1 (:hp stay)) " hp (" (:hostiles stay) " hostile(s) near the body)"))
             (and (pos? (:hp stay)) (price/cannot-heal? c)) (contract/never :stranded (str over "; staying here costs " (price/round1 (:hp stay)) " hp and the body cannot heal, so waiting does not help: seal in or go to a nearer refuge") :risk hp :allowed allowed :stay-hp (:hp stay))
             (wait-lowers? c (:cells in) (:exit-dawn? in) est (:heal-s stay)) (contract/later :risky over :risk hp :allowed allowed :recheck-after-ms recheck-ms)
             (pos? (:hp stay)) (contract/ok :cheaper-than-staying (str "route risk " (price/round1 hp) " hp, over the " (price/round1 allowed) " affordable, but waiting here lowers none of it and costs " (price/round1 (:hp stay)) " hp every " (price/round1 (:secs stay)) " s"))
             (<= hp (price/allowed c (:worth (:args c)) false)) (contract/ok :no-wait-helps (str "route risk " (price/round1 hp) " hp, over the " (price/round1 allowed) " affordable, but waiting here lowers none of it and the body can take it"))
             :else (contract/never :risky (str over "; waiting here lowers none of it") :risk hp :allowed allowed))))))))
