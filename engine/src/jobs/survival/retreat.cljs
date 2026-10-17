(ns jobs.survival.retreat
  (:require [clojure.set :as set]
            [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.survival.retreat-doors :as retreat-doors]
            [jobs.lib.goal :as goal]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.survival.retreat-cover :as cover]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-goal :as retreat-goal]
            [jobs.survival.retreat-home :as retreat-home]
            [jobs.survival.retreat-pillar :as pillar]
            [jobs.survival.retreat-refuge :as refuge]
            [jobs.survival.retreat-walk :as walk]))

(def doc
  "Flee from the hostiles in range, one whole flight per round: run until no mob chases the body any more.
  A real danger (jobs.lib.danger: a mob with a walkable way to the body, or a ranged one with a line of fire) within
  :radius (ranged ones :ranged-radius) starts a chase, or is seen afresh. A mob stops chasing (vanilla) once it is
  gone (dead, despawned, untracked), beyond its follow range (jobs.lib.threats: zombie 35, most 16), out of line of
  sight for retreat-flight/lost-s (4 s), or has no walkable way to the body (nor, ranged, a line of fire; a creeper within
  retreat-flight/::blast-reach (6) always has one: its blast).
  Each step, in this order:
  1. A door, gate or trapdoor standing open within a hand's reach and nearer the nearest chaser than the body is shut
     with one click (retreat.door-shut info). Each door is clicked at most once a flight.
  2. Runs into a closed building first (not while a creeper chases: its blast would take the door and what is inside;
     with a creeper within blast reach no room is looked for, and the one the body stands in is no refuge: it steps away
     or takes the cornered options): the latest :bed or :home within retreat-cover/home-range (64), or a room behind a wooden door or
     gate the body has seen (jobs.survival.retreat-home/rooms, bounded by its settings), that is a closed room (walls,
     doors that shut: jobs.lib.reach/room) whose door it walks to (over seen blocks, at its own speed) ahead of every
     melee chaser (jobs.lib.cost/reach-first?), the fewest steps of those, with a go-to child to the room cell inside
     that door (retreat.home info, once a flight); a step that gets no nearer drops that room for the flight. Not while
     low in a ranged chaser's line of fire (the cover step below comes first). Any cell of the room counts as home:
     there an open door of the room is shut (a step back to one out of reach first), and a chaser still with a way in
     gets the cornered options (4.), its plug the walls alone (no pit or pocket dug in the hut): it does not walk out.
  Underground (shelter/buried?) it first takes the stairs the body cut (ledger/ways-of: the highest stair cell within 14
  blocks, above it and no nearer a chaser), before any walk to the :goal, and with none digs a side pocket (as below) before it walks across the open cave,
  unless a creeper or a melee mob is within cover-gap.
  2b. A lone melee mob (no creeper, enderman, spider, flyer or shooter) between ::cover-gap (5) and ::refuge-gap (10) that a fight would not win, with
     pillar-height blocks carried: pillar up, else seal in, before any flee step (a walk gains little on it); when neither can be
     done the flight walks. Each flee step emits debug retreat.step-speed {:blocks :seconds :bps}.
  3. Walks a short step (:step blocks) away from all chasers (nearer ones weigh more) with a go-to child (:ways #{:walk :clear-seal}),
     leaning toward the first closed room of 2. (else a home it is nearer to than every hostile it knows, none on the way; else the :goal, the destination of the walk the flight
     interrupted, while it is safe: jobs.survival.retreat-goal/verdict, judged every step in three dimensions against every hostile it knows and the places of those it fled lately; else the latest :bed or :home within retreat-cover/home-range) when not through the
     hostiles (a closed room behind them: turned retreat-walk ::flank-turn degrees toward it, to go round them, while it still gains on them so:
     jobs.lib.cost/outruns-at?), avoiding :hazard positions.
     When a wall blocks the way away it turns up to 120 degrees towards open ground (at least 2 clear cells).
     It leaves out the ends within retreat-flight/::died-radius of a recent :died place, then dark ones at night or under ground
     (retreat-flight/::prefer-lit?), each only while another end is open.
     It prefers, in this order: a step whose end is out of every ranged chaser's line of fire (cover: leaves stop arrows),
     the full step that takes least time (a swim block costs :swim-factor), the most open.
     It first looks away and to either side where it has not seen the ground (at most 3 looks); a cell still unseen
     reads as rock.
     Eats one bite a step (up to food 20) when the nearest chaser is at least eat-gap (12) blocks away and food is carried.
  Low on health (at most :hide-health) in a seen ranged chaser's line of fire (over the cells it has seen) with no
  covered step, it takes cover where it stands first: plug in (as below);
  not possible, it walks on. A plug begun where it stands is finished before it walks on (refuge/plugging-here?).
  Not while a creeper chases it or a melee mob is within cover-gap: it walks (or backs off, cornered) first.
  A step of at most 1.5 s there that left the body where it stood (a search) is followed by that cover at once.
  A mob that attacks at close range within :radius and outruns the body (jobs.lib.cost/outruns?: a spider at any food;
  not a creeper or an enderman): no flee step. After the door and room steps (1., 2., the race at each chaser's own
  speed) it takes the cornered options (4.) without the back-off.
  4. Cornered (no open direction worth a walk, or the walk is blocked, or a melee hostile within 3 hit the body since the last step, or melee mobs within close-radius hit it retreat-cornered/::beaten-hits (2) times within its ::hurt-window-s: it cannot be outrun, and the fight is weighed over those hostiles alone; checked afresh every step) with no hostile within
     :radius: holds a second (wait, why cornered) and looks again; after :no-gain-steps such steps or holds with no gain the flight ends :keeps-off. With one within :radius: takes the safest option it has not yet failed.
     - an angry enderman near (one the client flags angry, that hurt the body this flight or is within 3 blocks; it teleports; a walk is no flight): at once, before any walk: water within 6 blocks (it keeps away from water; the body steps in and holds), else a seen 2-high gap under a roof within 8 blocks (a 3-tall enderman cannot enter; walks there and holds), else plug in, last of all fight.
     - a drowned near while the body is in water (and no fight wins): swims to the nearest seen dry stand within 16 blocks first (past the mob if need be), before seal, pillar or a fist fight.
     - fight (jobs.survival.fight-back) only when jobs.lib.cost/fight-damage leaves :reserve health. Never against a creeper.
     - plug in (jobs.survival.plug-in, a child: the walls, and later a pit or side pocket, never at home; the early refuge the walls alone): shuts the body
       in with carried blocks (or, with none to wall with, ones it digs), the sides towards a seen ranged hostile first. Every
       plug block goes to the scaffold ledger (purpose :retreat-plug), taken back by jobs.access.cleanup once no hostile is near.
     - pillar up 3 (jobs.access.pillar, the carried block with the most, at least 3). Needs a solid floor, free cells above and no slope or ledge a mob could climb to beside its top.
       Not against a ranged mob or a spider (it climbs the pillar). Every block goes to the scaffold ledger (purpose :pillar) for jobs.access.cleanup.
     - back off: a step of up to 2 cells in any of 8 directions that gains at least a block on the hostile.
     - last of all, fight with the best weapon or tool (pickaxe, shovel, hoe) or the fist. A body with no weapon
       (the call's :use :tools, else jobs.lib.combat/default-weapons) first looks for a way to walk once more (:fist-sweeps, default 1).
       A pillared body that is hit raises the pillar `retreat-pillar/::pillar-extend` (2) blocks, once, before the refuge fails.
     Order: fight if it wins, then plug in (walls), pillar (not against a ranged mob or a spider), back off (not from a mob that outruns the body), plug in by a pit or pocket (not at home), fight.
     Against a creeper back off comes first.
     Hit ::beaten-hits times in ::hurt-window-s by melee mobs within close-radius, the fight comes first, armed or not.
     Under fire (::fire-hits hits within ::hurt-window-s whose attacker is a ranged mob, or arrow damage: the mob need not be seen or known) cover comes before any fight: the order is
     plug in (walls), back off, plug in (pit or pocket), fight, though a melee mob is at the body; the fight is weighed over every hostile near, and never counts as won
     while no ranged hostile near has been seen. With none seen the last-resort fight swings only at a mob within 3 blocks, none walked to. In another cell than the
     last time it was under fire, a plug that failed is tried again.
     An option that fails is not tried again until all have failed. Then, after a second's hold, all are tried again
     (one retreat_blocked warning per flight).
  Plugged in or up a pillar it hides (one retreat_sealed warning; a declared hold, wait why hiding) while a
  hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius) would have a walkable way to
  the refuge if its own blocks were gone, and for :quiet-s more after the last such danger (a silent mob is forgotten
  after a few seconds); then the flight ends :hidden. After :hide-s in the refuge with no real danger on the body as it
  stands (jobs.lib.danger/danger-near?, the hostile-near trigger's test) it ends :hidden even while that hostile stays;
  the refuge stays, its plugs too while a hostile is near (jobs.access.cleanup).
  A chase is bounded by the call's time spend (default ::job-time-s): past it the flight ends limit-reached :job-time where it
  stands (refuge included) and the hostile trigger fires again while the danger stays. With no new best gap to the
  nearest chaser in :no-gain-steps steps it takes the cornered options instead of walking on; it ends nothing-possible
  :cannot_escape only once every option failed in two sweeps in a row with no gain between.
  Each change of what it chose among is said in one retreat.choice info: the hostiles and where, every closed room and why
  it is not run to, the cells that are no closed room, the home and why it is no run (:home-why), the goal and why it is dropped, where it leans.
  Blocks placed are the call's :use :blocks, else every :build filler, carried or dug.
  Needs nothing to start (no conditions); with no real danger near and no flight under way it is done at once. Ends done {:fled [ids] :ended :gone|:far|:lost|:closed|:hidden|:keeps-off|:none},
  nothing-possible :cannot_escape {:fled [ids]}, or limit-reached. Quote: time (0, ::mean-s, ##Inf).
  Memory: one :threat entry per mob fled (jobs.lib.threats); the third from one mob within 5 min warns hostile.chased.
  A cell in another's zone is used only as a last resort (retreat.trespass-last-resort warning).")

(a/defargs args
  {:radius {:doc "hostiles within this many blocks start a flight" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks start a flight" :spec (a/num-in 0 nil) :default 16}
   :goal {:doc "a position {:x :y :z} the flight walks to with go-to legs (planner danger costs keep it round the hostiles) while the body outruns them (done within 3 blocks of it; given up for jobs.survival.retreat-goal/retry-s after a leg that lost ground or left the goal's height), else leans toward, like a refuge; nil: away only" :spec goal/pos-spec :default nil}
   :step {:doc "blocks per walk" :spec (a/num-in 1 nil) :default 6}
   :reserve {:doc "health a cornered fight must be expected to leave" :spec (a/num-in 0 nil) :default cost/default-reserve}
   :max-places {:doc "seal placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pocket or pit is dropped and the lava left" :spec #{:seal :stop} :default :seal}
   :quiet-s {:doc "a hidden body keeps its refuge this many seconds after the last danger" :spec (a/num-in 0 nil) :default 30}
   :hide-s {:doc "a hidden body hands back after this many seconds in its refuge, though a danger it keeps off stays, once no real danger is on it as it stands" :spec (a/num-in 0 nil) :default 60}
   :swim-factor {:doc "blocks of walking a block of swimming costs a flight; nil: jobs.lib.cost/swim-factor of the boots" :spec (a/num-in 1 nil) :default nil}
   :hide-health {:doc "at or under this health, in a ranged chaser's line of fire with no covered step, take cover where it stands before walking on" :spec (a/num-in 0 20) :default 12}
   :no-gain-steps {:doc "flight steps without a new best gap to the nearest chaser before the cornered options are tried" :spec (a/int-in 1 nil) :default 20}
   :fist-sweeps {:doc "times a body with no weapon, every cornered option failed, looks for a way to walk before it punches a hostile it would not beat; 0: it punches at once" :spec (a/int-in 0 nil) :default 1}})


(def low-leg-s
  "Bound of a flight step while exposed low in a ranged chaser's line of fire: a search that stands still must not last."
  1.5)

(def slow-step-cap "Flee steps in a row that ran out of their seconds before the body moved (a slow start, nothing in the way) tried again before the way counts as blocked." 2)

(def stall-blocks "A low step that moved the body less than this stood still: cover where it stands comes next." 1)


(a/defargs settings
  {::job-time-s {:default 420 :doc "Seconds of a flight's time spend (its pauses not counted): past it the flight ends limit-reached and the hostile trigger fires again while the danger stays." :spec (a/num-in 0 nil)}
   ::mean-s {:default 20 :doc "Seconds a flight is guessed to take, the mean of its quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :phase :part-of '[jobs.survival.respond-to-hostile]})

(def safe? ready/ok)

(def fit? ready/ok)


(defn done?
  "Done once no real danger is within range and the flight holds no chaser, fled mob or refuge: those are written by the
  flight's end (flight/end-flight!), which the call still has to make."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)
        {:keys [fled chasers refuge]} (ctx/mem c)
        data [:units :hostiles :done (count fled) :left (count chasers)]]
    (if (or (seq fled) (seq chasers) refuge (seq (danger-q/dangers (:primitives c) radius {:ranged-radius ranged-radius} {:sight? false})))
      (apply contract/later :fleeing "a flight is on" data)
      (apply contract/ok :clear "no hostile is near" (conj data :result {:fled [] :ended :none})))))

(defn quote-of
  "A flight has to find out how long the mobs chase: time (0, ::mean-s, ##Inf)."
  [_c]
  {:uses {:job-time [0 (settings/get settings ::mean-s) ##Inf]}})


(defn close-hitters
  "Of the hostiles near, those that attack at close range (creepers included, not ranged mobs or endermen) within close-radius."
  [c]
  (filterv #(and (not (combat/ranged? %)) (not (combat/enderman? %))
                 (<= (danger-q/mob-distance (:primitives c) %) (cornered/close-radius)))
           (cornered/near-hostiles c)))


(def eat-gap
  "Blocks to the nearest hostile at which a bite is eaten per flee step."
  12)

(defn ^:async eat-on-the-run!
  "One bite a flee step, with the nearest hostile at least eat-gap away; none for the rest of the flight once
  nothing is left to eat or a bite fails."
  [c threat]
  (when (and (not (:ate (ctx/mem c)))
             (>= (danger-q/mob-distance (:primitives c) threat) eat-gap))
    (let [r (await (ctx/call c :eat 'jobs.survival.eat {:until 20 :max-bites 1}))]
      (when (and (contract/ending? r) (not= :done (:kind r)))
        (ctx/update-mem! c assoc :ate true)))))


(defn ^:async wait-far!
  "Cornered with every chaser beyond :radius: pause a moment (why cornered) and look again."
  [c]
  (await (child/pause! c (/ (flight/wait-ms) 1000) "cornered"))
  :again)

(defn ^:async run-home!
  "A step to room (home or another closed building) to the room cell inside its door way (retreat-home/way-in), said
  once a flight (retreat.home): :again; a step that gets no nearer drops that room for the rest of the flight."
  [c room way]
  (retreat-home/say-once! c room)
  (when-not (await (cornered/step! c :home (:inside way) 0)) (retreat-home/failed! c room))
  :again)


(defn say-target!
  "Log (retreat.target) the cell a flee step walks to, the chasers it runs from and whether it is out of every ranged one's line of fire."
  [c target threats covered?]
  (let [cell (mapv #(js/Math.floor %) [(:x target) (:y target) (:z target)])
        from (mapv #(str (.-name %) " " (.-id %)) threats)]
    (ctx/emit! c :retreat.target :info {:target cell :from from :cover (boolean (and covered? (covered? target)))
                                        :text (str "flee step to " (str/join " " cell) " away from " (str/join ", " from))})))

(defn say-choice!
  "Log (retreat.choice info), when it changes, what the flight chose among: the hostiles it knows and where, each
  closed room (survey) and why it is not run to (verdicts; nil: it is), the cells that are no closed room, the home,
  the goal and why it is dropped (judged), and where the flight leans. blast: the creepers within blast reach
  (flight/blaster?), for which no room was judged."
  [c survey verdicts judged blast]
  (let [p (:primitives c)
        at (fn [pos] (when pos (mapv #(js/Math.floor (% pos)) [:x :y :z])))
        rooms (mapv (fn [{:keys [room why inside?]}]
                      {:at (at (:start room)) :own? (boolean (:home? room)) :why (if inside? :inside why)})
                    verdicts)
        goal (at (:goal judged))
        blast (mapv #(str (.-name %) " " (.-id %)) blast)
        lean (at (or (:start (first (:rooms survey))) (:home judged) (retreat-goal/usable judged) (when (empty? blast) (cover/home-pos c))))
        choice {:blast blast :rooms rooms :dropped (:dropped survey) :home (at (cover/home-pos c)) :home-why (retreat-goal/home-why (u/self-pos c) (retreat-goal/hostile-spots c) (cover/home-pos c)) :goal goal :goal-why (:why judged) :lean lean}]
    (when (not= choice (:choice-said (ctx/mem c)))
      (ctx/update-mem! c assoc :choice-said choice)
      (ctx/emit! c :retreat.choice :info
                 (assoc choice
                        :hostiles (mapv #(str (.-name %) " " (.-id %) " " (at (danger-q/mob-pos p %))) (flight/known-chasers p))
                        :text (str "flee choice: leans to " (or lean "nothing")
                                   (when (:home-why choice) (str "; home " (at (cover/home-pos c)) " dropped (" (name (:home-why choice)) ")"))
                                   (when goal (str "; goal " goal (when (:why judged) (str " dropped (" (name (:why judged)) ")"))))
                                   (when (seq blast) (str "; no room is a refuge within blast reach of " (str/join ", " blast)))
                                   (when (seq rooms)
                                     (str "; rooms " (str/join ", " (map #(str (:at %) " " (name (or (:why %) :run))) rooms))))
                                   (when (seq (:dropped survey))
                                     (str "; no room " (str/join ", " (map #(str (:at %) " " (name (:why %))) (:dropped survey)))))))))))

(defn lone-melee?
  "Whether hostiles is one melee mob a pillar stops (not a creeper, enderman, spider, flyer or shooter) farther than cover-gap (it would stand in the cells to fill) and within refuge-gap."
  [p hostiles]
  (let [h (first hostiles)]
    (boolean (and h (nil? (second hostiles)) (pos? (cornered/refuge-gap))
                  (not (combat/ranged? h)) (not (combat/creeper? h)) (not (combat/enderman? h))
                  (not (combat/climber? h)) (not (combat/flyer? h))
                  (< (cornered/cover-gap) (danger-q/mob-distance p h) (inc (cornered/refuge-gap)))))))

(defn ^:async early-refuge!
  "A lone melee hostile close by that a fight would not win, at least a pillar's blocks carried: pillar up, else seal in,
  before any flee step (a walk gains little on it). Once a flight: nil when it cannot, and then the flight walks."
  [c]
  (let [hostiles (cornered/near-hostiles c)]
    (when (and (not (:early-refuge-failed (ctx/mem c))) (lone-melee? (:primitives c) hostiles)
               (pillar/pillar-item c) (not (pillar/lean? c)) (not (cornered/fight-wins? c hostiles)))
      (let [r (or (when-not (flight/tried? c :pillar) (await (refuge/pillar! c)))
                  (await (refuge/plug-option! c :plug #{:walls})))]
        (when (nil? r) (ctx/update-mem! c assoc :early-refuge-failed true))
        r))))

(defn ^:async flight-step!
  "One step of the flight (flight/look!, then shut a door, or run home, or walk a step away, or the cornered options),
  :again; the flight's end (flight/end-flight!) once no chaser is left."
  [c]
  (let [{:keys [no-gain-steps]} (:args c)
        p (:primitives c)
        threats (flight/look! c)
        threat (first threats)
        hit? (or (cornered/hit-on-the-run! c) (cornered/beaten? c))
        door (when threat (retreat-doors/door-to-shut c (danger-q/mob-pos p threat)))
        early (when (and threat (not hit?) (not door)) (await (early-refuge! c)))
        covered? (cover/cover-fn p threats)
        blast (filterv #(flight/blaster? p %) threats)
        survey (when (and threat (not hit?) (empty? blast))
                 (retreat-home/survey c (cover/home-pos c)
                                      (if (some #(and (not (combat/ranged? %)) (<= (danger-q/mob-distance p %) (cornered/cover-gap))) threats)
                                        0
                                        (retreat-home/judge-per-step))))
        rooms (:rooms survey)
        verdicts (when survey (retreat-home/verdicts p rooms threats (not (cover/exposed-low? c covered?))))
        {:keys [room way inside?]} (retreat-home/pick verdicts)
        home-now? (fn [] (or inside? (and (nil? survey) (retreat-home/in-home? c (cover/home-pos c)))))
        judged (when threat (retreat-goal/judge c (if (seq blast) [] (remove nil? [(:start (first rooms)) (cover/home-pos c)]))))
        _ (when threat (say-choice! c survey verdicts judged blast))
        stuck (fn ^:async stuck [why]
                (if (seq (cornered/near-hostiles c))
                  (await (cornered/cornered! c why (or inside? (and (seq blast) (retreat-home/in-home? c (cover/home-pos c))))))
                  (do (ctx/update-mem! c update :since-gain (fnil inc 0))
                      (if (flight/no-gain? (ctx/mem c) no-gain-steps)
                        (flight/end-flight! c :keeps-off)
                        (await (wait-far! c))))))]
    (cond
      (nil? threat) (or (await (flight/look-back! c))
                        (when (refuge/plugging-here? c) (await (refuge/plug-on! c)))
                        (flight/end-flight! c nil))
      (some? early) early
      (flight/no-gain? (ctx/mem c) no-gain-steps)
      (do (when (seq (cornered/near-hostiles c)) (ctx/update-mem! c assoc :since-gain 0))
          (await (stuck "the chaser keeps up")))
      (cornered/angry-enderman? c (cornered/near-hostiles c)) (await (cornered/cornered! c "an enderman cannot be outrun" (home-now?)))
      hit? (await (cornered/cornered! c "a hostile keeps hitting the body as it flees" (home-now?)))
      (some combat/flyer? (cornered/near-hostiles c)) (await (cornered/cornered! c "a phantom cannot be outrun" (home-now?)))
      door (await (retreat-doors/shut-door! c door))
      (and inside? (retreat-doors/home-door c room)) (await (retreat-doors/shut-home-door! c (retreat-doors/home-door c room)))
      inside? (await (stuck "at home, the hostile still has a way in"))
      way (await (run-home! c room way))
      (not (cornered/outrun? c (cornered/near-hostiles c))) (await (cornered/cornered! c "the hostile outruns the body" inside?))
      :else
      (let [_ (await (eat-on-the-run! c threat))
            away? (fn [cell] (when (and cell (not (walk/too-close? (mapv #(danger-q/mob-pos p %) threats) cell))) cell))
            stair (cover/way-up c threats)
            leg (when-not stair (away? (retreat-goal/leg c judged)))
            gap0 (flight/nearest-gap p threats)
            goal-gap0 (when leg (u/dist (u/self-pos c) leg))
            target (or leg (away? (await (cover/flight-target! c threats covered? rooms))))
            grown? (and (zero? (:regrown (ctx/mem c) 0))
                        (seq (set/difference (set (map #(.-id %) (flight/look! c))) (set (map #(.-id %) threats)))))
            low? (cover/exposed-low? c covered?)
            cover (cond
                    grown? (do (ctx/update-mem! c update :regrown (fnil inc 0)) :again)
                    (and low?
                         (or (refuge/plugging-here? c) (not (and target (covered? target))) (pos? (:stalled (ctx/mem c) 0)))
                         (not (cover/close-danger? p threats)))
                    (await (cover/take-cover! c))
                    (and target (not stair) (sh/buried? p) (not (cover/close-danger? p threats)))
                    (await (cover/dig-pocket! c)))]
        (cond
          (some? cover) cover
          (nil? target) (await (stuck "no open way away from the hostile"))
          :else
          (let [_ (when (:cornered (ctx/mem c)) (ctx/update-mem! c dissoc :cornered))
                _ (say-target! c target threats covered?)
                _ (when leg (retreat-goal/start-leg! c))
                from (u/self-pos c)
                moved (await (cornered/leg-step! c :step target 1
                                        (cond (or (seq (close-hitters c)) (cornered/recently-hurt? c)) (cornered/close-leg-s)
                                              low? low-leg-s
                                              :else cornered/flight-timeout-s)
                                        (if leg (retreat-goal/margin c) retreat-goal/leg-margin)))]
            (ctx/update-mem! c #(if (and low? (< (u/dist from (u/self-pos c)) stall-blocks))
                                  (update % :stalled (fnil inc 0))
                                  (dissoc % :stalled)))
            (cond
              (and leg (not (and moved
                                 (>= (or (flight/nearest-gap p threats) js/Infinity) (dec (or gap0 0)))
                                 (< (u/dist (u/self-pos c) leg) (- goal-gap0 0.5)))))
              (do (retreat-goal/off! c leg :lost-ground) :again)
              (and leg (retreat-goal/drifted? c leg)) (do (retreat-goal/off! c leg :height) :again)
              (and (not moved) (:leg-timed-out (ctx/mem c)) (< (:slow-steps (ctx/mem c) 0) slow-step-cap))
              (do (ctx/update-mem! c update :slow-steps (fnil inc 0)) :again)
              (not moved) (await (stuck "the way away from the hostile is blocked"))
              :else
              (let [gap (flight/nearest-gap p threats)]
                (ctx/update-mem! c #(-> % (dissoc :tried :slow-steps :regrown) (flight/note-gap gap)))
                :again))))))))

(defn ^:async round
  "One whole flight: steps (flight-step!, or the refuge's) until it ends."
  [c]
  (ctx/update-mem! c flight/resume-flight (ctx/now c))
  (when-not (:flight-start (ctx/mem c)) (ctx/update-mem! c assoc :flight-start (ctx/now c) :flight-from (u/self-pos c) :flight-health (.-health (.self (:primitives c)))))
  (loop []
    (ctx/update-mem! c assoc :last-step (ctx/now c))
    (let [r (await (if (:refuge (ctx/mem c)) (refuge/refuge-round! c) (flight-step! c)))]
      (if (= :again r)
        (do (await (pace/pace!)) (recur))
        r))))
