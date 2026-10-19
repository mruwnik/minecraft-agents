(ns jobs.survival.retreat
  (:require [jobs.lib.ready :as ready]
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
            [jobs.lib.util :as u]
            [jobs.survival.retreat-choose :as choose]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.survival.retreat-cover :as cover]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-goal :as retreat-goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.retreat-refuge :as refuge]
            [jobs.survival.retreat-step :as step]))

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
     gate the body has seen (jobs.lib.rooms/rooms, bounded by its settings), that is a closed room (walls,
     doors that shut: jobs.lib.reach/room) whose door it walks to (over seen blocks, at its own speed) ahead of every
     melee chaser (jobs.lib.cost/reach-first?), the fewest steps of those, with a go-to child to the room cell inside
     that door (retreat.home info, once a flight); a building dropped only for unseen cells, with its door side within 6 blocks, is looked into once a flight from its doorway, up to ::door-looks (4) looks at its unseen sides (room-lib/glance-in!) before the choice; a step that gets no nearer drops that room for the flight. Not while
     low in a ranged chaser's line of fire (the cover step below comes first). Any cell of the room counts as home:
     there an open door of the room is shut (a step back to one out of reach first), and a chaser still with a way in
     gets the cornered options (4.), its plug the walls alone (no pit or pocket dug in the hut): it does not walk out.
  Underground (jobs.lib.shelter.sense/buried?) it first takes the stairs the body cut (ledger/ways-of: the highest stair cell within 14
  blocks, above it and no nearer a chaser), before any walk to the :goal, and with none digs a side pocket (as below) before it walks across the open cave,
  unless a creeper or a melee mob is within cover-gap.
  2b. A lone melee mob (no creeper, enderman, spider, flyer or shooter) between ::cover-gap (5) and ::refuge-gap (10) that a fight would not win, with
     pillar-height blocks carried: pillar up, else seal in, before any flee step (a walk gains little on it); when neither can be
     done the flight walks. Each flee step emits debug retreat.step-speed {:blocks :seconds :bps}.
  3. Walks a short step (the retreat-flight ::step setting, 6 blocks) away from all chasers (nearer ones weigh more) with a go-to child (:ways #{:walk :clear-seal}),
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
     :radius: holds a second (wait, why cornered) and looks again; after the retreat-flight ::no-gain-steps setting (20) of such steps or holds with no gain the flight ends :keeps-off. With one within :radius: takes the safest option it has not yet failed.
     - an angry enderman near (one the client flags angry, that hurt the body this flight or is within 3 blocks; it teleports; a walk is no flight): at once, before any walk: water within 6 blocks (it keeps away from water; the body steps in and holds), else a seen 2-high gap under a roof within 8 blocks (a 3-tall enderman cannot enter; walks there and holds), else plug in, last of all fight.
     - a drowned near while the body is in water (and no fight wins): swims to the nearest seen dry stand within 16 blocks first (past the mob if need be), before seal, pillar or a fist fight.
     - fight (jobs.combat.attack) only when jobs.lib.cost/fight-damage leaves :reserve health. Never against a creeper.
     - plug in (jobs.survival.shut-in, a child: the walls, and later a pit or side pocket, never at home; the early refuge the walls alone): shuts the body
       in with carried blocks (or, with none to wall with, ones it digs), the sides towards a seen ranged hostile first. Every
       plug block goes to the scaffold ledger (purpose :retreat-plug), taken back by jobs.access.cleanup once no hostile is near.
     - pillar up 3 (jobs.access.pillar, the carried block with the most, at least 3). Needs a solid floor, free cells above and no slope or ledge a mob could climb to beside its top.
       Not against a ranged mob or a spider (it climbs the pillar). Every block goes to the scaffold ledger (purpose :pillar) for jobs.access.cleanup.
     - back off: a step of up to 2 cells in any of 8 directions that gains at least a block on the hostile.
     - last of all, fight with the best weapon or tool (pickaxe, shovel, hoe) or the fist. A body with no weapon
       (the call's :use :tools, else jobs.lib.combat/default-weapons) first looks for a way to walk once more (retreat-cornered ::fist-sweeps, 1).
       A pillared body that is hit raises the pillar `retreat-pillar/::pillar-extend` (2) blocks, once, before the refuge fails.
     Order: fight if it wins, then plug in (walls), pillar (not against a ranged mob or a spider), back off (not from a mob that outruns the body), plug in by a pit or pocket (not at home), fight.
     Against a creeper back off comes first.
     Hit ::beaten-hits times in ::hurt-window-s by melee mobs within close-radius, the fight comes first if armed or with no blocks to wall with, else it walls in first.
     Under fire (::fire-hits hits within ::hurt-window-s whose attacker is a ranged mob, or arrow damage: the mob need not be seen or known) cover comes before any fight: the order is
     plug in (walls), back off, plug in (pit or pocket), fight, though a melee mob is at the body; the fight is weighed over every hostile near, and never counts as won
     while no ranged hostile near has been seen. With none seen the last-resort fight swings only at a mob within 3 blocks, none walked to. In another cell than the
     last time it was under fire, a plug that failed is tried again. A shot's direction (the hurt event's :dir, ::shot-window-s) stands for an unseen shooter ::shot-reach blocks out: cover, the seal's sides, the way away and back-off all use it.
     An option that fails is not tried again until all have failed. Then, after a second's hold, all are tried again
     (one retreat_blocked warning per flight).
  Plugged in or up a pillar it hides (one retreat_sealed warning; a declared hold, wait why hiding) while a
  hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius) would have a walkable way to
  the refuge if its own blocks were gone, and for the retreat-refuge ::quiet-s setting (30 s) more after the last such danger (a silent mob is forgotten
  after a few seconds); then the flight ends :hidden. After the ::hide-s setting (60 s) in the refuge with no real danger on the body as it
  stands (jobs.lib.danger/danger-near?, the hostile-near trigger's test) it ends :hidden even while that hostile stays;
  the refuge stays, its plugs too while a hostile is near (jobs.access.cleanup).
  A chase is bounded by the call's time spend (default ::job-time-s): past it the flight ends limit-reached :job-time where it
  stands (refuge included) and the hostile trigger fires again while the danger stays. With no new best gap to the
  nearest chaser in ::no-gain-steps (20) steps it takes the cornered options instead of walking on; it ends nothing-possible
  :cannot-escape only once every option failed in two sweeps in a row with no gain between.
  Each change of what it chose among is said in one retreat.choice info: the hostiles and where, every closed room and why
  it is not run to, the cells that are no closed room, the home and why it is no run (:home-why), the goal and why it is dropped, where it leans.
  Blocks placed are the call's :use :blocks, else every :build filler, carried or dug.
  Needs nothing to start (no conditions); with no real danger near and no flight under way it is done at once. Ends done {:fled [ids] :ended :gone|:far|:lost|:closed|:hidden|:keeps-off|:none},
  nothing-possible :cannot-escape {:fled [ids]}, or limit-reached. Quote: time (0, ::mean-s, ##Inf).
  Memory: one :threat entry per mob fled (jobs.lib.threats); the third from one mob within 5 min warns hostile.chased.
  A cell in another's zone is used only as a last resort (shut-in.trespass-last-resort warning).")

(a/defargs args
  {:radius {:doc "hostiles within this many blocks start a flight" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks start a flight" :spec (a/num-in 0 nil) :default 16}
   :goal {:doc "a position {:x :y :z} the flight walks to with go-to legs (planner danger costs keep it round the hostiles) while the body outruns them (done within 3 blocks of it; given up for jobs.survival.retreat-goal/retry-s after a leg that lost ground or left the goal's height), else leans toward, like a refuge; nil: away only" :spec goal/pos-spec :default nil}
   :reserve {:doc "health a cornered fight must be expected to leave" :spec (a/num-in 0 nil) :default cost/default-reserve}
   :max-places {:doc "seal placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pocket or pit is dropped and the lava left" :spec #{:seal :stop} :default :seal}
   :swim-factor {:doc "blocks of walking a block of swimming costs a flight; nil: jobs.lib.cost/swim-factor of the boots" :spec (a/num-in 1 nil) :default nil}
   :hide-health {:doc "at or under this health, in a ranged chaser's line of fire with no covered step, take cover where it stands before walking on" :spec (a/num-in 0 20) :default 12}})

(a/defargs settings
  {::job-time-s {:default 420 :doc "Seconds of a flight's time spend (its pauses included): past it the flight ends limit-reached and the hostile trigger fires again while the danger stays." :spec (a/num-in 0 nil)}
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


(defn ^:async wait-far!
  "Cornered with every chaser beyond :radius: pause a moment (why cornered) and look again."
  [c]
  (await (child/pause! c (/ (flight/wait-ms) 1000) "cornered"))
  :again)

(defn say-once!
  "One retreat.home info a flight: it runs to its home or to another closed building, room."
  [c {:keys [start home?]}]
  (when-not (:home-said (ctx/mem c))
    (ctx/update-mem! c assoc :home-said true)
    (let [{:keys [x y z]} start]
      (ctx/emit! c :retreat.home :info {:home [x y z] :own? home?
                                        :text (str (if home? "runs home to [" "runs into the closed building at [")
                                                   x " " y " " z "], ahead of the hostile")}))))

(defn failed!
  "A run to room got no nearer: no run to it again this flight."
  [c room]
  (ctx/update-mem! c update :refuges-failed (fnil conj #{}) (room-lib/cell-key (:start room))))

(defn ^:async run-home!
  "A step to room (home or another closed building) to the room cell inside its door way (room-lib/way-in), said
  once a flight (retreat.home): :again; a step that gets no nearer drops that room for the rest of the flight."
  [c room way]
  (say-once! c room)
  (when-not (await (cornered/step! c :home (:inside way) 0)) (failed! c room))
  :again)


(defn ^:async flight-step!
  "One step of the flight (flight/look!, then retreat-choose picks: shut a door, run home, walk a step away or the
  cornered options), :again; the flight's end (flight/end-flight!) once no chaser is left."
  [c]
  (let [p (:primitives c)
        threats (flight/look! c)
        threat (first threats)
        hit? (or (cornered/hit-on-the-run! c) (cornered/beaten? c))
        door (when threat (retreat-doors/door-to-shut c (danger-q/mob-pos p threat)))
        early (when (and threat (not hit?) (not door)) (await (step/early-refuge! c)))
        covered? (cover/cover-fn p threats (flight/fire-spots c))
        blast (filterv #(flight/blaster? p %) threats)
        survey-now (fn []
                     (room-lib/survey c (cover/home-pos c)
                                          (if (some #(and (not (combat/ranged? %)) (<= (danger-q/mob-distance p %) (cornered/cover-gap))) threats)
                                            0
                                            (room-lib/judge-per-step))))
        survey0 (when (and threat (not hit?) (empty? blast)) (survey-now))
        survey (if (and survey0 (await (room-lib/glance-in! c survey0))) (survey-now) survey0)
        rooms (:rooms survey)
        verdicts (when survey (room-lib/verdicts p rooms threats (not (cover/exposed-low? c covered?))))
        {:keys [room way inside?]} (room-lib/pick verdicts)
        home-now? (fn [] (or inside? (and (nil? survey) (room-lib/in-home? c (cover/home-pos c)))))
        judged (when threat (retreat-goal/judge c (if (seq blast) [] (remove nil? [(:start (first rooms)) (cover/home-pos c)]))))
        _ (when threat (step/say-choice! c survey verdicts judged blast))
        stuck (fn ^:async stuck [why]
                (if (seq (cornered/near-hostiles c))
                  (await (cornered/cornered! c why (or inside? (and (seq blast) (room-lib/in-home? c (cover/home-pos c))))))
                  (do (ctx/update-mem! c update :since-gain (fnil inc 0))
                      (if (flight/no-gain? (ctx/mem c) (flight/no-gain-steps))
                        (flight/end-flight! c :keeps-off)
                        (await (wait-far! c))))))
        near (when threat (cornered/near-hostiles c))
        {:keys [option why reset-gain?]}
        (choose/choose {:threat threat :early early :no-gain? (flight/no-gain? (ctx/mem c) (flight/no-gain-steps))
                        :angry-enderman? (and threat (cornered/angry-enderman? c near)) :hit? hit?
                        :flyer? (some combat/flyer? near) :door door :inside? inside?
                        :home-door (when inside? (retreat-doors/home-door c room)) :way way
                        :outruns? (or (nil? threat) (cornered/outrun? c near))})]
    (case option
      :end (or (await (flight/look-back! c))
               (when (cornered/plug-continues? c) (await (refuge/plug-on! c)))
               (flight/end-flight! c nil))
      :early early
      :stuck (do (when (and reset-gain? (seq (cornered/near-hostiles c))) (ctx/update-mem! c assoc :since-gain 0))
                 (await (stuck why)))
      :cornered (await (cornered/cornered! c why (home-now?)))
      :cornered-inside (await (cornered/cornered! c why inside?))
      :shut-door (await (retreat-doors/shut-door! c door))
      :shut-home-door (await (retreat-doors/shut-home-door! c (retreat-doors/home-door c room)))
      :run-home (await (run-home! c room way))
      :walk (await (step/walk-away! c threats judged rooms covered? stuck)))))

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
