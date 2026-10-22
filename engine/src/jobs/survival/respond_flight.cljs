(ns jobs.survival.respond-flight
  "Flee from the hostiles in range, one whole flight per round: run until no mob chases the body any more.
  A real danger (jobs.lib.danger: a mob with a walkable way to the body, or a ranged one with a line of fire) within
  ::hostile-radius (ranged ones ::ranged-radius) starts a chase, or is seen afresh. A mob stops chasing (vanilla) once it is
  gone (dead, despawned, untracked), beyond its follow range (jobs.lib.threats: zombie 35, most 16), out of line of
  sight for respond.state/lost-s (4 s), a melee one beyond ::hostile-radius that stood at one place in sight for respond.state/still-s (5 s), or has no walkable way to the body (nor, ranged, a line of fire; a creeper within
  respond.state/::blast-reach (6) always has one: its blast).
  Each step, in this order:
  1. A door, gate or trapdoor standing open within a hand's reach and nearer the nearest chaser than the body is shut
     with one click (retreat.door-shut info). Each door is clicked at most once a flight.
  2. Runs into a closed building first (not while a creeper chases: its blast would take the door and what is inside;
     with a creeper within blast reach no room is looked for, and the one the body stands in is no refuge: it steps away
     or takes the cornered options): the latest :bed or :home within respond.cover/home-range (64), or a room behind a wooden door or
     gate the body has seen (jobs.lib.rooms/rooms, bounded by its settings), that is a closed room (walls,
     doors that shut: jobs.lib.reach.room/room) whose door it walks to (over seen blocks, at its own speed) ahead of every
     melee chaser (jobs.lib.cost/reach-first?), the fewest steps of those, with a go-to child to the room cell inside
     that door (retreat.home info, once a flight); a building dropped only for unseen cells, with its door side within 6 blocks, is looked into once a flight from its doorway (then from one cell beyond it), up to ::door-looks (4) looks at its unseen sides (room-lib/glance-in!; never with a creeper chasing) before the choice; a step that gets no nearer drops that room for the flight. Not while
     low in a ranged chaser's line of fire (the cover step below comes first). Any cell of the room counts as home:
     there an open door of the room is shut (a step back to one out of reach first), and a chaser still with a way in
     gets the cornered options (4.), its plug the walls alone (no pit or pocket dug in the hut): it does not walk out.
  Underground (jobs.lib.shelter.sense/buried?) it first takes the stairs the body cut (ledger/ways-of) or the places its recent walks began (cover/trail-floors): the highest such cell within 14
  blocks, above it and no nearer a chaser), before any walk to the :goal, and with none digs a side pocket (as below) before it walks across the open cave,
  unless a creeper or a melee mob is within cover-gap.
  2b. A lone melee mob (no creeper, enderman, spider, flyer or shooter) between ::cover-gap (5) and ::refuge-gap (10) that a fight would not win, with
     pillar-height blocks carried: pillar up, else seal in, before any flee step (a walk gains little on it); when neither can be
     done the flight walks. Each flee step emits debug retreat.step-speed {:blocks :seconds :bps}.
  3. Walks a short step (the respond.state ::step setting, 6 blocks) away from all chasers (nearer ones weigh more) with a go-to child (:ways #{:walk :clear-seal}),
     leaning toward the first closed room of 2. (else a home it is nearer to than every hostile it knows, none on the way; else the :goal, the destination of the walk the flight
     interrupted, while it is safe: jobs.survival.respond.goal/verdict, judged every step in three dimensions against every hostile it knows and the places of those it fled lately; else the latest :bed or :home within respond.cover/home-range) when not through the
     hostiles (a closed room behind them: turned lib.flee-walk ::flank-turn degrees toward it, to go round them, while it still gains on them so:
     jobs.lib.cost/outruns-at?), avoiding :hazard positions.
     When a wall blocks the way away it turns up to 120 degrees towards open ground (at least 2 clear cells).
     It leaves out the ends within respond.state/::died-radius of a recent :died place, then dark ones
     (respond.state/::prefer-lit?), each only while another end is open.
     It prefers, in this order: a step whose end is out of every ranged chaser's line of fire (cover: leaves stop arrows),
     the full step that takes least time (a swim block costs respond.cover/::swim-factor; with a drowned among the chasers no step ends in water: none dry is cornered), the most open.
     It first looks away and to either side where it has not seen the ground (at most 3 looks); a cell still unseen
     reads as rock.
     Eats one bite a step (up to food 20) when the nearest chaser is at least eat-gap (12) blocks away and food is carried.
  Low on health (at most respond.state/::hide-health, 12) in a seen ranged chaser's line of fire (over the cells it has seen) with no
  covered step, it takes cover where it stands first: plug in (as below);
  not possible, it walks on. A plug begun where it stands is finished before it walks on (refuge/plugging-here?).
  Not while a creeper chases it or a melee mob is within cover-gap: it walks (or backs off, cornered) first.
  A step of at most 1.5 s there that left the body where it stood (a search) is followed by that cover at once.
  A mob that attacks at close range within ::hostile-radius and outruns the body (jobs.lib.cost/outruns?: a spider at any food;
  not a creeper or an enderman): no flee step. After the door and room steps (1., 2., the race at each chaser's own
  speed) it takes the cornered options (4.) without the back-off.
  4. Cornered (no open direction worth a walk, or the walk is blocked, or a melee hostile within 3 hit the body since the last step, or the body is in a closed room with a melee mob within close-radius and carries no weapon or block, or melee mobs within close-radius hit it respond.options/::beaten-hits (2) times, or a ranged mob ::shot-hits (2) times, within its ::hurt-window-s: it cannot be outrun, and the fight is weighed over those hostiles alone; checked afresh every step) with no hostile within
     ::hostile-radius: holds a second (wait, why cornered) and looks again; after the respond.state ::no-gain-steps setting (20) of such steps or holds with no gain the flight ends :keeps-off. With one within ::hostile-radius: takes the safest option it has not yet failed.
     - an angry enderman near (one the client flags angry, that hurt the body this flight or is within 3 blocks; it teleports; a walk is no flight): at once, before any walk: water within 6 blocks (it keeps away from water; the body steps in and holds), else a seen 2-high gap under a roof within 16 blocks, not beside the enderman (a 3-tall enderman cannot enter; walks there and holds), else plug in, last of all fight.
     - a drowned near while the body is in water (and no fight wins): swims to the nearest seen dry stand within 16 blocks first (past the mob if need be), before seal, pillar or a fist fight.
     - fight (jobs.combat.attack) only when jobs.lib.cost/fight-damage leaves ::fight-reserve health. Never against a creeper.
     - plug in (jobs.survival.shut-in, a child: the walls, and later a pit or side pocket, never at home; the early refuge the walls alone): shuts the body
       in with carried blocks (or, with none to wall with, ones it digs), the sides towards a seen ranged hostile first. Every
       plug block goes to the scaffold ledger (purpose :retreat-plug), taken back by jobs.access.cleanup once no hostile is near.
     - pillar up 3 (jobs.access.pillar, the carried block with the most, at least 3). Needs a solid floor, free cells above and no slope or ledge a mob could climb to beside its top.
       Not against a ranged mob or a spider (it climbs the pillar). Every block goes to the scaffold ledger (purpose :pillar) for jobs.access.cleanup.
     - back off: a step of up to 2 cells in any of 8 directions that gains at least a block on the hostile.
     - last of all, fight with the best weapon or tool (pickaxe, shovel, hoe) or the fist. A body with no weapon
       (the call's :use :tools, else jobs.lib.combat/default-weapons) first looks for a way to walk once more (respond.options ::fist-sweeps, 1).
       A pillared body that is hit raises the pillar `respond.pillar/::pillar-extend` (2) blocks, up to `::pillar-raises` (3) times, before the refuge fails.
     Order: fight if it wins, then plug in (walls), pillar (not against a ranged mob or a spider), back off (not from a mob that outruns the body), plug in by a pit or pocket (not at home), a pillar through another's area (the last resort), fight.
     Against a creeper back off comes first.
     Hit ::beaten-hits times in ::hurt-window-s by melee mobs within close-radius, the fight comes first if armed or with no blocks to wall with, else it walls in first.
     Under fire (::fire-hits hits within ::hurt-window-s whose attacker is a ranged mob, or arrow damage: the mob need not be seen or known) cover comes before any fight: the order is
     plug in (walls), back off, plug in (pit or pocket), fight, though a melee mob is at the body; the fight is weighed over every hostile near, and never counts as won
     while no ranged hostile near has been seen. With none seen the last-resort fight swings only at a mob within 3 blocks, none walked to. In another cell than the
     last time it was under fire, a plug that failed is tried again. A shot's direction (the hurt event's :dir, jobs.lib.shot window-s) stands for an unseen shooter jobs.lib.shot reach blocks out: cover, the seal's sides, the way away and back-off all use it.
     An option that fails is not tried again until all have failed. Then, after a second's hold, all are tried again
     (one retreat_blocked warning per flight).
  Plugged in or up a pillar it hides (one retreat_sealed warning; a declared hold, wait why hiding) while a
  hostile within its follow range (jobs.lib.threats; ranged ones at least ::ranged-radius) would have a walkable way to
  the refuge if its own blocks were gone, and for the hide-out ::quiet-s setting (30 s) more after the last such danger (a silent mob is forgotten
  after a few seconds); then the flight ends :hidden. After the hide-out ::hide-s setting (60 s) in the refuge with no real danger on the body as it
  stands (jobs.lib.danger/danger-near?, the hostile-near trigger's test) it ends :hidden even while that hostile stays;
  the refuge stays, its plugs too while a hostile is near (jobs.access.cleanup). In a plug or pit with a melee hostile at its foot (::foot-reach) past ::hide-s,
  a body digs a stair up (jobs.access.stair, ::dig-out-steps steps) on the heading away from that mob, the next heading when one stops, before it hands back.
  A chase is bounded by the response's time spend: past it the response ends limit-reached :job-time where it
  stands (refuge included) and the hostile trigger fires again while the danger stays. With no new best gap to the
  nearest chaser in ::no-gain-steps (20) steps it takes the cornered options instead of walking on; it ends nothing-possible
  :cannot-escape only once every option failed in two sweeps in a row with no gain between.
  Each change of what it chose among is said in one retreat.choice info: the hostiles and where, every closed room and why
  it is not run to, the cells that are no closed room, the home and why it is no run (:home-why), the goal and why it is dropped, where it leans.
  Blocks placed are the call's :use :blocks, else every :build filler, carried or dug.
  The flight ends done {:fled [ids] :ended :gone|:far|:lost|:closed|:hidden|:keeps-off|:none} or nothing-possible
  :cannot-escape {:fled [ids]}; it is a step loop of jobs.survival.respond-to-hostile, which makes every choice above.
  Memory: one :threat entry per mob fled (jobs.lib.threats); the third from one mob within 5 min warns hostile.chased.
  A cell in another's zone is used only as a last resort (shut-in.trespass-last-resort warning)."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.survival.respond.doors :as respond.doors]
            [jobs.lib.pace :as pace]
            [jobs.lib.shot :as shot]
            [jobs.lib.util :as u]
            [jobs.survival.respond-choose :as choose]
            [jobs.survival.respond.options :as cornered]
            [jobs.survival.respond.cornered :as pick]
            [jobs.survival.respond.cover :as cover]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.respond.refuge :as refuge]
            [jobs.survival.respond.step :as step]))


(defn ^:async wait-far!
  "Cornered with every chaser beyond ::hostile-radius: pause a moment (why cornered) and look again."
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
  "A step to room (home or another closed building): one leg of jobs.survival.shut-in's :room way to the room cell inside its
  door way (room-lib/way-in), said once a flight (retreat.home): :again; a way that ends other than done (no leg got nearer, a chaser
  reaches the door first) drops that room for the rest of the flight."
  [c room way]
  (say-once! c room)
  (let [e (await (ctx/call c :room 'jobs.survival.shut-in {:ways #{:room} :at (:inside way) :flee? true}))]
    (when-not (or (= :continue e) (= :done (:kind e))) (failed! c room))
    :again))


(defn ^:async flight-step!
  "One step of the flight (flight/look!, then respond-choose picks: shut a door, run home, walk a step away or the
  cornered options), :again; the flight's end (flight/end-flight!) once no chaser is left."
  [c]
  (let [p (:primitives c)
        threats (flight/look! c)
        threat (first threats)
        hit? (or (cornered/hit-on-the-run! c) (pick/beaten? c) (pick/shot-on-the-run? c) (pick/bare-boxed-in? c (cornered/near-hostiles c)))
        door (when threat (respond.doors/door-to-shut c (danger-q/mob-pos p threat)))
        covered? (cover/cover-fn p threats (flight/fire-spots c))
        blast (filterv #(flight/blaster? p %) threats)
        survey-now (fn []
                     (room-lib/survey c (cover/home-pos c)
                                          (if (some #(and (not (combat/ranged? %)) (<= (danger-q/mob-distance p %) (cornered/cover-gap))) threats)
                                            0
                                            (room-lib/judge-per-step))))
        survey0 (when (and threat (not hit?) (empty? blast)) (survey-now))
        glanced? (boolean (and survey0 (not-any? combat/creeper? threats) (await (room-lib/glance-in! c survey0))))
        survey (if glanced? (survey-now) survey0)
        rooms (:rooms survey)
        verdicts (when survey (room-lib/verdicts p rooms threats (not (cover/exposed-low? c covered?))))
        {:keys [room way inside?]} (room-lib/pick verdicts)
        _ (when (and glanced? room inside?) (say-once! c room))
        early (when (and threat (not hit?) (not door) (nil? room) (not inside?)) (await (step/early-refuge! c)))
        home-now? (fn [] (or inside? (and (nil? survey) (room-lib/in-home? c (cover/home-pos c)))))
        judged (when threat (respond.goal/judge c (if (seq blast) [] (remove nil? [(:start (first rooms)) (cover/home-pos c)]))))
        _ (when threat (step/say-choice! c survey verdicts judged blast))
        stuck (fn ^:async stuck [why]
                (if (seq (cornered/near-hostiles c))
                  (await (pick/cornered! c why (or inside? (and (seq blast) (room-lib/in-home? c (cover/home-pos c))))))
                  (do (ctx/update-mem! c update :since-gain (fnil inc 0))
                      (if (flight/no-gain? (ctx/mem c) (flight/no-gain-steps))
                        (flight/end-flight! c :keeps-off {:text (flight/keeps-off-text p threats)})
                        (await (wait-far! c))))))
        near (when threat (cornered/near-hostiles c))
        {:keys [option why reset-gain?]}
        (choose/choose {:threat threat :early early :no-gain? (flight/no-gain? (ctx/mem c) (flight/no-gain-steps))
                        :angry-enderman? (and threat (cornered/angry-enderman? c near)) :hit? hit?
                        :flyer? (some combat/flyer? near) :door door :inside? inside?
                        :home-door (when inside? (respond.doors/home-door c room)) :way way
                        :outruns? (or (nil? threat) (cornered/outrun? c near))
                        :shot-open? (and (nil? threat) (shot/open? p (ctx/view c)))})]
    (case option
      :end (or (await (flight/look-back! c))
               (when (pick/plug-continues? c) (await (refuge/plug-on! c)))
               (flight/end-flight! c nil))
      :early early
      :dodge (await (step/dodge-shot! c))
      :stuck (do (when (and reset-gain? (seq (cornered/near-hostiles c))) (ctx/update-mem! c assoc :since-gain 0))
                 (await (stuck why)))
      :cornered (await (pick/cornered! c why (home-now?)))
      :cornered-inside (await (pick/cornered! c why inside?))
      :shut-door (await (respond.doors/shut-door! c door))
      :shut-home-door (await (respond.doors/shut-home-door! c (respond.doors/home-door c room)))
      :run-home (await (run-home! c room way))
      :walk (await (step/walk-away! c threats judged rooms covered? stuck)))))

(defn clear?
  "Whether no real danger is within range, nor a shot from an unseen shooter, and the flight holds no chaser, fled mob or
  refuge (those are written by the flight's end, flight/end-flight!, which the loop still has to make)."
  [c]
  (let [{:keys [fled chasers refuge]} (ctx/mem c)]
    (not (or (seq fled) (seq chasers) refuge (shot/open? (:primitives c) (ctx/view c))
             (seq (danger-q/dangers (:primitives c) (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)} {:sight? false}))))))

(def kept-keys
  "Memory the response owns beyond a flight: the child calls, the fight's counts and the last flight's ending."
  [:children :fight-prior :flight-end])

(defn forget-flight!
  "Drop what the flight wrote to job memory (everything not there when it began): a later flight starts afresh."
  [c]
  (ctx/update-mem! c #(select-keys % (concat (:pre-flight %) kept-keys))))

(defn ^:async flee!
  "One whole flight: steps (flight-step!, or the refuge's) until it ends: its ending, or :continue when the round is cut or
  the response may not run on. An ending clears the flight's memory (forget-flight!)."
  [c]
  (when-not (:flight-start (ctx/mem c)) (ctx/update-mem! c #(assoc % :pre-flight (set (keys %)))))
  (ctx/update-mem! c flight/resume-flight (ctx/now c))
  (when-not (:flight-start (ctx/mem c)) (ctx/update-mem! c assoc :flight-start (ctx/now c) :flight-from (u/self-pos c)))
  (let [r (loop []
            (ctx/update-mem! c assoc :last-step (ctx/now c))
            (let [r (await (if (:refuge (ctx/mem c)) (refuge/refuge-round! c) (flight-step! c)))
                  clear? (and (= :again r) (ctx/alive? c) (clear? c))]
              (cond
                (not= :again r) r
                (not (ctx/alive? c)) :continue
                clear? (ctx/end c :done nil {:fled [] :ended :none})
                (not= :ok (first (ctx/can-run c))) :continue
                :else (do (await (pace/pace!)) (recur)))))]
    (when (contract/ending? r) (forget-flight! c))
    r))
