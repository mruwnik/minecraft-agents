(ns jobs.survival.night
  (:require [jobs.lib.ledger :as ledger]
            [jobs.lib.ready :as ready]
            [jobs.survival.log-out :as log-out]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.checks :as checks]
            [jobs.lib.breath :as breath]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.price :as price]
            [jobs.lib.pace :as pace]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.look :as look]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.places :as places]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.sense :as sense]
            [jobs.lib.surface-up :as surface]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.threats :as threats]
            [jobs.lib.spent :as spent]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.walk.world :as wworld]
            [jobs.survival.bed-down :as bed-down]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.room-plug :as room-plug]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.shut-in-night :as sin]
            [jobs.survival.enter-room :as enter-room]
            [jobs.survival.night.ending :as ending]
            [jobs.survival.night.flee :as flee]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.light :as light]
            [jobs.survival.night.sites :as sites]))


(def doc
  "The night: one round from the trigger until the body is sheltered tonight or the night is over. The morning (out of the shelter, blocks taken back) is jobs.survival.leave-shelter, started by the dawn trigger.
  Ways (the call's :ways, all by default): :bed (a bed used or put down, a sleeper's log-out), :shelter (a roofed place
  entered, shut-in, niche, a breached room walled in, the exposed retry), :flee (a walk somewhere safer, a cave walled in).
  A door is always shut.
  Condition at the start: the night trigger's (a listed night waits :not-night, :spawn with the respawn bed's state); sheltered with nothing for the night to do, it is done before its first round. Quote: the time left of the night.
  Each pass chooses again from the world as it is, the first that applies:
  - light outside and no bed term of the trigger (over?): a furnace the hold put down is dug up once more; then the night is over: done {:night :slept|:dug-in|:logged-out|:roofed :fled n}, or nothing-possible :exposed after a night with no shelter.
  - asleep: hold (:sleeping), priced again whenever what it was priced on changes (asleep!): up (night.bed-left) when staying in bed is no longer the cheapest answer.
  - afloat: swim to the nearest dry land first (surface-up/leave-water!); none seen: nothing-possible :in-water. A swimmer is never roofed or sheltered.
  - roofed or buried in a closed region with sides never seen: a look at the nearest, else a walk to it (look-edge/look!: each walk priced, the walks before it spent from the night's worth) (an unseen side is open until seen: a lit tunnel with its walls seen is shelter, one with an unseen branch is not).
  - sheltered (slept, dug in, shut in): log out for a stint while someone else sleeps or is online (log-out-wanted?), else end so the queue runs (no all-night hold: listed jobs run when their own start checks pass).
  - a remembered roofed :roofed-places place not seen a roof on open sides (enter-room/unclosable?) whose walk go-to quotes cheapest (walkable-home: only those whose straight walk fits the time left before the dark plus a margin, night.sites home-walk-*, and bed-down/home-bound-s, are quoted best-first; a quote is asked once a pass), with no place remembered a closed room the body has seen (its walk plus the chance it is open times the dig-in's quote; a room not seen inside yet has the chance cost/closed-chance of its seen and unseen sides, a closed one 1), the body not roofed: when :home (a whole route found that fits that time, or any quoted with a carried bed) is the cheapest of bed-down/answers (night-answer), jobs.survival.enter-room walks in, worth what the next answer costs more than the home once there (one try tonight). Farther or dearer places are left to shut-in's :room way, priced against digging in.
  - an open door of the closed room the body stands in is shut; the room's doorway is filled before a carried bed goes down (each worth room-worth: what the cheapest way left while it stays open costs more than the shut room) (ledger :doorway; one that cannot be filled for want of blocks, or was filled, is :doorway-tried, not tried again tonight).
  - a bed to use (sh-beds/beds-to-use, the one whose walk go-to quotes cheapest) whose walk is safe (bed-walk-safe?: a whole route priced no dearer than staying put), a carried bed, or someone else asleep or online (log-out-wanted?, never mid-danger?): the cheapest of bed-down/answers (night-answer, debug night.answers): lie in the bed (its walk priced in) or the carried one put down here, put the carried bed down and shut its row in first (:sealed), jobs.survival.log-out (a 30 s stint, till morning when coming back costs more, log-out/back-unsafe; not retried tonight when not ok), or shelter (on below). A bed is jobs.survival.bed-down, called again until it ends; one that stops hostile-near ends the round nothing-possible :monsters-near (the trigger fires again after its cooldown).
  - the sky not dark yet (dusk, a bed term started it): done; the shelter ways below wait for the dark.
  - a roof over a room with a hole (shelter.room_open warn): walled in under the roof (:enclose).
  - roofed, buried or shut in a closed room: done {:night :roofed}.
  - in a sealed room only dark cells beyond the spawn-free radius spoil, with torches or coal held, once a night, when its quote (torches x place time, crafting short torches from coal) is no more than the cheapest wall, pit or pocket: jobs.build.light-area over the room's cells (night.light), then the pass chooses again.
  - else the cheapest of shelter-prices (bed-down/answers): jobs.survival.shut-in {:for :night} (walls, pit, room by quote; its pit walks to other ground after a failed site, jobs.survival.dig-pit-site; priced again while another site may be left, at most max-sites), a niche (pocket way, once; with no site left, out of the failed pit and a look round first), a flee, or exposed, the night left in the open here (a shut-in that quotes no way here is called first all the same: its ending marks the sites failed, so a flee is not priced to ground beside them): one shelter.exposed warn, a log-out stint, and the round ends nothing-possible :exposed; the trigger fires again after its cooldown and the night looks again (a bed or sleeper that turned up is taken then). A shut-in, niche or flee in flight is called again until it ends.
  Fleeing (the night.sites ::flee-radius setting, 0 never; at most max-flees walks): to the not-yet-tried candidate whose walk go-to quotes cheapest (walk-pick/cheapest!): a known bed, a :roofed-places place, a seen cave or overhang cell (walled in on arrival), or seen natural ground the carried tools dig. shelter.fled on arrival, shelter.flee_failed otherwise.
  Only sleep holds (declared, ctx/hold-still!, hold-ms per wait; night.hold cooks, eats or runs get-food first).
  An overdue body warns needs_bed once a day; a respawn bed seen gone is written :spawn-lost (warn spawn_lost once).
  Memory: body memory :night-exposed (where the night ended with no shelter: later firings there go straight to the exposed log-out); job memory :edge-looked, :edge-phase, :edge-spent, :edge-walked, :enclose, :enclose-failed (feet cells where walling in failed), :sheltered, :log-out-failed, :no-site, :shut-ins, :digging, :place-other-s, :bed-down, :pit-trapped, :roof-walk-failed, :left; body memory :doorway-tried and shut-in's :shut-in-failed (forgotten when the night ends, at most ten minutes).
  Muting :night does not end a running night job: cancel it too.")

(a/defargs args
  {:roofed-places {:doc "place names walked to at night, when close and the route is lit, before digging in" :spec (a/coll-of (a/or-of keyword? string?)) :default [:home]}})

(a/defargs settings
  {::job-time-s {:default 2400 :doc "Seconds the night may take: a bound against a runaway night (the night itself ends it in about 10 minutes)." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds the night may last from its start, its yields to other jobs included." :spec (a/num-in 0 nil)}})

(def target [])

(def ways [:bed :shelter :flee])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s :wall-time ::wall-time-s} :ways (set ways)})

(def meta {:category :reflex :tags [:sleep :shelter]})

(def safe? ready/ok)

(def fit? ready/ok)

(defn trigger-args [c] (assoc (:args c) :ways (:ways (ctx/frozen-call c))))

(defn over?
  "The sky is light and no bed term of the trigger holds (sh/bed-holds?: a bed takes a sleeper from dusk, before the dark)."
  [c]
  (let [p (:primitives c)]
    (not (or (sh-sense/dark-outside? p) (sh/bed-holds? p (ctx/view c) (trigger-args c) (bed-down/bed-permit c))))))

(defn quiet-night
  "While the night trigger's condition fails: {:shelter} with the shelter the body is in (nil when none), else nil."
  [c]
  (let [p (:primitives c)]
    (when-not (sh/night-holds? p (ctx/view c) (trigger-args c) (bed-down/bed-permit c))
      {:shelter (sh-inside/night-sheltered p (:data (ctx/latest c :shelter)))})))

(defn can-start?
  ":later :not-night (with :spawn when the respawn bed is known or lost) while the trigger's condition fails and the
  body is in no shelter."
  [c]
  (let [q (quiet-night c)]
    (if (and q (not (:shelter q)))
      (let [spawn (sh-spawn/spawn-state (:primitives c) (ctx/view c))]
        (contract/status :later :not-night "not night" (if spawn {:spawn spawn} {})))
      (contract/ok :night "night, or in a shelter"))))

(defn spawn-lost-unnoted?
  "The respawn bed is seen gone and :spawn-lost does not name it yet (note-spawn-lost! runs only in a round)."
  [c]
  (when-let [pos (:lost (sh-spawn/spawn-state (:primitives c) (ctx/view c)))]
    (not= pos (:pos (:data (ctx/latest c :spawn-lost))))))

(defn night-over?
  "A started night is over (over?: light outside, no bed term), not an exposed night (the pass ends that nothing-possible), no furnace left to dig up, no lost respawn bed to note."
  [c]
  (and (checks/running? c)
       (over? c)
       (not= :exposed (:sheltered (ctx/mem c)))
       (or (:furnace-final (ctx/mem c)) (not (:placed-furnace (ctx/mem c))))
       (not (spawn-lost-unnoted? c))))

(defn done?
  "The whole goal: once started, the night is over (result {:night how :fled n}); before the start,
  already sheltered with nothing for the night to do (and no lost respawn bed to note). Asked every round."
  [c]
  (if (checks/running? c)
    (let [over (night-over? c)
          result (when over (ending/done-data c (:sheltered (ctx/mem c))))]
      (checks/goal over [:morning (or (:text result) "the night is over")] [:night "the night is not over"]
                   (cond-> {:units :nights} over (assoc :result result))))
    (checks/goal (and (boolean (:shelter (quiet-night c))) (not (spawn-lost-unnoted? c)))
                 [:sheltered "sheltered, nothing for the night to do"] [:night "the night is not over"] {:units :nights})))

(defn ^:async cleanup
  "However the night ends: tonight's failed sites, edge walks spent and filled doorway cells someone else changed are forgotten, and a root night releases its tree's scaffold."
  [c _]
  (ending/forget-night! c)
  (ledger/release-root! c)
  (ctx/end c :done nil))

(defn quote-of
  "The time left of the night (jobs.time.wait-for-light's quote), a bed possibly ending it at once."
  [c]
  (let [[_ mean most] (get-in (sh-clock/night-left-quote c) [:uses :job-time])]
    {:uses {:job-time [0 mean most]}}))

(def worth-of "The loss the night averts (bed-down/worth-of)." bed-down/worth-of)

(def needs-bed-policy {:cap 1 :ttl sh-clock/ms-per-day})

(defn note-needs-bed! [c]
  (when (empty? (ctx/entries c :needs-bed))
    (ctx/remember! c :needs-bed {} needs-bed-policy)
    (ctx/emit! c :needs_bed :warn {:days (sh-beds/days-awake c)
                                   :text "not slept for too long; phantoms will come; a bed is needed (the :needs-bed trigger fetches one by day when registered)"})))

(defn note-spawn-lost!
  "The respawn bed is seen gone: remember :spawn-lost and warn spawn_lost once (sh-spawn/lose-spawn!)."
  [c]
  (when (spawn-lost-unnoted? c)
    (sh-spawn/lose-spawn! c (:lost (sh-spawn/spawn-state (:primitives c) (ctx/view c))) :seen-gone)))

(defn log-out-wanted? [c]
  (and (not (:log-out-failed (ctx/mem c)))
       (bed-down/log-out-open? c)))

(defn ^:async log-out-step
  "Someone sleeps: a bite if hungry (hold/eat-if-hungry!; a logged-out body cannot eat), then a log-out stint (jobs.survival.log-out). Ok or cut: :logged-out unless already sheltered, and the
  next pass looks again. Declined or failed (unsupported, closed): :log-out-failed, not tried again by this job."
  [c]
  (hold/busy! c)
  (await (hold/eat-if-hungry! c))
  (let [l (await (ctx/call c :log-out 'jobs.survival.log-out {}))]
    (cond
      (= :continue l) :again
      (= :done (:kind l)) (do (when-not (:sheltered (ctx/mem c)) (hold/sheltered! c :logged-out)) :again)
      :else (do (ctx/update-mem! c assoc :log-out-failed true) :again))))

(defn ^:async bed-down-step
  "Sleep in bed, or put the carried bed down (bed nil) and sleep, answer :sealed its row shut in first: jobs.survival.bed-down. Asleep it is :sheltered :slept;
  the next pass chooses again. While the child goes on (:continue) job memory :bed-down holds {:bed bed :answer answer}, so the next
  pass calls it again rather than choosing anew. In a closed home (jobs.lib.rooms/closed-home?) the bed goes down under the roof only (:roofed-only)."
  [c bed answer]
  (hold/busy! c)
  (let [d (await (ctx/call c :bed-down 'jobs.survival.bed-down
                                 (cond-> {}
                                   bed (assoc :bed bed)
                                   (and (nil? bed) answer) (assoc :answer answer)
                                   (room-lib/closed-home? (:primitives c)) (assoc :roofed-only true))))]
    (if (= :continue d)
      (ctx/update-mem! c assoc :bed-down {:bed bed :answer answer})
      (ctx/update-mem! c dissoc :bed-down))
    (when (and (= :done (:kind d)) (:slept (:data d)))
      (hold/sheltered! c :slept))
    (when (= :sleep-failed (contract/cause-key d))
      (ctx/update-mem! c assoc :sleep-why (:text (:data d))))
    (if (= :hostile-near (:reason (:cause d)))
      (ctx/end c :nothing-possible :monsters-near {:text "monsters are near the bed; the hostile reflexes act, the night looks again after its cooldown"})
      :again)))

;; ------------------------------------------------------------------ a safe place

(defn walk-home-wanted?
  "Whether to walk to a roofed place before using a bed: none failed tonight, no shut-in in flight, no walk failed."
  [c]
  (let [m (ctx/mem c)]
    (and (not (:roof-walk-failed m)) (not (:digging m)) (empty? (pit-site/failed c)))))

(defn ^:async enter-room!
  "jobs.survival.enter-room (into the :place if given, else the home or a closed room seen: walks, shuts its doors, fills its doorway or roof hole); :again. A
  walk (a place given, or walk? for the room seen) is made once a night: once its call ends, :roof-walk-failed is set whatever came of it (a body that got in is roofed, and
  no walk is wanted). worth is the call's :worth (home-worth, room-worth)."
  ([c place] (enter-room! c place (some? place)))
  ([c place walk?] (enter-room! c place walk? nil nil))
  ([c place walk? at worth]
   (hold/busy! c)
   (let [w (await (ctx/call c :enter-room 'jobs.survival.enter-room (cond-> {}
                                                                      place (assoc :place place)
                                                                      at (assoc :at at)
                                                                      worth (assoc :worth worth))))]
     (when (and walk? (not= :continue w))
       (ctx/update-mem! c assoc :roof-walk-failed true))
     (when (and (map? w) (= :no-blocks (contract/cause-key w)))
       (some->> (room-lib/gap c) first (#(ctx/remember! c :doorway-tried % room-lib/doorway-tried-policy))))
     :again)))

(defn ^:async cheapest-walk!
  "jobs.lib.walk-pick/cheapest! with the pass's shared quotes (pick/quote-once!): {:pick x :price seconds} of the one of xs whose walk go-to
  quotes cheapest, within limit-s seconds; :searching while a quote's search goes on; nil when no walk is priced."
  [c xs opts]
  (await (pick/cheapest! c xs (assoc opts :quote pick/quote-once!))))

(defn open-room-cost
  "Seconds a room closed with chance closed-chance adds to a walk in: the chance it is open times dig-in-s, the dig-in the body then
  makes after all (jobs.lib.cost/closed-chance); 0 for a closed room, or when no dig-in is quoted (##Inf: nothing to weigh the walk against)."
  [closed-chance dig-in-s]
  (if (or (>= closed-chance 1) (infinite? dig-in-s)) 0 (* (- 1 closed-chance) dig-in-s)))

(defn room-chance
  "The chance the room at pose is closed: 1 for a closed room, cost/closed-chance of its sides seen and unseen for a room
  not seen inside yet (an opening can only be in a side), nil for any other."
  [p pose]
  (cond
    (room-lib/refuge-room? p pose) 1
    (reach-room/unseen-room? p pose) (let [{:keys [seen unseen]} (reach-room/boundary p pose)] (cost/closed-chance seen (count unseen)))))

(defn ^:async seen-room!
  "{:at cell :price seconds :quote q} of the room behind a door the body has seen whose walk plus open-room-cost is the least (cheapest-walk!, a room
  judged only when its turn comes) within limit-s seconds: a closed room (chance 1), or a room not seen inside yet (the chance none of its unseen
  sides hides an opening, cost/closed-chance of the sides seen closed; the walk shows it). :searching or nil as cheapest-walk!."
  [c limit-s]
  (let [p (:primitives c)
        poses (enter-room/seen-poses c)
        chances (atom {})
        chance (fn [pose] (get (swap! chances #(if (contains? % pose) % (assoc % pose (room-chance p pose)))) pose))]
    (when (seq poses)
      (let [dig-s (await (bed-down/dig-in-s c))
            walk #(enter-room/walk-args {:pos %})
            best (await (cheapest-walk! c poses {:target walk :walk walk :limit-s limit-s :ok? #(some? (chance %))
                                                 :price (fn [pose s] (+ s (open-room-cost (chance pose) dig-s)))}))]
        (if (map? best) (assoc best :at (:pick best)) best)))))

(defn place-walk-target [place] {:pos (:pos place) :range 0})

(defn roofed-places
  "room-lib/roofed-places-in of the night's :roofed-places, none seen a roof on open sides (enter-room/unclosable?)."
  [c]
  (remove #(enter-room/unclosable? (:primitives c) (:pos %)) (room-lib/roofed-places-in c (:args c) ##Inf)))

(defn ^:async walkable-home
  "Where the night may walk in to now, within the time left before the dark plus a margin (night.sites home-walk-*) and the seconds
  (limit-s!) resolves to (bed-down/home-bound-s: no walk longer could be the cheapest answer), asked only with somewhere to walk to: the remembered roofed place (roofed-places) whose walk
  go-to quotes cheapest (cheapest-walk!: only places whose straight walk fits, quoted best-first); with no place remembered and
  seen-room? true, the room the body has seen whose walk plus the chance it is open (seen-room!) costs least. Either as
  {:name :pos :cost :route :walkable?} (cost: the walk's price, its risk in, a seen room's chance open too; route: go-to's;
  walkable?: go-to found a whole route whose quoted seconds fit that time, so it is bed-down/answers' :home; a seen room only
  then). :searching while a quote's search goes on. Else nil."
  [c seen-room? limit-s!]
  (let [p (:primitives c)
        places (roofed-places c)
        budget (sites/home-walk-budget-s p)
        best (cond
               (seq places) (await (cheapest-walk! c places {:target place-walk-target :walk enter-room/walk-args :limit-s (min budget (await (limit-s!)))}))
               (and seen-room? (seq (enter-room/seen-poses c))) (await (seen-room! c (min budget (await (limit-s!))))))
        route (get-in best [:quote :route])
        walkable? (boolean (and (:found route) (<= (second (get-in best [:quote :uses :job-time])) budget)))]
    (cond
      (= :searching best) :searching
      (and (map? best) (or walkable? (not (:at best)))) (merge (if (:at best) {:seen true :at (:at best) :pos (:at best)} (:pick best))
                         {:cost (:price best) :route route :walkable? walkable?}))))

(defn ^:async exposed!
  "Nothing shelters the body tonight (the first time one shelter.exposed warn): a log-out stint (jobs.survival.log-out) while
  it lasts, then the round ends nothing-possible :exposed, so the trigger fires again after its cooldown and the night
  looks again. Where logging out is not possible the round ends at once."
  [c]
  (let [p (:primitives c)]
    (when-not (= :exposed (:sheltered (ctx/mem c)))
      (hold/sheltered! c :exposed)
      (when-not (pit-site/exposed-here? c)
        (ctx/remember! c :night-exposed {:pos (sh-sense/feet p)} pit-site/exposed-policy)
        (ctx/emit! c :shelter.exposed :warn {:pos (sh-sense/feet p) :sites (pit-site/failed c)
                                             :text (str "cannot shelter here (no bed, nobody asleep, no site to dig in, nowhere safer known: "
                                                        (sites/exposed-why (pit-site/failed c) (:no-way-text (ctx/mem c))) "); logging out until it is safe")})))
    (hold/busy! c)
    (let [l (if (:log-out-failed (ctx/mem c)) {:kind :nothing-possible} (await (ctx/call c :log-out 'jobs.survival.log-out {})))]
      (cond
        (= :continue l) :again
        (= :done (:kind l)) (ending/end-exposed! c)
        :else (do (ctx/update-mem! c assoc :log-out-failed true)
                  (ending/end-exposed! c))))))

(defn niche-args [] {:ways #{:pocket} :for :night :settings {:jobs.survival.dig-pocket/walk-radius (sites/niche-reach)}})

(defn ^:async niche-quote
  "sin/shelter-quote of shut-in's quote of a niche (niche-args), asked once a pass (walk-pick/once!)."
  [c]
  (await (pick/once! c :niche (fn ^:async f [] (sin/shelter-quote (await (ctx/quote c 'jobs.survival.shut-in (niche-args))))))))

(defn ^:async niche!
  "Out of the failed pit first (dig-in-leave/climb!, a stair), then jobs.survival.shut-in digs a pocket
  into a hillside or wall within niche-reach; the blocks that plug it are the :shelter entry's door (written by shut-in). Whatever it ends in, it is tried once (:niche :tried; :running
  while the call goes on)."
  [c pit]
  (hold/busy! c)
  (if pit
    (let [r (await (dig-leave/climb! c {:start pit} nil))]
      (when (map? r) (ctx/update-mem! c assoc :pit-trapped true))
      :again)
    (if-not (look/surveyed? c)
      (do (await (look/look-around! c)) :again)
      (let [d (await (ctx/call c :niche 'jobs.survival.shut-in (niche-args)))]
        (if (= :continue d)
          (ctx/update-mem! c assoc :niche :running)
          (do (ctx/update-mem! c assoc :niche :tried)
              (spent/absorb! c (:data d))))
        :again))))

;; ------------------------------------------------------------------ somewhere safer

(defn may-enclose?
  "Whether a wall-in may be started at feet cell f: none is under way, and none failed at f."
  [mem f]
  (not (or (:enclose mem) (contains? (:enclose-failed mem) f))))

(defn ^:async enclose!
  "At a cave target (:enclose is its feet cell): wall in with shut-in's walls way, one call. Sealed in: :dug-in, held until
  morning (dig-in-leave/leave! opens the door). Stopped or declined: shut-in has marked the site failed, and the
  roofed body ends the night :roofed. A body that is no longer at the cell forgets the target."
  [c]
  (let [p (:primitives c)
        pos (:enclose (ctx/mem c))
        roof-height (sh-sense/roof-height)]
    (if (not= pos (sh-sense/feet p))
      (do (ctx/update-mem! c dissoc :enclose) :again)
      (let [_ (hold/busy! c)
            d (await (ctx/call c :enclose 'jobs.survival.shut-in {:ways #{:walls} :for :night}))]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :enclose)
              (when (= :nothing-possible (:kind d)) (ctx/update-mem! c update :enclose-failed (fnil conj #{}) pos))
              (if (and (= :done (:kind d)) (dig-cells/sealed-in? p :walls roof-height))
                (hold/sheltered! c :dug-in))
              :again))))))

(def pit-roof-height
  "How far above the feet a block counts as the roof when dig-in must roof a pit under a roof that leaves the room open (an
  open door): the pit's own roof sits two above the original feet, the room's roof at three or more."
  2)

(defn dig-in-roof-height
  "The :roof-height for dig-in: the night's, but pit-roof-height at most while an open door stands in the room, so the room's
  roof does not make dig-in decline :already-sealed."
  [c]
  (let [h (sh-sense/roof-height)]
    (if (seq (sh/open-room-doors (:primitives c))) (min h pit-roof-height) h)))


(defn dig-in-inputs
  "What the night read when it chose to dig in here (the event night.dig-in-why)."
  [p]
  {:pos (sh-sense/feet p)
   :roofed (sh-sense/roofed? p (sh-sense/room-roof-height))
   :buried (sh-sense/buried? p)
   :shut-in (sh-sense/shut-in-room? p)
   :safe (sh-inside/safe-from-night? p)
   :head-in-block (sh-sense/head-in-block? p)
   :open-doors (count (sh/open-room-doors p))
   :unseen-roof (sh/unseen-roof-cell p)})

(defn more-sites?
  "Whether another shut-in call is worth making: fewer than max-sites made, and the pit has other ground to walk to."
  [c]
  (and (< (:shut-ins (ctx/mem c) 0) (sites/max-sites)) (= :there (:at (pit-site/site c)))))

(defn ^:async dig-in!
  "No bed, nobody asleep, not roofed: dig in here (jobs.survival.shut-in, one call). The call is marked :digging (a listed night
  resumes its child; dig-pit marks the site :interrupted while its pit is dug, so a firing after a cut does not dig it deeper).
  Ended unsheltered, the next pass prices it again while shut-in left a site to try (more-sites?), else :no-site drops it.
  place: the remembered roofed place shut-in prices its :room way for, nil for none."
  [c place]
  (let [p (:primitives c)
        resume (:digging (ctx/mem c))
        _ (when-not resume
            (ctx/emit! c :night.dig-in-why :info (dig-in-inputs p))
            (ctx/update-mem! c assoc :digging (sh-sense/feet p)))
        _ (when (and place (not= (sh-sense/feet p) (:route-looked (ctx/mem c))))
            (ctx/update-mem! c assoc :route-looked (sh-sense/feet p))
            (await (sense/look-round! c [])))
        d (await (ctx/call c :shut-in 'jobs.survival.shut-in
                           (cond-> {:ways (cond-> #{:walls :pit} (or place (sh-sense/roofed? p (sh-sense/room-roof-height)) (room-plug/room-plug p (sh-sense/feet p) (dig-in-roof-height c))) (conj :room))
                                    :for :night :settings {:jobs.lib.shelter.sense/roof-height (dig-in-roof-height c)}}
                             place (assoc :place place))))]
    (when-not (= :continue d)
      (ctx/update-mem! c #(cond-> (-> % (dissoc :digging :place-other-s) (update :shut-ins (fnil inc 0))) place (assoc :roof-walk-failed true)))
      (cond
        (not (and (= :done (:kind d)) (sh-sense/roofed? p (dig-in-roof-height c)))) (do (ctx/update-mem! c assoc :no-way-text (:text (:data d)))
                                                                                        (when-not (more-sites? c) (ctx/update-mem! c assoc :no-site true)))
        (not (and (= :room (:way (:data d))) (empty? (:placed (:data d))))) (hold/sheltered! c :dug-in)))
    :again))


(declare shelter-prices)

(defn ^:async place-other-s
  "Seconds of the way through the night a roofed place found spares: digging in here (dig-s, bed-down/dig-in-s), else, shut-in
  quoting none, the cheapest other way (shelter-prices, once a dig-in: job memory :place-other-s while :digging); nil while
  that is still searching, ##Inf when none is priced."
  [c dig-s]
  (let [m (ctx/mem c)]
    (cond
      (js/isFinite dig-s) dig-s
      (and (:digging m) (:place-other-s m)) (:place-other-s m)
      :else (let [r (await (shelter-prices c))]
              (when-not (= :searching r)
                (let [s (apply min ##Inf (vals (first r)))] (ctx/update-mem! c assoc :place-other-s s) s))))))

(defn ^:async shelter!
  "No bed, nobody asleep, not roofed: dig in here (dig-in!), shut-in pricing a walk into the roofed place whose walk go-to quotes cheapest
  (cheapest-walk!, only those no dearer than digging in is quoted, bed-down/dig-in-s); the looks for a place are priced by
  what one spares (place-other-s, no look while unknown; sense/find-price), at the chance a guessed roof cell is closed
  (cost/closed-cell-chance); a quote still searching waits a pass."
  [c]
  (hold/busy! c)
  (let [dig-s (await (bed-down/dig-in-s c))
        other-s (when-not (:roof-walk-failed (ctx/mem c)) (await (place-other-s c dig-s)))
        places (when-not (:roof-walk-failed (ctx/mem c))
                 (await (sense/decide! c ::roofed-place #(seq (roofed-places c))
                                       (cond-> {:verify-guesses true} other-s (assoc :price (sense/find-price c other-s cost/closed-cell-chance))))))
        pick (when (seq places) (await (cheapest-walk! c places {:target place-walk-target :walk enter-room/walk-args :limit-s dig-s})))]
    (if (= :searching pick)
      :again
      (await (dig-in! c (:name (:pick pick)))))))

(defn note-bed-far!
  "Once a night, with no bed to use: say the nearest known bed or :home is too far, so the agent can carry a bed on long trips."
  [c]
  (when-not (:bed-far-noted (ctx/mem c))
    (let [view (ctx/view c)
          here (u/self-pos c)]
      (when-let [pos (let [known (sort-by #(u/dist here %) (concat (sh-spawn/known-beds (:primitives c) view) (keep #(mem/place view %) [:home])))
                  nearest (first known)]
              (when (and nearest (> (u/dist here nearest) (bed-down/radius c))) nearest))]
        (ctx/update-mem! c assoc :bed-far-noted true)
        (ctx/emit! c :night.bed-too-far :info
                   {:pos pos :distance (js/Math.round (u/dist here pos))
                    :text (str "the bed or home at " (pr-str pos) " is " (js/Math.round (u/dist here pos))
                               " blocks away, too far to walk to tonight; digging in here (carry a bed to sleep on long trips)")})))))

(defn note-room-open!
  "Once a night, with a bed found open beside a roofed body: say the room is not shut in, so the roof alone does not end the night."
  [c]
  (when-not (:room-open-noted (ctx/mem c))
    (ctx/update-mem! c assoc :room-open-noted true)
    (let [feet (sh-sense/feet (:primitives c))
          escape (reach-room/escape-cell (:primitives c) feet)]
      (ctx/emit! c :shelter.room_open :warn (cond-> {:pos feet
                                                     :text (str "the room around the bed is not shut in (a hole in its walls" (when escape (str ", open to " (pr-str escape))) "); walling in under the roof at " (pr-str feet))}
                                              escape (assoc :escape escape))))))

(defn carried-bed-wanted? [c]
  (sh-beds/place-wanted? c))

(defn retract-gone-bed!
  "Retract the remembered :bed place when its block is seen and no bed, before any walk to it."
  [c]
  (when-let [pos (mem/place (ctx/view c) :bed)]
    (when (sh-spawn/seen-not-bed? (:primitives c) pos)
      (places/retract-if-missing! c :bed pos "missing"))))

(defn walk-hp-s
  "Seconds the hp a walk to pos along go-to's quoted route r is expected to cost: spawns over its dark seconds (the planner's
  :dark-seconds over jobs.lib.cost/dark-factor of the kit, by jobs.lib.price/stay-hp) and the fight with the hostiles known near pos
  (jobs.lib.price/way-fight-hp), each hp at jobs.lib.cost/hp-price."
  [c pos r]
  (let [secs (or (:seconds r) 0)
        dark-s (min secs (/ (or (:dark-seconds r) 0) (cost/dark-factor (wworld/kit-of c))))]
    (* (cost/hp-price (or (.-health (.self (:primitives c))) 20))
       (+ (price/stay-hp c (cost/spawn-chance [0 0 0]) dark-s) (price/way-fight-hp c pos)))))

(defn bed-walk-price
  "{:walk :walk-hp :stay} in seconds: the hp the walk to bed is expected to cost (spawns over its seconds in the dark, the planner's
  :dark-seconds over jobs.lib.cost/dark-factor of the kit; the fight with the hostiles known near its way) against
  staying put (spawns at the feet, spawn-light/night-chance, over the walk's
  seconds but no longer than shelter-s, the seconds sheltering here takes, which closes them out (none when already enclosed:
  spawns at the feet go on); the hostiles known near
  the body), both priced by jobs.lib.price/stay-hp and jobs.lib.cost/fight-damage, each hp at jobs.lib.cost/hp-price (:walk-hp,
  :stay); :walk also costs jobs.lib.cost/commute-s (in the ground, the walk there and back is work time lost). r is go-to's
  quoted route."
  [c bed r shelter-s]
  (let [p (:primitives c)
        secs (or (:seconds r) 0)
        hp-s (cost/hp-price (or (.-health (.self p)) 20))
        here (spawn-light/night-chance p (sh-sense/feet p))
        hp (walk-hp-s c bed r)]
    {:walk (+ hp (cost/commute-s (sh/in-ground? p (ctx/view c)) secs shelter-s))
     :walk-hp hp
     :stay (* hp-s (+ (price/stay-hp c here (if (pos? shelter-s) (min secs shelter-s) secs)) (price/way-fight-hp c (u/self-pos c))))}))

(defn ^:async bed-walk-safe?
  "Whether the night may walk to bed: within 2 blocks, or go-to's quote finds a whole route there whose hp (bed-walk-price
  :walk-hp) is no more than staying put costs, sheltering here taking the rest of the night at most (bed-down/dig-in-time-s), or, not
  overdue (overdue, bed-down/cheapest takes the bed whatever it costs), lying in that bed, its walk priced in, is the cheapest of
  bed-down/answers: a body on
  dark open ground walks (its price with the work it costs, :walk, kept in job memory :bed-walk {:bed :walk-s} for night-answer, which weighs it). Else :bed-walk-unsafe {:pos bed :state} is remembered (sh-beds/bed-to-use skips the bed while the body's cell and the hostiles near the way are as priced) with a
  night.bed-walk-unsafe info, and false."
  [c bed]
  (or (<= (u/dist (u/self-pos c) bed) 2)
      (let [r (:route (await (pick/quote-once! c {:pos bed :range 2})))
            [_ left] (get-in (sh-clock/night-left-quote c) [:uses :job-time])
            dig-s (when (:found r) (await (bed-down/dig-in-time-s c)))
            {:keys [walk walk-hp stay]} (when (:found r) (bed-walk-price c bed r (min left dig-s)))]
        (or (and (:found r)
                 (or (<= walk-hp stay)
                     (and (not (bed-down/overdue? c))
                          (= :bed (bed-down/cheapest-priced (await (bed-down/answers c {:bed bed :walk-s walk :log-out? (log-out-wanted? c)}))))))
                 (do (ctx/update-mem! c assoc :bed-walk {:bed bed :walk-s walk}) true))
            (do (ctx/remember! c :bed-walk-unsafe {:pos bed :state (sh-beds/walk-state (:primitives c) (ctx/view c) bed)} sh-beds/walk-unsafe-policy)
                (ctx/emit! c :night.bed-walk-unsafe :info
                           {:bed bed :found (boolean (:found r)) :walk-s walk :stay-s stay
                            :text (str "not walking to the bed at " (pr-str bed) ": "
                                       (if (:found r)
                                         (str "the walk is priced " (price/round1 walk) " s, staying here " (price/round1 stay) " s")
                                         "no whole way to it is known")
                                       "; sheltering here tonight")})
                false)))))

(defn ^:async walkable-bed
  "The bed the night walks to now: of the beds sh-beds/beds-to-use names, the one whose walk go-to quotes cheapest (cheapest-walk!; the first
  when none is priced; no time limit, a far bed's walk is weighed against staying by bed-walk-safe?), if its walk bed-walk-safe? finds fine, unless a bed-down is already under way; :searching while a quote's search goes on; else nil."
  [c]
  (let [p (:primitives c)
        beds (await (sense/decide! c ::bed #(seq (sh-beds/beds-to-use p (ctx/view c) (bed-down/radius c) (bed-down/bed-permit c))) {:price (sense/fails-to c)}))
        walk #(hash-map :pos % :range 2)
        picked (when (seq beds) (await (cheapest-walk! c beds {:target walk :walk walk :limit-s ##Inf})))
        bed (if (map? picked) (:pick picked) (first beds))]
    (cond
      (= :searching picked) :searching
      (and bed (or (:bed-down (ctx/mem c)) (await (bed-walk-safe? c bed)))) bed)))

(defn emit-answers! [c pick prices]
  (let [shown (into {} (map (fn [[k v]] [k (price/round1 v)])) prices)]
    (ctx/emit! c :night.answers :debug {:pick pick :prices shown
                                        :text (str "ways through the night priced (seconds): " (pr-str shown) "; taking " (if pick (name pick) "none"))})))

(defn ^:async shelter-prices
  "[prices target]: the ways through the night from here without a bed (bed-down/answers, its :bed, :sealed and :log-out left out):
  :shelter (shut-in here) with the :shelter way while a site is left (not :no-site), :niche (niche-quote, the climb out of the failed pit first in) with it while not tried, :flee
  with the :flee way to flee/flee-target (target, searched with a survey first when no shelter way is left). :searching while that
  search goes on."
  [c]
  (let [m (ctx/mem c)
        shelter? (ctx/way? c :shelter)
        dig? (and shelter? (not (:no-site m)))
        n (when (and shelter? (not= :tried (:niche m))) (await (niche-quote c)))
        pit (pit-site/pit-start c)
        feet (sh-sense/feet (:primitives c))
        n (if (and n pit) (update n :seconds + (pit-site/climb-s c feet (- (:y pit) (:y feet)))) n)
        t (when (ctx/way? c :flee) (await (flee/flee-target c (not dig?))))]
    (if (= :searching t)
      :searching
      (let [flee (when (map? t) (assoc t :seconds (get-in t [:route :seconds])))
            prices (await (bed-down/answers c {:niche n :flee flee}))]
        [(cond-> (dissoc prices :bed :sealed :log-out) (not dig?) (dissoc :shelter)) t]))))

(defn stay!
  "The night in the open here is the cheapest of prices while another way is priced too: the round ends nothing-possible
  :exposed {:prices :text}, nothing remembered of the site (no :night-exposed), so the trigger fires again after its
  cooldown and the night prices again. The failed sites stay known in the dark (:sheltered :exposed, ending/forget-night!)."
  [c prices]
  (let [shown (into {} (map (fn [[k v]] [k (price/round1 v)])) prices)]
    (hold/sheltered! c :exposed)
    (ctx/end c :nothing-possible :exposed {:prices shown
                                           :text (str "the night in the open here is the cheapest way through it (seconds: " (pr-str shown) "); priced again at the next firing")})))

(defn ^:async shelter-answer!
  "The cheapest of shelter-prices (bed-down/cheapest; debug night.answers): dig in here (shelter!), a niche (niche!), a flee
  (flee/flee!), or the night in the open: stay! while another way is priced, else exposed! (none is left)."
  [c]
  (hold/busy! c)
  (let [r (await (shelter-prices c))]
    (if (= :searching r)
      :again
      (let [[prices t] r
            pick (bed-down/cheapest c prices)]
        (emit-answers! c pick prices)
        (case pick
          :shelter (await (shelter! c))
          :niche (await (niche! c (pit-site/pit-start c)))
          :flee (await (flee/flee! c t))
          (if (next prices) (stay! c prices) (await (exposed! c))))))))

(defn ^:async answer-pass
  "The rest of a pass (see doc) once pick, night-answer's choice, is known: bed the bed to use, nil for the carried one."
  [c sheltered bed pick]
  (let [p (:primitives c)]
        (cond
          (#{:bed :sealed} pick) (await (bed-down-step c bed pick))
          (= :log-out pick) (await (log-out-step c))
          (not (sh-sense/dark-outside? p)) (ctx/end c :done nil (assoc (ending/done-data c sheltered) :text "no bed to take and the sky is not dark yet"))
          (and (:enclose (ctx/mem c)) (not= :dug-in sheltered)) (await (enclose! c))
          (and (sh-inside/safe-from-night? p) (empty? (sh/open-room-doors p)))
          (do (ctx/end c :done nil (ending/done-data c (if (= :logged-out sheltered) :logged-out :roofed))))
          (and (ctx/way? c :shelter) (await (light/wanted? c))) (await (light/light! c))
          (:fleeing (ctx/mem c)) (await (flee/flee! c (:fleeing (ctx/mem c))))
          (or (= :exposed sheltered) (pit-site/exposed-here? c) (:pit-trapped (ctx/mem c))) (await (exposed! c))
          (:digging (ctx/mem c)) (await (shelter! c))
          (= :running (:niche (ctx/mem c))) (await (niche! c nil))
          (and (:no-site (ctx/mem c)) (not= :tried (:niche (ctx/mem c))) (pit-site/pit-start c)) (await (niche! c (pit-site/pit-start c)))
          (and (:no-site (ctx/mem c)) (not (look/surveyed? c))) (do (hold/busy! c) (await (look/look-around! c)) :again)
          :else (await (shelter-answer! c)))))

(defn ^:async night-prices
  "bed-down/answers from here: :bed (bed, its walk's price from :bed-walk, else with carried? the carried bed here) and :sealed only
  with one of them, :log-out with bed-way and log-out-wanted?, :home with home (walkable-home's)."
  [c bed carried? bed-way home]
  (let [walk (:bed-walk (ctx/mem c))
        prices (await (bed-down/answers c {:bed bed :walk-s (when (= bed (:bed walk)) (:walk-s walk)) :log-out? (and bed-way (log-out-wanted? c))
                                           :home (some-> home (assoc :seconds (get-in home [:route :seconds])))}))]
    (if (or bed carried?) prices (dissoc prices :bed :sealed))))

(defn ^:async night-answer
  "[pick prices]: the cheapest of night-prices (bed-down/cheapest; debug night.answers), :bed, :sealed, :log-out, :home, :shelter
  or :exposed; nil when there is neither a bed, a log-out nor a home to weigh."
  [c bed carried? bed-way home]
  (when (or bed carried? (and bed-way (log-out-wanted? c)) home)
    (let [prices (await (night-prices c bed carried? bed-way home))
          pick (bed-down/cheapest c prices)]
      (emit-answers! c pick prices)
      [pick prices])))

(defn home-worth
  "Seconds the walk into home (walkable-home's, :cost its walk's price) is worth (Q54): what the next way (the cheapest of
  prices but :home) costs more than the home once there, ##Inf with none."
  [prices home]
  (max 0 (- (apply min ##Inf (vals (dissoc prices :home))) (- (:home prices) (:cost home)))))

(defn ^:async room-worth
  "Seconds shutting the room the body stands in is worth (Q54), as home-worth: what the cheapest way left while it stays
  open costs more than the night once it is shut, at least 0. closed: the answer the shut room is (:bed, the carried bed
  put down behind its filled doorway; for an open door shut, :bed when a bed lies in the room), nil when the shut room costs nothing more."
  [c bed carried? bed-way closed]
  (let [prices (await (night-prices c bed carried? bed-way nil))]
    (max 0 (- (apply min ##Inf (vals (dissoc prices closed))) (get prices closed 0)))))

(defn ^:async look-worth
  "What an edge walk to look is bought from (Q54): the cheapest way the night has without the look (night-prices, no bed)."
  [c bed-way]
  (apply min ##Inf (vals (await (night-prices c nil false bed-way nil)))))

(defn sleep-inputs
  "What staying asleep is priced on, read now (bed-down/answers): who else must still lie down (sh-sleepers/sleepers-short),
  whether the bed is sheltered, the spawn chance at the feet and at the cell the body gets up to, the fight with the
  hostiles known near each (price/way-fight-hp), the hp price (cost/hp-price) only while some of those can hurt, and
  whether a log-out is open (sh-sleepers/log-out-open?) and then what its quote reads of the body's health
  (log-out/back-death-cost, hurt-past-healing?)."
  [c]
  (let [p (:primitives c)
        view (ctx/view c)
        feet (sh-sense/feet p)
        wakes (bed-down/wake-cells p)
        chance (spawn-light/night-chance p feet)
        wake-chance (apply max (map #(spawn-light/night-chance p %) wakes))
        fight (price/round1 (price/way-fight-hp c feet))
        wake-fight (price/round1 (apply max (map #(price/way-fight-hp c %) wakes)))
        open? (and (not (:log-out-failed (ctx/mem c))) (sh-sleepers/log-out-open? p view))]
    {:short (sh-sleepers/sleepers-short p view)
     :sheltered (sh-beds/bed-sheltered? p feet)
     :chance chance
     :wake-chance wake-chance
     :fight fight
     :wake-fight wake-fight
     :hp-price (when (some pos? [chance wake-chance fight wake-fight]) (cost/hp-price (.-health (.self p))))
     :log-out open?
     :log-out-risk (when open? {:back (price/round1 (log-out/back-death-cost c)) :hurt (log-out/hurt-past-healing? (.self p))})}))

(defn ^:async asleep!
  "Asleep: priced again (bed-down/answers) whenever what it was priced on changes (sleep-inputs, against job memory
  :sleep-judged). While staying in bed is the cheapest, hold (:sleeping); else get up (any act leaves the bed;
  night.bed-left) and choose again."
  [c]
  (let [p (:primitives c)
        inputs (sleep-inputs c)
        judged (:sleep-judged (ctx/mem c))
        prices (if (= inputs (:inputs judged)) (:prices judged) (await (bed-down/answers c {:log-out? (:log-out inputs)})))
        pick (bed-down/cheapest c prices)]
    (when-not (= inputs (:inputs judged))
      (ctx/emit! c :night.answers :debug {:pick pick :inputs inputs :prices (into {} (map (fn [[k v]] [k (price/round1 v)])) prices)
                                          :text (str "asleep, priced again on " (pr-str inputs) ": " (pr-str (into {} (map (fn [[k v]] [k (price/round1 v)])) prices)))}))
    (ctx/update-mem! c assoc :sleep-judged {:inputs inputs :prices prices})
    (if (= :bed pick)
      (do (when-not (= :slept (:sheltered (ctx/mem c))) (hold/sheltered! c :slept))
          (await (hold/hold! c :sleeping)))
      (let [{:keys [x y z]} (sh-sense/feet p)]
        (hold/busy! c)
        (ctx/update-mem! c dissoc :sheltered :sleep-judged)
        (ctx/emit! c :night.bed-left :info {:pos (sh-sense/feet p) :answer pick :prices (into {} (map (fn [[k v]] [k (price/round1 v)])) prices) :inputs inputs
                                            :text (str "getting up: staying in bed is priced " (price/round1 (:bed prices)) " s, " (name pick) " " (price/round1 (pick prices)) " s"
                                                       (when (pos? (:short inputs)) (str " (" (:short inputs) " more must sleep for the night to skip)")))})
        (await (ctx/act c :look #js {:pos #js {:x (+ x 0.5) :y (- y 0.5) :z (+ z 0.5)}}))
        :again))))

(defn ^:async pass
  "One choice of the night (see doc): :again, or the round's end."
  [c0]
  (let [c (assoc c0 ::pick/pass (atom {}))
        p (:primitives c)
        breached (sh/breached-home? p (ctx/view c))
        roofed (and (sh-sense/roofed? p (sh-sense/roof-height)) (not breached))
        sheltered (:sheltered (ctx/mem c))
        bed-way (ctx/way? c :bed)
        edge (delay (look-edge/pending c))]
    (when (and (sh-sense/night? p) (bed-down/overdue? c)) (note-needs-bed! c))
    (note-spawn-lost! c)
    (retract-gone-bed! c)
    (cond
      (or (and bed-way (sh/unlooked-for-bed? p (ctx/view c))) (sh/unlooked-for-roof? p (ctx/view c)))
      (do (when-let [roof (sh/unseen-roof-cell p)] (await (sense/look-at! c roof)))
          (await (look/look-around! c))
          (ctx/remember! c :night-looked {:cell (sh-sense/feet p)} sh/night-looked-policy)
          :again)
      (and (over? c) (:placed-furnace (ctx/mem c)) (not (:furnace-final (ctx/mem c))))
      (do (hold/busy! c)
          (when-not (await (hold/pick-up-furnace! c)) (ctx/update-mem! c assoc :furnace-final true))
          :again)
      (over? c) (if (= :exposed sheltered)
                                         (ending/end-exposed! c)
                                         (ctx/end c :done nil (ending/done-data c sheltered)))
      (sh-sense/sleeping? p) (await (asleep! c))
      (breath/in-water? p) (let [w (await (surface/leave-water! c))]
                             (cond (not= :out w) w
                                   (breath/in-water? p) (ending/end-in-water! c)
                                   :else :again))
      (and (not sheltered) (seq @edge)) (do (hold/busy! c) (await (look-edge/look! c @edge nil (await (look-worth c bed-way)))))
      (and roofed (or (#{:slept :dug-in} sheltered) (dig-leave/sheltered-in c)) (not (and bed-way (await (walkable-bed c)))))
      (do (when-not sheltered (hold/sheltered! c :dug-in))
          (cond
            (and bed-way (log-out-wanted? c)) (await (log-out-step c))
            :else (ctx/end c :done nil (ending/done-data c (:sheltered (ctx/mem c))))))
      :else
      (let [bed (when bed-way (await (walkable-bed c)))
            bed-wanted (and bed-way (carried-bed-wanted? c))
            door (sh/open-room-door p (ctx/view c))
            home-way? (walk-home-wanted? c)
            home (when-not roofed
                   (await (walkable-home c (not bed) (fn ^:async bound [] (if bed-wanted ##Inf (bed-down/home-bound-s c (vals (await (night-prices c bed false bed-way nil)))))))))
            offered (when (and home-way? (map? home) (or (:walkable? home) bed-wanted)) home)]
        (when (and bed-way (not bed)) (note-bed-far! c))
        (when breached
          (note-room-open! c)
          (when-not (or door (not (may-enclose? (ctx/mem c) (sh-sense/feet p))) (pit-site/failed-here? c) (not (ctx/way? c :shelter)))
            (ctx/update-mem! c assoc :enclose (sh-sense/feet p))))
        (cond
          (= :searching bed) (do (hold/busy! c) :again)
          door (await (enter-room! c nil false nil (await (room-worth c bed bed-wanted bed-way (when bed :bed)))))
          (:bed-down (ctx/mem c)) (let [{:keys [bed answer]} (:bed-down (ctx/mem c))] (await (bed-down-step c bed answer)))
          (and roofed bed-wanted (not bed) (sh-beds/bed-layout c true) (room-lib/gap c)) (await (enter-room! c nil false nil (await (room-worth c bed bed-wanted bed-way :bed))))
          (and (= :searching home) home-way?) :again
          :else (let [[pick prices] (when (or bed-way offered) (await (night-answer c bed bed-wanted bed-way offered)))]
                  (if (= :home pick)
                    (await (enter-room! c (:name home) true (:at home) (home-worth prices home)))
                    (await (answer-pass c sheltered bed (when bed-way pick))))))))))

(defn ^:async round
  "The whole night in one round (see doc): passes until one ends it or done? holds."
  [c]
  (await (pace/steps! c #(pass c))))
