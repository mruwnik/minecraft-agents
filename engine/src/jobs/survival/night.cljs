(ns jobs.survival.night
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.look :as look]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.threats :as threats]
            [jobs.lib.spent :as spent]
            [jobs.lib.util :as u]
            [jobs.survival.bed-down :as bed-down]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night.flee :as flee]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.morning :as morning]
            [jobs.survival.night.sites :as sites]))


(def doc
  "The night: one round from the trigger until the night is over and the body is out of its shelter.
  Ways (the call's :ways, all by default): :bed (a bed used or put down, a sleeper's log-out), :shelter (a roofed place
  entered, shut-in, niche, a breached room walled in, the exposed retry), :flee (a walk somewhere safer, a
  cave walled in). A door is always shut and the morning always runs.
  Condition at the start: the night trigger's (a listed night waits :not-night, :spawn with the respawn bed's state);
  sheltered with nothing for the night to do, it is done before its first round. Quote: the time left of the night (jobs.time.wait-for-light) and ::morning-s, least 0.
  The round repeats one choice, each time from the world as it is now (the first that applies), with a 50 ms timer between:
  - day: a doorway the night filled (:doorway-filled, still holding the block it put there) is dug again and the drop walked over (a cell it cannot open waits :doorway-open-failed, 2 minutes, and warns night.doorway-stuck once per cell and cause); a dug-in or exposed night leaves the shelter (dig-in-leave/leave!); :no-way-out ends it nothing-possible :no-way-out (shelter.failed warn,
    a :shelter-trapped entry, 5 minutes); then a bed it put down outside its own zones (:bed-placed) is dug up and its
    drop walked over (go-to), unless it is the respawn bed (:spawn-bed); then the job ends: done {:night :slept|:dug-in|:logged-out}, or nothing-possible :exposed {:sites} after a
    night with no shelter.
  - asleep: hold (:sleeping).
  - roofed and sheltered (slept, dug in, or shut in its own latest :shelter): a log-out stint while someone else sleeps,
    else hold (:sheltered) while the trigger would still fire (a bed to use, a sleeper), else the job ends: a body
    that is safe leaves the queue to run (the morning's trigger lets it out).
  - a bed to use or a carried bed to put down, with a roofed :roofed-places place within the night.sites ::home-radius setting (12) and the body not roofed: enters the place first (jobs.survival.enter-room {:place}: go-to, lit or not, its doors shut and doorway filled; one try, a failed walk is not repeated tonight). A carried bed is never put down in the open while such a place is close (info night.bed-kept once); the night goes on below.
  - an open door of the closed room the body stands in is shut before any bed is used or put down (below).
  - roofed, with a carried bed to put down under the roof and a doorway out of the room (jobs.lib.rooms/gap): the doorway is filled first (jobs.survival.enter-room, with jobs.blocks.fill-cells: a carried wooden door, else building blocks; never a cell an animal or mob stands in; tried once a night, :doorway-tried; the cells remembered as :doorway-filled, a day).
  - a bed-down in flight (its walk to the bed or to where the bed goes down) is called again until it ends; one that stops hostile-near makes the night hold :monsters-near (5 s) first.
  - a bed (sh-beds/bed-to-use: seen, or remembered within :bed-radius, :urgent-bed-radius once :max-days-awake days without
    sleep; never an occupied one, one given up on, or, unless :max-days-awake days without sleep or another player sleeps and the bed is within `courtesy-radius` 32, one whose walk leaves an underground, dug-in or room shelter): jobs.survival.bed-down sleeps in it (jobs.survival.sleep). A sleep that ends without sleeping writes
    :sleep-failed (5 minutes).
  - a bed item carried and no bed to use (jobs.survival.bed-down): put down beside the body (roofed cells first, else in the open, never in the open from a closed home (jobs.lib.rooms/closed-home?: every cell roofed, or one doorway shuts the roof); never in a
    doorway or another's zone), slept in, recorded as :bed unless a live :bed is; outside its own zones also as
    :bed-placed for the morning (info night.bed-placed {:pos :own-zone} says where). No room writes :bed-place-failed (10 minutes, or until the body is 6+ blocks away).
  - someone else asleep (sh-sleepers/log-out-for-sleepers?): jobs.survival.log-out, a 30 s stint at most until morning, again
    while anyone sleeps or nothing is known. A log-out that is not ok or cut is not tried again tonight.
  - a closed room around the body (roofed within :room-roof-height) with an open door (an iron one too, which no hand shuts): the door is shut (jobs.survival.enter-room: retreat-doors/shut-home-door!,
    walking to it when out of reach); a door that stays open writes :night-door-failed (shelter.door-failed warn) and the
    night goes on as below.
  - a roof over a room with a hole (a fresh :bed-open entry within sh/breach-near and no closed room, sh/breached-home?): says so once
    (shelter.room_open warn) and walls the body in under the roof (the :enclose step), not ending roofed.
  - roofed, buried or shut in a closed room: done {:night :roofed} (the queue runs).
  - by day, a dug-in body with a known hostile near (jobs.lib.dawn/hostile-hold?) stays in (info night.dawn-hold) until it is gone or the dawn grace passes.
  - else a safe place: jobs.survival.shut-in (:for :night, the ways walls and pit, and room when a remembered roofed place (:roofed-places, nearest first) lies within :walk-radius, its column not seen open (a roof cell unseen after a look counts as roof), or the body is under a roof: the room way enters that place, priced by quote against the others) here (its pit walks to other ground when the feet are a site that failed tonight, jobs.survival.dig-pit-site). A shut-in that does not
    roof the body is called again while its pit failed and another site may be left, at most max-sites calls. Then, or with no such site, it cuts a niche (jobs.survival.shut-in with the pocket way, once, :niche;
    out of a failed pit first; never a tool fetched in the dark), else flees somewhere safer, else holds exposed until day (one shelter.exposed
    warn, hold :exposed), still taking a bed or a sleeper's log-out when one turns up. A body held exposed digs in again
     when it has moved off its spot or retry-after-ms passed, at most max-retries times.
  Fleeing (:flee-radius, 0 never): to the candidate within the radius that is not tried tonight which go-to reaches soonest
  (one planner search over them, jobs.lib.targets/nearest!: known mobs and darkness costed), at most max-flees
  walks (the route need not be lit): a known bed farther than :bed-radius, a :roofed-places
  place with a seen roof, a seen cave or overhang cell (a seen solid floor and roof, two free dry cells, within about 16
  blocks: only those seen, and only while the carried blocks (dig-in's) cover every wall cell still open; none whose
  wall a zone refuses), or seen natural ground (never sand or gravel) the carried tools dig, 9+ from every failed
  site, with standing room (dig-pit-site/pit-site?). While that search goes on the night holds its pass. Arrived at a cave cell it walls itself in (shut-in with the walls way only, never a
  pit): sealed, it holds :dug-in until morning; a refusal is shut-in's failed site and the roofed body ends the night :roofed. shelter.fled info on arrival, shelter.flee_failed when not arrived; a walk cut by a
  reflex resumes the same target. Arrived: the next pass sleeps, is roofed, or digs in there (a niche is tried again).
  Done {:night how :fled n} (nothing-possible :exposed {:fled n}) when it fled.
  Every hold is declared (ctx/hold-still!) and waits hold-ms at a time, eating one carried food when the hungry
  trigger's condition holds (hungry?, or below 7 hp; not asleep); a starving body with no food runs get-food (when no danger is near).
  An overdue body (no sleep for :max-days-awake in-game days) warns needs_bed once an in-game day (:needs-bed). A respawn bed
  it sees is no bed is written :spawn-lost and warned (spawn_lost) once; its waiting reasons carry :spawn ({:bed pos} or {:lost pos :why}).
  Memory: job memory :enclose (the cave cell being walled in), :sheltered (:slept, :dug-in, :logged-out, :exposed), :log-out-failed, :no-site and :shut-ins (shut-in calls made), :digging (a shut-in call in flight), :bed-down (a bed-down in flight), :pit-trapped and leave!'s :dig-out; :roof-walk-failed (no second walk to a roofed place tonight); body memory :doorway-tried and shut-in's :shut-in-failed (failed sites, forgotten whenever the night ends, at most ten minutes), so a
  firing after a cut does not dig a failed site again.
  Muting :night does not end a running night job: the agent cancels it too.")

(a/defargs args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh-sense/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used (nil: the jobs.lib.shelter.beds/default-bed-radius setting)" :spec (a/num-in 0 nil) :default nil}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep (nil: the jobs.lib.shelter.beds/urgent-bed-radius setting)" :spec (a/num-in 0 nil) :default nil}
   :roofed-places {:doc "place names walked to at night, when close and the route is lit, before digging in" :spec (a/coll-of (a/or-of keyword? string?)) :default [:home]}
   :walk-radius {:doc "a roofed place farther than this is not walked to" :spec (a/num-in 0 nil) :default 32}
   :max-days-awake {:doc "in-game days without sleep before finding a bed becomes urgent" :spec (a/num-in 0 nil) :default sh-beds/max-days-awake}
   :wall-value {:doc "a block worth more than this each (jobs.lib.cost/item-value) is not counted as a wall block when a cave is walled in (dig-in's :wall-value)" :spec (a/num-in 0 nil) :default 3}
   :take-back? {:doc "by day, dig the blocks (above dirt) dig-in placed in the shelter again and collect them" :spec boolean? :default true}
   :flee-radius {:doc "when no shelter can be dug here, how far the night looks for somewhere safer (a known bed or roofed place, a seen cave or overhang to wall in, or seen ground the carried tools dig); 0 never flees" :spec (a/num-in 0 nil) :default 64}})

(a/defargs settings
  {::job-time-s {:default 2400 :doc "Seconds the night may take, the morning included: a bound against a runaway night (the night itself ends it in about 10 minutes)." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds the night may last from its start, its yields to other jobs included." :spec (a/num-in 0 nil)}
   ::morning-s {:default 60 :doc "Seconds the morning (out of the shelter, a put-down bed picked up) is reckoned to take at most, in the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def ways [:bed :shelter :flee])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s :wall-time ::wall-time-s} :ways (set ways)})

(def meta {:category :reflex :tags [:sleep :shelter]})

(def safe? ready/ok)

(def fit? ready/ok)

(defn quiet-night
  "While the night trigger's condition fails: {:shelter} with the shelter the body is in (nil when none), else nil."
  [c]
  (let [p (:primitives c)]
    (when-not (sh/night-holds? p (ctx/view c) (assoc (:args c) :ways (:ways (ctx/frozen-call c))) (bed-down/bed-permit c))
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

(defn done?
  "Already sheltered with nothing for the night to do (and no lost respawn bed to note): only before the start, the round ends a night it began."
  [c]
  (checks/goal (and (not (checks/running? c)) (boolean (:shelter (quiet-night c))) (not (spawn-lost-unnoted? c)))
               [:sheltered "sheltered, nothing for the night to do"] [:night "the night is not over"] {:units :nights}))

(defn quote-of
  "The time left of the night (jobs.time.wait-for-light's quote), a bed possibly ending it at once, and ::morning-s on top."
  [c]
  (let [[_ mean most] (get-in (sh-clock/night-left-quote c) [:uses :job-time])]
    {:uses {:job-time [0 mean (+ most (settings/get settings ::morning-s))]}}))

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
       (sh-sleepers/log-out-for-sleepers? (:primitives c) (ctx/view c))))

(defn ^:async log-out-step
  "Someone sleeps: a log-out stint (jobs.survival.log-out). Ok or cut: :logged-out unless already sheltered, and the
  next pass looks again. Declined or failed (unsupported, closed): :log-out-failed, not tried again by this job."
  [c]
  (hold/busy! c)
  (let [l (await (ctx/call c :log-out 'jobs.survival.log-out {}))]
    (cond
      (= :continue l) :again
      (= :done (:kind l)) (do (when-not (:sheltered (ctx/mem c)) (hold/sheltered! c :logged-out)) :again)
      :else (do (ctx/update-mem! c assoc :log-out-failed true) :again))))

(defn ^:async bed-down-step
  "Sleep in bed, or put the carried bed down (bed nil) and sleep: jobs.survival.bed-down. Asleep it is :sheltered :slept;
  the next pass chooses again. While the child goes on (:continue) job memory :bed-down holds {:bed bed}, so the next
  pass calls it again rather than choosing anew. In a closed home (jobs.lib.rooms/closed-home?) the bed goes down under the roof only (:roofed-only)."
  [c bed]
  (hold/busy! c)
  (let [d (await (ctx/call c :bed-down 'jobs.survival.bed-down
                                 (cond-> (select-keys (:args c) [:roof-height :bed-radius :urgent-bed-radius :max-days-awake])
                                   bed (assoc :bed bed)
                                   (room-lib/closed-home? (:primitives c)) (assoc :roofed-only true))))]
    (if (= :continue d)
      (ctx/update-mem! c assoc :bed-down {:bed bed})
      (ctx/update-mem! c dissoc :bed-down))
    (when (and (= :done (:kind d)) (:slept (:data d)))
      (hold/sheltered! c :slept))
    (when (= :hostile-near (:reason (:cause d)))
      (await (hold/hold! c :monsters-near)))
    :again))

;; ------------------------------------------------------------------ a safe place

(defn walk-home-wanted?
  "Whether to walk to a roofed place before using a bed: none failed tonight, no shut-in in flight, no walk failed."
  [c]
  (let [m (ctx/mem c)]
    (and (not (:roof-walk-failed m)) (not (:digging m)) (empty? (pit-site/failed c)))))

(defn ^:async enter-room!
  "jobs.survival.enter-room (into the :place if given: walks, shuts its doors, fills its doorway or roof hole); :again. A
  walk is made once a night: once its call ends, :roof-walk-failed is set whatever came of it (a body that got in is roofed, and
  no walk is wanted)."
  [c place]
  (hold/busy! c)
  (let [w (await (ctx/call c :enter-room 'jobs.survival.enter-room (cond-> {:roof-height (:roof-height (:args c))}
                                                                     place (assoc :place place))))]
    (when (and place (not= :continue w))
      (ctx/update-mem! c assoc :roof-walk-failed true))
    :again))

(defn ^:async hold-exposed!
  "Nothing shelters the body tonight: hold it anyway until day (:exposed; the first time one shelter.exposed warn).
  Each pass still looks for a bed or a sleeper first."
  [c]
  (when-not (= :exposed (:sheltered (ctx/mem c)))
    (hold/sheltered! c :exposed)
    (ctx/update-mem! c update :exposed #(or % {:pos (sh-sense/feet (:primitives c)) :at (ctx/now c)}))
    (ctx/emit! c :shelter.exposed :warn {:pos (sh-sense/feet (:primitives c)) :sites (pit-site/failed c)
                                         :text (str "cannot shelter here (no bed, nobody asleep, no site to dig in, nowhere safer known: "
                                                    (sites/exposed-why (pit-site/failed c)) "); holding until day")}))
  (await (hold/hold! c (str "exposed: " (sites/exposed-why (pit-site/failed c))))))

(defn retry-dig-in!
  "Start over from the exposed hold: give-ups forgotten, one retry counted."
  [c]
  (let [p (:primitives c)
        retries (inc (or (:retries (:exposed (ctx/mem c))) 0))]
    (ctx/update-mem! c #(-> % (dissoc :no-site :shut-ins :pit-trapped :niche :fleeing :flee-failed)
                            (assoc :exposed {:pos (sh-sense/feet p) :at (ctx/now c) :retries retries})))))

(def niche-reach
  "Blocks within which the niche's wall is looked for."
  16)

(defn ^:async niche!
  "No pit site is left: out of the failed pit first (dig-in-leave/climb!, a stair), then jobs.survival.shut-in digs a pocket
  into a hillside or wall within niche-reach; the blocks that plug it are the :shelter entry's door (written by shut-in). Whatever it ends in, it is tried once (:niche :tried); the next pass holds exposed if it
  did not shut the body in."
  [c pit]
  (hold/busy! c)
  (if pit
    (let [r (await (dig-leave/climb! c {:start pit} nil))]
      (when (map? r) (ctx/update-mem! c assoc :pit-trapped true))
      :again)
    (if-not (look/surveyed? c)
      (do (await (look/look-around! c)) :again)
      (let [d (await (ctx/call c :niche 'jobs.survival.shut-in {:ways #{:pocket} :for :night :settings {:jobs.survival.dig-pocket/walk-radius niche-reach}}))]
        (when-not (= :continue d)
          (ctx/update-mem! c assoc :niche :tried)
          (spent/absorb! c (:data d)))
        :again))))

;; ------------------------------------------------------------------ somewhere safer

(defn ^:async enclose!
  "At a cave target (:enclose is its feet cell): wall in with shut-in's walls way, one call. Sealed in: :dug-in, held until
  morning (dig-in-leave/leave! opens the door). Stopped or declined: shut-in has marked the site failed, and the
  roofed body ends the night :roofed. A body that is no longer at the cell forgets the target."
  [c]
  (let [p (:primitives c)
        pos (:enclose (ctx/mem c))
        roof-height (:roof-height (:args c))]
    (if (not= pos (sh-sense/feet p))
      (do (ctx/update-mem! c dissoc :enclose) :again)
      (let [_ (hold/busy! c)
            d (await (ctx/call c :enclose 'jobs.survival.shut-in {:ways #{:walls} :for :night :roof-height roof-height :wall-value (:wall-value (:args c))}))]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :enclose)
              (if (and (= :done (:kind d)) (dig-cells/sealed-in? p :walls roof-height))
                (hold/sheltered! c :dug-in))
              :again))))))

(defn ^:async flee-or-hold!
  "No site to dig in: flee somewhere safer (way :flee), else hold exposed."
  [c]
  (let [t (when (ctx/way? c :flee) (await (flee/flee-target c)))]
    (cond
      (= :searching t) :again
      t (await (flee/flee! c t))
      :else (await (hold-exposed! c)))))

(defn ^:async no-site!
  "Shut-in found no way left (or max-sites calls ended unsheltered): out of the failed pit and a niche once, then flee or hold exposed."
  [c]
  (cond
    (:pit-trapped (ctx/mem c)) (await (hold-exposed! c))
    (not= :tried (:niche (ctx/mem c))) (await (niche! c (pit-site/pit-start c)))
    :else (await (flee-or-hold! c))))

(def pit-roof-height
  "How far above the feet a block counts as the roof when dig-in must roof a pit under a roof that leaves the room open (an
  open door): the pit's own roof sits two above the original feet, the room's roof at three or more."
  2)

(defn dig-in-roof-height
  "The :roof-height for dig-in: the night's, but pit-roof-height at most while an open door stands in the room, so the room's
  roof does not make dig-in decline :already-sealed."
  [c]
  (let [h (:roof-height (:args c))]
    (if (seq (sh/open-room-doors (:primitives c))) (min h pit-roof-height) h)))


(defn dig-in-inputs
  "What the night read when it chose to dig in here (the event night.dig-in-why)."
  [p]
  {:pos (sh-sense/feet p)
   :roofed (sh-sense/roofed? p (sh-sense/room-roof-height))
   :buried (sh-sense/buried? p)
   :shut-in (sh-sense/shut-in-room? p)
   :open-cave (sh-sense/open-cave? p)
   :cave-cells (sh-sense/cave-cells p)
   :head-in-block (sh-sense/head-in-block? p)
   :open-doors (count (sh/open-room-doors p))
   :unseen-roof (sh/unseen-roof-cell p)})

(defn more-sites?
  "Whether another shut-in call is worth making: fewer than max-sites made, and the pit has other ground to walk to."
  [c]
  (and (< (:shut-ins (ctx/mem c) 0) (sites/max-sites)) (= :there (:at (pit-site/site c)))))

(defn ^:async shelter!
  "No bed, nobody asleep, not roofed: dig in here (jobs.survival.shut-in, one call). The call is marked :digging (a listed night
  resumes its child; dig-pit marks the site :interrupted while its pit is dug, so a firing after a cut does not dig it deeper).
  Ended unsheltered, the next pass calls again while shut-in left a site to try (more-sites?), else no-site! takes over."
  [c]
  (let [p (:primitives c)
        resume (:digging (ctx/mem c))
        _ (hold/busy! c)
        _ (when-not resume
            (ctx/emit! c :night.dig-in-why :info (dig-in-inputs p))
            (ctx/update-mem! c assoc :digging (sh-sense/feet p)))
        place (:name (first (await (sense/decide! c ::roofed-place #(seq (room-lib/roofed-places-in c (:args c) (:walk-radius (:args c)))) {:verify-guesses true}))))
        _ (when (and place (not= (sh-sense/feet p) (:route-looked (ctx/mem c))))
            (ctx/update-mem! c assoc :route-looked (sh-sense/feet p))
            (await (sense/look-round! c [])))
        d (await (ctx/call c :shut-in 'jobs.survival.shut-in
                           (cond-> {:ways (cond-> #{:walls :pit} (or place (sh-sense/roofed? p (sh-sense/room-roof-height)) (dig-cells/room-plug p (sh-sense/feet p) (dig-in-roof-height c))) (conj :room))
                                    :for :night :roof-height (dig-in-roof-height c) :wall-value (:wall-value (:args c))}
                             place (assoc :place place))))]
    (when-not (= :continue d)
      (ctx/update-mem! c #(-> % (dissoc :digging) (update :shut-ins (fnil inc 0))))
      (cond
        (not (and (= :done (:kind d)) (sh-sense/roofed? p (dig-in-roof-height c)))) (when-not (more-sites? c) (ctx/update-mem! c assoc :no-site true))
        (not (and (= :room (:way (:data d))) (empty? (:placed (:data d))))) (hold/sheltered! c :dug-in)))
    :again))


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
          escape (reach/escape-cell (:primitives c) feet)]
      (ctx/emit! c :shelter.room_open :warn (cond-> {:pos feet
                                                     :text (str "the room around the bed is not shut in (a hole in its walls" (when escape (str ", open to " (pr-str escape))) "); walling in under the roof at " (pr-str feet))}
                                              escape (assoc :escape escape))))))

(defn carried-bed-wanted? [c]
  (sh-beds/bed-place-wanted? (:primitives c) (ctx/view c) (bed-down/radius c) (bed-down/bed-permit c)))

(defn note-bed-kept!
  "Once a night, with a carried bed and a roofed home close that could not be entered: say the bed stays in the bag."
  [c]
  (when (and (sh-beds/carried-bed (:primitives c)) (not (:bed-kept-noted (ctx/mem c))))
    (ctx/update-mem! c assoc :bed-kept-noted true)
    (ctx/emit! c :night.bed-kept :info {:text "a roofed home is close but could not be entered; the carried bed is not put down in the open"})))

(defn retract-gone-bed!
  "Retract the remembered :bed place when its block is seen and no bed, before any walk to it."
  [c]
  (when-let [pos (mem/place (ctx/view c) :bed)]
    (when (sh-spawn/seen-not-bed? (:primitives c) pos)
      (places/retract-if-missing! c :bed pos "missing"))))

(defn ^:async pass
  "One choice of the night (see doc): :again, or the round's end."
  [c]
  (let [p (:primitives c)
        breached (sh/breached-home? p (ctx/view c))
        roofed (and (sh-sense/roofed? p (:roof-height (:args c))) (not breached))
        sheltered (:sheltered (ctx/mem c))
        bed-way (ctx/way? c :bed)]
    (when (and (sh-sense/night? p) (bed-down/overdue? c)) (note-needs-bed! c))
    (note-spawn-lost! c)
    (retract-gone-bed! c)
    (cond
      (or (and bed-way (sh/unlooked-for-bed? p (ctx/view c))) (sh/unlooked-for-roof? p (ctx/view c)))
      (do (when-let [roof (sh/unseen-roof-cell p)] (await (sense/look-at! c roof)))
          (await (look/look-around! c))
          (ctx/remember! c :night-looked {:cell (sh-sense/feet p)} sh/night-looked-policy)
          :again)
      (not (sh-sense/night? p)) (await (morning/morning! c))
      (sh-sense/sleeping? p) (do (when-not (= :slept sheltered) (hold/sheltered! c :slept)) (await (hold/hold! c :sleeping)))
      (and roofed (or (#{:slept :dug-in} sheltered) (dig-leave/sheltered-in c)))
      (do (when-not sheltered (hold/sheltered! c :dug-in))
          (cond
            (and bed-way (log-out-wanted? c)) (await (log-out-step c))
            (sh/night-holds? p (ctx/view c) (assoc (:args c) :ways (:ways (ctx/frozen-call c))) (bed-down/bed-permit c)) (await (hold/hold! c :sheltered))
            :else (morning/end-night! c)))
      :else
      (let [bed (when bed-way (await (sense/decide! c ::bed #(sh-beds/bed-to-use p (ctx/view c) (bed-down/radius c) (bed-down/bed-permit c)))))
            bed-wanted (and bed-way (carried-bed-wanted? c))
            door (sh/open-room-door p (ctx/view c))
            home-near (when-not roofed (first (room-lib/roofed-places-in c (:args c) (sites/home-radius))))]
        (when (and bed-way (not bed)) (note-bed-far! c))
        (when (and bed-way home-near (:roof-walk-failed (ctx/mem c)) (not bed)) (note-bed-kept! c))
        (when breached
          (note-room-open! c)
          (when-not (or door (:enclose (ctx/mem c)) (pit-site/failed-here? c) (not (ctx/way? c :shelter)))
            (ctx/update-mem! c assoc :enclose (sh-sense/feet p))))
        (cond
          door (await (enter-room! c nil))
          (:bed-down (ctx/mem c)) (await (bed-down-step c (:bed (:bed-down (ctx/mem c)))))
          (and roofed bed-wanted (not bed) (bed-down/bed-layout c true) (room-lib/gap c)) (await (enter-room! c nil))
          (and home-near (walk-home-wanted? c) (or bed bed-wanted)) (await (enter-room! c (:name home-near)))
          (or bed (and bed-wanted (not home-near))) (await (bed-down-step c bed))
          (and bed-way (log-out-wanted? c)) (await (log-out-step c))
          (and (:enclose (ctx/mem c)) (not= :dug-in sheltered)) (await (enclose! c))
          (and (or roofed (and (sh-sense/buried? p) (not (sh-sense/open-cave? p))) (sh-sense/shut-in-room? p)) (empty? (sh/open-room-doors p))
               (not (sh-sense/head-in-block? p)))
          (do (ctx/end c :done nil (morning/done-data c (if (= :logged-out sheltered) :logged-out :roofed))))
          (:fleeing (ctx/mem c)) (await (flee/flee! c (:fleeing (ctx/mem c))))
          (and (= :exposed sheltered) (ctx/way? c :shelter) (sites/retry-due? c)) (do (retry-dig-in! c) (await (shelter! c)))
          (= :exposed sheltered) (await (hold/hold! c (str "exposed: " (sites/exposed-why (pit-site/failed c)))))
          (not (ctx/way? c :shelter)) (await (flee-or-hold! c))
          (:no-site (ctx/mem c)) (await (no-site! c))
          :else (await (shelter! c)))))))

(defn ^:async round
  "The whole night in one round (see doc): passes until one ends it."
  [c]
  (loop []
    (let [r (await (pass c))]
      (if (= :again r)
        (do (await (pace/pace!)) (recur))
        r))))
