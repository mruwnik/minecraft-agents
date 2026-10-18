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
            [jobs.survival.retreat-doors :as retreat-doors]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.look :as look]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
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
            [jobs.survival.night.flee :as flee]
            [jobs.survival.night.doorway :as doorway]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.morning :as morning]
            [jobs.survival.night.sites :as sites]))


(def doc
  "The night: one round from the trigger until the night is over and the body is out of its shelter.
  Ways (the call's :ways, all by default): :bed (a bed used or put down, a sleeper's log-out), :shelter (a roofed place
  walked to, dig-in, relocation, niche, a breached room walled in, the exposed retry), :flee (a walk somewhere safer, a
  cave walled in). A door is always shut and the morning always runs.
  Condition at the start: the night trigger's (a listed night waits :not-night, :spawn with the respawn bed's state);
  sheltered with nothing for the night to do, it is done before its first round. Quote: the time left of the night (jobs.time.wait-for-day) and ::morning-s, least 0.
  The round repeats one choice, each time from the world as it is now (the first that applies), with a 50 ms timer between:
  - day: a doorway the night filled (:doorway-filled, still holding the block it put there) is dug again and the drop walked over (a cell it cannot open waits :doorway-open-failed, 2 minutes, and warns night.doorway-stuck once per cell and cause); a dug-in or exposed night leaves the shelter (dig-in-leave/leave!); :no-way-out ends it nothing-possible :no-way-out (shelter.failed warn,
    a :shelter-trapped entry, 5 minutes); then a bed it put down outside its own zones (:bed-placed) is dug up and its
    drop walked over (go-to), unless it is the respawn bed (:spawn-bed); then the job ends: done {:night :slept|:dug-in|:logged-out}, or nothing-possible :exposed {:sites} after a
    night with no shelter.
  - asleep: hold (:sleeping).
  - roofed and sheltered (slept, dug in, or shut in its own latest :shelter): a log-out stint while someone else sleeps,
    else hold (:sheltered) while the trigger would still fire (a bed to use, a sleeper), else the job ends: a body
    that is safe leaves the queue to run (the morning's trigger lets it out).
  - a bed to use or a carried bed to put down, with a roofed :roofed-places place within the night.sites ::home-radius setting (12) and the body not roofed: walks into the place first (go-to :place, lit or not; one try, a failed walk is not repeated tonight). A carried bed is never put down in the open while such a place is close (info night.bed-kept once); the night goes on below.
  - an open door of the closed room the body stands in is shut before any bed is used or put down (below).
  - roofed, with a carried bed to put down under the roof and a doorway out of the room (night.doorway/gap): the doorway is filled first (jobs.blocks.fill-cells: a carried wooden door, else building blocks; never a cell an animal or mob stands in; tried once a night; the cells remembered as :doorway-filled, a day).
  - a bed-down in flight (its walk to the bed or to where the bed goes down) is called again until it ends; one that stops hostile-near makes the night hold :monsters-near (5 s) first.
  - a bed (sh-beds/bed-to-use: seen, or remembered within :bed-radius, :urgent-bed-radius once :max-days-awake days without
    sleep; never an occupied one, one given up on, or, unless :max-days-awake days without sleep, one whose walk leaves an underground, dug-in or room shelter): jobs.survival.bed-down sleeps in it (jobs.survival.sleep). A sleep that ends without sleeping writes
    :sleep-failed (5 minutes).
  - a bed item carried and no bed to use (jobs.survival.bed-down): put down beside the body (roofed cells first, else in the open, never in the open from a closed home (night.doorway/closed-home?: every cell roofed, or one doorway shuts the roof); never in a
    doorway or another's zone), slept in, recorded as :bed unless a live :bed is; outside its own zones also as
    :bed-placed for the morning (info night.bed-placed {:pos :own-zone} says where). No room writes :bed-place-failed (10 minutes, or until the body is 6+ blocks away).
  - someone else asleep (sh-sleepers/log-out-for-sleepers?): jobs.survival.log-out, a 30 s stint at most until morning, again
    while anyone sleeps or nothing is known. A log-out that is not ok or cut is not tried again tonight.
  - a closed room around the body (roofed within :room-roof-height) with an open door (an iron one too, which no hand shuts): the door is shut (retreat-doors/shut-home-door!,
    walking to it when out of reach); a door that stays open writes :night-door-failed (shelter.door-failed warn) and the
    night goes on as below.
  - a roof over a room with a hole (a fresh :bed-open entry within sh/breach-near and no closed room, sh/breached-home?): says so once
    (shelter.room_open warn) and walls the body in under the roof (the :enclose step), not ending roofed.
  - roofed, buried or shut in a closed room: done {:night :roofed} (the queue runs).
  - by day, a dug-in body with a known hostile near (jobs.lib.dawn/hostile-hold?) stays in (info night.dawn-hold) until it is gone or the dawn grace passes.
  - else, before any pit and while no site has failed tonight: a remembered roofed place (:roofed-places, nearest first)
    within :walk-radius whose column is not seen open (a roof cell unseen after a look counts as roof), and whose route (go-to's quote, dark cells priced in) costs at most
    sites/dig-in-seconds (8) seconds (quoted again after a look round): walk there (go-to :place). A walk
    that does not arrive is not tried again tonight, and the night digs in; arrived, the next pass is roofed.
  - else a safe place: jobs.survival.dig-in here, unless a site tonight failed within 8 blocks. A dig-in that does not
    roof the body (any other ending) writes a :night-site {:pos :reason} entry, and the body walks (go-to) to the
    nearest cell within 16 blocks, 9+ from every failed site, that looks dry and solid from the surface (shelter.relocated info; dig-in finds out the
    rest), to dig in there. After max-sites failed sites, or with no such cell, it cuts a niche (jobs.survival.shut-in with the pocket way, once, :niche;
    out of a failed pit first; never a tool fetched in the dark), else flees somewhere safer, else holds exposed until day (one shelter.exposed
    warn, hold :exposed), still taking a bed or a sleeper's log-out when one turns up. A body held exposed digs in again (failed sites
    forgotten) when it has moved off its spot or retry-after-ms passed, at most max-retries times.
  Fleeing (:flee-radius, 0 never): to the candidate within the radius that is not tried tonight which go-to reaches soonest
  (one planner search over them, jobs.lib.targets/nearest!: known mobs and darkness costed), at most max-flees
  walks (the route need not be lit): a known bed farther than :bed-radius, a :roofed-places
  place with a seen roof, a seen cave or overhang cell (a seen solid floor and roof, two free dry cells, within about 16
  blocks: only those seen, and only while the carried blocks (dig-in's) cover every wall cell still open; none whose
  wall a zone refuses), or seen natural ground (never sand or gravel) the carried tools dig, 9+ from every failed
  site, with standing room (pit-site?). While that search goes on the night holds its pass. Arrived at a cave cell it walls itself in (dig-in :enclose, walls only, never a
  pit): sealed, it holds :dug-in until morning; a refusal marks the site failed and the roofed body ends the night :roofed. shelter.fled info on arrival, shelter.flee_failed when not arrived; a walk cut by a
  reflex resumes the same target. Arrived: the next pass sleeps, is roofed, or digs in there (a niche is tried again).
  Done {:night how :fled n} (nothing-possible :exposed {:fled n}) when it fled.
  Every hold is declared (ctx/hold-still!) and waits hold-ms at a time, eating one carried food when the hungry
  trigger's condition holds (hungry?, or below 7 hp; not asleep); a starving body with no food runs get-food (when no danger is near).
  An overdue body (no sleep for :max-days-awake in-game days) warns needs_bed once an in-game day (:needs-bed). A respawn bed
  it sees is no bed is written :spawn-lost and warned (spawn_lost) once; its waiting reasons carry :spawn ({:bed pos} or {:lost pos :why}).
  Memory: job memory :enclose (the cave cell being walled in), :sheltered (:slept, :dug-in, :logged-out, :exposed), :log-out-failed, :relocating, :digging (the
  site of a dig-in in flight), :bed-down (a bed-down in flight), :doorway-tried, :pit-trapped and leave!'s :dig-out; :roof-walk-failed (no second walk to a roofed place tonight); body memory :night-site (failed sites, forgotten whenever the night ends, with dig-in's :dig-in-futile when it ends by day or dug in, at most a day), so a
  firing after a cut does not dig a failed site again.
  Muting :night does not end a running night job: the agent cancels it too.")

(a/defargs args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh-sense/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :spec (a/num-in 0 nil) :default sh-beds/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :spec (a/num-in 0 nil) :default sh-beds/urgent-bed-radius}
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
  "The time left of the night (jobs.time.wait-for-day's quote), a bed possibly ending it at once, and ::morning-s on top."
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
  pass calls it again rather than choosing anew. In a closed home (night.doorway/closed-home?) the bed goes down under the roof only (:roofed-only)."
  [c bed]
  (hold/busy! c)
  (let [d (await (ctx/call c :bed-down 'jobs.survival.bed-down
                                 (cond-> (select-keys (:args c) [:roof-height :bed-radius :urgent-bed-radius :max-days-awake])
                                   bed (assoc :bed bed)
                                   (doorway/closed-home? (:primitives c)) (assoc :roofed-only true))))]
    (if (= :continue d)
      (ctx/update-mem! c assoc :bed-down {:bed bed})
      (ctx/update-mem! c dissoc :bed-down))
    (when (and (= :done (:kind d)) (:slept (:data d)))
      (hold/sheltered! c :slept))
    (when (= :hostile-near (:reason (:cause d)))
      (await (hold/hold! c :monsters-near)))
    :again))

;; ------------------------------------------------------------------ a safe place

(defn ^:async roof-walk!
  "Walk (go-to :place) to the roofed place; :again. A walk that ends without arriving, or arrives and is still not roofed (or its room is breached), is marked :roof-walk-failed."
  [c {:keys [name]}]
  (hold/busy! c)
  (let [w (await (ctx/call c :roof-walk 'jobs.movement.go-to {:place name :range 0}))]
    (when (and (not= :continue w)
               (not (and (:arrived (:data w)) (sh-sense/roofed? (:primitives c) (:roof-height (:args c)))
                         (not (sh/breached-home? (:primitives c) (ctx/view c))))))
      (ctx/update-mem! c assoc :roof-walk-failed true))
    :again))

(defn ^:async hold-exposed!
  "Nothing shelters the body tonight: hold it anyway until day (:exposed; the first time one shelter.exposed warn).
  Each pass still looks for a bed or a sleeper first."
  [c]
  (when-not (= :exposed (:sheltered (ctx/mem c)))
    (hold/sheltered! c :exposed)
    (ctx/update-mem! c update :exposed #(or % {:pos (sh-sense/feet (:primitives c)) :at (ctx/now c)}))
    (ctx/emit! c :shelter.exposed :warn {:pos (sh-sense/feet (:primitives c)) :sites (sites/failed-sites c)
                                         :text (str "cannot shelter here (no bed, nobody asleep, no site to dig in, nowhere safer known: "
                                                    (sites/exposed-why (sites/failed-sites c)) "); holding until day")}))
  (await (hold/hold! c (str "exposed: " (sites/exposed-why (sites/failed-sites c))))))

(defn retry-dig-in!
  "Start over from the exposed hold: failed sites and give-ups forgotten, one retry counted."
  [c]
  (let [p (:primitives c)
        retries (inc (or (:retries (:exposed (ctx/mem c))) 0))]
    (ctx/forget-where! c :night-site (constantly true))
    (ctx/update-mem! c #(-> % (dissoc :relocating :pit-trapped :niche :fleeing :flee-failed)
                            (assoc :exposed {:pos (sh-sense/feet p) :at (ctx/now c) :retries retries})))))

(def niche-reach
  "Blocks within which the niche's wall is looked for."
  16)

(defn ^:async niche!
  "No pit site is left: out of the failed pit first (dig-in-leave/climb!, a stair), then jobs.survival.shut-in digs a pocket
  into a hillside or wall within niche-reach; the blocks that plug it are the :shelter entry's door. Whatever it ends in, it is tried once (:niche :tried); the next pass holds exposed if it
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
          (spent/absorb! c (:data d))
          (when (= :done (:kind d))
            (ctx/remember! c :shelter {:pos (sh-sense/feet (:primitives c)) :state :built
                                       :door (mapv (fn [{[x y z] :cell}] {:x x :y y :z z}) (:placed (:data d)))}
                           sh/shelter-policy)))
        :again))))

;; ------------------------------------------------------------------ somewhere safer

(defn site-reason
  "Why a dig-in ending d left the site unsheltered: its cause, :unsealed when it ended done."
  [d]
  (if (= :done (:kind d)) :unsealed (contract/cause-key d)))

(defn ^:async enclose!
  "At a cave target (:enclose is its feet cell): wall in with dig-in :enclose, one call. Sealed in: :dug-in, held until
  morning (dig-in-leave/leave! opens the door). Stopped or declined: the site is marked failed with dig-in's reason, and the
  roofed body ends the night :roofed. A body that is no longer at the cell forgets the target."
  [c]
  (let [p (:primitives c)
        pos (:enclose (ctx/mem c))
        roof-height (:roof-height (:args c))]
    (if (not= pos (sh-sense/feet p))
      (do (ctx/update-mem! c dissoc :enclose) :again)
      (let [_ (hold/busy! c)
            d (await (ctx/call c :enclose 'jobs.survival.dig-in {:roof-height roof-height :enclose true :wall-value (:wall-value (:args c))}))]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :enclose)
              (if (and (= :done (:kind d)) (dig-cells/sealed-in? p :walls roof-height))
                (hold/sheltered! c :dug-in)
                (sites/site-failed! c pos (site-reason d)))
              :again))))))

(defn ^:async flee-or-hold!
  "No site to dig in: flee somewhere safer (way :flee), else hold exposed."
  [c]
  (let [t (when (ctx/way? c :flee) (await (flee/flee-target c)))]
    (cond
      (= :searching t) :again
      t (await (flee/flee! c t))
      :else (await (hold-exposed! c)))))

(defn ^:async relocate!
  "Get to a nearby cell where a pit can be dug; the next pass digs in there. Out of its own failed pit first by a stair
  (dig-in-leave/climb!; one that finds no way out holds exposed), then a walk (go-to). A walk that does not arrive marks that
  cell failed (:unreachable). A chosen site a known mob the body would flee has come by is dropped for another. With sites/max-sites failed sites, or no such cell: hold exposed."
  [c]
  (let [site (or (let [s (:relocating (ctx/mem c))] (when-not (and s ((threats/danger-near-fn c) s)) s))
                 (when (< (count (sites/failed-sites c)) (sites/max-sites)) (sites/relocation-site c)))
        pit (sites/pit-start c)]
    (cond
      (:pit-trapped (ctx/mem c)) (await (hold-exposed! c))
      (and (nil? site) (not= :tried (:niche (ctx/mem c)))) (await (niche! c pit))
      (nil? site) (await (flee-or-hold! c))
      pit (do (hold/busy! c)
              (ctx/update-mem! c assoc :relocating site)
              (let [r (await (dig-leave/climb! c {:start pit} site))]
                (when (map? r) (ctx/update-mem! c assoc :pit-trapped true))
                :again))
      :else
      (do (hold/busy! c)
          (ctx/update-mem! c #(-> % (assoc :relocating site) (dissoc :dig-out)))
          (let [w (await (ctx/call c :relocate 'jobs.movement.go-to {:pos site :range 0}))]
            (if (= :continue w)
              :again
              (do (ctx/update-mem! c dissoc :relocating)
                  (if (:arrived (:data w))
                    (ctx/emit! c :shelter.relocated :info {:pos site :sites (sites/failed-sites c)
                                                           :text "the last site failed; moved to dig in elsewhere"})
                    (sites/site-failed! c site :unreachable))
                  :again)))))))

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

(defn ^:async shelter!
  "No bed, nobody asleep, not roofed: dig in here (jobs.survival.dig-in, one call), unless a site failed here tonight.
  The site is written :interrupted before the call (a firing after a cut mid-pit does not dig it deeper; a listed night
  resumes its dig-in child instead), then forgotten when the body is roofed, else marked with dig-in's stop or wait
  reason."
  [c]
  (let [resume (:digging (ctx/mem c))
        free-site (when-not (or resume (sites/failed-here? c) (not (sites/zone-refused? c (sh-sense/feet (:primitives c)))))
                    (or (:relocating (ctx/mem c))
                            (when (< (count (sites/failed-sites c)) (sites/max-sites)) (sites/relocation-site c {:free? true}))))]
    (cond
      (and (sites/failed-here? c) (not resume))
      (await (relocate! c))

      free-site
      (do (ctx/update-mem! c assoc :relocating free-site)
          (await (relocate! c)))

      :else
      (let [p (:primitives c)
            start (or resume (sh-sense/feet p))
            _ (hold/busy! c)
            _ (when-not resume
                (ctx/emit! c :night.dig-in-why :info (dig-in-inputs p))
                (ctx/update-mem! c assoc :digging start)
                (sites/site-failed! c start :interrupted))
            d (await (ctx/call c :dig-in 'jobs.survival.dig-in {:roof-height (dig-in-roof-height c) :wall-value (:wall-value (:args c))}))]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :digging)
              (ctx/forget-where! c :night-site #(= {:pos start :reason :interrupted} %))
              (if (and (= :done (:kind d)) (sh-sense/roofed? p (dig-in-roof-height c)))
                (hold/sheltered! c :dug-in)
                (sites/site-failed! c start (site-reason d)))
              :again))))))


(defn ^:async shut-room-door!
  "Shut an open door of the closed room the body stands in (retreat-doors/shut-home-door!: from where it stands within a hand's
  reach, else a step to the room cell inside it first). No zone rule applies: shutting a door changes no block. A door still
  open once tried (clicked, or no step got nearer) writes :night-door-failed for this door (shelter.door-failed warn), so the
  night does not call the body safe behind it. :again."
  [c door]
  (await (retreat-doors/shut-home-door! c door))
  (when (and (some #(= (sh/door-key (:cell door)) (sh/door-key (:cell %))) (sh/open-doors (:primitives c) (ctx/view c)))
             (contains? (:doors-clicked (ctx/mem c) #{}) (retreat-doors/door-key (:cell door))))
    (ctx/emit! c :shelter.door-failed :warn {:cell (retreat-doors/door-key (:cell door))
                                             :text "an open door of the room would not shut; the room is not safe"})
    (ctx/remember! c :night-door-failed {:door (sh/door-key (:cell door))} {:cap 1 :ttl (sh/door-failed-ms)}))
  :again)

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
            home-near (when-not roofed (sites/near-roofed-home c))]
        (when (and bed-way (not bed)) (note-bed-far! c))
        (when (and bed-way home-near (:roof-walk-failed (ctx/mem c)) (not bed)) (note-bed-kept! c))
        (when breached
          (note-room-open! c)
          (when-not (or door (:enclose (ctx/mem c)) (sites/failed-here? c) (not (ctx/way? c :shelter)))
            (ctx/update-mem! c assoc :enclose (sh-sense/feet p))))
        (cond
          door (await (shut-room-door! c door))
          (:bed-down (ctx/mem c)) (await (bed-down-step c (:bed (:bed-down (ctx/mem c)))))
          (and roofed bed-wanted (not bed) (bed-down/bed-layout c true) (doorway/gap c)) (await (doorway/shut! c (doorway/gap c)))
          (and home-near (sites/roof-walk-wanted? c) (or bed bed-wanted)) (await (roof-walk! c home-near))
          (or bed (and bed-wanted (not home-near))) (await (bed-down-step c bed))
          (and bed-way (log-out-wanted? c)) (await (log-out-step c))
          (and (:enclose (ctx/mem c)) (not= :dug-in sheltered)) (await (enclose! c))
          (and (or roofed (and (sh-sense/buried? p) (not (sh-sense/open-cave? p))) (sh-sense/shut-in-room? p)) (empty? (sh/open-room-doors p))
               (not (sh-sense/head-in-block? p)))
          (do (ctx/forget-where! c :night-site (constantly true))
              (ctx/end c :done nil (morning/done-data c (if (= :logged-out sheltered) :logged-out :roofed))))
          (:fleeing (ctx/mem c)) (await (flee/flee! c (:fleeing (ctx/mem c))))
          (and (= :exposed sheltered) (ctx/way? c :shelter) (sites/retry-due? c)) (do (retry-dig-in! c) (await (shelter! c)))
          (= :exposed sheltered) (await (hold/hold! c (str "exposed: " (sites/exposed-why (sites/failed-sites c)))))
          (not (ctx/way? c :shelter)) (await (flee-or-hold! c))
          :else (if-let [place (and (sites/roof-walk-wanted? c)
                                    (await (sites/roofed-place! c (await (sense/decide! c ::roofed-place #(seq (sites/roofed-places-in c (:walk-radius (:args c)))) {:verify-guesses true})))))]
                  (await (roof-walk! c place))
                  (await (shelter! c))))))))

(defn ^:async round
  "The whole night in one round (see doc): passes until one ends it."
  [c]
  (loop []
    (let [r (await (pass c))]
      (if (= :again r)
        (do (await (pace/pace!)) (recur))
        r))))
