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
            [jobs.lib.look-edge :as look-edge]
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
            [jobs.survival.room-plug :as room-plug]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night.flee :as flee]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.morning :as morning]
            [jobs.survival.night.sites :as sites]))


(def doc
  "The night: one round from the trigger until the night is over and the body is out of its shelter.
  Ways (the call's :ways, all by default): :bed (a bed used or put down, a sleeper's log-out), :shelter (a roofed place
  entered, shut-in, niche, a breached room walled in, the exposed retry), :flee (a walk somewhere safer, a cave walled in).
  A door is always shut and the morning always runs.
  Condition at the start: the night trigger's (a listed night waits :not-night, :spawn with the respawn bed's state); sheltered with nothing for the night to do, it is done before its first round. Quote: the time left of the night plus ::morning-s.
  Each pass chooses again from the world as it is, the first that applies:
  - day: the morning (night.morning): a doorway the night filled is dug again, a dug-in or exposed night leaves the shelter (:no-way-out ends nothing-possible), a bed put down outside its own zones is picked up (not the respawn bed); a dug-in body stays while a known hostile is near and the dawn grace lasts (night.dawn-hold). Then done? ends it done {:night :slept|:dug-in|:logged-out|:roofed :fled n}, or the morning ends nothing-possible :exposed after a night with no shelter.
  - asleep: hold (:sleeping).
  - roofed or buried in a closed region with sides never seen: a look at the nearest, else a walk to it, 15 failed cells and 200 steps a night at most (an unseen side is open until seen: a lit tunnel with its walls seen is shelter, one with an unseen branch is not).
  - sheltered (slept, dug in, shut in): log out for a stint while someone else sleeps, else hold (:sheltered) while a bed to use or a sleeper keeps the trigger firing, else end so the queue runs.
  - a roofed :roofed-places place within the night.sites ::home-radius setting, a bed to use or carry and the body not roofed: jobs.survival.enter-room walks in (one try tonight); a carried bed is not put down in the open while one is close (night.bed-kept).
  - an open door of the closed room the body stands in is shut; the room's doorway is filled before a carried bed goes down (:doorway-tried, :doorway-filled).
  - a bed to use or carried (sh-beds/bed-to-use): jobs.survival.bed-down, called again until it ends; one that stops hostile-near makes the night hold :monsters-near.
  - someone else asleep: jobs.survival.log-out, a 30 s stint, not retried tonight when not ok.
  - a roof over a room with a hole (shelter.room_open warn): walled in under the roof (:enclose).
  - roofed, buried or shut in a closed room: done {:night :roofed}.
  - else jobs.survival.shut-in {:for :night} (walls, pit, room by quote; its pit walks to other ground after a failed site, jobs.survival.dig-pit-site; called again while another site may be left, at most max-sites), then a niche (pocket way, once), then a flee, then hold exposed (one shelter.exposed warn), still taking a bed or log-out that turns up, and digging in again after moving or retry-after-ms (max-retries).
  Fleeing (the night.sites ::flee-radius setting, 0 never; at most max-flees walks): to the not-yet-tried candidate go-to reaches soonest (jobs.lib.targets/nearest!): a known bed, a :roofed-places place, a seen cave or overhang cell (walled in on arrival), or seen natural ground the carried tools dig. shelter.fled on arrival, shelter.flee_failed otherwise.
  Every hold is declared (ctx/hold-still!), waits hold-ms, and (night.hold) cooks, eats or runs get-food first.
  An overdue body warns needs_bed once a day; a respawn bed seen gone is written :spawn-lost (warn spawn_lost once).
  Memory: job memory :edge-looked, :edge-phase, :edge-steps, :enclose, :enclose-failed (feet cells where walling in failed), :sheltered, :log-out-failed, :no-site, :shut-ins, :digging, :bed-down, :pit-trapped, :roof-walk-failed, :left; body memory :doorway-tried and shut-in's :shut-in-failed (forgotten when the night ends, at most ten minutes).
  Muting :night does not end a running night job: cancel it too.")

(a/defargs args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh-sense/default-roof-height}
   :roofed-places {:doc "place names walked to at night, when close and the route is lit, before digging in" :spec (a/coll-of (a/or-of keyword? string?)) :default [:home]}})

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

(defn morning-over?
  "A started night is over: light outside, no shelter to leave, no doorway or bed left to take up, not an exposed night
  (the morning ends that nothing-possible), no lost respawn bed to note."
  [c]
  (and (checks/running? c)
       (not (sh-sense/dark-outside? (:primitives c)))
       (not= :exposed (:sheltered (ctx/mem c)))
       (not (spawn-lost-unnoted? c))
       (not (morning/pending? c))))

(defn done?
  "The whole goal: once started, the night is over and the morning done (result {:night how :fled n}); before the start,
  already sheltered with nothing for the night to do (and no lost respawn bed to note). Asked every round."
  [c]
  (if (checks/running? c)
    (let [over (morning-over? c)
          result (when over (morning/done-data c (:sheltered (ctx/mem c))))]
      (checks/goal over [:morning (or (:text result) "the night is over, the morning done")] [:night "the night is not over"]
                   (cond-> {:units :nights} over (assoc :result result))))
    (checks/goal (and (boolean (:shelter (quiet-night c))) (not (spawn-lost-unnoted? c)))
                 [:sheltered "sheltered, nothing for the night to do"] [:night "the night is not over"] {:units :nights})))

(defn ^:async cleanup
  "However the night ends: tonight's failed sites and filled doorway cells someone else changed are forgotten."
  [c _]
  (morning/forget-night! c)
  (ctx/end c :done nil))

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
                                 (cond-> (select-keys (:args c) [:roof-height])
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
  (let [w (await (ctx/call c :enter-room 'jobs.survival.enter-room (cond-> {:settings {:jobs.survival.plug-way/roof-height (:roof-height (:args c))}}
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
                                                    (sites/exposed-why (pit-site/failed c) (:no-way-text (ctx/mem c))) "); holding until day")}))
  (await (hold/hold! c (str "exposed: " (sites/exposed-why (pit-site/failed c) (:no-way-text (ctx/mem c)))))))

(defn retry-dig-in!
  "Start over from the exposed hold: give-ups forgotten, one retry counted."
  [c]
  (let [p (:primitives c)
        retries (inc (or (:retries (:exposed (ctx/mem c))) 0))]
    (ctx/update-mem! c #(-> % (dissoc :no-site :no-way-text :shut-ins :pit-trapped :niche :fleeing :flee-failed)
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
        roof-height (:roof-height (:args c))]
    (if (not= pos (sh-sense/feet p))
      (do (ctx/update-mem! c dissoc :enclose) :again)
      (let [_ (hold/busy! c)
            d (await (ctx/call c :enclose 'jobs.survival.shut-in {:ways #{:walls} :for :night :settings {:jobs.survival.plug-way/roof-height roof-height :jobs.survival.wall-in/wall-value (sites/wall-value)}}))]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :enclose)
              (when (= :nothing-possible (:kind d)) (ctx/update-mem! c update :enclose-failed (fnil conj #{}) pos))
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
   :safe (sh-inside/safe-from-night? p)
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
        place (:name (first (await (sense/decide! c ::roofed-place #(seq (room-lib/roofed-places-in c (:args c) (sites/walk-radius))) {:verify-guesses true}))))
        _ (when (and place (not= (sh-sense/feet p) (:route-looked (ctx/mem c))))
            (ctx/update-mem! c assoc :route-looked (sh-sense/feet p))
            (await (sense/look-round! c [])))
        d (await (ctx/call c :shut-in 'jobs.survival.shut-in
                           (cond-> {:ways (cond-> #{:walls :pit} (or place (sh-sense/roofed? p (sh-sense/room-roof-height)) (room-plug/room-plug p (sh-sense/feet p) (dig-in-roof-height c))) (conj :room))
                                    :for :night :settings {:jobs.survival.plug-way/roof-height (dig-in-roof-height c) :jobs.survival.wall-in/wall-value (sites/wall-value)}}
                             place (assoc :place place))))]
    (when-not (= :continue d)
      (ctx/update-mem! c #(-> % (dissoc :digging) (update :shut-ins (fnil inc 0))))
      (cond
        (not (and (= :done (:kind d)) (sh-sense/roofed? p (dig-in-roof-height c)))) (do (ctx/update-mem! c assoc :no-way-text (:text (:data d)))
                                                                                        (when-not (more-sites? c) (ctx/update-mem! c assoc :no-site true)))
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
      (not (sh-sense/dark-outside? p)) (await (morning/morning! c))
      (sh-sense/sleeping? p) (do (when-not (= :slept sheltered) (hold/sheltered! c :slept)) (await (hold/hold! c :sleeping)))
      (and (not sheltered) (seq (look-edge/pending c))) (do (hold/busy! c) (await (look-edge/look! c (look-edge/pending c))))
      (and roofed (or (#{:slept :dug-in} sheltered) (dig-leave/sheltered-in c)))
      (do (when-not sheltered (hold/sheltered! c :dug-in))
          (cond
            (and bed-way (log-out-wanted? c)) (await (log-out-step c))
            (sh/night-holds? p (ctx/view c) (assoc (:args c) :ways (:ways (ctx/frozen-call c))) (bed-down/bed-permit c)) (await (hold/hold! c :sheltered))
            :else (ctx/end c :done nil (morning/done-data c (:sheltered (ctx/mem c))))))
      :else
      (let [bed (when bed-way (await (sense/decide! c ::bed #(sh-beds/bed-to-use p (ctx/view c) (bed-down/radius c) (bed-down/bed-permit c)))))
            bed-wanted (and bed-way (carried-bed-wanted? c))
            door (sh/open-room-door p (ctx/view c))
            home-near (when-not roofed (first (room-lib/roofed-places-in c (:args c) (sites/home-radius))))]
        (when (and bed-way (not bed)) (note-bed-far! c))
        (when (and bed-way home-near (:roof-walk-failed (ctx/mem c)) (not bed)) (note-bed-kept! c))
        (when breached
          (note-room-open! c)
          (when-not (or door (not (may-enclose? (ctx/mem c) (sh-sense/feet p))) (pit-site/failed-here? c) (not (ctx/way? c :shelter)))
            (ctx/update-mem! c assoc :enclose (sh-sense/feet p))))
        (cond
          door (await (enter-room! c nil))
          (:bed-down (ctx/mem c)) (await (bed-down-step c (:bed (:bed-down (ctx/mem c)))))
          (and roofed bed-wanted (not bed) (bed-down/bed-layout c true) (room-lib/gap c)) (await (enter-room! c nil))
          (and home-near (walk-home-wanted? c) (or bed bed-wanted)) (await (enter-room! c (:name home-near)))
          (or bed (and bed-wanted (not home-near))) (await (bed-down-step c bed))
          (and bed-way (log-out-wanted? c)) (await (log-out-step c))
          (and (:enclose (ctx/mem c)) (not= :dug-in sheltered)) (await (enclose! c))
          (and (sh-inside/safe-from-night? p) (empty? (sh/open-room-doors p)))
          (do (ctx/end c :done nil (morning/done-data c (if (= :logged-out sheltered) :logged-out :roofed))))
          (:fleeing (ctx/mem c)) (await (flee/flee! c (:fleeing (ctx/mem c))))
          (and (= :exposed sheltered) (ctx/way? c :shelter) (sites/retry-due? c)) (do (retry-dig-in! c) (await (shelter! c)))
          (= :exposed sheltered) (await (hold/hold! c (str "exposed: " (sites/exposed-why (pit-site/failed c) (:no-way-text (ctx/mem c))))))
          (not (ctx/way? c :shelter)) (await (flee-or-hold! c))
          (:no-site (ctx/mem c)) (await (no-site! c))
          :else (await (shelter! c)))))))

(defn ^:async round
  "The whole night in one round (see doc): passes until one ends it or done? holds."
  [c]
  (await (pace/steps! c #(pass c))))
