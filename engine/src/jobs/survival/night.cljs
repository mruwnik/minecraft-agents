(ns jobs.survival.night
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.child :as child]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.result :as result]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.spent :as spent]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.survival.bed-down :as bed-down]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.night.flee :as flee]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.sites :as sites]
            [jobs.survival.retreat :as retreat]))

(def doc
  "The night: one round from the trigger until the night is over and the body is out of its shelter. The round repeats
  one choice, each time from the world as it is now (the first that applies), with a 50 ms timer between:
  - day: a dug-in or exposed night leaves the shelter (dig-in-leave/leave!); :no-way-out stops the job (shelter.failed warn,
    a :shelter-trapped entry, 5 minutes); then a bed it put down outside its own zones (:bed-placed) is dug up and its
    drop walked over (go-to), unless it is the respawn bed (:spawn-bed); then the job ends: done {:night :slept|:dug-in|:logged-out}, or stopped :exposed {:sites} after a
    night with no shelter.
  - asleep: hold (:sleeping).
  - roofed and sheltered (slept, dug in, or shut in its own latest :shelter): a log-out stint while someone else sleeps,
    else hold (:sheltered) while the trigger would still fire (a bed to use, a sleeper), else the job ends: a body
    that is safe leaves the queue to run (the morning's trigger lets it out).
  - a bed to use or a carried bed to put down, with a roofed :roofed-places place within :home-radius and the body not roofed: walks into the place first (go-to :place, lit or not; one try, a failed walk is not repeated tonight). A carried bed is never put down in the open while such a place is close (info night.bed-kept once); the night goes on below.
  - an open door of the closed room the body stands in is shut before any bed is used or put down (below).
  - a bed (sh/bed-to-use: seen, or remembered within :bed-radius, :urgent-bed-radius once :max-days-awake days without
    sleep; never an occupied one, one given up on, or, unless :max-days-awake days without sleep, one whose walk leaves an underground, dug-in or room shelter): jobs.survival.bed-down sleeps in it (jobs.survival.sleep). A sleep that ends without sleeping writes
    :sleep-failed (5 minutes, shelter.sleep_failed warn).
  - a bed item carried and no bed to use (jobs.survival.bed-down): put down beside the body (roofed cells first, else in the open; never in a
    doorway or another's zone), slept in, recorded as :bed unless a live :bed is; outside its own zones also as
    :bed-placed for the morning (info night.bed-placed {:pos :own-zone} says where). No room writes :bed-place-failed (10 minutes, or until the body is 6+ blocks away).
  - someone else asleep (sh/log-out-for-sleepers?): jobs.survival.log-out, a 30 s stint at most until morning, again
    while anyone sleeps or nothing is known. A log-out that is not ok or cut is not tried again tonight.
  - a closed room around the body (roofed within :room-roof-height) with an open door: the door is shut (retreat/shut-home-door!,
    walking to it when out of reach); a door that stays open writes :night-door-failed (shelter.door-failed warn) and the
    night goes on as below.
  - a roof over a room with a hole (a fresh :bed-open entry within sh/breach-near and no closed room, sh/breached-home?): says so once
    (shelter.room_open warn) and walls the body in under the roof (the :enclose step), not ending roofed.
  - roofed, buried or shut in a closed room: done {:night :roofed} (the queue runs).
  - else, before any pit and while no site has failed tonight: a remembered roofed place (:roofed-places, nearest first)
    within :walk-radius whose roof the body has seen, and whose route (the straight line, sampled every 2 blocks) is lit
    as far as the body has seen it (a seen cell with block light >= :lit-light; an unseen cell is not lit; the line approximates go-to's route): walk there (go-to :place). A walk
    that does not arrive is not tried again tonight, and the night digs in; arrived, the next pass is roofed.
  - else a safe place: jobs.survival.dig-in here, unless a site tonight failed within 8 blocks. A dig-in that does not
    roof the body (stopped, or declined) writes a :night-site {:pos :reason} entry, and the body walks (go-to) to the
    nearest cell within 16 blocks, 9+ from every failed site, that looks dry and solid from the surface (shelter.relocated info; dig-in finds out the
    rest), to dig in there. After max-sites failed sites, or with no such cell, it cuts a niche (jobs.survival.dig-niche with :fetch false, once, :niche;
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
  Done {:night how :fled n} (stopped :exposed {:fled n}) when it fled.
  Every hold is declared (ctx/hold-still!) and waits hold-ms at a time, eating one carried food when the hungry
  trigger's condition holds (hungry?, or below 7 hp; not asleep); a starving body with no food runs get-food (when no danger is near).
  An overdue body (no sleep for :max-days-awake in-game days) warns needs_bed once an in-game day (:needs-bed). A respawn bed
  it sees is no bed is written :spawn-lost and warned (spawn_lost) once; its waiting reasons carry :spawn ({:bed pos} or {:lost pos :why}).
  Memory: job memory :enclose (the cave cell being walled in), :sheltered (:slept, :dug-in, :logged-out, :exposed), :log-out-failed, :relocating, :digging (the
  site of a dig-in in flight), :pit-trapped and leave!'s :dig-out; :roof-walk-failed (no second walk to a roofed place tonight); body memory :night-site (failed sites, forgotten whenever the night ends, at most a day), so a
  firing after a cut does not dig a failed site again.
  Muting :night does not end a running night job: the agent cancels it too.")

(a/defargs args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :spec (a/num-in 0 nil) :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :spec (a/num-in 0 nil) :default sh/urgent-bed-radius}
   :roofed-places {:doc "place names walked to at night, when close and the route is lit, before digging in" :spec (a/coll-of (a/or-of keyword? string?)) :default [:home]}
   :walk-radius {:doc "a roofed place farther than this is not walked to" :spec (a/num-in 0 nil) :default 32}
   :lit-light {:doc "block light a route cell needs to count as lit (mobs do not spawn at 1+)" :spec (a/int-in 0 15) :default 1}
   :max-days-awake {:doc "in-game days without sleep before finding a bed becomes urgent" :spec (a/num-in 0 nil) :default sh/max-days-awake}
   :wall-value {:doc "a block worth more than this each (jobs.lib.cost/item-value) is not counted as a wall block when a cave is walled in (dig-in's :wall-value)" :spec (a/num-in 0 nil) :default 3}
   :take-back? {:doc "by day, dig the blocks (above dirt) dig-in placed in the shelter again and collect them" :spec boolean? :default true}
   :home-radius {:doc "a remembered roofed place (:roofed-places) this close is walked into, whatever the light, before a bed is used or put down; a carried bed is never put down in the open while one is" :spec (a/num-in 0 nil) :default 12}
   :flee-radius {:doc "when no shelter can be dug here, how far the night looks for somewhere safer (a known bed or roofed place, a seen cave or overhang to wall in, or seen ground the carried tools dig); 0 never flees" :spec (a/num-in 0 nil) :default 64}})

(defn check
  "The night trigger's condition, or job memory :sheltered (a listed night cut before its morning)."
  [c]
  (let [spawn (sh/spawn-state (:primitives c) (ctx/view c))]
    (or (some? (:sheltered (ctx/mem c)))
        (boolean (sh/night-holds? (:primitives c) (ctx/view c) (:args c) (bed-down/bed-permit c)))
        (if-let [shelter (sh/night-sheltered (:primitives c) (:data (ctx/latest c :shelter)))]
          (ctx/wait c (cond-> {:reason :night-safe :shelter shelter} spawn (assoc :spawn spawn)))
          (ctx/wait c (cond-> {:reason :not-night} spawn (assoc :spawn spawn)))))))

(def needs-bed-policy {:cap 1 :ttl sh/ms-per-day})

(defn note-needs-bed! [c]
  (when (empty? (ctx/entries c :needs-bed))
    (ctx/remember! c :needs-bed {} needs-bed-policy)
    (ctx/emit! c :needs_bed :warn {:days (sh/days-awake c)
                                   :text "not slept for too long; phantoms will come; a bed is needed (the :needs-bed trigger fetches one by day when registered)"})))

(defn note-spawn-lost!
  "The respawn bed is seen gone: remember :spawn-lost and warn spawn_lost once (sh/lose-spawn!)."
  [c]
  (when-let [pos (:lost (sh/spawn-state (:primitives c) (ctx/view c)))]
    (when-not (= pos (:pos (:data (ctx/latest c :spawn-lost))))
      (sh/lose-spawn! c pos :seen-gone))))

(defn log-out-wanted? [c]
  (and (not (:log-out-failed (ctx/mem c)))
       (sh/log-out-for-sleepers? (:primitives c) (ctx/view c))))

(defn ^:async log-out-step
  "Someone sleeps: a log-out stint (jobs.survival.log-out). Ok or cut: :logged-out unless already sheltered, and the
  next pass looks again. Declined or failed (unsupported, closed): :log-out-failed, not tried again by this job."
  [c]
  (hold/busy! c)
  (let [l (await (ctx/call-child c :log-out 'jobs.survival.log-out {}))
        status (:status (ctx/child-result c :log-out))]
    (cond
      (= :continue l) :again
      (and (= :done l) (#{"ok" "cut"} status)) (do (when-not (:sheltered (ctx/mem c)) (hold/sheltered! c :logged-out)) :again)
      :else (do (ctx/update-mem! c assoc :log-out-failed true) :again))))

(defn ^:async bed-down-step
  "Sleep in bed, or put the carried bed down (bed nil) and sleep: jobs.survival.bed-down. Asleep it is :sheltered :slept;
  the next pass chooses again."
  [c bed]
  (hold/busy! c)
  (let [d (await (ctx/call-child c :bed-down 'jobs.survival.bed-down
                                 (cond-> (select-keys (:args c) [:roof-height :bed-radius :urgent-bed-radius :max-days-awake])
                                   bed (assoc :bed bed))))]
    (when (and (= :done d) (:slept (ctx/child-result c :bed-down)))
      (hold/sheltered! c :slept))
    :again))

;; ------------------------------------------------------------------ a safe place

(defn ^:async roof-walk!
  "Walk (go-to :place) to the roofed place; :again. A walk that ends without arriving, or arrives and is still not roofed, is marked :roof-walk-failed."
  [c {:keys [name]}]
  (hold/busy! c)
  (let [w (await (ctx/call-child c :roof-walk 'jobs.movement.go-to {:place name :range 0 :at-night :go}))]
    (when (and (not= :continue w)
               (not (and (:arrived (ctx/child-result c :roof-walk)) (sh/roofed? (:primitives c) (:roof-height (:args c))))))
      (ctx/update-mem! c assoc :roof-walk-failed true))
    :again))

(defn ^:async hold-exposed!
  "Nothing shelters the body tonight: hold it anyway until day (:exposed; the first time one shelter.exposed warn).
  Each pass still looks for a bed or a sleeper first."
  [c]
  (when-not (= :exposed (:sheltered (ctx/mem c)))
    (hold/sheltered! c :exposed)
    (ctx/update-mem! c update :exposed #(or % {:pos (sh/feet (:primitives c)) :at (ctx/now c)}))
    (ctx/emit! c :shelter.exposed :warn {:pos (sh/feet (:primitives c)) :sites (sites/failed-sites c)
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
                            (assoc :exposed {:pos (sh/feet p) :at (ctx/now c) :retries retries})))))

(defn ^:async niche!
  "No pit site is left: out of the failed pit first (dig-in-leave/climb!, a stair), then jobs.survival.dig-niche cuts a niche
  into a hillside or wall. Whatever it ends in, it is tried once (:niche :tried); the next pass holds exposed if it
  did not shut the body in."
  [c pit]
  (hold/busy! c)
  (if pit
    (let [r (await (dig-leave/climb! c {:start pit} nil))]
      (when (map? r) (ctx/update-mem! c assoc :pit-trapped true))
      :again)
    (let [d (await (ctx/call-child c :niche 'jobs.survival.dig-niche {:fetch false}))]
      (when-not (= :continue d)
        (ctx/update-mem! c assoc :niche :tried)
        (spent/absorb! c (ctx/child-result c :niche)))
      :again)))

;; ------------------------------------------------------------------ somewhere safer

(defn ^:async enclose!
  "At a cave target (:enclose is its feet cell): wall in with dig-in :enclose, one call. Sealed in: :dug-in, held until
  morning (dig-in-leave/leave! opens the door). Stopped or declined: the site is marked failed with dig-in's reason, and the
  roofed body ends the night :roofed. A body that is no longer at the cell forgets the target."
  [c]
  (let [p (:primitives c)
        pos (:enclose (ctx/mem c))
        roof-height (:roof-height (:args c))]
    (if (not= pos (sh/feet p))
      (do (ctx/update-mem! c dissoc :enclose) :again)
      (let [_ (hold/busy! c)
            d (await (ctx/call-child c :enclose 'jobs.survival.dig-in {:roof-height roof-height :enclose true :wall-value (:wall-value (:args c))}))
            res (ctx/child-result c :enclose)]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :enclose)
              (if (and (= :done d) (dig-cells/sealed-in? p :walls roof-height) (not= :stopped (:status res)))
                (hold/sheltered! c :dug-in)
                (sites/site-failed! c pos (or (:reason res) (if (= :declined d) :declined :unsealed))))
              :again))))))

(defn ^:async relocate!
  "Get to a nearby cell where a pit can be dug; the next pass digs in there. Out of its own failed pit first by a stair
  (dig-in-leave/climb!; one that finds no way out holds exposed), then a walk (go-to). A walk that does not arrive marks that
  cell failed (:unreachable). With sites/max-sites failed sites, or no such cell: hold exposed."
  [c]
  (let [site (or (:relocating (ctx/mem c))
                 (when (< (count (sites/failed-sites c)) (sites/max-sites)) (sites/relocation-site c)))
        pit (sites/pit-start c)]
    (cond
      (:pit-trapped (ctx/mem c)) (await (hold-exposed! c))
      (and (nil? site) (not= :tried (:niche (ctx/mem c)))) (await (niche! c pit))
      (nil? site) (let [t (await (flee/flee-target c))]
                    (cond
                      (= :searching t) :again
                      t (await (flee/flee! c t))
                      :else (await (hold-exposed! c))))
      pit (do (hold/busy! c)
              (ctx/update-mem! c assoc :relocating site)
              (let [r (await (dig-leave/climb! c {:start pit} site))]
                (when (map? r) (ctx/update-mem! c assoc :pit-trapped true))
                :again))
      :else
      (do (hold/busy! c)
          (ctx/update-mem! c #(-> % (assoc :relocating site) (dissoc :dig-out)))
          (let [w (await (ctx/call-child c :relocate 'jobs.movement.go-to {:pos site :range 0 :at-night :go}))]
            (if (= :continue w)
              :again
              (do (ctx/update-mem! c dissoc :relocating)
                  (if (:arrived (ctx/child-result c :relocate))
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


(defn ^:async shelter!
  "No bed, nobody asleep, not roofed: dig in here (jobs.survival.dig-in, one call), unless a site failed here tonight.
  The site is written :interrupted before the call (a firing after a cut mid-pit does not dig it deeper; a listed night
  resumes its dig-in child instead), then forgotten when the body is roofed, else marked with dig-in's stop or wait
  reason."
  [c]
  (let [resume (:digging (ctx/mem c))
        free-site (when-not (or resume (sites/failed-here? c) (not (sites/zone-refused? c (sh/feet (:primitives c)))))
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
            start (or resume (sh/feet p))
            _ (hold/busy! c)
            _ (when-not resume
                (ctx/update-mem! c assoc :digging start)
                (sites/site-failed! c start :interrupted))
            d (await (ctx/call-child c :dig-in 'jobs.survival.dig-in {:roof-height (dig-in-roof-height c) :wall-value (:wall-value (:args c))}))
            res (ctx/child-result c :dig-in)]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :digging)
              (ctx/forget-where! c :night-site #(= {:pos start :reason :interrupted} %))
              (if (and (= :done d) (sh/roofed? p (dig-in-roof-height c)) (not= :stopped (:status res)))
                (hold/sheltered! c :dug-in)
                (sites/site-failed! c start (or (:reason res) (if (= :declined d) :declined :unsealed))))
              :again))))))


(defn ^:async shut-room-door!
  "Shut an open door of the closed room the body stands in (retreat/shut-home-door!: from where it stands within a hand's
  reach, else a step to the room cell inside it first). No zone rule applies: shutting a door changes no block. A door still
  open once tried (clicked, or no step got nearer) writes :night-door-failed for this door (shelter.door-failed warn), so the
  night does not call the body safe behind it. :again."
  [c door]
  (await (retreat/shut-home-door! c door))
  (when (and (some #(= (sh/door-key (:cell door)) (sh/door-key (:cell %))) (sh/open-doors (:primitives c) (ctx/view c)))
             (contains? (:doors-clicked (ctx/mem c) #{}) (retreat/door-key (:cell door))))
    (ctx/emit! c :shelter.door-failed :warn {:cell (retreat/door-key (:cell door))
                                             :text "an open door of the room would not shut; the room is not safe"})
    (ctx/remember! c :night-door-failed {:door (sh/door-key (:cell door))} {:cap 1 :ttl (sh/door-failed-ms)}))
  :again)

;; ------------------------------------------------------------------ morning

(defn ^:async collect-bed!
  "By day: dig up the bed at pos the night put down outside the body's zones, then walk over its drop. One try: the
  :bed-placed entry is dropped whatever happens, and a :bed entry at pos is retracted."
  [c pos]
  (let [walk (await (ctx/call-child c :bed-collect 'jobs.movement.go-to {:pos pos :range 2 :at-night :go}))]
    (if (= :continue walk)
      :again
      (do (when (:arrived (ctx/child-result c :bed-collect))
            (let [r (await (tidy/dig! c pos))]
              (when (= "dug" (.-status r))
                (when (= pos (mem/place (ctx/view c) :bed))
                  (ctx/remember! c :bed {:gone true :was pos} mem/place-policy))
                (ctx/update-mem! c assoc :pickup pos))))
          (ctx/forget-where! c :bed-placed (constantly true))
          :again))))

(defn ^:async pick-up-drop!
  "Walk over the dug bed's drop at pos (go-to); one try, then the walk is over."
  [c pos]
  (let [w (await (ctx/call-child c :bed-pickup 'jobs.movement.go-to {:pos pos :range 0 :at-night :go}))]
    (when-not (= :continue w) (ctx/update-mem! c dissoc :pickup))
    :again))

(defn fled
  "Walks to somewhere safer this night made, or nil when none."
  [c]
  (let [n (:flees (ctx/mem c))]
    (when (and n (pos? n)) n)))

(defn spent-fields
  "What the night's children placed (the niche), as the :spent, :spent-at and :text a done result carries; {} when nothing."
  [c]
  (let [log (spent/log c)]
    (if log (assoc (spent/summary log) :text (spent/text log "night shelter")) {})))

(defn end-night!
  "The night is over: forget tonight's failed sites and end, done {:night how}, or stopped :exposed {:sites} when
  nothing sheltered the body."
  [c]
  (let [how (:sheltered (ctx/mem c))
        sites (sites/failed-sites c)]
    (ctx/forget-where! c :night-site (constantly true))
    (if (= :exposed how)
      (result/stop! c :exposed (str "no shelter tonight: no bed, nobody asleep, no site to dig in, nowhere safer known (" (sites/exposed-why sites) ")")
                    :sites sites :fled (fled c))
      (result/finish! c (cond-> (spent-fields c) how (assoc :night how) (fled c) (assoc :fled (fled c)))))))

(defn ^:async morning-step!
  "By day, nothing to walk over: out of a dig-in pit first, then a bed picked up, then the end."
  [c]
  (if (or (#{:dug-in :exposed} (:sheltered (ctx/mem c))) (:dig-out (ctx/mem c)) (dig-leave/sheltered-in c))
    (let [r (await (dig-leave/leave! c {:take-back? (:take-back? (:args c))}))]
      (cond
        (= :continue r) :again
        (= :out (:reason r)) (do (ctx/update-mem! c dissoc :dig-out)
                                 (when-not (= :exposed (:sheltered (ctx/mem c))) (hold/sheltered! c :dug-in))
                                 (if (sh/bed-to-collect (:primitives c) (ctx/view c)) :again (end-night! c)))
        (= :no-way-out (:reason r))
        (let [text "no way out of the shelter by day; giving the body back"]
          (ctx/emit! c :shelter.failed :warn {:reason :no-way-out :at (:at r) :tries (:tries r) :text text})
          (ctx/remember! c :shelter-trapped {:pos (sh/feet (:primitives c))} sh/trapped-policy)
          (ctx/forget-where! c :night-site (constantly true))
          (result/stop! c :no-way-out text :at (:at r) :tries (:tries r)))
        :else (await (hold/hold! c :leaving))))
    (if-let [bed (sh/bed-to-collect (:primitives c) (ctx/view c))]
      (await (collect-bed! c bed))
      (end-night! c))))

(defn ^:async morning!
  "By day: a dug bed's drop is walked over first, else morning-step!."
  [c]
  (hold/busy! c)
  (if-let [pos (:pickup (ctx/mem c))]
    (await (pick-up-drop! c pos))
    (morning-step! c)))

(defn note-bed-far!
  "Once a night, with no bed to use: say a remembered :bed or :home is too far, so the agent can carry a bed on long trips."
  [c]
  (when-not (:bed-far-noted (ctx/mem c))
    (let [view (ctx/view c)
          here (u/self-pos c)]
      (when-let [pos (some #(when (> (u/dist here %) (bed-down/radius c)) %) (keep #(mem/place view %) [:bed :home]))]
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
    (ctx/emit! c :shelter.room_open :warn {:pos (sh/feet (:primitives c))
                                           :text (str "the room around the bed is not shut in (a hole in its walls); walling in under the roof at " (pr-str (sh/feet (:primitives c))))})))

(defn carried-bed-wanted? [c]
  (sh/bed-place-wanted? (:primitives c) (ctx/view c) (bed-down/radius c) (bed-down/bed-permit c)))

(defn note-bed-kept!
  "Once a night, with a carried bed and a roofed home close that could not be entered: say the bed stays in the bag."
  [c]
  (when (and (sh/carried-bed (:primitives c)) (not (:bed-kept-noted (ctx/mem c))))
    (ctx/update-mem! c assoc :bed-kept-noted true)
    (ctx/emit! c :night.bed-kept :info {:text "a roofed home is close but could not be entered; the carried bed is not put down in the open"})))

(defn ^:async pass
  "One choice of the night (see doc): :again, or the round's end."
  [c]
  (let [p (:primitives c)
        breached (sh/breached-home? p (ctx/view c))
        roofed (and (sh/roofed? p (:roof-height (:args c))) (not breached))
        sheltered (:sheltered (ctx/mem c))]
    (when (and (sh/night? p) (bed-down/overdue? c)) (note-needs-bed! c))
    (note-spawn-lost! c)
    (cond
      (not (sh/night? p)) (await (morning! c))
      (sh/sleeping? p) (do (when-not (= :slept sheltered) (hold/sheltered! c :slept)) (await (hold/hold! c :sleeping)))
      (and roofed (or (#{:slept :dug-in} sheltered) (dig-leave/sheltered-in c)))
      (do (when-not sheltered (hold/sheltered! c :dug-in))
          (cond
            (log-out-wanted? c) (await (log-out-step c))
            (sh/night-holds? p (ctx/view c) (:args c) (bed-down/bed-permit c)) (await (hold/hold! c :sheltered))
            :else (end-night! c)))
      :else
      (let [bed (await (sense/decide! c ::bed #(sh/bed-to-use p (ctx/view c) (bed-down/radius c) (bed-down/bed-permit c))))
            door (sh/open-room-door p (ctx/view c))
            home-near (when-not roofed (sites/near-roofed-home c))]
        (when-not bed (note-bed-far! c))
        (when (and home-near (:roof-walk-failed (ctx/mem c)) (not bed)) (note-bed-kept! c))
        (when breached
          (note-room-open! c)
          (when-not (or door (:enclose (ctx/mem c)) (sites/failed-here? c))
            (ctx/update-mem! c assoc :enclose (sh/feet p))))
        (cond
          door (await (shut-room-door! c door))
          (and home-near (sites/roof-walk-wanted? c) (or bed (carried-bed-wanted? c))) (await (roof-walk! c home-near))
          (or bed (and (carried-bed-wanted? c) (not home-near))) (await (bed-down-step c bed))
          (log-out-wanted? c) (await (log-out-step c))
          (and (:enclose (ctx/mem c)) (not= :dug-in sheltered)) (await (enclose! c))
          (and (or roofed (and (sh/buried? p) (not (sh/open-cave? p))) (sh/shut-in-room? p)) (empty? (sh/open-room-doors p))) (do (ctx/forget-where! c :night-site (constantly true))
                                                                 (result/finish! c (cond-> (assoc (spent-fields c) :night (if (= :logged-out sheltered) :logged-out :roofed)) (fled c) (assoc :fled (fled c)))))
          (:fleeing (ctx/mem c)) (await (flee/flee! c (:fleeing (ctx/mem c))))
          (and (= :exposed sheltered) (sites/retry-due? c)) (do (retry-dig-in! c) (await (shelter! c)))
          (= :exposed sheltered) (await (hold/hold! c (str "exposed: " (sites/exposed-why (sites/failed-sites c)))))
          :else (if-let [place (and (sites/roof-walk-wanted? c) (await (sense/decide! c ::roofed-place #(sites/roofed-place c))))]
                  (await (roof-walk! c place))
                  (await (shelter! c))))))))

(defn ^:async round
  "The whole night in one round (see doc): passes until one ends it."
  [c]
  (loop []
    (let [r (await (pass c))]
      (if (= :again r)
        (do (await (child/pace!)) (recur))
        r))))
