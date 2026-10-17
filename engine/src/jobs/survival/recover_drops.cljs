(ns jobs.survival.recover-drops
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter :as sh]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.deaths :as dth]
            [jobs.survival.recover-drops.estimate :as est]
            [jobs.survival.recover-drops.kit :as kt]
            [jobs.survival.recover-drops.pile :as pl]
            [jobs.survival.recover-drops.config :as cfg]))

(def doc
  "After a death, go back for the drops when they are worth it.
  Serves one :died entry no :recovered covers (its :death-t): the one a trip is keyed to, else the pile inside its window with the best value net of the walk, so an older open pile is not lost to a newer death. A death whose window has closed while a newer one is open is closed :abandoned :superseded without a trip. Once every death is decided (a skip too) the job waits (condition :no-drops) naming the decision. It starts while a death is unrecovered, however old,
  so an expired trip still gets to write :abandoned.
  One run is the whole trip: it holds still (declared :respawning, then :settling) until a :respawned entry newer than
  the death exists and its 2 s have passed (no decision, no walk), decides, walks, collects and writes :recovered.
  Every run that leaves the death open emits recover-drops.declined with a :reason (:danger with :mob and :mob-pos, :unreachable) and a text.
  Ready: fit? ok; safe? is ok by day, ok at night when the pile despawns before morning (weighed by its route danger alone), else the exposure of the pile's cell (ready/exposed: later at night in the open or out of a shelter, so the died trigger is held with the reason and the pile is fetched by day while it lasts). The job's children walk under :checks {:safe? (always)}: the trip prices its own danger.
  The trip owns the danger it priced (the died trigger ranks above hostile-near, so no flight cuts the run it fires; an agent's run any reflex cuts):
  a real danger within :danger-radius has the trip weighed again with it near, once per mob, and so does a hit (:hurt) since the decision, at once: it backs off or goes on; before a decision
  the run ends condition-failed :danger so a reflex can deal with it (the died trigger fires it again); nothing-possible :unreachable when go-to does not arrive; the despawn window ends it :abandoned.
  A real danger is a mob that can reach the body, or a ranged one with a line of fire (jobs.lib.danger/nearest-danger).
  Decision: skips when the value of what was carried is at most the fetch cost plus :margin.
  - Value: jobs.lib.cost/item-value (with :value-overrides), plus 5 per level of experience.
  - Cost: a trip of 10, 0.3 per block of straight distance, and 10 per point of route danger (jobs.lib.cost/route-danger)
    past the hostiles the body knows of (seen or heard, jobs.lib.danger/known-hostiles; :danger-overrides, after armour),
    many times dearer for a mob the body (its best weapon, armour, health) cannot afford to fight.
    The killer named in the :died cause is assumed at the pile for ::killer-guard-ms after the death only, and so is
    the killer of any other death that young where it killed. The pile itself ends the route, however far below the surface.
  - A pile in water (its cell or the one above) or lost to drowning adds :wet-danger points of route danger (named water or drowning).
  - Infinite when lava, fire or the void took the pile, or the walk would end after the despawn.
  A skip for danger alone (the pile would be fetched were the route safe) is not final: the run ends condition-failed :danger and the death stays open. The danger is remembered (:recover-danger) for ::danger-memory-ms (an assumed killer alone: while its guard lasts), at most until the walk back from the flight's end (go-to's quote, the farthest priced mob's follow range past it at walking speed, plus ::collect-s) would end at the despawn, the hostile reflex taking the body away meanwhile; no such time left: :abandoned :danger. The died trigger rests and the job waits :danger-remembered for that death meanwhile, cutting no other job; then the pile is weighed anew, until the window closes. :retry weighs it now. A skip for value is final.
  A fetch emits recover-drops.fetching. A skip emits recover-drops.decided. Both texts give the value, the cost and their parts.
  Kit: before the decision, when the pile is worth the trip were the route safe, a body with no weapon or with an empty armour slot, whose killer or the hostiles it knows make the route
  dangerous, fetches the best weapon and armour piece a chest of known stock (:fetch/stock) holds (jobs.lib.need, chests only,
  :fetch false: off) and wears the armour (jobs.items.wear); nothing known to be in a chest: unchanged.
  A kit fetch waiting on the world is waited for (declared hold :fetching-kit); a fetch that ended the job drops the kit (warn recover-drops.kit-dropped).
  Fetch: walks to the death point (jobs.movement.go-to, in legs of :leg-s seconds; after each leg the value and cost are weighed again from where the body stands, at its health, a pile gone dear with danger backing off as above), then collects (passes, up to 3) the pile's items it can see within :collect-radius
  (jobs.forestry.collect-drops; only the carried item names, never items out of sight). Drops that went out of sight
  (down a shaft) get one walk to the cell the first was last seen in and a look again before the trip ends :partial, and so do drops never in sight at the pile: once, a walk to four cells 3 blocks round it.
  The pile out of reach (go-to :goal-enclosed, or no item in sight at it) with blocks this body placed within ::seal-radius of it (the scaffold ledger, a flee
  seal): once per trip they are taken back (jobs.access.cleanup, event recover-drops.seal) and the walk goes on.
  Ends by writing a :recovered entry {:decision :skip|:collected|:partial|:abandoned}.
  :collected counts the pile's items that entered the inventory since the decision, those picked up on the walk too.
  :abandoned with :reason :unreachable (the walk was blocked, or did not arrive or got no nearer :max-blocked-retries times; a pile too far to walk after such walks ends so, not :skip), :danger
  (the trip was cut by a hostile, or declined for a danger, :max-hostile-cuts times), :nothing-found or :window-closed
  (the five minute despawn window closed). :partial carries :left and :reason :unreachable or :not-visible.
  Endings (data :decision :text :pos :items :left): :collected is done; a skip for value condition-failed :not-worth; :partial nothing-possible :unreachable or :not-visible; :abandoned by its reason, :window-closed limit-reached (line :time), :danger and :nothing-found condition-failed, :unreachable nothing-possible; a danger or a walk that did not arrive leave the death open (see above). The abandoned text names the pile and what to do.
  A pile already carried again ends :collected without a collect step.
  Restart-safe: the decision, the baseline of carried items and the phase are kept in a :recover-trip entry keyed to the death.
  A run cut by a higher reflex, or a restarted body, goes on with the same trip.
  Warns respawn_away_from_bed once per death when the body respawned over 10 blocks from its respawn bed (the live :spawn-bed, else the :bed it slept in).
  Memory: reads :died, :respawned, :bed, :slept, :recover-trip, :recover-danger, the scaffold ledger. Writes :recovered, :recover-trip and :recover-danger.")

(a/defargs args
  {:margin {:doc "added to the fetch cost before comparing it to the value" :spec (a/num-in nil nil) :default 0}
   :value-overrides {:doc "jobs.lib.cost/item-value overrides, a map: item name or group (ore tool armor food block unknown) -> worth of one item, or {:times n}; e.g. {\"raw_iron\" 500}" :spec (a/map-of string? (a/or-of number? map?)) :default {}}
   :danger-overrides {:doc "jobs.lib.cost/route-danger overrides, a map: mob name -> threat in points of damage before armour, or {:times n}; e.g. {\"creeper\" 100 \"zombie\" 0}" :spec (a/map-of string? (a/or-of number? map?)) :default {}}
   :danger-radius {:doc "a hostile this close makes the job yield without acting" :spec (a/num-in 0 nil) :default 8}
   :fetch {:doc "get a weapon and armour from a chest whose stock the body knows before walking to a pile its killer guards (jobs.lib.need limits); false: no kit fetch" :spec need/option? :default {:what #{:item} :how #{:chest} :minutes 3}}
   :leg-s {:doc "the walk to the pile goes in legs of this many seconds; after each the danger and the cost are weighed again" :spec (a/num-in 0.1 120) :default 6}
   :retry {:doc "try a pile again that an earlier run ended :abandoned or declined for a danger still remembered, while its despawn window is open (a reflex run keeps the decision; an agent asks for another try)" :spec boolean? :default false}
   :collect-radius {:doc "collect the pile's drops within this many blocks of the death point (a pile on open ground rolls 6-8 out)" :spec (a/num-in 0 nil) :default 10}})

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds of the call's time limit: the despawn window, which the job keeps itself in game ticks." :spec (a/num-in 0 nil)}
   ::collect-s {:default 10 :doc "Seconds a collect pass at the pile is reckoned to take, in the job's quote." :spec (a/num-in 0 nil)}})

(def target [])

(def own-checks
  "The :checks of the job's children: the trip prices its own danger, and safe? has judged the night for the pile."
  {:safe? '(always)})

(defn pile-exposure
  "Whether the walk to the pile of the next open death is exposed: ok by day and when the pile despawns before morning
  (weighed by its route danger alone), else the exposure of the pile's cell."
  [c]
  (let [d (first (dth/open-deaths c))
        night-ms (sh/ms-until-morning (.-timeOfDay (.self (:primitives c))))]
    (if (or (nil? d) (zero? night-ms) (>= night-ms (settings/ticks->ms (game/despawn-left-ticks d (ctx/view c)))))
      (contract/ok :ready "no night to wait out for the pile")
      (ready/exposed c (:pos (:data d))))))

(def safe? (with-meta pile-exposure {::ready/exposure pile-exposure}))

(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn can-run?
  "A death open to serve, else :later with the danger remembered for one, else the decision already taken."
  [c]
  (if (seq (dth/open-deaths c))
    (contract/ok :open-death "a death to serve")
    (checks/cause-status :later (or (dth/danger-wait (ctx/view c)) (est/decided-wait (ctx/view c))))))

(defn done?
  "How much of the latest pile is back since the decision. Never done at once: the :recovered entry is the trip's to write."
  [c]
  (let [pile (:inventory (:data (dth/current-death c)))
        total (reduce + 0 (map :count pile))
        n (if (:baseline (ctx/mem c)) (pl/recovered-items c pile) 0)]
    (contract/later :recovering (str n " of " total " items back") :units :items :done n :left (- total n))))

(defn ^:async trip-s
  "[least mean] seconds of the walk to pos as go-to quotes it, the mean with ::collect-s at the pile."
  [c pos]
  (let [[least mean] (or (when pos (get-in (await (ctx/quote c 'jobs.movement.go-to {:pos pos :range (cfg/arrive-range)})) [:uses :job-time])) [0 0])]
    [least (+ mean (settings/get settings ::collect-s))]))

(defn window-ms
  "Milliseconds left before the drops of death entry despawn."
  [c entry]
  (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))))

(defn ^:async quote-of
  "The walk to the pile as go-to quotes it, then ::cfg/collect-s at the pile; at most the despawn window left."
  [c]
  (let [d (first (dth/open-deaths c))
        window-s (if d (/ (window-ms c d) 1000) 0)
        [least mean] (await (trip-s c (:pos (:data d))))]
    {:uses {:job-time [least mean (max mean window-s)]}}))

(def endings
  "The kind and cause of a trip's end by its decision (and for an abandoned pile its reason); a pile not worth the trip is a fact about the goal."
  {:collected [:done nil]
   :skip [:condition-failed :not-worth]
   [:abandoned :window-closed] [:limit-reached {:line :job-time :reason :window-closed}]
   [:abandoned :danger] [:condition-failed :danger]
   [:abandoned :nothing-found] [:condition-failed :nothing-found]})

(defn ending-for
  "The ending of a trip that wrote decision with fields: collected is done, a skip condition-failed :not-worth, an abandoned
  pile by its reason (:window-closed limit-reached, :danger and :nothing-found condition-failed), the rest nothing-possible
  with the reason (:unreachable, or for a partial :unreachable / :not-visible)."
  [c decision fields text pos]
  (let [[kind cause] (or (endings decision) (endings [decision (:reason fields)]) [:nothing-possible (:reason fields)])]
    (ctx/end c kind cause (assoc (select-keys fields [:items :left :detail]) :decision decision :text text :pos pos))))

(defn finish!
  "Write the :recovered entry, report the decision and end the job with its ending."
  [c decision fields]
  (let [death (dth/current-death c)
        pos (:pos (:data death))
        text (cond-> (est/decision-text decision fields (:margin (:args c)))
               (= :abandoned decision) (str "; the pile at " (str/join " " (map #(js/Math.floor (% pos)) [:x :y :z]))))]
    (ctx/remember! c :recovered (merge {:decision decision :death-t (:t death)} (dissoc fields :at :flight-s)) cfg/recovered-policy)
    (ctx/emit! c :recover-drops.decided (if (= :abandoned decision) :warn :info)
               (assoc (select-keys fields [:value :cost :reason :items :left :parts :top-items :top-mobs])
                      :decision decision :pos pos :text text))
    (ending-for c decision fields text pos)))

(defn decline!
  "Say why this run ends with the death still open (an event with the reason and a text), and end it: condition-failed
  for a danger or the night, nothing-possible for a walk that did not arrive."
  [c reason text fields]
  (let [text (str "recover-drops declined: " text)]
    (ctx/emit! c :recover-drops.declined :info (assoc fields :reason reason :text text))
    (ctx/end c (if (= :unreachable reason) :nothing-possible :condition-failed) reason (assoc fields :text text))))

(defn own-seal-near
  "The open scaffold ledger entries (jobs.lib.ledger) within seal-radius of pos: blocks this body placed round the pile."
  [c pos]
  (filterv #(<= (u/dist pos (zipmap [:x :y :z] (:cell %))) (cfg/seal-radius)) (ledger/open-entries (ctx/view c))))

(defn ^:async clear-seal!
  "When the body's own placed blocks stand near the pile (its flee seal): take back those within seal-radius of it
  (jobs.access.cleanup :near) and walk again, after a walk to within ::seal-sight of the nearest of them (the cleanup offers only blocks the body has seen). Tried twice a trip at most; :seal-cleared is set once a run finished.
  :next when it ran, else nil."
  [c pos]
  (when-let [seal (and (not (:seal-cleared (ctx/mem c))) (< (:seal-tries (ctx/mem c) 0) 2) (seq (own-seal-near c pos)))]
    (ctx/update-mem! c #(-> % (update :seal-tries (fnil inc 0)) (assoc :phase :go)))
    (dth/save-trip! c)
    (ctx/emit! c :recover-drops.seal :info {:pos pos :text "the pile is out of reach behind blocks this body placed: taking them back"})
    (let [[x y z] (:cell (apply min-key #(u/dist (u/self-pos c) (zipmap [:x :y :z] (:cell %))) seal))]
      (await (child/run!* c :seal-walk 'jobs.movement.go-to {:pos {:x x :y y :z z} :range (cfg/seal-sight) :ways #{:walk} :checks own-checks})))
    (when (= :done (:kind (await (child/run!* c :seal 'jobs.access.cleanup {:job :all :near {:pos pos :radius (cfg/seal-radius)} :checks own-checks}))))
      (ctx/update-mem! c assoc :seal-cleared true))
    :next))

(declare threatened? meet-danger!)

(defn ^:async walk-leg!
  "One leg of the walk to pos: [:danger d] when a danger the trip has not priced is near (no walk is made), else [r] of
  a go-to child with :leg-s seconds (:continue, or its ending; its :data has the result, with :arrived or :leg)."
  [c pos]
  (if-let [d (threatened? c)]
    [:danger d]
    [(await (child/call-walk* c :go {:pos pos :range (cfg/arrive-range) :checks own-checks :spend {:job-time (:leg-s (:args c))}}))]))

(defn ^:async go!
  "Walk one leg (:leg-s) toward the death point: :next once there (the collect phase) or after a leg (the danger and the
  cost are weighed again; one that got no nearer counts as a walk that did not arrive), else :declined (go-to waits on the
  world, or it did not arrive: the next run walks again while the pile is worth it, and the window closes it). A walk
  go-to refused for the body's own shelter (:inside-own-shelter) is no failed try: the window closes the trip."
  [c pos]
  (let [before (u/dist (u/self-pos c) pos)
        [r danger] (await (walk-leg! c pos))
        walked (:data r)]
    (cond
      (= :danger r) (meet-danger! c danger)
      (or (= :continue r) (and (not= :done (:kind r)) (nil? walked)))
      (decline! c :unreachable (str "the walk to " (pr-str pos) " is waiting on the world") {:pos pos})
      (:inside-own-shelter walked)
      (decline! c :unreachable (str "the walk to " (pr-str pos) " is refused: the body is sealed in its own shelter, which opens by day") {:pos pos})
      (and (= :goal-enclosed (:why walked)) (await (clear-seal! c pos))) :next
      (:leg walked)
      (let [nearer? (< (u/dist (u/self-pos c) pos) (- before 1))]
        (ctx/update-mem! c #(cond-> (assoc % :recheck true)
                              (not nearer?) (-> (assoc :blocked true) (update :blocked-retries (fnil inc 0)))))
        (when-not nearer? (dth/save-trip! c))
        :next)
      (not (:arrived walked))
      (do (ctx/update-mem! c #(-> % (assoc :blocked true) (update :blocked-retries (fnil inc 0))))
          (dth/save-trip! c)
          (decline! c :unreachable (str "the walk to " (pr-str pos) " did not arrive") {:pos pos}))
      :else (do (ctx/update-mem! c assoc :phase :collect :blocked false) (dth/save-trip! c) :next))))

(defn finish-collect!
  "End the collect phase: :collected when all of the pile is back, else :partial naming what is left and why."
  [c pile ids]
  (let [items (pl/recovered-items c pile)
        left (pl/left-over c pile)
        decided (:decided (ctx/mem c))]
    (cond
      (empty? left) (finish! c :collected (assoc decided :items items))
      (zero? items) (finish! c :abandoned (assoc decided :items 0 :reason :nothing-found))
      :else (finish! c :partial (assoc decided :items items :left left :reason (if (seq ids) :unreachable :not-visible))))))

(defn ^:async look-round!
  "Drops of the pile went out of sight (down a shaft): once, walk to the cell the first was last seen in (go-to, range 1)
  and look again. :next with a pass counted when drops are in sight now, else the end of the trip (what is left, with its
  reason). Not an arrival ends the trip :partial too."
  [c [x y z] pos names radius pile]
  (ctx/update-mem! c assoc :looked true)
  (await (child/run!* c :look 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 1 :ways #{:walk :clear-seal} :checks own-checks}))
  (let [again (pl/pile-ids c pos names radius)]
    (if (seq again)
      (do (ctx/update-mem! c update :passes (fnil inc 0)) (dth/save-trip! c) :next)
      (finish-collect! c pile again))))

(defn ^:async search-round!
  "Once: items of the pile are nowhere in sight at the pile (behind a corner, in a hollow). Walk to cells around it
  (go-to, range 1, 3 blocks out) until some are in sight. True when some are."
  [c [x y z] pos names radius]
  (ctx/update-mem! c assoc :searched true)
  (loop [cells (map-indexed (fn [i [dx dz]] [i {:x (+ x dx) :y y :z (+ z dz)}]) [[3 0] [0 3] [-3 0] [0 -3]])]
    (when-let [[i cell] (first cells)]
      (await (child/run!* c (keyword (str "search" i)) 'jobs.movement.go-to {:pos cell :range 1 :ways #{:walk :clear-seal} :checks own-checks}))
      (or (seq (pl/pile-ids c pos names radius)) (recur (rest cells))))))

(defn ^:async collect!
  "One collect pass over the pile's visible items: :next after a pass that left some in sight (walk back, at most
  max-collect-passes), else the job's end."
  [c pos radius pile]
  (let [names (vec (distinct (map :name pile)))
        total (reduce + 0 (map :count pile))]
    (if (>= (pl/recovered-items c pile) total)
      (finish-collect! c pile [])
      (let [ids (pl/pile-ids c pos names radius)]
        (if (empty? ids)
          (or (await (clear-seal! c pos))
              (when-not (:searched (ctx/mem c))
                (when (await (search-round! c (mapv pos [:x :y :z]) pos names radius))
                  :next))
              (finish-collect! c pile ids))
          (let [swept (await (child/run!* c :collect 'jobs.forestry.collect-drops
                                          {:radius (+ radius (cfg/stray-range) 4) :ids ids :filter names :checks own-checks}))
                cell (pl/look-cell (:data swept))]
            (if (empty? (pl/left-over c pile))
              (finish-collect! c pile [])
              (let [again (pl/pile-ids c pos names radius)
                    passes (:passes (ctx/mem c) 0)]
                (cond
                  (and (seq again) (< passes (cfg/max-collect-passes)))
                  (do (ctx/update-mem! c assoc :passes (inc passes) :phase :go) (dth/save-trip! c) :next)

                  (and (empty? again) (not (:looked (ctx/mem c))) cell)
                  (await (look-round! c cell pos names radius pile))

                  (and (empty? again) (not (:searched (ctx/mem c))) (await (search-round! c (mapv pos [:x :y :z]) pos names radius)))
                  :next

                  :else (finish-collect! c pile again))))))))))

(defn threatened?
  "The nearest real danger within :danger-radius the trip has not been weighed with near (:weighed), or nil."
  [c]
  (danger-q/nearest-danger (:primitives c) (:danger-radius (:args c)) {} {:skip (:weighed (ctx/mem c))}))

(defn hit-since?
  "Whether the body was hurt since the decision was taken."
  [c decided]
  (boolean (seq (ctx/since c :hurt (inc (:at decided 0))))))

(defn danger-fields
  "{:mob :mob-pos} of the danger m: its place when seen, else the rough spot its band gives (jobs.lib.danger/mob-pos)."
  [p m]
  {:mob (.-name m) :mob-pos (danger-q/mob-pos p m)})

(defn decline-danger!
  "End condition-failed :danger for the danger m: its name and place are in the event."
  [c m]
  (let [{:keys [mob mob-pos] :as fields} (danger-fields (:primitives c) m)]
    (ctx/update-mem! c assoc :hostile-cut true)
    (dth/save-trip! c)
    (decline! c :danger (str mob " at " (pr-str mob-pos) " within " (:danger-radius (:args c)) " blocks") fields)))

(defn meet-danger!
  "A danger m the trip has not been weighed with near: :next with the trip to be weighed again with it, once per mob;
  else (no decision yet, or a mob with no id) the decline."
  [c m]
  (if (and (:decided (ctx/mem c)) (some? (.-id m)))
    (do (ctx/update-mem! c #(-> % (assoc :recheck true) (update :weighed (fnil conj #{}) (.-id m)))) :next)
    (decline-danger! c m)))

(defn ^:async wait-while!
  "Hold still (reason) while (pending?) is true: nil when it clears, else the ending of a danger within :danger-radius,
  :continue for a cut, :closed when the despawn window passes."
  [c entry reason pending?]
  (loop []
    (cond
      (not (pending?)) (do (ctx/hold-still! c nil) nil)
      (not (ctx/alive? c)) :continue
      (dth/window-closed? c entry) (do (ctx/hold-still! c nil) :closed)
      (threatened? c) (do (ctx/hold-still! c nil) (decline-danger! c (threatened? c)))
      :else (do (await (child/pause! c (cfg/pause-s) reason)) (await (pace/pace!)) (recur)))))

(defn decide!
  "The decision of this trip: the saved one, else the estimate, save and report it. A resumed trip estimates again once (from
  where the body stands, with the hostiles it knows now), and so does a walk after each leg; its baseline stays."
  [c entry]
  (if (and (:decided (ctx/mem c)) (not (:recheck (ctx/mem c))) (or (zero? (:cuts (ctx/mem c) 0)) (:reestimated (ctx/mem c))))
    (:decided (ctx/mem c))
    (let [margin (:margin (:args c))
          e (-> (est/estimate c (:data entry) (settings/ticks->ms (game/ticks-since entry (ctx/view c))))
                (update :cost est/finite)
                (assoc :at (ctx/now c)))
          rechecked? (:recheck (ctx/mem c))
          fetch? (> (:value e) (+ (if (= :infinite (:cost e)) js/Infinity (:cost e)) margin))]
      (ctx/update-mem! c #(cond-> (assoc % :decided e :reestimated true :recheck false) (nil? (:baseline %)) (assoc :baseline (storage/carried-totals (u/inventory (:primitives c))))))
      (dth/save-trip! c)
      (when (and fetch? (not rechecked?))
        (ctx/emit! c :recover-drops.fetching :info
                   (assoc (select-keys e [:value :cost :parts :top-items :top-mobs])
                          :pos (:pos (:data entry)) :text (est/decision-text :fetch e margin))))
      e)))

(defn ^:async wear-kit!
  "Put on what the kit fetch brought (jobs.items.wear), once its child has ended."
  [c]
  (await (child/run!* c :wear 'jobs.items.wear {})))

(defn kit-minutes
  "The kit fetch's limit: the fetch option's :minutes, at most the minutes the pile's despawn window has left."
  [c entry limits]
  (let [left (/ (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))) 60000)]
    (max 0.1 (min left (or (:minutes limits) left)))))

(defn ^:async kit!
  "Once per run: fetch and wear a weapon and armour when the pile is worth the trip, the danger calls for them and a known
  chest has them (jobs.lib.need, one piece at a time). A fetch child waiting on the world is waited for (declared hold
  :fetching-kit) until it ends, a danger or the despawn window. nil when done, dropped (a fetch that failed: warn) or
  nothing to do; else the ending of a danger, or :continue for a cut."
  [c entry]
  (let [limits (need/opts c 'jobs.survival.recover-drops)]
    (if (or (:kit-done (ctx/mem c)) (not limits) (nil? (kt/kit-problem c)) (not (kt/kit-called-for? c entry)))
      (do (ctx/update-mem! c assoc :kit-done true) nil)
      (loop []
        (let [wanted (kt/kit-problem c)
              r (if wanted (await (need/blocks! c (:any-of wanted) 1 (assoc (select-keys limits [:how :explore]) :minutes (kit-minutes c entry limits)))) :done)]
          (cond
            (nil? wanted)
            (do (await (wear-kit! c))
                (ctx/update-mem! c assoc :kit-done true)
                nil)

            (and (not= :continue r) (not= :done (:kind r)))
            (do (ctx/update-mem! c assoc :kit-done true)
                (ctx/emit! c :recover-drops.kit-dropped :warn {:text "recover-drops: the kit fetch failed; going without a kit"})
                nil)

            (not= :continue r) (recur)
            (not (ctx/alive? c)) :continue
            (dth/window-closed? c entry) (do (ctx/hold-still! c nil) nil)
            (threatened? c) (do (ctx/hold-still! c nil) (decline-danger! c (threatened? c)))
            :else (do (await (child/pause! c (cfg/pause-s) :fetching-kit)) (await (pace/pace!)) (recur))))))))

(defn retreat!
  "Remember the danger for hold-ms, report the skip and end condition-failed :danger (see retry-later!)."
  [c entry decided hold-ms mobs]
  (ctx/update-mem! c dissoc :decided)
  (dth/save-trip! c)
  (ctx/remember! c :recover-danger {:death-t (:t entry) :until (+ (ctx/now c) hold-ms) :text mobs} cfg/danger-policy)
  (ctx/emit! c :recover-drops.decided :info
             (assoc (select-keys decided [:value :cost :parts :top-items :top-mobs])
                    :decision :skip :retry true :pos (:pos (:data entry)) :hold-s (js/Math.round (/ hold-ms 1000))
                    :text (str (est/decision-text :skip decided (:margin (:args c))) "; looks again in " (js/Math.round (/ hold-ms 1000)) " s")))
  (ctx/end c :condition-failed :danger {:top-mobs (:top-mobs decided) :hold-s (js/Math.round (/ hold-ms 1000)) :pos (:pos (:data entry))}))

(defn ^:async retry-later!
  "A skip for danger backs off while the trip can still come back: report it, forget the estimate, remember the danger
  (:recover-danger, for ::danger-memory-ms; an assumed killer alone only while its guard lasts; at most until the walk back
  from the flight's end (est/flight-s) and the collect, as trip-s quotes them, would end at the despawn) and end condition-failed :danger: the hostile reflex
  takes the body away. The died trigger and the check rest for this death meanwhile, then the danger is weighed anew.
  No time left to come back: the pile is abandoned :danger."
  [c entry decided]
  (let [room (- (window-ms c entry) (* 1000 (+ (second (await (trip-s c (:pos (:data entry))))) (:flight-s decided 0))))
        hold-ms (min room (:hold-ms decided (cfg/danger-memory-ms)))
        mobs (str/join ", " (map :name (:top-mobs decided)))]
    (if (pos? room)
      (retreat! c entry decided hold-ms mobs)
      (finish! c :abandoned (assoc decided :reason :danger :detail (str mobs " guard it and no walk back would end before it despawns"))))))

(defn ^:async dig-down!
  "A stair down to the pile (jobs.access.tunnel, kept): :next once it ended there, else the trip ends :abandoned. Not when
  the window left cannot hold it (about ::stair-step-s per block): go-to has no way down and would only stand above."
  [c entry pos {:keys [below aside]}]
  (let [need-s (* (cfg/stair-step-s) (max below aside))
        left-s (/ (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))) 1000)
        give-up! (fn [reason text]
                   (ctx/emit! c :recover-drops.deep :info {:reason reason :below below :aside aside :text text})
                   (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason reason :detail text)))]
    (if (> need-s left-s)
      (give-up! :window-closed (str "the pile is " below " blocks down: a stair takes about " (js/Math.round need-s) " s, " (js/Math.round left-s) " s are left"))
      (let [args {:target [(js/Math.floor (:x pos)) (js/Math.floor (:y pos)) (js/Math.floor (:z pos))] :max-length 96 :keep true :by-hand false :max-steps 1 :checks own-checks}]
        (loop [n 0]
          (let [danger (threatened? c)
                r (when-not danger (await (ctx/call c :dig 'jobs.access.tunnel args)))]
            (cond
              danger (meet-danger! c danger)
              (= :done (:kind r)) :next
              (not (ctx/alive? c)) :continue
              (and (= :continue r) (< n (child/default-max-calls))) (do (await (pace/pace!)) (recur (inc n)))
              (= :continue r) (decline! c :unreachable "the stair down to the pile is waiting on the world" {:pos pos})
              :else (give-up! :unreachable (str "no stair down to the pile: " (pr-str (contract/cause-key r))
                                                    (when (get-in r [:cause :fetch]) " (no pickaxe could be got)"))))))))))

(defn ^:async trip!
  "The decision, then the walk and the collect passes of a trip inside its window."
  [c entry]
  (let [{:keys [margin collect-radius]} (:args c)
        kit (await (kit! c entry))
        decided (when-not kit (decide! c entry))
        pos (:pos (:data entry))]
    (cond
      kit kit

      (<= (:value decided) (+ (if (= :infinite (:cost decided)) js/Infinity (:cost decided)) margin))
      (if (est/danger-only? decided margin)
        (await (retry-later! c entry decided))
        (if (and (= :too-far (:reason decided)) (pos? (:blocked-retries (ctx/mem c) 0)))
          (finish! c :abandoned (assoc decided :reason :unreachable :detail (est/unreachable-detail c pos)))
          (finish! c :skip decided)))

      (>= (:cuts (ctx/mem c) 0) (cfg/max-hostile-cuts))
      (finish! c :abandoned (assoc decided :reason :danger))

      (>= (:blocked-retries (ctx/mem c) 0) (cfg/max-blocked-retries))
      (finish! c :abandoned (assoc decided :reason :unreachable :detail (est/unreachable-detail c pos)))

      (hit-since? c decided) (do (ctx/update-mem! c assoc :recheck true) :next)

      (and (= :collect (:phase (ctx/mem c))) (threatened? c)) (meet-danger! c (threatened? c))

      (and (= :collect (:phase (ctx/mem c))) (> (u/dist (u/self-pos c) pos) (cfg/stray-range)))
      (do (ctx/update-mem! c assoc :phase :go) :next)

      (= :collect (:phase (ctx/mem c))) (await (collect! c pos collect-radius (:inventory (:data entry))))
      (and (est/deep-pile c pos) (:blocked (ctx/mem c))) (await (dig-down! c entry pos (est/deep-pile c pos)))
      :else (let [r (await (go! c pos))]
              (if (and (= :unreachable (:cause r)) (:blocked (ctx/mem c)) (est/deep-pile c pos)) :next r)))))

(defn ^:async step!
  "One step of the trip: :next to go on, or the round's end (an ending, or :continue for a cut)."
  [c entry]
  (cond
    (not (ctx/alive? c)) :continue
    (dth/window-closed? c entry)
    (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason (if (:blocked (ctx/mem c)) :unreachable :window-closed)))
    :else (await (trip! c entry))))

(defn warn-away-from-bed!
  "Once per death: warn when the body respawned far from its respawn bed: the live :spawn-bed (set by a click or a
  sleep), else the known :bed it slept in (the respawn point was not set there). Nothing for a bed it never slept in."
  [c entry]
  (let [spawn-bed (:pos (:data (sh/spawn-bed-live (ctx/view c))))
        bed (or spawn-bed
                (let [b (mem/place (ctx/view c) :bed)]
                  (when (some #(= b (:pos (:data %))) (ctx/entries c :slept)) b)))
        at (:pos (:data (ctx/latest c :respawned)))]
    (when (and bed at (not (:bed-checked (ctx/mem c))))
      (ctx/update-mem! c assoc :bed-checked true)
      (when (> (u/dist bed at) (cfg/bed-away-blocks))
        (ctx/emit! c :respawn_away_from_bed :warn
                   {:bed bed :pos at :text "respawned away from its bed: the respawn point was not set there"})))))

(defn ^:async round
  "One whole run: wait for the respawn and its settling (declared holds), decide, walk, collect, write :recovered, and end
  with the ending of the trip (see the doc). A danger within :danger-radius ends it condition-failed :danger (the hostile
  reflex acts, the trigger fires again), a walk that did not arrive nothing-possible :unreachable, the death staying open."
  [c]
  (doseq [d (dth/stale-deaths c)]
    (ctx/remember! c :recovered {:decision :abandoned :death-t (:t d) :reason :superseded} cfg/recovered-policy))
  (let [entry (dth/current-death c)]
    (when entry (dth/key-to-death! c entry))
    (if (nil? entry)
      (ctx/end c :condition-failed :no-drops)
      (let [waited (or (await (wait-while! c entry :respawning #(not (body/respawned-since? (ctx/view c) entry))))
                       (when (nil? (:decided (ctx/mem c)))
                         (await (wait-while! c entry :settling #(dth/settling? c entry)))))]
        (if (and waited (not= :closed waited))
          waited
          (do (warn-away-from-bed! c entry)
              (loop []
                (let [entry (dth/current-death c)
                      r (if entry (await (step! c entry)) (ctx/end c :condition-failed :no-drops))]
                  (if (= :next r) (recur) r)))))))))
