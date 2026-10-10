(ns jobs.survival.recover-drops
  (:require [engine.args :as a]
            [jobs.lib.shelter :as sh]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.combat :as combat]
            [jobs.lib.armour :as armour]
            [jobs.lib.cost :as cost]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [jobs.lib.body :as body]
            [engine.game :as game]
            [engine.settings :as settings]
            [engine.memory :as mem]))

(defn death-summary
  "What status reports of a :died memory entry at view {:now :age}: {:pos :cause? :ago-ms :despawns-in-ms :recovered?}
  while the drops can still be there (under the despawn window, 6000 game ticks counted in world age), else nil. :recovered is the decision
  of a :recovered entry newer than the death (:collected, :partial, :skip, :abandoned), when there is one."
  ([entry view] (death-summary entry view nil))
  ([entry view recovered]
   (when entry
     (let [since (game/ticks-since entry view)
           {:keys [pos cause]} (:data entry)
           decision (when (and recovered (> (:t recovered) (:t entry))) (:decision (:data recovered)))]
       (when (< since game/despawn-ticks)
         (cond-> {:pos pos :ago-ms (settings/ticks->ms since) :despawns-in-ms (settings/ticks->ms (- game/despawn-ticks since))}
           cause (assoc :cause cause)
           decision (assoc :recovered decision)))))))

(defn death-status
  "The status :died extra for a memory view."
  [view]
  (death-summary (mem/latest view :died) view (mem/latest view :recovered)))

(def doc
  "After a death, go back for the drops when they are worth it.
  Takes the latest :died entry with no newer :recovered; once a death is decided (a skip too) the check waits :no-drops and the wait names the decision. The check holds while a death is unrecovered, however old,
  so an expired trip still gets to write :abandoned.
  One run is the whole trip: it holds still (declared :respawning, then :settling) until a :respawned entry newer than
  the death exists and its 2 s have passed (no decision, no walk), decides, walks, collects and writes :recovered.
  Every :declined emits recover-drops.declined with a :reason (:danger with :mob and :mob-pos, :unreachable) and a text.
  It ends :declined, without acting, while a real danger is within :danger-radius, so a reflex can deal with it
  (the died trigger fires it again), and when go-to does not arrive; the despawn window ends it :abandoned.
  A real danger is a mob that can reach the body, or a ranged one with a line of fire (jobs.lib.danger/nearest-danger).
  Decision: skips when the value of what was carried is at most the fetch cost plus :margin.
  - Value: jobs.lib.cost/item-value (with :value-overrides), plus 5 per level of experience.
  - Cost: a trip of 10, 0.3 per block of straight distance, and 10 per point of route danger (jobs.lib.cost/route-danger)
    past the hostiles the body knows of (seen or heard, jobs.lib.danger/known-hostiles; :danger-overrides, after armour),
    many times dearer for a mob the body (its best weapon, armour, health) cannot afford to fight.
  - A pile in water (its cell or the one above) or lost to drowning adds :wet-danger points of route danger (named water or drowning).
  - Infinite when lava, fire or the void took the pile, or the walk would end after the despawn.
  A fetch emits recover-drops.fetching. A skip emits recover-drops.decided. Both texts give the value, the cost and their parts.
  Kit: before the decision, when the pile is worth the trip were the route safe, a body with no weapon or with an empty armour slot, whose killer or the hostiles it knows make the route
  dangerous, fetches the best weapon and armour piece a chest of known stock (:fetch/stock) holds (jobs.lib.fetch, chests only,
  :fetch false: off) and wears the armour (jobs.items.wear); nothing known to be in a chest: unchanged.
  A kit fetch waiting on the world is waited for (declared hold :fetching-kit); a fetch that ended the job drops the kit (warn recover-drops.kit-dropped).
  Fetch: walks to the death point (jobs.movement.go-to), then collects (passes, up to 3) the pile's items it can see within :collect-radius
  (jobs.forestry.collect-drops; only the carried item names, never items out of sight). Drops that went out of sight
  (down a shaft) get one walk to the cell the first was last seen in and a look again before the trip ends :partial.
  The pile out of reach (go-to :goal-enclosed, or no item in sight at it) with blocks this body placed within ::seal-radius of it (the scaffold ledger, a flee
  seal): once per trip they are taken back (jobs.access.cleanup, event recover-drops.seal) and the walk goes on.
  Ends by writing a :recovered entry {:decision :skip|:collected|:partial|:abandoned}.
  :collected counts the pile's items that entered the inventory since the decision, those picked up on the walk too.
  :abandoned with :reason :unreachable (the walk was blocked, or did not arrive :max-blocked-retries times), :danger
  (the trip was cut by a hostile, or declined for a danger, :max-hostile-cuts times), :nothing-found or :window-closed
  (the five minute despawn window closed). :partial carries :left and :reason :unreachable or :not-visible.
  A pile already carried again ends :collected without a collect step.
  Restart-safe: the decision, the baseline of carried items and the phase are kept in a :recover-trip entry keyed to the death.
  A run cut by a higher reflex, or a restarted body, goes on with the same trip.
  Warns respawn_away_from_bed once per death when the body respawned over 10 blocks from its respawn bed (the live :spawn-bed, else the :bed it slept in).
  Memory: reads :died, :respawned, :bed, :slept, :recover-trip, the scaffold ledger. Writes :recovered and :recover-trip.")

(a/defargs args
  {:margin {:doc "added to the fetch cost before comparing it to the value" :spec (a/num-in nil nil) :default 0}
   :value-overrides {:doc "jobs.lib.cost/item-value overrides, a map: item name or group (ore tool armor food block unknown) -> worth of one item, or {:times n}; e.g. {\"raw_iron\" 500}" :spec (a/map-of string? (a/or-of number? map?)) :default {}}
   :danger-overrides {:doc "jobs.lib.cost/route-danger overrides, a map: mob name -> threat in points of damage before armour, or {:times n}; e.g. {\"creeper\" 100 \"zombie\" 0}" :spec (a/map-of string? (a/or-of number? map?)) :default {}}
   :danger-radius {:doc "a hostile this close makes the job yield without acting" :spec (a/num-in 0 nil) :default 8}
   :fetch {:doc "get a weapon and armour from a chest whose stock the body knows before walking to a pile its killer guards (jobs.lib.fetch limits); false: no kit fetch" :spec fetch/option? :default {:what #{:item} :how #{:chest} :minutes 3}}
   :collect-radius {:doc "collect the pile's drops within this many blocks of the death point (a pile on open ground rolls 6-8 out)" :spec (a/num-in 0 nil) :default 10}})

(a/defargs settings
  {::arrive-range {:default 2 :doc "How near the death point the walk back ends, in blocks." :spec (a/int-in 0 nil)}
   ::xp-per-level {:default 5 :doc "Worth of one experience level carried at the death (the game drops a few levels' worth)." :spec (a/int-in 0 nil)}
   ::top-n {:default 3 :doc "Items and mobs named in the report of a death site, at most." :spec (a/int-in 0 nil)}
   ::max-collect-passes {:default 3 :doc "Walks back to the pile after a collect pass that left visible items of it behind." :spec (a/int-in 0 nil)}
   ::stray-range {:default 5 :doc "A body this far from the death point in the collect phase (a flee cut the trip) walks back first." :spec (a/int-in 0 nil)}
   ::max-hostile-cuts {:default 2 :doc "Resumes of one trip that followed a cut by a hostile reflex, or a decline for a danger, before it is given up as :danger." :spec (a/int-in 1 nil)}
   ::max-blocked-retries {:default 3 :doc "Walks of one trip that did not arrive before it is given up as :unreachable." :spec (a/int-in 1 nil)}
   ::wet-danger {:default 30 :doc "Points of route danger added for a pile in water or lost to drowning (a swim with the air running out); 10 cost each." :spec (a/num-in 0 nil)}
   ::seal-radius {:default 4 :doc "Scaffold blocks this body placed within this many blocks of the pile (a flee seal) are taken back when they keep it out of reach." :spec (a/num-in 0 nil)}
   ::bed-away-blocks {:default 10 :doc "A respawn farther than this from the bed the body slept in is reported." :spec (a/int-in 0 nil)}})
(defn seal-radius [] (settings/get settings ::seal-radius))
(defn wet-danger [] (settings/get settings ::wet-danger))
(defn arrive-range [] (settings/get settings ::arrive-range))
(defn max-hostile-cuts [] (settings/get settings ::max-hostile-cuts))
(defn max-blocked-retries [] (settings/get settings ::max-blocked-retries))
(def settle-ms
  "After a respawn the client needs this long to re-receive nearby entities;
  an estimate made sooner sees no hostiles."
  2000)
(def day-ms (* 24 60 60 1000))
(def recovered-policy {:cap 10 :ttl day-ms})
(def trip-policy {:cap 1 :ttl day-ms}) ; wall clock; the despawn window itself is counted in game ticks

(defn finite [n] (if (js/isFinite n) n :infinite))

(defn round1 [n] (/ (js/Math.round (* 10 n)) 10))

(defn why-text
  "The value and the cost with their main parts: \"value 3058.5 (24 raw_iron 2880, ...), cost 31 (trip 10, walk 6,
  danger 15: zombie 1.5)\"."
  [{:keys [value cost top-items parts reason top-mobs]} margin]
  (str "value " (round1 value)
       (when (seq top-items) (str " (" (str/join ", " (map (fn [{:keys [name count value]}] (str count " " name " " (round1 value))) top-items)) ")"))
       ", cost " (if (= :infinite cost) (str "infinite (" (name (or reason :unknown)) ")") (round1 cost))
       (when (seq parts)
         (str " (" (str/join ", " (map (fn [[k n]] (str (name k) " " (round1 n))) parts))
              (when (seq top-mobs) (str ": " (str/join ", " (map (fn [{:keys [name danger]}] (str name " " (round1 danger))) top-mobs))))
              ")"))
       (when (and (number? margin) (not (zero? margin))) (str " + margin " margin))))

(defn decision-text [decision {:keys [reason items] :as decided} margin]
  (case decision
    :skip (str "skip: " (why-text decided margin))
    :fetch (str "fetch: " (why-text decided margin))
    :collected (str "collected " items " items")
    :partial (str "partial: " items " items back, left " (str/join ", " (map (fn [[k n]] (str n " " k)) (:left decided)))
                  ", " (case reason :unreachable "in sight but not picked up" "not in sight"))
    :abandoned (str "abandoned: " (name (or reason :unknown)))))

(defn decided-wait
  "The wait reason when no death is unrecovered: {:reason :no-drops}, with :decision and a :text naming it when the
  latest death was already decided (a skip is final for that death, whoever asks again)."
  [view]
  (let [died (mem/latest view :died)
        recovered (mem/latest view :recovered)
        decision (when (and died recovered (> (:t recovered) (:t died))) (:data recovered))]
    (cond-> {:reason :no-drops}
      decision (assoc :decision (:decision decision)
                      :text (str "the latest death is already decided: " (decision-text (:decision decision) decision nil))))))

(defn check [c]
  (or (some? (body/unrecovered-death (ctx/view c)))
      (ctx/wait c (decided-wait (ctx/view c)))))

(defn finish!
  "Write the :recovered entry, report the decision and end the job."
  [c decision fields]
  (let [pos (:pos (:data (body/unrecovered-death (ctx/view c))))]
    (ctx/remember! c :recovered (merge {:decision decision} fields) recovered-policy)
    (ctx/emit! c :recover-drops.decided :info
               (assoc (select-keys fields [:value :cost :reason :items :left :parts :top-items :top-mobs])
                      :decision decision :pos pos
                      :text (decision-text decision fields (:margin (:args c)))))
    :done))

(defn settling?
  "True while the latest :respawned entry, newer than the death, is younger than settle-ms."
  [c entry]
  (let [respawned (ctx/latest c :respawned)]
    (boolean (and respawned
                  (> (:t respawned) (:t entry))
                  (< (- (ctx/now c) (:t respawned)) settle-ms)))))

(defn saved-trip
  "The :recover-trip entry's data for the death at death-t, else nil."
  [c death-t]
  (let [trip (:data (ctx/latest c :recover-trip))]
    (when (= death-t (:death-t trip)) trip)))

(defn save-trip!
  "Write the trip so far (job memory :decided :baseline :phase :cuts :blocked :blocked-retries :hostile-cut) to the :recover-trip entry."
  [c]
  (let [m (ctx/mem c)]
    (ctx/remember! c :recover-trip (select-keys m [:death-t :decided :baseline :phase :cuts :blocked :blocked-retries :hostile-cut]) trip-policy)))

(defn hostile-cut?
  "Whether the saved trip was last cut by a hostile: it declined for a danger, or the hostile reflex wrote a :hostile
  entry since the trip was saved. A cut by night, eating or a restart is not."
  [c saved]
  (boolean (or (:hostile-cut saved)
               (seq (ctx/since c :hostile (:t (ctx/latest c :recover-trip)))))))

(defn key-to-death!
  "Reset the job memory when it belongs to another death than entry: start from the saved trip of this death (a run
  cut before, or a restart), else afresh. The hostile cuts are counted whatever :blocked says."
  [c entry]
  (when (not= (:t entry) (:death-t (ctx/mem c)))
    (let [saved (saved-trip c (:t entry))]
      (ctx/update-mem! c (constantly (merge {:death-t (:t entry)}
                                            (select-keys saved [:decided :baseline :phase :blocked :blocked-retries])
                                            (when saved {:cuts (+ (:cuts saved 0) (if (hostile-cut? c saved) 1 0))}))))
      (when saved (save-trip! c)))))

(defn xp-per-level [] (settings/get settings ::xp-per-level))
(defn top-n [] (settings/get settings ::top-n))

(defn wet-pile?
  "Whether the pile's cell or the one above it is water (the body would swim to collect)."
  [kind-at pos]
  (let [[x y z] (map js/Math.floor [(:x pos) (:y pos) (:z pos)])]
    (boolean (and pos (or (= :water (kind-at x y z)) (= :water (kind-at x (inc y) z)))))))

(defn estimate
  "{:value :cost :parts :reason :top-items :top-mobs} of recovering the drops of the :died data (cost js/Infinity when
  they cannot be fetched)."
  [c {:keys [pos inventory cause experience]} elapsed]
  (let [p (:primitives c)
        here (u/self-pos c)
        kind-at (reach/lookup p)
        worth (cost/item-value inventory :overrides (:value-overrides (:args c)))
        route (when pos (cost/straight-route kind-at here pos))
        version (game/version-of p)
        guard (when (and pos (string? cause)) [{:name cause :pos pos}]) ; route-danger drops a cause that is no hostile mob
        threat (if route
                 (cost/route-danger version kind-at route (concat (danger-q/seen-hostiles p) guard) (.-equipment (.self p))
                                    :overrides (:danger-overrides (:args c)) :weapon (combat/best-weapon p combat/default-weapons) :health (.-health (.self p)))
                 {:danger 0 :mobs []})
        wet (when (or (= "drowning" cause) (wet-pile? kind-at pos))
              {:name (if (= "drowning" cause) "drowning" "water") :danger (wet-danger)})
        threat (cond-> threat wet (-> (update :danger + (:danger wet)) (update :mobs #(vec (cons wet %)))))
        fetch (cost/fetch-cost {:distance (when pos (u/dist here pos)) :danger (:danger threat) :elapsed-ms elapsed :cause cause})]
    (cond-> {:value (+ (:value worth) (* (xp-per-level) (or (:level experience) 0)))
             :cost (:cost fetch)
             :top-items (mapv #(select-keys % [:name :count :value]) (take (top-n) (:items worth)))}
      (:parts fetch) (assoc :parts (:parts fetch))
      (:reason fetch) (assoc :reason (:reason fetch))
      (seq (:mobs threat)) (assoc :top-mobs (mapv #(select-keys % [:name :danger]) (take (top-n) (:mobs threat)))))))

(defn max-collect-passes [] (settings/get settings ::max-collect-passes))
(defn stray-range [] (settings/get settings ::stray-range))

(defn decline!
  "Say why this run ends :declined (an event with the reason and a text), then :declined."
  [c reason text fields]
  (ctx/emit! c :recover-drops.declined :info (assoc fields :reason reason :text (str "recover-drops declined: " text)))
  :declined)

(defn own-seal-near
  "The open scaffold ledger entries (jobs.lib.ledger) within seal-radius of pos: blocks this body placed round the pile."
  [c pos]
  (filterv #(<= (u/dist pos (zipmap [:x :y :z] (:cell %))) (seal-radius)) (ledger/open-entries (ctx/view c))))

(defn ^:async clear-seal!
  "When the body's own placed blocks stand near the pile (its flee seal): take back those within seal-radius of it
  (jobs.access.cleanup :near) and walk again. Tried twice a trip at most; :seal-cleared is set once a run finished.
  :next when it ran, else nil."
  [c pos]
  (when (and (not (:seal-cleared (ctx/mem c))) (< (:seal-tries (ctx/mem c) 0) 2) (seq (own-seal-near c pos)))
    (ctx/update-mem! c #(-> % (update :seal-tries (fnil inc 0)) (assoc :phase :go)))
    (save-trip! c)
    (ctx/emit! c :recover-drops.seal :info {:pos pos :text "the pile is out of reach behind blocks this body placed: taking them back"})
    (when (= :done (await (child/run! c :seal 'jobs.access.cleanup {:job :all :near {:pos pos :radius (seal-radius)}})))
      (ctx/update-mem! c assoc :seal-cleared true))
    :next))

(defn ^:async go!
  "Walk to the death point by one go-to call: :next once there (the collect phase), else :declined (go-to waits on the
  world, or it did not arrive: the next run walks again while the pile is worth it, and the window closes it). A walk
  go-to refused for the body's own shelter (:inside-own-shelter) is no failed try: the window closes the trip."
  [c pos]
  (let [r (await (child/call-walk c :go {:pos pos :range (arrive-range)}))]
    (cond
      (not= :done r) (decline! c :unreachable (str "the walk to " (pr-str pos) " is waiting on the world") {:pos pos})
      (:inside-own-shelter (ctx/child-result c :go))
      (decline! c :unreachable (str "the walk to " (pr-str pos) " is refused: the body is sealed in its own shelter, which opens by day") {:pos pos})
      (and (= :goal-enclosed (:why (ctx/child-result c :go))) (await (clear-seal! c pos))) :next
      (not (:arrived (ctx/child-result c :go)))
      (do (ctx/update-mem! c #(-> % (assoc :blocked true) (update :blocked-retries (fnil inc 0))))
          (save-trip! c)
          (decline! c :unreachable (str "the walk to " (pr-str pos) " did not arrive") {:pos pos}))
      :else (do (ctx/update-mem! c assoc :phase :collect :blocked false) (save-trip! c) :next))))

(defn carried-counts
  "{item name total} of what the body carries."
  [c]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory (:primitives c))))

(defn recovered-items
  "How many of the pile's items entered the inventory since the baseline (taken when the
  trip was decided): per item name the rise, capped by what the pile held. Items picked up
  on the walk count as well as those the collect step picks up."
  [c pile]
  (let [baseline (:baseline (ctx/mem c))
        now (carried-counts c)
        wanted (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} pile)]
    (reduce + 0 (map (fn [[name n]] (min n (max 0 (- (get now name 0) (get baseline name 0))))) wanted))))

(defn pile-ids
  "Ids of the item entities in sight lying within radius of pos whose name is one of names."
  [c pos names radius]
  (let [wanted (set names)]
    (->> (array-seq (.entities (:primitives c) #js {:radius (+ radius (stray-range) 4) :kind "item" :max 64}))
         (remove #(false? (.-visible %)))
         (filter #(wanted (some-> (.-item %) .-name)))
         (filter #(<= (u/dist pos (u/pos-of (.-pos %))) radius))
         (mapv #(.-id %)))))

(defn left-over
  "{item name count} of the pile not yet back in the inventory."
  [c pile]
  (let [baseline (:baseline (ctx/mem c))
        now (carried-counts c)
        wanted (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} pile)]
    (into {} (keep (fn [[name n]] (let [l (- n (max 0 (- (get now name 0) (get baseline name 0))))] (when (pos? l) [name l]))) wanted))))

(defn finish-collect!
  "End the collect phase: :collected when all of the pile is back, else :partial naming what is left and why."
  [c pile ids]
  (let [items (recovered-items c pile)
        left (left-over c pile)
        decided (:decided (ctx/mem c))]
    (cond
      (empty? left) (finish! c :collected (assoc decided :items items))
      (zero? items) (finish! c :abandoned (assoc decided :items 0 :reason :nothing-found))
      :else (finish! c :partial (assoc decided :items items :left left :reason (if (seq ids) :unreachable :not-visible))))))

(defn look-cell
  "The cell [x y z] the first unseen drop of the last collect pass was last seen in, or nil."
  [c]
  (->> (:left (ctx/child-result c :collect))
       (filter #(= :unseen (:reason %)))
       (mapcat (comp vals :last))
       first))

(defn ^:async look-round!
  "Drops of the pile went out of sight (down a shaft): once, walk to the cell the first was last seen in (go-to, range 1)
  and look again. :next with a pass counted when drops are in sight now, else the end of the trip (what is left, with its
  reason). Not an arrival ends the trip :partial too."
  [c [x y z] pos names radius pile]
  (ctx/update-mem! c assoc :looked true)
  (await (child/run! c :look 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 1 :escalate false}))
  (let [again (pile-ids c pos names radius)]
    (if (seq again)
      (do (ctx/update-mem! c update :passes (fnil inc 0)) (save-trip! c) :next)
      (finish-collect! c pile again))))

(defn ^:async collect!
  "One collect pass over the pile's visible items: :next after a pass that left some in sight (walk back, at most
  max-collect-passes), else the job's end."
  [c pos radius pile]
  (let [names (vec (distinct (map :name pile)))
        total (reduce + 0 (map :count pile))]
    (if (>= (recovered-items c pile) total)
      (finish-collect! c pile [])
      (let [ids (pile-ids c pos names radius)]
        (if (empty? ids)
          (or (await (clear-seal! c pos)) (finish-collect! c pile ids))
          (do
            (await (child/run! c :collect 'jobs.forestry.collect-drops
                               {:radius (+ radius (stray-range) 4) :ids ids :filter names}))
            (if (empty? (left-over c pile))
              (finish-collect! c pile [])
              (let [again (pile-ids c pos names radius)
                    passes (:passes (ctx/mem c) 0)]
                (cond
                  (and (seq again) (< passes (max-collect-passes)))
                  (do (ctx/update-mem! c assoc :passes (inc passes) :phase :go) (save-trip! c) :next)

                  (and (empty? again) (not (:looked (ctx/mem c))) (look-cell c))
                  (await (look-round! c (look-cell c) pos names radius pile))

                  :else (finish-collect! c pile again))))))))))

(defn window-closed?
  "True once the despawn window of the death entry has passed."
  [c entry]
  (>= (game/ticks-since entry (ctx/view c)) game/despawn-ticks))

(defn threatened?
  "The nearest real danger within :danger-radius, or nil."
  [c]
  (danger-q/nearest-danger (:primitives c) (:danger-radius (:args c)) {} {}))

(defn danger-fields
  "{:mob :mob-pos} of the danger m: its place when seen, else the rough spot its band gives (jobs.lib.danger/mob-pos)."
  [p m]
  {:mob (.-name m) :mob-pos (danger-q/mob-pos p m)})

(defn decline-danger!
  "End :declined for the danger m: its name and place are in the event."
  [c m]
  (let [{:keys [mob mob-pos] :as fields} (danger-fields (:primitives c) m)]
    (ctx/update-mem! c assoc :hostile-cut true)
    (save-trip! c)
    (decline! c :danger (str mob " at " (pr-str mob-pos) " within " (:danger-radius (:args c)) " blocks") fields)))

(defn ^:async wait-while!
  "Hold still (reason) while (pending?) is true: nil when it clears, else :declined for a danger within
  :danger-radius or a cut, :closed when the despawn window passes."
  [c entry reason pending?]
  (loop []
    (cond
      (not (pending?)) (do (ctx/hold-still! c nil) nil)
      (not (ctx/alive? c)) :declined
      (window-closed? c entry) (do (ctx/hold-still! c nil) :closed)
      (threatened? c) (do (ctx/hold-still! c nil) (decline-danger! c (threatened? c)))
      :else (do (ctx/hold-still! c reason) (await (pace/pace!)) (recur)))))

(defn decide!
  "The decision of this trip: the saved one, else estimate, save and report it. A resumed trip estimates again once (from
  where the body stands, with the hostiles it knows now); its baseline stays."
  [c entry]
  (if (and (:decided (ctx/mem c)) (or (zero? (:cuts (ctx/mem c) 0)) (:reestimated (ctx/mem c))))
    (:decided (ctx/mem c))
    (let [margin (:margin (:args c))
          e (update (estimate c (:data entry) (settings/ticks->ms (game/ticks-since entry (ctx/view c)))) :cost finite)
          fetch? (> (:value e) (+ (if (= :infinite (:cost e)) js/Infinity (:cost e)) margin))]
      (ctx/update-mem! c #(cond-> (assoc % :decided e :reestimated true) (nil? (:baseline %)) (assoc :baseline (carried-counts c))))
      (save-trip! c)
      (when fetch?
        (ctx/emit! c :recover-drops.fetching :info
                   (assoc (select-keys e [:value :cost :parts :top-items :top-mobs])
                          :pos (:pos (:data entry)) :text (decision-text :fetch e margin))))
      e)))

;; ------------------------------------------------------------------ the kit

(defn stock-names
  "The item names the known chests hold (:fetch/stock)."
  [c]
  (into #{} (mapcat keys) (vals (fetch/stock-of (ctx/view c)))))

(defn piece-points
  "Armour points of one piece (jobs.lib.cost/armour-stats), 0 for nil."
  [slot item]
  (if item (:points (cost/armour-stats {slot {:name item}})) 0))

(defn kit-problem
  "The need wait of the best weapon, else of the best piece for an empty or weaker armour slot, that a known chest holds
  and the body does not carry (ranked by jobs.lib.cost: damage per attack gap, armour points); nil when none."
  [c]
  (let [p (:primitives c)
        have (set (map :name (u/inventory p)))
        stock (remove have (stock-names c))
        worn (armour/worn-of (.-equipment (.self p)))
        weapons (->> stock (filter #(combat/weapon? combat/default-weapons %)) (sort-by #(/ (cost/weapon-damage %) (cost/attack-gap-ms %)) >))
        pieces (fn [slot] (->> stock
                               (filter #(and (= slot (:slot (armour/parse %))) (> (piece-points slot %) (piece-points slot (get worn slot)))))
                               (sort-by #(piece-points slot %) >)))
        need (fn [names] (when (seq names) {:reason :need :any-of (vec names) :count 1}))]
    (or (when-not (combat/best-weapon p combat/default-weapons) (need weapons))
        (some #(need (pieces %)) armour/slots))))

(defn kit-called-for?
  "Whether the pile is worth the trip were the route safe (its value against the fetch cost without danger and the margin)
  and the killer or the hostiles the body knows make the route dangerous (the estimate names mobs)."
  [c entry]
  (let [{:keys [pos cause]} (:data entry)
        elapsed (settings/ticks->ms (game/ticks-since entry (ctx/view c)))
        e (estimate c (:data entry) elapsed)
        safe (cost/fetch-cost {:distance (when pos (u/dist (u/self-pos c) pos)) :danger 0 :elapsed-ms elapsed :cause cause})
        safe-cost (if (js/isFinite (:cost safe)) (:cost safe) js/Infinity)]
    (boolean (and (seq (:top-mobs e)) (> (:value e) (+ safe-cost (:margin (:args c))))))))

(defn ^:async kit!
  "Once per run: fetch and wear a weapon and armour when the pile is worth the trip, the danger calls for them and a known
  chest has them. A fetch child waiting on the world is waited for (declared hold :fetching-kit) until it ends, a danger
  or the despawn window. nil when done, dropped (a fetch that ended the job: warn) or nothing to do; :declined for a
  danger or a death."
  [c entry]
  (if (or (:kit-done (ctx/mem c)) (not (fetch/opts c 'jobs.survival.recover-drops))
          (nil? (kit-problem c)) (not (kit-called-for? c entry)))
    (do (ctx/update-mem! c assoc :kit-done true) nil)
    (loop []
      (let [r (await (fetch/fetch! c 'jobs.survival.recover-drops kit-problem))]
        (cond
          (= :done r)
          (do (ctx/update-mem! c assoc :kit-done true)
              (ctx/emit! c :recover-drops.kit-dropped :warn {:text "recover-drops: the kit fetch ended the job; going without a kit"})
              nil)

          (not= :continue r)
          (do (await (ctx/call-child c :wear 'jobs.items.wear {}))
              (ctx/update-mem! c assoc :kit-done true)
              nil)

          (not (ctx/alive? c)) :declined
          (window-closed? c entry) (do (ctx/hold-still! c nil) nil)
          (threatened? c) (do (ctx/hold-still! c nil) (decline-danger! c (threatened? c)))
          :else (do (ctx/hold-still! c :fetching-kit) (await (pace/pace!)) (recur)))))))

(defn ^:async trip!
  "The decision, then the walk and the collect passes of a trip inside its window."
  [c entry]
  (let [{:keys [margin collect-radius]} (:args c)
        kit (await (kit! c entry))
        decided (when-not kit (decide! c entry))
        pos (:pos (:data entry))]
    (cond
      kit :declined

      (<= (:value decided) (+ (if (= :infinite (:cost decided)) js/Infinity (:cost decided)) margin))
      (finish! c :skip decided)

      (>= (:cuts (ctx/mem c) 0) (max-hostile-cuts))
      (finish! c :abandoned (assoc decided :reason :danger))

      (>= (:blocked-retries (ctx/mem c) 0) (max-blocked-retries))
      (finish! c :abandoned (assoc decided :reason :unreachable))

      (threatened? c) (decline-danger! c (threatened? c))

      (and (= :collect (:phase (ctx/mem c))) (> (u/dist (u/self-pos c) pos) (stray-range)))
      (do (ctx/update-mem! c assoc :phase :go) :next)

      (= :collect (:phase (ctx/mem c))) (await (collect! c pos collect-radius (:inventory (:data entry))))
      :else (await (go! c pos)))))

(defn ^:async step!
  "One step of the trip: :next to go on, or the round's end (:done, :declined)."
  [c entry]
  (cond
    (not (ctx/alive? c)) :declined
    (window-closed? c entry)
    (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason (if (:blocked (ctx/mem c)) :unreachable :window-closed)))
    :else (await (trip! c entry))))

(defn bed-away-blocks [] (settings/get settings ::bed-away-blocks))

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
      (when (> (u/dist bed at) (bed-away-blocks))
        (ctx/emit! c :respawn_away_from_bed :warn
                   {:bed bed :pos at :text "respawned away from its bed: the respawn point was not set there"})))))

(defn ^:async round
  "One whole run: wait for the respawn and its settling (declared holds), decide, walk, collect, write :recovered.
  :declined when a danger is within :danger-radius (the hostile reflex acts, the trigger fires again) or the walk
  did not arrive."
  [c]
  (let [entry (body/unrecovered-death (ctx/view c))]
    (when entry (key-to-death! c entry))
    (if (nil? entry)
      :done
      (let [waited (or (await (wait-while! c entry :respawning #(not (body/respawned-since? (ctx/view c) entry))))
                       (when (nil? (:decided (ctx/mem c)))
                         (await (wait-while! c entry :settling #(settling? c entry)))))]
        (when-not (= :declined waited) (warn-away-from-bed! c entry))
        (if (= :declined waited)
          :declined
          (loop []
            (let [entry (body/unrecovered-death (ctx/view c))
                  r (if entry (await (step! c entry)) :done)]
              (if (= :next r) (recur) r))))))))
