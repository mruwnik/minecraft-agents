(ns jobs.survival.recover-drops
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [triggers.survival.died :as died]
            [engine.game :as game]))

(def doc
  "After a death, go back for the drops when they are worth it.
  Takes the latest :died entry with no newer :recovered. The check holds while a death is unrecovered, however old,
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
    past the hostiles the body knows of (seen or heard, jobs.lib.danger/known-hostiles; :danger-overrides, after armour).
  - Infinite when lava, fire or the void took the pile, or the walk would end after the despawn.
  A fetch emits recover-drops.fetching. A skip emits recover-drops.decided. Both texts give the value, the cost and their parts.
  Fetch: walks to the death point (jobs.movement.go-to), then collects (passes, up to 3) the pile's items it can see within :collect-radius
  (jobs.forestry.collect-drops; only the carried item names, never items out of sight).
  Ends by writing a :recovered entry {:decision :skip|:collected|:partial|:abandoned}.
  :collected counts the pile's items that entered the inventory since the decision, those picked up on the walk too.
  :abandoned with :reason :unreachable (the walk was blocked), :nothing-found or :window-closed (the five minute
  despawn window closed). :partial carries :left and :reason :unreachable or :not-visible.
  A pile already carried again ends :collected without a collect step.
  Restart-safe: the decision, the baseline of carried items and the phase are kept in a :recover-trip entry keyed to the death.
  A run cut by a higher reflex, or a restarted body, goes on with the same trip.
  Memory: reads :died, :respawned, :recover-trip. Writes :recovered and :recover-trip.")

(def args
  {:margin {:doc "added to the fetch cost before comparing it to the value" :default 0}
   :value-overrides {:doc "jobs.lib.cost/item-value overrides, a map: item name or group (ore tool armor food block unknown) -> worth of one item, or {:times n}; e.g. {\"raw_iron\" 500}" :default {}}
   :danger-overrides {:doc "jobs.lib.cost/route-danger overrides, a map: mob name -> threat in points of damage before armour, or {:times n}; e.g. {\"creeper\" 100 \"zombie\" 0}" :default {}}
   :danger-radius {:doc "a hostile this close makes the job yield without acting" :default 8}
   :collect-radius {:doc "collect the pile's drops within this many blocks of the death point (a pile on open ground rolls 6-8 out)" :default 10}})

(def arrive-range 2)
(def settle-ms
  "After a respawn the client needs this long to re-receive nearby entities;
  an estimate made sooner sees no hostiles."
  2000)
(def day-ms (* 24 60 60 1000))
(def recovered-policy {:cap 10 :ttl day-ms})
(def trip-policy {:cap 1 :ttl game/despawn-ms})

(defn check [c]
  (or (some? (died/unrecovered-death (ctx/view c)))
      (ctx/wait c {:reason :no-drops})))

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

(defn finish!
  "Write the :recovered entry, report the decision and end the job."
  [c decision fields]
  (let [pos (:pos (:data (died/unrecovered-death (ctx/view c))))]
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

(defn key-to-death!
  "Reset the job memory when it belongs to another death than entry: start from the saved trip of this death (a run
  cut before, or a restart), else afresh."
  [c entry]
  (when (not= (:t entry) (:death-t (ctx/mem c)))
    (ctx/update-mem! c (constantly (merge {:death-t (:t entry)}
                                          (select-keys (saved-trip c (:t entry)) [:decided :baseline :phase]))))))

(defn save-trip!
  "Write the trip so far (job memory :decided :baseline :phase) to the :recover-trip entry."
  [c]
  (let [m (ctx/mem c)]
    (ctx/remember! c :recover-trip (select-keys m [:death-t :decided :baseline :phase]) trip-policy)))

(def xp-per-level "Worth of one experience level carried at the death (the game drops a few levels' worth)." 5)
(def top-n 3)

(defn overrides-arg
  "The override map of arg k, or {} with a recover-drops.bad-overrides warn when it is not a map."
  [c k]
  (let [o (get (:args c) k)]
    (cond
      (nil? o) {}
      (map? o) o
      :else (do (ctx/emit! c :recover-drops.bad-overrides :warn
                           {:arg k :got (pr-str o) :text (str "recover-drops: " (name k) " must be a map such as {\"raw_iron\" 500}; ignored " (pr-str o))})
                {}))))

(defn estimate
  "{:value :cost :parts :reason :top-items :top-mobs} of recovering the drops of the :died data (cost js/Infinity when
  they cannot be fetched)."
  [c {:keys [pos inventory cause experience]} elapsed]
  (let [p (:primitives c)
        here (u/self-pos c)
        kind-at (reach/lookup p)
        worth (cost/item-value inventory :overrides (overrides-arg c :value-overrides))
        route (when pos (cost/straight-route kind-at here pos))
        threat (if route
                 (cost/route-danger (game/version-of p) kind-at route (danger-q/seen-hostiles p) (.-equipment (.self p)) :overrides (overrides-arg c :danger-overrides))
                 {:danger 0 :mobs []})
        fetch (cost/fetch-cost {:distance (when pos (u/dist here pos)) :danger (:danger threat) :elapsed-ms elapsed :cause cause})]
    (cond-> {:value (+ (:value worth) (* xp-per-level (or (:level experience) 0)))
             :cost (:cost fetch)
             :top-items (mapv #(select-keys % [:name :count :value]) (take top-n (:items worth)))}
      (:parts fetch) (assoc :parts (:parts fetch))
      (:reason fetch) (assoc :reason (:reason fetch))
      (seq (:mobs threat)) (assoc :top-mobs (mapv #(select-keys % [:name :danger]) (take top-n (:mobs threat)))))))

(def max-collect-passes "Walks back to the pile after a collect pass that left visible items of it behind." 3)
(def stray-range "A body this far from the death point in the collect phase (a flee cut the trip) walks back first." 5)

(defn decline!
  "Say why this run ends :declined (an event with the reason and a text), then :declined."
  [c reason text fields]
  (ctx/emit! c :recover-drops.declined :info (assoc fields :reason reason :text (str "recover-drops declined: " text)))
  :declined)

(defn ^:async go!
  "Walk to the death point by one go-to call: :next once there (the collect phase), else :declined (go-to waits on the
  world, or it did not arrive: the next run walks again while the pile is worth it, and the window closes it)."
  [c pos]
  (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos pos :range arrive-range}))]
    (cond
      (not= :done r) (decline! c :unreachable (str "the walk to " (pr-str pos) " is waiting on the world") {:pos pos})
      (not (:arrived (ctx/child-result c :go)))
      (do (ctx/update-mem! c assoc :blocked true)
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
    (->> (array-seq (.entities (:primitives c) #js {:radius (+ radius stray-range 4) :kind "item" :max 64}))
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
          (finish-collect! c pile ids)
          (do
            (await (child/run! c :collect 'jobs.forestry.collect-drops
                               {:radius (+ radius stray-range 4) :ids ids :filter names}))
            (if (empty? (left-over c pile))
              (finish-collect! c pile [])
              (let [again (pile-ids c pos names radius)
                    passes (:passes (ctx/mem c) 0)]
                (if (and (seq again) (< passes max-collect-passes))
                  (do (ctx/update-mem! c assoc :passes (inc passes) :phase :go) (save-trip! c) :next)
                  (finish-collect! c pile again))))))))))

(defn window-closed?
  "True once the despawn window of the death entry has passed."
  [c entry]
  (>= (- (ctx/now c) (:t entry)) game/despawn-ms))

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
  "The decision of this trip: the saved one, else estimate, save and report it."
  [c entry]
  (or (:decided (ctx/mem c))
      (let [margin (:margin (:args c))
            e (update (estimate c (:data entry) (- (ctx/now c) (:t entry))) :cost finite)
            fetch? (> (:value e) (+ (if (= :infinite (:cost e)) js/Infinity (:cost e)) margin))]
        (ctx/update-mem! c assoc :decided e :baseline (carried-counts c))
        (save-trip! c)
        (when fetch?
          (ctx/emit! c :recover-drops.fetching :info
                     (assoc (select-keys e [:value :cost :parts :top-items :top-mobs])
                            :pos (:pos (:data entry)) :text (decision-text :fetch e margin))))
        e)))

(defn ^:async trip!
  "The decision, then the walk and the collect passes of a trip inside its window."
  [c entry]
  (let [{:keys [margin collect-radius]} (:args c)
        decided (decide! c entry)
        pos (:pos (:data entry))]
    (cond
      (<= (:value decided) (+ (if (= :infinite (:cost decided)) js/Infinity (:cost decided)) margin))
      (finish! c :skip decided)

      (threatened? c) (decline-danger! c (threatened? c))

      (and (= :collect (:phase (ctx/mem c))) (> (u/dist (u/self-pos c) pos) stray-range))
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

(defn ^:async round
  "One whole run: wait for the respawn and its settling (declared holds), decide, walk, collect, write :recovered.
  :declined when a danger is within :danger-radius (the hostile reflex acts, the trigger fires again) or the walk
  did not arrive."
  [c]
  (let [entry (died/unrecovered-death (ctx/view c))]
    (when entry (key-to-death! c entry))
    (if (nil? entry)
      :done
      (let [waited (or (await (wait-while! c entry :respawning #(not (died/respawned-since? (ctx/view c) entry))))
                       (when (nil? (:decided (ctx/mem c)))
                         (await (wait-while! c entry :settling #(settling? c entry)))))]
        (if (= :declined waited)
          :declined
          (loop []
            (let [entry (died/unrecovered-death (ctx/view c))
                  r (if entry (await (step! c entry)) :done)]
              (if (= :next r) (recur) r))))))))
