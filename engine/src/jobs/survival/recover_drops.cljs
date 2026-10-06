(ns jobs.survival.recover-drops
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [triggers.survival.died :as died]
            [engine.game :as game]))

(def doc
  "After a death, go back for the drops when they are worth it.
  Takes the latest :died entry with no newer :recovered. The check holds while a death is unrecovered, however old,
  so an expired trip still gets to write :abandoned.
  Waits (no decision, no walk) until a :respawned entry newer than the death exists.
  Yields without acting while a real danger is within :danger-radius, so a reflex can deal with it.
  A real danger is a mob that can reach the body, or a ranged one with a line of fire (jobs.lib.reach/nearest-danger).
  Decision: skips when the value of what was carried is at most the fetch cost plus :margin.
  - Value: jobs.lib.cost/item-value (with :value-overrides), plus 5 per level of experience.
  - Cost: a trip of 10, 0.3 per block of straight distance, and 10 per point of route danger (jobs.lib.cost/route-danger)
    past the hostiles the body knows of (seen or heard, jobs.lib.reach/known-hostiles; :danger-overrides, after armour).
  - Infinite when lava, fire or the void took the pile, or the walk would end after the despawn.
  A fetch emits recover-drops.fetching. A skip emits recover-drops.decided. Both texts give the value, the cost and their parts.
  Fetch: walks to the death point (jobs.movement.go-to), then collects the pile's items it can see within :collect-radius
  (jobs.forestry.collect-drops; only the carried item names, never items out of sight).
  Ends by writing a :recovered entry {:decision :skip|:collected|:partial|:abandoned}.
  :collected counts the pile's items that entered the inventory since the decision, those picked up on the walk too.
  :abandoned with :reason :unreachable (the walk was blocked), :nothing-found or :window-closed (the five minute
  despawn window closed). :partial carries :left and :reason :unreachable or :not-visible.
  A pile already carried again ends :collected without a collect step.
  The window keeps running while the body waits.
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

(defn check [c] (some? (died/unrecovered-death (ctx/view c))))

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

(defn seen-hostiles
  "The hostiles the body knows of within 64 (jobs.lib.reach/known-hostiles: the perception's mob memory, seen or
  heard and remembered while likely still near, as a player would; none it never sensed). Primitives without that
  memory: the ones in sight now."
  [p]
  (let [known (reach/known-hostiles p 64 {})]
    (if (.-knownMobs p) known (filterv reach/seen-mob? known))))

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
                 (cost/route-danger kind-at route (seen-hostiles p) (.-equipment (.self p)) :overrides (overrides-arg c :danger-overrides))
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

(defn ^:async go!
  "Walk to the death point. A walk that does not arrive (a fire or a mob in the way, a door) is retried on the next
  round while the pile is worth it: round writes :abandoned itself when the window closes."
  [c pos]
  (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos pos :range arrive-range}))]
    (cond
      (not= :done r) :continue
      (not (:arrived (ctx/child-result c :go))) (do (ctx/update-mem! c assoc :blocked true) :continue)
      :else (do (ctx/update-mem! c assoc :phase :collect :blocked false) (save-trip! c) :continue))))

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

(defn ^:async collect! [c pos radius pile]
  (let [names (vec (distinct (map :name pile)))
        total (reduce + 0 (map :count pile))]
    (if (>= (recovered-items c pile) total)
      (finish-collect! c pile [])
      (let [ids (pile-ids c pos names radius)]
        (if (empty? ids)
          (finish-collect! c pile ids)
          (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                         {:radius (+ radius stray-range 4) :ids ids :filter names :visible-only true}))]
            (cond
              (= :continue r) :continue
              (empty? (left-over c pile)) (finish-collect! c pile [])
              :else
              (let [again (pile-ids c pos names radius)
                    passes (:passes (ctx/mem c) 0)]
                (if (and (seq again) (< passes max-collect-passes))
                  (do (ctx/update-mem! c assoc :passes (inc passes) :phase :go) (save-trip! c) :continue)
                  (finish-collect! c pile again))))))))))

(defn ^:async round [c]
  (let [{:keys [margin danger-radius collect-radius]} (:args c)
        entry (died/unrecovered-death (ctx/view c))
        elapsed (- (ctx/now c) (:t entry))
        threatened? (some? (reach/nearest-danger (:primitives c) danger-radius {} {}))]
    (when entry (key-to-death! c entry))
    (cond
      (nil? entry) :done
      (>= elapsed game/despawn-ms) (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason (if (:blocked (ctx/mem c)) :unreachable :window-closed)))
      threatened? :continue
      (not (died/respawned-since? (ctx/view c) entry)) :continue
      (and (nil? (:decided (ctx/mem c))) (settling? c entry)) :continue
      :else
      (let [decided (or (:decided (ctx/mem c))
                        (let [e (update (estimate c (:data entry) elapsed) :cost finite)
                              fetch? (> (:value e) (+ (if (= :infinite (:cost e)) js/Infinity (:cost e)) margin))]
                          (ctx/update-mem! c assoc :decided e :baseline (carried-counts c))
                          (save-trip! c)
                          (when fetch?
                            (ctx/emit! c :recover-drops.fetching :info
                                       (assoc (select-keys e [:value :cost :parts :top-items :top-mobs])
                                              :pos (:pos (:data entry)) :text (decision-text :fetch e margin))))
                          e))
            pos (:pos (:data entry))]
        (cond
          (<= (:value decided) (+ (if (= :infinite (:cost decided)) js/Infinity (:cost decided)) margin))
          (finish! c :skip decided)

          (and (= :collect (:phase (ctx/mem c))) (> (u/dist (u/self-pos c) pos) stray-range))
          (do (ctx/update-mem! c assoc :phase :go) :continue)

          (= :collect (:phase (ctx/mem c))) (await (collect! c pos collect-radius (:inventory (:data entry))))
          :else (await (go! c pos)))))))
