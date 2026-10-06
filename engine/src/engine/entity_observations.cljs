(ns engine.entity-observations
  "In-memory knowledge of the entities the body perceived, as a player would (the /entities tool).
  What was sensed stays known for `ttl-ms` (two minutes). Nothing is written to disk and the engine's own
  reflexes do not read this cache.

  Each entity is listed with how it was sensed:
  - hostile mobs come only from engine.perception's known-mobs memory (`known-sense`): :seen, :heard, or
    :remembered at the place last sensed, with :age-ms
  - everything else goes through perception's rule for a thing at a place (`sense`): a clear line, light and
    the view cone, or hearing. Drops and the like make no sound.
  - a :heard row has no :pos, only :direction (8 compass points) and :band (:near or :far); :seen and :remembered keep
    the exact place.
  - the body itself is :self.
  Mineflayer tracks far more (mobs deep in the rock under the body). Those are never listed."
  (:require [clojure.string :as str]
            [engine.perception :as perception]
            ["crypto" :as crypto]))

(def ttl-ms 120000)
(def sample-ms 1000)
(def max-entities 10000)
(def max-snapshot-bytes (* 4 1024 1024))
(def silent-types
  "Entity types that make no sound a player would hear through a wall."
  #{"item" "experience_orb" "arrow" "spectral_arrow" "painting" "item_frame" "glow_item_frame" "armor_stand"
    "leash_knot" "marker" "item_display" "block_display" "text_display" "interaction" "falling_block"})

(defn short-text [value limit]
  (when (and (string? value) (not (str/blank? value)))
    (subs value 0 (min limit (count value)))))

(defn dimension [value]
  (let [s (short-text value 64)]
    (case s "minecraft:overworld" "overworld" "minecraft:the_nether" "the_nether"
          "minecraft:the_end" "the_end" s)))

(defn object-id! [store table object]
  (when object
    (or (.get table object)
        (let [id (swap! (:next-id store) inc)] (.set table object id) id))))

(defn entity-type [e]
  (if (or (= "player" (.-type e)) (string? (.-username e)))
    "player"
    (or (short-text (.-name e) 64) (short-text (.-type e) 64) "unknown")))

(defn hostile? [^js e]
  (boolean (or (= "hostile" (.-type e)) (re-find #"(?i)hostile" (str (.-kind e))))))

(defn sense
  "How the body perceives entity e, a non-hostile one (hostile mobs: see `known-sense`). Uses
  perception/sense-thing: a clear line from the eye to the entity's middle or head, within sight, lit and in
  the view cone, or heard within hearing range unless it makes no sound. per is the perception (nil: no
  world, so nothing is sensed). Returns :self, :seen, :heard or nil."
  [per ^js source ^js e]
  (let [^js raw (:raw per)
        ^js eye (when raw (.eye raw))
        ^js table (when raw (.sightTable raw))
        ^js p (.-position e)]
    (cond
      (and source (identical? e (.-entity source))) :self
      (or (nil? eye) (nil? p) (nil? table) (zero? (.-length table))) nil
      :else
      (let [h (or (.-height e) 1.8)
            my (+ (.-y p) (/ h 2)) hy (+ (.-y p) (max 0.1 (- h 0.1)))
            clear? (fn [y] (perception/line-clear? raw table (.-x eye) (.-y eye) (.-z eye) (.-x p) y (.-z p)))]
        (perception/sense-thing per eye p (boolean (or (clear? my) (clear? hy))) (boolean (silent-types (entity-type e))))))))

(defn known-sense
  "How the body perceives hostile mob e by perception's known-mobs entry (a knownMobs() row, or nil when the body knows
  nothing of it): :seen, :heard, :remembered (not sensed now: its place is where it was last sensed), or nil."
  [^js entry]
  (cond (nil? entry) nil
        (.-remembered entry) :remembered
        (.-heard entry) :heard
        :else :seen))

(defn open
  "A cache. :sense (fn [source e]) is the perception rule for what is not a hostile mob; :known (fn [] known-mobs rows)
  is perception's mob memory, the only source of hostile mobs; by default (no perception) only the body itself."
  [{:keys [world body now cap known] sense-fn :sense :or {now js/Date.now cap max-entities}}]
  {:state (atom {:entities {} :online? false :available? true :dropped 0 :overflow-until 0})
   :opts {:world world :body body :now now :cap (min max-entities (max 1 cap))
          :sense (or sense-fn (partial sense nil)) :known known
          :session (.randomUUID crypto)}
   :connections (js/WeakMap.) :objects (js/WeakMap.) :dead (js/WeakSet.) :next-id (atom 0)})

(defn entity-uuid [source e]
  (or (short-text (.-uuid e) 80)
      ;; Mineflayer's login entity omits uuid, but its own player-list entry carries it.
      (when (and source (identical? e (.-entity source)))
        (short-text (some-> source .-player .-uuid) 80))))

(defn entity-identity [store source e]
  (let [type (entity-type e) uuid (entity-uuid source e)]
    (if uuid
      {:key (str type "/" uuid) :uuid uuid :identity :uuid}
      {:key (str type "/ephemeral:" (get-in store [:opts :body]) ":" (get-in store [:opts :session]) ":"
                 (object-id! store (:connections store) source) ":" (object-id! store (:objects store) e))
       :identity :ephemeral})))

(defn position [e]
  (let [p (or (.-position e) (.-pos e))]
    (when (and p (every? #(and (number? %) (js/Number.isFinite %)) [(.-x p) (.-y p) (.-z p)]))
      {:x (.-x p) :y (.-y p) :z (.-z p)})))

(def near-band "A heard thing within this many blocks of the body is :near, else :far." 8)
(def directions [:north :north-east :east :south-east :south :south-west :west :north-west])

(defn rough-hearing
  "What a sound tells of where it came from: {:direction one of 8 compass points (north is -z), :band :near or :far}.
  from and to are {:x :y :z}."
  [from to]
  (let [dx (- (:x to) (:x from)) dz (- (:z to) (:z from))
        sector (mod (js/Math.round (/ (js/Math.atan2 dx (- dz)) (/ js/Math.PI 4))) 8)]
    {:direction (nth directions sector)
     :band (if (<= (js/Math.hypot dx (- (:y to) (:y from)) dz) near-band) :near :far)}))

(defn observation [store source dim [e how entry] now]
  (when-let [pos (when-not (.has (:dead store) e) (position (or entry e)))]
    (merge (entity-identity store source e)
           {:type (entity-type e) :id (when (integer? (.-id e)) (.-id e)) :world (get-in store [:opts :world])
            :dimension dim :observed-at (- now (if entry (or (.-ageMs entry) 0) 0))
            :expires-at (+ (- now (if entry (or (.-ageMs entry) 0) 0)) ttl-ms)
            :sense how}
           ;; Heard only: a player hears roughly where, not the exact place.
           (if (= :heard how)
             (rough-hearing (or (position (.-entity source)) pos) pos)
             {:pos pos})
           (when-let [username (short-text (.-username e) 64)] {:username username})
           (when (identical? e (.-entity source)) {:self? true}))))

(defn prune [entities now]
  (into {} (filter (fn [[_ entity]] (> (:expires-at entity) now))) entities))

(defn observe!
  "Apply a local provider sample (every tracked entity), keeping only what the body senses. Offline/stalled samples
  never refresh observations."
  [store sample now]
  (let [online? (true? (.-online sample)) dim (dimension (.-dimension sample))
        source (.-source sample)
        sense-of (get-in store [:opts :sense])
        tracked (when (and online? source dim) (array-seq (.-entities sample)))
        known (when-let [f (get-in store [:opts :known])]
                (when tracked (into {} (map (fn [^js m] [(.-id m) m])) (f))))
        sense-entry (fn [e]
                      (if (hostile? e)
                        (let [entry (get known (.-id e))] [e (known-sense entry) entry])
                        [e (sense-of source e)]))
        senses (map sense-entry tracked)
        loaded (keep (fn [[e how entry]] (when how [e how entry])) senses)
        ;; Still loaded but no longer sensed (walked behind a wall, teleported away): the row would be a ghost.
        lost (into #{} (keep (fn [[e how]] (when-not how (:key (entity-identity store source e))))) senses)
        cap (get-in store [:opts :cap])
        incoming (keep #(observation store source dim % now) (take cap loaded))
        prior (apply dissoc (prune (:entities @(:state store)) now) lost)
        merged (reduce (fn [m entity] (assoc m (:key entity) entity)) prior incoming)
        dropped (+ (max 0 (- (count loaded) cap)) (max 0 (- (count merged) cap)))
        kept (if (> (count merged) cap)
               (into {} (take cap (sort-by (fn [[key e]] [(- (:observed-at e)) key]) merged))) merged)]
    (swap! (:state store)
           (fn [state]
             (cond-> (assoc state :entities kept :online? online? :available? true)
               (pos? dropped) (assoc :dropped dropped :overflow-until (+ now ttl-ms)))))
    nil))

(defn dead!
  "Forget an entity early. An entityDead sample also keeps it from being resampled.
  A :removal sample (entityGone or a pickup) forgets only dropped items and does not block a resample, so a
  partly collected stack that is still loaded comes back. Any other unload keeps the last observation."
  [store sample]
  (let [e (.-entity sample) source (.-source sample)
        dim (dimension (.-dimension sample))
        removal? (some? (.-removal sample))
        known? (when e (or (entity-uuid source e) (.get (:objects store) e)))]
    (when (and e source dim (or (not removal?) (= "item" (entity-type e))))
      (when-not removal? (.add (:dead store) e))
      (when known?
        (let [key (:key (entity-identity store source e))]
          (when (= dim (get-in @(:state store) [:entities key :dimension]))
            (swap! (:state store) update :entities dissoc key))))))
  nil)

(defn bounded-entities [entities]
  (loop [[entity & more] entities bytes 1024 kept []]
    (if entity
      (let [n (.byteLength js/Buffer (pr-str entity) "utf8")]
        (if (> (+ bytes n 1) max-snapshot-bytes)
          {:entities kept :dropped (inc (count more))}
          (recur more (+ bytes n 1) (conj kept entity))))
      {:entities kept :dropped 0})))

(defn snapshot
  ([store] (snapshot store ((get-in store [:opts :now]))))
  ([store now]
   (swap! (:state store) update :entities prune now)
   (let [state @(:state store)
         values (->> (vals (:entities state))
                     (map #(assoc % :age-ms (max 0 (- now (:observed-at %)))))
                     (sort-by (juxt :dimension :key)))
         bounded (bounded-entities values)
         dropped (+ (:dropped bounded) (if (> (:overflow-until state) now) (:dropped state) 0))]
     {:ok true :world (get-in store [:opts :world]) :body (get-in store [:opts :body])
      :now now :ttl-ms ttl-ms :online? (:online? state)
      :entities (:entities bounded) :count (count (:entities bounded)) :cached-count (count values)
      :cap (get-in store [:opts :cap]) :snapshot-cap-bytes max-snapshot-bytes
      :truncated? (pos? dropped) :dropped dropped})))

(defn request [store method]
  (let [value (cond (not= "GET" method) {:ok false :reason :method-not-allowed}
                    (or (nil? store) (false? (:available? @(:state store))))
                    {:ok false :reason :entities-unavailable :error (when store (:error @(:state store)))}
                    :else (snapshot store))]
    #js {:status (cond (not= "GET" method) 405 (or (nil? store) (false? (:available? @(:state store)))) 503 :else 200)
         :contentType "application/edn" :text (str (pr-str value) "\n")}))

(defn start!
  "Register once for body lifetime. A missing/broken provider reports endpoint503 without failing body jobs."
  [primitives options]
  (let [store (open (merge {:sense (partial sense (.-perception primitives))
                          :known (when (fn? (.-knownMobs primitives)) #(array-seq (.knownMobs primitives)))} options)) now (get-in store [:opts :now])
        sample! (fn []
                  (try
                    (when-not (fn? (.-entityObservation primitives))
                      (throw (js/Error. "local entity observation provider unavailable")))
                    (observe! store (.entityObservation primitives) (now))
                    (catch :default e
                      (swap! (:state store) assoc :available? false :online? false
                             :error (short-text (ex-message e) 180)))))
        unlisten (try (if (fn? (.-onEntityDeath primitives))
                        (.onEntityDeath primitives #(dead! store %)) (fn []))
                      (catch :default _ (fn [])))]
    (sample!)
    (let [timer (js/setInterval sample! sample-ms)]
      (.unref timer)
      (assoc store :stop (fn [] (js/clearInterval timer) (unlisten))))))
