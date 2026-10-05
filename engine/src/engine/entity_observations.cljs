(ns engine.entity-observations
  "Two-minute in-memory knowledge of the entities the body perceived, as a player would. No writes, jobs or network
  sensing. A sample keeps an entity only when the body senses it (see `sense`): itself, one it hears (within
  `hearing-range` of the eye, through walls; not silent things such as drops), or one it could see by turning to it
  (within `sight-range`, a clear line from the eye to its middle or its head). Mineflayer tracks far more (mobs deep in
  the rock under the body); those are never listed. What was sensed stays known for `ttl-ms`. The engine's own
  reflexes do not read this cache (they use primitives.entities)."
  (:require [clojure.string :as str]
            [engine.perception :as perception]
            ["crypto" :as crypto]))

(def ttl-ms 120000)
(def sample-ms 1000)
(def max-entities 10000)
(def max-snapshot-bytes (* 4 1024 1024))
(def hearing-range 16)
(def sight-range 64)
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

(defn sense
  "How the body perceives entity e over the raw world (engine.perception's reader; nil when there is none):
  :self, :seen, :heard, or nil when a player standing there could neither see nor hear it."
  [^js raw ^js source ^js e]
  (let [^js eye (when raw (.eye raw))
        ^js table (when raw (.sightTable raw))
        ^js p (.-position e)]
    (cond
      (and source (identical? e (.-entity source))) :self
      (or (nil? eye) (nil? p)) nil
      :else
      (let [h (or (.-height e) 1.8)
            mx (.-x p) mz (.-z p) my (+ (.-y p) (/ h 2)) hy (+ (.-y p) (max 0.1 (- h 0.1)))
            d (js/Math.hypot (- mx (.-x eye)) (- my (.-y eye)) (- mz (.-z eye)))
            seen? (fn [y] (perception/line-clear? raw table (.-x eye) (.-y eye) (.-z eye) mx y mz))]
        (cond
          (and table (pos? (.-length table)) (<= d sight-range) (or (seen? my) (seen? hy))) :seen
          (and (<= d hearing-range) (not (silent-types (entity-type e)))) :heard
          :else nil)))))

(defn open
  "A cache. :sense (fn [source e]) is the perception rule; by default (no raw world) only the body itself."
  [{:keys [world body now cap] sense-fn :sense :or {now js/Date.now cap max-entities}}]
  {:state (atom {:entities {} :online? false :available? true :dropped 0 :overflow-until 0})
   :opts {:world world :body body :now now :cap (min max-entities (max 1 cap))
          :sense (or sense-fn (partial sense nil))
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
  (let [p (.-position e)]
    (when (and p (every? #(and (number? %) (js/Number.isFinite %)) [(.-x p) (.-y p) (.-z p)]))
      {:x (.-x p) :y (.-y p) :z (.-z p)})))

(defn observation [store source dim [e how] now]
  (when-let [pos (when-not (.has (:dead store) e) (position e))]
    (merge (entity-identity store source e)
           {:type (entity-type e) :id (when (integer? (.-id e)) (.-id e)) :world (get-in store [:opts :world])
            :dimension dim :pos pos :observed-at now :expires-at (+ now ttl-ms)
            :sense how}
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
        senses (map (fn [e] [e (sense-of source e)]) tracked)
        loaded (keep (fn [[e how]] (when how [e how])) senses)
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
  "An explicit entityDead removes knowledge early and keeps the entity from being resampled. A :removal
  sample (server entityGone or a pickup's collect) forgets only dropped items, without suppressing a
  resample: a partly collected stack that is still loaded comes back with the next sample. Ordinary
  unload of anything else retains its last observation."
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
         values (sort-by (juxt :dimension :key) (vals (:entities state)))
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
  (let [store (open (merge {:sense (partial sense (.-rawWorld primitives))} options)) now (get-in store [:opts :now])
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
