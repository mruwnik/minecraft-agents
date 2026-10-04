(ns engine.entity-observations
  "Two-minute in-memory knowledge of server-tracked entities. No writes, jobs or network sensing."
  (:require [clojure.string :as str]
            ["crypto" :as crypto]))

(def ttl-ms 120000)
(def sample-ms 1000)
(def max-entities 10000)
(def max-snapshot-bytes (* 4 1024 1024))

(defn short-text [value limit]
  (when (and (string? value) (not (str/blank? value)))
    (subs value 0 (min limit (count value)))))

(defn dimension [value]
  (let [s (short-text value 64)]
    (case s "minecraft:overworld" "overworld" "minecraft:the_nether" "the_nether"
          "minecraft:the_end" "the_end" s)))

(defn open [{:keys [world body now cap] :or {now js/Date.now cap max-entities}}]
  {:state (atom {:entities {} :online? false :available? true :dropped 0 :overflow-until 0})
   :opts {:world world :body body :now now :cap (min max-entities (max 1 cap))
          :session (.randomUUID crypto)}
   :connections (js/WeakMap.) :objects (js/WeakMap.) :dead (js/WeakSet.) :next-id (atom 0)})

(defn object-id! [store table object]
  (when object
    (or (.get table object)
        (let [id (swap! (:next-id store) inc)] (.set table object id) id))))

(defn entity-type [e]
  (if (or (= "player" (.-type e)) (string? (.-username e)))
    "player"
    (or (short-text (.-name e) 64) (short-text (.-type e) 64) "unknown")))

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

(defn observation [store source dim e now]
  (when-let [pos (when-not (.has (:dead store) e) (position e))]
    (merge (entity-identity store source e)
           {:type (entity-type e) :id (when (integer? (.-id e)) (.-id e)) :world (get-in store [:opts :world])
            :dimension dim :pos pos :observed-at now :expires-at (+ now ttl-ms)}
           (when-let [username (short-text (.-username e) 64)] {:username username})
           (when (identical? e (.-entity source)) {:self? true}))))

(defn prune [entities now]
  (into {} (filter (fn [[_ entity]] (> (:expires-at entity) now))) entities))

(defn observe!
  "Apply an unfiltered local provider sample. Offline/stalled samples never refresh observations."
  [store sample now]
  (let [online? (true? (.-online sample)) dim (dimension (.-dimension sample))
        source (.-source sample)
        loaded (when (and online? source dim) (array-seq (.-entities sample)))
        cap (get-in store [:opts :cap])
        incoming (keep #(observation store source dim % now) (take cap loaded))
        prior (prune (:entities @(:state store)) now)
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
  "Only an explicit entityDead removes knowledge early; ordinary unload retains its last observation."
  [store sample]
  (let [e (.-entity sample) source (.-source sample)
        dim (dimension (.-dimension sample))
        known? (when e (or (entity-uuid source e) (.get (:objects store) e)))]
    (when (and e source dim)
      (.add (:dead store) e)
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
  (let [store (open options) now (get-in store [:opts :now])
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
