(ns engine.migrate
  "Migrating an old bot's state into the engine's structure. Pure: the caller
  (engine.migrate-cli) reads and parses the files and passes the data in.

  (convert {:name :config :places :events :now :places-mtime}) returns
  {:memory memory.edn data (always; empty-data when there is nothing to carry), :pose pose.json map or nil,
   :beds n, :chests n  (entries carried over),
   :beds-dropped [..], :chests-dropped [..]  (earlier entries that lost),
   :skipped [{:file :error}], :world w}.

  A file that failed to parse is passed as {:parse-error msg} instead of its
  data; it is skipped and named in :skipped, the rest converts. A missing file
  is nil and is not an error.

  Every converted body gets a memory (empty when it has no bed or chest) so
  that it has an engine/ dir, which the dashboard needs to show it.

  Carried: places of kind \"bed\" and \"chest\" whose :by equals the body's
  name, as memory kinds :bed and :chest. Both are cap 1 under
  mem/place-policy, so the last one in file order wins and the earlier ones
  are listed as dropped. Each entry is {:t t :wt 0 :data {:pos {:x :y :z}}}:
  :t is :places-mtime (else :now), when the old places file was last written,
  and :wt is 0 (the old files carry no world time). The place policy has
  :ttl :forever, so the entries are never expired and the first start's sweep
  keeps them whatever :t is.

  Pose: the offline record from the last event (file order) whose :pos or
  :position is a map of numeric x y z, with :t that event's time in ms and the
  world from the config and the event's :dimension when it has one (the old events that do all say overworld; a nether position would carry the_nether); none when there is no such event or no world. Not
  carried: everything else (jobs, journal, watches, other places, events)."
  (:require [engine.memory :as mem]))

(def carried-kinds {"bed" :bed "chest" :chest})

(defn parse-error? [x] (and (map? x) (contains? x :parse-error)))

(defn num-pos
  "{:x :y :z} from m when all three are finite numbers, else nil."
  [m]
  (when (map? m)
    (let [{:keys [x y z]} m]
      (when (every? #(and (number? %) (js/isFinite %)) [x y z])
        {:x x :y y :z z}))))

(defn place-entry [{:keys [x y z]}] {:x x :y y :z z})

(defn carried-places
  "{:bed [pos ...] :chest [pos ...]} in file order, for places of this body."
  [places name]
  (->> places
       (filter #(and (map? %) (= name (:by %)) (carried-kinds (:kind %)) (num-pos %)))
       (reduce (fn [acc p] (update acc (carried-kinds (:kind p)) (fnil conj []) (place-entry p))) {})))

(defn memory-for [carried t]
  (reduce-kv (fn [data kind positions]
               (mem/add-entry data kind {:t t :wt 0 :data {:pos (peek positions)}} mem/place-policy))
             mem/empty-data
             carried))

(defn event-pose
  "The pose of the last event with a numeric position, or nil. It carries
  :dimension (as in pose.json) when that event has one."
  [events world]
  (let [found (->> events
                   (keep (fn [e]
                           (when (map? e)
                             (let [p (or (num-pos (:pos e)) (num-pos (:position e)))
                                   t (when (string? (:t e)) (js/Date.parse (:t e)))]
                               (when (and p t (not (js/isNaN t))) [t p (:dimension e)])))))
                   last)]
    (when (and found world)
      (let [[t p dimension] found]
        (cond-> {:v 1 :t t :world world :status "offline" :pos p}
          (string? dimension) (assoc :dimension dimension))))))

(defn convert [{:keys [name config places events now places-mtime]}]
  (let [inputs {"config.json" config "places.json" places "events.jsonl" events}
        skipped (vec (for [[file v] inputs :when (parse-error? v)] {:file file :error (:parse-error v)}))
        usable (fn [v] (when-not (parse-error? v) v))
        carried (carried-places (usable places) name)
        dropped (fn [kind] (vec (butlast (get carried kind))))]
    {:name name
     :world (:world (usable config))
     :memory (memory-for carried (or places-mtime now))
     :pose (event-pose (usable events) (:world (usable config)))
     :beds (if (:bed carried) 1 0)
     :chests (if (:chest carried) 1 0)
     :beds-dropped (dropped :bed)
     :chests-dropped (dropped :chest)
     :skipped skipped}))
