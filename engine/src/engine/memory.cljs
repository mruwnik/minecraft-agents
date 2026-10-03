(ns engine.memory
  "Body memory: one EDN store per body, in memory.edn under the engine dir.

  The data is {:entries {kind [entry]} :policies {kind policy}}: each kind
  maps to a vector of entries {:t wall-clock-ms :wt world-time :data ...},
  newest last, and its policy {:cap n :ttl ms-or-:forever} is stored beside
  it. A write may pass a policy; a new kind written without one gets
  default-policy. Forever is the keyword :forever, never an omitted ttl.

  Writes change the store in RAM; save! sweeps and writes the file. The
  sweep drops expired entries, kinds with no entries left, and :job/<id>
  kinds whose instance is not live (see open). Reads take a view
  {:data :now} and are time-filtered, so an expired entry is never read even
  before a sweep.

  Job memory: a listed instance owns kind :job/<id> (cap 1, forever). Its
  single entry's data is {:args ... :children {slot child-map}} plus whatever
  the job keeps; children nest the same way."
  (:require [clojure.string :as str]
            [engine.fsutil :as fsu]
            ["path" :as path]))

(def default-policy {:cap 50 :ttl (* 60 60 1000)})

(def job-policy {:cap 1 :ttl :forever})

(def empty-data {:entries {} :policies {}})

(defn file [dir] (path/join dir "memory.edn"))

;; ------------------------------------------------------------------ pure

(defn valid-policy? [{:keys [cap ttl]}]
  (and (int? cap) (pos? cap)
       (or (= :forever ttl) (and (number? ttl) (pos? ttl)))))

(defn check-policy! [kind policy]
  (when-not (valid-policy? policy)
    (throw (ex-info (str "bad memory policy for " kind ": " (pr-str policy)
                         "; it needs :cap n and :ttl ms or :forever")
                    {:kind kind :policy policy}))))

(defn expired? [policy now entry]
  (and (not= :forever (:ttl policy))
       (>= (- now (:t entry)) (:ttl policy))))

(defn add-entry
  "data with entry appended to kind, under policy (nil keeps the kind's own,
  or the default for a new kind), capped to the newest :cap entries."
  [data kind entry policy]
  (let [policy (or policy (get-in data [:policies kind]) default-policy)]
    (-> data
        (assoc-in [:policies kind] policy)
        (update-in [:entries kind] #(vec (take-last (:cap policy) (conj (vec %) entry)))))))

(defn remove-kind [data kind]
  (-> data
      (update :entries dissoc kind)
      (update :policies dissoc kind)))

(defn remove-entries
  "data without the entries of kind for which (pred entry) holds."
  [data kind pred]
  (if (contains? (:entries data) kind)
    (update-in data [:entries kind] #(filterv (complement pred) %))
    data))

(defn job-kind [id] (keyword "job" id))

(defn job-kind? [kind] (= "job" (namespace kind)))

(defn sweep
  "data without expired entries, empty kinds, and job kinds whose id is not
  in live-jobs (a set; nil keeps every job kind)."
  [data now live-jobs]
  (let [dead-job? (fn [kind] (and live-jobs (job-kind? kind) (not (live-jobs (name kind)))))
        swept (reduce-kv (fn [d kind entries]
                           (let [policy (get-in data [:policies kind] default-policy)
                                 kept (filterv #(not (expired? policy now %)) entries)]
                             (if (or (empty? kept) (dead-job? kind))
                               (remove-kind d kind)
                               (assoc-in d [:entries kind] kept))))
                         data
                         (:entries data))]
    (update swept :policies select-keys (keys (:entries swept)))))

;; ------------------------------------------------------------------ reads

(defn policy [view kind]
  (get-in view [:data :policies kind]))

(defn entries
  "The unexpired entries of kind, oldest first."
  [{:keys [data now]} kind]
  (let [p (get-in data [:policies kind] default-policy)]
    (filterv #(not (expired? p now %)) (get-in data [:entries kind]))))

(defn latest [view kind] (peek (entries view kind)))

(defn since
  "Entries of kind written at or after wall-clock t."
  [view kind t]
  (filterv #(>= (:t %) t) (entries view kind)))

(defn count-in
  "How many entries of kind were written within the last ms."
  [view kind ms]
  (count (filter #(> (:t %) (- (:now view) ms)) (entries view kind))))

(def place-policy
  "Known places (:bed, :chest): the latest one, kept until replaced."
  {:cap 1 :ttl :forever})

(defn place
  "The position of the known place of kind (:bed, :chest), or nil. Written
  as (write! store :bed {:pos pos} place-policy)."
  [view kind]
  (:pos (:data (latest view kind))))

(defn slot-path
  "The get-in path of a job's sub-map for slots [s1 s2 ...]."
  [slots]
  (vec (mapcat (fn [s] [:children s]) slots)))

(defn job-mem
  "The memory map of job root-id's descendant at slots ([] for the job itself);
  {} when there is none."
  [view root-id slots]
  (or (get-in (:data (latest view (job-kind root-id))) (slot-path slots)) {}))

(defn path->id [root-id slots]
  (str/join "/" (cons root-id (map name slots))))

;; ------------------------------------------------------------------ the store

(defn open
  "Load (or start) the memory under dir and sweep it. Options: :now (ms
  clock), :world-time (fn, the :wt of new entries), :live-jobs (fn returning
  the set of live instance ids, for the sweep; nil keeps every job kind)."
  [dir {:keys [now world-time live-jobs] :or {now js/Date.now world-time (constantly nil)}}]
  (let [loaded (or (fsu/read-edn (file dir)) empty-data)]
    (atom {:dir dir :now now :world-time world-time :live-jobs live-jobs
           :data (sweep loaded (now) (when live-jobs (live-jobs)))})))

(defn view
  "A read view of the store now: {:data :now}."
  [store]
  (let [{:keys [data now]} @store]
    {:data data :now (now)}))

(defn write!
  "Append an entry with data to kind. policy is optional; see the ns doc."
  ([store kind data] (write! store kind data nil))
  ([store kind data policy]
   (when policy (check-policy! kind policy))
   (let [{:keys [now world-time]} @store
         entry {:t (now) :wt (world-time) :data data}]
     (swap! store update :data add-entry kind entry policy)
     entry)))

(defn forget! [store kind]
  (swap! store update :data remove-kind kind))

(defn forget-until!
  "Drop the entries of kind written at or before t; later ones stay."
  [store kind t]
  (swap! store update :data remove-entries kind #(<= (:t %) t)))

(defn forget-where!
  "Drop the entries of kind whose data matches pred."
  [store kind pred]
  (swap! store update :data remove-entries kind #(pred (:data %))))

(defn save!
  "Sweep, then write memory.edn."
  [store]
  (let [{:keys [dir now live-jobs]} @store
        s (swap! store update :data sweep (now) (when live-jobs (live-jobs)))]
    (fsu/write-edn! (file dir) (:data s))))

(defn create-job!
  "The memory of a newly listed instance: {:args args :children {}}."
  [store id args]
  (write! store (job-kind id) {:args args :children {}} job-policy))

(defn update-job!
  "Apply (f current & args) to the sub-map at slots of job root-id."
  [store root-id slots f & args]
  (let [root (job-mem (view store) root-id [])
        updated (if (empty? slots)
                  (apply f root args)
                  (apply update-in root (slot-path slots) (fnil f {}) args))]
    (write! store (job-kind root-id) updated job-policy)))

(defn delete-job!
  "Drop job id's memory, its children's included."
  [store id]
  (forget! store (job-kind id)))
