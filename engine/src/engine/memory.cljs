(ns engine.memory
  "The three memory scopes (common, body, job), each committed to JSON on disk.

  Job memory is a tree of nodes {:mem {} :children {slot node} :done? bool},
  one file per top-level instance, children inside their parent's file."
  (:require [clojure.string :as str]
            [engine.fsutil :as fsu]
            ["fs" :as fs]
            ["path" :as path]))

(def empty-body {:records []})

(defn scope-file [dir scope] (path/join dir (str (name scope) ".json")))
(defn job-file [dir id] (path/join dir "jobs" (str id ".json")))

(defn load-jobs [dir]
  (let [jobs-dir (path/join dir "jobs")]
    (if-not (fs/existsSync jobs-dir)
      {}
      (->> (fs/readdirSync jobs-dir)
           (filter #(str/ends-with? % ".json"))
           (map (fn [f] (let [id (subs f 0 (- (count f) 5))]
                          [id (fsu/read-json (path/join jobs-dir f))])))
           (into {})))))

(defn open
  "Load (or start) the memory under dir. Returns a store atom."
  [dir]
  (atom {:dir dir
         :data {:common (or (fsu/read-json (scope-file dir :common)) {})
                :body (merge empty-body (fsu/read-json (scope-file dir :body)))
                :jobs (load-jobs dir)}}))

(defn path->id [[root & slots]]
  (str/join "/" (cons root (map name slots))))

(defn node-path [[root & slots]]
  (into [:data :jobs root] (mapcat (fn [s] [:children (keyword s)]) slots)))

(defn snapshot
  "The scopes as plain data: {:common :body :jobs}."
  [store]
  (:data @store))

(defn scope [store k] (get-in @store [:data k]))

(defn job [store p] (get-in @store (conj (node-path p) :mem) {}))

(defn done? [store p] (true? (get-in @store (conj (node-path p) :done?))))

(defn apply-change [current m-or-f]
  (if (fn? m-or-f) (m-or-f current) m-or-f))

(defn write-job! [store root]
  (let [{:keys [dir data]} @store]
    (fsu/write-json! (job-file dir root) (get-in data [:jobs root]))))

(defn commit!
  "Replace scope k (:common or :body) with m, or with (f current); write it."
  [store k m-or-f]
  (let [s (swap! store update-in [:data k] apply-change m-or-f)]
    (fsu/write-json! (scope-file (:dir s) k) (get-in s [:data k]))
    (get-in s [:data k])))

(defn commit-job!
  "Replace the memory of the job at path p with m, or with (f current)."
  [store p m-or-f]
  (let [k (conj (node-path p) :mem)]
    (swap! store update-in k #(apply-change (or % {}) m-or-f))
    (write-job! store (first p))
    (get-in @store k)))

(defn mark-done!
  "A child that returned :done keeps only its done flag."
  [store p]
  (swap! store assoc-in (node-path p) {:done? true})
  (write-job! store (first p)))

(defn delete-job! [store id]
  (swap! store update-in [:data :jobs] dissoc id)
  (let [f (job-file (:dir @store) id)]
    (when (fs/existsSync f) (fs/unlinkSync f))))

(defn add-record! [store record]
  (commit! store :body #(update % :records (fnil conj []) record)))

(defn records
  "Records of kind (a string) in a memory snapshot's body scope."
  [memory kind]
  (filterv #(= kind (:kind %)) (get-in memory [:body :records])))

(defn drop-records
  "Body memory without the records of kind; for (ctx/commit! ctx :body ...)."
  [body kind]
  (update body :records #(filterv (fn [r] (not= kind (:kind r))) %)))

(defn places
  "Known places of a kind from common memory, e.g. (places memory :bed)."
  [memory kind]
  (get-in memory [:common :places kind] []))
