(ns agent-tools.observe.lock
  "The per-observer lock directory and checkpoint file observe --wait keeps under worlds/<world>/observers/."
  (:require [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:path" :as path]
            [clojure.string :as str]
            [agent-tools.observe.status :refer [code-of coded id-str]]))

(def private-dir-mode 448)  ; 0700
(def private-file-mode 384) ; 0600

;; Observer lock and checkpoint

;; a lock dir with no pid file this old was left by a process that died between mkdir and writing its pid
(def pidless-lock-stale-ms 5000)

(defn process-alive?
  "kill -0: ESRCH means gone, EPERM means it exists under another user."
  ([pid] (process-alive? pid #(.kill js/process % 0)))
  ([pid kill!]
   (try (kill! pid) true
        (catch :default e
          (case (code-of e) "ESRCH" false "EPERM" true (throw e))))))

(defn lock-owner-gone?
  "True when the lock dir at `dir` belongs to nobody: its pid is dead, or it never got a pid and is old."
  [dir]
  (let [pid (try (js/Number (.readFileSync fs (.join path dir "pid") "utf8")) (catch :default _ nil))
        pid? (and pid (js/Number.isInteger pid) (not= 0 pid))]
    (if pid?
      (not (process-alive? pid))
      (>= (- (js/Date.now) (.-mtimeMs (.statSync fs dir))) pidless-lock-stale-ms))))

(defn take-reclaim-mutex!
  "Reclaimers take turns through a mutex dir beside the lock. One left by a crashed reclaimer (older than
  pidless-lock-stale-ms) is renamed away atomically, then taken again. Throws EEXIST while another reclaimer works."
  [mutex]
  (let [take! #(.mkdirSync fs mutex #js {:mode private-dir-mode})]
    (try (take!)
         (catch :default error
           (when-not (= "EEXIST" (code-of error)) (throw error))
           (let [old? (try (>= (- (js/Date.now) (.-mtimeMs (.statSync fs mutex))) pidless-lock-stale-ms)
                           (catch :default _ false))]
             (when-not old? (throw error))
             (try (.renameSync fs mutex (str mutex ".dead-" (.-pid js/process)))
                  (catch :default e (when-not (= "ENOENT" (code-of e)) (throw e))))
             (.rmSync fs (str mutex ".dead-" (.-pid js/process)) #js {:recursive true :force true})
             (take!))))))

(defn reclaim-stale-lock!
  "Replace a dead owner's lock with a fresh one. Reclaimers are serialised by a mutex dir, and a lock held by a live
  owner is never moved: only a dead owner's dir (which nobody else may touch meanwhile) is removed. Throws EEXIST
  when the lock is live or another reclaimer or observer got there first. `step!` is a test hook called at each stage."
  ([lock] (reclaim-stale-lock! lock (fn [_])))
  ([lock step!]
   (let [mutex (str lock ".reclaim")]
     (take-reclaim-mutex! mutex)
     (try
       (step! :mutex-held)
       (when (.existsSync fs lock)
         (when-not (lock-owner-gone? lock) (throw (coded "EEXIST" "lock is held")))
         (.rmSync fs lock #js {:recursive true :force true}))
       (step! :lock-removed)
       (.mkdirSync fs lock #js {:mode private-dir-mode})
       (finally (.rmSync fs mutex #js {:recursive true :force true}))))))

(defn acquire!
  "Take the observer's lock directory; the function that gives it back. Throws EOBSERVERBUSY while a live process holds it."
  [dir observer]
  (.mkdirSync fs dir #js {:recursive true :mode private-dir-mode})
  (let [lock (.join path dir (str observer ".lock"))
        busy #(coded "EOBSERVERBUSY" "observer busy")]
    (try (.mkdirSync fs lock #js {:mode private-dir-mode})
         (catch :default error
           (when-not (= "EEXIST" (code-of error)) (throw error))
           (when-not (lock-owner-gone? lock) (throw (busy)))
           (try (reclaim-stale-lock! lock)
                (catch :default e
                  (throw (if (= "EEXIST" (code-of e)) (busy) e))))))
    (try (.writeFileSync fs (.join path lock "pid") (str (.-pid js/process)) #js {:mode private-file-mode})
         (catch :default error
           (throw (if (= "ENOENT" (code-of error)) (busy) error))))
    #(.rmSync fs lock #js {:recursive true :force true})))

(defn checkpoint! [file {:keys [cursor generation seen pending lookup cancelled]}]
  (let [temp (str file "." (.-pid js/process) ".tmp")
        state (cond-> (array-map :cursor cursor :generation generation
                                 :seen (into (array-map) (map (fn [[id sig]] [(keyword id) sig])) seen))
                (some? pending) (assoc :pending pending)
                (seq cancelled) (assoc :cancelled cancelled)
                lookup (assoc :lookup true))]
    (.writeFileSync fs temp (str (data/write-edn state) "\n") #js {:mode private-file-mode})
    (.renameSync fs temp file)))

(defn saved-checkpoint [file]
  (when (.existsSync fs file)
    (let [saved (data/read-edn (.readFileSync fs file "utf8"))]
      (update saved :seen #(into {} (map (fn [[id sig]] [(id-str id) sig])) %)))))

(defn observer-count [dir]
  (->> (array-seq (.readdirSync fs dir))
       (filter #(or (str/ends-with? % ".edn") (str/ends-with? % ".lock")))
       (map #(str/replace % #"\.(edn|lock)$" ""))
       set count))
