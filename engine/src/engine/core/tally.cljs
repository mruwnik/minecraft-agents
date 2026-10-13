(ns engine.core.tally
  "The contract's counting (engine.contract): each act's uses and yields go to every frame of its chain, each round's
  time to its own frame, kept in job memory under :contract so they survive a cut and a restart; a frame that ends
  hands out its report."
  (:require [engine.contract :as contract]
            [engine.hurt :as hurt]
            [engine.memory :as mem]))

(defn snapshot
  "What the body carries and has lost to each hurt cause, for engine.contract/act-delta; no :inventory offline."
  [eng]
  (let [s (.self (:primitives eng))]
    {:inventory (some-> (.-inventory s) (js->clj :keywordize-keys true))
     :equipment (some-> (.-equipment s) (js->clj :keywordize-keys true))
     :hurt (hurt/totals eng)}))

(defn frame-paths
  "The memory paths of every frame from the root down to the job at slots."
  [slots]
  (map #(mem/slot-path (subvec slots 0 %)) (range (inc (count slots)))))

(defn update-frames!
  "Apply (f contract-state) at each of paths that still holds a frame of job root; nothing once root's memory is gone."
  [eng root paths f]
  (let [store (:store eng)]
    (when (seq (mem/job-mem (mem/view store) root []))
      (mem/update-job! store root []
                       (fn [m] (reduce (fn [m path]
                                         (cond-> m (or (empty? path) (get-in m path)) (update-in (conj path :contract) f)))
                                       m paths))))))

(defn count-act!
  "Add what the act since before (a snapshot) used and yielded to every frame from root down to slots."
  [eng root slots before]
  (let [delta (contract/act-delta before (snapshot eng))]
    (when (seq delta)
      (update-frames! eng root (frame-paths slots) #(update % :tally contract/add delta)))))

(defn add-time!
  "Book ms of round time on the frame at slots only: a parent's round already holds its children's."
  [eng root slots ms]
  (when (pos? ms)
    (update-frames! eng root [(mem/slot-path slots)] #(update % :ran-ms (fnil + 0) ms))))

(defn report [eng root slots]
  (contract/report (:contract (mem/job-mem (mem/view (:store eng)) root slots))))

(defn remember!
  "Keep {:job sym :report report} (and :target, the args the job's target names) in body memory :job-report."
  [eng sym target report]
  (when sym
    (mem/write! (:store eng) :job-report (cond-> {:job sym :report report} (seq target) (assoc :target target))
                contract/report-policy)))

(defn end-root!
  "Root job id's report, read before its memory goes; with remember? a leaf job's is also kept in :job-report."
  [eng id remember?]
  (let [r (report eng id [])
        {:keys [op job args]} (get-in @(:state eng) [:instances id :spec])]
    (when (and remember? (= :leaf op))
      (remember! eng job (some->> (get-in eng [:jobs job :target]) (select-keys args)) r))
    r))
