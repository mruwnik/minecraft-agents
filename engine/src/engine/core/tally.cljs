(ns engine.core.tally
  "The contract's counting (engine.contract): each act's uses and yields go to every frame of its chain, each round's
  time to its own frame, kept in job memory under :contract so they survive a cut and a restart; a frame that ends
  hands out its report."
  (:require [engine.contract :as contract]
            [engine.core.clock :refer [ttl-now]]
            [engine.hurt :as hurt]
            [engine.memory :as mem]
            [engine.settings :as settings]))

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

(def settle-ticks
  "Physics steps an act's item uses wait before they are read again: the client's inventory view can read short for a
  moment after a container closes (the server's resync lands after the act returned)."
  3)

(defn add-delta!
  [eng root slots delta]
  (when (seq delta)
    (update-frames! eng root (frame-paths slots) #(update % :tally contract/add delta))))

(defn count-gap!
  "Book as yields what the bag gained between the last act's end and this act's start (before, a snapshot): a pickup the
  inventory view showed only after the act returned, or made while no act ran, is in no act's delta."
  [eng root slots before]
  (let [last-bag @(:bag eng)]
    (when (and last-bag (:inventory before))
      (add-delta! eng root slots (contract/act-delta {:inventory last-bag} {:inventory (:inventory before)})))))

(defn count-act!
  "Add what the act since before (a snapshot) used and yielded to every frame from root down to slots, from the bag as it reads now."
  [eng root slots before]
  (let [after (snapshot eng)]
    (reset! (:bag eng) (:inventory after))
    (add-delta! eng root slots (contract/act-delta before after))))

(defn beat
  "A promise that resolves after ms of wall clock: the wait for the client's inventory to settle (a test stands in a next turn)."
  [ms]
  (js/Promise. (fn [ok] (js/setTimeout ok ms))))

(defn ^:async count-act-settled!
  "count-act! for an act that returned: when it seems to have used items, the bag is read again after a beat and that
  read counts. Not for a failed or cut act, which must not wait."
  [eng root slots before]
  (let [first-after (snapshot eng)
        first-delta (contract/act-delta before first-after)
        [after delta] (if (seq (get-in first-delta [:uses :items]))
                        (do (await (beat (* settle-ticks (settings/physics-ms (:primitives eng)))))
                            (let [a (snapshot eng)] [a (contract/act-delta before a)]))
                        [first-after first-delta])]
    (reset! (:bag eng) (:inventory after))
    (add-delta! eng root slots delta)))

(defn count-trip!
  "Book an act's walk or dig on every frame from root down to slots (the :trip of the report): from the position before it, to
  where it ended; dug? when it dug a cell."
  [eng root slots from to dug?]
  (when (or from to)
    (update-frames! eng root (frame-paths slots) #(update % :trip contract/add-trip from to dug?))))

(defn count-try!
  "Book a child ending (reason of job) as a try on every frame from root down to slots (the :tries spend line), the
  last one kept for the cause; a frame of a job that counts its own tries (meta :own-tries?) is left out."
  [eng root slots job reason]
  (update-frames! eng root (frame-paths slots)
                  #(if (get-in eng [:jobs (:job %) :own-tries?])
                     %
                     (-> % (update-in [:tally :uses :tries] (fnil inc 0)) (assoc :last-try {:job job :reason reason})))))

(defn add-time!
  "Book ms of round time on the frame at slots only: a parent's round already holds its children's."
  [eng root slots ms]
  (when (pos? ms)
    (update-frames! eng root [(mem/slot-path slots)] #(update % :ran-ms (fnil + 0) ms))))

(defn add-time-chain!
  "Book ms of waiting on every frame from root down to slots (a wait is in no round, so no parent's round holds it)."
  [eng root slots ms]
  (when (pos? ms)
    (update-frames! eng root (frame-paths slots) #(update % :ran-ms (fnil + 0) ms))))

(defn report [eng root slots]
  (contract/report (:contract (mem/job-mem (mem/view (:store eng)) root slots)) (ttl-now eng)))

(defn remember!
  "Keep {:job sym :report report} (and :target, the args the job's target names, and the :kind and :cause of its
  ending e, when given) in body memory :job-report."
  ([eng sym target report] (remember! eng sym target report nil))
  ([eng sym target report e]
   (when sym
     (mem/write! (:store eng) :job-report (cond-> {:job sym :report report}
                                            (seq target) (assoc :target target)
                                            e (merge (select-keys e [:kind :cause])))
                 contract/report-policy))))

(defn end-root!
  "Root job id's report, read before its memory goes; with remember? a leaf job's is also kept in :job-report, with
  its ending e when given."
  ([eng id remember?] (end-root! eng id remember? nil))
  ([eng id remember? e]
   (let [r (report eng id [])
         {:keys [op job args]} (get-in @(:state eng) [:instances id :spec])]
     (when (and remember? (= :leaf op))
       (remember! eng job (some->> (get-in eng [:jobs job :target]) (select-keys args)) r e))
     r)))
