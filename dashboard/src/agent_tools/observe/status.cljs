(ns agent-tools.observe.status
  "Reading a body's events-socket data into the compact status, attention items and wake classification observe prints."
  (:require ["node:crypto" :as crypto]
            [agent-tools.world-data :as data]
            [agent-tools.job-results :as job-results]
            [clojure.string :as str]))

;; Small helpers

(defn clean-pairs
  "An array map of the given keys and values (flat), in order, without the nil values."
  [& kvs]
  (apply array-map (mapcat identity (remove (comp nil? second) (partition 2 kvs)))))

(defn clean [m] (apply clean-pairs (mapcat identity m)))

(defn clip
  ([s] (clip s 240))
  ([s limit] (if (string? s) (subs s 0 (min limit (count s))) s)))

(defn round1 [n] (/ (js/Math.round (* n 10)) 10))

(defn position [pos] (when pos (mapv #(round1 (% pos)) [:x :y :z])))

(defn id-str [k] (str (data/name k)))

(defn truthy-text? [text] (and (some? text) (not= "" text)))

(defn coded [code message]
  (doto (js/Error. message) (aset "code" code)))

(defn code-of [error] (aget error "code"))

;; Compact status

(defn job-view
  "A job row; :waiting is why its check declines (the engine's reason map), when it waits."
  [item]
  (clean-pairs :id (:id item) :name (clip (:name item) 120) :status (:status item) :reflex (:reflex item)
               :waiting (:waiting item)))

(defn attention-item [a]
  (clean-pairs :id (:request-id a) :job (:job-id a) :reason (:reason a) :message (clip (:message a))))

(defn failure-view [x]
  (cond-> (array-map)
    (contains? x :id) (assoc :id (:id x))
    (contains? x :error) (assoc :error (clip (:error x)))))

(defn nonzero [n] (when (and (number? n) (not= 0 n) (not (js/Number.isNaN n))) n))

(defn died-view
  "A recent death: where it happened (the drops lie there), the cause when known, seconds since and seconds
  until the drops despawn; once the pile is picked up (:recovered \"collected\") the pile and timer are left out."
  [{:keys [pos cause ago-ms despawns-in-ms recovered]}]
  (let [at (position pos)
        picked-up? (= "collected" (name (or recovered "")))]
    (clean-pairs :at at :cause (clip cause 80) :ago-s (js/Math.round (/ ago-ms 1000))
                 :recovered (when recovered (name recovered))
                 :pile-at (when-not picked-up? at)
                 :despawns-in-s (when-not picked-up? (js/Math.round (/ despawns-in-ms 1000))))))

(defn offline-view
  "The away record with :back-in-s (seconds until a planned return) beside the epoch :back-at."
  [away now]
  (when away
    (cond-> away
      (number? (:back-at away)) (assoc :back-in-s (max 0 (js/Math.round (/ (- (:back-at away) now) 1000)))))))

(defn back-text
  "When a logged-out body is due back, for a person: the local clock and the seconds left, e.g. \"06:31, in 25 s\"."
  [back-at now]
  (when (number? back-at)
    (let [d (js/Date. back-at)
          pad (fn [n] (if (< n 10) (str "0" n) (str n)))]
      (str (pad (.getHours d)) ":" (pad (.getMinutes d)) ", in " (max 0 (js/Math.round (/ (- back-at now) 1000))) " s"))))

(defn compact-status
  "Status without metadata or empty collections: positions rounded, queued jobs apart from the current one."
  ([s] (compact-status s (js/Date.now)))
  ([s now]
  (if (false? (:ok s))
    s
    (let [{:keys [current jobs failed outstanding exclusive]} s
          items (:items jobs)
          queued (remove #(= (:id %) (:id current)) items)
          queued-total (- (or (:total jobs) 0) (if (some #(= (:id %) (:id current)) items) 1 0))]
      (clean-pairs
       :mode (:mode s)
       :offline (offline-view (:offline s) now)
       :last-known (when (:last-known s) true)
       :idle (when-not current true)
       :build (:build s)
       :pos (position (:position s))
       :died (when (:died s) (died-view (:died s)))
       :health (:health s)
       :food (:food s)
       :air (:air s)
       :in-water (when (:in-water s) true)
       :current (when current (job-view current))
       :exclusive (when exclusive (clean-pairs :job (:job exclusive) :name (clip (:name exclusive) 80)))
       :jobs (when (nonzero queued-total)
               (cond-> (array-map :total queued-total :items (mapv job-view queued))
                 (:more? jobs) (assoc :more? true)))
       :failed (when (nonzero (:total failed))
                 (array-map :total (:total failed) :items (mapv failure-view (:items failed))))
       :attention (when (nonzero (:total outstanding))
                    (array-map :total (:total outstanding) :items (mapv attention-item (:items outstanding)))))))))

;; Attention

(defn sha256 [text] (.digest (.update (.createHash crypto "sha256") text) "hex"))

(defn signature
  "Digest of what an attention request says, ignoring when and where it was raised. The EDN text hashed is kept
  byte-identical across versions: observer checkpoints hold these digests."
  [r]
  (let [e (or (:event r) {})]
    (sha256 (data/write-edn {:job (:job-id r) :reason (:reason r) :kind (:kind e) :message (clip (:message e))
                             :data (dissoc (or (:data e) {}) :pos :time-ms)}))))

(def max-attention 4096)
(def max-changed 4)

(defn attention-changes
  "Compare the outstanding requests (id -> request) with the digests already seen (id string -> digest): the new
  seen map, up to four requests that are new or changed, and whether more remain."
  [outstanding seen]
  (when (> (count outstanding) max-attention) (throw (coded "EATTENTIONLIMIT" "too many attention requests")))
  (let [signed (mapv (fn [[id r]] [(id-str id) r (signature r)]) outstanding)
        [next changed] (reduce (fn [[next changed] [id r sig]]
                                 (let [fresh? (and (not= (get seen id) sig) (< (count changed) max-changed))
                                       known (if fresh? sig (get seen id))]
                                   [(cond-> next (some? known) (assoc id known))
                                    (cond-> changed fresh? (conj (attention-item (assoc r :request-id id :message (get-in r [:event :message])))))]))
                               [{} []] signed)]
    {:seen next :changed changed
     :more (boolean (some (fn [[id _ sig]] (not= (get next id) sig)) signed))}))

;; Wake classification and quiet summaries

(defn lower [text] (when (string? text) (str/lower-case text)))

(defn addressed? [e body]
  (let [d (:data e)]
    (or (= :whisper (:kind e)) (true? (:whisper d)) (= (lower (:to d)) (lower body))
        (.test (js/RegExp. (str "(^|[^A-Za-z0-9_])" body "([^A-Za-z0-9_]|$)") "i") (or (:message e) "")))))

(defn recovered?
  "Whether an event says the body is back."
  [e]
  (and (= :body (:source e)) (boolean (#{:online :spawned :respawned} (:kind e)))))

(defn stale-reconnect?
  "Whether a reconnect-failed event is not worth a wake: a later try of the same outage (only the first wakes; the
  rest are counted in the summary) or one the body has since recovered from (a later online/spawned in the backlog)."
  [e later]
  (boolean (or (> (or (get-in e [:data :attempt]) 1) 1)
               (some recovered? later))))

(defn stale-notice?
  "Whether a notice in the backlog is from a job that has ended since (a later terminal event of the same job): its
  news is in the summary and the job's result, not a wake."
  [e later]
  (let [job (get-in e [:context :job-id])]
    (boolean (and (= :notice (:attention e)) job
                  (some #(and (= :job (:source %)) (job-results/terminal-kinds (:kind %)) (= job (get-in % [:context :job-id])))
                        later)))))

(def body-notice-kinds
  "Notices about the body itself, from a reflex job the agent never watches: they wake under any --watch."
  #{:hostile.fled})

(defn unwatched-notice?
  "Whether a notice belongs to a job outside a non-empty --watch list (a child counts as its top job)."
  [e opts]
  (let [job (get-in e [:context :job-id])]
    (boolean (and (seq (:watch opts)) job (not (body-notice-kinds (:kind e)))
                  (not-any? #{(first (str/split job #"/"))} (:watch opts))))))

(def jobs-changed-kinds (conj job-results/terminal-kinds :queued))

(defn stale-idle?
  "Whether a body.idle in the backlog is out of date: a job was queued or ended after it, or the body walked
  (a moveTo act), so its job list and its claim of no useful act no longer hold."
  [e later]
  (boolean (some #(or (and (= :job (:source %)) (jobs-changed-kinds (:kind %)))
                      (and (= :action (:source %)) (= :started (:kind %)) (= "moveTo" (get-in % [:data :name]))))
                 later)))

(defn idle-before-watch?
  "Whether e is a body.idle or a death stamped before the wait began, under a --watch: the watch is about its jobs, the
  idle was said of a body state that has changed since, and a death in the backlog is news the summary carries (a watched
  job it cancelled wakes as :job-finished)."
  [e opts started-ms]
  (boolean (and (seq (:watch opts)) (= :body (:source e)) (#{:idle :died} (:kind e)) started-ms
                (some-> (:time-ms e) (< started-ms)))))

(defn stale-death?
  "Whether a death or a hit stamped before the wait began is old news the summary carries, not a wake: a hit is
  (the health is in the status); a death is once the body has respawned (a later online/spawned/respawned in the backlog)."
  [e later started-ms]
  (boolean (and (= :body (:source e)) (#{:died :hurt} (:kind e)) started-ms
                (some-> (:time-ms e) (< started-ms))
                (or (= :hurt (:kind e)) (some recovered? later)))))

(defn idle-wake
  "The body.idle wake. Under --watch it keeps only the watched jobs' entries and message segments (\"<id> <name> waiting: ...\"),
  so another job's wait text is never read as theirs; unchanged when none of them is listed."
  [e watch]
  (let [d (:data e)
        mine (filterv #(some #{(:job %)} watch) (:jobs d))
        [head body] (str/split (str (:message e)) #"(?<= with no useful act): " 2)
        kept (filter (fn [segment] (some #(str/starts-with? segment (str (:job %) " ")) mine))
                     (str/split (str body) #"; "))]
    (if (and (seq mine) (seq kept))
      (clean-pairs :wake :idle :message (clip (str head ": " (str/join "; " kept))) :jobs mine)
      (clean-pairs :wake :idle :message (clip (:message e)) :jobs (:jobs d)))))

(defn classify
  "The wake an event causes under the options, or nil. Options: :from :chatter :watch :danger :disconnect
  :quiet-notices. A body.idle and a death always wake, and so does any :attention :notice event but a job end; under
  --watch only the notices of the watched jobs (and body notices) wake."
  [e opts body]
  (let [{:keys [source kind]} e
        d (or (:data e) {})]
    (cond
      (and (= :body source) (#{:chat :whisper} kind))
      (when-not (or (and (:from opts) (not= (lower (:from d)) (lower (:from opts))))
                    (= "none" (:chatter opts))
                    (and (= "addressed" (:chatter opts)) (not (addressed? e body))))
        (clean-pairs :wake :chat :from (clip (:from d) 40) :message (clip (:message e))
                     :whisper (when (= :whisper kind) true)))

      (and (= :body source) (= :reconnect-failed kind))
      (clean-pairs :wake :reconnect-failed :reason (clip (:reason d)))

      (and (= :body source) (= :idle kind))
      (idle-wake e (:watch opts))

      (and (= :body source) (= :died kind))
      (clean-pairs :wake :died :cause (clip (:cause d) 80) :at (position (:pos d))
                   :survived (not-empty (mapv id-str (:survived d))))

      (and (= :body source) (:danger opts) (= :hurt kind)
           (let [h (:health d) below (:danger-below opts)] (or (nil? below) (not (number? h)) (<= h below))))
      (clean-pairs :wake :danger :event (:kind e) :health (:health d) :amount (:amount d)
                   :causes (not-empty (mapv #(clip % 40) (:causes d))))

      (and (= :body source) (:disconnect opts) (= :disconnected kind))
      (clean-pairs :wake :disconnected :reason (clip (:reason d)))

      (and (= :job source) (job-results/terminal-kinds kind) (some #{(get-in e [:context :job-id])} (:watch opts)))
      (clean-pairs :wake :job-finished :job (get-in e [:context :job-id]) :result (:kind e)
                   :message (clip (or (:error d) (:message e))) :next (job-results/next-text (or (:next d) (get-in d [:cause :next]))))

      (and (= :notice (:attention e)) (not (:quiet-notices opts)) (not (job-results/terminal-kinds kind))
           (not (unwatched-notice? e opts)))
      (clean-pairs :wake :notice :event kind :job (get-in e [:context :job-id]) :message (clip (:message e))))))

(def max-noticed 256)

(defn notice-key [e] (str (some-> (:kind e) name) "|" (get-in e [:context :job-id])))

(defn repeat-notice?
  "Whether e is a notice of a (kind, job) that already woke the observer in this stretch."
  [noticed e]
  (and (= :notice (:attention e)) (boolean (some #{(notice-key e)} noticed))))

(defn note-notice
  "The noticed (kind|job) keys after e: a notice adds its key (the latest max-noticed are kept); a job end drops the
  keys of that job, so its notices wake again if the job runs again."
  [noticed e]
  (let [job (get-in e [:context :job-id])]
    (cond
      (and (= :job (:source e)) (job-results/terminal-kinds (:kind e)))
      (filterv #(not (str/ends-with? % (str "|" job))) noticed)
      (= :notice (:attention e))
      (let [k (notice-key e)]
        (if (some #{k} noticed) noticed (vec (take-last max-noticed (conj noticed k)))))
      :else noticed)))

(defn spec-label
  "The job name of a spec node as the engine prints it (a leaf's job, else the combinator form)."
  [{:keys [op job children child]}]
  (case op
    :leaf (str job)
    :repeat (str "(repeat " (spec-label child) ")")
    (str "(" (some-> op name) " " (str/join " " (map spec-label children)) ")")))

(defn instance-names
  "Job id -> name for the scheduler instances in a snapshot (jobs queued before the observer's call began)."
  [snap]
  (into {} (keep (fn [[id inst]] (when (:spec inst) [(id-str id) (clip (spec-label (:spec inst)) 120)])))
        (get-in snap [:state :instances])))

(def max-cancelled 50)

(defn track-deaths
  "Fold an event into the death tracker {:names :cancelled}: job names from :queued events, and the latest
  max-cancelled jobs a death cancelled (the engine emits those before the :died event; clear-reported drops them
  once reported)."
  [tracker e]
  (let [id (get-in e [:context :job-id])]
    (cond
      (and (= :job (:source e)) (= :queued (:kind e))) (assoc-in tracker [:names id] (get-in e [:data :name]))
      (and (= :body (:source e)) (= :hurt (:kind e)))
      (assoc tracker :hurt {:cause (str/join ", " (get-in e [:data :causes])) :time-ms (:time-ms e)})
      (and (= :job (:source e)) (= :cancelled (:kind e)) (= :death (get-in e [:data :by])))
      (update tracker :cancelled #(vec (take-last max-cancelled (conj (or % []) (clean-pairs :id id :name (get-in tracker [:names id]))))))
      :else tracker)))

(defn clear-reported
  "The tracker without its cancelled jobs once the :died event they belong to has been seen."
  [tracker e]
  (cond-> tracker
    (and (= :body (:source e)) (= :died (:kind e))) (dissoc :cancelled :hurt)))

(defn death-jobs
  "The jobs the tracker saw cancelled by death when e is the :died event, else nil."
  [tracker e]
  (when (and (= :body (:source e)) (= :died (:kind e)))
    (not-empty (:cancelled tracker))))

(def hurt-window-ms 5000)
(def lost-shown 3)

(defn lost-items
  "The inventory of a death as [\"name xN\" ...] for the lost-shown largest stacks, merged by name."
  [inventory]
  (->> (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} inventory)
       (sort-by (fn [[n c]] [(- c) n]))
       (map (fn [[n c]] (str n " x" c)))))

(defn recover-view [e]
  (let [d (:data e)]
    (clean-pairs :decision (some-> (:decision d) name) :reason (some-> (:reason d) name)
                 :value (some-> (:value d) js/Math.round))))

(def recover-decisions #{:collected :skip :partial :abandoned})

(defn recover-ending?
  "Whether e ends a recover-drops run: a job or reflex ending whose :data carries its :decision."
  [e]
  (and (#{:job :reflex} (:source e)) (#{:completed :stopped :ended} (:kind e)) (contains? recover-decisions (get-in e [:data :decision]))))

(defn death-details
  "For the :died event e: its :cause (the last hurt within hurt-window-ms), :lost (the biggest stacks, :lost-more
  the number of other names) and :recover (the recover-drops decision among the later events of the page); else nil."
  [tracker e later]
  (when (and (= :body (:source e)) (= :died (:kind e)))
    (let [{:keys [cause time-ms]} (:hurt tracker)
          cause (when (and time-ms (:time-ms e) (<= 0 (- (:time-ms e) time-ms) hurt-window-ms)) cause)
          lost (lost-items (get-in e [:data :inventory]))
          decided (first (filter recover-ending? later))]
      (clean-pairs :cause (some-> (not-empty cause) (clip 80))
                   :lost (not-empty (vec (take lost-shown lost)))
                   :lost-more (nonzero (- (count lost) lost-shown))
                   :recover (some-> decided recover-view)))))

(defn with-death-details [result details]
  (cond-> result
    (and result (seq details)) (merge (into (array-map) (remove (fn [[k _]] (and (= :cause k) (:cause result)))) details))))

(defn with-death-jobs [result jobs]
  (cond-> result (seq jobs) (assoc :cancelled-jobs jobs)))

(def max-items 4)

(defn key-category?
  "Whether a summary category is news the agent must not miss (a job end, a death, a notice) rather than routine."
  [category]
  (boolean (or (job-results/terminal-kinds category) (#{:died :notices} category))))

(defn routine-item? [item] (not (:key? (meta item))))

(defn room-for
  "The summary items with room for one more, or nil: under max-items as they are; full, a key item drops the oldest
  routine one (the rest keep their order)."
  [items key?]
  (cond
    (< (count items) max-items) items
    (not key?) nil
    :else (let [i (first (keep-indexed (fn [i item] (when (routine-item? item) i)) items))]
            (when i (into (subvec items 0 i) (subvec items (inc i)))))))

(def old-event-ms
  "An event this much older than the wait is backlog from before the observer last looked: counted, not listed."
  (* 10 60 1000))

(defn collect
  "Fold one routine event into the bounded quiet summary {:counts :items :more}; jobs are the ones a death cancelled.
  An event stamped before `cutoff-ms` only adds to the counts."
  ([summary e] (collect summary e nil))
  ([summary e jobs] (collect summary e jobs nil))
  ([summary e jobs cutoff-ms]
  (let [{:keys [source kind]} e
        d (or (:data e) {})
        category (cond
                   (and (= :job source) (job-results/terminal-kinds kind)) kind
                   (and (= :reflex source) (= :fired kind)) :reflexes
                   (= :make-room.tossed kind) :tossed
                   (#{:picked-up :hurt :died :disconnected :online :reconnect-failed} kind) kind
                   (and (= :action source) (= :logged-out kind)) kind
                   (= :notice (:attention e)) :notices)]
    (if-not category
      summary
      (let [summary (update-in summary [:counts category] #(min 1000000 (inc (or % 0))))]
        (if-let [items (when-not (and cutoff-ms (some-> (:time-ms e) (< cutoff-ms)))
                         (room-for (:items summary) (key-category? category)))]
          (assoc summary :more (or (:more summary) (< (count items) (count (:items summary)))) :items (conj items
                  (with-meta (clean-pairs :event (:kind e) :job (when-not (= :reflexes category) (get-in e [:context :job-id]))
                               :reflex (get-in e [:context :reflex-id]) :by (some-> (:by d) name) :why (some-> (:why d) name) :item (clip (:item d) 80) :count (:count d)
                               :message (clip (first (filter some? [(:message e) (:error d) (:reason d)])))
                               :cancelled-jobs (not-empty jobs))
                             {:key? (key-category? category)})))
          (assoc summary :more true)))))))

(defn summary-result [summary result]
  (if (seq (:counts summary))
    (assoc (clean result) :summary (cond-> (array-map :counts (:counts summary) :items (:items summary))
                                     (:more summary) (assoc :more? true)))
    (clean result)))

;; What happened since the observer last looked

(def max-missed 4)

(defn missed?
  "Whether an event is news a plain status points to: a job end, a flight, a fight or flee decision, a reflex end, a notice, damage or a death."
  [e]
  (boolean (or (and (= :job (:source e)) (job-results/terminal-kinds (:kind e)))
               (= :hostile.fled (:kind e))
               (and (= :hostile.decided (:kind e)) (#{:fight :flee} (get-in e [:data :decision])))
               (and (= :reflex (:source e)) (= :ended (:kind e)))
               (= :notice (:attention e))
               (and (= :body (:source e)) (#{:hurt :died :respawned} (:kind e))))))

(defn critical?
  "Whether a missed event is always shown, however many others came after it: a death or a respawn."
  [e]
  (and (= :body (:source e)) (contains? #{:died :respawned} (:kind e))))

(defn hurt-event?
  [e]
  (and (= :body (:source e)) (= :hurt (:kind e))))

(defn hurt-item
  "One item standing for all the hits: total health lost, the lowest health and the last cause named."
  [hits]
  (let [d :data]
    {:kind :hurt :time-ms (:time-ms (last hits))
     :damage (reduce + (keep (comp :amount d) hits))
     :lowest-health (some->> (keep (comp :health d) hits) seq (apply min))
     :cause (last (mapcat (comp :causes d) hits))}))

(defn missed-item [now-ms e]
  (clean-pairs :event (:kind e) :job (get-in e [:context :job-id]) :reflex (some-> (get-in e [:context :reflex-id]) name)
               :decision (some-> (get-in e [:data :decision]) name)
               :count (when (< 1 (:repeats e 1)) (:repeats e))
               :damage (:damage e) :lowest-health (:lowest-health e) :cause (:cause e) :message (clip (:message e))
               :ago-s (some-> (:time-ms e) (#(max 0 (js/Math.round (/ (- now-ms %) 1000)))))))

(defn merge-repeated-notices
  "events with the notices of one job and kind folded into one, at the place of the first, holding the latest text and
  :repeats, so a job re-emitting a notice each round cannot crowd out the rest."
  [events]
  (let [k (fn [e] (when (= :notice (:attention e)) [(get-in e [:context :job-id]) (:kind e)]))
        groups (group-by k (filter k events))]
    (into [] (keep (fn [e]
                     (let [g (some-> (k e) groups)]
                       (cond (nil? g) e
                             (identical? e (first g)) (assoc (last g) :repeats (count g))))))
          events)))

(defn missed-summary
  "{:count n :items [...]} of the missed events among events, or nil when there are none: the latest max-missed and every
  death or respawn before them, in order; all hits are one item, in the place of the first; repeated notices of one job
  and kind are one item with :count. skipped (events never read)
  is added as :skipped when positive."
  ([events now-ms] (missed-summary events now-ms 0))
  ([events now-ms skipped]
  (let [news (merge-repeated-notices (filterv missed? events))
        hits (filterv hurt-event? news)
        hurt (when (seq hits) (hurt-item hits))
        items (into [] (keep #(if (hurt-event? %) (when (identical? % (first hits)) hurt) %)) news)]
    (when (seq items)
      (let [latest (set (take-last max-missed items))
            shown (filter #(or (critical? %) (= :hurt (:kind %)) (latest %)) items)]
        (cond-> (array-map :count (count items)
                           :items (mapv #(missed-item now-ms %) shown))
          (pos? skipped) (assoc :skipped skipped)))))))
