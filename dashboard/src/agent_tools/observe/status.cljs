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

(defn compact-status
  "Status without metadata or empty collections: positions rounded, queued jobs apart from the current one."
  ([s] (compact-status s (js/Date.now)))
  ([s now]
  (if (false? (:ok s))
    s
    (let [{:keys [current jobs failed outstanding manual]} s
          items (:items jobs)
          queued (remove #(= (:id %) (:id current)) items)
          queued-total (- (or (:total jobs) 0) (if (some #(= (:id %) (:id current)) items) 1 0))]
      (clean-pairs
       :mode (:mode s)
       :offline (offline-view (:offline s) now)
       :last-known (when (:last-known s) true)
       :idle (when-not current true)
       :pos (position (:position s))
       :died (when (:died s) (died-view (:died s)))
       :health (:health s)
       :food (:food s)
       :current (when current (job-view current))
       :manual (when manual (clean-pairs :who (clip (:who manual) 80) :why (clip (:why manual) 160)))
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

(defn classify
  "The wake an event causes under the options, or nil. Options: :from :chatter :watch :danger :disconnect."
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

      (and (= :body source) (:danger opts) (#{:hurt :died} kind))
      (clean-pairs :wake :danger :event (:kind e) :health (:health d))

      (and (= :body source) (:disconnect opts) (= :disconnected kind))
      (clean-pairs :wake :disconnected :reason (clip (:reason d)))

      (and (= :job source) (job-results/terminal-kinds kind) (some #{(get-in e [:context :job-id])} (:watch opts)))
      (clean-pairs :wake :job-finished :job (get-in e [:context :job-id]) :result (:kind e)
                   :message (clip (or (:message e) (:error d)))))))

(defn collect
  "Fold one routine event into the bounded quiet summary {:counts :items :more}."
  [summary e]
  (let [{:keys [source kind]} e
        d (or (:data e) {})
        category (cond
                   (and (= :job source) (job-results/terminal-kinds kind)) kind
                   (and (= :reflex source) (= :fired kind)) :reflexes
                   (= :make-room.tossed kind) :tossed
                   (#{:picked-up :hurt :died :disconnected :online :reconnect-failed} kind) kind
                   (= :notice (:attention e)) :notices)]
    (if-not category
      summary
      (let [summary (update-in summary [:counts category] #(min 1000000 (inc (or % 0))))]
        (if (< (count (:items summary)) 4)
          (update summary :items conj
                  (clean-pairs :event (:kind e) :job (when-not (= :reflexes category) (get-in e [:context :job-id]))
                               :reflex (get-in e [:context :reflex-id]) :item (clip (:item d) 80) :count (:count d)
                               :message (clip (first (filter some? [(:message e) (:error d) (:reason d)])))))
          (assoc summary :more true))))))

(defn summary-result [summary result]
  (if (seq (:counts summary))
    (assoc (clean result) :summary (cond-> (array-map :counts (:counts summary) :items (:items summary))
                                     (:more summary) (assoc :more? true)))
    (clean result)))
