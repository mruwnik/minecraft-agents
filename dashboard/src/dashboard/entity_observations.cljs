(ns dashboard.entity-observations
  "Bounded transient observations from engine sockets. Original observation times determine expiry."
  (:require [clojure.string :as str]))

(def ttl-ms 120000)
(def max-per-body 10000)
(def max-cached 20000)
(def max-output 10000)
(def max-sources 128)
(def max-output-bytes (* 4 1024 1024))

(defn dimension-name [value]
  (let [value (when (or (string? value) (keyword? value)) (name value))]
    (case value
      "minecraft:overworld" "overworld"
      "minecraft:the_nether" "the_nether"
      "minecraft:the_end" "the_end"
      value)))

(defn alive? [entity now]
  (and (js/Number.isFinite (:observed-at entity)) (js/Number.isFinite (:expires-at entity))
       (<= (:observed-at entity) now) (> (:expires-at entity) now)))

(defn identity-key [body entity]
  [(:world body)
   (if (string? (:uuid entity)) [:uuid (:uuid entity)] [:ephemeral (:name body) (:key entity)])])

(defn entity [body value now]
  (when (and (map? value) (= (:world body) (:world value))
             (string? (:key value)) (not (str/blank? (:key value)))
             (string? (:type value)) (<= (count (:type value)) 128)
             (<= (count (:key value)) 256)
             (or (nil? (:uuid value)) (and (string? (:uuid value)) (<= 1 (count (:uuid value)) 128)))
             (or (nil? (:username value)) (and (string? (:username value)) (<= (count (:username value)) 128)))
             (string? (dimension-name (:dimension value)))
             (<= (count (dimension-name (:dimension value))) 128)
             (or (nil? (:id value)) (js/Number.isFinite (:id value)))
             (or (nil? (:identity value)) (contains? #{:uuid :ephemeral} (:identity value)))
             (or (nil? (:self? value)) (boolean? (:self? value)))
             (every? #(js/Number.isFinite (get (:pos value) %)) [:x :y :z])
             (js/Number.isFinite (:observed-at value)) (js/Number.isFinite (:expires-at value)))
    (let [value (assoc (select-keys value [:key :type :uuid :id :world :dimension :pos :observed-at :expires-at :username :identity :self?]) :dimension (dimension-name (:dimension value))
                       :expires-at (min (:expires-at value) (+ (:observed-at value) ttl-ms))
                       :seen-by (:name body) :pos (select-keys (:pos value) [:x :y :z]))]
      (when (alive? value now) (assoc value :identity-key (identity-key body value))))))

(defn body-snapshot [body payload now]
  (when-not (and (map? payload) (true? (:ok payload)) (= (:world body) (:world payload))
                 (= (:name body) (:body payload)) (vector? (:entities payload))
                 (<= (count (:entities payload)) max-per-body))
    (throw (js/Error. "invalid engine entity snapshot: body, world or entity count differs")))
  {:body (select-keys body [:world :name]) :status :ready :requested-at now :received-at now
   :entities (vec (keep #(entity body % now) (:entities payload)))
   :online? (boolean (:online? payload)) :reported-count (count (:entities payload))
   :truncated? (boolean (:truncated? payload)) :dropped (if (js/Number.isFinite (:dropped payload)) (:dropped payload) 0)})

(defn prune [cache now]
  (into {} (map (fn [[key entry]] [key (update entry :entities #(filterv (fn [e] (alive? e now)) %))])) cache))

(defn bound [cache now]
  (let [cache (into {} (take max-sources (sort-by (comp - #(or % 0) :requested-at val) (prune cache now))))
        copies (sort-by (juxt (comp - :observed-at second) (comp pr-str first) (comp pr-str :identity-key second))
                        (for [[body entry] cache e (:entities entry)] [body e]))
        kept (group-by first (take max-cached copies))]
    (into {} (map (fn [[body entry]]
                    (let [entities (mapv second (get kept body []))]
                      [body (cond-> (assoc entry :entities entities)
                              (< (count entities) (count (:entities entry))) (assoc :cache-truncated? true))]))) cache)))

(defn merge-world [cache world dimension now]
  (let [dimension (or (dimension-name dimension) "overworld")
        sources (sort-by (comp :name :body) (filter #(= world (get-in % [:body :world])) (vals cache)))
        candidates (for [source sources e (:entities source) :when (alive? e now)] e)
        winners (reduce (fn [result e]
                          (let [key (:identity-key e) old (get result key)]
                            (if (or (nil? old) (> (:observed-at e) (:observed-at old))
                                    (and (= (:observed-at e) (:observed-at old))
                                         (neg? (compare (:seen-by e) (:seen-by old)))))
                              (assoc result key e) result))) {} candidates)
        all (sort-by (juxt (comp - :observed-at) (comp pr-str :identity-key))
                     (filter #(= dimension (:dimension %)) (vals winners)))
        source-views (mapv #(select-keys % [:body :status :error :received-at :online? :truncated? :cache-truncated? :dropped]) sources)
        encoder (js/TextEncoder.)]
    (let [output (loop [remaining (seq (take max-output all)) bytes (+ 4096 (.-length (.encode encoder (pr-str source-views)))) result []]
                   (if-let [e (first remaining)]
                     (let [e (dissoc e :identity-key)
                           size (.-length (.encode encoder (pr-str e)))]
                       (if (> (+ bytes size) max-output-bytes) result
                           (recur (next remaining) (+ bytes size) (conj result e))))
                     result))]
    {:world world :dimension dimension :ttl-ms ttl-ms :now now
     :entities output :count (count all) :cap max-output :cap-bytes max-output-bytes
     :truncated? (boolean (or (> (count all) (count output)) (some #(or (:truncated? %) (:cache-truncated? %)) sources)))
     :sources source-views})))

(defn villager-record [e]
  {:uuid (:uuid e) :key (if (:uuid e) (:key e) (str (:seen-by e) "/" (:key e))) :world (:world e) :dimension (:dimension e) :lastPosition (:pos e)
   :lastSeenAt (.toISOString (js/Date. (:observed-at e))) :lastSeenBy (:seen-by e)
   :t (:observed-at e) :until (:expires-at e) :type (:type e)})

(defn villagers [snapshot]
  (let [records (into {} (for [e (:entities snapshot)
                             :when (= "villager" (:type e))]
                         (let [record (villager-record e)] [(or (:uuid record) (:key record)) record])))]
    (assoc (dissoc snapshot :entities) :version 1 :villagers records :count (count records))))
