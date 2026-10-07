(ns agent-tools.map
  "Agent map command: intent markers, access zones and expiring social claims."
  (:require [engine.bodies :as bodies]
            [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as rt]
            [clojure.string :as str]
            [agent-tools.world-data :as data]
            ["node:path" :as path]
            ["node:util" :refer [parseArgs]]))

(def usage "map.mjs --world WORLD find|show|add|edit|remove|renew|release [marker|zone|claim] [ID] [--data EDN --revision REV --by ACTOR --for 30m --dry-run --raw --center EDN --place ID --radius 128 --type TYPE --owner OWNER --status STATUS --text TEXT --limit 10 --offset 0 --worlds DIR --state LEGACY_PARENT]\nShared map of markers (a point: {:x :y :z :kind :note}, kind is free text such as :base or :farm, default :place, note up to 80 characters), zones (a box others avoid) and claims (a timed hold). :box is [[x y z] [x y z]] corner to corner; :total counts all matches, :next-offset pages.")
(def default-state-dir (.resolve path js/__dirname "../.."))

(defn coalesce [& values] (first (filter some? values)))

(defn ttl
  ([] (ttl "30m"))
  ([text]
   (let [[_ n unit] (re-matches #"^(\d+(?:\.\d+)?)(ms|s|m|h|d)?$" (str text))
         ms (if n (* (js/Number n) (get {"ms" 1 "s" 1000 "m" 60000 "h" 3600000 "d" 86400000} (or unit "s"))) js/NaN)]
     (when (or (not (js/Number.isFinite ms)) (< ms 100) (> ms 604800000))
       (throw (data/fail :invalid-duration "claim duration must be 100ms..7d")))
     ms)))

(defn parse-options [argv spec]
  ;; Node's parseArgs returns a null-prototype values object; js->clj does not
  ;; convert those objects. Its values are only strings and booleans.
  (let [parsed (parseArgs #js {:args (clj->js argv) :allowPositionals true :options (clj->js spec)})
        values (.-values parsed)]
    {:positionals (vec (array-seq (.-positionals parsed)))
     :values (into {} (map (fn [key] [(keyword key) (aget values key)]) (array-seq (js/Object.keys values))))}))

(defn read-edn [text]
  (let [reader (rt/string-push-back-reader text)
        form (edn/read {:eof ::eof} reader)]
    (when (or (= form ::eof) (not= ::eof (edn/read {:eof ::eof} reader)))
      (throw (data/fail :bad-args "expected one EDN value")))
    form))

(defn options [argv]
  (let [{:keys [positionals values]} (parse-options argv
           (merge (zipmap [:world :repo-root :data :revision :if-revision :by :for :center :place :radius :type :owner :status :text :limit :offset]
                          (repeat {:type "string"}))
                  {:state {:type "string"} :worlds {:type "string"}}
                  (zipmap [:dry-run :preview :raw] (repeat {:type "boolean"}))))
        [command kind id & extra] positionals
        command (keyword (or command "find"))
        kind (some-> kind keyword)
        v (if (:if-revision values)
            (do (when (:revision values) (throw (data/fail :invalid-option "choose --if-revision or --revision")))
                (-> values (assoc :revision (:if-revision values)) (dissoc :if-revision)))
            values)
        allowed (get {:find #{:center :place :radius :type :owner :status :text :limit :offset :raw}
                      :show #{:raw}
                      :add #{:data :by :for :dry-run :preview :raw}
                      :edit #{:data :revision :by :for :dry-run :preview :raw}
                      :remove #{:revision :by :dry-run :preview :raw}
                      :renew #{:revision :by :for :dry-run :preview :raw}
                      :release #{:revision :by :dry-run :preview :raw}} command)]
    (when (or (seq extra) (nil? allowed)
              (if (= command :find) (some? kind) (or (nil? id) (not (#{:marker :zone :claim} kind)))))
      (throw (data/fail :invalid-command usage)))
    (when (and (#{:renew :release} command) (not= kind :claim))
      (throw (data/fail :invalid-command "renew/release apply only to claims")))
    (doseq [key (keys v)]
      (when-not ((into #{:world :worlds :state :repo-root} allowed) key)
        (throw (data/fail :invalid-option (str "--" (name key) " does not apply to " (name command))))))
    (when (and (:for v) (not= kind :claim))
      (throw (data/fail :invalid-option "--for applies only to claims")))
    (let [ctx (data/context (select-keys v [:state :worlds :world :repo-root]))]
      (when (and (not (#{:find :show} command)) (or (nil? (:by v)) (str/blank? (:by v))))
        (throw (data/fail :actor-required "mutations require --by")))
      (when (and (not (#{:find :show :add} command)) (not (re-matches #"^[0-9a-f]{24}$" (or (:revision v) ""))))
        (throw (data/fail :revision-required "give --revision from show/find")))
      {:command command :kind kind :id id :ctx ctx :values v})))

(defn filters [ctx v]
  (when (and (:center v) (:place v)) (throw (data/fail :invalid-center "choose --center or --place")))
  (let [center (if-let [id (:place v)]
                 (let [place (data/read-document ctx :marker id)]
                   (when-not place (throw (data/fail :place-not-found "no such marker")))
                   ((juxt :x :y :z) (:value place)))
                 (when (:center v) (read-edn (:center v))))]
    (when (and (:radius v) (nil? center)) (throw (data/fail :invalid-center "radius requires explicit center or place")))
    (merge (select-keys v [:type :owner :status :text])
           {:center center :radius (js/Number (or (:radius v) 128))
            :limit (js/Number (or (:limit v) 10)) :offset (js/Number (or (:offset v) 0))})))

(defn summary [{:keys [kind id revision scope value distance]}]
  (let [v value
        owner (coalesce (:owner v) (:by v))
        box (data/box-of v)
        status (data/name (:status v))
        note (:note v)]
    (cond-> {:kind (keyword (data/name kind)) :id id :revision revision
             :scope (keyword (data/name scope))
             :source (keyword (if (= "marker" (data/name kind)) (data/name (or (:source v) :unknown)) "intent"))}
      (js/Boolean owner) (assoc :owner owner)
      (js/Boolean (:kind v)) (assoc :type (keyword (data/name (:kind v))))
      box (assoc :box box)
      (:status v) (assoc :status (keyword (if (and (= status "active") (:until v) (<= (:until v) (js/Date.now))) "expired" status)))
      (js/Boolean (:until v)) (assoc :until (:until v))
      (js/Boolean note) (assoc :note (subs note 0 (min 240 (count note))))
      (and (js/Boolean note) (> (count note) 240)) (assoc :note-truncated? true)
      (:structure v) (assoc :has-structure? true)
      (js/Number.isFinite distance) (assoc :distance (js/Math.round distance)))))

(defn preflight-raw-mutation [record kind id op]
  (data/raw-bound {:ok true :kind kind :id id :revision (apply str (repeat 24 "0"))
                   :op op :cursor {:stream (apply str (repeat 36 "0")) :seq js/Number.MAX_SAFE_INTEGER} :record record}))

(defn corners? [v]
  (and (every? #(and (vector? %) (= 3 (count %)) (every? js/Number.isInteger %)) [(:min v) (:max v)])
       (every? true? (map <= (:min v) (:max v)))))

(defn validate-zone [v]
  (when (or (not (map? v)) (some #(not (#{:name :min :max :owner :allow :note} %)) (keys v)))
    (throw (data/fail :validation "zone contains unsupported keys")))
  (when (or (not (string? (:name v))) (str/blank? (:name v)) (not (string? (:owner v))) (str/blank? (:owner v)))
    (throw (data/fail :validation "zone needs name and owner")))
  (when-not (corners? v) (throw (data/fail :validation "zone needs ordered integer min/max corners")))
  (when (and (contains? v :allow) (not (and (set? (:allow v)) (every? #(#{"dig" "place" "harvest" "take" "put"} (data/name %)) (:allow v)))))
    (throw (data/fail :validation "allow must be a set of :dig :place :harvest :take :put")))
  (when (and (contains? v :note) (not (string? (:note v)))) (throw (data/fail :validation "note must be text"))))

(defn owns! [previous actor]
  (when-let [owner (or (:owner previous) (:by previous))]
    (when (not= (str/lower-case owner) (str/lower-case actor))
      (throw (data/fail :not-owner (str "object belongs to " owner) {:owner owner})))))

(defn intersects? [a b]
  (and a b (every? (fn [i] (and (<= (get-in a [0 i]) (get-in b [1 i]))
                                (<= (get-in b [0 i]) (get-in a [1 i])))) [0 1 2])))

(defn map-refusal [saved actor]
  (let [{:keys [by name]} saved]
    (when (and by (not= (str/lower-case (str by)) (str/lower-case (str actor))))
      (str name " is on the shared map as " by "'s, and this is their own record of it: the plan, where it is and what it is are theirs to change or take off the map. Save yours under a name of your own, or ask " by " in chat to change theirs. Adding to its note= is still open to you, and working the ground is a different question (the note on it answers that one)"))))

(defn marker-value [previous patch value id actor]
  (when (and (contains? patch :name) (not= (:name patch) id)) (throw (data/fail :validation "name must match ID")))
  (when (contains? patch :type)
    (throw (data/fail :validation "a marker has no :type; what it is goes in :kind (free text, e.g. :base :farm :mine, default :place). Nothing was marked")))
  (let [source (data/name (coalesce (:source patch) (:source previous) :intent))
        type (if (string? (:kind value)) (:kind value) (if (some? (:kind value)) (data/name (:kind value)) "place"))
        note (str (coalesce (:note value) (:note previous) ""))
        by (or (:by previous) actor)]
    (when-not (#{"intent" "observation"} source) (throw (data/fail :validation "marker source must be intent or observation")))
    (when-not (every? #(and (js/Number.isFinite (get value %)) (<= (js/Math.abs (get value %)) 30000000)) [:x :y :z])
      (throw (data/fail :validation "marker needs finite x/y/z within world bounds")))
    (when-not (string? type) (throw (data/fail :validation "kind must be text")))
    (when (> (count note) 80)
      (throw (data/fail :validation (str "note= is " (count note) " characters and a place note holds 80: shorten it. Nothing was marked"))))
    (when (and (contains? patch :by) (not= (:by patch) by))
      (throw (data/fail :not-owner "marker authorship cannot be transferred by edit")))
    (assoc value :name id :by by :source source :kind type :note note)))

(defn zone-value [patch value id actor]
  (when (and (contains? patch :name) (not= (:name patch) id)) (throw (data/fail :validation "name must match ID")))
  (let [value (assoc value :name id :owner (coalesce (:owner value) actor))]
    (validate-zone value)
    value))

(defn claim-value [patch value id command v]
  (when (and (contains? patch :id) (not= (:id patch) id)) (throw (data/fail :validation "claim id must match ID")))
  (let [value (cond-> (assoc value :id id :owner (coalesce (:owner value) (:by v)))
                (or (#{:add :renew} command) (:for v)) (assoc :until (+ (js/Date.now) (ttl (or (:for v) "30m"))) :status :active)
                (= command :release) (assoc :status :released :until (js/Date.now)))]
    (when-not (and (corners? value) (data/box-of value))
      (throw (data/fail :validation "claim needs ordered integer min/max corners")))
    (when-not (and (string? (:owner value)) (not (str/blank? (:owner value)))
                   (#{"active" "released"} (data/name (:status value))) (js/Number.isSafeInteger (:until value)))
      (throw (data/fail :validation "claim needs owner, valid status and expiry")))
    (when (some #(not (#{:id :owner :min :max :until :status :note} %)) (keys value))
      (throw (data/fail :validation "unsupported claim key")))
    (when (and (contains? value :note) (not (string? (:note value)))) (throw (data/fail :validation "note must be text")))
    (when (> (:until value) (+ (js/Date.now) 604800000))
      (throw (data/fail :invalid-duration "claim expiry cannot be more than 7d away")))
    value))

(defn active-claim? [value]
  (and (= "active" (data/name (:status value))) (> (:until value) (js/Date.now))))

(defn execute-request! [{:keys [ctx command kind id values]}]
  (let [v values]
    (case command
      :find (let [docs (filter #(or (not= (:kind %) :claim) (:status v) (active-claim? (:value %)))
                               (mapcat #(data/list-documents ctx %) [:marker :zone :claim]))
                  page (data/query docs (filters ctx v))]
              (data/raw-bound (update page :items #(mapv (fn [d] (cond-> (summary d) (:raw v) (assoc :record (:value d)))) %))))
      :show (let [d (data/read-document ctx kind id)]
              (when-not d (throw (data/fail :not-found "object not found" {:kind kind :id id})))
              (data/raw-bound (cond-> (summary d) (:raw v) (assoc :record (:value d)))))
      (let [patch (when (#{:add :edit} command)
                    (when (or (nil? (:data v)) (> (.byteLength js/Buffer (:data v)) 65536))
                      (throw (data/fail :invalid-data "give --data EDN map up to 64KiB")))
                    (let [parsed (read-edn (:data v))]
                      (when-not (map? parsed) (throw (data/fail :invalid-data "data must be a map")))
                      parsed))
            previous (data/read-document ctx kind id)
            _ (when (and (#{:remove :renew :release} command) (nil? previous))
                (throw (data/fail :not-found "object not found" {:kind kind :id id})))
            merged (when (not= command :remove) (merge (:value previous) patch))
            value (when merged (case kind
                                 :marker (marker-value (:value previous) patch merged id (:by v))
                                 :zone (zone-value patch merged id (:by v))
                                 :claim (claim-value patch merged id command v)))
            conflicts (atom [])]
        (when (:raw v) (preflight-raw-mutation (if (= command :remove) (:value previous) value) kind id command))
        (.then (data/mutate-document ctx
                    {:kind kind :id id :expected-revision (when (not= command :add) (:revision v))
                     :value value :by (:by v) :source (or (:source value) :intent) :dry-run (or (:dry-run v) (:preview v))
                     :validate (fn []
                                 (let [current (:value (data/read-document ctx kind id))]
                                   (if (= kind :marker)
                                     (do (when (or (= command :remove) (some #(not= % :note) (keys patch)))
                                           (when-let [refusal (map-refusal current (:by v))]
                                             (throw (data/fail :not-owner refusal {:owner (:by current)}))))
                                         (when (some #(contains? patch %) [:structure :map :legend])
                                           (throw (data/fail :validation "map tool does not edit embedded legacy structures; use plans"))))
                                     (do (owns! current (:by v))
                                         (when (and (:owner value) (not= (str/lower-case (:owner value)) (str/lower-case (:by v))))
                                           (throw (data/fail :not-owner "owner must be the acting author")))))
                                   (when (and (= kind :claim) value (active-claim? value))
                                     (reset! conflicts
                                             (->> (data/list-documents ctx :claim)
                                                  (filter #(and (not= (:id %) id) (active-claim? (:value %))
                                                                (not= (str/lower-case (:owner (:value %))) (str/lower-case (:by v)))
                                                                (intersects? (data/box-of value) (data/box-of (:value %)))))
                                                  (mapv summary)))
                                     (when (seq @conflicts)
                                       (let [center (mapv #(/ (+ %1 %2) 2) (:min value) (:max value))
                                             radius (js/Math.sqrt (reduce + (map #(let [half (/ (- %2 %1) 2)] (* half half)) (:min value) (:max value))))
                                             find-cmd (str "map.mjs --world " (:world ctx) " find --type claim --status active --center '" (pr-str center) "' --radius " radius " --limit 100")]
                                         (throw (data/fail :claim-conflict (str "another owner has an active overlapping intent claim; inspect other claims with " find-cmd)
                                                          {:conflicts @conflicts :conflicts-total (count @conflicts) :more? (> (count @conflicts) 10) :next find-cmd})))))))})
               (fn [result]
                 (let [record (when (:raw v) (cond (= command :remove) (:value previous)
                                                  (:preview result) value
                                                  :else (:value (data/read-document ctx kind id))))]
                   (data/raw-bound (cond-> result
                                     record (assoc :record record)
                                     (seq @conflicts) (assoc :conflicts @conflicts))))))))))

(defn execute! [request]
  ;; Match an async JS function: validation exceptions reject rather than escaping synchronously.
  (.then (js/Promise.resolve nil) (fn [_] (execute-request! request))))

(defn error-field [error key]
  (or (get (ex-data error) key) (get (.-data error) key) (aget error (name key))))

(defn error-field? [error key]
  (or (contains? (ex-data error) key) (contains? (.-data error) key)
      (.call (.-hasOwnProperty js/Object.prototype) error (name key))))

(defn error-result [error]
  (let [conflicts (error-field error :conflicts)
        conflicts (if (array? conflicts) (js->clj conflicts :keywordize-keys true) conflicts)]
    (cond-> {:ok false :reason (keyword (data/name (or (error-field error :reason) :bad-args)))
             :message (subs (or (.-message error) (str error)) 0 (min 500 (count (or (.-message error) (str error)))))}
      (error-field? error :current) (assoc :current (error-field error :current))
      (error-field error :owner) (assoc :owner (error-field error :owner))
      conflicts (assoc :conflicts (vec (take 10 conflicts))
                       :conflicts-total (or (error-field error :conflicts-total) (count conflicts))
                       :more? (if (some? (error-field error :more?)) (error-field error :more?) (> (count conflicts) 10)))
      (and conflicts (error-field error :next)) (assoc :next (error-field error :next)))))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv]
   (-> (.then (js/Promise.resolve nil) (fn [_] (execute! (options argv))))
       (.then (fn [result] (.write (.-stdout js/process) (str (data/write-edn result) "\n")) 0))
       (.catch (fn [error] (.write (.-stdout js/process) (str (data/write-edn (error-result error)) "\n")) 1)))))
