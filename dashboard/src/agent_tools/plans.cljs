(ns agent-tools.plans
  "Compiled Node commands for plans and shared blueprints. Source stays EDN throughout the command."
  (:require [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as rt]
            [clojure.string :as str]
            [agent-tools.world-data :as data]
            [dashboard.agent-plan-tools :as checks]
            [dashboard.plan-compare :as cmp]
            [plan.parse :as parse]
            [plan.shape :as shape]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:util" :as util]))

(def raw-bytes 65536)
(def names #"^[A-Za-z0-9_-]{1,100}$")
(def repo-root (.resolve path js/__dirname "../.."))
(def usage
  {:plan "usage: plans.mjs --world <world> <command> [options]\n  list [--limit 10 --offset 0] [--raw [--large]]\n  find <text> [--limit 10 --offset 0] [--raw [--large]] | show <id> [--raw [--large]] [--geometry [--large]]\n  add <id> --edn '<plan-map>' --by <name> [--dry-run] [--raw]\n  edit <id> --edn '<plan-map>' --by <name> --revision <digest> [--dry-run] [--raw]\n  remove <id> --by <name> --revision <digest> [--dry-run] [--raw]   (every submitted plan is active; remove retires one)\n  validate <id> --edn '<plan-map>' [--blueprint <id>=<blueprint-map>]... [--raw] [--geometry [--large]]\n  check <id> [--inventory '<block-count-map>'] [--blueprint <id>=<blueprint-map>]... [--raw] [--geometry [--large]]\n  Common: --worlds <dir> --state <legacy-parent> --repo <dir> --limit 1..100 --offset 0..10000\n  Large raw output is opt-in with --large; raw output otherwise stops at 64 KiB. Plan/world edits need --by."
   :blueprint "usage: blueprints.mjs --world <world> <command> [options]\n  list [--limit 10 --offset 0] [--raw [--large]] | find <text> [--limit 10 --offset 0] [--raw [--large]]\n  show <id> [--raw [--large]] | save <id> --edn '<blueprint-map>' --by <name> [--revision <digest>] [--dry-run] [--raw]\n  validate <id> --edn '<blueprint-map>' [--raw] [--large]\n  Common: --worlds <dir> --state <legacy-parent> --repo <dir> --limit 1..100 --offset 0..10000\n  Blueprints are shared globally; each result reports :scope :global. Writes need --by."})

(defn fail! [reason message] (throw (data/fail reason message)))

(defn one-form [text]
  (when (or (not (string? text)) (> (js/Buffer.byteLength text) (* 8 1024 1024)))
    (fail! :bad-edn "EDN input must be a string at most 8 MiB"))
  (try
    (let [input (rt/string-push-back-reader text)
          eof (js-obj)
          value (edn/read {:eof eof} input)]
      (when (or (identical? eof value) (not (identical? eof (edn/read {:eof eof} input))))
        (fail! :bad-edn "give exactly one EDN form"))
      value)
    (catch :default e
      (if (.-reason e) (throw e) (fail! :bad-edn (str "invalid EDN: " (ex-message e)))))))

(defn edn-value [text field]
  (let [value (one-form text)]
    (when-not (map? value) (fail! (keyword (str "bad-" field)) (str "--" field " must be one EDN map")))
    value))

(defn blueprint-form [arg]
  (let [at (.indexOf arg "=")]
    (when (< at 1) (fail! :bad-blueprint "--blueprint must be <id>=<EDN map>"))
    (let [id (.slice arg 0 at) text (.slice arg (inc at))]
      (when-not (re-matches names id) (fail! :invalid-id (str "invalid blueprint ID " id)))
      {:id id :text text :value (edn-value text "blueprint")})))

(defn parsed-args [kind argv]
  (let [options (merge (into {} (map (fn [k] [k {:type "string"}])
                                    [:world :by :revision :edn :status :limit :offset :query :inventory]))
                       {:state {:type "string"} :worlds {:type "string"}
                        :repo {:type "string" :default repo-root}
                        :blueprint {:type "string" :multiple true}}
                       (into {} (map (fn [k] [k {:type "boolean" :default false}])
                                    [:raw :geometry :large :dry-run])))
        parsed (data/from-json (.parseArgs util #js {:args argv :allowPositionals true :options (clj->js options)}))
        [command id & extra] (:positionals parsed)
        v (:values parsed)
        commands (if (= kind :plan) #{"list" "find" "show" "add" "edit" "remove" "validate" "check"}
                     #{"list" "find" "show" "save" "validate"})
        mutations #{"add" "edit" "save" "remove"}
        allowed (cond-> #{:world :worlds :state :repo}
                  (#{"list" "find"} command) (into [:limit :offset :raw :large])
                  (= command "show") (into [:raw :geometry :large])
                  (= command "validate") (into [:edn :limit :offset :raw :geometry :large])
                  (and (= command "validate") (= kind :plan)) (conj :blueprint)
                  (= command "check") (into [:inventory :blueprint :raw :geometry :large :limit :offset])
                  (mutations command) (into [:by :revision :dry-run :raw :large])
                  (#{"add" "edit" "save"} command) (conj :edn))]
    (when-not (and (:world v) (re-matches #"^[A-Za-z0-9_-]{1,64}$" (:world v)))
      (fail! :invalid-world "supply an explicit valid --world"))
    (when (and (= kind :plan) (= command "status"))
      (fail! :usage "plans have no status: every submitted plan is active, and a draft is a plan you keep locally; to retire a plan use `remove <id> --by <name> --revision <digest>`"))
    (when (and (= kind :plan) (:status v))
      (fail! :bad-option "plans have no status field; every submitted plan is active"))
    (when-not (commands command) (fail! :usage (str "unknown " (name kind) " command " command)))
    (when (or (seq extra) (if (= command "list") (some? id) (nil? id)))
      (fail! :usage (str command (if (= command "list") " takes no ID" (if (= command "find") " needs one search string" " needs exactly one ID")))))
    (when (and id (not= command "find") (not (re-matches names id)))
      (fail! :invalid-id "ID must use letters, digits, _ or - (up to 100)"))
    (doseq [[key value] v]
      (when (and (not (allowed key)) (not (false? value))) (fail! :bad-option (str "--" (name key) " is not valid for " command))))
    (doseq [[key low high] [[:limit 1 100] [:offset 0 10000]]]
      (when-let [value (get v key)]
        (let [n (js/Number value)]
          (when-not (and (js/Number.isInteger n) (<= low n high)) (fail! :bad-page (str "--" (name key) " must be " low ".." high))))))
    (when (and (:large v) (not (:raw v)) (not (:geometry v))) (fail! :bad-option "--large requires --raw or --geometry"))
    (when (and (:geometry v) (not (#{"show" "validate" "check"} command))) (fail! :bad-option "--geometry is only valid for show, validate or check"))
    (when (and (:dry-run v) (not (mutations command))) (fail! :bad-option "--dry-run is only valid for mutations"))
    (when (and (#{"add" "edit" "save" "validate"} command) (nil? (:edn v))) (fail! :bad-option (str command " requires --edn")))
    (when (and (mutations command) (or (not (string? (:by v))) (str/blank? (:by v)) (> (count (:by v)) 80)))
      (fail! :actor-required "mutations require --by (up to 80 characters)"))
    (when (and (= command "add") (some? (:revision v))) (fail! :bad-option "add is create-only and does not take --revision"))
    (when (and (#{"edit" "remove"} command) (nil? (:revision v))) (fail! :revision-required (str command " requires the --revision from list/show")))
    (when (and (seq (:blueprint v)) (not (and (= kind :plan) (#{"validate" "check"} command))))
      (fail! :bad-option "--blueprint is only valid for plan validate/check"))
    (merge v {:kind kind :command command} (if (= command "find") {:search id} {:id id}))))

(defn request-for [kind argv]
  (try (parsed-args kind argv)
       (catch :default e {:error (ex-message e) :reason (or (.-reason e) :bad-request)})))

;; The raw document is inserted verbatim after printing the surrounding EDN result.
;; This preserves comments, map key spelling and formatting, including blueprint string keys.
(defrecord RawEDN [text])
(defn raw-edn [text] (->RawEDN text))
(defn print-value [value large]
  (let [bound (if large (* 8 1024 1024) raw-bytes)]
    (try
      (let [fragments (atom [])
            scrub (fn scrub [v]
                    (cond
                      (instance? RawEDN v) (let [token (str "RAW_EDN_" (count @fragments) "_" (random-uuid) "_END")]
                                             (swap! fragments conj [token (:text v)]) token)
                      (map? v) (into {} (map (fn [[k x]] [k (scrub x)]) v))
                      (sequential? v) (mapv scrub v)
                      :else v))
            encoded (pr-str (data/raw-bound (scrub value) bound))
            output (reduce (fn [s [token fragment]] (str/replace s (pr-str token) (fn [_] (str fragment "\n")))) encoded @fragments)]
        (when (> (js/Buffer.byteLength output) bound)
          (fail! :output-too-large (str "raw result exceeds " bound " bytes; scope or page the query")))
        (str output "\n"))
      (catch :default e
        (str (pr-str {:ok false :reason (keyword (or (.-reason e) "output-too-large"))
                      :message (subs (str (ex-message e)) 0 (min 240 (count (str (ex-message e)))))
                      :next "add --large or request a smaller page"}) "\n")))))

(defn page [values limit offset]
  (let [items (vec (take limit (drop offset values)))]
    (cond-> {:total (count values) :items items}
      (< (+ offset (count items)) (count values)) (assoc :next-offset (+ offset (count items))))))
(defn errors-page
  ([errors] (page errors 10 0))
  ([errors offset limit] (page errors limit offset)))
(defn current-docs [ctx kind] (vec (remove nil? (data/list-documents ctx kind))))
(defn plan-summary [doc]
  (let [p (:value doc)] {:id (:id doc) :revision (:revision doc) :note (:note p) :parts (count (:parts p))}))
(defn blueprint-summary [doc errors]
  (let [bp (:value doc)]
    {:id (:id doc) :scope :global :revision (:revision doc) :title (:title bp) :front (:front bp)
     :width (count (first (first (:layers bp)))) :depth (count (first (:layers bp))) :height (count (:layers bp))
     :errors (errors-page errors)}))
(defn plan-output [doc ctx raw]
  (if raw {:ok true :kind :plan :id (:id doc) :world (:world ctx) :scope :world :revision (:revision doc) :document (raw-edn (:text doc))}
      (assoc (plan-summary doc) :validation (errors-page (:errors (parse/parse (:text doc) (:id doc)))) :scope :world)))
(defn blueprint-output [doc raw]
  (if raw {:ok true :kind :blueprint :id (:id doc) :scope :global :revision (:revision doc) :document (raw-edn (:text doc))}
      (blueprint-summary doc (:errors (parse/parse-blueprint (:text doc) (:id doc))))))
(defn merged-blueprints [ctx inline]
  (vals (into {} (map (juxt :id identity) (concat (current-docs ctx :blueprint) inline)))))
(defn prepare [ctx text id inline zones claims]
  (checks/prepare-native text id (merged-blueprints ctx inline) (current-docs ctx :plan) zones claims))

(defn list-result [kind records req]
  (let [search (some-> (:search req) str/lower-case)
        filtered (vec (filter (fn [{:keys [id value]}]
                                (or (nil? search) (str/includes? (str/lower-case (str id " " (:note value) " " (:title value))) search))) records))
        limit (js/Number (or (:limit req) 10)) offset (js/Number (or (:offset req) 0))
        chosen (mapv (fn [doc]
                       (cond-> (if (= kind :plan) (plan-summary doc) (blueprint-summary doc []))
                         (:raw req) (assoc :document (raw-edn (:text doc)))
                         (and (:raw req) (= kind :plan)) (assoc :world (:world req) :scope :world))) (take limit (drop offset filtered)))]
    (cond-> {:ok true :kind kind :items chosen :total (count filtered)}
      (= kind :blueprint) (assoc :scope :global)
      (= kind :plan) (assoc :world (:world req))
      (< (+ offset (count chosen)) (count filtered)) (assoc :next-offset (+ offset (count chosen)) :more? true))))

(defn inventory-value [text]
  (when (some? text)
    (reduce-kv (fn [acc item count]
                 (let [item-name (cond (keyword? item) (subs (str item) 1) (string? item) item :else "")]
                   (when-not (and (re-matches #"^(?:minecraft:)?[a-z][a-z0-9_]*$" item-name) (js/Number.isSafeInteger count) (>= count 0))
                     (fail! :bad-inventory "inventory is a map of block/item names to nonnegative integer counts"))
                   (assoc acc (str/replace item-name #"^minecraft:" "") count))) {} (edn-value text "inventory"))))

(defn geometry-check! [cells large]
  (when (> (count cells) (if large 200000 1000)) (fail! :geometry-limit "use --large for more than 1000 cells")))

(defn read-zone-text [ctx]
  (try (.readFileSync fs (data/document-path ctx :zone "collection") "utf8")
       (catch :default e (if (= "ENOENT" (.-code e)) nil (throw e)))))

(defn check-plan [ctx text id inline req]
  (let [prepared (prepare ctx text id inline (read-zone-text ctx) (mapv :value (current-docs ctx :claim)))]
    (if (or (not (:ok prepared)) (nil? (:expansion prepared)))
      (js/Promise.resolve (assoc prepared :world (:world ctx) :worldEvidence "saved-column-dumps" :liveLoaded false))
      (do
        (geometry-check! (:cells prepared) (:large req))
        (-> ((:load-worldblocks req))
            (.then (fn [worldblocks]
                     (let [columns ((aget worldblocks "createWorldBlocks") #js {:stateDir (clj->js (:state ctx)) :world (:world ctx)})
                           blocks (atom {}) mtimes (atom {})
                           inventory (inventory-value (:inventory req))]
                       (try
                         (doseq [[x y z :as pos] (:cells prepared)]
                           (when-let [block (.call (aget columns "blockAt") columns x y z)]
                             (swap! blocks assoc pos {:name (aget block "name") :state (js->clj (aget block "state"))}))
                           (let [cx (js/Math.floor (/ x 16)) cz (js/Math.floor (/ z 16)) key [cx cz]]
                             (when-not (contains? @mtimes key)
                               (swap! mtimes assoc key
                                      (try (.-mtimeMs (.statSync fs (.join path (:columns-dir ctx) (str cx "." cz ".bin"))))
                                           (catch :default e (if (= "ENOENT" (.-code e)) nil (throw e))))))))
                         (let [score (checks/score-native (:expansion prepared) #(get @blocks %) #(get @mtimes [%1 %2])
                                                         (or inventory {}) (some? inventory)
                                                         (js/Number (or (:offset req) 0)) (js/Number (or (:limit req) 10)))
                               checked (:checked score)
                               stale (cmp/stale-note checked cmp/stale-ms)]
                           (cond-> (assoc (dissoc prepared :expansion :cells)
                                          :world (:world ctx) :evidence :saved-column-dumps :live-loaded? false
                                          :score (assoc score :checked
                                                        (cond-> (assoc checked :newest-age-ms (when (js/Number.isFinite (:newest checked)) (max 0 (- (:now checked) (:newest checked))))
                                                                       :freshness (if (< (:dumped checked) (:chunks checked)) :incomplete-saved-dumps :saved-dumps)
                                                                       :oldest-age-ms (when (js/Number.isFinite (:oldest checked)) (max 0 (- (:now checked) (:oldest checked)))))
                                                          stale (assoc :stale stale))))
                             (:geometry req) (assoc :cells (:cells prepared))
                             (:large req) (assoc :large true)))
                         (finally (.call (aget columns "close") columns)))))))))))

(defn validate-revision! [revision]
  (when (and (some? revision) (not (re-matches #"^[a-f0-9]{24}$" revision)))
    (fail! :bad-revision "--revision must be the 24-character digest from show/list")))

(defn mutate-plan [req ctx write]
  (let [old (data/read-document ctx :plan (:id req))
        command (:command req)]
    (when (and (= command "add") old) (fail! :already-exists (str "plan " (:id req) " already exists; use edit")))
    (when (and (#{"edit" "remove"} command) (nil? old)) (fail! :not-found (str "no plan " (:id req) " in " (:world ctx))))
    (validate-revision! (:revision req))
    (let [value (when-not (= command "remove") (some-> (edn-value (:edn req) "edn") (as-> v (if (map? v) (shape/with-author v (or (shape/author (:value old)) (:by req))) v))))
          source (:edn req)]
      (when (and value (not= (:id value) (:id req))) (fail! :bad-plan (str "plan :id must be \"" (:id req) "\"")))
      (-> (data/mutate-document ctx (cond-> {:kind :plan :id (:id req) :value value :by (:by req)
                                           :expected-revision (when-not (= command "add") (:revision req)) :dry-run (boolean (:dry-run req))}
                                    value (assoc :validate (fn [_] (let [result (prepare ctx source (:id req) [] nil [])]
                                                                   (when-not (:ok result) {:errors (:errors result)}))))))
          (.then (fn [result]
                   (let [updated (if (= command "remove") old (data/read-document ctx :plan (:id req)))
                         raw (if (and (:dry-run req) value) source (:text updated))]
                     (write (cond-> (assoc result :world (:world ctx))
                              (:dry-run req) (assoc :next :review-preview)
                              (and (:raw req) raw) (assoc :document (raw-edn raw)))) 0)))))))

(defn mutate-blueprint [req ctx write]
  (let [old (data/read-document ctx :blueprint (:id req))
        value (edn-value (:edn req) "edn")]
    (validate-revision! (:revision req))
    (when (and old (nil? (:revision req))) (fail! :revision-required "updating a blueprint requires the --revision from list/show"))
    (when-not (= (:id value) (:id req)) (fail! :bad-blueprint (str "blueprint :id must be \"" (:id req) "\"")))
    (-> (data/mutate-document ctx {:kind :blueprint :id (:id req) :value value :text (:edn req) :by (:by req)
                                   :expected-revision (:revision req) :dry-run (boolean (:dry-run req))
                                   :validate (fn [_] (let [errors (:errors (parse/parse-blueprint (:edn req) (:id req)))]
                                                      (when (seq errors) {:errors errors})))})
        (.then (fn [result]
                 (write (cond-> (assoc result :next (if (:dry-run req) :review-preview :show))
                          (:raw req) (assoc :document (raw-edn (:edn req))))) 0)))))

(defn compact-result [result req source]
  (let [limit (js/Number (or (:limit req) 10)) offset (js/Number (or (:offset req) 0))
        output (reduce (fn [acc field] (if (vector? (get acc field)) (update acc field page limit offset) acc))
                       (dissoc result :expansion :cells) [:conflicts :claims :parts])
        output (cond-> (assoc output :errors (errors-page (:errors output) offset limit))
                 (get-in output [:zones :overlaps]) (update-in [:zones :overlaps] page limit offset)
                 (seq (:blueprint-errors output)) (update :blueprint-errors errors-page offset limit)
                 (seq (:plan-errors output)) (update :plan-errors errors-page offset limit)
                 (:raw req) (assoc :document (raw-edn source)))]
    (when (:geometry req) (geometry-check! (:cells result) (:large req)))
    (cond-> output (:geometry req) (assoc :geometry (:cells result)))))

(defn execute-plan [req write]
  (let [ctx (data/context {:state (:state req) :worlds (:worlds req) :world (:world req) :repo-root (:repo req)}) command (:command req)]
    (cond
      (#{"list" "find"} command) (do (write (list-result :plan (current-docs ctx :plan) req)) 0)
      (#{"add" "edit" "remove"} command) (mutate-plan req ctx write)
      (= command "show") (let [doc (data/read-document ctx :plan (:id req))]
                            (when-not doc (fail! :not-found (str "no plan " (:id req) " in " (:world ctx))))
                            (let [output (plan-output doc ctx (:raw req))]
                              (if (:geometry req)
                                (let [result (prepare ctx (:text doc) (:id doc) [] nil [])]
                                  (if-not (:ok result) (do (write (assoc output :validation (errors-page (:errors result)))) 1)
                                          (do (geometry-check! (:cells result) (:large req)) (write (assoc output :geometry (:cells result))) 0)))
                                (do (write output) 0))))
      (#{"validate" "check"} command)
      (let [doc (when (= command "check") (data/read-document ctx :plan (:id req)))
            source (if (= command "check") (:text doc) (:edn req))]
        (when-not source (fail! :not-found (str "no plan " (:id req) "; validate an inline plan with --edn")))
        (let [value (if (= command "check") (:value doc) (edn-value source "edn"))
              inline (mapv blueprint-form (:blueprint req))]
          (when-not (= (:id value) (:id req)) (fail! :bad-plan (str "plan :id must be \"" (:id req) "\"")))
          (-> (js/Promise.resolve (if (= command "check") (check-plan ctx source (:id req) inline req)
                                      (prepare ctx source (:id req) inline nil [])))
              (.then (fn [result]
                       (write (compact-result (cond-> result (= command "check") (assoc :revision (:revision doc))) req source))
                       (if (false? (:ok result)) 1 0))))))
      :else (fail! :usage "unknown plan command"))))

(defn execute-blueprint [req write]
  (let [ctx (data/context {:state (:state req) :worlds (:worlds req) :world (:world req) :repo-root (:repo req)}) command (:command req)]
    (cond
      (#{"list" "find"} command) (do (write (list-result :blueprint (current-docs ctx :blueprint) req)) 0)
      (= command "show") (let [doc (data/read-document ctx :blueprint (:id req))]
                            (when-not doc (fail! :not-found (str "no global blueprint " (:id req))))
                            (write (blueprint-output doc (:raw req))) 0)
      (= command "save") (mutate-blueprint req ctx write)
      (= command "validate") (let [value (edn-value (:edn req) "edn")]
                                (when-not (= (:id value) (:id req)) (fail! :bad-blueprint (str "blueprint :id must be \"" (:id req) "\"")))
                                (let [errors (:errors (parse/parse-blueprint (:edn req) (:id req)))
                                      output (cond-> {:ok (empty? errors) :id (:id req) :scope :global
                                                      :errors (errors-page errors (js/Number (or (:offset req) 0)) (js/Number (or (:limit req) 10)))}
                                               (:raw req) (assoc :document (raw-edn (:edn req))))]
                                  (write output) (if (:ok output) 0 1)))
      :else (fail! :usage "unknown blueprint command"))))

(defn format-error [error]
  (let [message (str (or (ex-message error) error))
        errors (.-errors error)
        current (.-current error)]
    (cond-> {:ok false :reason (keyword (or (.-reason error) "internal-error")) :message (subs message 0 (min 240 (count message)))}
      errors (assoc :errors (vec (take 10 errors)))
      (> (count errors) 10) (assoc :next-offset 10)
      current (assoc :current current))))

(defn execute [kind argv output load-worldblocks]
  (let [kind (if (= kind "plan") :plan :blueprint)
        req (assoc (request-for kind argv) :load-worldblocks
                   (or load-worldblocks (fn [] (fail! :terrain-loader-required "plan checks need the Node terrain loader; run tools/plans.mjs"))))
        emit #(output (print-value % (:large req)))]
    (if (:error req)
      (do (emit {:ok false :reason (keyword (:reason req)) :message (:error req) :usage (get usage kind)}) (js/Promise.resolve 2))
      (-> (js/Promise.resolve nil)
          (.then (fn [_] (if (= kind :plan) (execute-plan req emit) (execute-blueprint req emit))))
          (.catch (fn [e] (emit (format-error e)) (if (= (keyword (or (.-reason e) "")) :not-found) 1 2)))))))
