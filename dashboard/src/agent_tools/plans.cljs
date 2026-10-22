(ns agent-tools.plans
  "Compiled Node commands for plans and shared blueprints. Source stays EDN throughout the command."
  (:require [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as rt]
            [clojure.string :as str]
            [agent-tools.http :as http]
            [agent-tools.plan-route :as plan-route]
            [agent-tools.world-data :as data]
            [dashboard.agent-plan-tools :as checks]
            [plan.parse :as parse]
            [plan.shape :as shape]
            [plan.surface :as surface]
            ["node:fs" :as fs]
            ["node:path" :as path]
            ["node:util" :as util]))

(def raw-bytes 65536)
(def names #"^[A-Za-z0-9_-]{1,100}$")
(def repo-root (.resolve path js/__dirname "../.."))
(def usage
  {:plan "usage: plans.mjs --world <world> <command> [options]\n  list [--limit 10 --offset 0] [--raw [--large]]\n  find <text> [--limit 10 --offset 0] [--raw [--large]] | show <id> [--raw [--large]] [--geometry [--large]]\n  add <id> --edn '<plan-map>' --by <name> [--dry-run] [--raw]\n  edit <id> --edn '<plan-map>' --by <name> --revision <digest> [--dry-run] [--raw]\n  remove <id> --by <name> --revision <digest> [--dry-run] [--raw]   (every submitted plan is active; remove retires one)\n  route <id> --from=x,y,z --to=x,y,z --body <name> --by <name> [--tunnel N --bridge N --launch 3..5] [--dry-run] [--raw]   (a rail line between two rail cells (the cell above the ground) over the ground that body has seen, avoiding other owners' zones; --tunnel/--bridge set the longest span through rock 2+ above the rail or over a drop 2+ below, default 0; the plan carries those :spans and jobs.build.rail-line builds them first; the result lists the materials)\n  mineshaft <id> --at=x,y,z --heading north|east|south|west --to-y <y> --by <name> [--dry-run] [--raw]   (declare a stair shaft: :at is the feet cell of its top step, --to-y below it; show names its depth and heading, check counts its steps dug)\n  validate <id> --edn '<plan-map>' [--blueprint <id>=<blueprint-map>]... [--raw] [--geometry [--large]]\n  check <id> --body <name> [--inventory '<block-count-map>'] [--blueprint <id>=<blueprint-map>]... [--raw] [--geometry [--large]]\n  Common: --worlds <dir> --state <legacy-parent> --repo <dir> --limit 1..100 --offset 0..10000\n  check judges only what that body has seen (its seen memory, flushed from the running body first; :seen-note says when it could not be and the data is the file on disk); cells never seen count unknown and :checked :unseen. Large raw output is opt-in with --large; raw output otherwise stops at 64 KiB. Plan/world edits need --by."
   :blueprint "usage: blueprints.mjs --world <world> <command> [options]\n  list [--limit 10 --offset 0] [--raw [--large]] | find <text> [--limit 10 --offset 0] [--raw [--large]]\n  show <id> [--raw [--large]] | save <id> --edn '<blueprint-map>' --by <name> [--revision <digest>] [--dry-run] [--raw]\n  remove <id> --by <name> --revision <digest> [--dry-run] [--raw]   (deletes the global blueprint file)\n  validate <id> --edn '<blueprint-map>' [--raw] [--large]\n  Common: --worlds <dir> --state <legacy-parent> --repo <dir> --limit 1..100 --offset 0..10000\n  Blueprints are shared globally; each result reports :scope :global. Writes need --by."})

(defn fail! [reason message] (throw (data/fail reason message)))

(def air-names #{"air" "cave_air" "void_air"})

(defn mineshaft-args!
  "The {:from :heading :to-y} stair of the mineshaft options, or a refusal naming the bad option."
  [{:keys [at heading to-y]}]
  (let [from (when (string? at) (mapv js/Number (str/split at #",")))
        down (js/Number to-y)]
    (when-not (and (= 3 (count from)) (every? js/Number.isSafeInteger from))
      (fail! :bad-at "--at needs the feet cell of the top step as x,y,z integers (write --at=-5,64,3 for a negative x)"))
    (when-not (contains? shape/headings (keyword heading))
      (fail! :bad-heading "--heading must be north, east, south or west"))
    (when-not (js/Number.isSafeInteger down) (fail! :bad-to-y "--to-y needs an integer"))
    (when-not (< down (second from))
      (fail! :bad-to-y (str "--to-y " down " must be below the top step's y " (second from) ": a shaft goes down")))
    {:from from :heading (keyword heading) :to-y down}))

(defn shaft-of
  "{:heading :from :to-y :depth :steps :bottom} of the first valid :stair part of a :mineshaft plan value, or nil."
  [value]
  (when (= :mineshaft (:kind value))
    (when-let [{:keys [from heading to-y] :as stair} (some #(when (and (:stair %) (nil? (shape/stair-error (:stair %)))) (:stair %)) (:parts value))]
      (let [n (shape/stair-steps stair) [dx dy dz] (shape/delta :down heading)]
        {:heading heading :from from :to-y to-y :depth n :steps n
         :bottom (shape/add from [(* n dx) (* n dy) (* n dz)])}))))

(defn shaft-progress
  "The shaft of a plan value with its steps counted from block-at (pos -> block name or nil when unseen): a step is :dug
  when all three cells it cuts are seen air, :undug when one is seen as anything else, else :unseen."
  [value block-at]
  (when-let [{:keys [from heading steps depth]} (shaft-of value)]
    (let [state (fn [feet]
                  (let [names (map block-at (:cut (shape/step-cells feet :down heading)))]
                    (cond (some #(and % (not (air-names %))) names) :undug
                          (every? some? names) :dug
                          :else :unseen)))
          counts (frequencies (map state (take steps (iterate #(shape/add % (shape/delta :down heading)) from))))]
      {:heading heading :depth depth :steps steps :dug (get counts :dug 0) :undug (get counts :undug 0) :unseen (get counts :unseen 0)})))

;; before parsed-args: it refuses a bad option before anything is read
(defn route-args!
  "The {:from :to :tunnel :bridge :launch} of the route options, or a refusal naming the bad option."
  [{:keys [from to tunnel bridge launch]}]
  (let [a (plan-route/cell-arg from) b (plan-route/cell-arg to)
        launch-n (when launch (js/Number launch))]
    (when-not a (fail! :bad-from "--from needs the rail cell as x,y,z integers (write --from=-5,64,3 for a negative x)"))
    (when-not b (fail! :bad-to "--to needs the rail cell as x,y,z integers (write --to=-5,64,3 for a negative x)"))
    (when (= [(first a) (nth a 2)] [(first b) (nth b 2)]) (fail! :bad-to "--from and --to are the same column"))
    (when-not (and (plan-route/limit-arg tunnel) (plan-route/limit-arg bridge)) (fail! :bad-span "--tunnel and --bridge are integers from 0 to 64"))
    (when (and launch (not (and (js/Number.isSafeInteger launch-n) (<= 3 launch-n 5)))) (fail! :bad-launch "--launch is 3, 4 or 5 lit powered rails per launch group"))
    {:from a :to b :tunnel (plan-route/limit-arg tunnel) :bridge (plan-route/limit-arg bridge) :launch launch-n}))

(defn mineshaft-edn
  "The plan text of a mineshaft the options declare."
  [req]
  (pr-str {:id (:id req) :kind :mineshaft
           :parts [{:id "stair" :stair (mineshaft-args! req) :want :clear}]}))

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
                                    [:world :by :revision :edn :status :limit :offset :query :inventory :body :at :heading :to-y :from :to :tunnel :bridge :launch]))
                       {:state {:type "string"} :worlds {:type "string"}
                        :repo {:type "string" :default repo-root}
                        :blueprint {:type "string" :multiple true}}
                       (into {} (map (fn [k] [k {:type "boolean" :default false}])
                                    [:raw :geometry :large :dry-run])))
        parsed (data/from-json (.parseArgs util #js {:args argv :allowPositionals true :options (clj->js options)}))
        [command id & extra] (:positionals parsed)
        v (:values parsed)
        commands (if (= kind :plan) #{"list" "find" "show" "add" "edit" "remove" "validate" "check" "mineshaft" "route"}
                     #{"list" "find" "show" "save" "remove" "validate"})
        mutations #{"add" "edit" "save" "remove" "mineshaft" "route"}
        allowed (cond-> #{:world :worlds :state :repo}
                  (#{"list" "find"} command) (into [:limit :offset :raw :large])
                  (= command "show") (into [:raw :geometry :large])
                  (= command "validate") (into [:edn :limit :offset :raw :geometry :large])
                  (and (= command "validate") (= kind :plan)) (conj :blueprint)
                  (= command "check") (into [:body :inventory :blueprint :raw :geometry :large :limit :offset])
                  (mutations command) (into [:by :revision :dry-run :raw :large])
                  (#{"add" "edit" "save"} command) (conj :edn)
                  (= command "mineshaft") (into [:at :heading :to-y])
                  (= command "route") (into [:from :to :body :tunnel :bridge :launch]))]
    (when-not (and (:world v) (re-matches #"^[A-Za-z0-9_-]{1,64}$" (:world v)))
      (fail! :invalid-world "supply an explicit valid --world"))
    (when (and (= kind :plan) (= command "status"))
      (fail! :usage "plans have no status: every submitted plan is active, and a draft is a plan you keep locally; to retire a plan use `remove <id> --by <name> --revision <digest>`"))
    (when (and (#{"check" "route"} command) (not (re-matches #"^[A-Za-z0-9_-]{1,64}$" (str (:body v)))))
      (fail! :body-required "check and route need --body <name>: the body whose seen memory is judged"))
    (when (and (= kind :plan) (:status v))
      (fail! :bad-option "plans have no status field; every submitted plan is active"))
    (when-not (commands command) (fail! :usage (str "unknown " (name kind) " command " command)))
    (when (or (seq extra) (if (= command "list") (some? id) (nil? id)))
      (fail! :usage (str command (if (= command "list") " takes no ID" (if (= command "find") " needs one search string" " needs exactly one ID")))))
    (when (= command "mineshaft") (mineshaft-args! v))
    (when (= command "route") (route-args! v))
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
    (when (and (#{"add" "route"} command) (some? (:revision v))) (fail! :bad-option "add is create-only and does not take --revision"))
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
  (let [p (:value doc)]
    (cond-> {:id (:id doc) :revision (:revision doc) :note (:note p) :parts (count (:parts p))}
      (:kind p) (assoc :kind (:kind p))
      (shaft-of p) (assoc :shaft (select-keys (shaft-of p) [:heading :from :depth])))))
(defn blueprint-summary [doc errors]
  (let [bp (:value doc)]
    {:id (:id doc) :scope :global :revision (:revision doc) :title (:title bp) :front (:front bp)
     :width (count (first (first (:layers bp)))) :depth (count (first (:layers bp))) :height (count (:layers bp))
     :errors (errors-page errors)}))
(defn plan-output [doc ctx raw]
  (if raw {:ok true :kind :plan :id (:id doc) :world (:world ctx) :scope :world :revision (:revision doc) :document (raw-edn (:text doc))}
      (cond-> (assoc (plan-summary doc) :validation (errors-page (:errors (parse/parse (:text doc) (:id doc)))) :scope :world)
        (shaft-of (:value doc)) (assoc :shaft (shaft-of (:value doc))))))
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

(defn seen-checked
  "What a check rests on: how many of the cells the body has seen, how many never, and the age of the oldest and
  newest seen cell (seen-times: ms of each seen cell)."
  [cell-count seen-times memory]
  (let [now (js/Date.now)]
    {:cells cell-count :seen (count seen-times) :unseen (- cell-count (count seen-times)) :now now
     :memory-sections (aget memory "sections") :memory-file? (aget memory "found")
     :oldest (when (seq seen-times) (apply min seen-times)) :newest (when (seq seen-times) (apply max seen-times))}))

(def flush-timeout-ms 5000)

(defn flush-seen!
  "Asks the running body to write its sight memory to seen.bin now: a promise of {:flushed? true} or
  {:flushed? false :why <reason>} (no running body, an engine without the route, a failure); never rejects."
  [ctx body]
  (let [socket (.join path (:world-dir ctx) "agents" (str body) "engine" "events.sock")]
    (-> (http/request {:socket-path socket :method "POST" :path "/seen/flush" :label "seen flush" :timeout-ms flush-timeout-ms})
        (.then (fn [{:keys [status text]}]
                 (if (and (= 200 status) (str/includes? (str text) ":ok true"))
                   {:flushed? true}
                   {:flushed? false :why (if (= 404 status) :engine-without-flush :flush-refused)})))
        (.catch (fn [e] {:flushed? false :why (http/transport-reason e)})))))

(defn seen-age-note
  "What to tell when the seen data is the file on disk, not the body's sight now."
  [why]
  (str "seen data is read from the body's seen.bin, which the body writes about every minute (flush failed: " (name why)
       "); cells seen or built since the last write count as unseen or old. Check :checked :newest-age-ms."))

(defn check-plan [ctx text id inline req]
  (let [prepared (prepare ctx text id inline (read-zone-text ctx) (mapv :value (current-docs ctx :claim)))]
    (if (or (not (:ok prepared)) (nil? (:expansion prepared)))
      (js/Promise.resolve (assoc prepared :world (:world ctx) :evidence :seen-memory :live-loaded? false))
      (do
        (geometry-check! (:cells prepared) (:large req))
        (-> (js/Promise.all #js [(flush-seen! ctx (:body req)) ((:load-worldblocks req))])
            (.then (fn [[flush seen-module]]
                     (let [memory ((aget seen-module "createSeenBlocks") #js {:stateDir (clj->js (:state ctx)) :world (:world ctx) :body (:body req) :dim (:dim req)})
                           blocks (atom {}) seen-times (atom [])
                           inventory (inventory-value (:inventory req))
                           seen-name (fn [{:keys [x y z]}] (some-> (.call (aget memory "blockAt") memory x y z) (aget "name")))
                           expansion (cond-> (:expansion prepared)
                                       (:surface (:plan-value req)) (update :cells #(surface/follow-plan (:plan-value req) % (surface/top-fn seen-name))))
                           cells (mapv :pos (:cells expansion))]
                       (try
                         (doseq [[x y z :as pos] cells]
                           (when-let [block (.call (aget memory "blockAt") memory x y z)]
                             (swap! blocks assoc pos {:name (aget block "name") :state (js->clj (aget block "state"))})
                             (swap! seen-times conj (.call (aget memory "seenAt") memory x y z))))
                         (let [score (checks/score-native expansion #(get @blocks %) (constantly nil)
                                                         (or inventory {}) (some? inventory)
                                                         (js/Number (or (:offset req) 0)) (js/Number (or (:limit req) 10)))
                               checked (seen-checked (count cells) @seen-times memory)]
                           (cond-> (assoc (dissoc prepared :expansion :cells)
                                          :world (:world ctx) :evidence :seen-memory :body (:body req) :live-loaded? (:flushed? flush)
                                          :score (assoc score :checked
                                                        (assoc checked :newest-age-ms (when (:newest checked) (max 0 (- (:now checked) (:newest checked))))
                                                                       :oldest-age-ms (when (:oldest checked) (max 0 (- (:now checked) (:oldest checked)))))))
                             (shaft-progress (:plan-value req) #(some-> (get @blocks %) :name)) (assoc :shaft (shaft-progress (:plan-value req) #(some-> (get @blocks %) :name)))
                             (:geometry req) (assoc :cells cells)
                             (not (:flushed? flush)) (assoc :seen-note (seen-age-note (:why flush)))
                             (:large req) (assoc :large true)))
                         (finally (.call (aget memory "close") memory)))))))))))

(defn validate-revision! [revision]
  (when (and (some? revision) (not (re-matches #"^[a-f0-9]{24}$" revision)))
    (fail! :bad-revision "--revision must be the 24-character digest from show/list")))

(defn body-dimension
  "The dimension the body named by is in now (its view/pose.json); nil when by is no body or has no pose."
  [ctx by]
  (when (re-matches names (str by))
    (let [file (.join path (:world-dir ctx) "agents" by "view" "pose.json")]
      (try (when (.existsSync fs file)
             (let [dim (:dimension (js->clj (js/JSON.parse (.readFileSync fs file "utf8")) :keywordize-keys true))]
               (when (string? dim) dim)))
           (catch :default _ nil)))))

(defn mutate-plan [req ctx write]
  (let [old (data/read-document ctx :plan (:id req))
        command (:command req)]
    (when (and (= command "add") old) (fail! :already-exists (str "plan " (:id req) " already exists; use edit")))
    (when (and (#{"edit" "remove"} command) (nil? old)) (fail! :not-found (str "no plan " (:id req) " in " (:world ctx))))
    (validate-revision! (:revision req))
    (let [value (when-not (= command "remove") (some-> (edn-value (:edn req) "edn") (as-> v (if (map? v) (cond-> (shape/with-author v (or (shape/author (:value old)) (:by req)))
                                                                                                    (= command "add") (shape/with-dim (body-dimension ctx (:by req)))) v))))
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

(defn remove-blueprint [req ctx write]
  (when-not (data/read-document ctx :blueprint (:id req)) (fail! :not-found (str "no global blueprint " (:id req))))
  (validate-revision! (:revision req))
  (-> (data/mutate-document ctx {:kind :blueprint :id (:id req) :value nil :by (:by req)
                                 :expected-revision (:revision req) :dry-run (boolean (:dry-run req))})
      (.then (fn [result] (write (cond-> result (:dry-run req) (assoc :next :review-preview))) 0))))

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

(defn route-plan!
  "plans route: the seen ground between the two cells becomes the plan :id (see agent-tools.plan-route); the written
  plan's result carries the route summary. Flushes the body's sight memory first, as check does."
  [req ctx write]
  (let [{:keys [from to] :as args} (route-args! req)
        zones (plan-route/zone-list (read-zone-text ctx))]
    (when (:error zones) (fail! :bad-zones (:error zones)))
    (-> (js/Promise.all #js [(flush-seen! ctx (:body req)) ((:load-worldblocks req))])
        (.then (fn [[flush seen-module]]
                 (let [memory ((aget seen-module "createSeenBlocks") #js {:stateDir (clj->js (:state ctx)) :world (:world ctx) :body (:body req) :dim (shape/plan-dim {:dim (body-dimension ctx (:body req))})})
                       name-at (fn [{:keys [x y z]}] (some-> (.call (aget memory "blockAt") memory x y z) (aget "name")))]
                   (try
                     (assoc (plan-route/route-plan (assoc args :id (:id req) :by (:by req)) from to name-at (:zones zones)) :flush flush)
                     (finally (.call (aget memory "close") memory))))))
        (.then (fn [{:keys [plan summary error message flush]}]
                 (when error (fail! error message))
                 (mutate-plan (assoc req :command "add" :edn (pr-str plan)) ctx
                              (fn [result & _] (write (cond-> (assoc result :route summary)
                                                        (not (:flushed? flush)) (assoc :seen-note (seen-age-note (:why flush)))) 0))))))))

(defn execute-plan [req write]
  (let [ctx (data/context {:state (:state req) :worlds (:worlds req) :world (:world req) :repo-root (:repo req)}) command (:command req)]
    (cond
      (#{"list" "find"} command) (do (write (list-result :plan (current-docs ctx :plan) req)) 0)
      (#{"add" "edit" "remove"} command) (mutate-plan req ctx write)
      (= command "route") (route-plan! req ctx write)
      (= command "mineshaft") (mutate-plan (assoc req :command "add" :edn (mineshaft-edn req)) ctx write)
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
          (-> (js/Promise.resolve (if (= command "check") (check-plan ctx source (:id req) inline (assoc req :dim (shape/plan-dim value) :plan-value value))
                                      (prepare ctx source (:id req) inline (read-zone-text ctx) (mapv :value (current-docs ctx :claim)))))
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
      (= command "remove") (remove-blueprint req ctx write)
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
