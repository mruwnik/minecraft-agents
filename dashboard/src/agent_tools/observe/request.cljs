(ns agent-tools.observe.request
  "observe.mjs usage, option parsing and the request each command makes."
  (:require [engine.bodies :as bodies]
            [agent-tools.world-data :as data]
            [agent-tools.http :as http]
            [agent-tools.map :as map-tool]
            ["node:path" :as path]
            [clojure.string :as str]
            [agent-tools.observe.status :refer [truthy-text?]]))

(defn usage-for [prefix]
  "Build the usage string with the given invocation prefix (e.g., 'observe.mjs' or './bin/observe')."
  (str "usage: " prefix " <agent> --world <world> [status [--raw|--verbose] [--wait --timeout 60s --chatter addressed --observer agent --watch j12] | inventory [--raw] [--slots] | equipment [--raw] | seen [--radius 32 (1 to 64)] [--names a,b,*_ore] [--limit 16] | chests [--limit 16] | light [x y z] [--radius 0 to 8] | job <id> | result <id> | events [--kinds died,hurt --job j12 --level debug|info|warn --since 10m|death|<ms> --until death|10m|<ms> --before <seq> --limit 20 (1 to 64)] | catalog <job|trigger> <name> | catalog <jobs|triggers> [prefix] [--category goal|phase|reflex|debug] [--tag <word>] | settings [--changed] [--raw] | settings reload | settings set <:ns/key> <edn> | settings unset <:ns/key>] [--limit <n>] [--offset <n>] [--worlds <dir>] [--state <legacy-parent>]\n--wait: a hit on the body wakes it too (--no-danger turns that off; --danger-below 10 wakes only when health is 10 or less after the hit); job notices wake once per kind and job; notices of a job that has since ended go to the summary; with --watch only the watched jobs' notices wake; the summary's 4 items keep job ends, deaths and notices before routine ones; events over 10 min old are only counted.\nsettings: every key with value and layer (:code, :world, :body) and the game rate; set and unset edit the body's settings.edn and reload.\nchests: what the body saw in chests it opened (remembered, may be out of date), nearest first with :distance and :age-ms.\nlight: the light the body perceives at a cell (default its feet; a negative coordinate needs `--` before x y z): :block, :sky, :sky-subtract (night's darkening), :light (effective), :lit?, :dark?, :seen?; with --radius the cube around it as :dark-fraction, :darkest, :unseen, :cells, :seen.\nseen: blocks the body has seen (memory, nothing through walls), nearest first with :distance (a crop also :age and :ripe, a block within 32 of a zone :zone {:name :owner :distance}); default names are workstations, storage, beds, doors, ores and crops.\nevents: past events, newest page first, printed oldest first with :next-before (pass it as --before for older ones), :window {:scanned :oldest-seq :partial?}; debug-level noise is left out unless --level debug; --until death ends at the newest death, so a post-mortem is `events --until death`; --since/--until also take a duration back from now or an epoch ms; at most the last 8000 retained events are scanned.\nstatus: :missed lists job ends, flights, fight/flee decisions, reflex ends, deaths and one summed :hurt (damage, lowest-health, cause) since the last observe --wait. :pos [x y z], :health and :food 0-20, :current the running job, :mode scheduled or manual. job <id>: its state and outcome; an unknown id answers :unknown-job."))

(def usage (usage-for "observe.mjs"))

;; Requests

(def body-name #"[A-Za-z0-9_-]{1,40}")
(def world-name #"[A-Za-z0-9_-]{1,64}")

(def option-spec
  {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}
   :limit {:type "string"} :offset {:type "string"} :radius {:type "string"} :names {:type "string"} :category {:type "string"} :tag {:type "string"}
   :raw {:type "boolean" :default false} :slots {:type "boolean" :default false}
   :verbose {:type "boolean" :default false} :wait {:type "boolean" :default false}
   :timeout {:type "string"} :chatter {:type "string"} :observer {:type "string"}
   :watch {:type "string" :multiple true}
   :from {:type "string"} :poll-ms {:type "string"}
   :changed {:type "boolean" :default false} :no-danger {:type "boolean" :default false} :danger-below {:type "string"} :disconnect {:type "boolean" :default false}
   :quiet-notices {:type "boolean" :default false}
   :kinds {:type "string"} :job {:type "string"} :level {:type "string"} :since {:type "string"} :until {:type "string"} :before {:type "string"}})

(def event-options [:kinds :job :level :since :until :before])

(defn fail [message] (throw (ex-info message {::error message})))

(defn check [bad? message] (when bad? (fail message)))

(def seen-default-names
  ["crafting_table" "furnace" "blast_furnace" "smoker" "chest" "trapped_chest" "barrel" "ender_chest" "enchanting_table"
   "anvil" "brewing_stand" "ancient_debris" "*_bed" "*_door" "*_ore"
   "wheat" "carrots" "potatoes" "beetroots" "nether_wart" "melon" "pumpkin" "sweet_berry_bush" "cocoa"])

(defn integer-in? [n low high] (and (js/Number.isInteger n) (<= low n high)))

(defn limit-param
  "The validated --limit as a query value (nil when absent)."
  [limit high]
  (when (some? limit)
    (let [n (js/Number limit)]
      (check (not (integer-in? n 1 high)) (str "--limit must be an integer from 1 to " high))
      (str n))))

(defn listed [values] (vec (mapcat #(str/split % #",") values)))

(def duration-units {"ms" 1 "s" 1000 "m" 60000})

(defn wait-options [{:keys [timeout observer chatter watch from poll-ms no-danger danger-below disconnect quiet-notices]}]
  (let [[_ amount unit] (re-matches #"^(\d+(?:\.\d+)?)(ms|s|m)?$" (or timeout "60s"))
        timeout-ms (if amount (* (js/Number amount) (duration-units (or unit "s"))) js/NaN)
        observer (or observer "agent")
        chatter (or chatter "addressed")
        watch (listed (some-> watch array-seq))
        poll (js/Number (or poll-ms 250))
        below (when (some? danger-below) (js/Number danger-below))]
    (check (not (and (js/Number.isFinite timeout-ms) (<= 10 timeout-ms 3600000))) "--timeout must be between 10ms and 60m")
    (check (not (re-matches body-name observer)) "--observer must be 1-40 letters, digits, underscores or hyphens")
    (check (not (#{"none" "addressed" "all"} chatter)) "--chatter must be none, addressed, or all")
    (check (or (some #(not (re-matches #"j[0-9]+" %)) watch) (> (count watch) 32)) "--watch needs up to 32 comma-separated job IDs")
    (check (and (truthy-text? from) (not (re-matches body-name from))) "--from must be a player name")
    (check (not (integer-in? poll 50 5000)) "--poll-ms must be 50-5000")
    (check (and below (not (integer-in? below 1 21))) "--danger-below must be 1-21 (a hit wakes when health is at or below it)")
    {:timeout-ms timeout-ms :observer observer :chatter chatter :watch watch
     :poll-ms poll :from from :danger (not no-danger) :danger-below below :disconnect disconnect :quiet-notices quiet-notices}))

(def setting-key #":[a-z][a-z0-9.-]*/[a-z][a-z0-9-]*")

(defn valid-edn? [text]
  (try (data/read-edn text) true (catch :default _ false)))

(defn one-form? [text]
  (= 1 (count (data/read-edn (str "[" text "]")))))

(defn settings-request
  "[endpoint params extras] of the settings operation: extras is {:method :body} or {:settings-mode}."
  [sub args values]
  (check (some? (:limit values)) "--limit is not valid for settings")
  (let [[k v & more] args
        post (fn [endpoint body] [endpoint [] {:method "POST" :body body}])]
    (case sub
      nil (do (check (seq args) "settings takes no further arguments")
              ["/settings" [] {:settings-mode {:changed (boolean (:changed values)) :raw (boolean (:raw values))}}])
      "reload" (do (check (seq args) "settings reload takes no arguments")
                   (post "/settings/reload" "{}"))
      "set" (do (check (or (nil? k) (nil? v) (seq more)) "settings set needs a key and an EDN value")
                (check (not (re-matches setting-key k)) "the key must be a keyword such as :jobs.items.smelt/wait-ms")
                (check (not (valid-edn? v)) (str "the value is not valid EDN: " v))
                (check (not (one-form? v)) (str "the value must be exactly one EDN form: " v))
                (post "/settings/set" (pr-str {:key (keyword (subs k 1)) :value (data/read-edn v)})))
      "unset" (do (check (or (nil? k) (seq (next args))) "settings unset needs a key")
                  (check (not (re-matches setting-key k)) "the key must be a keyword such as :jobs.items.smelt/wait-ms")
                  (post "/settings/unset" (str "{:key " k "}")))
      (fail "settings needs reload, set <key> <edn>, unset <key> or no argument"))))

(defn endpoint-for
  "[endpoint params] of the operation, validating its arguments."
  [op kind-or-id rest values]
  (case op
    "status"
    (do (check (or (some? kind-or-id) (seq rest)) "status takes no positional arguments")
        (check (some? (:offset values)) "--offset is only valid for catalog lists")
        (check (and (:raw values) (some? (:limit values))) "--limit cannot be combined with --raw")
        [(if (:raw values) "/snapshot" "/status")
         (if-let [limit (limit-param (:limit values) 32)] [["limit" limit]] [])])

    ("inventory" "equipment")
    (do (check (or (some? kind-or-id) (seq rest)) (str op " takes no positional arguments"))
        (check (or (some? (:limit values)) (some? (:offset values))) (str "--limit and --offset are not valid for " op))
        (check (and (:raw values) (:slots values)) "--slots is redundant with --raw")
        (check (and (= "equipment" op) (:slots values)) "--slots is only valid for inventory")
        ["/inventory" []])

    "seen"
    (let [radius (js/Number (or (:radius values) 32))
          limit (or (limit-param (:limit values) 64) "16")
          names (if (some? (:names values)) (str/split (:names values) #",") seen-default-names)]
      (check (or (some? kind-or-id) (seq rest)) "seen takes no positional arguments")
      (check (some? (:offset values)) "--offset is not valid for seen")
      (check (not (integer-in? radius 1 64)) "--radius must be an integer from 1 to 64")
      (check (or (> (count names) 32) (not-every? #(re-matches #"\*?[a-z0-9_]{1,80}" %) names))
             "--names needs up to 32 comma-separated block names (a *suffix matches every name ending so)")
      ["/seen" [["names" (str/join "," names)] ["radius" (str radius)] ["max" limit]]])

    "chests"
    (do (check (or (some? kind-or-id) (seq rest)) "chests takes no positional arguments")
        (check (some? (:offset values)) "--offset is not valid for chests")
        ["/chests" [["max" (or (limit-param (:limit values) 64) "16")]]])

    "light"
    (let [coords (remove nil? [kind-or-id (first rest) (second rest)])
          radius (js/Number (or (:radius values) 0))]
      (check (not (#{0 3} (count coords))) "light takes no cell (the feet) or x y z")
      (check (> (count rest) 2) "light takes at most x y z")
      (check (not-every? #(re-matches #"-?[0-9]+" %) coords) "light x y z must be integers")
      (check (some? (:limit values)) "--limit is not valid for light")
      (check (some? (:offset values)) "--offset is not valid for light")
      (check (not (integer-in? radius 0 8)) "--radius must be an integer from 0 to 8")
      ["/light" (into (if (seq coords) (mapv vector ["x" "y" "z"] coords) [])
                      (when (some? (:radius values)) [["radius" (str radius)]]))])

    ("job" "result")
    (do (check (or (not (truthy-text? kind-or-id)) (seq rest)) (str op " needs one job ID, such as j12"))
        (check (and (= "result" op) (not (re-matches #"j[0-9]+" kind-or-id))) "result needs a job ID such as j12")
        (check (and (= "result" op) (some? (:limit values))) "result has a fixed bounded history; --limit is not accepted")
        (check (or (:raw values) (some? (:offset values))) "--raw and --offset are only valid for status and catalog lists respectively")
        ["/job" (into [["id" kind-or-id]] (when-let [limit (limit-param (:limit values) 32)] [["limit" limit]]))])

    "catalog"
    (cond
      (#{"job" "trigger"} kind-or-id)
      (let [[name] rest]
        (check (or (:raw values) (some? (:limit values)) (some? (:offset values))) "catalog detail does not accept --raw, --limit, or --offset")
        (check (or (not (truthy-text? name)) (not= 1 (count rest))) "catalog needs job <jobs.namespace.name> or trigger <trigger-name>")
        (check (and (= "job" kind-or-id) (not (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)+" name))) "job name must be an exact jobs namespace")
        (check (and (= "trigger" kind-or-id) (not (re-matches #"[a-z][a-z0-9-]*" name))) "trigger name must be a lowercase identifier")
        ["/catalog" [["kind" kind-or-id] ["name" name]]])

      (#{"jobs" "triggers"} kind-or-id)
      (let [prefix (or (first rest) "")
            limit (if (nil? (:limit values)) 20 (js/Number (:limit values)))
            offset (if (nil? (:offset values)) 0 (js/Number (:offset values)))]
        (check (:raw values) "--raw is only valid for status")
        (check (> (count rest) 1) "catalog list accepts at most one prefix")
        (check (and (= "jobs" kind-or-id) (truthy-text? prefix) (not (re-matches #"jobs(?:\.[a-z][a-z0-9-]*)*(?:\.)?" prefix)))
               "job prefix must start with jobs.")
        (check (and (= "triggers" kind-or-id) (truthy-text? prefix) (not (re-matches #"[a-z][a-z0-9-]*" prefix)))
               "trigger prefix must be a lowercase identifier prefix")
        (check (not (integer-in? limit 1 64)) "--limit must be an integer from 1 to 64")
        (check (not (integer-in? offset 0 10000)) "--offset must be an integer from 0 to 10000")
        (check (and (:category values) (not= "jobs" kind-or-id)) "--category is only valid for catalog jobs")
        (check (and (:category values) (not (#{"goal" "phase" "reflex" "debug"} (:category values)))) "--category must be goal, phase, reflex or debug")
        (check (and (:tag values) (not= "jobs" kind-or-id)) "--tag is only valid for catalog jobs")
        (check (and (:tag values) (not (re-matches #"[a-z][a-z0-9-]{0,40}" (:tag values)))) "--tag must be a lowercase word")
        ["/catalog" (cond-> [["kind" kind-or-id] ["prefix" prefix] ["limit" (str limit)] ["offset" (str offset)]]
                      (:category values) (conj ["category" (:category values)])
                      (:tag values) (conj ["tag" (:tag values)]))])

      :else (fail "catalog needs job|trigger <name> or jobs|triggers [prefix]"))

    "events"
    (let [kinds (some-> (:kinds values) (str/split #","))
          time-ok? #(or (nil? %) (= "death" %) (re-matches #"\d+(ms|s|m|h)?" %))]
      (check (or (some? kind-or-id) (seq rest)) "events takes no positional arguments")
      (check (or (some? (:offset values)) (:raw values)) "--offset and --raw are not valid for events")
      (check (or (> (count kinds) 16) (not-every? #(re-matches #"[a-z][a-z0-9._-]{0,60}" %) kinds)) "--kinds needs up to 16 comma-separated event kinds such as died,hurt")
      (check (and (:job values) (not (re-matches #"j[0-9]+" (:job values)))) "--job needs a job ID such as j12")
      (check (and (:level values) (not (#{"debug" "info" "warn" "error"} (:level values)))) "--level must be debug, info, warn or error")
      (check (not (and (time-ok? (:since values)) (time-ok? (:until values))))
             "--since and --until take death, a duration such as 10m (ms, s, m, h) or an epoch ms")
      (check (and (:before values) (not (re-matches #"\d+" (:before values)))) "--before takes the :next-before seq of the previous page")
      (let [limit (limit-param (:limit values) 64)]
        ["/snapshot" [] {:events-query (cond-> {:limit (js/Number (or limit 20))}
                                         kinds (assoc :kinds (set kinds))
                                         (:job values) (assoc :job (:job values))
                                         (:level values) (assoc :level (keyword (:level values)))
                                         (:since values) (assoc :since (:since values))
                                         (:until values) (assoc :until (:until values))
                                         (:before values) (assoc :before (js/Number (:before values))))}]))

    "settings" (settings-request kind-or-id rest values)

    (fail (str "unknown operation " op))))

(defn query-string [params]
  (let [search (js/URLSearchParams.)]
    (doseq [[k v] params] (.set search k v))
    (.toString search)))

(defn request-for
  "The request an argv describes: {:agent :world :state :socket-path :path} plus :wait-options, :inventory-mode with
  :slots and :raw, and :verbose when asked for; {:error message} when it is not valid."
  [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options argv option-spec)
          [agent requested-op kind-or-id & rest] positionals
          op (or requested-op "status")
          world (:world values)]
      (check (and (some? (:radius values)) (not (#{"seen" "light"} op))) "--radius is only valid for seen and light")
      (check (and (some? (:names values)) (not= "seen" op)) "--names is only valid for seen")
      (check (and (:slots values) (not= "inventory" op)) "--slots is only valid for inventory")
      (check (and (some #(some? (get values %)) event-options) (not= "events" op)) "--kinds, --job, --level, --since, --until and --before are only valid for events")
      (check (not (and agent (re-matches body-name agent))) "agent must be a body name")
      (check (nil? world) (bodies/missing-world-error "--world"))
      (check (not (re-matches world-name world)) "the world must be a name of letters, digits, _ and -")
      (let [state (bodies/storage-root values map-tool/default-state-dir)
            [endpoint params extras] (endpoint-for op kind-or-id rest values)
            wait-options (when (:wait values)
                           (check (or (not= "status" op) (:raw values) (:verbose values) (truthy-text? (:limit values)))
                                  "--wait is only valid with compact status")
                           (wait-options values))
            query (query-string params)]
        (when-not (:wait values)
          (check (or (some #(some? (get values %)) [:timeout :chatter :observer :watch :from :poll-ms :danger-below])
                     (:no-danger values) (:disconnect values) (:quiet-notices values))
                 "wait options require --wait"))
        (check (and (:verbose values) (or (not= "status" op) (:raw values)))
               "--verbose is only valid for status without --raw")
        (cond-> {:agent agent :world world :state state
                 :socket-path (.join path (bodies/body-dir state world agent) "engine" "events.sock")
                 :path (if (seq query) (str endpoint "?" query) endpoint)}
          wait-options (assoc :wait-options wait-options)
          (:method extras) (assoc :method (:method extras) :body (:body extras))
          (:settings-mode extras) (assoc :settings-mode (:settings-mode extras))
          (= "result" op) (assoc :result-id kind-or-id)
          (:events-query extras) (assoc :events-query (:events-query extras))
          (#{"inventory" "equipment"} op) (assoc :inventory-mode (keyword op) :slots (:slots values) :raw (:raw values))
          (:verbose values) (assoc :verbose true))))
    (catch :default error
      {:error (or (some-> (ex-data error) ::error) (.-message error))})))

(defn legacy-notice [request]
  (let [endpoint (first (str/split (:path request) #"\?"))
        fallback (if (= "/inventory" endpoint)
                   ""
                   (str " :fallback {:op :status :raw true :world " (js/JSON.stringify (:world request))
                        " :state " (data/write-edn (:state request)) "}"))]
    (str "{:ok false :reason :observe-unavailable :body " (js/JSON.stringify (:agent request))
         " :endpoint " (js/JSON.stringify endpoint) " :action :restart-with-current-build" fallback "}")))

(def unsupported-route? http/unsupported-route?)
