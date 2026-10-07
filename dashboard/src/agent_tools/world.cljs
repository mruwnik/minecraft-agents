(ns agent-tools.world
  "Manual actions as jobs: world.mjs <agent> submit <action> ... submits the job of that action as the driver."
  (:require [agent-tools.drive :as drive]
            [agent-tools.http :as http]
            [agent-tools.jobs :as jobs]
            [agent-tools.map :as map-tool]
            [agent-tools.observe.request :as observe-request]
            [engine.bodies :as bodies]
            [clojure.string :as str]
            ["node:crypto" :as crypto]
            ["node:path" :as path]))

(def usage
  (str "usage: world.mjs <agent> submit <action> [args] --world <world> [--worlds <dir>] [--state <legacy-parent>] [--who NAME]\n"
       "  submit move-to <x> <y> <z> [--range <n>] [--doors shut|leave-open|never] [--no-escalate]   (jobs.movement.go-to)\n"
       "  submit dig <x> <y> <z> [--ignore-zones] | submit place <x> <y> <z> <item> [--ignore-zones]   (jobs.blocks.dig / place)\n"
       "  submit use-on <x> <y> <z> [--item <item>] [--face up|down|north|south|east|west]   (jobs.blocks.use-on)\n"
       "  submit interact <entity-id> [--item <item>]   (jobs.items.interact)\n"
       "  submit wear [<item>]   (jobs.items.wear: a carried armour piece; no item: the best carried piece for each empty or weaker slot)\n"
       "  submit equip <item> [--hand main|off]   (jobs.items.equip: hold a carried item in the main hand or off-hand; refuses a missing item)\n"
       "  submit mount <entity-id|name> | submit dismount   (jobs.movement.mount: walk to a boat, raft, minecart or rideable mob (by id, or the nearest of a name) and get on; jobs.movement.leave-vehicle: get off)\n"
       "Acquire the body first: drive.mjs take --who NAME --why \"<text>\" --idle-s N, and pass the same --who here.\n"
       "Under manual control the driver's job is the one slot: no other job runs, a new submit replaces the running one, and the body idles after it;\n"
       "jobs.mjs cancel <jID> or drive.mjs stop cancels it. Without manual control (or as another --who) the job joins the normal list.\n"
       "Submit returns at once with the job; add --wait [--timeout 60s] to block until it ends (or until anything that ends observe --wait)\n"
       "and print {:job .. :wait <the wake>}. Status, cancel and inventory: jobs.mjs show|cancel, observe inventory."))

(def options
  {:who {:type "string" :default "claude"} :state {:type "string"} :worlds {:type "string"} :world {:type "string"}
   :range {:type "string"} :doors {:type "string"} :no-escalate {:type "boolean"} :ignore-zones {:type "boolean"}
   :item {:type "string"} :hand {:type "string"} :face {:type "string"} :request-id {:type "string"}
   :wait {:type "boolean"} :timeout {:type "string"}})

(def action-options [:range :doors :no-escalate :ignore-zones :item :face :hand])

(def allowed
  {"move-to" #{:range :doors :no-escalate} "dig" #{:ignore-zones} "place" #{:ignore-zones}
   "use-on" #{:item :face} "interact" #{:item} "wear" #{} "equip" #{:hand} "mount" #{} "dismount" #{}})

(def doors #{"shut" "leave-open" "never"})

(defn fail [message] (throw (js/Error. message)))

(defn num [text name]
  (when-not (and (some? text) (not (str/blank? text)) (js/Number.isFinite (js/Number text)))
    (fail (str name " must be a finite number")))
  (js/Number text))

(defn pos-args [action args expected]
  (when-not (= expected (count args))
    (fail (str action " needs " (if (= expected 4) "x y z item" "x y z"))))
  {:pos {:x (num (nth args 0) "x") :y (num (nth args 1) "y") :z (num (nth args 2) "z")}})

(defn spec-for
  "The job spec of a manual action: (job-name args-map)."
  [action args {:keys [range doors no-escalate ignore-zones item face hand]}]
  (let [zones #(cond-> % ignore-zones (assoc :ignore-zones? true))]
    (case action
      "move-to" (list 'jobs.movement.go-to
                      (cond-> (pos-args action args 3)
                        (some? range) (assoc :range (num range "--range"))
                        (some? doors) (assoc :doors (keyword doors))
                        no-escalate (assoc :escalate false)))
      "dig" (list 'jobs.blocks.dig (zones (pos-args action args 3)))
      "place" (list 'jobs.blocks.place (zones (assoc (pos-args action args 4) :item (nth args 3))))
      "use-on" (list 'jobs.blocks.use-on (cond-> (pos-args action args 3) (seq item) (assoc :item item) (seq face) (assoc :face face)))
      "wear" (do (when (> (count args) 1) (fail "wear needs at most one item"))
                 (list 'jobs.items.wear (if (seq args) {:item (first args)} {})))
      "equip" (do (when-not (= 1 (count args)) (fail "equip needs one item"))
                  (when-not (contains? #{nil "main" "off"} hand) (fail "--hand must be main or off"))
                  (list 'jobs.items.equip (cond-> {:item (first args)} (some? hand) (assoc :hand hand))))
      "mount" (do (when-not (= 1 (count args)) (fail "mount needs one entity-id or entity name"))
                  (list 'jobs.movement.mount (if (re-matches #"[0-9]+" (first args))
                                               {:id (num (first args) "entity-id")}
                                               {:name (first args)})))
      "dismount" (do (when (seq args) (fail "dismount takes no arguments"))
                     (list 'jobs.movement.leave-vehicle {}))
      "interact" (do (when-not (= 1 (count args)) (fail "interact needs one entity-id"))
                     (list 'jobs.items.interact (cond-> {:id (num (first args) "entity-id")} (seq item) (assoc :item item))))
      (fail (str "unknown action " (or action "undefined"))))))

(defn submit-request [action args who values]
  (let [unsupported (first (filter (fn [k] (and (some? (k values)) (not (contains? (get allowed action #{}) k)))) action-options))
        _ (when unsupported (fail (str "--" (name unsupported) " is not valid for " action)))
        _ (when (and (some? (:doors values)) (not (doors (:doors values)))) (fail "--doors must be shut, leave-open or never"))
        spec (spec-for action args values)
        request-id (or (:request-id values) (.randomUUID crypto))]
    (when-not (re-matches #"[A-Za-z0-9._-]{1,80}" request-id)
      (fail "--request-id must be 1..80 letters, digits, dot, _ or -"))
    {:op :submit :request-id request-id :by who :spec spec}))

(defn command-request [command action args who values]
  (case command
    "submit" (submit-request action args who values)
    "status" (fail "status is gone: jobs.mjs show <jID> (the submit answer names the job)")
    "cancel" (fail "cancel is gone: jobs.mjs cancel <jID>, or drive.mjs stop")
    "inventory" (fail "inventory is gone: observe inventory")
    (fail (str "unknown command " command))))

(defn request-for-unsafe
  "The jobs.cljs request map (see agent-tools.jobs/request-for) of a world.mjs command, or {:error text}."
  [argv]
  (let [{:keys [positionals values]} (map-tool/parse-options (mapv drive/hide argv) options)
        [agent command action & args] (map drive/unhide positionals)
        world (:world values)
        who (:who values)]
    (cond
      (not (and (string? agent) (re-matches #"[A-Za-z0-9_-]{1,40}" agent)))
      {:error "agent must be a body name (letters, digits, _ or -, max 40)"}
      (or (nil? command) (= "" command)) {:error "need <agent> and <command>"}
      (nil? world) {:error (bodies/missing-world-error "--world")}
      (not (re-matches bodies/name-re world)) {:error "the world must be a name of letters, digits, _ and -"}
      (not (<= 1 (count who) 80)) {:error "--who must be 1..80 characters"}
      (and (some? (:request-id values)) (not= command "submit")) {:error "--request-id is only valid for submit"}
      (and (not= command "submit") (some #(some? (% values)) action-options)) {:error "action options are only valid with submit"}
      (and (:wait values) (not= command "submit")) {:error "--wait requires submit"}
      (and (:timeout values) (not (:wait values))) {:error "--timeout requires --wait"}
      :else (let [request (command-request command action (vec args) who values)
                  state (bodies/storage-root values map-tool/default-state-dir)]
              (when (:wait values) (observe-request/wait-options {:timeout (:timeout values)}))
              (cond-> {:body agent :world world :state state :who who :path "/jobs" :mutating true :request request
                       :socketPath (.join path (bodies/body-dir state world agent) "engine" "events.sock")}
                (:wait values) (assoc :wait {:timeout (:timeout values)}))))))

(defn request-for [argv]
  (try (request-for-unsafe argv) (catch :default error {:error (.-message error)})))

(defn print-text! [text] (.write (.-stdout js/process) text))

(defn main!
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [output] :or {output print-text!} :as opts}]
   (let [r (request-for argv)]
     (if (:error r)
       (http/print-bad-args! output (:error r) usage)
       (jobs/run-request! r output opts)))))
