(ns agent-tools.progress
  "progress.mjs <agent> --world <world> [--goal j14] [--want iron_ingot:24,iron_pickaxe] [--wait 60m [--stall 10m]]:
  the cheap check-in. Without --wait one line: health and food, the goal job's state, counts toward the wanted items
  and what needs the agent. --wait blocks (observe's wait under the observer `progress`, so nothing between calls is
  missed) until the goal job ends, the body dies or the agent is needed, and prints that, the line and up to 3 items.
  Reflexes, notices, night, routine job ends and engine restarts do not end it. --goal and --want are kept in
  worlds/<world>/observers/<agent>/progress-plan.edn for later calls (`none` clears one)."
  (:require [engine.bodies :as bodies]
            [agent-tools.http :as http]
            [agent-tools.inventory :as inventory]
            [agent-tools.job-results :as job-results]
            [agent-tools.map :as map-tool]
            [agent-tools.observe :as observe]
            [agent-tools.observe.request :as observe-request]
            [agent-tools.observe.status :as status :refer [clip coded code-of round1]]
            [agent-tools.vitals :as vitals]
            [agent-tools.warn-snapshot :as warn-snapshot]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def usage
  (str "usage: progress.mjs <agent> --world <world> [--goal <jN|none>] [--want <item[:n],...|none>] [--wait <60m> [--stall <10m>] [--no-danger | --danger-below <hp>]] [--worlds <dir>] [--state <legacy-parent>]\n"
       "One line: hp, food, the goal job, counts toward --want (carried and worn items only, not chest stock), queue size and needs-you (attention, a job waiting for something only you can fix, failures, a recent death, idle, manual, offline). "
       "--goal and --want are kept for later calls. --wait <dur> (10s-24h) blocks until the goal job ends, the body dies, a hit leaves health at or below --danger-below (default 10; --no-danger: never), a job gives up, the body idles for a reason you must clear, new attention, addressed chat, the body goes offline, or no progress for --stall (default 10m; job, position, inventory unchanged); then prints a WAKE line, the line and up to 3 recent items. Run it in the background."))

(def routine-reasons
  "Wait reasons that clear by themselves (time, healing, a furnace, the night handler) or mean nothing is wrong: no
  needs-you while a job waits for one of these."
  #{:night :night-safe :night-sheltered :not-night :day :dark :cooking :low-health :needs-health
    :chunks-loading :not-loaded :goal-unloaded :receding :walk-waiting :falling-block :not-ready :not-hungry
    :not-burning :not-underwater :not-wedged :not-mounted :no-hostile :no-ranged-danger :nothing-to-do
    :nothing-to-restore :reflex-holds :enough-room :already-sealed :already-sound :at-target})

(def recent-death-s 600)
(def default-stall-ms (* 10 60 1000))

(def default-danger-below 10)
(def default-slice-ms 60000)
(def unreachable-ms 120000)
(def max-line 300)
(def max-items 3)

;; Requests

(def item-name #"[a-z0-9_]{1,80}")

(defn parse-want
  "\"iron_ingot:24,iron_pickaxe\" -> [[\"iron_ingot\" 24] [\"iron_pickaxe\" 1]]; throws on a bad entry."
  [text]
  (let [entries (str/split text #",")]
    (when (> (count entries) 16) (throw (js/Error. "--want takes at most 16 items")))
    (mapv (fn [entry]
            (let [[_ item n] (re-matches #"([^:]+)(?::(\d+))?" entry)
                  n (if n (js/Number n) 1)]
              (when-not (and item (re-matches item-name item) (<= 1 n 100000))
                (throw (js/Error. (str "--want needs item names with an optional :count (1-100000): " entry))))
              [item n]))
          entries)))

(def duration-units {"ms" 1 "s" 1000 "m" 60000 "h" 3600000})

(defn duration-ms [flag text]
  (let [[_ amount unit] (re-matches #"^(\d+(?:\.\d+)?)(ms|s|m|h)?$" text)
        ms (when amount (* (js/Number amount) (duration-units (or unit "s"))))]
    (when-not (and ms (<= 10000 ms 86400000)) (throw (js/Error. (str flag " must be a duration from 10s to 24h, such as 60m"))))
    ms))

(defn plan-file [state world agent]
  (.join path (bodies/worlds-dir state) world "observers" agent "progress-plan.edn"))

(defn request-for
  "{:base {:agent :world :state :socket-path} :goal :want :wait-ms :stall-ms :plan-file} or {:error message}."
  [argv]
  (try
    (let [{:keys [positionals values]} (map-tool/parse-options (vec argv)
                                          {:state {:type "string"} :worlds {:type "string"} :world {:type "string"}
                                           :goal {:type "string"} :want {:type "string"} :wait {:type "string"}
                                           :stall {:type "string"} :workspace {:type "string"} :no-danger {:type "boolean" :default false} :danger-below {:type "string"}})
          [agent & extra] positionals
          {:keys [world goal want wait stall no-danger danger-below workspace]} values]
      (when-not (and (string? agent) (re-matches observe-request/body-name agent)) (throw (js/Error. "agent must be a body name")))
      (when-not (and (string? world) (re-matches observe-request/world-name world)) (throw (js/Error. "missing or invalid --world <world>")))
      (when (seq extra) (throw (js/Error. "progress takes one body name")))
      (when (and goal (not (re-matches #"j[0-9]+|none" goal))) (throw (js/Error. "--goal needs a job ID such as j14, or none")))
      (when (and danger-below (not (<= 1 (js/Number danger-below) 21))) (throw (js/Error. "--danger-below must be 1-21")))
      (when (and stall (not wait)) (throw (js/Error. "--stall needs --wait")))
      (let [state (bodies/storage-root values map-tool/default-state-dir)]
        (cond-> {:base {:agent agent :world world :state state
                        :socket-path (.join path (bodies/body-dir state world agent) "engine" "events.sock")}
                 :plan-file (plan-file state world agent)
                 :workspace (.resolve path (or workspace (bodies/body-dir state world agent)))}
          goal (assoc :goal goal)
          want (assoc :want (if (= "none" want) "none" (parse-want want)))
          wait (assoc :wait-ms (duration-ms "--wait" wait)
                      :stall-ms (if stall (duration-ms "--stall" stall) default-stall-ms)
                      :danger (not no-danger)
                      :danger-below (if danger-below (js/Number danger-below) default-danger-below)))))
    (catch :default error {:error (.-message error)})))

(defn remember-plan!
  "The request with :goal and :want from the saved plan when not given; given ones (\"none\" clears) are saved."
  [{:keys [goal want] :as r}]
  (let [file (:plan-file r)
        saved (when (.existsSync fs file) (data/read-edn (.readFileSync fs file "utf8")))
        pick (fn [given old] (cond (= "none" given) nil (some? given) given :else old))
        plan {:goal (pick goal (:goal saved)) :want (pick want (:want saved))}]
    (when (or goal want)
      (.mkdirSync fs (.dirname path file) #js {:recursive true})
      (.writeFileSync fs file (str (data/write-edn plan) "\n")))
    (merge r plan)))

;; The line

(defn reason-of [waiting] (if (map? waiting) (:reason waiting) waiting))

(defn word [x] (if (keyword? x) (name x) (str x)))

(defn routine-wait?
  "A wait the engine ends itself: a routine reason, or a check that daylight lowers (:lowered-by has :light)."
  [waiting]
  (or (contains? routine-reasons (reason-of waiting))
      (and (map? waiting) (boolean (some #(= "light" (word %)) (:lowered-by waiting))))))

(defn wait-text [waiting]
  (str (word (reason-of waiting)) (when-let [item (and (map? waiting) (:item waiting))] (str " " item))
       (some->> (and (map? waiting) (:next waiting)) job-results/next-text (str ", next: "))))

(defn job-rows [s] (cond->> (vec (get-in s [:jobs :items])) (:current s) (cons (:current s))))

(def low-hp 4)

(defn needs-you
  "What only the agent can clear, as short texts, from a compact status (observe.status/compact-status)."
  [s]
  (let [rows (job-rows s)
        died (:died s)]
    (vec (concat
          (when (:offline s) ["offline"])
          (when (and died (< (or (:ago-s died) 0) recent-death-s))
            [(str "died " (:ago-s died) "s ago" (when (:cause died) (str " (" (:cause died) ")")))])
          (when (seq (:muted-triggers s)) [(str "muted " (str/join "," (map word (:muted-triggers s))))])
          (map #(str "attention " (:job %) " " (word (:reason %))) (get-in s [:attention :items]))
          (map #(str "failed " (:id %) " " (clip (:error %) 60)) (get-in s [:failed :items]))
          (keep #(when (and (:waiting %) (not (routine-wait? (:waiting %))))
                   (str (:id %) " waits " (wait-text (:waiting %))))
                rows)
          (when (and (:health s) (<= (:health s) low-hp)) [(str "low hp " (round1 (:health s)))])
          (when (and (:idle s) (empty? rows)) ["idle: no jobs"])))))

(defn want-counts
  "[[item have wanted] ...] from a compact inventory (agent-tools.inventory/compact): carried plus worn."
  [inv want]
  (mapv (fn [[item n]]
          [item (+ (get (:counts inv) item 0)
                   (count (filter #(and (map? %) (= item (:name %))) (vals (:equipment inv)))))
           n])
        want))

(defn short-name [n] (clip (str/replace (str n) "(jobs." "(") 40))

(defn row-state [row] (if (:waiting row) (str "waits " (wait-text (:waiting row))) (word (:status row))))

(defn goal-text [s goal goal-job]
  (let [row (first (filter #(= goal (:id %)) (job-rows s)))
        current (:current s)]
    (cond
      row (str "goal " goal " " (short-name (:name row)) " " (row-state row))
      (and (:finished? goal-job) current (not (:reflex current)))
      (str "goal " goal " " (word (:status goal-job)) " (old goal; --goal " (:id current) " follows the running job)")
      (:finished? goal-job) (str "goal " goal " " (word (:status goal-job)))
      :else (str "goal " goal " gone"))))

(defn progress-line
  "One line of at most max-line characters from {:status (compact) :goal :goal-job (/job answer) :want-counts}."
  [{s :status :keys [goal goal-job want-counts]}]
  (let [current (:current s)
        needs (needs-you s)
        queued (get-in s [:jobs :total])
        parts [(str "hp " (some-> (:health s) round1) " food " (:food s))
               (when goal (goal-text s goal goal-job))
               (when (and current (not= goal (:id current)))
                 (str "now " (:id current) " " (short-name (:name current)) " " (row-state current)))
               (when (seq want-counts) (str/join " " (map (fn [[item have n]] (str item " " have "/" n)) want-counts)))
               (when queued (str "queue " queued))
               (when (seq needs)
                 (str "needs-you: " (str/join "; " (take 3 needs))
                      (when (> (count needs) 3) (str " +" (- (count needs) 3)))))]]
    (clip (str/join " | " (remove nil? parts)) max-line)))

;; Reading the body

(defn fatal? [error] (contains? #{"ABORT_ERR" "EOBSERVERBUSY" "EOBSERVERLIMIT"} (code-of error)))

(defn ^:async read-edn!
  "GET endpoint as EDN: {:status :value}."
  [get! base endpoint signal]
  (let [{:keys [status content-type text]} (await (get! (:socket-path base) endpoint {:signal signal}))]
    (when-not (http/edn-response? content-type) (throw (coded "EBADRESPONSE" "not EDN")))
    {:status status :value (data/read-edn text)}))

(defn ^:async read-view!
  "{:status (compact) :goal :goal-job :inventory (compact) :want-counts} of the body now."
  [get! {:keys [base goal want]} signal]
  (let [st (await (read-edn! get! base "/status?limit=32" signal))
        _ (when-not (= 200 (:status st)) (throw (coded "EOBSERVEUNAVAILABLE" "no status")))
        muted (try (let [r (await (read-edn! get! base "/triggers" signal))]
                     (when (= 200 (:status r)) (mapv :id (filter :muted (:items (:value r))))))
                   (catch :default error (when (fatal? error) (throw error))))
        s (cond-> (status/compact-status (:value st)) (seq muted) (assoc :muted-triggers muted))
        inv (inventory/compact (:value (await (read-edn! get! base "/inventory" signal))) :inventory)
        listed? (some #(= goal (:id %)) (job-rows s))
        goal-job (when (and goal (not listed?))
                   (let [r (await (read-edn! get! base (str "/job?id=" goal) signal))]
                     (when (= 200 (:status r)) (:value r))))]
    {:status s :goal goal :goal-job goal-job :inventory inv :want-counts (want-counts inv want)}))

(defn fingerprint
  "What changes when the body gets on: the goal job's state, the items carried and the want counts. The 4-block cell
  counts only while a non-reflex job is current: a reflex fires with a new id each time, and fleeing in a loop is not progress."
  [{s :status :keys [goal inventory want-counts]}]
  (let [current (:current s)
        row (first (filter #(= goal (:id %)) (job-rows s)))]
    [(:id row) (:status row) (:waiting row)
     (when-not (:reflex current) (mapv #(js/Math.floor (/ % 4)) (:pos s)))
     (:total-items inventory) want-counts]))

(defn watch-ids
  "The goal and the listed (not reflex) jobs of a view, at most 32."
  [{s :status :keys [goal]}]
  (->> (cons goal (map :id (remove :reflex (job-rows s))))
       (filter #(and (string? %) (re-matches #"j[0-9]+" %)))
       distinct (take 32) vec))

;; Wakes

(defn routine-idle?
  "Whether a body.idle is the engine working normally: some job holds on purpose or waits for a routine reason, and
  no job failed or waits for another. Jobs with neither (queued behind the running one) do not count; idle-ok jobs
  (a noop) neither."
  [jobs]
  (let [real (remove :idle-ok jobs)
        routine? #(or (:holding %) (and (:waiting %) (routine-wait? (:waiting %))))]
    (boolean (and (some routine? real)
                  (every? #(and (not (:failed %)) (or (routine? %) (not (:waiting %)))) real)))))

(defn idle-text [jobs]
  (if (every? :idle-ok jobs)
    (if (empty? jobs) "no jobs" "no work, only idle jobs")
    (str/join "; " (map #(str (:job %) " " (cond (:failed %) (str "failed " (clip (:failed %) 60))
                                                    (:waiting %) (str "waits " (wait-text (:waiting %)))
                                                    (:holding %) "holding"
                                                    :else "no act"))
                        (take 3 jobs)))))

(defn wake-verdict
  "nil for a wake of observe's wait the agent need not hear (the wait goes on), else {:wake :text}: :died, :chat,
  :goal-ended (the goal job ended, any way), :gave-up (another listed job ended neither completed nor cancelled) or
  :needs-you (attention, offline, an idle body with a job waiting for a reason not in routine-reasons)."
  [wake goal]
  (case (:wake wake)
    :died {:wake :died :text (str/join " " (remove nil? [(:cause wake) (some->> (:at wake) (str "at "))
                                                        (when (seq (:cancelled-jobs wake))
                                                          (str "cancelled " (str/join "," (map :id (:cancelled-jobs wake)))))
                                                        (let [kept (set (:survived wake))]
                                                          (cond (and goal (kept (str goal))) (str "goal " goal " kept")
                                                                goal (str "goal " goal " lost, re-submit")
                                                                (seq kept) (str "kept " (str/join "," (sort kept)))))]))}
    :attention {:wake :needs-you
                :text (str "attention: " (str/join "; " (map #(str (:job %) " " (word (:reason %)) " " (clip (:message %) 80))
                                                             (take 3 (:requests wake)))))}
    :chat {:wake :chat :text (str (:from wake) ": " (clip (:message wake) 160))}
    :danger {:wake :needs-you :text (str "hit: health " (:health wake))}
    :reconnect-failed {:wake :needs-you :text (str "body offline: " (:reason wake))}
    :idle (when-not (routine-idle? (:jobs wake)) {:wake :needs-you :text (str "body idle: " (idle-text (:jobs wake)))})
    :job-finished (let [then (some->> (:next wake) (str "; next: "))]
                    (cond
                      (= goal (:job wake)) {:wake :goal-ended :text (str goal " " (word (:result wake)) " " (clip (:message wake) 120) then)}
                      (#{:completed :cancelled} (:result wake)) nil
                      :else {:wake :gave-up :job (:job wake) :text (str (:job wake) " " (word (:result wake)) " " (clip (:message wake) 120) then)}))
    nil))

(defn item-text [{:keys [wake event job result message]}]
  (str/join " " (remove nil? [(word (or event result wake)) job (some-> message (clip 80))])))

(defn wake-items
  "Short texts of what an inner wait saw: its summary's items, then the wake itself unless it timed out."
  [wake]
  (cond-> (mapv item-text (get-in wake [:summary :items]))
    (not= :timeout (:wake wake)) (conj (item-text wake))))

(defn wake-text
  "The printed answer of a wait: a WAKE line, the progress line and up to max-items items."
  [{:keys [wake text line items vitals snapshot]}]
  (str (str/join "\n" (concat (when-let [hurt (:hurt vitals)] [(str "HURT: " hurt)])
                              [(str "WAKE " (word wake) (when (seq text) (str ": " text)))]
                              (when snapshot [(str "SNAPSHOT " snapshot " (what the body saw)")])
                              (when line [line])
                              (when (seq (dissoc vitals :hurt)) [(str "NOW " (str/join " " (map (fn [[k v]] (str (name k) " " (if (coll? v) (str/join "," v) v))) (dissoc vitals :hurt))))])
                              (map #(str "- " %) (take-last max-items items))))
       "\n"))

;; The wait

(defn ^:async inner-wait!
  "One slice of observe's wait under the observer `progress`, watching ids."
  [{:keys [base danger danger-below]} ids ms get! signal]
  (await (observe/wait-observe
          (assoc base :wait-options (observe-request/wait-options {:timeout (str (js/Math.round ms) "ms") :observer "progress"
                                                                  :watch (clj->js ids) :chatter "addressed" :no-danger (not danger)
                                                                  :danger-below danger-below}))
          get! signal identity)))

(defn ^:async wait-progress!
  "Wait until the agent is needed: a promise of {:wake :text :line :items}. r is the request (with :wait-ms,
  :stall-ms, :goal, :want; :slice-ms for tests); get! as for observe/wait-observe."
  [r get! signal]
  (let [now #(js/Date.now)
        deadline (+ (now) (:wait-ms r))
        slice-ms (or (:slice-ms r) default-slice-ms)
        goal (:goal r)
        finish (fn [verdict view items] (assoc verdict :line (some-> view progress-line) :items items))]
    (loop [last-view nil fp nil since (now) items [] failing-since nil]
      (let [attempt (try
                      (let [view (await (read-view! get! r signal))
                            mark (fingerprint view)
                            since (if (= mark fp) since (now))]
                        (cond
                          (:finished? (:goal-job view))
                          {:done (finish {:wake :goal-ended :text (str goal " " (word (get-in view [:goal-job :status])))} view items)}
                          (and (:stall-ms r) (>= (- (now) since) (:stall-ms r)))
                          {:done (finish {:wake :stall :text (str "no progress for " (js/Math.round (/ (- (now) since) 60000)) " min")} view items)}
                          (>= (now) deadline)
                          {:done (finish {:wake :timeout :text "no news"} view items)}
                          :else
                          (let [wake (await (inner-wait! r (watch-ids view) (max 10 (min slice-ms (- deadline (now)))) get! signal))
                                items (vec (take-last max-items (into items (wake-items wake))))]
                            (if-let [verdict (wake-verdict wake goal)]
                              {:done (finish verdict (try (await (read-view! get! r signal)) (catch :default _ view)) items)}
                              {:next [view mark since items]}))))
                      (catch :default error
                        (when (fatal? error) (throw error))
                        (let [first-fail (or failing-since (now))]
                          (if (and (>= (- (now) first-fail) (or (:unreachable-ms r) unreachable-ms))
                                   (not= :timeout (http/transport-reason error)))
                            {:done (finish {:wake :needs-you :text (str "engine unreachable: " (word (http/transport-reason error)))} last-view items)}
                            (do (await (observe/delay! (min 5000 (max 10 (- deadline (now)))) signal))
                                (if (>= (now) deadline)
                                  {:done (finish {:wake :timeout :text (str "engine unreachable: " (word (http/transport-reason error)))} last-view items)}
                                  {:retry first-fail}))))))]
        (cond
          (:done attempt) (:done attempt)
          (:retry attempt) (recur last-view fp since items (:retry attempt))
          :else (let [[view mark since items] (:next attempt)] (recur view mark since items nil)))))))

;; The command

(defn main!
  "Run the command; a promise of the exit code. Options (for tests): :output (text -> promise or nil), :request-fn."
  ([] (main! (vec (.slice (.-argv js/process) 2))))
  ([argv] (main! argv {}))
  ([argv {:keys [output request-fn render] :or {output observe/print-text!}}]
   (let [r (request-for argv)
         output #(js/Promise.resolve (output %))
         get! (fn [socket-path endpoint options] (observe/get! socket-path endpoint (cond-> options request-fn (assoc :request-fn request-fn))))]
     (if (:error r)
       (http/print-bad-args! output (:error r) usage)
       (let [controller (js/AbortController.)
             cancel #(.abort controller)]
         (.once js/process "SIGINT" cancel)
         (.once js/process "SIGTERM" cancel)
         (-> (js/Promise.resolve nil)
             (.then (fn []
                      (let [r (remember-plan! r)]
                        (if (:wait-ms r)
                          (.then (vitals/around! (get-in r [:base :socket-path]) get! #(wait-progress! r get! (.-signal controller)))
                                 (fn [[wake report]]
                                    (.then (warn-snapshot/capture! (assoc (:base r) :workspace (:workspace r) :render render) wake)
                                           #(output (wake-text (assoc wake :vitals report :snapshot %))))))
                          (.then (read-view! get! r (.-signal controller)) #(output (str (progress-line %) "\n")))))))
             (.then (constantly 0))
             (.catch (fn [error] (-> (output (observe/failure-text (:base r) error)) (.then (constantly 2)))))
             (.finally (fn []
                         (.removeListener js/process "SIGINT" cancel)
                         (.removeListener js/process "SIGTERM" cancel)))))))))

(defn main-js!
  "The launcher's entry: `render` is renderView of tools/view/render.mjs, drawing the picture of a trouble wake."
  [argv render]
  (main! (vec argv) {:render render}))
