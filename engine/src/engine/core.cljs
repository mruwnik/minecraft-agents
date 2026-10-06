(ns engine.core
  "The scheduler: the job list (round-robin over checks, with holding),
  the reflex register (ordered triggers with TTL mutes and moves), cuts by
  ownership-token rotation, and persistence of both to engine.edn.

  This namespace is the API and the lifecycle (create, start!, shutdown!); the parts live in engine.core.*:
  base, attention, fruitless, register, activity, list-edits, round, settle, schedule, restart.

  The state atom holds plain EDN, written on every change. Memory (engine.memory)
  is saved at every round end and whenever the engine itself writes to it.
    :list [id]              listed instance ids, in cycle order
    :instances {id inst}    {:id :spec :round :hold? :reflex}; :spec is a parsed
                            job expression node (engine.expr)
    :register [entry]       {:id :trigger :job :args :persistence :cooldown-s :backoff :builtin?}
    :seen-triggers #{id}    every trigger id the body has had, removed or declined ones too; never offered as new
    :new-defaults [entry]   scenario triggers the last restart offered, until upgraded or declined
    :scenario-order [id]    the scenario's trigger order, for placing upgraded entries
    :changes {rid {prop {:value v :until ms-or-nil}}}  prop is :mute or :position
    :reflex-state {rid {:cooldown-until ms :stopped? bool}}
    :cursor n               where the next round-robin scan starts
    :resume id              a cut listed job, next once the body is free
    :current id             the listed job whose round is in flight
    :failed {id {:error text :t ms}}  listed jobs whose round threw; they keep
                            their place and memory, the scheduler skips them
                            until retry! or cancel!
    :deferred-ends [{:reflex :job :outcome :ended-at ms :text}]  reflex jobs that ended while
                            the body was settling or offline, judged on the first ready tick
    :next-id n
  Not persisted:
    the in-flight round ({:id :token :reflex :round})
    the backoff: the :backoffs atom {reflex-id entry} (see engine.backoff); register entries only, listed jobs
      have none. The entry is {:fruitless n :last {:act :status :reason}}, plus :delay-ms :until :since
      :alerted (ms of the last warn) once backing off. An entry goes on progress, and all go when the engine
      leaves a pause."
  (:require [engine.core.base :as base]
            [engine.core.attention :as attention]
            [engine.core.fruitless :as fruitless]
            [engine.core.register :as register]
            [engine.core.activity :as activity]
            [engine.core.list-edits :as list-edits]
            [engine.core.round :as round]
            [engine.core.settle :as settle]
            [engine.core.schedule :as schedule]
            [engine.core.restart :as restart]
            [engine.hurt :as hurt]
            [engine.events :as events]
            [engine.fsutil :as fsu]
            [engine.memory :as mem]
            [engine.hooks :as hooks]
            ["crypto" :as crypto]
            ["path" :as path]
            [engine.game :as game]))

;; ------------------------------------------------------------------ the API, from the parts

(def cut-error base/cut-error)
(def cut? base/cut?)
(def default-idle-s base/default-idle-s)
(def default-stats-ms base/default-stats-ms)
(def default-sweep-ms base/default-sweep-ms)
(def distance base/distance)
(def emit! base/emit!)
(def empty-save-stats base/empty-save-stats)
(def empty-state base/empty-state)
(def job-memory base/job-memory)
(def job-of base/job-of)
(def manual? base/manual?)
(def now base/now)
(def offline? base/offline?)
(def paused? base/paused?)
(def running base/running)
(def save-file! base/save-file!)
(def save-memory! base/save-memory!)
(def self-pos base/self-pos)
(def set-owner! base/set-owner!)
(def settling? base/settling?)
(def state base/state)
(def outstanding attention/outstanding)
(def replay-attention! attention/replay-attention!)
(def request-attention! attention/request-attention!)
(def resolve-attention! attention/resolve-attention!)
(def resolve-job-attention! attention/resolve-job-attention!)
(def same-request? attention/same-request?)
(def backoff-entries fruitless/backoff-entries)
(def backoff-entry fruitless/backoff-entry)
(def default-backoff-alert-ms fruitless/default-backoff-alert-ms)
(def forget-backoff! fruitless/forget-backoff!)
(def reset-backoff! fruitless/reset-backoff!)
(def clear-change! register/clear-change!)
(def effective-register register/effective-register)
(def entry-from register/entry-from)
(def live? register/live?)
(def move! register/move!)
(def mute! register/mute!)
(def register-reflex! register/register-reflex!)
(def remove-reflex! register/remove-reflex!)
(def away activity/away)
(def holding activity/holding)
(def cancel! list-edits/cancel!)
(def insert-front list-edits/insert-front)
(def insert-now list-edits/insert-now)
(def retry! list-edits/retry!)
(def submit! list-edits/submit!)
(def act! round/act!)
(def make-ctx round/make-ctx)
(def drop-reflex-job! settle/drop-reflex-job!)
(def cut! schedule/cut!)
(def do-now! schedule/do-now!)
(def holder schedule/holder)
(def tick! schedule/tick!)
(def waiting schedule/waiting)
(def drop-leftover-reflex-jobs! restart/drop-leftover-reflex-jobs!)
(def drop-unknown-jobs! restart/drop-unknown-jobs!)
(def repair-entries! restart/repair-entries!)
(def restore restart/restore)

;; ------------------------------------------------------------------ scenario, body events, lifecycle

(defn load-scenario!
  "Register a scenario's reflexes and queue its job specs, in order."
  [eng {:keys [register queue]}]
  (doseq [spec register] (register-reflex! eng spec))
  (doseq [spec queue]
    (submit! eng spec {:by :scenario})))

(def error-kinds #{"died" "error" "reconnect-failed" "dependency-patches-missing"})

(def warn-kinds #{"world-not-loaded" "physics-stalled"})

(def debug-kinds #{"picked-up"})

(defn body-event-level [kind]
  (cond (contains? error-kinds kind) :error
        (contains? warn-kinds kind) :warn
        (contains? debug-kinds kind) :debug
        :else :info))

(defn drop-jobs-on-death!
  "A job does not survive its body's death: cancel every listed job (queued, held,
  cut or running) and drop every reflex job. Register entries stay, so a trigger
  that still holds after the respawn starts its job again."
  [eng]
  (doseq [id (:list (state eng))]
    (cancel! eng id :death))
  (doseq [[id inst] (:instances (state eng))
          :when (:reflex inst)]
    (when (= id (:id (running eng)))
      (set-owner! eng nil)
      (reset! (:running eng) nil))
    (drop-reflex-job! eng id (:reflex inst) :dropped {:how :dropped :by :death})))

(defn record-body-event!
  "A momentary body event becomes an entry of its kind (:hurt, :died, ...).
  A death also drops every job."
  [eng e]
  (let [m (js->clj e :keywordize-keys true)]
    (when (= "died" (:kind m))
      (hurt/flush! eng #(emit! eng %))
      (drop-jobs-on-death! eng))
    (mem/write! (:store eng) (keyword (:kind m)) (dissoc m :kind))
    (save-memory! eng)
    (if (= "hurt" (:kind m))
      (hurt/record! eng #(emit! eng %) m)
      (emit! eng (merge (dissoc m :kind)
                        {:source :body :kind (keyword (:kind m))
                         :level (body-event-level (:kind m))})))))

(defn create
  "An engine over primitives with state under dir. Restores engine.edn and
  memory.edn when present, sweeps memory and appends a :restart entry.
  Options:
    :primitives, :dir, :now, :events, :body
    :jobs        the registry {sym {:check :round :doc :args}}
    :triggers    {name trigger}
    :world       the body's world store (hooks :world/open; :world/blank when not given)
    :backoff     engine-wide backoff config of register entries, a map or false (see engine.backoff)
    :backoff-alert-ms  least gap between two job.backoff warns (300000)
    :idle-s            seconds with no act and no declared hold before job.idle (10)
    :sweep-ms          memory sweep interval (60000)
    :stats-ms          memory.save-stats interval (60000)
    :max-event-bytes   event log size cap (64 MiB)"
  [{:keys [primitives jobs triggers dir now events body idle-s sweep-ms stats-ms world
           max-event-bytes
           backoff backoff-alert-ms]
    :or {now js/Date.now idle-s default-idle-s sweep-ms default-sweep-ms
         stats-ms default-stats-ms backoff-alert-ms default-backoff-alert-ms}}]
  (let [_ (game/select! primitives)
        file (path/join dir "engine.edn")
        saved (fsu/read-edn file)
        username (or body (.-username (.self primitives)))
        initial-state (if saved (restore saved) empty-state)
        initial-state (cond-> initial-state
                        (nil? (:generation-id initial-state)) (assoc :generation-id (.randomUUID crypto)))
        st (atom initial-state)
        ev (or events (events/make {:file (path/join dir "events.edn")
                                    :generation-id (:generation-id initial-state)
                                    :max-bytes (or max-event-bytes 67108864)
                                    :stdout? true :now now :pos-fn #(self-pos primitives)}))
        store (mem/open dir {:now now
                             :world-time #(or (.-timeOfDay (.self primitives)) nil)
                             :live-jobs #(set (keys (:instances @st)))})
        eng {:primitives primitives :jobs jobs :triggers triggers :dir dir :now now :events ev
             :store store
             :hurt (hurt/new-state)
             :world (or world ((:world/blank hooks/all)))
             :warned (atom #{})
             :continued (atom {})
             :state st
             :running (atom nil)
             :tokens (atom 0)
             :manual (atom nil)
             :away (atom nil)
             :waiting (atom {})
             :world-ops (atom {:active nil :records {} :order [] :queue []})
             :activity (atom nil)
             :fruitless (atom {})
             :said (atom [])
             :rounds (atom {})
             :passes (atom {})
             :backoffs (atom {})
             :was-paused (atom false)
             :backoff backoff
             :backoff-alert-ms backoff-alert-ms
             :idle-s idle-s
             :sweep-ms sweep-ms
             :last-sweep (atom (now))
             :stats-ms stats-ms
             :last-stats (atom (now))
             :save-stats (atom empty-save-stats)}]
    (add-watch st ::persist (fn [_ _ old new]
                              (when (not= old new) (save-file! eng file #(fsu/write-edn! file new)))))
    (save-file! eng file #(fsu/write-edn! file (state eng)))
    (set-owner! eng nil)
    (.onBodyEvent primitives #(record-body-event! eng %))
    (drop-leftover-reflex-jobs! eng saved)
    (let [notices (into (filterv some? (drop-unknown-jobs! eng)) (repair-entries! eng))]
      (doseq [[request-id request] (:attention (state eng))
              :when (and (:job-id request) (not (some #{(:job-id request)} (:list (state eng)))))]
        (resolve-attention! eng request-id :job-dropped))
      (doseq [n notices] (request-attention! eng n)))
    ;; A parked failed job must have a required request before the replay below
    ;; (covers older snapshots and a crash between the two writes).
    (doseq [[id failure] (:failed (state eng))
            :when (and (not (:attention-closed? failure))
                       (not-any? #(same-request? % id :round-failed) (vals (:attention (state eng))))) ]
      (request-attention! eng {:job-id id :reason :round-failed :kind :failed
                               :data {:error (:error failure)}
                               :message (str "Job failed and is parked until retried or cancelled: " (:error failure))}))
    (replay-attention! eng)
    (mem/write! store :restart {})
    (save-memory! eng)
    (emit! eng {:source :system :kind (if saved :restored :started) :level :info
                :list (:list (state eng)) :register (mapv :id (:register (state eng)))})
    eng))

(defn shutdown!
  "Release the body for a shutdown. The in-flight round is cut by token
  rotation and its outcome is never booked, so its job stays on the list
  (persisted as :current, resumed first after a restart) with its memory."
  [eng]
  (let [h (holder eng)]
    (set-owner! eng nil)
    (reset! (:running eng) nil)
    (when (:reflex h)
      (drop-reflex-job! eng (:id h) (:reflex h) :dropped {:how :dropped :by :shutdown})))
  (emit! eng {:source :system :kind :stopping :level :info :job (:current (state eng))}))

(defn report-tick-failure! [eng e]
  (emit! eng {:source :system :kind :error :level :error :text (str "tick failed: " e)}))

(defn start!
  "Tick every tick-ms until the returned stop fn is called. A tick that throws,
  or whose round fails to settle, is reported as an error event; the loop goes on.
  :before-tick, when given, runs at the top of each step (also while paused), before tick!."
  [eng {:keys [tick-ms before-tick] :or {tick-ms 250}}]
  (let [stopped (atom false)]
    (letfn [(step []
              (when-not @stopped
                (try
                  (when before-tick (before-tick))
                  (some-> (tick! eng) (.catch #(report-tick-failure! eng %)))
                  (catch :default e
                    (report-tick-failure! eng e)))
                (js/setTimeout step tick-ms)))]
      (step))
    #(reset! stopped true)))
