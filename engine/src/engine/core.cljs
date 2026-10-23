(ns engine.core
  "The scheduler: the job list (round-robin over checks, with holding),
  the reflex register (ordered triggers with TTL mutes and moves), cuts by
  ownership-token rotation, and persistence of both to engine.edn.

  This namespace is the API and the lifecycle (create, start!, shutdown!); the parts live in engine.core.*:
  base, attention, fruitless, register, activity, list-edits, round, settle, schedule, restart.

  The state atom holds plain EDN, written on every change. Memory (engine.memory)
  is saved at every round end and whenever the engine itself writes to it.
    :list [id]              listed instance ids, in cycle order
    :instances {id inst}    {:id :spec :round :reflex}; :spec is a parsed
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
    :deferred-ends [{:reflex :job :outcome :reason :ended-at ms :text}]  reflex jobs that ended while
                            the body was settling or offline, judged on the first ready tick
    :next-id n
  The outcomes of the last jobs that ended ({id {:name :status :result :ended-at}}, bounded by the settings
  engine.core.base/recent-results-limit and reflex-results-limit) live in (:results eng), saved to results.edn
  when a job ends and served on /job and /results, not in the state.
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
            [engine.contract :as contract]
            [engine.core.ending :as ending]
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
            [engine.settings :as settings]
            ["crypto" :as crypto]
            ["path" :as path]
            [engine.game :as game]))

;; ------------------------------------------------------------------ the API, from the parts

(def cut-error base/cut-error)
(def cut? base/cut?)
(def default-idle-s base/default-idle-s)
(def default-sweep-ms base/default-sweep-ms)
(def distance base/distance)
(def emit! base/emit!)
(def empty-save-stats base/empty-save-stats)
(def empty-state base/empty-state)
(def job-memory base/job-memory)
(def job-of base/job-of)
(def exclusive-job base/exclusive-job)
(def free-owner! base/free-owner!)
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
(def recent-results base/results)
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
(def levels register/levels)
(def live? register/live?)
(def ttl-now base/ttl-now)
(def move! register/move!)
(def mute! register/mute!)
(def register-reflex! register/register-reflex!)
(def remove-reflex! register/remove-reflex!)
(def away activity/away)
(def holding activity/holding)
(defn cancel!
  "End job id (list-edits/cancel!); its cut-off carries done?'s count and :result (ending/with-progress)."
  ([eng id] (cancel! eng id :agent))
  ([eng id by]
   (list-edits/cancel! eng id by (ending/progress-of eng id (contract/ending :cut-off by nil)))))
(def insert-next list-edits/insert-next)
(def insert-now list-edits/insert-now)
(def retry! list-edits/retry!)
(def submit! list-edits/submit!)
(def act! round/act!)
(def make-ctx round/make-ctx)
(def drop-reflex-job! base/drop-reflex-job!)
(def cut! schedule/cut!)
(def do-now! schedule/do-now!)
(def exclusive-run! schedule/exclusive-run!)
(def holder schedule/holder)
(def tick! schedule/tick!)
(def waiting base/waiting)
(def held-trigger schedule/held-trigger)
(def drop-leftover-reflex-jobs! restart/drop-leftover-reflex-jobs!)
(def drop-leftover-slot-jobs! restart/drop-leftover-slot-jobs!)
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
  "Cancel every listed job (queued, held, cut or running) but those submitted with :survive-death? (a running one is
  cut, memory kept), and drop every reflex job. Register entries stay, so a trigger that still holds after the
  respawn starts its job again. Returns the ids of the jobs that survive."
  [eng]
  (let [survives? #(get-in (state eng) [:instances % :survive-death?])
        r (running eng)]
    (when (and r (not (:reflex r)) (survives? (:id r)))
      (cut! eng r :death nil))
    (doseq [id (remove survives? (:list (state eng)))]
      (cancel! eng id :death)))
  (doseq [[id inst] (:instances (state eng))
          :when (:reflex inst)]
    (when (= id (:id (running eng)))
      (free-owner! eng)
      (reset! (:running eng) nil))
    (drop-reflex-job! eng id (:reflex inst) :dropped {:how :dropped :by :death}))
  (:list (state eng)))

(def deaths-policy "The :deaths list: the ten latest, kept for good." {:cap 10 :ttl :forever})

(defn record-death!
  "Add the :died entry to the :deaths list (pos, dimension, cause) unless its :death-t is there already."
  [eng entry]
  (when-not (some #(= (:t entry) (:death-t (:data %))) (mem/entries (mem/view (:store eng)) :deaths))
    (mem/write! (:store eng) :deaths
                (assoc (select-keys (:data entry) [:pos :dimension :cause]) :death-t (:t entry)) deaths-policy)))

(defn record-body-event!
  "A momentary body event becomes an entry of its kind (:hurt, :died, ...).
  A death also drops every job but the survivors (named in the event's :survived) and joins the :deaths list,
  whatever runs. A hurt entry carries the body's :pos, for jobs.lib.death-spot."
  [eng e]
  (let [m (js->clj e :keywordize-keys true)
        m (if (= "died" (:kind m))
            (do (hurt/flush! eng #(emit! eng %))
                (let [kept (drop-jobs-on-death! eng)]
                  (cond-> m (seq kept) (assoc :survived kept))))
            m)
        pos (when (= "hurt" (:kind m)) (self-pos (:primitives eng)))
        m (cond-> m pos (assoc :pos pos))]
    (let [entry (mem/write! (:store eng) (keyword (:kind m)) (dissoc m :kind :survived))]
      (when (= "died" (:kind m)) (record-death! eng entry)))
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
    :jobs        the registry {sym {:round :doc :args}}; a contract job's {:can-start? :round ...} is wrapped
                 (engine.core.ending)
    :triggers    {name trigger}
    :world       the body's world store (hooks :world/open; :world/blank when not given)
    :backoff     engine-wide backoff config of register entries, a map or false (see engine.backoff)
    :backoff-alert-ms  least gap between two job.backoff warns (300000)
    :idle-s            seconds with no act and no declared hold before job.idle (10)
    :body-idle-s       seconds with no useful act before body.idle (setting :engine.core.activity/body-idle-s)
    :sweep-ms          memory sweep interval (60000)
    :stats-ms          memory.save-stats and body.heap interval (setting :engine.core.base/stats-ms, 60000)
    :max-event-bytes   event log size cap (setting :engine.events/max-bytes, 64 MiB)"
  [{:keys [primitives jobs triggers dir now events body idle-s body-idle-s sweep-ms stats-ms world
           max-event-bytes
           backoff backoff-alert-ms mono]
    :or {idle-s default-idle-s sweep-ms default-sweep-ms
         backoff-alert-ms default-backoff-alert-ms}}]
  (let [_ (game/select! primitives)
        mono (or mono (when-not now #(.now js/performance)) now)
        now (or now js/Date.now)
        file (path/join dir "engine.edn")
        saved (fsu/read-edn file)
        username (or body (.-username (.self primitives)))
        initial-state (if saved (restore saved) empty-state)
        initial-state (cond-> initial-state
                        (nil? (:generation-id initial-state)) (assoc :generation-id (.randomUUID crypto)))
        st (atom initial-state)
        ev (or events (events/make {:file (path/join dir "events.edn")
                                    :generation-id (:generation-id initial-state)
                                    :max-bytes (or max-event-bytes (settings/get events/settings :engine.events/max-bytes))
                                    :stdout? true :now now :pos-fn #(self-pos primitives)}))
        store (mem/open dir {:now now
                             :world-time #(or (.-timeOfDay (.self primitives)) nil)
                             :world-age #(.worldAge primitives)
                             :live-jobs #(set (keys (:instances @st)))})
        eng {:primitives primitives :jobs (ending/registry jobs) :triggers triggers :dir dir :now now :mono mono :clock-mark (atom [(now) (mono)]) :events ev
             :store store
             :hurt (hurt/new-state)
             :world (or world ((:world/blank hooks/all)))
             :warned (atom #{})
             :continued (atom {})
             :state st
             :results (atom (or (fsu/read-edn (path/join dir "results.edn")) {}))
             :running (atom nil)
             :tokens (atom 0)
             :exclusive (atom nil)
             :exclusive-t (atom nil)
             :away (atom nil)
             :waiting (atom {})
             :activity (atom nil)
             :holds (atom {})
             :deferred (atom {})
             :held-triggers (atom {})
             :fruitless (atom {})
             :silent (atom {})
             :asked (atom {})
             :seen-done (atom {})
             :looks (atom {})
             :said (atom [])
             :rounds (atom {})
             :passes (atom {})
             :backoffs (atom {})
             :was-paused (atom false)
             :backoff backoff
             :backoff-alert-ms backoff-alert-ms
             :idle-s idle-s
             :body-idle-s body-idle-s
             :body-idle (atom {:busy 0 :last-at (now) :told? false :list (:list initial-state)})
             :sweep-ms sweep-ms
             :last-sweep (atom (now))
             :stats-ms (or stats-ms (settings/get base/settings :engine.core.base/stats-ms))
             :last-stats (atom (now))
             :memory-save-ms (settings/get base/settings :engine.core.base/memory-save-ms)
             :memory-saved-at (atom 0)
             :memory-dirty (atom false)
             :save-stats (atom empty-save-stats)}]
    (add-watch st ::persist (fn [_ _ old new]
                              (when (not= old new)
                                ;; save-file! already told the failure; it must not break the swap! that caused it.
                                (try (save-file! eng file #(fsu/write-edn! file new))
                                     (catch :default _ nil)))))
    (save-file! eng file #(fsu/write-edn! file (state eng)))
    (set-owner! eng nil)
    (.onBodyEvent primitives #(record-body-event! eng %))
    (drop-leftover-reflex-jobs! eng saved)
    (drop-leftover-slot-jobs! eng saved)
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
                :list (:list (state eng)) :register (mapv :id (:register (state eng)))
                :difficulty (game/difficulty) :simulation-distance (game/simulation-distance)})
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
  (save-memory! eng)
  (emit! eng {:source :system :kind :stopping :level :info :job (:current (state eng))}))

(defn report-tick-failure!
  "Emit the error event. A failing log (disk full) goes to stderr: the tick loop must survive it."
  [eng e]
  (try
    (emit! eng {:source :system :kind :error :level :error :text (str "tick failed: " e)})
    (catch :default e2
      (.write js/process.stderr (str "tick failed: " e " (and the log failed: " e2 ")\n")))))

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
                  (-> (js/Promise.resolve)
                      (.then #(tick! eng))
                      (.catch #(report-tick-failure! eng %)))
                  (catch :default e
                    (report-tick-failure! eng e)))
                (js/setTimeout step tick-ms)))]
      (step))
    #(reset! stopped true)))
