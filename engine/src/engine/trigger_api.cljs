(ns engine.trigger-api
  "Edits of the reflex register from outside a running body (POST /triggers on
  the event socket) and from the scenario at boot, through one validation.
  Refusals are data: {:ok false :reason r :at [key ...] :message text}.

  Ad hoc triggers: an entry whose :when is a condition (an EDN list, see
  engine.condition) instead of a :trigger. It is registered under the
  :condition trigger (with-conditions), which holds the compiled fn of each
  such entry by its id; the form is what engine.edn keeps, and
  restore-conditions! compiles it again on boot. See README.md, Local event API."
  (:require [engine.backoff :as backoff]
            [engine.condition :as condition]
            [engine.core :as core]
            [engine.expr :as expr]
            [engine.job-api :as job-api]
            [engine.memory :as mem]))

(def persistences #{:retry :cooldown :stop})

(def entry-keys
  "Keys a register entry may have, in a scenario or a put."
  #{:id :trigger :when :job :args :persistence :cooldown-s :backoff :builtin? :ttl-s :by})

(def common-keys #{:op :id :by :generation-id :request-id})

(def op-keys
  "Keys each request op may have; a put cannot make a built-in."
  {:put (into common-keys (disj entry-keys :builtin?))
   :remove common-keys
   :mute (conj common-keys :ttl-s)
   :move (conj common-keys :above :below :ttl-s)
   :clear (conj common-keys :property)})

(def properties #{:mute :position})

(defn refuse
  ([reason at message] {:ok false :reason reason :at at :message message})
  ([reason at message extra] (merge (refuse reason at message) extra)))

(defn id? [x]
  (and (keyword? x) (nil? (namespace x)) (boolean (re-matches #"[a-z][a-z0-9-]{0,39}" (name x)))))

(defn pos-number? [x] (and (number? x) (pos? x) (js/isFinite x)))

(defn find-entry [s id] (some #(when (= id (:id %)) %) (:register s)))

;; ------------------------------------------------------------------ the :condition trigger (the seam)

(defn compile-condition
  "The condition compiler the body uses (engine.condition): form (data, or text
  from a command line) -> {:ok true :when f :explain g} or the language's
  refusal {:ok false :reason :at :message :allowed}. f is (fn [world memory
  & _]), true only on a definite true; g is (fn [world memory]) -> [{:form
  :value}], reading f's state without advancing it. Each call makes fresh
  state (held-for timers, scan cache), kept in this process only."
  [form]
  (let [read (condition/read-condition form)
        compiled (when (:ok read) (condition/compile (:form read)))]
    (cond
      (not (:ok read)) read
      (not (:ok compiled)) compiled
      :else
      (let [node (:node compiled)
            state (atom {})]
        {:ok true
         :when (fn [world memory & _]
                 (let [{:keys [value] :as r} (condition/evaluate node {:world world :memory memory} @state)]
                   (reset! state (:state r))
                   (true? value)))
         :explain (fn [world memory] (condition/explain node {:world world :memory memory} @state))}))))

(defn condition-trigger
  "The trigger behind every ad hoc entry: it holds when that entry's own
  compiled condition does (args carry the entry's :id). compile is the
  condition compiler (see compile-condition); :fns {id compiled} the compiled
  ones ({:when f :explain g?})."
  [compile]
  (let [fns (atom {})]
    {:name :condition
     :doc "An ad hoc trigger: holds when the entry's :when condition does."
     :when (fn [world view args & more]
             (if-let [f (get-in @fns [(:id args) :when])]
               (boolean (apply f world view args more))
               false))
     :job nil
     :args {}
     :persistence :stop
     :cooldown-s 0
     :compile compile
     :fns fns}))

(defn with-conditions
  "triggers plus the :condition trigger over compile (none when compile is nil)."
  [triggers compile]
  (cond-> triggers compile (assoc :condition (condition-trigger compile))))

(defn compile-with [triggers form]
  ((get-in triggers [:condition :compile]) form))

(defn set-condition!
  "Keep compiled (a compile result, or nil to forget) for entry id."
  [eng id compiled]
  (when-let [fns (get-in eng [:triggers :condition :fns])]
    (if compiled (swap! fns assoc id compiled) (swap! fns dissoc id))))

(defn explain
  "{:id id :terms [{:form :value} ...]}: every sub-term of entry id's condition
  with its value now, root first; {:id id :message text} when it has none."
  [eng id]
  (if-let [g (get-in @(get-in eng [:triggers :condition :fns] (atom {})) [id :explain])]
    {:id id :terms (g (:primitives eng) (mem/view (:store eng)))}
    {:id id :message (str id " has no condition to explain (not an ad hoc entry, or not in the register)")}))

;; ------------------------------------------------------------------ validation

(defn job-problem
  "Why job spec job cannot be a register entry's job, or nil."
  [jobs job]
  (if (and (seq? job) (= 'hold (first job)))
    (str "hold is not allowed in a register entry, in " (pr-str job))
    (try (expr/parse jobs job) nil
         (catch :default e (ex-message e)))))

(defn backoff-problem [cfg]
  (try (backoff/validate! cfg) nil
       (catch :default e (ex-message e))))

(defn condition-problem [triggers form]
  (let [r (compile-with triggers form)]
    (when-not (:ok r)
      (refuse :bad-condition [:when] (str "condition " (pr-str form) ": " (:message r))
              {:condition (dissoc r :ok)}))))

(defn entry-problem
  "Why register entry e (as a scenario or a put gives it) is refused, as a
  refusal; nil when it is fine. jobs is the job registry, triggers the trigger
  defs (with :condition for ad hoc entries)."
  [jobs triggers e]
  (let [{:keys [id trigger job args persistence cooldown-s ttl-s]} e
        adhoc? (contains? e :when)
        unknown (when (map? e) (first (remove entry-keys (keys e))))]
    (cond
      (not (map? e)) (refuse :bad-entry [] (str "a register entry is a map, not " (pr-str e)))
      unknown (refuse :unknown-key [unknown] (str "unknown key " unknown "; allowed " (pr-str (sort entry-keys))))
      (and (contains? e :id) (not (id? id))) (refuse :bad-id [:id] (str "an id is a plain lower-case keyword, not " (pr-str id)))
      (= adhoc? (contains? e :trigger)) (refuse :trigger-or-when [] "give exactly one of :trigger (a built-in trigger) or :when (a condition)")
      (= :condition trigger) (refuse :bad-trigger [:trigger] "an ad hoc trigger is given by its :when condition, not :trigger :condition")
      (and (not adhoc?) (not (contains? triggers trigger))) (refuse :unknown-trigger [:trigger] (str "unknown trigger " trigger))
      (and adhoc? (nil? id)) (refuse :missing-id [:id] "an ad hoc trigger needs an :id")
      (and adhoc? (nil? job)) (refuse :missing-job [:job] "an ad hoc trigger needs a :job")
      (and adhoc? (contains? e :args)) (refuse :bad-args [:args] "an ad hoc trigger takes no :args; put them in its :job")
      (and (contains? e :args) (not (map? args))) (refuse :bad-args [:args] (str ":args is a map, not " (pr-str args)))
      (and (some? job) (job-problem jobs job)) (refuse :bad-job [:job] (job-problem jobs job))
      (and (some? persistence) (not (persistences persistence))) (refuse :bad-persistence [:persistence] (str ":persistence is one of " (pr-str (sort persistences))))
      (and (contains? e :cooldown-s) (not (and (number? cooldown-s) (>= cooldown-s 0)))) (refuse :bad-cooldown [:cooldown-s] ":cooldown-s is a number of seconds, 0 or more")
      (and (contains? e :backoff) (backoff-problem (:backoff e))) (refuse :bad-backoff [:backoff] (backoff-problem (:backoff e)))
      (and (contains? e :ttl-s) (not (pos-number? ttl-s))) (refuse :bad-ttl [:ttl-s] ":ttl-s is a positive number of seconds")
      (and adhoc? (nil? (:condition triggers))) (refuse :conditions-unavailable [:when] "this body has no condition language")
      adhoc? (condition-problem triggers (:when e)))))

(defn register-problems
  "The problems of a scenario's register entries as text, then duplicate ids
  among the valid ones."
  [jobs triggers register]
  (let [checked (mapv (fn [e] [e (entry-problem jobs triggers e)]) register)
        ids (keep (fn [[e problem]] (when-not problem (or (:id e) (:trigger e)))) checked)]
    (vec (concat (keep (comp :message second) checked)
                 (for [[id n] (frequencies ids) :when (> n 1)]
                   (str "duplicate register id " id))))))

(defn anchor-problem [s r]
  (let [ks (filter #(contains? r %) [:above :below])
        k (first ks)
        anchor (get r k)]
    (cond
      (not= 1 (count ks)) (refuse :bad-anchor [:above] "a move takes exactly one of :above or :below")
      (= anchor (:id r)) (refuse :bad-anchor [k] "a trigger cannot move relative to itself")
      (not (find-entry s anchor)) (refuse :no-such-trigger [k] (str "no trigger " (pr-str anchor) " in the register")))))

(defn request-problem
  "Why request r cannot be applied to eng now, as a refusal; nil when it can."
  [eng r]
  (let [s (core/state eng)
        op (when (map? r) (:op r))
        unknown (when (op-keys op) (first (remove (op-keys op) (keys r))))
        id (if (= :put op) (or (:id r) (:trigger r)) (:id r))]
    (cond
      (not (map? r)) (refuse :bad-request [] "a request is an EDN map")
      (not (op-keys op)) (refuse :bad-op [:op] (str ":op is one of " (pr-str (sort (keys op-keys)))))
      unknown (refuse :unknown-key [unknown] (str "unknown key " unknown " for " op))
      (and (contains? r :generation-id) (not= (:generation-id r) (:generation-id s)))
      (refuse :generation-mismatch [:generation-id] "the body's engine state was replaced; read it again")
      (and (contains? r :by) (not (job-api/valid-by? (:by r)))) (refuse :bad-by [:by] ":by is a short string or keyword naming who asks")
      (= :put op) (or (entry-problem (:jobs eng) (:triggers eng) (dissoc r :op :generation-id :request-id))
                      (when (:builtin? (find-entry s id))
                        (refuse :builtin [:id] (str id " is built in; mute or move it, or put another id"))))
      (not (id? id)) (refuse :bad-id [:id] (str "an id is a plain lower-case keyword, not " (pr-str id)))
      (not (find-entry s id)) (refuse :no-such-trigger [:id] (str "no trigger " id " in the register"))
      (and (= :remove op) (:builtin? (find-entry s id))) (refuse :builtin [:id] (str id " is built in; mute or move it instead"))
      (and (contains? r :ttl-s) (some? (:ttl-s r)) (not (pos-number? (:ttl-s r)))) (refuse :bad-ttl [:ttl-s] ":ttl-s is a positive number of seconds")
      (= :move op) (anchor-problem s r)
      (and (= :clear op) (not (properties (:property r)))) (refuse :bad-property [:property] (str ":property is one of " (pr-str (sort properties)))))))

;; ------------------------------------------------------------------ views

(defn entry-view
  "Entry e as the route shows it: the entry plus its live changes, latch,
  cooldown and backoff."
  [eng e]
  (let [s (core/state eng)
        t (core/now eng)
        id (:id e)
        changes (get-in s [:changes id])
        {:keys [cooldown-until stopped?]} (get-in s [:reflex-state id])
        bo (core/backoff-entry eng id)]
    (cond-> (select-keys e [:id :trigger :when :job :args :persistence :cooldown-s :backoff :builtin? :by :until])
      (:when e) (dissoc :args)
      (core/live? (:mute changes) t) (assoc :muted (select-keys (:mute changes) [:until]))
      (core/live? (:position changes) t) (assoc :moved (select-keys (:position changes) [:value :until]))
      (and cooldown-until (> cooldown-until t)) (assoc :cooling-until cooldown-until)
      stopped? (assoc :stopped? true)
      (:since bo) (assoc :backing-off (select-keys bo [:since :until :fruitless :last])))))

(defn triggers-view
  "GET /triggers: the register in its own order with each entry's state, and
  :order, the ids in firing order now (moves applied, muted ones left out).
  With id (keyword or name, GET /triggers?id=), also :explain for that entry."
  ([eng] (triggers-view eng nil))
  ([eng id]
   (let [s (core/state eng)]
     (cond-> {:ok true
              :generation-id (:generation-id s)
              :total (count (:register s))
              :order (mapv :id (core/effective-register s (core/now eng)))
              :items (mapv #(entry-view eng %) (:register s))}
       (some? id) (assoc :explain (explain eng (keyword id)))))))

;; ------------------------------------------------------------------ applying

(defn entry-of
  "The register entry a valid put (or scenario entry) r makes."
  [eng r]
  (let [given (into {} (remove (comp nil? val)) r)
        base (if (contains? given :when)
               {:id (:id given) :trigger :condition :when (:when given) :job (:job given)
                :args {:id (:id given)} :persistence :stop :cooldown-s 0 :builtin? false}
               (core/entry-from eng given))]
    (cond-> (merge base (select-keys given [:persistence :cooldown-s :backoff :builtin? :by]))
      (:ttl-s given) (assoc :until (+ (core/now eng) (* 1000 (:ttl-s given)))))))

(defn attention-job
  "The job id an entry's backoff request is filed under (requests are per job)."
  [id]
  (str "reflex:" (name id)))

(defn forget!
  "Drop what the engine keeps beside entry id: its condition, backoff and request."
  [eng id]
  (set-condition! eng id nil)
  (core/forget-backoff! eng id)
  (core/resolve-job-attention! eng (attention-job id) :job-cancelled))

(defn put!
  "Create or replace the entry r names. A replaced entry keeps its place and
  its mute or move; its latch, cooldown, backoff and condition state start over."
  [eng r]
  (let [entry (entry-of eng r)
        id (:id entry)
        old (find-entry (core/state eng) id)]
    (forget! eng id)
    (swap! (:state eng) (fn [s] (-> s
                                    (update :register #(if old
                                                         (mapv (fn [e] (if (= id (:id e)) entry e)) %)
                                                         (conj % entry)))
                                    (update :reflex-state dissoc id))))
    (when (:when entry)
      (set-condition! eng id (compile-with (:triggers eng) (:when entry))))
    (core/emit! eng {:source :reflex :kind :changed :level :info :reflex id :property :registered
                     :value (pr-str (:job entry)) :when (some-> (:when entry) pr-str)
                     :replaced (some? old) :by (:by entry)
                     :text (str (name id) (if old " replaced" " added") (when (:by entry) (str " by " (:by entry))))})
    {:ok true :op :put :id id :created? (nil? old) :trigger (entry-view eng entry)}))

(defn remove! [eng id]
  (core/remove-reflex! eng id)
  (forget! eng id)
  {:ok true :op :remove :id id})

(defn changed [eng op id]
  {:ok true :op op :id id :trigger (entry-view eng (find-entry (core/state eng) id))})

(defn request!
  "Apply one POST /triggers request; the reply, or a refusal naming the
  offending part. Runs between ticks (the handler and tick! share one event
  loop); a reflex job in flight is never cut by an edit."
  [eng r]
  (or (request-problem eng r)
      (let [{:keys [op ttl-s]} r
            id (:id r)]
        (case op
          :put (put! eng r)
          :remove (remove! eng id)
          :mute (do (core/mute! eng id ttl-s) (changed eng op id))
          :move (do (core/move! eng id (select-keys r [:above :below]) ttl-s) (changed eng op id))
          :clear (do (core/clear-change! eng id (:property r)) (changed eng op id))))))

(defn load-scenario!
  "Put a scenario's register entries (by :scenario) and queue its job specs, in
  order. Throws on an entry the validation refuses (main validates first)."
  [eng {:keys [register queue]}]
  (doseq [e register]
    (when-let [problem (entry-problem (:jobs eng) (:triggers eng) e)]
      (throw (ex-info (:message problem) problem)))
    (put! eng (assoc e :by :scenario)))
  (doseq [spec queue]
    (core/submit! eng spec {:by :scenario})))

;; ------------------------------------------------------------------ boot and every tick

(defn drop-entry! [eng e message]
  (let [id (:id e)]
    (swap! (:state eng) (fn [s] (-> s
                                    (update :register #(filterv (fn [x] (not= id (:id x))) %))
                                    (update :changes dissoc id)
                                    (update :reflex-state dissoc id))))
    (core/emit! eng {:source :system :kind :dropped :level :warn :reflex id :error message
                     :text (str "reflex " id " dropped on restore: condition " (pr-str (:when e)) ": " message)})))

(defn restore-conditions!
  "After a restore: compile each ad hoc entry's condition again (the condition
  language may have changed); drop one that no longer compiles with one warn."
  [eng]
  (doseq [e (:register (core/state eng))
          :when (= :condition (:trigger e))]
    (let [r (compile-with (:triggers eng) (:when e))]
      (if (:ok r)
        (set-condition! eng (:id e) r)
        (drop-entry! eng e (:message r))))))

(defn expire-entries!
  "Remove entries whose :ttl-s has run out, each with a reflex.expired event."
  [eng]
  (let [t (core/now eng)]
    (doseq [e (:register (core/state eng))
            :when (and (:until e) (>= t (:until e)))]
      (core/emit! eng {:source :reflex :kind :expired :level :info :reflex (:id e)
                       :text (str (name (:id e)) " expired")})
      (remove! eng (:id e)))))

(defn drop-orphan-reflex-job!
  "A reflex job between rounds whose entry was removed gets no further round:
  the round in flight at the removal finished, now the job goes."
  [eng]
  (let [s (core/state eng)
        id (:pending-reflex s)
        reflex (get-in s [:instances id :reflex])]
    (when (and id reflex (not (find-entry s reflex)))
      (core/drop-reflex-job! eng id reflex :dropped {:how :removed}))))

(defn agent-added?
  "Whether entry e was put over the route (not by the scenario)."
  [e]
  (and (some? (:by e)) (not= :scenario (:by e))))

(defn watch-backoffs!
  "One required request per agent-added entry that is backing off (its job keeps
  failing while its trigger holds); resolved when the backoff ends or the entry goes."
  [eng]
  (let [s (core/state eng)
        open (set (keep (fn [[_ q]] (when (= :reflex-backoff (:reason q)) (:job-id q))) (:attention s)))
        backing (into {} (keep (fn [e]
                                 (let [bo (core/backoff-entry eng (:id e))]
                                   (when (and (agent-added? e) (:since bo))
                                     [(attention-job (:id e)) [e bo]]))))
                      (:register s))]
    (doseq [[job-id [e bo]] backing
            :when (not (open job-id))]
      (core/request-attention! eng {:job-id job-id :reason :reflex-backoff :kind :reflex-backoff
                                    :context {:reflex-id (:id e)}
                                    :data (merge {:reflex (:id e) :by (:by e)}
                                                 (select-keys bo [:fruitless :delay-ms :since :last]))
                                    :message (str "trigger " (name (:id e)) " backs off: its job keeps failing while it holds")}))
    (doseq [job-id open
            :when (not (contains? backing job-id))]
      (core/resolve-job-attention! eng job-id :condition-recovered))))

(defn tick!
  "Run before every engine tick (also while paused): expire entries, drop the
  job of a removed entry, keep the backoff requests current."
  [eng]
  (expire-entries! eng)
  (drop-orphan-reflex-job! eng)
  (watch-backoffs! eng))
