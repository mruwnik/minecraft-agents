(ns jobs.lib.checks
  "Checks of a job's own, shared by jobs and never offered to agents: fns of the ctx giving a status (engine.contract).
  jobs.lib.cond holds the checks of the agent language."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.body :as body]
            [jobs.lib.cond :as cond]
            [jobs.lib.storage :as storage]
            [jobs.lib.world :as known]))

(defn running?
  "Whether c's call has started its rounds: a done? asked before the start sees the would-be call (:quote-call) in
  ctx/frozen-call, so that alone cannot tell."
  [c]
  (and (nil? (:quote-call c)) (some? (ctx/frozen-call c))))

(defn args-fit
  "The check that ok? (a fn of the ctx) holds of the call's args: :ok :args, else :never :bad-args with text."
  [text ok?]
  (fn [c] (if (ok? c) (contract/ok :args "args fit") (contract/never :bad-args text))))

(defn holds "A :ok status for a check with nothing to say." [] (contract/ok :holds "no precondition"))

(defn cause-text [m] (str (or (:why m) (:text m) (name (:reason m)))))

(defn cause-status
  "[kind text data] for a cause, a map {:reason ..} or just the reason keyword, with text or cause-text; nil is :ok
  (nothing to say)."
  ([kind cause] (cause-status kind cause nil))
  ([kind cause text]
   (if (nil? cause)
     (holds)
     (let [cause (if (keyword? cause) {:reason cause} cause)]
       (contract/status kind (:reason cause) (or text (cause-text cause)) (dissoc cause :reason))))))

(defn goal
  "A done? status: :ok [reason text] when holds?, else :later [reason text]; data: :units :done :left :result."
  [holds? [ok-reason ok-text] [later-reason later-text] data]
  (if holds?
    (apply contract/ok ok-reason ok-text (mapcat identity data))
    (apply contract/later later-reason later-text (mapcat identity data))))

(defn child-under-way?
  "Whether a child of this job yielded and has not ended: its goal may hold while the child still wraps up (a craft
  picking its table up), so done? waits for it."
  [c]
  (boolean (seq (:children (ctx/mem c)))))

(defn chest-known
  "Later :no-chest until the call's :chest or the known :chest place is there."
  [c]
  (if (storage/chest-of (ctx/view c) (:args c))
    (contract/ok :chest "a chest is known")
    (contract/later :no-chest "no chest known")))

(defn container-refused
  "Later :refused with the zones and claims as data when enough? (every? or some?) of the positions of (positions c) is
  refused for action (:take or :put), else ok. A zone can be changed, so it is a wait, not an ending."
  [enough? action positions]
  (fn [c]
    (let [verdicts (map #(access/container-refusal c action %) (positions c))]
      (if (and (seq verdicts) (enough? some? verdicts))
        (let [fields (access/refusal-fields (remove nil? verdicts))]
          (contract/later :refused (str "refused: " (access/refusal-text fields)) (select-keys fields [:zones :claims])))
        (contract/ok :allowed "a container may be used")))))

(defn refusing
  "The check of problem (c -> nil, or the cause map the job cannot run for): nil is ok with ok-reason and ok-text, a
  cause is a status of kind (:later by default; a fn of c, as for a :look while a look round may still change it)."
  ([ok-reason ok-text problem] (refusing :later ok-reason ok-text problem))
  ([kind ok-reason ok-text problem]
   (fn [c] (if-let [cause (problem c)]
             (cause-status (if (keyword? kind) kind (kind c)) cause)
             (contract/ok ok-reason ok-text)))))

(defn zones-read
  "Later :no-zones while no zone list has been read, unless the call's :ignore-zones?."
  [c]
  (if (or (:ignore-zones? (:args c)) (some? (known/zones c)))
    (contract/ok :zones-read "zones read")
    (contract/later :no-zones "no zone list has been read")))

(defn night "Night, else :later with reason." [reason] (cond/because reason "not night" (cond/not (cond/daytime))))

(defn day "Day, else :later with reason." [reason] (cond/because reason "night" (cond/daytime)))

(defn low-health
  "Health at jobs.lib.body/risky-min-health or above, not poisoned and not in spectator or creative mode, else :later :low-health {:health :min-health}, :poisoned or :game-mode."
  [c]
  (if-let [hurt (or (body/off-mode (:primitives c)) (body/too-hurt (:primitives c)))]
    (cause-status :later hurt)
    (contract/ok :healthy "health enough")))

(defn survival-mode
  "Later :game-mode while the body is a spectator or in creative mode."
  [c]
  (if-let [off (body/off-mode (:primitives c))]
    (cause-status :later off)
    (contract/ok :survival "survival mode")))
