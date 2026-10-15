(ns jobs.lib.checks
  "Checks of a job's own, shared by jobs and never offered to agents: fns of the ctx giving a status (engine.contract).
  jobs.lib.cond holds the checks of the agent language."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.job-common :as common]
            [jobs.lib.body :as body]
            [jobs.lib.cond :as cond]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
            [jobs.lib.submitted :as submitted]
            [jobs.lib.world :as known]))

(defn args-fit
  "The check that ok? (a fn of the ctx) holds of the call's args: :ok :args, else :never :bad-args with text."
  [text ok?]
  (fn [c] (if (ok? c) (contract/ok :args "args fit") (contract/never :bad-args text))))

(defn unseen-later
  "A :later with reason, text and data, a :look instead while the body has not looked round."
  [c reason text & data]
  (apply (if (look/surveyed? c) contract/later contract/look) reason text data))

(defn later-of
  "A :later of the cause map m ({:reason r ...data}) with text."
  [text m]
  (apply contract/later (:reason m) text (mapcat identity (dissoc m :reason))))

(defn never-of
  "A :never of the cause map m ({:reason r ...data}) with text."
  [text m]
  (apply contract/never (:reason m) text (mapcat identity (dissoc m :reason))))

(defn not-moved
  "Later :moved-since-submit while the call has a :from the body has left before its first round (jobs.lib.submitted)."
  [c]
  (if-let [away (submitted/away-cause c)]
    (later-of (str "moved " (:distance away) " blocks since submit") away)
    (contract/ok :here "where it was submitted")))

(defn zones-read
  "Later :no-zones while no zone list has been read, unless the call's :ignore-zones?."
  [c]
  (if (or (:ignore-zones? (:args c)) (some? (known/zones c)))
    (contract/ok :zones-read "zones read")
    (contract/later :no-zones "no zone list has been read")))

(defn holds "A :ok status for a check with nothing to say." [] (contract/ok :holds "no precondition"))

(defn cause-text [m] (str (or (:why m) (:text m) (name (:reason m)))))

(defn later-with
  "The status of an optional cause map ({:reason r ...data}): :ok when nil, else :later with its :why or :text."
  [m]
  (if m (later-of (cause-text m) m) (holds)))

(defn never-with "Like later-with, :never." [m]
  (if m (never-of (cause-text m) m) (holds)))

(defn chest-known
  "Later :no-chest until the call's :chest or the known :chest place is there."
  [c]
  (if (storage/chest-of (ctx/view c) (:args c))
    (contract/ok :chest "a chest is known")
    (contract/later :no-chest "no chest known")))

(defn cause-status
  "[kind text data] for a cause, a map {:reason ..} or just the reason keyword (cause-text is the text)."
  [kind cause]
  (let [cause (if (keyword? cause) {:reason cause} cause)]
    (contract/status kind (:reason cause) (cause-text cause) (dissoc cause :reason))))

(defn refusing
  "The check of problem (c -> nil, or the cause map the job cannot run for): nil is ok with ok-reason and ok-text, a
  cause is a status of kind (:later by default)."
  ([ok-reason ok-text problem] (refusing :later ok-reason ok-text problem))
  ([kind ok-reason ok-text problem]
   (fn [c] (if-let [cause (problem c)] (cause-status kind cause) (contract/ok ok-reason ok-text)))))

(defn looking
  "refusing, but a cause is a :look while (look? c) holds: a look round may still change it."
  [look? ok-reason ok-text problem]
  (fn [c] (if-let [cause (problem c)] (cause-status (if (look? c) :look :later) cause) (contract/ok ok-reason ok-text))))

(defn zones-declined
  "zones-read, with one warn of kind from job-name (jobs.lib.job-common/no-zones)."
  [kind job-name]
  (refusing :zones-read "zone list read" #(common/no-zones % kind job-name)))

(defn night "Night, else :later with reason." [reason] (cond/because reason "not night" (cond/not (cond/daytime))))

(defn day "Day, else :later with reason." [reason] (cond/because reason "night" (cond/daytime)))

(defn low-health
  "Health at jobs.lib.body/risky-min-health or above, else :later :low-health {:health :min-health}."
  [c]
  (if-let [hurt (body/too-hurt (:primitives c))]
    (cause-status :later hurt)
    (contract/ok :healthy "health enough")))

(defn all
  "The status of every check asked (none short-cut): any :never, else the first :later, else :ok. Unlike
  jobs.lib.cond/and it keeps no :parts, so an ending's cause stays the failing check's."
  [& checks]
  (fn [c] (contract/all (mapv #(contract/asked c %) checks))))
