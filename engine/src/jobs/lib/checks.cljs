(ns jobs.lib.checks
  "Checks of a job's own, shared by jobs and never offered to agents: fns of the ctx giving a status (engine.contract).
  jobs.lib.cond holds the checks of the agent language."
  (:require [engine.contract :as contract]
            [jobs.lib.look :as look]
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

(defn always
  "The can-start? of a job with no precondition: it still makes the job a contract job."
  [_]
  (contract/ok :holds "no precondition"))
