(ns jobs.lib.plan-field
  "Reading the plan of a plan-mode farm or forestry job: its cells, why it cannot be worked, and the can-run? built on that."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.world :as known]))

(defn part-cells
  "The cells of a plan answer in part (all when nil)."
  [answer part]
  (filter #(or (nil? part) (= part (:part %))) (:cells answer)))

(defn trouble
  "Why a plan answer with these cells (called what, e.g. \"crop cells\") cannot be worked, or nil."
  [answer cells what]
  (cond
    (nil? answer) "no such plan"
    (:broken answer) (str "the plan cannot be read: " (:broken answer))
    (empty? cells) (str "no " what)))

(def no-zones "no zone list has been read")

(defn read-plan
  "The plan of c's args: {:answer plan-answer :cells cells} when it can be worked, else {:trouble text :later? bool}
  (later when the plan is missing or unreadable or the zone list is unread: it may come; never when it has no cells).
  opts: :cells (fn [answer part] -> cells), :what (name of the cells in the trouble text), :zones? true to want a zone
  list unless the job's :ignore-zones? is set, :event and :label to warn once per reason as <event> (\"<label> declines
  plan ...\"); no :event is a pure read."
  [c {:keys [cells what zones? event label]}]
  (let [{:keys [plan part ignore-zones?]} (:args c)
        answer (known/plan c plan)
        found (cells answer part)
        unzoned (when (and zones? (nil? (known/zones c)) (not ignore-zones?)) no-zones)
        why (or (trouble answer found what) unzoned)]
    (if-not why
      {:answer answer :cells found}
      (do (when event
            (ctx/warn-once! c [plan why] event
                            {:plan plan :part part :reason why
                             :text (str label " declines plan " plan (when part (str " part " part)) ": " why)}))
          {:trouble why :later? (or (known/unreadable? answer) (= unzoned why))}))))

(defn plan-can-run?
  "Later :plan-trouble while the plan is missing or unreadable (or the zone list unread), never when it has no cells."
  [{:keys [trouble later?]}]
  (cond
    (nil? trouble) (contract/ok :plan "no plan trouble")
    later? (contract/later :plan-trouble trouble :why trouble)
    :else (contract/never :plan-trouble trouble :why trouble)))
