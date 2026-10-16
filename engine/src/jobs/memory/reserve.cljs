(ns jobs.memory.reserve
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.ready :as ready]
            [jobs.lib.reserve :as reserve]))

(def doc
  "Reserve carried items in the body's register (jobs.lib.reserve), in one round: eating, depositing, making room and
  crafting leave a reserved unit alone, or use it only when the alternative costs more (strength under 1).
  Name one :item (an item name) or one :tag (:food :ore :metal :bed :seeds, the carried names of the tag). :count is
  the units reserved (default :all of what is carried). :strength 0..1 (default 1 = never used; 0.5 = used when it is
  worth twice the item; 0 = no reservation). :ttl-s ends the entry after that many seconds (default: kept until
  unreserved). :id names the entry (default \"agent-<item or tag>\"); writing an id again replaces its entry.
  Refused (warn reserve.refused; ends :condition-failed with cause {:reason :text}): :no-target (neither or both of
  :item and :tag).
  Success: info reserve.set with the entry; ends :done with data {:id}. Entries stay until unreserved or expired; see
  `observe inventory` for the register.")

(a/defargs args
  {:id {:doc "the entry's name; nil: \"agent-<item or tag>\"" :spec (a/or-of keyword? string?) :default nil}
   :item {:doc "an item name to reserve" :spec a/name? :default nil}
   :tag {:doc "a tag of item names: :food :ore :metal :bed :seeds" :spec (a/pred "one of :food :ore :metal :bed :seeds" (set (keys reserve/tags))) :default nil}
   :count {:doc "units reserved, or :all of what is carried" :spec (a/or-of #{:all} (a/int-in 1 nil)) :default :all}
   :strength {:doc "0..1: 1 never used, lower is used when worth the item's value times s/(1-s)" :spec (a/num-in 0 1) :default 1}
   :why {:doc "a note for whoever reads the register" :spec string? :default nil}
   :ttl-s {:doc "seconds the entry lasts; nil: until unreserved" :spec a/pos-num? :default nil}})

(def target [:item :tag])

(a/defargs settings
  {::job-time-s {:default 5 :doc "Seconds one call may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(defn bad-args
  "The refusal cause when the call names not exactly one of :item and :tag."
  [{:keys [item tag]}]
  (when (= (some? item) (some? tag))
    {:reason :no-target :text "name exactly one of :item and :tag"}))

(defn entry-of
  "The register entry the call writes, at time now."
  [{:keys [id item tag count strength why ttl-s]} now]
  (cond-> {:id (if id (name id) (str "agent-" (or item (name tag))))
           :count count :strength strength :by "agent" :why why
           :until (when ttl-s (+ now (* 1000 ttl-s)))}
    item (assoc :item item)
    tag (assoc :tag tag)))

(defn can-start? [c] (checks/cause-status :never (bad-args (:args c))))

(defn done?
  "Done when the register holds the entry the call would write."
  [c]
  (let [now (:now (ctx/view c))
        e (entry-of (:args c) now)
        held? (some #(= (dissoc e :until) (dissoc % :until)) (reserve/entries (ctx/view c)))]
    (if held?
      (contract/ok :reserved (str (:id e) " is in the register") :units :entries :done 1 :left 0 :result {:id (:id e)})
      (contract/later :not-reserved (str (:id e) " is not in the register") :units :entries :done 0 :left 1 :result {:id (:id e)}))))

(defn quote-of [_c] {:uses {:job-time [0 0 1]}})

(def safe? ready/ok)
(def fit? ready/ok)

(defn round [c]
  (if-let [cause (bad-args (:args c))]
    (do (ctx/emit! c :reserve.refused :warn cause)
        (ctx/end c :condition-failed cause))
    (let [e (entry-of (:args c) (:now (ctx/view c)))]
      (ctx/forget-where! c reserve/kind #(= (:id e) (:id %)))
      (ctx/remember! c reserve/kind e reserve/policy)
      (ctx/emit! c :reserve.set :info (assoc e :text (str (:id e) " reserved")))
      (ctx/end c :done nil {:id (:id e)}))))
