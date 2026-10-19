(ns engine.core.notices
  "The info notice a memory write earns, whoever wrote it: a place set or forgotten, a reservation set or cleared,
  and (for a write the agent asked for) a remembered entry. Pure: the ctx emits what these return.
  A place is an entry written under mem/place-policy with a :pos; the reservation register is kind :reserve."
  (:require [engine.memory :as mem]))

(defn place-kind? [view kind] (= mem/place-policy (mem/policy view kind)))

(defn written
  "[event-kind fields] for the write of data to kind under policy (view is the memory after it), or nil.
  was is the entry data before it; explicit? is a write a caller asked for by name (set-place, remember)."
  [view kind data policy was explicit?]
  (cond
    (and (= mem/place-policy policy) (:pos data))
    [:place.set (cond-> {:name kind :pos (:pos data) :was (:pos was)
                         :text (str (name kind) " recorded at " (pr-str (:pos data)))}
                  (not explicit?) (assoc :auto true))]
    (= :reserve kind)
    [:reserve.set (assoc data :text (str (:id data) " reserved"))]
    explicit?
    [:memory.remembered {:memory-kind kind :entry data :policy policy :text (str (name kind) " remembered")}]))

(defn forgotten
  "[event-kind fields] for the entries (data maps) dropped from kind, or nil when none."
  [view kind dropped]
  (cond
    (empty? dropped) nil
    (and (place-kind? view kind) (:pos (peek dropped)))
    [:place.forgotten {:name kind :was (:pos (peek dropped)) :text (str (name kind) " forgotten")}]
    (= :reserve kind)
    [:reserve.cleared {:removed (count dropped) :ids (mapv :id dropped)
                       :text (str (count dropped) " reserved entries dropped")}]))
