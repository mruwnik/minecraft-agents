(ns jobs.lib.declined
  "For a parent whose round returns :declined when its child declined: the parent books the child that declined
  (slot, job, args) in its memory, and its check runs that child's check (ctx/check-child). While the child would still
  decline the parent is parked, its wait reason is the child's (told once), and no rounds run. The booking is dropped
  at the start of each round (begin!), so a round that calls no child, or whose child does not decline, leaves none."
  (:require [engine.ctx :as ctx]))

(def key* :declined-child)

(defn begin!
  "Drop the booking: call first in the parent's round. A child that declines again in this round is booked again."
  [c]
  (when (key* (ctx/mem c))
    (ctx/update-mem! c dissoc key*)))

(defn ^:async call-child!
  "ctx/call-child, then book the child when it declined and drop the booking when it did not. A promise of the result."
  [c slot job args]
  (let [r (await (ctx/call-child c slot job args))
        booked (key* (ctx/mem c))
        want (when (= :declined r) {:slot slot :job job :args args})]
    (when (not= booked want)
      (ctx/update-mem! c #(if want (assoc % key* want) (dissoc % key*))))
    r))

(defn check
  "False, with the child's wait reason, while the booked child's check would decline; true otherwise."
  [c]
  (if-let [{:keys [slot job args]} (key* (ctx/mem c))]
    (boolean (ctx/check-child c slot job args))
    true))
