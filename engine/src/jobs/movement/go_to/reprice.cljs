(ns jobs.movement.go-to.reprice
  "go-to's safe? asked again mid-walk, once per hostile it comes to know near the way: the engine asks it only as a round
  starts, and a go-to round walks on for up to a minute (an exclusive run has no reflex to meet a hostile met on the way)."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.danger :as danger]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.walk-risk :as risk]))

(defn status
  "go-to's own safe? for the walk to pos: not threatened (ready/threatened-walking) and the risk acceptable (risk/status);
  ok with the head under water (air first)."
  [c pos]
  (if (breath/head-under? (:primitives c))
    (contract/ok :submerged "head under water: air first")
    (contract/all [(ready/threatened-walking c pos) (risk/status c pos)])))

(defn check
  "near/walk-round!'s :stair-check: check's why, else on a re-plan that knows of a hostile near the way to pos not known
  before (at the round's start or a re-plan that passed), {:hostile s} when status s, read anew (check-cache/fresh: the round start's reads are up to 2 s old), for the rest of the walk, is :later or
  :never (a :refresh re-plan keeps its old plan whatever the check says, so a failed hostile is asked again). Other re-plans read only the hostile list. None under a caller's :checks :safe?, which replaces go-to's."
  [c pos check]
  (let [p (:primitives c)
        ids #(into #{} (map (fn [e] (.-id e))) (danger/hostiles-near-way p pos (settings/get price/settings ::price/way-radius) {}))
        known (atom (ids))
        own? (nil? (get-in (ctx/frozen-call c) [:checks :safe?]))]
    (fn [plan first?]
      (or (when check (check plan first?))
          (when (and own? (not first?))
            (let [now (ids)]
              (when (seq (remove @known now))
                (let [s (status (check-cache/fresh c) pos)]
                  (if (#{:later :never} (first s))
                    {:hostile s}
                    (do (swap! known into now) nil))))))))))

(defn end
  "The step result for a walk stopped by check's {:hostile s}: as pace/steps! treats a can-run? that fails between steps."
  [c s]
  (if (= :never (first s))
    (ctx/end c :impossible (contract/status-cause s))
    (if (ctx/returns-on-later? c) (ctx/end c :condition-failed (contract/status-cause s)) :continue)))
