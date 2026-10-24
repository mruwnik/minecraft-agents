(ns jobs.movement.go-to.reprice
  "go-to's safe? asked of the plan its round is about to walk, and again mid-walk once per hostile it comes to know near the
  way: the engine asks it only as a round starts, before there is a plan, and a go-to round walks on for up to a minute
  (an exclusive run has no reflex to meet a hostile met on the way)."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.danger :as danger]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]
            [jobs.lib.walk-risk :as risk]))

(defn status
  "go-to's own safe? for the walk to pos on plan (risk/status; none: the plan its round held on, to its goal): not threatened
  (ready/threatened-walking, on the plan's way when there is one) and the risk acceptable; ok with the head under water (air first)."
  ([c pos] (let [kept (risk/planned c)] (status c (or (:goal kept) pos) kept)))
  ([c pos plan]
   (if (breath/head-under? (:primitives c))
     (contract/ok :submerged "head under water: air first")
     (contract/all [(ready/threatened-walking c pos (when plan (risk/timed-way c pos plan))) (risk/status c pos plan)]))))

(defn unfinished?
  "Whether plan walks to where a search that has not ended has got to (wsearch/unfinished-plan, which drops the search
  unless its :search-on keeps it)."
  [plan]
  (= "searching" (some-> ^js (:r plan) .-reason)))

(defn safe-prefix?
  "Whether plan stops short of pos, the walk to pos is within price/allowed but for the unseen cells guessed dark caves
  (risk/estimate's caves-free?), and the walk on the plan's own steps to their end passes status: walking that and
  looking turns unseen into seen, so the walk is priced again from there rather than held on a guess."
  [c pos plan]
  (let [end (peek (:steps plan))
        feet (mapv js/Math.floor ((juxt :x :y :z) (u/self-pos c)))]
    (and (not= "found" (:status plan)) end
         (not= feet (mapv js/Math.floor ((juxt :x :y :z) end)))
         (<= (:hp (risk/estimate c pos {:plan plan :caves-free? true})) (price/allowed c))
         (= :ok (first (status c end (assoc plan :status "found")))))))

(defn check
  "near/walk-round!'s :stair-check: check's why, else status of the round's first plan, of every re-plan once a plan
  passed by safe-prefix? (a re-plan would walk on past the prefix unpriced), and of a re-plan that knows of a
  hostile near the way to pos not known before (at the first plan or a re-plan that passed), read anew (check-cache/fresh):
  {:risk s} when s is :later or :never, the plan kept (risk/note-plan!) for the start check to hold the call on, unless
  the plan stops short of pos with a safe-prefix? (walked, then priced again), or
  {:searching true} for a plan to where a search that has not ended has got to, whose rest is priced unseen: the search
  goes on rather than hold (a :refresh re-plan keeps its old plan whatever the check says, so a failed hostile is asked
  again). Other re-plans read only the hostile list. None under a caller's :checks :safe?, which replaces go-to's."
  [c pos check]
  (let [p (:primitives c)
        ids #(into #{} (map (fn [e] (.-id e))) (danger/hostiles-near-way p pos (settings/get price/settings ::price/way-radius) {}))
        known (atom (ids))
        prefixed (atom false)
        own? (nil? (get-in (ctx/frozen-call c) [:checks :safe?]))]
    (fn [plan first?]
      (or (when check (check plan first?))
          (when own?
            (let [now (ids)]
              (when (or first? @prefixed (seq (remove @known now)))
                (let [c' (cond-> c (not first?) check-cache/fresh)
                      s (status c' pos plan)]
                  (cond
                    (not (#{:later :never} (first s))) (do (swap! known into now) nil)
                    (unfinished? plan) (do (some-> (:search-on plan) (apply [])) {:searching true})
                    (safe-prefix? c' pos plan) (do (reset! prefixed true) nil)
                    :else (do (risk/note-plan! c pos plan) {:risk s}))))))))))

(defn end
  "The step result for a walk stopped by check's {:risk s}: as pace/steps! treats a can-run? that fails between steps; a
  yield names s's cause as its wait (an exclusive run, which a yield ends, stops with it)."
  [c s]
  (cond
    (= :never (first s)) (ctx/end c :impossible (contract/status-cause s))
    (ctx/returns-on-later? c) (ctx/end c :condition-failed (contract/status-cause s))
    :else (do (some-> (:child-wait c) (reset! (contract/status-cause s))) :continue)))
