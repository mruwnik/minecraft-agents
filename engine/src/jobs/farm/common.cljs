(ns jobs.farm.common
  "What the farm jobs share as contract jobs: the walk to a cell (a go-to child) and the
  test of a partial outcome against the call's :least."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.places :as places]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(defn enough?
  "Whether n of total units (cells, uses, bone meal) meet the call's :least (default 1; :all wants every one)."
  [c n total]
  (let [least (:least (ctx/frozen-call c) 1)]
    (>= n (if (= :all least) total least))))

(defn no-zones
  "The cause {:reason :no-zones} while no zone list has been read (unless the call's :ignore-zones?), with one kind warn
  (a job.declined event of job-name); else nil."
  [c kind job-name]
  (when-not (or (:ignore-zones? (:args c)) (some? (known/zones c)))
    (ctx/warn-once! c [:access :no-zones] kind
                    {:reason :no-zones
                     :text (str job-name " declined: no zone list has been read (zones.edn missing or never valid)")})
    {:reason :no-zones}))

(defn ^:async walk!
  "Walk until the body's cell is within range of pos with a go-to child (slot :walk, no retry, no warn), carrying the
  call's :ignore-zones?. opts: :tolls (go-to's), :zone-tolls. Resolves to :there, :continue (go-to waits on the world)
  or :blocked (it gave up, or pos is no cell)."
  [c pos range {:keys [tolls zone-tolls]}]
  (let [cell (:pos (places/parse-pos pos))]
    (cond
      (nil? cell) :blocked
      (u/within? (u/self-pos c) cell range) :there
      :else (let [e (await (child/call-walk* c :walk {:pos cell :range range :doors :shut :dangers true :tolls tolls
                                                      :zone-tolls (boolean zone-tolls) :escalate true :look-round true
                                                      :one-way :closed :warn false :retry false
                                                      :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
              (cond (= :continue e) :continue
                    (and (= :done (:kind e)) (:arrived (:data e))) :there
                    :else :blocked)))))

(defn ^:async walk-to!
  "walk! to pos with the tolls of the cell: :there, :continue or :blocked."
  [c pos range]
  (await (walk! c pos range {:tolls (tc/walk-tolls c (:pos (places/parse-pos pos)))})))
