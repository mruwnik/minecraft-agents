(ns jobs.movement.go-to.restore
  "go-to's put-back: once the walk is on past an escalation, the holes it dug are filled again."
  (:require [jobs.lib.block-sets :as bs]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.movement.go-to.holes :refer [note-placed! own-holes]]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.torch :as torch]))

(defn restore-done!
  "Tell what was put back and what was left, and end the put-back (the walk goes on from a fresh count; a goal cell held
  back, :restore-held, keeps the put-back pending for the arrival). The blocks a stair kept for it (jobs.lib.reserve
  entries this call holds) are free again."
  [c]
  (reserve/set-job-entries! c [])
  (let [{:keys [restored skipped]} (ctx/mem c)]
    (when (seq restored)
      (ctx/emit! c :go-to.restored :info {:cells restored :text (str "put back " (count restored) " dug blocks")}))
    (when (seq skipped)
      (ctx/emit! c :go-to.restore-skipped :warn {:cells skipped
                                                 :text (str "left " (count skipped) " dug blocks for restore-broken")}))
    (ctx/update-mem! c #(-> % (dissoc :restore-now :restore-seen :restore-stepped :restored :skipped :changed :best :restore-held)
                            (cond-> (:restore-held %) (assoc :restore-pending true))
                            (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {})))))

(defn changed?
  "Whether hole cell is seen filled (not air); a cell the body does not know is not."
  [p cell]
  (let [n (u/seen-name p (zipmap [:x :y :z] cell))]
    (boolean (and n (not (bs/air n))))))

(defn deep-below?
  "Whether cell lies more than 2 below the body's feet: a place walk (range 3) has to go down into the shaft for it."
  [c [_ y _]]
  (> (- (second (end/feet-cell c)) y) 2))

(defn skip-why
  "Why hole e must not be filled now, or nil: :changed (no longer air: forgotten), :kept (a cell of a stair way cut to
  climb away from a dive, recorded in memory :stair-way: the next go-to walks it; forgotten), :occupied (the body is in it),
  :needed (filling it would shut in a body that has room now: the stair it stands on, its way out), :away (out of
  reach and deep below the feet: walking down to it would put the body inside its own refilled stair)."
  [c {:keys [cell]}]
  (let [p (:primitives c)]
    (cond
      (changed? p cell) :changed
      (and (:dive-climb (ctx/mem c)) (contains? (ledger/own-cells c) cell)) :kept
      (tidy/in-body? p cell) :occupied
      (reach-room/closes-in? p :fill cell) :needed
      (and (not (b/in-reach? c cell)) (deep-below? c cell)) :away)))

(defn goal-cell?
  "Whether cell is the goal's feet or head cell while the body is still outside the call's range of it: the walk ends in it."
  [c pos cell]
  (let [goal (when (:y pos) (mapv #(js/Math.floor (% pos)) [:x :y :z]))
        [gx gy gz] goal
        feet (end/feet-cell c)]
    (boolean (and goal (#{[gx gy gz] [gx (inc gy) gz]} (vec cell))
                  (> (u/dist (zipmap [:x :y :z] feet) pos) (:range (:args c) 1))))))

(defn ahead-cell?
  "Whether cell is held for the walk: the goal's feet or head cell, or a cell of the last dug route (memory :route-ahead,
  in walking order) the body has not passed (at or after the route cell nearest its feet), while the body is outside
  the call's range of the goal. A refill would send the walk back to dig it again."
  [c pos cell]
  (let [feet (end/feet-cell c)
        route (mapv vec (:route-ahead (ctx/mem c)))
        at (fn [x] (u/dist (zipmap [:x :y :z] (mapv + [0.5 0.5 0.5] x)) (zipmap [:x :y :z] (mapv + [0.5 0.5 0.5] feet))))
        near (when (seq route) (apply min-key #(at (route %)) (range (count route))))
        i (.indexOf route (vec cell))]
    (boolean (or (goal-cell? c pos cell)
                 (and (:y pos) (> (u/dist (zipmap [:x :y :z] feet) pos) (:range (:args c) 1))
                      near (>= i near))))))

(defn shut-in-short?
  "Whether the body is out of the call's range of a cell goal pos and shut in where it stands (escape/walk-enclosed?)."
  [c pos]
  (boolean (and (:y pos) (> (u/dist (zipmap [:x :y :z] (end/feet-cell c)) pos) (:range (:args c) 1))
                (escape/walk-enclosed? c))))

(defn ^:async restore-round!
  "Put back the next hole this job's escalations dug (own-holes, each in another's zone, claim or plan: undoing the
  trespass, so the place ignores zones), lowest first, with jobs.blocks.place (the dug block or what it drops), one
  child call each (ctx/call). It runs once the body got on past the escalation (restore-next!). A hole skip-why
  refuses, or the place child ends without placing (nothing to place), stays in the ledger for restore-broken (a
  :changed one is forgotten). A hole the body stands in is stepped off once (jobs.lib.step-off) before it is tried again.
  A placed one is forgotten. A hole of the dug route the body has not passed is held until the body is within range of the goal (:restore-held), and so
  is a cut of the body's own build (:mend) while the body is still shut in short of the goal (shut-in-short?): the next way
  would cut it again, its put-back owed once."
  [c pos]
  (let [p (:primitives c)
        seen (:restore-seen (ctx/mem c) #{})
        e (first (remove #(seen (:cell %)) (own-holes c)))]
    (if (nil? e)
      (do (restore-done! c) :again)
      (let [{:keys [cell was any-of]} e
            args (cond-> {:pos cell :ways #{:carried} :ignore-zones? true} (seq any-of) (assoc :use {:blocks (vec any-of)}))
            done! (fn [k entry]
                    (ctx/update-mem! c #(-> % (update :restore-seen (fnil conj #{}) cell) (update k (fnil conj []) entry)))
                    :again)
            skip! (fn [why]
                    (if (#{:changed :kept} why)
                      (do (tidy/forget-cell! c cell) (done! :changed cell))
                      (done! :skipped {:cell cell :block was :why why})))]
        (if (or (ahead-cell? c pos cell) (and (:mend e) (shut-in-short? c pos)))
          (do (ctx/update-mem! c #(-> % (update :restore-seen (fnil conj #{}) cell) (assoc :restore-held true))) :again)
        (if-let [why (skip-why c e)]
          (if (and (= :occupied why) (not (contains? (:restore-stepped (ctx/mem c)) cell)))
            (let [r (await (step-off/step-off! c cell {:avoid (into #{} (map (comp vec :cell)) (own-holes c))}))]
              (if (= :continue r)
                :continue
                (do (ctx/update-mem! c update :restore-stepped (fnil conj #{}) cell)
                    :again)))
            (skip! why))
          (let [_ (await (torch/return-torch! c))
                r (await (ctx/call c :restore 'jobs.blocks.place args))]
            (cond
              (= :continue r) :continue
              (:placed (:data r)) (do (tidy/forget-cell! c cell) (note-placed! c cell) (done! :restored cell))
              :else (skip! (contract/cause-key r))))))))))

(defn restore-next!
  "The body got on past an escalation (a walk round that arrived or made progress): put back its holes next round
  (restore-round!). Not before: a stair or door short of the way on is still the body's way, and refilling it under
  the body would strand it."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :restore-pending) (assoc :restore-now true)))
  :again)
