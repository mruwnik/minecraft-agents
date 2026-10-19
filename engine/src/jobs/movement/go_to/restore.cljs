(ns jobs.movement.go-to.restore
  "go-to's put-back: once the walk is on past an escalation, the holes it dug are filled again."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.reach :as reach]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.movement.go-to.holes :refer [note-placed! own-holes]]
            [jobs.movement.go-to.result :as end]
            [jobs.movement.go-to.torch :as torch]))

(defn restore-done!
  "Tell what was put back and what was left, and end the put-back (the walk goes on from a fresh count)."
  [c]
  (let [{:keys [restored skipped]} (ctx/mem c)]
    (when (seq restored)
      (ctx/emit! c :go-to.restored :info {:cells restored :text (str "put back " (count restored) " dug blocks")}))
    (when (seq skipped)
      (ctx/emit! c :go-to.restore-skipped :warn {:cells skipped
                                                 :text (str "left " (count skipped) " dug blocks for restore-broken")}))
    (ctx/update-mem! c #(-> % (dissoc :restore-now :restore-seen :restored :skipped :changed :best)
                            (assoc :blocked 0 :searching 0 :frontier-best {} :target-best {})))))

(defn changed?
  "Whether hole cell is seen filled (not air); a cell the body does not know is not."
  [p cell]
  (let [n (u/seen-name p (zipmap [:x :y :z] cell))]
    (boolean (and n (not (b/air n))))))

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
      (reach/closes-in? p cell) :needed
      (and (not (b/in-reach? c cell)) (deep-below? c cell)) :away)))

(defn ^:async restore-round!
  "Put back the next hole this job's escalations dug (own-holes, each in another's zone, claim or plan: undoing the
  trespass, so the place ignores zones), lowest first, with jobs.blocks.place (the dug block or what it drops), one
  child call each (ctx/call). It runs once the body got on past the escalation (restore-next!). A hole skip-why
  refuses, or the place child ends without placing (nothing to place), stays in the ledger for restore-broken (a
  :changed one is forgotten). A placed one is forgotten."
  [c]
  (let [p (:primitives c)
        seen (:restore-seen (ctx/mem c) #{})
        e (first (remove #(seen (:cell %)) (own-holes c)))]
    (if (nil? e)
      (do (restore-done! c) :again)
      (let [{:keys [cell was any-of]} e
            args (cond-> {:pos cell :fetch false :ignore-zones? true} (seq any-of) (assoc :use {:blocks (vec any-of)}))
            done! (fn [k entry]
                    (ctx/update-mem! c #(-> % (update :restore-seen (fnil conj #{}) cell) (update k (fnil conj []) entry)))
                    :again)
            skip! (fn [why]
                    (if (#{:changed :kept} why)
                      (do (tidy/forget-cell! c cell) (done! :changed cell))
                      (done! :skipped {:cell cell :block was :why why})))]
        (if-let [why (skip-why c e)]
          (skip! why)
          (let [_ (await (torch/return-torch! c))
                r (await (ctx/call c :restore 'jobs.blocks.place args))]
            (cond
              (= :continue r) :continue
              (:placed (:data r)) (do (tidy/forget-cell! c cell) (note-placed! c cell) (done! :restored cell))
              :else (skip! (contract/cause-key r)))))))))

(defn restore-next!
  "The body got on past an escalation (a walk round that arrived or made progress): put back its holes next round
  (restore-round!). Not before: a stair or door short of the way on is still the body's way, and refilling it under
  the body would strand it."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :restore-pending) (assoc :restore-now true)))
  :again)
