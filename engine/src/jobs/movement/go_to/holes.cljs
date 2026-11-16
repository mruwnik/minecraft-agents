(ns jobs.movement.go-to.holes
  "go-to's bookkeeping of the holes its escalations dig and the blocks it places: what the body may dig, what is its own,
  the cells a child planned to cut and the ledger entries that put them back."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.cost :as cost]
            [engine.ctx :as ctx]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.own-builds :as own :refer [own-plan-fn]]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.movement.go-to.result :as end]))

(defn forget-dug-own!
  "Drop from the scaffold ledger each entry of dug ({:cell :block}) whose block it was and whose cell is air now: a
  block built there later is someone else's."
  [c dug]
  (let [p (:primitives c)
        l (ledger/entries c)
        gone (set (for [{:keys [cell block]} dug
                        :let [e (ledger/entry-at l (vec cell))]
                        :when (and e (= block (:item e)) (bs/air (u/seen-name p (zipmap [:x :y :z] cell))))]
                    (vec cell)))]
    (when (seq gone)
      (ledger/remember! c (remove (comp gone :cell) l)))))

(defn note-placed! [c cell]
  (when-let [block (u/seen-name (:primitives c) (zipmap [:x :y :z] cell))]
    (ctx/remember! c :escalation-placed {:cell (vec cell) :block block} own/placed-policy)))


(defn planned-cells
  "The solid cells escalation e from feet would cut, [{:cell :block}]: the stair's cuts, those
  the body has sensed (the child notes what it digs of the rest)."
  [p {:keys [step heading steps]} feet]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        cells (case step
                :stair (escape/stair-cuts feet (escape/heading-dirs heading) steps)
                nil)]
    (vec (for [cell cells :let [n (block-at cell)] :when (and (escape/solid? n) (not (unseen? cell)))] {:cell cell :block n}))))

(defn own-holes
  "The ledger entries (jobs.lib.tidy :tidy, body memory) of cells this go-to's escalations dug, lowest first, on a level
  the farthest from the feet first (a dead-end cut is filled from its end back to its mouth)."
  [c]
  (let [[fx _ fz] (end/feet-cell c)]
    (->> (tidy/entries c)
         (filter #(and (:escalation %) (= (:root c) (:job %)) (= (:id c) (:go-to %))))
         (sort-by (fn [{[x y z] :cell}] [y (- (+ (* (- x fx) (- x fx)) (* (- z fz) (- z fz))))]))
         vec)))

(defn hole-tag
  "What marks a hole as this go-to's in the tidy ledger; its stair child writes it (:note) as it digs.
  :mend while the escalation cuts the body's own builds (:own-cut): every hole of it is put back, own ground or not."
  [c]
  (cond-> {:escalation true :go-to (:id c)} (:own-cut (:escalation (ctx/mem c))) (assoc :mend true)))

(defn note-holes!
  "Write every cell of dug ({:cell :block}, :dug true when a child reported digging it) that is air now or so dug and not yet noted to the ledger, as a :dig entry with
  :escalation true and :any-of, the items that put it back (the block or what it drops). A cell under a dug build cell
  (a fence's ground) is a :mend too: the restored build must not hang over a hole. Body memory outlives the
  round, a cut, a cancel and a restart: jobs.survival.restore-broken puts back what go-to did not."
  [c dug]
  (let [own-plan (own-plan-fn c)
        tag (hole-tag c)]
    (reduce (fn [mends {:keys [cell block] :as d}]
              (let [cell (vec cell)
                    [x y z] cell
                    mend? (or (contains? mends [x (inc y) z]) (pos? (cost/destroyed-s block nil nil false)))]
                (when-let [plan (own-plan cell)]
                  (ctx/warn-once! c [:dug-own cell] :go-to.dug-own-build
                                  {:cell cell :block block :plan plan :text (str "dug " block " at " (pr-str cell) " in its own plan " plan)}))
                (escape/note-hole! c (cond-> tag mend? (assoc :mend true)) cell block (boolean (:dug d)))
                (cond-> mends mend? (conj cell))))
            #{} (sort-by (comp - second :cell) (distinct dug)))
    nil))
