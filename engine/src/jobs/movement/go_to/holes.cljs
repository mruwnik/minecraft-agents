(ns jobs.movement.go-to.holes
  "go-to's bookkeeping of the holes its escalations dig and the blocks it places: what the body may dig, what is its own,
  the cells a child planned to cut and the ledger entries that put them back."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.own-pen :as own-pen]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]
            [jobs.movement.go-to.result :as end]))

(defn spare-own?
  "Whether go-to keeps off the plans this body made (arg :spare-own-builds, default true)."
  [c]
  (not (false? (:spare-own-builds (:args c)))))

(defn own-plan-fn
  "(own-plan cell): the id of the plan this body made that covers cell, else nil."
  [c]
  (let [footprints (known/footprints c)
        own (access/own-plans c)]
    (fn [cell] (some own [(get footprints (vec cell))]))))

(defn may-dig-fn
  "(may-dig? cell): no other owner's zone, claim or plan footprint refuses a dig there (jobs.lib.tidy/refusal), and
  when spare? (default: go-to's spare-own?) no plan this body made covers it and, when pens? (default spare?), it is
  not the wall of the own pen holding livestock the body stands in (jobs.lib.own-pen)."
  ([c] (may-dig-fn c (spare-own? c)))
  ([c spare?] (may-dig-fn c spare? spare?))
  ([c spare? pens?]
   (let [own-plan (own-plan-fn c)
         pen-wall? (if pens? (own-pen/wall? c (end/feet-cell c)) (constantly false))]
     (fn [cell]
       (and (or (not spare?) (nil? (own-plan cell)))
            (not (pen-wall? cell))
            (or (boolean (:ignore-zones? (:args c))) (nil? (tidy/refusal c :dig (zipmap [:x :y :z] cell)))))))))

(def placed-policy
  "Body memory policy of :escalation-placed, the cells go-to put a block back in: later escalations may dig them."
  {:cap 100 :ttl (* 6 60 60 1000)})

(defn own-placed-fn
  "(own? cell block): go-to itself placed block at cell (a put-back), or the body's scaffold ledger holds it there
  placed (a pillar, a plug: its own temporary block; an :intent proves nothing), so it is not someone's build."
  [c]
  (let [placed (into (set (map (comp (juxt :cell :block) :data) (ctx/entries c :escalation-placed)))
                     (comp (filter (comp #{:placed :removing} :state)) (map (juxt :cell :item)))
                     (ledger/open-entries (ctx/view c)))]
    (fn [cell block] (contains? placed [(vec cell) block]))))

(defn forget-dug-own!
  "Drop from the scaffold ledger each entry of dug ({:cell :block}) whose block it was and whose cell is air now: a
  block built there later is someone else's."
  [c dug]
  (let [p (:primitives c)
        l (ledger/open-entries (ctx/view c))
        gone (set (for [{:keys [cell block]} dug
                        :let [e (ledger/entry-at l (vec cell))]
                        :when (and e (= block (:item e)) (b/air (u/seen-name p (zipmap [:x :y :z] cell))))]
                    (vec cell)))]
    (when (seq gone)
      (ledger/remember! c (remove (comp gone :cell) l)))))

(defn note-placed! [c cell]
  (when-let [block (u/seen-name (:primitives c) (zipmap [:x :y :z] cell))]
    (ctx/remember! c :escalation-placed {:cell (vec cell) :block block} placed-policy)))


(defn planned-cells
  "The solid cells escalation e from feet would cut, [{:cell :block}]: the stair's cuts or clear-path's door, those
  the body has sensed (the child notes what it digs of the rest)."
  [p {:keys [step heading steps]} feet]
  (let [block-at (escape/block-at-of p)
        unseen? (escape/unseen-of p)
        cells (case step
                :stair (escape/stair-cuts feet (escape/heading-dirs heading) steps)
                :clear-path (:cells (escape/door block-at unseen? feet (escape/heading-dirs heading) (escape/max-door)))
                nil)]
    (vec (for [cell cells :let [n (block-at cell)] :when (and (escape/solid? n) (not (unseen? cell)))] {:cell cell :block n}))))

(defn own-holes
  "The ledger entries (jobs.lib.tidy :tidy, body memory) of cells this go-to's escalations dug, lowest first."
  [c]
  (->> (tidy/entries c)
       (filter #(and (:escalation %) (= (:root c) (:job %)) (= (:id c) (:go-to %))))
       (sort-by (comp second :cell))
       vec))

(defn hole-tag
  "What marks a hole as this go-to's in the tidy ledger; its stair and clear-path children write it (:note) as they dig."
  [c]
  {:escalation true :go-to (:id c)})

(defn note-holes!
  "Write every cell of dug ({:cell :block}) that is air now and not yet noted to the ledger, as a :dig entry with
  :escalation true and :any-of, the items that put it back (the block or what it drops). Body memory outlives the
  round, a cut, a cancel and a restart: jobs.survival.restore-broken puts back what go-to did not."
  [c dug]
  (let [own-plan (own-plan-fn c)]
    (doseq [{:keys [cell block]} (distinct dug)]
      (when-let [plan (own-plan cell)]
        (ctx/warn-once! c [:dug-own cell] :go-to.dug-own-build
                        {:cell (vec cell) :block block :plan plan :text (str "dug " block " at " (pr-str (vec cell)) " in its own plan " plan)}))
      (escape/note-hole! c (hole-tag c) cell block))))
