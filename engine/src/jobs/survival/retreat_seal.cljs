(ns jobs.survival.retreat-seal
  "A cornered body's seal (jobs.survival.retreat): the open cells round it filled with carried blocks."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.seal :as seal]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]))

(defn occupied? [c cell] (contains? (:seal-occupied (ctx/mem c)) cell))

(defn ^:async place-seal!
  "Place carried blocks at cells in order, each ledgered (flight/plug-purpose) for the cleanup. :ok, :wait while a mob stands in a cell (the place is refused and tried again
  for dig-cells/mob-wait-ms), or :failed at the first placement refused or with nothing left to place. A cell with
  nothing to place against yet goes to the end of the order (its neighbours may be placed first); :failed once every
  cell left has none. A door, gate or trapdoor standing open is shut instead
  (dig-cells/shut-open!); a cell the place answers occupied (a torch, a chest, a bed: a block the seal leaves alone), or an
  open iron door, is remembered in :seal-occupied and never tried again this flight."
  [c cells]
  (loop [cells cells skipped 0]
    (let [item (lb/pick c (flight/blocks c))
          cell (first cells)]
      (cond
        (empty? cells) :ok
        (dig-cells/sealed? (:primitives c) cell) (recur (rest cells) 0)
        (nil? item) :failed
        :else (let [door (await (dig-cells/shut-open! c cell))
                    l (when-not door (ledger/intend (ledger/open-entries (ctx/view c))
                                                    {:cell [(:x cell) (:y cell) (:z cell)] :item item :before (u/seen-name (:primitives c) cell)
                                                     :job (:id c) :purpose (flight/plug-purpose c)}))
                    _ (some->> l (ledger/remember! c))
                    r (when-not door (await (tidy/place! c cell item true)))
                    _ (some->> l (#(ledger/reconcile % (escape/block-at-of (:primitives c)))) (ledger/remember! c))
                    status (some-> r .-status)]
                (cond
                  (or (= :shut door) (= "placed" status))
                  (do (dig-cells/forget-wait! c :seal-mob-since)
                      (recur (rest cells) 0))
                  (and (= "no-support" status) (< (inc skipped) (count cells)))
                  (recur (concat (rest cells) [cell]) (inc skipped))
                  (or (= :open door) (= "occupied" status))
                  (do (ctx/update-mem! c update :seal-occupied (fnil conj #{}) cell)
                      (recur (rest cells) 0))
                  (dig-cells/keep-waiting! c :seal-mob-since r) :wait
                  :else (do (dig-cells/forget-wait! c :seal-mob-since)
                            (ctx/update-mem! c assoc :seal-refusal (str "a place at " (pr-str [(:x cell) (:y cell) (:z cell)]) " was refused: " status (dig-cells/mob-text r)))
                            :failed)))))))

(defn shooters
  "Where the seen ranged hostiles within the flight's :ranged-radius stand."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)
        p (:primitives c)]
    (->> (danger-q/known-hostiles p radius {:ranged-radius ranged-radius})
         (filter #(and (combat/ranged? %) (danger-q/seen-only? %)))
         (mapv #(danger-q/mob-pos p %)))))

(defn facing-first
  "cells (dig-cells/fill-cells order) with the sides towards the nearest of shooters first, each side's feet cell then its
  head cell, so the first blocks stop arrows; the roof support and the roof stay last. As given with no shooter
  (seal! then orders by nearest-first of the melee hostiles)."
  [feet shooters cells]
  (if (empty? shooters)
    cells
    (let [side-of (fn [{:keys [x y z]}] (when (<= (:y feet) y (inc (:y feet))) [x z]))
          gap (fn [[x z]] (apply min (map #(u/dist {:x (+ x 0.5) :y (:y feet) :z (+ z 0.5)} %) shooters)))
          sides (->> cells (keep side-of) distinct (sort-by gap))
          rank (zipmap sides (range))]
      (sort-by (fn [{:keys [y] :as cell}]
                 (if-let [r (rank (side-of cell))] [0 r (- y (:y feet))] [1 0 0]))
               cells))))

(defn nearest-first
  "cells (dig-cells/fill-cells order) with the side cells (feet and head height) nearest the nearest of threats ({:x :y :z})
  first; the roof support and the roof keep their order after them (a block needs a neighbour to be placed against).
  As given with no threat."
  [feet threats cells]
  (if (empty? threats)
    cells
    (let [side? (fn [{:keys [y]}] (<= (:y feet) y (inc (:y feet))))
          gap (fn [{:keys [x y z]}]
                (apply min (map #(u/dist {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)} %) threats)))]
      (concat (sort-by gap (filter side? cells)) (remove side? cells)))))

(defn melee-spots
  "Where the seen melee hostiles within the flight's :radius stand."
  [c]
  (let [p (:primitives c)]
    (->> (danger-q/known-hostiles p (:radius (:args c)) {})
         (filter #(and (not (combat/ranged? %)) (danger-q/seen-only? %)))
         (mapv #(danger-q/mob-pos p %)))))

(defn sealing-here?
  "Whether a seal was begun at the body's feet cell this flight and has not failed: it is finished before the walk goes on."
  [c]
  (and (= (:seal-at (ctx/mem c)) (sh/feet (:primitives c))) (not (flight/tried? c :seal))))

(defn ^:async seal!
  "Fill the open cells around the body (dig-in's 1x1: sides at feet and head
  height, a roof support, the roof) with carried :blocks, at most
  :max-places this step. :sealed when none is left open, :again while
  more are owed, :failed when a hostile stands in one (not with :purpose :shelter: the place is tried and a mob in its cell waited out), none is carried, a
  placement is refused, or only cells it cannot fill are left open (place-seal!'s :seal-occupied: a torch, chest or bed
  in a side cell, which the seal leaves alone; the option then counts as failed this flight, so a cornered body moves
  on to the next one instead of placing there step after step).
  A door standing open in a side cell is no wall (dig-cells/sealed?): it is shut. Cells it has not sensed (a ceiling
  out of view) are looked at first: an unseen cell is not taken for an open one."
  [c]
  (await (look/look-unknown! c (map lb/cell (dig-cells/open-cells (:primitives c) (sh/feet (:primitives c))))))
  (let [{:keys [radius max-places]} (:args c)
        blocks (flight/blocks c)
        p (:primitives c)
        open (dig-cells/open-cells p (sh/feet p))
        free (remove #(occupied? c %) open)
        cells (if (seq (shooters c)) (facing-first (sh/feet p) (shooters c) free) (nearest-first (sh/feet p) (melee-spots c) free))
        forget! #(dig-cells/forget-wait! c :seal-mob-since)
        unfillable (fn [] (forget!) (flight/tried! c :seal) :failed)]
    (cond
      (empty? open) (do (forget!) :sealed)
      (empty? cells) (unfillable)
      (and (not= :shelter (:purpose (:args c))) (some (seal/hostile-cells p radius) cells)) (do (forget!) :failed)
      (nil? (lb/pick c blocks)) (do (forget!) :failed)
      :else
      (do (when (seal/off-centre? p) (await (seal/centre! c)))
          (access/trespass! c "retreat" (some #(access/trespass-refusal (access/rules-input c) :place %) cells))
          (ctx/update-mem! c #(-> % (update :seal-cells (fnil into #{}) (map (juxt :x :y :z) (take max-places cells)))
                                  (assoc :seal-at (sh/feet p))))
          (case (await (place-seal! c (take max-places cells)))
            :failed (do (flight/tried! c :seal) :failed)
            :wait :again
            (let [left (dig-cells/open-cells p (sh/feet p))]
              (cond
                (empty? left) (do (forget!) :sealed)
                (every? #(occupied? c %) left) (unfillable)
                :else :again)))))))
