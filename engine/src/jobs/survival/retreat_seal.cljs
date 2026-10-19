(ns jobs.survival.retreat-seal
  "A cornered body's seal (jobs.survival.retreat): the open cells round it filled with carried blocks."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.click :as click]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.seal :as seal]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]))

(def max-refusals
  "Times the place primitive may refuse one cell before it is given up on."
  2)

(defn occupied?
  "Whether the seal leaves cell alone: a block it cannot clear stands in it, or the server refused it max-refusals times."
  [c cell]
  (let [m (ctx/mem c)]
    (or (contains? (:seal-occupied m) cell) (>= (get-in m [:seal-refused cell] 0) max-refusals))))

(defn melee-spots
  "Where the seen melee hostiles within the flight's :radius stand."
  [c]
  (let [p (:primitives c)]
    (->> (danger-q/known-hostiles p (:radius (:args c)) {})
         (filter #(and (not (combat/ranged? %)) (danger-q/seen-only? %)))
         (mapv #(danger-q/mob-pos p %)))))

(def hit-reach
  "Blocks within which a melee mob that hurt the body during a seal step ends the step (the fight is decided anew)."
  3)

(defn hit-while-placing?
  "Whether the body lost health since health0 with a seen melee hostile within hit-reach: more blocks beside it only cost more hits."
  [c health0]
  (let [p (:primitives c)]
    (boolean (and (< (.-health (.self p)) health0)
                  (some #(<= (u/dist (u/self-pos c) %) hit-reach) (melee-spots c))))))

(defn ^:async place-seal!
  "Place carried blocks at cells in order, each ledgered (flight/plug-purpose) for the cleanup. :hit once a melee mob hit the body during the step (hit-while-placing?: no more blocks beside it), :ok, :wait while a mob stands in a cell (the place is refused and tried again
  for dig-cells/mob-wait-ms), or :failed with nothing left to place. A cell with nothing to place against yet, or one the
  server refused, goes to the end of the order (its neighbours may be placed first); :failed once every cell left has none, a mob
  that stays past the wait, or a refused one max-refusals times. A door, gate or trapdoor standing open is shut instead (dig-cells/shut-open!). A cell
  the place answers occupied by a block a mob walks through (a torch, a sapling) is dug once and placed again; one that
  stays (a full block, a bed, an iron door left open, a dig that fails) is remembered in :seal-occupied and never tried again this flight."
  [c cells]
  (let [health0 (.-health (.self (:primitives c)))]
  (loop [cells cells skipped 0]
    (let [item (lb/pick c (flight/blocks c))
          cell (first cells)]
      (cond
        (empty? cells) :ok
        (hit-while-placing? c health0) :hit
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
                      (when (= "placed" status) (flight/note-placed! c cell item))
                      (recur (rest cells) 0))
                  (and (= "no-support" status) (< (inc skipped) (count cells)))
                  (recur (concat (rest cells) [cell]) (inc skipped))
                  (= :open door)
                  (do (ctx/update-mem! c update :seal-occupied (fnil conj #{}) cell)
                      (recur (rest cells) 0))
                  (= "occupied" status)
                  (if (await (dig-cells/clear-over! c cell :seal-cleared :seal-occupied))
                    (recur cells 0)
                    (recur (rest cells) 0))
                  (dig-cells/keep-waiting! c :seal-mob-since r) :wait
                  :else (let [support? (= "no-support" status)]
                          (dig-cells/forget-wait! c :seal-mob-since)
                          (ctx/update-mem! c assoc :seal-status status)
                          (ctx/update-mem! c assoc :seal-refusal (str "a place at " (pr-str [(:x cell) (:y cell) (:z cell)]) " was refused: " status (dig-cells/mob-text r)))
                          (when-not support? (ctx/update-mem! c update-in [:seal-refused cell] (fnil inc 0)))
                          (cond
                            (or support? (dig-cells/refusing-mobs r)) :failed
                            (occupied? c cell) (recur (rest cells) 0)
                            :else (recur (concat (rest cells) [cell]) 0))))))))))

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

(defn stuck-door?
  "Whether cell holds a door a hand cannot shut (iron) standing open: no wall, and nothing can be placed into it."
  [p cell]
  (let [b (dig-cells/open-openable p cell)]
    (and b (= :iron (click/kind-of (.-name b))))))

(defn door-free-spot
  "A side neighbour of feet to stand in that the walls can enclose, because no stuck-door? is among its fill cells, or nil: its feet and
  head cells are seen free, its floor is solid and it is no door cell."
  [p feet]
  (let [free? #(let [f (u/seen-facts p %)] (and f (not (:full-cube? f)) (not (dig-cells/open-openable p %))))]
    (first (for [[dx dz] dig-cells/sides
                 :let [n (assoc feet :x (+ (:x feet) dx) :z (+ (:z feet) dz))]
                 :when (and (free? n) (free? (update n :y inc))
                            (:full-cube? (u/seen-facts p (update n :y dec)))
                            (not-any? #(stuck-door? p %) (dig-cells/fill-cells (constantly false) n)))]
             n))))

(defn ^:async step-from-door!
  "Walls beside a stuck-door? cell cannot close: step once to a door-free-spot (go-to child :door-step) so the cell between becomes a wall.
  :again while the walk goes on or after it ends; nil when there is no door beside the body, no spot, or the step was tried."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)]
    (when (and (not (:door-stepped (ctx/mem c))) (some #(stuck-door? p %) (dig-cells/fill-cells (constantly false) feet)))
      (when-let [spot (door-free-spot p feet)]
        (let [r (await (ctx/call c :door-step 'jobs.movement.go-to {:pos {:x (+ (:x spot) 0.5) :y (:y spot) :z (+ (:z spot) 0.5)} :range 0}))]
          (when-not (= :continue r) (ctx/update-mem! c assoc :door-stepped true))
          :again)))))

(defn ^:async seal-cells!
  "The body of seal!: place the open cells round the feet."
  [c]
  (let [threats (into (shooters c) (melee-spots c))
        p (:primitives c)
        _ (await (look/look-unknown! c (map lb/cell (dig-cells/open-cells p (sh-sense/feet p) threats))))
        {:keys [radius max-places]} (:args c)
        blocks (flight/blocks c)
        open (dig-cells/open-cells p (sh-sense/feet p) threats)
        free (remove #(occupied? c %) open)
        cells (if (seq (shooters c)) (facing-first (sh-sense/feet p) (shooters c) free) (nearest-first (sh-sense/feet p) (melee-spots c) free))
        forget! #(dig-cells/forget-wait! c :seal-mob-since)
        unfillable (fn [] (forget!) :failed)]
    (cond
      (empty? open) (do (forget!) :sealed)
      (empty? cells) (unfillable)
      (and (not= :night (:for (:args c))) (some (seal/hostile-cells p radius) cells)) (do (forget!) :failed)
      (nil? (lb/pick c blocks)) (do (forget!) :failed)
      :else
      (do (when (seal/off-centre? p) (await (seal/centre! c)))
          (access/trespass! c "shut-in" (some #(access/trespass-refusal (access/rules-input c) :place %) cells))
          (case (await (place-seal! c (take max-places (remove #(and (not (some #{%} cells)) (access/trespass-refusal c :place %))
                                                                 (dig-cells/bottom-up p cells (sh-sense/feet p))))))
            :failed :failed
            :wait :again
            :hit :hit
            (let [left (dig-cells/open-cells p (sh-sense/feet p) threats)]
              (cond
                (empty? left) (do (forget!) :sealed)
                (every? #(occupied? c %) left) (unfillable)
                :else :again)))))))

(defn ^:async seal!
  "Fill the open cells around the body (dig-in's 1x1: sides at feet and head
  height, a roof support, the roof) with carried :blocks, at most
  :max-places this step. :sealed when none is left open, :again while
  more are owed, :hit when a melee mob hit the body during the step (the caller decides anew), :failed when a hostile stands in one (not with :for :night: the place is tried and a mob in its cell waited out), none is carried, a
  placement is refused, or only cells it cannot fill are left open (place-seal!'s :seal-occupied: a torch, chest or bed
  in a side cell, which the seal leaves alone).
  A door standing open in a side cell is no wall (dig-cells/sealed?): it is shut. Cells it has not sensed (a ceiling
  out of view) are looked at first: an unseen cell is not taken for an open one. A support block under a wall cell is never placed in another's
  area (the wall cell then fails no-support); the wall cells themselves are a last resort.
  An open iron door in a side cell can be neither shut nor filled: the body first steps to a spot not beside it (step-from-door!)."
  [c]
  (let [stepped (await (step-from-door! c))]
    (or stepped (await (seal-cells! c)))))
