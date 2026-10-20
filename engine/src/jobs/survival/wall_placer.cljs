(ns jobs.survival.wall-placer
  "A cornered body's seal (jobs.survival.respond-flight): the open cells round it filled with carried blocks."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.click :as click]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as look]
            [jobs.lib.seal :as seal]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.respond.state :as flight]))

(defn melee-spots
  "Where the seen melee hostiles within the flight's :radius stand."
  [c]
  (let [p (:primitives c)]
    (->> (danger-q/known-hostiles p (danger-q/hostile-radius) {})
         (filter #(and (not (combat/ranged? %)) (danger-q/seen-only? %)))
         (mapv #(danger-q/mob-pos p %)))))

(a/defargs settings
  {::max-places {:default 4 :doc "Seal placements a step of a flight makes (also the shut-in ways')." :spec (a/int-in 1 nil)}
   ::max-refusals {:default 1 :doc "Failed steps (a place refused twice) at one cell before the seal gives up on it." :spec (a/int-in 1 nil)}
   ::hit-reach {:default 3 :doc "Blocks within which a melee mob that hurt the body during a seal step ends the step (the fight is decided anew)." :spec (a/num-in 0 nil)}})

(defn occupied?
  "Whether the seal leaves cell alone: a block it cannot clear stands in it."
  [c cell]
  (contains? (:seal-occupied (ctx/mem c)) cell))

(defn hit-reach [] (settings/get settings ::hit-reach))

(defn hit-since?
  "Whether the body lost health since the last fill! step ended (health0) with a seen melee hostile within hit-reach: more blocks beside it only cost more hits."
  [c health0]
  (let [p (:primitives c)]
    (boolean (and health0
                  (< (.-health (.self p)) health0)
                  (some #(<= (u/dist (u/self-pos c) %) (hit-reach)) (melee-spots c))))))

(defn leftover
  "Of cells (as given to fill!), those still open that the fill does not owe: a block it cannot clear or shut (a bed) holds them."
  [c cells left]
  (let [owed (set left)]
    (->> cells
         (remove #(or (seal/sealed? (:primitives c) %) (owed %)))
         (into #{}))))

(defn ^:async fill-one!
  "Place the first of cells (in the order given; sides, support, roof) that is still open through a jobs.blocks.fill-cells child
  (in place, ledgered flight/plug-purpose, an open door shut, a torch dug out first): :ok once a cell is filled or none is owed
  (a cell the fill cannot fill goes to :seal-occupied), :wait while a mob stands in the cell (job memory :seal-wait), :hit when a
  melee mob hit the body since the last step, :failed with job memory :seal-refusal and :seal-status (no block carried, a refused
  place, a zone)."
  [c cells]
  (let [p (:primitives c)
        health (.-health (.self p))]
    (if (hit-since? c (:seal-health (ctx/mem c)))
      (do (ctx/update-mem! c assoc :seal-health health) :hit)
      (let [fillable (remove #(and (access/trespass-refusal c :dig %) (= :loose (seal/kind (u/seen-name p %)))) cells)
            e (await (ctx/call c :fill 'jobs.blocks.fill-cells
                               {:cells (mapv lb/cell fillable) :order :given :least 1 :clear-loose true :shut-open true :in-place true :danger? true
                                :purpose (flight/plug-purpose c) :ignore-zones? true :use {:blocks (vec (flight/blocks c))} :ways #{:carried}
                                :checks {:safe? '(always) :fit? '(always)} :settings {:jobs.blocks.fill-cells/max-refusals (settings/get settings ::max-refusals)}}))
            _ (ctx/update-mem! c assoc :seal-health health)
            data (:data e)]
        (ctx/update-mem! c dissoc :seal-wait)
        (cond
          (= :continue e) (do (ctx/update-mem! c assoc :seal-wait true) :wait)
          (= :done (:kind e)) (do (doseq [{:keys [cell item]} (:placed data)]
                                    (spent/note! c item cell)
                                    (flight/note-placed! c (u/cell-pos cell) item))
                                  (ctx/update-mem! c update :seal-occupied (fnil into #{}) (leftover c cells (:left data)))
                                  (if (seq (:placed data)) :placed :ok))
          (= :blocked (get-in e [:cause :reason]))
          (do (ctx/update-mem! c update :seal-occupied (fnil conj #{}) (get-in e [:cause :cell]))
              :ok)
          :else (do (ctx/update-mem! c assoc :seal-status (:status data) :seal-refusal (:text data))
                    :failed))))))

(defn max-places "Seal placements a step." [] (settings/get settings ::max-places))

(defn ^:async fill!
  "fill-one! up to max-places times in a step while it places a block and a cell is left open: the walls go up together, as
  enclosed? is judged between steps; a body that was moved meanwhile ends the step (the cells are judged anew). :ok, :wait, :hit or :failed as fill-one!."
  [c cells]
  (loop [n 1]
    (let [p (:primitives c)
          feet (sh-sense/feet p)
          r (await (fill-one! c cells))]
      (if (and (= :placed r) (< n (max-places)) (= feet (sh-sense/feet p)) (not-every? #(seal/sealed? p %) cells))
        (recur (inc n))
        (if (= :placed r) :ok r)))))

(defn shooters
  "Where the seen ranged hostiles within the flight's :ranged-radius stand, and the unseen shooters of recent shots (flight/fire-spots)."
  [c]
  (let [radius (danger-q/hostile-radius)
        ranged-radius (danger-q/ranged-radius)
        p (:primitives c)]
    (into (->> (danger-q/known-hostiles p radius {:ranged-radius ranged-radius})
               (filter #(and (combat/ranged? %) (danger-q/seen-only? %)))
               (mapv #(danger-q/mob-pos p %)))
          (flight/fire-spots c))))

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
  (let [b (seal/open-openable p cell)]
    (and b (= :iron (click/kind-of (.-name b))))))

(defn door-free-spot
  "A side neighbour of feet to stand in that the walls can enclose, because no stuck-door? is among its fill cells, or nil: its feet and
  head cells are seen free, its floor is solid and it is no door cell."
  [p feet]
  (let [free? #(let [f (u/seen-facts p %)] (and f (not (:full-cube? f)) (not (seal/open-openable p %))))]
    (first (for [[dx dz] dig-cells/sides
                 :let [n (assoc feet :x (+ (:x feet) dx) :z (+ (:z feet) dz))]
                 :when (and (free? n) (free? (update n :y inc))
                            (:full-cube? (u/seen-facts p (update n :y dec)))
                            (not-any? #(stuck-door? p %) (dig-cells/fill-cells (constantly false) n nil (constantly false))))]
             n))))

(defn ^:async step-from-door!
  "Walls beside a stuck-door? cell cannot close: step once to a door-free-spot (go-to child :door-step) so the cell between becomes a wall.
  :again while the walk goes on or after it ends; nil when there is no door beside the body, no spot, or the step was tried."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)]
    (when (and (not (:door-stepped (ctx/mem c))) (some #(stuck-door? p %) (dig-cells/fill-cells (constantly false) feet nil (constantly false))))
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
        radius (danger-q/hostile-radius)
        blocks (flight/blocks c)
        open (dig-cells/open-cells p (sh-sense/feet p) threats)
        free (remove #(occupied? c %) open)
        cells (if (seq (shooters c)) (facing-first (sh-sense/feet p) (shooters c) free) (nearest-first (sh-sense/feet p) (melee-spots c) free))
        ]
    (cond
      (empty? open) :sealed
      (empty? cells) :failed
      (and (not= :night (:for (:args c))) (some (seal/hostile-cells p radius) cells)) :failed
      (and (false? (:trespass? (:args c))) (some #(access/trespass-refusal (access/rules-input c) :place %) cells))
      (do (ctx/update-mem! c assoc :stop {:reason :trespass :text "the walls need a cell of another's area"}) :failed)
      (nil? (lb/pick c blocks)) :failed
      :else
      (do (when (seal/off-centre? p) (await (seal/centre! c)))
          (access/trespass! c "shut-in" (some #(access/trespass-refusal (access/rules-input c) :place %) cells))
          (case (await (fill! c (remove #(and (not (some #{%} cells)) (access/trespass-refusal c :place %))
                                         (dig-cells/bottom-up p cells (sh-sense/feet p)))))
            :failed :failed
            :wait :again
            :hit :hit
            (let [left (dig-cells/open-cells p (sh-sense/feet p) threats)]
              (cond
                (empty? left) :sealed
                (every? #(occupied? c %) left) :failed
                :else :again)))))))

(defn ^:async seal!
  "Fill the open cells around the body (dig-in's 1x1: sides at feet and head
  height, a roof support, the roof) with carried :blocks, at most
  :max-places this step. :sealed when none is left open, :again while
  more are owed, :hit when a melee mob hit the body during the step (the caller decides anew), :failed when a hostile stands in one (not with :for :night: the place is tried and a mob in its cell waited out), none is carried, a
  placement is refused, or only cells it cannot fill are left open (place-seal!'s :seal-occupied: a torch, chest or bed
  in a side cell, which the seal leaves alone).
  A door standing open in a side cell is no wall (seal/sealed?): it is shut. Cells it has not sensed (a ceiling
  out of view) are looked at first: an unseen cell is not taken for an open one. A support block under a wall cell is never placed in another's
  area (the wall cell then fails no-support); the wall cells themselves are a last resort.
  An open iron door in a side cell can be neither shut nor filled: the body first steps to a spot not beside it (step-from-door!)."
  [c]
  (let [stepped (await (step-from-door! c))]
    (or stepped (await (seal-cells! c)))))
