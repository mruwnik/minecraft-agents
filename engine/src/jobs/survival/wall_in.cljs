(ns jobs.survival.wall-in
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.choose :as choose]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.plug-way :as way]
            [jobs.survival.wall-placer :as placer]
            [jobs.survival.respond.state :as flight]))

(def doc
  "A plug way: fill the open sides at feet and head height, a roof support and the roof round the body with carried blocks, the side
  towards a shooter, else the nearest melee hostile, first; a cell with nothing to place against waits for the cell it rests on, or gets the support chain under it (dig-in-cells/bottom-up). An open iron door in a side cell (no hand shuts it, nothing is placed into it) makes the body step once to a neighbour cell not beside it first. Needs carried blocks (the call's :use :blocks, else every :build filler).
  One call is a whole attempt: :done {:pos :way :walls} once enclosed, at once when it already is; nothing-possible :failed when a
  hostile stands in a cell (not with :for :night), no block is left or a place is refused. A place refused for a mob in the cell is waited out
  (jobs.lib.seal/mob-wait-ticks), then fails; one refused otherwise goes after the others (blocks.fill-cells: the walls are placed one cell a step through it, in place, ledgered; a torch in a cell is dug first, an open door shut) and fails with the other failures; a block that stays in a cell is left alone. Every block goes to the scaffold ledger with :for (default :danger, ledger :retreat-plug, taken
  back by jobs.access.cleanup once no hostile is near; :night: ledger :shelter, left alone). Quote: time, a placement per open cell
  (##Inf with no block carried; ::short-s more with fewer blocks than open cells). With :for :night the quote counts night seconds only (placing), and a block worth more than the ::wall-value setting
  (jobs.lib.cost/item-value) adds the dig back at dawn; it needs a carried block for every open cell. A cell another's zone refuses adds plug-way's ::trespass-s.
  Picked by jobs.survival.shut-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:ignore-zones? :shared
   :trespass? {:doc "false: walls that would need a cell of another's zone, claim or plan fail instead (the caller tries again with true as a last resort)" :spec boolean? :default true}
   :for {:doc "why the body shuts itself in: :danger (blocks are taken back by the cleanup once no hostile is near; a hostile in a cell fails it), :night (blocks stay for the night; a mob in a cell is waited out)" :spec #{:danger :night} :default :danger}})

(a/defargs settings
  {::wall-value {:default 3 :doc ":night: a block worth more than this each (jobs.lib.cost/item-value; dirt 0.35, cobblestone 0.85, stone 2.85, planks 3.4, log 12.8) is taken back at dawn, which the quote counts" :spec (a/num-in 0 nil)}
   ::job-time-s {:default 60 :doc "Seconds a walls attempt may take." :spec (a/num-in 0 nil)}
   ::short-s {:default 60 :doc "Seconds the quote adds when fewer blocks are carried than open cells: the walls would stop half built, so a way that can finish (a pit digs its own plug) is chosen first." :spec (a/num-in 0 nil)}
   ::dear-s {:default 0.4 :doc "Seconds a wall block worth more than ::wall-value adds per cell for the night: it is dug again at dawn." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :walls))

(defn cell-seconds
  "Seconds one wall cell is priced at: a placement, for the night only the placing plus the dawn dig of a block worth more than
  ::wall-value when the carried cheap ones do not cover every open cell."
  [c open]
  (if-not (= :night (:for (:args c)))
    (way/place-s c)
    (let [cheap (choose/cheap-blocks (flight/blocks c) (settings/get settings ::wall-value))
          have (reduce + (map :count (lb/carried c cheap)))]
      (+ (way/place-s c) (if (>= have open) 0 (settings/get settings ::dear-s))))))

(defn why-not
  "The stop {:stop :no-blocks :text} when too few blocks are carried: none, or for the night fewer than open cells; for the night also {:stop :own-plan} when an open cell lies in a plan the body made; nil otherwise."
  [c]
  (let [cells (dig-cells/open-cells (:primitives c) (sh-sense/feet (:primitives c)))
        open (count cells)
        have (reduce + (map :count (lb/carried c (flight/blocks c))))]
    (cond
      (placer/own-plan-cell c cells) {:stop :own-plan :text "the walls need a cell of the body's own plan"}
      (nil? (lb/pick c (flight/blocks c))) {:stop :no-blocks :text "no block to wall the body in with"}
      (and (= :night (:for (:args c))) (< have open)) {:stop :no-blocks :text (str "fewer blocks carried than the " open " open cells")})))

(defn can-start?
  "A block of the call's :use (else every :build filler) is carried, or the body is already enclosed (done? first)."
  [c]
  (if (nil? (lb/pick c (flight/blocks c)))
    (contract/later :no-blocks "no block to wall the body in with")
    (contract/ok :blocks "a block to wall with")))

(defn short-penalty
  "::short-s when fewer blocks are carried than open cells, else 0."
  [c open]
  (if (< (reduce + (map :count (lb/carried c (flight/blocks c)))) open) (settings/get settings ::short-s) 0))

(defn quote-of [c]
  (let [cells (dig-cells/open-cells (:primitives c) (sh-sense/feet (:primitives c)))
        open (count cells)]
    (way/quote-of c (if (why-not c) ##Inf (+ (* open (cell-seconds c open)) (way/zone-penalty c {:place cells}) (short-penalty c open)))
                  (settings/get settings ::job-time-s))))

(defn exposed-s
  "Seconds the body stays open to a shooter: the placements of the first side (feet and head), which go up first (wall-placer/facing-first)."
  [c]
  (* (way/place-s c) (min 2 (count (dig-cells/open-cells (:primitives c) (sh-sense/feet (:primitives c)))))))

(def safe? ready/ok)
(def fit? ready/ok)

(defn ^:async round [c] (await (way/run! c :walls way/walls-step!)))
