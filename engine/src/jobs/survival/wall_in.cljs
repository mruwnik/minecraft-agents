(ns jobs.survival.wall-in
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.choose :as choose]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.danger :as cost-danger]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
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
   :on-lava {:doc ":seal: lava in a cell to fill is filled like any; :stop: the walls are not built beside lava (it is left)" :spec #{:seal :stop} :default :seal}
   :for {:doc "why the body shuts itself in: :danger (blocks are taken back by the cleanup once no hostile is near; a hostile in a cell fails it), :night (blocks stay for the night; a mob in a cell is waited out)" :spec #{:danger :night} :default :danger}})

(a/defargs settings
  {::wall-value {:default 3 :doc ":night: a block worth more than this each (jobs.lib.cost/item-value; dirt 0.35, cobblestone 0.85, stone 2.85, planks 3.4, log 12.8) is taken back at dawn, which the quote counts" :spec (a/num-in 0 nil)}
   ::job-time-s {:default 60 :doc "Seconds a walls attempt may take." :spec (a/num-in 0 nil)}
   ::short-s {:default 60 :doc "Seconds the quote adds when fewer blocks are carried than open cells: the walls would stop half built, so a way that can finish (a pit digs its own plug) is chosen first." :spec (a/num-in 0 nil)}
   ::dear-s {:default 0.4 :doc "Seconds a wall block worth more than ::wall-value adds per cell for the night: it is dug again at dawn." :spec (a/num-in 0 nil)}
   ::mob-fails {:default {:reached {:k 4 :n 7} :clear {:k 0 :n 1}}
                :spec (a/map-of #{:reached :clear} (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)}))
                :doc "Of n walls attempts of a flight (closed, or failed for a mob in a cell), k failed for a mob, by the melee hostiles when it began: :reached, one could strike the body before the walls were closed (arrive-s), :clear none. Attempts that failed for another reason are left out of n, so k is a floor for :reached (they might have lost to the mob later). The trial bodies' logs on 2026-10-28. Recount: node tools/event-counts.mjs wall-places."}})

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
  "The stop {:stop :no-blocks :text} when too few blocks are carried: none, or for the night fewer than open cells; for the night also {:stop :own-plan} when an open cell lies in a plan the body made; {:stop :lava-adjacent} with :on-lava :stop when a cell to fill holds lava; nil otherwise."
  [c]
  (let [cells (dig-cells/open-cells (:primitives c) (sh-sense/feet (:primitives c)))
        open (count cells)
        have (reduce + (map :count (lb/carried c (flight/blocks c))))]
    (cond
      (and (= :stop (:on-lava (:args c))) (some #(some->> (u/seen-name (:primitives c) %) (re-find #"lava")) cells))
      {:stop :lava-adjacent :text "lava in a cell the walls would fill"}
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

(defn beside?
  "Whether a melee hostile the body knows (not a shooter, a creeper or an enderman) stands within across cells and 1 up or
  down of the body's feet."
  [c across]
  (let [p (:primitives c)
        {fx :x fy :y fz :z} (sh-sense/feet p)]
    (boolean (some (fn [e] (let [{:keys [x y z]} (danger-q/mob-pos p e)]
                             (and (not (combat/ranged? e)) (not (combat/creeper? e)) (not (combat/enderman? e))
                                  (<= (js/Math.abs (- (js/Math.floor x) fx)) across) (<= (js/Math.abs (- (js/Math.floor z) fz)) across)
                                  (<= (js/Math.abs (- (js/Math.floor y) fy)) 1))))
                   (danger-q/known-hostiles p 5 {})))))

(defn mob-fails
  "The chance a cover fails for a mob: answer/counted-chance of the counts {:beside {:k :n} :clear {:k :n}} of its class."
  [counts beside?]
  (answer/counted-chance (get counts (if beside? :beside :clear))))

(defn arrive-s
  "Seconds before the nearest melee hostile the body knows within the hostile radius (not a shooter, a creeper or an
  enderman) can strike it (cost-danger/arrival-s); nil with none."
  [c]
  (let [p (:primitives c)
        at (u/pos-of (.-pos (.self p)))]
    (some->> (danger-q/known-hostiles p (danger-q/hostile-radius) {})
             (remove #(or (combat/ranged? %) (combat/creeper? %) (combat/enderman? %)))
             (map #(cost-danger/arrival-s (cost-danger/mob-kind %) (danger-q/mob-pos p %) at))
             seq
             (reduce min))))

(defn open-count [c] (count (dig-cells/open-cells (:primitives c) (sh-sense/feet (:primitives c)))))

(defn fails
  "The chance the walls cannot be closed for a mob in a cell: answer/counted-chance of ::mob-fails of the class the body
  stands in (:reached when a melee hostile can strike before every open cell is placed, arrive-s); 0 with :for :night (a
  mob in a cell is waited out)."
  [c]
  (if (= :night (:for (:args c)))
    0
    (let [s (arrive-s c)]
      (answer/counted-chance (get (settings/get settings ::mob-fails) (if (and s (< s (* (way/place-s c) (open-count c)))) :reached :clear))))))

(defn exposed-s
  "Seconds the body stays open: to a melee hostile, every open cell's placement (it walks to the side still open); to a
  shooter only, the first side's (feet and head), which goes up first (wall-placer/facing-first)."
  [c]
  (let [open (open-count c)]
    (* (way/place-s c) (if (arrive-s c) open (min 2 open)))))

(def safe? ready/ok)
(def fit? ready/ok)

(defn ^:async round [c]
  (when-not (:open-said (ctx/mem c))
    (ctx/update-mem! c assoc :open-said true)
    (let [open (open-count c)]
      (ctx/emit! c :walls.open :info {:open open :text (str "walls over " open " open cells")})))
  (await (way/run! c :walls way/walls-step!)))
