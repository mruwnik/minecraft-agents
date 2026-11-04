(ns jobs.survival.wall-in
  (:require [jobs.lib.shelter.fill :as fill]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as lb]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.danger :as cost-danger]
            [jobs.lib.cost.health :as health]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.spawn-light :as spawn-light]
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
  (##Inf with no block carried; with fewer blocks than open cells, which would stop half built, a melee hostile known gets in by an open cell, a mob unseen spawns: their hp-cost over the stay (::job-time-s) added, half-built-s). With :for :night the quote counts night seconds only (placing), and :morning-s names the door dug out and the blocks left (plug-way/morning-s); it needs a carried block for every open cell. A cell another's zone refuses adds plug-way's ::trespass-s.
  Picked by jobs.survival.shut-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:ignore-zones? :shared
   :trespass? {:doc "false: walls that would need a cell of another's zone, claim or plan fail instead (the caller tries again with true as a last resort)" :spec boolean? :default true}
   :on-lava {:doc ":seal: lava in a cell to fill is filled like any; :stop: the walls are not built beside lava (it is left)" :spec #{:seal :stop} :default :seal}
   :for {:doc "why the body shuts itself in: :danger (blocks are taken back by the cleanup once no hostile is near; a hostile in a cell fails it), :night (blocks stay for the night; a mob in a cell is waited out)" :spec #{:danger :night} :default :danger}})

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds a walls attempt may take." :spec (a/num-in 0 nil)}
   ::mob-fails {:default {:reached {:k 4 :n 7} :clear {:k 0 :n 1}}
                :spec (a/map-of #{:reached :clear} (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)}))
                :doc "Of n walls attempts of a flight (closed, or failed for a mob in a cell), k failed for a mob, by the melee hostiles when it began: :reached, one could strike the body before the walls were closed (arrive-s), :clear none. Attempts that failed for another reason are left out of n, so k is a floor for :reached (they might have lost to the mob later). The trial bodies' logs on 2026-10-28. Recount: node tools/event-counts.mjs wall-places."}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(defn done? [c] (way/done? c :walls))

(defn why-not
  "The stop {:stop :no-blocks :text} when too few blocks are carried: none, or for the night fewer than open cells; for the night also {:stop :own-plan} when an open cell lies in a plan the body made; {:stop :lava-adjacent} with :on-lava :stop when a cell to fill holds lava; nil otherwise."
  [c]
  (let [cells (fill/open-cells (:primitives c) (sh-sense/feet (:primitives c)))
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

(declare half-built-s)

(def door-cells "Wall cells dug again to step out by day (jobs.lib.dig-in-leave/leave!: the door at the feet and the head)." 2)

(defn quote-of [c]
  (let [cells (fill/open-cells (:primitives c) (sh-sense/feet (:primitives c)))
        open (count cells)]
    (way/quote-of c (if (why-not c) ##Inf (+ (* open (way/place-s c)) (way/zone-penalty c {:place cells}) (half-built-s c open)))
                  (settings/get settings ::job-time-s)
                  {:morning-s (way/morning-s c (min open door-cells) (max 0 (- open door-cells)))})))

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
  "The chance a cover fails for a mob: cost/counted-chance of the counts {:beside {:k :n} :clear {:k :n}} of its class."
  [counts beside?]
  (cost/counted-chance (get counts (if beside? :beside :clear))))

(defn melee-hostiles
  "The hostiles the body knows within the hostile radius that walk in through an open cell: not a shooter, a creeper or an enderman."
  [c]
  (let [p (:primitives c)]
    (->> (danger-q/known-hostiles p (danger-q/hostile-radius) {})
         (remove #(or (combat/ranged? %) (combat/creeper? %) (combat/enderman? %))))))

(defn arrivals
  "[seconds mob] of each melee hostile the body knows (melee-hostiles): the seconds before it can strike the body (cost-danger/arrival-s)."
  [c]
  (let [p (:primitives c)
        at (u/pos-of (.-pos (.self p)))]
    (map #(vector (cost-danger/arrival-s (cost-danger/mob-kind %) (danger-q/mob-pos p %) at) %) (melee-hostiles c))))

(defn arrive-s
  "Seconds before the nearest melee hostile the body knows within the hostile radius (not a shooter, a creeper or an
  enderman) can strike it (cost-danger/arrival-s); nil with none."
  [c]
  (some->> (seq (arrivals c)) (map first) (reduce min)))

(defn open-count [c] (count (fill/open-cells (:primitives c) (sh-sense/feet (:primitives c)))))

(defn half-built-s
  "Seconds a wall left half built (fewer carried blocks than open cells, so a gap stays open for the whole stay,
  ::job-time-s) costs: the nearest melee hostile known that arrives (arrive-s) within the stay gets in by a cell left open,
  at the hp-cost of its blow (answer/blow, at the body's health and armour), and a mob unseen spawns at the chance a second
  at the feet by their light (spawn-light/chance-at; price/stay-hp); 0 with the blocks for every cell."
  [c open]
  (let [have (reduce + (map :count (lb/carried c (flight/blocks c))))
        p (:primitives c)
        self (.self p)
        health (let [h (.-health self)] (if (number? h) h 20))
        stats (cost/armour-stats (cost/equipment-of (.-equipment self)))
        stay (settings/get settings ::job-time-s)
        nearest (some->> (seq (filter #(< (first %) stay) (arrivals c))) (apply min-key first) second)
        known (if nearest (answer/blow stats {:name (.-name nearest)}) 0)
        unseen (price/stay-hp c (spawn-light/chance-at p (sh-sense/feet p) false) stay)]
    (if (>= have open)
      0
      (health/hp-cost health (+ known unseen)))))

(defn fails
  "The chance the walls cannot be closed for a mob in a cell: cost/counted-chance of ::mob-fails of the class the body
  stands in (:reached when a melee hostile can strike before every open cell is placed, arrive-s); 0 with :for :night (a
  mob in a cell is waited out)."
  [c]
  (if (= :night (:for (:args c)))
    0
    (let [s (arrive-s c)]
      (cost/counted-chance (get (settings/get settings ::mob-fails) (if (and s (< s (* (way/place-s c) (open-count c)))) :reached :clear))))))

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
