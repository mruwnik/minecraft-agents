(ns jobs.lib.seal
  "What a body sealing itself in shares: the cells a hostile's hitbox overlaps (no block goes there), centring the
  body in its own cell."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.blocks :as lb]
            [jobs.lib.click :as click]
            [jobs.lib.found :as found]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def mob-half-width
  "Half the width of a zombie-sized mob's hitbox."
  0.3)

(defn hitbox-cells
  "The feet and head cells a mob standing at pos overlaps (its hitbox is 0.6 wide, so one straddling a cell
  boundary is in both cells: the server refuses a block there)."
  [{:keys [x y z]}]
  (let [span (fn [v] (distinct [(js/Math.floor (- v mob-half-width)) (js/Math.floor (+ v mob-half-width))]))
        fy (js/Math.floor y)]
    (for [cx (span x) cz (span z) cy [fy (inc fy)]] {:x cx :y cy :z cz})))

(defn hostile-cells
  "The cells the hostiles within radius overlap: no block goes there."
  [p radius]
  (set (mapcat #(hitbox-cells (danger-q/mob-pos p %)) (danger-q/known-hostiles p radius {}))))

(defn off-centre?
  "Whether the body's hitbox (0.6 wide) reaches out of its cell into a side cell."
  [p]
  (let [{:keys [x z]} (u/self-pos {:primitives p})
        frac #(- % (js/Math.floor %))]
    (boolean (some #(or (< (frac %) 0.3) (> (frac %) 0.7)) [x z]))))

(defn ^:async centre!
  "Step to the middle of the body's own cell, so the side cells around it can take blocks."
  [c]
  (let [{:keys [x y z]} (sh-sense/feet (:primitives c))]
    ;; raw moveTo kept: a step inside the body's own cell, range 0.15; the planner has no goal finer than a cell.
    (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0.15}))))

(def mob-proof-shapes
  "Blocks that stop a mob although their collision shape does not fill the cell: fences (1.5 high), walls, panes,
  iron bars, gates, doors and trapdoors."
  #"(_fence|_fence_gate|_wall|_pane|_door|_trapdoor)$|^iron_bars$")

(defn open-openable
  "The block at cell when it is a door, gate or trapdoor (wooden, copper or iron) standing open, else nil."
  [p cell]
  (let [b (u/seen-block p cell)]
    (when (and b (#{:openable :iron} (click/kind-of (.-name b))) (click/reached? :open (click/props-of b)))
      b)))

(defn sealed?
  "Whether a cell already stops a mob: its block fills the cell (blockAt's :fullCube: stone, leaves, glass) or is a
  fence, wall, pane, bar, or a gate, door or trapdoor that is shut (one standing open is a way in). Signs, rails,
  plates, buttons, levers, carpets, torches, plants, slabs and stairs let a mob walk or step through (or leave a gap
  it fits through), so they are not sealed; nor is an unloaded cell."
  [p cell]
  (let [b (u/seen-block p cell)]
    (boolean (and b
                  (not (open-openable p cell))
                  (or (.-fullCube b) (re-find mob-proof-shapes (.-name b)))))))

(defn ^:async shut-open!
  "Shut the door, gate or trapdoor standing open at cell with one click of the hand (jobs.lib.click): :shut when
  it is shut now, :open when it stays open (an iron one, or a click that did nothing), nil when the cell holds none.
  A shelter fills such a cell by shutting it, never by placing into it or digging it."
  [c cell]
  (when-let [b (open-openable (:primitives c) cell)]
    (if (= :openable (click/kind-of (.-name b)))
      (let [r (await (click/click! c cell :closed (.-name b)))]
        (if (= :changed (:outcome r)) :shut :open))
      :open)))

(a/defargs settings
  {::mob-wait-ticks {:default 200 :doc "Game ticks a shelter's place refused for a mob in its cell is retried before the shelter gives up."
                     :spec (a/int-in 0 nil)}})

(defn mob-wait-ms [] (settings/ticks->ms (settings/get settings ::mob-wait-ticks)))

(def loose #"(^|_)(rail|torch|fence|fence_gate|ladder|lantern|button|lever|carpet|sign|banner|pressure_plate|sapling)$|^cobweb$")

(defn kind
  "What a block name means for a fill: :open (air, water or replaceable), :container (never touched), :loose (dug first with
  :clear-loose) or :solid (done)."
  [n]
  (cond
    (or (bs/air n) (= "water" n) (bs/replaceable n)) :open
    (found/containers n) :container
    (or (lb/clearable n) (and n (re-find loose n))) :loose
    :else :solid))
