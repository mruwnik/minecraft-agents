(ns jobs.movement.go-to.risk
  "go-to's start check: the risk of the walk to the goal, in hit points, against what the body can afford. It replaces any
  wait for dawn: a night walk is judged by the light the body perceives along the straight way (Q334), a walk underground
  only by the hostiles it can reach (Q279).
  Risk = the mobs expected to spawn on the exposed stretches (jobs.lib.cost/spawn-chance per second of walking, each costed
  as a zombie fight with the kit carried, jobs.lib.cost/fight-damage) plus, on a walk with such a stretch (in the light a hostile is seen and avoided; the reflexes meet it), the fight with every hostile known near the way.
  A walk to a goal within the death-spot radius of a place the body died lately (jobs.lib.death-spot, fading with time) adds the fight with what killed it, weighted by how fresh the death is, underground too.
  Acceptable when it stays within ::risk-share of the health the body has now."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.danger :as danger]
            [jobs.lib.look :as look]
            [jobs.lib.ready :as ready]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage of a walk (spawns met on its dark open stretches, hostiles near its way) may take; above it go-to's start check waits."}
   ::surface-band {:default 4 :spec (a/num-in 0 nil)
                   :doc "Blocks below the higher end of a walk that unseen ground still counts as open surface; deeper is underground, where only hostiles count."}
   ::exit-blocks {:default 8 :spec (a/num-in 0 nil)
                  :doc "Blocks of open ground a walk out of a night shelter crosses before it can go under, even to a goal underground."}
   ::way-radius {:default 16 :spec (a/num-in 0 nil)
                 :doc "Blocks from the straight way within which a known hostile adds its fight to the risk of the walk."}
   ::hungry-food {:default 6 :spec (a/int-in 0 20)
                  :doc "Food at or under which the body cannot sprint away and does not heal: the damage it can afford is cut by ::hungry-share."}
   ::hungry-share {:default 0.5 :spec (a/num-in 0 1)
                   :doc "Share of the affordable damage a body at ::hungry-food or under may risk."}
   ::step {:default 4 :spec (a/num-in 1 nil)
           :doc "Blocks between the cells of the way whose light is judged; a way longer than ::max-cells steps is judged at that many cells, spaced wider."}
   ::max-cells {:default 256 :spec (a/int-in 1 nil)
                :doc "Most cells of the way whose light is judged, so a far goal costs a bounded number of reads."}})

(defn- cells-along
  "The cells {:x :y :z} every step blocks along the straight way from `from` to `to`, floored, the goal last."
  [from to step]
  (let [len (u/dist from to)
        n (max 1 (js/Math.ceil (/ len step)))]
    (for [i (range 1 (inc n))
          :let [t (/ i n)]]
      (update-vals (merge-with #(+ %1 (* t (- %2 %1))) from to) js/Math.floor))))

(defn- chance-at
  "The spawn chance per second at `cell` for a body at feet: a seen cell by its light (sky reaching it down its column only), an unseen one
  by the open-sky guess when it lies within the surface band of `top`; underground-unseen cells carry none."
  [p cell top underground?]
  (let [night (not (look/lit-light? 0 15 (sh-sense/sky-subtract-now p)))
        sky (if night 0 15)
        cell (if (sh-sense/solid-at? p cell) (update cell :y inc) cell)
        block (when (or (sh-sense/seen-cell? p cell) (look/glimpse? p cell)) (sh-sense/block-light-at p cell))]
    (cond
      block (cost/spawn-chance [(:x cell) (:y cell) (:z cell)]
                               {:light (constantly {:block block :sky (if (sh-sense/sky-reaches-from-above? p cell) sky 15)})})
      (or underground? (< (:y cell) (- top (settings/get settings ::surface-band)))) 0
      :else (cost/spawn-chance [(:x cell) (:y cell) (:z cell)] {:light (constantly {:block 0 :sky sky})}))))

(defn estimate
  "{:hp :spawn-hp :hostile-hp :dark-s :hostiles} of the walk of c's body to `goal` ({:x :y :z}): hp the expected damage, dark-s
  the seconds walked where a mob could spawn."
  [c goal]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        goal (update goal :y #(or % (:y feet)))
        top (max (:y feet) (:y goal))
        underground? (and (sh-sense/buried? p)
                          (or (sh-sense/buried-at? p goal) (ready/hidden-below? p goal)))
        leaves? (sh-inside/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) goal 1)
        step (max (settings/get settings ::step) (/ (u/dist feet goal) (settings/get settings ::max-cells)))
        dt (/ step (cost/walk-blocks-per-s))
        exit (if leaves? (settings/get settings ::exit-blocks) 0)
        exposed-s (+ (/ exit (cost/walk-blocks-per-s))
                     (reduce + (for [cell (cells-along feet goal step)
                                     :let [ch (chance-at p cell top underground?)]
                                     :when (pos? ch)]
                                 dt)))
        spawns (* exposed-s (cost/spawn-chance [0 0 0]))
        kit (wworld/kit-of c)
        hit (fn [mobs] (if (seq mobs) (cost/fight-damage (assoc kit :mobs mobs)) 0))
        hostiles (danger/hostiles-near-way p goal (settings/get settings ::way-radius) {})
        deaths (death-spot/near (death-spot/recent c) goal)
        death-hp (reduce + (for [{:keys [cause weight]} deaths] (* weight (hit [{:name cause :distance 0 :hits 0}]))))
        hide-hp (reduce + (for [{:keys [mobs]} (hide-spot/near (hide-spot/recent c) goal)] (hit (mapv (fn [n] {:name n :distance 0 :hits 0}) mobs))))
        spawn-hp (* spawns (hit [{:name "zombie" :distance 0 :hits 0}]))
        hostile-hp (if (pos? exposed-s) (hit (mapv (fn [e] {:name (.-name e) :distance (danger/mob-distance p e) :hits 0}) hostiles)) 0)]
    {:hp (+ spawn-hp hostile-hp death-hp hide-hp) :hide-hp hide-hp :spawn-hp spawn-hp :hostile-hp hostile-hp :death-hp death-hp :dark-s exposed-s :hostiles (count hostiles)}))

(defn- round1 [x] (/ (js/Math.round (* 10 x)) 10))

(defn status
  "Ok while the estimated damage of the walk to `goal` is within ::risk-share of the body's health, else later :risky with the
  risk and its parts."
  [c goal]
  (let [p (:primitives c)
        {:keys [hp spawn-hp hostile-hp death-hp hide-hp dark-s hostiles]} (check-cache/cached c [::risk goal (sense/round-looked c)] #(estimate c goal))
        self (.self p)
        hungry? (some-> (.-food self) (<= (settings/get settings ::hungry-food)))
        allowed (* (settings/get settings ::risk-share) (.-health self) (if hungry? (settings/get settings ::hungry-share) 1))]
    (if (<= hp allowed)
      (contract/ok :acceptable (str "route risk " (round1 hp) " hp, within " (round1 allowed)))
      (contract/later :risky (str "route risk " (round1 hp) " hp over the " (round1 allowed) " the body can afford: "
                                  (round1 dark-s) " s on dark open ground costs " (round1 spawn-hp) " hp, "
                                  hostiles " hostile(s) near the way " (round1 hostile-hp) " hp, " (round1 death-hp) " hp for a recent death near it, " (round1 hide-hp) " hp for a recent hide near it")
                      :risk hp :allowed allowed :recheck-after-ms 10000))))
