(ns jobs.movement.go-to.risk
  "go-to's start check: the risk of the walk to the goal, in hit points, against what the body can afford. It replaces any
  wait for dawn: a night walk is judged by the light the body perceives along the straight way (Q334), a walk underground
  only by the hostiles it can reach (Q279).
  Risk = the mobs expected to spawn on the exposed stretches (jobs.lib.cost/spawn-chance per second of walking, each costed
  as a zombie fight with the kit carried, jobs.lib.cost/fight-damage) plus, on a walk with such a stretch (in the light a hostile is seen and avoided; the reflexes meet it), the fight with every hostile known near the way.
  A walk to a goal within the death-spot radius of a place the body died lately (jobs.lib.death-spot, fading with time) adds the fight with what killed it, weighted by how fresh the death is, underground too, only when the walk ends nearer the death than it starts (a body standing, or walking away from the spot, is no closer to it) and the spot has not been seen since the death (fresh sight beats the record; a hostile seen there prices as a known hostile); not within ::pile-radius of the goal (a walk to the body's own drops: only a hostile known there counts).
  Seen lava within jobs.lib.lava-risk/radius of a cell the straight way steps into adds jobs.lib.lava-risk/hp, scaled by its closeness (a slip beside it is lethal); a body already at the goal, or a way clear of lava, adds none.
  Acceptable when it stays within jobs.lib.price/::risk-share of the health the body has now, or, when the caller says what the goal is worth (:worth), within the hp that worth buys (price/allowed)."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.danger :as danger]
            [jobs.lib.lava-risk :as lava-risk]
            [jobs.lib.look :as look]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::surface-band {:default 4 :spec (a/num-in 0 nil)
                   :doc "Blocks below the higher end of a walk that unseen ground still counts as open surface; deeper is underground, where only hostiles count."}
   ::exit-blocks {:default 8 :spec (a/num-in 0 nil)
                  :doc "Blocks of open ground a walk out of a night shelter crosses before it can go under, even to a goal underground."}
   ::pile-radius {:default 6 :spec (a/num-in 0 nil)
                  :doc "Blocks around the goal within which a remembered death or hide adds no risk: the body walking to the spot goes for its own things, and only a hostile known there counts."}
   ::way-radius {:default 16 :spec (a/num-in 0 nil)
                 :doc "Blocks from the straight way within which a known hostile adds its fight to the risk of the walk."}
   ::heal-s-per-hp {:default 4 :spec (a/num-in 0 nil)
                    :doc "Seconds a body that can heal takes to regain an hp, for the wait that would make a risky walk affordable (stay-estimate)."}
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

(def lava-scan-radius 128)

(defn route-lava-hp
  "The most lava-hp of any cell the straight way from feet to goal steps into (the cell the body stands in is no step): lava the walk keeps clear of, or a body already at its goal, costs nothing. Reads the seen lava once, within the way's length (at most lava-scan-radius) of the body."
  [p feet goal step]
  (let [start (update-vals feet js/Math.floor)
        cells (remove #(= % start) (cells-along feet goal step))
        lavas (when (seq cells)
                (mapv (comp (juxt :x :y :z) :pos)
                      (look/seen-blocks p {:names ["lava"] :radius (min lava-scan-radius (+ (u/dist feet goal) (lava-risk/radius))) :max 256})))]
    (reduce max 0 (for [cell cells] (lava-risk/hp-from lavas [(:x cell) (:y cell) (:z cell)])))))

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
        away-from-goal (fn [records] (filterv #(> (u/dist goal (:pos %)) (settings/get settings ::pile-radius)) records))
        nearer-goal? (fn [records] (filterv #(< (u/dist goal (:pos %)) (u/dist feet (:pos %))) records))
        seen-clear? (fn [{:keys [pos age-ms]}] (let [b (look/seen-block p pos)] (and b (not (:unknown b)) (< (:age-ms b) age-ms))))
        deaths (remove seen-clear? (nearer-goal? (away-from-goal (death-spot/near (death-spot/recent c) goal))))
        death-hp (reduce + (for [{:keys [cause weight]} deaths] (* weight (hit [{:name cause :distance 0 :hits 0}]))))
        hide-hp (reduce + (for [{:keys [mobs]} (nearer-goal? (away-from-goal (hide-spot/near (hide-spot/recent c) goal)))] (hit (mapv (fn [n] {:name n :distance 0 :hits 0}) mobs))))
        lava (route-lava-hp p feet goal step)
        spawn-hp (* spawns (hit [{:name "zombie" :distance 0 :hits 0}]))
        hostile-hp (if (pos? exposed-s) (hit (mapv (fn [e] {:name (.-name e) :distance (danger/mob-distance p e) :hits 0}) hostiles)) 0)]
    {:hp (+ spawn-hp hostile-hp death-hp hide-hp lava) :lava-hp lava :hide-hp hide-hp :spawn-hp spawn-hp :hostile-hp hostile-hp :death-hp death-hp :dark-s exposed-s :hostiles (count hostiles)}))

(defn stay-estimate
  "hp the body expects to lose staying at its cell instead of walking `route-hp` to `goal`: the fight with every hostile known within ::way-radius
  of the body (those that reach it where it waits; one near the far end of the way does not) plus the spawns met at its cell by the light seen there (price/stay-hp), for the
  time the walk takes, or, for a body that can heal, for the wait until its health would afford the route (::heal-s-per-hp an hp)."
  [c goal route-hp]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        health (.-health (.self p))
        heal-s (if (price/cannot-heal? c) 0 (* (settings/get settings ::heal-s-per-hp) (max 0 (- (min 20 (/ route-hp (settings/get price/settings ::price/risk-share))) health))))
        secs (max heal-s (/ (u/dist feet goal) (cost/walk-blocks-per-s)))
        hostiles (danger/hostiles-near-way p feet (settings/get settings ::way-radius) {})
        hit (fn [mobs] (if (seq mobs) (cost/fight-damage (assoc (wworld/kit-of c) :mobs mobs)) 0))
        chance (chance-at p (update-vals feet js/Math.floor) (:y feet) false)]
    {:hp (+ (hit (mapv (fn [e] {:name (.-name e) :distance (danger/mob-distance p e) :hits 0}) hostiles))
            (price/stay-hp c chance secs))
     :hostiles (count hostiles)}))

(defn status
  "Ok while the estimated damage of the walk to `goal` is within (price/allowed c) or within what staying put would cost (a risk
  staying does not avoid is no reason to wait). Over both: later :risky with the risk and its parts, or never :stranded when the body
  cannot heal, so waiting only repeats the same exposure and the caller must seal in or go to a nearer refuge."
  [c goal]
  (let [{:keys [hp spawn-hp hostile-hp death-hp hide-hp lava-hp dark-s hostiles]} (check-cache/cached c [::risk goal (sense/round-looked c)] #(estimate c goal))
        allowed (price/allowed c)]
    (if (<= hp allowed)
      (contract/ok :acceptable (str "route risk " (price/round1 hp) " hp, within " (price/round1 allowed)))
      (let [stay (check-cache/cached c [::stay goal (sense/round-looked c)] #(stay-estimate c goal hp))
            over (str "route risk " (price/round1 hp) " hp over the " (price/round1 allowed) " the body can afford: "
                      (price/round1 dark-s) " s on dark open ground costs " (price/round1 spawn-hp) " hp, "
                      hostiles " hostile(s) near the way " (price/round1 hostile-hp) " hp, " (price/round1 death-hp) " hp for a recent death near it, " (price/round1 hide-hp) " hp for a recent hide near it, " (price/round1 lava-hp) " hp for seen lava near the way")]
        (cond
          (<= hp (:hp stay)) (contract/ok :cheaper-than-staying (str "route risk " (price/round1 hp) " hp, over the " (price/round1 allowed) " affordable but staying here costs " (price/round1 (:hp stay)) " hp (" (:hostiles stay) " hostile(s) near the body)"))
          (and (pos? (:hp stay)) (price/cannot-heal? c)) (contract/never :stranded (str over "; staying here costs " (price/round1 (:hp stay)) " hp and the body cannot heal, so waiting does not help: seal in or go to a nearer refuge") :risk hp :allowed allowed :stay-hp (:hp stay))
          :else (contract/later :risky over :risk hp :allowed allowed :recheck-after-ms 10000))))))
