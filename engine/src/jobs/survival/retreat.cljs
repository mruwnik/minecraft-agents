(ns jobs.survival.retreat
  (:require [jobs.lib.click :as click]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-refuge :as refuge]
            [jobs.survival.retreat-walk :as walk]))

(def doc
  "Flee from the hostiles in range, one whole flight per round: run until no mob chases the body any more.
  A real danger (jobs.lib.danger: a mob with a walkable way to the body, or a ranged one with a line of fire) within
  :radius (ranged ones :ranged-radius) starts a chase, or is seen afresh. A mob stops chasing (vanilla) once it is
  gone (dead, despawned, untracked), beyond its follow range (jobs.lib.threats: zombie 35, most 16), out of line of
  sight for :lost-s, or has no walkable way to the body (nor, ranged, a line of fire).
  Each step, in this order:
  1. A door, gate or trapdoor standing open within a hand's reach and nearer the nearest chaser than the body is shut
     with one click (retreat.door-shut info). Each door is clicked at most once a flight.
  2. Walks a short step (:step blocks) away from all chasers (nearer ones weigh more) with a go-to child (:escalate false),
     leaning toward the latest :bed or :home when it is within :home-range and not through the hostiles, avoiding :hazard positions.
     When a wall blocks the way away it turns up to 120 degrees towards open ground (at least 2 clear cells).
     Eats one bite a step (up to food 20) when the nearest chaser is at least :eat-gap blocks away and food is carried.
  3. Cornered (no open direction worth a walk, or the walk is blocked; checked afresh every step) with no hostile within
     :radius: holds a second (wait, why cornered) and looks again. With one within :radius: takes the safest option it has not yet failed.
     - fight (jobs.survival.fight-back) only when jobs.lib.cost/fight-damage leaves :reserve health. Never against a creeper.
     - seal in: fill the open sides at feet and head height and the roof (dig-in's 1x1 cells) with carried :blocks,
       at most :max-places a step. It first steps to the middle of its cell, and does not place while a hostile's hitbox
       overlaps a cell to fill. An open door is shut, not filled. A cell that answers occupied (torch, chest, bed) is left alone.
       When only such cells stay open the seal has failed.
     - pillar up 3 (jobs.access.pillar, the carried block with the most, at least 3). Needs a solid floor and free cells above.
       Not against a ranged mob. Every block goes to the scaffold ledger (purpose :pillar) for jobs.access.cleanup.
     - back off: a step of up to 2 cells in any of 8 directions that gains at least a block on the hostile.
     - dig down and plug: dig-in's pit, then a carried or dug block over the head (ledger purpose :retreat-plug).
       The pit is 2 deep under a solid side, else 3.
       Every cell must be solid, harvestable with what is carried, with no fluid beside and solid under it.
     - side pocket: with no block to seal with, dig a pocket beside the body (feet and head cell, solid on every other side,
       harvestable with what is carried), step in and seal the way in with the dug blocks (the seal option again).
     - last of all, fight with the best weapon or tool (pickaxe, shovel, hoe) or the fist.
     Order: fight if it wins, then seal, pillar (not against a ranged mob), back off, pit, pocket, fight.
     Against a creeper back off comes first.
     An option that fails is not tried again until all have failed. Then, after a second's hold, all are tried again
     (one retreat_blocked warning per flight).
  Sealed in, up a pillar or down a pit it hides (one retreat_sealed warning; a declared hold, wait why hiding) while a
  hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius) would have a walkable way to
  the refuge if its own blocks were gone, and for :quiet-s more after the last such danger (a silent mob is forgotten
  after a few seconds); then the flight ends :hidden.
  A chase has no time limit. With no new best gap to the nearest chaser in :no-gain-steps steps it takes the
  cornered options instead of walking on; stopped :cannot_escape only once every option failed in two sweeps in a row
  with no gain between.
  Returns done {:fled [ids] :ended :gone|:far|:lost|:closed|:hidden|:none}, or stopped :cannot_escape.
  Memory: one :threat entry per mob fled (jobs.lib.threats); the third from one mob within 5 min warns hostile.chased.
  A cell in another's zone is used only as a last resort (retreat.trespass-last-resort warning).")

(def args
  {:radius {:doc "hostiles within this many blocks start a flight" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks start a flight" :default 16}
   :eat-gap {:doc "with at least this many blocks to the nearest hostile, eat a bite per flee step" :default 12}
   :step {:doc "blocks per walk" :default 6}
   :home-range {:doc "a flight leans towards the latest :bed or :home only when it lies within this many blocks" :default 64}
   :weapons {:doc "item name substrings that count as weapons, for a cornered fight" :default combat/default-weapons}
   :reserve {:doc "health a cornered fight must be expected to leave" :default 4}
   :blocks {:doc "names of the blocks a cornered body may seal itself in with" :default dig-in/building-blocks}
   :max-places {:doc "seal placements per step" :default 4}
   :lost-s {:doc "a mob out of line of sight this many seconds has stopped chasing" :default 4}
   :quiet-s {:doc "a hidden body keeps its refuge this many seconds after the last danger" :default 30}
   :no-gain-steps {:doc "flight steps without a new best gap to the nearest chaser before the cornered options are tried" :default 20}})

(def tool-weapons
  "Item name substrings a cornered body with no weapon and no seal swings: any of them beats the fist."
  ["_pickaxe" "_shovel" "_hoe"])

(def flight-timeout-s
  "Bound of one step of the flight: the mob moves, so the next step aims again."
  5)

(defn home-pos
  "The position of the latest :bed or :home entry within :home-range blocks of the body, or nil."
  [c]
  (let [pos (->> [(ctx/latest c :bed) (ctx/latest c :home)]
                 (remove nil?)
                 (sort-by :t >)
                 first
                 :data
                 :pos)]
    (when (and pos (<= (u/dist pos (u/self-pos c)) (:home-range (:args c)))) pos)))

(defn check [_c] true)

(defn block-at-fn [p]
  (fn [pos] (u/block-name p pos)))

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close: :again, nil when the fight
  cannot reach any hostile (the option failed this flight)."
  [c weapons]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (ctx/update-mem! c assoc :cornered true)
    (let [r (await (ctx/call-child c :cornered 'jobs.survival.fight-back
                                   {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons
                                    :skip (flight/dead-ids c)}))]
      (ctx/update-mem! c update :dead #(into (vec %) (concat (get-in (ctx/mem c) [:children :cornered :killed])
                                                             (:killed (ctx/child-result c :cornered)))))
      (if (= :declined r)
        (do (flight/tried! c :fight) nil)
        :again))))

;; ------------------------------------------------------------------ cornered: the safest option

(defn escape-order
  "Pure: the cornered options to try, safest first. {:win? the odds say a fight leaves reserve health (never true
  for a creeper), :creeper? one is near, :ranged? the threat shoots}."
  [{:keys [win? creeper? ranged?]}]
  (cond
    win? [:fight]
    creeper? [:back-off :seal :pillar :pit :pocket :fight]
    :else (into (if ranged? [:seal] [:seal :pillar]) [:back-off :pit :pocket :fight])))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (flight/near-known (:primitives c) (set (flight/dead-ids c)) radius ranged-radius)))

(defn fight-wins?
  "Whether fighting hostiles with the best weapon carried is expected to leave :reserve health
  (jobs.lib.cost/fight-damage); never against a creeper."
  [c hostiles]
  (let [p (:primitives c)
        self (.self p)
        {:keys [weapons reserve]} (:args c)]
    (boolean
     (and (seq hostiles)
          (not-any? combat/creeper? hostiles)
          (<= (cost/fight-damage {:weapon (combat/best-weapon p weapons)
                                  :equipment (cost/equipment-of (.-equipment self))
                                  :mobs (map (fn [e] {:name (.-name e) :distance (danger-q/mob-distance p e) :hits 0}) hostiles)})
              (- (.-health self) reserve))))))

(defn back-off-target
  "A cell to back off to: of the ends of the open cells (refuge/up to 2, see walk-cells) in the eight compass directions,
  the farthest from threat, when it gains at least a block on it and passes no hazard; nil when none does."
  [block-at from threat hazards]
  (let [now (u/dist from threat)]
    (->> (range 0 360 45)
         (keep #(peek (walk/walk-cells block-at from (walk/rotate [1 0] %) 2)))
         (remove #(walk/near-hazard? hazards from %))
         (filter #(>= (u/dist % threat) (inc now)))
         (sort-by #(- (u/dist % threat)))
         first)))

(defn ^:async step!
  "One flee step to target as a go-to child (a flee never digs or pillars: :escalate false; known dangers stay costed):
  true when the body arrived or got on a leg nearer, false when go-to gave up, waited or was declined."
  [c slot target range]
  (let [r (await (ctx/call-child c slot 'jobs.movement.go-to {:pos target :range range :escalate false
                                                              :warn false :leg-s flight-timeout-s
                                                              :retry false :look-round false}))
        res (when (= :done r) (ctx/child-result c slot))]
    (boolean (or (:arrived res) (:leg res)))))

(defn ^:async back-off!
  "Step back from threat (back-off-target): :again, nil when there is no such cell or the walk is blocked."
  [c threat]
  (when-let [target (back-off-target (block-at-fn (:primitives c)) (u/self-pos c) (danger-q/mob-pos (:primitives c) threat)
                                     (keep (comp :pos :data) (ctx/entries c :hazard)))]
    (if-not (await (step! c :back-off target 0))
      (do (flight/tried! c :back-off) nil)
      :again)))

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, hold a moment (wait, why cornered) and go on: the next step
  tries them again from the start (the danger still stands). The second sweep in a row ends the flight :cannot-escape."
  [c why]
  (ctx/update-mem! c #(-> % (dissoc :tried) flight/count-sweep))
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (await (ctx/act c :wait #js {:ms flight/wait-ms :why "cornered"}))
  (if (flight/cannot-escape? (ctx/mem c)) (flight/end-flight! c :cannot-escape) :again))

(defn ^:async cornered!
  "Nowhere worth walking to: the first option of escape-order that it can take and has not failed since the last
  start over (blocked!)."
  [c why]
  (let [{:keys [weapons]} (:args c)
        hostiles (near-hostiles c)
        threat (first hostiles)
        order (escape-order {:win? (fight-wins? c hostiles)
                             :creeper? (boolean (some combat/creeper? hostiles))
                             :ranged? (boolean (and threat (combat/ranged? threat)))})]
    (loop [[option & more] order]
      (let [r (when-not (flight/tried? c option)
                (case option
                  :fight (await (fight! c (if (= [:fight] order) weapons (into (vec weapons) tool-weapons))))
                  :seal (await (refuge/hide! c))
                  :pillar (await (refuge/pillar! c))
                  :back-off (when threat (await (back-off! c threat)))
                  :pit (await (refuge/pit! c))
                  :pocket (await (refuge/pocket! c))))]
        (cond
          (some? r) r
          (seq more) (recur more)
          :else (await (blocked! c why)))))))

(defn ^:async eat-on-the-run!
  "One bite a flee step, with the nearest hostile at least :eat-gap away; none for the rest of the flight once
  nothing is left to eat or a bite fails."
  [c threat]
  (when (and (not (:ate (ctx/mem c)))
             (>= (danger-q/mob-distance (:primitives c) threat) (:eat-gap (:args c))))
    (let [r (await (ctx/call-child c :eat 'jobs.survival.eat {:until 20 :max-bites 1}))]
      (when (or (= :declined r) (:reason (ctx/child-result c :eat)))
        (ctx/update-mem! c assoc :ate true)))))

(def door-reach
  "Farthest (blocks, feet to the cell's middle) an open door may be for the flight to shut it: within a hand's reach."
  4)

(defn door-key [{:keys [x y z]}] [x y z])

(defn lower-half
  "The cell of a door's lower half (a gate's or trapdoor's own cell) for an openable block b at cell."
  [b cell]
  (if (= "upper" (:half (click/props-of b))) (update cell :y dec) cell))

(defn door-to-shut
  "The nearest door, gate or trapdoor a hand shuts that stands open within door-reach of the body, is nearer the
  threat at threat-pos than the body is (it lies between them, or beyond the body towards the threat), is not the
  one the body stands in, and has not been clicked this flight (:doors-clicked): {:cell :name}, or nil."
  [c threat-pos]
  (let [p (:primitives c)
        self (u/self-pos c)
        {fx :x fy :y fz :z} (sh/feet p)
        r (js/Math.ceil door-reach)
        clicked (:doors-clicked (ctx/mem c) #{})
        mid (fn [{:keys [x y z]}] {:x (+ x 0.5) :y y :z (+ z 0.5)})]
    (->> (for [x (range (- fx r) (+ fx r 1)) y (range (dec fy) (+ fy 3)) z (range (- fz r) (+ fz r 1))]
           {:x x :y y :z z})
         (keep (fn [cell]
                 (when-let [b (dig-cells/open-openable p cell)]
                   (when (= :openable (click/kind-of (.-name b)))
                     (let [low (lower-half b cell)]
                       {:cell low :name (.-name b) :half (:half (click/props-of b))})))))
         distinct
         (remove #(clicked (door-key (:cell %))))
         (remove #(click/standing-in? self (:cell %) (:half %)))
         (filter #(<= (u/dist self (mid (:cell %))) door-reach))
         (filter #(< (u/dist threat-pos (mid (:cell %))) (u/dist threat-pos self)))
         (sort-by #(u/dist self (mid (:cell %))))
         first)))

(defn ^:async shut-door!
  "Shut door (door-to-shut) with one click; every door is clicked at most once a flight, so a door that will not stay
  shut does not hold the flight. :again: the next step sees whether the danger is still there."
  [c {:keys [cell name]}]
  (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell))
  (let [r (await (click/click! c cell :closed name))]
    (when (= :changed (:outcome r))
      (ctx/emit! c :retreat.door-shut :info {:cell (door-key cell) :text (str "shut the " name " at " (door-key cell)
                                                                             " on the hostile")})))
  :again)

(defn ^:async wait-far!
  "Cornered with every chaser beyond :radius: hold still a moment (a :wait, why cornered) and look again."
  [c]
  (await (ctx/act c :wait #js {:ms flight/wait-ms :why "cornered"}))
  :again)

(defn ^:async flight-step!
  "One step of the flight (flight/look!, then shut a door, or walk a step away, or the cornered options), :again; the flight's
  end (flight/end-flight!) once no chaser is left."
  [c]
  (let [{:keys [step no-gain-steps]} (:args c)
        p (:primitives c)
        threats (flight/look! c)
        threat (first threats)
        door (when threat (door-to-shut c (danger-q/mob-pos p threat)))
        stuck (fn ^:async stuck [why]
                (if (empty? (near-hostiles c)) (await (wait-far! c)) (await (cornered! c why))))]
    (cond
      (nil? threat) (flight/end-flight! c nil)
      (flight/no-gain? (ctx/mem c) no-gain-steps)
      (do (ctx/update-mem! c assoc :since-gain 0)
          (await (stuck "the chaser keeps up")))
      door (await (shut-door! c door))
      :else
      (let [_ (await (eat-on-the-run! c threat))
            from (u/self-pos c)
            target (walk/choose-target (block-at-fn p) from (mapv #(danger-q/mob-pos p %) threats) (home-pos c)
                                  (keep (comp :pos :data) (ctx/entries c :hazard)) step)]
        (if (nil? target)
          (await (stuck "no open way away from the hostile"))
          (let [_ (when (:cornered (ctx/mem c)) (ctx/update-mem! c dissoc :cornered))
                moved (await (step! c :step target 1))]
            (if-not moved
              (await (stuck "the way away from the hostile is blocked"))
              (let [gap (flight/nearest-gap p (near-hostiles c))]
                (ctx/update-mem! c #(-> % (dissoc :tried) (flight/note-gap gap)))
                :again))))))))

(defn ^:async round
  "One whole flight: steps (flight-step!, or the refuge's) until it ends."
  [c]
  (ctx/update-mem! c flight/resume-flight (ctx/now c))
  (when-not (:flight-start (ctx/mem c)) (ctx/update-mem! c assoc :flight-start (ctx/now c)))
  (loop []
    (ctx/update-mem! c assoc :last-step (ctx/now c))
    (let [r (await (if (:refuge (ctx/mem c)) (refuge/refuge-round! c) (flight-step! c)))]
      (if (= :again r)
        (do (await (pace/pace!)) (recur))
        r))))
