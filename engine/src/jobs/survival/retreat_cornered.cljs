(ns jobs.survival.retreat-cornered
  "The cornered options of a flight (jobs.survival.retreat): which to take (escape-order), the fight, back-off, water, roof and swim-out, and the settings of the retreat jobs."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.hurt :as hurt]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.escape :as escape]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as result]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]
            [jobs.lib.shot :as shot]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-goal :as retreat-goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.retreat-pillar :as pillar]
            [jobs.survival.retreat-refuge :as refuge]
            [jobs.survival.retreat-walk :as walk]))

(def tool-weapons
  "Item name substrings a cornered body with no weapon and no seal swings: any of them beats the fist."
  ["_pickaxe" "_shovel" "_hoe"])



(defn block-at-fn
  "pos -> the block name the body sees or remembers there. A cell it has not seen reads as rock (behind the body or a
  wall is no free way until it looks: glance-away!); nil (open: the walk finds out) when the cell is not loaded."
  [p]
  (let [at (access/sensed-at p access/hidden-guess)]
    (fn [{:keys [x y z]}] (at [x y z]))))


(defn weapons
  "The weapons of the flight's call: its :use :tools, else the default substrings."
  [c]
  (or (ctx/use c :tools) combat/default-weapons))

(a/defargs settings
  {:jobs.survival.retreat-cornered/lost-memory-ms {:default 30000 :doc "Milliseconds a mob the flight lost (its :threat entry) still counts, at the place it was last known, when choosing where to flee; a creeper counts triple." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/fist-sweeps {:default 1 :doc "Times a body with no weapon, every cornered option failed, looks for a way to walk before it punches a hostile it would not beat; 0: it punches at once." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/hit-reach {:default 3 :doc "Blocks within which a melee hostile counts as the one that hit the body." :spec (a/num-in 0 nil)}
   :jobs.survival.retreat-cornered/glance-reach {:default 3 :doc "Blocks along a direction to the feet cell a glance looks at." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/glance-memory {:default 24 :doc "Most glanced cells a flight remembers." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/cover-gap {:default 5 :doc "Blocks within which a melee chaser keeps a low body from taking cover where it stands." :spec (a/num-in 0 nil)}
   :jobs.survival.retreat-cornered/close-radius {:default 8 :doc "Blocks within which a hostile that attacks at close range (a creeper too, not a ranged mob or an enderman) bounds a flee step to close-leg-s, at its start and again during its search." :spec (a/num-in 0 nil)}
   :jobs.survival.retreat-cornered/close-leg-s {:default 1 :doc "Seconds a flee step may search or walk while a close-range hostile is within close-radius, or the body was hurt: a body that stands searching there is hit for the whole leg." :spec (a/num-in 0 nil)}
   :jobs.survival.retreat-cornered/hurt-window-s {:default 5 :doc "Seconds after the body lost health in which a flee step is bounded to close-leg-s, even with no hostile seen in reach (an unseen attacker hits as hard)." :spec (a/num-in 0 nil)}
   :jobs.survival.retreat-cornered/fire-hits {:default 1 :doc "Hits by a ranged mob (the damage's attacker, or an arrow) within hurt-window-s that put a cornered body under fire: cover comes before any fight, and a shooter it has not seen is never weighed as beaten. 0: never." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/shot-hits {:default 2 :doc "Hits by a ranged mob within hurt-window-s that make a walking body take cover (seal, or break the line of fire) instead of walking on under fire it cannot answer. 0: it keeps walking." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/cover-tries {:default 2 :doc "Cornered steps under fire in which cover (seal, back off) is tried before an armed body fights a melee mob pressing it: a mob standing in the cells to wall keeps every seal failing, and each back-off step makes the covers open again." :spec (a/int-in 1 nil)}
   :jobs.survival.retreat-cornered/beaten-hits {:default 2 :doc "Hits by a melee mob near (the damage names one within close-radius; not a creeper or an enderman) within hurt-window-s that show the flight is failing: the body fights it first, armed or not. 0: never." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/press-gap {:default 2 :doc "Blocks within which a melee mob (not a creeper or an enderman) makes a cornered armed body fight before it builds a seal or pillar, hit or not: the mob strikes before a block is placed and stands in the cells to fill." :spec (a/num-in 0 nil)}
   :jobs.survival.retreat-cornered/hold-cap {:default 60 :doc "Holds (one wait-ms each) a flight makes in water or under a roof against an angry enderman before it tries the next option (seal, pit, pocket, fight); 0: none." :spec (a/int-in 0 nil)}
   :jobs.survival.retreat-cornered/refuge-gap {:default 10 :doc "Blocks within which a lone melee hostile the body cannot outfight is pillared against (else sealed against) before any flee step, when blocks are carried; 0 never." :spec (a/num-in 0 nil)}
   ::flight-timeout-s {:default 5 :doc "Seconds bounding one step of the flight: the mob moves, so the next step aims again." :spec (a/num-in 0 nil)}
   ::enderman-close {:default 3 :doc "Blocks within which an enderman counts as angry though it has not hurt the body yet (a calm one does not come this near)." :spec (a/num-in 0 nil)}
   ::water-reach {:default 6 :doc "Blocks within which a body hunted by an enderman looks for water to stand in." :spec (a/num-in 0 nil)}
   ::roof-reach {:default 8 :doc "Blocks within which a body hunted by an enderman looks for a 2-high gap under a roof." :spec (a/num-in 0 nil)}
   ::swim-reach {:default 16 :doc "Blocks within which a body in water looks for dry land to swim out to." :spec (a/num-in 0 nil)}})
(defn flight-timeout-s [] (settings/get settings ::flight-timeout-s))
(defn enderman-close [] (settings/get settings ::enderman-close))
(defn water-reach [] (settings/get settings ::water-reach))
(defn roof-reach [] (settings/get settings ::roof-reach))
(defn swim-reach [] (settings/get settings ::swim-reach))

(defn fist-sweeps [] (settings/get settings :jobs.survival.retreat-cornered/fist-sweeps))
(defn hit-reach [] (settings/get settings :jobs.survival.retreat-cornered/hit-reach))

(defn hits-total
  "The hits an attack ending's (or prior's) :struck counts over all mobs."
  [struck] (reduce + 0 (map :hits (vals struck))))

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close: :again, nil when the fight
  cannot reach any hostile (the option failed this flight; hits new in this call reset the sweep count). reach-only?: only a hostile within hit-reach is
  swung at, none walked to. A fight that ends other than done keeps its memory (its given-up counts) for the next."
  [c weapons reach-only?]
  (let [radius (danger-q/radius-in (:args c))
        ranged-radius (danger-q/ranged-radius-in (:args c))
        radius (if reach-only? (min radius (hit-reach)) radius)
        ranged-radius (if reach-only? radius ranged-radius)]
    (ctx/update-mem! c assoc :cornered true)
    (ctx/update-mem! c #(cond-> % (:cornered (:children %)) (update-in [:children :cornered] dissoc :start)))
    (let [hits-before (hits-total (:struck (:fight-prior (ctx/mem c))))
          r (await (ctx/call c :cornered 'jobs.combat.attack
                             {:targets :hostiles :radius radius :ranged-radius ranged-radius :use {:tools (mapv name weapons)}
                              :skip (flight/dead-ids c) :prior (:fight-prior (ctx/mem c)) :checks {:safe? '(always) :fit? '(always)}}))
          ended (when (contract/ending? r) r)]
      (ctx/update-mem! c update :dead #(into (vec %) (concat (get-in (ctx/mem c) [:children :cornered :killed])
                                                             (:killed (:data ended)))))
      (if (and ended (not= :done (:kind ended)))
        (do (when (:struck (:data ended)) (ctx/update-mem! c assoc :fight-prior (select-keys (:data ended) [:killed :given-up :names :struck])))
            (flight/tried! c :fight)
            (when (> (hits-total (:struck (:data ended))) hits-before)
              (ctx/update-mem! c assoc :sweeps 0))
            nil)
        :again))))


;; ------------------------------------------------------------------ cornered: the safest option

(defn escape-order
  "Pure: the cornered options to try, safest first. {:win? the odds say a fight leaves reserve health (never true
  for a creeper), :flyer? a phantom is near (it outflies a walker: shelter, else fight), :creeper? one is near (it comes first), :enderman? an angry enderman is near (angry-enderman?) (it teleports, so no walk or back-off, and a
  pillar is within its reach: the body enters water within water-reach, which it avoids, else walks under a 2-high roof within roof-reach, which it cannot enter, else shuts itself in), :ranged? the threat shoots, or the body is under fire (under-fire?), :climber? it is a spider (it climbs the pillar), :faster? a mob
  the body cannot outrun is near (no step back: it gains nothing), :home? the body is in its home
  room: it digs no pit or pocket in its own floor or walls, :lean? the pillar would leave the body too few blocks to roof in on its top (retreat-pillar/lean?): the dig-down comes before it (a pit regains its blocks), :pressed? pressed? holds and the body is not under fire (a seal or pillar cannot be built beside the mob: the fight comes first, except against a creeper or an enderman), :swim? the body is in water with a drowned near: swimming out to dry land comes first unless a fight wins}."
  [{:keys [win? creeper? enderman? flyer? ranged? climber? faster? swim? home? pressed? lean?]}]
  (cond->> (cond
             creeper? [:back-off :plug :pillar :plug-dig :fight]
             enderman? [:water :roof :plug :plug-dig :fight]
             win? [:fight]
             flyer? [:plug :plug-dig :fight]
             :else (into (if pressed? [:fight] [])
                         (concat (if (or ranged? climber?) [:plug] [:plug :pillar])
                                 (when-not faster? [:back-off])
                                 [:plug-dig]
                                 (when-not pressed? [:fight]))))
    (and swim? (not creeper?) (not enderman?) (not win?)) (into [:swim-out])
    (and lean? (not home?)) (#(if (some #{:pillar} %)
                                (vec (mapcat (fn [o] (case o :pillar [:plug-dig :pillar] :plug-dig [] [o])) %))
                                %))
    home? (filterv #(not= :plug-dig %))))

(defn outrun?
  "Whether the body gets away on foot from every one of hostiles that attacks at close range (jobs.lib.cost/outruns?,
  at its food). Creepers, endermen and ranged mobs have their own rules."
  [c hostiles]
  (cost/outruns? (room-lib/food (:primitives c))
                 (->> hostiles
                      (remove #(or (combat/creeper? %) (combat/enderman? %) (combat/ranged? %)))
                      (map cost/mob-kind))))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [radius (danger-q/radius-in (:args c))
        ranged-radius (danger-q/ranged-radius-in (:args c))]
    (flight/near-known (:primitives c) (set (flight/dead-ids c)) radius ranged-radius)))

(defn angry-enderman?
  "Whether one of hostiles is an enderman that is angry (the client's flag), hurt the body this flight (its health is under the flight's start) or
  stands within enderman-close: a calm one is walked away from like any mob, never sealed against."
  [c hostiles]
  (let [p (:primitives c)
        hurt? (< (.-health (.self p)) (:flight-health (ctx/mem c) 0))]
    (boolean (some #(and (combat/enderman? %) (or hurt? (true? (.-angry %)) (<= (danger-q/mob-distance p %) (enderman-close)))) hostiles))))

(defn hitters
  "Of hostiles, the melee ones (not a creeper or an enderman) within hit-reach."
  [c hostiles]
  (filterv #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                 (<= (danger-q/mob-distance (:primitives c) %) (hit-reach)))
           hostiles))

(defn close-radius [] (settings/get settings :jobs.survival.retreat-cornered/close-radius))

(defn hurt-by
  "The causes of a :hurt memory entry (the raw body event: its attacker's name, else the damage's cause)."
  [e]
  (hurt/causes-of (:data e)))

(defn hit-on-the-run!
  "Whether a melee chaser hit the body since the previous flight step: health lost with a hitter near, or a hurt entry
  naming a melee hostile within close-radius (a fed body heals the hit within a second, so health alone misses it). A chaser
  that hits it cannot be outrun. Noted in job memory (:hit-on-the-run), with the health, for the cornered options' odds."
  [c]
  (let [p (:primitives c)
        health (.-health (.self p))
        {:keys [step-health step-at]} (ctx/mem c)
        now (ctx/now c)
        near (near-hostiles c)
        names (->> near
                   (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                                 (<= (danger-q/mob-distance p %) (close-radius))))
                   (map #(.-name %))
                   set)
        named? (and step-at (some #(some names (hurt-by %)) (ctx/since c :hurt step-at)))
        hit? (boolean (or (and step-health (< health step-health) (seq (hitters c near))) named?))]
    (ctx/update-mem! c assoc :step-health health :step-at now :hit-on-the-run hit?)
    hit?))

(defn fight-wins?
  "Whether fighting hostiles with the best weapon carried is expected to leave :reserve health
  (jobs.lib.cost/fight-damage); never against a creeper."
  [c hostiles]
  (let [p (:primitives c)
        self (.self p)
        reserve (danger-q/reserve-in (:args c))]
    (boolean
     (and (seq hostiles)
          (not-any? combat/creeper? hostiles)
          (<= (cost/fight-damage {:weapon (combat/best-weapon p (weapons c))
                                  :equipment (cost/equipment-of (.-equipment self))
                                  :mobs (map (fn [e] {:name (.-name e) :distance (danger-q/mob-distance p e) :hits 0}) hostiles)})
              (- (.-health self) reserve))))))

(defn back-off-target
  "A cell to back off to: of the ends of the open cells (escape/up to 2, see walk-cells) in the eight compass directions,
  the farthest from threat, when it gains at least a block on it and passes no hazard; nil when none does."
  [block-at from threat hazards]
  (let [now (u/dist from threat)]
    (->> (range 0 360 45)
         (keep #(peek (walk/walk-cells block-at from (walk/rotate [1 0] %) 2)))
         (remove #(walk/near-hazard? hazards from %))
         (filter #(>= (u/dist % threat) (inc now)))
         (sort-by #(- (u/dist % threat)))
         first)))

(defn glance-reach [] (settings/get settings :jobs.survival.retreat-cornered/glance-reach))

(defn glance-memory [] (settings/get settings :jobs.survival.retreat-cornered/glance-memory))

(defn cover-gap [] (settings/get settings :jobs.survival.retreat-cornered/cover-gap))

(defn refuge-gap [] (settings/get settings :jobs.survival.retreat-cornered/refuge-gap))

(defn press-gap [] (settings/get settings :jobs.survival.retreat-cornered/press-gap))

(defn close-leg-s [] (settings/get settings :jobs.survival.retreat-cornered/close-leg-s))

(defn lost-memory-ms [] (settings/get settings :jobs.survival.retreat-cornered/lost-memory-ms))

(defn ^:async leg-step!
  "One flee step to target as a go-to child of legs of at most leg-s seconds (a flee never digs or pillars: :ways #{:walk :clear-seal}, and plans no route deeper or higher than y-margin (16) blocks past body and target;
  known dangers stay costed): true when the body arrived or got on a leg nearer, false when go-to gave up, waited or was declined (mem :leg-timed-out: it ran out of the leg's seconds unhurt)."
  ([c slot target range leg-s] (await (leg-step! c slot target range leg-s (retreat-goal/leg-margin))))
  ([c slot target range leg-s y-margin]
  (let [from (u/self-pos c)
        t0 (ctx/now c)
        health0 (.-health (.self (:primitives c)))
        r (await (ctx/call c slot 'jobs.movement.go-to {:pos target :range range :ways #{:walk :clear-seal}
                                                              :settings (cond-> {:jobs.movement.go-to.tuning/look-round false}
                                                                          y-margin (assoc :jobs.lib.walk.world/y-margin y-margin)) :spend {:job-time leg-s :tries 1}
                                                              :leg-alert-within (close-radius) :leg-alert-s (close-leg-s)
                                                              :floor {:health 1}}))
        res (when (contract/ending? r) (:data r))
        _ (when (= :goal-not-standable (result/why r)) (ctx/update-mem! c update :refused (fnil conj #{}) (mapv target [:x :y :z])))
        s (/ (- (ctx/now c) t0) 1000)
        d (u/dist from (u/self-pos c))
        _ (ctx/update-mem! c assoc :leg-timed-out (and (= :limit-reached (:kind r)) (>= (.-health (.self (:primitives c))) health0)))]
    (ctx/emit! c :retreat.step-speed :debug {:slot slot :blocks (/ (js/Math.round (* 10 d)) 10) :seconds (/ (js/Math.round (* 10 s)) 10)
                                             :bps (when (pos? s) (/ (js/Math.round (* 10 (/ d s))) 10))})
    (boolean (or (:arrived res) (:leg res))))))

(defn step! [c slot target range] (leg-step! c slot target range (flight-timeout-s)))

(defn ^:async back-off!
  "Step back from the place from (back-off-target): :again, nil when there is no such cell or the walk is blocked."
  [c from]
  (when-let [target (back-off-target (block-at-fn (:primitives c)) (u/self-pos c) from
                                     (keep (comp :pos :data) (ctx/entries c :hazard)))]
    (if-not (await (step! c :back-off target 0))
      (do (flight/tried! c :back-off) nil)
      :again)))

(defn ^:async hold!
  "One hold (a pause of flight/wait-ms) in a spot an enderman cannot use: :again, or nil once the flight has held
  hold-cap times (the option is then tried! and the next one of the order comes)."
  [c option why]
  (if (>= (:holds (ctx/mem c) 0) (settings/get settings :jobs.survival.retreat-cornered/hold-cap))
    (do (flight/tried! c option) nil)
    (do (ctx/update-mem! c update :holds (fnil inc 0))
        (await (child/pause! c (/ (flight/wait-ms) 1000) why))
        :again)))

(defn water-cell
  "The nearest cell within reach (blocks, horizontally and up to 2 vertically) of from that block-at reads as water, or nil."
  [block-at from reach]
  (let [[fx fy fz] (mapv #(js/Math.floor %) [(:x from) (:y from) (:z from)])]
    (->> (for [dx (range (- reach) (inc reach)) dy (range -2 3) dz (range (- reach) (inc reach))
               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))
               :let [cell {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)}]
               :when (= "water" (block-at cell))]
           cell)
         (sort-by #(u/dist from {:x (+ 0.5 (:x %)) :y (:y %) :z (+ 0.5 (:z %))}))
         first)))

(defn ^:async seen-once!
  "find (a cell or nil), else once a flight after a look round (a cell not seen reads as rock) the first find that follows a look."
  [c option find]
  (or (find)
      (when-not (contains? (:glanced (ctx/mem c)) option)
        (ctx/update-mem! c update :glanced (fnil conj #{}) option)
        (await (look/look-around-until! c find)))))

(defn ^:async water!
  "Stand in water (an enderman keeps away from it): a step to the nearest water within water-reach, a short hold once
  in it. :again, nil when there is none or the walk is blocked."
  [c]
  (let [block-at (block-at-fn (:primitives c))
        self (u/self-pos c)]
    (if (= "water" (block-at {:x (js/Math.floor (:x self)) :y (js/Math.floor (:y self)) :z (js/Math.floor (:z self))}))
      (await (hold! c :water "in water, away from the enderman"))
      (when-let [cell (await (seen-once! c :water #(water-cell block-at (u/self-pos c) (water-reach))))]
        (if (await (step! c :water {:x (+ 0.5 (:x cell)) :y (:y cell) :z (+ 0.5 (:z cell))} 0))
          :again
          (do (flight/tried! c :water) nil))))))

(defn roof-cell
  "The nearest cell within reach (blocks, horizontally and 1 vertically) of from that block-at reads as a 2-high gap
  on a solid floor under a solid roof, which a 3-tall enderman cannot enter, or nil. A cell with no cell an enderman
  could stand in (3 clear above a solid floor) within 2 blocks comes first: it cannot reach one at the roof's edge."
  [block-at from reach]
  (let [[fx fy fz] (mapv #(js/Math.floor %) [(:x from) (:y from) (:z from)])
        at (fn [cell dy] (block-at (update cell :y + dy)))
        gap? (fn [cell] (and (escape/solid? (at cell -1)) (bs/passable? (at cell 0))
                             (bs/passable? (at cell 1)) (escape/solid? (at cell 2))))
        tall? (fn [cell] (and (escape/solid? (at cell -1)) (bs/passable? (at cell 0))
                              (bs/passable? (at cell 1)) (bs/passable? (at cell 2))))
        deep? (fn [cell] (not-any? tall? (for [dx (range -2 3) dz (range -2 3)]
                                           (-> cell (update :x + dx) (update :z + dz)))))]
    (->> (for [dx (range (- reach) (inc reach)) dy (range -1 2) dz (range (- reach) (inc reach))
               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))
               :let [cell {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)}]
               :when (gap? cell)]
           cell)
         (sort-by (juxt (complement deep?) #(u/dist from {:x (+ 0.5 (:x %)) :y (:y %) :z (+ 0.5 (:z %))})))
         first)))

(defn ^:async roof!
  "Stand under a roof an enderman cannot enter: a step to the nearest roof-cell, a short hold once there (the cell over the
  head is looked at first: an unseen one reads as rock). :again, nil when there is none or the walk is blocked."
  [c]
  (let [block-at (block-at-fn (:primitives c))
        self (u/self-pos c)
        here {:x (js/Math.floor (:x self)) :y (js/Math.floor (:y self)) :z (js/Math.floor (:z self))}]
    (when (true? (some-> (u/sensed (:primitives c) (update here :y + 2)) .-unknown))
      (await (ctx/act c :look (clj->js {:pos {:x (+ 0.6 (:x here)) :y (+ 2.5 (:y here)) :z (+ 0.5 (:z here))}})))
      (look/see! c))
    (if (and (bs/passable? (block-at (update here :y inc))) (escape/solid? (block-at (update here :y + 2))))
      (await (hold! c :roof "under a roof, out of the enderman's reach"))
      (when-let [cell (await (seen-once! c :roof #(roof-cell block-at (u/self-pos c) (roof-reach))))]
        (if (await (step! c :roof {:x (+ 0.5 (:x cell)) :y (:y cell) :z (+ 0.5 (:z cell))} 0))
          :again
          (do (flight/tried! c :roof) nil))))))

(defn in-water?
  "Whether the body's feet cell reads as water."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    (= "water" ((block-at-fn (:primitives c)) {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)}))))

(defn ^:async swim-out!
  "Swim to the nearest seen dry stand within swim-reach (a go-to step, past the mob if that is the only way): :again,
  nil when there is none or the walk is blocked."
  [c]
  (let [p (:primitives c)
        block-at (block-at-fn p)
        feet (u/self-pos c)
        dry? (fn [{:keys [x y z]}] (and (not= "water" (block-at {:x x :y y :z z})) (not= "water" (block-at {:x x :y (inc y) :z z}))))
        to (first (step-off/candidates p {:x (js/Math.floor (:x feet)) :y (js/Math.floor (:y feet)) :z (js/Math.floor (:z feet))}
                                       {:reach (swim-reach) :ok? dry?}))]
    (when to
      (if (await (step! c :swim-out {:x (+ 0.5 (:x to)) :y (:y to) :z (+ 0.5 (:z to))} 0))
        :again
        (do (flight/tried! c :swim-out) nil)))))

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, pause a moment (why cornered) and go on: the next step
  tries them again from the start (the danger still stands). The second sweep in a row ends the flight :cannot-escape."
  [c why]
  (ctx/update-mem! c #(-> % (dissoc :tried) flight/count-sweep))
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (await (child/pause! c (/ (flight/wait-ms) 1000) "cornered"))
  (if (flight/cannot-escape? (ctx/mem c)) (flight/end-flight! c :cannot-escape) :again))

(defn fist-hold?
  "Pure: whether a cornered body walks on if it can once more before it fights: no weapon carried (armed? false), the odds
  not won (win?) and fewer than fist-sweeps sweeps done."
  [{:keys [armed? win? sweeps fist-sweeps]}]
  (and (not armed?) (not win?) (< sweeps fist-sweeps)))

(defn ^:async fist-look!
  "Count a sweep and let the flight step again: it walks away if a way has opened, else the options (all failed) come back here."
  [c]
  (ctx/update-mem! c flight/count-sweep)
  :again)

(defn hurt-window-s [] (settings/get settings :jobs.survival.retreat-cornered/hurt-window-s))

(defn recently-hurt?
  "Whether the body lost health within hurt-window-s."
  [c]
  (pos? (ctx/count-in c :hurt (* 1000 (hurt-window-s)))))

(defn pressed?
  "Whether an armed body has a melee mob (hitters) on it: one within press-gap, or one within hit-reach and the body
  was hit this step or within hurt-window-s (a hit in an earlier step counts: the step since may have gained nothing)."
  [c hostiles]
  (let [near (hitters c hostiles)
        p (:primitives c)]
    (boolean (and (seq near)
                  (combat/best-weapon p (weapons c))
                  (or (:hit-on-the-run (ctx/mem c))
                      (recently-hurt? c)
                      (some #(<= (danger-q/mob-distance p %) (press-gap)) near))))))

(defn plug-continues?
  "Whether a plug begun here goes on (refuge/plugging-here?): not while an armed body is pressed, the fight comes first."
  [c]
  (and (refuge/plugging-here? c) (not (pressed? c (near-hostiles c)))))

(defn beaten-hits [] (settings/get settings :jobs.survival.retreat-cornered/beaten-hits))

(defn beaten?
  "Whether melee mobs within close-radius hit the body beaten-hits times within hurt-window-s: knocked back and
  stepping away, it is out of hit-reach at each step, yet the flight is failing."
  [c]
  (let [n (beaten-hits)
        p (:primitives c)
        names (->> (near-hostiles c)
                   (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                                 (<= (danger-q/mob-distance p %) (close-radius))))
                   (map #(.-name %))
                   set)]
    (and (pos? n)
         (seq names)
         (<= n (->> (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s))))
                    (filter #(some names (hurt-by %)))
                    (map #(:hits (:data %) 1))
                    (reduce + 0))))))

(defn beaten-fights?
  "Pure: whether a beaten body (beaten?) fights before it seals or pillars: armed, or with no blocks to wall with. An
  unarmed body that carries blocks builds first (a spider cannot be fought with fists)."
  [{:keys [beaten? armed? blocks?]}]
  (boolean (and beaten? (or armed? (not blocks?)))))

(defn shots-in
  "Shots in one :hurt entry: its hits when the entry is a shot, else the shot packets among its merged damages (a skeleton's arrow and a zombie's blow in one loss)."
  [e]
  (if (shot/shot? e)
    (:hits (:data e) 1)
    (count (filter #(shot/shot? {:data %}) (:damages (:data e))))))

(defn fire-hits [] (settings/get settings :jobs.survival.retreat-cornered/fire-hits))

(defn under-fire?
  "Whether a ranged mob hit the body fire-hits times within hurt-window-s, seen or not."
  [c]
  (let [n (fire-hits)]
    (and (pos? n)
         (<= n (transduce (map shots-in) + 0 (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s)))))))))

(defn shot-on-the-run?
  "Whether a ranged mob hit the body shot-hits times within hurt-window-s and it carries blocks to wall with: walking on only feeds the shooter, so the flight takes cover."
  [c]
  (let [n (settings/get settings :jobs.survival.retreat-cornered/shot-hits)]
    (and (pos? n)
         (seq (lb/carried c (flight/blocks c)))
         (<= n (->> (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s))))
                    (map shots-in)
                    (reduce + 0))))))

(defn press-fights?
  "Pure: whether a melee mob pressing the body (pressed?) makes it fight before it builds: always unless it is under fire, where cover comes first for limit steps."
  [{:keys [fire? pressed? tries limit]}]
  (boolean (and pressed? (or (not fire?) (> tries limit)))))

(defn retry-covers
  "Pure: mem of a body under fire at feet cell: in another cell than the last time (:fire-cell), the covers that failed
  there (plug, plug-dig) are open again."
  [mem cell]
  (if (= cell (:fire-cell mem))
    mem
    (-> mem (update :tried #(reduce disj (set %) [:plug :plug-dig])) (assoc :fire-cell cell))))

(defn boxed-in?
  "Whether the body stands in a closed room with a melee mob (hitters) within hit-reach and was hit this step or within
  hurt-window-s: no step leads out of it, so backing off only prolongs the beating and the fight is the way."
  [c hostiles]
  (boolean (and (seq (hitters c hostiles))
                (or (:hit-on-the-run (ctx/mem c)) (recently-hurt? c))
                (sh-sense/shut-in-room? (:primitives c)))))

(defn bare-boxed-in?
  "Whether the body is in a closed hut (not a one-wide tunnel, which may lead on) with a melee mob (not a creeper or an enderman) within close-radius in that room (not outside its door) and has no
  weapon and no block to wall with: the fist is all it has, so it fights at once rather than dodge about the room."
  [c hostiles]
  (let [p (:primitives c)
        close (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                            (<= (danger-q/mob-distance p %) (close-radius)))
                      hostiles)
        in-room? (fn [] (let [cells (:cells (reach/room p (sh-sense/feet p)))]
                          (some (fn [e] (let [{:keys [x y z]} (danger-q/mob-pos p e)]
                                          (contains? cells [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])))
                                close)))]
    (boolean (and (seq close)
                  (nil? (combat/best-weapon p (weapons c)))
                  (empty? (lb/carried c (flight/blocks c)))
                  (sh-sense/shut-in-hut? p)
                  (in-room?)))))

(defn mob-in-corner-cell?
  "Whether a melee mob (hitters) stands in a diagonal cell the walls still have to fill: a placed block is refused
  there and it hits across the corner, so the walls cannot close until it is fought off. A mob beside the body is left
  to the pillar."
  [c hostiles]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        corner? (fn [{:keys [x z]}] (and (== 1 (js/Math.abs (- x (:x feet)))) (== 1 (js/Math.abs (- z (:z feet))))))
        open (set (filter corner? (dig-cells/open-cells p feet)))]
    (boolean (some (fn [e] (let [{:keys [x y z]} (danger-q/mob-pos p e)
                                 cell {:x (int (js/Math.floor x)) :y (int (js/Math.floor y)) :z (int (js/Math.floor z))}]
                             (or (open cell) (open (update cell :y inc)))))
                   (hitters c hostiles)))))

(defn ^:async cornered!
  "Nowhere worth walking to: the first option of escape-order that it can take and has not failed since the last
  start over (blocked!). home?: the body is in its home room."
  ([c why] (await (cornered! c why false)))
  ([c why home?]
  (let [weapons (weapons c)
        max-fist-sweeps (fist-sweeps)
        hostiles (near-hostiles c)
        threat (first hostiles)
        fire? (under-fire? c)
        beaten (and (not fire?) (or (beaten? c) (boxed-in? c hostiles)))
        blind? (and fire? (not-any? #(and (combat/ranged? %) (danger-q/seen-only? %)) hostiles))
        _ (when fire? (ctx/update-mem! c retry-covers (let [{:keys [x y z]} (sh-sense/feet (:primitives c))] [x y z])))
        _ (when fire? (ctx/update-mem! c update :fire-tries (fnil inc 0)))
        fire-tries (:fire-tries (ctx/mem c) 0)
        armed? (some? (combat/best-weapon (:primitives c) weapons))
        beaten-fight? (or (beaten-fights? {:beaten? beaten :armed? armed? :blocks? (boolean (seq (lb/carried c (flight/blocks c))))})
                          (and (not fire?) (mob-in-corner-cell? c hostiles)))
        win? (and (not blind?)
                  (fight-wins? c (if (and (:hit-on-the-run (ctx/mem c)) (not fire?)) (hitters c hostiles) hostiles)))
        order (escape-order {:win? win?
                             :creeper? (boolean (some combat/creeper? hostiles))
                             :enderman? (angry-enderman? c hostiles)
                             :flyer? (boolean (some combat/flyer? hostiles))
                             :ranged? (boolean (or fire? (and threat (combat/ranged? threat))))
                             :climber? (boolean (some combat/climber? hostiles))
                             :faster? (not (outrun? c hostiles))
                             :pressed? (or beaten-fight? (press-fights? {:fire? fire? :pressed? (pressed? c hostiles) :tries fire-tries :limit (settings/get settings :jobs.survival.retreat-cornered/cover-tries)}))
                             :swim? (and (boolean (some #(= "drowned" (.-name %)) hostiles)) (in-water? c))
                             :home? home?
                             :lean? (pillar/lean? c)})]
    (loop [[option & more] order]
      (let [r (when-not (flight/tried? c option)
                (case option
                  :fight (if (fist-hold? {:armed? (or beaten-fight? armed?) :win? win?
                                          :sweeps (:sweeps (ctx/mem c) 0) :fist-sweeps max-fist-sweeps})
                           (await (fist-look! c))
                           (await (fight! c (if (= [:fight] order) weapons (into (vec weapons) tool-weapons)) blind?)))
                  :plug (await (refuge/plug-option! c :plug #{:walls}))
                  :plug-dig (await (refuge/plug-option! c :plug-dig #{:pit :pocket}))
                  :pillar (await (refuge/pillar! c))
                  :water (await (water! c))
                  :roof (await (roof! c))
                  :swim-out (await (swim-out! c))
                  :back-off (when-let [from (or (some->> threat (danger-q/mob-pos (:primitives c))) (first (flight/fire-spots c)))]
                              (await (back-off! c from)))))]
        (cond
          (some? r) r
          (seq more) (recur more)
          :else (await (blocked! c why))))))))
