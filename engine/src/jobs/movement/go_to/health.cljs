(ns jobs.movement.go-to.health
  "go-to when the damage budget (jobs.lib.cost/damage-budget) is why no way was found (the plan's :damage-refused): heal
  first, then go over the budget, so a walk still arrives unless it truly cannot.
  - A body that can heal (heal-way) waits: with food of 18 or more it regenerates (info go-to.waiting-health {:health}; the
    round writes mem :heal {:to :until} and yields :continue, and go-to's can-run? says :later :healing until the body is at
    :to or :until, the regeneration time with slack, passes), with a carried food it eats first (a jobs.survival.eat child,
    ctx/call). A wait that ends short of :to stops healing.
  - A body that cannot heal, or whose healing stalled, goes over the budget only when the call's :floor :health is 1 or
    less (go-to's walk-limits; mem :over-budget; info go-to.over-budget {:health}): the walks then plan with the
    survivable budget, all but 1 hp. Not with a higher floor (never crossed) or a :spend :health (a cap): those, a call
    that does not allow it, and a body that still has no way, give up :needs-health."
  (:require [engine.constants :as constants]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.health :as health]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

(def heal-slack "How many times the regeneration time the wait for health may last before healing counts as stalled." 2)

(defn heal-way
  "How a body {:health :food :effects (names) :on-fire :carried-food} can gain hp: :full (nothing to gain), :regen (food
  of top-up-food or more: it regenerates), :eat (food is low and food is carried), :none (burning, poisoned or withered, or
  too hungry to regenerate with nothing to eat)."
  [{:keys [health food effects on-fire carried-food]}]
  (cond
    (>= health 20) :full
    (or on-fire (some health/ticking-effects effects)) :none
    (>= food foods/top-up-food) :regen
    carried-food :eat
    :else :none))

(defn with-body-food
  "c with the body's own food now (a felt fact) as its :food arg: the walk counts on it."
  [c]
  (let [food (.-food (.self (:primitives c)))]
    (cond-> c (number? food) (assoc-in [:args :food] food))))

(defn body-of
  "The body's {:health :food :effects :on-fire :carried-food}: the food is the job's (wworld/food-of), the rest from its primitives."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    {:health (.-health self) :food (wworld/food-of c) :on-fire (.-onFire self)
     :effects (map #(.-name %) (array-seq (.-effects self)))
     :carried-food (boolean (some #(foods/edible? (:name %)) (u/inventory p)))}))

(defn refused?
  "Whether the damage budget is why result found no way, and the caller left the budget to go-to (no :max-damage)."
  [c result]
  (boolean (and (:damage-refused result) (nil? (:max-damage (wworld/walk-settings c))))))

(defn heal-state
  "The healing in progress, mem's :heal {:to :until :ate} (the health waited for, the time (ms) the wait may last, whether it
  ate), nil when there is none or the body has reached :to."
  [m health]
  (let [h (:heal m)]
    (when (and h (or (nil? (:to h)) (< health (:to h)))) h)))

(defn waiting?
  "Whether st is a wait for regeneration still running at now."
  [st now]
  (boolean (and (:until st) (< now (:until st)))))

(defn stalled?
  "Whether the wait st ran out before the body reached its :to."
  [st health now]
  (boolean (and (:until st) (>= now (:until st)) (< health (:to st)))))

(defn heal-target
  "The health at which the planned hp of the drop fit the damage budget: at most 20."
  [c health planned]
  (min 20 (+ health (max 1 (js/Math.ceil (- planned (wworld/damage-budget c)))))))

(defn can-run?
  "A wait for regeneration (mem :heal, written by heal!) is a :later :healing {:health :want} while the body is below its :to,
  can regenerate and the wait's time is not up."
  [c]
  (let [c (with-body-food c)
        st (:heal (ctx/mem c))
        health (.-health (.self (:primitives c)))]
    (if (and (:until st) (< health (:to st)) (waiting? st (ctx/now c)) (= :regen (heal-way (body-of c))))
      (contract/later :healing (str "healing from " health " hp to " (:to st)) :health health :want (:to st))
      (contract/ok :not-healing "no wait for health"))))

(defn ^:async heal!
  "One step of healing for way (:regen or :eat) toward to hp: eating is a child; regenerating writes mem :heal {:to :until}
  (the wait's deadline from the regeneration rate) and go-to's can-run? holds the call there. :continue."
  [c way body st to]
  (let [now (ctx/now c)]
    (cond
      (= :eat way) (do (ctx/update-mem! c assoc :heal (assoc st :ate true))
                       (await (ctx/call c :eat 'jobs.survival.eat {:until foods/top-up-food})))
      (waiting? st now) nil
      :else (do (when-not (:until st)
                  (ctx/emit! c :go-to.waiting-health :info {:health (:health body) :food (:food body)
                                                            :text (str "waiting to heal (" (:health body) " hp) before a drop it cannot afford")}))
                (ctx/update-mem! c update :heal merge (select-keys st [:ate])
                                 {:to to :until (+ now (* 1000 heal-slack constants/regen-seconds-per-hp (- to (:health body))))}))))
  :continue)

(def look-drop-min "Blocks of a drop from which its landing is looked at before the way is judged too dear." 4)

(def lip-reach "How far from the middle of its cell toward a drop the body leans to look over the edge: sneaking stops the box at the edge, so only the centre overhangs." 0.44)

(def lean-ticks "Ticks a lean to the lip may take." 60)

(defn lean-to!
  "Sneaks to the point x z (the game's sneak keeps the body from going off the edge), within a tenth of a block."
  [c x z]
  (let [ticks (volatile! 0)]
    (ctx/act c :steer (walk/steer-args (walk/steer-s c 3)
                                       (fn [pose]
                                         (let [dx (- x (.-x pose)) dz (- z (.-z pose))]
                                           (if (or (and (< (js/Math.abs dx) 0.1) (< (js/Math.abs dz) 0.1)) (> (vswap! ticks inc) lean-ticks))
                                             #js {:done true}
                                             #js {:controls #js {:forward true :sneak true} :yaw (js/Math.atan2 (- dx) (- dz))})))))))

(defn drops
  "The drops of look-drop-min blocks or more in steps (a planner path's JS steps), as {:from cell :floor the cell under the landing}."
  [steps]
  (vec (for [[a b] (partition 2 1 (some-> steps array-seq))
             :when (>= (- (.-y a) (.-y b)) look-drop-min)]
         {:from [(.-x a) (.-y a) (.-z a)] :floor [(.-x b) (dec (.-y b)) (.-z b)]})))

(defn lip-unsafe?
  "Whether looking over the edge of drop is held back: a hostile the body knows of near the lip, or lava seen in the drop's column or beside it."
  [c {:keys [from floor]}]
  (let [p (:primitives c)
        [fx fy fz] from
        [tx ty tz] floor]
    (or (seq (danger-q/hostiles-near-way p {:x fx :y fy :z fz} (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)}))
        (some (fn [[dx dz]] (some #(some-> (u/seen-block p {:x (+ tx dx) :y % :z (+ tz dz)}) .-name (= "lava")) (range ty (inc fy))))
              [[0 0] [1 0] [-1 0] [0 1] [0 -1]]))))

(defn ^:async at-lip-cell!
  "Whether the body stands on the takeoff cell [x y z] of a drop, walking there with a go-to child if it does not (the lean
  is a straight steer, which goes off any edge on the way). The child may spend no health, so it never meets the budget
  and never looks at a drop itself (refused? needs no :max-damage)."
  [c [x y z]]
  (let [pos (u/self-pos c)]
    (or (= [x y z] (mapv #(js/Math.floor (% pos)) [:x :y :z]))
        (:arrived (:data (await (child/run!* c :lip 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 0 :ways #{:walk :clear-seal} :spend {:health 0}})))))))

(defn ^:async look-at-drops!
  "A drop the budget refuses may be priced by what the body has not looked at: a bounce counts only on a pad in view
  (landing-seen), and a floor far below shows only from the lip. Once per drop of the probed way, walks to its takeoff cell (at-lip-cell!), leans to the lip
  (sneaking: the edge stops the box) and looks at the floor: true when it did (the walk plans again). Not while a hostile is
  near or lava is seen in the drop; the drop is looked at on a later probe."
  [c]
  (let [m (ctx/mem c)
        {:keys [from floor] :as drop} (first (remove (set (:drops-looked m)) (:probe-drops m)))]
    (when (and drop (not (lip-unsafe? c drop)))
      (let [arrived? (await (at-lip-cell! c from))
            [fx _ fz] from
            [tx _ tz] floor
            lip (fn [f t] (+ f 0.5 (* lip-reach (compare t f))))]
        (when-not (and arrived? (lip-unsafe? c drop))
          (ctx/update-mem! c update :drops-looked (fnil conj []) drop))
        (when (and arrived? (not (lip-unsafe? c drop)))
          (await (lean-to! c (lip fx tx) (lip fz tz)))
          (await (dig-look/look-at! c floor))
          (look/see! c)
          true)))))

(defn ^:async probe-plan!
  "Whether a whole way exists for the body that may spend all but 1 hp (:max-damage still capping): the hp it costs, or nil
  when there is none. A refusal of the budget in a search that finds no whole way anyway (:damage-refused is only a move the
  budget turned away) is no reason to heal."
  [c pos range]
  (let [{:keys [drop-cost costs]} (:args c)
        {:keys [max-damage]} (wworld/walk-settings c)
        policy (cond-> (assoc (wworld/body-policy c) :damage-budget (cost/survivable-budget (wworld/damage-body c) {:max-damage max-damage}))
                 (some? drop-cost) (assoc :drop-cost drop-cost)
                 (some? costs) (assoc :costs costs))
        within (await (wplan/plan-within! c (wworld/path-world (:primitives c) (:unknown c)) [(:x pos) (:y pos) (:z pos)] range walk/default-weight policy))
        ^js r (:r within)]
    (when (= "found" (.-status r))
      (ctx/update-mem! c assoc :probe-drops (drops (some-> r .-path .-steps)))
      (or (some-> r .-path .-cost .-damage) 0))))

(def probe-keep-ms "How long a probe is kept while the body waits." 10000)

(defn ^:async probe!
  "The hp of the whole way for a body that may spend all but 1 hp, planned again only when the health or the cell changed
  since the last probe of this wait (a probe that found no way is not kept) (mem :probe, kept probe-keep-ms): see probe-plan!."
  [c pos range]
  (let [now (ctx/now c)
        key [(.-health (.self (:primitives c))) (mapv #(js/Math.floor (% pos)) [:x :y :z])]
        m (:probe (ctx/mem c))]
    (if (and m (= key (:key m)) (< (- now (:at m)) probe-keep-ms))
      (:planned m)
      (let [planned (await (probe-plan! c pos range))]
        ;; a nil (no way) is not kept: it ends the wait, and the world may have changed by the next call
        (when planned (ctx/update-mem! c assoc :probe {:key key :planned planned :at now}))
        planned))))

(defn ^:async heal-or-drop!
  "A walk whose search found no way and met the damage budget (:damage-refused): nil when a way with all but 1 hp spent
  does not exist either (the budget is not why); else heal (heal!, :continue), or go over the budget (:again), or {:needs-health true :heal :health :hp-cost :hp-budget}
  when that is not allowed (a :min-health, or already over) and healing is no more."
  [c pos range]
  (let [{:keys [min-health]} (wworld/walk-settings c)
        planned (await (probe! c pos range))]
    (cond
      (not planned) nil
      (await (look-at-drops! c)) :again
      :else
      (let [now (ctx/now c)
            body (cond-> (body-of c) (:ate (:heal (ctx/mem c))) (assoc :carried-food false))
            st (heal-state (ctx/mem c) (:health body))
            way (heal-way body)
            way (if (and (= :regen way) (stalled? st (:health body) now)) :none way)]
        (cond
          (#{:regen :eat} way) (await (heal! c way body st (heal-target c (:health body) planned)))
          (or (:over-budget (ctx/mem c)) min-health (not (:allow-over-budget (:args c))))
          {:needs-health true :heal way :health (:health body) :hp-cost planned :hp-budget (wworld/damage-budget c)}
          :else (do (ctx/update-mem! c assoc :over-budget true)
                    (ctx/emit! c :go-to.over-budget :info {:health (:health body) :planned planned
                                                           :text (str "no way within the damage budget and no way to heal: going over it at " (:health body) " hp for " planned " hp")})
                    :again))))))
