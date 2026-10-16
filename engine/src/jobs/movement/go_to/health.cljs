(ns jobs.movement.go-to.health
  "go-to when the damage budget (jobs.lib.cost/damage-budget) is why no way was found (the plan's :damage-refused): heal
  first, then go over the budget, so a walk still arrives unless it truly cannot.
  - A body that can heal (heal-way) waits: with food of 18 or more it regenerates (info go-to.waiting-health {:health},
    :continue after a one second wait), with a carried food it eats first (a jobs.survival.eat child, ctx/call). Healing that gains
    no hp in heal-stall-s stops.
  - A body that cannot heal, or whose healing stalled, goes over the budget only when the call's :floor :health is 1 or
    less (go-to's walk-limits; mem :over-budget; info go-to.over-budget {:health}): the walks then plan with the
    survivable budget, all but 1 hp. Not with a higher floor (never crossed) or a :spend :health (a cap): those, a call
    that does not allow it, and a body that still has no way, give up :needs-health."
  (:require [engine.ctx :as ctx]
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

(def heal-stall-s "Seconds of waiting without gaining an hp after which healing is given up." 30)

(def wait-ms "How long each wait for health lasts before the call yields." 1000)

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

(def heal-gap-ms "A wait for health older than this is a new one (the call may have been cut or ended)." 10000)

(defn heal-state
  "The healing in progress, mem's :heal {:health :t :last :ate :stalled} (health at the last gain and when, the last wait,
  whether it ate, whether it gave up), nil when there is none or its last wait is over heal-gap-ms old."
  [m now]
  (let [h (:heal m)]
    (when (and h (< (- now (:last h)) heal-gap-ms)) h)))

(defn stalled?
  "Whether healing at health gained no hp since the state st's last gain for heal-stall-s of now (ms)."
  [st health now]
  (boolean (and st (or (:stalled st) (and (<= health (:health st)) (>= (- now (:t st)) (* 1000 heal-stall-s)))))))

(defn next-state
  "The healing state after a step at health and now: the gain time moves with every gain; flags keep their value unless given."
  [st health now flags]
  (merge {:health (if (and st (<= health (:health st))) (:health st) health)
          :t (if (and st (<= health (:health st))) (:t st) now)
          :last now}
         (select-keys st [:ate :stalled])
         flags))

(defn ^:async heal!
  "One step of healing for way (:regen or :eat): :continue after waiting or eating."
  [c way body st]
  (let [now (ctx/now c)]
    (ctx/update-mem! c assoc :heal (next-state st (:health body) now (when (= :eat way) {:ate true})))
    (if (= :eat way)
      (await (ctx/call c :eat 'jobs.survival.eat {:until foods/top-up-food}))
      (do (when-not st
            (ctx/emit! c :go-to.waiting-health :info {:health (:health body) :food (:food body)
                                                      :text (str "waiting to heal (" (:health body) " hp) before a drop it cannot afford")}))
          (await (child/pause! c (/ wait-ms 1000) :healing))))
    :continue))

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
  "Whether looking over the edge of drop is held back: a hostile the body knows of near the lip, or lava seen in the drop's column."
  [c {:keys [from floor]}]
  (let [p (:primitives c)
        [fx fy fz] from
        [tx ty tz] floor]
    (or (seq (danger-q/hostiles-near-way p {:x fx :y fy :z fz} (danger-q/hostile-radius) {:ranged-radius (danger-q/ranged-radius)}))
        (some #(some-> (u/seen-block p {:x tx :y % :z tz}) .-name (= "lava")) (range ty (inc fy))))))

(defn ^:async at-lip-cell!
  "Whether the body stands on the takeoff cell [x y z] of a drop, walking there with a go-to child if it does not (the lean
  is a straight steer, which goes off any edge on the way)."
  [c [x y z]]
  (let [pos (u/self-pos c)]
    (or (= [x y z] (mapv #(js/Math.floor (% pos)) [:x :y :z]))
        (:arrived (:data (await (child/run!* c :lip 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 0 :ways #{:walk :clear-seal}})))))))

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
        (ctx/update-mem! c update :drops-looked (fnil conj []) drop)
        (when arrived?
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

(defn ^:async probe!
  "The hp of the whole way for a body that may spend all but 1 hp, planned again only when the health or the cell changed
  since the last probe of this wait (a probe that found no way is not kept) (mem :probe, kept heal-gap-ms): see probe-plan!."
  [c pos range]
  (let [now (ctx/now c)
        key [(.-health (.self (:primitives c))) (mapv #(js/Math.floor (% pos)) [:x :y :z])]
        m (:probe (ctx/mem c))]
    (if (and m (= key (:key m)) (< (- now (:at m)) heal-gap-ms))
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
            st (heal-state (ctx/mem c) now)
            body (cond-> (body-of c) (:ate st) (assoc :carried-food false))
            way (heal-way body)
            way (if (and (= :regen way) (stalled? st (:health body) now))
                  (do (ctx/update-mem! c assoc :heal (next-state st (:health body) now {:stalled true})) :none)
                  way)]
        (cond
          (#{:regen :eat} way) (await (heal! c way body st))
          (or (:over-budget (ctx/mem c)) min-health (not (:allow-over-budget (:args c))))
          {:needs-health true :heal way :health (:health body) :hp-cost planned :hp-budget (wworld/damage-budget c)}
          :else (do (ctx/update-mem! c assoc :over-budget true)
                    (ctx/emit! c :go-to.over-budget :info {:health (:health body) :planned planned
                                                           :text (str "no way within the damage budget and no way to heal: going over it at " (:health body) " hp for " planned " hp")})
                    :again))))))
