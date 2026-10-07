(ns jobs.movement.go-to.health
  "go-to when the damage budget (jobs.lib.cost/damage-budget) is why no way was found (the plan's :damage-refused): heal
  first, then go over the budget, so a walk still arrives unless it truly cannot.
  - A body that can heal (heal-way) waits: with food of 18 or more it regenerates (info go-to.waiting-health {:health},
    :continue after a one second wait), with a carried food it eats first (a jobs.survival.eat child). Healing that gains
    no hp in heal-stall-s stops.
  - A body that cannot heal, or whose healing stalled, goes over the budget (mem :over-budget; info go-to.over-budget
    {:health}): the walks then plan with the survivable budget, all but 1 hp. Not with a :min-health (a caller's floor is
    never crossed) or a :max-damage (a caller's cap): those, and a body that still has no way, give up :needs-health."
  (:require [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.health :as health]
            [jobs.lib.foods :as foods]
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
  (boolean (and (:damage-refused result) (nil? (:max-damage (:args c))))))

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
      (await (ctx/call-child c :eat 'jobs.survival.eat {:until foods/top-up-food}))
      (do (when-not st
            (ctx/emit! c :go-to.waiting-health :info {:health (:health body) :food (:food body)
                                                      :text (str "waiting to heal (" (:health body) " hp) before a drop it cannot afford")}))
          (await (ctx/act c :wait #js {:ms wait-ms}))))
    :continue))

(defn ^:async probe-plan!
  "Whether a whole way exists for the body that may spend all but 1 hp (:max-damage still capping): the hp it costs, or nil
  when there is none. A refusal of the budget in a search that finds no whole way anyway (:damage-refused is only a move the
  budget turned away) is no reason to heal."
  [c pos range]
  (let [{:keys [drop-cost max-damage]} (:args c)
        policy (cond-> (assoc (wworld/body-policy c) :damage-budget (cost/survivable-budget (wworld/damage-body c) {:max-damage max-damage}))
                 (some? drop-cost) (assoc :drop-cost drop-cost))
        within (await (wplan/plan-within! c (wworld/path-world (:primitives c)) [(:x pos) (:y pos) (:z pos)] range walk/default-weight policy))
        ^js r (:r within)]
    (when (= "found" (.-status r))
      (or (some-> r .-path .-cost .-damage) 0))))

(defn ^:async probe!
  "The hp of the whole way for a body that may spend all but 1 hp, planned again only when the health or the cell changed
  since the last probe of this wait (mem :probe, kept heal-gap-ms): see probe-plan!."
  [c pos range]
  (let [now (ctx/now c)
        key [(.-health (.self (:primitives c))) (mapv #(js/Math.floor (% pos)) [:x :y :z])]
        m (:probe (ctx/mem c))]
    (if (and m (= key (:key m)) (< (- now (:at m)) heal-gap-ms))
      (:planned m)
      (let [planned (await (probe-plan! c pos range))]
        (ctx/update-mem! c assoc :probe {:key key :planned planned :at now})
        planned))))

(defn ^:async heal-or-drop!
  "A walk whose search found no way and met the damage budget (:damage-refused): nil when a way with all but 1 hp spent
  does not exist either (the budget is not why); else heal (heal!, :continue), or go over the budget (:again), or :needs-health
  when that is not allowed (a :min-health, or already over) and healing is no more."
  [c pos range]
  (let [{:keys [min-health]} (:args c)
        planned (await (probe! c pos range))]
    (when planned
      (let [now (ctx/now c)
            st (heal-state (ctx/mem c) now)
            body (cond-> (body-of c) (:ate st) (assoc :carried-food false))
            way (heal-way body)
            way (if (and (= :regen way) (stalled? st (:health body) now))
                  (do (ctx/update-mem! c assoc :heal (next-state st (:health body) now {:stalled true})) :none)
                  way)]
        (cond
          (#{:regen :eat} way) (await (heal! c way body st))
          (or (:over-budget (ctx/mem c)) min-health) :needs-health
          :else (do (ctx/update-mem! c assoc :over-budget true)
                    (ctx/emit! c :go-to.over-budget :info {:health (:health body) :planned planned
                                                           :text (str "no way within the damage budget and no way to heal: going over it at " (:health body) " hp for " planned " hp")})
                    :again))))))
