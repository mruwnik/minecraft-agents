(ns jobs.movement.go-to.health
  "go-to when the damage budget (jobs.lib.cost/damage-budget) is why no way was found (the plan's :damage-refused). go-to
  does not heal or eat: the walk gives up :needs-health ({:heal how the body could gain hp, :health :hp-cost :hp-budget}),
  and the caller or the hurt and hungry reflexes heal. Only a call with a :floor :health of 1 or less goes over the budget
  (go-to's walk-limits; mem :over-budget; info go-to.over-budget {:health}): the walks then plan with the survivable
  budget, all but 1 hp. Not with a higher floor (never crossed) or a :spend :health (a cap).

  Rules of go-to:
  - A drop or plant that costs hp is planned only within a damage budget (jobs.lib.cost/damage-budget: the health over
    the call's :floor :health, default 12, less 1, at most its :spend :health; a caller's floor counts only above 12
    unless this call gives one, walk-limits). When that budget is why no way was found, go-to does not heal or eat: it gives up :why :needs-health (limit-reached
    :health; :heal says how the body could gain hp) and the caller or the hurt and hungry reflexes heal; at full health, where
    healing cannot help, :why :damage-budget (nothing-possible). Only a given :floor :health of 1
    or less goes over it, down to 1 hp (info go-to.over-budget). See jobs.movement.go-to.health."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.health :as health]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.drive :as drive]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]))

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

(def look-drop-min "Blocks of a drop from which its landing is looked at before the way is judged too dear." 4)

(defn settled?
  "Whether looking at drop would change nothing about its price: its landing is water in view, or its floor is in fresh view and no bouncing block (a plain fall; a pad is judged by what the body sees)."
  [p {:keys [floor]} [lx ly lz]]
  (let [seen (wworld/landing-seen p)
        name-at (fn [x y z] (some-> (u/seen-block p {:x x :y y :z z}) .-name))
        [fx fy fz] floor]
    (boolean (and seen
                  (or (and (= "water" (name-at lx ly lz)) (seen lx ly lz false))
                      (and (seen fx fy fz false) (not (some #{(name-at fx fy fz)} wworld/bounce-blocks))))))))

(defn drops
  "The drops of look-drop-min blocks or more in steps (a planner path's JS steps) that a look can still decide, as {:from cell :floor the cell under the landing}: not a landing in water, nor a floor in view that does not bounce."
  [p steps]
  (vec (for [[a b] (partition 2 1 (some-> steps array-seq))
             :when (>= (- (.-y a) (.-y b)) look-drop-min)
             :let [drop {:from [(.-x a) (.-y a) (.-z a)] :floor [(.-x b) (dec (.-y b)) (.-z b)]}]
             :when (not (settled? p drop [(.-x b) (.-y b) (.-z b)]))]
         drop)))

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
    (when (and drop (not (drive/lip-unsafe? c drop)))
      (let [arrived? (await (at-lip-cell! c from))]
        (when-not (and arrived? (drive/lip-unsafe? c drop))
          (ctx/update-mem! c update :drops-looked (fnil conj []) drop))
        (when (and arrived? (not (drive/lip-unsafe? c drop)))
          (await (drive/lean-to! c (drive/lip from floor)))
          (await (dig-look/look-at! c floor))
          (look/see! c)
          true)))))

(defn ^:async probe-plan!
  "Whether a whole way exists for the body that may spend all but 1 hp (:max-damage still capping): the hp it costs, or nil
  when there is none. A refusal of the budget in a search that finds no whole way anyway (:damage-refused is only a move the
  budget turned away) is no reason to heal."
  [c pos range]
  (let [{:keys [max-damage]} (wworld/walk-settings c)
        policy (assoc (wworld/body-policy c) :damage-budget (cost/survivable-budget (wworld/damage-body c) {:max-damage max-damage}))
        pw (wworld/path-world (:primitives c) (:unknown c))
        within (await (wplan/plan-within! c pw [(:x pos) (:y pos) (:z pos)] range (walk/default-weight) policy))
        ^js r (:r within)]
    (when (= "found" (.-status r))
      (ctx/update-mem! c assoc :probe-drops (drops (:primitives c) (some-> r .-path .-steps)))
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

(defn ^:async over-budget!
  "planned, the hp of the whole way for a body that may spend all but 1 hp (nil: none): nil when there is none or it costs no
  more than the budget (the budget is not why); else :again after a look at a drop that may reprice it (look-at-drops!) or
  after going over the budget (only a call with a :floor :health of 1 or less), or {:needs-health true :heal :health :hp-cost
  :hp-budget :hungry-food?}: healing is the caller's or the reflexes'."
  [c planned]
  (cond
    (or (not planned) (<= planned (wworld/damage-budget c))) nil
    (await (look-at-drops! c)) :again
    :else
    (let [{:keys [min-health]} (wworld/walk-settings c)
          body (body-of c)]
      (if (or (:over-budget (ctx/mem c)) min-health (not (:allow-over-budget (:args c))))
        (cond-> {:needs-health true :heal (heal-way body) :health (:health body) :hp-cost planned :hp-budget (wworld/damage-budget c)}
          (health/food-limited? (wworld/damage-body c) (wworld/walk-settings c)) (assoc :hungry-food (:food body)))
        (do (ctx/update-mem! c assoc :over-budget true)
            (ctx/emit! c :go-to.over-budget :info {:health (:health body) :planned planned
                                                   :text (str "no way within the damage budget: going over it at " (:health body) " hp for " planned " hp")})
            :again)))))

(defn budget-end
  "The give-up of over-budget!'s {:needs-health true ...}: :why :needs-health, or :damage-budget at full health, where
  healing cannot help (only the call's :floor :health can let the way go)."
  [handled]
  (merge {:status :no-path :reason (if (= :full (:heal handled)) :damage-budget :needs-health)}
         (select-keys handled [:heal :health :hp-cost :hp-budget :hungry-food])))

(defn ^:async heal-or-drop!
  "A walk whose search found no way and met the damage budget (:damage-refused): over-budget! of the whole way's hp (probe!)."
  [c pos range]
  (await (over-budget! c (await (probe! c pos range)))))
