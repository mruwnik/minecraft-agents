(ns jobs.lib.price
  "The one acceptable-risk rule of go-to's walks and the readiness place checks: the hp of expected damage the body may take
  (allowed), and the price of staying where mobs spawn (stay-hp). A place check that fails only on exposure is ok while the
  seconds the job stays there (::stay-s of the ctx, else ::place-stay-s) cost no more than allowed (Q380, Q382); the walk to the
  place is priced by the go-to it runs."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage of a walk (spawns met on its dark open stretches, hostiles near its way) or of a stay at an exposed place may take; above it the start check waits."}
   ::hungry-food {:default 6 :spec (a/int-in 0 20)
                  :doc "Food at or under which the body cannot sprint away and does not heal: the damage it can afford is cut by ::hungry-share."}
   ::hungry-share {:default 0.5 :spec (a/num-in 0 1)
                   :doc "Share of the affordable damage a body at ::hungry-food or under may risk."}
   ::place-stay-s {:default 10 :spec (a/num-in 0 nil)
                   :doc "Seconds a job is taken to stay at an exposed place when it does not say (a job's own safe? wraps go-to.risk/priced with its figure)."}})

(defn cannot-heal?
  "Whether the body of ctx c can not heal by waiting: food under foods/top-up-food and nothing a meal would eat carried (reserved food, which the hungry trigger and an unnamed meal leave alone, does not count)."
  [c]
  (let [self (.self (:primitives c))]
    (and (< (.-food self) foods/top-up-food)
         (not (foods/carries-food? self (reserve/spared (ctx/view c) self))))))

(defn allowed
  "The hp of expected damage the body may take on a risk now: ::risk-share of its health (cut while hungry), all but 1 hp while it
  can not heal by waiting (cannot-heal?: waiting never lowers the price). With `worth` (seconds the caller counts the
  goal worth, the call's :worth), as many hp as that buys at the price of an hp now (dearer when hurt), all but 1 at most, if more."
  ([c] (allowed c (:worth (:args c))))
  ([c worth]
   (let [self (.self (:primitives c))
         health (.-health self)
         hungry? (some-> (.-food self) (<= (settings/get settings ::hungry-food)))
         share (* (settings/get settings ::risk-share) health (if hungry? (settings/get settings ::hungry-share) 1))
         share (if (cannot-heal? c) (max share (dec health)) share)]
     (if worth
       (max share (min (dec health) (/ worth (* (cost/walk-hp-seconds) (cost/health-scale health)))))
       share))))

(defn stay-hp
  "Expected damage of staying `seconds` where mobs spawn at `chance` per second, each a zombie fight with the kit carried."
  [c chance seconds]
  (* chance seconds (cost/fight-damage (assoc (wworld/kit-of c) :mobs [{:name "zombie" :distance 0 :hits 0}]))))

(defn round1 [x] (/ (js/Math.round (* 10 x)) 10))

(defn priced
  "The status s of a place check, but ok where it is :later or :never on exposure alone (:exposed with a :chance) while the hp the
  job's stay there costs are within (allowed c)."
  [c s]
  (let [{:keys [reason chance]} (nth s 2 nil)]
    (if-not (and (contains? #{:later :never} (first s)) (= :exposed reason) (number? chance))
      s
      (let [secs (or (::stay-s c) (settings/get settings ::place-stay-s))
            hp (stay-hp c chance secs)
            ok-hp (allowed c)]
        (if (<= hp ok-hp)
          (contract/ok :priced (str "exposed, but " (round1 secs) " s there cost " (round1 hp) " hp, within " (round1 ok-hp)))
          s)))))
