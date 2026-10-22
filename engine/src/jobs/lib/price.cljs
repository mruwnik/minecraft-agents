(ns jobs.lib.price
  "The one acceptable-risk rule of go-to's walks and the readiness place checks: the hp of expected damage the body may take
  (allowed), and the price of staying where mobs spawn (stay-hp). A place check that fails only on exposure is ok while the
  seconds the job stays there (::stay-s of the ctx: its own figure, via stays, else its category's, via staying: ::place-stay-s
  for a click act, ::work-stay-s, ::long-stay-s) cost no more than allowed (Q380, Q382); the walk to the
  place is priced by the go-to it runs."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage of a walk (spawns met on its dark open stretches, hostiles near its way) or of a stay at an exposed place may take; above it the start check waits."}
   ::hungry-food {:default 6 :spec (a/int-in 0 20)
                  :doc "Food at or under which the body cannot sprint away and does not heal: the damage it can afford is cut by ::hungry-share."}
   ::hungry-share {:default 0.5 :spec (a/num-in 0 1)
                   :doc "Share of the affordable damage a body at ::hungry-food or under may risk."}
   ::place-stay-s {:default 10 :spec (a/num-in 0 nil)
                   :doc "Seconds a click act (a door, a lever, one block, a trade) is taken to stay at an exposed place."}
   ::work-stay-s {:default 120 :spec (a/num-in 0 nil)
                  :doc "Seconds a job that works a place for a while (a field, a pen, a tree, an apiary) is taken to stay at an exposed place."}
   ::long-stay-s {:default 600 :spec (a/num-in 0 nil)
                  :doc "Seconds a mine, a strip or a build is taken to stay at an exposed place."}})

(defn cannot-heal?
  "Whether the body of ctx c can not heal by waiting: food under foods/top-up-food and nothing a meal would eat carried (reserved food, which the hungry trigger and an unnamed meal leave alone, does not count)."
  [c]
  (let [self (.self (:primitives c))]
    (and (< (.-food self) foods/top-up-food)
         (not (foods/carries-food? self (reserve/spared (ctx/view c) self))))))

(defn allowed
  "The hp of expected damage the body may take on a risk now. With `worth` (seconds the caller counts the goal worth, the call's
  :worth, when above 0): as many hp as that buys at the price of an hp now (dearer when hurt), all but 1 at most; a goal worth
  little is refused a dear trip, whatever the share of health or the food (death is a cost). Without one: ::risk-share of the
  health (cut while hungry), all but 1 hp while the body can not heal by waiting (cannot-heal?: waiting never lowers the price)."
  ([c] (allowed c (:worth (:args c))))
  ([c worth]
   (let [self (.self (:primitives c))
         health (.-health self)]
     (if (and (number? worth) (pos? worth))
       (min (dec health) (/ worth (cost/hp-price health)))
       (let [hungry? (some-> (.-food self) (<= (settings/get settings ::hungry-food)))
             share (* (settings/get settings ::risk-share) health (if hungry? (settings/get settings ::hungry-share) 1))]
         (if (cannot-heal? c) (max share (dec health)) share))))))

(defn stay-hp
  "Expected damage of staying `seconds` where mobs spawn at `chance` per second, each a zombie fight with the kit carried."
  [c chance seconds]
  (* chance seconds (cost/fight-damage (assoc (wworld/kit-of c) :mobs [{:name "zombie" :distance 0 :hits 0}]))))

(defn round1 [x] (/ (js/Math.round (* 10 x)) 10))

(def stay-settings {:click ::place-stay-s :work ::work-stay-s :long ::long-stay-s})

(defn with-stay
  "The checker `safe` run with (stay-of c) seconds as the ctx's stay (kept when keep? and one is set); its :jobs.lib.ready/exposure half too."
  [safe stay-of keep?]
  (let [run (fn [g] (fn [c] (g (if (and keep? (::stay-s c)) c (assoc c ::stay-s (stay-of c))))))]
    (with-meta (run safe) (update (meta safe) :jobs.lib.ready/exposure #(some-> % run)))))

(defn staying
  "The checker `safe`, run with the stay of category (:click, :work or :long) unless the caller gave one."
  [category safe]
  (with-stay safe (fn [_] (settings/get settings (stay-settings category))) true))

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

(defn stays
  "The checker `safe` of a job that stays put or visits a place, run with (stay-s c) seconds as its stay there: `priced` prices an
  exposed place by it, so a short act in the dark goes ahead (Q382) and a long one waits."
  [safe stay-s]
  (with-stay safe stay-s false))

(def chest-act-s "Seconds a chest visit (deposit, withdraw) keeps the body at the chest." 3)

(defn trip-s
  "stay-s of a job that walks to the place (f c) (nil: here), acts act-s seconds there and walks back."
  [f act-s]
  (fn [c] (let [at (f c)]
            (+ act-s (if (and at (:y at)) (/ (* 2 (u/dist (u/self-pos c) at)) (cost/walk-blocks-per-s)) 0)))))
