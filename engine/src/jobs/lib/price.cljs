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
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger]
            [jobs.lib.foods :as foods]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::risk-share {:default 0.5 :spec (a/num-in 0 nil)
                 :doc "Share of the health the body has that the expected damage of a walk (spawns met on its dark open stretches, hostiles near its way) or of a stay at an exposed place may take; above it the start check waits."}
   ::way-radius {:default 16 :spec (a/num-in 0 nil)
                 :doc "Blocks from a way or place within which a known hostile adds its fight to the price of walking or staying there."}
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

(defn stay-parts
  "{:hurt :dies} of staying `seconds` where mobs spawn at `chance` per second: hurt the hp they take (cost/spawns-hurt),
  dies the chance they kill the body (cost/spawns-chance)."
  [c chance seconds]
  (let [body (threats/death-body c)
        at (u/self-pos c)
        spawns (* chance seconds)]
    (check-cache/keyed c [::stay spawns body at]
                       (fn [] {:hurt (cost/spawns-hurt body spawns at)
                               :dies (cost/spawns-chance body spawns at)}))))

(defn stay-hp
  "The hp staying `seconds` where mobs spawn at `chance` per second risks: stay-parts' hurt and its death in hp (cost/lethal-hp)."
  [c chance seconds]
  (let [{:keys [hurt dies]} (stay-parts c chance seconds)]
    (+ hurt (cost/lethal-hp (threats/death-body c) dies (u/self-pos c)))))

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

(defn way-fight-hp
  "hp of the fight with every hostile known within ::way-radius of `pos` ({:x :y :z}), with the kit carried."
  [c pos]
  (let [p (:primitives c)
        mobs (danger/hostiles-near-way p pos (settings/get settings ::way-radius) {})]
    (if (seq mobs)
      (cost/fight-damage (assoc (wworld/kit-of c) :mobs (mapv (fn [e] {:name (.-name e) :distance (danger/mob-distance p e) :hits 0}) mobs)))
      0)))

(defn ^:async walk-back-worth?
  "Whether walking back to cell ([x y z]) to go on with a job there costs less than redo-s seconds of its work done again
  where the body stands: go-to quotes a whole route there taking no longer."
  [c cell redo-s]
  (let [r (:route (await (ctx/quote c 'jobs.movement.go-to {:pos (zipmap [:x :y :z] cell) :range 0})))]
    (boolean (and (:found r) (number? (:seconds r)) (<= (:seconds r) redo-s)))))
