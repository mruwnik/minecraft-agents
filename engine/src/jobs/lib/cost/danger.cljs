(ns jobs.lib.cost.danger
  "Danger of mobs the body knows (an input, e.g. jobs.lib.danger/known-hostiles; no world entity reads): route-danger, the
  expected damage of walking a route past them (recover-drops' fetch cost), and danger-rate / threat-factor, what a
  second within a known mob's reach costs go-to's planner (jobs.lib.threats/threat-tolls).

  route-danger per mob: hostile (jobs.lib.cost.threat; provoked-only mobs x0.1) or named in overrides; mob-hurt over 3 s
  after armour, dearer for a body that cannot afford to fight it (no-answer-factor; not for a provoked-only mob, which is never fought, and a mob with :angry false adds none);
  weighted by the most its reach (its way and line of fire, jobs.lib.threat-field) weighs a cell of the route, none
  outside it. Overrides {mob-name number-or-{:times n}}: threat before armour.

  exposure-price: the seconds a second exposed to mobs that reach the body costs (hurt-rate, the hp a second, at the hp price).

  danger-rate: mob-hurt over 1 s x stance factor (:flee x4, :fight x0.1), at most max-rate: hp a second (the factors and the
  cap are settings, danger-opts); threat-factor prices it at the hp price of the body's health."
  (:require [engine.args :as a]
            [engine.constants :as constants]
            [engine.settings :as settings]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::exposure-s {:default 3 :doc "Seconds a body is exposed to a mob on a route." :spec (a/int-in 0 nil)}
   ::snap-span {:default 4 :doc "How far up or down a straight route looks for ground in each column." :spec (a/int-in 0 nil)}
   ::sprint-speed {:default 5.6 :doc "Blocks a second a sprinting body covers (reach-first?)." :spec (a/num-in 0.1 nil)}
   ::walk-speed {:default 4.3 :doc "Blocks a second a walking body covers, one too hungry to sprint (reach-first?)." :spec (a/num-in 0.1 nil)}
   ::flee-factor {:default 4 :doc "Danger price: a known mob the body would flee costs this times its hp a second." :spec (a/num-in 0 nil)}
   ::fight-factor {:default 0.1 :doc "Danger price: a known mob the body would fight costs this times its hp a second." :spec (a/num-in 0 nil)}
   ::max-rate {:default 4 :doc "Danger price: hp a second one known danger costs at most; above 4 it also raises the total of all dangers a second (the planner's dangerCap)." :spec a/pos-num?}
   ::sprint-food {:default 6 :doc "Food at or under which a body cannot sprint (reach-first?)." :spec (a/int-in 0 20)}
   ::chase-speed {:default 3 :doc "Blocks a second a chasing melee mob covers, a little over a zombie's (reach-first?)." :spec (a/num-in 0.1 nil)}
   ::baby-chase-speed {:default 5.1 :doc "Blocks a second a baby zombie covers chasing (chase-speed of mob-kind \"baby\")." :spec (a/num-in 0.1 nil)}
   ::flight-starts {:default [0 0.08 0.13 0.22 0.32 0.45 0.63 0.88 1.25 2.28] :doc "Seconds from a flight's decision until it gets going, its deciles (flight-starts): measured 2026-10-23 over the trial bodies' first flee decisions from a creeper (n=119, mean 0.64; any mob n=311, mean 0.76) as the time to 1 block off less that block at sprint speed. A blast on the walk is priced over each (cost.answer/flee-mob)." :spec (a/coll-of number?)}
   ::outrun-factor {:default 1.5 :doc "Times a mob's chase speed the body must run to get away from it on foot (outruns?): a flight goes in steps and round obstacles, the mob comes straight on." :spec (a/num-in 0 nil)}
   ::no-answer-scale {:default 60 :doc "How much dearer a mob's route danger is for each time over what the body can afford to lose in a fight with it (no-answer-factor)." :spec (a/num-in 0 nil)}
   ::race-margin-s {:default 0.5 :doc "Seconds the body must win a race to a refuge by (reach-first?)." :spec (a/num-in 0 nil)}
   ::melee-reach {:default 1.4 :doc "Blocks from the body within which a melee mob's blow lands, vanilla's for a zombie (catches?)." :spec (a/num-in 0 nil)}})


;; ---------------------------------------------------------------- reading inputs

(defn mob-of
  "{:name :pos {:x :y :z} :kind} of a mob given as a cljs map or a JS entity."
  [m]
  (if (map? m)
    (select-keys m [:name :pos :kind :id :angry])
    (let [pos (.-pos m)]
      {:name (.-name m) :kind (.-kind m) :id (.-id m) :angry (.-angry m) :pos (when pos {:x (.-x pos) :y (.-y pos) :z (.-z pos)})})))

(defn override-fn [o]
  (cond (number? o) (constantly o)
        (map? o) (let [t (or (:times o) (get o "times"))] (if (number? t) #(* t %) identity))
        :else identity))

(defn centre [{:keys [x y z]}] {:x (+ (js/Math.floor x) 0.5) :y (js/Math.floor y) :z (+ (js/Math.floor z) 0.5)})

;; ---------------------------------------------------------------- route danger

(defn exposure-s [] (settings/get settings ::exposure-s))

(defn no-answer-scale [] (settings/get settings ::no-answer-scale))

(defn no-answer-factor
  "How much dearer a mob's danger is for a body with weapon (item name, nil a fist) and health that cannot afford to
  fight it: 1 when fight-damage (alone, at distance) leaves the fight reserve, else 1 + no-answer-scale x (damage over
  the health above the reserve, less 1). Nil weapon and health count as a fist at 20."
  [equipment weapon health name distance]
  (let [damage (fight/fight-damage {:weapon weapon :equipment equipment :mobs [{:name name :distance distance}]})
        affordable (max 1 (- (if (number? health) health 20) fight/default-reserve))
        ratio (/ damage affordable)]
    (if (> ratio 1) (+ 1 (* (no-answer-scale) (- ratio 1))) 1)))

(defn route-danger
  "{:danger total :mobs [{:name :pos :distance :weight :threat :danger} ...]} of walking route ([{:x :y :z}] cells)
  past mobs (cljs maps {:name :pos} or JS entities) for a body wearing equipment. Lists only mobs that add danger, the
  worst first. version: the minecraft-data version; reach-of: a fn of a mob {:name :pos} to its reach (a fn of a cell x y
  z, its weight 0-1, nil outside: jobs.lib.threat-field). Options: :overrides, :weapon (the item held or to hold,
  default a fist) and :health (default 20): see no-answer-factor. See the ns doc."
  [version reach-of route mobs equipment & {:keys [overrides weapon health]}]
  (let [overrides (into {} (map (fn [[k o]] [(if (keyword? k) (name k) (str k)) o])) overrides)
        stats (armour/armour-stats equipment)
        route (vec route)
        rows (when (seq route)
               (for [m (map mob-of mobs)
                     :when (and (string? (:name m)) (:pos m)
                                (not (false? (:angry m)))
                                (or (contains? overrides (:name m)) (threat/hostile? version m)))
                     :let [reach (reach-of m)
                           weight (reduce max 0 (keep (fn [{:keys [x y z]}] (reach (js/Math.floor x) (js/Math.floor y) (js/Math.floor z))) route))
                           d (reduce min ##Inf (map #(u/dist (:pos m) (centre %)) route))
                           share (if (contains? threat/provoked-only (:name m)) (threat/provoked-share) 1)
                           threat-fn #(* share ((override-fn (get overrides (:name m))) %))
                           threat (threat-fn (:threat (threat/base-threat (:name m) (exposure-s))))]
                     :when (and (pos? weight) (pos? threat))
                     :let [hurt (threat/mob-hurt stats (:name m) (exposure-s) threat-fn)
                           scale (if (contains? threat/provoked-only (:name m)) 1 (no-answer-factor equipment weapon health (:name m) d))]]
                 {:name (:name m) :pos (:pos m) :distance d :weight weight :threat threat :danger (* weight hurt scale)}))
        rows (vec (sort-by :danger > rows))]
    {:danger (reduce + 0 (map :danger rows)) :mobs rows}))

(defn snap-span [] (settings/get settings ::snap-span))

(defn straight-route
  "The cells [{:x :y :z}] one a block from the cell of from to the cell of to on a straight line, each put on the
  ground of kind-at nearest the cell before it (within 4 up or down: feet and head free, something under), else at
  that cell's height. A cheap stand-in for a planned route when only the danger along the way is wanted."
  [kind-at from to]
  (let [[fx fy fz] (map js/Math.floor [(:x from) (:y from) (:z from)])
        [tx _ tz] (map js/Math.floor [(:x to) (:y to) (:z to)])
        n (max (js/Math.abs (- tx fx)) (js/Math.abs (- tz fz)))
        ground (fn [x prev z]
                 (or (some #(when (reach/standable? kind-at x % z) %)
                           (cons prev (mapcat (fn [k] [(+ prev k) (- prev k)]) (range 1 (inc (snap-span))))))
                     prev))]
    (if (zero? n)
      [{:x fx :y fy :z fz}]
      (loop [i 0 prev fy out []]
        (if (> i n)
          out
          (let [x (+ fx (js/Math.round (/ (* i (- tx fx)) n)))
                z (+ fz (js/Math.round (/ (* i (- tz fz)) n)))
                y (ground x prev z)]
            (recur (inc i) y (conj out {:x x :y y :z z}))))))))

;; ---------------------------------------------------------------- the planner's danger rate

(def max-dangers 8)
(def stances "Rate factor: :flee from a mob costs the body far more than one it would :fight." {:flee 4 :fight 0.1})
(def max-rate "hp a second one danger costs at most." 4)
(def danger-cap "hp a second all known dangers together cost at most (the planner's dangerCap); a larger ::max-rate raises it." 4)

(defn danger-opts
  "The danger options as the {:stances :max-rate} that danger-rate takes, from the settings ::flee-factor,
  ::fight-factor and ::max-rate."
  []
  {:stances {:flee (settings/get settings ::flee-factor) :fight (settings/get settings ::fight-factor)}
   :max-rate (settings/get settings ::max-rate)})

(defn stance
  "How the planner prices mob-name alone: :fight when a fight with it leaves fight/default-reserve health (never a creeper), else :flee."
  [{:keys [health equipment weapon]} mob-name]
  (if (and (not= "creeper" mob-name)
           (fight/leaves-reserve? {:health health :reserve fight/default-reserve
                                   :damage (fight/fight-damage {:weapon weapon :equipment equipment :mobs [{:name mob-name :distance 0}]})}))
    :fight
    :flee))

(defn hurt-rate
  "hp a second mob-name takes off a body wearing equipment while it can reach it."
  [equipment mob-name]
  (threat/mob-hurt (armour/armour-stats equipment) mob-name 1))

(defn danger-rate
  "hp a second near mob-name costs body {:health :equipment :weapon} (see the ns doc). opts: danger-opts (default none)."
  ([body mob-name] (danger-rate body mob-name nil))
  ([{:keys [equipment] :as body} mob-name opts]
   (min (get opts :max-rate max-rate)
        (* (hurt-rate equipment mob-name)
           (get (merge stances (:stances opts)) (stance body mob-name))))))

(defn exposure-price
  "Seconds each second spent exposed costs body {:health :equipment} to mob-names that can reach it: their hurt-rates
  summed, at health/hp-price. 0 with none."
  [{:keys [health equipment]} mob-names]
  (* (reduce + 0 (map #(hurt-rate equipment %) mob-names)) (health/hp-price (if (number? health) health 20))))

(defn threat-factor
  "Seconds more each second within a known mob's reach costs body {:health :equipment :weapon}: mob-name's danger-rate x
  weight (0-1: a fading memory, the cell's place in its field) at the hp price of the body's health (the planner's
  options.tolls factor)."
  [{:keys [health] :as body} mob-name weight]
  (* weight (danger-rate body mob-name (danger-opts)) (health/hp-price (if (number? health) health 20))))

(defn threat-factor-cap
  "The most threat-factor all known mobs together cost a cell for body: the danger cap (or a larger ::max-rate) at the hp
  price of its health."
  [{:keys [health]}]
  (* (max danger-cap (:max-rate (danger-opts))) (health/hp-price (if (number? health) health 20))))

;; ---------------------------------------------------------------- the race to a refuge

(defn body-speed
  "Blocks a second a body with food food covers running: ::sprint-speed, ::walk-speed at ::sprint-food or under."
  [food]
  (settings/get settings (if (> food (settings/get settings ::sprint-food)) ::sprint-speed ::walk-speed)))

(defn chase-speed
  "Blocks a second mob-name covers chasing the body: engine.constants/mob-speeds, ::baby-chase-speed for \"baby\" (mob-kind), ::chase-speed for a mob not listed there."
  [mob-name]
  (if (= mob-name "baby")
    (settings/get settings ::baby-chase-speed)
    (get constants/mob-speeds mob-name (settings/get settings ::chase-speed))))

(def water-speeds
  "Blocks a second a mob swims chasing a body in water, where the game makes it a swimmer (vanilla): a drowned under water
  pushes 0.01 (its travel) + 0.005 (its move control, toward a path node about a block off) times its speed 0.23 a tick
  against a drag of 0.9 a tick, about 0.0345 blocks a tick."
  {"drowned" 0.69})

(defn swim-chase-speed
  "Blocks a second mob-name covers chasing a body that swims: water-speeds, else chase-speed."
  [mob-name]
  (get water-speeds mob-name (chase-speed mob-name)))

(defn mob-kind
  "The key of engine.constants/mob-speeds for hostile entity e: \"baby\" for a baby, else its name."
  [e]
  (if (true? (.-baby e)) "baby" (.-name e)))

(defn wins-race?
  "Whether a body body-s seconds from a place gets there margin-s (default ::race-margin-s) before a chaser arrive-s
  seconds from it."
  ([body-s arrive-s] (wins-race? body-s arrive-s (settings/get settings ::race-margin-s)))
  ([body-s arrive-s margin-s] (< (+ body-s margin-s) arrive-s)))

(defn reach-first?
  "Whether a body with food food (body-speed), body-dist blocks from a refuge, gets there ::race-margin-s before every
  chaser, each (mob-dists) that many blocks from it at ::chase-speed, or its own chase-speed when that is more
  (mob-names, in the order of mob-dists). True with no chaser."
  ([body-dist mob-dists food] (reach-first? body-dist mob-dists food (repeat nil)))
  ([body-dist mob-dists food mob-names]
   (let [body-s (/ body-dist (body-speed food))
         floor (settings/get settings ::chase-speed)]
     (every? true? (map #(wins-race? body-s (/ %1 (max floor (chase-speed %2)))) mob-dists mob-names)))))

(defn flight-speed
  "Blocks a second a flight gains away from its start: body-speed over ::outrun-factor."
  [food]
  (/ (body-speed food) (settings/get settings ::outrun-factor)))

(defn melee-reach [] (settings/get settings ::melee-reach))
(defn outrun-factor [] (settings/get settings ::outrun-factor))
(defn flight-starts [] (settings/get settings ::flight-starts))

(defn outruns?
  "Whether a body with food food gets away from every one of mob-names on foot: its flight-speed is at least each one's
  chase-speed. A spider is faster at any food, a zombie never."
  [food mob-names]
  (every? #(>= (flight-speed food) (chase-speed %)) mob-names))

(defn arrival-s
  "Seconds a chaser (mob-kind name) at pos takes to come within ::melee-reach of `at` ({:x :y :z}) at its chase-speed
  (at least ::chase-speed)."
  [chaser pos at]
  (/ (max 0 (- (u/dist pos at) (melee-reach)))
     (max (settings/get settings ::chase-speed) (chase-speed chaser))))

(defn caught-on?
  "Whether a melee chaser (mob-kind name) at pos lands a blow on a body walking route ([{:x :y :z :t}], :t the seconds
  from the start the body is there): at some point of it the chaser arrives (arrival-s) before the body wins the race
  there (wins-race?, by margin-s: default ::race-margin-s)."
  [{:keys [chaser pos route margin-s]}]
  (let [margin (or margin-s (settings/get settings ::race-margin-s))]
    (boolean (some #(not (wins-race? (:t %) (arrival-s chaser pos %) margin)) route))))

(defn catches?
  "Whether a melee chaser (mob-name, chaser its mob-kind) at pos lands a blow on a body with food food walking the straight
  way from `from` to `to` ({:x :y :z}) before it is past (caught-on? over the way, a point every step blocks, at
  body-speed). A mob at the heels or ahead by the way catches; one the walk leaves behind does not."
  [{:keys [chaser pos from to food step]}]
  (let [len (u/dist from to)
        n (js/Math.ceil (/ len step))
        speed (body-speed food)]
    (caught-on? {:chaser chaser :pos pos
                 :route (for [i (range (inc n))
                              :let [t (if (zero? n) 0 (/ i n))]]
                          (assoc (merge-with #(+ %1 (* t (- %2 %1))) (select-keys from [:x :y :z]) (select-keys to [:x :y :z]))
                                 :t (/ (* t len) speed)))})))
