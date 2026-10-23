(ns jobs.lib.cost.answer
  "The one price of each answer to hostiles, so the reflexes take the cheapest (decide). Pure.

  An answer is {:damage hp :hit hp :blasts [hp] :spread [hp] :seconds s :then [answer] :caught {:chance :damage :hit :blasts}}:
  the hp it is expected to cost, the creeper blasts in it landing for certain (:spread: as many equally likely blast
  totals) and the rest in blows of about :hit hp each, the seconds it takes, what may follow it (the cheapest of :then is paid too) and,
  for a walk away, the chance its mobs catch it and the blows they then land. answer-price: the hp at the health each
  leaves (health/hp-cost), the death the blows risk (death-chance x death/death-cost), the seconds, and the cheapest
  :then at full health.

  death-chance: the one chance of dying every route, spot, wait or answer is priced by. A known threat on a route costs
  the death of the answer the body would take to it (decide over fight, walk away and the covers it has).

  fight-answer: a fight with the kit (fight/fight-damage, fight-seconds); a mob out of reach is left alive, hitting all
  the fight (followed: what comes at once after it). A lone creeper fought hit-and-back risks the
  blast of a late step back: its :hit is the blast at the creeper's swell start.
  flee-answer: a walk away from the mobs, nil when one of them keeps up (outruns?: the flight's speed is body-speed over
  ::outrun-factor). A melee mob hits while within its reach, a ranged one while the body is within its bow's range (both
  follow at their chase speed; a body in water first swims to land against their swim speed), a creeper that reaches its swell start before the body gets going
  (danger/flight-starts) blows at the distance the body reached when its fuse ends. The mob is left alive:
  :then the fight with it, or the same walk once more. A stub until the priced flee search (card ee045b60).
  build-answer: building a refuge (seal or pillar) for some seconds: the blows of the melee mobs that arrive meanwhile.
  cover-answer: shutting the body in (walls, pit or pocket) by build-answer, then hidden for the quiet.

  Game facts kept as constants: a creeper's explosion power (3; damage falls off to none at twice it), its swell start
  (3 blocks) and fuse (1.5 s), a skeleton's bow range (15 blocks), a trident's throw (10 blocks)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.planner :as planner]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(a/defargs settings
  {::caught-chance {:default 0.15 :spec (a/num-in 0 1)
                    :doc "Chance a walk away is caught, its mobs landing their blows over the whole flight: the prior for a flight whose route is unknown, measured from the trial logs on 2026-10-23 (n=63)."}
   ::drowned-meet-per-s {:default 0.0021 :spec (a/num-in 0 nil)
                         :doc "Chance a second a drowned meets a body in water a drowned can spawn in (its column holds water over water at block light 0, look/water-fn), by day or night: (k+1)/n over the trial bodies' logs on 2026-10-23, k=11 meets in n=5767 s, each meet once (a respond run soon after another is the same meet) and classed by the body's cell (tools/event-counts.mjs drowned-meets :prior :deep; river or ocean and other biomes, sky light high or low, met it alike)."}
   ::drowned-meet-other-per-s {:default 0.00043 :spec (a/num-in 0 nil)
                               :doc "As ::drowned-meet-per-s in any other water (one deep, or block-lit to its floor): k=1 in n=4602 s (:prior :other)."}
   ::unseen-water-lags {:default [{:k 1072482 :n 1095455} {:k 1054916 :n 1093513} {:k 1038865 :n 1091547} {:k 1024204 :n 1089545}
                                  {:k 1010196 :n 1087526} {:k 996729 :n 1085500} {:k 983992 :n 1083481} {:k 971757 :n 1081465}
                                  {:k 960096 :n 1079468} {:k 948840 :n 1077476} {:k 937962 :n 1075465} {:k 927498 :n 1073431}
                                  {:k 917387 :n 1071419} {:k 907520 :n 1069423} {:k 897880 :n 1067429} {:k 888501 :n 1065430}
                                  {:k 879531 :n 1063558} {:k 870863 :n 1061652} {:k 862325 :n 1059744} {:k 853928 :n 1057848}
                                  {:k 845800 :n 1055963} {:k 837921 :n 1054111} {:k 830319 :n 1052278} {:k 822685 :n 1050429}]
                        :spec (a/coll-of (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)}))
                        :doc "For lag d (the d-th entry), of n cells d blocks along an axis from a surface water cell at its level, k water too: the prior that an unseen cell is water when the nearest seen cell along an axis at its level, d off, is water (unseen-water-factors). Oceans dominate it. Recount: node tools/event-counts.mjs water-lags --stride 20 (4017 recorded chunk columns, 2026-10-23)."}})

(def blast-power "A creeper's explosion power (vanilla)." 3)
(def swell-start "Blocks within which a creeper starts to swell (vanilla)." 3)
(def fuse-s "Seconds from a creeper's first swell to its blast (vanilla: 30 ticks)." 1.5)
(def bow-range "Blocks within which a skeleton shoots (vanilla)." 15)
(def trident-range "Blocks within which a drowned holding a trident throws it (vanilla)." 10)

(defn hit-range
  "Blocks from the body within which mob {:name :held} lands its blows: a trident's throw, a bow's shot, else a melee reach."
  [{:keys [name] :as m}]
  (cond (= threat/trident-kind (threat/kind m)) trident-range
        (contains? weapon/ranged-mobs name) bow-range
        :else (danger/melee-reach)))

(defn blast-damage
  "hp a creeper's blast deals a body distance blocks from it, before armour (vanilla, full exposure: none at twice the power), at the server difficulty."
  [distance]
  (let [impact (- 1 (/ distance (* 2 blast-power)))]
    (if (pos? impact)
      (threat/difficulty-scale (js/Math.floor (+ 1 (* (/ (+ (* impact impact) impact) 2) 7 2 blast-power))))
      0)))

(defn blast-hurt
  "blast-damage at distance after the armour stats."
  [stats distance]
  (armour/after-armour stats :explosion (blast-damage distance) 1))

(defn blow
  "hp one blow of mob {:name :held} deals through the armour stats: a creeper's is its blast at its swell start."
  [stats m]
  (if (= "creeper" (:name m))
    (blast-hurt stats swell-start)
    (threat/mob-hurt stats (threat/kind m) 1 identity 1)))

(defn mean [xs] (if (seq xs) (/ (reduce + xs) (count xs)) 0))

(defn blows-chance
  "The chance the blows of answer kill body: death/blow-chance of its :damage, its :blasts sure (each of its :spread too,
  averaged), or of the :caught blows for its :chance."
  [body {:keys [caught] :as answer}]
  (let [chance (fn [{:keys [damage hit blasts spread]}]
                 (let [sure (reduce + 0 blasts)
                       small (- damage sure (mean spread))]
                   (mean (map #(death/blow-chance body (+ small sure %) hit (+ sure %)) (or (seq spread) [0])))))
        clean (chance answer)]
    (if caught
      (+ (* (- 1 (:chance caught)) clean) (* (:chance caught) (chance caught)))
      clean)))

(defn expected-damage [{:keys [damage caught]}]
  (if caught (+ (* (- 1 (:chance caught)) damage) (* (:chance caught) (:damage caught))) damage))

(defn healed [body] (assoc body :health 20 :absorption 0))

(defn after
  "body once answer's expected blows have landed (at least 1 hp left: a death in them is the answer's own)."
  [{:keys [health] :as body} answer]
  (assoc body :health (max 1 (- (or health 20) (expected-damage answer)))))

(defn answer-price
  "Seconds answer costs body {:health :absorption, death-cost's :respawn :level :carried} at `at` (see the ns doc); :then
  is priced at full health: the body meets the mob again once it has healed. :outcomes [[chance answer]] (a look's,
  jobs.lib.cost.look) are each paid by its chance, at the health the answer's blows leave."
  [{:keys [health] :as body} {:keys [seconds then outcomes] :as answer} at]
  (let [health (or health 20)
        own (+ (health/hp-cost health (min (expected-damage answer) health))
               (* (blows-chance body answer) (death/death-cost body at))
               seconds)]
    (cond
      (seq outcomes) (+ own (reduce + (map (fn [[chance a]] (* chance (answer-price (after body answer) a at))) outcomes)))
      (seq then) (+ own (apply min (map #(answer-price (healed body) % at) then)))
      :else own)))

(defn answer-death-chance
  "The chance answer kills body: its blows (blows-chance), then the cheapest of its :then (answer-price at `at`) at full
  health, or each of its :outcomes by its chance."
  [body {:keys [then outcomes] :as answer} at]
  (let [own (blows-chance body answer)
        next (when (seq then) (apply min-key #(answer-price (healed body) % at) then))]
    (cond
      (seq outcomes) (- 1 (* (- 1 own) (- 1 (reduce + (map (fn [[chance a]] (* chance (answer-death-chance (after body answer) a at))) outcomes)))))
      next (- 1 (* (- 1 own) (- 1 (answer-death-chance (healed body) next at))))
      :else own)))

(defn decide
  "{:answer key :prices {key seconds}}: the cheapest of answers {key answer-or-nil} (nil: not open to the body) by
  answer-price; nil when none is open."
  [body answers at]
  (let [prices (into {} (keep (fn [[k a]] (when a [k (answer-price body a at)]))) answers)]
    (when (seq prices)
      {:answer (key (first (sort-by val prices))) :prices prices})))

(defn fight-answer
  "The answer of fight (fight/fight-damage's argument): its damage and seconds, :hit the hardest blow of its mobs. A mob
  with :reach? false (the body cannot get within a swing of it) is not fought: its blows land over the whole fight, the
  fight's length as if it were fought when no mob is in reach; :left the mobs the fight leaves alive."
  [{:keys [equipment mobs] :as fight}]
  (let [stats (armour/armour-stats equipment)
        {left false fought true} (group-by #(not (false? (:reach? %))) mobs)
        fought (vec fought)
        seconds (fight/fight-seconds (assoc fight :mobs (if (seq fought) fought mobs)))]
    (cond-> {:damage (+ (fight/fight-damage (assoc fight :mobs fought))
                        (reduce + 0 (map #(threat/mob-hurt stats (threat/kind %) seconds) left)))
             :hit (reduce max 0 (map #(blow stats %) mobs))
             :seconds seconds}
      (seq left) (assoc :left (vec left)))))

(defn kill-price
  "Seconds killing one mob-name met beside body costs at `at`: answer-price of fighting it with the body's :weapon and
  :equipment."
  [{:keys [weapon equipment] :as body} mob-name at]
  (answer-price body (fight-answer {:weapon weapon :equipment equipment :mobs [{:name mob-name :distance 0}]}) at))

(defn followed
  "Answer a, then answer b at once (nil: a once more, later): the blows and seconds of both, b's chance of being caught
  (a's blows landed either way) and what follows b."
  [a b]
  (if b
    (cond-> (assoc a :damage (+ (:damage a) (:damage b)) :hit (max (:hit a) (:hit b)) :seconds (+ (:seconds a) (:seconds b))
                   :blasts (into (vec (:blasts a)) (:blasts b)) :spread (:spread b))
      (:caught b) (assoc :caught (-> (:caught b) (update :damage + (:damage a)) (update :blasts #(into (vec (:blasts a)) %))))
      (:then b) (assoc :then (:then b))
      (not (:then b)) (dissoc :then))
    (assoc a :then [a])))

(defn swim-leg
  "{:in-reach s :gap blocks :seconds s :away? bool} of swimming blocks at speed sv from a mob gap blocks off that swims at
  ms and hits within r: the seconds it is within r, the gap at the end, and the seconds taken, cut short (:away?) where
  the gap reaches follow."
  [sv ms gap r follow blocks]
  (let [gain (- sv ms)
        t (/ blocks sv)
        out (cond (>= gap follow) 0 (pos? gain) (/ (- follow gap) gain))
        t (if (and out (< out t)) out t)
        in-reach (cond (pos? gain) (min t (/ (max 0 (- r gap)) gain))
                       (< gap r) t
                       (zero? gain) 0
                       :else (max 0 (- t (/ (- gap r) (- gain)))))]
    {:in-reach in-reach :gap (max 0 (+ gap (* gain t))) :seconds t :away? (boolean (and out (= t out)))}))

(defn flee-mob
  "{:damage :hit :spread :seconds} of a walk away from mob {:name :held :distance :follow (blocks it keeps chasing out to) :speed
  (its chase speed, default danger/chase-speed of the name)} at speed v through the armour stats, nil when it keeps up.
  It hits while within its hit-range. A flight that starts in water first swims swim blocks to dry land (##Inf: no land
  known) at speed sv, the mob chasing at danger/swim-chase-speed, then walks from the gap the swim left, over the land blocks
  there is to walk on (a ledge with none: the mob keeps up). A creeper that
  reaches its swell start before the body gets going blows at the distance the body reached when its fuse ends, :spread
  that blast for each of danger/flight-starts: a blast is not felt before it lands, as other mobs' blows are (respond's
  felt rate)."
  ([stats v m] (flee-mob stats v nil m))
  ([stats v {:keys [sv swim land]} {:keys [name distance follow speed] :as m}]
   (let [d (or distance 0)
         follow (or follow d)
         chase (or speed (danger/chase-speed name))
         r (hit-range m)
         wet (when (and swim (pos? swim) (not= "creeper" name))
               (swim-leg sv (danger/swim-chase-speed name) d r follow swim))
         d (if wet (:gap wet) d)
         gain (- v chase)
         escape (cond (:away? wet) 0
                      (and wet (not (js/isFinite swim))) nil
                      (pos? gain) (/ (max 0 (- follow d)) gain))
         escape (when-not (and wet escape land (< land (* v escape))) escape)]
     (cond
       (nil? escape) nil
       (= "creeper" name)
       (let [starts (danger/flight-starts)
             swell (/ (max 0 (- d swell-start)) chase)
             spread (mapv #(if (< swell %)
                             (blast-hurt stats (+ (min d swell-start) (* v (max 0 (- fuse-s (- % swell))))))
                             0)
                          starts)]
         {:damage (mean spread) :hit 0 :spread spread :seconds (+ (mean starts) escape)})
       :else
       {:damage (threat/mob-hurt stats (threat/kind m) (+ (:in-reach wet 0) (if (:away? wet) 0 (/ (max 0 (- r d)) gain))) identity (threat/lone-hit-rate (threat/kind m)))
        :hit (blow stats m) :seconds (+ (:seconds wet 0) escape)}))))

(declare flee-answer-on)

(defn flee-answer
  "flee-answer-on over the land known; when that is no answer and open (the chance, 0..1, that land goes on past it, cells
  never seen) is given, the answer over land enough, in which a flight whose land ends instead (chance 1 - open) is caught
  on the shore as well, taking the blows of the fight ashore (ashore, else later; the flight's own when none)."
  [{:keys [open later ashore] :as args}]
  (let [args (dissoc args :open :ashore)
        later (or ashore later)]
    (or (flee-answer-on args)
        (when open
          (when-let [a (flee-answer-on (assoc args :land ##Inf))]
            (let [{c0 :chance d0 :damage} (:caught a)
                  ends (* (- 1 c0) (- 1 open))
                  chance (+ c0 ends)]
              (if (zero? chance)
                a
                (-> a
                    (assoc-in [:caught :chance] chance)
                    (assoc-in [:caught :damage] (/ (+ (* c0 d0) (* ends (:damage later d0))) chance))
                    (update-in [:caught :hit] max (:hit later 0))))))))))

(defn flee-answer-on
  "The answer of a walk away from mobs [flee-mob's] by a body with food wearing equipment, the walk there and back (it
  goes back to its work), times 1 + returns (the flights from such mobs lately: each predicts another); :then the answer
  later (the fight with them, nil none) or the walk again. :caught: each flight is caught at chance caught (default
  ::caught-chance), its mobs then landing their blows (a creeper its blast) over the flight. A creeper that blows on the
  walk (at the median of its :spread) is spent: what follows is the walk from the rest or later (none left: nothing).
  swim: the blocks a body in water swims to dry land first (flee-mob's; ##Inf none known) and land the blocks to walk on there, at its planner/swim-speed over danger's outrun factor as on foot.
  nil when a mob keeps up or none is given."
  [{:keys [food equipment mobs returns later caught swim land] :as args}]
  (let [stats (armour/armour-stats equipment)
        v (danger/flight-speed (or food 20))
        wet {:swim swim :land land :sv (/ (planner/swim-speed equipment) (danger/outrun-factor))}
        per (map #(flee-mob stats v wet %) mobs)
        times (inc (or returns 0))]
    (when (and (seq per) (every? some? per))
      (let [hit (reduce max 0 (map :hit per))
            walk {:damage (* times (reduce + (map :damage per)))
                  :hit hit
                  :spread (when-let [s (seq (keep :spread per))] (mapv #(* times %) (apply map + s)))
                  :seconds (* 2 times (reduce max 0 (map :seconds per)))
                  :caught {:chance (- 1 (js/Math.pow (- 1 (or caught (settings/get settings ::caught-chance))) times))
                           :damage (reduce + 0 (map (fn [m f] (if (= "creeper" (:name m))
                                                                  (blow stats {:name "creeper"})
                                                                  (threat/mob-hurt stats (threat/kind m) (:seconds f) identity (threat/lone-hit-rate (threat/kind m)))))
                                                    mobs per))
                           :hit hit
                           :blasts (vec (for [m mobs :when (= "creeper" (:name m))] (blow stats m)))}}]
        (let [blows? (fn [{:keys [spread]}] (pos? (nth (sort spread) (quot (count spread) 2))))
              spent (set (keep (fn [[m f]] (when (and (= "creeper" (:name m)) (blows? f)) m)) (map vector mobs per)))
              left (remove spent mobs)]
          (cond
            (empty? spent) (assoc walk :then (cond-> [walk] later (conj later)))
            (seq left) (assoc walk :then (cond-> [(flee-answer-on (assoc args :mobs left))] later (conj later)))
            :else walk))))))

(defn build-answer
  "The answer of building a refuge for seconds against mobs [flee-mob's] wearing equipment: the blows of each melee mob
  from its arrival (its chase speed), and the arrows of each shooter within its bow's range, to the end, at its measured
  hit rate; none from a creeper or an enderman."
  [{:keys [equipment mobs seconds]}]
  (let [stats (armour/armour-stats equipment)
        hitting (remove #(or (#{"creeper" "enderman"} (:name %))
                             (and (threat/ranged? %) (> (or (:distance %) 0) (hit-range %))))
                        mobs)
        left (fn [{:keys [name distance speed] :as m}]
               (if (threat/ranged? m)
                 seconds
                 (max 0 (- seconds (/ (max 0 (- (or distance 0) (danger/melee-reach))) (or speed (danger/chase-speed name)))))))]
    {:damage (reduce + 0 (map #(threat/mob-hurt stats (threat/kind %) (left %) identity (threat/lone-hit-rate (threat/kind %))) hitting))
     :hit (reduce max 0 (map #(blow stats %) hitting))
     :seconds seconds}))

(defn cover-answer
  "The answer of shutting the body in against mobs [flee-mob's] wearing equipment by a cover {:open-s :seconds}
  (jobs.survival.shut-in/covers: the seconds a side stays open, the seconds it takes with the quiet after it): the blows
  of build-answer over open-s; then the same cover once more or one of then. nil with a creeper (its blast takes the walls)."
  [{:keys [equipment mobs then]} {:keys [open-s seconds]}]
  (when-not (some #(= "creeper" (:name %)) mobs)
    (let [a (assoc (build-answer {:equipment equipment :mobs mobs :seconds open-s}) :seconds seconds)]
      (assoc a :then (conj (vec then) a)))))

(defn threat-answers
  "The answers {key answer-or-nil} body {:health :food :equipment :weapon :covers :swim :distance} has to mob-name met
  :distance blocks off (default 0, beside it): the
  fight (in water with :swim, fight/fight-damage's wet?), the walk away (caught at chance caught, nil the prior; :swim, flee-answer's, for a body in water) and each of :covers {way {:open-s :seconds}}
  (jobs.survival.shut-in/covers) by cover-answer, as respond prices them."
  [{:keys [food equipment weapon covers swim distance]} mob-name caught]
  (let [m {:name mob-name :distance (or distance 0) :follow (threat/follow-range mob-name)}
        fight (fight-answer {:weapon weapon :equipment equipment :mobs [m] :wet? (some? swim)})]
    (into {:fight fight
           :flee (flee-answer {:food food :equipment equipment :mobs [m] :caught caught :later fight :swim swim})}
          (map (fn [[way cover]] [way (cover-answer {:equipment equipment :mobs [m]} cover)]))
          covers)))

(defn threat-chance
  "The chance mob-name met beside body kills it at `at`: the answer-death-chance of the answer decide takes of
  threat-answers; a provoked-only mob counts threat/provoked-share."
  [body mob-name caught at]
  (let [answers (threat-answers body mob-name caught)
        {:keys [answer]} (decide body answers at)]
    (* (if (contains? threat/provoked-only mob-name) (threat/provoked-share) 1)
       (answer-death-chance body (get answers answer) at))))

(defn water-factors
  "The planner's extra share of a second in water (options.dark.water) for body {:health :absorption :food :equipment
  :weapon}: [a cell a drowned can spawn in, other water], each its drowned meet rate (::drowned-meet-per-s,
  ::drowned-meet-other-per-s) times the price of the cheapest of threat-answers to a drowned met beside the body: in the
  first swimming with no shore known (it swims away, never walks), in the other standing (mostly a wade one deep)."
  [body]
  (let [price #(apply min (vals (:prices (decide body (threat-answers % "drowned" nil) nil))))]
    [(* (settings/get settings ::drowned-meet-per-s) (price (assoc body :swim ##Inf)))
     (* (settings/get settings ::drowned-meet-other-per-s) (price body))]))

(defn unseen-water-factors
  "The planner's extra share of a second walked over a phantom floor (an unseen floor read as ground) whose nearest seen
  cell along an axis at its level, d blocks off, is water (the d-th entry): the chance it is water too, Beta(1,1) over
  ::unseen-water-lags, times the swim it is then over the walk: planner/swim-s a block (the body's Depth Strider), paying
  the share of water a drowned can spawn in (water-factors' first: its depths are unseen), less the walk's planner/walk-floor."
  [body]
  (let [[wet] (water-factors body)
        swim (/ (* (planner/swim-s (planner/depth-strider (:equipment body))) (inc wet)) planner/walk-floor)]
    (mapv (fn [{:keys [k n]}] (* (/ (inc k) (+ n 2)) (dec swim))) (settings/get settings ::unseen-water-lags))))

(defn life [{:keys [health absorption]}] (+ (or health 20) (or absorption 0)))

(defn threat-hurt
  "hp mob-name met beside body takes off it at `at`: the expected-damage of the answer decide takes of threat-answers, at
  most the body's life (a lethal answer costs the life, its death is death-chance's); a provoked-only mob
  threat/provoked-share of it."
  [body mob-name caught at]
  (let [answers (threat-answers body mob-name caught)]
    (* (if (contains? threat/provoked-only mob-name) (threat/provoked-share) 1)
       (min (life body) (expected-damage (get answers (:answer (decide body answers at))))))))

(defn route-hurt
  "hp the known threats of route {:cells :threats :at} (death-chance's) are expected to take off body, at most its life:
  for each, its threat-hurt at :at (default the first cell) times its weight and the chance it gets to the body on the
  :cells (death/met-chance): death-chance in hp, for a walk priced against health."
  [body {:keys [cells threats at]}]
  (let [at (or at (first cells))
        hurt (memoize #(threat-hurt body %1 %2 at))]
    (min (life body)
         (reduce + 0 (for [{:keys [mob weight reach caught]} (when (seq cells) threats)
                           :let [met (death/met-chance mob reach cells)]
                           :when (pos? met)]
                       (* (or weight 1) met (hurt mob caught)))))))

(defn threats-chance
  "The chance the known threats of route {:cells :threats :at} kill body (death-chance's, the route's class left out): for
  each, its threat-chance at :at (default the first cell) times its weight and the chance it gets to the body on the
  :cells (death/met-chance)."
  [body {:keys [cells threats at]}]
  (let [at (or at (first cells))
        chance (memoize (fn [mob caught] (threat-chance body mob caught at)))
        survive (reduce (fn [s {:keys [mob weight reach caught]}]
                          (let [met (death/met-chance mob reach cells)]
                            (if (pos? met)
                              (* s (- 1 (* (or weight 1) met (chance mob caught))))
                              s)))
                        1 (when (seq cells) threats))]
    (- 1 survive)))

(defn death-chance
  "The chance of dying on route {:underground? :night? :cells :threats :at} (cells {:x :y :z :s}, :s the seconds spent
  there, death/met-chance): the chance by its class (underground, surface
  at night or by day: the mobs not yet known) and its threats-chance, for each known threat {:mob :weight :reach :caught}
  (weight 0-1, reach a fn of a cell x y z, its weight 0-1, nil outside: jobs.lib.threat-field; caught the chance a walk
  away from it is caught, nil the prior). body as threat-answers, with answer-price's. An answer {:damage :hit
  :caught}: its blows-chance."
  [body {:keys [underground? night? damage] :as route}]
  (if damage
    (blows-chance body route)
    (let [base (settings/get death/settings (cond underground? ::death/chance-underground night? ::death/chance-surface-night :else ::death/chance-surface-day))]
      (+ base (* (- 1 base) (threats-chance body route))))))

(defn spawns-chance
  "The chance spawns mobs (an expected count, a Poisson one) spawned beside body kill it at `at`: each its threat-chance,
  caught at the prior, over threat/spawn-weights."
  [body spawns at]
  (let [per (/ (reduce + (map (fn [[mob w]] (* w (threat-chance body mob nil at))) threat/spawn-weights))
               (reduce + (vals threat/spawn-weights)))]
    (- 1 (js/Math.exp (- (* spawns per))))))

(defn lethal-hp
  "The hp a death chance of body at `at` weighs beside the hp it risks: chance x death/death-cost, at health/hp-seconds an hp."
  [body chance at]
  (/ (* chance (death/death-cost body at)) health/hp-seconds))

(defn route-death-cost
  "What a route adds to its cost for the death it risks: death-chance x death/death-cost at `at` (where it would die)."
  [body route at]
  (* (death-chance body (assoc route :at at)) (death/death-cost body at)))
