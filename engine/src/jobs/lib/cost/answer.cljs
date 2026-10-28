(ns jobs.lib.cost.answer
  "The one price of each answer to hostiles, so the reflexes take the cheapest (decide). Pure.

  An answer is {:damage hp :hit hp :shape :blasts [hp] :spread [hp] :seconds s :then [answer] :caught {:chance :damage :hit :blasts}}:
  the hp it is expected to cost, the creeper blasts in it landing for certain (:spread: as many equally likely blast
  totals) and the rest in blows of about :hit hp each (:shape: their count's width, death/blow-chance), the seconds it takes, what may follow it (the cheapest of :then is paid too; with :stays only by that chance, the
  answer itself again one of them: its mobs may still be there when it ends; :at-once, at the health it leaves) and,
  for a walk away, the chance its mobs catch it and the blows they then land; :spent when its one mob is a creeper (the
  blast that kills spends it: no mob waits at the death spot). answer-price: the hp at the health each
  leaves in the outcomes it lives through (lived-damage, health/hp-cost), the death the blows risk (death-chance x death-price), the seconds, and the cheapest
  :then at full health. death-price: the one price of a death, the mob met again where the body goes back.

  death-chance: the one chance of dying every route, spot, wait or answer is priced by. A known threat on a route costs
  the death of the answer the body would take to it (decide over fight, walk away and the covers it has).

  fight-answer: a fight with the kit (fight/fight-damage, fight-seconds); a mob out of reach is left alive, hitting all
  the fight (followed: what comes at once after it). A lone creeper fought hit-and-back risks the
  blast of a late step back: its :hit is the blast at the creeper's swell start.
  flee-answer: a walk away from the mobs, nil when one of them keeps up (its danger/closing-rate is not positive). A melee mob hits while within its reach, a ranged one while the body is within its bow's range (it
  stands and shoots there; both follow at their chase speed past it; a body in water first swims to land against their swim speed), a creeper that reaches its swell start before the body gets going
  (danger/flight-starts) blows at the distance the body reached when its fuse ends. The mob is left alive:
  :then the fight with it, or the walk once more as a return. A stub until the priced flee search (card ee045b60).
  build-answer: building a refuge (seal or pillar) for some seconds: the blows of the melee mobs that arrive meanwhile.
  cover-answer: shutting the body in (walls, pit or pocket) by build-answer, then hidden for the quiet.

  Game facts kept as constants: a creeper's explosion power (3; damage falls off to none at twice it), its swell start
  (3 blocks) and fuse (1.5 s), a skeleton's bow range (15 blocks), a trident's throw (10 blocks)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.caught :as caught-prior]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.health :as health]
            [jobs.lib.cost.planner :as planner]
            [jobs.lib.cost.threat :as threat]
            [jobs.lib.cost.weapon :as weapon]))

(a/defargs settings
  {::drowned-meet-per-s {:default 0.0021 :spec (a/num-in 0 nil)
                         :doc "Chance a second a drowned meets a body in water a drowned can spawn in (its column holds water over water at block light 0, look/water-fn), by day or night: (k+1)/n over the trial bodies' logs on 2026-10-23, k=11 meets in n=5767 s, each meet once (a respond run soon after another is the same meet) and classed by the body's cell (tools/event-counts.mjs drowned-meets :prior :deep; river or ocean and other biomes, sky light high or low, met it alike)."}
   ::drowned-meet-other-per-s {:default 0.00043 :spec (a/num-in 0 nil)
                               :doc "As ::drowned-meet-per-s in any other water (one deep, or block-lit to its floor): k=1 in n=4602 s (:prior :other)."}
   ::hide-stays {:default {:k 33 :n 52} :spec (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 0 nil)})
                 :doc "Of n flights of the trial bodies that ended hidden in a refuge, k handed back with a danger still about (hide-out :hold :spent), 2026-10-28. Recount: node tools/event-counts.mjs hide-ends."}
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
(def melee-gap-s "Seconds between a melee mob's blows while the body is within its reach (vanilla MeleeAttackGoal: 20 ticks)." 1)
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

(defn small
  "hp of answer's damage in its small blows, the ones its :shape is the width of: less its :blasts and its :spread's mean."
  [{:keys [damage blasts spread]}]
  (- damage (reduce + 0 blasts) (mean spread)))

(defn blows-outcome
  "{:dies :lived} of the blows of answer against body (death/blow-outcome's): of its :damage (its count's :shape), its
  :blasts sure (each of its :spread too, averaged), or of the :caught blows for its :chance."
  [body {:keys [caught] :as answer}]
  (let [outcome (fn [{:keys [hit blasts spread shape] :as a}]
                  (let [sure (reduce + 0 blasts)
                        blows (small a)
                        os (map #(death/blow-outcome body (+ blows sure %) hit (+ sure %) shape) (or (seq spread) [0]))]
                    {:dies (mean (map :dies os)) :lived (mean (map :lived os))}))
        clean (outcome answer)]
    (if caught
      (let [c (:chance caught) o (outcome caught)]
        {:dies (+ (* (- 1 c) (:dies clean)) (* c (:dies o))) :lived (+ (* (- 1 c) (:lived clean)) (* c (:lived o)))})
      clean)))

(defn blows-chance
  "The chance the blows of answer kill body: blows-outcome's :dies."
  [body answer]
  (:dies (blows-outcome body answer)))

(defn expected-damage [{:keys [damage caught]}]
  (if caught (+ (* (- 1 (:chance caught)) damage) (* (:chance caught) (:damage caught))) damage))

(defn lived-damage
  "hp answer's blows take off body in the outcomes it lives through, by their chance (blows-outcome's :lived): a death's
  hp is not paid, respawn heals it."
  [body answer]
  (:lived (blows-outcome body answer)))

(defn healed [body] (assoc body :health 20 :absorption 0))

(defn after
  "body once answer's blows have landed, given it lives through them: their mean over those outcomes (at least 1 hp left)."
  [{:keys [health] :as body} answer]
  (let [{:keys [dies lived]} (blows-outcome body answer)]
    (assoc body :health (max 1 (- (or health 20) (if (< dies 1) (/ lived (- 1 dies)) 0))))))

(defn bare
  "The death price {:met :clear} with no mob met again: death/death-cost."
  [body at]
  (let [d (death/death-cost body at)] {:met d :clear d}))

(declare repeat-price answer-death-chance)

(defn answer-price*
  "answer-price with death {:met :clear} the seconds each death costs (death-price): :clear one by a blast that spends
  the answer's mobs (:spent), :met any other."
  [{:keys [health] :as body} {:keys [seconds then outcomes spent stays] :as answer} at death]
  (let [health (or health 20)
        {:keys [dies lived]} (blows-outcome body answer)
        lives (- 1 dies)
        own (+ (if (pos? lives) (* lives (health/hp-cost health (min (/ lived lives) health))) 0)
               (if (pos? dies) (* dies (if spent (:clear death) (:met death))) 0)
               seconds)
        base (if (seq outcomes)
               (+ own (* lives (reduce + (map (fn [[chance a]] (* chance (answer-price* (after body answer) a at death))) outcomes))))
               own)]
    (cond
      stays (let [h (healed body)]
              (+ base (* lives stays (min (reduce min ##Inf (map #(answer-price* h % at death) then)) (repeat-price h answer at death)))))
      (seq outcomes) base
      (seq then) (+ own (* lives (apply min (map #(answer-price* (if (:at-once answer) (after body answer) (healed body)) % at death) then))))
      :else own)))

(defn repeat-price
  "The price at body of answer taken again each time its mobs stay (its :stays): the fixed point p = base + lives x stays x p
  (base its price with no :stays or :then, lives the chance it leaves body alive), ##Inf when it never ends."
  [body {:keys [stays] :as answer} at death]
  (let [base (dissoc answer :stays :then)
        k (* stays (- 1 (answer-death-chance body base at death)))]
    (if (< k 1) (/ (answer-price* body base at death) (- 1 k)) ##Inf)))

(defn death-price
  "{:met :clear} seconds a death at `at` costs body: death/experience-cost and the cheaper of giving `at` up
  (death/give-up-cost) and going back (death/back-cost, the items lost unless death/kept?), there meeting the mob again
  (:met; :clear, none, is death/death-cost) at full health with nothing carried by the cheapest of answers, each death
  there costing the same once more. That meet's price is the fixed point x = min(stake, back + min over answers of
  p + k x): p an answer's price with its deaths free, k its chance of a death its mob outlives (the price's rise per
  second of such a death), so x = min(stake, min over answers of (back + p) / (1 - k)). No answers: no mob waits."
  [body answers at]
  (let [answers (filter some? answers)
        clear (bare body at)]
    (if (empty? answers)
      clear
      (let [e (assoc (healed body) :carried 0 :level 0)
            stake (death/give-up-cost e)
            back (death/back-cost e at)
            price (fn [a x] (answer-price* e a at {:met x :clear (min stake back)}))
            lines (map (fn [a] (let [p (price a 0)] [p (- (price a 1) p)])) answers)
            at-x (fn [[p k] x] (if (pos? k) (+ p (* k x)) p))
            x (reduce min stake (map (fn [[p k]] (if (< k 1) (/ (+ back p) (- 1 k)) ##Inf)) lines))
            meet (reduce min (map #(at-x % x) lines))]
        (assoc clear :met (+ (death/experience-cost body)
                             (min (death/give-up-cost body)
                                  (+ (death/back-cost body at) (if (death/kept? body at) 0 (or (:carried body) 0)) meet))))))))

(defn answer-price
  "Seconds answer costs body {:health :absorption, death-price's :respawn :level :carried :stake} at `at` (see the ns
  doc); :then is priced at full health: the body meets the mob again once it has healed. :outcomes [[chance answer]] (a
  look's, jobs.lib.cost.look) are each paid by its chance, at the health the answer's blows leave. Both are paid only by
  the chance the body lives through the answer's own blows: a death ends the meet (as answer-death-chance). Each death
  pays death-price over answers, all the body has there (answer among them)."
  [body answer at answers]
  (answer-price* body answer at (death-price body answers at)))

(defn answer-death-chance
  "The chance answer kills body: its blows (blows-chance), then the cheapest of its :then (answer-price* at `at`, by
  death, default bare) at full health, or each of its :outcomes by its chance; with :stays, by that chance the cheapest
  at full health of its :then and itself again (repeat-price; again and again: d / (1 - stays x (1 - d)), d its own
  chance); :at-once, its :then at the health it leaves."
  ([body answer at] (answer-death-chance body answer at (bare body at)))
  ([body {:keys [then outcomes stays] :as answer} at death]
   (let [own (blows-chance body answer)
         base (if (seq outcomes)
                (- 1 (* (- 1 own) (- 1 (reduce + (map (fn [[chance a]] (* chance (answer-death-chance (after body answer) a at death))) outcomes)))))
                own)
         h (if (and (:at-once answer) (not stays)) (after body answer) (healed body))
         next (when (seq then) (apply min-key #(answer-price* h % at death) then))]
     (cond
       stays (let [again? (or (nil? next) (< (repeat-price h answer at death) (answer-price* h next at death)))
                   d (if again?
                       (let [d (answer-death-chance h (dissoc answer :stays :then) at death)] (if (pos? d) (/ d (- 1 (* stays (- 1 d)))) 0))
                       (answer-death-chance h next at death))]
               (- 1 (* (- 1 base) (- 1 (* stays d)))))
       (seq outcomes) base
       next (- 1 (* (- 1 own) (- 1 (answer-death-chance h next at death))))
       :else own))))

(defn decide
  "{:answer key :prices {key seconds} :death}: the cheapest of answers {key answer-or-nil} (nil: not open to the body) by
  answer-price, death the death-price over again (default all of them): the answers of the meet the walk back comes to,
  fresh (again-answers); nil when none is open."
  ([body answers at] (decide body answers at (vals answers)))
  ([body answers at again]
   (let [death (delay (death-price body again at))
         prices (into {} (keep (fn [[k a]] (when a [k (answer-price* body a at @death)]))) answers)]
     (when (seq prices)
       {:answer (key (first (sort-by val prices))) :prices prices :death @death}))))

(defn lone-creeper? [mobs] (and (= 1 (count mobs)) (= "creeper" (:name (first mobs)))))

(defn fight-answer
  "The answer of fight (fight/fight-damage's argument, :health the body's, default 20): its damage and seconds, :hit the
  hardest blow of its mobs. A mob with :reach? false (the body cannot get within a swing of it) is not fought: its blows
  land over the whole fight, the fight's length as if it were fought when no mob is in reach; :left the mobs the fight
  leaves alive. :caught: the fight goes bad at the trial prior of the body's health (jobs.lib.cost.caught/prior
  ::fights of the fought mobs, the likeliest gone bad; a creeper or a wet drowned has its own price), its mob then
  landing that prior's blows; none when the fight's own are more."
  [{:keys [equipment mobs health] :as fight}]
  (let [stats (armour/armour-stats equipment)
        {left false fought true} (group-by #(not (false? (:reach? %))) mobs)
        fought (vec fought)
        seconds (fight/fight-seconds (assoc fight :mobs (if (seq fought) fought mobs)))
        damage (fight/fight-damage (assoc fight :mobs fought))
        left-damage (reduce + 0 (map #(threat/mob-hurt stats (threat/kind %) seconds) left))
        shape (when-let [s (fight/blow-shape (assoc fight :mobs fought))] (fight/sum-shape [[damage s] [left-damage nil]]))
        hit (reduce max 0 (map #(blow stats %) mobs))
        bad (keep #(when-not (or (= "creeper" (:name %)) (fight/wet-drowned? fight %))
                     [% (caught-prior/prior ::caught-prior/fights (:name %) health)])
                  fought)
        [m pr] (when (seq bad) (apply max-key #(:chance (second %)) bad))
        gone-bad (when m (threat/mob-hurt stats (threat/kind m) (:blows pr) identity 1))]
    (cond-> {:damage (+ damage left-damage)
             :hit hit
             :seconds seconds}
      shape (assoc :shape shape)
      (lone-creeper? mobs) (assoc :spent true)
      (seq left) (assoc :left (vec left))
      (and m (< (+ damage left-damage) gone-bad)) (assoc :caught (cond-> {:chance (:chance pr) :hit hit :damage gone-bad}
                                                                  (:shape pr) (assoc :shape (:shape pr)))))))

(defn mix
  "{:damage :shape} of blows that are one of parts [[chance {:damage :shape}]] (chances summing to 1): their mean by
  chance and the width of the mix, each part its own (shape nil: a Poisson count)."
  [parts]
  (let [d (reduce + (map (fn [[w x]] (* w (:damage x))) parts))
        v (reduce + (map (fn [[w {:keys [damage shape]}]] (* w (+ (if shape (/ (* damage damage) shape) 0) (* damage damage)))) parts))
        extra (- v (* d d))]
    (cond-> {:damage d} (and (> extra 1e-9) (pos? d)) (assoc :shape (/ (* d d) extra)))))

(defn followed
  "Answer a, then answer b at once (nil: a once more, later): the blows and seconds of both and what follows b. a's
  spread is kept at its mean, a part of the small blows. :caught: a's going bad, b's being caught, or both, by their
  chances: one mix of those outcomes (mix of their small blows; the blasts the likeliest one's)."
  [a b]
  (if b
    (let [both (fn [x y] (let [s (fight/sum-shape [[(small x) (:shape x)] [(mean (:spread x)) nil] [(small y) (:shape y)]])]
                           (cond-> {:damage (+ (:damage x) (:damage y)) :hit (max (:hit x 0) (:hit y 0)) :blasts (into (vec (:blasts x)) (:blasts y))}
                             s (assoc :shape s))))
          clean (both a b)
          ca (:chance (:caught a) 0)
          cb (:chance (:caught b) 0)
          parts (filter #(pos? (first %)) [[(* ca (- 1 cb)) (both (:caught a) b)]
                                           [(* (- 1 ca) cb) (both a (:caught b))]
                                           [(* ca cb) (both (:caught a) (:caught b))]])
          chance (reduce + 0 (map first parts))
          caught (when (pos? chance)
                   (let [blasts (:blasts (second (apply max-key first parts)))
                         sure (reduce + 0 blasts)
                         {:keys [damage shape]} (mix (map (fn [[w o]] [(/ w chance) (assoc o :damage (- (:damage o) sure))]) parts))]
                     (cond-> {:chance chance :damage (+ damage sure) :hit (reduce max 0 (map (comp :hit second) parts)) :blasts blasts}
                       shape (assoc :shape shape))))]
      (cond-> (-> a
                  (merge (select-keys clean [:damage :hit :blasts]) {:seconds (+ (:seconds a) (:seconds b)) :spread (:spread b)})
                  (dissoc :shape :caught :then :spent))
        (:spent b) (assoc :spent (:spent a))
        (:shape clean) (assoc :shape (:shape clean))
        caught (assoc :caught caught)
        (:then b) (assoc :then (:then b))))
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

(defn reach-rate
  "Blows a second mob m lands while the body is within its hit-range: a melee mob's every melee-gap-s, else its
  measured lone-fight rate (threat/lone-hit-rate)."
  [m]
  (if (= (hit-range m) (danger/melee-reach)) (/ 1 melee-gap-s) (threat/lone-hit-rate (threat/kind m))))

(defn start-reach
  "Seconds a mob d blocks off, chasing at chase and hitting within r, is within r of a body that stands start seconds
  (a flight's start, danger/flight-starts) and then gains gain blocks a second on it: the seconds it stands within r,
  then those it takes to open the gap left to r."
  [d r chase gain start]
  (let [stood (max 0 (- start (/ (max 0 (- d r)) chase)))
        gap (max 0 (- d (* chase start)))]
    (+ stood (/ (max 0 (- r gap)) gain))))

(defn flee-mob
  "{:damage :hit :spread :shape :rate-shape :seconds} of a walk away from mob {:name :held :distance :follow (blocks it keeps chasing out to) :speed
  (its chase speed, default danger/chase-speed of the name)} at speed v through the armour stats, nil when it keeps up: the
  gap opens at gain (a fn of the mob's name, default v less its chase speed), v less :speed when that is given.
  It hits while within its hit-range (reach-rate); a shooter on land stands there while it sees the body (vanilla's
  ranged attack goals stop it within range) and chases only past it; a melee mob on land from the decision: start-reach over each of
  danger/flight-starts, :shape the width of its blow count over them (fight/sum-shape's); a ranged mob at its uncertain
  rate, :rate-shape (threat/hit-rate-shape), the one rate of its kind (flee-answer-on adds a kind's blows before its shape). A flight that starts in water first swims swim blocks to dry land (##Inf: no land
  known) at speed sv, the mob chasing at danger/swim-chase-speed, then walks from the gap the swim left, over the land blocks
  there is to walk on (a ledge with none: the mob keeps up). A creeper that
  reaches its swell start before the body gets going blows at the distance the body reached when its fuse ends, :spread
  that blast for each of danger/flight-starts: a blast is not felt before it lands, as other mobs' blows are (respond's
  felt rate)."
  ([stats v m] (flee-mob stats v nil m))
  ([stats v {:keys [sv swim land gain]} {:keys [name distance follow speed] :as m}]
   (let [d (or distance 0)
         follow (or follow d)
         chase (or speed (danger/chase-speed name))
         r (hit-range m)
         wet (when (and swim (pos? swim) (not= "creeper" name))
               (swim-leg sv (danger/swim-chase-speed name) d r follow swim))
         d (if wet (:gap wet) d)
         gain (if (or speed (nil? gain)) (- v chase) (gain name))
         stands (when-not (or wet (= r (danger/melee-reach))) (/ (max 0 (- r d)) v))
         escape (cond (:away? wet) 0
                      (and wet (not (js/isFinite swim))) nil
                      (not (pos? gain)) nil
                      stands (+ stands (/ (max 0 (- follow (max d r))) gain))
                      :else (/ (max 0 (- follow d)) gain))
         escape (when-not (and wet escape land (< land (* v escape))) escape)]
     (cond
       (nil? escape) nil
       (= "creeper" name)
       (let [starts (danger/flight-starts name)
             swell (/ (max 0 (- d swell-start)) chase)
             spread (mapv #(if (< swell %)
                             (blast-hurt stats (+ (min d swell-start) (* v (max 0 (- fuse-s (- % swell))))))
                             0)
                          starts)]
         {:damage (mean spread) :hit 0 :spread spread :seconds (+ (mean starts) escape)})
       :else
       (let [hurt #(threat/mob-hurt stats (threat/kind m) % identity (reach-rate m))
             in-reach (cond wet [(+ (:in-reach wet) (if (:away? wet) 0 (/ (max 0 (- r d)) gain)))]
                            (= r (danger/melee-reach)) (map #(start-reach d r chase gain %) (danger/flight-starts name))
                            :else [stands])
             damages (map hurt in-reach)
             damage (mean damages)
             spread (- (mean (map #(* % %) damages)) (* damage damage))]
         (cond-> {:damage damage :hit (blow stats m) :seconds (+ (:seconds wet 0) escape)}
           (> spread 1e-9) (assoc :shape (/ (* damage damage) spread))
           (and (not= r (danger/melee-reach)) (threat/hit-rate-shape (threat/kind m))) (assoc :rate-shape (threat/hit-rate-shape (threat/kind m)))))))))

(declare flee-answer-on)

(defn flee-answer
  "flee-answer-on over the land known; when that is no answer and open (the chance, 0..1, that land goes on past it, cells
  never seen) is given, the answer over land enough, in which a flight whose land ends instead (chance 1 - open) is caught
  on the shore as well, taking the blows of the fight ashore (ashore, else later; its expected-damage, gone bad by its chance; the flight's own when none): one caught
  on the way too meets that end, so a flight to a shore the mob reaches is never cheaper than the fight."
  [{:keys [open later ashore] :as args}]
  (let [args (dissoc args :open :ashore)
        later (or ashore later)]
    (or (flee-answer-on args)
        (when open
          (when-let [a (flee-answer-on (assoc args :land ##Inf))]
            (let [{c0 :chance d0 :damage} (:caught a)
                  ends (* (- 1 c0) (- 1 open))
                  end-d (max d0 (if later (expected-damage later) d0))
                  chance (+ c0 ends)]
              (if (zero? chance)
                a
                (-> a
                    (assoc-in [:caught :chance] chance)
                    (assoc-in [:caught :damage] (/ (+ (* c0 (+ (* open d0) (* (- 1 open) end-d))) (* ends end-d)) chance))
                    (update-in [:caught :hit] max (:hit later 0))))))))))

(defn flee-answer-on
  "The answer of a walk away from mobs [flee-mob's] by a body with food wearing equipment, the walk there and back (it
  goes back to its work), times 1 + returns (the flights from such mobs lately: each predicts another); :then the answer
  later (the fight with them, nil none) or the walk again, one return more, from a mob's :again (the distance it is met at
  on the way back, default its :distance) when nearer. :caught: each flight is caught by its way (caught: the flee
  search's chance of it, nil 0), its mobs then landing their blows (a creeper its blast) over the flight, a melee mob over
  caught-s seconds of it (the flee search's race) at reach-rate; or else at the prior of the body's health
  (jobs.lib.cost.caught/prior of its mobs, the likeliest caught), they then landing its blows (the trials counted a
  flight's blows from all its chasers) of that mob's, in a flight begun in water (the trials counted flights on land) a melee mob's, over the swim's share of the flight, no more than
  caught-s at reach-rate (blows do not outlast the race's seconds in reach); never fewer than the walk's own. The two catches are one by their chances (caught by both, the worse): its damage their mean, its :shape the width of their
  mix (each its own: the way's the walk's, the prior's its blows' uncertainty). A creeper that blows on the
  walk (at the median of its :spread) is spent: what follows is the walk from the rest or later (none left: nothing).
  swim: the blocks a body in water swims to dry land first (flee-mob's; ##Inf none known) and land the blocks to walk on there, at its planner/swim-speed times the share of a sprint a flight's pace is on foot (danger/flee-walk-speed over body-speed).
  The flight gains on each mob at its danger/closing-rate.
  nil when a mob keeps up or none is given."
  [{:keys [health food equipment mobs returns later caught caught-s swim land] :as args}]
  (let [stats (armour/armour-stats equipment)
        priors (map #(caught-prior/prior (:name %) health) mobs)
        prior (when (seq priors) (apply max-key :chance priors))
        way (or caught 0)
        caught (- 1 (* (- 1 (:chance prior 0)) (- 1 way)))
        v (danger/flee-walk-speed (or food 20))
        wet {:swim swim :land land :sv (* (planner/swim-speed equipment) (/ v (danger/body-speed (or food 20))))
             :gain #(danger/closing-rate (or food 20) %)}
        per-of (fn [mobs] (map #(flee-mob stats v wet %) mobs))
        per (per-of mobs)
        times (inc (or returns 0))
        swim-share (if (and swim (js/isFinite swim) (pos? swim)) (/ swim (+ swim (or land 0))) 1)]
    (when (and (seq per) (every? some? per))
      (let [walk-of (fn [per times]
                      (let [hit (reduce max 0 (map :hit per))
                            walk {:damage (* times (reduce + (map :damage per)))
                                  :hit hit
                                  :spread (when-let [s (seq (keep :spread per))] (mapv #(* times %) (apply map + s)))
                                  :seconds (* 2 times (reduce max 0 (map :seconds per)))
                                  :near (reduce max 0 (map :seconds per))
                                  :caught {:chance (- 1 (js/Math.pow (- 1 caught) times))
                                           :hit hit
                                           :blasts (vec (for [m mobs :when (= "creeper" (:name m))] (blow stats m)))}}
                            melee (keep (fn [[m f]] (when-not (= "creeper" (:name m)) (assoc f :kind (threat/kind m)))) (map vector mobs per))
                            shared (vals (group-by :kind (filter :rate-shape melee)))
                            shape (fight/sum-shape (concat (map (fn [f] [(* times (:damage f)) (some-> (:shape f) (* times))]) (remove :rate-shape melee))
                                                           (map (fn [fs] [(* times (reduce + (map :damage fs))) (:rate-shape (first fs))]) shared)))
                            extra (+ (reduce + 0 (keep (fn [{:keys [damage shape]}] (when shape (/ (* damage damage) shape))) (remove :rate-shape melee)))
                                     (reduce + 0 (map (fn [fs] (/ (js/Math.pow (reduce + (map :damage fs)) 2) (:rate-shape (first fs)))) shared)))
                            hitting (remove (fn [[m]] (= "creeper" (:name m))) (map vector mobs per priors))
                            dw (reduce + 0 (map (fn [[m f]] (max (:damage f)
                                                                 (if (and caught-s (= (hit-range m) (danger/melee-reach)))
                                                                   (threat/mob-hurt stats (threat/kind m) (min (:seconds f) caught-s) identity (reach-rate m))
                                                                   (threat/mob-hurt stats (threat/kind m) (:seconds f) identity (threat/lone-hit-rate (threat/kind m))))))
                                                hitting))
                            [m _ pr] (when (seq hitting) (apply max-key #(:chance (nth % 2)) hitting))
                            dp (max (reduce + 0 (map #(:damage (second %)) hitting))
                                    (if m (threat/mob-hurt stats (threat/kind m) (cond-> (:blows pr)
                                                                                   (and caught-s swim (pos? swim) (= (hit-range m) (danger/melee-reach)))
                                                                                   (as-> blows (+ (* swim-share (min blows (* caught-s (reach-rate m)))) (* (- 1 swim-share) blows))))
                                                           identity 1)
                                        0))
                            pc (:chance prior 0)
                            by-way {:damage dw :shape (when (pos? extra) (/ (* dw dw) extra))}
                            by-prior {:damage dp :shape (:shape pr)}
                            {d :damage d-shape :shape} (mix (if (pos? caught)
                                                              [[(/ (* way (- 1 pc)) caught) by-way] [(/ (* pc (- 1 way)) caught) by-prior]
                                                               [(/ (* way pc) caught) (max-key :damage by-prior by-way)]]
                                                              [[1 by-prior]]))]
                        (cond-> (assoc-in walk [:caught :damage] (+ d (reduce + 0 (get-in walk [:caught :blasts]))))
                          (lone-creeper? mobs) (assoc :spent true)
                          shape (assoc :shape shape)
                          d-shape (assoc-in [:caught :shape] d-shape))))
            walk (walk-of per times)
            again (let [per (per-of (map #(assoc % :distance (min (:distance %) (:again % ##Inf))) mobs))]
                    (when (every? some? per) (walk-of per (inc times))))]
        (let [blows? (fn [{:keys [spread]}] (pos? (nth (sort spread) (quot (count spread) 2))))
              spent (set (keep (fn [[m f]] (when (and (= "creeper" (:name m)) (blows? f)) m)) (map vector mobs per)))
              left (remove spent mobs)]
          (cond
            (empty? spent) (cond-> walk (or again later) (assoc :then (cond-> [] again (conj again) later (conj later))))
            (seq left) (assoc walk :then (cond-> [(flee-answer-on (assoc args :mobs left :returns times))] later (conj later)))
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

(defn counted-chance
  "The chance of a measured share, k of n counted {:k :n}, under a uniform Beta(1, 1) prior: (k + 1) / (n + 2)."
  [{:keys [k n]}]
  (/ (inc k) (+ n 2)))

(defn hide-stays
  "The chance the danger is still about when a hide in a refuge ends: counted-chance of ::hide-stays."
  []
  (counted-chance (settings/get settings ::hide-stays)))

(defn cover-answer
  "The answer of shutting the body in against mobs [flee-mob's] wearing equipment by a cover {:open-s :seconds :fails}
  (jobs.survival.shut-in/covers: the seconds a side stays open, the seconds it takes with the quiet after it, the chance
  it cannot be closed, default 0): the blows of build-answer over open-s; then, by the chance the mobs are still about
  when the hide ends (hide-stays), the cheapest of then and the same cover again (:stays). A cover that fails is the
  blows over open-s and then at once the cheapest of failed (the answers left: the fight, the other covers). nil with a
  creeper (its blast takes the walls)."
  [{:keys [equipment mobs then failed]} {:keys [open-s seconds fails]}]
  (when-not (some #(= "creeper" (:name %)) mobs)
    (let [a (build-answer {:equipment equipment :mobs mobs :seconds open-s})
          fails (if (seq failed) (or fails 0) 0)]
      (cond-> (assoc a :seconds seconds :stays (* (- 1 fails) (hide-stays)) :then (vec then))
        (pos? fails) (assoc :seconds open-s :outcomes [[fails {:damage 0 :hit 0 :seconds 0 :then (vec failed) :at-once true}]
                                                       [(- 1 fails) {:damage 0 :hit 0 :seconds (- seconds open-s)}]])))))

(defn cover-answers
  "{way answer} of covers {way {:open-s :seconds :fails}} against mobs [flee-mob's] wearing equipment (cover-answer,
  then what may follow it): a cover that fails leaves fight and the other covers, each of them failing to fight alone."
  [{:keys [equipment mobs then fight covers]}]
  (let [of (fn [failed cover] (cover-answer {:equipment equipment :mobs mobs :then then :failed failed} cover))
        alone (into {} (keep (fn [[w cover]] (some->> (of (when fight [fight]) cover) (vector w)))) covers)]
    (into {} (keep (fn [[w cover]] (some->> (of (cond-> (vec (vals (dissoc alone w))) fight (conj fight)) cover) (vector w)))) covers)))

(defn cornered-answers
  "The answers {key answer} a cornered body wearing equipment has to mobs [flee-mob's]: the fight and each of covers {way
  {:open-s :seconds :fails}} it can make (cover-answers, the fight after it); decide takes the cheapest."
  [{:keys [equipment mobs fight covers]}]
  (assoc (cover-answers {:equipment equipment :mobs mobs :then (when fight [fight]) :fight fight :covers covers}) :fight fight))

(defn threat-answers
  "The answers {key answer-or-nil} body {:health :food :equipment :weapon :covers :swim :distance} has to mob-name (or
  a coll of names, met together) met :distance blocks off (default 0, beside it): the
  fight (in water with :swim, fight/fight-damage's wet?) and each of :covers {way {:open-s :seconds :fails}}
  (jobs.survival.shut-in/covers) as cornered-answers builds them, and the walk away (caught by its way at chance caught, nil none, and at the prior of :health; :swim, flee-answer's, for a body in water), as respond prices them."
  [{:keys [health food equipment weapon covers swim distance]} mob-name caught]
  (let [ms (mapv (fn [n] {:name n :distance (or distance 0) :follow (threat/follow-range n)}) (if (string? mob-name) [mob-name] mob-name))
        fight (fight-answer {:weapon weapon :equipment equipment :health health :mobs ms :wet? (some? swim)})]
    (assoc (cornered-answers {:equipment equipment :mobs ms :fight fight :covers covers})
           :flee (flee-answer {:health health :food food :equipment equipment :mobs ms :caught caught :later fight :swim swim}))))

(defn again-answers
  "The answers of the meet a walk back after a death comes to, beside mob names at full health: threat-answers caught at
  the prior (it comes from the respawn side, not into this spot's dead end), no blows felt."
  [body names]
  (vals (threat-answers (healed body) names nil)))

(defn kill-price
  "Seconds killing one mob-name met beside body costs at `at`: answer-price of fighting it (threat-answers' :fight),
  among all the answers it has to it."
  [body mob-name at]
  (let [answers (threat-answers body mob-name nil)]
    (answer-price body (:fight answers) at (vals answers))))

(defn died
  "The seconds a death in answer a costs by death {:met :clear} (decide's): :clear when its blast spends its mobs."
  [death a]
  (if (:spent a) (:clear death) (:met death)))

(defn threat-parts
  "{:chance :death} of mob-name met beside body at `at`: the answer-death-chance of the answer decide takes of
  threat-answers (a provoked-only mob counts threat/provoked-share) and what that death costs (died)."
  [body mob-name caught at]
  (let [answers (threat-answers body mob-name caught)
        {:keys [answer death]} (decide body answers at)
        a (get answers answer)]
    {:chance (* (if (contains? threat/provoked-only mob-name) (threat/provoked-share) 1) (answer-death-chance body a at death))
     :death (died death a)}))

(defn threat-chance
  "The :chance of threat-parts."
  [body mob-name caught at]
  (:chance (threat-parts body mob-name caught at)))

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
  the share of water a drowned can spawn in (water-factors' first: its depths are unseen), less the walk's planner/walk-floor.
  wet-factors: water-factors of body, when the caller has it."
  ([body] (unseen-water-factors body (water-factors body)))
  ([body [wet]]
   (let [swim (/ (* (planner/swim-s (planner/depth-strider (:equipment body))) (inc wet)) planner/walk-floor)]
     (mapv (fn [{:keys [k n]}] (* (/ (inc k) (+ n 2)) (dec swim))) (settings/get settings ::unseen-water-lags)))))

(defn life [{:keys [health absorption]}] (+ (or health 20) (or absorption 0)))

(defn threat-hurt
  "hp mob-name met beside body takes off it at `at`: the lived-damage of the answer decide takes of threat-answers (its
  death is threat-chance's); a provoked-only mob threat/provoked-share of it."
  [body mob-name caught at]
  (let [answers (threat-answers body mob-name caught)]
    (* (if (contains? threat/provoked-only mob-name) (threat/provoked-share) 1)
       (lived-damage body (get answers (:answer (decide body answers at)))))))

(defn group-parts
  "{:chance :death :hurt :near} of mob names met together by body at `at`, caught at caught: the answer decide takes of
  threat-answers, its answer-death-chance and what that death costs (died), the hp taken in the outcomes the body lives through (lived-damage), and the seconds it holds the body
  at or near the place (its :near, a flight's way out; else its :seconds). A ranged mob gets to the body at its first hit
  (death/notice-prior's count): that blow has landed before the answer, whose blows are the ones after it."
  [body names caught at]
  (let [stats (armour/armour-stats (:equipment body))
        first-hits (reduce + 0 (map #(blow stats {:name %}) (filter #(threat/ranged? {:name %}) names)))
        shield (or (:absorption body) 0)
        struck (assoc body :absorption (max 0 (- shield first-hits)) :health (- (or (:health body) 20) (max 0 (- first-hits shield))))
        answers (threat-answers struck names caught)
        {k :answer death :death} (decide struck answers at)
        a (get answers k)]
    {:chance (if (pos? (life struck)) (answer-death-chance struck a at death) 1)
     :death (died death a)
     :hurt (if (pos? (life struck))
             (let [{:keys [dies lived]} (blows-outcome struck a)] (+ (* first-hits (- 1 dies)) lived))
             0)
     :near (or (:near a) (:seconds a) 0)}))

(defn met-threats
  "The known threats of route {:cells :threats} (death-chance's) that may get to the body, [{:mob :caught :c :prior
  :exposure :before :w :on}]: c its weight (a provoked-only one's times threat/provoked-share), prior and exposure its
  death/met-stretch over the cells' seconds dts, before (Float64Array) its weighted seconds before each cell, w its
  weight there, on (Int32Array) the cells it weighs."
  [{:keys [cells threats]} dts]
  (into []
        (keep (fn [{:keys [mob weight reach caught pos]}]
                (let [{:keys [met xs prior exposure]} (death/met-stretch mob reach cells dts pos)
                      c (min 1 (* (or weight 1) (if (contains? threat/provoked-only mob) (threat/provoked-share) 1)))
                      n (count xs)
                      before (js/Float64Array. n)
                      w (js/Float64Array. n)]
                  (when (pos? (* c met))
                    (dotimes [a n]
                      (let [x0 (if (pos? a) (nth xs (dec a)) 0)]
                        (aset before a x0)
                        (aset w a (if (pos? (nth dts a)) (/ (- (nth xs a) x0) (nth dts a)) 0))))
                    {:mob mob :caught caught :c c :prior prior :exposure exposure :before before :w w
                     :on (js/Int32Array.from (filter #(pos? (aget w %)) (range n)))}))))
        (when (seq cells) threats)))

(defn notice-view
  "How threat t ({:before :w :on :prior :exposure :dts}, met-threats') gets to the body over its prior (its notice rate
  unknown), as one of a known rate in the prior's cumulative hazard H(x) = shape ln(1 + x/rate), x the weighted seconds:
  {:h0 H at each cell's start, :y its rate over the cell (exact at both ends), :total H over the route, :prior}."
  [{:keys [before w on prior exposure dts]}]
  (let [n (alength w) h0 (js/Float64Array. n) y (js/Float64Array. n)
        {:keys [shape rate]} prior
        hz #(* shape (js/Math.log1p (/ % rate)))]
    (dotimes [i (alength on)]
      (let [a (aget on i) b (aget before a)]
        (aset h0 a (hz b))
        (aset y a (/ (* shape (js/Math.log1p (/ (* (aget w a) (aget dts a)) (+ rate b)))) (aget dts a)))))
    {:h0 h0 :y y :total (hz exposure) :prior prior}))

(defn met-in-cell
  "Of two threats neither at the body at a cell's start: the chance the one coming at rate u comes first within the cell's
  d seconds and the other (rate v) comes while it is answered, within window A of its hazard and the R it has left on
  the route, so never past its own chance of coming: the integral over t of u e^(-(u+v)t) (1 - e^(-min(A, R - v t)))."
  [u v A R d]
  (let [h (+ u v)
        t1 (if (pos? v) (min d (max 0 (/ (- R A) v))) (if (< A R) d 0))
        within (fn [c s] (if (pos? c) (/ (- (js/Math.expm1 (- (* c s)))) c) s))]
    (+ (* u (- (js/Math.expm1 (- A))) (within h t1))
       (* u (js/Math.exp (- (* h t1))) (within h (- d t1)))
       (* -1 (js/Math.exp (- (+ R (* u t1)))) (- (js/Math.expm1 (- (* u (- d t1)))))))))

(defn met-with
  "The chance threat j gets to the body in one answer with threat k, given k does, at notice rate r of k's class (and of
  j's, jv nil; else jv j's notice-view): on each cell both weigh (cells, an Int32Array), the first of the two to get to the
  body there (within the cell's seconds dts, a Float64Array) is answered for its :near seconds while the other, not there
  yet, may join it at its rate there (met-in-cell): its coming then is the coming it would have had on the rest of the
  route, so j comes in k's answer only where it comes at all and the chance is at most j's own."
  [k r j jv dts cells]
  (let [{bk :before wk :w sk :near ek :exposure} k
        {bj :before wj :w sj :near ej :exposure} j
        got (- (js/Math.expm1 (- (* r ek))))
        {:keys [shape rate]} (:prior jv)]
    (if (pos? got)
      (loop [i 0 acc 0]
        (if (< i (alength cells))
          (let [a (aget cells i)
                x (* r (aget wk a))
                h0k (* r (aget bk a))
                y (if jv (aget (:y jv) a) (* r (aget wj a)))
                h0j (if jv (aget (:h0 jv) a) (* r (aget bj a)))
                aj (if jv (* shape (js/Math.log1p (/ (* (aget wj a) sk) (+ rate (aget bj a))))) (* y sk))
                rj (- (if jv (:total jv) (* r ej)) h0j)
                d (aget dts a)]
            (recur (inc i) (+ acc (* (js/Math.exp (- (+ h0k h0j)))
                                     (+ (met-in-cell x y aj rj d) (met-in-cell y x (* x sj) (- (* r ek) h0k) d))))))
          (/ acc got)))
      0)))

(defn met-states
  "The sets of threats `earlier` threat k may meet at once, by the count of each kind [mob caught] (mixed radix: per earlier
  threat its kind's :stride and :radix, :n sets): :without and :with, fns of a set's index, its (parts names caught) met
  without k and with it (the worst caught of them), none {:chance 0 :hurt 0}; each priced once, when asked."
  [parts k earlier]
  (let [kind (juxt :mob :caught)
        kinds (vec (distinct (map kind earlier)))
        counts (frequencies (map kind earlier))
        radices (mapv #(inc (counts %)) kinds)
        strides (vec (reductions * 1 radices))
        index (zipmap kinds (range))
        members (fn [s] (mapcat (fn [kd r st] (repeat (mod (quot s st) r) kd)) kinds radices strides))
        price (fn [ms] (if (seq ms)
                         (parts (vec (sort (map first ms))) (some->> (seq (keep second ms)) (apply max)))
                         {:chance 0 :hurt 0}))
        cached (fn [f] (let [seen (js/Map.)] (fn [s] (or (.get seen s) (let [v (f s)] (.set seen s v) v)))))]
    {:n (peek strides)
     :stride (mapv #(strides (index (kind %))) earlier)
     :radix (mapv #(radices (index (kind %))) earlier)
     :without (cached #(price (members %)))
     :with (cached #(price (cons (kind k) (members %))))}))

(defn met-chances
  "Float64Array of the chance of each set of states (met-states) being the one met, each earlier threat in it at its
  chance of rhos (a Float64Array)."
  [{:keys [n stride radix]} rhos]
  (let [p (js/Float64Array. n)]
    (aset p 0 1)
    (dotimes [j (alength rhos)]
      (let [st (nth stride j) r (nth radix j) rho (aget rhos j)]
        (when (pos? rho)
          (loop [s (dec n)]
            (when (>= s 0)
              (aset p s (+ (* (- 1 rho) (aget p s)) (if (pos? (mod (quot s st) r)) (* rho (aget p (- s st))) 0)))
              (recur (dec s)))))))
    p))

(defn joins
  "What threat k, the i-th of found (route-parts', by when each first may get to the body), adds at each of nodes, the
  death/rate-nodes [[r w]] of its class: {:times :adds} (Float64Arrays: the survival of the threats before it, Z, becomes
  Z times + adds) and :hurt, the hurt it adds over the nodes. group: group-parts of names and caught; view: notice-view of
  a threat of found by its index."
  [group view found i nodes]
  (let [k (found i)
        states (met-states group k (subvec found 0 i))
        pairs (into [] (keep (fn [j] (let [cs (.filter (:on k) #(pos? (aget (:w (found j)) %)))]
                                       (when (pos? (alength cs)) [j (not= (:prior (found j)) (:prior k)) cs]))))
                    (range i))
        lone (:chance (group [(:mob k)] (:caught k)))
        at-nodes (mapv (fn [[r]]
                         (let [rhos (js/Float64Array. i)]
                           (doseq [[j cross cs] pairs]
                             (aset rhos j (* (:c (found j)) (met-with k r (found j) (when cross (view j)) (:dts k) cs))))
                           [(* (:c k) (- (js/Math.expm1 (- (* r (:exposure k)))))) (met-chances states rhos)]))
                       nodes)
        n (:n states)
        with (js/Float64Array. n)
        without (js/Float64Array. n)
        more (js/Float64Array. n)
        _ (dotimes [s n]
            (when (some (fn [[_ p]] (pos? (aget p s))) at-nodes)
              (let [w ((:with states) s) wo ((:without states) s)]
                (aset with s (- 1 (:chance w)))
                (aset without s (- 1 (:chance wo)))
                (aset more s (- (:hurt w) (:hurt wo))))))
        sum (fn [p xs] (areduce p s acc 0 (+ acc (* (aget p s) (aget xs s)))))
        times (js/Float64Array. (count nodes))
        adds (js/Float64Array. (count nodes))]
    (doseq [[ri [q p]] (map-indexed vector at-nodes)]
      (aset times ri (- 1 (* q lone)))
      (aset adds ri (* q (- (sum p with) (* (- 1 lone) (sum p without))))))
    {:times times :adds adds
     :hurt (reduce + (map (fn [[_ w] [q p]] (* w q (sum p more))) nodes at-nodes))}))

(defn route-parts
  "{:chance :hurt} of the known threats of route {:cells :threats :at} for body, priced at :at (default the first cell). A
  threat that gets to the body is answered there (group-parts, for its :near seconds) while the walk stands still; each other one that gets to it
  meanwhile, or whose answer it comes in, is met in that answer (met-with: a chance for each pair, no hard meetings).
  Taken by when they first may get to the body, each threat k joins the set of earlier ones it meets (met-states, each in
  it at its met-with chance, met-chances): with q its chance of getting to the body, D and H a set's chance of death and
  hurt, the survival of the threats so far Z becomes Z (1 - q D(k)) + q (E[1 - D(set + k)] - (1 - D(k)) E[1 - D(set)]), the
  hurt grows by q (E[H(set + k)] - E[H(set)]) (joins): exact for two threats, and for any that meet surely at once or never.
  A class's notice rate (death/notice-prior) is one unknown over the whole route: Z is kept at each of the classes'
  death/rate-nodes, a threat's sets reckoned at its own class's rate, other classes' notices over their priors. The hurt at
  most the body's life. :death what a death costs (group-parts'), each threat's by its weight times its own death chance (0 with none)."
  [body {:keys [cells at] :as route}]
  (let [at (or at (first cells))
        group (memoize #(group-parts body %1 %2 at))
        dts (death/cell-seconds cells)
        dt (js/Float64Array.from dts)
        found (->> (met-threats route dts)
                   (map (fn [{:keys [mob caught on] :as t}] (assoc t :near (:near (group [mob] caught)) :first (aget on 0) :dts dt)))
                   (sort-by :first)
                   vec)
        view (memoize #(notice-view (found %)))
        priors (vec (distinct (map :prior found)))
        nodes (mapv death/rate-nodes priors)
        strides (vec (reductions * 1 (map count nodes)))
        grid (peek strides)
        z (.fill (js/Float64Array. grid) 1)
        weights (.fill (js/Float64Array. grid) 1)
        _ (doseq [[rs st] (map vector nodes strides)]
            (dotimes [g grid] (aset weights g (* (aget weights g) (second (rs (mod (quot g st) (count rs))))))))
        hurt (reduce (fn [hurt i]
                       (let [c (.indexOf priors (:prior (found i)))
                             {:keys [times adds] :as step} (joins group view found i (nodes c))
                             st (strides c)
                             radix (count (nodes c))]
                         (dotimes [g grid]
                           (let [ri (mod (quot g st) radix)]
                             (aset z g (+ (* (aget times ri) (aget z g)) (aget adds ri)))))
                         (+ hurt (:hurt step))))
                     0 (range (count found)))
        survives (areduce z g acc 0 (+ acc (* (aget weights g) (aget z g))))
        shares (map (fn [{:keys [mob caught c]}] (let [g (group [mob] caught)] [(* c (:chance g)) (:death g)])) found)
        mass (reduce + 0 (map first shares))]
    {:chance (min 1 (max 0 (- 1 survives)))
     :death (if (pos? mass) (/ (reduce + (map #(* (first %) (second %)) shares)) mass) 0)
     :hurt (min (life body) (max 0 hurt))}))

(defn spawn-kill
  "{:chance :death} of one mob spawned beside body at `at`: each its threat-parts, caught at the prior, over
  threat/spawn-weights, the chance the weighted mean, the death each's by its weight times its chance."
  [body at]
  (let [parts (map (fn [[mob w]] [w (threat-parts body mob nil at)]) threat/spawn-weights)
        mass (reduce + (map (fn [[w {:keys [chance]}]] (* w chance)) parts))]
    {:chance (/ mass (reduce + (vals threat/spawn-weights)))
     :death (if (pos? mass)
              (/ (reduce + (map (fn [[w {:keys [chance death]}]] (* w chance death)) parts)) mass)
              0)}))

(defn spawns-chance
  "The chance spawns mobs (an expected count, a Poisson one) spawned beside body kill it at `at`, each at kill (default
  spawn-kill's chance: a caller pricing many routes at one place passes it once)."
  ([body spawns at] (spawns-chance body spawns at nil))
  ([body spawns at kill] (- 1 (js/Math.exp (- (* spawns (or kill (:chance (spawn-kill body at)))))))))

(defn spawns-hurt
  "hp spawns mobs (an expected count) spawned beside body take off it at `at`, at most its life (their death is
  spawns-chance's): each its threat-hurt, caught at the prior, over threat/spawn-weights."
  [body spawns at]
  (min (life body)
       (* spawns (/ (reduce + (map (fn [[mob w]] (* w (threat-hurt body mob nil at))) threat/spawn-weights))
                    (reduce + (vals threat/spawn-weights))))))

(defn either
  "{:chance :death} of dying of any of parts {:chance :death}, met apart: the chance one of them kills, and what that
  death costs, each part's by its chance (0 with none)."
  [& parts]
  (let [mass (reduce + 0 (map :chance parts))]
    {:chance (- 1 (reduce * 1 (map #(- 1 (:chance %)) parts)))
     :death (if (pos? mass) (/ (reduce + (map #(* (:chance %) (:death %)) (filter #(pos? (:chance %)) parts))) mass) 0)}))

(defn route-death
  "{:chance :death} of dying on route {:cells :threats :spawns :spawn-kill :at} (cells {:x :y :z :s}, :s the seconds
  spent there, death/met-stretch): its spawns (the expected count of mobs met spawning, spawns-chance at :at, default the
  first cell, each at :spawn-kill (spawn-kill's) when given) or route-parts' of its threats, for each known threat {:mob
  :pos :weight :reach :caught} (pos its place, for death/notice-factor; weight 0-1, reach a fn of a cell x y z, its weight
  0-1, nil outside: jobs.lib.threat-field; caught the chance a walk away from it is caught, nil the prior), kill the
  body (either); :death what that death costs. body as threat-answers, with answer-price's."
  [body {:keys [spawns] :as route}]
  (let [at (or (:at route) (first (:cells route)))
        kill (when (pos? (or spawns 0)) (or (:spawn-kill route) (spawn-kill body at)))]
    (either (if kill {:chance (spawns-chance body spawns at (:chance kill)) :death (:death kill)} {:chance 0 :death 0})
            (select-keys (route-parts body route) [:chance :death]))))

(defn death-chance
  "The :chance of route-death; of an answer {:damage :hit :caught}, its blows-chance."
  [body {:keys [damage] :as route}]
  (if damage (blows-chance body route) (:chance (route-death body route))))

(defn lethal-hp
  "The hp a death {:chance :death} (route-death's) weighs beside the hp it risks: chance x death, at the hp-seconds
  setting an hp."
  [{:keys [chance death]}]
  (if (pos? chance) (/ (* chance death) (health/walk-hp-seconds)) 0))

(defn route-death-cost
  "What a route adds to its cost for the death it risks at `at` (where it would die): route-death's chance x death."
  [body route at]
  (let [{:keys [chance death]} (route-death body (assoc route :at at))]
    (if (pos? chance) (* chance death) 0)))
