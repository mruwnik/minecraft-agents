(ns engine.cost-answer-test
  "jobs.lib.cost.answer: one price for each answer to hostiles (fight, walk away, build a refuge) and the cheapest."
  (:require [cljs.test :refer [deftest is are use-fixtures]]
            [engine.args :as a]
            [engine.fake :as fake]
            [engine.game :as game]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.armour :as armour]
            [jobs.lib.cost.caught :as caught]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.fight :as fight]
            [jobs.lib.cost.look :as glance]
            [jobs.lib.cost.threat :as threat]
            [engine.settings :as settings]))

(use-fixtures :each {:before #(game/select! (fake/create {}))})

(def here {:x 0 :y 64 :z 0})

(defn body
  "A fed body with health and no bed: a death walks it back from world spawn."
  [health]
  {:health health :food 20 :level 0 :carried 0})

(def sword "iron_sword")

(defn fight [weapon & mobs] {:weapon weapon :mobs (vec mobs)})

(defn mob
  ([name distance] {:name name :distance distance :follow 16})
  ([name distance follow] {:name name :distance distance :follow follow}))

(defn choice
  "What decide makes of fighting mobs with weapon or walking away from them, for a body at health with returns flights from them lately."
  ([health weapon mobs] (choice health weapon mobs 0))
  ([health weapon mobs returns]
   (let [f (cost/fight-answer (apply fight weapon mobs))]
     (:answer (cost/decide (body health)
                           {:fight f :flee (cost/flee-answer {:health health :food 20 :mobs mobs :returns returns :later f})}
                           here)))))

(deftest an-uncertain-blow-rate-widens-the-blow-count
  (let [poisson (death/blow-chance {:health 10} 5 2.5)]
    (is (> (death/blow-chance {:health 10} 5 2.5 0 2) poisson) "a wide rate makes the heavy tail likelier")
    (is (< (js/Math.abs (- (death/blow-chance {:health 10} 5 2.5 0 1e6) poisson)) 1e-4) "a sure rate is the Poisson")))

(deftest a-swim-fight-with-a-drowned-is-priced-over-the-fitted-posterior
  (let [chance (fn [k n] (settings/with-settings {:jobs.lib.cost.fight/drowned-wet-hits-per-swing {:k k :n n}}
                           #(let [a (cost/fight-answer {:weapon "stone_sword" :mobs [(mob "drowned" 0)] :wet? true})]
                              [(:damage a) (cost/death-chance (body 20) a)])))
        [thin-hp thin] (chance 1.5 2)
        [wide-hp wide] (chance 19.5 20)]
    (is (< (js/Math.abs (- thin-hp wide-hp)) 1e-9) "the same mean: the same expected hp")
    (is (> thin wide) "fewer meets behind the same mean: a likelier death")
    (is (nil? (:shape (cost/fight-answer {:weapon nil :mobs [(mob "drowned" 0)] :wet? true}))) "a fist fight is out of the fit: hit as on land")))

(deftest a-wet-fight-followed-by-a-blast-keeps-the-width-of-its-blow-count
  (let [wet (cost/fight-answer {:weapon "stone_sword" :mobs [(mob "drowned" 0)] :wet? true})]
    (doseq [[what b] [["a blast" {:damage 9 :hit 0 :blasts [9] :seconds 1}]
                      ["a creeper's spread" {:damage 6 :hit 0 :spread [0 12] :seconds 1}]]]
      (is (< (js/Math.abs (- (:shape (answer/followed wet b)) (:shape wet))) 1e-9)
          (str what " lands no small blows: the count's width is the fight's alone")))))

(deftest blows-that-may-take-the-health-are-the-death-risk
  (is (zero? (death/blow-chance {:health 20} 0 3)))
  (is (< (death/blow-chance {:health 20} 4 2.5) 0.01) "a fight of 4 hp at full health hardly kills")
  (is (< 0.4 (death/blow-chance {:health 5} 4 2.5) 0.6) "at 5 hp it may well")
  (is (< (death/blow-chance {:health 5 :absorption 4} 4 2.5) (death/blow-chance {:health 5} 4 2.5)) "absorption is health")
  (is (= (death/blow-chance (body 5) 4 2.5) (cost/death-chance (body 5) {:damage 4 :hit 2.5})) "the death chance of an answer's blows")
  (is (< 3.99 (:lived (death/blow-outcome {:health 20} 4 2.5 0 nil)) 4) "blows that hardly kill take their hp")
  (let [{:keys [dies lived]} (death/blow-outcome {:health 5} 4 2.5 0 nil)]
    (is (< lived (* (- 1 dies) 5)) "a death's hp are not counted in what the blows take"))
  (is (= 1 (cost/death-chance (body 20) {:damage 21 :hit 3 :blasts [20]})) "a blast that takes the health kills")
  (is (> (cost/death-chance (body 20) {:damage 21 :hit 3 :blasts [16]}) (cost/death-chance (body 20) {:damage 21 :hit 16}))
      "a blast of 16 leaves 4 hp against 5 hp of zombie blows, not 5/16 more of a blast"))

(deftest the-cheapest-open-answer-is-taken
  (is (= {:answer :b :prices {:a 10 :b 5}}
         (cost/decide (body 20) {:a {:damage 0 :seconds 10} :b {:damage 0 :seconds 5} :c nil} here)))
  (is (nil? (cost/decide (body 20) {:a nil} here)) "none open")
  (is (> (cost/answer-price (body 10) {:damage 4 :hit 2 :seconds 0} here) (cost/answer-price (body 20) {:damage 4 :hit 2 :seconds 0} here))
      "hp are dearer near the floor")
  (is (> (cost/answer-price (body 20) {:damage 0 :seconds 5 :then [{:damage 0 :seconds 7}]} here)
         (cost/answer-price (body 20) {:damage 0 :seconds 5} here))
      "what follows is paid too")
  (is (< (- (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1 :then [{:damage 0 :seconds 50}]} here)
            (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1} here))
         0.01)
      "a death all but sure ends the meet: what would follow it is not paid"))

(deftest a-fight-is-taken-when-it-is-cheaper-than-walking-away-and-meeting-the-mob-again
  (are [health weapon mobs expected] (= expected (choice health weapon mobs))
    20 sword [(mob "zombie" 3 35)] :fight
    5 sword [(mob "zombie" 3 35)] :fight
    20 nil [(mob "zombie" 3 35)] :flee
    20 sword [(mob "zombie" 3 35) (mob "zombie" 3 35) (mob "zombie" 3 35)] :flee
    20 sword [(mob "skeleton" 10)] :fight))

(deftest the-walk-again-comes-as-one-flight-more
  (let [skeleton (mob "skeleton" 10)
        again (first (:then (cost/flee-answer {:food 20 :mobs [skeleton]})))]
    (is (= (dissoc (cost/flee-answer {:food 20 :mobs [skeleton] :returns 1}) :then) again)
        "a mob walked from is met again on the way back: that walk is priced as the next decision would price it, a return")
    (is (nil? (:then again)) "and nothing after it")))

(deftest a-body-that-fled-a-skeleton-twice-fights-it-only-while-the-walk-to-it-costs-nothing
  (let [skeleton (mob "skeleton" 12)
        fight (cost/fight-answer (assoc (fight sword skeleton) :mobs [skeleton]))
        choose (fn [returns walk-hp]
                 (let [f (update fight :damage + walk-hp)]
                   (:answer (cost/decide (body 20) {:fight f :flee (cost/flee-answer {:food 20 :mobs [skeleton] :returns returns :later f})} here))))]
    (is (= :flee (choose 0 0)) "no flight lately: a step out of its range is cheaper than closing 12 blocks")
    (is (= :fight (choose 2 0)) "fled twice, the walks again dearer than the fight, if the walk to it is free (go-to's lower bound)")
    (is (= :flee (choose 2 4)) "4 hp of go-to's quote of the walk to it: the walks away are the cheaper")))

(deftest a-flight-ending-past-follow-range-is-not-flown-again-when-the-way-back-passes-the-skeleton
  (let [choose (fn [again returns]
                 (let [skeleton (cond-> (mob "skeleton" 15) again (assoc :again again))
                       f (cost/fight-answer (assoc (fight sword skeleton) :mobs [skeleton]))]
                   (:answer (cost/decide (body 20) {:fight f :flee (cost/flee-answer {:food 20 :mobs [skeleton] :returns returns :later f})} here))))]
    (is (= [:fight :fight] (mapv #(choose 1 %) [0 1])) "the goal lies past it: the trigger refiring on the walk back is already priced, at the first meet and after a flight, so the body does not pendulum at 16")
    (is (= [:flee :flee] (mapv #(choose nil %) [0 1])) "no way back past it: the flight is the last")))

(deftest a-mob-between-the-body-and-its-goal-is-walked-from-again-from-where-the-way-back-passes-it
  (let [fight (cost/fight-answer (fight sword (mob "skeleton" 12)))
        choose #(:answer (cost/decide (body 20) {:fight fight :flee (cost/flee-answer {:food 20 :mobs [%] :later fight})} here))]
    (is (= :flee (choose (mob "skeleton" 12))) "nothing past it: stepping out of its range now and again is cheaper than closing 12 blocks under its arrows")
    (is (= :fight (choose (assoc (mob "skeleton" 12) :again 1))) "the way back passes a block from it: the walk again is from its feet, the whole bow range")
    (is (= (choose (mob "skeleton" 12)) (choose (assoc (mob "skeleton" 12) :again 14))) "a way that passes farther than it stands changes nothing")))

(deftest a-walk-away-is-caught-at-the-prior-of-its-health-and-so-loses-to-a-fight-or-a-wall-at-low-health
  (let [zombie (mob "zombie" 4 35)
        f (cost/fight-answer (fight sword zombie))
        flee #(cost/flee-answer {:health %1 :food 20 :mobs [zombie] :later f :caught %2})
        wall (assoc (cost/build-answer {:mobs [zombie] :seconds 2}) :seconds 240)
        pick #(:answer (cost/decide (body %1) %2 here))]
    (is (= :fight (pick 8 {:fight f :flee (flee 8 0)})) "at 8 hp a free way is still caught at the prior")
    (is (= :wall (pick 8 {:wall wall :flee (flee 8 0)})))
    (is (< (get-in (flee 20 0) [:caught :chance]) (get-in (flee 8 0) [:caught :chance])) "less often caught from full health")
    (is (> (cost/death-chance (body 20) (flee 20 1)) 0.9) "caught on its way, the zombie lands its blows all the flight")
    (is (< (cost/death-chance (body 20) (cost/flee-answer {:food 20 :mobs [zombie] :later f :caught 1 :caught-s 0.5})) 0.1)
        "caught for the half second the race gives it, only those blows")
    (is (= (:caught (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 10)] :caught 1 :caught-s 0.5}))
           (:caught (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 10)] :caught 1})))
        "a shooter is no melee race: its arrows land all the flight")))

(deftest a-walk-caught-beside-a-creeper-takes-its-blast-as-a-sure-part-of-the-life
  (let [walk (cost/flee-answer {:food 20 :mobs [(mob "creeper" 1) (mob "zombie" 3 35)] :caught 1})
        {:keys [damage hit blasts]} (:caught walk)
        blast (cost/blast-hurt (cost/armour-stats nil) answer/swell-start)]
    (is (= [blast] blasts))
    (is (= (death/blow-chance (body 8) damage hit blast) (cost/death-chance (body 8) walk)))))

(deftest a-walk-from-a-mob-that-keeps-up-is-no-answer
  (is (nil? (cost/flee-answer {:food 20 :mobs [(mob "spider" 4)]})) "a spider outruns the body")
  (is (some? (cost/flee-answer {:food 20 :mobs [(mob "zombie" 4 35)]})))
  (is (pos? (:damage (cost/flee-answer {:food 20 :mobs [(mob "creeper" 2)]}))) "a creeper 2 off blows before the walk is out of its reach")
  (is (zero? (:damage (cost/flee-answer {:food 20 :mobs [(mob "creeper" 24 24)]}))) "far off, even the slowest start of the measured ones gets out of its reach"))

(deftest a-creeper-that-blows-on-the-walk-is-not-met-again
  (let [zombie (mob "zombie" 4 35)]
    (is (nil? (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 1)]}))) "a lone creeper blown is gone")
    (is (some? (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 3)]}))) "one 3 off mostly does not reach the walk")
    (is (= (cost/flee-answer {:food 20 :mobs [zombie] :returns 1}) (first (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 1) zombie]}))))
        "after the blast only the zombie is walked from again")))

(deftest a-body-that-can-neither-outrun-nor-outfight-a-spider-digs-in
  (let [spider (mob "spider" 2)
        answers {:fight (cost/fight-answer (fight nil spider)) :flee (cost/flee-answer {:food 20 :mobs [spider]})}]
    (is (= :fight (:answer (cost/decide (body 20) answers here))) "with no cover the fist is all that is left")
    (is (= :pit (:answer (cost/decide (body 20) (assoc answers :pit (cost/cover-answer {:mobs [spider]} {:open-s 4 :seconds 40})) here)))
        "a pit dug in 4 s under the spider's blows beats a fist fight it hardly survives")
    (is (nil? (cost/cover-answer {:mobs [(mob "creeper" 4)]} {:open-s 4 :seconds 40})) "a blast takes the cover")))

(deftest a-threats-cover-is-priced-as-respond-prices-it
  (let [cover {:open-s 2 :seconds 50}
        zombie (mob "zombie" 0 35)]
    (is (= (cost/cover-answer {:mobs [zombie]} cover)
           (:walls (answer/threat-answers {:food 20 :covers {:walls cover}} "zombie" nil)))
        "one cover-answer over shut-in/covers' seconds, the hide-out's quiet in them, for death-chance and respond alike")))

(deftest a-mob-that-came-back-before-makes-a-walk-dearer
  (let [walk #(cost/answer-price (body 20) (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 10)] :returns %}) here)]
    (is (> (walk 2) (walk 1) (walk 0)))))

(deftest a-lone-creeper-is-fought-hit-and-back-at-the-measured-exchange
  (let [price #(cost/answer-price (body %) (cost/fight-answer (fight sword (mob "creeper" 3))) here)]
    (is (> (price 10) (* 2 (price 20)))))
  (are [health weapon distance returns expected] (= expected (choice health weapon [(mob "creeper" distance)] returns))
    20 "stone_sword" 4 0 :fight
    20 "stone_sword" 9 0 :fight
    20 "stone_sword" 9 3 :fight))

(deftest a-creepers-blast-falls-off-to-nothing-at-twice-its-power
  (let [bare (cost/armour-stats nil)]
    (is (> (cost/blast-hurt bare 1) (cost/blast-hurt bare 3) (cost/blast-hurt bare 5) 0))
    (is (zero? (cost/blast-hurt bare 6)))
    (is (< (cost/blast-hurt (cost/armour-stats {:torso {:name "diamond_chestplate"}}) 3) (cost/blast-hurt bare 3)) "armour takes some")))

(deftest a-refuge-costs-the-blows-of-the-mobs-that-arrive-before-it-is-built
  (let [build #(:damage (cost/build-answer {:mobs [(mob "zombie" %)] :seconds 2}))]
    (is (> (build 1) (build 4) (build 8)))
    (is (zero? (build 8)) "a zombie 8 off is not there within 2 s"))
  (is (pos? (:damage (cost/build-answer {:mobs [(mob "skeleton" 10)] :seconds 2}))) "a shooter in bow range shoots meanwhile")
  (is (zero? (:damage (cost/build-answer {:mobs [(mob "skeleton" 20)] :seconds 2}))) "one beyond it does not"))




(deftest a-long-fist-stay-in-the-dark-open-risks-a-death-worth-more-than-half-the-health
  ;; 120 s at the dark spawn chance; no bed: a death walks the body back from world spawn
  (let [spawns (* 120 (cost/spawn-chance [0 0 0]))]
    (is (> (cost/lethal-hp (body 20) (cost/spawns-chance (body 20) spawns here) here) 10))
    (is (< (cost/spawns-chance (assoc (body 20) :weapon "diamond_sword") spawns here) (cost/spawns-chance (body 20) spawns here)) "a sword dies less")
    (is (zero? (cost/spawns-chance (body 20) 0 here)))))

(deftest a-fight-out-of-reach-takes-the-mobs-blows-and-kills-nothing-so-the-swim-out-wins
  (let [deep (assoc (mob "drowned" 3 35) :reach? false)
        unreached (cost/fight-answer (fight "stone_sword" deep))
        fought (cost/fight-answer (fight "stone_sword" (dissoc deep :reach?)))
        swim (cost/flee-answer {:food 20 :mobs [deep] :swim 4})]
    (is (= [deep] (:left unreached)) "the mob is left alive")
    (is (> (:damage unreached) (:damage fought)) "its blows land the whole fight")
    (is (= :fight (:answer (cost/decide (body 13) {:fight fought :flee (cost/flee-answer {:food 20 :mobs [deep] :swim 4 :later fought})} here)))
        "in reach, the fight at 13 hp beats the walk")
    (is (= :flee (:answer (cost/decide (body 13) {:fight (cost/followed unreached swim) :flee swim} here)))
        "out of reach, the fight is the blows and then the same swim out")))

(deftest a-swim-out-gains-on-a-drowned-and-a-swim-with-no-shore-from-a-faster-swimmer-is-no-answer
  (are [m swim expected] (= expected (some-> (cost/flee-answer {:food 20 :mobs [m] :swim swim}) :damage zero?))
    (mob "drowned" 3 35) 4 true
    (mob "drowned" 3 35) ##Inf true
    (mob "drowned" 0.5 35) 4 false
    (mob "guardian" 3 35) ##Inf nil)
  (is (> (:seconds (cost/flee-answer {:food 20 :mobs [(mob "drowned" 3 35)] :swim 4}))
         (:seconds (cost/flee-answer {:food 20 :mobs [(mob "drowned" 3 35)]})))
      "a swim is slower than a run")
  (is (pos? (:damage (cost/flee-answer {:food 20 :mobs [(assoc (mob "drowned" 8 35) :held "trident")] :swim 4})))
      "a trident is thrown from 10 blocks"))

(deftest a-trident-drowned-is-priced-by-its-throws-not-a-melee-blow
  (let [trident (assoc (mob "drowned" 8 35) :held "trident")
        melee (mob "drowned" 8 35)
        hit #(:hit (cost/flee-answer {:food 20 :mobs [%] :swim 4}))]
    (is (= (threat/difficulty-scale 8) (hit trident)) "a throw is 8 hp, before the server difficulty")
    (is (> (hit trident) (hit melee)))
    (is (> (:damage (cost/fight-answer (fight "stone_sword" trident)))
           (:damage (cost/fight-answer (fight "stone_sword" melee))))
        "the fight prices its throws too")))

(deftest a-swim-to-a-ledge-with-no-land-to-walk-on-is-no-escape-from-a-drowned
  (let [drowned (mob "drowned" 3 35)
        flee #(cost/flee-answer {:food 20 :mobs [drowned] :swim 4 :land %})]
    (is (nil? (flee 0)) "the drowned keeps up on a ledge")
    (is (some? (flee ##Inf)) "land that goes on, or not known, is the walk away")))

(deftest a-lethal-threat-costs-the-body-its-death-not-the-hp-it-would-have-lost-too
  ;; a fist fight with a witch is 26 punches 1.3 s apart under her potions: 84 hp expected; the walk out of her range about 13
  (let [route (fn [weight & mobs] {:cells [(assoc here :s 60)] :threats (for [m mobs] {:mob m :weight weight :reach (constantly 1)})})]
    (doseq [r [(route 1 "witch") (route 0.3 "witch")]]
      (let [{:keys [chance hurt]} (cost/route-parts (body 8) r)]
        (is (< 0 hurt (* (- 1 chance) 8)) "the hp are paid only where the body lives: under its life by the chance it lives")))
    (is (< 0.6 (:chance (cost/route-parts (body 8) (route 1 "witch")))) "60 s in her range: she seldom misses the body")
    (is (<= (:hurt (cost/route-parts (body 8) (route 1 "witch" "witch"))) 8) "two take no more than the life"))
  (let [death (death/death-cost (body 5) here)]
    (is (< (js/Math.abs (- (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1} here) (+ death 1))) 1e-3)
        "a sure death is its death-cost and seconds: respawn heals the hp")))

(deftest a-second-in-water-costs-the-drowned-meet-rate-times-the-cheapest-answer-to-one
  (let [kit (assoc (body 20) :weapon "stone_sword")
        price (apply min (vals (:prices (cost/decide kit (answer/threat-answers (assoc kit :swim ##Inf) "drowned" nil) nil))))
        standing (apply min (vals (:prices (cost/decide kit (answer/threat-answers kit "drowned" nil) nil))))
        [deep other] (cost/water-factors kit)]
    (is (< (js/Math.abs (- deep (* 0.0021 price))) 1e-9) "water a drowned can spawn in: its measured meet rate")
    (is (< (js/Math.abs (- other (* 0.00043 standing))) 1e-9) "other water, mostly a wade: the meet answered standing")
    (is (< deep (first (cost/water-factors (body 20)))) "a bare body pays more than one with a sword")
    (is (< deep (first (cost/water-factors (assoc kit :health 8)))) "and a hurt one more than a whole one")))

(deftest a-phantom-floor-beside-seen-water-pays-its-chance-of-the-swim-there
  (settings/with-settings {:jobs.lib.cost.answer/unseen-water-lags [{:k 8 :n 8} {:k 2 :n 8}]}
    (fn []
      (let [kit (assoc (body 20) :weapon "stone_sword")
            [deep] (cost/water-factors kit)
            swim (dec (/ (* (/ 0.2 0.392) (inc deep)) 0.23164234422052352))]
        (is (every? #(< (js/Math.abs %) 1e-9) (map - [(* 0.9 swim) (* 0.3 swim)] (cost/unseen-water-factors kit))) "Beta(1,1) of each lag's count times the swim over the walk")))))

(deftest a-look-s-outcomes-meet-the-mobs-where-the-look-leaves-them
  (let [zombie (mob "zombie" 4)
        door #(hash-map :door (cost/cover-answer {:mobs % :then []} {:open-s 1 :seconds 5}))
        outcome-hp (fn [looks] (:damage (second (first (:outcomes (cost/look-answer (body 20) {:mobs [zombie] :looks looks :outcomes [[1 door]]} here))))))]
    (is (< (outcome-hp 1) (outcome-hp 10)) "the longer the look, the nearer the zombie when the door is shut")
    (is (= [(danger/melee-reach)] (map :distance (glance/approached [zombie] 60))) "it comes no nearer than its reach")
    (is (= [(mob "skeleton" 8)] (glance/approached [(mob "skeleton" 8)] 1)) "a shooter keeps its range")))

(deftest a-look-is-worth-what-it-may-change-less-what-it-costs
  (let [walls (cost/cover-answer {:mobs [(mob "skeleton" 8)]} {:open-s 9 :seconds 39})
        door (cost/cover-answer {:mobs [(mob "skeleton" 8)]} {:open-s 3 :seconds 33})
        now {:walls walls}
        look (fn [mobs chance] (cost/look-answer (body 5) {:mobs mobs :looks 4 :outcomes [[chance (constantly {:walls walls :door door})] [(- 1 chance) (constantly now)]]} here))
        value (fn [mobs chance] (cost/look-value (body 5) now (look mobs chance) here))]
    (is (zero? (:value (cost/look-value (body 5) now (cost/look-answer (body 5) {:mobs [] :looks 4 :outcomes [[1 (constantly now)]]} here) here)))
        "a look that can show nothing new changes nothing")
    (is (< (:cost (value [] 0.9)) (:value (value [] 0.9))) "likely closed: the cheaper door is worth the look")
    (is (= :look (:answer (cost/decide (body 5) (assoc now :look (look [] 0.9)) here))) "decide takes it")
    (is (> (:value (value [] 0.9)) (:value (value [] 0.2))) "the less likely the door, the less the look is worth")
    (is (= 1 (cost/closed-chance 11 0)) "all seen: closed")
    (is (= (/ 12 13) (cost/closed-chance 11 1)) "one unseen of twelve: the rule of succession")
    (is (> (cost/closed-chance 11 1) (cost/closed-chance 2 1) (cost/closed-chance 2 3)))))

(deftest a-swim-to-a-shore-whose-land-may-go-on-is-caught-at-the-chance-it-ends
  (let [m (mob "drowned" 3 35)
        known (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land 0})
        open (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land 0 :open 0.25})
        seen-land (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land ##Inf})]
    (is (nil? known) "the land seen ends at once and the mob keeps up")
    (is (< (js/Math.abs (- (- 1 (* (- 1 (:chance (caught/prior "drowned" 20))) 0.25)) (-> open :caught :chance))) 1e-9) "caught at 1 - (1 - the prior at 20 hp) x 0.25")
    (is (< (-> open :caught :damage) (-> (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land 0 :open 0.25 :later {:damage 100 :hit 3 :seconds 5}}) :caught :damage)) "a later fight is what the ending flight takes")
    (is (> (-> open :caught :chance) (-> seen-land :caught :chance)))
    (is (= (:caught seen-land) (:caught (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land ##Inf :open 0.25}))) "land enough already: the chance is no matter")))

(deftest a-drowned-fought-in-water-lands-its-measured-blows-for-each-swing-its-kill-needs
  (let [kit (assoc (body 20) :weapon "stone_sword")
        wet (:fight (answer/threat-answers (assoc kit :swim ##Inf) "drowned" nil))
        dry (:fight (answer/threat-answers kit "drowned" nil))
        blows (* 4 (:mean (fight/wet-rate)))]
    (is (< (js/Math.abs (- (:damage wet) (threat/mob-hurt (cost/armour-stats nil) "drowned" blows identity 1))) 1e-9) "4 stone sword swings kill it")
    (is (> (:damage wet) (* 1.5 (:damage dry))) "a land fight lands its swings")))

(deftest a-drowned-met-swimming-kills-by-the-answer-taken-its-weapon-and-the-shore
  (let [chance #(answer/threat-chance %1 "drowned" nil here)
        iron (assoc (body 20) :weapon "iron_sword")
        fist (body 20)]
    (is (< (chance iron) 0.01) "standing, as in a wade, an iron sword wins")
    (is (> (chance (assoc fist :swim ##Inf)) 0.1) "a bare body far from shore dies often whatever it answers")
    (let [answers (answer/threat-answers (assoc fist :swim 1) "drowned" nil)]
      (is (= :flee (:answer (cost/decide fist answers here))) "a bare body a block from shore swims out")
      (is (< (* 2 (answer/answer-death-chance fist (:flee answers) here)) (answer/answer-death-chance fist (:fight answers) here))
          "and that swim risks far less than a fight with it"))))

(deftest a-flight-whose-land-ends-where-the-mob-reaches-is-never-cheaper-than-the-fight-it-ends-in
  (let [m (mob "drowned" 4 35)
        fought (cost/fight-answer (fight "stone_sword" m))
        flee (cost/flee-answer {:food 20 :mobs [m] :swim 14 :land 0 :open 0 :later fought :caught 0.15 :caught-s 0.4})]
    (is (<= (:damage fought) (-> flee :caught :damage)) "caught on the way, the blows are the end's too")
    (is (= :fight (:answer (cost/decide (body 13) {:fight fought :flee flee} here))))))

(deftest a-walk-from-a-zombie-in-reach-takes-its-blows-until-the-gap-opens
  (let [price (fn [distance k]
                (let [m (mob "zombie" distance 35)
                      f (cost/fight-answer (fight "stone_sword" m))]
                  (get (:prices (cost/decide (body 3) {:fight f :flee (cost/flee-answer {:food 20 :mobs [m] :later f})} here)) k)))]
    (is (> (price 1 :flee) (price 1 :fight)) "at 3 hp a zombie beside the body lands its blows while the flight starts")
    (is (< (price 6 :flee) (price 6 :fight)) "one 6 off is left behind before it reaches the body")))

(deftest a-kinds-flight-starts-are-its-row-and-a-kind-with-none-the-pool
  (settings/with-settings {:jobs.lib.cost.danger/flight-starts {"pooled" {:n 10 :deciles (vec (repeat 10 1))}
                                                                 "fast" {:n 30 :deciles (vec (repeat 10 0))}}}
    #(is (= [(vec (repeat 10 0)) (vec (repeat 10 1))] [(danger/flight-starts "fast") (danger/flight-starts "unseen kind")])))
  (is (some? (a/problem :jobs.lib.cost.danger/flight-starts {"zombie" {:n 3 :deciles (vec (repeat 10 0))}}))
      "a setting with no pool is refused, not read as no starts"))


(deftest two-threats-are-met-in-one-answer-when-the-second-comes-while-the-first-is-answered
  (let [b (body 20)
        threat (fn [mob weight] {:mob mob :weight weight :reach (constantly 1)})
        route {:cells [(assoc here :s 1e6)] :threats [(threat "creeper" 0.5) (threat "zombie" 0.5)] :at here}
        alone #(answer/group-parts b % nil here)
        [c z cz] (map (comp :chance alone) [["creeper"] ["zombie"] ["creeper" "zombie"]])
        [tc tz] (map (comp :near alone) [["creeper"] ["zombie"]])
        both (fn [r] (* 0.25 0.5 (+ (- 1 (js/Math.exp (- (* r tc)))) (- 1 (js/Math.exp (- (* r tz)))))))
        lives (reduce + (map (fn [[r w]] (* w (+ (* (- 1 (* 0.5 c)) (- 1 (* 0.5 z))) (* (both r) (- (- 1 cz) (* (- 1 c) (- 1 z)))))))
                             (death/rate-nodes (death/notice-prior "zombie"))))]
    (is (< (js/Math.abs (- (:chance (cost/route-parts b route)) (- 1 lives))) 1e-9)
        "each there half the time, sure to notice over the stay, one rate for both: whichever comes first is answered while the other may come")))

(deftest a-flight-holds-the-body-near-the-mob-for-its-way-out-not-the-way-back
  (let [flee (:flee (answer/threat-answers (body 20) "zombie" nil))]
    (is (= :flee (:answer (cost/decide (body 20) (answer/threat-answers (body 20) "zombie" nil) here))))
    (is (= (/ (:seconds flee) 2) (:near (answer/group-parts (body 20) ["zombie"] nil here)))
        "another mob joins the flight only while the first still chases it")))

(deftest a-skeleton-gets-to-the-body-at-its-first-hit-so-that-arrow-has-landed-before-the-answer
  (let [arrow (answer/blow (armour/armour-stats nil) {:name "skeleton"})
        met #(answer/group-parts (body %) ["skeleton"] nil here)
        flee (fn [h] (let [as (answer/threat-answers (body h) "skeleton" nil)] (get as (:answer (cost/decide (body h) as here)))))]
    (is (= 1 (:chance (met arrow))) "a body one arrow kills dies when the skeleton gets to it")
    (is (< (js/Math.abs (- (:hurt (met 20)) (let [struck (body (- 20 arrow)) a (flee (- 20 arrow))]
                                              (+ (* arrow (- 1 (answer/blows-chance struck a))) (answer/lived-damage struck a)))))
           1e-9)
        "the hurt is that arrow and the answer's blows after it, where the body lives through them")
    (is (= (:near (met 20)) (:near (flee (- 20 arrow)))) "the answer starts at the hit")))

(deftest a-skeleton-stands-and-shoots-while-the-body-walks-out-of-its-bow-range
  (let [v (danger/flight-speed 20)
        walk (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 0)]})
        shot-s (/ answer/bow-range v)]
    (is (< (js/Math.abs (- (:damage walk) (threat/mob-hurt (armour/armour-stats nil) "skeleton" shot-s identity (threat/lone-hit-rate "skeleton")))) 1e-9)
        "shot only while the body walks out of its bow range at flight speed: vanilla's ranged attack goals stop the shooter within range")
    (is (< (js/Math.abs (- (:near walk) (+ shot-s (/ 1 (- v (danger/chase-speed "skeleton")))))) 1e-9)
        "past its range it chases, out to its follow range")))

(deftest on-hard-a-fed-body-beside-a-lone-skeleton-walks-away-and-most-likely-lives
  (settings/with-settings {::threat/difficulty 3}
    #(let [as (answer/threat-answers (body 20) "skeleton" nil)]
       (is (= :flee (:answer (cost/decide (body 20) as here))) "a fist fight is certain death")
       (is (< (answer/blows-chance (body 20) (:flee as)) 0.5)
           "the walk out of a 15-block bow range takes 4 s: two hard shots at most (2 s cycle), under 8 hp each, against 20 hp"))))

(defn partitions
  "Every partition of coll into blocks."
  [[x & more]]
  (if (nil? x)
    [[]]
    (mapcat (fn [p] (cons (cons [x] p) (map (fn [i] (update (vec p) i conj x)) (range (count p))))) (partitions more))))

(defn met-envelope
  "[lo hi], the least and the most chance that threats ts (met-threats') kill body on their route, over every way of
  meeting those that come (each partition of them into answers); nodes [[[r ...] w]] of their notice rates, one a threat."
  [b ts nodes]
  (let [alive (memoize #(- 1 (:chance (answer/group-parts b (vec (sort %)) nil here))))
        subsets (reduce (fn [ss i] (concat ss (map #(conj % i) ss))) [[]] (range (count ts)))
        lives (memoize (fn [s] (map (fn [p] (reduce * (map #(alive (map (comp :mob ts) %)) p))) (partitions s))))
        q (fn [r t] (* (:c t) (- 1 (js/Math.exp (- (* r (:exposure t)))))))
        expect (fn [pick] (reduce + (map (fn [[rs w]]
                                           (let [qs (mapv q rs ts)]
                                             (* w (reduce + (map (fn [s] (* (reduce * (map-indexed (fn [i qi] (if (some #{i} s) qi (- 1 qi))) qs))
                                                                            (apply pick (lives s))))
                                                                 subsets)))))
                                         nodes)))]
    [(- 1 (expect max)) (- 1 (expect min))]))

(def armed
  (assoc (body 20) :weapon sword
         :equipment {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"} :legs {:name "iron_leggings"} :feet {:name "iron_boots"}}))

(defn rate-grid
  "[[[r ...] w]]: the notice rates of mobs ms over their classes' death/rate-nodes, one rate a class."
  [ms]
  (let [ps (distinct (map death/notice-prior ms))
        grid (reduce (fn [g p] (for [[rs w] g [r v] (death/rate-nodes p)] [(assoc rs p r) (* w v)])) [[{} 1]] ps)]
    (map (fn [[rs w]] [(mapv #(rs (death/notice-prior %)) ms) w]) grid)))

(deftest a-pair-is-priced-between-met-apart-and-met-at-once-whenever-both-come
  (let [cells (for [x (range 160)] {:x x :y 64 :z 0})
        field (fn [mob x0] {:mob mob :weight 1 :reach (fn [x _ _] (let [d (js/Math.abs (- x x0))] (when (<= d (threat/follow-range mob)) (threat/way-weight d))))})]
    (doseq [[kit b] [["unarmed" (body 20)] ["armed" armed]]
            [m1 m2] [["zombie" "zombie"] ["skeleton" "skeleton"] ["zombie" "skeleton"]]
            gap [5 15 20 35 60]]
      (let [ts [(field m1 40) (field m2 (+ 40 gap))]
            chance (:chance (cost/route-parts b {:cells cells :threats ts}))
            [lo hi] (met-envelope b (answer/met-threats {:cells cells :threats ts} (death/cell-seconds cells)) (rate-grid [m1 m2]))]
        (is (<= (- lo 1e-12) chance (+ hi 1e-12))
            (str m1 " and " m2 " " gap " apart, " kit ": " chance " in " [lo hi]))))))

(deftest the-order-three-threats-are-taken-in-moves-their-price-by-under-a-quarter-of-what-meeting-them-can
  (let [cells (for [x (range 100)] {:x x :y 64 :z 0})
        ;; every field also weighs the first cell, so all three are first met there and taken in the order given
        field (fn [mob x0 w] {:mob mob :weight w :reach (fn [x _ _] (let [d (js/Math.abs (- x x0))] (when (or (zero? x) (<= d (threat/follow-range mob))) (threat/way-weight d))))})
        orders (fn [[a b c]] [[a b c] [a c b] [b a c] [b c a] [c a b] [c b a]])]
    (doseq [[kit b] [["unarmed" (body 20)] ["armed" armed]]
            ms [["zombie" "zombie" "zombie"] ["skeleton" "skeleton" "zombie"] ["creeper" "zombie" "skeleton"]]
            [g1 g2] [[0 10] [5 20] [15 30]]]
      (let [ts (map field ms [10 (+ 10 g1) (+ 10 g2)] [1 0.7 0.4])
            cs (map #(:chance (cost/route-parts b {:cells cells :threats %})) (orders ts))
            [lo hi] (met-envelope b (answer/met-threats {:cells cells :threats ts} (death/cell-seconds cells)) (rate-grid ms))
            what (str (apply str (interpose " " ms)) " at 10, " (+ 10 g1) ", " (+ 10 g2) ", " kit ": " (vec cs) " in " [lo hi])]
        (is (<= (- (apply max cs) (apply min cs)) (* 0.25 (- hi lo))) what)))))