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
   (let [f (cost/fight-answer (assoc (apply fight weapon mobs) :health health))]
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
          (str what " lands no small blows: the count's width is the fight's alone")))
    (let [walk (cost/flee-answer {:food 20 :mobs [(mob "drowned" 2 35)] :caught 1})
          caught (:caught (answer/followed wet walk))]
      (is (< (js/Math.abs (- (:shape caught) (fight/sum-shape [[(:damage wet) (:shape wet)] [(answer/small (:caught walk)) (:shape (:caught walk))]]))) 1e-9)
          "the walk caught after the fight: the width of both parts' blows"))))

(deftest a-fight-goes-bad-at-the-trial-fights-prior-and-a-flight-after-it-adds-its-own-catch
  (let [f #(cost/fight-answer (assoc (fight "stone_sword" (mob "zombie" 0)) :health %))
        bad (caught/prior ::caught/fights "zombie" 20)]
    (is (= (:chance bad) (-> (f 20) :caught :chance)) "gone bad at the prior of its health")
    (is (< (-> (f 20) :caught :chance) (-> (f 5) :caught :chance)) "likelier from low health")
    (is (< 0.03 (answer/answer-death-chance (body 20) (f 20) here) 0.12) "a sword fight begun at 20 hp dies about as often as the trials' (4 of 51)")
    (let [walk (cost/flee-answer {:food 20 :mobs [(mob "zombie" 2)] :caught 0.5})]
      (is (< (js/Math.abs (- (- 1 (* (- 1 (:chance bad)) (- 1 (-> walk :caught :chance)))) (-> (answer/followed (f 20) walk) :caught :chance))) 1e-9)
          "a fight left to a walk: caught if either goes bad"))))

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
         (select-keys (cost/decide (body 20) {:a {:damage 0 :seconds 10} :b {:damage 0 :seconds 5} :c nil} here) [:answer :prices])))
  (is (nil? (cost/decide (body 20) {:a nil} here)) "none open")
  (is (> (cost/answer-price (body 10) {:damage 4 :hit 2 :seconds 0} here []) (cost/answer-price (body 20) {:damage 4 :hit 2 :seconds 0} here []))
      "hp are dearer near the floor")
  (is (> (cost/answer-price (body 20) {:damage 0 :seconds 5 :then [{:damage 0 :seconds 7}]} here [])
         (cost/answer-price (body 20) {:damage 0 :seconds 5} here []))
      "what follows is paid too")
  (is (< (- (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1 :then [{:damage 0 :seconds 50}]} here [])
            (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1} here []))
         0.01)
      "a death all but sure ends the meet: what would follow it is not paid"))

(deftest a-death-goes-back-and-meets-its-mob-again-unless-giving-the-spot-up-is-cheaper
  (let [bed (assoc (body 5) :respawn {:x 40 :y 64 :z 0})
        lethal {:damage 100 :hit 5 :blasts [100] :seconds 1}
        safe {:damage 0 :seconds 30}
        back (death/back-cost bed here)
        near (fn [x y] (< (js/Math.abs (- x y)) 0.01))
        price (fn [b a answers] (cost/answer-price b a here answers))]
    (is (near (+ 1 back 30) (price bed lethal [lethal safe])) "nothing carried: its job brings it back, to the cheapest answer at full health")
    (is (near (+ 1 back 30) (price (assoc bed :carried 200) lethal [lethal safe])) "the items kept are picked up on that walk")
    (is (near (+ 1 1 back 30) (price bed {:damage 0 :seconds 1 :then [lethal]} [{:damage 0 :seconds 1 :then [lethal]} safe]))
        "a death in what follows pays it once too")
    (is (near (+ 1 (death/give-up-cost (assoc (body 5) :carried 200))) (price (assoc (body 5) :carried 200) lethal [lethal safe]))
        "no bed: the walk from world spawn costs more than the work there, the spot and the items are given up")
    (is (near (+ 1 10) (price (assoc bed :stake 10) lethal [lethal])) "a sure death waits there: the spot is given up, its work and no walk")
    (is (near (+ 1 back 1 10) (price (assoc bed :carried 200 :stake 10) lethal [lethal]))
        "with items worth more than the walk, it goes back for them once, and gives the spot up after the next death")
    (is (near (+ 1 (death/give-up-cost bed)) (price bed lethal [lethal])) "by default the work there is worth ::stake-s")
    (is (near 1 (price (assoc bed :stake 0) lethal [lethal safe])) "work worth nothing there is given up for free")
    (let [risky {:damage 20 :hit 5 :seconds 10}
          {:keys [met]} (cost/death-price bed [risky] here)
          again (cost/answer-price* (answer/healed bed) risky here {:met met :clear met})]
      (is (< 0.3 (cost/death-chance (body 20) risky) 0.9))
      (is (near met (+ back again)) "each death there costs the same once more: the fixed point"))
    (let [fight #(cost/fight-answer {:weapon nil :health 5 :mobs [{:name % :distance 0}]})
          extra #(- (price bed (fight %) [(fight %)]) (price bed (fight %) []))]
      (is (zero? (extra "creeper")) "a creeper's blast spends it: no mob waits at the spot")
      (is (pos? (extra "zombie")) "a zombie does"))))

(deftest a-death-price-stopped-by-the-round-backstop-says-so
  (let [bed (assoc (body 5) :respawn {:x 40 :y 64 :z 0})
        risky {:damage 12 :hit 5 :seconds 10}]
    (is (not (:capped (cost/death-price bed [risky] here))))
    (is (true? (:capped (with-redefs [answer/fixed-point-rounds 0] (cost/death-price bed [risky] here)))))))

(deftest the-death-price-solves-its-fixed-point-near-the-kinks-of-the-price
  (let [bed (assoc (body 5) :respawn {:x 40 :y 64 :z 0})
        e (assoc (answer/healed bed) :carried 0 :level 0)
        stake (death/give-up-cost e)
        back (death/back-cost e here)]
    (doseq [damage [8 12 16 20 30 60]
            seconds [1 10 60 400]
            then [nil [{:damage 0 :seconds 500}]]
            :let [a (cond-> {:damage damage :hit 5 :seconds seconds} then (assoc :then then))
                  x (- (:met (cost/death-price bed [a] here)) (death/experience-cost bed))]
            :when (< x (death/give-up-cost bed))
            :let [again (cost/answer-price* e a here {:met x :clear (min stake back)})]]
      (is (< (js/Math.abs (- x (min stake (+ back again)))) 1e-4)
          (str "damage " damage " seconds " seconds " then " (some? then) ": the price is its own fixed point")))))

(deftest the-death-price-is-its-own-fixed-point-where-a-kink-lies-just-over-its-root
  (let [bed (assoc (body 5) :respawn {:x 40 :y 64 :z 0})
        e (assoc (answer/healed bed) :carried 0 :level 0)
        stake (death/give-up-cost e)
        back (death/back-cost e here)]
    (doseq [damage [8 12 16]
            seconds (range 0.5 40 0.35)
            :let [a {:damage damage :hit 5 :seconds 1 :then [{:damage 12 :hit 5 :seconds seconds}]}
                  x (- (:met (cost/death-price bed [a] here)) (death/experience-cost bed))]
            :when (< x (death/give-up-cost bed))
            :let [again (cost/answer-price* e a here {:met x :clear (min stake back)})]]
      (is (< (js/Math.abs (- x (min stake (+ back again)))) 1e-4)
          (str "damage " damage " then-seconds " seconds ": the price is its own fixed point")))))

(def tol answer/fixed-point-tolerance-s)

(defn root [f stake] (:root (answer/least-fixed-point f stake)))

(deftest the-least-fixed-point-is-the-least-of-several-roots
  (let [f (fn [x] (cond (< x 10) (+ 5 (* 0.5 x)) (< x 12) (+ 10 (* 5 (- x 10))) :else 20))]
    (is (< (js/Math.abs (- 10 (root f 100))) tol) "f(x)-x is positive between the roots 10 and 20: a scan with a step over them lands on 20")
    (is (zero? (root (constantly 0) 100)))
    (is (= ##Inf (root #(+ 1 (* 2 %)) ##Inf)) "no root and no stake to bound it")
    (is (= 7 (root #(min 7 (+ 1 (* 2 %))) 7)))))

(deftest a-slope-near-one-is-stopped-by-its-error-in-seconds-in-a-few-rounds-not-1000
  (let [calls (atom 0)
        f (fn [x] (swap! calls inc) (+ 1 (* 0.99 x)))
        r (answer/least-fixed-point f 1e6)]
    (is (< (js/Math.abs (- 100 (:root r))) tol) "the root is 100: the geometric tail is added, not left as the lower bound")
    (is (not (:capped r)))
    (is (< @calls 30) (str "rounds: " @calls))))

(deftest steps-that-grow-are-iterated-on-to-the-root
  (is (= 8 (root #(min 8 (+ 1 (* 2 %))) 100)) "steps 1 2 4 grow before the root: the iterates 0 1 3 7 8 are lower bounds, not a limit past the stake")
  (is (= 8 (root #(min 8 (+ 1 (* 2 %))) ##Inf)) "an infinite stake does not make a finite root infinite"))

(deftest steps-that-never-shrink-end-at-the-stake-or-infinity
  (let [grow #(+ 1 %)]
    (is (= ##Inf (root grow ##Inf)))
    (is (= 100 (root grow 100)) "iterating reaches the stake")
    (is (= ##Inf (root #(* 2 (+ 1 %)) ##Inf)) "doubling passes the cap in a few rounds")))

(deftest the-round-backstop-flags-a-limit-that-never-settles
  (let [r (with-redefs [answer/fixed-point-rounds 5] (answer/least-fixed-point #(+ % (/ 1 (+ 1 %))) 1e6))]
    (is (:capped r))
    (is (< (:root r) ##Inf))))

(deftest a-backstop-hit-on-an-infinite-stake-with-steps-not-shrinking-is-infinity
  (let [r (with-redefs [answer/fixed-point-rounds 5] (answer/least-fixed-point #(+ 1 %) ##Inf))]
    (is (= ##Inf (:root r)))))

(defn lcg
  "A deterministic stream of numbers in [0 1) from seed."
  [seed]
  (rest (map #(/ % 2147483647.0) (iterate #(mod (+ (* % 48271) 11) 2147483647) seed))))

(defn gen-answer
  "An answer built from the stream rs, of the shapes the answers take (see the ns doc): a plain one, with a :then, :at-once
  into a :then, :stays beside a :then (a cover, with a failing outcome that goes at once into a :then), :outcomes, :caught
  or :blasts; returns [answer rest-of-stream]."
  [depth [a b c d e f g & rs]]
  (let [base {:damage (* 22 a) :hit 5 :seconds (* 30 b)}
        [sub rs] (if (pos? depth) (gen-answer (dec depth) rs) [nil rs])
        [sub2 rs] (if (pos? depth) (gen-answer (dec depth) rs) [nil rs])]
    [(cond-> (cond
               (nil? sub) base
               (< c 0.2) (assoc base :then [sub])
               (< c 0.35) (assoc base :then [sub] :at-once true)
               (< c 0.55) (assoc base :then [sub] :stays (* 0.95 d))
               (< c 0.75) (assoc base :then [sub] :stays (* 0.95 d)
                                 :outcomes [[(* 0.9 e) {:damage 0 :hit 0 :seconds 0 :then [sub2] :at-once true}]])
               (< c 0.9) (assoc base :outcomes [[(* 0.9 d) sub] [(- 1 (* 0.9 d)) sub2]])
               :else base)
       (< 0.6 e 0.8) (assoc :caught {:chance f :damage (* 22 f) :hit 5})
       (< 0.9 g) (assoc :blasts [(* 10 f)]))
     rs]))

(deftest the-death-price-iteration-is-nondecreasing-in-the-price-of-a-death-met-again
  (let [bed (assoc (body 5) :respawn {:x 40 :y 64 :z 0})]
    (loop [seed 1 n 0]
      (when (< n 60)
        (let [[a rs] (gen-answer 2 (lcg seed))
              [b _] (gen-answer 1 rs)
              {:keys [stake fixed]} (answer/death-fixed (if (even? n) bed (body 12)) [a b] here)
              xs (range 0 (min stake 600) 7)
              ys (mapv fixed xs)]
          (is (every? (fn [[y1 y2]] (>= (+ y2 1e-7) y1)) (partition 2 1 ys)) (str "seed " seed " answers " (pr-str [a b])))
          (recur (+ seed 7919) (inc n)))))))

(deftest a-stake-nothing-bounds-leaves-a-death-that-never-ends-unpriced-not-nan
  (let [bed (assoc (body 5) :respawn {:x 40 :y 64 :z 0} :stake ##Inf)
        lethal {:damage 100 :hit 5 :blasts [100] :seconds 1}]
    (is (= ##Inf (:met (cost/death-price bed [lethal] here))))))

(deftest a-survivor-goes-on-or-gives-the-spot-up-as-a-death-does
  (let [dear {:damage 100 :hit 5 :seconds 5000}
        after-it {:damage 0 :seconds 1 :then [dear]}]
    (is (= (+ 1 (death/give-up-cost (body 20))) (cost/answer-price (assoc (body 20) :carried 300) after-it here []))
        "what follows costs more than the work there: the spot is given up, the items kept")
    (is (zero? (answer/answer-death-chance (body 20) after-it here)) "and the deadly meet is not taken")))

(deftest the-walk-back-meets-the-mob-fresh-not-with-the-blows-felt-or-the-dead-end-here
  (let [bed (assoc (body 20) :respawn {:x 40 :y 64 :z 0} :weapon sword)
        zombie (mob "zombie" 2 35)
        f (cost/fight-answer (fight sword zombie))
        felt (assoc f :damage 40)
        dead-end (cost/flee-answer {:food 20 :mobs [zombie] :later f :caught 1})
        here {:fight felt :flee dead-end}
        fresh (answer/again-answers bed "zombie")
        met #(:met (:death (apply cost/decide bed here {:x 0 :y 64 :z 0} %&)))]
    (is (< (met fresh) (met)) "the blows felt and the dead end are this meet's: the walk back comes from the bed side")
    (is (= (cost/death-price bed fresh {:x 0 :y 64 :z 0}) (:death (cost/decide bed here {:x 0 :y 64 :z 0} fresh))))))

(deftest a-blast-spends-its-creeper-only-when-no-other-mob-is-left
  (is (:spent (cost/fight-answer (fight sword (mob "creeper" 3)))))
  (is (not (:spent (cost/fight-answer (fight sword (mob "creeper" 3) (mob "creeper" 5))))) "the other creeper still waits"))

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
  (settings/with-settings {:jobs.lib.cost.threat/difficulty 3}
    (fn []
      (let [skeleton (mob "skeleton" 12)
            fight (cost/fight-answer (assoc (fight sword skeleton) :mobs [skeleton]))
            choose (fn [returns walk-hp]
                     (let [f (-> fight (update :damage + walk-hp) (cond-> (:caught fight) (update-in [:caught :damage] + walk-hp)))]
                       (:answer (cost/decide (body 20) {:fight f :flee (cost/flee-answer {:food 20 :mobs [skeleton] :returns returns :later f})} here))))]
        (is (= :flee (choose 0 0)) "no flight lately: a step out of its range is cheaper than closing 12 blocks")
        (is (= :fight (choose 2 0)) "fled twice, the walks again dearer than the fight, if the walk to it is free (go-to's lower bound)")
        (is (= :flee (choose 2 8)) "8 hp of go-to's quote of the walk to it: the walks away are the cheaper")))))

(deftest a-flight-ending-past-follow-range-is-not-flown-again-when-the-way-back-passes-the-skeleton
  (settings/with-settings {:jobs.lib.cost.threat/difficulty 3}
    (fn []
      (let [choose (fn [again returns]
                     (let [skeleton (cond-> (mob "skeleton" 15) again (assoc :again again))
                           f (cost/fight-answer (assoc (fight sword skeleton) :mobs [skeleton]))]
                       (:answer (cost/decide (body 20) {:fight f :flee (cost/flee-answer {:food 20 :mobs [skeleton] :returns returns :later f})} here))))]
        (is (= [:fight :fight] (mapv #(choose 1 %) [0 1])) "the goal lies past it: the trigger refiring on the walk back is already priced, at the first meet and after a flight, so the body does not pendulum at 16")
        (is (= [:flee :flee] (mapv #(choose nil %) [0 1])) "no way back past it: the flight is the last")))))

(deftest a-mob-between-the-body-and-its-goal-is-walked-from-again-from-where-the-way-back-passes-it
  (settings/with-settings {:jobs.lib.cost.threat/difficulty 3}
    (fn []
      (let [fight (cost/fight-answer (fight sword (mob "skeleton" 12)))
            choose #(:answer (cost/decide (body 20) {:fight fight :flee (cost/flee-answer {:food 20 :mobs [%] :later fight})} here))]
        (is (= :flee (choose (mob "skeleton" 12))) "nothing past it: stepping out of its range now and again is cheaper than closing 12 blocks under its arrows")
        (is (= :fight (choose (assoc (mob "skeleton" 12) :again 1))) "the way back passes a block from it: the walk again is from its feet, the whole bow range")
        (is (= (choose (mob "skeleton" 12)) (choose (assoc (mob "skeleton" 12) :again 14))) "a way that passes farther than it stands changes nothing")))))

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

(deftest the-prior-and-the-ways-catch-are-one-mix-so-a-small-dead-end-chance-moves-the-price-little
  (let [zombie (mob "zombie" 4 35)
        price #(cost/answer-price (body 12) (cost/flee-answer {:health 12 :food 20 :mobs [zombie] :caught %}) here [])
        caught #(:caught (cost/flee-answer {:health 12 :food 20 :mobs [zombie] :caught %1 :caught-s %2}))
        blows #(let [{:keys [chance damage]} (caught % 0.5)] (* chance damage))]
    (is (< (/ (- (price 0.01) (price 0)) (price 0)) 0.02) "a way that goes on 99 times in 100 prices as a seen one, near enough")
    (is (< (price 0) (price 0.5) (price 1)) "the likelier its dead end, the dearer")
    (is (< (blows 0) (blows 0.5) (blows 1)) "caught by the race's half second and the prior both, the body takes the prior's blows")
    (is (= (:damage (caught 0 nil)) (:damage (caught 0 0.5))) "on land the race's seconds bound nothing: the prior's own blows, counted on land flights")))

(deftest a-swim-then-a-walk-is-caught-for-no-longer-than-the-race-keeps-the-drowned-in-reach-on-the-swim-only
  (let [m (mob "drowned" 4 35)
        caught #(:damage (:caught (cost/flee-answer {:health 13 :food 20 :mobs [m] :swim 6 :land %1 :caught 0 :caught-s %2})))
        own (:damage (cost/flee-answer {:health 13 :food 20 :mobs [m] :swim 6 :land ##Inf}))
        capped (max own (threat/mob-hurt (cost/armour-stats nil) (threat/kind m) (* 0.5 (answer/reach-rate m)) identity 1))]
    (is (< capped (caught ##Inf 0.5) (caught ##Inf nil)) "the race's seconds bound the swim's blows, the prior's the walk's on land")
    (is (= (caught 1000 0.5) (caught ##Inf 0.5)) "land past the walk that outruns the follow range adds no blows")))

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

(deftest a-cover-that-may-fail-pays-the-fight-after-it-by-that-chance
  (let [zombie (mob "zombie" 1)
        fight (cost/fight-answer (fight nil zombie))
        price #(cost/answer-price (body 10) (cost/cover-answer {:mobs [zombie] :then [fight] :failed [fight]} {:open-s 1.4 :seconds 35 :fails %}) here)]
    (is (< (price 0) (price 0.4) (price 0.9)) "a placement refused for the mob in its cell leaves the fight, not seconds in the open")))

(deftest a-cover-whose-mob-may-stay-pays-the-meet-again-or-the-cover-again
  (let [zombie (mob "zombie" 1)
        fight (cost/fight-answer (fight nil zombie))
        cover {:open-s 1 :seconds 31}
        price (fn [stays] (settings/with-settings {:jobs.lib.cost.answer/hide-stays stays}
                            #(cost/answer-price (body 20) (cost/cover-answer {:mobs [zombie] :then [fight]} cover) here)))
        alone (cost/answer-price (body 20) (assoc (cost/build-answer {:mobs [zombie] :seconds 1}) :seconds 31) here)]
    (is (< (price {:k 0 :n 1000}) (price {:k 40 :n 100}) (price {:k 90 :n 100})) "the likelier the zombie waits below, the dearer the pillar")
    (is (< (js/Math.abs (- (price {:k 0 :n 1000}) alone)) 0.05) "it never waits: the cover alone")
    (is (< (price {:k 99 :n 100}) (+ alone (cost/answer-price (body 20) fight here))) "hiding again and again is bounded by the fight it may end in")))

(deftest a-threats-cover-is-priced-as-respond-prices-it
  (let [cover {:open-s 2 :seconds 50}
        zombie (mob "zombie" 0 35)]
    (is (= (:walls (cost/cornered-answers {:mobs [zombie] :fight (:fight (answer/threat-answers {:food 20} "zombie" nil)) :covers {:walls cover}}))
           (:walls (answer/threat-answers {:food 20 :covers {:walls cover}} "zombie" nil)))
        "one cover-answer over shut-in/covers' seconds, the hide-out's quiet in them, for death-chance and respond alike")))

(deftest a-mob-that-came-back-before-makes-a-walk-dearer
  (let [walk #(cost/answer-price (body 20) (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 10)] :returns %}) here [])]
    (is (> (walk 2) (walk 1) (walk 0)))))

(deftest a-lone-creeper-is-fought-hit-and-back-at-the-measured-exchange
  (let [price #(cost/answer-price (body %) (cost/fight-answer (fight sword (mob "creeper" 3))) here [])]
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
    (is (> (cost/lethal-hp (cost/route-death (body 20) {:spawns spawns :at here})) 10))
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
    (is (< (js/Math.abs (- (cost/answer-price (body 5) {:damage 100 :hit 5 :seconds 1} here []) (+ death 1))) 1e-3)
        "a sure death with no mob met again is its death-cost and seconds: respawn heals the hp")))

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
    (let [ashore #(-> (cost/flee-answer {:food 20 :mobs [m] :swim 4 :land 0 :open 0.25 :later %}) :caught :damage)
          fight {:damage 1 :hit 3 :seconds 5}]
      (is (< (ashore fight) (ashore (assoc fight :caught {:chance 0.5 :damage 100 :hit 3}))) "and that fight's going bad"))
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
    (is (< (* 2 (chance iron)) (chance (assoc fist :swim ##Inf))) "standing, as in a wade, an iron sword wins, but for its fight going bad (cost.caught ::fights)")
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
  (let [v (danger/flee-walk-speed 20)
        walk (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 0)]})
        shot-s (/ answer/bow-range v)]
    (is (< (js/Math.abs (- (:damage walk) (threat/mob-hurt (armour/armour-stats nil) "skeleton" shot-s identity (threat/lone-hit-rate "skeleton")))) 1e-9)
        "shot only while the body walks out of its bow range at flight speed: vanilla's ranged attack goals stop the shooter within range")
    (is (< (js/Math.abs (- (:near walk) (+ shot-s (/ 1 (danger/closing-rate 20 "skeleton"))))) 1e-9)
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
