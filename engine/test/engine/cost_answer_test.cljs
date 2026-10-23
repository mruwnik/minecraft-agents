(ns engine.cost-answer-test
  "jobs.lib.cost.answer: one price for each answer to hostiles (fight, walk away, build a refuge) and the cheapest."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.death :as death]))

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
                           {:fight f :flee (cost/flee-answer {:food 20 :mobs mobs :returns returns :later f})}
                           here)))))

(deftest blows-that-may-take-the-health-are-the-death-risk
  (is (zero? (death/blow-chance {:health 20} 0 3)))
  (is (< (death/blow-chance {:health 20} 4 2.5) 0.01) "a fight of 4 hp at full health hardly kills")
  (is (< 0.4 (death/blow-chance {:health 5} 4 2.5) 0.6) "at 5 hp it may well")
  (is (< (death/blow-chance {:health 5 :absorption 4} 4 2.5) (death/blow-chance {:health 5} 4 2.5)) "absorption is health")
  (is (= (death/blow-chance (body 5) 4 2.5) (cost/death-chance (body 5) {:damage 4 :hit 2.5})) "the death chance of an answer's blows"))

(deftest the-cheapest-open-answer-is-taken
  (is (= {:answer :b :prices {:a 10 :b 5}}
         (cost/decide (body 20) {:a {:damage 0 :seconds 10} :b {:damage 0 :seconds 5} :c nil} here)))
  (is (nil? (cost/decide (body 20) {:a nil} here)) "none open")
  (is (> (cost/answer-price (body 10) {:damage 4 :hit 2 :seconds 0} here) (cost/answer-price (body 20) {:damage 4 :hit 2 :seconds 0} here))
      "hp are dearer near the floor")
  (is (> (cost/answer-price (body 20) {:damage 0 :seconds 5 :then [{:damage 0 :seconds 7}]} here)
         (cost/answer-price (body 20) {:damage 0 :seconds 5} here))
      "what follows is paid too"))

(deftest a-fight-is-taken-when-it-is-cheaper-than-walking-away-and-meeting-the-mob-again
  (are [health weapon mobs expected] (= expected (choice health weapon mobs))
    20 sword [(mob "zombie" 3 35)] :fight
    5 sword [(mob "zombie" 3 35)] :flee
    20 nil [(mob "zombie" 3 35)] :flee
    20 sword [(mob "zombie" 3 35) (mob "zombie" 3 35) (mob "zombie" 3 35)] :flee
    20 sword [(mob "skeleton" 10)] :fight))

(deftest a-walk-away-is-caught-at-the-prior-and-so-loses-to-a-fight-or-a-wall-it-would-beat-on-an-open-way
  (let [zombie (mob "zombie" 3 35)
        f (cost/fight-answer (fight sword zombie))
        flee #(cost/flee-answer {:food 20 :mobs [zombie] :later f :caught %})
        wall (assoc (cost/build-answer {:mobs [zombie] :seconds 2}) :seconds 120)
        pick #(:answer (cost/decide (body 8) %1 here))]
    (is (= :flee (pick {:fight f :flee (flee 0)})) "at 8 hp an open way beats the fight")
    (is (= :fight (pick {:fight f :flee (flee nil)})))
    (is (= :flee (pick {:wall wall :flee (flee 0)})) "a wall that holds the body long costs more than an open way")
    (is (= :wall (pick {:wall wall :flee (flee nil)})))
    (is (> (cost/death-chance (body 20) (flee 1)) 0.9) "caught, the zombie lands its blows all the flight")))

(deftest a-walk-from-a-mob-that-keeps-up-is-no-answer
  (is (nil? (cost/flee-answer {:food 20 :mobs [(mob "spider" 4)]})) "a spider outruns the body")
  (is (some? (cost/flee-answer {:food 20 :mobs [(mob "zombie" 4 35)]})))
  (is (pos? (:damage (cost/flee-answer {:food 20 :mobs [(mob "creeper" 5)]}))) "a creeper 5 off swells before the walk gets going")
  (is (zero? (:damage (cost/flee-answer {:food 20 :mobs [(mob "creeper" 9)]})))))

(deftest a-creeper-that-blows-on-the-walk-is-not-met-again
  (let [zombie (mob "zombie" 4 35)]
    (is (nil? (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 3)]}))) "a lone creeper blown is gone")
    (is (= (cost/flee-answer {:food 20 :mobs [zombie]}) (first (:then (cost/flee-answer {:food 20 :mobs [(mob "creeper" 3) zombie]}))))
        "after the blast only the zombie is walked from again")))

(deftest a-body-that-can-neither-outrun-nor-outfight-a-spider-digs-in
  (let [spider (mob "spider" 2)
        answers {:fight (cost/fight-answer (fight nil spider)) :flee (cost/flee-answer {:food 20 :mobs [spider]})}]
    (is (= :fight (:answer (cost/decide (body 20) answers here))) "with no cover the fist is all that is left")
    (is (= :pit (:answer (cost/decide (body 20) (assoc answers :pit (cost/cover-answer {:mobs [spider]} {:open-s 4 :seconds 40})) here)))
        "a pit dug in 4 s under the spider's blows beats a fist fight it hardly survives")
    (is (nil? (cost/cover-answer {:mobs [(mob "creeper" 4)]} {:open-s 4 :seconds 40})) "a blast takes the cover")))

(deftest a-mob-that-came-back-before-makes-a-walk-dearer
  (let [walk #(cost/answer-price (body 20) (cost/flee-answer {:food 20 :mobs [(mob "skeleton" 10)] :returns %}) here)]
    (is (> (walk 2) (walk 1) (walk 0)))))

(deftest a-creeper-near-enough-to-swell-before-the-walk-gets-going-or-that-keeps-coming-back-is-fought-hit-and-back
  (let [price #(cost/answer-price (body %) (cost/fight-answer (fight sword (mob "creeper" 3))) here)]
    (is (> (price 10) (* 2 (price 20)))))
  (are [health weapon distance returns expected] (= expected (choice health weapon [(mob "creeper" distance)] returns))
    20 "stone_sword" 4 0 :fight
    20 "stone_sword" 9 0 :flee
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



