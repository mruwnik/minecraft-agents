(ns engine.threats-test
  "jobs.lib.threats' known threats and their tolls for the planner: sensed real dangers and remembered :threat spots, costed by what the hostile
  reflex would do (flee: dear, fight: cheap)."
  (:require [cljs.test :refer [deftest is are use-fixtures]]
            [engine.fake :as fake]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.ready-test :as ready-test]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.threats :as threats]))

(use-fixtures :each {:before #(game/select! (fake/create {}))})

(def unarmed {:health 20 :equipment nil :weapon nil :pos {:x 0 :y 64 :z 0}})
(def armed (assoc unarmed :weapon "diamond_sword"))

(deftest stance-is-what-the-reflex-would-do
  (are [body mob stance] (= stance (cost/stance body mob))
    unarmed "zombie" :flee
    armed "zombie" :fight
    armed "creeper" :flee))

(deftest rate-falls-with-a-weapon-and-armour-and-is-the-hp-a-second-not-the-price
  (let [rate #(/ (js/Math.round (* 100 (cost/danger-rate %1 %2))) 100)]
    (is (= 4 (rate unarmed "zombie")) "3 dps x 4 (flee), capped at 4")
    (is (= 0.25 (rate armed "zombie")) "2.5 hp a second (a hit of 2.5 on easy) x 0.1 (fight)")
    (is (= 0.25 (rate (assoc armed :health 10) "zombie")) "health 10: still a fight; the dearer hp is the planner's damage weight")
    (is (= 4 (rate (assoc armed :health 5) "zombie")) "health 5: the fight would leave too little, fled")
    (is (= 0.16 (rate (assoc armed :equipment {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"}
                                              :feet {:name "iron_boots"}}) "zombie"))
        "10 iron points: vanilla's 8.5 effective points against a 2.5 hp hit take off more")
    (is (= 4 (rate armed "creeper")) "a creeper is always fled")))

(defn zombie [id x z] {:id id :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z}})

(def wall (into {} (for [x [2 3] y [64 65 66] z (range -4 5)] [(str x "," y "," z) "stone"])))

(defn tolls [world] (into {} (map (juxt (juxt :x :y :z) :factor)) (threats/threat-tolls (ready-test/ctx-of world))))

(deftest a-sensed-zombie-tolls-its-way-dear-to-flee-cheap-to-fight
  (let [bare (tolls {:entities [(zombie 1 6 0)]})
        armed (tolls {:entities [(zombie 1 6 0)] :inventory [{:name "diamond_sword" :count 1}]})]
    (is (> (get bare [6 64 0]) (get bare [-14 64 0]) 0) "dearest at the zombie, less along its way")
    (is (< (get armed [6 64 0]) (get bare [6 64 0])) "with a diamond sword: a fight, cheap")
    (is (= {} (tolls {:blocks wall :entities [(zombie 1 6 0)]})) "behind a wall the body never saw it through: not known (no x-ray)")))

(deftest the-sensed-place-wins-over-a-flight-from-the-same-mob-and-at-most-max-threats
  (let [c (assoc (ready-test/ctx-of {:entities [(assoc (zombie 1 6 0) :uuid "a")]}) :view
                 (fn [] {:now 0 :data {:entries {:threat [{:t 0 :data {:mob "zombie" :id 1 :uuid "a" :key "a" :pos {:x 30 :y 64 :z 0}}}]}}}))]
    (is (= [6] (mapv (comp :x :pos) (threats/known-threats c)))))
  (is (= (threats/max-threats)
         (count (threats/known-threats (ready-test/ctx-of {:entities (for [i (range 12)] (zombie i (* 2 i) 0))}))))))

(deftest danger-terms-are-options-and-the-defaults-keep-the-price
  (let [rate #(cost/danger-rate unarmed "zombie" %)]
    (is (= (rate nil) (rate {})) "no options: today's rate")
    (is (= 4 (rate {})) "default 2.5 hp a second x 4 (flee), capped at 4")
    (is (= 1.25 (rate {:stances {:flee 0.5}})) "the flee factor scales the rate")
    (is (< (js/Math.abs (- 0.5 (cost/danger-rate armed "zombie" {:stances {:fight 0.2}}))) 1e-9) "the fight factor")
    (is (= 2 (rate {:max-rate 2})) "max-rate caps one danger")
    (is (= 8 (rate {:max-rate 8 :stances {:flee 8}})) "a higher cap lets more through: 12 capped at 8")))

(deftest the-danger-settings-name-the-danger-options
  (is (= {:stances {:flee 2 :fight 0.5} :max-rate 3}
         (settings/with-settings {:jobs.lib.cost.danger/flee-factor 2 :jobs.lib.cost.danger/fight-factor 0.5 :jobs.lib.cost.danger/max-rate 3}
           cost/danger-opts)))
  (is (= {:stances {:flee 4 :fight 0.1} :max-rate 4} (cost/danger-opts)) "the defaults"))

(defn fled-view
  "A memory view at t 1000 whose :threat entries were written at the given ms with the given data."
  [& written]
  (let [now (atom 0)
        s (mem/open (tu/tmp-dir) {:now #(deref now)})]
    (doseq [[t data] (partition 2 written)]
      (reset! now t)
      (mem/write! s :threat data threats/threat-policy))
    (reset! now 1000)
    (mem/view s)))

(defn ctx-on "A job ctx over primitives p and memory view v." [p v] {:primitives p :view (fn [] v) :id "j" :slots [] :engine {:warned (atom #{})}})

(def pen
  "A stone ring 3 high round x 7..13, z -3..3 (the skeleton's place at its middle)."
  (into {} (for [x (range 7 14) z (range -3 4) :when (or (#{7 13} x) (#{-3 3} z)) y [64 65 66]] [(str x "," y "," z) "stone"])))

(def skeleton-9 {:id 9 :name "skeleton" :kind "hostile" :pos {:x 10 :y 64 :z 0}})

(deftest a-flight-counts-for-the-fled-memory-window-only
  (let [v (fled-view 0 {:mob "skeleton" :id 9 :pos {:x 10 :y 64 :z 0}})
        p (tu/fake-on-floor {:entities [skeleton-9]})]
    (is (true? (threats/fled-recently? p v)))
    (is (false? (settings/with-settings {::threats/fled-memory-ms 500} #(threats/fled-recently? p v))))
    (is (false? (threats/fled-recently? p (fled-view))))
    (is (false? (threats/fled-recently? (tu/fake-on-floor {}) v)) "the fled mob was killed: no danger to keep away from")))

(deftest a-fled-mob-counts-only-where-its-field-reaches
  (let [v (fled-view 0 {:mob "zombie" :id 9 :pos {:x 10 :y 64 :z 0}})
        p (tu/seeing-all (tu/fake-on-floor {:blocks pen :entities [(assoc skeleton-9 :name "zombie")]}))]
    (is (true? (threats/fled-near? (ctx-on p v) {:x 11 :y 64 :z 1})) "inside its pen")
    (is (false? (threats/fled-near? (ctx-on p v) {:x 16 :y 64 :z 0})) "6 blocks off outside the pen it cannot leave")))

(deftest a-fled-mob-is-near-a-spot-its-field-holds-from-where-it-was-last-known
  (let [v (fled-view 0 {:mob "skeleton" :id 9 :pos {:x 10 :y 64 :z 0}})
        p (tu/fake-on-floor {:entities [skeleton-9]})]
    (are [spot near?] (= near? (threats/fled-near? (ctx-on p v) spot))
      {:x 12 :y 64 :z 0} true
      {:x 100 :y 64 :z 0} false)
    (is (false? (settings/with-settings {::threats/fled-memory-ms 500} #(threats/fled-near? (ctx-on p v) {:x 12 :y 64 :z 0}))))))

(deftest a-fled-mob-the-client-no-longer-lists-is-not-near-anywhere
  (let [v (fled-view 0 {:mob "creeper" :id 9 :pos {:x 10 :y 64 :z 0}})
        p (tu/fake-on-floor {})]
    (is (false? (threats/fled-near? (ctx-on p v) {:x 12 :y 64 :z 0})))
    (is (= [] (threats/listed-threats p (mem/entries v :threat))))))

(deftest a-fled-mob-heard-only-is-placed-by-its-direction-and-band
  (let [v (fled-view 0 {:mob "zombie" :id 9 :direction :north :band :near :from {:x 0 :y 64 :z 0}})
        p (tu/fake-on-floor {:entities [(assoc skeleton-9 :name "zombie")]})]
    (is (true? (threats/fled-near? (ctx-on p v) {:x 0 :y 64 :z -5})))))
