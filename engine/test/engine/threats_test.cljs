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
            [jobs.lib.cost.mob-toll :as mob-toll]
            [jobs.lib.cost.death]
            [jobs.lib.cost.health]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.threats :as threats]))

(use-fixtures :each {:before #(game/select! (fake/create {}))})

;; :carried: a body with nothing to lose fist-fights a zombie (its death costs only the walk back)
(def unarmed {:health 20 :equipment nil :weapon nil :pos {:x 0 :y 64 :z 0} :carried 1000})
(def armed (assoc unarmed :weapon "diamond_sword"))

(defn answered [body mob] (cost/answered-together body [mob] (:pos body)))

(deftest stance-is-what-the-reflex-would-do
  (are [body mob stance] (= stance (:stance (answered body mob)))
    unarmed "zombie" :flee
    armed "zombie" :fight
    (assoc armed :health 5) "zombie" :fight
    unarmed "creeper" :flee
    armed "creeper" :fight))

(defn toll [body mob] (cost/threat-factor body {:name mob} 1 (answered body mob)))

(deftest toll-falls-with-a-weapon-and-armour-and-follows-the-fights-own-price
  (let [wooden (assoc unarmed :weapon "wooden_sword")
        iron {:head {:name "iron_helmet"} :torso {:name "iron_chestplate"} :feet {:name "iron_boots"}}]
    (is (= :fight (:stance (answered wooden "zombie"))))
    (is (< 0 (toll armed "zombie") (toll wooden "zombie")) "a diamond sword's fight costs less a second than a wooden one's")
    (is (< (toll (assoc armed :equipment iron) "zombie") (toll armed "zombie")) "armour lowers the fight's blows")
    (is (< (toll armed "zombie") (toll unarmed "zombie")) "a flight costs more than a fight")))

(defn zombie [id x z] {:id id :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z}})

(def wall (into {} (for [x [2 3] y [64 65 66] z (range -4 5)] [(str x "," y "," z) "stone"])))

(defn tolls [world] (into {} (map (juxt (juxt :x :y :z) :factor)) (threats/threat-tolls (ready-test/ctx-of world))))

(deftest a-pack-met-together-is-fled-though-each-of-it-is-fought-alone
  (let [body (assoc armed :carried 1000 :respawn {:x 5000 :y 64 :z 0})
        together (cost/answered-together body (repeat 4 "zombie") (:pos body))]
    (is (= :fight (:stance (answered body "zombie"))) "one zombie: a fight")
    (is (= :flee (:stance together)) "four met together: the reflex flees them, none is tolled as a fight")))

;; old stances keyed a whole kind ({:name :held}) by its dearest group and joined fields sharing any cell, so the far
;; zombie, 55 blocks off, was fled with the pack
(deftest a-lone-zombie-far-from-a-fled-pack-keeps-its-own-fight
  (let [c (ready-test/ctx-of {:inventory [{:name "diamond_sword" :count 1}]
                              :entities (conj (mapv #(zombie % 30 (- % 2)) [1 2 3 4]) (zombie 5 -25 0))})
        ts (threats/known-threats c)
        st (threats/stances c ts)
        stance-at (fn [x] (set (map #(:stance (st (threats/threat-id %))) (filter #(= x (:x (:pos %))) ts))))]
    (is (= #{:flee} (stance-at 30)) "the four met together: fled")
    (is (= #{:fight} (stance-at -25)) "the lone one met alone: fought")))

(deftest the-planner-answers-a-mob-with-the-reflex-s-body-and-chase-speed
  (let [at (:pos unarmed)
        adult (mob-toll/answer-mob {:mob "zombie"})
        baby (mob-toll/answer-mob {:mob "zombie" :chaser "baby"})]
    (is (= :flee (:stance (cost/answered-together unarmed [adult] at))))
    (is (= :fight (:stance (cost/answered-together unarmed [baby] at))) "a baby zombie keeps up: no flight, so a fight")
    (is (< (:price (cost/answered-together (assoc armed :absorption 8) [adult] at)) (:price (cost/answered-together armed [adult] at)))
        "absorption hearts take blows")))

(deftest a-mob-tolls-its-meet-rate-times-its-meeting-s-price
  (let [a (answered armed "zombie")
        {:keys [shape rate]} (jobs.lib.cost.death/notice-prior "zombie")]
    (is (= (* 0.5 (* (/ shape rate) (:price a))) (cost/threat-factor armed {:name "zombie"} 0.5 a)) "the first term of the met chance, at the weight")
    (is (= (* 4 (jobs.lib.cost.health/hp-price 20)) (cost/threat-factor armed {:name "zombie"} 1 {:price ##Inf})) "at most ::max-rate hp a second")))

(deftest a-sensed-zombie-tolls-its-way-dear-to-flee-cheap-to-fight
  (let [bare (tolls {:entities [(zombie 1 6 0)]})
        armed (tolls {:entities [(zombie 1 6 0)] :inventory [{:name "diamond_sword" :count 1}]})]
    (is (> (get bare [6 64 0]) (get bare [-14 64 0]) 0) "dearest at the zombie, less along its way")
    (is (< (get armed [6 64 0]) (get bare [6 64 0])) "with a diamond sword: a fight, cheap")
    (is (= {} (tolls {:blocks wall :entities [(zombie 1 6 0)]})) "behind a wall the body never saw it through: not known (no x-ray)")))

;; a stay is priced by the blows the mob lands in it (cost/stand-loss), not by a rate whatever its arrival
(deftest a-stay-over-before-the-mob-can-come-costs-nothing
  (let [c (ready-test/ctx-of {:entities [(zombie 1 6 0)]})
        at {:x 3.5 :y 64 :z 0.5}]
    (is (nil? (threats/stay-danger c at 0.1 0)) "a tenth of a second: the zombie some blocks off lands no blow")
    (is (some? (threats/stay-danger c at 60 0)) "a minute: it comes and hits")))

(deftest the-sensed-place-wins-over-a-flight-from-the-same-mob-and-at-most-max-threats
  (let [c (assoc (ready-test/ctx-of {:entities [(assoc (zombie 1 6 0) :uuid "a")]}) :view
                 (fn [] {:now 0 :data {:entries {:threat [{:t 0 :data {:mob "zombie" :id 1 :uuid "a" :key "a" :pos {:x 30 :y 64 :z 0}}}]}}}))]
    (is (= [6] (mapv (comp :x :pos) (threats/known-threats c)))))
  (is (= (threats/max-threats)
         (count (threats/known-threats (ready-test/ctx-of {:entities (for [i (range 12)] (zombie i (* 2 i) 0))}))))))

(deftest a-remembered-flight-keeps-what-the-body-saw-the-mob-hold
  (let [c (assoc (ready-test/ctx-of {:entities [(assoc (zombie 1 6 0) :uuid "a")]}) :view
                 (fn [] {:now 0 :data {:entries {:threat [{:t 0 :data {:mob "skeleton" :held "stone_sword" :chaser "skeleton" :id 1 :uuid "b" :key "b" :pos {:x 30 :y 64 :z 0}}}]}}}))
        fled (first (filter :fled (threats/known-threats c)))]
    (is (= ["skeleton" "stone_sword" "skeleton"] ((juxt :mob :held :chaser) fled)))))

(deftest max-rate-caps-one-danger
  (is (= (* 0.1 (jobs.lib.cost.health/hp-price 20))
         (settings/with-settings {:jobs.lib.cost.danger/max-rate 0.1} (fn [] (toll unarmed "zombie")))))
  (is (= {:max-rate 3} (settings/with-settings {:jobs.lib.cost.danger/max-rate 3} cost/danger-opts))))

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

;; a sighting goes by the toll its weight could cost, not by its age: a newer one whose mob has as good as despawned
;; (weight 1e-9) is dropped, an older one still likely near (1e-4) is kept; the old (pos? weight) test kept both
(deftest a-sighting-is-dropped-by-its-negligible-toll-not-its-age
  (let [p (tu/fake-on-floor {})
        faded #js [#js {:id 1 :name "zombie" :pos #js {:x 5 :y 64 :z 0} :ageMs 3600000}
                   #js {:id 2 :name "zombie" :pos #js {:x -5 :y 64 :z 0} :ageMs 600000}]
        _ (aset p "fadedMobs" (fn [] faded))
        weights {3600000 1e-4 600000 1e-9}]
    (with-redefs [death-spot/seen-clear? (constantly false)
                  death-spot/sighting-share (fn [_ {:keys [age-ms]}] (weights age-ms))]
      (is (= [[1 1e-4]] (mapv (juxt :key :weight) (threats/sightings (ctx-on p (fled-view)))))))))

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

(defn skeleton [id x z] {:id id :uuid (str "s" id) :name "skeleton" :kind "hostile" :pos {:x x :y 64 :z z}})

(deftest two-skeletons-a-block-apart-are-two-threats-and-price-above-one
  (let [one (ready-test/ctx-of {:entities [(skeleton 1 10 0)] :inventory [{:name "diamond_sword" :count 1}]})
        two (ready-test/ctx-of {:entities [(skeleton 1 10 0) (skeleton 2 11 0)] :inventory [{:name "diamond_sword" :count 1}]})
        route {:cells (for [x (range -3 3)] {:x x :y 64 :z 0 :s 1})}
        chance #(cost/death-chance unarmed (assoc route :threats (threats/threats-of %)))
        toll #(get (into {} (map (juxt (juxt :x :y :z) :factor)) (threats/threat-tolls %)) [0 64 0])]
    (is (= 2 (count (threats/known-threats two))))
    (is (> (chance two) (chance one) 0) "the second skeleton adds its own shots")
    (is (> (toll two) (toll one) 0) "and its own toll")))

(deftest tolls-are-built-from-the-weight-rounded-as-the-shared-key-rounds-it
  (let [c (ready-test/ctx-of {:entities [(skeleton 1 10 0)]})
        t (first (threats/known-threats c))
        table #(into {} (map (juxt (juxt :x :y :z) :factor)) (threats/tolls-of c [(assoc t :weight %)]))]
    (is (= (threats/round-weight 0.52) (threats/round-weight 0.5)))
    (is (= (table 0.5) (table 0.52)) "weights in one 1/20 step give one table")
    (is (not= (table 0.5) (table 0.6)))))

(deftest one-ctx-prices-mobs-differing-only-in-what-they-hold-or-in-their-key-each-its-own-way
  (let [pool (into {} (for [x (range 8 11) z (range -1 2)] [(str x ",63," z) "water"]))
        c (ready-test/ctx-of {:blocks pool :inventory [{:name "diamond_sword" :count 1}]})
        p (:primitives c)
        _ (aset p "worldEpoch" (constantly 0))
        tolls-with (fn [entities]
                     (swap! (fake/state p) assoc :entities (mapv fake/entity-in entities))
                     (into {} (map (juxt (juxt :x :y :z) :factor)) (threats/threat-tolls c)))
        drowned {:id 3 :uuid "d" :name "drowned" :kind "hostile" :pos {:x 9.5 :y 63 :z 0.5}}
        at (fn [s] (assoc (skeleton 1 10 0) :uuid s))]
    (is (not= (tolls-with [(assoc drowned :held "trident")]) (tolls-with [(assoc drowned :held "stick")]))
        "a trident is thrown, a stick is nothing")
    (is (not= (tolls-with [(at "a") (assoc (skeleton 2 11 0) :uuid "b")]) (tolls-with [(at "a") (assoc (skeleton 2 11 0) :uuid "a")]))
        "two mobs add their tolls, one mob known twice counts its dearest")))

(deftest a-skeleton-is-priced-by-what-it-holds-a-bow-shoots-where-it-sees-a-sword-strikes-along-its-way
  (let [c (ready-test/ctx-of {:blocks (merge (tu/box 8 64 1 8 66 4 "stone") (tu/box 16 64 0 16 66 0 "stone"))
                              :inventory [{:name "diamond_sword" :count 1}]})
        p (:primitives c)
        _ (aset p "worldEpoch" (constantly 0))
        tolls-with (fn [held]
                     (swap! (fake/state p) assoc :entities [(fake/entity-in (assoc (skeleton 1 10 0) :held held))])
                     (into {} (map (juxt (juxt :x :y :z) :factor)) (threats/threat-tolls c)))
        bow (tolls-with "bow")
        sword (tolls-with "stone_sword")
        behind-wall [7 64 2]
        on-pillar [16 67 0]]
    (is (> (get sword behind-wall) (get bow behind-wall) 0) "behind a wall a bow has no line of fire; a sword walks round and strikes")
    (is (= [true nil] [(pos? (get bow on-pillar)) (get sword on-pillar)]) "a bow shoots up a pillar a sword cannot climb")))

(deftest a-helmet-saves-a-mob-from-the-sun-and-unsensed-equipment-counts-as-worn
  (let [mob #(clj->js (cond-> {} % (assoc :equipment %)))]
    (is (death-spot/bare-headed? (mob [nil nil nil nil nil nil])) "sensed, nothing on the head: it burns")
    (is (not (death-spot/bare-headed? (mob [nil nil nil nil nil {:name "iron_helmet"}]))) "a helmet keeps it from burning")
    (is (not (death-spot/bare-headed? (mob nil))) "equipment never sensed: the worse case, no burn")))

(deftest near-deaths-prices-the-body-once-not-once-a-hurt-entry
  (let [now (atom 0)
        s (mem/open (tu/tmp-dir) {:now #(deref now)})
        builds (atom 0)
        body (cost/death-body (tu/fake-on-floor {}) nil)]
    (doseq [x [0 50 100]]
      (swap! now inc)
      (mem/write! s :hurt {:health 1 :food 20 :pos {:x x :y 64 :z 0}} mem/default-policy))
    (reset! now 1000)
    (with-redefs [cost/death-body (fn [_ _] (swap! builds inc) body)]
      (is (= 3 (count (death-spot/near-deaths (ctx-on (tu/fake-on-floor {}) (mem/view s)))))))
    (is (= 1 @builds))))

;; the fake's world counter stands while only the body changes, as a real one does after a hit or a pickup
(deftest a-shared-toll-table-is-not-served-to-a-body-hurt-or-armed-since
  (let [c (ready-test/ctx-of {:entities [(zombie 1 6 0)]})
        p (:primitives c)
        at-zombie #(:factor (first (filter (fn [r] (= [6 64 0] [(:x r) (:y r) (:z r)])) (threats/threat-tolls c))))
        well (at-zombie)]
    (swap! (fake/state p) assoc-in [:self :health] 6)
    (is (> (at-zombie) well) "hurt: the same mob costs more a second at fewer hp")
    (swap! (fake/state p) assoc-in [:self :health] 20)
    (fake/add-item! p "diamond_sword" 1)
    (is (< (at-zombie) well) "armed: a mob it would fight is cheap")))
