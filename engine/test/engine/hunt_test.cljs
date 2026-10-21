(ns engine.hunt-test
  "jobs.combat.hunt against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [engine.fake :as fake]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.hostile-test :as h]
            [engine.settings :as settings]
            [engine.shelter-support :as st]
            [engine.sky-perception :as sky]
            [engine.test-util :as tu]
            [jobs.combat.hunt :as hunt]
            [jobs.lib.hunting :as hunting]
            [jobs.lib.world-files :as ew]))

(defn spec [args] (list 'jobs.combat.hunt args))

(def cow-drops [{:name "beef" :count 1} {:name "leather" :count 1}])

(defn animal
  ([id name x] (animal id name x {}))
  ([id name x more] (merge {:id id :name name :kind "passive" :pos {:x x :y 64 :z 0}} more)))

(defn cow [id x] (animal id "cow" x {:drops cow-drops}))

(defn ^:async run-ticks
  "Tick n times, the clock moving step ms before each."
  [{:keys [eng clock]} n step]
  (tu/run-ticks eng clock n step))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world 20)]
    (sky/with-sky (:p s))
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn hear-passives-until!
  "Passive mobs are out of sight (entities() drops them) and heard as heardMobs rows until the body stands where
  (seen-from? {x z}) holds, as one below a lip is seen from the edge."
  [p seen-from?]
  (let [orig (.-entities p)
        sees? #(let [pos (.-pos (.self p))] (seen-from? {:x (.-x pos) :z (.-z pos)}))]
    (set! (.-entities p)
          (fn [opts]
            (let [all (.call orig p opts)]
              (if (sees?) all (.filter all #(not= "passive" (.-kind %)))))))
    (set! (.-heardMobs p)
          (fn [opts]
            (let [me (.-pos (.self p))
                  from {:x (.-x me) :y (.-y me) :z (.-z me)}]
              (if (sees?)
                #js []
                (into-array
                 (for [^js e (array-seq (.call orig p #js {:radius 24 :kind "passive"}))
                       :let [pos (.-pos e) at {:x (.-x pos) :y (.-y pos) :z (.-z pos)}]]
                   (clj->js (assoc (obs/rough-hearing from at) :id (.-id e) :name (.-name e) :kind "passive"
                                   :distance (.-distance e)))))))))))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (filterv #(#{:completed :stopped} (:kind %)) @(:seen s))))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn world-ids [{:keys [p]}] (set (map :id (fake/entities p))))
(defn job-mem [{:keys [eng]}] (core/job-memory eng "j1"))

(deftest kills-one-cow-and-collects-its-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 1 :keep 0} {:inventory h/sword :entities [(cow 1 3) (cow 2 5) (cow 3 7)]} 30))]
          (is (= #{2 3} (world-ids s)) "the nearest cow died, two remain")
          (is (= 1 (get (inv s) "beef")))
          (is (= 1 (get (inv s) "leather")))
          (is (= :count (:reason (done-event s))))
          (is (= 1 (:killed (done-event s))))
          (is (finished? s)))))))

(deftest drops-seen-but-none-taken-end-the-pickup-and-the-hunt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (h/setup {:inventory h/sword :entities [(cow 1 3)]} 20)]
          (.override (.-world p) "collect" (fn [_ _ _] #js {:status "unreachable" :reason "no-path" :gained #js []}))
          (core/submit! (:eng s) (spec {:item "beef" :count 1 :keep 0}) {})
          (await (run-ticks s 30 700))
          (is (= 1 (:killed (done-event s))))
          (is (finished? s) "no loop on the pickup"))))))

(deftest collecting-waits-while-the-collect-child-is-held-and-ends-with-any-other-ending
  (doseq [[e ended?] [[:continue false]
                      [{:kind :condition-failed :cause {:floor :health :min 10}} false]
                      [{:kind :done :data {:collected 1}} true]
                      [{:kind :nothing-possible :cause :none-collected} true]
                      [{:kind :cut-off :cause :cut} true]]]
    (is (= ended? (hunting/collect-ended? e)) (pr-str e))))

(deftest keep-stops-the-hunt-before-the-last-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pigs (mapv #(animal % "pig" (+ 2 %) {:drops [{:name "porkchop" :count 1}]}) [1 2 3 4])
              s (await (scenario {:item "porkchop" :count 3 :keep 2} {:inventory h/sword :entities pigs} 40))]
          (is (= 2 (count (world-ids s))) "two pigs are left")
          (is (= 2 (:killed (done-event s))))
          (is (= :keep (:reason (done-event s))))
          (is (= 2 (get (inv s) "porkchop")))
          (is (finished? s)))))))

(deftest at-most-keep-of-the-kind-waits-without-attacking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args entities note]
                [[{:keep 2} [(cow 1 3) (cow 2 4)] "exactly keep"]
                 [{:keep 2} [(cow 1 3)] "fewer than keep"]
                 [{:keep 0} [] "none at all"]
                 [{:keep 1} [(cow 1 3) (animal 2 "sheep" 2) (animal 3 "sheep" 3)] "other kinds do not count"]
                 [{:keep 1 :radius 10} [(cow 1 3) (cow 2 30)] "outside the radius does not count"]]]
          (let [{:keys [eng p] :as s} (h/setup {:inventory h/sword :entities entities})]
            (core/submit! eng (spec args) {})
            (await (run-ticks s 40 700))
            (is (zero? (count (h/calls p "attack"))) note)
            (is (zero? (count (h/calls p "equip"))) note)
            (is (= [:too-few] (distinct (map :reason (events-of s :waiting)))) note)
            (is (not (finished? s)) "the order waits for animals")))))))

(deftest a-cow-hunt-never-attacks-a-sheep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 2 :keep 0} {:inventory h/sword :entities [(animal 9 "sheep" 1) (cow 1 4) (cow 2 5)]} 40))]
          (is (not-any? #{9} (attacked s)))
          (is (= #{9} (world-ids s)))
          (is (= :count (:reason (done-event s)))))))))

(deftest only-the-kinds-drops-are-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities [(cow 1 3)]} 20)]
          (fake/add-entity! (:p s) {:id 50 :kind "item" :name "item" :item {:name "dirt" :count 1}
                                    :pos [3 64 0]})
          (core/submit! (:eng s) (spec {:item "beef" :keep 0}) {})
          (await (run-ticks s 1 700))
          (is (= 1 (get (inv s) "beef")))
          (is (nil? (get (inv s) "dirt")) "dirt stays on the ground")
          (is (contains? (world-ids s) 50))
          (is (finished? s)))))))

(deftest an-invulnerable-cow-is-skipped-and-the-hunt-ends-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :keep 0} {:inventory h/sword :entities [(animal 1 "cow" 3 {:invulnerable true})]} 30))]
          (is (<= (count (attacked s)) 20) "bounded: attack gives up after sixteen no-damage hits")
          (is (= :none (:reason (done-event s))))
          (is (= 0 (:killed (done-event s))))
          (is (finished? s)))))))

(deftest three-unreachable-cows-give-the-hunt-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 3 :keep 0}
                                 {:inventory h/sword :entities [(cow 1 10) (cow 2 11) (cow 3 12)]
                                  :unreachable ["10,64,0" "11,64,0" "12,64,0"]} 200))]
          (is (= :gave-up (:reason (done-event s))))
          (is (seq (:skipped (done-event s))) "the ending names the skipped animals")
          (is (= 0 (:killed (done-event s))))
          (is (finished? s)))))))

(deftest a-hunt-resumes-on-its-target-even-when-a-nearer-animal-appears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities [(cow 1 4)]} 20)
              added (atom false)]
          (.override (.-world (:p s)) "attack" (fn [t a impl]
                                                 (when-not @added
                                                   (reset! added true)
                                                   (fake/add-entity! (:p s) {:id 2 :kind "passive" :name "cow" :health 20 :pos [1 64 0]}))
                                                 (impl t a)))
          (core/submit! (:eng s) (spec {:item "beef" :keep 0 :count 1}) {})
          (await (run-ticks s 1 700))
          (is (= #{1} (set (attacked s))) "only the original cow is hit")
          (is (not (contains? (world-ids s) 1)))
          (is (contains? (world-ids s) 2)))))))

(deftest the-result-is-handed-to-a-parent
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock]} (h/setup {:inventory h/sword :entities [(cow 1 3) (cow 2 5)]} 20)
              out (atom nil)
              parent (tu/recording-parent 'jobs.combat.hunt {:item "beef" :count 2 :keep 0} out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'hunting-parent parent))]
          (core/submit! eng '(hunting-parent) {})
          (await (tu/run-ticks eng clock 60 700))
          (is (= {:killed 2 :gained 2 :reason :count :spared 0 :remaining 0 :status :done} (dissoc @out :text))))))))

(defn baby-cow [id x] (animal id "cow" x {:drops cow-drops :baby true}))

(deftest a-baby-is-never-a-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 1 :keep 0} {:inventory h/sword :entities [(baby-cow 1 2) (cow 2 5)]} 30))]
          (is (= [2] (distinct (attacked s))) "only the adult is attacked")
          (is (contains? (world-ids s) 1) "the baby lives")
          (is (not (contains? (world-ids s) 2))))))))

(deftest keep-counts-adults-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory h/sword
                                              :entities [(cow 1 3) (cow 2 4) (baby-cow 3 2) (baby-cow 4 2) (baby-cow 5 2)]})]
          (core/submit! eng (spec {:item "beef" :keep 2}) {})
          (await (run-ticks s 40 700))
          (is (= [:too-few] (distinct (map :reason (events-of s :waiting)))) "two adults and three babies: nothing to take")
          (is (zero? (count (h/calls p "attack")))))))))

(deftest keep-ends-the-hunt-on-adults-with-babies-around
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 5 :keep 2}
                                 {:inventory h/sword
                                  :entities [(cow 1 3) (cow 2 4) (cow 3 5) (baby-cow 4 2) (baby-cow 5 2)]} 40))]
          (is (= :keep (:reason (done-event s))))
          (is (= 1 (:killed (done-event s))))
          (is (= #{2 3 4 5} (world-ids s)) "one adult died, two adults and both babies remain"))))))

;; ------------------------------------------------------------------ the pair rule (default :keep)

(deftest default-keep-spares-two-adult-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 10} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) [1 2 3 4 5])} 60))
              d (done-event s)]
          (is (= 2 (count (world-ids s))) "two cows are left")
          (is (= 3 (:killed d)))
          (is (= :keep (:reason d)))
          (is (= 7 (:spared d)))
          (is (= 2 (:remaining d)))
          (is (finished? s)))))))

(deftest default-keep-ends-the-job-with-two-adults-and-babies
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (h/setup {:inventory h/sword
                                        :entities [(cow 1 3) (cow 2 4) (baby-cow 3 2) (baby-cow 4 2) (baby-cow 5 2)]})]
          (core/submit! eng (spec {:item "beef"}) {})
          (await (core/tick! eng))
          (is (zero? (count (h/calls p "attack"))))
          (is (= 5 (count (fake/entities p)))))))))

(deftest default-keep-never-takes-a-baby
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 1} {:inventory h/sword
                                             :entities [(cow 1 3) (cow 2 4) (cow 3 5) (cow 4 6) (baby-cow 5 2) (baby-cow 6 2)]} 40))]
          (is (= 1 (:killed (done-event s))))
          (is (= :count (:reason (done-event s))))
          (is (= 0 (:spared (done-event s))))
          (is (every? (world-ids s) [5 6]) "both babies live"))))))

(deftest explicit-zero-keep-switches-the-pair-rule-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 10 :keep 0} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) [1 2 3 4 5])} 80))
              d (done-event s)]
          (is (empty? (world-ids s)))
          (is (= 5 (:killed d)))
          (is (= 0 (:spared d)))
          (is (= 0 (:remaining d))))))))

(defn zombie [id x] (animal id "zombie" x {:kind "hostile" :drops [{:name "rotten_flesh" :count 1}]}))

(deftest default-keep-is-zero-for-a-hostile-kind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "rotten_flesh" :count 3} {:inventory h/sword :entities (mapv #(zombie % (+ 2 %)) [1 2 3])} 80))]
          (is (empty? (world-ids s)) "all three zombies died")
          (is (= 3 (:killed (done-event s))))
          (is (= :count (:reason (done-event s)))))))))

(deftest an-unarmed-hunt-of-a-hostile-kind-fetches-a-weapon-and-ends-with-the-failure-or-unarmed-when-fetch-is-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args kind reason] [[{:item "rotten_flesh"} :condition-failed :tool]
                                    [{:item "rotten_flesh" :ways #{:carried}} :condition-failed :unarmed]]]
          (let [s (h/setup {:inventory [] :entities [(zombie 1 3)]} 20)
                e (await (ending-of s 'jobs.combat.hunt args 60))]
            (is (= [kind reason] [(:kind e) (get-in e [:cause :reason] (:cause e))]) (pr-str args))
            (is (empty? (h/calls (:p s) "attack")) "no fist fight")))))))

(deftest the-check-passes-for-a-single-hostile-mob
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (h/setup {:inventory h/sword :entities [(zombie 1 3)]} 20)]
          (core/submit! eng (spec {:item "rotten_flesh"}) {})
          (await (core/tick! eng))
          (is (pos? (count (h/calls p "attack")))))))))

(deftest a-chicken-hunt-collects-meat-and-feather
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [chicken (animal 1 "chicken" 3 {:drops [{:name "feather" :count 1} {:name "chicken" :count 1}]})
              s (await (scenario {:item "chicken" :count 1 :keep 0} {:inventory h/sword :entities [chicken]} 30))]
          (is (= 1 (get (inv s) "chicken")))
          (is (= 1 (get (inv s) "feather")))
          (is (finished? s)))))))

;; ------------------------------------------------------- one call hunts and collects (card 96dccacc)

(deftest one-call-kills-two-cows-and-collects-both
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (h/first-round-ms (spec {:item "beef" :count 2 :keep 0}) {:inventory h/sword :entities [(cow 1 3) (cow 2 5) (cow 3 7)]} 20))]
          (is (= #{3} (world-ids s)))
          (is (= 2 (get (inv s) "beef")))
          (is (= 2 (get (inv s) "leather")))
          (is (= :count (:reason (done-event s))))
          (is (finished? s) "one call"))))))

(deftest too-few-animals-fail-the-condition-saying-how-many-are-in-range-and-how-far-the-nearest-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args entities want note]
                [[{:item "porkchop" :keep 0 :radius 24} [(animal 1 "pig" 36)]
                  {:keep 0 :in-range 0 :radius 24 :nearest 36} "a pig seen past the radius"]
                 [{:item "porkchop" :keep 1 :radius 24} [(animal 1 "pig" 5) (animal 2 "pig" 40)]
                  {:keep 1 :in-range 1 :radius 24 :nearest 5} "one in range, keep 1"]
                 [{:item "porkchop" :keep 0 :radius 24} []
                  {:keep 0 :in-range 0 :radius 24} "none seen at all"]
                 [{:item "bone" :radius 24} [(animal 1 "skeleton" 32 {:kind "hostile"})]
                  {:keep 0 :in-range 0 :radius 24 :nearest 32} "a hostile kind keeps none, with none in range to say it is hostile"]]]
          (let [{:keys [eng]} (h/setup {:inventory h/sword :entities entities})
                cause (:cause (:ending (await (tu/child-outcome eng 'jobs.combat.hunt args 60))))]
            (is (= (assoc want :reason :too-few :mob (first (hunting/mobs {:args args})) :killed 0)
                   (select-keys cause [:reason :mob :killed :keep :in-range :radius :nearest])) note)
            (is (= (pos? (:keep want)) (str/includes? (:text cause) "keeping")) note)))))))

;; ------------------------------------------------------------------ heard, not seen

(defn north-cow [id z] (animal id "cow" 0 {:pos {:x 0 :y 64 :z z} :drops cow-drops}))

(deftest cows-heard-but-not-seen-are-stepped-toward-then-hunted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities [(north-cow 1 -8) (north-cow 2 -9)]} 20)
              _ (hear-passives-until! (:p s) #(<= (:z %) -2))
              _ (core/submit! (:eng s) (spec {:item "beef" :count 1 :keep 0 :radius 24}) {})
              _ (await (run-ticks s 60 700))]
          (is (= :count (:reason (done-event s))) "walked north toward the sound, saw the cows, killed one")
          (is (= 1 (:killed (done-event s)))))))))

(deftest heard-cows-that-stay-out-of-sight-end-the-hunt-after-the-look-outs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities [(north-cow 1 -8)]} 20)
              _ (hear-passives-until! (:p s) (constantly false))
              _ (core/submit! (:eng s) (spec {:item "beef" :count 1 :keep 0 :radius 24}) {})
              _ (await (run-ticks s 80 700))]
          (is (finished? s))
          (is (<= 1 (count (h/calls (:p s) "steer")) 4) "a few steps toward the sound, no more")
          (is (empty? (attacked s))))))))

(deftest nothing-heard-and-nothing-seen-waits-too-few-without-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities []} 20)
              _ (hear-passives-until! (:p s) (constantly false))
              _ (core/submit! (:eng s) (spec {:item "beef" :count 1 :keep 0 :radius 24}) {})
              _ (await (run-ticks s 60 700))]
          (is (zero? (count (h/calls (:p s) "steer"))) "no sound, no steps")
          (is (= [:too-few] (distinct (map :reason (events-of s :waiting))))))))))

(deftest a-cow-behind-the-body-is-hunted-once-the-body-has-looked-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup {:inventory h/sword :entities [(cow 1 8)]} 20)
              p (:p s)
              orig (.-entities p)]
          (set! (.-entities p)
                (fn [opts] (let [all (.call orig p opts)]
                             (if (seq (h/calls p "look")) all (.filter all #(not= "passive" (.-kind %)))))))
          (core/submit! (:eng s) (spec {:item "beef" :count 1 :keep 0 :radius 24}) {})
          (await (run-ticks s 20 700))
          (is (seq (h/calls p "look")) "the body turned before it called the hunt too-few")
          (is (= [1] (distinct (attacked s)))))))))

;; ------------------------------------------------------------------ colour

(defn sheep [id x color] (animal id "sheep" x {:color color :drops [{:name (str color "_wool") :count 1}]}))

(deftest a-colour-hunt-attacks-only-sheep-of-that-colour
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "white_wool" :count 1 :keep 0} {:inventory h/sword :entities [(sheep 1 3 "brown") (sheep 2 6 "white")]} 30))]
          (is (= #{2} (set (attacked s))) "the nearer brown sheep is left alone")
          (is (= #{1} (world-ids s)))
          (is (= 1 (get (inv s) "white_wool")))
          (is (= :count (:reason (done-event s)))))))))

(deftest a-colour-hunt-with-no-such-sheep-waits-naming-the-colours-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "white_wool" :count 1 :keep 0} {:inventory h/sword :entities [(sheep 1 3 "brown") (sheep 2 6 "gray")]} 5))
              e (last (events-of s :waiting))
              cause (or (:cause e) e)]
          (is (empty? (attacked s)))
          (is (= :no-color (:reason cause)))
          (is (= ["brown" "gray"] (:seen-colors cause)))
          (is (re-find #"no white sheep" (:text cause)))
          (is (not (finished? s))))))))

(deftest a-colour-hunt-counts-only-its-colour-against-keep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args entities reason note]
                [[{:item "white_wool" :count 1 :keep 1} [(sheep 1 3 "white") (sheep 2 5 "brown") (sheep 3 6 "brown")] :too-few "one white, keep 1: too few, however many of other colours"]
                 [{:item "white_wool" :count 1 :keep 2} [(sheep 1 3 "brown")] :no-color "none of the colour wins over too-few"]]]
          (let [{:keys [eng]} (h/setup {:inventory h/sword :entities entities})
                r (await (tu/child-outcome eng 'jobs.combat.hunt args 60))]
            (is (= reason (:reason r)) note)))))))

(deftest a-colour-hunt-skips-sheared-sheep-and-says-when-only-sheared-ones-are-in-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shorn #(assoc (sheep %1 %2 "white") :sheared true)
              s (await (scenario {:item "white_wool" :count 1 :keep 0} {:inventory h/sword :entities [(shorn 1 3) (sheep 2 6 "white")]} 30))
              s2 (await (scenario {:item "white_wool" :count 1 :keep 0} {:inventory h/sword :entities [(shorn 1 3) (shorn 2 6)]} 5))
              e (last (events-of s2 :waiting))
              cause (or (:cause e) e)]
          (is (= #{2} (set (attacked s))) "the nearer sheared sheep is left alone")
          (is (empty? (attacked s2)))
          (is (= :no-color (:reason cause)))
          (is (re-find #"sheared" (:text cause))))))))

(deftest a-badly-hurt-body-hunts-a-passive-animal-but-waits-to-heal-before-a-hostile-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cow-hunt (await (scenario {:item "beef" :count 1 :keep 0} {:inventory h/sword :self {:health 0.5} :entities [(cow 1 3)]} 20))
              zombie-hunt (await (scenario {:item "rotten_flesh" :count 1} {:inventory h/sword :self {:health 0.5} :entities [(zombie 1 3)]} 10))]
          (is (= #{1} (set (attacked cow-hunt))) "the cow is the way to the food that heals")
          (is (empty? (attacked zombie-hunt)) "nothing hostile attacked at 0.5 hp")
          (is (= [:low-health] (distinct (map :reason (events-of zombie-hunt :waiting))))))))))

(deftest a-hunt-at-night-in-the-open-waits-for-dawn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 1 :keep 0} {:inventory h/sword :time st/night :entities [(cow 1 3)]} 10))]
          (is (empty? (attacked s)) "nothing attacked at night")
          (is (= [:dark] (distinct (map :reason (events-of s :waiting))))))))))

(deftest the-calls-checks-reach-the-attack-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "beef" :count 1 :keep 0 :checks {:safe? '(always)}} {:inventory h/sword :time st/night :entities [(cow 1 3)]} 10))]
          (is (seq (attacked s)) "the caller's :checks replace attack's own dark-sky check below the hunt")
          (is (empty? (filter #(= :dark (:reason %)) (events-of s :waiting)))))))))

(deftest a-hunt-under-way-waits-when-hurt-and-is-not-ended-by-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:inventory h/sword :entities [(zombie 1 3) (zombie 2 5)]} 20)]
          (.override (.-world p) "attack" (fn [token a impl] (-> (impl token a) (.then (fn [r] (fake/swap-self! p assoc :health 0.5) r)))))
          (core/submit! eng (spec {:item "rotten_flesh" :count 2}) {})
          (await (run-ticks s 15 700))
          (is (= #{1} (set (attacked s))) "the first zombie was attacked, the second waits for the body to heal")
          (is (not (finished? s)) "low health does not end the hunt")
          (is (= :low-health (:reason (last (events-of s :waiting))))))))))

;; ------------------------------------------------------------------ animals the body spares

(def own-pen [{:name "linden-west-pen" :owner "Fake" :min [0 60 -3] :max [20 70 3]}])

(def pen-ring
  (merge (tu/box 3 64 -2 12 64 -2 "oak_fence") (tu/box 3 64 2 12 64 2 "oak_fence")
         (tu/box 3 64 -1 3 64 1 "oak_fence") (tu/box 12 64 -1 12 64 1 "oak_fence")))

(defn four-sheep [] (mapv #(animal % "sheep" (+ 4 %) {:color "white" :drops [{:name "white_wool" :count 1}]}) (range 1 5)))

(defn ^:async spared-hunt [args world zones]
  (let [s (h/setup world 20 (ew/of-data {} {} zones))]
    (core/submit! (:eng s) (spec (merge {:item "white_wool" :count 2 :keep 0} args)) {})
    (await (run-ticks s 30 700))
    s))

(deftest a-hunt-leaves-animals-in-the-bodys-own-zone-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (spared-hunt {} {:inventory h/sword :entities (four-sheep)} own-pen))
              e (done-event s)]
          (is (empty? (attacked s)))
          (is (= [:spared 0 {:pen 4}] [(:reason e) (:killed e) (:spared-by e)]))
          (is (re-find #"pen" (:text e))))))))

(deftest a-hunt-leaves-animals-in-a-closed-fence-that-is-no-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (spared-hunt {} {:inventory h/sword :entities (four-sheep) :blocks pen-ring} nil))]
          (is (empty? (attacked s)))
          (is (= {:pen 4} (:spared-by (done-event s)))))))))

(def stone-pit
  (merge (tu/box 3 64 -2 12 65 -2 "stone") (tu/box 3 64 2 12 65 2 "stone")
         (tu/box 3 64 -1 3 65 1 "stone") (tu/box 12 64 -1 12 65 1 "stone")))

(def planks-pen
  (merge (tu/box 3 64 -2 12 65 -2 "oak_planks") (tu/box 3 64 2 12 65 2 "oak_planks")
         (tu/box 3 64 -1 3 65 1 "oak_planks") (tu/box 12 64 -1 12 65 1 "oak_planks")))

(deftest a-hunt-spares-animals-in-a-pen-walled-with-full-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (spared-hunt {} {:inventory h/sword :self {:pos {:x 10.5 :y 64 :z 0.5}} :entities (four-sheep) :blocks planks-pen} nil))]
          (is (empty? (attacked s)))
          (is (= {:pen 4} (:spared-by (done-event s)))))))))

(deftest a-hunt-takes-animals-in-a-hollow-closed-by-plain-terrain
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (spared-hunt {} {:inventory h/sword :self {:pos {:x 10.5 :y 64 :z 0.5}} :entities (four-sheep) :blocks stone-pit} nil))]
          (is (seq (attacked s)))
          (is (nil? (:pen (:spared-by (done-event s))))))))))

(deftest a-hunt-leaves-off-limits-kinds-alone
  (async done
    (tu/run-async done
      (fn []
        (settings/with-settings {:jobs.lib.animal-spare/off-limits #{"sheep"}}
          (fn ^:async t []
            (let [s (await (spared-hunt {} {:inventory h/sword :entities (four-sheep)} nil))]
              (is (empty? (attacked s)))
              (is (= {:off-limits 4} (:spared-by (done-event s)))))))))))

(deftest the-spare-pens-setting-off-lifts-the-pen-rule
  (async done
    (tu/run-async done
      (fn []
        (settings/with-settings {:jobs.lib.animal-spare/spare-pens false}
          (fn ^:async t []
            (let [s (await (spared-hunt {} {:inventory h/sword :entities (four-sheep)} own-pen))]
              (is (seq (attacked s))))))))))

(deftest every-kill-of-a-passive-mob-is-an-event-naming-the-rule-that-allowed-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pigs (mapv #(animal % "pig" (+ 2 %) {:drops [{:name "porkchop" :count 1}]}) [1 2 3 4])
              s (await (scenario {:item "porkchop" :count 2 :keep 2} {:inventory h/sword :entities pigs} 40))
              es (events-of s :animal.killed)]
          (is (= [["pig" "jobs.combat.hunt" :surplus] ["pig" "jobs.combat.hunt" :surplus]]
                 (mapv (juxt :animal :by :rule) es)))
          (is (= #{1 2} (set (map :entity es))))
          (is (every? #(= 64 (second (:cell %))) es)))))))

(deftest a-hostile-kill-is-no-passive-kill-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "rotten_flesh" :count 1} {:inventory h/sword :entities [(animal 1 "zombie" 3 {:kind "hostile"})]} 20))]
          (is (empty? (events-of s :animal.killed))))))))

(deftest fish-items-have-a-mob-that-drops-them
  (is (= ["salmon"] (hunting/mobs {:args {:item "salmon"}})))
  (is (= ["cod"] (hunting/mobs {:args {:item "cod"}})))
  (is (= ["cod"] (hunting/mobs {:args {:item "bone_meal"}}))))
