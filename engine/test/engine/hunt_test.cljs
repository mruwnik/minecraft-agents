(ns engine.hunt-test
  "jobs.combat.hunt against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.combat.hunt :as hunt]
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
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world 20)]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n 700))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :hunt.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn world-ids [{:keys [p]}] (set (map :id (fake/entities p))))
(defn job-mem [{:keys [eng]}] (core/job-memory eng "j1"))

;; ------------------------------------------------------------------ several kinds in one call

(defn pig [id x] (animal id "pig" x {:drops [{:name "porkchop" :count 1}]}))

(deftest a-mob-list-hunts-each-kind-with-count-and-keep-per-kind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob ["cow" "pig"] :count 1 :keep 1}
                                 {:inventory h/sword :entities [(cow 1 3) (cow 2 5) (pig 3 4) (pig 4 6)]} 60))
              e (first (events-of s :hunt.batch-done))]
          (is (= #{2 4} (world-ids s)) "the nearest cow and the nearest pig died")
          (is (= 1 (get (inv s) "beef")))
          (is (= 1 (get (inv s) "porkchop")))
          (is (= 2 (:killed e)))
          (is (finished? s)))))))

(deftest food-animals-means-cows-pigs-sheep-and-chickens
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob :food-animals :count 1 :keep 0}
                                 {:inventory h/sword :entities [(cow 1 3) (pig 2 4) (animal 3 "horse" 5)]} 60))]
          (is (= #{3} (world-ids s)) "the cow and the pig died, the horse is no food animal")
          (is (finished? s)))))))

(deftest a-mob-list-with-no-animal-of-any-kind-ends-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob ["cow" "pig"] :keep 0} {:inventory h/sword :entities []} 20))
              e (first (events-of s :hunt.batch-done))]
          (is (= 0 (:killed e)))
          (is (= :stopped (:status e)))
          (is (finished? s)))))))

;; ------------------------------------------------------------------ the job

(deftest kills-one-cow-and-collects-its-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 1 :keep 0} {:inventory h/sword :entities [(cow 1 3) (cow 2 5) (cow 3 7)]} 30))]
          (is (= #{2 3} (world-ids s)) "the nearest cow died, two remain")
          (is (= 1 (get (inv s) "beef")))
          (is (= 1 (get (inv s) "leather")))
          (is (= :count (:reason (done-event s))))
          (is (= 1 (:killed (done-event s))))
          (is (finished? s)))))))

(deftest keep-stops-the-hunt-before-the-last-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pigs (mapv #(animal % "pig" (+ 2 %) {:drops [{:name "porkchop" :count 1}]}) [1 2 3 4])
              s (await (scenario {:mob "pig" :count 3 :keep 2} {:inventory h/sword :entities pigs} 40))]
          (is (= 2 (count (world-ids s))) "two pigs are left")
          (is (= 2 (:killed (done-event s))))
          (is (= :keep (:reason (done-event s))))
          (is (= 2 (get (inv s) "porkchop")))
          (is (finished? s)))))))

(deftest at-most-keep-of-the-kind-ends-the-job-without-attacking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args entities note]
                [[{:keep 2} [(cow 1 3) (cow 2 4)] "exactly keep"]
                 [{:keep 2} [(cow 1 3)] "fewer than keep"]
                 [{:keep 0} [] "none at all"]
                 [{:keep 1} [(cow 1 3) (animal 2 "sheep" 2) (animal 3 "sheep" 3)] "other kinds do not count"]
                 [{:keep 1 :radius 10} [(cow 1 3) (cow 2 30)] "outside the radius does not count"]]]
          (let [{:keys [eng p]} (h/setup {:inventory h/sword :entities entities})]
            (core/submit! eng (spec args) {})
            (await (core/tick! eng))
            (is (zero? (count (h/calls p "attack"))) note)
            (is (zero? (count (h/calls p "equip"))) note)
            (is (empty? (:list (core/state eng))) note)))))))

(deftest a-cow-hunt-never-attacks-a-sheep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 2 :keep 0} {:inventory h/sword :entities [(animal 9 "sheep" 1) (cow 1 4) (cow 2 5)]} 40))]
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
          (core/submit! (:eng s) (spec {:keep 0}) {})
          (await (run-ticks s 1 700))
          (is (= 1 (get (inv s) "beef")))
          (is (nil? (get (inv s) "dirt")) "dirt stays on the ground")
          (is (contains? (world-ids s) 50))
          (is (finished? s)))))))

(deftest an-unlisted-kind-collects-every-item-nearby
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "llama" :keep 0} {:inventory h/sword
                                                         :entities [(animal 1 "llama" 3 {:drops [{:name "dirt" :count 1}]})]} 30))]
          (is (= 1 (get (inv s) "dirt")))
          (is (= :count (:reason (done-event s)))))))))

(deftest an-invulnerable-cow-is-skipped-and-the-hunt-ends-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:keep 0} {:inventory h/sword :entities [(animal 1 "cow" 3 {:invulnerable true})]} 30))]
          (is (<= (count (attacked s)) 8) "bounded: attack gives up after four no-damage hits")
          (is (= :none (:reason (done-event s))))
          (is (= 0 (:killed (done-event s))))
          (is (finished? s)))))))

(deftest three-unreachable-cows-give-the-hunt-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 3 :keep 0}
                                 {:inventory h/sword :entities [(cow 1 10) (cow 2 11) (cow 3 12)]
                                  :unreachable ["10,64,0" "11,64,0" "12,64,0"]} 200))
              gave-up (events-of s :hunt.gave-up)]
          (is (= 1 (count gave-up)))
          (is (= :warn (:level (first gave-up))))
          (is (= :gave-up (:reason (done-event s))))
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
          (core/submit! (:eng s) (spec {:keep 0 :count 1}) {})
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
              parent {:check (constantly true)
                      :round (fn ^:async hunting-parent [c]
                               (let [r (await (ctx/call-child c :kid 'jobs.combat.hunt {:count 2 :keep 0}))]
                                 (when (= :done r) (reset! out (ctx/child-result c :kid)))
                                 r))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'hunting-parent parent))]
          (core/submit! eng '(hunting-parent) {})
          (dotimes [_ 60]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (= {:killed 2 :reason :count :spared 0 :remaining 0} @out)))))))

(defn baby-cow [id x] (animal id "cow" x {:drops cow-drops :baby true}))

(deftest a-baby-is-never-a-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 1 :keep 0} {:inventory h/sword :entities [(baby-cow 1 2) (cow 2 5)]} 30))]
          (is (= [2] (distinct (attacked s))) "only the adult is attacked")
          (is (contains? (world-ids s) 1) "the baby lives")
          (is (not (contains? (world-ids s) 2))))))))

(deftest keep-counts-adults-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (h/setup {:inventory h/sword
                                        :entities [(cow 1 3) (cow 2 4) (baby-cow 3 2) (baby-cow 4 2) (baby-cow 5 2)]})]
          (core/submit! eng (spec {:keep 2}) {})
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))) "two adults and three babies: nothing to take")
          (is (zero? (count (h/calls p "attack")))))))))

(deftest keep-ends-the-hunt-on-adults-with-babies-around
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 5 :keep 2}
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
        (let [s (await (scenario {:count 10} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) [1 2 3 4 5])} 60))
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
          (core/submit! eng (spec {}) {})
          (await (core/tick! eng))
          (is (zero? (count (h/calls p "attack"))))
          (is (= 5 (count (fake/entities p)))))))))

(deftest default-keep-never-takes-a-baby
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 1} {:inventory h/sword
                                             :entities [(cow 1 3) (cow 2 4) (cow 3 5) (cow 4 6) (baby-cow 5 2) (baby-cow 6 2)]} 40))]
          (is (= 1 (:killed (done-event s))))
          (is (= :count (:reason (done-event s))))
          (is (= 0 (:spared (done-event s))))
          (is (every? (world-ids s) [5 6]) "both babies live"))))))

(deftest explicit-zero-keep-switches-the-pair-rule-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 10 :keep 0} {:inventory h/sword :entities (mapv #(cow % (+ 2 %)) [1 2 3 4 5])} 80))
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
        (let [s (await (scenario {:mob "zombie" :count 3} {:inventory h/sword :entities (mapv #(zombie % (+ 2 %)) [1 2 3])} 80))]
          (is (empty? (world-ids s)) "all three zombies died")
          (is (= 3 (:killed (done-event s))))
          (is (= :count (:reason (done-event s)))))))))

(deftest the-check-passes-for-a-single-hostile-mob
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (h/setup {:inventory h/sword :entities [(zombie 1 3)]} 20)]
          (core/submit! eng (spec {:mob "zombie"}) {})
          (await (core/tick! eng))
          (is (pos? (count (h/calls p "attack")))))))))

(deftest a-chicken-hunt-collects-meat-and-feather
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [chicken (animal 1 "chicken" 3 {:drops [{:name "feather" :count 1} {:name "chicken" :count 1}]})
              s (await (scenario {:mob "chicken" :count 1 :keep 0} {:inventory h/sword :entities [chicken]} 30))]
          (is (= 1 (get (inv s) "chicken")))
          (is (= 1 (get (inv s) "feather")))
          (is (finished? s)))))))

;; ------------------------------------------------------- one call hunts and collects (card 96dccacc)

(deftest one-call-kills-two-cows-and-collects-both
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (h/first-round-ms (spec {:count 2 :keep 0}) {:inventory h/sword :entities [(cow 1 3) (cow 2 5) (cow 3 7)]} 20))]
          (is (= #{3} (world-ids s)))
          (is (= 2 (get (inv s) "beef")))
          (is (= 2 (get (inv s) "leather")))
          (is (= :count (:reason (done-event s))))
          (is (finished? s) "one call"))))))

(deftest too-few-animals-end-the-job-stopped-saying-how-many-are-in-range-and-how-far-the-nearest-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args entities want note]
                [[{:mob "pig" :keep 0 :radius 24} [(animal 1 "pig" 36)]
                  {:keep 0 :in-range 0 :radius 24 :nearest 36} "a pig seen past the radius"]
                 [{:mob "pig" :keep 1 :radius 24} [(animal 1 "pig" 5) (animal 2 "pig" 40)]
                  {:keep 1 :in-range 1 :radius 24 :nearest 5} "one in range, keep 1"]
                 [{:mob "pig" :keep 0 :radius 24} []
                  {:keep 0 :in-range 0 :radius 24} "none seen at all"]]]
          (let [{:keys [eng]} (h/setup {:inventory h/sword :entities entities})
                r (await (tu/child-outcome eng 'jobs.combat.hunt args 20))]
            (is (= (assoc want :status :stopped :reason :too-few :mob "pig" :killed 0)
                   (select-keys r [:status :reason :mob :killed :keep :in-range :radius :nearest])) note)
            (is (string? (:text r)) note)))))))

;; ------------------------------------------------------------------ colour

(defn sheep [id x color] (animal id "sheep" x {:color color :drops [{:name (str color "_wool") :count 1}]}))

(deftest a-colour-hunt-attacks-only-sheep-of-that-colour
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "sheep" :color "white" :count 1 :keep 0} {:inventory h/sword :entities [(sheep 1 3 "brown") (sheep 2 6 "white")]} 30))]
          (is (= #{2} (set (attacked s))) "the nearer brown sheep is left alone")
          (is (= #{1} (world-ids s)))
          (is (= 1 (get (inv s) "white_wool")))
          (is (= :count (:reason (done-event s)))))))))

(deftest a-colour-hunt-with-no-such-sheep-ends-at-once-naming-the-colours-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "sheep" :color "white" :count 1 :keep 0} {:inventory h/sword :entities [(sheep 1 3 "brown") (sheep 2 6 "gray")]} 5))
              e (done-event s)]
          (is (empty? (attacked s)))
          (is (= :no-color (:reason e)))
          (is (= ["brown" "gray"] (:seen-colors e)))
          (is (re-find #"no white sheep" (:text e)))
          (is (finished? s)))))))

(deftest a-colour-hunt-counts-only-its-colour-against-keep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args entities reason note]
                [[{:mob "sheep" :color "white" :count 1 :keep 1} [(sheep 1 3 "white") (sheep 2 5 "brown") (sheep 3 6 "brown")] :too-few "one white, keep 1: too few, however many of other colours"]
                 [{:mob "sheep" :color "white" :count 1 :keep 2} [(sheep 1 3 "brown")] :no-color "none of the colour wins over too-few"]]]
          (let [{:keys [eng]} (h/setup {:inventory h/sword :entities entities})
                r (await (tu/child-outcome eng 'jobs.combat.hunt args 20))]
            (is (= reason (:reason r)) note)))))))

(deftest a-colour-hunt-skips-sheared-sheep-and-says-when-only-sheared-ones-are-in-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shorn #(assoc (sheep %1 %2 "white") :sheared true)
              s (await (scenario {:mob "sheep" :color "white" :count 1 :keep 0} {:inventory h/sword :entities [(shorn 1 3) (sheep 2 6 "white")]} 30))
              s2 (await (scenario {:mob "sheep" :color "white" :count 1 :keep 0} {:inventory h/sword :entities [(shorn 1 3) (shorn 2 6)]} 5))
              e (done-event s2)]
          (is (= #{2} (set (attacked s))) "the nearer sheared sheep is left alone")
          (is (empty? (attacked s2)))
          (is (= :no-color (:reason e)))
          (is (re-find #"sheared" (:text e))))))))

(deftest a-badly-hurt-body-waits-to-heal-before-it-hunts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:count 1 :keep 0} {:inventory h/sword :self {:health 0.5} :entities [(cow 1 3)]} 10))]
          (is (empty? (attacked s)) "nothing attacked at 0.5 hp")
          (is (= [:low-health] (distinct (map :reason (events-of s :waiting))))))))))

;; ------------------------------------------------------------------ animals the body spares

(def own-pen [{:name "linden-west-pen" :owner "Fake" :min [0 60 -3] :max [20 70 3]}])

(def pen-ring
  (merge (tu/box 3 64 -2 12 64 -2 "oak_fence") (tu/box 3 64 2 12 64 2 "oak_fence")
         (tu/box 3 64 -1 3 64 1 "oak_fence") (tu/box 12 64 -1 12 64 1 "oak_fence")))

(defn four-sheep [] (mapv #(animal % "sheep" (+ 4 %) {:color "white" :drops [{:name "white_wool" :count 1}]}) (range 1 5)))

(defn ^:async spared-hunt [args world zones]
  (let [s (h/setup world 20 (ew/of-data {} {} zones))]
    (core/submit! (:eng s) (spec (merge {:mob "sheep" :count 2 :keep 0} args)) {})
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

(deftest a-hunt-leaves-off-limits-kinds-alone
  (async done
    (tu/run-async done
      (fn []
        (settings/with-settings {:jobs.lib.animal-spare/off-limits #{"sheep"}}
          (fn ^:async t []
            (let [s (await (spared-hunt {} {:inventory h/sword :entities (four-sheep)} nil))]
              (is (empty? (attacked s)))
              (is (= {:off-limits 4} (:spared-by (done-event s)))))))))))

(deftest an-explicit-hunt-may-lift-the-pen-rule
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (spared-hunt {:spare-pens false} {:inventory h/sword :entities (four-sheep)} own-pen))]
          (is (seq (attacked s))))))))

(deftest every-kill-of-a-passive-mob-is-an-event-naming-the-rule-that-allowed-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pigs (mapv #(animal % "pig" (+ 2 %) {:drops [{:name "porkchop" :count 1}]}) [1 2 3 4])
              s (await (scenario {:mob "pig" :count 2 :keep 2} {:inventory h/sword :entities pigs} 40))
              es (events-of s :animal.killed)]
          (is (= [["pig" "jobs.combat.hunt" :surplus] ["pig" "jobs.combat.hunt" :surplus]]
                 (mapv (juxt :animal :by :rule) es)))
          (is (= #{1 2} (set (map :entity es))))
          (is (every? #(= 64 (second (:cell %))) es)))))))

(deftest a-hostile-kill-is-no-passive-kill-event
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "zombie" :count 1} {:inventory h/sword :entities [(animal 1 "zombie" 3 {:kind "hostile"})]} 20))]
          (is (empty? (events-of s :animal.killed))))))))
