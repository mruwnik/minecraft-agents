(ns engine.recover-drops-flight-test
  "recover-drops owns the danger it priced: no decline by radius on a priced mob, a mob new since the decision is
  weighed, and a trip turned dear backs off only while the walk back still fits the despawn window."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered pile-drops emitted]]
            [engine.library-test :refer [setup]]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.recover-drops-danger-test :refer [iron-kit death-t]]
            [engine.test-util :as tu :refer [run-until-empty]]))

(def guard
  "A skeleton 5.7 blocks from the pile: inside the hostile-near radius of the body once it stands there."
  {:id 9 :name "skeleton" :kind "hostile" :pos {:x 24 :y 64 :z 4}})

(def zombie-near-the-body "Near the body and beside the way ahead: it catches the walk." {:id 8 :name "zombie" :kind "hostile" :pos {:x 6.5 :y 64 :z 1.5}})

(def dear-zombie "A pile worth less than the trip with an armed body's fight past a zombie, more than without it."
  '(jobs.survival.recover-drops {:value-overrides {"raw_iron" 3 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 20 "diamond_sword" 10}}))

(defn danger-declines [seen] (filterv #(and (= :danger (:reason %)) (:mob %)) (emitted seen :stopped)))

(deftest a-guarded-pile-the-trip-priced-is-collected-without-a-danger-decline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pile dt/death-pile-of-the-field-report
              {:keys [eng seen]} (setup (merge iron-kit {:floor tu/walk-floor :entities (conj (vec (pile-drops pile)) guard)}))]
          (die! eng {:pos dt/death-pos :inventory pile :cause "skeleton"})
          (core/submit! eng job {})
          (await (run-until-empty eng 30))
          (is (= [] (danger-declines seen)))
          (is (= :collected (:decision (recovered eng)))))))))

(deftest a-danger-unknown-at-the-decision-is-weighed-not-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :inventory dt/sword :entities (conj (vec (pile-drops dt/diamonds)) zombie-near-the-body)})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (mem/write! (:store eng) :recover-trip {:death-t (death-t eng) :decided {:value 1400 :cost 50} :baseline {} :phase :go}
                      {:cap 1 :ttl 1000000})
          (core/submit! eng dear-zombie {})
          (await (run-until-empty eng 20))
          (is (= [] (danger-declines seen)))
          (is (re-find #"zombie" (:text (dt/danger-remembered eng)))))))))

(defn dear-run
  "A death ago-ms back past a zombie that makes the pile dear (dear-zombie), run to its decision."
  [ago-ms]
  (fn ^:async t []
    (let [{:keys [eng]} (setup {:floor tu/walk-floor :inventory dt/sword :entities (conj (vec (pile-drops dt/diamonds)) dt/zombie-by-the-way)})]
      (die! eng {:pos dt/death-pos :inventory dt/diamonds} ago-ms)
      (core/submit! eng dear-zombie {})
      (await (run-until-empty eng 5))
      {:eng eng :danger (:data (mem/latest (mem/view (:store eng)) :recover-danger))})))

(deftest a-back-off-comes-back-before-the-pile-despawns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng danger]} (await ((dear-run 200000)))]
          (is (nil? (recovered eng)) "the death stays open")
          (is (< (:until danger) (+ (death-t eng) 300000))))))))

(deftest a-back-off-with-no-time-to-come-back-fetches-the-pile-under-the-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng danger]} (await ((dear-run 288000)))]
          (is (nil? danger) "no wait that the window cannot hold")
          (is (not= :abandoned (:decision (recovered eng)))))))))

(defn on-first-leg!
  "Cut the first walk step short (2 s, a few blocks, the clock moved on by them so the leg's seconds are used) and run (f) as it is taken."
  [p clock f]
  (let [n (atom 0)]
    (.override (.-world p) "steer"
               (fn [token args impl]
                 (when (= 1 (swap! n inc))
                   (set! (.-timeoutS args) 2)
                   (swap! clock + 2000)
                   (f))
                 (impl token args)))))

(defn place-entity! [p id pos]
  (swap! (fake/state p) update :entities (fn [es] (mapv #(cond-> % (= id (:id %)) (assoc :pos pos)) es))))

(deftest a-zombie-near-at-the-start-is-weighed-as-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 8 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}
              {:keys [eng p seen clock]} (setup {:floor tu/walk-floor :inventory dt/sword :entities (conj (vec (pile-drops dt/diamonds)) zombie)})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (core/submit! eng dear-zombie {})
          (await (run-until-empty eng 5))
          (is (re-find #"zombie" (:text (dt/danger-remembered eng))) "the skip for danger is remembered, naming the mob")
          (is (= [] (tu/walk-calls p)) "the body did not walk toward the pile"))))))

(deftest a-priced-zombie-near-during-the-settling-hold-is-weighed-not-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 8 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}}
              {:keys [eng seen clock]} (setup {:floor tu/walk-floor :inventory dt/sword :entities (conj (vec (pile-drops dt/diamonds)) zombie)})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds} 2500)
          (core/submit! eng dear-zombie {})
          (let [timer (js/setInterval #(swap! clock + 500) 5)]
            (await (run-until-empty eng 12))
            (js/clearInterval timer))
          (is (= [] (danger-declines seen)))
          (is (some? (dt/danger-remembered eng))))))))

(deftest a-far-mob-that-closes-in-is-weighed-when-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 8 :name "zombie" :kind "hostile" :pos {:x 20 :y 64 :z -40}}
              {:keys [eng p seen clock]} (setup {:floor tu/walk-floor :inventory dt/sword :entities (conj (vec (pile-drops dt/diamonds)) zombie)})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (on-first-leg! p clock #(place-entity! p 8 [19.5 64 2.5]))
          (core/submit! eng (list 'jobs.survival.recover-drops {:settings {:jobs.survival.recover-drops.config/leg-s 1} :value-overrides {"raw_iron" 3 "cobblestone" 0.05 "oak_planks" 0.05 "stone_pickaxe" 1 "diamond_pickaxe" 20 "diamond_sword" 10}}) {})
          (await (run-until-empty eng 30))
          (is (some? (dt/danger-remembered eng)) "the trip turned dear: it backs off")
          (is (nil? (recovered eng)) "the death stays open"))))))

(deftest a-hit-on-the-way-weighs-the-trip-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:id 8 :name "zombie" :kind "hostile" :pos {:x 19.5 :y 64 :z 1.5}}
              {:keys [eng p seen clock]} (setup {:floor tu/walk-floor :inventory dt/sword :entities (conj (vec (pile-drops dt/diamonds)) zombie)})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (on-first-leg! p clock (fn []
                             (swap! (fake/state p) assoc-in [:self :health] 2)
                             (mem/write! (:store eng) :hurt {:amount 18 :health 2 :attacker {:id 8 :name "zombie"}} {:cap 50 :ttl 60000})))
          (core/submit! eng (list 'jobs.survival.recover-drops {:settings {:jobs.survival.recover-drops.config/leg-s 1} :value-overrides (:value-overrides (second dear-zombie))}) {})
          (await (run-until-empty eng 30))
          (is (some? (dt/danger-remembered eng)) "hit and low, the trip backs off")
          (is (nil? (recovered eng)) "the death stays open"))))))

(deftest a-back-off-counts-the-flight-before-the-walk-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng danger]} (await ((dear-run 281000)))]
          (is (nil? danger) "the flight and the walk back leave no time to wait")
          (is (not= :abandoned (:decision (recovered eng)))))))))
