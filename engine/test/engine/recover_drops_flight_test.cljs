(ns engine.recover-drops-flight-test
  "recover-drops owns the danger it priced: no decline by radius on a priced mob, a mob new since the decision is
  weighed, and a trip turned dear backs off only while the walk back still fits the despawn window."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.death-test :as dt :refer [die! job recovered pile-drops emitted]]
            [engine.library-test :refer [setup]]
            [engine.memory :as mem]
            [engine.recover-drops-danger-test :refer [iron-kit death-t]]
            [engine.test-util :as tu :refer [run-until-empty]]))

(def guard
  "A skeleton 5.7 blocks from the pile: inside :danger-radius of the body once it stands there."
  {:id 9 :name "skeleton" :kind "hostile" :pos {:x 24 :y 64 :z 4}})

(def zombie-near-the-body {:id 8 :name "zombie" :kind "hostile" :pos {:x 4.5 :y 64 :z 3.5}})

(def dear-zombie '(jobs.survival.recover-drops {:danger-overrides {"zombie" 1000}}))

(defn danger-declines [seen] (filterv #(= :danger (:reason %)) (emitted seen :recover-drops.declined)))

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
        (let [{:keys [eng seen]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops dt/diamonds)) zombie-near-the-body)})]
          (die! eng {:pos dt/death-pos :inventory dt/diamonds})
          (mem/write! (:store eng) :recover-trip {:death-t (death-t eng) :decided {:value 1400 :cost 50} :baseline {} :phase :go}
                      {:cap 1 :ttl 1000000})
          (core/submit! eng job {})
          (await (run-until-empty eng 20))
          (is (= [] (danger-declines seen)))
          (is (= ["zombie"] (map :name (:top-mobs (last (emitted seen :recover-drops.decided)))))))))))

(defn dear-run
  "A death ago-ms back with a zombie priced far dearer than the pile, run to its decision."
  [ago-ms]
  (fn ^:async t []
    (let [{:keys [eng]} (setup {:floor tu/walk-floor :entities (conj (vec (pile-drops dt/diamonds)) dt/zombie-by-the-way)})]
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

(deftest a-back-off-with-no-time-to-come-back-abandons-the-pile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await ((dear-run 288000)))]
          (is (= {:decision :abandoned :reason :danger} (select-keys (recovered eng) [:decision :reason]))))))))
