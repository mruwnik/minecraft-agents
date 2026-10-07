(ns engine.go-to-health-test
  "go-to when the damage budget refuses the only way: heal or wait, then the over-budget drop down to the floor, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.go-to-test :as g]
            [engine.test-util :as tu :refer [floor]]
            [jobs.movement.go-to.health :as gth]))

(deftest the-way-to-heal-follows-health-food-and-what-is-carried
  (are [body way] (= way (gth/heal-way body))
    {:health 20 :food 20} :full
    {:health 13 :food 20} :regen
    {:health 13 :food 18} :regen
    {:health 13 :food 10 :carried-food true} :eat
    {:health 13 :food 10} :none
    {:health 13 :food 20 :on-fire true} :none
    {:health 13 :food 20 :effects ["poison"]} :none
    {:health 13 :food 20 :effects ["wither"]} :none
    {:health 13 :food 20 :effects ["speed"]} :regen))

;; a 4-block cliff (1 hp): the plateau (feet 64) ends at x 10, the floor below it has feet 60. At health 13 the budget is 0.
(def cliff4 (merge (floor -2 -3 10 3) (floor 59 11 -3 47 3)))

(defn world [self & [extra]]
  (merge {:blocks cliff4 :self (merge {:pos g/start} self)} extra))

(defn heal-on-wait!
  "Every wait act of p heals the body 1 hp (to 20): the regeneration the fake lacks."
  [p]
  (let [s (fake/state p)]
    (.override (.-world p) "wait"
               (fn [token args impl]
                 (swap! s update-in [:self :health] #(min 20 (inc %)))
                 (impl token args)))))

(defn ^:async go-heal!
  "go-to over world with args as a child; the body heals while it waits when heal? is true; {:out :p :seen :eng}."
  [world args heal?]
  (let [{:keys [eng p] :as s} (g/setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (g/recording-parent out args)))]
    (when heal? (heal-on-wait! p))
    (core/submit! eng '(recording-parent) {})
    (assoc s :eng eng :out out :ticks (await (g/tick-out! eng 60)))))

(defn y-of [p] (js/Math.floor (second (g/at p))))

(def args {:pos [40 60 0] :escalate false})

(deftest a-hurt-body-with-food-waits-to-heal-and-then-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; health 13: the budget is 0, the cliff costs 1
        (let [{:keys [p out seen] :as r} (await (go-heal! (world {:health 13 :food 20}) args true))]
          (is (seq (g/events-of r :go-to.waiting-health)) "it said it waits for health")
          (is (empty? (g/events-of r :go-to.over-budget)) "it did not go over the budget")
          (is (= 60 (y-of p)) "down the cliff")
          (is (>= (:health (:self @(fake/state p))) 14) "healed to a budget of 1 hp before it dropped"))))))

(deftest a-hurt-body-with-nothing-to-heal-with-drops-over-its-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 10}) args false))]
          (is (empty? (g/events-of r :go-to.waiting-health)))
          (is (= 1 (count (g/events-of r :go-to.over-budget))) "it told it went over the budget")
          (is (= 60 (y-of p)) "down the cliff")
          (is (= {:arrived true} @out)))))))

(deftest a-hurt-body-with-bread-eats-before-it-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 10} {:inventory [{:name "bread" :count 3}]}) args true))]
          (is (< (count (filter #(= "bread" (:name %)) (.-inventory (.self p)))) 3) "it ate")
          (is (= 60 (y-of p)) "down the cliff")
          (is (= {:arrived true} @out)))))))

(deftest a-callers-floor-is-never-crossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 10}) (assoc args :min-health 12) false))]
          (is (= 64 (y-of p)) "still on the plateau")
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= {:arrived false :reason :unreachable :why :needs-health} (select-keys @out [:arrived :reason :why]))))))))

(deftest a-callers-max-damage-is-a-hard-cap-with-no-heal-or-over-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 20}) (assoc args :max-damage 0) true))]
          (is (= 64 (y-of p)))
          (is (empty? (g/events-of r :go-to.waiting-health)))
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= :unreachable (:reason @out))))))))

(deftest a-full-health-body-takes-the-drop-with-no-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as r} (await (go-heal! (world {:health 20 :food 20}) args false))]
          (is (= 60 (y-of p)))
          (is (empty? (g/events-of r :go-to.waiting-health))))))))

(deftest go-to-refuses-a-bad-hp-seconds-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [bad [0 -1 "x" js/Infinity]]
          (let [{:keys [out]} (await (g/go! {:blocks g/flat} {:pos [6 64 0] :hp-seconds bad}))]
            (is (= {:arrived false :reason :bad-hp-seconds} (select-keys @out [:arrived :reason])) (pr-str bad))))
        (let [{:keys [out]} (await (g/go! {:blocks g/flat} {:pos [6 64 0] :hp-seconds 30}))]
          (is (= {:arrived true} @out)))))))

(deftest healing-that-gains-nothing-stalls-after-the-stall-time
  (let [now 1000000
        st (gth/next-state nil 13 now {})]
    (is (not (gth/stalled? st 13 (+ now 29000))))
    (is (gth/stalled? st 13 (+ now 30000)))
    (is (not (gth/stalled? st 14 (+ now 31000))))
    (is (= {:health 14 :t (+ now 31000)} (select-keys (gth/next-state st 14 (+ now 31000) {}) [:health :t])))
    (is (nil? (gth/heal-state {:heal (assoc st :last now)} (+ now gth/heal-gap-ms))) "an old state is a new wait")))
