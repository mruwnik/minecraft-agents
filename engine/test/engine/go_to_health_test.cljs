(ns engine.go-to-health-test
  "go-to when the damage budget refuses the only way: no heal or wait: needs-health, or the over-budget drop down to the floor, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.args :as a]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.go-to-test :as g]
            [engine.test-util :as tu :refer [floor]]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.watch :as wwatch]
            [jobs.movement.go-to :as go-to]
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

(defn ^:async go-heal!
  "go-to over world with args as a child; each tick takes 1 s of the fake clock and, when heal? is true, heals the body 1 hp
  (to 20), the regeneration the fake lacks; {:out :p :seen :eng :clock}."
  [world args heal?]
  (let [clock (atom 1000000)
        {:keys [eng p] :as s} (g/setup (assoc world :clock clock))
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (g/recording-parent out args)))]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 60) (seq (:list (core/state eng))))
        (swap! clock + 1000)
        (await (core/tick! eng))
        (when heal? (swap! (fake/state p) update-in [:self :health] #(min 20 (inc %))))
        (recur (inc i))))
    (assoc s :eng eng :out out :clock clock)))

(defn y-of [p] (js/Math.floor (second (g/at p))))

(def args {:pos [40 60 0] :ways #{:walk :dig} :may-dig #{:own} :floor {:health 1}})

(deftest a-hurt-fed-body-neither-waits-nor-eats-it-gives-up-needs-health
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 20} {:inventory [{:name "bread" :count 3}]}) (dissoc args :floor) true))]
          (is (= {:why :needs-health :heal :regen :health 13} (select-keys @out [:why :heal :health])))
          (is (= 64 (y-of p)) "still on the plateau")
          (is (= 3 (transduce (comp (filter #(= "bread" (.-name %))) (map #(.-count %))) + (.-inventory (.self p)))) "it ate nothing"))))))

(deftest a-hurt-body-with-nothing-to-heal-with-drops-over-its-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 10}) args false))]
          (is (= 1 (count (g/events-of r :go-to.over-budget))) "it told it went over the budget")
          (is (= 60 (y-of p)) "down the cliff")
          (is (= {:arrived true} @out)))))))

(deftest by-default-a-walk-never-goes-over-its-damage-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 10}) (dissoc args :floor) false))]
          (is (= 64 (y-of p)) "still on the plateau")
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= {:arrived false :reason :unreachable :why :needs-health :hp-cost 1 :hp-budget 0} (select-keys @out [:arrived :reason :why :hp-cost :hp-budget])))
          (is (re-find #"a way exists but costs 1 hp, more than the 0 hp" (:text @out))))))))

(def cliff11 (merge (floor -2 -3 10 3) (floor 52 11 -3 47 3)))

(deftest at-full-health-a-too-dear-way-says-healing-cannot-help
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! {:blocks cliff11 :self {:pos g/start :health 20 :food 20}} (-> args (dissoc :floor) (assoc :pos [40 53 0])) false))]
          (is (= 64 (y-of p)) "still on the plateau")
          (is (= {:why :needs-health :heal :full :health 20 :hp-cost 8 :hp-budget 7}
                 (select-keys @out [:why :heal :health :hp-cost :hp-budget])))
          (is (re-find #"full health.*:floor \{:health 1\}" (:text @out))))))))

(deftest a-callers-floor-is-never-crossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 10}) (assoc-in args [:floor :health] 12) false))]
          (is (= 64 (y-of p)) "still on the plateau")
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= {:arrived false :reason :unreachable :why :needs-health} (select-keys @out [:arrived :reason :why]))))))))

(deftest a-callers-max-damage-is-a-hard-cap-with-no-heal-or-over-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 20}) (assoc args :spend {:health 0}) true))]
          (is (= 64 (y-of p)))
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= :unreachable (:reason @out))))))))

(deftest the-call-keys-are-the-floor-and-the-damage-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [call {:pos [40 60 0] :ways #{:walk}}
              low (await (go-heal! (world {:health 13 :food 10}) (assoc call :floor {:health 1}) false))
              floor (await (go-heal! (world {:health 13 :food 10}) (assoc call :floor {:health 12}) false))
              capped (await (go-heal! (world {:health 13 :food 20}) (assoc call :floor {:health 1} :spend {:health 0}) true))]
          (is (= 60 (y-of (:p low))) "a floor of 1 goes over the budget")
          (is (= 1 (count (g/events-of low :go-to.over-budget))))
          (is (= 64 (y-of (:p floor))) "a floor of 12 is never crossed")
          (is (= {:why :needs-health :kind :limit-reached} {:why (:why @(:out floor)) :kind (:kind (:ending (meta @(:out floor))))}))
          (is (= 64 (y-of (:p capped))) ":spend :health 0 takes no drop")
          (is (empty? (g/events-of capped :go-to.over-budget))))))))

(deftest the-walk-counts-on-the-bodys-own-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [starving (await (go-heal! (world {:health 13 :food 3}) args false))
              fed (await (go-heal! (world {:health 13 :food 20}) (dissoc args :floor) true))]
          (is (= 1 (count (g/events-of starving :go-to.over-budget))))
          (is (= :regen (:heal @(:out fed))) "food 20: counted as fed"))))))

(deftest a-full-health-body-takes-the-drop-with-no-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as r} (await (go-heal! (world {:health 20 :food 20}) args false))]
          (is (= 60 (y-of p)))
          (is (empty? (g/events-of r :go-to.over-budget))))))))

(deftest the-walk-price-settings-refuse-bad-values
  (doseq [[k bad] [[:jobs.lib.cost.health/hp-seconds [0 -1 "x" js/Infinity]]
                   [:jobs.lib.cost.danger/flee-factor [-1 "x" js/Infinity]]
                   [:jobs.lib.cost.danger/fight-factor [-1 "x" js/Infinity]]
                   [:jobs.lib.cost.danger/max-rate [0 -1 "x" js/Infinity]]
                   [:jobs.lib.cost.danger/shape ["x" {:sensed {:radius -1}} {:sensed {:close 5 :radius 2}} {:bogus {}}]]]
          v bad]
    (is (string? (a/setting-problem k v)) (pr-str k v))))

(deftest a-probe-that-found-no-way-is-not-kept-a-changed-world-may-have-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [memo (atom {})
              answers (atom ["none" "found"])
              orig-plan wplan/plan-within!
              orig-mem ctx/mem
              orig-update ctx/update-mem!
              orig-now ctx/now
              c {:primitives (tu/fake-on-floor {:floor [-5 -5 20 5]}) :args {}}]
          (set! wplan/plan-within! (fn [& _] (let [a (first @answers)] (swap! answers rest)
                                               (js/Promise.resolve {:r #js {:status a :path #js {:cost #js {:damage 1}}}}))))
          (set! ctx/mem (fn [_] @memo))
          (set! ctx/update-mem! (fn [_ f & args] (apply swap! memo f args)))
          (set! ctx/now (fn [_] 1000))
          (try
            (let [first-probe (await (gth/probe! c {:x 5 :y 64 :z 0} 0))
                  second-probe (await (gth/probe! c {:x 5 :y 64 :z 0} 0))]
              (is (nil? first-probe))
              (is (= 1 second-probe) "the second probe plans again: a nil is no reason to give up for heal-gap-ms"))
            (finally (set! wplan/plan-within! orig-plan) (set! ctx/mem orig-mem)
                     (set! ctx/update-mem! orig-update) (set! ctx/now orig-now))))))))
