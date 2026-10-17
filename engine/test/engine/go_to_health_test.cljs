(ns engine.go-to-health-test
  "go-to when the damage budget refuses the only way: heal or wait, then the over-budget drop down to the floor, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.expr :as expr]
            [engine.fake :as fake]
            [engine.go-to-test :as g]
            [engine.registry :as registry]
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

(def args {:pos [40 60 0] :ways #{:walk :clear-seal} :floor {:health 1}})

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
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13}) (assoc args :food 10) false))]
          (is (empty? (g/events-of r :go-to.waiting-health)))
          (is (= 1 (count (g/events-of r :go-to.over-budget))) "it told it went over the budget")
          (is (= 60 (y-of p)) "down the cliff")
          (is (= {:arrived true} @out)))))))

(deftest a-hurt-body-with-bread-eats-before-it-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13} {:inventory [{:name "bread" :count 3}]}) (assoc args :food 10) true))]
          (is (< (count (filter #(= "bread" (:name %)) (.-inventory (.self p)))) 3) "it ate")
          (is (= 60 (y-of p)) "down the cliff")
          (is (= {:arrived true} @out)))))))

(deftest by-default-a-walk-never-goes-over-its-damage-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13}) (assoc (dissoc args :floor) :food 10) false))]
          (is (= 64 (y-of p)) "still on the plateau")
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= {:arrived false :reason :unreachable :why :needs-health :hp-cost 1 :hp-budget 0} (select-keys @out [:arrived :reason :why :hp-cost :hp-budget])))
          (is (re-find #"a way exists but costs 1 hp, more than the 0 hp" (:text @out))))))))

(def cliff11 (merge (floor -2 -3 10 3) (floor 52 11 -3 47 3)))

(deftest at-full-health-a-too-dear-way-says-healing-cannot-help
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! {:blocks cliff11 :self {:pos g/start :health 20 :food 20}} (-> args (dissoc :floor) (assoc :food 20 :pos [40 53 0])) false))]
          (is (= 64 (y-of p)) "still on the plateau")
          (is (= {:why :needs-health :heal :full :health 20 :hp-cost 8 :hp-budget 7}
                 (select-keys @out [:why :heal :health :hp-cost :hp-budget])))
          (is (re-find #"full health.*:floor \{:health 1\}" (:text @out))))))))

(deftest a-callers-floor-is-never-crossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13}) (assoc-in (assoc args :food 10) [:floor :health] 12) false))]
          (is (= 64 (y-of p)) "still on the plateau")
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= {:arrived false :reason :unreachable :why :needs-health} (select-keys @out [:arrived :reason :why]))))))))

(deftest a-callers-max-damage-is-a-hard-cap-with-no-heal-or-over-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as r} (await (go-heal! (world {:health 13 :food 20}) (assoc args :spend {:health 0}) true))]
          (is (= 64 (y-of p)))
          (is (empty? (g/events-of r :go-to.waiting-health)))
          (is (empty? (g/events-of r :go-to.over-budget)))
          (is (= :unreachable (:reason @out))))))))

(deftest the-call-keys-are-the-floor-and-the-damage-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [call {:pos [40 60 0] :ways #{:walk} :food 10}
              low (await (go-heal! (world {:health 13}) (assoc call :floor {:health 1}) false))
              floor (await (go-heal! (world {:health 13}) (assoc call :floor {:health 12}) false))
              capped (await (go-heal! (world {:health 13 :food 20}) (assoc call :food 20 :floor {:health 1} :spend {:health 0}) true))]
          (is (= 60 (y-of (:p low))) "a floor of 1 goes over the budget")
          (is (= 1 (count (g/events-of low :go-to.over-budget))))
          (is (= 64 (y-of (:p floor))) "a floor of 12 is never crossed")
          (is (= {:why :needs-health :kind :limit-reached} {:why (:why @(:out floor)) :kind (:kind (:ending (meta @(:out floor))))}))
          (is (= 64 (y-of (:p capped))) ":spend :health 0 takes no drop")
          (is (empty? (g/events-of capped :go-to.waiting-health))))))))

(deftest the-food-defaults-to-the-bodys-own-and-the-caller-overrides-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [starving (await (go-heal! (world {:health 13 :food 3}) args false))
              told-fed (await (go-heal! (world {:health 13 :food 3}) (assoc args :food 20) true))
              told-hungry (await (go-heal! (world {:health 13 :food 20}) (assoc args :food 3) false))]
          (is (empty? (g/events-of starving :go-to.waiting-health)) "no :food given: the body's own 3, nothing to regenerate with")
          (is (= 1 (count (g/events-of starving :go-to.over-budget))))
          (is (seq (g/events-of told-fed :go-to.waiting-health)) ":food 20 given: counted as fed")
          (is (empty? (g/events-of told-hungry :go-to.waiting-health)) ":food 3 given: nothing to regenerate with")
          (is (= 1 (count (g/events-of told-hungry :go-to.over-budget)))))))))

(defn ^:async probes-while-waiting
  "How many probe plans (a survivable budget over 0) a body that never heals asks for while go-to waits for it."
  []
  (let [probes (atom 0)
        orig wplan/plan-within!]
    (set! wplan/plan-within! (fn [c pw to range weight policy]
                               (when (pos? (:damage-budget policy 0)) (swap! probes inc))
                               (orig c pw to range weight policy)))
    (try (await (go-heal! (world {:health 13}) args false))
         (finally (set! wplan/plan-within! orig)))
    @probes))

(deftest a-heal-wait-probes-once-while-health-and-place-stay
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= 1 (await (probes-while-waiting))))))))

(deftest a-full-health-body-takes-the-drop-with-no-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as r} (await (go-heal! (world {:health 20 :food 20}) args false))]
          (is (= 60 (y-of p)))
          (is (empty? (g/events-of r :go-to.waiting-health))))))))

(defn refusal [args] (expr/problem registry/jobs (list 'jobs.movement.go-to args)))

(deftest go-to-refuses-a-bad-hp-seconds-at-submit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [bad [0 -1 "x" js/Infinity]]
          (is (re-find #":hp-seconds must be a number above 0" (refusal {:pos [6 64 0] :hp-seconds bad})) (pr-str bad)))
        (let [{:keys [out]} (await (g/go! {:blocks g/flat} {:pos [6 64 0] :hp-seconds 30}))]
          (is (= {:arrived true} @out)))))))

(deftest go-to-refuses-bad-danger-options-and-fall-margin-at-submit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[k bad] [[:fall-margin [-1 "x" js/Infinity]]
                         [:flee-factor [-1 "x" js/Infinity]]
                         [:fight-factor [-1 "x" js/Infinity]]
                         [:danger-max-rate [0 -1 "x" js/Infinity]]
                         [:danger-shape ["x" {:sensed {:radius -1}} {:sensed {:close 5 :radius 2}} {:bogus {}}]]]
                v bad]
          (is (re-find (re-pattern (str k " must be ")) (refusal {:pos [6 64 0] k v})) (pr-str k v)))
        (let [{:keys [out]} (await (g/go! {:blocks g/flat} {:pos [6 64 0] :fall-margin 0 :flee-factor 2 :fight-factor 0 :danger-max-rate 3
                                                           :danger-shape {:sensed {:radius 9}}}))]
          (is (= {:arrived true} @out)))))))

(deftest a-heal-wait-ends-at-its-health-or-when-its-time-is-up
  (let [now 1000000
        st {:to 14 :until (+ now 8000)}]
    (is (gth/waiting? st (+ now 7999)))
    (is (not (gth/waiting? st (+ now 8000))))
    (is (not (gth/stalled? st 13 (+ now 7999))))
    (is (gth/stalled? st 13 (+ now 8000)))
    (is (not (gth/stalled? st 14 (+ now 9000))) "reached the health: not stalled")
    (is (nil? (gth/heal-state {:heal st} 14)) "a wait that reached its health is over")
    (is (= st (gth/heal-state {:heal st} 13)))))

(deftest go-to-can-run-says-later-healing-only-while-a-fed-body-below-its-health-is-within-the-wait
  (let [now 1000000
        st {:to 14 :until (+ now 8000)}]
    (are [self at mem kind] (= kind
                               (with-redefs [ctx/mem (constantly mem) ctx/now (constantly at)]
                                 (first (go-to/can-run? {:primitives (tu/fake-on-floor {:floor [-5 -5 20 5] :self self}) :args {}}))))
      {:health 13 :food 20} now {:heal st} :later
      {:health 14 :food 20} now {:heal st} :ok
      {:health 13 :food 20} (+ now 8000) {:heal st} :ok
      {:health 13 :food 3} now {:heal st} :ok
      {:health 13 :food 20} now {} :ok)))

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
