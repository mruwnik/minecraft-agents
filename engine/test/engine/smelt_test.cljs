(ns engine.smelt-test
  "jobs.items.smelt: the decisions as plain functions, then against the fake world's furnaces."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.smelt :as smelt]))

;; ------------------------------------------------------------------ tables

(deftest what-each-kind-smelts
  (are [kind item ok] (= ok (boolean (smelt/smelts? kind item)))
    "furnace" "raw_iron" true
    "blast_furnace" "raw_iron" true
    "smoker" "raw_iron" false
    "furnace" "beef" true
    "smoker" "beef" true
    "blast_furnace" "beef" false
    "furnace" "iron_ore" true
    "furnace" "deepslate_gold_ore" true
    "furnace" "cobblestone" true
    "furnace" "oak_log" true
    "smoker" "potato" true
    "furnace" "dirt" false
    "furnace" "coal" false))

(deftest fuel-values-in-items-per-fuel-item
  (are [item per] (= per (smelt/fuel-per-unit item))
    "coal" 8
    "charcoal" 8
    "oak_planks" 1.5
    "spruce_log" 1.5
    "stick" 0.5
    "dirt" nil))

(deftest cook-times-per-kind
  (are [kind ticks] (= ticks (smelt/cook-ticks kind))
    "furnace" 200
    "blast_furnace" 100
    "smoker" 100))

;; ------------------------------------------------------------------ the load plan

(def empty-furnace {:kind "furnace" :input nil :fuel nil :output nil :lit false :burn {:left 0 :total 0} :cook {:done 0 :total 0}})
(defn inv [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))
(defn plan [m] (smelt/plan-load (merge {:state empty-furnace :carried [] :item nil :count nil :fuel nil} m)))

(deftest the-plan-loads-what-was-asked-and-the-fuel-it-needs
  (are [m expected] (= expected (plan m))
    {:carried (inv "raw_iron" 5 "coal" 3) :item "raw_iron" :count 3}
    {:item "raw_iron" :count 3 :fuel {:item "coal" :count 1}}

    {:carried (inv "raw_iron" 5 "coal" 3) :item "raw_iron"}
    {:item "raw_iron" :count 5 :fuel {:item "coal" :count 1}}

    {:carried (inv "raw_iron" 20 "coal" 3) :item "raw_iron" :count 9}
    {:item "raw_iron" :count 9 :fuel {:item "coal" :count 2}}

    {:carried (inv "raw_iron" 3 "oak_planks" 4) :item "raw_iron" :count 3}
    {:item "raw_iron" :count 3 :fuel {:item "oak_planks" :count 2}}

    {:carried (inv "beef" 4 "coal" 3) :item "beef" :count 4 :state (assoc empty-furnace :kind "smoker")}
    {:item "beef" :count 4 :fuel {:item "coal" :count 1}}

    {:carried (inv "raw_iron" 3 "oak_planks" 4 "coal" 1) :item "raw_iron" :count 3 :fuel "oak_planks"}
    {:item "raw_iron" :count 3 :fuel {:item "oak_planks" :count 2}}

    {:carried (inv "raw_iron" 3 "oak_planks" 4 "coal" 1 "stick" 8) :item "raw_iron" :count 3}
    {:item "raw_iron" :count 3 :fuel {:item "coal" :count 1}}

    {:carried (inv "raw_iron" 3 "stick" 8 "oak_planks" 4) :item "raw_iron" :count 3}
    {:item "raw_iron" :count 3 :fuel {:item "oak_planks" :count 2}}))

(deftest the-item-is-chosen-from-what-is-carried-when-not-given
  (are [m expected] (= expected (:item (plan m)))
    {:carried (inv "dirt" 3 "coal" 3 "raw_iron" 2)} "raw_iron"
    {:carried (inv "raw_iron" 2 "beef" 2 "coal" 3) :state (assoc empty-furnace :kind "smoker")} "beef"))

(deftest the-plan-gives-up-with-a-reason
  (are [m reason] (= {:give-up reason} (plan m))
    {:carried (inv "dirt" 3 "coal" 3)} "nothing-smeltable"
    {:carried (inv "coal" 3) :item "raw_iron"} "no-item"
    {:carried (inv "raw_iron" 3 "coal" 3) :item "raw_iron" :state (assoc empty-furnace :kind "smoker")} "not-smeltable"
    {:carried (inv "raw_iron" 3) :item "raw_iron"} "no-fuel"
    {:carried (inv "raw_iron" 3 "dirt" 1) :item "raw_iron" :fuel "coal"} "no-fuel"
    {:carried (inv "raw_iron" 3 "oak_log" 5) :item "oak_log" :count 3} "no-fuel"
    {:carried (inv "raw_iron" 3 "coal" 3) :item "raw_iron" :state (assoc empty-furnace :input {:name "beef" :count 2})} "furnace-busy"
    {:carried (inv "raw_iron" 3 "coal" 3) :item "raw_iron" :state (assoc empty-furnace :input {:name "raw_iron" :count 64})} "furnace-full"
    {:carried (inv "raw_iron" 3 "coal" 3) :item "raw_iron" :state (assoc empty-furnace :fuel {:name "oak_planks" :count 1})} "fuel-busy"
    {:carried (inv "raw_iron" 3 "oak_planks" 1) :item "raw_iron" :count 3 :state (assoc empty-furnace :input {:name "raw_iron" :count 10})} "no-fuel"))

(deftest a-given-fuel-is-used-and-an-unknown-one-is-refused
  (is (= {:name "charcoal" :count 1} (let [f (:fuel (plan {:carried (inv "raw_iron" 3 "coal" 3 "charcoal" 2) :item "raw_iron" :fuel "charcoal"}))] {:name (:item f) :count (:count f)})))
  (is (= {:give-up "unknown-fuel"} (plan {:carried (inv "raw_iron" 3 "dirt" 3) :item "raw_iron" :fuel "dirt"}))))

(deftest less-fuel-than-the-count-needs-smelts-what-the-fuel-covers
  (are [m expected] (= expected (plan m))
    {:carried (inv "raw_iron" 20 "coal" 1) :item "raw_iron" :count 20}
    {:item "raw_iron" :count 8 :fuel {:item "coal" :count 1}}

    {:carried (inv "raw_iron" 20 "oak_planks" 2) :item "raw_iron" :count 20}
    {:item "raw_iron" :count 3 :fuel {:item "oak_planks" :count 2}}))

(deftest fuel_already_in_the_furnace_counts
  (are [state expected] (= expected (plan {:carried (inv "raw_iron" 3) :item "raw_iron" :count 3 :state (merge empty-furnace state)}))
    {:burn {:left 1600 :total 1600}} {:item "raw_iron" :count 3 :fuel nil}
    {:fuel {:name "coal" :count 1}} {:item "raw_iron" :count 3 :fuel nil}
    {:fuel {:name "oak_planks" :count 2}} {:item "raw_iron" :count 3 :fuel nil}
    {:burn {:left 300 :total 1600}} {:item "raw_iron" :count 1 :fuel nil}))

(deftest a-given-fuel-that-is-not-carried-leaves-the-count-to-the-fire-already-burning
  (is (= {:item "raw_iron" :count 3 :fuel nil}
         (plan {:carried (inv "raw_iron" 5) :item "raw_iron" :count 5 :fuel "coal"
                :state (assoc empty-furnace :burn {:left 600 :total 1600})}))))

(deftest the-fuel-slot-takes-the-same-fuel-on-top
  (is (= {:item "raw_iron" :count 3 :fuel {:item "oak_planks" :count 2}}
         (plan {:carried (inv "raw_iron" 3 "oak_planks" 4 "coal" 1) :item "raw_iron" :count 3
                :state (assoc empty-furnace :fuel {:name "oak_planks" :count 1})}))))

(deftest input-already-in-the-slot-counts-for-fuel-and-room
  (are [m expected] (= expected (plan (merge {:item "raw_iron" :carried (inv "raw_iron" 100 "coal" 9)} m)))
    {:state (assoc empty-furnace :input {:name "raw_iron" :count 10})}
    {:item "raw_iron" :count 54 :fuel {:item "coal" :count 8}}))

(deftest a-target-makes-the-load-resumable
  (are [m expected] (= expected (plan (merge {:item "raw_iron" :target 3 :carried (inv "raw_iron" 2 "coal" 2)} m)))
    {:state (assoc empty-furnace :input {:name "raw_iron" :count 3} :burn {:left 1500 :total 1600})}
    {:item "raw_iron" :count 0 :fuel nil}

    {:state (assoc empty-furnace :input {:name "raw_iron" :count 1})}
    {:item "raw_iron" :count 2 :fuel {:item "coal" :count 1}}))

;; ------------------------------------------------------------------ what a visit found

(deftest judging-what-the-furnace-holds
  (are [state verdict] (= verdict (smelt/judge (merge empty-furnace state)))
    {:output {:name "iron_ingot" :count 1}} :take
    {:output {:name "iron_ingot" :count 1} :input {:name "raw_iron" :count 1}} :take
    {:input {:name "raw_iron" :count 2} :lit true} :wait
    {:input {:name "raw_iron" :count 2} :lit false :fuel {:name "coal" :count 1}} :not-smeltable
    {:input {:name "raw_iron" :count 2} :lit false} :out-of-fuel
    {} :done))

(deftest the-wait-is-the-cook-time-left-plus-a-margin
  (is (= (+ smelt/slack-ms (* 50 600)) (smelt/wait-ms "furnace" {:input {:count 3} :cook {:done 0 :total 200}})))
  (is (= (+ smelt/slack-ms (* 50 (- 300 120))) (smelt/wait-ms "smoker" {:input {:count 3} :cook {:done 120 :total 100}})))
  (is (= (+ smelt/slack-ms (* 50 150)) (smelt/wait-ms "blast_furnace" {:input {:count 2} :cook {:done 50 :total 100}})))
  (is (= (+ smelt/slack-ms (* 50 600)) (smelt/wait-ms "furnace" {:input {:count 3} :cook nil})) "no bar: the whole cook"))

;; ------------------------------------------------------------------ the check

(def spot {:x 1 :y 64 :z 0})

(defn check-of
  "The job's check on a ctx holding memory m at time now, over a primitives whose block at spot is named block-name
  (a furnace by default) and has properties props; none when props is nil."
  ([m now props] (check-of m now props "furnace"))
  ([m now props block-name]
   (let [p #js {:blockAt (fn [_] (when props #js {:name block-name :properties (clj->js props)}))}]
     (boolean (smelt/check {:args {:furnace spot} :primitives p :root "j1" :slots []
                            :view (fn [] {:now now :data {:policies {(mem/job-kind "j1") mem/job-policy}
                                                         :entries {(mem/job-kind "j1") [{:t now :data m}]}}})})))))

(deftest the-check-runs-the-first-round-at-once
  (is (true? (check-of {} 1000 {:lit false}))))

(deftest the-check-declines-while-it-cooks
  (are [now props expected] (= expected (check-of {:owed {:item "raw_iron" :count 3} :ready-at 100000 :unlit-from 7000} now props))
    1000 {:lit true} false
    1000 {:lit false} false
    6999 {:lit false} false
    7000 {:lit false} true
    7000 {:lit true} false
    7000 nil false
    99999 {:lit true} false
    100000 {:lit true} true
    100000 nil true))

(deftest the-check-wakes-for-a-furnace-that-is-no-longer-one
  (are [now block-name expected] (= expected (check-of {:owed {:item "raw_iron" :count 3} :ready-at 100000 :unlit-from 7000} now {:lit true} block-name))
    6999 "air" false
    7000 "air" true
    7000 "chest" true
    7000 "smoker" false
    7000 "blast_furnace" false))

(deftest without-a-furnace-the-first-round-runs-to-choose-one
  (is (true? (boolean (smelt/check {:args {:furnace nil} :root "j1" :slots [] :view (fn [] {:now 0 :data {}})})))))

;; ------------------------------------------------------------------ against the fake world

(def pos-key "1,64,0")
(def base-world {:blocks {pos-key "furnace"}
                 :inventory [{:name "raw_iron" :count 5} {:name "coal" :count 3}]})

(defn setup [world]
  (let [clock (atom 1000000)
        dir (tu/tmp-dir)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        make (fn [] (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}))]
    {:eng (make) :make make :p p :seen seen :clock clock}))

(defn ^:async ticks [eng n]
  (loop [i 0]
    (when (< i n)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn ops [p] (mapv #(.-op (.-args %)) (calls p "furnace")))
(defn inv-of [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn furnace-of [p] (get (:furnaces @(fake/state p)) (fake/parse-cell pos-key)))
(defn advance! [p ticks] (.advance (.-world p) ticks))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))
(def iron-job '(jobs.items.smelt {:furnace {:x 1 :y 64 :z 0} :item "raw_iron" :count 3}))

(deftest loads-then-the-body-is-free-and-the-output-is-taken-when-cooked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup base-world)
              _ (core/submit! eng iron-job {})
              _ (core/submit! eng '(jobs.debug.notify {:text "free"}) {})
              _ (await (ticks eng 1))]
          (is (= {"raw_iron" 2 "coal" 2} (inv-of p)) "3 iron and a coal went into the furnace")
          (is (= {:name "raw_iron" :count 3} (:input (furnace-of p))))
          (is (= ["read" "load"] (ops p)))
          (is (empty? (of-kind seen :job.notify)) "the notify job has not had its turn yet")
          (advance! p 1) ; the server lights the furnace on its next tick
          (swap! clock + 250)
          (await (ticks eng 3))
          (is (= 1 (count (of-kind seen :job.notify))) "while it cooks the body does the other job")
          (is (= ["read" "load"] (ops p)) "and the furnace is left alone")
          (is (= 1 (count (:list (core/state eng)))) "the smelt job stays listed")
          (swap! clock + 20000)
          (await (ticks eng 3))
          (is (= ["read" "load"] (ops p)) "20 s after the load three items are not cooked: still not looked at")
          (advance! p 600)
          (swap! clock + 40000)
          (await (ticks eng 3))
          (is (= {"raw_iron" 2 "coal" 2 "iron_ingot" 3} (inv-of p)))
          (is (empty? (:list (core/state eng))))
          (is (= ["read" "load" "read" "take"] (ops p)))
          (let [done-event (first (of-kind seen :smelt.done))]
            (is (= 3 (:smelted done-event)))))))))

(deftest a-check-that-wakes-early-finds-the-cook-unfinished-and-waits-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup base-world)
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (advance! p 200)
              _ (swap! clock + 40000)
              _ (await (ticks eng 1))]
          (is (nil? (:output (furnace-of p))) "one of three was done, and is taken")
          (is (= 1 (count (:list (core/state eng)))) "still listed: two more to cook")
          (is (= {"raw_iron" 2 "coal" 2 "iron_ingot" 1} (inv-of p)))
          (advance! p 400)
          (swap! clock + 40000)
          (await (ticks eng 2))
          (is (= 3 (get (inv-of p) "iron_ingot")))
          (is (empty? (:list (core/state eng))))
          (is (= 3 (:smelted (first (of-kind seen :smelt.done))))))))))

(defn ^:async smelt-to-the-end
  "A world with one furnace block, the job over item and count, the cook done and the clock past it: [p seen eng]."
  [block inventory item count]
  (let [{:keys [eng p seen clock]} (setup {:blocks {pos-key block} :inventory inventory})]
    (core/submit! eng (list 'jobs.items.smelt {:furnace {:x 1 :y 64 :z 0} :item item :count count}) {})
    (await (ticks eng 1))
    (advance! p 2000)
    (swap! clock + 100000)
    (await (ticks eng 2))
    [p seen eng]))

(deftest food-in-a-smoker-and-iron-in-a-blast-furnace
  (async done
    (tu/run-async done
      (fn ^:async t []
        (loop [rows [["smoker" "beef" "cooked_beef"] ["blast_furnace" "raw_iron" "iron_ingot"] ["furnace" "beef" "cooked_beef"]]]
          (when-let [[block item out] (first rows)]
            (let [[p _ eng] (await (smelt-to-the-end block [{:name item :count 4} {:name "coal" :count 2}] item 4))]
              (is (= 4 (get (inv-of p) out)) block)
              (is (empty? (:list (core/state eng))) block)
              (recur (rest rows)))))))))

(def gave-up-rows
  [["no furnace there" {:inventory [{:name "raw_iron" :count 3} {:name "coal" :count 2}]} "raw_iron" "no-furnace"]
   ["a chest" {:blocks {pos-key "chest"} :inventory [{:name "raw_iron" :count 3} {:name "coal" :count 2}]} "raw_iron" "not-a-furnace"]
   ["no fuel carried" {:blocks {pos-key "furnace"} :inventory [{:name "raw_iron" :count 3}]} "raw_iron" "no-fuel"]
   ["nothing smeltable" {:blocks {pos-key "furnace"} :inventory [{:name "dirt" :count 3} {:name "coal" :count 2}]} nil "nothing-smeltable"]
   ["the item is not carried" {:blocks {pos-key "furnace"} :inventory [{:name "coal" :count 2}]} "raw_iron" "no-item"]
   ["a smoker given iron" {:blocks {pos-key "smoker"} :inventory [{:name "raw_iron" :count 3} {:name "coal" :count 2}]} "raw_iron" "not-smeltable"]
   ["someone else's different input" {:blocks {pos-key "furnace"} :furnaces {pos-key {:input {:name "beef" :count 2}}}
                                      :inventory [{:name "raw_iron" :count 3} {:name "coal" :count 2}]} "raw_iron" "furnace-busy"]])

(deftest gives-up-with-a-reason-and_touches_nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (loop [rows gave-up-rows]
          (when-let [[label world item reason] (first rows)]
            (let [{:keys [eng p seen]} (setup world)
                  before (inv-of p)]
              (core/submit! eng (list 'jobs.items.smelt {:furnace {:x 1 :y 64 :z 0} :item item :count 3}) {})
              (await (ticks eng 6))
              (is (= reason (:reason (first (of-kind seen :smelt.gave-up)))) label)
              (is (= 1 (count (of-kind seen :smelt.gave-up))) label)
              (is (empty? (:list (core/state eng))) label)
              (is (not (some #{"load"} (ops p))) label)
              (is (= before (inv-of p)) label)
              (recur (rest rows)))))))))

(deftest the-body-walks-to-a-furnace-out-of-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"20,64,0" "furnace"} :inventory [{:name "raw_iron" :count 3} {:name "coal" :count 2}]})]
          (core/submit! eng '(jobs.items.smelt {:furnace {:x 20 :y 64 :z 0} :item "raw_iron" :count 3}) {})
          (await (ticks eng 2))
          (is (seq (tu/walk-calls p)))
          (is (= {"raw_iron" 0 "coal" 1} (select-keys (merge {"raw_iron" 0} (inv-of p)) ["raw_iron" "coal"])))
          (is (= 3 (:count (:input (get (:furnaces @(fake/state p)) [20 64 0]))))))))))

(deftest a-full-inventory-at-take-time-gives-up-and-leaves-the-output
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "item_" i) :count 1}) (range 35))
              [p seen eng] (await (smelt-to-the-end "furnace" (into filler [{:name "raw_iron" :count 1} {:name "coal" :count 2}]) "raw_iron" 1))]
          (is (= "inventory-full" (:reason (first (of-kind seen :smelt.gave-up)))))
          (is (= {:name "iron_ingot" :count 1} (:output (furnace-of p))) "the ingot is still in the furnace")
          (is (empty? (:list (core/state eng)))))))))

(deftest a-restart-mid-cook-still-collects-the-output
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make p clock]} (setup base-world)
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (core/shutdown! eng)
              again (make)]
          (advance! p 600)
          (swap! clock + 40000)
          (await (ticks again 3))
          (is (= 3 (get (inv-of p) "iron_ingot")))
          (is (empty? (:list (core/state again)))))))))

(deftest a-load-cut-before-its-result-was-kept-does-not-load-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup base-world)
              id (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (mem/update-job! (:store eng) id [] #(dissoc % :owed :ready-at :unlit-from))
              _ (swap! clock + 1000)
              _ (await (ticks eng 1))]
          (is (= {"raw_iron" 2 "coal" 2} (inv-of p)) "nothing more was put in")
          (is (= {:name "raw_iron" :count 3} (:input (furnace-of p))))
          (advance! p 600)
          (swap! clock + 40000)
          (await (ticks eng 3))
          (is (= 3 (get (inv-of p) "iron_ingot"))))))))

(deftest the-furnace-broken-mid-cook-ends-the-job-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup base-world)
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (fake/remove-block! p (fake/parse-cell pos-key))
              _ (swap! clock + 40000)
              _ (await (ticks eng 5))
              n (count (calls p "furnace"))]
          (is (= "furnace-gone" (:reason (first (of-kind seen :smelt.gave-up)))))
          (is (empty? (:list (core/state eng))))
          (await (ticks eng 5))
          (is (= n (count (calls p "furnace"))) "no loop"))))))

(defn put-out!
  "The fire in the fake furnace at pos-key goes out and its fuel slot is empty."
  [p]
  (let [cell (fake/parse-cell pos-key)]
    (swap! (fake/state p) #(-> %
                               (update-in [:furnaces cell] assoc :burn 0 :fuel nil)
                               (assoc-in [:states cell] {:lit false})))))

(deftest an-out-fire-is-fed-again-from-the-pockets
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock seen]} (setup base-world)
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (advance! p 200)
              _ (put-out! p)
              _ (swap! clock + 40000)
              _ (await (ticks eng 1))
              _ (swap! clock + 7000)
              _ (await (ticks eng 1))]
          (is (= 1 (get (inv-of p) "iron_ingot")))
          (is (= 1 (count (:list (core/state eng)))))
          (is (= {:name "raw_iron" :count 2} (:input (furnace-of p))))
          (is (= 1 (get (inv-of p) "coal")) "one more coal went in")
          (advance! p 400)
          (swap! clock + 40000)
          (await (ticks eng 3))
          (is (= 3 (get (inv-of p) "iron_ingot")))
          (is (empty? (of-kind seen :smelt.gave-up))))))))

(deftest an-out-fire-with-no-fuel-left-gives-the-raw-input-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock seen]} (setup {:blocks {pos-key "furnace"} :inventory [{:name "raw_iron" :count 3} {:name "coal" :count 1}]})
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (advance! p 200)
              _ (put-out! p)
              _ (swap! clock + 40000)
              _ (await (ticks eng 1))
              _ (swap! clock + 7000)
              _ (await (ticks eng 2))
              gave-up (first (of-kind seen :smelt.gave-up))]
          (is (= "out-of-fuel" (:reason gave-up)))
          (is (= {"iron_ingot" 1 "raw_iron" 2} (inv-of p)))
          (is (empty? (:list (core/state eng)))))))))

(deftest an-input-the-furnace-never-lights-for-is-given-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock seen]} (setup {:blocks {pos-key "furnace"} :inventory [{:name "clay" :count 3} {:name "coal" :count 2}]})
              _ (core/submit! eng '(jobs.items.smelt {:furnace {:x 1 :y 64 :z 0} :item "clay" :count 3}) {})
              _ (await (ticks eng 1))
              _ (swap! clock + 7000)
              _ (await (ticks eng 2))]
          (is (= "not-smeltable" (:reason (first (of-kind seen :smelt.gave-up)))))
          (is (= 3 (get (inv-of p) "clay")))
          (is (empty? (:list (core/state eng)))))))))

(deftest output-already-in-the-furnace-is-taken-first-and-not-counted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup (assoc base-world :furnaces {pos-key {:output {:name "iron_ingot" :count 2}}}))
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))]
          (is (= {"raw_iron" 2 "coal" 2 "iron_ingot" 2} (inv-of p)) "the two ingots lying there came first")
          (is (= ["read" "take" "load"] (ops p)))
          (advance! p 600)
          (swap! clock + 40000)
          (await (ticks eng 3))
          (is (= 5 (get (inv-of p) "iron_ingot")))
          (is (= 3 (:smelted (first (of-kind seen :smelt.done))))))))))

(deftest a-furnace-emptied-by_someone_else_is_reported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup base-world)
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (swap! (fake/state p) assoc-in [:furnaces (fake/parse-cell pos-key) :input] nil)
              _ (swap! clock + 40000)
              _ (await (ticks eng 2))]
          (is (= "output-gone" (:reason (first (of-kind seen :smelt.gave-up)))))
          (is (empty? (:list (core/state eng)))))))))

(deftest a-furnace-replaced-by-another-block-mid-cook-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup base-world)
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 1))
              _ (fake/set-block! p (fake/parse-cell pos-key) "chest")
              _ (swap! clock + 40000)
              _ (await (ticks eng 2))]
          (is (= "furnace-gone" (:reason (first (of-kind seen :smelt.gave-up))))))))))

(deftest an-unreachable-furnace-is-given-up-after-three-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks {"20,64,0" "furnace"} :unreachable ["20,64,0"]
                                            :inventory [{:name "raw_iron" :count 3} {:name "coal" :count 2}]})]
          (core/submit! eng '(jobs.items.smelt {:furnace {:x 20 :y 64 :z 0} :item "raw_iron" :count 3}) {})
          (await (ticks eng 2))
          (is (empty? (of-kind seen :smelt.gave-up)) "two tries do not end it")
          (await (ticks eng 3))
          (is (= "unreachable" (:reason (first (of-kind seen :smelt.gave-up)))))
          (is (empty? (:list (core/state eng))))
          (is (empty? (calls p "furnace"))))))))

(deftest output-in-the-furnace-that-does-not-fit-the-pockets-stops-before-loading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "item_" i) :count 1}) (range 34))
              {:keys [eng p seen]} (setup (assoc base-world
                                                 :furnaces {pos-key {:output {:name "iron_ingot" :count 2}}}
                                                 :inventory (into filler [{:name "raw_iron" :count 5} {:name "coal" :count 3}])))
              _ (core/submit! eng iron-job {})
              _ (await (ticks eng 2))]
          (is (= "inventory-full" (:reason (first (of-kind seen :smelt.gave-up)))))
          (is (not (some #{"load"} (ops p))) "nothing was loaded")
          (is (= {:name "iron_ingot" :count 2} (:output (furnace-of p)))))))))

(defn forcing
  "Make the fake's furnace answer op with result; the other ops run as they are."
  [p op result]
  (.override (.-world p) "furnace" (fn [token a impl] (if (= op (.-op a)) (clj->js result) (impl token a)))))

(deftest what-the-furnace-answers-is-named-in-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (loop [rows [["read" {:status "timeout"} "furnace timeout"]
                     ["load" {:status "busy" :slot "input" :holds {:name "beef" :count 1}} "furnace-busy"]
                     ["load" {:status "busy" :slot "fuel" :holds {:name "stick" :count 1}} "fuel-busy"]
                     ["load" {:status "rejected" :slot "fuel" :item "coal" :reason "not-accepted"} "rejected-fuel"]
                     ["load" {:status "rejected" :slot "input" :item "raw_iron" :reason "slot-full"} "furnace-full"]
                     ["load" {:status "no-item" :slot "input" :item "raw_iron"} "no-item"]
                     ["load" {:status "weird"} "load weird"]]]
          (when-let [[op result reason] (first rows)]
            (let [{:keys [eng p seen]} (setup base-world)]
              (forcing p op result)
              (core/submit! eng iron-job {})
              (await (ticks eng 3))
              (is (= reason (:reason (first (of-kind seen :smelt.gave-up)))) reason)
              (is (empty? (:list (core/state eng))) reason)
              (recur (rest rows)))))))))

(defn ^:async result-of
  "Run the job (spec args) as the child of a recording parent, cooking and advancing the clock between ticks, until the list is empty; the child's result."
  [world args]
  (let [{:keys [eng p clock]} (setup world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid 'jobs.items.smelt args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 8) (seq (:list (core/state eng))))
        (await (core/tick! eng))
        (advance! p 700)
        (swap! clock + 40000)
        (recur (inc i))))
    @out))

(deftest the-result-hands-over-what-was-smelted-or-why-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:smelted 3 :wanted 3} (await (result-of base-world {:furnace {:x 1 :y 64 :z 0} :item "raw_iron" :count 3}))))
        (is (= {:smelted 0 :wanted 0 :reason "no-furnace"}
               (await (result-of (dissoc base-world :blocks) {:furnace {:x 1 :y 64 :z 0} :item "raw_iron" :count 3}))))))))

;; ------------------------------------------------------------------ no :furnace: the nearest one seen
;; The fake body stands at 0,64,0 facing south (+z): what lies ahead is seen.

(defn seeing-setup
  "setup over primitives that see through the body's perception (no x-ray)."
  [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor world)
        p (perception/wrap raw (perception/create (fake-raw/create raw) {:radius 20 :ray-deg 1}))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async seeing-ticks [{:keys [eng p]} n]
  (dotimes [_ n]
    (perception/pass! (aget p "perception"))
    (await (core/tick! eng))))

(def iron-and-coal [{:name "raw_iron" :count 5} {:name "coal" :count 3}])

(deftest without-a-furnace-it-uses-the-nearest-seen-one-that-cooks-the-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (seeing-setup {:blocks {"1,64,2" "smoker" "-1,64,4" "furnace" "0,64,7" "furnace"}
                                                         :inventory iron-and-coal})]
          (core/submit! eng '(jobs.items.smelt {:item "raw_iron" :count 3}) {})
          (await (seeing-ticks s 3))
          (is (= {:name "raw_iron" :count 3} (:input (get (:furnaces @(fake/state p)) [-1 64 4])))
              "the nearer furnace; the nearer smoker cannot cook iron")
          (is (nil? (:input (get (:furnaces @(fake/state p)) [0 64 7]))))
          (is (= {:x -1 :y 64 :z 4} (:furnace (first (of-kind seen :smelt.furnace))))
              "the chosen furnace is told")
          (is (= [:cooking] (mapv :reason (of-kind seen :waiting))) "while it cooks the job says why it waits"))))))

(deftest without-a-furnace-and-none-seen-it-ends-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (seeing-setup {:inventory iron-and-coal})
              before (inv-of p)]
          (core/submit! eng '(jobs.items.smelt {:item "raw_iron" :count 3}) {})
          (await (seeing-ticks s 3))
          (is (= ["no-furnace-seen"] (mapv :reason (of-kind seen :smelt.gave-up))))
          (is (empty? (:list (core/state eng))))
          (is (= before (inv-of p))))))))
