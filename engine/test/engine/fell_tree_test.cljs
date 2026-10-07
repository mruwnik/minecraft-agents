(ns engine.fell-tree-test
  "jobs.forestry.fell-tree from a pillar: a trunk too tall to reach from the ground is felled from a pillar that is
  taken back (jobs.access.pillar, jobs.access.cleanup) before the job ends."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.fake :as fake]
            [engine.forest-maintain-test :as fm]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
            [engine.registry :as registry]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.ledger :as ledger]
            [engine.memory :as mem]))

(def tall {:blocks (lt/tree 6 0 "oak" 8) :inventory [{:name "dirt" :count 20}]})

(defn dug-ys
  "The heights of the digs in the trunk's column."
  [p]
  (into [] (comp (filter #(= 6 (first %))) (map second)) (fm/digs p)))

(defn pillar-cells
  "The cells of dirt standing in the fake world."
  [p]
  (vec (keep (fn [[k v]] (when (= "dirt" v) k)) (:blocks @(fake/state p)))))

(def eye-height 1.62)

(defn eye-reach!
  "The fake measures a dig from the feet to a cell corner; the real body from the eye to the centre. Digs here run with
  the body lifted to its eye height, as the planner counts reach."
  [p]
  (.override (.-world p) "dig"
             (fn ^:async f [token args impl]
               (let [st (fake/state p)]
                 (swap! st update-in [:self :pos 1] + eye-height)
                 (let [r (await (impl token args))]
                   (swap! st update-in [:self :pos 1] - eye-height)
                   r)))))

(defn ^:async fell [spec args n]
  (let [s (fm/start spec {})
        _ (eye-reach! (:p s))
        out (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree args n))]
    (assoc s :out out)))

(deftest a-tall-trunk-is-felled-from-a-pillar-and-the-pillar-is-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out]} (await (fell tall {:radius 10} 200))]
          (is (= {:base {:x 6 :y 64 :z 0}} out))
          (is (= (set (range 64 72)) (set (dug-ys p))) "every log dug")
          (is (pos? (count (h/calls p "jumpPlace"))) "a pillar was built")
          (is (= [] (ledger/open-entries (mem/view (:store eng)))) "ledger empty")
          (is (= [] (pillar-cells p)) "no pillar block left"))))))

(deftest logs-above-the-ground-reach-are-dug-top-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (fell tall {:radius 10} 200))
              ys (dug-ys p)]
          (is (= 8 (count ys)))
          (is (apply < (take 5 ys)) "the low logs go bottom-up from the ground")
          (is (apply > (drop 5 ys)) "the high ones top-down from the pillar"))))))

(deftest no-blocks-to-pillar-with-waits-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (fell (assoc tall :inventory []) {:radius 10} 60))]
          (is (= :not-done out))
          (is (= [] (h/calls p "jumpPlace")))
          (is (= [] (pillar-cells p))))))))

(deftest a-tree-in-a-zone-that-bars-placing-is-declined-without-a-pillar
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "grove" :min [-6 64 -9] :max [16 80 9] :allow #{:dig}}]
              {:keys [p]} (await (fm/with-zones zones #(fell tall {:radius 10} 80)))]
          (is (= [] (h/calls p "jumpPlace")))
          (is (< (count (dug-ys p)) 8) "the high logs stay"))))))

(defn kinds [{:keys [seen]} kind] (h/events-of seen kind))

(deftest a-cleanup-that-leaves-the-pillar-open-ends-the-job-with-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cleanup (assoc (get registry/jobs 'jobs.access.cleanup) :round (fn ^:async f [_] :done))
              s (with-redefs [registry/jobs (assoc registry/jobs 'jobs.access.cleanup cleanup)]
                  (await (fell tall {:radius 10} 200)))]
          (is (not= :not-done (:out s)) "the job ends instead of cleaning for ever")
          (is (= 1 (count (kinds s :tree_blocked)))))))))

(deftest a-pillar-with-no-log-in-reach-marks-the-tree-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              s (with-redefs [approach/plan (fn [_] (swap! calls inc) {:stand [[5 64 0]]})]
                  (await (fell tall {:radius 10} 200)))]
          (is (not= :not-done (:out s)))
          (is (<= @calls 2) "the plan is not made again and again")
          (is (= 1 (count (kinds s :tree_blocked)))))))))

(deftest a-planner-refusal-other-than-a-zone-warns-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (with-redefs [approach/plan (fn [_] {:reason :no-base})]
                  (await (fell tall {:radius 10} 120)))
              w (first (kinds s :fell-tree.declined))]
          (is (= :no-base (:reason w)))
          (is (= 1 (count (kinds s :fell-tree.declined)))))))))

(def chest-at "-2,64,3")

(defn chest-world [stock]
  (assoc tall :inventory [] :blocks (assoc (:blocks tall) chest-at "chest") :containers {chest-at stock}))

(defn dirt-in-chest [{:keys [p]}]
  (some #(when (= "dirt" (:name %)) (:count %)) (get-in @(fake/state p) [:containers [-2 64 3]])))

(deftest a-fetch-plan-for-a-need-asks-for-the-count
  (let [o {:what #{:item} :how #{:chest} :depth 2}]
    (is (= {:any-of ["dirt" "cobblestone"] :count 4} (:args (fetch/plan-for {:reason :need :any-of ["dirt" "cobblestone"] :count 4} o))))
    (is (= {:item "dirt" :count 3} (:args (fetch/plan-for {:reason :need :item "dirt" :count 3} o))))))

(deftest too-few-blocks-for-the-pillar-are-fetched-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (chest-world [{:name "dirt" :count 20}]) {:radius 10} 300))
              {:keys [p out]} s]
          (is (= {:base {:x 6 :y 64 :z 0}} out))
          (is (= (set (range 64 72)) (set (dug-ys p))) "every log dug")
          (is (> (- 20 (dirt-in-chest s)) 1) "the chest gave the pillar's height, not one block")
          (is (= [] (pillar-cells p)) "no pillar block left"))))))

(deftest fetch-false-waits-for-the-blocks-and-leaves-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (fell (chest-world [{:name "dirt" :count 20}]) {:radius 10 :fetch false} 80))]
          (is (= :not-done (:out s)))
          (is (= 20 (dirt-in-chest s))))))))

(deftest a-log-over-hidden-lava-is-dug-and-the-lava-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start {:blocks (assoc (lt/tree 6 0 "oak" 3) "6,63,0" "lava")
                           :inventory [{:name "cobblestone" :count 2}]} {})
              p (:p s)]
          (bd/hiding p #(= [6 63 0] %))
          (await (tu/child-outcome (:eng s) 'jobs.forestry.fell-tree {:radius 10} 200))
          (is (= "air" (h/block-at p 6 64 0)) "the bottom log is dug")
          (is (= "cobblestone" (h/block-at p 6 63 0)) "the lava under it is sealed"))))))
