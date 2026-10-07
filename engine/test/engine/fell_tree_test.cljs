(ns engine.fell-tree-test
  "jobs.forestry.fell-tree from a pillar: a trunk too tall to reach from the ground is felled from a pillar that is
  taken back (jobs.access.pillar, jobs.access.cleanup) before the job ends."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.forest-maintain-test :as fm]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
            [engine.test-util :as tu]
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
        out (await (h/child-outcome (:eng s) 'jobs.forestry.fell-tree args n))]
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
