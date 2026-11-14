(ns engine.build-repair-test
  "jobs.build.repair and its diff (jobs.lib.build.diff) against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.build-from-plan-test :as b]
            [engine.core :as core]
            [engine.harvest-test :as h]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.lib.build.diff :as diff]
            [jobs.lib.cost :as cost]
            [jobs.lib.crops :as crops]))

(def yard
  "A wall with one cell broken in sight and one broken far off, a crop cell over a farmland cell trampled to dirt, an :any cell holding dirt, and a gate the plan wants shut."
  {:id "yard"
   :parts [{:id "wall" :cells [[3 64 2] [4 64 2] [5 64 2]] :want "cobblestone"}
           {:id "far" :cells [[30 64 2]] :want "cobblestone"}
           {:id "field" :cells [[4 64 4]] :want {:crop "wheat"}}
           {:id "soil" :cells [[4 63 4]] :want "farmland"}
           {:id "free" :cells [[6 64 2]] :want :any}
           {:id "gate" :cells [[3 64 6]] :want {:block "oak_fence_gate" :facing "north" :open "false"}}]})

(def yard-world
  {:blocks {"3,64,2" "cobblestone" "5,64,2" "cobblestone" "6,64,2" "dirt" "4,63,4" "dirt" "3,64,6" "oak_fence_gate"}
   :states {"3,64,6" {:open true :facing "north"}}
   :sight 10
   :self {:pos {:x 4.5 :y 64 :z 8.5}}
   :inventory [{:name "cobblestone" :count 4} {:name "wheat_seeds" :count 2}]})

(defn diff-of [eng]
  (let [c (core/make-ctx eng {:root "repair" :slots [] :chain [] :token nil :args {}})]
    (mapv :pos (diff/diff c (:cells (diff/plan-cells c "yard" nil))))))

(deftest only-cells-seen-differ-and-no-one-else-keeps-are-listed
  (let [{:keys [eng]} (b/start yard-world {"yard" yard} [])]
    (is (= [[4 64 2]] (diff-of eng)) "not the wall cell far out of sight, the crop, the trampled farmland (tend tills it), the :any cell, the open gate")))

(deftest a-door-standing-in-a-wall-cell-is-listed-wrong
  (let [{:keys [eng]} (b/start (assoc-in yard-world [:blocks "4,64,2"] "oak_door") {"yard" yard} [])]
    (is (= [[4 64 2]] (diff-of eng)) "the wall wants cobblestone there; the open gate the plan wants is still left alone")))

(deftest a-broken-barrier-beside-a-herd-is-worth-its-gap-loss-more-than-a-plain-wall
  (let [plan {:parts [{:id "pen" :box [[3 64 3] [5 64 5]] :herd {:mob "cow" :target 4}}] :cells []}
        fence (fn [pos] [{:pos pos :want "oak_fence"}])]
    (is (< (js/Math.abs (- (cost/gap-loss "cow" 4 1)
                           (- (diff/worth plan (fence [2 64 4])) (diff/worth plan (fence [9 64 9])))))
           1e-9))))

(deftest a-water-cell-is-worth-the-dry-growth-of-the-crops-it-alone-waters
  (let [water {:pos [5 63 5] :want "water"}
        crop {:pos [6 64 5] :want {:crop "wheat"}}
        dry (- (cost/sown-worth "wheat" (crops/stages-per-s true nil)) (cost/sown-worth "wheat" (crops/stages-per-s false nil)))]
    (is (= (+ (cost/put-back-s "water") dry) (diff/worth {:cells [water crop]} [water])))
    (is (= (cost/put-back-s "water") (diff/worth {:cells [water crop {:pos [7 63 5] :want "water"}]} [water]))
        "another water cell still waters the crop")))

(deftest the-repair-builds-only-the-cell-seen-broken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (b/start yard-world {"yard" yard} [])
              result (await (tu/child-outcome eng 'jobs.build.repair {:plan "yard"} 300))]
          (is (= [[[4 64 2] "cobblestone"]] (b/places p)) "the far gap, unseen, the crop, the :any cell and the open gate are left")
          (is (= "dirt" (b/block p [6 64 2])))
          (is (= "oak_fence_gate" (b/block p [3 64 6])) "the open gate is not dug: shutting it is the gate jobs'")
          (is (< (.-x (.-pos (.self p))) 20) "no walk to look at the far cell")
          (is (= :done (:status result)) (pr-str result)))))))

(deftest an-alternative-want-that-includes-farmland-is-the-farm-jobs-not-repair
  (is (true? (boolean (diff/others-own? [:any "farmland" "dirt"]))))
  (is (not (diff/others-own? [:any "stone" "dirt"]))))

(def lit-yard
  "A post with a torch on it and a lantern hanging under a beam, both planned and both gone."
  {:id "lit"
   :parts [{:id "post" :cells [[3 64 2]] :want "cobblestone"}
           {:id "beam" :cells [[5 66 2]] :want "cobblestone"}
           {:id "lights" :cells [[3 65 2]] :want "torch"}
           {:id "hung" :cells [[5 65 2]] :want {:block "lantern" :hanging "true"}}]})

(def lit-world
  {:blocks {"3,64,2" "cobblestone" "5,66,2" "cobblestone"}
   :sight 10
   :self {:pos {:x 4.5 :y 64 :z 6.5}}
   :inventory [{:name "torch" :count 2} {:name "lantern" :count 1}]})

(deftest a-missing-torch-and-lantern-of-a-plan-are-listed-and-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (b/start lit-world {"lit" lit-yard} [])
              c (core/make-ctx eng {:root "repair" :slots [] :chain [] :token nil :args {}})
              listed (set (map :pos (diff/diff c (:cells (diff/plan-cells c "lit" nil)))))
              result (await (tu/child-outcome eng 'jobs.build.repair {:plan "lit"} 300))]
          (is (= #{[3 65 2] [5 65 2]} listed))
          (is (= "torch" (b/block p [3 65 2])))
          (is (= "lantern" (b/block p [5 65 2])))
          (is (= :done (:status result)) (pr-str result)))))))

(deftest a-missing-light-is-worth-the-spawns-it-averts-so-the-repair-mends-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (update lit-yard :parts #(filterv (comp #{"post" "lights"} :id) %))
              {:keys [eng p]} (b/start (assoc lit-world :time st/night) {"lit" plan} [])
              c (core/make-ctx eng {:root "repair" :slots [] :chain [] :token nil :args {}})
              pc (diff/plan-cells c "lit" nil)
              worth (diff/worth pc (diff/diff c (:cells pc)))
              result (await (tu/child-outcome eng 'jobs.build.repair {:plan "lit" :worth worth} 300))]
          (is (< (cost/put-back-s "torch") worth))
          (is (= :done (:status result)) (pr-str result))
          (is (= "torch" (b/block p [3 65 2]))))))))
