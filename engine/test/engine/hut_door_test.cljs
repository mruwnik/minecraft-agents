(ns engine.hut-door-test
  "A body in its own hut whose door stands open, with a skeleton outside (game-agent bug #69: a skeleton walked into
  the open doorway and shot the body dead while the flee kept placing blocks into its chest, bed and torch cells).
  The flight shuts the open door between it and the hostile; a seal never counts an open door as a wall and never
  places into cells a block it leaves alone already holds, round after round; dig-in shuts an open door beside the
  body instead of digging it."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.cornered-test :as ct]
            [engine.fake :as fake]
            [engine.hut-shelter-test :as hs]
            [engine.test-util :as tu]
            [jobs.survival.dig-in :as dig-in]))

(def open-door-states
  {"5,64,2" {:open true :half "lower" :facing "south"} "5,65,2" {:open true :half "upper" :facing "south"}})

(defn open-hut
  "The hut of hut-shelter-test with its door open, the body in the cell inside the door, a skeleton south of it in
  line with the doorway."
  [more]
  (merge (assoc (hs/hut-world {:x 5.5 :y 64 :z 1.5} {}) :states open-door-states)
         {:entities [{:id 9 :name "skeleton" :kind "hostile" :pos {:x 5.5 :y 64 :z 8.5} :health 20 :visible true}]}
         more))

(defn door-cell? [pos] (and (= 5 (.-x pos)) (= 2 (.-z pos))))

(defn places-at-door [p] (filterv #(door-cell? (.. % -args -pos)) (ct/calls p "place")))

(deftest a-fleeing-body-shuts-its-open-hut-door-on-the-skeleton
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (ct/first-round ct/retreat (open-hut {:inventory [{:name "cobblestone" :count 20}]})))]
          (is (false? (hs/door-open? p)) "the door is shut on the skeleton")
          (is (= 1 (count (filter #(door-cell? (.. % -args -pos)) (ct/calls p "useOn")))) "one click on the door")
          (is (empty? (places-at-door p)) "no block placed into the doorway")
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "the shut door stops the arrows: the flight is over")
          (is (= {} (hs/room-blocks p)) "no block left in the hut"))))))

(deftest a-hurt-body-in-its-hut-shuts-the-door-rather-than-fight-or-wall-itself-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock seen]} (await (ct/first-round '(jobs.survival.respond-to-hostile)
                                                           (open-hut {:self {:pos {:x 5.5 :y 64 :z 1.5} :health 4.8 :food 13}
                                                                      :inventory [{:name "cobbled_deepslate" :count 20}]})))]
          (is (false? (hs/door-open? p)))
          (is (empty? (ct/calls p "attack")))
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= {:kind :waiting :reason :not-ready} (select-keys (last @seen) [:kind :reason]))
              "no danger left once the door is shut: its check no longer holds (as a reflex, the trigger clears)")
          (is (empty? (ct/calls p "place")))
          (is (= {} (hs/room-blocks p))))))))

(deftest a-flight-shuts-a-door-only-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock seen]} (await (ct/first-round ct/retreat (open-hut {:inventory [{:name "cobblestone" :count 20}]})))]
          (swap! (fake/state p) assoc-in [:states [5 64 2] :open] true)
          (swap! (fake/state p) assoc-in [:states [5 65 2] :open] true)
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 1 (count (filter #(= :retreat.door-shut (:kind %)) @seen)))
              "opened again (the door does not stay shut): the flight does not click it again but goes on")
          (is (= ["j1"] (:list (core/state eng)))))))))

(deftest sealed-means-shut-for-doors-gates-and-trapdoors
  (let [p (tu/fake {:blocks {"1,64,0" "oak_door" "2,64,0" "oak_fence_gate" "3,64,0" "oak_fence" "4,64,0" "stone"}
                    :states {"1,64,0" {:open true :half "lower"} "2,64,0" {:open false}}})]
    (are [cell sealed] (= sealed (dig-in/sealed? p cell))
      {:x 1 :y 64 :z 0} false
      {:x 2 :y 64 :z 0} true
      {:x 3 :y 64 :z 0} true
      {:x 4 :y 64 :z 0} true)))

(def torch-tunnel
  "The cornered-test dead end with a torch at feet and head height in the one open side."
  (merge ct/dead-end {"1,64,0" "torch" "1,65,0" "torch"}))

(deftest a-seal-that-cannot-fill-a-cell-moves-on-instead-of-placing-there-every-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (await (ct/first-round ct/retreat {:blocks torch-tunnel
                                                                       :inventory [{:name "cobblestone" :count 20}]
                                                                       :entities [(ct/skeleton 3 64 0)]}))]
          (dotimes [_ 3]
            (swap! clock + 1000)
            (await (core/tick! eng)))
          (is (<= (count (ct/calls p "place")) 2) "each torch cell is tried once")
          (is (pos? (count (ct/calls p "attack"))) "the seal failed: it fights back"))))))

(deftest dig-in-shuts-an-open-door-beside-the-body-instead-of-digging-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (ct/setup {:time 18000
                                         :self {:pos {:x 0.5 :y 64 :z 0.5}}
                                         :blocks {"1,64,0" "oak_door" "1,65,0" "oak_door"}
                                         :states {"1,64,0" {:open true :half "lower" :facing "east"}
                                                  "1,65,0" {:open true :half "upper" :facing "east"}}
                                         :inventory [{:name "cobblestone" :count 20}]})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (false? (:open (get-in @(fake/state p) [:states [1 64 0]]))) "the door is shut")
          (is (= "oak_door" (.-name (.blockAt p (tu/pos 1 64 0)))) "not dug")
          (is (empty? (filterv #(= 1 (.. % -args -pos -x)) (ct/calls p "dig")))))))))
