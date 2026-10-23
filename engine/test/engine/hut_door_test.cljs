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
            [engine.test-util :as tu :refer [box]]
            [engine.settings :as settings]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.seal :as seal]
            [jobs.survival.respond.doors :as doors]
            [jobs.survival.shut-in :as shut-in]
            [jobs.survival.dig-in-cells :as dig-cells]))

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
        (let [{:keys [eng p]} (await (ct/first-round ct/retreat (open-hut {:inventory [{:name "cobblestone" :count 20}]})))]
          (is (false? (hs/door-open? p)) "the door is shut on the skeleton")
          (is (= 1 (count (filter #(door-cell? (.. % -args -pos)) (ct/calls p "useOn")))) "one click on the door")
          (is (empty? (places-at-door p)) "no block placed into the doorway")
          (is (= [] (:list (core/state eng))) "the shut door stops the arrows: the flight is over")
          (is (= {} (hs/room-blocks p)) "no block left in the hut"))))))

(deftest a-hurt-body-in-its-hut-shuts-the-door-rather-than-fight-or-wall-itself-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (ct/first-round '(jobs.survival.respond-to-hostile)
                                                (open-hut {:self {:pos {:x 5.5 :y 64 :z 1.5} :health 4.8 :food 13}
                                                           :inventory [{:name "cobbled_deepslate" :count 20}]})))]
          (is (false? (hs/door-open? p)))
          (is (empty? (ct/calls p "attack")))
          (is (= [] (:list (core/state eng))) "no danger left once the door is shut: done in the one round")
          (is (empty? (ct/calls p "place")))
          (is (= {} (hs/room-blocks p))))))))

(deftest a-lone-zombie-outside-gets-the-door-shut-not-a-refuge-built
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (hs/hut-world {:x 5.5 :y 64 :z 1.5} (tu/box 3 63 -2 7 63 2 "cobblestone"))
                           :states open-door-states :inventory [{:name "cobblestone" :count 20}]
                           :entities [{:id 9 :name "zombie" :kind "hostile" :pos {:x 5.5 :y 64 :z 8.5} :health 20 :visible true}])
              {:keys [p]} (await (ct/first-round ct/retreat world))]
          (is (false? (hs/door-open? p)) "the door is shut on the zombie")
          (is (empty? (ct/calls p "place")) "no seal")
          (is (empty? (ct/calls p "jumpPlace")) "no pillar"))))))

(deftest a-flight-shuts-a-door-only-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (open-hut {:inventory [{:name "cobblestone" :count 20}]})
              reopen! (fn [p]
                        (swap! (fake/state p) assoc-in [:states [5 64 2] :open] true)
                        (swap! (fake/state p) assoc-in [:states [5 65 2] :open] true))
              {:keys [seen]} (await (ct/hidden-round ct/retreat world 1000 (constantly nil) []
                                                     (fn [p]
                                                       (let [n (atom 0)]
                                                         (.override (.-world p) "useOn"
                                                                    (fn [token a impl]
                                                                      (-> (impl token a)
                                                                          (.then (fn [r] (when (= 1 (swap! n inc)) (reopen! p)) r)))))))))]
          (is (= 1 (count (filter #(= :retreat.door-shut (:kind %)) @seen)))
              "opened again (the door does not stay shut): the flight does not click it again but goes on"))))))

(deftest sealed-means-shut-for-doors-gates-and-trapdoors
  (let [p (tu/fake {:blocks {"1,64,0" "oak_door" "2,64,0" "oak_fence_gate" "3,64,0" "oak_fence" "4,64,0" "stone"}
                    :states {"1,64,0" {:open true :half "lower"} "2,64,0" {:open false}}})]
    (are [cell sealed] (= sealed (seal/sealed? p cell))
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
        (let [{:keys [p]} (await (ct/first-round ct/retreat {:blocks torch-tunnel
                                                           :inventory [{:name "cobblestone" :count 20}]
                                                           :entities [(ct/skeleton 3 64 0)]
                                                           :zones [{:name "keep-out" :min [1 64 0] :max [1 65 0]}]}))]
          (is (<= (count (ct/calls p "place")) 4) "each torch cell is tried twice, then left alone")
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
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (false? (:open (get-in @(fake/state p) [:states [1 64 0]]))) "the door is shut")
          (is (= "oak_door" (.-name (.blockAt p (tu/pos 1 64 0)))) "not dug")
          (is (empty? (filterv #(= 1 (.. % -args -pos -x)) (ct/calls p "dig")))))))))

(defn zombie-at [id z] {:id id :name "zombie" :kind "hostile" :pos {:x 5.5 :y 64 :z z} :health 20 :visible true})

(defn ^:async door-price
  "The :door price respond logged at its first decision for the hurt body in the open hut (with extra blocks) and the skeleton south of it, nil when no door cover was priced."
  ([extra] (door-price extra {}))
  ([extra world]
  (let [{:keys [seen]} (await (ct/first-round '(jobs.survival.respond-to-hostile)
                                              (merge (open-hut (merge {:self {:pos {:x 5.5 :y 64 :z 1.5} :health 4.8 :food 13}
                                                          :inventory [{:name "cobbled_deepslate" :count 20}]} world))
                                                    {:blocks (merge (box 3 63 -2 7 63 2 "stone") (:blocks (hs/hut-world {:x 5.5 :y 64 :z 1.5} extra)))})))]
    (-> (filter #(= :hostile.decided (:kind %)) @seen) first :prices :door))))

(deftest an-unseen-room-cell-leaves-the-door-a-priced-cover
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (some? (await (door-price {} {:unloaded #{[5 64 -2] [5 65 -2]}}))) "a room cell not seen: the door cover is priced, not dropped")))))

(deftest the-door-is-a-cover-only-where-the-room-has-no-other-opening
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (some? (await (door-price {}))) "a closed hut: the shut door is a priced cover")
        (is (nil? (await (door-price {"7,64,0" "air" "7,65,0" "air"}))) "a gap in the wall beside the door: shutting it seals nothing")))))

(deftest a-door-a-mob-may-break-holds-only-part-of-the-quiet-by-the-chance
  (let [c {:args {}}
        open-s (fn [difficulty mobs unseen?]
                 (settings/with-settings {:jobs.lib.cost.threat/difficulty difficulty}
                   #(:open-s (shut-in/door-cover c 1 mobs unseen?))))
        none (open-s 1 ["zombie"] false)]
    (is (< none (open-s 3 ["zombie"] false) (open-s 3 ["vindicator"] false)) "hard: a zombie breaks by its share, a vindicator always")
    (is (= none (open-s 3 ["drowned"] false) (open-s 3 ["skeleton"] false) (open-s 2 ["zombie"] false)) "no breaker: a drowned never, a zombie below hard")
    (is (= (open-s 3 ["vindicator"] false) (open-s 2 ["vindicator"] false)) "a vindicator breaks from normal on")
    (is (< (open-s 3 ["zombie"] false) (open-s 3 ["zombie" "zombie"] false)) "two zombies: the chance one breaks is greater")
    (is (< (open-s 3 ["vindicator"] false) (open-s 1 ["skeleton"] true)) "an unseen side: all the quiet is in the open")))

(deftest the-cover-and-the-flight-pick-the-door-of-the-same-threat
  (let [world (assoc (hs/hut-world {:x 5.5 :y 64 :z 1.5} {"5,64,-2" "oak_door" "5,65,-2" "oak_door"})
                     :states (merge open-door-states {"5,64,-2" {:open true :half "lower" :facing "north"} "5,65,-2" {:open true :half "upper" :facing "north"}})
                     :entities [(zombie-at 9 5.5) (zombie-at 10 -9.5)])
        p (tu/fake world)
        c {:primitives p :view (fn [] {:data {} :now 0}) :args {}}
        threats (vec (danger-q/known-hostiles p 40 {}))
        cell #(some-> (doors/door-for c %) :cell)]
    (is (some? (cell threats)))
    (is (= (cell threats) (cell (vec (reverse threats)))) "whatever the order the threats come in")))
