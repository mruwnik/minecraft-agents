(ns engine.make-room-test
  "jobs.storage.make-room, the reflex of inventory-nearly-full, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.storage.make-room :as mr]))

(def t0 1000000)

(defn setup [world]
  (let [clock (atom t0)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn arg-of [k call] (aget (.-args call) k))

(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))

(defn stack-count [p] (.-length (.-inventory (.self p))))

(defn many
  "n stacks named prefix_0 ... of count 1, or of count c."
  ([prefix n] (many prefix n 1))
  ([prefix n c] (mapv (fn [i] {:name (str prefix "_" i) :count c}) (range n))))

(defn same [name n c] (vec (repeat n {:name name :count c})))

(defn ^:async run-reflex
  "Register make-room as the nearly-full reflex (job args given, trigger args
  given) and tick until its instance is gone, at most 60 ticks."
  [eng job-args trigger-args]
  (core/register-reflex! eng {:trigger :inventory-nearly-full :args trigger-args
                              :job (list 'jobs.storage.make-room job-args)})
  (loop [i 0]
    (await (core/tick! eng))
    (when (and (< i 60) (seq (:instances (core/state eng))))
      (recur (inc i)))))

(defn declined? [seen] (boolean (some #(= [:reflex :declined] [(:source %) (:kind %)]) @seen)))

(defn event-kinds [seen] (set (map :kind (filter #(= :job (:source %)) @seen))))

(defn chest-entries [eng] (mem/entries (mem/view (:store eng)) :chest-unusable))

(defn know-chest! [eng pos] (mem/write! (:store eng) :chest {:pos pos} mem/place-policy))

;; ------------------------------------------------------------------ pure

(deftest protected-names
  (are [name expected] (= expected (mr/protected? name))
    "iron_pickaxe" true
    "diamond_sword" true
    "iron_chestplate" true
    "shield" true
    "bucket" true
    "water_bucket" true
    "lava_bucket" true
    "bread" false
    "cobblestone" false
    "oak_log" false))

(deftest keep-counts-protect-tools-and-share-the-food-and-block-budgets
  (let [inventory [{:name "iron_pickaxe" :count 1}
                   {:name "water_bucket" :count 1}
                   {:name "bread" :count 10}
                   {:name "cooked_beef" :count 12}
                   {:name "apple" :count 5}
                   {:name "cobblestone" :count 64}
                   {:name "cobblestone" :count 64}
                   {:name "dirt" :count 30}
                   {:name "stick" :count 7}]]
    (is (= {"iron_pickaxe" 1 "water_bucket" 1
            "cooked_beef" 12 "bread" 4 "apple" 0
            "dirt" 30 "cobblestone" 34
            "stick" 0}
           (mr/keep-counts inventory {:keep-food 16 :keep-blocks 64}))
        "beef (8 points) keeps its 12, bread (5) the 4 left of 16, apple none; dirt then cobblestone share 64")))

(deftest keep-counts-with-zero-budgets-keep-only-the-protected
  (is (= {"iron_axe" 1 "bread" 0 "dirt" 0 "stick" 0}
         (mr/keep-counts [{:name "iron_axe" :count 1} {:name "bread" :count 3} {:name "dirt" :count 9} {:name "stick" :count 2}]
                         {:keep-food 0 :keep-blocks 0}))))

(deftest recency-is-the-newest-picked-up-time-per-item
  (let [view {:now t0 :data {:entries {:picked-up [{:t 5 :data {:item "stick" :count 1}}
                                                   {:t 9 :data {:item "dirt" :count 1}}
                                                   {:t 7 :data {:item "stick" :count 2}}]}}}]
    (is (= {"stick" 7 "dirt" 9} (mr/recency view)))
    (is (= {} (mr/recency {:now t0 :data {}})))))

(defn names-of [order] (mapv :name order))

(deftest toss-order-sorts-by-worth-then-recency-then-count-then-slot
  (let [inventory [{:name "oak_log" :count 4 :slot 0}
                   {:name "dirt" :count 40 :slot 1}
                   {:name "gravel" :count 9 :slot 2}
                   {:name "stick" :count 3 :slot 3}
                   {:name "flint" :count 3 :slot 4}]
        order (fn [recency] (names-of (mr/toss-order inventory {} recency 5)))]
    (is (= ["stick" "flint" "gravel" "dirt" "oak_log"] (order {})) "worth 0 first; the same worth by count (3, 3 by slot, 9, 40)")
    (is (= ["flint" "gravel" "dirt" "stick" "oak_log"] (order {"stick" 100})) "the one picked up most recently goes last of its worth")
    (is (= ["stick" "gravel" "dirt" "flint" "oak_log"] (order {"flint" 100 "gravel" 50 "dirt" 50}))
        "stick has no pick-up (0): before the picked-up names; later pick-ups after earlier ones")))

(deftest toss-order-orders-by-recency-before-count
  (let [inventory [{:name "gravel" :count 9 :slot 0} {:name "flint" :count 3 :slot 1}]]
    (is (= ["gravel" "flint"] (names-of (mr/toss-order inventory {} {"gravel" 1 "flint" 2} 5))))
    (is (= ["flint" "gravel"] (names-of (mr/toss-order inventory {} {"gravel" 2 "flint" 1} 5))))))

(deftest toss-order-skips-protected-food-and-stacks-worth-max-or-more
  (let [inventory [{:name "iron_pickaxe" :count 1 :slot 0}
                   {:name "bread" :count 3 :slot 1}
                   {:name "oak_log" :count 4 :slot 2}
                   {:name "diamond" :count 1 :slot 3}
                   {:name "dirt" :count 4 :slot 4}]]
    (is (= ["dirt"] (names-of (mr/toss-order inventory {} {} 1))))
    (is (= ["dirt" "oak_log"] (names-of (mr/toss-order inventory {} {} 2))))
    (is (= [] (names-of (mr/toss-order inventory {} {} 0))))
    (is (= ["dirt" "oak_log" "diamond"] (names-of (mr/toss-order inventory {} {} 100))))))

(deftest toss-order-never-cuts-into-a-floor
  (let [stacks (fn [& counts] (vec (map-indexed (fn [i c] {:name "cobblestone" :count c :slot i}) counts)))]
    (are [counts floor expected] (= expected (mapv :slot (mr/toss-order (apply stacks counts) {"cobblestone" floor} {} 1)))
      [64 64 64] 64 [0 1]
      [64 64 64] 0 [0 1 2]
      [64 64 64] 192 []
      [64 64 64] 100 [0]
      [40 30] 64 []
      [40 30] 30 [1]
      [10 64] 64 [0])))

(deftest deposit-names-lists-the-names-above-their-keep
  (let [inventory [{:name "iron_pickaxe" :count 1}
                   {:name "bread" :count 20}
                   {:name "cobblestone" :count 64}
                   {:name "cobblestone" :count 64}
                   {:name "dirt" :count 3}
                   {:name "water_bucket" :count 1}]]
    (is (= ["bread" "cobblestone" "dirt"] (mr/deposit-names inventory {"bread" 16 "cobblestone" 64 "dirt" 0 "iron_pickaxe" 1})))
    (is (= ["dirt"] (mr/deposit-names inventory {"bread" 20 "cobblestone" 128 "dirt" 0})))
    (is (= [] (mr/deposit-names inventory {"bread" 20 "cobblestone" 128 "dirt" 3})))))

;; ------------------------------------------------------------------ the job

(def chest-pos {:x 10 :y 64 :z 0})

(deftest enough-free-slots-is-done-with-no-acting-calls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "item" 10)})]
          (await (run-reflex eng {} {:free 40}))
          (is (= {} (:instances (core/state eng))))
          (is (empty? (.-calls (.-world p)))))))))

(def full-inventory
  (vec (concat [{:name "iron_pickaxe" :count 1} {:name "iron_chestplate" :count 1} {:name "water_bucket" :count 1}
                {:name "cooked_beef" :count 16} {:name "bread" :count 10}
                {:name "cobblestone" :count 64} {:name "cobblestone" :count 64} {:name "cobblestone" :count 64}]
               (many "junk" 26))))

(deftest a-chest-in-range-takes-what-may-be-put-away-and-nothing-is-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory full-inventory :containers {"10,64,0" []}})]
          (know-chest! eng chest-pos)
          (await (run-reflex eng {:free 6} {}))
          (is (= 0 (count (calls p "toss"))) "no toss")
          (is (= 4 (count (calls p "transfer"))) "bread, two cobblestone stacks, one junk")
          (is (= {"iron_pickaxe" 1 "iron_chestplate" 1 "water_bucket" 1 "cooked_beef" 16 "cobblestone" 64}
                 (select-keys (inv p) ["iron_pickaxe" "iron_chestplate" "water_bucket" "cooked_beef" "cobblestone"])))
          (is (nil? (get (inv p) "bread")) "the bread went")
          (is (= 6 (- 36 (stack-count p))) "six slots are free")
          (is (contains? (event-kinds seen) :make-room.done))
          (is (not (declined? seen))))))))

(deftest a-chest-beyond-chest-range-is-not-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "junk" 35) :containers {"50,64,0" []}})]
          (know-chest! eng {:x 50 :y 64 :z 0})
          (await (run-reflex eng {} {}))
          (is (= 0 (count (calls p "transfer"))))
          (is (= 3 (count (calls p "toss"))) "35 stacks to 4 free slots: three tosses")
          (is (= [0 0 0] (mapv #(arg-of "slot" %) (calls p "toss"))) "each toss names its slot (the fake closes the gap each time)"))))))

(deftest a-full-chest-is-remembered-unusable-and-junk-is-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "junk" 35) :containers {"10,64,0" []}})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "full" :moved 0}))
          (know-chest! eng chest-pos)
          (await (run-reflex eng {} {}))
          (is (= [{:pos chest-pos :reason "full"}] (mapv :data (chest-entries eng))))
          (is (= 3 (count (calls p "transfer"))) "three tries, then it stops using the chest")
          (is (= 3 (count (calls p "toss"))) "then it tosses"))))))

(deftest a-chest-marked-unusable-is-not-tried-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "junk" 35) :containers {"10,64,0" []}})]
          (know-chest! eng chest-pos)
          (mem/write! (:store eng) :chest-unusable {:pos chest-pos :reason "full"} mr/unusable-policy)
          (await (run-reflex eng {} {}))
          (is (= 0 (count (calls p "transfer"))))
          (is (= 3 (count (calls p "toss")))))))))

(deftest no-chest-tosses-the-cheapest-first-and-stops-at-the-free-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (vec (concat [{:name "oak_log" :count 5}] (same "diamond_pickaxe" 31 1) [{:name "item_a" :count 1} {:name "item_b" :count 1}]))})]
          (await (run-reflex eng {:toss-below 2} {}))
          (is (= ["item_a" "item_b"] (mapv #(arg-of "item" %) (calls p "toss"))) "worth 0 before the worth-1 oak_log; stops at 4 free")
          (is (= 32 (stack-count p)))
          (is (= [{:x -4 :y 64 :z 0}] (mapv #(js->clj (arg-of "pos" %) :keywordize-keys true) (calls p "moveTo")))
              "then it walks 4 blocks back from where it threw, against the throw direction")
          (is (= [:make-room.tossed :make-room.done] (vec (distinct (filter #{:make-room.tossed :make-room.done} (map :kind @seen)))))))))))

(deftest the-name-picked-up-more-recently-is-tossed-last
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [order (fn ^:async order [newer]
                      (let [{:keys [eng p clock]} (setup {:inventory (vec (concat (same "diamond_pickaxe" 32 1) [{:name "item_a" :count 1} {:name "item_b" :count 1}]))})
                            older (if (= newer "item_a") "item_b" "item_a")]
                        (mem/write! (:store eng) :picked-up {:item older :count 1})
                        (swap! clock + 1000)
                        (mem/write! (:store eng) :picked-up {:item newer :count 1})
                        (await (run-reflex eng {:free 3} {}))
                        (mapv #(arg-of "item" %) (calls p "toss"))))]
          (is (= ["item_b"] (await (order "item_a"))))
          (is (= ["item_a"] (await (order "item_b")))))))))

(deftest tools-and-food-are-never-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (vec (concat (same "iron_pickaxe" 20 1) (same "bread" 10 3) (same "shield" 4 1)))})]
          (await (run-reflex eng {:toss-below 1000} {}))
          (is (= 0 (count (calls p "toss"))))
          (is (declined? seen)))))))

(deftest buckets-are-never-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (vec (concat (same "water_bucket" 12 1) (same "bucket" 12 1) (same "lava_bucket" 12 1)))})]
          (await (run-reflex eng {:toss-below 1000} {}))
          (is (= 0 (count (calls p "toss")))))))))

(deftest building-blocks-keep-their-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (vec (concat (same "iron_pickaxe" 33 1) (same "cobblestone" 3 64)))})]
          (await (run-reflex eng {:keep-blocks 64} {}))
          (is (= 2 (count (calls p "toss"))) "at most two of the three stacks")
          (is (= 64 (get (inv p) "cobblestone")))
          (is (not (declined? seen)) "it acted, so ending short is :done")
          (is (some #(and (= :make-room.done (:kind %)) (:short %)) @seen)))))))

(deftest everything-worth-keeping-declines-with-no-toss
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (same "diamond" 35 1)})]
          (await (run-reflex eng {} {}))
          (is (= 0 (count (calls p "toss"))))
          (is (declined? seen))
          (is (contains? (event-kinds seen) :make-room.declined))
          (is (= {} (:instances (core/state eng)))))))))

(def diamond-on-ground
  {:id 70 :name "item" :kind "item" :pos {:x 4 :y 64 :z 0} :item {:name "diamond" :count 1}})

(deftest a-better-item-on-the-ground-is-swapped-in-when-no-slot-is-free
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (same "oak_log" 36 5) :entities [diamond-on-ground]})]
          (await (run-reflex eng {} {}))
          (is (= [["oak_log" 5]] (mapv (juxt #(arg-of "item" %) #(arg-of "count" %)) (calls p "toss"))) "one oak_log stack")
          (is (= [70] (mapv #(arg-of "id" %) (calls p "collect"))))
          (is (= 1 (get (inv p) "diamond")))
          (is (= 36 (stack-count p)) "35 oak_log stacks and the diamond")
          (is (not (declined? seen)) "a completed swap with nothing more to toss is :done")
          (is (some #(and (= :make-room.done (:kind %)) (:short %)) @seen))
          (is (contains? (event-kinds seen) :make-room.swapped)))))))

(deftest the-swap-throws-away-from-the-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (same "oak_log" 36 5) :entities [diamond-on-ground]})]
          (await (run-reflex eng {} {}))
          (is (= [{:x -3 :y 65.5 :z 0}] (mapv #(js->clj (arg-of "pos" %) :keywordize-keys true) (take 1 (calls p "look"))))
              "the item is east, so it looks west"))))))

(deftest a-ground-item-no-better-than-the-cheapest-stack-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [ground ["dirt" "stick"]]
          (let [{:keys [eng p]} (setup {:inventory (vec (concat (same "oak_log" 35 5) [{:name "stick" :count 1}]))
                                        :entities [(assoc-in diamond-on-ground [:item :name] ground)]})]
            (await (run-reflex eng {:toss-below 0} {}))
            (is (= 0 (count (calls p "toss"))) ground)
            (is (= 0 (count (calls p "collect"))) ground)))))))

(deftest a-walled-direction-is-skipped-when-tossing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "junk" 35)
                                      :blocks {"1,65,0" "stone"}})]
          (await (run-reflex eng {:free 2} {}))
          (is (= [{:x -3 :y 65.5 :z 0}] (mapv #(js->clj (arg-of "pos" %) :keywordize-keys true) (calls p "look")))
              "east is walled at eye level: west is the first free direction"))))))

(deftest max-rounds-gives-up-with-a-stall-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (many "junk" 35) :containers {"10,64,0" []}})]
          (.override (.-world p) "toss" (fn ^:async f [_ _ _] #js {:status "tossed" :count 1}))
          (await (run-reflex eng {:max-rounds 3 :free 36} {}))
          (is (= 3 (count (calls p "toss"))))
          (is (some #(= :make-room.stalled (:kind %)) @seen))
          (is (declined? seen)))))))
