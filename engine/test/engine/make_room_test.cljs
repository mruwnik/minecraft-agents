(ns engine.make-room-test
  "jobs.storage.make-room, the reflex of inventory-nearly-full, against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.storage.make-room :as mr]))

(def t0 1000000)

(defn setup
  ([world] (setup world nil))
  ([world zones]
  (let [clock (atom t0)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (when zones (ew/of-data {} {} zones))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock})))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn arg-of [k call] (aget (.-args call) k))

(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))

(defn carried
  "{name total} over all stacks (inv keeps only the last stack of a name)."
  [p]
  (reduce (fn [m s] (update m (.-name s) (fnil + 0) (.-count s))) {} (.-inventory (.self p))))

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

(defn returns-parent
  "A parent that calls make-room with args as its child, keeps every call's return in returns and the child's result in
  out. seed, when given, is put in the child's memory before its first call (left by an earlier, cut round)."
  [out returns args seed]
  {:check (constantly true)
   :round (fn ^:async returns-round [c]
            (when (and seed (empty? @returns) (nil? (get-in (ctx/mem c) [:children :kid])))
              (ctx/update-mem! c assoc-in [:children :kid] (merge {:args args :children {}} seed)))
            (let [r (await (ctx/call-child c :kid 'jobs.storage.make-room args))]
              (swap! returns conj r)
              (when (= :done r) (reset! out (ctx/child-result c :kid)))
              r))})

(defn with-parent
  "setup world, with returns-parent listed over make-room args; adds :out and :returns."
  ([world args] (with-parent world args nil))
  ([world args seed]
   (let [{:keys [eng] :as s} (setup world)
         out (atom :not-done)
         returns (atom [])
         eng (assoc eng :jobs (assoc (:jobs eng) 'returns-parent (returns-parent out returns args seed)))]
     (core/submit! eng '(returns-parent) {})
     (assoc s :eng eng :out out :returns returns))))

(defn ^:async tick-out!
  "Tick until the list is empty, at most n ticks."
  [eng n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn ^:async run-child
  "Run make-room as the child of returns-parent to its end; {:eng :p :seen :out :returns}."
  [world args seed]
  (let [s (with-parent world args seed)]
    (await (tick-out! (:eng s) 30))
    s))

(defn feet [p] (let [pos (.-pos (.self p))] {:x (Math/floor (.-x pos)) :y (Math/floor (.-y pos)) :z (Math/floor (.-z pos))}))

(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

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

(deftest keep-counts-without-keep-food-uses-the-food-reserve
  (is (= {"bread" 12 "apple" 0 "golden_apple" 2 "dirt" 0}
         (mr/keep-counts [{:name "bread" :count 20} {:name "apple" :count 5} {:name "golden_apple" :count 2} {:name "dirt" :count 9}]
                         {:keep-food nil :keep-blocks 0}))
      "the 60-point reserve is 12 bread; golden apples are kept whole, outside the reserve")
  (is (= {"golden_apple" 2 "bread" 0}
         (mr/keep-counts [{:name "golden_apple" :count 2} {:name "bread" :count 3}] {:keep-food 0 :keep-blocks 0}))))

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
                   {:name "poppy" :count 3 :slot 4}]
        order (fn [recency] (names-of (mr/toss-order inventory {} recency 5)))]
    (is (= ["dirt" "gravel" "stick" "poppy" "oak_log"] (order {})) "junk blocks first (bigger stack first), then by worth: stick and poppy equal (3, 3 by slot), the log dearer")
    (is (= ["dirt" "gravel" "poppy" "stick" "oak_log"] (order {"stick" 100})) "the one picked up most recently goes last of its worth")
    (is (= ["dirt" "gravel" "stick" "poppy" "oak_log"] (order {"poppy" 100 "gravel" 50 "dirt" 50}))
        "stick has no pick-up (0): before the picked-up names; later pick-ups after earlier ones")))

(deftest toss-order-throws-junk-blocks-first-and-keeps-coal-and-ores-below-their-worth
  (let [inventory [{:name "coal" :count 30 :slot 0}
                   {:name "raw_copper" :count 12 :slot 1}
                   {:name "tuff" :count 20 :slot 2}
                   {:name "cobbled_deepslate" :count 64 :slot 3}
                   {:name "granite" :count 64 :slot 4}
                   {:name "iron_ore" :count 5 :slot 5}
                   {:name "diamond" :count 2 :slot 6}]]
    (is (= ["cobbled_deepslate" "granite" "tuff"]
           (names-of (mr/toss-order inventory {} {} 3)))
        "junk blocks first, whole stacks before partial; coal and raw copper are worth keeping")
    (is (= ["cobbled_deepslate" "granite" "tuff" "iron_ore" "raw_copper" "coal"]
           (names-of (mr/toss-order inventory {} {} 8)))
        "raised :toss-below lets the stacks of raw copper and coal go")))

(deftest toss-order-orders-by-recency-before-count
  (let [inventory [{:name "item_b" :count 9 :slot 0} {:name "item_a" :count 3 :slot 1}]]
    (is (= ["item_b" "item_a"] (names-of (mr/toss-order inventory {} {"item_b" 1 "item_a" 2} 5))))
    (is (= ["item_a" "item_b"] (names-of (mr/toss-order inventory {} {"item_b" 2 "item_a" 1} 5))))))

(deftest toss-order-skips-protected-food-and-stacks-worth-max-or-more
  (let [inventory [{:name "iron_pickaxe" :count 1 :slot 0}
                   {:name "bread" :count 3 :slot 1}
                   {:name "oak_log" :count 4 :slot 2}
                   {:name "diamond" :count 1 :slot 3}
                   {:name "dirt" :count 4 :slot 4}]]
    (is (= ["dirt"] (names-of (mr/toss-order inventory {} {} 0.5))))
    (is (= ["dirt" "oak_log"] (names-of (mr/toss-order inventory {} {} 2))))
    (is (= [] (names-of (mr/toss-order inventory {} {} 0))))
    (is (= ["dirt" "oak_log" "diamond"] (names-of (mr/toss-order inventory {} {} 1000))))))

(deftest toss-order-never-cuts-into-a-floor
  (let [stacks (fn [& counts] (vec (map-indexed (fn [i c] {:name "cobblestone" :count c :slot i}) counts)))]
    (are [counts floor expected] (= expected (mapv :slot (mr/toss-order (apply stacks counts) {"cobblestone" floor} {} 1)))
      [64 64 64] 64 [0 1]
      [64 64 64] 0 [0 1 2]
      [64 64 64] 192 []
      [64 64 64] 100 [0]
      [40 30] 64 []
      [40 30] 30 [0]
      [10 64] 64 [0])))

(deftest deposit-names-lists-the-names-above-their-keep-least-worth-keeping-first
  (let [inventory [{:name "iron_pickaxe" :count 1}
                   {:name "bread" :count 20}
                   {:name "cobblestone" :count 64}
                   {:name "cobblestone" :count 64}
                   {:name "dirt" :count 3}
                   {:name "water_bucket" :count 1}]]
    (are [keep expected] (= expected (mr/deposit-names inventory keep {}))
      {"bread" 16 "cobblestone" 64 "dirt" 0 "iron_pickaxe" 1} ["dirt" "cobblestone" "bread"]
      {"bread" 20 "cobblestone" 128 "dirt" 0} ["dirt"]
      {"bread" 20 "cobblestone" 128 "dirt" 3} [])))

(deftest deposit-names-puts-the-older-pick-up-first-among-equal-worth
  (let [inventory [{:name "gravel" :count 64} {:name "sand" :count 64} {:name "dirt" :count 64}]
        keep {"gravel" 0 "sand" 0 "dirt" 0}]
    (are [recency expected] (= expected (mr/deposit-names inventory keep recency))
      {} ["gravel" "sand" "dirt"]
      {"gravel" 5} ["sand" "dirt" "gravel"]
      {"gravel" 5 "sand" 3 "dirt" 9} ["sand" "gravel" "dirt"])))

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

(deftest a-chest-in-range-puts-away-the-least-worth-keeping-first-and-nothing-is-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory full-inventory :containers {"10,64,0" []}})]
          (know-chest! eng chest-pos)
          (await (run-reflex eng {:free 6} {}))
          (is (= 0 (count (calls p "toss"))) "no toss")
          (is (= 4 (count (calls p "transfer"))) "four junk stacks")
          (is (= {"iron_pickaxe" 1 "iron_chestplate" 1 "water_bucket" 1 "cooked_beef" 16 "bread" 10 "cobblestone" 192}
                 (select-keys (carried p) ["iron_pickaxe" "iron_chestplate" "water_bucket" "cooked_beef" "bread" "cobblestone"]))
              "bread and the building blocks stay")
          (is (= 6 (- 36 (stack-count p))) "six slots are free")
          (is (contains? (event-kinds seen) :make-room.done))
          (is (not (declined? seen))))))))

;; The live case: bread and cobblestone sit in low slots, junk after them.
(deftest a-chest-takes-the-junk-and-not-the-bread-and-cobblestone-in-low-slots
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inventory (vec (concat [{:name "iron_pickaxe" :count 1} {:name "iron_sword" :count 1}]
                                     (same "bread" 2 64) (same "cobblestone" 3 64)
                                     (same "gravel" 14 64) (same "sand" 13 64)))
              {:keys [eng p]} (setup {:inventory inventory :containers {"10,64,0" []}})]
          (know-chest! eng chest-pos)
          (await (run-reflex eng {:free 4} {}))
          (is (= 0 (count (calls p "toss"))))
          (is (= {"bread" 128 "cobblestone" 192 "iron_pickaxe" 1 "iron_sword" 1}
                 (select-keys (carried p) ["bread" "cobblestone" "iron_pickaxe" "iron_sword"])))
          (is (seq (calls p "transfer")))
          (is (every? #{"gravel" "sand"} (map #(arg-of "item" %) (calls p "transfer")))))))))

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
        (let [{:keys [eng p seen]} (setup {:inventory (vec (concat [{:name "coal" :count 5}] (same "diamond_pickaxe" 31 1) [{:name "item_a" :count 1} {:name "item_b" :count 1}]))})]
          (await (run-reflex eng {:toss-below 2} {}))
          (is (= ["item_a" "item_b"] (mapv #(arg-of "item" %) (calls p "toss"))) "the coal is kept; stops at 4 free")
          (is (= 32 (stack-count p)))
          (is (>= 1 (Math/abs (- -4 (:x (feet p))))) "then it walks 4 blocks back from where it threw, against the throw direction")
          (is (= 0 (:z (feet p))))
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
          (is (= [:nothing-to-go] (mapv :reason (of-kind seen :make-room.stopped)))))))))

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
          (is (not (declined? seen)))
          (is (= [:short] (mapv :reason (of-kind seen :make-room.stopped))) "ending short is stopped, not done")
          (is (empty? (of-kind seen :make-room.done))))))))

(deftest everything-worth-keeping-stops-with-no-toss
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (same "diamond" 35 1)})]
          (await (run-reflex eng {} {}))
          (is (= 0 (count (calls p "toss"))))
          (is (not (declined? seen)))
          (is (= [:nothing-to-go] (mapv :reason (of-kind seen :make-room.stopped))))
          (is (= {} (:instances (core/state eng)))))))))

(def diamond-on-ground
  {:id 70 :name "item" :kind "item" :pos {:x 4 :y 64 :z 0} :item {:name "diamond" :count 1}})

(deftest a-better-item-on-the-ground-is-swapped-in-when-no-slot-is-free
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (same "coal" 36 5) :entities [diamond-on-ground]})]
          (await (run-reflex eng {} {}))
          (is (= [["coal" 5]] (mapv (juxt #(arg-of "item" %) #(arg-of "count" %)) (calls p "toss"))) "one coal stack")
          (is (= [70] (mapv #(arg-of "id" %) (calls p "collect"))))
          (is (= 1 (get (inv p) "diamond")))
          (is (= 36 (stack-count p)) "35 coal stacks and the diamond")
          (is (not (declined? seen)))
          (is (= [:short] (mapv :reason (of-kind seen :make-room.stopped))) "a swap with nothing more to toss is still short")
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

(deftest max-steps-gives-up-with-a-stall-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (many "junk" 35) :containers {"10,64,0" []}})]
          (.override (.-world p) "toss" (fn ^:async f [_ _ _] #js {:status "tossed" :count 1}))
          (await (run-reflex eng {:max-steps 3 :free 36} {}))
          (is (= 3 (count (calls p "toss"))))
          (is (= 1 (count (of-kind seen :make-room.stalled))))
          (is (= {} (:instances (core/state eng)))))))))


(deftest junk-blocks-are-tossed-before-coal-and-copper-and-the-result-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (vec (concat [{:name "coal" :count 30} {:name "raw_copper" :count 12}
                                                                   {:name "cobbled_deepslate" :count 64} {:name "tuff" :count 20}]
                                                                  (same "diamond_pickaxe" 30 1)))})]
          (await (run-reflex eng {:free 4 :keep-blocks 0} {}))
          (is (= ["cobbled_deepslate" "tuff"] (mapv #(arg-of "item" %) (calls p "toss"))))
          (is (= {"coal" 30 "raw_copper" 12} (select-keys (carried p) ["coal" "raw_copper"])))
          (is (= [{:item "cobbled_deepslate" :count 64} {:item "tuff" :count 20}]
                 (:tossed (first (filter #(= :make-room.done (:kind %)) @seen)))))
          (is (some #(and (= :make-room.tossed (:kind %)) (:junk %)) @seen) "the event says it was a junk block"))))))

(deftest a-submitted-make-room-ends-done-after-the-toss-that-made-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:inventory (vec (concat [{:name "coal" :count 30}
                                                                          {:name "cobbled_deepslate" :count 64} {:name "granite" :count 64}]
                                                                         (same "diamond_pickaxe" 33 1)))})]
          (core/submit! eng '(jobs.storage.make-room {:free 2 :keep-blocks 0}) {})
          (dotimes [_ 12]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (some #(= :make-room.done (:kind %)) @seen) "done is emitted, not a wait for not-ready")
          (is (empty? (:list (core/state eng))) "the job ended"))))))

(deftest torches-are-never-tossed
  (let [inventory [{:name "torch" :count 16 :slot 0} {:name "poppy" :count 3 :slot 1}]]
    (is (= ["poppy"] (names-of (mr/toss-order inventory {} {} 2))))))

;; ------------------------------------------------------------------ one round is one whole attempt

(deftest one-call-tosses-until-enough-is-free-with-no-continue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out returns p]} (await (run-child {:inventory (many "junk" 35)} {} nil))]
          (is (= [:done] @returns) "three tosses and the walk away in one call")
          (is (= {:status :done :free 4 :tossed [{:item "junk_0" :count 1} {:item "junk_1" :count 1} {:item "junk_2" :count 1}]}
                 @out))
          (is (= 32 (stack-count p))))))))

(deftest one-call-puts-away-at-the-chest-with-no-continue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out returns p]} (with-parent {:inventory full-inventory :containers {"10,64,0" []}} {:free 6})]
          (know-chest! eng chest-pos)
          (await (tick-out! eng 30))
          (is (= [:done] @returns))
          (is (= :done (:status @out)))
          (is (= 4 (count (calls p "transfer"))))
          (is (= 0 (count (calls p "toss")))))))))

(deftest a-reflex-run-is-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:inventory (many "junk" 35)})]
          (await (run-reflex eng {} {}))
          (is (= 1 (count (of-kind seen :round_started))))
          (is (= 1 (count (of-kind seen :make-room.done)))))))))

(deftest ending-short-is-stopped-with-the-count-and-what-went
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out returns]} (await (run-child {:inventory (vec (concat (same "iron_pickaxe" 33 1) (same "cobblestone" 3 64)))}
                                                      {:keep-blocks 64} nil))]
          (is (= [:done] @returns))
          (is (= {:status :stopped :reason :short :free 2
                  :tossed [{:item "cobblestone" :count 64} {:item "cobblestone" :count 64}]}
                 (dissoc @out :text)))
          (is (string? (:text @out))))))))

(deftest nothing-that-may-go-is-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out returns p]} (await (run-child {:inventory (same "diamond" 35 1)} {} nil))]
          (is (= [:done] @returns))
          (is (= [:stopped :nothing-to-go 1] ((juxt :status :reason :free) @out)))
          (is (empty? (.-calls (.-world p))) "no act"))))))

(deftest a-cut-mid-round-resumes-and-keeps-what-it-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out returns]} (with-parent {:inventory (many "junk" 35)} {})
              looks (atom 0)]
          (.override (.-world p) "look" (fn ^:async f [token a impl]
                                          (when (= 2 (swap! looks inc)) (takeover/take! eng {:who "claude" :why "cut"}))
                                          (await (impl token a))))
          (await (core/tick! eng))
          (is (= [] @returns) "the cut call returned nothing")
          (is (= 34 (stack-count p)) "one toss before the cut")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (tick-out! eng 10))
          (is (= [:done] @returns) "the resumed call ended done")
          (is (= 32 (stack-count p)))
          (is (= [{:item "junk_0" :count 1} {:item "junk_1" :count 1} {:item "junk_2" :count 1}] (:tossed @out))
              "the toss before the cut is in the result"))))))

(deftest a-toss-intent-left-by-a-restart-is-inspected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [intent (fn [item] {:tossing {:item item :count 1 :before 1 :at {:x 0 :y 64 :z 0} :dir [1 0]}})
              gone (await (run-child {:inventory (many "junk" 35)} {} (intent "flint")))
              kept (await (run-child {:inventory (many "junk" 35)} {} (intent "junk_9")))]
          (is (= [{:item "flint" :count 1} {:item "junk_0" :count 1} {:item "junk_1" :count 1} {:item "junk_2" :count 1}]
                 (:tossed @(:out gone)))
              "flint is no longer carried: the toss went out before the restart")
          (is (= [{:item "junk_0" :count 1} {:item "junk_1" :count 1} {:item "junk_2" :count 1}] (:tossed @(:out kept)))
              "junk_9 is still carried: the toss never happened"))))))

(deftest a-swap-intent-left-by-a-cut-picks-up-the-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-child {:inventory (same "oak_log" 35 5) :entities [diamond-on-ground]}
                                            {:free 1 :toss-below 0}
                                            {:acted true :swap {:id 70 :item "diamond" :worth 10}}))]
          (is (= [70] (mapv #(arg-of "id" %) (calls p "collect"))))
          (is (= 0 (count (calls p "toss"))))
          (is (= 1 (get (inv p) "diamond"))))))))

(deftest a-remembered-chest-that-is-gone-is-dropped-and-junk-is-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (with-parent {:inventory (many "junk" 35)} {})]
          (know-chest! eng chest-pos)
          (await (tick-out! eng 30))
          (is (= [:done] @(:returns s)))
          (is (= :done (:status @(:out s))))
          (is (= 3 (count (calls p "toss"))))
          (is (= 1 (count (calls p "transfer"))) "one try finds it missing")
          (is (nil? (mem/place (mem/view (:store eng)) :chest)) "the missing chest is no longer known")
          (is (seq (of-kind seen :chest_missing))))))))

(deftest a-reflex-cut-after-a-toss-keeps-the-spot-in-body-memory-and-walks-away-after-the-refire
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "junk" 35)})
              looks (atom 0)
              spots #(mem/entries (mem/view (:store eng)) :make-room-tossed)]
          (core/register-reflex! eng {:trigger :inventory-nearly-full :args {} :job (list 'jobs.storage.make-room {})})
          (.override (.-world p) "look" (fn ^:async f [token a impl]
                                          (when (= 2 (swap! looks inc)) (takeover/take! eng {:who "claude" :why "cut"}))
                                          (await (impl token a))))
          (await (core/tick! eng))
          (is (= 1 (count (spots))) "the first toss's spot outlives the cut reflex")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (loop [i 0]
            (await (core/tick! eng))
            (when (and (< i 60) (seq (:instances (core/state eng)))) (recur (inc i))))
          (is (= 3 (count (calls p "toss"))) "no stack thrown twice")
          (is (empty? (calls p "collect")) "nothing picked up again")
          (is (empty? (spots)) "the walk away was made, the spot is forgotten"))))))

(deftest a-walk-away-that-yields-is-waited-out-and-make-room-never-yields
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory (many "junk" 35)})
              walks (atom 0)
              go-to {:check (constantly true)
                     :round (fn ^:async go-to-round [_c] (if (= 1 (swap! walks inc)) :continue :done))}
              returns (atom [])
              spots #(mem/entries (mem/view (:store eng)) :make-room-tossed)
              eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.movement.go-to go-to
                                          'returns-parent (returns-parent (atom nil) returns {} nil)))]
          (core/submit! eng '(returns-parent) {})
          (await (tick-out! eng 30))
          (is (= [:done] @returns) "a reflex never yields: the walk is waited out inside the call")
          (is (= 2 @walks))
          (is (empty? (spots)) "the spot is forgotten once the walk is done"))))))

(deftest a-refused-chest-and-nothing-to-toss-is-nothing-to-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out returns]} (with-parent {:inventory (same "diamond" 35 1) :containers {"10,64,0" []}} {})]
          (.override (.-world p) "transfer" (fn ^:async f [_ _ _] #js {:status "full" :moved 0}))
          (know-chest! eng chest-pos)
          (await (tick-out! eng 30))
          (is (= [:done] @returns))
          (is (= [:stopped :nothing-to-go] ((juxt :status :reason) @out))))))))

(deftest toss-failures-count-in-a-row
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (many "junk" 35)})
              n (atom 0)]
          (.override (.-world p) "toss" (fn ^:async f [token a impl]
                                          (if (odd? (swap! n inc))
                                            #js {:status "failed"}
                                            (await (impl token a)))))
          (await (run-reflex eng {} {}))
          (is (empty? (of-kind seen :make-room.toss-failed)) "a failure between successes does not add up")
          (is (= 1 (count (of-kind seen :make-room.done)))))))))

;; ------------------------------------------------------------------ zones

(def chest-zone {:name "vault" :owner "Miles" :min [9 63 -1] :max [11 65 1]})

(deftest a-chest-in-another-s-zone-is-not-filled-and-the-junk-is-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:inventory (many "junk" 35) :containers {"10,64,0" []}} [chest-zone])]
          (know-chest! eng chest-pos)
          (await (run-reflex eng {:free 3} {}))
          (is (= 0 (count (calls p "transfer"))) "nothing put into another's chest")
          (is (pos? (count (calls p "toss"))) "tossed instead")
          (is (= 3 (- 36 (stack-count p))) "three slots are free")
          (is (contains? (event-kinds seen) :make-room.done)))))))

(deftest ignore-zones-fills-a-chest-in-another-s-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "junk" 35) :containers {"10,64,0" []}} [chest-zone])]
          (know-chest! eng chest-pos)
          (await (run-reflex eng {:free 3 :ignore-zones? true} {}))
          (is (pos? (count (calls p "transfer"))) "put away")
          (is (= 0 (count (calls p "toss"))) "nothing tossed"))))))

(deftest a-chest-in-the-body-s-own-zone-is-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory (many "junk" 35) :containers {"10,64,0" []}} [(assoc chest-zone :owner "Fake")])]
          (know-chest! eng chest-pos)
          (await (run-reflex eng {:free 3} {}))
          (is (pos? (count (calls p "transfer"))))
          (is (= 0 (count (calls p "toss")))))))))

(deftest a-toss-spot-far-from-the-body-is-forgotten-without-a-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory (same "diamond" 35 1)})
              walks (atom 0)
              go-to {:check (constantly true)
                     :round (fn ^:async go-to-round [_c] (swap! walks inc) :done)}
              spots #(mem/entries (mem/view (:store eng)) :make-room-tossed)
              eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.movement.go-to go-to))]
          (mem/write! (:store eng) :make-room-tossed {:at {:x 400 :y 64 :z 400} :dir [1 0]} mr/tossed-policy)
          (core/submit! eng '(jobs.storage.make-room) {})
          (await (tick-out! eng 10))
          (is (zero? @walks) "the old toss site is not walked back to")
          (is (empty? (spots))))))))

(deftest a-recent-toss-spot-near-the-body-is-walked-away-from-even-when-this-run-tossed-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory (same "diamond" 35 1)})
              walks (atom 0)
              go-to {:check (constantly true)
                     :round (fn ^:async go-to-round [_c] (swap! walks inc) :done)}
              spots #(mem/entries (mem/view (:store eng)) :make-room-tossed)
              eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.movement.go-to go-to))]
          (mem/write! (:store eng) :make-room-tossed {:at {:x 2 :y 64 :z 0} :dir [1 0]} mr/tossed-policy)
          (core/submit! eng '(jobs.storage.make-room) {})
          (await (tick-out! eng 10))
          (is (= 1 @walks) "the items thrown in the last 2 minutes may still lie there: step away once")
          (is (empty? (spots))))))))
