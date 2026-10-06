(ns engine.extinguish-test
  "jobs.survival.extinguish and the burning trigger against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.zones-survival-test :as zs]
            [engine.fake :as fake]
            [engine.events :as events]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.extinguish :as extinguish]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn stopped-reason [seen] (:reason (first (of-kind seen :stopped))))

(defn on-place!
  "Run f (state, args) after each place of item, the fake's own place first."
  [p item f]
  (.override (.-world p) "place"
             (fn ^:async g [tok a impl]
               (let [r (await (impl tok a))]
                 ((get {item f} (.-item a) (fn [& _])) (fake/state p) a)
                 r))))

(defn on-wait!
  "Run f with the wait's number (from 1) and ms before each wait."
  [p f]
  (let [n (atom 0)]
    (.override (.-world p) "wait"
               (fn ^:async g [tok a impl]
                 (f (swap! n inc) (.-ms a))
                 (await (impl tok a))))))

(defn floor
  "Stone at y 63 for every x and z within n of the origin."
  [n]
  (into {} (for [x (range (- n) (inc n)) z (range (- n) (inc n))] [(str x ",63," z) "stone"])))

(defn cells [name ys xs zs]
  (into {} (for [y ys x xs z zs] [(str x "," y "," z) name])))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [c] (js->clj (.-args c) :keywordize-keys true))

(defn pos-now [p] (js->clj (.-pos (.self p)) :keywordize-keys true))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn bare-ctx
  "A ctx with empty job memory over primitives p, for calling check and round directly."
  [p args]
  {:primitives p :args args :view (fn [] {:data {} :now 0}) :root "j1" :slots []})

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

;; ------------------------------------------------------------------- check

(deftest check-is-true-on-fire-or-in-lava-and-false-otherwise
  (doseq [[self expected] [[{:onFire true} true]
                           [{:inLava true} true]
                           [{:onFire true :inLava true} true]
                           [{:inWater true} false]
                           [{} false]]]
    (is (= expected (extinguish/check (bare-ctx (tu/fake {:self self}) {}))) (pr-str self))))

(def fire-resistance [{:name "fire_resistance" :amplifier 0 :duration 600}])

(deftest a-fire-resistant-body-is-not-burning
  (doseq [[self expected] [[{:onFire true :effects fire-resistance} false]
                           [{:inLava true :effects fire-resistance} false]
                           [{:onFire true :effects [{:name "speed" :amplifier 0 :duration 600}]} true]
                           [{:onFire true} true]]]
    (is (= expected (extinguish/check (bare-ctx (tu/fake {:self self}) {}))) (pr-str self))
    (is (= expected ((:when (:burning triggers/all)) (tu/fake {:self self}) nil {})) (pr-str self))))

(deftest burning-trigger-holds-on-fire-or-in-lava
  (doseq [[self expected] [[{:onFire true} true] [{:inLava true} true] [{} false]]]
    (is (= expected ((:when (:burning triggers/all)) (tu/fake {:self self}) nil {})) (pr-str self))))

;; ------------------------------------------------------------------- rounds

(deftest round-is-done-without-acting-when-clear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/fake {})]
          (is (= :done (await (extinguish/round (bare-ctx p {})))))
          (is (= [] (vec (.-calls (.-world p))))))))))

(deftest heads-for-water-when-it-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 8) {"3,64,0" "water" "0,64,40" "water"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [{:x 3 :y 64 :z 0}] (mapv (comp :pos call-args) (calls p "moveTo"))))
          (is (not (.-onFire (.self p))))
          (is (= [] (:list (core/state eng))) "done in one run once the fire is out")
          (is (= [{:pos {:x 0 :y 64 :z 0} :cause :fire}] (entries eng :extinguish))))))))

(deftest water-beyond-the-radius-is-ignored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 12) {"9,64,0" "water" "1,64,0" "fire"})})]
          (core/submit! eng '(jobs.survival.extinguish {:water-radius 6}) {})
          (await (core/tick! eng))
          (let [target (:pos (call-args (first (calls p "moveTo"))))]
            (is (not= {:x 9 :y 64 :z 0} target))
            (is (<= (Math/hypot (:x target) (:z target)) 6) "the walk is short")
            (is (= "stone" (.-name (.blockAt p (clj->js (update target :y dec))))) "onto a solid block")))))))

(deftest fire-blocks-are-not-a-place-to-stand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 8) {"0,63,0" "magma_block" "1,64,0" "fire"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (let [target (:pos (call-args (first (calls p "moveTo"))))
                under (.-name (.blockAt p (clj->js (update target :y dec))))]
            (is (= "stone" under))
            (is (not= {:x 1 :y 64 :z 0} target))))))))

(deftest moves-out-of-lava-preferring-higher-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inLava true}
                                      :blocks (merge (floor 6)
                                                     (cells "lava" [64] [-1 0 1] [-1 0 1])
                                                     {"2,64,0" "stone"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= {:x 2 :y 65 :z 0} (pos-now p)) "up onto the ledge, out of the pool")
          (is (not (.-inLava (.self p))))
          (is (some #{{:kind :lava :pos {:x -1 :y 64 :z -1}}} (entries eng :hazard)) "the pool is remembered as hazards")
          (is (= 9 (count (entries eng :hazard))))
          (is (= [{:pos {:x 0 :y 64 :z 0} :cause :lava}] (entries eng :extinguish)))
          (is (= {:cap 20 :ttl 3600000} (select-keys (mem/policy (mem/view (:store eng)) :extinguish) [:cap :ttl])))
          (is (= {:cap 50 :ttl 21600000} (select-keys (mem/policy (mem/view (:store eng)) :hazard) [:cap :ttl]))))))))

(deftest hazards-are-not-written-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inLava true}
                                      :blocks (merge (floor 6) (cells "lava" [64] [-1 0 1] [-1 0 1]))})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status "blocked"}))
          (await (core/tick! eng))
          (is (= 3 (count (calls p "moveTo"))) "three blocked walks in one run")
          (is (= 9 (count (entries eng :hazard)))))))))

;; ------------------------------------------------------------------- bucket

(deftest uses-a-water-bucket-at-the-feet-when-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (floor 6)})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= {:pos {:x 0 :y 64 :z 0} :item "water_bucket"} (call-args (first (calls p "place")))))
          (is (= [] (calls p "moveTo")) "no walking needed")
          (is (not (.-onFire (.self p))))
          (is (= [] (:list (core/state eng))) "poured and scooped in one run")
          (is (= [] (entries eng :extinguish-pour)) "the pour is forgotten once scooped"))))))

(deftest scoops-the-poured-water-back-once-the-fire-is-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (floor 6)})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= [{:pos {:x 0 :y 64 :z 0} :item "water_bucket"}
                  {:pos {:x 0 :y 64 :z 0} :item "bucket"}]
                 (mapv call-args (calls p "place"))))
          (is (not= "water" (.-name (.blockAt p #js {:x 0 :y 64 :z 0}))))
          (is (some #(= "water_bucket" (:name %)) (u/inventory p))))))))

(deftest a-poured-cell-that-is-no-longer-water-ends-the-job-without-a-scoop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (floor 6)})]
          (on-place! p "water_bucket" (fn [s _] (fake/remove-block! p [0 64 0])))
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= 1 (count (calls p "place"))))
          (is (= extinguish/max-water-waits (count (calls p "wait"))) "waited the bound for the water block"))))))

(deftest no-safe-cell-gives-up-after-three-rounds-with-one-warning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:onFire true} :blocks {"0,63,0" "stone" "1,64,0" "fire"}})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "gives up in one run")
          (is (= 1 (count (filter #(= :extinguish_stuck (:kind %)) @seen))))
          (is (= :stuck (stopped-reason seen)) "ends stopped, not completed"))))))

(deftest does-not-pour-water-into-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:inLava true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (merge (floor 6) (cells "lava" [64] [0] [0]))})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (calls p "place")))
          (is (= 1 (count (calls p "moveTo")))))))))

(deftest burning-on-bare-stone-with-no-water-or-hazards-stands-still-and-holds-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:onFire true} :blocks (floor 8)})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (let [holds (atom [])]
            (on-wait! p (fn [n _]
                          (swap! holds conj (:reason (core/holding eng (:id (core/holder eng)))))
                          (when (= n 3) (swap! (fake/state p) assoc-in [:self :onFire] false))))
            (core/submit! eng '(jobs.survival.extinguish) {})
            (await (core/tick! eng))
            (is (= [] (calls p "moveTo")) "does not walk")
            (is (= 1 (count (filter #(= :extinguish_wait (:kind %)) @seen))) "the agent is told once")
            (is (= [:burning-wait :burning-wait :burning-wait] @holds) "a declared hold while it waits")
            (is (= [] (:list (core/state eng))) "one run until the fire is out")
            (is (= 1 (count (filter #(= :completed (:kind %)) @seen))))
            (is (not-any? #(or (= :required (:attention %)) (= :failed (:kind %))) @seen))))))))

(deftest standing-still-gives-up-after-the-wait-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:onFire true} :blocks (floor 8)})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= extinguish/max-stand-waits (count (calls p "wait"))))
          (is (= :still-burning (stopped-reason seen)) "still burning is stopped, not completed"))))))

(deftest standing-still-eats-to-keep-regenerating
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true :food 10} :inventory [{:name "bread" :count 2}]
                                      :blocks (floor 8)})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (pos? (count (calls p "eat")))))))))

(deftest powder-snow-counts-as-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 8) {"3,64,0" "powder_snow"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= {:pos {:x 3 :y 64 :z 0} :range 0} (call-args (first (calls p "moveTo"))))))))))

(deftest covers-an-adjacent-lava-source-with-a-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :inventory [{:name "cobblestone" :count 4}]
                                      :blocks (merge (floor 8) {"1,64,0" "lava"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [{:pos {:x 1 :y 64 :z 0} :item "cobblestone"}] (mapv call-args (calls p "place"))))
          (is (= [] (calls p "moveTo")) "no walk past the lava")
          (is (= "cobblestone" (.-name (.blockAt p #js {:x 1 :y 64 :z 0})))))))))

(deftest burning-next-to-a-fire-block-still-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :blocks (merge (floor 8) {"1,64,0" "fire"})})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= 1 (count (calls p "moveTo")))))))))

(deftest a-block-read-that-lags-the-pour-does-not-skip-the-scoop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (floor 6)})]
          (on-place! p "water_bucket" (fn [s _] (fake/remove-block! p [0 64 0]))) ; the block update lags
          (on-wait! p (fn [n _] (when (= n 2) (fake/set-block! p [0 64 0] "water")))) ; now it arrives
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= 2 (count (calls p "wait"))) "waited for the water block")
          (is (= [] (:list (core/state eng))))
          (is (= ["water_bucket" "bucket"] (mapv :item (map call-args (calls p "place")))))
          (is (not= "water" (.-name (.blockAt p #js {:x 0 :y 64 :z 0}))))
          (is (some #(= "water_bucket" (:name %)) (u/inventory p))))))))

(deftest a-failed-scoop-warns-with-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (tu/fake {:self {:onFire true} :inventory [{:name "water_bucket" :count 1}] :blocks (floor 6)})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (on-place! p "water_bucket" (fn [s _] (swap! s assoc :inventory [])))   ; no empty bucket to scoop with
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (let [[e & more] (filter #(= :extinguish.scoop_failed (:kind %)) @seen)]
            (is (empty? more))
            (is (some? e))
            (is (= {:x 0 :y 64 :z 0} (:pos e)))))))))

(defn ^:async cover-run
  "Ticks n rounds of a burning body with inventory on stone, lava at the cells; the engine's place calls."
  [inv lava n & [zones]]
  (let [w {:self {:onFire true} :inventory inv
           :blocks (merge (floor 8) (into {} (map (fn [k] [k "lava"]) lava)))}
        {:keys [eng p]} (if zones (zs/setup w zones) (setup w))]
    (core/submit! eng '(jobs.survival.extinguish) {})
    (dotimes [_ n] (await (core/tick! eng)))
    (mapv call-args (calls p "place"))))

(deftest sand-and-gravel-are-never-a-cover-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [item ["sand" "gravel" "red_sand" "white_concrete_powder"]]
          (is (= [] (await (cover-run [{:name item :count 8}] ["1,64,0"] 1))) item))
        (is (= 1 (count (await (cover-run [{:name "cobblestone" :count 8}] ["1,64,0"] 1)))))))))

(deftest a-cover-is-not-placed-in-anothers-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [] (await (cover-run [{:name "cobblestone" :count 4}] ["1,64,0"] 1 [(zs/whole-zone "Miles")]))))))))

(deftest covers-per-run-are-capped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [placed (await (cover-run [{:name "cobblestone" :count 20}]
                                       ["1,64,0" "-1,64,0" "0,64,1" "0,64,-1" "1,64,1" "2,64,0" "-2,64,0" "0,64,2"] 1))]
          (is (<= (count (filter #(= "cobblestone" (:item %)) placed)) extinguish/max-covers)))))))

(deftest lava-under-an-already-covered-cell-is-not-covered-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [placed (await (cover-run [{:name "cobblestone" :count 8}] ["1,64,0" "1,63,0"] 1))]
          (is (= [{:pos {:x 1 :y 64 :z 0} :item "cobblestone"}] placed)))))))

(deftest lava-beside-the-feet-under-an-overhang-is-covered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:self {:onFire true} :inventory [{:name "cobblestone" :count 8}]
                 :blocks (merge (floor 8) {"1,64,0" "lava" "1,65,0" "stone"})}
              {:keys [eng p]} (setup w)]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [{:pos {:x 1 :y 64 :z 0} :item "cobblestone"}] (mapv call-args (calls p "place")))))))))

;; ------------------------------------------------------------------- one run, cuts

(deftest a-cut-while-waiting-on-a-pour-leaves-the-pour-in-body-memory-and-the-next-run-scoops-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:onFire true}
                                      :inventory [{:name "water_bucket" :count 1}]
                                      :blocks (floor 6)})]
          ;; the fire burns on after the pour (the server has not put it out yet); a hostile cuts the wait
          (on-place! p "water_bucket" (fn [s _] (swap! s assoc-in [:self :onFire] true)))
          (on-wait! p (fn [n _] ((get {1 #(core/cut! eng (core/holder eng) :test nil)} n (fn [])))))
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [{:pos {:x 0 :y 64 :z 0}}] (entries eng :extinguish-pour)) "the pour outlives the cut")
          (swap! (fake/state p) assoc-in [:self :onFire] false)
          (await (run-until-empty eng 3))
          (is (= [] (:list (core/state eng))))
          (is (= ["water_bucket" "bucket"] (mapv (comp :item call-args) (calls p "place"))) "scooped by the next run")
          (is (= [] (entries eng :extinguish-pour))))))))

(deftest every-ending-of-a-run-is-done-never-continue
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w] [[:stand {:self {:onFire true} :blocks (floor 8)}]
                           [:stuck {:self {:onFire true} :blocks {"0,63,0" "stone" "1,64,0" "fire"}}]
                           [:water {:self {:onFire true} :blocks (merge (floor 8) {"3,64,0" "water"})}]
                           [:bucket {:self {:onFire true} :inventory [{:name "water_bucket" :count 1}] :blocks (floor 6)}]]]
          (let [{:keys [eng seen]} (setup w)]
            (core/submit! eng '(jobs.survival.extinguish) {})
            (await (core/tick! eng))
            (is (= [] (:list (core/state eng))) label)
            (is (= [] (of-kind seen :yielded)) label)))))))

;; ------------------------------------------------------------------- how a run ends

(deftest a-body-that-burns-on-while-standing-ends-stopped-still-burning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:onFire true} :blocks (floor 8)})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= :still-burning (stopped-reason seen)))
          (is (= extinguish/max-stand-waits (count (calls p "wait"))) "one wait per stand pass"))))))

(deftest a-body-with-no-safe-cell-ends-stopped-stuck-after-three-failed-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:self {:onFire true} :blocks {"0,63,0" "stone" "1,64,0" "fire"}})]
          (core/submit! eng '(jobs.survival.extinguish) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= :stuck (stopped-reason seen)))
          (is (= 1 (count (of-kind seen :extinguish_stuck)))))))))

(defn ^:async pour-then-flee
  "A cut run poured at the origin, then the body (out of the fire) stands at x blocks away; runs to the end."
  [x]
  (let [{:keys [eng p seen] :as s} (setup {:self {:onFire true}
                                           :inventory [{:name "water_bucket" :count 1}]
                                           :blocks (floor 40)})]
    (on-place! p "water_bucket" (fn [s _] (swap! s assoc-in [:self :onFire] true)))
    (on-wait! p (fn [n _] ((get {1 #(core/cut! eng (core/holder eng) :test nil)} n (fn [])))))
    (core/submit! eng '(jobs.survival.extinguish) {})
    (await (core/tick! eng))
    (swap! (fake/state p) assoc-in [:self :onFire] false)
    (swap! (fake/state p) assoc-in [:self :pos] [x 64 0])
    (await (run-until-empty eng 3))
    s))

(deftest a-pour-left-within-scoop-reach-is-scooped-in-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (pour-then-flee extinguish/max-scoop-distance))]
          (is (= [] (entries eng :extinguish-pour)))
          (is (= ["water_bucket" "bucket"] (mapv (comp :item call-args) (calls p "place"))))
          (is (= 0 (count (calls p "moveTo"))) "no walk back")
          (is (= [] (of-kind seen :extinguish.scoop_failed))))))))

(deftest a-pour-left-just-past-scoop-reach-is-walked-back-to-and-scooped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (pour-then-flee (inc extinguish/max-scoop-distance)))]
          (is (= [] (entries eng :extinguish-pour)))
          (is (= ["water_bucket" "bucket"] (mapv (comp :item call-args) (calls p "place"))))
          (is (<= (u/dist (u/pos-of (.-pos (.self p))) {:x 0 :y 64 :z 0}) extinguish/max-scoop-distance) "walked back")
          (is (= [] (of-kind seen :extinguish.scoop_failed))))))))

(deftest a-pour-left-at-the-walk-back-limit-is-scooped-and-farther-is-dropped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [near (await (pour-then-flee extinguish/max-return-distance))
              far (await (pour-then-flee (inc extinguish/max-return-distance)))]
          (is (= ["water_bucket" "bucket"] (mapv (comp :item call-args) (calls (:p near) "place"))))
          (is (= ["water_bucket"] (mapv (comp :item call-args) (calls (:p far) "place"))) "no scoop attempted from afar")
          (is (= [] (entries (:eng far) :extinguish-pour)) "the entry is gone")
          (is (= 1 (count (of-kind (:seen far) :extinguish.scoop_failed)))))))))
