(ns engine.shelter-test
  "Surviving the night: the night-unsafe trigger and the shelter, sleep,
  log-out and dig-in jobs against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.fake :as fake]
            [engine.jobs.shelter :as sh]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def day-ms 1200000)
(def night 14000)
(def noon 1000)

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async tick-n [eng n]
  (loop [i 0]
    (when (< i n)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn arg-pos [call] (js->clj (.-pos (.-args call)) :keywordize-keys true))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn know-bed! [eng pos] (mem/write! (:store eng) :bed {:pos pos} mem/place-policy))

(defn pos-of [p] (js->clj (.-pos (.self p)) :keywordize-keys true))

(defn teleport! [p x y z] (fake/swap-self! p assoc :pos [x y z]))

(defn emitted [seen kind] (filterv #(= kind (:kind %)) @seen))

(def floor {"0,63,0" "dirt" "0,62,0" "stone" "0,61,0" "stone" "0,60,0" "stone"})
(def dirt-stack [{:name "dirt" :count 12}])
(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 10 :y 64 :z 0}})
(def awake {:id 6 :name "Sam" :kind "player" :pos {:x 10 :y 64 :z 0}})

(defn declined-events [seen reflex]
  (filterv #(= [:reflex :declined reflex] [(:source %) (:kind %) (:reflex %)]) @seen))

(def always-shelter
  {:name :always-shelter :job '(jobs.survival.shelter) :args {:roof-height 4}
   :persistence :cooldown :cooldown-s 10 :when (constantly true)})

(defn refuse-placing! [p]
  (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "no-support"})))

;; ------------------------------------------------------------------ trigger

(defn fires? [world args]
  (boolean ((:when (get triggers/all :night-unsafe)) (tu/fake world) {} args)))

(deftest night-unsafe-holds-at-night-in-the-open
  (are [world args held] (= held (fires? world args))
    {:time night} {} true
    {:time noon} {} false
    {:time night :blocks {"0,66,0" "stone"}} {} false
    {:time night :blocks {"0,68,0" "stone"}} {} false
    {:time night :blocks {"0,69,0" "stone"}} {:roof-height 5} false
    {:time night :blocks {"0,69,0" "stone"}} {} true
    {:time night :blocks {"0,66,0" "oak_leaves"}} {} true
    {:time night :blocks {"0,66,0" "stone"} :self {:pos {:x 1 :y 64 :z 0}}} {} true
    {:time night :self {:isSleeping true}} {} false))

(defn sleeping-nearby?
  ([world args] (sleeping-nearby? world args nil))
  ([world args bed]
   (let [{:keys [eng]} (setup {})]
     (when bed (know-bed! eng bed))
     (boolean ((:when (get triggers/all :player-sleeping-nearby)) (tu/fake world) (mem/view (:store eng)) args)))))

(defn sleeping-nearby-after-unsupported? [world]
  (let [{:keys [eng]} (setup {})]
    (mem/write! (:store eng) :log-out {:ms 0 :status "unsupported"} {:cap 10 :ttl day-ms})
    (boolean ((:when (get triggers/all :player-sleeping-nearby)) (tu/fake world) (mem/view (:store eng)) {}))))

(deftest player-sleeping-nearby-holds-at-night-with-a-sleeper-and-no-bed
  (are [world args bed held] (= held (sleeping-nearby? world args bed))
    {:time night :entities [sleeper]} {} nil true
    {:time night :entities [sleeper] :blocks {"0,66,0" "stone"}} {} nil true
    {:time noon :entities [sleeper]} {} nil false
    {:time night :entities [awake]} {} nil false
    {:time night} {} nil false
    {:time night :entities [sleeper]} {} {:x 6 :y 64 :z 0} false
    {:time night :entities [sleeper]} {} {:x 80 :y 64 :z 0} true
    {:time night :entities [sleeper]} {:bed-radius 100} {:x 80 :y 64 :z 0} false
    {:time night :entities [sleeper]} {:offline-allowed false} nil false
    {:time night :entities [sleeper]} {:player-radius 5} nil false))

(deftest player-sleeping-nearby-does-not-hold-after-an-unsupported-log-out
  (is (false? (sleeping-nearby-after-unsupported? {:time night :entities [sleeper]}))))

(defn sleeping-nearby-after-log-out-ago?
  ([ago-s args] (sleeping-nearby-after-log-out-ago? ago-s args "returned"))
  ([ago-s args status]
  (let [{:keys [eng clock]} (setup {})]
    (mem/write! (:store eng) :log-out {:ms 20000 :status status} {:cap 10 :ttl day-ms})
    (swap! clock + (* 1000 ago-s))
    (boolean ((:when (get triggers/all :player-sleeping-nearby))
              (tu/fake {:time night :entities [sleeper]}) (mem/view (:store eng)) args)))))

(deftest a-failed-log-out-does-not-block-but-unsupported-is-permanent
  (are [status ago-s held] (= held (sleeping-nearby-after-log-out-ago? ago-s {} status))
    "closed" 10 true
    "cut" 10 true
    "unsupported" 31 false
    "unsupported" 1100 false))

(deftest player-sleeping-nearby-is-registered-and-fires-log-out
  (let [t (get triggers/all :player-sleeping-nearby)]
    (is (= '(jobs.survival.log-out {:offline-ms 20000}) (:job t)))
    (is (= [:cooldown 30] ((juxt :persistence :cooldown-s) t)))
    (is (= {:player-radius 128 :bed-radius 48 :offline-allowed true} (:args t)))))

(deftest a-roofed-body-still-logs-out-for-a-sleeper-from-the-register
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :blocks {"0,66,0" "stone"}})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe} {:trigger :player-sleeping-nearby}]}"))
          (await (tick-n eng 3))
          (is (= 1 (count (calls p "offline"))) "night-unsafe is silent under the roof, the log-out reflex fired")
          (is (= 1 (count (entries eng :log-out)))))))))

(deftest night-unsafe-ignores-a-built-shelter-by-day
  (let [{:keys [eng]} (setup {})]
    (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built} {:cap 10 :ttl day-ms})
    (is (not (boolean ((:when (get triggers/all :night-unsafe)) (tu/fake {:time noon}) (mem/view (:store eng)) {})))
        "night-only: a built shelter at day does not hold it")))

(deftest night-unsafe-is-registered-and-fires-shelter
  (let [t (get triggers/all :night-unsafe)]
    (is (= '(jobs.survival.shelter) (:job t)))
    (is (= 4 (get-in t [:args :roof-height])))
    (is (= (dissoc t :name) (dissoc (get triggers/all :night-and-bed-known) :name)) "the old name is an alias")))

;; -------------------------------------------------------------------- sleep

(deftest sleep-goes-to-the-bed-sleeps-and-writes-slept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 6))
          (is (= [{:x 6 :y 64 :z 0}] (mapv arg-pos (calls p "sleep"))))
          (is (= ["steer" "sleep"] (mapv #(.-name %) (.-calls (.-world p)))) "walked first, then slept")
          (is (= [{:pos {:x 6 :y 64 :z 0}}] (entries eng :slept)))
          (is (= {:cap 10 :ttl (* 7 day-ms)} (mem/policy (mem/view (:store eng)) :slept))))))))

(deftest sleep-declines-in-the-day-and-beyond-the-bed-radius
  (are [world bed]
       (let [{:keys [eng]} (setup world)]
         (know-bed! eng bed)
         (core/submit! eng '(jobs.survival.sleep) {})
         (nil? (core/tick! eng)))
    {:time noon :blocks {"6,64,0" "red_bed"}} {:x 6 :y 64 :z 0}
    {:time night :blocks {"60,64,0" "red_bed"}} {:x 60 :y 64 :z 0}))

(deftest sleep-retracts-a-bed-that-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 6))
          (is (= [] (entries eng :slept)))
          (is (nil? (mem/place (mem/view (:store eng)) :bed)) "the bed is no longer a known place")
          (is (= [{:gone true :was {:x 6 :y 64 :z 0}}] (entries eng :bed)))
          (is (= 1 (count (emitted seen :bed_missing)))))))))

(deftest sleep-keeps-a-bed-whose-chunk-is-not-loaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:time night :unloaded ["6,64,0"]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 10))
          (is (= [{:x 6 :y 64 :z 0}] (mapv :pos (entries eng :bed))) "the bed is still remembered")
          (is (= {:x 6 :y 64 :z 0} (mem/place (mem/view (:store eng)) :bed)))
          (is (= [] (emitted seen :bed_missing)))
          (is (= 1 (count (emitted seen :bed_unloaded)))))))))

(deftest sleep-gives-up-on-an-unreachable-bed-after-three-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"6,64,0" "red_bed"} :unreachable ["6,64,0"]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (is (< (await (run-until-empty eng 20)) 20))
          (is (= [] (calls p "sleep")))
          (is (= 9 (count (tu/walked-to eng))) "each of the three tries walks go-to afresh, three blocked walks each")
          (is (= 1 (count (emitted seen :bed_unreachable)))))))))

(def unreachable-world {:time night :blocks {"6,64,0" "red_bed"} :unreachable ["6,64,0"]})

(deftest sleep-remembers-an-unreachable-bed-for-ten-minutes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock seen]} (setup unreachable-world)]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 20))
          (is (= [{:pos {:x 6 :y 64 :z 0}}] (entries eng :bed-unreachable)))
          (is (= {:cap 5 :ttl 600000} (mem/policy (mem/view (:store eng)) :bed-unreachable)))
          (let [gave-up (count (emitted seen :unreachable))]
            (core/submit! eng '(jobs.survival.sleep) {})
            (is (nil? (core/tick! eng)) "declines while the entry is fresh")
            (is (= gave-up (count (emitted seen :unreachable))))
            (swap! clock + 600001)
            (await (tick-n eng 3))
            (is (< gave-up (count (emitted seen :unreachable))) "tries the bed again once the entry expired: three more fruitless go-to rounds")))))))

(deftest sleep-still-tries-a-different-bed-than-the-unreachable-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks {"7,64,0" "red_bed"}})]
          (mem/write! (:store eng) :bed-unreachable {:pos {:x 6 :y 64 :z 0}} {:cap 5 :ttl 600000})
          (know-bed! eng {:x 7 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 6))
          (is (= 1 (count (calls p "sleep")))))))))

;; ----------------------------------------------------------------- log-out

(deftest log-out-goes-offline-when-another-player-sleeps-and-no-bed-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper]})]
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (run-until-empty eng 3))
          (is (= [475050] (mapv #(.-ms (.-args %)) (calls p "offline"))) "until morning by default")
          (is (= [{:ms 475050 :status "ok"}] (entries eng :log-out)))
          (is (= {:cap 10 :ttl day-ms} (mem/policy (mem/view (:store eng)) :log-out))))))))

(deftest log-out-declines-without-a-sleeper-with-a-bed-or-when-not-allowed
  (are [world bed? spec]
       (let [{:keys [eng]} (setup world)]
         (when bed? (know-bed! eng {:x 6 :y 64 :z 0}))
         (core/submit! eng spec {})
         (nil? (core/tick! eng)))
    {:time night :entities [awake]} false '(jobs.survival.log-out)
    {:time night} false '(jobs.survival.log-out)
    {:time noon :entities [sleeper]} false '(jobs.survival.log-out)
    {:time night :entities [sleeper] :blocks {"6,64,0" "red_bed"}} true '(jobs.survival.log-out)
    {:time night :entities [sleeper]} false '(jobs.survival.log-out {:offline-allowed false})))

;; ------------------------------------------------------------------ dig-in

(deftest dig-in-walls-the-body-in-from-carried-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack :blocks floor})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (core/tick! eng))
          (is (<= (count (calls p "place")) 4) "one round places only a few blocks")
          (is (<= (await (run-until-empty eng 6)) 3) "a few rounds of a few placements")
          (is (= 10 (count (calls p "place"))) "four sides at feet and head height, a roof support, the roof")
          (is (= [] (calls p "dig")))
          (is (= "dirt" (.-name (.blockAt p (tu/pos 0 66 0)))) "the roof")
          (is (= "dirt" (.-name (.blockAt p (tu/pos 1 65 0)))) "a wall at head height")
          (is (= [{:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built
                   :door [{:x 1 :y 64 :z 0} {:x 1 :y 65 :z 0}]}]
                 (entries eng :shelter)))
          (is (= {:cap 10 :ttl day-ms} (mem/policy (mem/view (:store eng)) :shelter))))))))

(deftest dig-in-skips-sides-that-are-already-solid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack :blocks (assoc floor "1,64,0" "stone" "0,64,1" "stone")})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 6))
          (is (= 8 (count (calls p "place")))))))))

(deftest dig-in-digs-down-three-on-flat-ground-and-roofs-with-a-dug-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks floor})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= [{:x 0 :y 63 :z 0} {:x 0 :y 62 :z 0} {:x 0 :y 61 :z 0}] (mapv arg-pos (calls p "dig"))))
          (is (= {:x 0 :y 61 :z 0} (pos-of p)) "three blocks down")
          (is (= [{:x 0 :y 63 :z 0}] (mapv arg-pos (calls p "place"))) "the roof goes in the ground layer, beside solid ground")
          (is (= [{:pos {:x 0 :y 61 :z 0} :roof {:x 0 :y 63 :z 0} :start {:x 0 :y 64 :z 0} :state :built}] (entries eng :shelter))))))))

(deftest dig-in-digs-down-two-and-roofs-at-the-start-cell-beside-a-solid-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks (assoc floor "1,64,0" "stone")})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= 2 (count (calls p "dig"))))
          (is (= [{:x 0 :y 64 :z 0}] (mapv arg-pos (calls p "place"))) "the roof goes where the body stood"))))))

(deftest dig-in-with-few-carried-blocks-digs-down-and-roofs-with-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :drops {"iron_ore" "raw_iron"} :inventory [{:name "cobblestone" :count 1}]
                                      :blocks {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "iron_ore" "0,60,0" "stone"}})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= 3 (count (calls p "dig"))))
          (is (= "cobblestone" (.-item (.-args (first (calls p "place"))))) "the carried block is used first"))))))

(deftest dig-in-stops-after-one-dig-when-nothing-can-roof-the-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :drops {"iron_ore" "raw_iron"}
                                           :blocks {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "stone"}})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= 1 (count (calls p "dig"))) "no second dig")
          (is (= [] (calls p "place")))
          (is (= 1 (count (emitted seen :dig_in_failed))))
          (is (re-find #"nothing to roof the pit with" (:text (first (emitted seen :dig_in_failed)))))
          (is (= [] (:list (core/state eng))) "ended"))))))

(deftest dig-in-walls-converge-on-the-current-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory [{:name "dirt" :count 30}] :blocks floor})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (core/tick! eng))
          (teleport! p 5 64 0)
          (await (run-until-empty eng 8))
          (is (= "dirt" (.-name (.blockAt p (tu/pos 5 66 0)))) "roof over where the body now is")
          (is (= {:x 5 :y 64 :z 0} (:pos (last (entries eng :shelter)))))
          (is (= {:x 5 :y 66 :z 0} (:roof (last (entries eng :shelter))))))))))

(deftest dig-in-dig-mode-rechooses-when-the-body-moved-off-its-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks (merge floor {"5,63,0" "stone" "5,62,0" "stone" "5,61,0" "stone" "5,60,0" "stone" "6,64,0" "stone"})})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (core/tick! eng))
          (teleport! p 5 64 0)
          (await (run-until-empty eng 10))
          (is (= [{:x 5 :y 64 :z 0}] (mapv arg-pos (calls p "place"))) "roof at the new column's start")
          (is (= {:x 5 :y 62 :z 0} (pos-of p)))
          (is (= {:x 5 :y 64 :z 0} (:roof (last (entries eng :shelter))))))))))

(deftest dig-in-gives-up-on-an-unplaceable-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory dirt-stack :blocks floor})]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "no-support"}))
          (core/submit! eng '(jobs.survival.dig-in) {})
          (is (<= (await (run-until-empty eng 10)) 5) "bounded")
          (is (= 1 (count (emitted seen :dig_in_failed))))
          (is (= [{:pos {:x 0 :y 64 :z 0} :state :built}] (entries eng :shelter)) "records that it stopped, unroofed"))))))

(deftest dig-in-walls-place-a-support-before-the-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory dirt-stack
                                           :blocks (merge floor {"1,63,0" "stone" "-1,63,0" "stone" "0,63,1" "stone" "0,63,-1" "stone"})})
              supported? (fn [pos] (some #(let [n (.-name (.blockAt p (tu/pos (+ (.-x pos) (first %)) (+ (.-y pos) (second %)) (+ (.-z pos) (nth % 2)))))]
                                            (not (#{"air" "cave_air"} n)))
                                         [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))]
          (.override (.-world p) "place"
                     (fn ^:async f [_ args impl]
                       (if (supported? (.-pos args))
                         (await (impl _ args))
                         #js {:status "no-support"})))
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= [{:x 1 :y 66 :z 0} {:x 0 :y 66 :z 0}] (mapv arg-pos (take-last 2 (calls p "place")))) "support, then roof")
          (is (= [] (emitted seen :dig_in_failed)))
          (is (= [{:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built
                   :door [{:x 1 :y 64 :z 0} {:x 1 :y 65 :z 0}]}]
                 (entries eng :shelter))))))))

;; the fake's place does not check support; this one refuses a cell with no solid neighbour, as the real one does
(defn require-support! [p]
  (let [supported? (fn [pos] (some #(let [n (.-name (.blockAt p (tu/pos (+ (.-x pos) (first %)) (+ (.-y pos) (second %)) (+ (.-z pos) (nth %  2)))))]
                                      (not (#{"air" "cave_air"} n)))
                                   [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))]
    (.override (.-world p) "place"
               (fn ^:async f [_ args impl]
                 (if (supported? (.-pos args))
                   (await (impl _ args))
                   #js {:status "no-support"})))))

(deftest dig-in-dig-mode-on-flat-ground-roofs-in-the-ground-layer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 3}] :blocks floor})]
          (require-support! p)
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 12))
          (is (= [] (emitted seen :dig_in_failed)))
          (is (= {:x 0 :y 61 :z 0} (pos-of p)) "three down")
          (is (= "dirt" (.-name (.blockAt p (tu/pos 0 63 0)))) "roof flush with the ground, beside solid ground")
          (is (= {:x 0 :y 63 :z 0} (:roof (last (entries eng :shelter))))))))))

(deftest dig-in-does-not-dig-where-nothing-can-be-roofed-against
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 3}]
                                           :blocks (assoc floor "1,63,0" "water" "-1,63,0" "water" "0,63,1" "water" "0,63,-1" "water")})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 12))
          (is (= [] (calls p "dig")) "floor intact")
          (is (= 1 (count (emitted seen :dig_in_failed))))
          (is (= :no-roof-support (:reason (first (entries eng :dig-in-futile))))))))))

(deftest dig-in-will-not-dig-through-a-thin-floor-into-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"0,63,0" "stone" "0,62,0" "water"}})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")))
          (is (= 1 (count (emitted seen :dig_in_failed))))
          (is (re-find #"water" (:text (first (emitted seen :dig_in_failed))))))))))

(deftest dig-in-will-not-dig-over-an-air-gap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"0,63,0" "stone"}})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")))
          (is (= 1 (count (emitted seen :dig_in_failed)))))))))

;; ----------------------------------------------------------------- shelter

(deftest shelter-prefers-a-bed-over-logging-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :players ["Alex"] :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 8))
          (is (= 1 (count (calls p "sleep"))))
          (is (= [] (calls p "offline")))
          (is (= 1 (count (entries eng :slept)))))))))

(deftest ms-until-morning-counts-to-the-end-of-the-night-plus-a-margin
  (are [t ms] (= ms (sh/ms-until-morning t))
    14000 475050
    12542 547950
    20000 175050
    23460 2050
    23461 0
    1000 0
    0 0))

(deftest shelter-digs-in-when-alone-on-the-server
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack :blocks floor})]
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (tick-n eng 8))
          (is (= [] (calls p "offline")) "nobody else online: no log-out")
          (is (= 10 (count (calls p "place"))) "dug in")
          (is (= [] (entries eng :log-out))))))))

(def ms-of-offline (fn [p] (mapv #(.-ms (.-args %)) (calls p "offline"))))

(deftest shelter-logs-out-until-morning-when-another-player-is-online-and-no-bed-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :players ["Alex"] :inventory dirt-stack :blocks floor})]
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 8))
          (is (= [475050] (ms-of-offline p)) "away until the night is over")
          (is (= [{:ms 475050 :status "ok"}] (entries eng :log-out)))
          (is (= [] (calls p "place")) "did not dig in")
          (is (true? (.-isDay (.self p))) "back by day")
          (is (= [] (:list (core/state eng))) "the shelter ended at day"))))))

(deftest shelter-logs-out-again-when-it-comes-back-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :players ["Alex"] :inventory dirt-stack :blocks floor})
              n (atom 0)]
          (.override (.-world p) "offline"
                     (fn ^:async f [token a impl]
                       (let [r (await (impl token a))]
                         (when (= 1 (swap! n inc)) (.setTime (.-world p) 20000))
                         r)))
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 12))
          (is (= [475050 175050] (ms-of-offline p)) "the second log-out lasts the rest of the night")
          (is (= [] (calls p "place")))
          (is (= [] (:list (core/state eng)))))))))

(deftest shelter-digs-in-when-the-log-out-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :players ["Alex"] :inventory dirt-stack :blocks floor})]
          (.override (.-world p) "offline" (fn [_ _ _] #js {:status "unsupported"}))
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (tick-n eng 8))
          (is (= 1 (count (calls p "offline"))) "tried once; unsupported is not tried again")
          (is (= ["unsupported"] (mapv :status (entries eng :log-out))))
          (is (= 10 (count (calls p "place"))) "fell through to dig-in"))))))

(deftest shelter-falls-through-a-missing-bed-to-the-next-choice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 8))
          (is (= [{:gone true :was {:x 6 :y 64 :z 0}}] (entries eng :bed)))
          (is (= [] (calls p "offline")) "nobody else online: no log-out"))))))

(defn notified [seen] (count (emitted seen :job.notify)))

(deftest a-dug-in-shelter-holds-the-body-until-day-then-the-queued-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory dirt-stack :blocks floor :entities [awake]})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (tick-n eng 20))
          (is (= 10 (count (calls p "place"))) "dug in")
          (is (= [] (calls p "dig")))
          (is (= [:built] (mapv :state (entries eng :shelter))))
          (is (some? (:pending-reflex (core/state eng))) "the shelter reflex is still running at night")
          (is (zero? (notified seen)) "the queued job does not start before day")
          (.setTime (.-world p) noon)
          (await (tick-n eng 6))
          (is (nil? (:pending-reflex (core/state eng))) "the shelter ended at day")
          (is (not= {:x 0 :y 64 :z 0} (pos-of p)) "the body stepped out of its walls")
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(def ground
  "Dirt (no tool needed) from y 60 to 63 over x and z -4..4: room for a pit and a stair out of it."
  (into {} (for [x (range -4 5) z (range -4 5) y (range 60 64)] [(str x "," y "," z) "dirt"])))

(deftest a-pit-shelter-holds-until-day-then-ends-with-the-body-out-of-the-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks ground})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (tick-n eng 20))
          (is (< (:y (pos-of p)) 64) "in the pit")
          (is (some? (:pending-reflex (core/state eng))) "the shelter holds the body at night")
          (is (zero? (notified seen)))
          (.setTime (.-world p) noon)
          (await (tick-n eng 40))
          (is (nil? (:pending-reflex (core/state eng))) "the shelter ended at day")
          (is (>= (:y (pos-of p)) 64) "the body stands on the surface, out of the pit")
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(deftest a-pit-shelter-with-no-way-out-keeps-holding-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone (into {} (map (fn [[k _]] [k "stone"])) ground)
              {:keys [eng p seen]} (setup {:time night :blocks stone})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe}]}"))
          (await (tick-n eng 20))
          (is (< (:y (pos-of p)) 64) "in the pit")
          (.setTime (.-world p) noon)
          (await (tick-n eng 40))
          (is (seq (emitted seen :dig-in.trapped)) "no pickaxe for the stone stair: trapped")
          (is (some? (:pending-reflex (core/state eng))) "the shelter does not end until the body is out"))))))

(deftest a-sleeping-shelter-holds-until-day-then-the-queued-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :skipNight false :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (tick-n eng 20))
          (is (true? (.-isSleeping (.self p))) "asleep in the bed, the night not skipped")
          (is (zero? (notified seen)) "the queued job does not wake the body before day")
          (.setTime (.-world p) noon)
          (await (tick-n eng 6))
          (is (= 1 (notified seen))))))))

(deftest shelter-ends-without-acting-when-already-roofed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"0,66,0" "stone"}})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (core/tick! eng))
          (is (= [] (declined-events seen :always-shelter)) "done, not declined")
          (is (zero? (count (.-calls (.-world p))))))))))

(deftest shelter-ends-at-day-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time noon})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (core/tick! eng))
          (is (= [] (declined-events seen :always-shelter)))
          (is (zero? (count (.-calls (.-world p))))))))))

(defn walks-to-bed [eng]
  (count (filter #(= {:x 6 :y 64 :z 0} %) (tu/walked-to eng))))

(deftest shelter-does-not-retry-a-sleep-that-already-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory dirt-stack :blocks floor :entities [awake]
                                           :unreachable ["6,64,0"]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.shelter) {})
          (loop [i 0]
            (when (and (< i 60) (empty? (emitted seen :bed_unreachable)))
              (await (core/tick! eng))
              (recur (inc i))))
          (is (= 1 (count (emitted seen :bed_unreachable))))
          (let [walks (walks-to-bed eng)]
            (await (tick-n eng 12))
            (is (= walks (walks-to-bed eng)) "no more walking at the unreachable bed")
            (is (= 10 (count (calls p "place"))) "dig-in finished the walls")))))))

(deftest shelter-treats-a-long-awake-body-as-needing-a-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:floor [-30 -10 90 10] :time night :blocks {"80,64,0" "red_bed"}})]
          (know-bed! eng {:x 80 :y 64 :z 0})
          (mem/write! (:store eng) :slept {:pos {:x 0 :y 64 :z 0}} {:cap 10 :ttl (* 7 day-ms)})
          (reset! clock (+ 1000000 (* 4 day-ms)))
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 10))
          (is (= 1 (count (calls p "sleep"))) "an 80 block walk is worth it after four days awake")
          (is (= 1 (count (emitted seen :needs_bed)))))))))

(deftest shelter-does-not-walk-that-far-when-rested
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks (merge floor {"80,64,0" "red_bed"})})]
          (know-bed! eng {:x 80 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 3))
          (is (= [] (calls p "sleep"))))))))

(deftest night-unsafe-fires-shelter-from-the-register
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe}]}"))
          (await (tick-n eng 4))
          (is (= 1 (count (calls p "sleep")))))))))

;; ------------------------------------------------- declining when stuck

(def shelter-policy {:cap 10 :ttl day-ms})

(deftest shelter-declines-when-dig-in-ends-without-a-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory dirt-stack :blocks floor})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 8))
          (is (= 1 (count (declined-events seen :always-shelter))))
          (is (nil? (core/running eng))))))))

(deftest shelter-declines-when-the-pit-cannot-be-roofed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:time night :drops {"iron_ore" "raw_iron"}
                                         :blocks {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "stone"}})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 6))
          (is (= 1 (count (declined-events seen :always-shelter)))))))))

(deftest a-declined-shelter-reflex-lets-a-lower-reflex-get-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :self {:food 5} :blocks floor
                                           :inventory [{:name "dirt" :count 12} {:name "bread" :count 3}]})]
          (refuse-placing! p)
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe} {:trigger :hungry}]}"))
          (await (tick-n eng 8))
          (is (seq (declined-events seen :night-unsafe)) "the shelter reflex was dropped with reflex.declined")
          (is (< 5 (.-food (.self p))) "hungry got the body and ate"))))))

(deftest a-failed-unroofed-shelter-at-night-does-not-throw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:time night :blocks floor})]
          (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :state :built} shelter-policy)
          (core/register-reflex! eng {:trigger :night-unsafe})
          (await (tick-n eng 4))
          (is (not (nil? eng))))))))

(deftest shelter-retries-dig-in-at-night-beside-an-unroofed-shelter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks floor :inventory [{:name "dirt" :count 16}]})]
          (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :state :built} shelter-policy)
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (tick-n eng 4))
          (is (seq (calls p "place")) "dig-in was tried again, not a bare decline"))))))

(def futile-world
  {:time night :drops {"iron_ore" "raw_iron"}
   :blocks {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "stone"}})

(deftest a-futile-pit-is-not-dug-again-on-the-next-firing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup futile-world)
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 6))
          (is (= [{:pos {:x 0 :y 63 :z 0}}] (entries eng :dig-in-futile)))
          (swap! clock + 11000)
          (await (tick-n eng 6))
          (is (= 1 (count (calls p "dig"))) "the second firing does not dig")
          (is (= 2 (count (declined-events seen :always-shelter)))))))))

(deftest a-futile-pit-does-not-stop-a-body-that-carries-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc futile-world :inventory dirt-stack))]
          (mem/write! (:store eng) :dig-in-futile {:pos {:x 0 :y 64 :z 0}} {:cap 5 :ttl 600000})
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (seq (calls p "place"))))))))

(deftest dig-in-refuses-lateral-fluid-before-opening-a-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [fluid ["water" "lava"]]
          (let [{:keys [eng p]} (setup {:time night :blocks (assoc floor "1,63,0" fluid)
                                      :inventory [{:name "dirt" :count 1}]})]
            (core/submit! eng '(jobs.survival.dig-in) {})
            (await (run-until-empty eng 8))
            (is (empty? (calls p "dig")) "the fluid barrier remains intact")
            (is (empty? (calls p "place")) "no roof traps the body beside fluid")
            (core/submit! eng '(jobs.survival.dig-in) {})
            (is (nil? (core/tick! eng)) "carried dirt does not make the site safe")
            (is (= :fluid-adjacent (:reason (first (entries eng :dig-in-futile)))))))))))

(deftest dig-in-bounds-descent-without-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [status ["blocked" "arrived"]]
          (let [{:keys [eng p seen]} (setup {:time night :blocks (assoc floor "0,63,0" "air")})]
            (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status status}))
            (core/submit! eng '(jobs.survival.dig-in) {})
            (is (< (await (run-until-empty eng 10)) 10))
            (is (= 3 (count (calls p "moveTo"))) "measure descent, even when the primitive says arrived")
            (is (= 1 (count (emitted seen :dig_in_failed))))
            (is (= :descent-stalled (:reason (first (entries eng :dig-in-futile)))))
            (core/submit! eng '(jobs.survival.dig-in) {})
            (is (nil? (core/tick! eng)))))))))

(deftest a-failed-roof-does-not-deepen-the-pit-on-refiring
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:time night :blocks floor})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 8))
          (is (= 3 (count (calls p "dig"))))
          (is (= :roof-failed (:reason (first (entries eng :dig-in-futile)))))
          (swap! clock + 11000)
          (await (tick-n eng 8))
          (is (= 3 (count (calls p "dig"))) "fresh reflex cannot excavate another pit")
          (is (= 3 (count (calls p "place"))) "failed roof is not retried at the same site"))))))

(deftest unsupported-walls-are-not-retried-on-every-reflex-firing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:time night :blocks floor :inventory dirt-stack})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 8))
          (is (= :walls-failed (:reason (first (entries eng :dig-in-futile)))))
          (swap! clock + 11000)
          (await (tick-n eng 8))
          (is (= 3 (count (calls p "place"))))
          (is (empty? (calls p "dig"))))))))

(deftest needs-bed-is-warned-once-across-firings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:time night :inventory dirt-stack :blocks floor})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (mem/write! (:store eng) :slept {:pos {:x 0 :y 64 :z 0}} {:cap 10 :ttl (* 7 day-ms)})
          (swap! clock + (* 4 day-ms))
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 6))
          (swap! clock + 11000)
          (await (tick-n eng 6))
          (is (= 2 (count (declined-events seen :always-shelter))))
          (is (= 1 (count (emitted seen :needs_bed)))))))))

(deftest dig-in-treats-an-occupied-non-solid-cell-as-sealed-and-does-not-retry-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack
                                      :blocks (assoc floor "1,66,0" "oak_leaves" "0,66,0" "oak_leaves")})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (is (<= (await (run-until-empty eng 8)) 5) "the job ends instead of looping")
          (is (= [] (:list (core/state eng))))
          (is (<= (count (filter #(contains? #{[1 66 0] [0 66 0]} [(:x (arg-pos %)) (:y (arg-pos %)) (:z (arg-pos %))])
                                 (calls p "place")))
                  2)
              "each occupied cell is tried at most once"))))))

(deftest dig-in-clears-a-passable-occupied-cell-and-seals-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [passable ["torch" "oak_sapling"]]
          (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack
                                        :blocks (assoc floor "1,64,0" passable "1,65,0" passable)})]
            (core/submit! eng '(jobs.survival.dig-in) {})
            (is (<= (await (run-until-empty eng 10)) 8) (str passable ": the job ends"))
            (is (= [] (:list (core/state eng))))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 1 64 0)))) (str passable " at feet height is walled, not left a hole"))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 1 65 0)))) (str passable " at head height is walled"))))))))

(deftest dig-in-ends-when-a-passable-cell-cannot-be-cleared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack :blocks (assoc floor "1,64,0" "torch")})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "unreachable"}))
          (core/submit! eng '(jobs.survival.dig-in) {})
          (is (<= (await (run-until-empty eng 12)) 10) "the job still ends")
          (is (= [] (:list (core/state eng)))))))))

(deftest dig-in-walls-over-blocks-a-mob-walks-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [passable ["oak_sign" "oak_wall_sign" "rail" "stone_pressure_plate" "stone_button" "lever" "white_carpet"]]
          (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack
                                        :blocks (assoc floor "1,64,0" passable "0,65,1" passable "0,66,0" passable)})]
            (core/submit! eng '(jobs.survival.dig-in) {})
            (is (<= (await (run-until-empty eng 12)) 10) (str passable ": the job ends"))
            (is (= [] (:list (core/state eng))))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 1 64 0)))) (str passable " in a feet wall cell is walled"))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 0 65 1)))) (str passable " in a head wall cell is walled"))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 0 66 0)))) (str passable " in the roof cell is walled"))))))))

(deftest dig-in-keeps-blocks-that-already-stop-a-mob
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack
                                      :blocks (assoc floor "1,64,0" "oak_fence" "0,64,1" "glass" "-1,64,0" "iron_bars")})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")))
          (is (= ["oak_fence" "glass" "iron_bars"]
                 (mapv #(.-name (.blockAt p (apply tu/pos %))) [[1 64 0] [0 64 1] [-1 64 0]]))))))))
