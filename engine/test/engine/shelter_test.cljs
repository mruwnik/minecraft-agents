(ns engine.shelter-test
  "Surviving the night: the night-unsafe trigger and the shelter, sleep,
  log-out and dig-in jobs against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def day-ms 1200000)
(def night 14000)
(def noon 1000)

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001} world))
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

(defn emitted [seen kind] (filterv #(= kind (:kind %)) @seen))

(def floor {"0,63,0" "dirt" "0,62,0" "stone" "0,61,0" "stone"})
(def dirt-stack [{:name "dirt" :count 12}])
(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 10 :y 64 :z 0}})
(def awake {:id 6 :name "Sam" :kind "player" :pos {:x 10 :y 64 :z 0}})

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
          (is (= ["moveTo" "sleep"] (mapv #(.-name %) (.-calls (.-world p)))) "walked first, then slept")
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

(deftest sleep-gives-up-on-an-unreachable-bed-after-three-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"6,64,0" "red_bed"} :unreachable ["6,64,0"]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (is (< (await (run-until-empty eng 20)) 20))
          (is (= [] (calls p "sleep")))
          (is (= 9 (count (calls p "moveTo"))) "each of the three tries walks go-to afresh, three blocked walks each")
          (is (= 1 (count (emitted seen :bed_unreachable)))))))))

;; ----------------------------------------------------------------- log-out

(deftest log-out-goes-offline-when-another-player-sleeps-and-no-bed-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper]})]
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (run-until-empty eng 3))
          (is (= [300000] (mapv #(.-ms (.-args %)) (calls p "offline"))) "five minutes by default")
          (is (= [{:ms 300000 :status "ok"}] (entries eng :log-out)))
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

(deftest dig-in-digs-down-two-and-roofs-with-a-dug-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks floor})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= [{:x 0 :y 63 :z 0} {:x 0 :y 62 :z 0}] (mapv arg-pos (calls p "dig"))))
          (is (= {:x 0 :y 62 :z 0} (pos-of p)) "two blocks down")
          (is (= [{:x 0 :y 64 :z 0}] (mapv arg-pos (calls p "place"))) "the roof goes where the body stood")
          (is (= [{:pos {:x 0 :y 62 :z 0} :roof {:x 0 :y 64 :z 0} :state :built}] (entries eng :shelter))))))))

(deftest dig-in-with-few-carried-blocks-digs-down-and-roofs-with-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :drops {"iron_ore" "raw_iron"} :inventory [{:name "cobblestone" :count 1}]
                                      :blocks {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "stone"}})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= 2 (count (calls p "dig"))))
          (is (= "cobblestone" (.-item (.-args (first (calls p "place"))))) "the carried block is used first"))))))

(deftest dig-in-without-any-placeable-block-just-digs-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :drops {"iron_ore" "raw_iron"}
                                      :blocks {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "stone"}})]
          (core/submit! eng '(jobs.survival.dig-in) {})
          (await (run-until-empty eng 8))
          (is (= 2 (count (calls p "dig"))))
          (is (= [] (calls p "place")))
          (is (= {:x 0 :y 62 :z 0} (pos-of p)))
          (is (= [{:pos {:x 0 :y 62 :z 0} :state :built}] (entries eng :shelter)) "no roof to reopen"))))))

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
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 8))
          (is (= 1 (count (calls p "sleep"))))
          (is (= [] (calls p "offline")))
          (is (= 1 (count (entries eng :slept)))))))))

(deftest shelter-logs-out-when-no-bed-and-another-player-sleeps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :inventory dirt-stack :blocks floor})]
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 8))
          (is (= 1 (count (calls p "offline"))))
          (is (= [] (calls p "place")) "did not dig in")
          (is (= 1 (count (entries eng :log-out)))))))))

(deftest shelter-falls-through-a-missing-bed-to-the-next-choice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 8))
          (is (= [{:gone true :was {:x 6 :y 64 :z 0}}] (entries eng :bed)))
          (is (= 1 (count (calls p "offline"))) "the same round went on to log out"))))))

(deftest shelter-digs-in-then-waits-and-reopens-at-dawn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack :blocks floor :entities [awake]})]
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (tick-n eng 12))
          (is (= 10 (count (calls p "place"))))
          (is (= [] (calls p "dig")) "still walled in")
          (is (= 1 (count (:list (core/state eng)))) "still waiting for day")
          (is (= [:built] (mapv :state (entries eng :shelter))))
          (.setTime (.-world p) noon)
          (await (run-until-empty eng 4))
          (is (= [{:x 0 :y 66 :z 0} {:x 1 :y 64 :z 0} {:x 1 :y 65 :z 0}] (mapv arg-pos (calls p "dig")))
              "opened the roof and both door cells")
          (is (= "air" (.-name (.blockAt p (tu/pos 0 66 0)))))
          (is (= {:x 2 :y 64 :z 0} (arg-pos (last (calls p "moveTo")))) "walked out through the door, not to the roof")
          (is (= [:built :reopened] (mapv :state (entries eng :shelter))))
          (is (= [] (:list (core/state eng)))))))))

(defn walks-to-bed [p]
  (count (filter #(= {:x 6 :y 64 :z 0} (arg-pos %)) (calls p "moveTo"))))

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
          (let [walks (walks-to-bed p)]
            (await (tick-n eng 12))
            (is (= walks (walks-to-bed p)) "no more walking at the unreachable bed")
            (is (= 10 (count (calls p "place"))) "dig-in finished the walls")))))))

(deftest shelter-waits-without-new-actions-while-it-is-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory dirt-stack :blocks floor})]
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (tick-n eng 8))
          (let [n (count (.-calls (.-world p)))]
            (await (tick-n eng 5))
            (is (= n (count (.-calls (.-world p)))))))))))

(deftest shelter-treats-a-long-awake-body-as-needing-a-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:time night :blocks {"80,64,0" "red_bed"}})]
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

;; ------------------------------------------------- declining while it waits

(def shelter-policy {:cap 10 :ttl day-ms})

(defn write-shelter! [eng state]
  (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state state} shelter-policy))

(defn holds-with-memory? [eng world]
  (boolean ((:when (get triggers/all :night-unsafe)) (tu/fake world) (mem/view (:store eng)) {})))

(defn declined-events [seen reflex]
  (filterv #(= [:reflex :declined reflex] [(:source %) (:kind %) (:reflex %)]) @seen))

(def always-shelter
  {:name :always-shelter :job '(jobs.survival.shelter) :args {:roof-height 4}
   :persistence :cooldown :cooldown-s 10 :when (constantly true)})

(deftest shelter-declines-at-night-while-roofed-in-a-built-shelter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"0,66,0" "stone"}})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (write-shelter! eng :built)
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (core/tick! eng))
          (is (= 1 (count (declined-events seen :always-shelter))) "nothing to do until day: declined, not held")
          (is (nil? (core/running eng)))
          (is (= [] (calls p "dig"))))))))

(deftest night-unsafe-holds-at-day-only-with-a-built-shelter-here
  (let [{:keys [eng]} (setup {})
        roofed {:blocks {"0,66,0" "stone"}}]
    (is (not (holds-with-memory? eng {:time noon})) "no shelter")
    (write-shelter! eng :built)
    (is (holds-with-memory? eng {:time noon}) "built shelter at the body's place, day")
    (is (not (holds-with-memory? eng {:time noon :self {:pos {:x 5 :y 64 :z 0}}})) "body is elsewhere")
    (is (not (holds-with-memory? eng (assoc roofed :time night))) "night under a roof")
    (write-shelter! eng :reopened)
    (is (not (holds-with-memory? eng {:time noon})) "reopened")))

(deftest a-shelter-reflex-at-night-is-dropped-and-a-lower-reflex-gets-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :self {:food 5}
                                           :inventory [{:name "bread" :count 3}]})]
          (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :state :built} shelter-policy)
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe} {:trigger :hungry}]}"))
          (await (tick-n eng 6))
          (is (seq (declined-events seen :night-unsafe))
              "the shelter reflex was dropped with reflex.declined")
          (is (< 5 (.-food (.self p))) "hungry got the body and ate"))))))

(deftest a-failed-unroofed-shelter-at-night-does-not-throw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:time night :blocks floor})]
          (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :state :built} shelter-policy)
          (core/register-reflex! eng {:trigger :night-unsafe})
          (await (tick-n eng 4))
          (is (nil? (core/running eng))))))))

(deftest night-unsafe-fires-at-dawn-and-shelter-opens-the-built-shelter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time noon :blocks {"0,66,0" "stone"}})]
          (write-shelter! eng :built)
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night-unsafe}]}"))
          (await (tick-n eng 4))
          (is (= [{:x 0 :y 66 :z 0}] (mapv arg-pos (calls p "dig"))))
          (is (= :reopened (:state (last (entries eng :shelter))))))))))

(defn write-unroofed-shelter! [eng]
  (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :state :built} shelter-policy))

(deftest shelter-retries-dig-in-at-night-beside-an-unroofed-shelter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks floor :inventory [{:name "dirt" :count 16}]})]
          (write-unroofed-shelter! eng)
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (tick-n eng 4))
          (is (seq (calls p "place")) "dig-in was tried again, not a bare decline"))))))

(deftest shelter-reopens-an-unroofed-shelter-at-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time noon :blocks floor})]
          (write-unroofed-shelter! eng)
          (core/submit! eng '(jobs.survival.shelter) {})
          (await (run-until-empty eng 4))
          (is (= :reopened (:state (last (entries eng :shelter)))))
          (is (= [] (calls p "place")))
          (is (= [] (:list (core/state eng)))))))))
