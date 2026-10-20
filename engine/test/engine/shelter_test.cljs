(ns engine.shelter-test
  "Surviving the night: the night trigger and the night, sleep,
  log-out and dig-in jobs against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.scenario :as scenario]
            [engine.fake :as fake]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.night.sites :as sites]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.shelter-support :refer [walls-cheap day-ms night noon on-wait! dawn-after! mid-night! default-dawn setup tick-n night-then-day calls arg-pos entries know-bed! looked-round! pos-of teleport! emitted no-way-stops emitted-by floor bed-room dirt-stack wall-stack sleeper awake declined-events always-shelter refuse-placing! after-first! fires? ground stone-ground hard-ground ms-of-offline notified refuse-cell!]]
            [engine.triggers :as triggers]))

(deftest without-a-sky-nothing-is-dark-with-one-the-night-is
  (let [bare (doto (tu/fake {:time night}) (aset "rawWorld" nil))]
    (is (false? (sh-sense/dark-outside? bare)) "the clock is no sky: nothing is known, nothing is dark")
    (is (true? (sh-sense/dark-outside? (tu/fake {:time night}))))))

(deftest night-holds-at-night-in-the-open-or-under-a-roof-with-open-sides
  (are [world held] (= held (fires? world {}))
    {:time night} true
    {:time noon} false
    {:time night :blocks {"0,66,0" "stone"}} true
    {:time night :blocks {"0,66,0" "oak_leaves"}} true
    {:time night :self {:isSleeping true}} false))

(def stone-above
  "A column of stone from y+n for k blocks over the body at y 64."
  (fn [n k] (into {} (map (fn [dy] [(str "0," (+ 64 n dy) ",0") "stone"]) (range k)))))

(def dark {:light-default [0 0]})

(defn gave-ups
  "The go-to children that gave up (child_ended with the cause :unreachable)."
  [seen]
  (filterv #(= :unreachable (get-in % [:ending :cause :reason])) (emitted seen :child_ended)))

(defn seen-fake
  "A fake body looking up, wrapped in perception after one sight pass: it knows only what it has seen."
  [world]
  (let [raw-p (tu/fake (merge {:pitch -80} world))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
        p (perception/wrap raw-p per)]
    (perception/pass! per)
    p))

(def cave-blocks
  "A stone floor at y 63 and ceiling at y 70 over x, z -20..20 (1681 standable cells: more than a closed room holds), open at the sides."
  (into {} (for [x (range -20 21) z (range -20 21) y [63 70]] [(str x "," y "," z) "stone"])))

(defn seen-dark
  "A fake body with light data (as the night trigger reads it) that has seen every block."
  [blocks & [light extra]]
  (let [raw-p (tu/fake (merge {:time night :blocks blocks} extra))
        _ (swap! (fake/state raw-p) merge {:light-default [0 0]} (when light {:light light}))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})]
    (tu/seeing-all (perception/wrap raw-p per))))

(defn box
  "Stone walls, floor (y 63) and roof (y 66) round a w by d room of cells x 0..w-1, z 0..d-1 at y 64-65; open-sides? leaves the walls out."
  [w d open-sides?]
  (into (if open-sides? (into {} (for [x (range -20 (+ w 20)) z (range -20 (+ d 20))] [(str x ",63," z) "stone"])) {})
        (for [x (range -1 (inc w)) z (range -1 (inc d)) y [63 64 65 66]
                 :let [inside? (and (<= 0 x (dec w)) (<= 0 z (dec d)))]
                 :when (cond inside? (#{63 66} y) open-sides? (= y 66) :else true)]
             [(str x "," y "," z) "stone"])))

(defn seen-box [blocks light-default]
  (let [raw-p (tu/fake {:time night :blocks blocks})
        _ (swap! (fake/state raw-p) assoc :light-default light-default)
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})]
    (tu/seeing-all (perception/wrap raw-p per))))

(deftest a-closed-room-is-shelter-unless-a-dark-spawnable-cell-lies-beyond-the-spawn-free-radius
  (are [w d open-sides? light safe?] (= safe? (sh-inside/safe-from-night? (seen-box (box w d open-sides?) light)))
    5 5 false [0 14] true
    5 5 false [0 0] true
    1 1 false [0 0] true
    1 25 false [0 0] true
    1 26 false [0 0] false
    1 40 false [0 14] true
    5 5 true [0 14] false))

(deftest a-leaf-roof-keeps-mobs-out-like-stone
  (let [leaf-roof (into {} (map (fn [[k v]] [k (if (= k "0,66,0") "oak_leaves" v)])) (box 3 3 false))]
    (is (true? (sh-inside/safe-from-night? (seen-box leaf-roof [0 0]))))))

(deftest light-in-s-is-the-wait-until-the-sky-light-is-enough
  (is (nil? (sh-sense/light-in-s 0 18000 0 0)) "no sky light reaches it")
  (is (zero? (sh-sense/light-in-s 15 6000 0 0)) "noon")
  (is (pos? (sh-sense/light-in-s 15 18000 0 0)) "midnight")
  (is (< (sh-sense/light-in-s 15 22000 0 0) (sh-sense/light-in-s 15 14000 0 0)) "closer to dawn")
  (is (> (sh-sense/light-in-s 9 22000 0 0) (sh-sense/light-in-s 15 22000 0 0)) "a dim cell lights later"))

(deftest a-cell-is-lit-by-any-source-not-by-the-clock
  (are [light lit] (= lit (sh-sense/lit-here? (seen-dark cave-blocks light) {:x 0 :y 64 :z 0}))
    nil false
    {[0 64 0] [0 14]} true
    {[0 64 0] [15 0]} false))

(deftest light-read-reports-the-darkest-spawnable-corner-not-a-median
  (let [p (seen-dark cave-blocks {[0 64 0] [0 14] [1 64 0] [0 14]})
        cells [{:x 0 :y 64 :z 0} {:x 1 :y 64 :z 0} {:x 2 :y 64 :z 0}]]
    (is (= {:dark-fraction (/ 1 3) :darkest 0 :unseen 0} (sh-sense/light-read p cells)))
    (is (nil? (sh-sense/light-in p cells)) "the dark corner has no sky light: waiting never lights it")
    (is (zero? (sh-sense/light-in p (take 2 cells))) "the lit ones: nothing to wait for")))

(deftest light-view-reports-the-light-parts-of-a-cell-and-a-cube-read
  (let [lit (seen-dark cave-blocks {[0 64 0] [0 14] [1 64 0] [0 14]})
        view (sh-sense/light-view lit {:cell {:x 0 :y 64 :z 0}})]
    (is (= {:cell [0 64 0] :block 14 :sky 0 :light 14 :lit? true}
           (select-keys view [:cell :block :sky :light :lit?])))
    (is (= [0 64 0] (:cell (sh-sense/light-view lit {}))) "the feet by default")
    (is (= {:seen? false :light nil :lit? nil} (select-keys (sh-sense/light-view (tu/with-light-data (tu/fake {:time night :blocks cave-blocks :unloaded ["40,64,0"]})) {:cell {:x 40 :y 64 :z 0}}) [:seen? :light :lit?])) "no light data: unknown, not a guess")
    (is (= {:cells 27 :radius 1 :darkest 0} (select-keys (sh-sense/light-view lit {:cell {:x 0 :y 64 :z 0} :radius 1}) [:cells :radius :darkest])))))

(deftest night-without-light-data-falls-back-to-the-seen-roof
  (is (sh-sense/buried-by-column? (seen-fake {:time night :blocks (stone-above 2 1)})) "one seen block over the head, air above it")
  (is (not (sh-sense/buried-by-column? (seen-fake {:time night :blocks (stone-above 5 3)}))) "air over the head")
  (is (not (sh-sense/buried-by-column? (tu/fake {:time night :blocks (stone-above 2 1)}))) "nothing seen without a perception"))

(defn buried-cells
  "buried-at? of each cell for a body at the origin looking up in the world, wrapped in perception after one sight pass."
  [world cells]
  (let [raw-p (tu/fake (merge {:pitch -80 :time night} (dissoc world :light-default :light)))
        _ (swap! (fake/state raw-p) merge (select-keys world [:light-default :light]))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
        p (perception/wrap raw-p per)]
    (perception/pass! per)
    (mapv #(sh-sense/buried-at? p %) cells)))

(deftest buried-at-judges-any-cell-by-its-sky-light-when-the-body-has-seen-it
  (are [world cell expected] (= [expected] (buried-cells world [cell]))
    (merge dark {:blocks (stone-above 2 1)}) {:x 0 :y 64 :z 0} true
    ;; seen, open sky, a roof one block thick: lit, not buried
    {:blocks (stone-above 3 1) :light {[0 65 0] [15 0] [0 66 0] [15 0]}} {:x 0 :y 65 :z 0} false
    ;; seen cell in a tall cave: no sky light, ceiling far over
    (merge dark {:blocks (stone-above 10 1)}) {:x 0 :y 65 :z 0} true
    ;; never seen
    (merge dark {:blocks (stone-above 2 1)}) {:x 30 :y 64 :z 0} false
    (merge dark {:blocks (stone-above 2 1)}) {:x 0 :y 64 :z 1} false))

(deftest a-roofed-body-logs-out-for-a-sleeper-from-the-register
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :blocks {"0,66,0" "stone"}})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (await (tick-n eng 3))
          (is (< 1 (count (calls p "offline"))) "the roof does not matter; out again while the sleeper sleeps")
          (is (= (min 10 (count (calls p "offline"))) (count (entries eng :log-out))) "each stint booked (cap 10)"))))))

(def own-shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built})

(deftest night-does-not-hold-at-night-while-shut-in-its-own-shelter
  (let [{:keys [eng]} (setup {})
        holds? (fn [world] (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {})))]
    (mem/write! (:store eng) :shelter own-shelter {:cap 10 :ttl day-ms})
    (is (not (holds? {:time night :blocks {"0,66,0" "stone"}})) "shut in at night is safe: the queue runs")
    (is (not (holds? {:time night :blocks {"0,66,0" "stone"} :self {:isSleeping true}})) "asleep")
    (is (holds? {:time noon :blocks {"0,66,0" "stone"}}) "by day it lets the body out")
    (is (not (holds? {:time night :blocks {"0,66,0" "stone" "3,66,0" "stone"} :self {:pos {:x 3 :y 64 :z 0}}}))
        "roofed elsewhere, out of its shelter")))

(def walls
  (into {} (for [[x z] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str x "," y "," z) "stone"])))

(deftest a-pit-with-a-way-out-does-not-hold-the-body-by-day
  (let [{:keys [eng]} (setup {})
        holds? (fn [world] (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {})))]
    (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :start {:x 0 :y 66 :z 0} :state :built} {:cap 10 :ttl day-ms})
    (is (holds? {:time noon :floor tu/walk-floor :blocks (assoc walls "0,66,0" "stone")}) "shut in a pit by day: let out")
    (is (not (holds? {:time noon :floor tu/walk-floor :blocks (dissoc walls "1,64,0" "1,65,0")}))
        "a side opened (the stair it left by): walking back in is not being shut in")))

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

(deftest sleep-warns-when-no-respawn-point-is-set
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:time night :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 6))
          (is (= 1 (count (emitted seen :spawn_not_set))))
          (is (= :warn (:level (first (emitted seen :spawn_not_set))))))))))

(deftest sleep-does-not-warn-when-the-respawn-point-was-set-or-already-there
  (are [before? during?]
       (async done
         (tu/run-async done
           (fn ^:async t []
             (let [{:keys [eng p seen]} (setup {:time night :blocks {"6,64,0" "red_bed"}})
                   spawn! #(mem/write! (:store eng) :spawn-set {:pos {:x 5 :y 64 :z 0}} {:cap 5 :ttl day-ms})]
               (know-bed! eng {:x 6 :y 64 :z 0})
               (when before? (spawn!))
               (.override (.-world p) "sleep" (fn ^:async g [token a impl]
                                                (when during? (spawn!))
                                                (await (impl token a))))
               (core/submit! eng '(jobs.survival.sleep) {})
               (await (run-until-empty eng 6))
               (is (= 1 (count (calls p "sleep"))))
               (is (= [] (emitted seen :spawn_not_set)))))))
    true false
    false true))

(defn ^:async spawn-bed-warns
  "How many spawn_not_set warns a sleep gives when :spawn-bed was written two hours ago and a :spawn-reset follows or not."
  [spawn-bed reset?]
  (let [{:keys [eng seen clock]} (setup {:time night :blocks {"6,64,0" "red_bed"}})
        store (:store eng)]
    (know-bed! eng {:x 6 :y 64 :z 0})
    (when spawn-bed
      (mem/write! store :spawn-bed {:pos {:x 5 :y 64 :z 0}} {:cap 1 :ttl :forever})
      (swap! clock + 7200000))
    (when reset?
      (mem/write! store :spawn-reset {} {:cap 5 :ttl day-ms}))
    (core/submit! eng '(jobs.survival.sleep) {})
    (await (run-until-empty eng 6))
    (count (emitted seen :spawn_not_set))))

(deftest sleep-keeps-the-spawn-bed-after-the-spawn-set-entry-expires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= 0 (await (spawn-bed-warns true false))) "known spawn bed, server silent: no warn")
        (is (= 1 (await (spawn-bed-warns true true))) "a spawn-reset after it clears it")
        (is (= 1 (await (spawn-bed-warns false false))) "nothing known: warn")))))

(deftest sleep-records-the-spawn-bed-when-the-respawn-point-is-set
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks {"6,64,0" "red_bed"}})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (.override (.-world p) "sleep" (fn ^:async g [token a impl]
                                           (mem/write! (:store eng) :spawn-set {:pos {:x 6 :y 64 :z 0}} {:cap 5 :ttl day-ms})
                                           (await (impl token a))))
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 6))
          (is (= [{:pos {:x 6 :y 64 :z 0}}] (entries eng :spawn-bed)))
          (is (= {:cap 1 :ttl :forever} (mem/policy (mem/view (:store eng)) :spawn-bed))))))))

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
          (is (= 1 (count (filterv #(= :bed-missing (:reason %)) @seen)))))))))

(deftest sleep-finding-the-respawn-bed-gone-says-the-spawn-is-lost-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:time night})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (mem/write! (:store eng) :spawn-bed {:pos {:x 6 :y 64 :z 0}} {:cap 1 :ttl :forever})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 6))
          (is (= [{:pos {:x 6 :y 64 :z 0} :why :missing}] (entries eng :spawn-lost)))
          (is (= 1 (count (emitted seen :spawn_lost)))))))))

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
          (is (= [] (filterv #(= :bed-missing (:reason %)) @seen)))
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
          (is (= 9 (count (tu/walked-to eng))) "each of the three tries walks go-to afresh: three blocked walks at the bed's cell re-aimed one up before any walk")
          (is (= 1 (count (emitted seen :bed_unreachable)))))))))

(def walled-bed-hut
  "A birch hut with no door, walls x 10..14, z -2..2, y 64..66, roof y 67, a bed inside (the walk floor is y 63)."
  (let [room? (fn [k] (let [[x _ z] (map js/Number (.split k ","))] (and (<= 11 x 13) (<= -1 z 1))))]
    (merge (into {} (remove (comp room? key)) (tu/box 10 64 -2 14 66 2 "birch_planks"))
           (tu/box 10 67 -2 14 67 2 "birch_planks")
           {"12,64,0" "white_bed" "13,64,0" "white_bed"})))

(deftest sleep-says-why-the-walk-to-a-walled-in-bed-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :self {:pos {:x 12 :y 64 :z -8}}
                                           :blocks walled-bed-hut})]
          (know-bed! eng {:x 12 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.sleep) {})
          (await (run-until-empty eng 20))
          (is (empty? (calls p "sleep")))
          (is (re-find #"cannot reach the bed: .*walled in" (:text (first (emitted seen :bed_unreachable))))))))))

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
          (let [gave-up (count (gave-ups seen))]
            (core/submit! eng '(jobs.survival.sleep) {})
            (is (nil? (core/tick! eng)) "declines while the entry is fresh")
            (is (= gave-up (count (gave-ups seen))))
            (swap! clock + 600001)
            (await (tick-n eng 3))
            (is (< gave-up (count (gave-ups seen))) "tries the bed again once the entry expired: three more fruitless go-to rounds")))))))

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

(deftest log-out-goes-offline-for-a-short-stint
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night})]
          (core/submit! eng '(jobs.survival.log-out) {})
          (await (run-until-empty eng 3))
          (is (= [30000] (mapv #(.-ms (.-args %)) (calls p "offline"))))
          (is (= ["logged-out-for-sleeping-player"] (mapv #(.-why (.-args %)) (calls p "offline"))))
          (is (= [{:ms 30000 :status "ok"}] (entries eng :log-out)))
          (is (= {:cap 10 :ttl day-ms} (mem/policy (mem/view (:store eng)) :log-out))))))))

(deftest log-out-declines-by-day
  (let [{:keys [eng]} (setup {:time noon})]
    (core/submit! eng '(jobs.survival.log-out) {})
    (is (nil? (core/tick! eng)))))

(deftest log-out-not-allowed-or-unsupported-is-impossible-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[seed! spec] [[#(mem/write! (:store %) :log-out {:ms 0 :status "unsupported"} {:cap 10 :ttl day-ms}) '(jobs.survival.log-out)]
                              [identity '(jobs.survival.log-out {:offline-allowed false})]]]
          (let [{:keys [eng p]} (setup {:time night})]
            (seed! eng)
            (core/submit! eng spec {})
            (await (core/tick! eng))
            (is (= [] (:list (core/state eng))) (pr-str spec))
            (is (= [] (calls p "offline")) (pr-str spec))))))))

(deftest a-log-out-the-server-cannot-do-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night})]
          (.override (.-world p) "offline" (fn [_ _ _] #js {:status "unsupported"}))
          (is (= :impossible (:kind (:ending (await (tu/child-outcome eng 'jobs.survival.log-out {} 3)))))))))))

;; ------------------------------------------------------------------ dig-in

(def pit-floor
  "Flat ground a pit can be dug in (a lone column has open sides, which a pit rules out): dirt over x -4..8, z -4..4, the body's column dirt over stone."
  (merge (into {} (for [x (range -4 9) z (range -4 5) y (range 60 64)] [(str x "," y "," z) "dirt"])) floor))

(def ore-ground
  "pit-floor with iron ore (which drops no block) from y 61 to 63 everywhere: no pit anywhere can be roofed with what it digs."
  (merge pit-floor (into {} (for [x (range -4 9) z (range -4 5) y [61 62 63]] [(str x "," y "," z) "iron_ore"]))))

(deftest dig-in-walls-the-body-in-from-carried-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory wall-stack :blocks floor})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "one call: done in one round")
          (is (= 18 (count (calls p "place"))) "sides and corners at feet and head height, a roof support, the roof")
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
        (let [{:keys [eng p]} (setup {:time night :inventory wall-stack :blocks (assoc floor "1,64,0" "stone" "0,64,1" "stone")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
          (await (run-until-empty eng 6))
          (is (= 16 (count (calls p "place")))))))))

(deftest dig-in-digs-down-three-on-flat-ground-and-roofs-with-a-dug-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks pit-floor})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [{:x 0 :y 63 :z 0} {:x 0 :y 62 :z 0} {:x 0 :y 61 :z 0}] (mapv arg-pos (calls p "dig"))))
          (is (= {:x 0 :y 61 :z 0} (pos-of p)) "three blocks down")
          (is (= [{:x 0 :y 63 :z 0}] (mapv arg-pos (calls p "place"))) "the roof goes in the ground layer, beside solid ground")
          (is (= [{:pos {:x 0 :y 61 :z 0} :roof {:x 0 :y 63 :z 0} :start {:x 0 :y 64 :z 0} :state :built}] (entries eng :shelter))))))))

(deftest dig-in-whose-pit-is-filled-over-the-head-is-not-done
  ;; a block set in the head cell (a neighbouring fill) as the roof goes on: roofed over, but the body suffocates
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks pit-floor})]
          (.override (.-world p) "place" (fn ^:async g [token a impl]
                                           (fake/set-block! p [0 62 0] "dirt")
                                           (await (impl token a))))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= {:x 0 :y 61 :z 0} (pos-of p)))
          (is (empty? (emitted seen :shut-in.sealed)))
          (is (= [:no-way] (mapv :reason (emitted seen :stopped)))))))))

(deftest night-does-not-end-done-with-the-head-in-a-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks (merge floor {"0,65,0" "dirt" "0,66,0" "dirt"})})]
          (is (nil? (sh-inside/night-sheltered p nil)))
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-n eng 6))
          (is (false? (sh-sense/head-in-block? p)) "a night that ended roofed had dug its head out first"))))))

(deftest dig-in-digs-down-two-and-roofs-at-the-start-cell-beside-a-solid-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks (assoc pit-floor "1,64,0" "stone")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= 2 (count (calls p "dig"))))
          (is (= [{:x 0 :y 64 :z 0}] (mapv arg-pos (calls p "place"))) "the roof goes where the body stood"))))))

(deftest dig-in-with-few-carried-blocks-digs-down-and-roofs-with-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :drops {"iron_ore" "raw_iron"} :inventory [{:name "cobblestone" :count 1} {:name "stone_pickaxe" :count 1}]
                                      :blocks (merge pit-floor {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "iron_ore" "0,60,0" "stone"})})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= 3 (count (calls p "dig"))))
          (is (= "cobblestone" (.-item (.-args (first (calls p "place"))))) "the carried block is used first"))))))

(deftest dig-in-digs-nothing-when-nothing-can-roof-the-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :drops {"iron_ore" "raw_iron"} :inventory [{:name "iron_pickaxe" :count 1}]
                                           :blocks ore-ground})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")) "no cell dug: none of the cells drops a block")
          (is (= [] (calls p "place")))
          (is (re-find #"nothing to roof the pit with" (:text (first (no-way-stops seen)))))
          (is (= [:no-way] (mapv :reason (emitted seen :stopped))))
          (is (= [] (:list (core/state eng))) "ended"))))))

(deftest dig-in-roofs-with-a-carried-log
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :drops {"iron_ore" "raw_iron"}
                                           :inventory [{:name "iron_pickaxe" :count 1} {:name "oak_log" :count 5}]
                                           :blocks (merge pit-floor {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "iron_ore" "0,60,0" "stone"})})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (no-way-stops seen)))
          (is (= ["oak_log"] (map #(.-item (.-args %)) (calls p "place"))) "the log roofs the pit"))))))

(deftest a-submitted-dig-in-in-a-shut-room-is-done-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks (merge floor bed-room (tu/box -1 63 -1 7 63 1 "stone")) :light {} :light-default [15 14]})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 4))
          (is (empty? (calls p "place")))
          (is (empty? (emitted seen :waiting)))
          (is (empty? (emitted seen :stopped))))))))

(def stone-pit (assoc stone-ground "1,64,0" "stone"))

(defn act-names
  "The names of the dig and equip calls, in order."
  [p]
  (mapv #(.-name %) (filter #(#{"dig" "equip"} (.-name %)) (.-calls (.-world p)))))

(deftest dig-in-holds-the-best-pickaxe-before-each-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[inventory held names]
                [[[{:name "stone_pickaxe" :count 1} {:name "iron_pickaxe" :count 1} {:name "dirt" :count 1}] "dirt" ["equip" "dig" "dig"]]
                 [[{:name "iron_pickaxe" :count 1}] nil ["equip" "dig" "dig"]]
                 [[{:name "iron_pickaxe" :count 1}] "iron_pickaxe" ["dig" "dig"]]]]
          (let [{:keys [eng p]} (setup {:time night :blocks stone-pit :inventory inventory :self {:held held}})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (= names (act-names p)) (pr-str inventory held))
            (is (= "iron_pickaxe" (.-held (.self p))) (pr-str inventory held))))))))

(deftest dig-in-does-not-dig-a-pit-it-cannot-harvest-with-nothing-to-roof-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks stone-pit :inventory [{:name "wooden_shovel" :count 1}]})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")) "a hand dig of stone drops nothing: no roofless pit")
          (is (re-find #"cannot harvest stone" (:text (first (no-way-stops seen)))))
          (is (= [] (entries eng :shut-in-failed)) "a lack of tool is no failed site: the quote says so")
          (is (= [] (:list (core/state eng))) "ended"))))))

(deftest dig-in-walls-converge-on-the-current-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory [{:name "dirt" :count 30}] :blocks floor})]
          (after-first! p "place" #(teleport! p 5 64 0))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= "dirt" (.-name (.blockAt p (tu/pos 5 66 0)))) "roof over where the body now is")
          (is (= {:x 5 :y 64 :z 0} (:pos (last (entries eng :shelter)))))
          (is (= {:x 5 :y 66 :z 0} (:roof (last (entries eng :shelter))))))))))

(deftest dig-in-walls-out-of-blocks-falls-back-to-a-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 30}]
                                           :blocks (merge pit-floor {"5,63,0" "dirt" "5,62,0" "stone" "5,61,0" "stone" "5,60,0" "stone" "6,64,0" "stone"})})]
          (after-first! p "place" #(do (swap! (fake/state p) assoc :inventory []) (teleport! p 5 64 0)))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 12))
          (is (empty? (no-way-stops seen)) "no failure: it switched to a pit")
          (is (seq (calls p "dig")) "it dug down")
          (is (< (:y (pos-of p)) 64) "the body is in the pit")
          (is (= "dirt" (.-name (.blockAt p (tu/pos 5 64 0)))) "the pit is roofed with a dug block"))))))

(deftest dig-in-dig-mode-rechooses-when-the-body-moved-off-its-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks (merge pit-floor {"5,63,0" "stone" "5,62,0" "stone" "5,61,0" "stone" "5,60,0" "stone" "6,64,0" "stone"})})]
          (after-first! p "collect" #(teleport! p 5 64 0))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 10))
          (is (= [{:x 5 :y 64 :z 0}] (mapv arg-pos (calls p "place"))) "roof at the new column's start")
          (is (= {:x 5 :y 62 :z 0} (pos-of p)))
          (is (= {:x 5 :y 64 :z 0} (:roof (last (entries eng :shelter))))))))))

(deftest dig-in-gives-up-on-an-unplaceable-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack :blocks floor})]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "no-support"}))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (is (<= (await (run-until-empty eng 10)) 5) "bounded")
          (is (= 1 (count (no-way-stops seen))))
          (is (= [{:pos {:x 0 :y 64 :z 0} :state :built :sealed false}] (entries eng :shelter)) "records that it stopped, unroofed"))))))

(deftest dig-in-walls-place-a-support-before-the-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack
                                           :blocks (merge floor {"1,63,0" "stone" "-1,63,0" "stone" "0,63,1" "stone" "0,63,-1" "stone"})})
              supported? (fn [pos] (some #(let [n (.-name (.blockAt p (tu/pos (+ (.-x pos) (first %)) (+ (.-y pos) (second %)) (+ (.-z pos) (nth % 2)))))]
                                            (not (#{"air" "cave_air"} n)))
                                         [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))]
          (.override (.-world p) "place"
                     (fn ^:async f [_ args impl]
                       (if (supported? (.-pos args))
                         (await (impl _ args))
                         #js {:status "no-support"})))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [{:x 1 :y 66 :z 0} {:x 0 :y 66 :z 0}] (mapv arg-pos (take-last 2 (calls p "place")))) "support, then roof")
          (is (= [] (no-way-stops seen)))
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
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 3}] :blocks pit-floor})]
          (require-support! p)
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 12))
          (is (= [] (no-way-stops seen)))
          (is (= {:x 0 :y 61 :z 0} (pos-of p)) "three down")
          (is (= "dirt" (.-name (.blockAt p (tu/pos 0 63 0)))) "roof flush with the ground, beside solid ground")
          (is (= {:x 0 :y 63 :z 0} (:roof (last (entries eng :shelter))))))))))

(deftest dig-in-does-not-dig-where-the-roof-has-only-water-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 3}]
                                           :blocks (assoc pit-floor "1,63,0" "water" "-1,63,0" "water" "0,63,1" "water" "0,63,-1" "water")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 12))
          (is (= [] (calls p "dig")) "floor intact")
          (is (= 1 (count (no-way-stops seen))))
          (is (= :hazard-below (:reason (first (entries eng :shut-in-failed))))))))))

(def one-side-open
  "pit-floor with water beside the straight pit's bottom and the three other zigzag columns: only a zigzag towards +x fits."
  (assoc pit-floor "-1,61,0" "water" "0,61,1" "water" "0,61,-1" "water"))

(deftest dig-in-does-not-dig-a-pit-through-the-only-open-side-column-with-a-hostile-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 3}]
                                           :blocks one-side-open})]
          (fake/add-entity! p {:id 7 :name "zombie" :kind "hostile" :pos {:x 1 :y 64 :z 0}})
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")) "no pit towards the hostile")
          (is (= [:hostile-near] (mapv :reason (entries eng :shut-in-failed))) "the reason names the hostile, not a floor"))))))

(deftest dig-in-will-not-dig-through-a-thin-floor-into-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"0,63,0" "stone" "0,62,0" "water"}})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")))
          (is (= 1 (count (no-way-stops seen))))
          (is (re-find #"water" (:text (first (no-way-stops seen))))))))))

(deftest dig-in-will-not-dig-over-an-air-gap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks {"0,63,0" "stone"}})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")))
          (is (= 1 (count (no-way-stops seen)))))))))

(deftest dig-in-does-not-claim-sealed-when-the-roof-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack :blocks floor})]
          (tu/seeing-all p)
          (refuse-cell! p [0 66 0])
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (is (<= (await (run-until-empty eng 12)) 10) "bounded")
          (is (= [] (emitted seen :shut-in.sealed)) "no sealed claim while the roof is open")
          (is (some? (first (emitted seen :shut-in.unsealed))) "reported honestly")
          (is (false? (:sealed (last (entries eng :shelter)))) "the entry says it is open"))))))

(deftest dig-in-places-the-other-walls-before-retrying-a-refused-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory wall-stack :blocks floor})]
          (refuse-cell! p [-1 64 0])
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
          (await (run-until-empty eng 12))
          (is (every? #(= "dirt" (.-name (.blockAt p (tu/pos (first %) (second %) (nth % 2)))))
                      [[1 64 0] [0 64 1] [0 64 -1] [1 65 0] [-1 65 0] [0 66 0]])
              "every cell but the refused one is filled, its head cell too"))))))

;; ----------------------------------------------------------------- shelter

(deftest shelter-prefers-a-bed-over-logging-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :players ["Alex"] :blocks bed-room})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 8))
          (is (= 1 (count (calls p "sleep"))))
          (is (= [] (calls p "offline")))
          (is (= 1 (count (entries eng :slept)))))))))

(deftest ms-until-morning-counts-to-the-end-of-the-night-plus-a-margin
  (are [t ms] (= ms (sh-clock/ms-until-morning t))
    14000 475050
    12542 547950
    20000 175050
    23460 2050
    23461 0
    1000 0
    0 0))

(deftest another-player-online-and-awake-is-no-reason-to-log-out
  (async done
    (tu/run-async done
      (walls-cheap (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :players ["Alex"] :entities [awake] :inventory wall-stack :blocks floor})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-n eng 8))
          (is (= [] (calls p "offline")))
          (is (= 18 (count (calls p "place"))) "dug in")))))))

(deftest shelter-digs-in-when-the-log-out-fails
  (async done
    (tu/run-async done
      (walls-cheap (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :inventory wall-stack :blocks floor})]
          (.override (.-world p) "offline" (fn [_ _ _] #js {:status "unsupported"}))
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-n eng 8))
          (is (= 1 (count (calls p "offline"))) "tried once; unsupported is not tried again")
          (is (= ["unsupported"] (mapv :status (entries eng :log-out))))
          (is (= 18 (count (calls p "place"))) "fell through to dig-in")))))))

(deftest shelter-falls-through-a-missing-bed-to-the-next-choice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [awake]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 8))
          (is (= [{:gone true :was {:x 6 :y 64 :z 0}}] (entries eng :bed)))
          (is (= [] (calls p "offline")) "nobody else online: no log-out"))))))

(deftest a-dug-in-shelter-holds-the-body-until-day-then-the-queued-job-runs
  (async done
    (tu/run-async done
      (walls-cheap (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack :blocks floor :entities [awake]})
              _ (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
              _ (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
              mid (await (night-then-day eng p 4 #(hash-map :places (count (calls p "place")) :digs (count (calls p "dig"))
                                                            :holder (some? (core/holder eng)) :notified (notified seen))
                                         6))]
          (is (= {:places 18 :digs 0 :holder false :notified 1} mid)
              "dug in and safe: the night does not hold the body, the queued job ran at night")
          (is (= [:built] (mapv :state (entries eng :shelter))))
          (is (nil? (core/holder eng)) "the shelter ended at day")
          (is (not= {:x 0 :y 64 :z 0} (pos-of p)) "the body stepped out of its walls")
          (is (= 1 (notified seen)) "then the queued job ran")))))))

(deftest a-pit-shelter-holds-until-day-then-ends-with-the-body-out-of-the-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks ground})
              _ (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
              _ (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
              mid (await (night-then-day eng p 4 #(hash-map :y (:y (pos-of p)) :holder (some? (core/holder eng)) :notified (notified seen)) 6))]
          (is (= {:y 61 :holder false :notified 1} mid) "in the pit, safe: the queued job ran at night")
          (is (nil? (core/holder eng)) "the shelter ended at day")
          (is (>= (:y (pos-of p)) 64) "the body stands on the surface, out of the pit")
          (is (empty? (emitted seen :shelter.failed)) "out: done, not failed")
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(deftest a-pit-shelter-with-no-way-out-ends-failed-by-day-and-the-queued-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks stone-ground :inventory [{:name "dirt" :count 1}]})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (let [mid (await (night-then-day eng p 4 #(hash-map :pit (< (:y (pos-of p)) 64) :notified (notified seen)) 6))]
            (is (= {:pit true :notified 1} mid) "in the pit, safe: the queued job ran at night"))
          (is (seq (emitted seen :dig-in.trapped)) "no pickaxe for the stone stair: trapped")
          (let [[w & more] (emitted seen :shelter.failed)]
            (is (empty? more) "one warn")
            (is (= :no-way-out (:reason w)))
            (is (= (let [{:keys [x y z]} (pos-of p)] [x y z]) (:at w)) "the pit position"))
          (is (nil? (core/holder eng)) "the shelter ended: the body is the agent's again")
          (is (= 1 (notified seen)) "the queued job ran"))))))

(deftest a-pit-shelter-with-a-hostile-near-still-leaves-by-day-and-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks ground})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (night-then-day eng p 4 #(fake/add-entity! p {:id 7 :name "zombie" :kind "hostile" :pos {:x 2 :y 64 :z 0}}) 6))
          (is (>= (:y (pos-of p)) 64) "out of the pit: a hostile is the hostile reflex's business, not the shelter's")
          (is (nil? (core/holder eng)) "no hold: the shelter ended")
          (is (empty? (emitted seen :dig-in.staying)))
          (is (empty? (emitted seen :shelter.failed)))
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(deftest a-sleeping-shelter-holds-until-day-then-the-queued-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :skipNight false :blocks bed-room})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (let [mid (mid-night! p 2 #(hash-map :asleep (.-isSleeping (.self p)) :notified (notified seen)))]
            (await (tick-n eng 6))
            (is (= {:asleep true :notified 0} @mid) "asleep in the bed, the night not skipped; the queued job does not wake it"))
          (is (= 1 (notified seen))))))))

(deftest shelter-ends-without-acting-when-already-shut-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks (merge {"0,66,0" "stone"} (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "stone"])))})
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
      (walls-cheap (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack :blocks (merge floor {"6,64,0" "red_bed"}) :entities [awake]
                                           :unreachable ["6,64,0"]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (loop [i 0]
            (when (and (< i 60) (empty? (emitted seen :bed_unreachable)))
              (await (core/tick! eng))
              (recur (inc i))))
          (is (= 1 (count (emitted seen :bed_unreachable))))
          (let [walks (walks-to-bed eng)]
            (await (tick-n eng 12))
            (is (= walks (walks-to-bed eng)) "no more walking at the unreachable bed")
            (is (= 18 (count (calls p "place"))) "dig-in finished the walls"))))))))

(deftest shelter-treats-a-long-awake-body-as-needing-a-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:floor [-30 -10 90 10] :time night :blocks {"80,64,0" "red_bed"}})]
          (know-bed! eng {:x 80 :y 64 :z 0})
          (mem/write! (:store eng) :slept {:pos {:x 0 :y 64 :z 0}} {:cap 10 :ttl (* 7 day-ms)})
          (reset! clock (+ 1000000 (* 4 day-ms)))
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 10))
          (is (= 1 (count (calls p "sleep"))) "an 80 block walk is worth it after four days awake")
          (is (= 1 (count (emitted seen :needs_bed)))))))))

(deftest shelter-does-not-walk-that-far-when-rested
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks (merge floor {"80,64,0" "red_bed"})})]
          (know-bed! eng {:x 80 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 3))
          (is (= [] (calls p "sleep"))))))))

(deftest night-fires-shelter-from-the-register
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks bed-room})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (await (tick-n eng 4))
          (is (= 1 (count (calls p "sleep")))))))))

;; ------------------------------------------------- declining when stuck

(def shelter-policy {:cap 10 :ttl day-ms})

(defn ^:async tick-nights
  "n batches of ticks, the clock 11 s on after each (past the night cooldown, so a dropped reflex would fire again)."
  [eng clock n]
  (loop [i 0]
    (when (< i n)
      (await (tick-n eng 6))
      (swap! clock + 11000)
      (recur (inc i)))))

(deftest a-shelter-that-cannot-roof-holds-the-body-until-day-without-refiring
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:time night :inventory (conj wall-stack {:name "iron_pickaxe" :count 1}) :blocks floor})]
          (refuse-placing! p)
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (let [mid (mid-night! p 10 #(hash-map :holder (some? (core/holder eng)) :places (count (calls p "place"))
                                               :exposed (count (emitted seen :shelter.exposed)) :notified (notified seen)))]
            (await (tick-nights eng clock 2))
            (is (= {:holder true :exposed 1 :notified 0} (dissoc @mid :places))
                "held at night; one warn that the body is unsheltered; the queued job waits")
            (is (= (* 5 (sites/max-sites)) (:places @mid)) "max-sites sites tried"))
          (is (= [] (declined-events seen :night)) "never declined, so never dropped and fired again")
          (is (= 1 (count (filter #(= :fired (:kind %)) (emitted-by seen :reflex :night)))) "fired once")
          (is (nil? (core/holder eng)) "the shelter ended at day")
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(deftest a-body-with-no-pickaxe-on-a-stone-floor-at-night-seals-in-above-ground-and-digs-no-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack :blocks (tu/box -30 58 -30 30 63 30 "stone")})
              _ (core/submit! eng '(jobs.survival.night) {})
              mid (await (night-then-day eng p 8 #(hash-map :holder (some? (core/holder eng)) :pos (pos-of p)) 2))]
          (is (= {:holder false :pos {:x 0.5 :y 64 :z 0.5}} mid) "in place at night (stepped to its cell's middle to place the walls), on the surface, not held")
          (is (= 1 (count (emitted seen :shut-in.sealed))) "walled in with placed blocks")
          (is (every? #(>= (:y (arg-pos %)) 64) (calls p "dig")) "no dig below the surface: the only digs are the placed wall, taken down by day"))))))

(deftest an-unsheltered-hold-does-not-rerun-a-failing-dig-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[blocks reason] [[{"0,63,0" "stone" "0,62,0" "air"} :no-floor]
                                 [{"0,63,0" "stone" "0,62,0" "water"} :no-floor]
                                 [{"0,63,0" "water"} :hazard-below]]]
          (let [{:keys [eng p seen]} (setup {:time night :blocks blocks :inventory [{:name "dirt" :count 1} {:name "iron_pickaxe" :count 1}]})
                mid (mid-night! p 4 #(hash-map :jobs (count (:list (core/state eng)))
                                               :futile (:reason (first (entries eng :shut-in-failed)))))]
            (core/submit! eng '(jobs.survival.night) {})
            (await (tick-n eng 3))
            (is (= [] (calls p "dig")) (pr-str blocks))
            (is (= 5 (count (no-way-stops seen))) (str (pr-str blocks) ": one try at the site, then max-sites sites in all, then the niche"))
            (is (= {:jobs 1 :futile reason} @mid) (str (pr-str blocks) ": held exposed at night, dig-in's futile entry written"))
            (is (= [] (entries eng :shut-in-failed)) "forgotten when the night ends")))))))

(deftest night-over-an-open-floor-walks-to-another-site-before-holding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 1} {:name "iron_pickaxe" :count 1}]
                                           :blocks {"0,63,0" "stone" "0,62,0" "air"}})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-n eng 30))
          (let [pos (pos-of p)]
            (is (> (js/Math.hypot (:x pos) (:z pos)) 9) "left the site whose floor is hollow (a player cannot see that, dig-in finds out)")))))))

(deftest an-unsheltered-hold-logs-out-once-someone-falls-asleep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory wall-stack :blocks floor})]
          (refuse-placing! p)
          (let [before (mid-night! p 3 #(do (.emit (.-world p) #js {:kind "sleep-status" :sleeping 1 :needed 2})
                                            (count (calls p "offline"))))]
            (core/submit! eng '(jobs.survival.night) {})
            (await (tick-n eng 3))
            (is (= 0 @before) "nobody asleep")
            (is (pos? (count (calls p "offline"))) "the hold chose again and logged out")))))))

(deftest an-unsheltered-hold-sleeps-in-a-bed-given-or-seen-later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[how change] [[:carried #(swap! (fake/state %) assoc :inventory [{:name "red_bed" :count 1}])]
                              [:seen #(swap! (fake/state %) update :blocks assoc "3,64,0" "red_bed")]]]
          (let [{:keys [eng p seen clock]} (setup {:time night :inventory wall-stack :blocks floor})]
            (refuse-placing! p)
            (core/submit! eng '(jobs.survival.night) {})
            (await (tick-nights eng clock 3))
            (is (= 1 (count (emitted seen :shelter.exposed))) (str how " held exposed first"))
            (change p)
            (await (tick-nights eng clock 3))
            (is (pos? (count (concat (calls p "sleep") (calls p "place")))) (str how " the hold looked for a bed again"))))))))

(deftest an-unsheltered-hold-does-not-thrash-while-nothing-changes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:time night :inventory wall-stack :blocks floor})]
          (refuse-placing! p)
          (let [at3 (atom nil)
                at9 (atom nil)]
            (dawn-after! p 10 (fn [n] (case n 3 (reset! at3 (count (.-calls (.-world p))))
                                            9 (reset! at9 (vec (.-calls (.-world p))))
                                            nil)))
            (core/submit! eng '(jobs.survival.night) {})
            (await (tick-nights eng clock 2))
            (is (every? #{"wait"} (map #(.-name %) (drop @at3 @at9))) "only waits while nothing changes")))))))

(deftest a-failed-sleep-is-tried-again-once-the-entry-expires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:time night :blocks (merge floor {"3,64,0" "red_bed"}) :floor [-4 -4 4 4]})
              tries (atom 0)]
          (set! (.-sleep p) (fn [& _] (swap! tries inc) (js/Promise.resolve #js {:status "occupied"})))
          (let [at2 (atom nil)
                at4 (atom nil)]
            (dawn-after! p 7 (fn [n] (case n 2 (reset! at2 @tries)
                                           4 (do (reset! at4 @tries) (swap! clock + (:ttl sh-beds/sleep-failed-policy)))
                                           nil)))
            (core/submit! eng '(jobs.survival.night {:settings {:jobs.lib.shelter.beds/max-days-awake 0}}) {})
            (await (tick-nights eng clock 2))
            (is (pos? @at2))
            (is (= @at2 @at4) "the failed entry holds it off")
            (is (< @at4 @tries) "after the entry expired the bed is tried again")))))))

(deftest a-place-failure-holds-only-near-where-it-happened
  (let [p (tu/fake {:time night})]
    (are [data held] (= held (sh-beds/near-failed-place? p {:data data}))
      {:pos {:x 0 :y 64 :z 0}} true
      {:pos {:x 3 :y 64 :z 0}} true
      {:pos {:x 20 :y 64 :z 0}} false
      {} true)))

(deftest an-unsheltered-hold-after-a-futile-dig-ends-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :drops {"iron_ore" "raw_iron"} :inventory [{:name "iron_pickaxe" :count 1}]
                                           :blocks ore-ground})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (let [mid (mid-night! p 2 #(hash-map :digs (count (calls p "dig")) :holder (some? (core/holder eng))))]
            (await (tick-n eng 6))
            (is (= {:digs 0 :holder true} @mid) "nothing to roof the pit with: no dig; held at night"))
          (is (nil? (core/holder eng)) "the shelter ended at day")
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(deftest shelter-retries-dig-in-at-night-beside-an-unroofed-shelter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks floor :inventory [{:name "dirt" :count 16}]})]
          (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :state :built} shelter-policy)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-n eng 4))
          (is (seq (calls p "place")) "dig-in was tried again, not a bare decline"))))))

(def futile-world
  {:time night :drops {"iron_ore" "raw_iron"} :inventory [{:name "iron_pickaxe" :count 1}]
   :blocks (merge hard-ground {"0,63,0" "iron_ore" "0,62,0" "iron_ore" "0,61,0" "stone"})})

(deftest a-futile-pit-is-not-dug-again-while-the-shelter-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup (assoc futile-world :floor [-4 -4 4 4]))
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (core/register-reflex! eng {:trigger :always-shelter})
          (let [held (mid-night! p (dec default-dawn) #(hash-map :futile (entries eng :shut-in-failed) :places (count (calls p "place")) :digs (count (calls p "dig"))))]
            (await (tick-n eng 6))
            (is (= #{{:way :pit :reason :roof-failed} {:way :walls :reason :failed}} (set (map #(select-keys % [:way :reason]) (:futile @held)))) "written while the night holds")
            (is (= 4 (:places @held)) "the pit's roof and the walls' first place, each refused once; no second pit is roofed")
            (await (tick-nights eng clock 3))
            (is (= (:digs @held) (count (calls p "dig"))) "the hold does not dig again"))
          (is (= [] (declined-events seen :always-shelter))))))))

(deftest a-futile-pit-is-not-dug-again-on-the-next-firing-after-a-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup futile-world)
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (core/register-reflex! eng {:trigger :always-shelter})
          (let [held-again (atom false)
                places (atom nil)
                digs (atom nil)]
            (on-wait! p (fn [n] (case n
                                  2 (do (reset! digs (count (calls p "dig"))) (core/cut! eng (core/holder eng) :test nil) (swap! clock + 11000))
                                  3 (do (reset! held-again (some? (core/holder eng))) (reset! places (count (calls p "place"))))
                                  4 (.setTime (.-world p) noon)
                                  nil)))
            (await (tick-n eng 6))
            (is (true? @held-again) "fired again and holds")
            (is (= 4 @places) "the second firing roofs no second pit")
            (is (= @digs (count (calls p "dig"))) "the second firing does not dig")))))))

(deftest dig-in-refuses-lateral-fluid-before-opening-a-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [fluid ["water" "lava"]]
          (let [{:keys [eng p]} (setup {:time night :blocks (assoc pit-floor "1,63,0" fluid)
                                      :inventory [{:name "dirt" :count 1}]})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (empty? (calls p "dig")) "the fluid barrier remains intact")
            (is (empty? (calls p "place")) "no roof traps the body beside fluid")
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (empty? (calls p "place")) "carried dirt does not make the site safe")
            (is (= :fluid-adjacent (:reason (first (entries eng :shut-in-failed)))))))))))

(deftest dig-in-refuses-water-plants-bubbles-and-waterlogged-blocks-beside-the-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[block states] [["seagrass" nil] ["tall_seagrass" nil] ["kelp" nil] ["kelp_plant" nil] ["bubble_column" nil]
                                ["oak_slab" {"1,63,0" {:waterlogged true}}]]]
          (let [{:keys [eng p]} (setup {:time night :blocks (assoc pit-floor "1,63,0" block) :states states
                                        :inventory [{:name "dirt" :count 1}]})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (empty? (calls p "dig")) block)
            (is (= :fluid-adjacent (:reason (first (entries eng :shut-in-failed)))) block)))))))

(deftest dig-in-does-not-dig-down-from-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [cell ["0,64,0" "0,65,0"]]
          (let [{:keys [eng p]} (setup {:time night :blocks (merge ground {cell "water"})})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (empty? (calls p "dig")) cell)
            (is (= :fluid-here (:reason (first (entries eng :shut-in-failed)))) cell)))))))

(deftest dig-in-stops-digging-when-fluid-appears-above-the-roof-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [fluid ["water" "lava"]]
          (let [{:keys [eng p]} (setup {:time night :blocks (assoc ground "1,64,0" "stone")})]
            (.override (.-world p) "dig" (fn ^:async f [token a impl]
                                           (let [r (await (impl token a))]
                                             (fake/set-block! p [0 65 0] fluid)
                                             r)))
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (= 1 (count (calls p "dig"))) fluid)
            (is (empty? (calls p "place")) fluid)
            (is (= :fluid-above (:reason (first (entries eng :shut-in-failed)))) fluid)))))))

(deftest dig-plan-needs-a-full-block-beside-the-roof-cell
  (are [side plan] (= plan (dig-cells/dig-plan (tu/fake {:blocks (merge ground {"1,64,0" side})}) {:x 0 :y 64 :z 0}))
    "stone" {:roof {:x 0 :y 64 :z 0} :depth 2}
    "poppy" {:roof {:x 0 :y 63 :z 0} :depth 3}
    "sugar_cane" {:roof {:x 0 :y 63 :z 0} :depth 3}
    "oak_sapling" {:roof {:x 0 :y 63 :z 0} :depth 3}))

(deftest a-flower-beside-the-start-is-no-roof-support
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks (assoc ground "1,64,0" "poppy")})]
          (require-support! p)
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 12))
          (is (= [{:x 0 :y 63 :z 0} {:x 0 :y 62 :z 0} {:x 0 :y 61 :z 0}] (mapv arg-pos (calls p "dig"))))
          (is (= [{:x 0 :y 63 :z 0}] (mapv arg-pos (calls p "place"))) "roofed in the ground layer")
          (is (= [] (no-way-stops seen))))))))

(deftest dig-in-bounds-digging-without-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks pit-floor})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "failed"}))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (is (< (await (run-until-empty eng 10)) 10))
          (is (= 3 (count (calls p "dig"))) "three failed digs in a row, then it gives up")
          (is (= 1 (count (no-way-stops seen))))
          (is (= :dig-failed (:reason (first (entries eng :shut-in-failed)))))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 10))
          (is (= 3 (count (calls p "dig"))) "the failed pit is not dug again"))))))

(deftest dig-in-bounds-descent-without-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [status ["blocked" "arrived"]]
          (let [{:keys [eng p seen]} (setup {:time night :blocks one-side-open})]
            (.override (.-world p) "moveTo" (fn ^:async f [_ _ _] #js {:status status}))
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (is (< (await (run-until-empty eng 12)) 12))
            (is (= 1 (count (no-way-stops seen))))
            (is (= :descent-stalled (:reason (first (entries eng :shut-in-failed)))) status)))))))

(deftest unsupported-walls-are-not-retried-on-every-reflex-firing
  (async done
    (tu/run-async done
      (walls-cheap (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:time night :blocks floor :inventory wall-stack :floor [-4 -4 4 4]})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (core/register-reflex! eng {:trigger :always-shelter})
          (let [futile (mid-night! p (dec default-dawn) #(entries eng :shut-in-failed))]
            (await (tick-n eng 8))
            (is (= {:way :walls :reason :failed} (select-keys (first @futile) [:way :reason])) "written while the night holds"))
          (let [digs (count (calls p "dig"))]
            (swap! clock + 11000)
            (await (tick-n eng 8))
            (is (= 4 (count (calls p "place"))) "each of the four cells of one step tried once")
            (is (= digs (count (calls p "dig"))) "the failed ways are not tried again"))))))))

(deftest needs-bed-is-warned-once-across-firings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:time night :inventory wall-stack :blocks floor})
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (mem/write! (:store eng) :slept {:pos {:x 0 :y 64 :z 0}} {:cap 10 :ttl (* 7 day-ms)})
          (swap! clock + (* 4 day-ms))
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 6))
          (swap! clock + 11000)
          (await (tick-n eng 6))
          (is (= [] (declined-events seen :always-shelter)) "it holds rather than declining")
          (is (= 1 (count (emitted seen :needs_bed)))))))))

(deftest dig-in-treats-an-occupied-non-solid-cell-as-sealed-and-does-not-retry-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory wall-stack
                                      :blocks (assoc floor "1,66,0" "oak_leaves" "0,66,0" "oak_leaves")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
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
          (let [{:keys [eng p]} (setup {:time night :inventory wall-stack
                                        :blocks (assoc floor "1,64,0" passable "1,65,0" passable)})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
            (is (<= (await (run-until-empty eng 10)) 8) (str passable ": the job ends"))
            (is (= [] (:list (core/state eng))))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 1 64 0)))) (str passable " at feet height is walled, not left a hole"))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 1 65 0)))) (str passable " at head height is walled"))))))))

(deftest dig-in-ends-when-a-passable-cell-cannot-be-cleared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory wall-stack :blocks (assoc floor "1,64,0" "torch")})]
          (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "unreachable"}))
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (is (<= (await (run-until-empty eng 12)) 10) "the job still ends")
          (is (= [] (:list (core/state eng)))))))))

(deftest dig-in-walls-over-blocks-a-mob-walks-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [passable ["oak_sign" "oak_wall_sign" "rail" "stone_pressure_plate" "stone_button" "lever" "white_carpet"]]
          (let [{:keys [eng p]} (setup {:time night :inventory wall-stack
                                        :blocks (assoc floor "1,64,0" passable "0,65,1" passable "0,66,0" passable)})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
            (is (<= (await (run-until-empty eng 12)) 10) (str passable ": the job ends"))
            (is (= [] (:list (core/state eng))))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 1 64 0)))) (str passable " in a feet wall cell is walled"))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 0 65 1)))) (str passable " in a head wall cell is walled"))
            (is (= "dirt" (.-name (.blockAt p (tu/pos 0 66 0)))) (str passable " in the roof cell is walled"))))))))

(deftest dig-in-keeps-blocks-that-already-stop-a-mob
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :inventory wall-stack
                                      :blocks (assoc floor "1,64,0" "oak_fence" "0,64,1" "glass" "-1,64,0" "iron_bars")})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")))
          (is (= ["oak_fence" "glass" "iron_bars"]
                 (mapv #(.-name (.blockAt p (apply tu/pos %))) [[1 64 0] [0 64 1] [-1 64 0]]))))))))

(def overhang
  (merge floor
         (into {} (for [x [-1 0 1] z [-1 0 1]] [(str x ",66," z) "stone"]))
         {"-1,64,0" "stone" "-1,65,0" "stone"}))

(deftest dig-in-enclose-walls-in-under-an-overhang-placing-only-the-open-sides
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 20}] :blocks overhang})]
          (tu/seeing-all p)
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty eng 8))
          (let [placed (mapv arg-pos (calls p "place"))]
            (is (<= 4 (count placed) 14) "only the open side and corner cells of the 16, less the ones already solid, as many as it takes to close the room")
            (is (every? #(<= (:y %) 65) placed) "roof and support already solid: none placed up there"))
          (is (= 1 (count (emitted seen :shut-in.sealed))))
          (is (some? (:door (last (entries eng :shelter))))))))))

(deftest dig-in-enclose-with-too-few-blocks-stops-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 3}] :blocks overhang})]
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:walls} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (empty? (calls p "place")))
          (is (empty? (calls p "dig")))
          (is (= [:no-way] (mapv :reason (emitted seen :stopped)))))))))

(deftest fill-cells-count-the-walls-a-cell-needs-by-a-sealed-predicate
  (let [feet {:x 0 :y 64 :z 0}
        none (constantly false)
        roofed #(= 66 (:y %))]
    (is (= 10 (count (dig-cells/fill-cells none feet))) "sides, support, roof")
    (is (= 8 (count (dig-cells/fill-cells roofed feet))) "a sealed roof cell needs no support")))

(deftest fill-cells-include-the-corners-a-mob-could-stand-in
  (let [feet {:x 0 :y 64 :z 0}
        floor? #(= 63 (:y %))
        corner? (fn [c] (and (not= 0 (:x c)) (not= 0 (:z c)) (<= 64 (:y c) 65)))
        corners (fn [sealed?] (set (filter corner? (dig-cells/fill-cells sealed? feet))))]
    (is (= 8 (count (corners floor?))) "on flat ground a mob stands in every corner, feet and head height")
    (is (= #{{:x 1 :y 65 :z 1}} (corners #(or (floor? %) (= {:x 1 :y 64 :z 1} %) (and (corner? %) (not= {:x 1 :y 65 :z 1} %)))))
        "a rock corner at feet height leaves its head cell as a stand")
    (is (empty? (corners (constantly false))) "no floor under a corner: nobody stands there")))

(deftest fill-cells-corners-follow-what-a-mob-can-stand-on
  (let [feet {:x 0 :y 64 :z 0}
        floor? #(= 63 (:y %))
        slab {:x 1 :y 63 :z 1}
        corners (fn [stand-on?]
                  (set (filter #(and (not= 0 (:x %)) (not= 0 (:z %)) (<= 64 (:y %) 65))
                               (dig-cells/fill-cells floor? feet nil stand-on?))))]
    (is (= #{{:x 1 :y 64 :z 1} {:x 1 :y 65 :z 1}}
           (set (filter #(= [1 1] [(:x %) (:z %)]) (corners #(or (floor? %) (= slab %))))))
        "a corner floor that is no full cube but is stood on (a slab top) is walled out")
    (is (empty? (filter #(= [1 1] [(:x %) (:z %)]) (corners #(and (floor? %) (not= slab %)))))
        "a corner floor nobody stands on needs no walls")))

(deftest fill-cells-put-the-roof-support-away-from-the-threats
  (let [feet {:x 0 :y 64 :z 0}
        none (constantly false)
        beside? (fn [cell] (= 1 (+ (Math/abs (:x cell)) (Math/abs (:z cell)))))
        supports (fn [sealed? threats] (filterv #(and (= 66 (:y %)) (beside? %)) (dig-cells/fill-cells sealed? feet threats)))]
    (is (= [{:x 1 :y 66 :z 0}] (supports none [])) "no threat: the +x side")
    (is (= [{:x -1 :y 66 :z 0}] (supports none [{:x 4.5 :y 66 :z 0.5}])) "a zombie on the +x side: the -x one")
    (is (= [] (supports #(= {:x 0 :y 66 :z 1} %) [{:x 4.5 :y 66 :z 0.5}])) "one placed already: no other")))

(deftest ms-until-morning-follows-the-game-rate
  (settings/set-clock! 40 false :packet)
  (try
    (is (= 238525 (sh-clock/ms-until-morning 14000)) "the ticks at 25 ms, plus the same margin")
    (finally (settings/set-clock! 20 false :assumed))))

(defn reads-during
  "How many sensedAt reads (f) makes on p."
  [p f]
  (let [n (atom 0)
        orig (.-sensedAt p)]
    (set! (.-sensedAt p) (fn [& args] (swap! n inc) (.apply orig p (to-array args))))
    (f)
    @n))

(deftest the-night-trigger-in-a-wide-cave-floods-the-room-once-for-a-body-standing-still
  (let [p (seen-dark cave-blocks)
        ask #(reads-during p (fn [] ((:when (get triggers/all :night)) p {} {})))
        first-ask (ask)]
    (is (< (ask) (/ first-ask 5)) "the second ask reuses the room")))

(def displaced-view {:now 0 :data {:entries {:displaced [{:t 0 :data {}}]}}})

(deftest bed-radii-follow-settings-unless-the-args-name-one
  (are [overrides view args radius]
       (= radius (settings/with-settings overrides #(sh-beds/bed-radius view args)))
    {} {:now 0} {} 48
    {:jobs.lib.shelter.beds/default-bed-radius 80} {:now 0} {} 80
    {:jobs.lib.shelter.beds/default-bed-radius 80} {:now 0} {:bed-radius 20} 20
    {} displaced-view {} 128
    {:jobs.lib.shelter.beds/urgent-bed-radius 200} displaced-view {} 200
    {:jobs.lib.shelter.beds/urgent-bed-radius 200} displaced-view {:urgent-bed-radius 90} 90))

(deftest sleep-takes-a-bed-beyond-the-default-radius-when-the-setting-allows
  (let [{:keys [eng]} (setup {:time night :blocks {"60,64,0" "red_bed"}})]
    (know-bed! eng {:x 60 :y 64 :z 0})
    (core/submit! eng '(jobs.survival.sleep) {})
    (is (some? (settings/with-settings {:jobs.lib.shelter.beds/default-bed-radius 80} #(core/tick! eng))))))

(def tunnel-blocks
  "A 1-wide tunnel x 0..9 at y 64-65 in solid stone x -3..12, z -3..3, y 62..67; open-end? opens x 9 into a cave x 10..60."
  (fn [open-end?]
    (into (if open-end?
            (into {} (for [x (range 10 61) z (range -3 4) y [63 66]] [(str x "," y "," z) "stone"]))
            {})
          (for [x (range -3 13) z (range -3 4) y (range 62 68)
                :when (not (and (<= 0 x 9) (zero? z) (#{64 65} y)))
                :when (not (and open-end? (<= 10 x)))]
            [(str x "," y "," z) "stone"]))))

(defn sight-of-tunnel
  "A fake body at x 0 of the tunnel that has had one sight pass, not seen everything. The tunnel is lit 14; the cave,
  when open, is lit fading from the mouth and dark beyond 23."
  [open-end?]
  (let [raw-p (tu/fake {:time night :yaw -90 :blocks (tunnel-blocks open-end?)})
        _ (swap! (fake/state raw-p) assoc :light-default [0 0]
                 :light (into {} (for [x (range 0 (if open-end? 24 10)) z (if open-end? (range -3 4) [0]) y [64 65]]
                              [[x y z] [0 (max 0 (- 14 (max 0 (- x 9))))]])))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
        p (perception/wrap raw-p per)]
    (perception/pass! per)
    p))

(deftest a-lit-dead-end-tunnel-is-shelter-though-its-sides-were-not-looked-at
  (is (true? (sh-inside/safe-from-night? (sight-of-tunnel false)))))

(deftest a-tunnel-open-into-a-dark-cave-is-no-shelter
  (is (false? (sh-inside/safe-from-night? (sight-of-tunnel true)))))



(deftest a-long-lit-tunnel-with-torches-is-shelter-though-far-walls-were-not-seen
  (let [raw-p (tu/fake {:time night :yaw -90 :blocks (into {} (for [x (range -3 53) z (range -3 4) y (range 62 68)
                                                                 :when (not (and (<= 0 x 49) (zero? z) (#{64 65} y)))]
                                                             [(str x "," y "," z) "stone"]))})
        _ (swap! (fake/state raw-p) assoc :light-default [0 0]
                 :light (into {} (for [x (range 0 50) y [64 65]]
                                   [[x y 0] [0 (max 0 (- 14 (apply min (map #(js/Math.abs (- x %)) (range 0 50 6)))))]])))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
        p (perception/wrap raw-p per)]
    (perception/pass! per)
    (is (true? (sh-inside/safe-from-night? p)))))
