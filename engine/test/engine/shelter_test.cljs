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
            [engine.blocks-dig-test :as bd]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.look :as look]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.torch :as torch]
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

(defn seen-box [blocks light-default & [light]]
  (let [raw-p (tu/fake {:time night :blocks blocks})
        _ (swap! (fake/state raw-p) #(cond-> (assoc % :light-default light-default) light (assoc :light light)))
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
    40 40 false [0 14] true
    40 40 false [0 0] false
    5 5 true [0 14] false))

(deftest a-sensed-hostile-or-fire-or-lava-in-or-beside-the-closed-room-makes-it-no-shelter
  (let [safe? (fn [world blocks] (sh-inside/safe-from-night?
                                  (let [raw-p (tu/fake (merge {:time night :blocks (merge (box 1 4 false) blocks)} world))
                                        _ (swap! (fake/state raw-p) assoc :light-default [0 14])
                                        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})]
                                    (perception/pass! per)
                                    (tu/seeing-all (perception/wrap raw-p per)))))
        zombie {:id 9 :name "zombie" :kind "hostile" :visible true :pos {:x 0.5 :y 64 :z 3.5}}]
    (is (true? (safe? {} {})))
    (is (false? (safe? {:entities [zombie]} {})) "a zombie seen in the room's other cell")
    (is (true? (safe? {:entities [(assoc zombie :pos {:x 0.5 :y 64 :z 6.5})]} {})) "one outside the walls is no inside danger")
    (is (false? (safe? {} {"0,64,3" "fire"})))
    (is (false? (safe? {} {"0,64,3" "lava"})))))

(defn without-blocks [blocks cells] (apply dissoc blocks (map (fn [[x y z]] (str x "," y "," z)) cells)))

(deftest a-two-wide-one-high-gap-in-a-wall-is-a-way-in-for-a-spider-but-a-one-wide-one-is-not
  (let [safe? (fn [gap] (sh-inside/safe-from-night? (seen-box (without-blocks (box 5 5 false) gap) [0 0])))]
    (is (true? (safe? [])))
    (is (true? (safe? [[-1 65 1]])) "a spider is 1.4 wide")
    (is (false? (safe? [[-1 65 1] [-1 65 2]])))
    (is (false? (safe? [[-1 64 1] [-1 64 2]])) "at the floor too")))

(deftest the-exposure-of-a-work-cell-follows-the-shelter-rule
  (let [chance (fn [w d at] (:chance (spawn-light/exposure (seen-box (box w d false) [0 0]) at)))]
    (is (zero? (chance 5 5 {:x 2 :y 64 :z 2})) "a dark closed room within the spawn-free radius")
    (is (zero? (chance 1 25 {:x 0 :y 64 :z 12})))
    (is (pos? (chance 1 40 {:x 0 :y 64 :z 39})) "the room reaches a dark spawnable cell beyond 24 blocks of it")))

(deftest the-exposure-of-a-dark-cell-under-the-sky-counts-only-far-dark-cells-a-mob-reaches-on-foot
  (let [unroofed (fn [w d] (into (into {} (remove (fn [[k _]] (let [[_ y _] (map js/parseInt (.split k ","))] (= y 66)))) (box w d false))
                                 (for [x (range -1 (inc w)) z (range -1 (inc d)) y [66 67 68 69]
                                       :when (not (and (<= 0 x (dec w)) (<= 0 z (dec d))))]
                                   [(str x "," y "," z) "stone"])))
        chance (fn [blocks at] (:chance (spawn-light/exposure (seen-box blocks [15 0]) at)))]
    (is (zero? (chance (unroofed 5 5) {:x 2 :y 64 :z 2})) "walled yard open to the night sky: no dark cell beyond 24 blocks")
    (is (zero? (chance (unroofed 20 20) {:x 10 :y 64 :z 10})) "a yard of 400 cells")
    (is (pos? (chance (unroofed 1 40) {:x 0 :y 64 :z 39})) "the walled yard reaches a far dark cell")
    (is (zero? (:chance (spawn-light/exposure (seen-box (unroofed 3 40) [15 0] (into {} (for [x (range 3) z (range 20 40) y [64 65]] [[x y z] [15 14]])))
                                              {:x 1 :y 64 :z 1})))
        "a yard longer than 24 blocks whose far end is lit")
    (is (pos? (chance (box 5 5 true) {:x 2 :y 64 :z 2})) "open sides: a flood too big to close is never safe")))

(deftest one-cell-gives-one-spawn-chance-to-readiness-the-bed-and-the-planner
  ;; exposure and the night stay are chance-at; the planner's coarse mask may call dark a cell chance-at frees, never lit one it prices
  (let [field (into {} (for [x (range -30 31) z (range -30 31)] [(str x ",63," z) "stone"]))]
    (doseq [[what blocks light at spawns? mask] [["a small dark roofed room" (box 5 5 false) [0 0] {:x 2 :y 64 :z 2} false 1]
                                                ["a dark roofed hall" (box 1 40 false) [0 0] {:x 0 :y 64 :z 39} true 1]
                                                ["a torch-lit room" (assoc (box 5 5 false) "4,64,4" "torch") [0 14] {:x 2 :y 64 :z 2} false 0]
                                                ["open ground under the night sky" field [15 0] {:x 0 :y 64 :z 0} true 1]]]
      (let [p (seen-box blocks light)
            chance (spawn-light/chance-at p at false)]
        (is (= spawns? (pos? chance)) what)
        (is (= chance (:chance (spawn-light/exposure p at)) (spawn-light/night-chance p at)) what)
        (is (= mask ((:at (look/dark-fn p)) (:x at) (:y at) (:z at))) what)))))

(deftest a-one-high-gap-over-a-drop-beside-a-one-cell-pocket-is-no-shelter
  (let [safe? (fn [cells] (sh-inside/safe-from-night? (seen-box (assoc (without-blocks (box 1 1 false) cells) "-1,62,0" "stone") [0 0])))]
    (is (true? (safe? [])))
    (is (false? (safe? [[-1 64 0] [-1 63 0]])) "no walker gets in, but a mob in the drop below the gap reaches the body")))

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
    (is (false? (:seen? (sh-sense/light-view (tu/fake {:self {:status "offline" :pos nil}}) {}))) "offline: no feet to read, a note not a throw")
    (is (true? (:seen? (sh-sense/light-view (tu/with-light-data (tu/fake {:time night :blocks cave-blocks :self {:status "offline" :pos nil}})) {:cell {:x 0 :y 64 :z 0}}))) "offline with a given cell and light data: no throw")
    (is (= {:seen? false :light nil :lit? nil} (select-keys (sh-sense/light-view (tu/with-light-data (tu/fake {:time night :blocks cave-blocks :unloaded ["40,64,0"]})) {:cell {:x 40 :y 64 :z 0}}) [:seen? :light :lit?])) "no light data: unknown, not a guess")
    (is (= {:cells 27 :radius 1 :darkest 0} (select-keys (sh-sense/light-view lit {:cell {:x 0 :y 64 :z 0} :radius 1}) [:cells :radius :darkest])))))

(deftest light-view-lit-agrees-with-the-reported-light-of-a-cell-not-looked-at
  (let [raw-p (tu/fake {:time noon :blocks cave-blocks})
        _ (swap! (fake/state raw-p) assoc :light-default [0 0])
        p (perception/wrap raw-p (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)}))
        view (sh-sense/light-view p {:cell {:x 3 :y 64 :z 0}})]
    (is (= {:block 0 :sky 0 :light 0 :lit? false} (select-keys view [:block :sky :light :lit?])))))

(deftest night-without-light-data-falls-back-to-the-seen-roof
  (is (sh-sense/buried-by-column? (seen-fake {:time night :blocks (stone-above 2 1)})) "one seen block over the head, air above it")
  (is (not (sh-sense/buried-by-column? (seen-fake {:time night :blocks (stone-above 5 3)}))) "air over the head")
  (is (not (sh-sense/buried-by-column? (seen-fake {:time night :blocks {"0,66,0" "sugar_cane"}}))) "sugar cane over the head is no roof")
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

(deftest buried-at-judges-any-cell-by-its-light-and-roof-when-the-body-has-seen-it
  (are [world cell expected] (= [expected] (buried-cells world [cell]))
    (merge dark {:blocks (stone-above 2 1)}) {:x 0 :y 64 :z 0} true
    ;; seen, open sky, a roof one block thick: lit, not buried
    {:blocks (stone-above 3 1) :light {[0 65 0] [15 0] [0 66 0] [15 0]}} {:x 0 :y 65 :z 0} false
    ;; seen cell in a tall cave: no sky light, ceiling far over
    (merge dark {:blocks (stone-above 10 1)}) {:x 0 :y 65 :z 0} true
    ;; the same cave lit by a block, its column seen open past roof-height: lit open land (Q31); under a low roof still underground
    (merge dark {:blocks (stone-above 10 1) :light {[0 64 0] [0 14]}}) {:x 0 :y 64 :z 0} false
    (merge dark {:blocks (stone-above 3 1) :light {[0 64 0] [0 14]}}) {:x 0 :y 64 :z 0} true
    ;; never seen
    (merge dark {:blocks (stone-above 2 1)}) {:x 30 :y 64 :z 0} false
    (merge dark {:blocks (stone-above 2 1)}) {:x 0 :y 64 :z 1} false))

(deftest a-roofed-body-logs-out-for-a-sleeper-from-the-register
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :blocks {"0,66,0" "stone"}})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
          (await (tick-n eng 3))
          (is (< 1 (count (calls p "offline"))) "the roof does not matter; out again while the sleeper sleeps")
          (is (= (min 10 (count (calls p "offline"))) (count (entries eng :log-out))) "each stint booked (cap 10)"))))))

(def own-shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built})

(deftest night-does-not-hold-at-night-while-shut-in-its-own-shelter
  (let [{:keys [eng]} (setup {})
        holds? (fn [world] (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {})))
        leaves? (fn [world] (boolean ((:when (get triggers/all :dawn)) (tu/fake world) (mem/view (:store eng)) {})))]
    (mem/write! (:store eng) :shelter own-shelter {:cap 10 :ttl day-ms})
    (is (not (holds? {:time night :blocks {"0,66,0" "stone"}})) "shut in at night is safe: the queue runs")
    (is (not (holds? {:time night :blocks {"0,66,0" "stone"} :self {:isSleeping true}})) "asleep")
    (is (leaves? {:time noon :blocks {"0,66,0" "stone"}}) "by day the dawn trigger lets the body out")
    (is (not (leaves? {:time night :blocks {"0,66,0" "stone"}})))
    (is (not (holds? {:time night :blocks {"0,66,0" "stone" "3,66,0" "stone"} :self {:pos {:x 3 :y 64 :z 0}}}))
        "roofed elsewhere, out of its shelter")))

(def walls
  (into {} (for [[x z] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str x "," y "," z) "stone"])))

(deftest a-pit-with-a-way-out-is-left-by-day
  (let [{:keys [eng]} (setup {})
        holds? (fn [world] (boolean ((:when (get triggers/all :dawn)) (tu/fake world) (mem/view (:store eng)) {})))]
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

(deftest sleep-takes-the-spawn-bed-at-any-distance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:time night :blocks {"60,64,0" "red_bed"}})]
          (mem/write! (:store eng) :spawn-bed {:pos {:x 60 :y 64 :z 0}} {:cap 1 :ttl :forever})
          (core/submit! eng '(jobs.survival.sleep) {})
          (let [round (core/tick! eng)]
            (is (some? round))
            ;; a round left running walks on in the next tests, under their go-to's job id "j1/go"
            (await round)))))))

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
          (is (= [{:ms 30000 :status "ok" :assumed true}] (entries eng :log-out)))
          (is (= {:cap 10 :ttl day-ms} (mem/policy (mem/view (:store eng)) :log-out))))))))

(deftest log-out-declines-by-day
  (let [{:keys [eng]} (setup {:time noon})]
    (core/submit! eng '(jobs.survival.log-out) {})
    (is (nil? (core/tick! eng)))))

(deftest log-out-unsupported-is-impossible-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[seed! spec] [[#(mem/write! (:store %) :log-out {:ms 0 :status "unsupported"} {:cap 10 :ttl day-ms}) '(jobs.survival.log-out)]]]
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

(deftest dig-in-holds-the-cheapest-quick-pickaxe-before-each-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[inventory held names tool]
                [[[{:name "stone_pickaxe" :count 1} {:name "iron_pickaxe" :count 1} {:name "dirt" :count 1}] "dirt" ["equip" "dig" "dig"] "stone_pickaxe"]
                 [[{:name "iron_pickaxe" :count 1}] nil ["equip" "dig" "dig"] "iron_pickaxe"]
                 [[{:name "iron_pickaxe" :count 1}] "iron_pickaxe" ["dig" "dig"] "iron_pickaxe"]]]
          (let [{:keys [eng p]} (setup {:time night :blocks stone-pit :inventory inventory :self {:held held}})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (= names (act-names p)) (pr-str inventory held))
            (is (= tool (.-held (.self p))) (pr-str inventory held))))))))

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

(deftest dig-in-does-not-dig-a-pit-through-the-only-open-side-column-with-a-hostile-in-it-and-keeps-the-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory [{:name "dirt" :count 3}]
                                           :blocks one-side-open})]
          (fake/add-entity! p {:id 7 :name "zombie" :kind "hostile" :pos {:x 1 :y 64 :z 0}})
          (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
          (await (run-until-empty eng 8))
          (is (= [] (calls p "dig")) "no pit towards the hostile")
          (is (= [] (entries eng :shut-in-failed)) "a passing hostile does not make the site futile"))))))

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
        (let [{:keys [eng p]} (setup {:time night :entities [sleeper] :players ["Alex"] :blocks bed-room :light {} :light-default [15 0]})]
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
          (is (= [{:gone true :was {:x 6 :y 64 :z 0}}] (entries eng :bed))))))))

(deftest a-dug-in-shelter-holds-the-body-until-day-then-the-queued-job-runs
  (async done
    (tu/run-async done
      (walls-cheap (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack :blocks floor :entities [awake]})
              _ (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
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
              _ (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
              _ (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
              mid (await (night-then-day eng p 4 #(hash-map :y (:y (pos-of p)) :holder (some? (core/holder eng)) :notified (notified seen)) 6))]
          (is (= {:y 61 :holder false :notified 1} mid) "in the pit, safe: the queued job ran at night")
          (is (nil? (core/holder eng)) "the shelter ended at day")
          (is (>= (:y (pos-of p)) 64) "the body stands on the surface, out of the pit")
          (is (empty? (emitted seen :stopped)) "out: done, not failed")
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(deftest the-night-ends-at-first-light-and-leave-shelter-takes-the-body-out-of-the-pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:time night :blocks ground})
              _ (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
              _ (await (night-then-day eng p 4 (constantly nil) 6))]
          (is (nil? (core/holder eng)) "the night is over")
          (is (< (:y (pos-of p)) 64) "and it left the body in the pit")
          (is (boolean ((:when (get triggers/all :dawn)) p (mem/view (:store eng)) {} nil)) "the dawn trigger holds for the shelter")
          (core/submit! eng '(jobs.survival.leave-shelter) {})
          (await (run-until-empty eng 20))
          (is (>= (:y (pos-of p)) 64) "leave-shelter climbed out")
          (is (not ((:when (get triggers/all :dawn)) p (mem/view (:store eng)) {} nil))))))))

(deftest a-pit-shelter-with-no-way-out-ends-nothing-possible-by-day-and-the-queued-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks stone-ground :inventory [{:name "dirt" :count 1}]})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (let [wall-in! (fn [] (let [{:keys [x y z]} (pos-of p)]
                                  (doseq [dx (range -3 4) dz (range -3 4) dy (range -1 6) :when (not= [0 0] [dx dz])]
                                    (fake/set-block! p [(+ (js/Math.floor x) dx) (+ (js/Math.floor y) dy) (+ (js/Math.floor z) dz)] "bedrock"))))
                mid (await (night-then-day eng p 4 #(do (wall-in!) (hash-map :pit (< (:y (pos-of p)) 64) :notified (notified seen))) 6))]
            (is (= {:pit true :notified 1} mid) "in the pit, safe: the queued job ran at night"))
          (is (seq (emitted seen :dig-in.trapped)) "bedrock round the pit: trapped")
          (is (= [:no-way-out] (mapv :reason (emitted seen :stopped))) "one ending, nothing possible")
          (is (nil? (core/holder eng)) "the shelter ended: the body is the agent's again")
          (is (= 1 (notified seen)) "the queued job ran"))))))

(deftest a-pit-shelter-with-a-hostile-near-still-leaves-by-day-and-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :blocks ground})
              day? (atom false)]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          ;; the fake clock only moves when told: each hold of the day lets 1500 ticks pass, so the unarmed grace runs out
          (on-wait! p (fn [n] (when @day? (.setTime (.-world p) (+ noon (* n 1500))))))
          (await (night-then-day eng p 4 #(do (reset! day? true) (fake/add-entity! p {:id 7 :name "zombie" :kind "hostile" :pos {:x 2 :y 64 :z 0}})) 12))
          (is (>= (:y (pos-of p)) 64) "out of the pit once the unarmed grace is over: a hostile is the hostile reflex's business, not the shelter's")
          (is (nil? (core/holder eng)) "no hold: the shelter ended")
          (is (empty? (emitted seen :dig-in.staying)))
          (is (empty? (emitted seen :stopped)))
          (is (= 1 (notified seen)) "then the queued job ran"))))))

(deftest a-sleeping-shelter-holds-until-day-then-the-queued-job-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :skipNight false :blocks bed-room})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
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

(deftest shelter-does-not-walk-to-a-bed-no-route-reaches
  (async done
    (tu/run-async done
      (walls-cheap (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:time night :inventory wall-stack :blocks (merge floor {"6,64,0" "red_bed"}) :entities [awake]
                                           :unreachable ["6,64,0"]})]
          (know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-n eng 24))
          (is (= [{:x 6 :y 64 :z 0}] (map :bed (emitted seen :night.bed-walk-unsafe))))
          (is (zero? (walks-to-bed eng)) "no walk at the unreachable bed")
          (is (= 18 (count (calls p "place"))) "dig-in finished the walls")))))))

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
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
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

(deftest a-shelter-that-cannot-roof-logs-out-and-fires-again-without-digging-in-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:time night :inventory (conj wall-stack {:name "iron_pickaxe" :count 1}) :blocks floor})]
          (refuse-placing! p)
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (tick-nights eng clock 1))
          (let [places (count (calls p "place"))]
            (is (= (+ 1 (* 4 (sites/max-sites))) places) "4 places per shut-in call (one per wall), max-sites calls, after the first try's roof support (its walls under the 1-thick floor have nothing the eye can click)")
            (is (= 1 (count (emitted seen :shelter.exposed))) "one warn that the body is unsheltered")
            (await (tick-nights eng clock 2))
            (is (= places (count (calls p "place"))) "the failed sites are not dug in again")
            (is (pos? (count (calls p "offline"))) "logged out, not held")
            (is (< 1 (count (filter #(= :fired (:kind %)) (emitted-by seen :reflex :night)))) "the night fires again after its cooldown")
            (is (= [] (declined-events seen :night)))
            (is (pos? (notified seen)) "the queued job ran between firings")))))))

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
          (let [{:keys [eng p seen]} (setup {:time night :blocks blocks :inventory [{:name "dirt" :count 1} {:name "iron_pickaxe" :count 1}]})]
            (core/submit! eng '(jobs.survival.night) {})
            (await (tick-n eng 3))
            (is (= [] (calls p "dig")) (pr-str blocks))
            (is (= 5 (count (no-way-stops seen))) (str (pr-str blocks) ": one try at the site, then max-sites sites in all, then the niche"))
            (is (= reason (:reason (first (entries eng :shut-in-failed)))) (str (pr-str blocks) ": dig-in's futile entry written and kept for the next firing in the dark"))))))))

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

(deftest a-failed-sleep-is-tried-again-once-the-entry-expires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:time night :blocks (merge floor {"3,64,0" "red_bed"}) :floor [-4 -4 4 4]})
              tries (atom 0)
              night-once (fn ^:async once [] (swap! clock + 11000) (.setTime (.-world p) night)
                           (core/submit! eng '(jobs.survival.night {:settings {:jobs.lib.shelter.beds/max-days-awake 0}}) {})
                           (await (tick-n eng 6)))]
          (set! (.-sleep p) (fn [& _] (swap! tries inc) (js/Promise.resolve #js {:status "occupied"})))
          (await (night-once))
          (let [first-tries @tries]
            (is (pos? first-tries))
            (await (night-once))
            (is (= first-tries @tries) "the failed entry holds it off")
            (swap! clock + (:ttl sh-beds/sleep-failed-policy) 11000)
            (await (night-once))
            (is (< first-tries @tries) "after the entry expired the bed is tried again")))))))

(deftest a-place-failure-holds-only-near-where-it-happened
  (let [p (tu/fake {:time night})]
    (are [data held] (= held (sh-beds/near-failed-place? p {:data data}))
      {:pos {:x 0 :y 64 :z 0}} true
      {:pos {:x 3 :y 64 :z 0}} true
      {:pos {:x 20 :y 64 :z 0}} false
      {} true)))

(deftest an-unsheltered-night-after-a-futile-dig-digs-nothing-and-lets-the-queue-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup {:time night :drops {"iron_ore" "raw_iron"} :inventory [{:name "iron_pickaxe" :count 1}]
                                                 :blocks ore-ground})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night} {:trigger :dawn}]}"))
          (core/submit! eng '(jobs.debug.notify {:text "after"}) {})
          (await (tick-nights eng clock 1))
          (is (= 0 (count (calls p "dig"))) "nothing to roof the pit with: no dig")
          (is (= 1 (notified seen)) "the queued job ran"))))))

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

(deftest a-futile-pit-is-not-dug-again-on-later-firings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (setup (assoc futile-world :floor [-4 -4 4 4]))
              eng (update eng :triggers assoc :always-shelter always-shelter)]
          (refuse-placing! p)
          (core/register-reflex! eng {:trigger :always-shelter})
          (await (tick-n eng 6))
          (let [held {:futile (entries eng :shut-in-failed) :places (count (calls p "place")) :digs (count (calls p "dig"))}]
            (is (= #{{:way :pit :reason :roof-failed} {:way :walls :reason :failed-steps}} (set (map #(select-keys % [:way :reason]) (:futile held)))) "written in the first firing")
            (is (= 4 (:places held)) "the pit's roof and the walls' first place, each refused once; no second pit is roofed")
            (await (tick-nights eng clock 3))
            (is (= (:digs held) (count (calls p "dig"))) "later firings do not dig again"))
          (is (= [] (declined-events seen :always-shelter))))))))

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
        (doseq [[cell reason] [["0,64,0" nil] ["0,65,0" :fluid-here]]]
          (let [{:keys [eng p]} (setup {:time night :blocks (merge ground {cell "water"})})]
            (core/submit! eng '(jobs.survival.shut-in {:ways #{:room :walls :pit} :for :night}) {})
            (await (run-until-empty eng 8))
            (is (empty? (calls p "dig")) cell)
            (is (= reason (:reason (first (entries eng :shut-in-failed)))) (str cell ": feet in water try no way (:in-water), the site is not at fault"))))))))

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
            (.override (.-world p) "steer" (fn ^:async f [_ _ _] #js {:status status}))
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
          (await (tick-n eng 8))
          (is (= {:way :walls :reason :failed-steps} (select-keys (first (entries eng :shut-in-failed)) [:way :reason])) "written in the first firing")
          (let [digs (count (calls p "dig"))
                places (count (calls p "place"))]
            (swap! clock + 11000)
            (await (tick-n eng 8))
            (is (= places (count (calls p "place"))) "the refused cells are not tried again")
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
    (is (= 10 (count (dig-cells/fill-cells none feet nil none))) "sides, support, roof")
    (is (= 8 (count (dig-cells/fill-cells roofed feet nil roofed))) "a sealed roof cell needs no support")))

(deftest fill-cells-include-the-corners-a-mob-could-stand-in
  (let [feet {:x 0 :y 64 :z 0}
        floor? #(= 63 (:y %))
        corner? (fn [c] (and (not= 0 (:x c)) (not= 0 (:z c)) (<= 64 (:y c) 65)))
        corners (fn [sealed?] (set (filter corner? (dig-cells/fill-cells sealed? feet nil sealed?))))]
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
        supports (fn [sealed? threats] (filterv #(and (= 66 (:y %)) (beside? %)) (dig-cells/fill-cells sealed? feet threats sealed?)))]
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

(deftest bed-radii-follow-settings
  (are [overrides view radius]
       (= radius (settings/with-settings overrides #(sh-beds/bed-radius view)))
    {} {:now 0} 48
    {:jobs.lib.shelter.beds/default-bed-radius 80} {:now 0} 80
    {} displaced-view 128
    {:jobs.lib.shelter.beds/urgent-bed-radius 200} displaced-view 200))

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

(def lit-tunnel-box
  "Stone x -3..60, z -3..12, y 62..67 with a lit 1-wide tunnel x 0..19 (y 64-65); a branch at x 12 (z 1..) leads to a hall x 8..40, z 1..12.
  The tunnel is lit 14 all along, the branch cell by the tunnel 13, the hall dark."
  (fn [branch?]
    (let [open? (fn [x y z] (and (#{64 65} y)
                                 (or (and (zero? z) (<= 0 x 19))
                                     (and branch? (= 12 x) (<= 1 z 12))
                                     (and branch? (<= 8 x 40) (<= 1 z 12)))))]
      {:blocks (into {} (for [x (range -3 61) z (range -3 13) y (range 62 68) :when (not (open? x y z))] [(str x "," y "," z) "stone"]))
       :light (into {} (for [x (range 0 20) y [64 65]] [[x y 0] [0 14]]))})))

(defn lit-tunnel-body [branch? seen]
  (let [{:keys [blocks light]} (lit-tunnel-box branch?)
        raw-p (tu/fake {:time night :yaw -90 :blocks blocks})
        _ (swap! (fake/state raw-p) assoc :light-default [0 0] :light (cond-> light branch? (assoc [12 64 1] [0 13] [12 65 1] [0 13])))]
    (if-not (= :perceived seen)
      raw-p
      (let [per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
            p (perception/wrap raw-p per)]
        (perception/pass! per)
        p))))

(deftest a-lit-dead-end-tunnel-whose-walls-are-seen-is-shelter
  (is (true? (sh-inside/safe-from-night? (lit-tunnel-body false :all)))))

(deftest a-lit-tunnel-with-a-side-branch-to-a-dark-hall-is-no-shelter-until-the-branch-is-seen-closed
  (are [seen] (false? (sh-inside/safe-from-night? (lit-tunnel-body true seen)))
    :perceived
    :all))

(defn tunnel-edge-body
  "The lit dead-end tunnel after one sight pass, with its light replaced by `light` when given."
  [light]
  (let [{:keys [blocks] :as box} (lit-tunnel-box false)
        raw-p (tu/fake {:time night :yaw -90 :blocks blocks})
        _ (swap! (fake/state raw-p) assoc :light-default [0 0] :light (or light (:light box)))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
        p (perception/wrap raw-p per)]
    (perception/pass! per)
    p))

(defn tunnel-seen-body
  "The lit dead-end tunnel with every cell in range seen (seeded, so the farthest roofed cell is exact)."
  []
  (let [{:keys [blocks light]} (lit-tunnel-box false)
        raw-p (tu/fake {:time night :yaw -90 :blocks blocks})]
    (swap! (fake/state raw-p) assoc :light-default [0 0] :light light)
    (tu/seeing-all raw-p)))

(deftest the-edge-of-a-lit-region-is-looked-at-but-a-side-of-only-dark-cells-is-skipped
  (let [lit (look-edge/pending {:primitives (tunnel-edge-body nil)} #{} 0)]
    (is (seq lit) "an unseen side beside lit cells is looked at")
    (is (= lit (sort-by (fn [[x]] x) lit)) "nearest first")
    (is (empty? (look-edge/pending {:primitives (tunnel-edge-body {})} #{} 0)) "dark cells are never shown by a look: the region stays open")))

(deftest the-edge-walk-stops-after-fifteen-cells-or-two-hundred-steps
  (let [c {:primitives (tunnel-edge-body nil)}
        some-cell (first (look-edge/pending c #{} 0))
        fifteen (set (map (fn [i] [i 100 100]) (range look-edge/max-edge-looks)))]
    (is (seq (look-edge/pending c (disj fifteen [0 100 100]) 0)) "14 tried: more to do")
    (is (empty? (look-edge/pending c fifteen 0)) "15 tried cells")
    (is (seq (look-edge/pending c #{} (dec look-edge/max-edge-steps))))
    (is (empty? (look-edge/pending c #{} look-edge/max-edge-steps)) "200 steps")
    (is (some? some-cell))))

(deftest walkable-near-is-the-reachable-ground-nearest-the-cell
  (let [p (tunnel-seen-body)]
    (is (= {:x 19 :y 64 :z 0} (look-edge/walkable-near p [23 64 0])) "the farthest cell whose roof was seen, not past the dead end")
    (is (= {:x 5 :y 64 :z 0} (look-edge/walkable-near p [5 64 3])))))

(deftest walkable-near-is-nil-when-the-ground-is-farther-than-an-edge-walk-may-go
  (let [p (tunnel-seen-body)]
    (with-redefs [look-edge/max-edge-walk 10]
      (is (nil? (look-edge/walkable-near p [23 64 0])))
      (is (= {:x 5 :y 64 :z 0} (look-edge/walkable-near p [5 64 3]))))))

(deftest walkable-near-stays-inside-the-closed-region-and-does-not-open-its-door
  (let [{:keys [blocks light]} (lit-tunnel-box false)
        raw-p (tu/fake {:time night :yaw -90 :blocks (assoc blocks "10,64,0" "oak_door" "10,65,0" "oak_door")})
        _ (swap! (fake/state raw-p) assoc :light-default [0 0] :light light)
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})
        p (perception/wrap raw-p per)]
    (perception/pass! per)
    (is (<= (:x (look-edge/walkable-near p [15 64 0])) 9) "the far side of a shut door is not walked to")))

(deftest an-edge-cell-is-looked-at-walked-towards-looked-at-again-then-counted-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tunnel-seen-body)
              mem* (atom {})
              c {:primitives p :id "edge-stub" :root "edge-stub" :slots []
                 :view (fn [] {:now 1000000 :data {:entries {(mem/job-kind "edge-stub") [{:t 1000000 :data @mem*}]}
                                                    :policies {(mem/job-kind "edge-stub") mem/job-policy}}})
                 :update-mem (fn [f more] (apply swap! mem* f more))}
              cells [[20 64 0] [23 64 0] [30 64 0]]
              log (atom [])
              walk-result (atom :done)]
          (with-redefs [dig-look/look-at! (fn [_ cell] (swap! log conj [:look cell]) (js/Promise.resolve true))
                        ctx/call (fn [_ _ job args] (swap! log conj [:walk job (:pos args)]) (js/Promise.resolve @walk-result))]
            (reset! walk-result :continue)
            (await (look-edge/look! c cells))
            (await (look-edge/look! c cells))
            (reset! walk-result :done)
            (await (look-edge/look! c cells))
            (await (look-edge/look! c cells))
            (is (empty? (:edge-looked @mem*)) "not yet tried")
            (await (look-edge/look! c cells)))
          (is (= [:look :walk :walk :look] (map first @log))
              "look, walk (still walking: again), walk done, look again")
          (is (= 19 (:x (nth (nth @log 1) 2))) "the stride reaches the cell 3 away, not the one 10 away")
          (is (= #{[20 64 0]} (:edge-looked @mem*)))
          (is (= 5 (:edge-steps @mem*))))))))

(deftest the-night-looks-at-the-unseen-sides-of-a-lit-tunnel-and-is-done-without-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [blocks light]} (lit-tunnel-box false)
              {:keys [eng p seen]} (setup {:time night :yaw -90 :blocks blocks})
              _ (swap! (fake/state p) assoc :light-default [0 0] :light light)
              _ (tu/seeing-after-look p)]
          (is (false? (sh-inside/safe-from-night? p)) "nothing seen: not yet known shut")
          (core/submit! eng '(jobs.survival.night) {})
          (await (tick-n eng 6))
          (is (seq (calls p "look")) "it looked at the unseen sides")
          (is (empty? (calls p "place")) "a region seen closed is no reason to dig in")
          (is (true? (sh-inside/safe-from-night? p))))))))

(deftest an-open-topped-pit-under-a-distant-roof-is-no-shelter-a-ringed-one-is
  (let [world (fn [ring?]
                (let [raw-p (tu/fake {:time night
                                      :blocks (into {} (concat
                                                        (for [x (range -6 7) z (range -6 7) y (range 55 64)
                                                              :when (not (and (zero? x) (zero? z) (>= y 62)))] [(str x "," y "," z) "stone"])
                                                        (for [x (range -6 7) z (range -6 7)] [(str x ",70," z) "stone"])
                                                        (when ring? (for [x [-1 0 1] z [-1 0 1] y [64 65] :when (not (and (zero? x) (zero? z)))] [(str x "," y "," z) "stone"]))))})]
                  (swap! (fake/state raw-p) assoc :light-default [0 0])
                  (fake/swap-self! raw-p assoc :pos [0.5 62 0.5])
                  raw-p))]
    (is (false? (sh-inside/safe-from-night? (world false))) "a mob on the rim steps down into the pit")
    (is (true? (sh-inside/safe-from-night? (world true))))))

(deftest only-a-sealed-room-with-a-far-dark-cell-wants-light
  (are [w d open-sides? light wanted?] (= wanted? (sh-inside/dark-room-at? (seen-box (box w d open-sides?) light) {:x 0 :y 64 :z 0}))
    1 40 false [0 0] true
    1 40 false [0 14] false
    5 5 false [0 0] false
    1 40 true [0 0] false))

(deftest a-dark-hall-reaches-beyond-the-spawn-free-radius-whether-open-floored-or-sealed-a-small-dark-room-does-not
  (are [w d open-sides? light hall?] (= hall? (sh-inside/dark-hall-at? (seen-box (box w d open-sides?) light) {:x 0 :y 64 :z 0}))
    1 40 false [0 0] true
    1 40 true [0 0] true
    1 40 false [0 14] false
    5 5 false [0 0] false))

(deftest a-bed-shut-in-a-dark-hall-is-not-sheltered
  (are [w d light sheltered?] (= sheltered? (sh-beds/bed-sheltered? (seen-box (box w d false) light) {:x 0 :y 64 :z 0}))
    5 5 [0 0] true
    1 40 [0 14] true
    1 40 [0 0] false))

(deftest a-sealed-dark-hall-is-lit-with-the-torches-carried-before-any-wall-is-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw-p (tu/fake {:time night :blocks (box 1 40 false) :inventory [{:name "torch" :count 16}] :drops {"torch" "torch"}})
              _ (swap! (fake/state raw-p) assoc :light-default [0 0])
              p (tu/seeing-all (perception/wrap raw-p (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 1000000)
                                :events (events/make {:body "Fake" :sinks [] :now (constantly 1000000)})})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 30))
          (is (seq (for [z (range 40) y [64 65] :when (re-find #"torch" (or (bd/block-at raw-p {:x 0 :y y :z z}) ""))] z)) "torches hung in the hall")
          (is (empty? (for [z (range 40) :when (= "dirt" (bd/block-at raw-p {:x 0 :y 64 :z z}))] z)) "no wall or pit"))))))


(deftest a-sealed-dark-hall-is-not-lit-when-the-torches-carried-cannot-light-it-and-no-coal-makes-more
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw-p (tu/fake {:time night :blocks (box 1 40 false) :inventory [{:name "torch" :count 1} {:name "dirt" :count 64}] :drops {"torch" "torch"}})
              _ (swap! (fake/state raw-p) assoc :light-default [0 0])
              p (tu/seeing-all (perception/wrap raw-p (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 1000000)
                                :events (events/make {:body "Fake" :sinks [] :now (constantly 1000000)})})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (run-until-empty eng 30))
          (is (empty? (for [z (range 40) y [64 65] :when (re-find #"torch" (or (bd/block-at raw-p {:x 0 :y y :z z}) ""))] z)) "one torch cannot light the hall: a wall or pit is quoted instead"))))))

(defn ^:async torches-hung-in-dark-hall
  "Runs the night in a sealed dark hall carrying inv; the number of cells holding a torch afterwards."
  [inv]
  (let [raw-p (tu/fake {:time night :blocks (box 1 40 false) :inventory inv :drops {"torch" "torch"}})
        _ (swap! (fake/state raw-p) assoc :light-default [0 0])
        p (tu/seeing-all (perception/wrap raw-p (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now (constantly 1000000)
                          :events (events/make {:body "Fake" :sinks [] :now (constantly 1000000)})})]
    (core/submit! eng '(jobs.survival.night) {})
    (await (run-until-empty eng 30))
    (count (for [z (range 40) y [64 65] :when (re-find #"torch" (or (bd/block-at raw-p {:x 0 :y y :z z}) ""))] z))))

(deftest torches-short-for-the-hall-cannot-be-made-from-coal-without-a-stick
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (zero? (await (torches-hung-in-dark-hall [{:name "torch" :count 1} {:name "coal" :count 16} {:name "dirt" :count 64}])))
            "coal alone cannot make the missing torches: a wall or pit is quoted instead")))))

(deftest a-dark-hall-is-not-lit-when-nothing-can-light-it-and-no-way-is-quoted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (zero? (await (torches-hung-in-dark-hall [{:name "torch" :count 1} {:name "dirt" :count 2}])))
            "infinite light against infinite ways does not pick light")))))

(deftest sticks-come-from-carried-sticks-and-planks
  (are [inv n] (= n (torch/stick-supply (tu/fake {:inventory inv})))
    [{:name "stick" :count 3}] 3
    [{:name "oak_planks" :count 2} {:name "stick" :count 1}] 5
    [{:name "coal" :count 9}] 0))
