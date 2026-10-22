(ns engine.night-bed-safe-test
  "A body safe from the night (underground, dug in, shut in a closed room) is not sent through the dark to a remembered
  bed: sh-beds/bed-to-use skips a bed whose walk leaves that shelter (sh-inside/walk-leaves-shelter?), so the night trigger does not
  hold for it and the night job does not walk there."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.hut-shelter-test :as hut]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]))

(def far-bed {:x 40 :y 64 :z 0})

(def buried-light
  "Torch light and no sky light along the tunnel (x 0..20): sealed from the sky and lit."
  (into {} (for [x (range 0 21) y [64 65]] [[x y 0] [0 14]])))

(defn tunnel-world
  "A miner at x 5 in the one-wide stone tunnel (x 0..20, plugged at both ends), at night, with a bed block
  at far-bed in the open."
  [light]
  {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 0}} :time st/night :offlineScale 0.0001 :sees-all true
   :blocks (merge hut/tunnel {"-1,64,0" "stone" "-1,65,0" "stone" "21,64,0" "stone" "21,65,0" "stone" "40,64,0" "red_bed"}) :light light})

(defn lit-fake
  "{:raw :p}: a fake body and its perception wrap, so the raw world's sky light is read (sh-sense/buried?). :sees-all in the
  world: the fake's own sight (every block seen) with the perception only for its light."
  [world]
  (let [raw-p (tu/fake (dissoc world :light))
        _ (swap! (fake/state raw-p) assoc :light (:light world {}))
        per (perception/create (fake-raw/create raw-p) {:now (constantly 1000000)})]
    {:raw raw-p :p (if (:sees-all world)
                     (doto (js/Object.assign #js {} (tu/seeing-all raw-p)) (aset "perception" per))
                     (perception/wrap raw-p per))}))

(defn setup [world]
  (let [clock (atom 1000000)
        {:keys [raw p]} (lit-fake world)
        _ (st/dawn-after! raw st/default-dawn)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock) :events (events/make {:body "Fake" :sinks [] :now #(deref clock)})})]
    (st/know-bed! eng far-bed)
    {:eng eng :p p :raw raw :clock clock}))

(defn bed-to-use [world]
  (let [{:keys [eng p]} (setup world)]
    (sh-beds/bed-to-use p (mem/view (:store eng)) 64 (constantly true))))

(defn holds? [world]
  (let [{:keys [eng p]} (setup world)
        _ (st/looked-round! eng p)]
    (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {}))))

(deftest a-buried-body-does-not-use-a-bed-out-in-the-open
  (is (nil? (bed-to-use (tunnel-world buried-light))))
  (is (false? (holds? (tunnel-world buried-light))) "the night trigger does not hold"))

(def open-dark-tunnel
  "The tunnel with both plugs gone and no torch light: buried, but open at the ends and dark."
  (-> (tunnel-world (into {} (for [x (range 0 21) y [64 65]] [[x y 0] [0 0]])))
      (update :blocks dissoc "-1,64,0" "-1,65,0" "21,64,0" "21,65,0" "40,64,0")))

(deftest a-buried-miner-in-an-open-dark-tunnel-is-not-called-in-by-the-night
  (is (false? (holds? open-dark-tunnel)) "no sky light reaches it: waiting for day changes nothing"))

(deftest a-buried-body-neither-places-a-carried-bed-nor-logs-out-for-a-sleeper
  (let [carrying (assoc (tunnel-world buried-light) :inventory [{:name "red_bed" :count 1}])
        {:keys [eng p]} (setup carrying)]
    (st/looked-round! eng p)
    (mem/write! (:store eng) :sleep-status {:sleeping 1 :needed 2} nil)
    (is (false? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))) "mining goes on")))

(deftest a-buried-body-in-its-own-night-shelter-logs-out-for-a-sleeper
  (let [{:keys [eng p]} (setup (assoc (tunnel-world buried-light) :sees-all true))]
    (st/looked-round! eng p)
    (mem/write! (:store eng) :sleep-status {:sleeping 1 :needed 2} nil)
    (mem/write! (:store eng) :shelter {:pos {:x 5 :y 64 :z 0} :state :built} sh/shelter-policy)
    (is (true? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))))))

(deftest a-body-the-sky-reaches-logs-out-for-a-sleeper
  (let [{:keys [eng p]} (setup (tunnel-world {}))]
    (mem/write! (:store eng) :sleep-status {:sleeping 1 :needed 2} nil)
    (is (true? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))))))

(defn bed-to-use-after-days
  "bed-to-use for a body that last slept `days` in-game days ago."
  [world days]
  (let [{:keys [eng p]} (setup world)
        _ (mem/write! (:store eng) :slept {:pos {:x 0 :y 64 :z 0}} {:cap 10 :ttl (* 10 sh-clock/ms-per-day)})
        view (assoc (mem/view (:store eng)) :now (+ 1000000 (* days sh-clock/ms-per-day)))]
    (sh-beds/bed-to-use p view 128 (constantly true))))

(deftest a-buried-body-overdue-for-sleep-walks-to-its-bed
  (is (= far-bed (bed-to-use-after-days (tunnel-world buried-light) 3))))

(deftest a-buried-body-not-yet-overdue-stays-in-the-tunnel
  (is (nil? (bed-to-use-after-days (tunnel-world buried-light) 2))))

(deftest a-body-out-in-the-open-still-walks-to-its-bed
  (let [world {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 0}} :time st/night :blocks {"40,64,0" "red_bed"}}]
    (is (= far-bed (bed-to-use world)))
    (is (true? (holds? world)))))

(defn bed-after-looking
  "bed-to-use for a body that has looked round, `slept` true when it already slept tonight."
  [world slept]
  (let [{:keys [eng p]} (setup world)
        _ (st/looked-round! eng p)
        _ (when slept (mem/write! (:store eng) :slept {:pos {:x 0 :y 64 :z 0}} {:cap 10 :ttl (* 10 sh-clock/ms-per-day)}))]
    (sh-beds/bed-to-use p (mem/view (:store eng)) 64 (constantly true))))

(def open-bed-world {:sees-all true :floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 0}} :time st/night :blocks {"40,64,0" "red_bed" "8,64,0" "red_bed"}})

(deftest a-body-takes-the-nearest-bed-it-has-seen-though-another-is-remembered
  (is (= {:x 8 :y 64 :z 0} (bed-after-looking open-bed-world false))))

(deftest a-body-woken-in-the-open-goes-back-to-a-bed-but-a-sheltered-one-stays
  (is (= {:x 8 :y 64 :z 0} (bed-after-looking open-bed-world true)) "slept tonight, still outside")
  (is (nil? (bed-after-looking (tunnel-world buried-light) true)) "slept tonight, safe in the tunnel"))

(deftest a-body-shut-in-a-hut-does-not-walk-out-to-a-far-bed
  (let [world (merge (hut/hut-world {:x 5 :y 64 :z 0} {"30,64,0" "red_bed"}) {:time st/night})]
    (is (false? (hut/holds-with world (fn [eng] (st/know-bed! eng {:x 30 :y 64 :z 0})))))))

(defn lcg
  "A repeatable stand-in for Math.random (the looks and ray jitter pick which cells the body has seen)."
  [seed]
  (let [a (atom (+ 12345 (* 7919 seed)))]
    (fn [] (/ (swap! a #(mod (+ (* % 1103515245) 12345) 2147483648)) 2147483648))))

(deftest a-buried-body-asked-for-the-night-stays-in-the-tunnel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [random js/Math.random]
          (try
            (doseq [seed [0 1 5 7]]
              (set! js/Math.random (lcg seed))
              (let [{:keys [eng p raw]} (setup (tunnel-world buried-light))]
                (core/submit! eng '(jobs.survival.night) {})
                (await (st/tick-n eng 12))
                (is (= {:x 5 :y 64 :z 0} (update-vals (st/pos-of p) js/Math.floor)) (str "the body stays in the tunnel, seed " seed))
                (is (empty? (st/calls raw "sleep")))
                (is (empty? (st/calls raw "moveTo")) "no walk toward the far bed")))
            (finally (set! js/Math.random random))))))))

(def overdue-in-open-dark-tunnel
  "The tunnel open at both ends and unlit, the far bed standing out in the open: every cell of the walk is dark."
  (-> (tunnel-world (into {} (for [x (range 0 21) y [64 65]] [[x y 0] [0 0]])))
      (update :blocks dissoc "-1,64,0" "-1,65,0" "21,64,0" "21,65,0")
      (assoc :inventory [{:name "cobblestone" :count 32}])))

(deftest an-overdue-miner-does-not-walk-the-dark-to-a-far-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p raw clock]} (setup overdue-in-open-dark-tunnel)]
          (swap! clock + (* 4 sh-clock/ms-per-day))
          (is (= far-bed (sh-beds/bed-to-use p (mem/view (:store eng)) 128 (constantly true))) "overdue: the far bed is the bed to use")
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (= [far-bed] (map :pos (st/entries eng :bed-walk-unsafe))) "the dark walk was priced unsafe")
          (is (empty? (st/calls raw "sleep")))
          (is (empty? (filter #(< 21 (:x (st/arg-pos %))) (st/calls raw "moveTo"))) "no walk out of the tunnel")
          (is (nil? (sh-beds/bed-to-use p (mem/view (:store eng)) 128 (constantly true))) "nor does the night trigger hold for the bed meanwhile"))))))

(def on-open-ground
  "The body on the open floor at night, the far bed 35 blocks off over the same dark ground, no hostile known (overdue: a bed in
  the open is used)."
  {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 0}} :time st/night :offlineScale 0.0001 :sees-all true
   :blocks {"40,64,0" "red_bed"} :light {}})

(deftest a-body-on-dark-open-ground-walks-to-its-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng raw clock]} (setup on-open-ground)]
          (swap! clock + (* 4 sh-clock/ms-per-day))
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (empty? (st/entries eng :bed-walk-unsafe)) "staying out here costs as much as the walk")
          (is (seq (st/calls raw "sleep")) "it walks to the bed and sleeps"))))))

(def hut-light
  "A closed hut: torch light and no sky light in every cell of its room."
  (into {} (for [x (range 4 7) y [64 65] z (range -1 2)] [[x y z] [0 14]])))

(defn hut-night-world [blocks extra]
  (merge (hut/hut-world {:x 4 :y 64 :z -1} blocks) {:time st/night :light hut-light :sees-all true} extra))

(defn blind-above
  "p that has not seen the roof: the roof (above the walls) is unknown (a body cannot look up from inside)."
  [p]
  (let [seen (.-sensedAt p)]
    (doto p (aset "sensedAt" (fn [a] (if (> (.-y a) 66) #js {:unknown true} (.call seen p a)))))))

(deftest a-body-shut-in-a-hut-stays-in-for-a-near-bed-while-another-player-sleeps
  (let [{:keys [eng p]} (setup (hut-night-world {"20,64,0" "red_bed"} {}))]
    (st/looked-round! eng p)
    (mem/write! (:store eng) :sleep-status {:sleeping 1 :needed 2} nil)
    (is (nil? (sh-beds/bed-to-use p (mem/view (:store eng)) 64 (constantly true))))))

(deftest a-body-buried-in-a-closed-hut-places-its-carried-bed
  (let [{:keys [eng p]} (setup (hut-night-world {} {:inventory [{:name "red_bed" :count 1}]}))
        _ (blind-above p)]
    (is (true? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))))))

(deftest a-body-buried-in-a-closed-hut-uses-a-bed-in-the-same-hut
  (let [{:keys [eng p]} (setup (hut-night-world {"6,64,-1" "red_bed"} {}))
        _ (blind-above p)]
    (st/know-bed! eng {:x 6 :y 64 :z -1})
    (is (= {:x 6 :y 64 :z -1} (sh-beds/bed-to-use p (mem/view (:store eng)) 64 (constantly true))))))

(defn stale-until-looked
  "p whose memory of cell (a [x y z] vector) is air until the body looks: a bed set there since it last saw the cell."
  [p raw [x y z]]
  (let [seen (.-sensedAt p)]
    (doto p (aset "sensedAt" (fn [a] (if (and (= [x y z] [(.-x a) (.-y a) (.-z a)]) (empty? (st/calls raw "look")))
                                       #js {:name "air" :pos a}
                                       (.call seen p a)))))))

(deftest a-body-in-a-hut-looks-round-once-and-sleeps-in-a-bed-it-had-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p raw]} (setup (hut-night-world {"6,64,1" "red_bed"} {}))
              _ (stale-until-looked p raw [6 64 1])
              holds #(sh/night-holds? p (mem/view (:store eng)) {} (constantly true))]
          (is (true? (holds)) "the trigger holds to look round the hut")
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (seq (st/calls raw "look")) "looked")
          (is (= [{:x 6 :y 64 :z 1}] (mapv st/arg-pos (st/calls raw "sleep"))) "slept in the bed it then saw"))))))

(deftest a-body-in-a-hut-with-no-bed-looks-once-then-the-night-does-not-hold
  (let [{:keys [eng p]} (setup (hut-night-world {} {}))
        holds #(sh/night-holds? p (mem/view (:store eng)) {} (constantly true))]
    (is (true? (holds)))
    (st/looked-round! eng p)
    (is (false? (holds)) "looked from this room tonight")))

(defn unseen-hut-world
  "The closed hut, the body in it with nothing seen yet (no sight pass), not the fake's all-seeing sight."
  [blocks extra]
  (merge (hut/hut-world {:x 4 :y 64 :z -1} blocks) {:time st/night :light hut-light} extra))

(deftest a-body-in-a-hut-it-has-not-looked-at-looks-then-places-its-carried-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p raw]} (setup (unseen-hut-world {} {:inventory [{:name "red_bed" :count 1}]}))]
          (is (true? (sh/night-holds? p (mem/view (:store eng)) {} (constantly true))) "the trigger holds to look")
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (is (seq (st/calls raw "look")) "looked")
          (is (seq (st/calls raw "place")) "the bed is put down"))))))

(deftest a-buried-miner-looks-once-then-the-night-does-not-hold-on-the-next-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p raw]} (setup (assoc (dissoc (tunnel-world buried-light) :sees-all) :inventory [{:name "red_bed" :count 1}]))
              holds #(sh/night-holds? p (mem/view (:store eng)) {} (constantly true))]
          (core/submit! eng '(jobs.survival.night) {})
          (await (st/tick-n eng 12))
          (let [looks (count (st/calls raw "look"))]
            (is (pos? looks) "looked once")
            (is (false? (holds)) "no hold after the look, whatever stays unseen")
            (st/teleport! raw 6 64 0)
            (is (false? (holds)) "nor on the next cell of the tunnel")
            (core/submit! eng '(jobs.survival.night) {})
            (await (st/tick-n eng 12))
            (is (= looks (count (st/calls raw "look"))) "no second look pass")))))))

(def seven-hut
  "A stone hut, walls x and z 8..16, y 63..67 (floor y 63, roof y 67), lit by two torches, a red bed at 10..11 64 12."
  (let [inside? (fn [k] (let [[x y z] (map js/Number (.split k ","))] (and (<= 9 x 15) (<= 64 y 66) (<= 9 z 15))))]
    (merge (into {} (remove (comp inside? key)) (tu/box 8 63 8 16 67 16 "stone"))
           {"9,65,9" "torch" "15,65,15" "torch" "10,64,12" "red_bed" "11,64,12" "red_bed"})))

(def seen-from-the-start
  "What a body set down at 13 64 13 facing west had seen live (hut-bed-review fixture): the room's air cells at feet and
  head height, but the floor and roof only at x 9..11 (a level look never shows the floor beside the feet)."
  (set (concat
        (for [[z xs] {8 [9] 9 [8 9 10] 10 (range 8 12) 11 (range 8 13) 12 (range 8 13) 13 (range 8 14) 14 (range 8 13) 15 (range 8 13) 16 [9 10 11]}
              x xs] [x 64 z])
        (for [[z xs] {8 [9] 9 [8 9 10] 10 (range 8 12) 11 (range 8 13) 12 (range 8 14) 13 (range 8 14) 14 (range 8 14) 15 (range 8 13) 16 [9 10 11]}
              x xs] [x 65 z])
        (for [z (range 9 16) x [9 10 11] y [63 67]] [x y z]))))

(def seen-after-a-look
  "seen-from-the-start with the walls and the air at feet and head height seen all round; the floor beside the feet
  still unseen."
  (into seen-from-the-start (for [x (range 8 17) z (range 8 17) y [64 65]] [x y z])))

(defn seeing
  "p made to have seen only the cells in seen."
  [p seen]
  (let [sensed (.-sensedAt p)]
    (doto p (aset "sensedAt" (fn [a] (if (seen [(.-x a) (.-y a) (.-z a)]) (.call sensed p a) #js {:unknown true}))))))

(deftest a-seen-bed-in-a-hut-whose-floor-beside-the-body-is-unseen-is-used
  (let [{:keys [eng p]} (setup {:floor [0 0 20 20] :self {:pos {:x 13.5 :y 64 :z 13.5}} :time st/night :sees-all true
                                :blocks seven-hut :light {[13 64 13] [0 0] [13 65 13] [0 0]}})
        _ (mem/forget-where! (:store eng) :bed (constantly true))
        v (mem/view (:store eng))
        unseen-east (seeing (js/Object.assign #js {} p) seen-from-the-start)]
    (is (nil? (reach-room/room* unseen-east {:x 13 :y 64 :z 13})) "walls never seen behind the body: no closed room")
    (is (contains? (:cells (reach-room/room* (seeing p seen-after-a-look) {:x 13 :y 64 :z 13})) [11 64 11])
        "a seen-free cell beside the feet over an unseen floor is a room cell, and the room goes on from it")
    (is (= {:x 11 :y 64 :z 12} (sh-beds/bed-to-use unseen-east v 48 (constantly true))))
    (is (true? (sh/night-holds? unseen-east v {} (constantly true))))))


(def roofed-room
  "A stone room of x 10..12, z 0..1 (walls y 64..65, roof y 66 over those cells only), a doorway in the north wall at 11 -1."
  (let [inside? (fn [x z] (and (<= 10 x 12) (<= 0 z 1)))]
    (dissoc (merge (into {} (for [x (range 9 14) y [64 65] z (range -1 3) :when (not (inside? x z))] [(str x "," y "," z) "stone"]))
                   (into {} (for [x (range 10 13) z [0 1]] [(str x ",66," z) "stone"])))
            "11,64,-1" "11,65,-1")))

(def overhang-beside-gap
  "roofed-room with its east wall open two wide (13 64..65, z 0..1, unroofed) and one-wide gap at 14 64 0 between two
  stones: a roof edge beside a gap, not a room closed but for a doorway."
  (-> (apply dissoc roofed-room (for [y [64 65] z [0 1]] (str "13," y "," z)))
      (assoc "11,64,-1" "stone" "11,65,-1" "stone" "14,64,-1" "stone" "14,64,1" "stone")))

(defn home-p [blocks]
  (:p (setup {:floor [0 0 20 20] :self {:pos {:x 10.5 :y 64 :z 0.5}} :time st/night :sees-all true :blocks blocks})))

(deftest only-a-doorway-the-night-can-fill-leaves-a-home-closed
  (is (true? (room-lib/closed-home? (home-p roofed-room))) "the north doorway is the roof's only way out")
  (is (false? (room-lib/closed-home? (home-p overhang-beside-gap))) "shutting the gap leaves the roof's open side")
  (is (true? (room-lib/closed-home? (home-p (assoc roofed-room "11,64,-1" "stone" "11,65,-1" "stone")))) "a shut room"))

(deftest a-bed-too-far-to-walk-to-does-not-call-a-buried-miner-out-to-look
  (let [{:keys [eng p]} (setup (dissoc (tunnel-world buried-light) :sees-all))
        look? #(sh/unlooked-for-bed? p (mem/view (:store eng)))]
    (is (true? (look?)) "a bed in reach with unseen cells beside the feet: look first")
    (st/know-bed! eng {:x 300 :y 64 :z 0})
    (is (false? (look?)) "a bed beyond the bed radius is none to look for")))

(def three-high-room
  "A stone room x 10..12, z 10..11, air y 64..66 under a roof at 67, a bed at 10..11 64 10 with a stone at 11 65 10 over its head."
  (let [inside? (fn [k] (let [[x y z] (map js/Number (.split k ","))] (and (<= 10 x 12) (<= 64 y 66) (<= 10 z 11))))]
    (assoc (into {} (remove (comp inside? key)) (tu/box 9 63 9 13 67 12 "stone"))
           "11,64,10" "red_bed" "10,64,10" "red_bed" "11,65,10" "stone")))

(deftest an-unseen-wall-block-behind-a-bed-stone-does-not-open-a-walled-roofed-room
  (let [{:keys [p]} (setup {:floor [0 0 20 20] :self {:pos {:x 12.5 :y 64 :z 11.5}} :time st/night :sees-all true :blocks three-high-room})
        all (set (for [x (range 8 15) y (range 62 69) z (range 8 14)] [x y z]))
        room-at (fn [hidden] (reach-room/room* (seeing p (apply disj all hidden)) {:x 12 :y 64 :z 11}))]
    (is (some? (room-at #{[12 65 9]})) "an unseen cell over a seen wall block with a seen wall over it is no step up")
    (is (nil? (room-at #{[12 65 9] [12 66 9]})) "with the cell two over unseen too, the step up cannot be ruled out")))
