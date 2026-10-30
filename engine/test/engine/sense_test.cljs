(ns engine.sense-test
  "jobs.lib.sense/decide! over a real perception on the fake world: a sync decision that read cells the body has not
  seen is decided again after the body looks at them, as many looks as the call pays, never twice at one cell."
  (:require [cljs.test :refer [deftest is async]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.clock :as clock]
            [jobs.lib.cost.look :as cost]
            [jobs.lib.cost.value :as value]
            [jobs.lib.look :as look]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.sense :as sense]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]))

(defn seeing
  "Fake primitives over world wrapped in a perception (a shorter radius), no sight pass yet: the body knows only what a
  glance at its current view shows."
  [world]
  (let [p (tu/fake-on-floor world)]
    (.setOwner p "t1")
    (perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))))

(defn stub-ctx
  "A round's ctx over p: job memory in an atom, acts straight to the primitives, events into a vector."
  [p]
  (let [m (atom {})
        events (atom [])]
    {:primitives p :worth (constantly ##Inf) :root "j1" :slots [] :mem m :events events
     :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data @m}]}}})
     :act (fn [k args] (.call (aget p (name k)) p "t1" args))
     :update-mem (fn [f args] (apply swap! m f args))
     :emit (fn [kind level fields] (swap! events conj (assoc fields :kind kind :level level)))}))

(defn looks [p] (count (filter #(= "look" (.-name %)) (.-calls (.-world p)))))

(defn look-points
  "The points the body looked at, in order, as [x y z]."
  [p]
  (->> (.-calls (.-world p)) (filter #(= "look" (.-name %))) (mapv #(let [q (.. % -args -pos)] [(.-x q) (.-y q) (.-z q)]))))

(def feet {:x 0 :y 64 :z 0})

(defn priced
  "decide!'s opts with a price whose turn saves `saving` seconds."
  ([saving] (priced saving {}))
  ([saving opts] (assoc opts :price (constantly {:gap 0 :delta saving}))))

(deftest flat-ground-unseen-round-the-feet-is-looked-at-and-planned-3-deep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}}})
              c (stub-ctx p)]
          (is (nil? (dig-cells/dig-plan p feet)) "the ground beside the feet is unseen: no plan without a look")
          (is (nil? (await (sense/decide! c ::dig-plan #(dig-cells/dig-plan p feet)))) "no priced runner-up: no look turns it")
          (is (zero? (looks p)))
          (is (= {:roof {:x 0 :y 63 :z 0} :depth 3}
                 (await (sense/decide! c ::dig-plan #(dig-cells/dig-plan p feet) (priced 5)))))
          (is (= [[-1 64 0]] (:cells (first (filter #(= :sense.looked (:kind %)) @(:events c))))) "one look, at the unseen cell beside the feet")
          (is (= 1 (count (filter #(= :sense.looked (:kind %)) @(:events c)))) "one event per call that looked"))))))

(def rock
  "A solid stone cube x 4..8, y 62..66, z -2..2: its inside never shows."
  (tu/box 4 62 -2 8 66 2 "stone"))

(def inside (for [x [5 6 7] y [63 64 65] z [-1 0 1]] {:x x :y y :z z}))

(defn all-seen [p] (= (count inside) (count (keep #(u/seen-name p %) inside))))

(def in-sight
  "sense/occluded? redefined for a rock's inside to stand for cells in sight that never show: a look there turns nothing."
  (constantly false))

(deftest hidden-cells-cost-the-looks-the-chance-of-a-turn-pays-and-are-never-looked-at-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {::cost/turn-prior {:a 1 :b 9}}
          (fn ^:async f []
            (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
                  c (stub-ctx p)
                  opts (priced (* 19.5 (cost/look-s 1)))]
              (with-redefs [sense/occluded? in-sight]
                (is (false? (await (sense/decide! c ::rock #(all-seen p) opts))))
                (is (= 10 (looks p)) "a look turns it at 1/(10 + i) after i fruitless ones: a turn saving 19.5 looks pays ten")
                (dotimes [_ 4] (await (sense/decide! c ::rock #(all-seen p) opts)))
                (is (= 10 (looks p)) "the cells looked at from here count as fruitless looks: no later call pays one"))))))))))

(deftest a-call-at-inf-worth-looks-at-most-max-looked-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {::sense/max-looked 5}
          (fn ^:async f []
            (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
                  c (stub-ctx p)]
              (with-redefs [sense/occluded? in-sight]
                (await (sense/decide! c ::rock #(all-seen p) (priced 1000))))
              (is (= 5 (looks p)) "a turn saving 1000 s pays more looks; the memory of looked cells bounds the call")))))))))

(def long-rock
  "A solid stone box x 4..13, y 62..66, z -7..7: 8 x 3 x 13 = 312 cells inside that never show."
  (tu/box 4 62 -7 13 66 7 "stone"))

(deftest a-call-at-inf-worth-looks-while-a-cell-seen-bad-would-pass-the-runner-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn ^:async run [price]
                    (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks long-rock})
                          c (stub-ctx p)
                          cells (for [x (range 5 13) y (range 63 66) z (range -6 7)] {:x x :y y :z z})
                          seen (fn [] (= (count cells) (count (keep #(u/seen-name p %) cells))))]
                      (with-redefs [clock/now (constantly 0) sense/occluded? in-sight]
                        (await (sense/decide! c ::rock seen {:price (constantly price)})))
                      (looks p)))
              close (await (run {:gap 1 :delta 4}))
              far (await (run {:gap 3.5 :delta 4}))]
          (is (= (cost/cell-looks 3 312) close) "a runner-up 1 s off a 4 s bad cell: the 3 s a turn saves pays a few of the 312")
          (is (< 0 close 40) "far short of the hidden cells")
          (is (= 0 far) "a runner-up 3.5 s off: the 0.5 s a turn saves is under the first look's seconds at its chance")
          (is (zero? (await (run {:gap 5 :delta 4}))) "a bad cell that does not pass the runner-up turns nothing")
          (is (zero? (await (run nil))) "no priced runner-up (gap ##Inf): no look"))))))

(deftest a-call-whose-negative-ends-the-job-is-bounded-by-the-cells-a-look-can-show-and-the-worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (for [x (range 5 13) y (range 63 66) z (range -6 7)] [x y z])
              run (fn ^:async run [worth occluded?]
                    (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks long-rock})
                          c (assoc (stub-ctx p) :worth (constantly worth))
                          seen (fn [] (= (count cells) (count (keep #(u/seen-name p (zipmap [:x :y :z] %)) cells))))]
                      (with-redefs [clock/now (constantly 0) sense/occluded? occluded?]
                        (await (sense/decide! c ::rock seen {:price (sense/fails-to c)})))
                      [(looks p) p]))
              [inf p] (await (run ##Inf sense/occluded?))
              walk (sense/read-walk-s (stub-ctx p) {:cells cells})
              [few] (await (run (+ walk 2.5) in-sight))
              [none] (await (run walk in-sight))]
          (is (= 1 inf) "at ##Inf one look shows the near face")
          (is (every? #(sense/occluded? p feet %) cells) "the 312 cells behind it: no look from here can show one")
          (is (= (cost/cell-looks 2.5 312) few) "worth 2.5 s over the walk to the nearest: a few looks of the 312")
          (is (< 0 few 10))
          (is (zero? none) "worth no more than the walk to the nearest: no look"))))))

(deftest the-walk-a-find-pays-is-to-the-nearest-cell-it-read-an-area-or-entity-query-round-the-body-is-here
  (let [c (stub-ctx (tu/fake {:self {:pos feet}}))
        walk #(:cost (value/walk-cost {:distance (u/eye-dist feet %)}))]
    (is (= (walk [3 64 0]) (sense/read-walk-s c {:cells [[9 64 0] [3 64 0]]})) "cells: the nearest")
    (is (= (walk [2 64 0]) (sense/read-walk-s c {:cells [[9 64 0]] :guessed [[2 64 0]]})) "a guessed cell is read too")
    (is (= (:cost (value/walk-cost {:distance 0})) (sense/read-walk-s c {:area [{:radius 16}]}) (sense/read-walk-s c {:cells [[9 64 0]] :entities [{:radius 32}]}))
        "an area or entity query round the body: its nearest cell is here, whatever its radius")
    (is (nil? (sense/read-walk-s c {})) "nothing read: no walk, no price")))

(deftest a-find-looks-at-its-kinds-chance-not-the-turn-prior
  (let [c (stub-ctx (tu/fake {:self {:pos feet}}))
        reads {:cells [[3 64 0]]}
        saving 2
        priced (fn [chance] ((sense/find-price c (+ saving (sense/read-walk-s c reads)) chance) nil reads))]
    (is (= (cost/cell-looks saving 50) (sense/cell-looks (priced nil) 50)) "no kind's chance: turn-chance")
    (is (= (cost/cell-looks saving 50 0 0 cost/closed-cell-chance) (sense/cell-looks (priced cost/closed-cell-chance) 50)))
    (is (< (sense/cell-looks (priced nil) 50) (sense/cell-looks (priced cost/closed-cell-chance) 50))
        "a roof cell is closed at the roofed-room prior's ~0.6, far over the turn prior's 1/43: more looks pay")))

(deftest a-costly-decision-is-made-again-only-after-looks-as-long-as-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)
              ms (atom 0)
              runs (atom 0)
              decide-ms (* 1000 (cost/look-s 3.5))]
          (with-redefs [clock/now (fn [] @ms) sense/occluded? in-sight]
            (await (sense/decide! c ::rock #(do (swap! runs inc) (swap! ms + decide-ms) (all-seen p)) (priced 1000))))
          (is (= (count inside) (looks p)) "a dear turn: every hidden cell is looked at")
          (is (= 8 @runs) "a decision that takes 3.5 looks' seconds is made again every 4 looks: 1 + ceil(27 / 4), not 28"))))))

(deftest a-call-worth-every-look-looks-at-each-hidden-cell-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)]
          (with-redefs [sense/occluded? in-sight]
            (await (sense/decide! c ::rock #(all-seen p) (priced 1000)))
            (dotimes [_ 4] (await (sense/decide! c ::rock #(all-seen p) (priced 1000)))))
          (is (= (count inside) (looks p)) "each hidden cell looked at once, then no more looks"))))))

(deftest an-area-query-with-nothing-near-surveys-once-and-finds-the-block-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :yaw 0 :blocks {"0,64,-5" "iron_ore"}})
              c (stub-ctx p)
              ore #(first (look/seen-blocks p {:names ["iron_ore"] :radius 16}))]
          (is (nil? (ore)) "behind the body: not seen")
          (is (= "iron_ore" (:name (await (sense/decide! c ::ore ore (priced 10))))))
          (is (= [[0.5 65.62 -3.5]] (look-points p)) "one survey, stopped at its first look: level, north, at the ore")
          (let [before (looks p)]
            (is (nil? (await (sense/decide! c ::iron-again #(first (remove (constantly true) (look/seen-blocks p {:names ["iron_ore"] :radius 16}))) (priced 10)))))
            (is (= before (looks p)) "surveyed for iron from this cell already: no second survey")
            (is (nil? (await (sense/decide! c ::gold #(first (look/seen-blocks p {:names ["gold_ore"] :radius 16})) (priced 10)))))
            (is (< before (looks p)) "a look for iron is no look for gold: it surveys again")
            (let [before (looks p)]
              (is (nil? (await (sense/decide! c ::coal #(first (look/seen-blocks p {:names ["coal_ore"] :radius 16})) (priced 0)))))
              (is (= before (looks p)) "a survey no turn pays is not taken"))))))))

(deftest an-entity-query-stops-at-the-first-look-that-shows-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}}})
              c (stub-ctx p)
              cow #js {:id 7 :name "cow" :kind "passive" :pos #js {:x 0 :y 64 :z -5} :visible true}
              raw-entities (aget p "entities")
              _ (aset p "entities" (fn [opts] (if (>= (looks p) 3) #js [cow] (.call raw-entities p opts))))
              find-cow #(first (filter (fn [e] (= "cow" (:name e))) (look/seen-entities p {:radius 16})))]
          (is (= 7 (:id (await (sense/decide! c ::cow find-cow (priced 10))))))
          (is (= 3 (looks p)) "no look after the one that showed it"))))))

(deftest a-promise-throws-a-positive-takes-no-look-an-unloaded-cell-is-never-looked-at
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :unloaded #{"3,63,0"}})
              c (stub-ctx p)]
          (is (= :thrown (try (await (sense/decide! c ::p #(js/Promise.resolve 1))) (catch :default _ :thrown))))
          (is (= 1 (await (sense/decide! c ::one (constantly 1)))))
          (is (nil? (await (sense/decide! c ::unloaded #(u/seen-name p {:x 3 :y 63 :z 0}) (priced 10)))))
          (is (zero? (looks p))))))))

(deftest a-positive-that-guessed-is-checked-after-a-look-with-verify-guesses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,64,-3" "sand"}})
              c (stub-ctx p)
              floor #(u/block-name-or p {:x 0 :y 64 :z -3} "stone")]
          (is (= "stone" (await (sense/decide! c ::guess floor (priced 10)))) "without verify-guesses the guess stands")
          (is (zero? (looks p)))
          (is (= "sand" (await (sense/decide! c ::guess floor (priced 10 {:verify-guesses true})))))
          (is (= 1 (looks p))))))))

(deftest a-survey-hit-that-guessed-is-checked-after-a-look-with-verify-guesses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :yaw 0 :blocks (merge rock {"0,64,-5" "iron_ore"})})
              c (stub-ctx p)
              hidden {:x 6 :y 64 :z 0}
              ore-and-guess #(when-let [o (first (look/seen-blocks p {:names ["iron_ore"] :radius 16}))]
                               [(:name o) (u/block-name-or p hidden "air")])]
          (is (= ["iron_ore" "air"] (await (sense/decide! c ::ore ore-and-guess (priced 10 {:verify-guesses true})))))
          (is (= [[6 64 0]] (:cells (first (filter #(= :sense.looked (:kind %)) @(:events c)))))
              "the survey found the ore; the guessed cell is looked at after it"))))))

(deftest look-round-marks-every-cell-it-was-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)
              cells (mapv (fn [{:keys [x y z]}] [x y z]) inside)]
          (await (sense/look-round! c cells))
          (is (<= (count cells) (looks p)) "the check that named the cells priced them: every one is looked at")
          (is (= (set cells) (sense/round-looked c)) "one round per :look: the cells asked for count as looked, so the check asks no second"))))))

(deftest a-priced-round-pays-the-cells-a-turn-covers-and-the-looks-taken-here-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (settings/with-settings {::cost/turn-prior {:a 1 :b 9}}
          (fn ^:async f []
            (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
                  asked (atom [])
                  c (stub-ctx p)
                  price {:gap 0 :delta (* 19.5 (cost/look-s 1))}
                  cells (mapv (fn [{:keys [x y z]}] [x y z]) inside)]
              (with-redefs [sense/look-at! (fn ^:async [_ cell & _] (swap! asked conj cell))
                            look/look-around! (fn ^:async ([_ _ _]) ([_ _]) ([_]))
                            reach-room/unseen-sides (constantly [])]
                (let [{:keys [pay unpaid]} (sense/round-plan c cells price)]
                  (is (= 10 (count pay)) "ten of the 27 cells pay, each at its own falling chance")
                  (is (= 17 (count unpaid)))
                  (await (sense/look-round! c pay))
                  (is (= 10 (count @asked)) "the hook looks at every cell the check named"))
                (is (empty? (:pay (sense/round-plan c cells price))) "the ten looks taken from here leave no paid look for a second round"))))))))))

(def far-wall {"-6,64,0" "stone" "-6,65,0" "stone"})
(def open-door {:blocks {"2,64,0" "oak_door" "2,65,0" "oak_door"} :states {"2,64,0" {:open true} "2,65,0" {:open true}}})
(def open-gate {:blocks {"2,64,0" "oak_fence_gate"} :states {"2,64,0" {:open true}}})

(deftest look-round-stops-only-at-a-room-wall-a-mob-can-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[near far passes-taken] [[{} {} 0]
                                   [{"2,64,0" "snow"} {} 0]
                                   [{"2,64,0" "stone"} {} 0]
                                   [{"2,64,0" "water"} {} 0]
                                   [open-door {} 0]
                                   [open-gate {} 0]
                                   [{"2,64,0" "stone"} far-wall 1]
                                   [{"2,64,0" "oak_leaves"} far-wall 1]
                                   [{"2,65,0" "stone"} far-wall 1]
                                   [{"2,64,0" "glass" "2,65,0" "glass"} far-wall 1]
                                   [{"2,64,0" "oak_door" "2,65,0" "oak_door"} far-wall 1]
                                   [{"2,64,0" "iron_door" "2,65,0" "iron_door"} far-wall 1]]]
          (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge (or (:blocks near) near) far) :states (:states near)})
                c (stub-ctx p)
                cells [[2 64 0] [-6 64 0]]
                passes (atom 0)]
            (with-redefs [reach-room/unseen-sides (constantly cells)
                          look/look-around! (fn ^:async ([_ _ _] (swap! passes inc)) ([_ _] (swap! passes inc)) ([_] (swap! passes inc)))]
              (await (sense/look-round! c cells)))
            (is (= passes-taken @passes) (str "the round's own look pass follows only when no wall was open, walls " near far))
            (is (= (set cells) (sense/round-looked c)) "the walls left unlooked count as looked")))))))

(deftest look-round-skips-cells-asked-for-from-this-spot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (seeing {:self {:pos {:x 0 :y 64 :z 0}} :blocks rock})
              c (stub-ctx p)
              cells (mapv (fn [{:keys [x y z]}] [x y z]) inside)]
          (await (sense/look-round! c cells))
          (let [asked (atom [])]
            (with-redefs [sense/look-at! (fn ^:async [_ cell & _] (swap! asked conj cell))]
              (await (sense/look-round! c cells)))
            (is (empty? @asked) "a second round on the same cells from the same spot looks at none")))))))

