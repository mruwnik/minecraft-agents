(ns engine.build-light-area-test
  "jobs.build.light-area against the seeing fake world: dark rooms and corridors are lit with the fewest torches."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.ready-test :as ready-test]
            [engine.test-util :as tu]
            [jobs.build.light-area :as light-area]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.sense :as sense]
            [jobs.lib.torch-plan :as tp]
            [jobs.lib.util :as u]))

(def job 'jobs.build.light-area)
(def torches [{:name "torch" :count 16}])

(defn key-of [[x y z]] (str x "," y "," z))

(defn world
  "Stone with the open cells (feet and head levels y 64 and 65, floor y 63) carved in; where lit says, a glowstone ceiling (y 66) lights them, as the body's memory floods it; the light table is what the body sees by: no sky, block light 14 under the glowstone."
  [open lit inventory & [extra]]
  (let [cells (for [[x z] open y [64 65]] [x y z])
        glow (into {} (for [[x z] open :when (lit [x 64 z])] [(key-of [x 66 z]) "glowstone"]))
        env (bd/setup-seeing
             {:self {:pos {:x (+ 0.5 (ffirst open)) :y 64 :z (+ 0.5 (second (first open)))}}
              :blocks (merge (apply dissoc (tu/box -3 62 -3 70 67 40 "stone") (map key-of cells)) glow extra)
              :inventory inventory :drops {"torch" "torch"}})]
    (swap! (.. (:p env) -world -state) merge {:light (into {} (for [c cells] [c (if (lit c) [0 14] [0 0])]))})
    env))

(def room (for [x (range 0 9) z (range 0 9)] [x z]))
(def l-corridor (concat (for [x (range 0 21)] [x 0]) (for [z (range 1 21)] [20 z])))

(defn ^:async run [open lit args & [inventory extra]]
  (let [env (world open lit (or inventory torches) extra)
        r (await (tu/child-outcome (:eng env) job args 400))]
    [env r]))

(defn torch-cells [p open]
  (for [[x z] open y [64 65] :when (re-find #"torch" (or (bd/block-at p {:x x :y y :z z}) ""))] [x y z]))

(deftest a-dark-room-takes-one-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly false) {:boxes [[[0 64 0] [8 64 8]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 1 (count (torch-cells p room))))
          (is (every? (fn [[tx _ tz]] (every? (fn [[x z]] (< (+ (abs (- x tx)) (abs (- z tz))) 14)) room)) (torch-cells p room))
              "the torch's light (14 less a step a cell) reaches every cell of the room; a corner torch would leave the far corner unlit"))))))

(deftest a-room-dark-of-block-light-is-lit-by-day-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (world room (constantly false) torches)
              cells (for [[x z] room y [64 65]] [x y z])]
          (swap! (.. (:p env) -world -state) merge {:light (into {} (for [c cells] [c [15 0]]))})
          (let [r (await (tu/child-outcome (:eng env) job {:boxes [[[0 64 0] [8 64 8]]]} 400))]
            (is (= :done (:status r)) (pr-str r))
            (is (= 1 (count (torch-cells (:p env) room))) "sky light 15 does not stop monsters spawning at night: block light 0 is dark")))))))

(deftest the-lighting-is-planned-once-for-each-state-of-the-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plans (atom 0)
              states (atom #{})
              plan tp/plan
              keyed check-cache/keyed
              [{:keys [p]} r] (with-redefs [tp/plan (fn [x] (swap! plans inc) (plan x))
                                            check-cache/keyed (fn [c tag inputs thunk]
                                                                (when (= ::light-area/lighting tag) (swap! states conj inputs))
                                                                (keyed c tag inputs thunk))]
                                (await (run room (constantly false) {:boxes [[[0 64 0] [8 64 8]]]})))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 1 (count (torch-cells p room))))
          (is (< 1 (count @states)) "the body looked round and hung a torch: more than one state")
          (is (= (count @states) @plans) "each state of the room (args, torches hung, places failed, cells looked at, body cell) is planned once, however often the step's checks ask"))))))

(deftest the-lighting-is-planned-again-when-the-body-s-memory-of-the-world-changes
  (let [plans (atom 0)
        memory (atom 1)
        plan tp/plan
        c (assoc (ready-test/ctx-of {:self {:pos {:x 0.5 :y 64 :z 0.5}} :inventory torches}) :args {:boxes [[[0 64 0] [2 64 2]]]})]
    (with-redefs [tp/plan (fn [x] (swap! plans inc) (plan x))
                  sense/known-memory (fn [_] @memory)]
      (light-area/lighting c)
      (light-area/lighting c)
      (is (= 1 @plans) "an unchanged state is planned once")
      (reset! memory 2)
      (light-area/lighting c)
      (is (= 2 @plans) "a cell seen differently: planned again"))))

(deftest powder-snow-lets-light-through-like-air
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [snow (into {} (for [z (range 0 9) y [64 65]] [(key-of [4 y z]) "powder_snow"]))
              [{:keys [p]} r] (await (run room (constantly false) {:boxes [[[0 64 0] [8 64 8]]]} torches snow))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 1 (count (torch-cells p room))) "a wall of powder snow is no wall to light: one torch, as in the open room"))))))

(deftest a-body-with-only-soul-torches-hangs-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly false) {:boxes [[[0 64 0] [8 64 8]]]} [{:name "soul_torch" :count 16}]))]
          (is (= :done (:status r)) (pr-str r))
          (is (pos? (count (torch-cells p room)))))))))

(deftest near-lights-the-cells-round-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly false) {:near 3}))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 1 (count (torch-cells p room)))))))))

(deftest a-dark-l-corridor-takes-two-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run l-corridor (constantly false)
                                          {:boxes [[[0 64 0] [20 64 0]] [[20 64 1] [20 64 20]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (= 2 (count (torch-cells p l-corridor))) (pr-str r (torch-cells p l-corridor))))))))

(deftest a-lit-room-hangs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly true) {:boxes [[[0 64 0] [8 64 8]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (empty? (torch-cells p room))))))))

(deftest no-torch-and-none-to-get-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[{:keys [p]} r] (await (run room (constantly false) {:boxes [[[0 64 0] [8 64 8]]]} []))]
          (is (not= :done (:status r)) (pr-str r))
          (is (empty? (torch-cells p room))))))))

(deftest a-lit-seen-part-with-an-unseen-dark-part-is-looked-at-before-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (for [x (range 0 61)] [x 0])
              [{:keys [p]} r] (await (run corridor (fn [[x _ _]] (<= x 20)) {:boxes [[[0 64 0] [60 64 0]]]}))]
          (is (= :done (:status r)) (pr-str r))
          (is (seq (torch-cells p corridor)) "the dark far end got a torch, not an instant done")
          (is (every? (fn [[x _ _]] (> x 30)) (torch-cells p corridor))))))))

(deftest an-unseen-part-whose-walk-costs-more-than-the-call-is-worth-is-left-unlooked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (for [x (range 0 61)] [x 0])
              env (world corridor (fn [[x _ _]] (<= x 20)) torches)
              walks (atom [])
              r (await (tu/with-near-spy walks #(tu/child-outcome (:eng env) job {:boxes [[[0 64 0] [60 64 0]]]} 400 2)))]
          (is (not-any? (fn [[pos]] (> (:x pos) 2)) @walks)
              (str "Brindle S20: dark-work walked 36 blocks into a mob cave to look at one cell; a 2 s call does not buy a 16-block walk " @walks))
          (is (= :done (:status r)) (pr-str r))
          (is (seq (:unlooked r)) "the far part is reported unlooked, not lit"))))))

;; Brindle S9: the look walk pillared into the body's own dark feet cell, which then no longer counted as spawn ground,
;; so the call ended done with no torch hung and dark-work fired again, 43 times.
(deftest a-look-walk-only-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (for [x (range 0 61)] [x 0])
              env (world corridor (constantly false) torches)
              walks (atom [])
              r (await (tu/with-near-spy walks #(tu/child-outcome (:eng env) job {:boxes [[[0 64 0] [60 64 0]]]} 400)))
              look-walks (filter (fn [[_ range]] (= 1 range)) @walks)]
          (is (= :done (:status r)) (pr-str r))
          (is (seq look-walks) "the far part is out of sight from the start")
          (is (every? (fn [[_ _ opts]] (= #{:walk} (:ways opts))) look-walks) (pr-str look-walks)))))))

(deftest a-cell-looked-at-from-where-the-body-stands-is-not-looked-at-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (for [x (range 0 12)] [x 0])
              [_ r] (await (tu/with-near-stub (fn [& _] (js/Promise.resolve :there))
                              #(run corridor (fn [[x _ _]] (<= x 4)) {:boxes [[[0 64 0] [11 64 0]]]} torches)))]
          (is (not= :not-done r) "the unseen cells are each looked at once, not for ever"))))))

(deftest a-blocked-look-walk-leaves-the-cell-unlooked-and-the-torch-is-still-hung
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (for [x (range 0 61)] [x 0])
              [{:keys [p]} r] (await (tu/with-near-stub (fn [& _] (js/Promise.resolve :blocked))
                                        #(run corridor (constantly false) {:boxes [[[0 64 0] [60 64 0]]]} torches)))]
          (is (= :done (:status r)) (pr-str r))
          (is (seq (:unlooked r)) "the cell the body never saw is reported unlooked")
          (is (seq (torch-cells p corridor)) "the known dark cells still got a torch"))))))

(deftest an-open-cell-with-its-head-or-floor-unseen-is-not-decided-yet
  (let [seen (fn [known] (fn [_ {:keys [y]}] (known y)))
        half? #(with-redefs [u/seen-name (seen %)] (light-area/half-seen? nil [0 64 0]))]
    (is (true? (half? {64 "air" 65 nil 63 "stone"})) "head unseen")
    (is (true? (half? {64 "air" 65 "air" 63 nil})) "floor unseen")
    (is (false? (half? {64 "air" 65 "air" 63 "stone"})))
    (is (false? (half? {64 "stone" 65 nil 63 nil})) "rock is not a room cell")
    (is (false? (half? {})) "an unseen cell is the other frontier clause")))

(deftest a-field-box-is-lit-to-its-level-with-torches-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [box [[3 64 3] [5 64 5]]
              [{:keys [p]} r] (await (run room (constantly false) {:boxes [box] :level 9 :beside? true}))
              hung (torch-cells p room)
              inside? (fn [[x _ z]] (and (<= 3 x 5) (<= 3 z 5)))]
          (is (= :done (:status r)) (pr-str r))
          (is (seq hung))
          (is (not-any? inside? hung) "the field's own cells stay free for crops")
          (is (every? (fn [[x _ z]] (and (<= 2 x 6) (<= 2 z 6))) hung) "only the ring round the field"))))))
