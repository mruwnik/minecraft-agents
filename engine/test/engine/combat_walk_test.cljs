(ns engine.combat-walk-test
  "jobs.combat.attack walks with the engine walker (jobs.lib.near/walk-near!, doors :shut), so a body inside its doored
  hut leaves through the door instead of getting noPath from the raw pathfinder; and the stuck trigger counts a walk that
  found no path only when the body is enclosed (jobs.lib.reach/enclosed?), not when the goal alone is out of reach."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.hut-shelter-test :as hut]
            [jobs.lib.reach :as reach]
            [jobs.lib.threats :as threats]
            [engine.memory :as mem]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.unstick-test :as ut]))

(def sword [{:name "iron_sword" :count 1}])

(defn zed [id x z] {:id id :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z}})

(defn started-jobs [seen] (into #{} (keep :job) @seen))

;; ------------------------------------------------------------------ attack from inside the hut

(defn waits-move-clock!
  "Time passes only while the body waits: a whole fight in one call ends by time."
  [{:keys [p clock]}]
  (.override (.-world p) "wait" (fn [t a impl] (swap! clock + (.-ms a)) (impl t a))))

(deftest attack-from-inside-a-doored-hut-goes-out-through-the-door-and-kills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (ut/setup (merge (hut/hut-world {:x 5 :y 64 :z 0} {})
                                                          {:inventory sword :entities [(zed 7 5 7)]}))
              _ (waits-move-clock! s)]
          (core/submit! eng '(jobs.combat.attack {:targets [7] :timeout-s 1000}) {})
          (await (st/tick-n eng 12))
          (is (= [] (ut/calls p "moveTo")) "no raw pathfinder walk")
          (is (seq (ut/calls p "attack")) "the zombie outside was swung at")
          (is (<= 3 (.-z (.-pos (.self p)))) "the body is outside, past the door")
          (is (false? (hut/door-open? p)) "the door is shut behind it")
          (is (not (contains? (started-jobs seen) 'jobs.maintenance.unstick)) "no stuck reflex"))))))

(deftest attack-chases-a-target-that-moves-between-swings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (ut/setup {:floor tu/walk-floor :inventory sword :entities [(zed 7 10 0)]})
              n (atom 0)]
          (waits-move-clock! s)
          (.override (.-world p) "attack" (fn [t a impl]
                                            (when (= 1 (swap! n inc)) (swap! (fake/state p) assoc-in [:entities 0 :pos] [-6 64 0]))
                                            (impl t a)))
          (core/submit! eng '(jobs.combat.attack {:targets [7] :timeout-s 1000}) {})
          (await (core/tick! eng))
          (is (= [] (ut/calls p "moveTo")))
          (is (> -3 (.-x (.-pos (.self p)))) "the body walked after the target to its new spot")
          (is (seq (ut/call-args p "attack"))))))))

;; ------------------------------------------------------------------ stuck: no path is not stuck unless enclosed

(defn no-path-move [] (assoc (ut/bad-move) :no-path true))

(defn stuck-in? [world moves]
  (let [{:keys [eng p]} (ut/setup world)]
    (ut/seed-moved! eng moves)
    ((:when (get triggers/all :stuck)) p (mem/view (:store eng)) {})))

(deftest enclosed-is-a-pit-or-a-doorless-room-not-open-ground-or-a-doored-hut
  (is (true? (reach/enclosed? (tu/fake {:self {:pos ut/at5} :blocks ut/deep-pit}))) "a 3-deep pit")
  (is (false? (reach/enclosed? (tu/fake {:self {:pos ut/at5} :floor tu/walk-floor}))) "open ground")
  (is (false? (reach/enclosed? (tu/fake (hut/hut-world {:x 5 :y 64 :z 0} {})))) "a hut with a shut wooden door")
  (is (true? (reach/enclosed? (tu/fake (hut/hut-world {:x 5 :y 64 :z 0} {"5,64,2" "cobblestone" "5,65,2" "cobblestone"}))))
      "the same hut with its door walled up")
  (is (true? (reach/enclosed? (tu/fake (hut/hut-world {:x 5 :y 64 :z 0} {"5,64,2" "iron_door" "5,65,2" "iron_door"}))))
      "an iron door a hand cannot open"))

(deftest no-path-moves-fire-stuck-only-for-an-enclosed-body
  (is (false? (stuck-in? {:self {:pos ut/at5} :floor tu/walk-floor} (repeat 4 (no-path-move))))
      "open ground, the goal out of reach: not stuck")
  (is (false? (stuck-in? (hut/hut-world {:x 5 :y 64 :z 0} {}) (repeat 4 (no-path-move))))
      "inside a doored hut: not stuck")
  (is (true? (stuck-in? {:self {:pos ut/at5} :blocks ut/deep-pit} (repeat 4 (no-path-move)))) "in a pit: stuck")
  (is (true? (stuck-in? {:self {:pos ut/at5} :floor tu/walk-floor} (repeat 4 (ut/bad-move))))
      "a walk that had a way and did not move is stuck evidence even in the open")
  (is (true? (stuck-in? {:self {:pos ut/at5} :floor tu/walk-floor} (concat (repeat 3 (no-path-move)) [(ut/bad-move)])))
      "one physical failure among no-path ones"))

(deftest moved-entries-mark-a-walk-that-found-no-path
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (ut/setup {:noPath ["5,64,0"]})]
          (core/submit! eng '(jobs.movement.pace {:a {:x 5 :y 64 :z 0} :b {:x 5 :y 64 :z 0} :laps 1}) {})
          (await (core/tick! eng))
          (is (true? (:no-path (first (ut/moved eng)))) "moveTo answered noPath"))
        (let [{:keys [eng]} (ut/setup {:self {:pos ut/at5} :floor tu/walk-floor})]
          (core/submit! eng '(jobs.movement.go-to {:pos {:x 5 :y 70 :z 0}}) {})
          (await (core/tick! eng))
          (is (true? (:no-path (first (ut/moved eng)))) "the walker planned no way up into the air"))))))

(deftest attack-walks-up-to-its-target-without-costing-dangers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (ut/setup {:floor tu/walk-floor :entities [(zed 7 10 0)]})
              planned (atom 0)
              _ (waits-move-clock! s)]
          (with-redefs [threats/planner-dangers (fn [_] (swap! planned inc) nil)]
            (core/submit! eng '(jobs.combat.attack {:targets [7] :timeout-s 1000}) {})
            (await (tu/tick-until-idle! eng 6)))
          (is (= 0 @planned) "the walk to the mob it attacks plans with dangers off"))))))
