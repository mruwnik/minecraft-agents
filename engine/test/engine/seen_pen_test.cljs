(ns engine.seen-pen-test
  "The pen readers of jobs.animals.herd read what the body sees or remembers. A pen cell it has not seen is unknown:
  the job goes and looks at it; only a cell seen as open ends :no-pen."
  (:require [engine.lit-support :as lit]
            [cljs.test :refer [deftest is async use-fixtures]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.herd-test :as ht]
            [engine.hostile-test :as h]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [jobs.animals.herd-pen :as hp]
            [jobs.animals.herd-run :as hr]))

(use-fixtures :once lit/fixture)

(defn coarse-world
  "ht/world with sight rays 4 degrees apart: the fence stays in view and a pass costs a quarter."
  [w]
  (assoc (ht/world w) :ray-deg 4))

(defn scan!
  "Turn through every heading, level and down, a sight pass at each."
  [s]
  (doseq [yaw [0 45 90 135 180 225 270 315] pitch [0 20 35 50 70]]
    (swap! (fake/state (:p s)) assoc :yaw yaw :pitch pitch)
    (perception/pass! (aget (:p s) "perception"))))

(defn seeing
  "The body west of the pen after a full sight pass; aged?: then 30 s on and looking at its feet, so the fence and floor
  are remembered and the gate (a mutable block, remembered 10 s) is unknown."
  [w aged?]
  (let [s (ht/clock-on-wait! (h/setup-seeing (coarse-world w) nil 700))]
    (scan! s)
    (when aged?
      (swap! (:clock s) + 30000)
      (perception/pass! (aget (:p s) "perception")))
    s))

(defn read [s] (hr/read-pen {:primitives (:p s) :args {:box ht/box}}))

(defn gate-unknown? [s]
  (boolean (some #(and (= :unloaded (:why %)) (= ht/gate (:pos %))) (:leaks (read s)))))

(deftest read-pen-reads-what-the-body-sees
  (is (false? (gate-unknown? (seeing {} false))) "gate in view")
  (is (true? (gate-unknown? (seeing {} true))) "gate remembered past its 10 s: unknown, the fence still remembered"))

(deftest a-pen-with-an-unknown-gate-herds-and-does-not-end-no-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (seeing {:entities [(ht/cow 1 4 3)]} true)
              _ (ht/submit! s {:target 1})
              _ (await (ht/run-ticks s 12))]
          (is (= :brought (:reason (ht/done-event s)))))))))

(defn gate-state [s] (hr/gate-state (:p s) (hr/cell ht/gate)))

(defn sensing [answer] #js {:sensedAt (fn [_] (clj->js answer))})

(deftest a-gate-not-seen-lately-is-unknown-never-shut
  (let [cell (hr/cell ht/gate)]
    (is (= :shut (hr/gate-state (sensing {:name "oak_fence_gate" :properties {:open false}}) cell)) "shut gate seen")
    (is (= :open (hr/gate-state (sensing {:name "oak_fence_gate" :properties {:open true}}) cell)) "open gate seen")
    (is (= :unknown (hr/gate-state (sensing {:unknown true}) cell)) "never seen or aged out")
    (is (= :unknown (hr/gate-state (:p (seeing {} true)) cell)) "a real aged gate")
    (is (= :shut (hr/gate-state (:p (seeing {} false)) cell)) "a real gate in view")))

(defn dark-run
  "A herd run in a pitch-dark pen: a cell is made out only within 4 blocks (perception's near)."
  []
  (let [s (ht/clock-on-wait! (h/setup-seeing (coarse-world {:entities [(ht/cow 1 4 3)]}) [0 0] 700))]
    (ht/submit! s {:target 1})))

(deftest a-dark-pen-is-looked-at-then-the-run-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (dark-run)
              _ (await (ht/run-ticks s 40))]
          (is (seq (ht/events-of s :sense.looked)) "the dark cells are walked near and looked at")
          (is (= :brought (:reason (ht/done-event s))) "within 4 blocks no light is needed: read up close, the run brings, never :unreachable"))))))

(defn blind-pen!
  "Every cell of the pen and its gate stays unknown to the body even up close (dark, and a look gives nothing)."
  [s]
  (let [p (:p s)
        orig (.-sensedAt p)]
    (set! (.-sensedAt p) (fn [pos] (if (>= (.-x pos) 10) #js {:unknown true} (orig pos))))
    s))

(deftest a-pen-that-cannot-be-read-even-up-close-ends-no-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (blind-pen! (ht/clock-on-wait! (h/setup-seeing (coarse-world {:entities [(ht/cow 1 4 3)]}) [0 0] 700)))
              _ (ht/submit! s {:target 1})
              _ (await (ht/run-ticks s 60))]
          (is (= :no-pen (:reason (ht/done-event s))) "nothing can be read: the run ends :no-pen, never :unreachable, never brings"))))))

(defn aged-run
  "A herd run with the gate remembered past its 10 s, after f was applied to the setup."
  [f world]
  (let [s (f (seeing world true))]
    (ht/submit! (ht/clock-on-wait! s) {:target 1})))

(deftest check-unknown-walks-near-and-looks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (aged-run identity {:entities [(ht/cow 1 4 3)]})
              looks (atom 0)
              _ (.override (.-world (:p s)) "look" (fn [_ _ _] (swap! looks inc) #js {:status "ok"}))
              _ (await (ht/run-ticks s 40))]
          (is (pos? @looks) "the unknown cell is looked at")
          (is (<= (Math/abs (- 10 (ht/self-x s))) 4) "went within 4 of it"))))))

(deftest a-pen-never-looked-at-is-not-no-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ht/clock-on-wait! (h/setup-seeing (coarse-world {:entities [(ht/cow 1 4 3)]}) nil 700))
              _ (is (= :unloaded (:reason (read s))) "nothing seen yet: unloaded cells, not :no-start")
              _ (ht/submit! s {:target 1})
              _ (await (ht/run-ticks s 40))]
          (is (= ht/gate (:gate (ht/done-event s))) "looked at, read in full: the run got as far as choosing the pen's gate"))))))

(deftest a-walk-to-look-into-the-pen-stands-pressed-against-the-shut-gate
  ;; the walk check-unknown! makes to look at a pen floor cell: the plan's stand nearest it is the free sliver of the shut
  ;; gate's cell (x 10.0625, the gate's wall at 10.375), reached pressed against the gate as in the game
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (ht/setup (ht/world {}))
              _ (core/submit! (:eng s) '(jobs.movement.go-to {:pos [13 63 3] :range 4 :doors :never :ways #{:walk :clear-seal} :retry false}) {})
              _ (await (ht/run-ticks s 10))]
          (is (empty? (ht/events-of s :walker-fault)) (pr-str (ht/events-of s :walker-fault)))
          (is (= [10 64 3] (mapv #(js/Math.floor %) (:pos (fake/self (:p s))))) "in the gate's cell")
          (is (= false (get-in @(fake/state (:p s)) [:states [10 64 3] :open])) "the gate stays shut"))))))

(defn hide-fence-floor!
  "The pen floor beside the west fence is unseen until the body stands at the gate (x 8.5 or more): the fence hides it from outside."
  [s]
  (let [p (:p s)
        orig (.-sensedAt p)]
    (set! (.-sensedAt p)
          (fn [pos]
            (let [x (.-x pos)]
              (if (and (= 11 x) (= 63 (.-y pos)) (< (ht/self-x s) 8.5))
                #js {:unknown true}
                (orig pos)))))
    s))

(deftest a-pen-floor-hidden-by-the-fence-is-looked-at-from-the-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (hide-fence-floor! (seeing {:entities [(ht/cow 1 4 3)]} false))
              _ (ht/submit! s {:target 1})
              _ (await (ht/run-ticks s 60))]
          (is (= ht/gate (:gate (ht/done-event s))) "walked to the gate, saw the floor, read the pen in full")
          (is (= :brought (:reason (ht/done-event s)))))))))

(defn unseen-approach-with
  "unseen-approach over primitives that know nothing of the cells in unknown-cells, with looked-pen memory looked."
  ([unknown-cells looked] (unseen-approach-with unknown-cells looked :no-approach))
  ([unknown-cells looked why]
  (let [p #js {:sensedAt (fn [pos] (if (contains? unknown-cells [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} #js {}))}]
    (with-redefs [ctx/mem (constantly {:looked-pen looked})]
      (hp/unseen-approach {:primitives p} {:why why :cell [5 64 5]})))))

(deftest an-approach-cell-never-seen-is-looked-at-first-and-a-looked-at-one-is-not-again
  (is (= [5 63 5] (unseen-approach-with #{[5 63 5] [5 65 5]} [])) "floor first")
  (is (= [5 65 5] (unseen-approach-with #{[5 63 5] [5 65 5]} [[5 63 5]])) "looked-at floor skipped")
  (is (nil? (unseen-approach-with #{[5 63 5]} [[5 63 5]])) "all looked at: the verdict follows")
  (is (nil? (unseen-approach-with #{} [])) "all seen: the verdict follows")
  (is (= [5 63 5] (unseen-approach-with #{[5 63 5]} [] :no-floor-outside)) "the cell outside the gate is looked at too"))

(deftest only-a-no-approach-refusal-looks-at-an-approach-cell
  (with-redefs [ctx/mem (constantly {})]
    (is (nil? (hp/unseen-approach {:primitives #js {:sensedAt (fn [_] #js {:unknown true})}} {:why :other :cell [5 64 5]})))))
