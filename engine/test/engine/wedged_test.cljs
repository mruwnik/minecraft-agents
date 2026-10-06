(ns engine.wedged-test
  "The wedged trigger and the unwedge job against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [triggers.survival.wedged :as wedged]
            [jobs.survival.breathe :as breathe]))

(def unwedge 'jobs.survival.unwedge)

(defn holds? [p memory] (wedged/wedged p memory {}))

(def floor {"0,63,0" "stone" "1,63,0" "stone" "-1,63,0" "stone" "0,63,1" "stone" "0,63,-1" "stone"})

(def cases
  [["sand at the feet" {:blocks {"0,64,0" "sand"}} true]
   ["dirt at the feet" {:blocks {"0,64,0" "dirt"}} true]
   ["air at the feet" {} false]
   ["a slab at the feet" {:self {:pos {:x 0 :y 64.5 :z 0}} :blocks {"0,64,0" "oak_slab"}} false]
   ["farmland at the feet" {:self {:pos {:x 0 :y 64.9375 :z 0}} :blocks {"0,64,0" "farmland"}} false]
   ["a snow layer at the feet" {:blocks {"0,64,0" "snow"}} false]
   ["soul sand at the feet" {:blocks {"0,64,0" "soul_sand"}} false]
   ["a path at the feet" {:blocks {"0,64,0" "dirt_path"}} false]
   ["only the eye cell solid" {:blocks {"0,65,0" "sand"}} false]
   ["standing on a full block" {:blocks {"0,63,0" "stone"}} false]])

(deftest trigger-on-every-branch
  (doseq [[label world expected] cases]
    (is (= expected (holds? (tu/fake world) nil)) label)))

(deftest trigger-is-registered-after-suffocating-and-burning-before-hostile
  (let [t (get triggers/all :wedged)]
    (is (= '(jobs.survival.unwedge) (:job t)))
    (is (= 0 (:cooldown-s t)))))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn call-names [p] (mapv #(.-name %) (array-seq (.. p -world -calls))))

(deftest steps-out-to-a-free-side-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (merge floor {"0,64,0" "sand"})})]
          (core/submit! eng (list unwedge) {})
          (await (core/tick! eng))
          (is (not= {:x 0 :y 64 :z 0} (core/self-pos p)))
          (is (= ["moveTo"] (call-names p)) "no dig")
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(def boxed
  (into floor (for [[x z] [[1 0] [-1 0] [0 1] [0 -1]]] [(str x ",64," z) "stone"])))

(deftest digs-the-feet-block-when-no-side-is-free
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (merge boxed {"0,64,0" "sand"})})]
          (core/submit! eng (list unwedge) {})
          (await (core/tick! eng))
          (is (= "air" (.-name (.blockAt p (tu/pos 0 64 0)))))
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng)))))))))

(deftest bedrock-at-the-feet-warns-blocked-once-and-the-trigger-stays-quiet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (merge boxed {"0,64,0" "bedrock"})})]
          (core/submit! eng (list unwedge) {})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= 1 (count (filter #(= :unwedge.blocked (:kind %)) @seen))))
          (is (= [] (:list (core/state eng))))
          (is (false? (holds? p (mem/view (:store eng)))) "no refire while the entry lasts")
          (is (true? (holds? p nil)) "without the entry it would"))))))

(deftest a-free-body-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (setup {})]
          (is (false? ((:check (get registry/jobs unwedge)) {:primitives p :args {}}))))))))

(def stone-sides-but-east
  (into floor (for [[x z] [[-1 0] [0 1] [0 -1]]] [(str x ",64," z) "stone"])))

(deftest step-out-target-is-the-free-side-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (merge stone-sides-but-east {"0,64,0" "sand"})})]
          (core/submit! eng (list unwedge) {})
          (await (core/tick! eng))
          (let [call (first (array-seq (.. p -world -calls)))
                pos (.. call -args -pos)]
            (is (= "moveTo" (.-name call)))
            (is (= [1 64 0] [(.-x pos) (.-y pos) (.-z pos)]))))))))

(deftest a-lava-side-cell-is-not-stepped-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (merge stone-sides-but-east {"0,64,0" "sand" "1,64,0" "lava" "1,63,0" "stone"})})]
          (core/submit! eng (list unwedge) {})
          (await (core/tick! eng))
          (is (not (some #{"moveTo"} (call-names p))))
          (is (= "sand" (.-name (.blockAt p (tu/pos 0 64 0))))))))))

(deftest a-fire-or-magma-side-cell-is-not-stepped-into
  (doseq [[label blocks] [["fire" {"1,64,0" "fire"}] ["magma floor" {"1,63,0" "magma_block"}]]]
    (let [p (tu/fake {:blocks (merge stone-sides-but-east {"0,64,0" "sand"} blocks)})]
      (is (nil? (breathe/side-cell p (.self p))) label))))

(deftest a-dig-beside-lava-is-refused-and-counts-as-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks (merge boxed {"0,64,0" "sand" "0,65,0" "lava"})})]
          (core/submit! eng (list unwedge) {})
          (dotimes [_ 4] (await (core/tick! eng)))
          (is (= "sand" (.-name (.blockAt p (tu/pos 0 64 0)))) "no dig")
          (is (not (some #{"dig"} (call-names p))))
          (is (= 1 (count (filter #(= :unwedge.blocked (:kind %)) @seen))) "final after three refusals"))))))
