(ns engine.plug-in-test
  "jobs.survival.plug-in: seals the body in where it stands, by walls, a pit or a side pocket, and ends done once enclosed."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.cornered-test :as ct]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world-files]
            [jobs.lib.ledger :as ledger]
            [jobs.survival.plug-way :as way]))

(def cobble [{:name "cobblestone" :count 20}])
(def pick [{:name "stone_pickaxe" :count 1}])

(defn ^:async plug!
  "Run plug-in with args over world (its :zones go to the engine) through a recording parent (setup! gets the fake first): {:out the ending :p :eng :continues}."
  [world args & [setup!]]
  (let [zones (:zones world)
        world (dissoc world :zones)
        clock (atom 1000000)
        out (atom :not-done)
        continues (atom 0)
        digs-at-first (atom nil)
        [_ sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        _ (when setup! (setup! p))
        now (tu/act-clock clock p 1000)
        caller {:round (fn ^:async caller-round [c]
                         (let [r (await (ctx/call c :plug 'jobs.survival.plug-in args))]
                           (if (= :continue r) (do (swap! continues inc) (when-not @digs-at-first (reset! digs-at-first (count (ct/calls p "dig")))) :continue) (do (reset! out r) (ctx/end c :done nil)))))}
        eng (core/create {:primitives p :world (world-files/of-data {} {} (or zones [])) :jobs (assoc registry/jobs 'caller caller) :triggers triggers/all :dir (tu/tmp-dir)
                          :now now :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (core/submit! eng '(caller) {})
    (loop [i 0]
      (when (and (= :not-done @out) (< i 40))
        (await (core/tick! eng))
        (recur (inc i))))
    {:out @out :p p :eng eng :continues @continues :digs-at-first @digs-at-first}))

(defn cells-of [p name]
  (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)]) (ct/calls p name)))

(defn purposes [eng] (set (map :purpose (ledger/open-entries (mem/view (:store eng))))))

(deftest walls-seal-the-open-sides-and-roof-and-end-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (plug! {:blocks ct/dead-end :inventory cobble :entities [(ct/skeleton 4 64 0)]} {}))]
          (is (= [:done :walls] [(:kind out) (:way (:data out))]))
          (is (= [[1 64 0] [1 65 0]] (cells-of p "place")) "the open side towards the skeleton, feet then head")
          (is (= #{:retreat-plug} (purposes eng)))
          (is (= [{:cell [1 64 0] :item "cobblestone"} {:cell [1 65 0] :item "cobblestone"}] (:placed (:data out))) "every placed block, for a caller to take back"))))))

(deftest the-callers-purpose-labels-the-ledger-entries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (plug! {:blocks ct/dead-end :inventory cobble} {:purpose :shelter}))]
          (is (= #{:shelter} (purposes eng))))))))

(deftest a-pit-digs-down-and-plugs-the-hole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/deep-dead-end :inventory pick :entities [(ct/zombie 1 64 0)]} {:ways #{:pit}}))]
          (is (= [:done :pit] [(:kind out) (:way (:data out))]))
          (is (= [[0 63 0] [0 62 0]] (cells-of p "dig"))))))))

(deftest a-pocket-is-dug-and-its-way-in-sealed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/dead-end :inventory pick :entities [(ct/skeleton 4 64 0)]} {:ways #{:pocket}}))]
          (is (= [:done :pocket] [(:kind out) (:way (:data out))]))
          (is (= #{[-1 64 0] [-1 65 0]} (set (cells-of p "dig"))))
          (is (= #{[0 64 0] [0 65 0]} (set (cells-of p "place")))))))))

(deftest a-body-already-enclosed-is-done-before-anything-is-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks (ct/rock [-3 9] [62 67] #{[0 64 0] [0 65 0]}) :inventory cobble} {}))]
          (is (= :done (:kind out)))
          (is (empty? (ct/calls p "place"))))))))

(deftest nothing-to-seal-with-or-dig-with-is-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/dead-end} {}))]
          (is (= :nothing-possible (:kind out)))
          (is (empty? (ct/calls p "place"))))))))

(deftest a-way-left-out-of-ways-is-not-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/dead-end :inventory cobble} {:ways #{:pit}}))]
          (is (= :nothing-possible (:kind out)))
          (is (empty? (ct/calls p "place"))))))))

(def room-cells
  "A 3x3 room two high with a rock roof, over rock down to y 58: eight open cells at each of feet and head height round the body."
  (set (for [x [-1 0 1] z [-1 0 1] y [64 65]] [x y z])))

(def room (ct/rock [-3 3] [58 67] room-cells))

(deftest the-cheapest-way-is-started-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks room :inventory (into cobble pick)} {}))]
          (is (= [:done :pit] [(:kind out) (:way (:data out))]) "sixteen walls cost more than a pit")
          (is (= [0 63 0] (first (cells-of p "dig")))))))))

(deftest a-way-the-body-cannot-do-here-is-dropped-not-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (plug! {:blocks ct/dead-end} {}))]
          (is (= :nothing-possible (:kind out)))
          (is (= [:walls :pit :pocket] (:dropped (:data out))))
          (is (empty? (:tried (:data out))))
          (is (empty? (ct/calls p "place"))))))))

(deftest a-way-that-cannot-be-done-leaves-the-next-cheapest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (plug! {:blocks ct/dead-end :inventory pick :entities [(ct/skeleton 4 64 0)]} {:ways #{:walls :pocket}}))]
          (is (= [:done :pocket] [(:kind out) (:way (:data out))])))))))

(deftest a-way-that-fails-is-named-with-its-cause-and-the-dropped-ways-are-listed-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (plug! {:blocks ct/dead-end :inventory cobble :entities [(ct/zombie 1 64 0)]} {}))
              d (:data out)]
          (is (= :nothing-possible (:kind out)))
          (is (= [:walls] (:tried d)))
          (is (= [:pit :pocket] (:dropped d)))
          (is (= [:walls] (keys (:why d))))
          (is (re-find #"walls" (:text d))))))))

(deftest a-place-refused-for-a-mob-yields-continue-to-the-caller
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              refuse-twice (fn [p] (.override (.-world p) "place"
                                              (fn ^:async f [token a impl]
                                                (if (< (swap! calls inc) 3)
                                                  #js {:status "failed" :reason "Server refused" :refusal #js {:entities #js [#js {:name "zombie" :id 7}]}}
                                                  (await (impl token a))))))
              {:keys [out continues]} (await (plug! {:blocks ct/dead-end :inventory cobble} {} refuse-twice))]
          (is (pos? continues))
          (is (= :done (:kind out))))))))

(deftest a-way-that-fails-with-a-hostile-adjacent-yields-before-the-next-way-starts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [way/walls-step! (fn ^:async fail [_] nil)]
          (let [{:keys [digs-at-first]} (await (plug! {:blocks ct/dead-end :inventory (into cobble pick) :entities [(ct/zombie 1 64 0)]} {:ways #{:walls :pocket}}))]
            (is (= 0 digs-at-first) "the round ended :continue before the pocket dug")))))))

(deftest a-torch-in-an-off-limits-cell-is-not-dug-over
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (plug! {:blocks (assoc ct/dead-end "1,64,0" "torch") :inventory cobble :entities [(ct/skeleton 4 64 0)]
                                         :zones [{:name "keep-out" :min [1 64 0] :max [1 64 0]}]} {}))]
          (is (empty? (cells-of p "dig")) "the torch stays"))))))

(defn needs-support!
  "Make place answer no-support for a cell with no block beside, above or below it, as the real primitive does."
  [p]
  (.override (.-world p) "place"
             (fn ^:async f [token args impl]
               (let [x (.-x (.-pos args)) y (.-y (.-pos args)) z (.-z (.-pos args))
                     held? (some #(some-> (.blockAt p #js {:x (+ x (nth % 0)) :y (+ y (nth % 1)) :z (+ z (nth % 2))}) .-name (not= "air"))
                                 [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])]
                 (if held? (await (impl token args)) #js {:status "no-support"})))))

(deftest a-support-block-in-another-zone-is-not-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (plug! {:blocks {"0,63,0" "stone"} :floor [0 0 0 0] :inventory cobble :entities [(ct/skeleton 4 64 0)]
                                         :zones [{:name "keep-out" :min [-1 63 -1] :max [1 63 1]}]} {} needs-support!))]
          (is (empty? (filterv (fn [[_ y _]] (= 63 y)) (cells-of p "place"))) "nothing is placed under the wall cells"))))))
