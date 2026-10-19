(ns engine.go-to-night-test
  "go-to at night from a shelter (sh-inside/walk-leaves-shelter?): its safe? is :later (:exposed, until dawn) for a walk that
  would take the body out of it (a listed go-to waits, a child call makes its parent wait), so a queued job does not
  lead the body into the dark and the night trigger does not dig it in again and again. A caller's :checks (an escape's
  defaults, an agent's own) decide whether it walks."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
                        [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.hut-shelter-test :as hut]
            [engine.registry :as registry]
            [engine.sky-perception :as sky]
            [engine.scenario :as scenario]
            [engine.settings :as settings]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.child :as child]
            [jobs.lib.shelter.inside :as sh-inside]))

(def inside {:x 5 :y 64 :z 0})
(def outside {:x 15 :y 64 :z 0})

(defn night-hut [extra] (merge (hut/hut-world inside extra) {:time st/night}))

(defn ^:async ticks [eng n]
  (await (st/tick-n eng n)))

(defn waiting-of [eng id] (core/waiting eng id))

(def walker-job
  "A parent that walks out of the hut with one go-to child call."
  {:round (fn ^:async walker [c]
            (let [r (await (ctx/call c :walk 'jobs.movement.go-to {:pos [15 64 0]}))]
              (if (= :continue r) r (ctx/end c :done nil))))})

(defn with-sky-and-light
  "p with the fake sky, unless its world has light of its own (tu/fake then reads the sky and the cells' light)."
  [p world]
  (if (:light world) p (sky/with-sky p)))

(defn setup
  "st/setup with the body's perception over the fake sky (a world with light of its own has it already)."
  [world]
  (let [r (st/setup world)]
    (with-sky-and-light (:p r) world)
    r))

(defn hut-with
  "{:eng :p :seen} for the night hut with extra jobs registered."
  [extra]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (with-sky-and-light (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} (night-hut {}))) (night-hut {}))
        eng (core/create {:primitives p :jobs (merge registry/jobs extra) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(def escaper-job (assoc walker-job :defaults {:checks {:safe? '(always) :fit? '(always)}}))

;; ------------------------------------------------------------------ which walks leave the shelter

;; a 1x1 pit with a roof: dirt round the feet and head cells, a roof over the head (a closed room of one cell; dug in
;; when its :shelter entry says so)
(def pit
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str (+ 0 dx) "," y "," (+ 0 dz)) "dirt"]))
         {"0,66,0" "dirt"}))

(deftest walk-leaves-shelter-names-the-shelter-a-walk-would-leave
  (are [world shelter goal range kind] (= kind (sh-inside/walk-leaves-shelter? (tu/fake (merge {:floor tu/walk-floor} world)) shelter goal range))
    (night-hut {}) nil outside 1 :room
    (night-hut {}) nil {:x 6 :y 64 :z -1} 0 nil
    (night-hut {}) nil {:x 4 :y 64 :z 1} 1 nil
    (night-hut {}) nil {:x 6 :z -1} 1 nil
    (night-hut {}) nil {:x 15 :z 0} 1 :room
    (merge (night-hut {}) {:time st/noon}) nil outside 1 nil
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}}} nil outside 1 nil
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit} nil outside 1 :room
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit} nil {:x 0 :y 64 :z 1} 1 nil
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks pit} {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0}} outside 1 :dug-in
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks (merge pit {"15,66,0" "stone"})} {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0}} outside 1 :dug-in
    {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks {"0,67,0" "stone"}} nil outside 1 nil))

;; ------------------------------------------------------------------ a listed go-to

(deftest a-listed-go-to-out-of-the-hut-waits-at-night-and-walks-by-day
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (night-hut {}))
              id (core/submit! eng (list 'jobs.movement.go-to {:pos [15 64 0]}) {})]
          (await (ticks eng 6))
          (is (= inside (st/pos-of p)) "the body stays in")
          (is (= :exposed (:reason (waiting-of eng id))))
          (is (seq (:list (core/state eng))) "the go-to is still listed")
          (.setTime (.-world p) st/noon)
          (await (tu/run-until-empty eng 40))
          (is (empty? (:list (core/state eng))))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "by day it walks out"))))))

(deftest a-walk-inside-the-room-runs-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (night-hut {}))]
          (core/submit! eng '(jobs.movement.go-to {:pos [6 64 -1] :range 0}) {})
          (await (tu/run-until-empty eng 20))
          (is (empty? (:list (core/state eng))))
          (is (= {:x 6 :z -1} (select-keys (st/pos-of p) [:x :z]))))))))

(deftest a-checks-override-walks-out-anyway
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [safe ['(not (threatened)) '(always)]]
          (let [{:keys [eng p]} (setup (night-hut {}))]
            (core/submit! eng (list 'jobs.movement.go-to {:pos [15 64 0] :checks {:safe? safe}}) {})
            (await (tu/run-until-empty eng 40))
            (is (empty? (:list (core/state eng))))
            (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) (pr-str safe))))))))

(deftest a-child-under-an-escape-default-walks-out-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (hut-with {'escaper escaper-job})]
          (core/submit! eng '(escaper) {})
          (await (tu/run-until-empty eng 200))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "the escape's walk does not wait for day"))))))

(deftest an-exclusive-go-to-at-night-ends-at-once-with-the-reason-and-walks-with-a-checks-override
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args walks?] [[{:pos [15 64 0]} false] [{:pos [15 64 0] :checks {:safe? '(not (threatened))}} true]]]
          (let [{:keys [eng p seen]} (setup (night-hut {}))
                id (core/exclusive-run! eng (list 'jobs.movement.go-to args))]
            (await (ticks eng 40))
            (is (nil? (waiting-of eng id)) "the exclusive go-to is not held until day")
            (is (= walks? (> 2 (js/Math.abs (- 15 (:x (st/pos-of p)))))) (pr-str args))
            (is (= (not walks?) (some? (some #(= :exposed (:reason %)) (st/emitted seen :stopped)))) (pr-str args))))))))

;; ------------------------------------------------------------------ a child go-to

(deftest a-child-go-to-out-of-the-hut-ends-condition-failed-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (sky/with-sky (tu/fake-on-floor (night-hut {})))
              rounds (atom [])
              parent {:round (fn ^:async walker [c]
                               (let [r (await (ctx/call c :walk 'jobs.movement.go-to {:pos [15 64 0]}))]
                                 (swap! rounds conj r)
                                 (if (= :continue r) r (ctx/end c :done nil))))}
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng '(walker) {})
          (await (ticks eng 4))
          (is (= inside (st/pos-of p)))
          (is (seq @rounds))
          (is (= [:condition-failed :exposed] [(:kind (first @rounds)) (:reason (:cause (first @rounds)))]) "the child ends at once with the reason; the parent decides")
          (is (empty? (st/emitted seen :go-to.night-sheltered)) "go-to says nothing of its own"))))))

(deftest a-parent-walking-with-walk-into-shows-the-night-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (sky/with-sky (tu/fake-on-floor (night-hut {})))
              parent {:round (fn ^:async walker [c]
                               (await (child/walk-into!* c :walk [15 64 0])))}
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (let [id (core/submit! eng '(walker) {})]
            (await (ticks eng 4))
            (is (= inside (st/pos-of p)))
            (is (= :exposed (:reason (waiting-of eng id))) "the parent waits with the child's reason")))))))

(deftest get-seeds-in-a-shelter-at-night-waits-with-the-reason-instead-of-spinning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (sky/with-sky (tu/seeing-all (tu/fake-on-floor (-> (night-hut {})
                                                        (update :blocks merge {"10,64,0" "short_grass" "11,64,0" "short_grass"})
                                                        (assoc :drops {"short_grass" "wheat_seeds"})))))
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (let [id (core/submit! eng '(jobs.gather.get-seeds {:count 2}) {})]
            (await (ticks eng 6))
            (is (= inside (st/pos-of p)))
            (is (= :dark (:reason (waiting-of eng id))))))))))

(deftest a-parent-walking-with-run!-shows-the-night-wait-and-yields
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              p (sky/with-sky (tu/fake-on-floor (night-hut {})))
              parent {:round (fn ^:async walker [c]
                               (await (child/run!* c :walk 'jobs.movement.go-to {:pos [15 64 0]})))}
              eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                                :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (let [id (core/submit! eng '(walker) {})]
            (await (ticks eng 4))
            (is (= inside (st/pos-of p)))
            (is (= :exposed (:reason (waiting-of eng id))) "the parent waits with the child's reason")))))))

(def oak-tree
  "A trunk of four logs with leaves on top, outside the hut."
  (into {"10,68,0" "oak_leaves"} (for [y (range 64 68)] [(str "10," y ",0") "oak_log"])))

(defn ^:async fell-tree-world! [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (sky/with-sky (tu/seeing-all (tu/fake-on-floor world)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(jobs.forestry.harvest-wood {:count 1 :radius 16}) {})
    (await (ticks eng 60))
    {:eng eng :p p :seen seen}))

(deftest harvest-wood-in-a-shelter-at-night-waits-for-day-instead-of-ending-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (fell-tree-world! (update (night-hut {}) :blocks merge oak-tree)))]
          (is (= inside (st/pos-of p)) "stayed in the hut")
          (is (empty? (st/emitted seen :stopped)) (pr-str (map :reason (st/emitted seen :stopped)))))))))

;; ------------------------------------------------------------------ the night trigger and a queued walk (card 4252e02e)

(deftest a-queued-walk-does-not-lead-the-body-out-of-its-mended-room-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge (night-hut {"5,67,0" "air"}) {:inventory st/dirt-stack}))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.movement.go-to {:pos [15 64 0]}) {})
          (await (ticks eng 20))
          (is (= inside (st/pos-of p)) "the body stays in its room all night")
          (is (= [{:x 5 :y 67 :z 0}] (mapv st/arg-pos (st/calls p "place"))) "one block mends the roof, no dig-in after")
          (.setTime (.-world p) st/noon)
          (await (tu/run-until-empty eng 40))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "by day the walk runs")
          (is (= 1 (count (st/calls p "place")))))))))

;; ------------------------------------------------------------------ in-place jobs still run

(deftest a-listed-get-food-eats-carried-bread-in-the-hut-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (-> (night-hut {})
                                            (assoc-in [:self :food] 2)
                                            (assoc :inventory [{:name "bread" :count 4}])))]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (tu/run-until-empty eng 20))
          (is (= ["bread"] (distinct (map #(aget (.-args %) "item") (st/calls p "eat")))) (pr-str (map #(.-name %) (.-calls (.-world p)))))
          (is (= inside (st/pos-of p))))))))

;; ------------------------------------------------------------------ sleep from a shelter

(deftest sleep-from-a-shelter-waits-for-a-far-bed-and-leaves-for-a-near-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[bed expect-wait?] [[{:x 150 :y 64 :z 0} true] [outside false]]]
          (let [{:keys [eng seen]} (setup (night-hut {}))]
            (core/submit! eng (list 'jobs.survival.sleep {:bed bed}) {})
            (await (ticks eng 4))
            (is (= expect-wait? (some? (some #{:night-sheltered} (map :reason (st/emitted seen :holding))))) (pr-str bed))))))))

(deftest a-queued-craft-does-not-walk-out-of-the-hut-to-a-table-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (-> (night-hut {}) (update :blocks assoc "10,64,0" "crafting_table")
                                            (assoc :inventory [{:name "wheat" :count 3}])))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng '(jobs.items.craft {:item "bread" :table {:x 10 :y 64 :z 0}}) {})
          (await (ticks eng 20))
          (is (= inside (st/pos-of p)) "the body stays in the hut")
          (.setTime (.-world p) st/noon)
          (await (tu/run-until-empty eng 40))
          (is (> 4 (js/Math.abs (- 10 (:x (st/pos-of p))))) "by day it walks to the table"))))))

(deftest a-use-on-submitted-from-a-night-shelter-waits-with-the-walks-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (night-hut {}))
              id (core/submit! eng '(jobs.blocks.use-on {:pos [15 64 0]}) {})]
          (await (ticks eng 6))
          (is (= inside (st/pos-of p)))
          (is (= :exposed (:reason (waiting-of eng id)))))))))
