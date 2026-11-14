(ns engine.go-to-night-test
  "go-to at night from a shelter (sh-inside/walk-leaves-shelter?): no wait for dawn; its safe? judges the walk's risk
  (jobs.lib.walk-risk), so a short walk out of the hut starts. A caller's :checks (an escape's defaults, an agent's own)
  replace it."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
                        [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.hut-shelter-test :as hut]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.sky-perception :as sky]
            [engine.scenario :as scenario]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.movement.go-to.cut :as gcut]
            [jobs.movement.go-to.escalation :as escalation]
            [jobs.movement.go-to.reprice :as reprice]))

(def inside {:x 5 :y 64 :z 0})
(def outside {:x 15 :y 64 :z 0})

(defn night-hut [extra] (merge (hut/hut-world inside extra) {:time st/night}))

(defn ^:async ticks [eng n]
  (await (st/tick-n eng n)))

(defn waiting-of [eng id] (core/waiting eng id))

(defn far-hut
  "night-hut 300000 blocks from world spawn, the body with no bed: a death on a night walk out costs more than waiting for dawn."
  [extra]
  (assoc-in (night-hut extra) [:self :worldSpawn] [300000 64 0]))

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
  "{:eng :p :seen} for world with extra jobs registered."
  [world extra]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (with-sky-and-light (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world)) world)
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

(deftest a-listed-go-to-out-of-the-hut-walks-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (night-hut {}))]
          (core/submit! eng (tu/order (list 'jobs.movement.go-to {:pos [15 64 0]})) {})
          (await (tu/run-until-empty eng 40))
          (is (empty? (:list (core/state eng))))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "the short walk is not waited for dawn"))))))

(deftest a-walk-inside-the-room-runs-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (night-hut {}))]
          (core/submit! eng (tu/order '(jobs.movement.go-to {:pos [6 64 -1] :range 0})) {})
          (await (tu/run-until-empty eng 20))
          (is (empty? (:list (core/state eng))))
          (is (= {:x 6 :z -1} (select-keys (st/pos-of p) [:x :z]))))))))

(deftest a-checks-override-walks-out-anyway-when-the-walk-is-too-risky
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [safe ['(not (threatened)) '(always)]]
          (let [{:keys [eng p]} (setup (far-hut {}))]
            (core/submit! eng (tu/order (list 'jobs.movement.go-to {:pos [15 64 0] :checks {:safe? safe}})) {})
            (await (tu/run-until-empty eng 40))
            (is (empty? (:list (core/state eng))))
            (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) (pr-str safe))))))))

(deftest a-child-under-an-escape-default-walks-out-at-night-when-the-walk-is-too-risky
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (hut-with (far-hut {}) {'escaper escaper-job})]
          (core/submit! eng (tu/order '(escaper)) {})
          (await (tu/run-until-empty eng 200))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "the escape's walk ignores the risk"))))))

(deftest an-exclusive-go-to-too-risky-ends-at-once-with-the-reason-and-walks-with-a-checks-override
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args walks?] [[{:pos [15 64 0]} false] [{:pos [15 64 0] :checks {:safe? '(not (threatened))}} true]]]
          (let [{:keys [eng p seen]} (setup (far-hut {}))
                id (core/exclusive-run! eng (tu/order (list 'jobs.movement.go-to args)))]
            (await (ticks eng 40))
            (is (nil? (waiting-of eng id)) "the exclusive go-to is not held")
            (is (= walks? (> 2 (js/Math.abs (- 15 (:x (st/pos-of p)))))) (pr-str args))
            (is (= (not walks?) (some? (some #(= :risky (:reason %)) (st/emitted seen :stopped)))) (pr-str args))))))))

(defn parent-world
  "{:eng :p :seen} for far-hut with a parent job 'walker whose round is round-fn."
  [round-fn]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (sky/with-sky (tu/fake-on-floor (update (far-hut {}) :blocks merge (into {} (for [x (range 4 7) z (range -1 2)] [(str x ",63," z) "stone"])))))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker {:round round-fn}) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(deftest a-child-go-to-too-risky-ends-condition-failed-and-the-parent-decides
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [look/unseen-solid-at-fn (constantly (constantly 0))]
         (let [rounds (atom [])
              {:keys [eng p]} (parent-world (fn ^:async walker [c]
                                              (let [r (await (ctx/call c :walk 'jobs.movement.go-to {:pos [15 64 0]}))]
                                                (swap! rounds conj r)
                                                (if (= :continue r) r (ctx/end c :done nil)))))]
          (core/submit! eng (tu/order '(walker)) {})
          (await (ticks eng 4))
          (is (= inside (st/pos-of p)))
          (is (= [:condition-failed :risky] [(:kind (first @rounds)) (:reason (:cause (first @rounds)))]))))))))

(deftest a-parent-walking-with-walk-into-or-run!-waits-with-the-childs-risky-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [look/unseen-solid-at-fn (constantly (constantly 0))]
        (doseq [walk [(fn ^:async walker [c] (await (child/walk-into!* c :walk [15 64 0])))
                      (fn ^:async walker [c] (await (child/run!* c :walk 'jobs.movement.go-to {:pos [15 64 0]})))]]
          (let [{:keys [eng p]} (parent-world walk)
                id (core/submit! eng (tu/order '(walker)) {})]
            (await (ticks eng 4))
            (is (= inside (st/pos-of p)))
            (is (= :risky (:reason (waiting-of eng id))) "the parent waits with the child's reason"))))))))

(deftest an-exclusive-go-to-at-night-walks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (night-hut {}))]
          (core/exclusive-run! eng (tu/order (list 'jobs.movement.go-to {:pos [15 64 0]})))
          (await (ticks eng 40))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))))
          (is (empty? (filter #(= :risky (:reason %)) (st/emitted seen :stopped)))))))))

;; ------------------------------------------------------------------ a child go-to

(deftest get-seeds-in-a-shelter-at-night-waits-with-the-reason-instead-of-spinning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (-> (night-hut {})
                                         (update :blocks merge {"10,64,0" "short_grass" "11,64,0" "short_grass"})
                                         (assoc :drops {"short_grass" "wheat_seeds"})))
              id (core/submit! eng (tu/order '(jobs.gather.get-seeds {:count 2})) {})]
          (await (ticks eng 6))
          (is (= inside (st/pos-of p)))
          (is (= :dark (:reason (waiting-of eng id)))))))))

(def oak-tree
  "A trunk of four logs with leaves on top, outside the hut."
  (into {"20,68,0" "oak_leaves"} (for [y (range 64 68)] [(str "20," y ",0") "oak_log"])))

(defn ^:async fell-tree-world! [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (sky/with-sky (tu/seeing-all (tu/fake-on-floor world)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng (tu/order '(jobs.forestry.harvest-wood {:count 1 :radius 16})) {})
    (await (ticks eng 60))
    {:eng eng :p p :seen seen}))

(deftest harvest-wood-in-a-shelter-at-night-waits-for-day-instead-of-ending-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (fell-tree-world! (update (far-hut {}) :blocks merge oak-tree)))]
          (is (= inside (st/pos-of p)) "stayed in the hut")
          (is (empty? (st/emitted seen :stopped)) (pr-str (map :reason (st/emitted seen :stopped)))))))))

(deftest a-sealed-hut-far-from-spawn-holds-for-dawn-instead-of-cutting-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge (far-hut {"5,64,2" "cobblestone" "5,65,2" "cobblestone"})
                                            {:inventory [{:name "iron_pickaxe" :count 1}]}))
              id (core/submit! eng (tu/order '(jobs.movement.go-to {:pos [15 64 0]})) {})
              holds (atom []) ends (atom 0)
              orig-hold escalation/start-hold orig-end reprice/end]
          ;; the hold is priced once a choice: each start-hold's :later ends the round (a reprice walk is ~40 ms)
          (set! escalation/start-hold (fn [& args] (let [s (apply orig-hold args)] (swap! holds conj s) s)))
          (set! reprice/end (fn [c s] (when (some #(identical? s %) @holds) (swap! ends inc)) (orig-end c s)))
          (try (await (ticks eng 30))
               (finally (set! escalation/start-hold orig-hold) (set! reprice/end orig-end)))
          (is (= inside (st/pos-of p)) "the body stays in its room")
          (is (empty? (st/calls p "dig")) "no wall is cut before the walk is priced")
          (is (= :risky (:reason (waiting-of eng id))))
          (is (pos? @ends))
          (is (= (count @holds) @ends) "each choice prices its hold once"))))))

;; ------------------------------------------------------------------ the night trigger and a queued walk (card 4252e02e)

(deftest a-queued-walk-too-risky-does-not-lead-the-body-out-of-its-mended-room-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge (far-hut {"5,67,0" "air"}) {:inventory st/dirt-stack}))
              id (core/submit! eng (tu/order '(jobs.movement.go-to {:pos [15 64 0]})) {})]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (await (ticks eng 20))
          (is (= inside (st/pos-of p)) "the body stays in its room all night")
          (is (= [{:x 5 :y 67 :z 0}] (mapv st/arg-pos (st/calls p "place"))) "one block mends the roof, no dig-in after")
          (is (= :risky (:reason (waiting-of eng id))))
          (is (= 1 (count (st/calls p "place")))))))))

(deftest a-walk-with-an-acceptable-risk-goes-out-at-night-with-the-night-trigger-muted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge (night-hut {"5,67,0" "air"}) {:inventory st/dirt-stack}))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/mute! eng :night nil)
          (core/submit! eng (tu/order '(jobs.movement.go-to {:pos [15 64 0]})) {})
          (await (tu/run-until-empty eng 60))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "the walk runs at night")
          (is (<= (count (st/calls p "place")) 1) "no dig-in on the way"))))))

(def sent-back-seed
  "The memory of a go-to to outside left open in this session after a cut found the body at x 11, 4 blocks from it."
  {:open (gcut/session-id) :goal outside :closest 4 :ref 4 :last-dist 4 :prev {:x 11.5 :y 64 :z 0.5}})

(defn seeded-walker
  "walker-job whose go-to child starts with seed as its memory."
  [seed]
  {:round (fn ^:async seeded-walker [c]
            (when (nil? (get-in (ctx/mem c) [:children :walk]))
              (ctx/update-mem! c assoc-in [:children :walk] (merge {:args {:pos [15 64 0]} :children {}} seed)))
            (let [r (await (child/call-walk* c :walk {:pos [15 64 0]}))]
              (if (= :continue r) r (ctx/end c :done nil))))})

(deftest a-walk-sent-back-into-the-hut-at-night-waits-for-the-morning
  ;; a reflex cut the walk at x 11 and the body is back in its hut: walked again, it is cut again. The walk is lit and
  ;; risks nothing, no trigger is read: the body learns it from its own cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (hut-with (assoc (night-hut {}) :light {} :light-default [15 15]) {'seeded (seeded-walker sent-back-seed)})
              id (core/submit! eng (tu/order '(seeded)) {})]
          (await (ticks eng 20))
          (let [w (waiting-of eng id)
                sent (:data (mem/latest (mem/view (:store eng)) :sent-back))]
            (is (= inside (st/pos-of p)) "the body stays in its hut")
            (is (= [:risky :dawn] ((juxt :reason :wait) w)))
            (is (= {:place :room :to inside} (select-keys sent [:place :to])))))
        (let [{:keys [eng p]} (hut-with (assoc (night-hut {}) :light {} :light-default [15 15]) {'seeded (seeded-walker (dissoc sent-back-seed :open))})]
          (core/submit! eng (tu/order '(seeded)) {})
          (await (tu/run-until-empty eng 40))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "a call that ended, not cut, walks out")
          (is (nil? (mem/latest (mem/view (:store eng)) :sent-back))))
        (let [{:keys [eng]} (hut-with (assoc (night-hut {}) :light {} :light-default [15 15]) {'seeded (seeded-walker (assoc sent-back-seed :prev {:x 5.5 :y 64 :z 0.5}))})]
          (core/submit! eng (tu/order '(seeded)) {})
          (await (ticks eng 20))
          (is (nil? (mem/latest (mem/view (:store eng)) :sent-back)) "cut in the hut where it restarts (a reflex in place): no entry"))
        (let [{:keys [eng p]} (hut-with (assoc (night-hut {}) :light {} :light-default [15 15]) {'seeded (seeded-walker (assoc sent-back-seed :prev {:x 5.5 :y 64 :z 3.5}))})]
          (core/submit! eng (tu/order '(seeded)) {})
          (await (ticks eng 20))
          (is (= :room (:place (:data (mem/latest (mem/view (:store eng)) :sent-back)))) "cut on the step outside the door, a block back inside: an entry")
          (is (= inside (st/pos-of p)) "and the next walk out waits"))
        (let [{:keys [eng p]} (hut-with (assoc (night-hut {}) :time st/noon) {'seeded (seeded-walker sent-back-seed)})]
          (core/submit! eng (tu/order '(seeded)) {})
          (await (tu/run-until-empty eng 40))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "sent back by day, no home due: not for the night, it walks out")
          (is (nil? (mem/latest (mem/view (:store eng)) :sent-back))))))))

(deftest a-sent-back-entry-ends-when-a-sleep-brings-the-morning
  ;; the entry was written at night; the body slept and the world clock passed the morning, so the walk out is not held
  ;; (wall-clock ttl and time of day alone kept it holding all the next day)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (hut-with (assoc (night-hut {}) :light {} :light-default [15 15]) {'seeded (seeded-walker sent-back-seed)})
              id (core/submit! eng (tu/order '(seeded)) {})]
          (await (ticks eng 20))
          (let [sent (:data (mem/latest (mem/view (:store eng)) :sent-back))]
            (is (= :dawn (:wait (waiting-of eng id))))
            (is (< (.-dayTime (.self p)) (:until sent)) "the entry ends at an absolute morning")
            (is (> (:wait-s (waiting-of eng id)) 100)))
          (swap! (fake/state p) #(-> % (assoc :time 1000) (update :days inc)))
          (await (tu/run-until-empty eng 60))
          (is (> 2 (js/Math.abs (- 15 (:x (st/pos-of p))))) "the morning came: the walk out goes"))))))

;; ------------------------------------------------------------------ in-place jobs still run

(deftest a-listed-get-food-eats-carried-bread-in-the-hut-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (-> (night-hut {})
                                            (assoc-in [:self :food] 2)
                                            (assoc :inventory [{:name "bread" :count 4}])))]
          (core/submit! eng (tu/order '(jobs.survival.get-food {})) {})
          (await (tu/run-until-empty eng 20))
          (is (= ["bread"] (distinct (map #(aget (.-args %) "item") (st/calls p "eat")))) (pr-str (map #(.-name %) (.-calls (.-world p)))))
          (is (= inside (st/pos-of p))))))))

;; ------------------------------------------------------------------ sleep from a shelter

(deftest sleep-from-a-shelter-never-holds-night-sheltered-whatever-the-bed-distance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [bed [{:x 150 :y 64 :z 0} outside]]
          (let [{:keys [eng seen]} (setup (night-hut {}))]
            (core/submit! eng (tu/order (list 'jobs.survival.sleep {:bed bed})) {})
            (await (ticks eng 4))
            (is (not-any? #{:night-sheltered} (map :reason (st/emitted seen :holding))) (pr-str bed))))))))

(deftest sleep-walks-round-to-a-stand-that-sees-the-bed-not-one-behind-its-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge (hut/hut-world {:x 5 :y 64 :z -5} {"5,64,-1" "red_bed" "5,64,2" "air" "5,65,2" "air"}) {:time st/night}))]
          (tu/seeing-after-look p)
          (core/submit! eng (tu/order '(jobs.survival.sleep {:bed {:x 5 :y 64 :z -1}})) {})
          (await (tu/run-until-empty eng 60))
          (is (seq (st/calls p "sleep")) "it lies down")
          (is (<= -1 (:z (st/pos-of p)) 1) (str "from inside the room, not from behind the back wall: " (pr-str (st/pos-of p)))))))))

(deftest a-queued-craft-whose-walk-is-too-risky-stays-in-the-hut-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (-> (far-hut {}) (update :blocks assoc "10,64,0" "crafting_table")
                                         (assoc :inventory [{:name "wheat" :count 3}])))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/submit! eng (tu/order '(jobs.items.craft {:item "bread" :table {:x 10 :y 64 :z 0}})) {})
          (await (ticks eng 20))
          (is (= inside (st/pos-of p)) "the body stays in the hut"))))))

(deftest a-queued-craft-with-an-acceptable-walk-risk-goes-to-the-table-at-night-with-the-night-trigger-muted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (-> (night-hut {}) (update :blocks assoc "10,64,0" "crafting_table")
                                            (assoc :inventory [{:name "wheat" :count 3}])))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
          (core/mute! eng :night nil)
          (core/submit! eng (tu/order '(jobs.items.craft {:item "bread" :table {:x 10 :y 64 :z 0}})) {})
          (await (tu/run-until-empty eng 60))
          (is (> 4 (js/Math.abs (- 10 (:x (st/pos-of p))))) "it walks to the table"))))))

(deftest a-use-on-submitted-from-a-night-shelter-waits-with-the-walks-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (far-hut {}))
              id (core/submit! eng (tu/order '(jobs.items.use {:on [15 64 0]})) {})]
          (await (ticks eng 6))
          (is (= inside (st/pos-of p)))
          (is (= [:risky :dawn] ((juxt :reason :wait) (waiting-of eng id)))))))))
