(ns engine.path-preview-test
  "jobs.movement.path-preview against the fake world: plans like go-to, walks nowhere."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [jobs.lib.world-files :as world-files]
            ["fs" :as fs]
            ["path" :as path]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [jobs.movement.path-preview :as pp]))

(def job 'jobs.movement.path-preview)

(defn ^:async preview
  "Run the job as a child of a recording parent over the fake world (body at 0 64 1); {:out the result :p primitives :seen
  events}. opts: :places {name pos} written to memory, :markers shared markers, :no-path-world true."
  [blocks args & [{:keys [places markers no-path-world]}]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:blocks blocks :self {:pos {:x 0 :y 64 :z 1}}})
        dir (tu/tmp-dir)
        _ (when markers (fs/writeFileSync (path/join dir "places.json") (js/JSON.stringify (clj->js markers))))
        _ (when no-path-world (set! (.-pathWorld p) nil))
        w (when markers (world-files/open {:plans-dir (path/join dir "plans") :blueprint-dir (path/join dir "bps")
                                           :zones-file (path/join dir "zones.edn") :emit (fn [_])}))
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create (cond-> {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                                  :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                           w (assoc :world w)))]
    (doseq [[k pos] places] (mem/write! (:store eng) k {:pos pos} mem/place-policy))
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 200) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc i))))
    {:out @out :p p :seen seen}))

(defn pos-of [p] (let [q (.-pos (.self p))] [(.-x q) (.-y q) (.-z q)]))

(deftest a-found-route-is-reported-and-not-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (preview (floor 0 0 11 2) {:pos [10 64 1] :range 0}))]
          (is (= :completed (:status out)))
          (is (true? (:found out)))
          (is (= 10 (js/Math.round (:length out))))
          (is (pos? (:seconds out)))
          (is (string? (:summary out)))
          (is (pos? (:steps out)))
          (is (= [10 64 1] (last (:waypoints out))))
          (is (= [0 64 1] (pos-of p))))))))

(deftest a-goal-with-no-way-is-stopped-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (preview (merge (floor 0 0 3 2) (floor 8 0 11 2)) {:pos [10 64 1] :range 0}))]
          (is (= :stopped (:status out)))
          (is (false? (:found out)))
          (is (= :partial (:reason out)) "the island is out of reach: the plan only gets nearer")
          (is (number? (:near out))))))))

(deftest drop-cost-false-takes-no-two-block-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (floor 0 0 3 2) (floor 61 4 0 6 2))
              free (:out (await (preview world {:pos [5 62 1] :range 0})))
              none (:out (await (preview world {:pos [5 62 1] :range 0 :drop-cost false})))]
          (is (true? (:found free)))
          (is (= 1 (get-in free [:moves :drop])))
          (is (false? (:found none))))))))

(deftest tolls-send-the-route-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (floor 0 0 11 2)
              cheap (:out (await (preview world {:pos [10 64 1] :range 0})))
              dear (:out (await (preview world {:pos [10 64 1] :range 0
                                                :tolls (vec (for [x (range 2 9)] {:x x :y 64 :z 1 :factor 20}))})))]
          (is (= 1 (count (:waypoints cheap))))
          (is (> (count (:waypoints dear)) 1)))))))

(deftest a-bad-drop-cost-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (preview (floor 0 0 3 2) {:pos [2 64 1] :drop-cost -1}))]
          (is (= :stopped (:status out)))
          (is (false? (:found out)))
          (is (= :bad-drop-cost (:reason out))))))))

(deftest every-refusal-carries-found-false-and-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (floor 0 0 11 2)
              refused (fn ^:async refused [args opts] (:out (await (preview world args opts))))
              tolls (await (refused {:pos [5 64 1] :tolls [{:x 1}]} nil))
              pos (await (refused {:pos [1 -9999 1]} nil))
              place (await (refused {:place :mine} nil))
              sensing (await (refused {:pos [5 64 1]} {:no-path-world true}))]
          (doseq [[out reason] [[tolls :bad-tolls] [pos :bad-pos] [place :unknown-place] [sensing :unsupported]]]
            (is (= :stopped (:status out)) (str reason))
            (is (false? (:found out)) (str reason))
            (is (= reason (:reason out)))
            (is (string? (:text out)) (str reason))
            (is (not (contains? out :arrived)) (str reason))))))))

(deftest a-place-is-previewed-from-memory-and-from-a-shared-marker
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (floor 0 0 11 2)
              own (await (preview world {:place :hut :range 0} {:places {:hut {:x 5 :y 64 :z 1}}}))
              marker (await (preview world {:place :hut :range 0} {:markers [{:name "hut" :kind "base" :x 10 :y 64 :z 1}]}))
              both (await (preview world {:place :hut :range 0} {:places {:hut {:x 5 :y 64 :z 1}}
                                                                 :markers [{:name "hut" :kind "base" :x 10 :y 64 :z 1}]}))]
          (is (= [5 64 1] (last (:waypoints (:out own)))))
          (is (= [10 64 1] (last (:waypoints (:out marker)))))
          (is (= [5 64 1] (last (:waypoints (:out both)))) "the body's own place wins")
          (is (= [0 64 1] (pos-of (:p marker)))))))))

(deftest a-partial-plan-says-partial-and-hands-over-how-far-it-gets
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (preview (floor 0 0 6 2) {:pos [30 64 1] :range 0}))]
          (is (= :stopped (:status out)))
          (is (false? (:found out)))
          (is (= :partial (:reason out)))
          (is (pos? (:steps (:partial out))))
          (is (= 30 (:near out)) "the body's distance to the goal")
          (is (< (:length (:partial out)) 7))
          (is (= [0 64 1] (pos-of p))))))))

(deftest a-far-goal-is-one-plan-bounded-by-the-planners-node-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (preview (floor 0 0 80 80) {:pos [400 64 40] :range 0}))
              reports (filter #(= :path-preview (:kind %)) @seen)
              nodes (:nodes (first reports))]
          (is (= :stopped (:status out)))
          (is (= 1 (count reports)) "one plan, one report")
          (is (pos? nodes))
          (is (<= nodes 200000) "the planner's maxNodes bounds the plan: no budgeted rounds, no unbounded search"))))))

(deftest the-food-arg-defaults-to-the-body-not-to-20
  (is (nil? (get-in pp/args [:food :default]))))

(defn pond
  "Land at y 63 from x 0 to 20 and z -10 to 10, with a pond of water over x 5-15 and z -8 to 8 (stone beneath at y 61)."
  []
  (merge (floor 0 -10 20 10) (box 0 61 -10 20 61 10 "stone") (box 5 62 -8 15 63 8 "water")))

(deftest a-swim-price-flips-the-route-across-a-pond
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [route (fn ^:async route [costs] (:out (await (preview (pond) (cond-> {:pos [20 64 1] :range 0} costs (assoc :costs costs))))))
              dear (await (route {:swim-h 5}))
              default (await (route nil))
              cheap (await (route {:swim-h 0.25}))]
          (is (every? :found [dear default cheap]))
          (is (nil? (re-find #"swims" (:summary dear))) "dear water: the way round on land")
          (is (nil? (re-find #"swims" (:summary default))) "the default price: land is quicker too")
          (is (re-find #"swims" (:summary cheap)) "cheap water: straight across")
          (is (< (:seconds cheap) (:seconds default))))))))

(deftest a-swim-price-under-the-walk-floor-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [costs [{:swim-h 0.1} {:exit 0}]]
          (let [{:keys [out]} (await (preview (pond) {:pos [20 64 1] :range 0 :costs costs}))]
            (is (= :bad-costs (:reason out)) (pr-str costs))
            (is (false? (:found out)))))))))
