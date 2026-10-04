(ns engine.go-to-test
  "jobs.movement.go-to over the path planner and the executor, against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.takeover :as takeover]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]))

(def start {:x 0 :y 64 :z 0})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:self {:pos start}} world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn recording-parent
  "A parent that runs go-to with args as its child and keeps the child's result in out."
  [out args]
  {:check (constantly true)
   :round (fn ^:async recording-round [c]
            (let [r (await (ctx/call-child c :kid 'jobs.movement.go-to args))]
              (when (= :done r) (reset! out (ctx/child-result c :kid)))
              r))})

(defn ^:async tick-out!
  "Tick until the list is empty, at most n ticks; the ticks used."
  [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async go!
  "Run go-to with args as a child over world; {:eng :p :seen :out :ticks}, out the child's result."
  [world args]
  (let [{:keys [eng] :as s} (setup world)
        out (atom :not-done)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out args)))]
    (core/submit! eng '(recording-parent) {})
    (assoc s :eng eng :out out :ticks (await (tick-out! eng 30)))))

(defn at [p] (let [pos (.-pos (.self p))] [(.-x pos) (.-y pos) (.-z pos)]))

(defn moved [eng] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))

(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))

(def flat (floor -2 -3 40 3))

(deftest go-to-arrives-within-range
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [r [0 1 2]]
          (let [{:keys [out p]} (await (go! {:blocks flat} {:pos [10 64 0] :range r}))
                [x _ z] (at p)]
            (is (= {:arrived true} @out) (str "range " r))
            (is (<= (+ (* (- 10 x) (- 10 x)) (* z z)) (* r r)) (str "within range " r ", at " (at p)))))))))

(deftest go-to-already-within-range-does-not-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p]} (await (go! {:blocks flat} {:pos [1 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [0 64 0] (at p)))
          (is (= [] (moved eng))))))))

(deftest go-to-writes-a-moved-entry-for-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (go! {:blocks flat} {:pos [10 64 0] :range 0}))
              [entry] (moved eng)]
          (is (= 1 (count (moved eng))))
          (is (= {:from start :status "arrived" :target {:x 10 :y 64 :z 0}} (select-keys entry [:from :status :target])))
          (is (= 10 (:x (:to entry))))
          (is (= {:cap 20 :ttl 600000} (mem/policy (mem/view (:store eng)) :moved))))))))

(def ledge (box 7 64 0 11 64 2 "stone"))
(def gap-up-world {:blocks (merge (box 0 63 0 4 63 2 "stone") ledge)})

(deftest go-to-gives-up-after-three-fruitless-rounds-and-carries-the-planner-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out eng p] :as s} (await (go! (assoc gap-up-world :self {:pos {:x 0 :y 64 :z 1}}) {:pos [10 65 1]}))]
          (is (= {:arrived false :reason :unreachable :why :abilities :kind :gap-up} @out))
          (is (= [0 64 1] (at p)) "the body did not move")
          (is (= ["blocked" "blocked" "blocked"] (mapv :status (moved eng))) "one :moved entry per round")
          (is (= [{:tries 3 :why :abilities :refused-kind :gap-up}]
                 (mapv #(select-keys % [:tries :why :refused-kind]) (events-of s :unreachable))))
          (is (= [{:arrived false :reason :unreachable :why :abilities :refused-kind :gap-up}]
                 (mapv #(select-keys % [:arrived :reason :why :refused-kind]) (events-of s :result)))))))))

(deftest go-to-reports-an-unreachable-fake-cell-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! {:blocks flat :unreachable ["10,64,0"]} {:pos [10 64 0]}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (keyword? (:why @out)) "the planner's reason")
          (is (= [0 64 0] (at p))))))))

(deftest go-to-keeps-walking-while-rounds-get-closer
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the floor ends at x 40 and the goal is in unloaded land: the first round walks to the edge, then nothing gets closer
        (let [{:keys [out eng p]} (await (go! {:blocks flat} {:pos [120 64 0]}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (> (first (at p)) 30) "the first round walked a long way")
          (is (= ["partial" "blocked" "blocked" "blocked"] (mapv :status (moved eng)))
              "a round that gets closer is progress and resets the count"))))))

(deftest go-to-accepts-doors
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [doors [:never :shut :leave-open]]
          (let [{:keys [out]} (await (go! {:blocks flat} {:pos [6 64 0] :doors doors}))]
            (is (= {:arrived true} @out) (str doors))))))))

(deftest go-to-without-path-sensing-says-unsupported
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks flat})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [6 64 0]})))]
          (set! (.-pathWorld p) nil)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 10))
          (is (= {:arrived false :reason :unsupported} @out))
          (is (= [0 64 0] (at p)))
          (is (= [:unsupported] (mapv :reason (events-of s :refused)))))))))

(deftest a-cut-walk-leaves-no-controls_and_the-next-round-replans
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:blocks flat})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out {:pos [10 64 0] :range 0})))
              world (.-world p)]
          (core/submit! eng '(recording-parent) {})
          (.hold world "steer")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (takeover/take! eng {:who "claude" :why "cut"})
            (await running))
          (is (= {} (js->clj (.-controls (.-state world)))) "no control is left pressed")
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (tick-out! eng 10))
          (is (= {:arrived true} @out))
          (is (= [10 64 0] (at p)))
          (is (= 1 (count (moved eng))) "the cut walk wrote nothing; the replanned one wrote its entry"))))))
