(ns engine.pen-check-test
  "jobs.animals.pen-check against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.build-from-plan-test :as b]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.events :as events]
            [engine.fake.raw-world :as fake-raw]
            [engine.pen-build-test :as pb]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world-files]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.animals.pen-check :as pen-check]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.pen :as pen]))

(defn lcg [seed]
  (let [a (atom (+ 12345 (* 7919 seed)))]
    (fn [] (/ (swap! a #(mod (+ (* % 1103515245) 12345) 2147483648)) 2147483648))))

(def job 'jobs.animals.pen-check)

(def ground
  (into {} (for [x (range -10 16) z (range -10 16)] [(str x ",63," z) "stone"])))

(def fence-ring
  (into {} (for [x (range -1 6) z (range -1 6) :when (or (#{-1 5} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"])))

(def floor-box
  "The walk floor of the seeing bodies: the ground and no more (the default floor is a hundred blocks wide, and the fill walks all of it)."
  [-10 -10 15 15])

(defn world [more & [states]]
  {:floor floor-box
   :self {:pos {:x 2 :y 64 :z 2}}
   :blocks (merge ground fence-ring more)
   :states (or states {})})

(defn ending-event
  "The job's ending event (job.completed or job.stopped) without its nil fields, via pick (default first)."
  ([seen] (ending-event seen first))
  ([seen pick]
   (some->> seen (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %)))) not-empty pick
            (remove (comp nil? val)) (into {}))))

(defn ^:async run-job
  "Submit the job with args in a world and run two ticks; the ending event, or nil."
  [args w]
  (let [s (h/setup w)]
    (core/submit! (:eng s) (list job (merge {:ways #{:check}} args)) {})
    (await (tu/run-ticks (:eng s) (:clock s) 2 700))
    {:event (ending-event @(:seen s))
     :listed (:list (core/state (:eng s)))}))

(deftest a-closed-pen-from-a-cell-is-reported-closed-with-its-size
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event listed]} (await (run-job {:at [2 64 2]} (world {})))]
          (is (= {:closed? true :cells 25 :leaks [] :gates []}
                 (select-keys event [:closed? :reason :cells :leaks :gates])))
          (is (empty? listed)))))))

(deftest an-open-gate-is-a-leak-at-the-gate-and-listed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:at [2 64 2] :settings {:jobs.animals.pen-check/max-cells 100}}
                                              (world {"2,64,-1" "oak_fence_gate"} {"2,64,-1" {:open true}})))]
          (is (false? (:closed? event)))
          (is (= :leak (:reason event)))
          (is (= [{:pos {:x 2 :y 64 :z -1} :why :open-gate}] (:leaks event)))
          (is (= [{:pos {:x 2 :y 64 :z -1} :open? true}] (:gates event))))))))

(deftest a-box-gives-the-exact-crossings
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}}}
                                              (world {"5,64,1" "air"})))]
          (is (false? (:closed? event)))
          (is (= [{:pos {:x 5 :y 64 :z 1} :why :gap}] (:leaks event)))
          (is (= 25 (:cells event))))))))

(deftest a-start-in-the-air-is-no-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:at [2 70 2]} (world {})))]
          (is (= :no-start (:reason event))))))))

(deftest without-a-start-or-a-box-the-job-declines
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {} (world {})))]
          (is (= :no-pen (:reason event))))))))

(deftest animals-are-complete-only-when-every-inside-cell-is-in-view
  (let [checked {:inside #{[1 64 1] [2 64 1]} :remembered [[1 64 1] [9 64 9]]}]
    (is (= {:unseen-cells 1 :animals-complete? false} (pen-check/animal-coverage checked)))
    (is (= {:unseen-cells 0 :animals-complete? true} (pen-check/animal-coverage (assoc checked :remembered []))))))

(deftest the-event-and-result-list-the-animals-seen-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [es [{:id 7 :name "cow" :kind "passive" :pos [2.5 64 2.5]}
                  {:id 8 :name "sheep" :kind "passive" :pos [3.5 64 1.5]}
                  {:id 9 :name "cow" :kind "passive" :pos [8.5 64 2.5]}
                  {:id 10 :name "zombie" :kind "hostile" :pos [1.5 64 1.5]}]
              {:keys [event]} (await (run-job {:at [2 64 2] :checks {:safe? (quote (always))}} (assoc (world {}) :entities es)))]
          (is (= 2 (:animals-in event)))
          (is (= [{:kind "cow" :id 7 :pos [2.5 64 2.5]} {:kind "sheep" :id 8 :pos [3.5 64 1.5]}]
                 (sort-by :id (:animals event)))))))))

(deftest dedupe-leaks-keeps-first-seen-order-past-eight-leaks
  (let [leaks (mapv (fn [i] {:pos {:x i :y 64 :z 0} :why :gap}) (range 12))]
    (is (= leaks (pen/dedupe-leaks (concat leaks leaks))))))

(defn ^:async run-seeing
  "run-job with the body seeing through perception (sight jitter pinned to rand, default 0.5) and no look taken yet; facing yaw (Minecraft degrees), ticks rounds (default 6)."
  [args w yaw & [ticks rand]]
  (let [s (h/setup-seeing (assoc w :yaw yaw :rand (or rand (constantly 0.5))) nil)]
    (core/submit! (:eng s) (list job (merge {:ways #{:check}} args)) {})
    (dotimes [_ (or ticks 6)]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    (ending-event @(:seen s))))

(deftest a-pen-the-body-never-looked-at-is-looked-at-before-the-answer
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [event (await (run-seeing {:at [2 64 2]} (world {}) 0))]
          (is (= {:closed? true :cells 25} (select-keys event [:closed? :reason :cells]))
              "inside the ring: the floor round the feet and the fence behind are looked at, not :no-start"))
        (let [event (await (run-seeing {:at [2 64 2]} (assoc (world {}) :self {:pos {:x 6 :y 64 :z 2}}) 270))]
          (is (= {:closed? true :cells 25} (select-keys event [:closed? :reason :cells]))
              "the ring behind the body, outside it: no false :unloaded or :climb leak"))
        (let [event (await (run-seeing {:at [2 64 2] :settings {:jobs.animals.pen-check/max-cells 100}} (world {"2,64,-1" "air"}) 0))]
          (is (= :leak (:reason event)) "the gap in the fence behind the body is seen")
          (is (some #{{:x 2 :y 64 :z -1}} (map :pos (:leaks event)))))))))

(deftest a-shut-gate-behind-the-body-is-looked-at-not-unloaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [yaw [0 90 180 270]]
          (let [event (await (run-seeing {:at [2 64 2]} (world {"2,64,-1" "oak_fence_gate"} {"2,64,-1" {:open false}}) yaw))]
            (is (= {:closed? true :cells 25} (select-keys event [:closed? :reason :cells])) (str "yaw " yaw))
            (is (= [{:pos {:x 2 :y 64 :z -1} :open? false}] (:gates event)) (str "yaw " yaw))))))))

(deftest a-gate-hidden-behind-a-pillar-is-walked-to-and-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (world {"-1,64,2" "oak_fence_gate" "1,64,2" "stone" "1,65,2" "stone" "1,66,2" "stone"} {"-1,64,2" {:open false}})
              event (await (run-seeing {:at [3 64 2]} (assoc-in w [:self :pos] {:x 3 :y 64 :z 2}) 90 60))]
          (is (= {:closed? true :cells 24} (select-keys event [:closed? :cells])) "the body walks round the pillar to see the gate")
          (is (= [{:pos {:x -1 :y 64 :z 2} :open? false}] (:gates event))))))))

(deftest a-pen-longer-than-three-looks-is-walked-end-to-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [long-ring (into {} (for [x (range -1 41) z (range -1 6) :when (or (#{-1 40} x) (#{-1 5} z))] [(str x ",64," z) "oak_fence"]))
              floor (into {} (for [x (range -10 50) z (range -10 16)] [(str x ",63," z) "stone"]))
              w (assoc (world {}) :blocks (merge floor long-ring) :floor [-10 -10 50 16] :radius 8)
              event (await (run-seeing {:at [2 64 2]} w 90 120))]
          (is (= {:closed? true :cells 200} (select-keys event [:closed? :cells]))
              "each walk to an unseen cell reveals more, so the walks continue while they make progress"))))))

(defn ^:async check-twice
  "Run the job in a seeing body, take a fence cell the body does not see now out of the world (blasted while it looked away),
  and run it again; [cell second-event]."
  [args]
  (let [s (h/setup-seeing (assoc (world {}) :yaw 0) nil)
        run (fn ^:async run [] 
              (core/submit! (:eng s) (list job (merge {:ways #{:check}} args)) {})
              (await (tu/run-ticks (:eng s) (:clock s) 6 700))
              (ending-event @(:seen s) last))
        _ (await (run))
        cell (first (filter #(and (or (<= 0 (% 0) 4) (<= 0 (% 2) 4)) (not (.-visible (.sensedAt (:p s) (clj->js {:x (% 0) :y (% 1) :z (% 2)})))))
                            (keys (fake/cells fence-ring identity))))]
    (swap! (fake/state (:p s)) fake/drop-block cell)
    [cell (await (run))]))

(deftest a-fence-cell-gone-while-out-of-sight-is-found-not-remembered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[cell event] (await (check-twice {:at [2 64 2] :settings {:jobs.animals.pen-check/max-cells 100}}))]
          (is (= :leak (:reason event)))
          (is (some #{{:x (cell 0) :y (cell 1) :z (cell 2)}} (map :pos (:leaks event)))))))))

(deftest a-box-given-as-vectors-is-checked-and-the-leaks-are-named-in-the-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:box {:min [0 64 0] :max [4 64 4]}} (world {"5,64,1" "air"})))]
          (is (= [{:pos {:x 5 :y 64 :z 1} :why :gap}] (:leaks event)))
          (is (= "pen not closed: leak: gap at [5 64 1]" (:text event))))))))

(deftest open-ground-beside-the-fence-top-is-a-perch-in-leak-named-in-the-text
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-job {:box {:min [0 64 0] :max [4 64 4]}} (world {"6,64,2" "stone"})))]
          (is (= :leak (:reason event)))
          (is (= [{:pos {:x 5 :y 64 :z 2} :why :perch-in}] (:leaks event)))
          (is (= "pen not closed: leak: perch-in at [5 64 2]" (:text event))))))))

(deftest a-plan-names-the-pen-by-its-fence-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (b/start (pb/spec [] pb/built-pen) {"pen" (pb/ring-plan)} [])
              result (await (tu/child-outcome eng job {:plan "pen"} 300))]
          (is (true? (:closed? result)))
          (is (= 1 (:cells result))))
        (let [{:keys [eng]} (b/start (pb/spec [] (dissoc pb/built-pen "4,64,3")) {"pen" (pb/ring-plan)} [])
              result (await (tu/child-outcome eng job {:plan "pen"} 300))]
          (is (= :leak (:reason result)))
          (is (= [{:pos {:x 4 :y 64 :z 3} :why :gap}] (:leaks result))))))))

(def l-inside
  "A 3x3 pen at x 3..5, z 3..5 and a lobby of x 3, z 6..7."
  (into #{} cat [(for [x (range 3 6) z (range 3 6)] [x z]) [[3 6] [3 7]]]))

(def l-ring
  "Every cell next to the L (sides and corners) that is not inside it, at y 64."
  (vec (distinct (for [[x z] l-inside dx [-1 0 1] dz [-1 0 1]
                       :let [c [(+ x dx) (+ z dz)]]
                       :when (not (l-inside c))]
                   [(c 0) 64 (c 1)]))))

(deftest a-plan-of-an-l-shaped-pen-is-closed-and-counts-only-the-l
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "l" :parts [{:id "fence" :cells l-ring :want "oak_fence"}]}
              built (into {} (map (fn [[x y z]] [(str x "," y "," z) "oak_fence"])) l-ring)
              {:keys [eng]} (b/start (pb/spec [] built) {"l" plan} [])
              result (await (tu/child-outcome eng job {:plan "l"} 300))]
          (is (true? (:closed? result)) (pr-str (:leaks result)))
          (is (= 11 (:cells result))))
        (let [built (into {} (map (fn [[x y z]] [(str x "," y "," z) "oak_fence"])) (remove #{[3 64 8]} l-ring))
              plan {:id "l" :parts [{:id "fence" :cells l-ring :want "oak_fence"}]}
              {:keys [eng]} (b/start (pb/spec [] built) {"l" plan} [])
              result (await (tu/child-outcome eng job {:plan "l"} 300))]
          (is (= :leak (:reason result)))
          (is (some #{{:x 3 :y 64 :z 8}} (map :pos (:leaks result)))))))))

(defn ring-cells
  "The outline cells of the box [x0 z0] [x1 z1] at y 64."
  [[x0 z0] [x1 z1]]
  (for [x (range x0 (inc x1)) z (range z0 (inc z1)) :when (or (#{x0 x1} x) (#{z0 z1} z))] [x 64 z]))

(deftest a-plan-of-two-rings-and-two-gates-reads-closed-as-a-plan-and-from-a-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [gates [[2 64 4] [2 64 7]]
              plan {:id "p" :parts [{:id "fence" :outline [[0 64 0] [4 64 4]] :want "oak_fence"}
                                    {:id "lobby" :outline [[1 64 4] [3 64 7]] :want "oak_fence"}
                                    {:id "gates" :cells gates :want "oak_fence_gate"}]}
              built (merge (into {} (map (fn [[x y z]] [(str x "," y "," z) "oak_fence"]))
                                 (concat (ring-cells [0 0] [4 4]) (ring-cells [1 4] [3 7])))
                           (into {} (map (fn [[x y z]] [(str x "," y "," z) "oak_fence_gate"])) gates))
              {:keys [eng]} (b/start (pb/spec [] built) {"p" plan} [])
              by-plan (await (tu/child-outcome eng job {:plan "p"} 300))
              by-cell (await (tu/child-outcome eng job {:at [2 64 2]} 300))]
          (is (true? (:closed? by-cell)) (pr-str (:leaks by-cell)))
          (is (true? (:closed? by-plan)) (pr-str (:leaks by-plan))))))))

(deftest an-open-gate-seen-on-the-way-is-no-proof-of-a-leak-while-cells-are-unseen
  (is (false? (pen-check/seen-leak? nil {:leaks [{:pos {:x 1 :y 64 :z 1} :why :open-gate}]} [2 64 2]))))

(deftest a-seen-leak-other-than-an-open-gate-is-proof-while-cells-are-unseen
  (with-redefs [dig-look/unknown? (constantly false)]
    (is (true? (pen-check/seen-leak? nil {:leaks [{:pos {:x 1 :y 64 :z 1} :why :climb}]} [2 64 2])))))

(deftest unseen-cells-under-the-pen-floor-are-never-walked-to
  (let [leak (fn [y] {:pos {:x 1 :y y :z 1} :why :unloaded})]
    (are [ys want] (= want (pen-check/unseen-leak {:leaks (mapv leak ys)} [2 64 2]))
      [60 61] nil
      [60 63] {:x 1 :y 63 :z 1}
      [64 60] {:x 1 :y 64 :z 1})))

(deftest a-plan-alone-is-read-by-a-body-that-has-not-looked-at-the-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [gates [[2 64 4] [2 64 7]]
              blocks (merge ground
                            (into {} (map (fn [[x y z]] [(str x "," y "," z) "oak_fence"]))
                                  (concat (ring-cells [0 0] [4 4]) (ring-cells [1 4] [3 7])))
                            (into {} (map (fn [[x y z]] [(str x "," y "," z) "oak_fence_gate"])) gates))
              clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              raw (tu/fake-on-floor {:self {:pos {:x 2.5 :y 64 :z 2.5}} :blocks blocks :states {}})
              now (tu/act-clock clock raw 0)
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:radius 16 :ray-deg 2 :now now :rand (constantly 0.5)}))
              plan {:id "pen" :parts [{:id "fence" :outline [[0 64 0] [4 64 4]] :want "oak_fence"}
                                      {:id "lobby" :outline [[1 64 4] [3 64 7]] :want "oak_fence"}
                                      {:id "gates" :cells gates :want "oak_fence_gate"}]}
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                                :events (events/make {:body "Fake" :sinks [sink] :now now})
                                :world (world-files/of-data {"pen" plan} {} [])})]
          (core/submit! eng (list job {:plan "pen" :ways #{:check}}) {})
          (dotimes [_ 10]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (= {:closed? true :cells 11} (select-keys (ending-event @seen) [:closed? :reason :cells]))))))))

(def plain
  "Open ground far past the sight radius round the fence ring."
  (into {} (for [x (range -60 66) z (range -60 66)] [(str x ",63," z) "stone"])))

(defn ^:async run-counting
  "Run the job with args in world w on a seeing body until it ends: {:event :looks :fills}, a fill being one read of
  the floor cell ([x y z]) under the start."
  [args w floor]
  (let [s (h/setup-seeing (assoc w :yaw 0) nil)
        p (:p s)
        orig (.-sensedAt p)
        fills (atom 0)]
    (set! (.-sensedAt p) (fn [pos]
                           (when (= floor [(.-x pos) (.-y pos) (.-z pos)]) (swap! fills inc))
                           (orig pos)))
    (core/submit! (:eng s) (list job (merge {:ways #{:check}} args)) {})
    (dotimes [_ 12]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    {:event (ending-event @(:seen s))
     :looks (count (h/calls p "look"))
     :fills @fills}))

(deftest a-leaky-pen-in-open-ground-is-not-filled-again-after-every-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (world {}) :blocks (merge plain fence-ring {"2,64,-1" "air"}) :floor [-60 -60 65 65])
              {:keys [event looks fills]} (await (run-counting {:at [2 64 2]} w [2 63 2]))]
          (is (= :leak (:reason event)))
          (is (some #{{:x 2 :y 64 :z -1}} (map :pos (:leaks event))))
          (is (<= fills (+ 6 looks)) (str fills " fills for " looks " looks")))))))

(deftest a-leak-in-view-is-answered-without-a-look-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup-seeing (assoc (world {"2,64,-1" "air"}) :yaw 180) nil)]
          (perception/pass! (aget (:p s) "perception") {:effort 1})
          (core/submit! (:eng s) (list job {:at [2 64 2]}) {})
          (dotimes [_ 12]
            (swap! (:clock s) + 700)
            (await (core/tick! (:eng s))))
          (is (= :leak (:reason (ending-event @(:seen s)))))
          (is (not-any? #(= :unloaded (:why %)) (:leaks (ending-event @(:seen s)))) "unseen cells past a seen leak are not listed")
          (is (< (count (h/calls (:p s) "look")) 4) "a survey is a dozen looks"))))))

(deftest the-default-time-budget-grows-with-the-pen
  (let [time-of #(get-in (contract/freeze {:defaults (contract/resolve-settings pen-check/defaults (partial settings/get pen-check/settings) %)} % nil)
                         [:spend :job-time])
        box (fn [n] {:min [0 0 0] :max [(dec n) 0 (dec n)]})]
    (is (< 30 (time-of {:at [1 0 1]})) "no box: up to the most cells the fill visits")
    (is (< (time-of {:box (box 3)}) (time-of {:box (box 20)})))
    (is (< (time-of {:box (box 20)}) (time-of {:box (box 200)})) "a big box is capped by ::max-cells")
    (is (< (settings/scoped {:jobs.animals.pen-check/max-cells 50} #(time-of {:at [1 0 1]})) (time-of {:at [1 0 1]})))
    (is (= 5 (time-of {:at [1 0 1] :spend {:job-time 5}})) "a given time stays")))

(defn walled-water
  "A 7 x 7 two-high stone wall (x, z 2..8) with water depth deep inside and a stone pillar at 5 64 5 for the body to stand on."
  [deep]
  (merge (into {} (for [x (range 2 9) z (range 2 9) y [64 65] :when (or (#{2 8} x) (#{2 8} z))] [(str x "," y "," z) "stone"]))
         (into {} (for [x (range 3 8) z (range 3 8) y (range 64 (+ 64 deep))] [(str x "," y "," z) "water"]))
         {"5,64,5" "stone"}))

(deftest a-body-on-a-pillar-in-a-walled-pool-reads-the-pen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [at-pillar (fn [more] (-> (world more) (assoc :blocks (merge ground more)) (assoc-in [:self :pos] {:x 5 :y 65 :z 5})))]
          (is (true? (:closed? (await (run-seeing {:at [5 65 5]} (at-pillar (walled-water 1)) 0))))
              "one deep: the wall holds")
          (is (= {:closed? false :reason :leak}
                 (select-keys (await (run-seeing {:at [5 65 5]} (at-pillar (assoc (walled-water 2) "5,65,5" "air")) 0 30)) [:closed? :reason]))
              "two deep lifts an animal over the wall"))))))

(deftest a-gap-in-the-fence-is-a-leak-though-the-ground-beyond-stays-unseen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [event (await (run-seeing {:at [2 64 2]} (world {"2,64,-1" "air"}) 0))]
          (is (= :leak (:reason event)))
          (is (some #{{:x 2 :y 64 :z -1}} (map :pos (:leaks event)))))))))

(defn ring
  "A pen the way the world fixtures build it: a block ring x, z 2..8 of height high on stone ground (feet y 64), body at the middle."
  [block height]
  (merge ground
         (into {} (for [x (range 2 9) z (range 2 9) y (range 64 (+ 64 height)) :when (or (#{2 8} x) (#{2 8} z))] [(str x "," y "," z) block]))))

(def ring-cases
  [["an open gate in a fence ring" (assoc (ring "oak_fence" 1) "5,64,2" "oak_fence_gate") {"5,64,2" {:open true}} 0 :leak]
   ["a doorway under a lintel, facing it" (-> (ring "stone" 3) (dissoc "5,64,2" "5,65,2")) {} 180 :leak]
   ["a doorway under a lintel, looking away" (-> (ring "stone" 3) (dissoc "5,64,2" "5,65,2")) {} 0 :leak]
   ["a block against a 2-high wall, facing away" (assoc (ring "stone" 2) "3,64,5" "stone") {} 0 :leak]
   ["a block against a 2-high wall, facing it" (assoc (ring "stone" 2) "3,64,5" "stone") {} 90 :leak]])

(deftest a-ring-read-by-sight-from-inside-ends-with-the-leak-not-no-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[why blocks states yaw reason] ring-cases]
          (let [w {:self {:pos {:x 5.5 :y 64 :z 5.5}} :blocks blocks :states states :floor floor-box}
                event (await (run-seeing {:at [5 64 5]} w yaw 10))]
            (is (= reason (:reason event)) why)))))))

(deftest a-pool-one-deep-in-a-two-high-wall-reads-closed-from-a-pillar
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (merge (ring "stone" 2)
                            (into {} (for [x (range 3 8) z (range 3 8)] [(str x ",64," z) "water"]))
                            {"5,64,5" "stone"})
              event (await (run-seeing {:at [5 65 5]} {:self {:pos {:x 5.5 :y 65 :z 5.5}} :blocks blocks :states {} :floor floor-box} 0 10))]
          (is (true? (:closed? event))))))))

(deftest a-body-reading-a-doorway-pen-stays-inside-the-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [yaw [0 180]]
          (let [blocks (-> (ring "stone" 3) (dissoc "5,64,2" "5,65,2"))
                s (h/setup-seeing {:floor floor-box :self {:pos {:x 5.5 :y 64 :z 5.5}} :blocks blocks :states {} :yaw yaw :rand (constantly 0.5)} nil)]
            (core/submit! (:eng s) (list job {:at [5 64 5]}) {})
            (dotimes [_ 10]
              (swap! (:clock s) + 700)
              (await (core/tick! (:eng s))))
            (is (<= 3 (nth (fake/vec-pos (:pos (fake/self (:p s)))) 2)) (str "yaw " yaw))))))))

(deftest unloaded-leaks-are-listed-nearest-the-pen-not-the-body
  (are [args expected] (= expected (pen-check/start-near {:args args}))
    {:box {:min {:x 0 :y 64 :z 0} :max {:x 4 :y 64 :z 4}}} [2 64 2]
    {:at [7 64 3]} [7 64 3]))

(deftest a-pen-seen-from-outside-closes-whatever-the-ray-jitter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [seed [0 35]]
          (let [event (await (run-seeing {:at [2 64 2]} (assoc (world {}) :self {:pos {:x 6 :y 64 :z 2}}) 270 6 (lcg seed)))]
            (is (= {:closed? true :cells 25} (select-keys event [:closed? :reason :cells])) (str "ray jitter seed " seed))))))))

;; Card 3742f975: pen-check makes the pen secure.
(defn ^:async run-patching
  "Run pen-check (patching allowed) in a world carrying 3 oak fences; {:event :p}."
  [args w]
  (let [s (h/setup (merge {:inventory [{:name "oak_fence" :count 3}]} w))]
    (core/submit! (:eng s) (list job args) {})
    (await (tu/run-ticks (:eng s) (:clock s) 10 700))
    {:event (or (ending-event @(:seen s)) {:all (mapv #(select-keys % [:source :kind :text]) @(:seen s))}) :p (:p s)}))

(deftest a-gap-in-the-fence-is-patched-with-the-fence-beside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event p]} (await (run-patching {:at [2 64 2] :settings {:jobs.animals.pen-check/max-cells 100}} (world {"2,64,-1" "air"})))]
          (is (= "oak_fence" (tu/true-block-name p 2 64 -1)))
          (is (= :completed (:kind event)) (pr-str event))
          (is (true? (:closed? event)))
          (is (= 1 (get-in event [:patched :placed]))))))))

(deftest an-open-gate-is-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-patching {:at [2 64 2] :settings {:jobs.animals.pen-check/max-cells 100}} (world {"2,64,-1" "oak_fence_gate"} {"2,64,-1" {:open true :facing "north"}})))]
          (is (true? (:closed? event)) (pr-str event))
          (is (= 1 (get-in event [:patched :shut]))))))))

(deftest a-leak-that-cannot-be-patched-is-reported-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [event]} (await (run-patching {:at [2 64 2] :ways #{:check :patch} :settings {:jobs.animals.pen-check/max-cells 100}} (assoc (world {"2,64,-1" "air"}) :inventory [])))]
          (is (= {:kind :stopped :reason :leak} (select-keys event [:kind :reason])) (pr-str event))
          (is (= :gap (:why (first (:leaks event))))))))))
