(ns engine.go-to-doors-test
  "jobs.movement.go-to through shut gates, doors and trapdoors against the fake world: opened by hand, passed, shut again by
  the :doors policy, and what it leaves in memory."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-support :as gts]
            [engine.go-to-test :as gt]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pass :as pass]
            [jobs.lib.pen :as pen]
            [jobs.lib.world-files :as world]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.cut :as cut]
            [jobs.movement.go-to.faults :as gfaults]))

(defn dark-cells!
  "Perception's dark rule: the cells ([x y z] strings of the world spec) are never made out; sensedAt answers unknown."
  [p cells]
  (let [sensed (.-sensedAt p)
        dark (set (map #(vec (map js/parseInt (.split % ","))) cells))]
    (aset p "sensedAt" (fn [q] (if (dark [(.-x q) (.-y q) (.-z q)]) #js {:unknown true} (sensed q))))))

(defn setup [world zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        _ (dark-cells! p (:dark world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (world/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async tick-out!
  "Tick until the list is empty, at most n ticks, a second apart (a job whose walks get nowhere backs off for one)."
  [eng clock n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (swap! clock + 1000)
      (recur (inc i)))))

(defn ^:async go!
  "Run go-to with args as the child of a recording parent over world (with zones); {:eng :p :seen :out}, out the child's result."
  ([world args] (go! world args []))
  ([world args zones]
   (let [{:keys [eng clock] :as s} (setup world zones)
         out (atom :not-done)
         parent {:round (fn ^:async recording-round [c]
                          (let [e (await (ctx/call c :kid 'jobs.movement.go-to args))]
                            (if (= :continue e) :continue (do (reset! out (gts/result-of e)) (ctx/end c :done nil)))))}
         eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
     (core/submit! eng '(recording-parent) {})
     (await (tick-out! eng clock 40))
     (assoc s :eng eng :out out))))

(defn at [p] (let [pos (.-pos (.self p))] [(.-x pos) (.-y pos) (.-z pos)]))
(defn clicks [p] (count (filterv #(= "useOn" (.-name %)) (.-calls (.-world p)))))
(defn open? [p [x y z]] (:open (js->clj (.-properties (.blockAt p #js {:x x :y y :z z})) :keywordize-keys true)))
(defn opened [eng] (mapv :data (mem/entries (mem/view (:store eng)) :opened)))
(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))

(def flat (floor -2 -3 40 3))
(def gate-cell {:x 5 :y 64 :z 0})
(def gate-wall (assoc (box 5 64 -6 5 64 6 "oak_fence") "5,64,0" "oak_fence_gate")) ; longer than the floor: no free end to slip by

(defn gate-world
  "A fence across the lane at x 5 with a gate at z 0, open or not; the body at x z."
  ([open] (gate-world open {:x 0 :y 64 :z 0}))
  ([open pos]
   {:self {:pos pos} :blocks (merge flat gate-wall) :states {"5,64,0" {:open open :facing "east"}}}))

(deftest through-a-shut-gate-the-walker-opens-passes-and-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (gate-world false) {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [10 64 0] (at p)))
          (is (false? (open? p [5 64 0])) "shut again behind the body")
          (is (= 2 (clicks p)) "one click to open, one to shut")
          (is (= [] (opened eng)) "the :opened entry is cleared once the gate is shut"))))))

(deftest leave-open-leaves-the-gate-open-and-an-opened-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (gate-world false) {:pos [10 64 0] :range 0 :doors :leave-open}))]
          (is (= {:arrived true} @out))
          (is (true? (open? p [5 64 0])))
          (is (= 1 (clicks p)))
          (is (= [gate-cell] (mapv :cell (opened eng))))
          (is (every? some? (mapv (juxt :by :t) (opened eng)))))))))

(deftest never-treats-a-shut-gate-as-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (gate-world false) {:pos [10 64 0] :range 0 :doors :never}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (< (first (at p)) 5) "the body stays on its side")
          (is (zero? (clicks p)))
          (is (= [] (opened eng))))))))

(deftest a-gate-that-was-open-is-walked-through-and-not-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (gate-world true) {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (true? (open? p [5 64 0])))
          (is (zero? (clicks p)))
          (is (= [] (opened eng))))))))

(deftest a-gate-that-will-not-open-gives-door-stuck-after-one-replan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc-in (gate-world false) [:states "5,64,0" :locked] true)
              {:keys [out p eng] :as s} (await (go! w {:pos [10 64 0] :range 0}))]
          (is (= {:arrived false :reason :unreachable :why :door-stuck} (select-keys @out [:arrived :reason :why])))
          (is (< (first (at p)) 5) "the body ends on its own side")
          (is (false? (open? p [5 64 0])))
          (is (= [] (opened eng)) "nothing is left recorded"))))))

;; ------------------------------------------------------------------ a door in a hut wall

(def hut-wall (box 5 64 -3 5 66 3 "stone"))

(defn hut-world [pos]
  {:self {:pos pos}
   :blocks (merge flat hut-wall {"5,64,0" "oak_door" "5,65,0" "oak_door"})
   :states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}})

(deftest a-door-is-passed-both-ways-and-shut-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[from to] [[{:x 0 :y 64 :z 0} [10 64 0]] [{:x 10 :y 64 :z 0} [0 64 0]]]]
          (let [{:keys [out p eng]} (await (go! (hut-world from) {:pos to :range 0}))]
            (is (= {:arrived true} @out) (str from))
            (is (= to (at p)))
            (is (= [false false] (mapv #(open? p %) [[5 64 0] [5 65 0]])) "both halves shut")
            (is (= 2 (clicks p)) "a door is one click to open and one to shut")
            (is (= [] (opened eng)))))))))

(deftest a-door-whose-lower-half-is-hidden-by-the-upper-one-is-opened-by-the-half-in-sight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (assoc (hut-world {:x 0 :y 64 :z 0}) :dark ["5,64,0"]) {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [10 64 0] (at p)))
          (is (= [false false] (mapv #(open? p %) [[5 64 0] [5 65 0]])) "both halves shut again")
          (is (= 2 (clicks p)) "one click to open, one to shut")
          (is (= [] (opened eng))))))))

;; ------------------------------------------------------------------ a fenced pen with one gate

(def pen-blocks
  "A pen, interior x 5..9 z -2..2, fenced one cell out, with its gate in the west fence at z 0."
  (merge (box 4 64 -3 10 64 -3 "oak_fence") (box 4 64 3 10 64 3 "oak_fence")
         (box 4 64 -3 4 64 3 "oak_fence") (box 10 64 -3 10 64 3 "oak_fence")
         {"4,64,0" "oak_fence_gate"}))

(deftest a-goal-inside-a-fenced-pen-is-reached-through-its-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (merge flat pen-blocks)
                 :states {"4,64,0" {:open false :facing "east"}}}
              {:keys [out p]} (await (go! w {:pos [7 64 0] :range 1}))]
          (is (= {:arrived true} (select-keys @out [:arrived])) (pr-str @out))
          (is (false? (open? p [4 64 0])) "the gate is shut again"))))))

(deftest a-goal-above-the-floor-of-a-sunken-pen-is-reached-through-its-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [sunk (merge (box 5 63 -2 9 63 2 "air") (box 5 62 -2 9 62 2 "stone"))
              w {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (merge flat pen-blocks sunk)
                 :states {"4,64,0" {:open false :facing "east"}}}]
          (doseq [goal [[7 64 0] [7 63 0]]]
            (let [{:keys [out]} (await (go! w {:pos goal :range 1}))]
              (is (true? (:arrived @out)) (str goal " " (pr-str @out))))))))))

;; ------------------------------------------------------------------ an airlock of two gates

(def airlock-blocks
  (merge (box 4 64 -6 4 64 6 "oak_fence") (box 8 64 -6 8 64 6 "oak_fence")
         (box 5 64 -1 7 64 -1 "oak_fence") (box 5 64 1 7 64 1 "oak_fence")
         {"4,64,0" "oak_fence_gate" "8,64,0" "oak_fence_gate"}))

(deftest an-airlock-of-two-gates-is-passed-and-both-gates-are-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge flat airlock-blocks)
                 :states {"4,64,0" {:open false :facing "east"} "8,64,0" {:open false :facing "east"}}}
              {:keys [out p eng]} (await (go! w {:pos [12 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [12 64 0] (at p)))
          (is (= [false false] (mapv #(open? p %) [[4 64 0] [8 64 0]])))
          (is (= 4 (clicks p)))
          (is (= [] (opened eng))))))))

;; ------------------------------------------------------------------ a hatch over a ladder

(deftest a-trapdoor-over-a-ladder-is-opened-climbed-through-and-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deck (assoc (box 2 66 -1 8 66 1 "stone") "3,66,0" "oak_trapdoor")
              w {:self {:pos {:x 0 :y 64 :z 0}}
                 :blocks (merge flat deck {"3,64,0" "ladder" "3,65,0" "ladder"} (box 3 64 -1 3 65 -1 "stone"))
                 :states {"3,64,0" {:facing "south"} "3,65,0" {:facing "south"} "3,66,0" {:open false :half "bottom" :facing "south"}}}
              {:keys [out p eng]} (await (go! w {:pos [6 67 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [6 67 0] (at p)))
          (is (false? (open? p [3 66 0])))
          (is (= 2 (clicks p)))
          (is (= [] (opened eng))))))))

(defn hatch-world
  "A roof over the whole floor at y 66 with a trapdoor at x 3 over a two-high ladder (facing south): the only way down;
  the body on the roof."
  [trap-state]
  (let [deck (assoc (box -2 66 -3 40 66 3 "stone") "3,66,0" "oak_trapdoor")]
    {:self {:pos {:x 6 :y 67 :z 0}}
     :blocks (merge flat deck {"3,64,0" "ladder" "3,65,0" "ladder"} (box 3 64 -1 3 65 -1 "stone"))
     :states {"3,64,0" {:facing "south"} "3,65,0" {:facing "south"} "3,66,0" (merge {:half "bottom" :facing "south"} trap-state)}}))

(deftest down-through-a-trapdoor-over-a-ladder-opens-it-descends-and-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (hatch-world {:open false}) {:pos [0 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [0 64 0] (at p)))
          (is (false? (open? p [3 66 0])))
          (is (= 2 (clicks p)))
          (is (= [] (opened eng))))))))

(deftest down-through-an-open-trapdoor-over-a-ladder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! (hatch-world {:open true}) {:pos [0 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= [0 64 0] (at p))))))))

;; ------------------------------------------------------------------ iron

(deftest an-iron-door-is-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:self {:pos {:x 0 :y 64 :z 0}}
                 :blocks (merge flat hut-wall {"5,64,0" "iron_door" "5,65,0" "iron_door"})
                 :states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}}
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0 :ways #{:walk :clear-seal}}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (< (first (at p)) 5) "the body stays on its side")
          (is (zero? (clicks p))))))))

(def iron-doors {"5,64,0" "iron_door" "5,65,0" "iron_door"})
(def iron-states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}})
(def button-cell "4,65,-1")
(def plate-cell "4,64,0")

(defn iron-world
  "Flat land, the hut wall at x 5 with an iron door at z 0, the body at x 0, and extra spec keys."
  [blocks states & {:as spec}]
  (merge {:self {:pos {:x 0 :y 64 :z 0}}
          :blocks (merge flat hut-wall iron-doors blocks)
          :states (merge iron-states states)}
         spec))

(defn button-world [& {:as spec}]
  (apply iron-world {button-cell "stone_button"} {button-cell {:face "wall" :facing "west" :powered false}}
         (mapcat identity spec)))

(deftest an-iron-door-with-a-button-beside-it-opens-and-the-body-passes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! (button-world :wires {button-cell ["5,64,0"]}) {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (> (first (at p)) 5) "through the door")
          (is (= 1 (clicks p)) "one click, on the button"))))))

(deftest an-iron-door-with-a-pressure-plate-in-front-of-it-opens-and-the-body-passes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (iron-world {plate-cell "stone_pressure_plate"} {} :wires {plate-cell ["5,64,0"]})
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (> (first (at p)) 5))
          (is (zero? (clicks p)) "the body's weight is the press"))))))

(deftest a-button-not-wired-to-the-door-leaves-it-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! (button-world) {:pos [10 64 0] :range 0}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (< (first (at p)) 5) "the body stays on its side")
          (is (pos? (clicks p)) "pressed, nothing opened"))))))

(deftest a-button-the-hand-cannot-reach-leaves-the-door-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (button-world :wires {button-cell ["5,64,0"]} :unreachable [button-cell])
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (< (first (at p)) 5)))))))

(deftest a-button-door-that-shuts-itself-later-is-passed-in-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (button-world :wires {button-cell ["5,64,0"]} :pulseMoves 20)
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (> (first (at p)) 5))
          (is (= 1 (clicks p)) "no second press needed"))))))

(deftest a-button-door-that-shuts-before-the-body-is-through-is-pressed-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [pulse [[1 20] [2 20]]]
          (let [w (button-world :wires {button-cell ["5,64,0"]} :pulseMoves pulse)
                {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
            (is (= {:arrived true} @out) (str "pulse " pulse))
            (is (> (first (at p)) 5) (str "pulse " pulse))
            (is (>= (clicks p) 2) (str "pulse " pulse " clicks " (clicks p)))))))))

(deftest a-button-wins-over-a-nearer-lever
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (iron-world {button-cell "stone_button" "4,64,-1" "lever"}
                            {button-cell {:face "wall" :facing "west" :powered false}
                             "4,64,-1" {:face "wall" :facing "west" :powered false}}
                            :wires {button-cell ["5,64,0"]})
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (> (first (at p)) 5)))))))

(def lever-cell "4,65,-1")
(def lever-spec [{lever-cell "lever"} {lever-cell {:face "wall" :facing "west" :powered false}}])

(deftest a-lever-door-is-passed-and-left-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [doors [:shut :leave-open]]
          (let [w (iron-world {lever-cell "lever"} {lever-cell {:face "wall" :facing "west" :powered false}}
                              :wires {lever-cell ["5,64,0"]})
                {:keys [out p eng]} (await (go! w {:pos [10 64 0] :range 0 :doors doors}))]
            (is (= {:arrived true} @out) (str doors))
            (is (> (first (at p)) 5) (str doors))
            (is (= 1 (clicks p)) "one pull, never put back")
            (is (true? (open? p [5 64 0])) "the lever's door stays open")
            (is (= [] (opened eng)) "nothing to shut")))))))

(deftest a-lever-door-is-a-wall-under-doors-never
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (iron-world {lever-cell "lever"} {lever-cell {:face "wall" :facing "west" :powered false}}
                            :wires {lever-cell ["5,64,0"]})
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0 :doors :never :ways #{:walk :clear-seal}}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (zero? (clicks p))))))))

(deftest a-lever-door-is-skipped-for-a-short-walk-around
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (iron-world {lever-cell "lever"} {lever-cell {:face "wall" :facing "west" :powered false}}
                            :wires {lever-cell ["5,64,0"]})
              w (update w :blocks merge (floor -2 -8 40 8) (box 5 64 -3 5 66 3 "stone") iron-doors)
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (zero? (clicks p)) "walked round, no lever pulled"))))))

(def iron-room
  "Shut in at x 0..5, z -3..3 under a roof; the only way out is an iron door at x 5, z 0."
  (merge flat
         (box -1 64 -4 6 67 -4 "stone") (box -1 64 4 6 67 4 "stone") (box -1 64 -4 -1 67 4 "stone")
         (box 5 64 -3 5 66 3 "stone") (box 0 67 -3 5 67 3 "stone")
         {"5,64,0" "iron_door" "5,65,0" "iron_door"}))

(deftest out-of-a-room-shut-by-an-iron-door-says-unreachable-in-words
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [start [2 4]
                args [{:pos [10 64 0] :range 0 :ways #{:walk :clear-seal}} {:pos [10 64 3] :range 2 :ways #{:walk :clear-seal}}]]
          (let [w {:self {:pos {:x start :y 64 :z 0}}
                   :blocks iron-room
                   :states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}}
                {:keys [out p]} (await (go! w args))]
            (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])) (str start args))
            (is (contains? #{:exhausted :start-enclosed :goal-enclosed} (:why @out)) (str start args " " (select-keys @out [:why :text])))
            (is (< (first (at p)) 5) "the body stays inside")))))))

;; ------------------------------------------------------------------ zones

(deftest a-zone-of-another-owner-reaches-one-block-past-its-box
  (let [zone {:name "pen" :min [4 63 -3] :max [6 66 3] :owner "Other"}]
    (are [zones cell expected] (= expected (gfaults/foreign? zones "Fake" cell))
      [zone] [5 64 0] true
      [zone] [7 64 0] true
      [zone] [8 64 0] false
      [zone] [5 64 4] true
      [zone] [5 64 5] false
      [zone] [5 68 0] false
      [(assoc zone :owner "Fake")] [5 64 0] false
      [] [5 64 0] false)))

(def foreign-zone {:name "pen" :min [4 63 -3] :max [6 66 3] :owner "Other" :allow #{}})
(def own-zone (assoc foreign-zone :owner "Fake"))

(deftest a-gate-in-a-foreign-zone-is-shut-even-with-leave-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (gate-world false) {:pos [10 64 0] :range 0 :doors :leave-open} [foreign-zone]))]
          (is (= {:arrived true} @out))
          (is (false? (open? p [5 64 0])))
          (is (= [] (opened eng))))))))

(deftest a-gate-in-the-bodys-own-zone-is-left-open-by-leave-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! (gate-world false) {:pos [10 64 0] :range 0 :doors :leave-open} [own-zone]))]
          (is (= {:arrived true} @out))
          (is (true? (open? p [5 64 0]))))))))

(deftest a-gate-next-to-a-foreign-zone-counts-as-on-its-edge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [edge (assoc foreign-zone :min [6 63 -3] :max [9 66 3])
              {:keys [p]} (await (go! (gate-world false) {:pos [10 64 0] :range 0 :doors :leave-open} [edge]))]
          (is (false? (open? p [5 64 0]))))))))

;; ------------------------------------------------------------------ an animal in the gate cell

(deftest an-animal-in-the-gate-cell-keeps-the-gate-open-with-a-hazard-notice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cow {:id 7 :name "cow" :kind "passive" :pos {:x 5 :y 64 :z 0}}
              {:keys [out p eng] :as s} (await (go! (assoc (gate-world false) :entities [cow]) {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (true? (open? p [5 64 0])) "the animal is not pushed: the gate stays open")
          (is (= 1 (clicks p)))
          (is (= [gate-cell] (mapv :cell (opened eng))) "the entry stays for the trigger")
          (is (= [[5 64 0]] (mapv :cell (events-of s :door-left-open)))))))))

;; ------------------------------------------------------------------ lure food in hand at a gate

(defn bait-world [animal]
  (-> (gate-world false)
      (assoc :entities (when animal [animal]) :inventory [{:name "wheat" :count 4}])
      (assoc-in [:self :held] "wheat")))

(defn held [p] (.-held (.self p)))

(deftest the-lure-food-is-put-away-before-the-walker-opens-a-gate-with-its-animals-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[animal expected] [[{:id 7 :name "cow" :kind "passive" :pos {:x 3 :y 64 :z 2}} nil]
                                   [{:id 8 :name "pig" :kind "passive" :pos {:x 3 :y 64 :z 2}} "wheat"]
                                   [nil "wheat"]]]
          (let [{:keys [out p]} (await (go! (bait-world animal) {:pos [10 64 0] :range 0}))]
            (is (= {:arrived true} @out))
            (is (= expected (held p)) (pr-str animal))))))))

;; ------------------------------------------------------------------ a door on a sill, a step above the ground outside

(defn sill-hut-world
  "A room a step up (floor x 6..12 at y 64, feet y 65) behind the wall x 5, whose oak door at z 0 stands on a stone sill
  at y 64; outside, west of the wall, the feet are at y 64. The door open or shut, facing east or west; the body at pos."
  [pos open facing]
  {:self {:pos pos}
   :blocks (merge flat (box 6 64 -3 12 64 3 "stone") (box 5 64 -3 5 67 3 "stone")
                  {"5,65,0" "oak_door" "5,66,0" "oak_door"})
   :states {"5,65,0" {:open open :half "lower" :facing facing} "5,66,0" {:open open :half "upper" :facing facing}}})

(deftest out-of-and-into-a-room-through-a-door-on-a-sill
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [open [false true]
                facing ["east" "west"]
                [from to] [[{:x 10 :y 65 :z 0} [0 64 0]] [{:x 5 :y 65 :z 0} [0 64 0]] [{:x 0 :y 64 :z 0} [10 65 0]]]]
          (let [{:keys [out p]} (await (go! (sill-hut-world from open facing) {:pos to :range 0}))]
            (is (= {:arrived true} @out) (str open " " facing " " from))
            (is (= to (at p)) (str open " " facing " " from))
            (is (= [open open] (mapv #(open? p %) [[5 65 0] [5 66 0]])) "the door is left as it was")))))))

(deftest a-button-the-body-cannot-make-out-in-the-dark-is-still-pressed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! (button-world :wires {button-cell ["5,64,0"]} :dark [button-cell])
                                          {:pos [10 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= 1 (clicks p)) "the planned button is pressed"))))))

(deftest the-body-turns-to-the-door-after-pressing-its-button
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (go! (button-world :wires {button-cell ["5,64,0"]}) {:pos [10 64 0] :range 0}))
              names (mapv #(.-name %) (.-calls (.-world p)))
              press (.indexOf names "useOn")
              looks (keep-indexed (fn [i call] (when (and (> i press) (= "look" (.-name call))
                                                          (= 5.5 (.-x (.-pos (.-args call)))))
                                                 i))
                                  (.-calls (.-world p)))]
          (is (seq looks) "a look at the door's cell after the press"))))))

;; ------------------------------------------------------------------ a goal whose range takes in the doorway

(deftest a-walk-whose-range-takes-in-the-doorway-ends-past-the-door-and-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng]} (await (go! (hut-world {:x 0 :y 64 :z 0}) {:pos [7 64 0] :range 2}))]
          (is (= {:arrived true} @out))
          (is (not= 5 (js/Math.floor (first (at p)))) "the walk does not end in the doorway")
          (is (= [false false] (mapv #(open? p %) [[5 64 0] [5 65 0]])) "both halves shut")
          (is (= [] (opened eng))))))))

(deftest a-walk-to-the-doorway-itself-arrives-there
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! (hut-world {:x 0 :y 64 :z 0}) {:pos [5 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (= 5 (js/Math.floor (first (at p)))) "no other stand is in range: the body stays in the doorway"))))))

(deftest leave-open-ends-in-the-doorway-its-range-takes-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (go! (hut-world {:x 0 :y 64 :z 0}) {:pos [7 64 0] :range 2 :doors :leave-open}))]
          (is (= {:arrived true} @out))
          (is (= 5 (js/Math.floor (first (at p)))) "no step on: the door stays open anyway")
          (is (= [true true] (mapv #(open? p %) [[5 64 0] [5 65 0]]))))))))

;; ------------------------------------------------------------------ a stand on a vine

(deftest a-body-hanging-on-a-vine-over-a-drop-is-no-arrival-within-range-it-walks-on-to-a-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                 :blocks (-> flat (dissoc "0,63,0") (assoc "0,64,0" "vine" "0,65,0" "vine"))}
              {:keys [out p]} (await (go! w {:pos [2 64 0] :range 3}))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (not= "vine" (.-name (.blockAt p #js {:x (js/Math.floor (first (at p))) :y (js/Math.floor (second (at p))) :z 0})))
              (pr-str (at p))))))))

(deftest a-vine-is-the-only-stand-within-range-so-the-walk-ends-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                 :blocks (-> (floor 5 -3 40 3) (assoc "0,64,0" "vine" "0,65,0" "vine"))}
              {:keys [out]} (await (go! w {:pos [1 64 0] :range 2}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])) (pr-str @out)))))))

(def seal-cells
  "A 3x3x3 dirt box round the goal (10 64 0) (10 65 0) the body placed: all its cells are in the scaffold ledger."
  (vec (for [x [9 10 11] y [64 65 66] z [-1 0 1]
             :when (not (and (= [10 0] [x z]) (#{64 65} y)))]
         [x y z])))

(deftest a-goal-boxed-in-by-the-bodys-own-placed-blocks-has-them-taken-back-then-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label entries arrives?]
                [["own seal in the ledger" (mapv (fn [cell] {:cell cell :item "dirt" :before "air" :job "j9" :purpose :shelter :state :placed}) seal-cells) true]
                 ["same blocks, not the body's" [] false]]]
          (let [world {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge flat (zipmap (map #(clojure.string/join "," %) seal-cells) (repeat "dirt")))}
                {:keys [eng clock] :as s} (setup world [])
                out (atom :not-done)
                parent {:round (fn ^:async sealed-round [c]
                                 (let [e (await (ctx/call c :kid 'jobs.movement.go-to {:pos [10 64 0] :range 0 :ways #{:walk :clear-seal}}))]
                                   (if (= :continue e) :continue (do (reset! out (gts/result-of e)) (ctx/end c :done nil)))))}
                eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
            (ledger/write! (:store eng) entries)
            (core/submit! eng '(recording-parent) {})
            (await (tick-out! eng clock 60))
            (is (= arrives? (boolean (:arrived @out))) (pr-str label @out))))))))

(def body-seal-cells
  "A 3x3x3 dirt box round the body (0 64 0) (0 65 0) it placed: a retreat's seal."
  (vec (for [x [-1 0 1] y [64 65 66] z [-1 0 1]
             :when (not (and (= [0 0] [x z]) (#{64 65} y)))]
         [x y z])))

(deftest a-body-shut-in-by-its-own-seal-takes-it-back-then-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label entries arrives?]
                [["own seal in the ledger" (mapv (fn [cell] {:cell cell :item "dirt" :before "air" :job "j9" :purpose :seal :state :placed}) body-seal-cells) true]
                 ["same blocks, not the body's" [] false]]]
          (let [world {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (merge flat (zipmap (map #(clojure.string/join "," %) body-seal-cells) (repeat "dirt")))}
                {:keys [eng clock]} (setup world [])
                out (atom :not-done)
                parent {:round (fn ^:async sealed-round [c]
                                 (let [e (await (ctx/call c :kid 'jobs.movement.go-to {:pos [10 64 0] :range 0 :ways #{:walk :clear-seal}}))]
                                   (if (= :continue e) :continue (do (reset! out (gts/result-of e)) (ctx/end c :done nil)))))}
                eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
            (ledger/write! (:store eng) entries)
            (core/submit! eng '(recording-parent) {})
            (await (tick-out! eng clock 60))
            (is (= arrives? (boolean (:arrived @out))) (pr-str label (select-keys @out [:arrived :why])))))))))

(deftest a-dirt-plug-of-the-bodys-own-in-a-wall-across-the-way-is-taken-back-near-the-goal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label entries arrives?]
                [["plug in the ledger" (mapv (fn [cell] {:cell cell :item "dirt" :before "air" :job "j9" :purpose :shelter :state :placed}) [[9 64 0] [9 65 0]]) true]
                 ["same plug, not the body's" [] false]]]
          (let [wall (-> (box 9 64 -3 9 67 3 "stone") (assoc "9,64,0" "dirt" "9,65,0" "dirt"))
                world {:self {:pos {:x 0 :y 64 :z 0}} :blocks (merge flat wall)}
                {:keys [eng clock] :as s} (setup world [])
                out (atom :not-done)
                parent {:round (fn ^:async plug-round [c]
                                 (let [e (await (ctx/call c :kid 'jobs.movement.go-to {:pos [10 64 0] :range 0 :ways #{:walk :clear-seal}}))]
                                   (if (= :continue e) :continue (do (reset! out (gts/result-of e)) (ctx/end c :done nil)))))}
                eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
            (ledger/write! (:store eng) entries)
            (core/submit! eng '(recording-parent) {})
            (await (tick-out! eng clock 60))
            (is (= arrives? (boolean (:arrived @out))) (pr-str label (select-keys @out [:arrived :why])))))))))

;; ------------------------------------------------------------------ a pen holding animals

(def pen-world
  "A fence pen x 5..9, z -2..2 with a gate at 5,64,0 (open or not) and a cow and a sheep in it; the body at x."
  (let [stone (box 5 63 -2 9 63 2 "stone")
        fence (merge (box 5 64 -2 9 64 -2 "oak_fence") (box 5 64 2 9 64 2 "oak_fence")
                     (box 5 64 -2 5 64 2 "oak_fence") (box 9 64 -2 9 64 2 "oak_fence"))]
    (fn [open x]
      {:self {:pos {:x x :y 64 :z 0}} :blocks (merge flat stone fence {"5,64,0" "oak_fence_gate"})
       :states {"5,64,0" {:open open :facing "east"}}
       :entities [{:id 7 :name "cow" :kind "passive" :pos {:x 7.5 :y 64 :z 0.5}}
                  {:id 8 :name "sheep" :kind "passive" :pos {:x 7.5 :y 64 :z 1.5}}]})))

(deftest a-walk-through-a-crowded-pens-gate-shuts-it-behind-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng seen]} (await (go! (pen-world false 0) {:pos [7 64 0] :range 0}))]
          (is (= {:arrived true} @out))
          (is (false? (open? p [5 64 0])) "shut again as soon as the body is clear of the gate cell")
          (is (empty? (opened eng))))))))

(deftest a-walk-away-from-a-pen-whose-gate-stands-open-with-animals-in-it-warns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (await (go! (pen-world true 3) {:pos [20 64 -3] :range 0}))
              near (await (go! (pen-world true 3) {:pos [3 64 0] :range 0}))
              shut (await (go! (pen-world false 3) {:pos [20 64 -3] :range 0}))]
          (is (= 1 (count (events-of far :go-to.pen-gate-open))))
          (is (re-find #"\[5 64 0\]" (str (:text (first (events-of far :go-to.pen-gate-open))))))
          (is (empty? (events-of near :go-to.pen-gate-open)) "the goal is by the gate")
          (is (empty? (events-of shut :go-to.pen-gate-open)) "a shut gate"))))))

(deftest a-leave-open-walk-away-from-an-open-pen-gate-does-not-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (go! (pen-world true 3) {:pos [20 64 -3] :range 0 :doors :leave-open}))]
          (is (empty? (events-of r :go-to.pen-gate-open))))))))

(deftest pens-are-checked-once-a-call-however-many-attempts-it-starts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [checks (atom 0)
              check pen/check
              {:keys [eng clock seen]} (setup (pen-world true 3) [])
              pos {:x 20 :y 64 :z -3}
              twice {:round (fn ^:async twice-round [c]
                              (await (cut/start-attempt! c pos))
                              (await (cut/start-attempt! c pos))
                              (ctx/end c :done nil))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'twice twice))]
          (with-redefs [pen/check (fn [o] (swap! checks inc) (check o))]
            (core/submit! eng '(twice) {:range 0})
            (await (tick-out! eng clock 5))
            (is (= 1 @checks) "the second attempt of the call checks no pen")
            (is (= 1 (count (filter #(= :go-to.pen-gate-open (:kind %)) @seen))))))))))

(deftest a-gate-the-call-opened-is-checked-at-its-next-attempt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock seen]} (setup (pen-world false 3) [])
              pos {:x 20 :y 64 :z -3}
              cut-at-gate {:round (fn ^:async cut-at-gate-round [c]
                                    (await (cut/start-attempt! c pos))
                                    (await (pass/open-block! c gate-cell true))
                                    (await (cut/start-attempt! c pos))
                                    (ctx/end c :done nil))}
              eng (assoc eng :jobs (assoc (:jobs eng) 'cut-at-gate cut-at-gate))]
          (core/submit! eng '(cut-at-gate) {:range 0})
          (await (tick-out! eng clock 5))
          (is (= 1 (count (filter #(= :go-to.pen-gate-open (:kind %)) @seen)))
              "opened after the first check, left open when the attempt was cut"))))))

(deftest pens-are-checked-only-when-an-open-gate-is-near-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [checks (atom 0)
              check pen/check]
          (with-redefs [pen/check (fn [o] (swap! checks inc) (check o))]
            (await (go! (pen-world false 3) {:pos [20 64 -3] :range 0}))
            (is (zero? @checks) "a shut gate: no pen check")
            (await (go! (pen-world true 3) {:pos [3 64 0] :range 0}))
            (is (zero? @checks) "an open gate the goal is by: no pen check")
            (await (go! (pen-world true 3) {:pos [20 64 -3] :range 0}))
            (is (pos? @checks) "an open gate near the start")))))))
