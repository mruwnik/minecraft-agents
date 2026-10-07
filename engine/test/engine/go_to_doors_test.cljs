(ns engine.go-to-doors-test
  "jobs.movement.go-to through shut gates, doors and trapdoors against the fake world: opened by hand, passed, shut again by
  the :doors policy, and what it leaves in memory."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.movement.go-to :as go-to]))

(defn setup [world zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
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
         parent {:check (constantly true)
                 :round (fn ^:async recording-round [c]
                          (let [r (await (ctx/call-child c :kid 'jobs.movement.go-to args))]
                            (when (= :done r) (reset! out (ctx/child-result c :kid)))
                            r))}
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
          (is (= [] (opened eng)) "nothing is left recorded")
          (is (= [:door-stuck] (mapv :why (events-of s :unreachable)))))))))

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
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
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

(deftest a-lever-door-stays-a-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (iron-world {"4,65,-1" "lever"} {"4,65,-1" {:face "wall" :facing "west" :powered false}}
                            :wires {"4,65,-1" ["5,64,0"]})
              {:keys [out p]} (await (go! w {:pos [10 64 0] :range 0}))]
          (is (= {:arrived false :reason :unreachable} (select-keys @out [:arrived :reason])))
          (is (zero? (clicks p))))))))

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
                args [{:pos [10 64 0] :range 0} {:pos [10 64 0] :range 0 :escalate false} {:pos [10 64 3] :range 2 :escalate false}]]
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
    (are [zones cell expected] (= expected (go-to/foreign? zones "Fake" cell))
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
