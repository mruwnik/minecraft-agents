(ns engine.planner-flood-test
  "engine.path.planner-tuned's goal flood (the early pass, options.preFlood, and the late one): a goal it calls :goal-enclosed
  must really be walled in. A shut door, gate or trapdoor the search can open is a way in, and so is a cell the flood cannot
  see (unloaded, or out of the search's span)."
  (:require [cljs.test :refer [deftest is are]]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu :refer [box floor]]))

(defn plan-over
  "Plan from (0 64 0) to goal [x y z] over a fake world spec; {:status :reason}."
  ([spec goal] (plan-over spec goal {}))
  ([spec [x y z] options]
   (let [pw (.pathWorld (tu/fake spec))
         r (planner/plan (.-snapshot pw)
                         #js {:from #js {:x 0 :y 64 :z 0} :goal #js {:kind "near" :x x :y y :z z :range 0}}
                         (js/Object.assign #js {:table (.-table pw) :space (.-space pw)} (clj->js options)))]
     {:status (.-status r) :reason (.-reason r)})))

(def flat (floor -2 -3 40 3))

(defn hollow
  "A stone box x0..x1, y0..y1, z0..z1 with its inside left empty."
  [x0 y0 z0 x1 y1 z1]
  (apply dissoc (box x0 y0 z0 x1 y1 z1 "stone") (keys (box (inc x0) (inc y0) (inc z0) (dec x1) (dec y1) (dec z1) "stone"))))

;; a stone room x 5..9, z -2..2, feet y 64..65 inside, a roof at y 67
(def room (hollow 5 63 -2 9 67 2))

(def hatch
  {:blocks (merge flat (assoc (box 2 66 -1 8 66 1 "stone") "3,66,0" "oak_trapdoor")
                  {"3,64,0" "ladder" "3,65,0" "ladder"} (box 3 64 -1 3 65 -1 "stone"))
   :states {"3,64,0" {:facing "south"} "3,65,0" {:facing "south"} "3,66,0" {:open false :half "bottom" :facing "south"}}})

(def pen
  {:blocks (merge flat (apply dissoc (box 5 64 -2 9 64 2 "oak_fence") (keys (box 6 64 -1 8 64 1 "x"))) {"5,64,0" "oak_fence_gate"})
   :states {"5,64,0" {:open false :facing "east"}}})

(def door-room
  {:blocks (merge flat room {"5,64,0" "oak_door" "5,65,0" "oak_door"})
   :states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}})

(def ladder-deck
  {:blocks (merge flat (dissoc (box 2 66 -1 8 66 1 "stone") "3,66,0")
                  {"3,64,0" "ladder" "3,65,0" "ladder" "3,66,0" "ladder"} (box 3 64 -1 3 66 -1 "stone"))
   :states {"3,64,0" {:facing "south"} "3,65,0" {:facing "south"} "3,66,0" {:facing "south"}}})

(deftest a-goal-behind-something-the-search-opens-is-found
  (are [spec goal] (= {:status "found" :reason nil} (plan-over spec goal))
    hatch [6 67 0]
    pen [7 64 0]
    door-room [7 64 0]
    ladder-deck [6 67 0]))

(deftest a-goal-behind-something-the-search-opens-is-not-enclosed-early
  (are [spec goal] (not= "goal-enclosed" (:reason (plan-over spec goal {:maxNodes 1})))
    hatch [6 67 0]
    pen [7 64 0]
    door-room [7 64 0]))

;; A 2-wide roofed corridor x x0..x0+4, z 0..1, shut at its west end and open at its east one, with no way in from the
;; start's floor: what lies east of it decides whether the goal is walled in.
(defn corridor [x0]
  (let [x1 (+ x0 4)]
    (merge (floor -2 -3 3 3)
           (floor x0 0 x1 1)
           (box x0 64 -1 x1 66 -1 "stone") (box x0 64 2 x1 66 2 "stone")
           (box (dec x0) 64 -1 (dec x0) 66 2 "stone")
           (box x0 66 0 x1 66 1 "stone"))))

;; the fake world loads only the 16-block columns its blocks touch: the corridor's open end at x 15 faces the unloaded
;; column x 16..31
(def into-unloaded {:blocks (corridor 11)})

;; the search's span ends 2048 blocks from the start: x 2048 and on are out of it, a floor carries on there
(def into-out-of-span
  {:blocks (merge (corridor 2043) (floor 2048 -3 2060 4))})

;; the early flood by default, the late one alone with preFlood 0 and floodAfter 0
(deftest a-goal-whose-only-way-out-is-unseen-is-not-enclosed
  (are [spec goal options] (not= "goal-enclosed" (:reason (plan-over spec goal options)))
    into-unloaded [12 64 0] {}
    into-out-of-span [2045 64 0] {}
    into-unloaded [12 64 0] {:preFlood 0 :floodAfter 0}
    into-out-of-span [2045 64 0] {:preFlood 0 :floodAfter 0}
    hatch [6 67 0] {:preFlood 0 :floodAfter 0}))

(deftest a-walled-goal-is-still-enclosed-at-once
  (are [spec goal] (= "goal-enclosed" (:reason (plan-over spec goal {:maxNodes 1})))
    {:blocks (merge flat room)} [7 64 0]
    {:blocks (merge flat room {"5,64,0" "iron_door" "5,65,0" "iron_door"})
     :states {"5,64,0" {:open false :half "lower" :facing "east"} "5,65,0" {:open false :half "upper" :facing "east"}}}
    [7 64 0]
    ;; a one-cell pocket on the edge of the loaded columns: a gap jump from the unloaded z -1 cannot come over its wall
    {:blocks (merge (floor -2 0 11 2) (box 9 64 1 9 65 1 "stone") (box 11 64 1 11 65 1 "stone")
                    (box 10 64 0 10 65 0 "stone") (box 10 64 2 10 65 2 "stone") {"10,66,1" "stone"})}
    [10 64 1]))
