(ns engine.go-to-escalate-test
  "jobs.movement.go-to escalation (pillar, stair, clear-path, a walk to a wall) and jobs.lib.escape, against the fake
  world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.lib.escape :as escape]
            [jobs.movement.go-to.escalation :as esc]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]))

(defn setup
  ([world] (setup world nil))
  ([world zones]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p (tu/fake world)
         eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                                   :backoff false
                                   :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}
                            zones (assoc :world (ew/of-data {} {} zones))))]
     {:eng eng :p p :seen seen})))

(defn recording-parent
  "A parent that runs job with args as its child and keeps the child's result in out."
  [out job args]
  {:check (constantly true)
   :round (fn ^:async recording-round [c]
            (let [r (await (ctx/call-child c :kid job args))]
              (when (= :done r) (reset! out (ctx/child-result c :kid)))
              r))})

(defn ^:async tick-out! [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async run-job!
  "Run job (default go-to) with args as a child over world (zones: a zone list); {:eng :p :seen :out}."
  ([world args] (run-job! world 'jobs.movement.go-to args))
  ([world job args] (run-job! world job args nil))
  ([world job args zones]
   (let [{:keys [eng] :as s} (setup world zones)
         out (atom :not-done)
         eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out job args)))]
     (core/submit! eng '(recording-parent) {})
     (let [n (await (tick-out! eng 1000))]
       (assoc s :eng eng :out out :ticks n)))))

(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))

(defn block [p cell] (get-in @(fake/state p) [:blocks cell] "air"))

(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(def pit-cells #{"0,61,0" "0,62,0" "0,63,0"})

(def pit
  "A 3-deep 1x1 pit (feet at y 61) in a dirt block x -2..2, y 60..63, z -2..2; stone ground 4 deep east of it (x 3..14)
  whose top is at the pit's rim (feet y 64)."
  (apply dissoc (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "dirt")) pit-cells))

(def stone-pit
  "The pit with stone walls (a pickaxe block)."
  (apply dissoc (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "stone")) pit-cells))

(def in-pit {:self {:pos {:x 0 :y 61 :z 0}}})

(def room
  "A sealed 5x5 dirt room (inside x -2..2, z -2..2, feet y 64; walls one thick at x/z +-3, y 64..65; roof y 66) on a
  stone floor x -6..12, z -6..6."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 66 3 "dirt") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(def in-room {:self {:pos {:x 0 :y 64 :z 0}}})

(def hollow
  "A 2-deep 20x20 hollow in solid dirt (open x 3..22, z -10..9, y 65..66; floor y 64; top of the dirt at y 66, feet
  on top at y 67)."
  (apply dissoc
         (box 0 60 -13 25 66 12 "dirt")
         (for [x (range 3 23) z (range -10 10) y [65 66]] (str x "," y "," z))))

;; ------------------------------------------------------------------ escape (pure reads)

(defn ba [world] (fn [[x y z]] (get (:blocks world) (str x "," y "," z) "air")))

(deftest escape-pit-depth-counts-the-walled-levels
  (is (= 3 (escape/pit-depth (ba {:blocks pit}) [0 61 0])))
  (is (= 0 (escape/pit-depth (ba {:blocks room}) [0 64 0])) "the middle of a room is no pit"))

(deftest escape-door-is-the-solid-cells-of-a-thin-wall-with-floor-beyond
  (is (= {:cells [[3 65 0] [3 64 0]] :through [4 64 0]} (escape/door (ba {:blocks room}) [2 64 0] [1 0] 3)))
  (is (nil? (escape/door (ba {:blocks room}) [1 64 0] [1 0] 3)) "no wall right in front")
  (is (nil? (escape/door (ba {:blocks pit}) [0 61 0] [1 0] 3)) "no floor beyond the pit's wall"))

(deftest escape-door-never-goes-through-a-door-a-bed-or-a-container
  (doseq [b ["iron_door" "oak_door" "spruce_fence_gate" "red_bed" "chest" "barrel" "bedrock"]]
    (is (nil? (escape/door (ba {:blocks (assoc room "3,64,0" b)}) [2 64 0] [1 0] 3)) b)))

(deftest escape-door-is-at-most-max-thick
  (let [thick (merge room (box 3 64 -3 6 65 3 "dirt"))]
    (is (nil? (escape/door (ba {:blocks thick}) [2 64 0] [1 0] 3)) "4 thick")
    (is (= [7 64 0] (:through (escape/door (ba {:blocks thick}) [2 64 0] [1 0] 4))))))

(deftest escape-choose-walks-to-a-wall-first-from-the-middle-of-a-room
  (is (= {:step :approach :pos [2 64 0]} (escape/choose (tu/fake (merge in-room {:blocks room})) [0 64 0] [8 64 0]))
      "the nearest cell with the wall toward the goal beside it"))

(deftest escape-choose-pillars-only-with-enough-blocks
  (is (= {:step :pillar :height 3 :item "dirt"}
         (escape/choose (tu/fake (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 3}]})) [0 61 0] [10 64 0])))
  (is (= {:step :stair :heading :east :steps 3}
         (escape/choose (tu/fake (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})) [0 61 0] [10 64 0]))))

;; ------------------------------------------------------------------ go-to escalation

(deftest go-to-pillars-out-of-a-pit-when-it-carries-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]})
                                                {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:pillar] (mapv :step (events-of seen :go-to.escalated))))
          (is (= 3 (count (calls p "jumpPlace"))) "one block per pillar round")
          (is (empty? (calls p "dig"))))))))

(deftest go-to-stairs-out-of-a-pit-and-puts-back-what-it-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                {:pos [10 64 0] :range 1}))
              restored (first (events-of seen :go-to.restored))
              skipped (first (events-of seen :go-to.restore-skipped))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))) "2 blocks are too few for a 3-deep pillar")
          (is (= [[1 62 0] [1 63 0]] (:cells restored)) "the two blocks carried go back, lowest first")
          (is (= "dirt" (block p [1 62 0])))
          (is (= "dirt" (block p [1 63 0])))
          (is (= [[2 63 0]] (mapv :cell (:cells skipped))) "nothing left to put back the third"))))))

(def deep-pit
  "A 5-deep 1x1 pit (feet at y 59) in dirt x -2..2; stone ground east of it (x 3..14) topped at the rim (feet y 64)."
  (apply dissoc (merge (box 3 56 -3 14 63 3 "stone") (box -2 56 -2 2 63 2 "dirt")) (for [y (range 59 64)] (str "0," y ",0"))))

(deftest go-to-does-not-walk-back-down-its-stair-to-put-it-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! {:self {:pos {:x 0 :y 59 :z 0}} :blocks deep-pit
                                                     :inventory [{:name "dirt" :count 2}]}
                                                {:pos [10 64 0] :range 1}))
              skipped (:cells (first (events-of seen :go-to.restore-skipped)))
              low (set (mapv :cell skipped))]
          (is (= {:arrived true} @out))
          (is (>= (second (feet p)) 64) "the body stays up on the rim")
          (is (seq low) "the cells deep below the rim are left for restore-broken")
          (is (some #(= :away (:why %)) skipped) "cells the body would have to go down the shaft for are left")
          (is (every? #(not= :away (:why %)) (remove #(> (- 64 (second (:cell %))) 2) skipped)) "only deep ones")
          (is (every? #(= "air" (block p %)) low)))))))

(deftest go-to-clears-a-door-out-of-a-sealed-room-and-shuts-it-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-room {:blocks room :inventory [{:name "dirt" :count 2}]})
                                                {:pos [8 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))) "the failed walks took it to the east wall")
          (is (empty? (events-of seen :go-to.restore-skipped)))
          (is (= [[3 64 0] [3 65 0]] (:cells (first (events-of seen :go-to.restored)))))
          (is (= "dirt" (block p [3 64 0])) "the door is shut again")
          (is (= "dirt" (block p [3 65 0])))
          (is (< 6 (first (feet p)))))))))

(deftest go-to-walks-to-a-wall-and-stairs-out-of-a-wide-hollow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 12 :y 65 :z 0}} :blocks hollow}
                                              {:pos [24 67 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))) "the failed walks took it to the east wall"))))))

(deftest go-to-escalates-and-arrives-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[what world pos] [[:pillar (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]}) [10 64 0]]
                                  [:stair (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]}) [10 64 0]]
                                  [:clear-path (merge in-room {:blocks room}) [8 64 0]]]]
          (let [{:keys [out seen ticks]} (await (run-job! world {:pos pos :range 1}))]
            (is (= {:arrived true} @out) what)
            (is (= [what] (mapv :step (events-of seen :go-to.escalated))) what)
            (is (= 1 ticks) (str what ": one go-to call, the escalation and put-back inside it"))))))))

(deftest go-to-with-escalate-false-gives-up-as-before
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]})
                                                {:pos [10 64 0] :range 1 :escalate false}))]
          (is (= false (:arrived @out)))
          (is (= :unreachable (:reason @out)))
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "jumpPlace")))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-gives-up-with-the-escalation-s-wait-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run-job! (merge in-pit {:blocks stone-pit}) {:pos [10 64 0] :range 1}))]
          (is (= false (:arrived @out)))
          (is (= {:step :stair :reason :no-tool} (select-keys (:escalation @out) [:step :reason]))
              "stone walls, no pickaxe, no blocks: the stair waits for a pickaxe, and go-to says so")
          (is (= :stopped (:status @out)))
          (is (= "gave up walking to [10 64 0]: shut in here; could not dig a stair out: no pickaxe" (:text @out)))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-on-open-ground-never-escalates
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tower (merge (box -20 63 -20 20 63 20 "stone") (box 8 64 0 8 68 0 "stone"))
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0 :y 64 :z 0}} :blocks tower
                                                 :inventory [{:name "dirt" :count 9}]}
                                                {:pos [8 69 0] :range 0}))]
          (is (= false (:arrived @out)) "the top of a 5-high stone column cannot be walked to")
          (is (empty? (events-of seen :go-to.escalated)) "not shut in: go-to does not change the world")
          (is (empty? (calls p "jumpPlace"))))))))

(deftest a-dig-s-walk-does-not-escalate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "10,64,0" "dirt")
                                                               :inventory [{:name "dirt" :count 5}]})
                                                'jobs.blocks.dig {:pos [10 64 0]}))]
          (is (= :not-done @out) "the dig waits: it cannot reach the cell")
          (is (empty? (events-of seen :go-to.escalated)) "its go-to child runs with :escalate false")
          (is (empty? (calls p "jumpPlace"))))))))

(deftest go-to-sealed-in-its-own-shelter-does-not-dig-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cell (merge (box -6 63 -6 12 63 6 "stone") (dissoc (box -1 64 -1 1 66 1 "dirt") "0,64,0" "0,65,0"))
              {:keys [eng p seen]} (setup {:self {:pos {:x 0 :y 64 :z 0}} :blocks cell :inventory [{:name "dirt" :count 5}]})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent
                                          (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 1})))]
          (mem/write! (:store eng) :shelter {:pos {:x 0 :y 64 :z 0} :roof {:x 0 :y 66 :z 0} :state :built}
                      {:cap 10 :ttl 86400000})
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 300))
          (is (= false (:arrived @out)))
          (is (= {:x 0 :y 64 :z 0} (:inside-own-shelter @out)) "the give-up names the shelter")
          (is (empty? (events-of seen :go-to.escalated)) "a body sealed in its own shelter stays in it")
          (is (empty? (calls p "dig"))))))))

(def deep-shaft
  "A 1x1 shaft 30 deep (feet at y 34) in solid dirt x -30..30, y 30..63, z -30..30."
  (apply dissoc (box -30 30 -30 30 63 30 "dirt") (for [y (range 34 64)] (str "0," y ",0"))))

(deftest go-to-escalates-at-most-three-times
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; each stair climbs at most 8: three of them leave the body 6 short of the rim
        (let [{:keys [out seen]} (await (run-job! {:self {:pos {:x 0 :y 34 :z 0}} :blocks deep-shaft}
                                              {:pos [10 64 0] :range 1}))]
          (is (= [:stair :stair :stair] (mapv :step (events-of seen :go-to.escalated))))
          (is (= false (:arrived @out)))
          (is (= {:step :spent :n 3} (:escalation @out))))))))

;; ------------------------------------------------------------------ review fixes (card 30615854, be7e5bb6)

(defn ledger [eng] (mapv :data (mem/entries (mem/view (:store eng)) :tidy)))

(defn holes [eng] (set (map :cell (filter :escalation (ledger eng)))))

(defn dug-cells [p] (set (map #(mapv (js->clj (.-pos (.-args %)) :keywordize-keys true) [:x :y :z]) (calls p "dig"))))

(deftest escape-natural-is-a-whitelist-of-terrain
  (doseq [n ["stone" "dirt" "grass_block" "deepslate" "gravel" "sand" "netherrack" "iron_ore" "deepslate_diamond_ore"
             "red_terracotta" "andesite" "tuff" "oak_leaves" "mangrove_roots" "ice" "packed_ice" "blue_ice" "powder_snow"
             "magma_block" "pointed_dripstone" "amethyst_block" "suspicious_sand" "crimson_nylium" "warped_wart_block"
             "ancient_debris" "sculk" "infested_stone"]]
    (is (escape/natural? n) n))
  (doseq [n ["oak_planks" "bricks" "stone_bricks" "glass" "white_wool" "cobblestone" "oak_door" "iron_door" "oak_log" "glowstone" "cobbled_deepslate" "dirt_path"
             "oak_fence" "spruce_fence_gate" "white_glazed_terracotta" "smooth_stone" "polished_andesite" nil]]
    (is (not (escape/natural? n)) (pr-str n))))

(deftest escape-stair-cuts-are-each-step-s-three-cells
  (is (= [[0 63 0] [1 63 0] [1 62 0] [1 64 0] [2 64 0] [2 63 0]] (escape/stair-cuts [0 61 0] [1 0] 2))))

(deftest escape-choose-never-digs-built-blocks
  (doseq [wall ["oak_planks" "bricks" "glass" "white_wool" "cobblestone" "stone_bricks"]]
    (let [built (apply dissoc (merge room (box -3 64 -3 3 65 3 wall)) (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))]
      (is (= {:step :none :why :no-dig} (escape/choose (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks built})
                                                       [2 64 0] [8 64 0]))
          wall))))

(deftest escape-choose-prefers-a-stair-to-a-clear-path
  (let [hill (merge room (box 3 64 -3 8 66 3 "dirt"))]
    (is (= :stair (:step (escape/choose (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks hill}) [2 64 0] [8 67 0]))))))

(deftest escape-choose-stair-checks-every-step
  (let [ba-pit (assoc pit "2,63,0" "bricks")]
    (is (not= :east (:heading (escape/choose (tu/fake (merge in-pit {:blocks ba-pit})) [0 61 0] [10 64 0])))
        "the second step east would cut bricks: another heading")))

(deftest go-to-does-not-dig-through-a-built-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [wall ["oak_planks" "bricks" "glass"]]
          (let [built (apply dissoc (merge room (box -3 64 -3 3 65 3 wall))
                             (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))
                {:keys [out p seen]} (await (run-job! (merge in-room {:blocks built :inventory [{:name "dirt" :count 9}]})
                                                  {:pos [8 64 0] :range 1}))]
            (is (= false (:arrived @out)) wall)
            (is (= {:step :none :why :no-dig} (:escalation @out)) wall)
            (is (empty? (events-of seen :go-to.escalated)) wall)
            (is (empty? (calls p "dig")) wall)))))))

(deftest go-to-never-digs-in-another-s-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "vault" :owner "Miles" :min [-3 60 -3] :max [3 70 3]}]
              {:keys [out p seen]} (await (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to
                                                {:pos [8 64 0] :range 1} zones))]
          (is (= false (:arrived @out)))
          (is (= {:step :none :why :zone} (:escalation @out)) "the wall is another owner's")
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-roofed-pit-stairs-through-the-roof-rather-than-pillar
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "0,63,0" "dirt")
                                                                   :inventory [{:name "dirt" :count 5}]})
                                                {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (empty? (calls p "jumpPlace")) "the roof leaves no headroom to pillar")
          (is (= [:stair] (mapv :step (events-of seen :go-to.escalated))))
          (is (contains? (dug-cells p) [0 63 0]) "the stair's first cut is the roof"))))))

(deftest go-to-roofed-pit-with-no-way-to-dig-says-no-headroom
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walls (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "bricks"))
              roofed (assoc (apply dissoc walls pit-cells) "0,63,0" "dirt")
              {:keys [out p seen]} (await (run-job! (merge in-pit {:blocks roofed :inventory [{:name "dirt" :count 5}]})
                                                {:pos [10 64 0] :range 1}))]
          (is (= false (:arrived @out)))
          (is (= {:step :none :why :no-headroom} (:escalation @out)))
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "jumpPlace")))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-roofed-pit-in-another-s-zone-names-the-zone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "vault" :owner "Miles" :min [-3 60 -3] :max [3 70 3]}]
              {:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "0,63,0" "dirt")
                                                                   :inventory [{:name "dirt" :count 5}]})
                                                'jobs.movement.go-to {:pos [10 64 0] :range 1} zones))]
          (is (= false (:arrived @out)))
          (is (= {:step :none :why :zone} (:escalation @out)))
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-never-refills-a-stair-while-the-body-is-still-shut-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng out p seen]} (await (run-job! {:self {:pos {:x 0 :y 34 :z 0}} :blocks deep-shaft
                                                         :inventory [{:name "dirt" :count 64}]}
                                                    {:pos [10 64 0] :range 1}))]
          (is (= false (:arrived @out)))
          (is (empty? (calls p "place")) "every stair stopped short of the rim: none of it is put back")
          (is (empty? (events-of seen :go-to.restored)))
          (is (= (dug-cells p) (holes eng)) "every dug cell is in the ledger for restore-broken")
          (is (every? #(= "air" (block p %)) (holes eng))))))))

(deftest go-to-stair-stopped-part-way-walks-on-and-keeps-its-holes
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the third step would cut stone with no pickaxe: the stair stops after two, which already reach the rim
        (let [{:keys [eng out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "3,64,0" "stone")
                                                                       :inventory [{:name "dirt" :count 2}]})
                                                    {:pos [10 64 0] :range 1}))
              dug (dug-cells p)]
          (is (= {:arrived true} @out))
          (is (= #{[1 63 0] [1 62 0] [2 63 0]} dug))
          (is (= [{:step :stair :reason :no-tool}]
                 (mapv #(select-keys % [:step :reason]) (events-of seen :go-to.escalation-stopped))))
          (is (= [[1 62 0] [1 63 0]] (:cells (first (events-of seen :go-to.restored)))) "two carried dirt go back")
          (is (= #{[2 63 0]} (holes eng)) "the one it had nothing for stays in the ledger")
          (is (every? #(= "air" (block p %)) (holes eng))))))))

(deftest go-to-cancelled-mid-escalation-leaves-its-holes-in-the-ledger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plain (apply dissoc (merge (box -40 63 -40 40 63 40 "stone") pit) pit-cells)
              {:keys [eng p seen]} (setup (merge in-pit {:blocks plain :inventory [{:name "dirt" :count 2}]}))]
          (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0] :range 1}) {})
          (let [k (atom 0)]
            (.override (.-world p) "dig" (fn [token args impl]
                                           (if (< (swap! k inc) 3) (impl token args) (js/Promise. (fn [_ _])))))
            (core/tick! eng)
            (loop [i 0]
              (when (and (< i 300) (< @k 3))
                (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
                (recur (inc i)))))
          (is (= 3 (count (calls p "dig"))) "the third dig never ends: the go-to is cut inside its stair")
          (core/cancel! eng (first (:list (core/state eng))))
          (.override (.-world p) "dig" nil)
          (is (empty? (:list (core/state eng))))
          (is (= (set (take 2 (map #(mapv (js->clj (.-pos (.-args %)) :keywordize-keys true) [:x :y :z]) (calls p "dig"))))
                 (holes eng))
              "both dug cells outlive the cancelled job, noted as the stair dug them")
          (is (every? :any-of (filter :escalation (ledger eng))))
          (core/submit! eng '(jobs.survival.restore-broken) {})
          (await (tick-out! eng 100))
          (is (every? #(= "air" (block p %)) (holes eng)) "the body is still shut in: its holes stay open")
          (swap! (fake/state p) assoc-in [:self :pos] [5.5 64 0.5])
          (core/submit! eng '(jobs.survival.restore-broken) {})
          (await (tick-out! eng 300))
          (is (= "dirt" (block p [1 63 0])) "out of the pit, restore-broken puts them back")
          (is (= "dirt" (block p [1 62 0])))
          (is (empty? (holes eng))))))))

(deftest clear-path-collects-what-it-digs-so-go-to-can-put-it-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-room {:blocks room}) {:pos [8 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))))
          (is (empty? (events-of seen :go-to.restore-skipped)) "nothing carried at the start: the dug dirt was picked up")
          (is (= "dirt" (block p [3 64 0])))
          (is (= "dirt" (block p [3 65 0]))))))))

;; ------------------------------------------------------------------ review follow-ups (card 732fce4c)

(def cobble-room
  "room with cobblestone walls (the walls are not natural)."
  (merge (box -6 63 -6 12 63 6 "stone")
         (apply dissoc (box -3 64 -3 3 66 3 "cobblestone")
                (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(deftest escape-choose-digs-blocks-the-body-placed-itself
  (let [p (tu/fake {:self {:pos {:x 2 :y 64 :z 0}} :blocks cobble-room})
        mine? (fn [cell block] (and (= "cobblestone" block) (= 3 (first cell))))]
    (is (= {:step :none :why :no-dig} (escape/choose p [2 64 0] [8 64 0])) "someone's cobblestone wall")
    (is (= {:step :clear-path :heading :east} (escape/choose p [2 64 0] [8 64 0] (constantly true) {:own? mine?}))
        "its own put-back cobblestone")
    (is (= {:step :none :why :no-dig}
           (escape/choose p [2 64 0] [8 64 0] (constantly true) {:own? (fn [cell _] (= [3 64 0] cell))}))
        "one of the two door cells is not its own")))

(deftest escape-choose-skips-failed-steps
  (let [with-dirt (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 3}]})]
    (is (= :pillar (:step (escape/choose (tu/fake with-dirt) [0 61 0] [10 64 0]))))
    (is (= {:step :stair :heading :east :steps 3}
           (escape/choose (tu/fake with-dirt) [0 61 0] [10 64 0] (constantly true) {:skip #{:pillar}})))
    (is (= :none (:step (escape/choose (tu/fake with-dirt) [0 61 0] [10 64 0] (constantly true)
                                       {:skip #{:pillar :stair :clear-path :approach}}))))))

(deftest escape-door-beside-counts-only-doors-the-body-can-open
  (let [at (fn [b] (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks (assoc room "3,64,0" b)}))
        in-yard (fn [b] (escape/door-beside? (at b) [2 64 0]))]
    (is (true? (in-yard "oak_door")))
    (is (true? (in-yard "spruce_fence_gate")))
    (is (false? (in-yard "iron_door")) "iron doors are walls")
    (is (false? (in-yard "oak_trapdoor")) "a trapdoor in a wall is no exit")
    (is (false? (escape/door-beside? (at "oak_door") [2 64 0] :never)) "doors :never makes every door a wall")
    (is (true? (escape/door-beside? (at "oak_door") [2 64 0] :leave-open)))))

(deftest go-to-with-a-usable-door-beside-does-not-escalate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [yard (-> room (dissoc "-2,66,0") (assoc "3,64,0" "oak_door" "3,65,0" "oak_door"))
              {:keys [out p seen]} (await (run-job! (merge in-room {:blocks yard}) {:pos [8 64 0] :range 1}))]
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-tries-the-next-method-when-the-first-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; another owner's zone covers the pit's column: the pillar may not place there, the stair digs beside it
        (let [zones [{:name "shaft" :owner "Miles" :min [0 60 0] :max [0 64 0]}]
              {:keys [out p seen]} (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 5}]})
                                                'jobs.movement.go-to {:pos [10 64 0] :range 1} zones))]
          (is (= {:arrived true} @out))
          (is (= [:pillar :stair] (mapv :step (events-of seen :go-to.escalated))))
          (is (empty? (calls p "jumpPlace"))))))))

(deftest go-to-puts-back-with-a-block-it-may-dig-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone-hole (apply dissoc (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "stone")) pit-cells)
              {:keys [eng p]} (setup (merge in-pit {:blocks stone-hole
                                                     :inventory [{:name "iron_pickaxe" :count 1}
                                                                 {:name "cobblestone" :count 2}]}))]
          (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0] :range 1}) {})
          (await (tick-out! eng 600))
          (is (= "cobblestone" (block p [1 62 0])) "the put-back block is cobblestone")
          (is (= #{[[1 62 0] "cobblestone"] [[1 63 0] "cobblestone"]}
                 (set (map (comp (juxt :cell :block) :data) (mem/entries (mem/view (:store eng)) :escalation-placed)))
               )))))))

(deftest escape-choose-walks-along-the-wall-when-the-wall-in-line-is-an-iron-door
  (let [iron (-> room (assoc "3,64,0" "iron_door" "3,65,0" "iron_door"))
        c (escape/choose (tu/fake (merge in-room {:blocks iron :self {:pos {:x 2 :y 64 :z 0}}})) [2 64 0] [8 64 0])]
    (is (= :approach (:step c)))
    (is (not= [2 64 0] (:pos c)))))

(deftest escape-choose-searches-for-a-door-spot-once
  (let [iron (-> room (assoc "3,64,0" "iron_door" "3,65,0" "iron_door"))
        calls (atom 0)
        real escape/door-spot]
    (with-redefs [escape/door-spot (fn [& args] (swap! calls inc) (apply real args))]
      (escape/choose (tu/fake (merge in-room {:blocks iron :self {:pos {:x 2 :y 64 :z 0}}})) [2 64 0] [8 64 0]))
    (is (= 1 @calls))))

(deftest go-to-escalates-on-a-door-that-doors-never-refuses
  (let [ok? (fn [result doors] (esc/escalate-reason? result doors))]
    (is (true? (ok? {:reason :exhausted} :shut)))
    (is (true? (ok? {:reason :start-enclosed} :shut)) "a pen sealed in the loaded world, goal far and unloaded (card 5a822a99)")
    (is (true? (ok? {:reason :abilities :kind :open} :never)) "the planner's way through a door, refused as a wall")
    (is (false? (ok? {:reason :abilities :kind :open} :shut)))
    (is (false? (ok? {:reason :abilities :kind :break} :never)))
    (is (false? (ok? {:reason :stuck} :never)))))

(deftest escape-enclosed-counts-doors-as-walls-under-doors-never
  (let [wide (merge room (box -20 63 -20 30 63 20 "stone"))
        p (tu/fake (merge in-room {:blocks (assoc wide "3,64,0" "oak_door" "3,65,0" "oak_door")}))]
    (is (false? (escape/enclosed? p :shut)) "a wooden door is a way out")
    (is (true? (escape/enclosed? p :never)) "doors :never: the room is shut")))

;; ------------------------------------------------------------------ other headings (card 4ad29ed7)

(deftest escape-choose-skips-only-the-refused-stair-heading
  (let [p (tu/fake (merge in-pit {:blocks pit}))
        first-heading (:heading (escape/choose p [0 61 0] [10 64 0]))
        again (escape/choose p [0 61 0] [10 64 0] (constantly true) {:skip #{[:stair first-heading]}})]
    (is (= :east first-heading))
    (is (= :stair (:step again)))
    (is (not= first-heading (:heading again)))))

(deftest go-to-climbs-out-of-a-pit-by-another-heading-when-the-first-is-refused
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lava-pit (assoc pit "2,63,1" "lava")
              {:keys [out seen]} (await (run-job! (merge in-pit {:blocks lava-pit}) {:pos [10 64 0] :range 1}))]
          (is (true? (:arrived @out)))
          (is (< 1 (count (events-of seen :go-to.escalated))) "the first heading was refused, another was tried"))))))

(deftest go-to-escalates-from-the-free-part-of-a-shut-iron-door-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [iron (assoc room "3,64,0" "iron_door" "3,65,0" "iron_door")
              states {"3,64,0" {:open false :half "lower" :facing "west"} "3,65,0" {:open false :half "upper" :facing "west"}}
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 3.3 :y 64 :z 0.5}} :blocks iron :states states
                                                     :inventory [{:name "stone_pickaxe" :count 1}]}
                                                    {:pos [8 64 0] :range 1}))]
          (is (seq (events-of seen :go-to.escalated)) "a body shut in by the panel escalates")
          (is (= {:arrived true} @out))
          (is (< 6 (first (feet p)))))))))

;; ------------------------------------------------------------------ rounds follow-ups (card be8f7a88)

(deftest a-put-back-that-declines-at-the-call-is-skipped-and-stays-in-the-ledger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls* (atom 0)
              place (get registry/jobs 'jobs.blocks.place)
              ;; the wait pre-check passes (odd calls), the child's own check declines (even calls)
              flaky (assoc place :check (fn [c] (or (odd? (swap! calls* inc)) (do (ctx/wait c :flaky) false))))
              {:keys [out p seen eng]} (with-redefs [registry/jobs (assoc registry/jobs 'jobs.blocks.place flaky)]
                                         (await (run-job! (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 2}]})
                                                          {:pos [10 64 0] :range 1})))
              skipped (:cells (first (events-of seen :go-to.restore-skipped)))]
          (is (= {:arrived true} @out))
          (is (some #{:declined} (map :why skipped)) "a declined child is a skip with the reason :declined")
          (is (= [[1 62 0] [1 63 0] [2 63 0]] (sort (mapv :cell skipped))) "every hole is left for restore-broken")
          (is (= "air" (block p [1 62 0])) "nothing was put back"))))))

(def room-and-deck
  "The sealed room with a stone deck (x 7..9, z -1..1, y 70) floating high over the floor beside it: the deck is cut off by a drop."
  (merge room (box 7 70 -1 9 70 1 "stone")))

(deftest go-to-escalates-on-a-goal-cut-off-as-on-a-walled-in-one
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge {:self {:pos {:x 5 :y 64 :z 0}}} {:blocks room-and-deck})
              {:keys [out seen]} (await (run-job! world {:pos [8 71 0] :range 0}))
              {off :out} (await (run-job! world {:pos [8 71 0] :range 0 :escalate false}))]
          (is (= :goal-cut-off (:why @off)) "without escalation the planner's verdict is cut-off")
          (is (seq (events-of seen :go-to.escalated)) "shut in: it escalates")
          (is (= :unreachable (:reason @out))))))))
