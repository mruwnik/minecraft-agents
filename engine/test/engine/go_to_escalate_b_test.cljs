(ns engine.go-to-escalate-b-test
  "jobs.movement.go-to escalation (pillar, stair, clear-path, a walk to a wall) and jobs.lib.escape, against the fake
  world."
  (:require [engine.contract :as contract]
            [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.go-to-test :as gt]
            [engine.go-to-support :refer [hiding]]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.lib.access :as access]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.escape :as escape]
            [jobs.lib.pen :as pen]
            [jobs.lib.own-pen :as own-pen]
            [engine.ready-test :as rt]
            [jobs.lib.own-builds :as own-builds]
            [jobs.movement.go-to.holes :as holes]
            [jobs.movement.go-to.restore :as restore]
            [jobs.lib.reserve :as reserve]
            [jobs.movement.go-to.escalate-choose :as ec]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [plan.shape :as shape]
            [jobs.movement.go-to.escalation :as esc]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [triggers.access.scaffold-left :as scaffold-left]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [engine.go-to-escalate-support :refer [setup recording-parent tick-out! run-job! feet block events-of calls pit-cells pit stone-pit in-pit room in-room hollow ba ledge-room on-ledge wide-pit plank-yard holes dug-cells foreign trespass deep-pit stone-room run-in-shelter! ledger restores-after-the-last-stair? deep-shaft own-pit-wall cobble-room room-and-deck pocket free-by-pocket high-pocket moat door-cell dark-at gap-tunnel long-loop loop-tolls stair-below-surface cave below-pocket dig-route-run!]]))

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

(deftest escape-choose-digs-through-a-thin-wall-in-a-tunnel-to-a-goal-on-its-level
  ;; Juniper #47: a 1-wide tunnel sealed by one dirt block read as a pit, and the stair climbed 18 blocks
  (let [tunnel (merge (apply dissoc (box -8 62 -2 8 67 2 "stone")
                             (for [x (range -6 7) :when (not= x 1) y [64 65]] (str x "," y ",0")))
                      {"1,64,0" "dirt" "1,65,0" "dirt"})
        p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks tunnel})]
    (is (= {:step :clear-path :heading :east} (escape/choose p [0 64 0] [6 64 0])))
    (is (= :stair (:step (escape/choose p [0 64 0] [6 66 0]))) "a goal above still stairs")))

(deftest escape-choose-stairs-up-out-of-a-sealed-tunnel-to-a-goal-through-thick-rock
  ;; Rowan #22: a 1-wide pocket (two sides walled, pit depth 0), goal on its level behind rock with no floor beyond
  (let [pocket (apply dissoc (box -8 62 -2 8 67 2 "stone") (for [x (range -2 1) y [64 65]] (str x "," y ",0")))
        p (tu/fake {:self {:pos {:x -1 :y 64 :z 0}} :blocks pocket})]
    (is (= :stair (:step (escape/choose p [-1 64 0] [6 64 0]))))))

(deftest escape-choose-stair-checks-every-step
  (let [ba-pit (assoc pit "2,63,0" "bricks")]
    (is (not= :east (:heading (escape/choose (tu/fake (merge in-pit {:blocks ba-pit})) [0 61 0] [10 64 0])))
        "the second step east would cut bricks: another heading")))

(deftest go-to-does-not-dig-through-a-built-wall
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [wall ["oak_planks" "bricks" "glass" "oak_log"]]
          (let [built (apply dissoc (merge room (box -3 64 -3 3 65 3 wall))
                             (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))
                {:keys [out p seen]} (await (run-job! (merge in-room {:blocks built :inventory [{:name "dirt" :count 9}]})
                                                  {:pos [8 64 0] :range 1}))]
            (is (= false (:arrived @out)) wall)
            (is (= {:step :none :why :no-dig} (:escalation @out)) wall)
            (is (empty? (events-of seen :go-to.escalated)) wall)
            (is (empty? (calls p "dig")) wall)))))))

(deftest go-to-digs-out-through-a-wall-of-its-own-scaffold-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inner (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z))
              walled (apply dissoc (merge room (box -3 64 -3 3 65 3 "cobblestone")) inner)
              own (for [x (range -3 4) z (range -3 4) y [64 65]
                        :when (or (= 3 (abs x)) (= 3 (abs z)))]
                    {:cell [x y z] :item "cobblestone" :before "air" :job "j0" :purpose :pillar :state :placed})
              {:keys [eng p]} (setup (merge in-room {:blocks walled}))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 1})))]
          (ledger/write! (:store eng) own)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (true? (:arrived @out)) (pr-str @out))
          (is (seq (calls p "dig")) "it dug its own cobblestone")
          (is (empty? (filter (ledger/cells (ledger/open-entries (mem/view (:store eng))))
                              (map #(let [pos (.-pos (.-args %))] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "dig"))))
              "a dug block of its own leaves the ledger at once: a block built there later is not its own"))))))

(deftest a-body-its-shelter-keeps-in-for-a-hostile-cuts-another-way-out
  ;; Wicken #139: the take-back kept the night seal for a hostile; Teasel #6: giving up there left the body shut in for ever
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seal [{:cell [3 64 1] :item "dirt" :before "air" :job "j0" :purpose :shelter :state :placed}
                    {:cell [3 65 1] :item "dirt" :before "air" :job "j0" :purpose :shelter :state :placed}]
              {:keys [eng p]} (setup {:self {:pos {:x 2.5 :y 64 :z 0.5}} :blocks room
                                      :entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 11.5 :y 64 :z 5.5} :visible true}]})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent
                                          (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 1 :checks {:safe? '(always)}
                                                                                       :settings {:jobs.lib.shelter.keep/at-night? false}})))]
          (ledger/write! (:store eng) seal)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (true? (:arrived @out)) (pr-str @out))
          (is (seq (calls p "dig")) "another wall is cut")
          (is (= ["dirt" "dirt"] (mapv #(block p %) [[3 64 1] [3 65 1]])) "the seal the zombie would come in by stays"))))))

(deftest leave-shelter-agrees-with-go-to-that-a-body-its-seal-keeps-in-is-not-out
  ;; Wicken #155: leave-shelter said "out of the shelter" while go-to gave up :seal-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seal [{:cell [3 64 1] :item "dirt" :before "air" :job "j0" :purpose :shelter :state :placed}
                    {:cell [3 65 1] :item "dirt" :before "air" :job "j0" :purpose :shelter :state :placed}]
              ;; past the dawn grace: the seal is kept for the zombie, not held for it at first light
              {:keys [eng p seen]} (setup {:self {:pos {:x 2.5 :y 64 :z 0.5}} :blocks room :time 9000
                                           :entities [{:id 1 :name "zombie" :kind "hostile" :pos {:x 11.5 :y 64 :z 5.5} :visible true}]})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.survival.leave-shelter {})))]
          (ledger/write! (:store eng) seal)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 400))
          (is (re-find #"seal-kept" (pr-str @out)) (pr-str @out))
          (is (empty? (events-of seen :dig-in.left)) "never says it is out")
          (is (empty? (calls p "dig")) "the seal is kept for the hostile"))))))

(deftest go-to-does-not-dig-a-wall-its-ledger-meant-another-block-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inner (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z))
              walled (apply dissoc (merge room (box -3 64 -3 3 65 3 "cobblestone")) inner)
              intent (for [x (range -3 4) z (range -3 4) y [64 65]
                           :when (or (= 3 (abs x)) (= 3 (abs z)))]
                       {:cell [x y z] :item "dirt" :before "air" :job "j0" :purpose :pillar :state :intent})
              {:keys [eng p]} (setup (merge in-room {:blocks walled}))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [8 64 0] :range 1})))]
          (ledger/write! (:store eng) intent)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (false? (:arrived @out)) (pr-str @out))
          (is (empty? (calls p "dig")) "an :intent for another item is no proof the cobblestone there is its own"))))))

(deftest go-to-leaves-another-s-zone-it-stands-in-by-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "vault" :owner "Miles" :min [-3 60 -3] :max [3 70 3]}]
              {:keys [out]} (await (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to
                                             {:pos [8 64 0] :range 1} zones))]
          (is (true? (:arrived @out)) "a body inside the zone is never shut in by it"))))))

(deftest go-to-leaving-a-zone-never-digs-in-the-zone-round-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "vault" :owner "Miles" :min [-3 60 -3] :max [2 70 3]}
                     {:name "outer" :owner "Miles" :min [-60 0 -60] :max [60 120 60]}]
              {:keys [out p]} (await (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to
                                               {:pos [8 64 0] :range 1} zones))
              dug-in-outer (remove #(<= -3 (first %) 2) (dug-cells p))]
          (is (empty? dug-in-outer) "the way out ignores the vault, not the zone round it")
          (is (= {:step :none :why :zone} (:escalation @out))))))))

(deftest go-to-never-digs-in-another-s-zone-and-leaves-by-a-wall-that-is-free
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "vault" :owner "Miles" :min [3 60 -3] :max [3 70 3]}]
              {:keys [out p]} (await (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to
                                               {:pos [8 64 0] :range 1} zones))]
          (is (true? (:arrived @out)) "the east wall is another owner's; the south wall is free")
          (is (seq (dug-cells p)))
          (is (not-any? #(= 3 (first %)) (dug-cells p))))))))

(deftest go-to-digs-out-of-its-own-house-through-one-wall-column-and-puts-it-back
  ;; card 1ac3d50f: the only way out cuts the body's own plan: it is priced, not refused, and the fewest own cells are cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hut (shape/with-author {:id "hut" :parts [{:id "shell" :box [[-3 64 -3] [3 66 3]] :want "dirt"}]} "Fake")
              {:keys [out p seen]} (await (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to
                                                {:pos [8 64 0] :range 1} {:plans {"hut" hut}}))
              dug (dug-cells p)]
          (is (= {:arrived true} @out))
          (is (= #{[3 64 0] [3 65 0]} (set dug)) "one wall column toward the goal, nothing else")
          (is (= (set dug) (set (mapcat :cells (events-of seen :go-to.restored)))) "and put back")
          (is (= ["dirt" "dirt"] [(block p [3 64 0]) (block p [3 65 0])]))
          (is (= (set dug) (set (map :cell (events-of seen :go-to.dug-own-build)))) "each own cell dug is named"))))))

(deftest go-to-cuts-its-own-wall-when-that-costs-less-than-the-natural-way-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [east-wall (shape/with-author {:id "hut" :parts [{:id "east" :box [[3 64 -3] [3 66 3]] :want "dirt"}]} "Fake")
              go (fn [plans] (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to {:pos [8 64 0] :range 1} {:plans plans}))
              own (await (go {"hut" east-wall}))
              free (await (go {}))]
          (is (= {:arrived true} @(:out own)))
          (is (= 2 (count (dug-cells (:p own)))) "a door through its own wall is cheaper than any natural way")
          (is (every? #(= 3 (first %)) (dug-cells (:p own))))
          (is (seq (events-of (:seen own) :go-to.restored)) "its own cells are put back")
          (is (empty? (events-of (:seen free) :go-to.restored)) "natural ground is left open"))))))

(deftest go-to-pillars-out-of-a-pit-walled-by-its-own-plan-rather-than-cut-the-walls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge in-pit {:blocks pit :plans {"hut" own-pit-wall} :inventory [{:name "dirt" :count 8}]})
              {:keys [out p seen]} (await (run-job! world 'jobs.movement.go-to {:pos [10 64 0] :range 1} {:plans (:plans world)}))]
          (is (= {:arrived true} @out))
          (is (= [:pillar] (map :step (events-of seen :go-to.escalated))) "the pillar costs less than a stair through its walls and their put-back")
          (is (empty? (calls p "dig"))))))))

(defn dug-cells-of [p] (set (map #(let [pos (.-pos (.-args %))] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "dig"))))

(deftest go-to-digs-out-of-its-own-pen-holding-animals-and-puts-the-wall-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "pen" :owner "Fake" :min [-2 60 -2] :max [2 64 2]}]
              cow {:id 7 :name "cow" :kind "passive" :pos {:x 0.5 :y 61 :z 0.5}}
              go (fn [world zones] (run-job! world 'jobs.movement.go-to {:pos [10 64 0] :range 1} zones))
              {:keys [out p seen]} (await (go (merge in-pit {:blocks pit :entities [cow] :inventory [{:name "dirt" :count 8}]}) zones))
              open (await (go (merge in-pit {:blocks pit}) zones))]
          (is (= {:arrived true} @out))
          (is (seq (dug-cells-of p)))
          (is (= (dug-cells-of p) (set (mapcat :cells (events-of seen :go-to.restored)))) "animals stand in the pit the body owns: every cell dug is put back")
          (is (= {:arrived true} @(:out open)) "an own pit with no animals in it is dug out of as before")
          (is (empty? (events-of (:seen open) :go-to.restored)) "and its holes are left"))))))

(deftest a-way-through-own-builds-is-searched-only-when-its-least-cost-can-beat-the-way-that-spares-them
  ;; card f1d55046: an own cut adds at least one cell's put-back to its way
  (let [p (tu/fake (merge in-pit {:blocks pit :inventory [{:name "dirt" :count 8}]}))
        feet [0 61 0]
        pillar {:step :pillar :height 3}]
    (is (true? (esc/own-may-beat? p feet {:step :none})) "no way spares them")
    (is (< (cw/put-back-cost 0) (ec/price p feet pillar)))
    (is (true? (esc/own-may-beat? p feet pillar)))
    (is (false? (esc/own-may-beat? p feet {:step :dig-route :cells []})) "a way that costs nothing is never beaten")))

(deftest an-own-stocked-pen-wall-cut-costs-its-put-back-and-the-herd-s-loss-and-the-pen-is-read-once
  ;; card dbd43651: the herd-loss branch of the put-back price; the may-dig, own and price fns of one escalation share one pen read
  (let [cow {:id 7 :name "cow" :kind "passive" :pos {:x 0.5 :y 61 :z 0.5}}
        c (assoc (rt/ctx-of (merge in-pit {:blocks pit :entities [cow]})) :args {})
        checks (atom 0)
        check pen/check
        wall [1 62 0]]
    (with-redefs [access/in-own-area? (constantly true)
                  pen/check (fn [o] (swap! checks inc) (check o))]
      (let [c (own-builds/with-own-pen c)
            back (cw/put-back-cost 0)
            cut-s ((holes/own-cut-fn c) wall)]
        (is (false? ((holes/may-dig-fn c) wall)) "the wall is spared unless its cut is chosen")
        (is (true? ((own-builds/own-fn c (constantly false)) wall "dirt")))
        (is (= (+ back (cost/herd-loss ["cow"] (+ back (cost/dig-cost "dirt")))) cut-s))
        (is (< back cut-s) "the herd may walk out while the gap stands open")
        (is (zero? ((holes/own-cut-fn c) [3 62 0])) "natural ground beyond the wall costs no put-back")
        (is (= 1 @checks))))))

(def thick-pit-pen
  "A 2-deep 5x5 pit (inside x -2..2, z -2..2, feet y 64) in stone x -6..6, y 64..65, z -6..6 (walls 4 thick) on a stone
  floor x -10..14, z -10..10."
  (merge (box -10 63 -10 14 63 10 "stone")
         (apply dissoc (box -6 64 -6 6 65 6 "stone") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(deftest go-to-mends-a-dead-end-cut-of-its-own-pen-before-the-next-way
  ;; a door whose far side, past what the body saw, has no floor ends in a dead end; its cut opens the pen, so it is put back before the next way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "pen" :owner "Fake" :min [-2 60 -2] :max [2 66 2]}]
              cow {:id 7 :name "cow" :kind "passive" :pos {:x -1.5 :y 64 :z -1.5}}
              kept (atom [])
              done! restore/restore-done!
              {:keys [out p seen]} (with-redefs [restore/restore-done! (fn [c] (swap! kept conj (filterv #(= "the put-back of the stair's cut" (:why %)) (reserve/entries (ctx/view c)))) (done! c))]
                                     (await (run-job! {:sight 3 :self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (dissoc thick-pit-pen "7,63,0") :entities [cow]
                                                       :inventory [{:name "stone_pickaxe" :count 1}]}
                                                      'jobs.movement.go-to {:pos [10 64 0] :range 1} zones true)))]
          (is (= {:arrived true} @out))
          (is (some seq @kept) (str "the stair's drops are still held for the put-back after the stair ended: " (pr-str @kept)))
          (is (= [:go-to.escalated :go-to.escalation-stopped :go-to.restored]
                 (take 3 (map :kind (filter #(#{:go-to.escalated :go-to.escalation-stopped :go-to.restored} (:kind %)) @seen))))
              "the dead end's cut is put back before the next way, not once the walk is on")
          (is (some #{[3 64 0]} (:cells (first (events-of seen :go-to.restored)))) "the cell that opens into the pen first")
          (is (= (set (for [x [3 4 5] y [64 65]] [x y 0])) (set (:cells (first (events-of seen :go-to.restored))))) "the whole dead end is put back")
          ;; the next way, a stair through the pen wall, picks up its drops: nothing is left open for want of a block
          (is (empty? (filter #(= :need (:why %)) (mapcat :cells (events-of seen :go-to.restore-skipped)))) (pr-str (events-of seen :go-to.restore-skipped)))
          (is (not-any? #(= "air" (block p %)) [[3 64 0] [3 65 0]]) "the pen wall is whole again"))))))

(deftest go-to-reads-its-own-pen-once-per-escalation-round
  ;; choose-escalation! and the replan hold the pen read in c (with-own-pen); without it each fn reads the pen itself (own-builds/pen-of).
  ;; Only reads by a c that already holds the pen are counted: a legitimate read outside a round does not matter.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "pen" :owner "Fake" :min [-2 60 -2] :max [2 66 2]}]
              cow {:id 7 :name "cow" :kind "passive" :pos {:x -1.5 :y 64 :z -1.5}}
              reads (atom 0)
              rounds (atom 0)
              read own-pen/shut-in
              with-pen own-builds/with-own-pen]
          (with-redefs [own-pen/shut-in (fn [c f] (when (:own-pen c) (swap! reads inc)) (read c f))
                        own-builds/with-own-pen (fn [c] (swap! rounds inc) (with-pen c))]
            (await (run-job! {:sight 3 :self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks thick-pit-pen :entities [cow]
                                :inventory [{:name "stone_pickaxe" :count 1}]}
                               'jobs.movement.go-to {:pos [10 64 0] :range 1} zones)))
          (is (pos? @rounds) "the escalation round holds the pen read in c")
          (is (zero? @reads) "and no fn of the round reads the pen again"))))))

(deftest go-to-hops-the-1-high-sill-of-its-pen-gap-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pen (-> (merge (box -10 63 -10 10 63 10 "stone") (box 0 64 -2 4 65 2 "dirt"))
                      (#(apply dissoc % (for [x (range 1 4) z (range -1 2) y [64 65]] (str x "," y "," z))))
                      (dissoc "0,65,0"))
              sheep {:id 7 :name "sheep" :kind "passive" :pos {:x 3.5 :y 64 :z 1.5}}
              {:keys [out p]} (await (run-job! {:self {:pos {:x 2.5 :y 64 :z 0.5}} :blocks pen :entities [sheep]}
                                              'jobs.movement.go-to {:pos [-2 64 0] :range 0}
                                              [{:name "pen" :owner "Fake" :min [0 64 -2] :max [4 65 2]}]))]
          (is (true? (:arrived @out)) (pr-str @out))
          (is (empty? (calls p "dig")) "a 1-high sill is hopped, the pen wall stays"))))))

(deftest go-to-pillars-out-of-its-own-wide-pen-pit-holding-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "pen" :owner "Fake" :min [-3 60 -3] :max [3 64 3]}]
              cow {:id 7 :name "cow" :kind "passive" :pos {:x -1.5 :y 61 :z -1.5}}
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0 :y 61 :z 0}} :blocks wide-pit :entities [cow]
                                                     :inventory [{:name "dirt" :count 5}]}
                                                    'jobs.movement.go-to {:pos [10 64 0] :range 1} zones))]
          (is (= {:arrived true} @out))
          (is (= :pillar (:step (last (events-of seen :go-to.escalated)))))
          (is (= 2 (count (calls p "jumpPlace"))))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-roofed-pit-pillars-digging-its-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "0,63,0" "dirt")
                                                                   :inventory [{:name "dirt" :count 5}]})
                                                {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (= 2 (count (calls p "jumpPlace"))) "two blocks, cheaper than a 3-step stair (cost.ways)")
          (is (= [:pillar] (mapv :step (events-of seen :go-to.escalated))))
          (is (contains? (dug-cells p) [0 63 0]) "the pillar digs the roof over its head"))))))

(deftest go-to-roofed-pit-with-no-way-to-dig-says-no-headroom
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walls (merge (box 3 60 -3 14 63 3 "stone") (box -2 60 -2 2 63 2 "bricks"))
              roofed (assoc (apply dissoc walls pit-cells) "0,63,0" "bricks")
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
        (let [zones [{:name "vault" :owner "Miles" :min [-3 62 -3] :max [3 70 3]}]
              {:keys [out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "0,63,0" "dirt")
                                                                   :inventory [{:name "dirt" :count 5}]})
                                                'jobs.movement.go-to {:pos [10 64 0] :range 1} zones))]
          (is (= false (:arrived @out)))
          (is (= {:step :none :why :zone} (:escalation @out)))
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-stair-stopped-part-way-walks-on-and-keeps-its-holes
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the third step would cut stone with no pickaxe: the stair stops after two, which already reach the rim
        (let [{:keys [eng out p seen]} (await (run-job! (merge in-pit {:blocks (assoc pit "3,64,0" "stone")
                                                                       :inventory [{:name "dirt" :count 2}]})
                                                    'jobs.movement.go-to (merge {:pos [10 64 0] :range 1 :ways #{:walk :stair}} trespass) foreign))
              dug (dug-cells p)]
          (is (= {:arrived true} @out))
          (is (= #{[1 63 0] [1 62 0] [2 63 0]} dug))
          (is (= [{:step :stair :reason :no-tool}]
                 (mapv #(select-keys % [:step :reason]) (events-of seen :go-to.escalation-stopped))))
          (is (= [[1 62 0] [1 63 0]] (:cells (first (events-of seen :go-to.restored)))) "two carried dirt go back")
          (is (= #{[2 63 0]} (holes eng)) "the one it had nothing for stays in the ledger")
          (is (every? #(= "air" (block p %)) (holes eng))))))))

(deftest go-to-stair-with-no-tool-for-its-heading-tries-another-heading-before-digging-by-hand
  ;; sweep 6763 go-to-stair-heading/obsidian-east-and-north-arrives: the stair met obsidian and dug it by hand (64 s a cell)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stone-east (merge pit (box 1 61 0 1 64 0 "stone"))
              {:keys [out p seen]} (await (run-job! (merge in-pit {:blocks stone-east}) {:pos [10 64 0] :range 1}))]
          (is (= {:arrived true} @out))
          (is (empty? (events-of seen :go-to.by-hand)))
          (is (= "stone" (block p [1 62 0])) "the stone is left"))))))

(deftest go-to-cancelled-mid-clear-path-notes-the-cell-dug-so-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (merge in-room {:blocks room :inventory [{:name "dirt" :count 2}]}) foreign)]
          (core/submit! eng '(jobs.movement.go-to {:pos [8 64 0] :range 1 :ignore-zones? true}) {})
          (let [k (atom 0)]
            (.override (.-world p) "dig" (fn [token args impl]
                                           (if (< (swap! k inc) 2) (impl token args) (js/Promise. (fn [_ _])))))
            (core/tick! eng)
            (loop [i 0]
              (when (and (< i 300) (< @k 2))
                (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
                (recur (inc i)))))
          (core/cancel! eng (first (:list (core/state eng))))
          (.override (.-world p) "dig" nil)
          (await (tick-out! eng 5))
          (is (empty? (:list (core/state eng))))
          (is (= #{(first (dug-cells p))} (holes eng))
              "the one cell dug before the cut is in the ledger, tagged as go-to's escalation")
          (is (every? :go-to (filter :escalation (ledger eng)))))))))

(deftest go-to-cancelled-mid-escalation-leaves-its-holes-in-the-ledger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plain (apply dissoc (merge (box -40 63 -40 40 63 40 "stone") pit) pit-cells)
              {:keys [eng p seen]} (setup (merge in-pit {:blocks plain :inventory [{:name "dirt" :count 2}]}) foreign)]
          (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0] :range 1 :ignore-zones? true :ways #{:walk :stair}}) {})
          (let [k (atom 0)]
            (.override (.-world p) "dig" (fn [token args impl]
                                           (if (< (swap! k inc) 3) (impl token args) (js/Promise. (fn [_ _])))))
            (core/tick! eng)
            (loop [i 0]
              (when (and (< i 300) (< @k 3))
                (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
                (recur (inc i)))))
          (is (= 3 (count (calls p "dig"))) "the third dig never ends: the go-to is cut inside its stair")
          (swap! wsearch/searches assoc p {:goal :kept})
          (core/cancel! eng (first (:list (core/state eng))))
          (.override (.-world p) "dig" nil)
          (await (tick-out! eng 5))
          (is (empty? (:list (core/state eng))) "the stair child's cleanup runs on the cancel")
          (is (not (contains? @wsearch/searches p)) "go-to's cleanup releases what the planner kept")
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
        (let [{:keys [out p seen]} (await (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to
                                                (merge {:pos [8 64 0] :range 1} trespass) foreign))]
          (is (= {:arrived true} @out))
          (is (= [:clear-path] (mapv :step (events-of seen :go-to.escalated))))
          (is (empty? (events-of seen :go-to.restore-skipped)) "nothing carried at the start: the dug dirt was picked up")
          (is (= "dirt" (block p [3 64 0])))
          (is (= "dirt" (block p [3 65 0]))))))))

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

(deftest escape-choose-offers-no-approach-when-clear-path-is-barred
  (let [p (tu/fake {:self {:pos {:x 1 :y 64 :z 0}} :blocks room})]
    (is (= :approach (:step (escape/choose p [1 64 0] [8 64 0] (constantly true) {:skip #{:stair :pillar}}))) "the door spot, when digging is allowed")
    (is (= :none (:step (escape/choose p [1 64 0] [8 64 0] (constantly true) {:skip #{:stair :pillar :clear-path}}))) "no walk to a door that may not be dug")))

(deftest escape-door-beside-counts-only-doors-the-body-can-open
  (let [at (fn [b] (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :blocks (assoc room "3,64,0" b)}))
        in-yard (fn [b] (escape/door-beside? (at b) [2 64 0]))]
    (is (true? (in-yard "oak_door")))
    (is (true? (in-yard "spruce_fence_gate")))
    (is (false? (in-yard "iron_door")) "iron doors are walls")
    (is (false? (in-yard "oak_trapdoor")) "a trapdoor in a wall is no exit")
    (is (false? (escape/door-beside? (at "oak_door") [2 64 0] :never)) "doors :never makes every door a wall")
    (is (true? (escape/door-beside? (at "oak_door") [2 64 0] :leave-open)))
    (is (false? (escape/door-beside? (tu/fake {:self {:pos {:x 0 :y 64 :z 0}}
                                                :blocks (assoc room "3,64,0" "spruce_fence_gate" "4,65,0" "oak_log")})
                                     [2 64 0]))
        "a gate with a trunk over the cell behind it leads nowhere")))

(deftest go-to-with-a-usable-door-beside-does-not-escalate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [yard (-> room (dissoc "-2,66,0") (assoc "3,64,0" "oak_door" "3,65,0" "oak_door"))
              {:keys [out p seen]} (await (run-job! (merge in-room {:blocks yard}) {:pos [8 64 0] :range 1}))]
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-escalates-when-the-door-opens-onto-a-step-under-its-lintel
  ;; the room's door leads out onto ground one block higher, with the wall's top over the doorway: no walk or jump leaves
  ;; the doorway, so the door beside the body is no way out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stepped (-> room (merge (box 4 64 -6 12 64 6 "grass_block")) (assoc "3,64,0" "oak_door" "3,65,0" "oak_door"))
              states {"3,64,0" {:open true :half "lower" :facing "east"} "3,65,0" {:open true :half "upper" :facing "east"}}
              {:keys [out seen]} (await (run-job! (merge in-room {:blocks stepped :states states}) {:pos [8 65 0] :range 1}))]
          (is (seq (events-of seen :go-to.escalated)) (pr-str @out))
          (is (= {:arrived true} @out)))))))

(deftest go-to-escalates-out-of-a-pen-whose-gate-has-a-trunk-behind-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ring (into {} (for [x (range -3 4) z (range -3 4) :when (or (= 3 (js/Math.abs x)) (= 3 (js/Math.abs z)))]
                              [(str x ",64," z) "oak_fence"]))
              ;; the ground rises one outside the gate, and a trunk stands on the first cell of the rise
              pen (merge (box -6 63 -6 12 63 6 "stone") (box 4 64 -6 12 64 6 "stone") ring
                         {"3,64,0" "spruce_fence_gate" "4,65,0" "oak_log" "4,66,0" "oak_log" "4,67,0" "oak_log" "4,68,0" "oak_leaves"})
              {:keys [out p seen]} (await (run-job! (merge in-room {:blocks pen :inventory [{:name "iron_axe" :count 1}]})
                                                    {:pos [6 65 2] :range 1}))]
          (is (seq (events-of seen :go-to.escalated)) (pr-str @out))
          (is (= "air" (block p [4 65 0])) "the trunk over the cell behind the gate is cut")
          (is (= "oak_fence" (block p [3 64 1])) "the fence is a build and stays"))))))

(deftest go-to-with-doors-never-digs-out-past-a-door-in-the-wall
  ;; live go-to-escalate/oak-door-with-doors-never-digs: the planner's only way is the door, refused (:abilities :open)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shut (-> room (assoc "3,64,0" "oak_door" "3,65,0" "oak_door"))
              states {"3,64,0" {:open false :half "lower" :facing "east"} "3,65,0" {:open false :half "upper" :facing "east"}}
              {:keys [out seen]} (await (run-job! (merge in-room {:blocks shut :states states :inventory [{:name "dirt" :count 2}]})
                                                  {:pos [8 64 0] :range 1 :doors :never}))]
          (is (seq (events-of seen :go-to.escalated)) (pr-str @out))
          (is (= {:arrived true} @out)))))))

(deftest go-to-tries-the-next-method-when-the-first-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; another owner's zone covers the pit's column: the pillar may not place there, the stair digs beside it
        (let [zones [{:name "shaft" :owner "Miles" :min [0 62 0] :max [0 64 0]}]
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
                                                                 {:name "cobblestone" :count 2}]})
                                     foreign)]
          (core/submit! eng '(jobs.movement.go-to {:pos [10 64 0] :range 1 :ignore-zones? true :ways #{:walk :stair}}) {})
          (await (tick-out! eng 600))
          (is (= "cobblestone" (block p [1 62 0])) "the put-back block is cobblestone")
          (is (= #{[[1 62 0] "cobblestone"] [[1 63 0] "cobblestone"]}
                 (set (map (comp (juxt :cell :block) :data) (mem/entries (mem/view (:store eng)) :escalation-placed)))
               )))))))

(defn ^:async pillar-call-checks
  "The :checks go-to's escalation pillar child is called with when go-to is called with go-checks."
  [go-checks]
  (let [seen-args (atom nil)
        real ctx/call
        zones [{:name "pen" :owner "Fake" :min [-3 60 -3] :max [3 64 3]}]]
    (with-redefs [ctx/call (fn [c k job args] (when (= 'jobs.access.pillar job) (reset! seen-args (:checks args))) (real c k job args))]
      (await (run-job! {:self {:pos {:x 0 :y 61 :z 0}} :blocks wide-pit :inventory [{:name "dirt" :count 5}]}
                       'jobs.movement.go-to (cond-> {:pos [10 64 0] :range 1} go-checks (assoc :checks go-checks)) zones)))
    @seen-args))

(deftest an-escalation-pillar-takes-the-callers-safe-form-else-ignores-hostiles
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:safe? '(always) :fit? '(always)} (await (pillar-call-checks nil))) "no caller form: a hostile or the dark pit does not stall the way out")
        (is (nil? (:safe? (await (pillar-call-checks {:safe? '(always)})))) "a caller's form reaches the pillar (card 02b795f1)")))))

(deftest go-to-digs-out-of-a-pit-pen-that-is-a-plan-of-the-body-and-puts-the-wall-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (apply dissoc (merge (box 10 59 10 30 59 22 "grass_block") (box 10 60 10 22 61 22 "stone"))
                            (for [x (range 14 19) z (range 14 19) y [60 61]] (str x "," y "," z)))
              plan (shape/with-author {:id "pit" :parts [{:id "inside" :box [[14 60 14] [18 61 18]] :want [:any "air"]}]} "Fake")
              cows [{:id 7 :name "cow" :kind "passive" :pos {:x 15.5 :y 60 :z 15.5}} {:id 8 :name "cow" :kind "passive" :pos {:x 17.5 :y 60 :z 17.5}}]
              {:keys [out p seen]} (await (run-job! {:sight 3 :self {:pos {:x 18.4 :y 60 :z 16.5}} :blocks blocks :entities cows}
                                                    'jobs.movement.go-to {:pos [26 60 16] :range 1} {:plans {"pit" plan}}))]
          (is (= {:arrived true} @out))
          (is (seq (dug-cells-of p)))
          (is (= (dug-cells-of p) (set (concat (mapcat :cells (events-of seen :go-to.restored)) (map :cell (mapcat :cells (events-of seen :go-to.restore-skipped))))))
              "every cell dug is put back, or left for restore-broken when nothing carried fills it"))))))

(deftest go-to-takes-back-the-own-blocks-that-shut-the-body-in-when-the-goal-is-farther-than-the-clear-radius
  ;; a seal beside the feet, the goal 6 blocks past it: the cleanup takes back every own block near the feet, also the one behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tunnel (merge (apply dissoc (box -8 62 -2 8 67 2 "stone") (for [x (range -6 7) y [64 65]] (str x "," y ",0")))
                            (into {} (for [x [-2 2] y [64 65]] [(str x "," y ",0") "cobblestone"])))
              own (for [x [-2 2] y [64 65]] {:cell [x y 0] :item "cobblestone" :before "air" :job "j0" :purpose :pillar :state :placed})
              {:keys [eng p]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks tunnel :inventory [{:name "stone_pickaxe" :count 1}]})
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [6 64 0] :range 1})))]
          (ledger/write! (:store eng) own)
          (core/submit! eng '(recording-parent) {})
          (await (tick-out! eng 1000))
          (is (true? (:arrived @out)) (pr-str @out))
          (is (= "air" (block p [-2 64 0])) "the seal behind the body is taken back too"))))))

(def own-pillar "A 3-high column under the body: every way off it is a 3-block drop." {"0,64,0" "cobblestone" "0,65,0" "cobblestone" "0,66,0" "cobblestone"})

(def bedrock-pit
  "Bedrock round the cell (3 64 0) on its four sides, 3 high: nothing the body could dig or climb out by."
  (into {} (for [[x z] [[2 0] [4 0] [3 -1] [3 1]] y [64 65 66]] [(str x "," y "," z) "bedrock"])))

(deftest a-drop-needs-a-way-out-only-where-it-lands-in-a-pit
  (let [priced? (fn [blocks] (wplan/one-way-priced? {:primitives (tu/fake {:self {:pos {:x 0.5 :y 67 :z 0.5}} :floor [-10 -10 10 10] :blocks (merge own-pillar blocks)}) :args {}}
                                                    [8 64 0] [2 67 0] [3 64 0] false))]
    (is (true? (priced? {})) "open ground: escape/choose has no wall to dig, which is no trap")
    (is (false? (priced? bedrock-pit)) "a pit with nothing to dig is a certain trap")))

(def fence-pen
  "A gateless oak-fence ring (x/z -3..3 at y 64) on a stone floor x -10..12, z -6..6 (feet y 64)."
  (merge (box -10 63 -6 12 63 6 "stone")
         (into {} (for [x (range -3 4) z (range -3 4) :when (or (#{-3 3} x) (#{-3 3} z))] [(str x "," 64 "," z) "oak_fence"]))))

(deftest go-to-breaks-out-of-its-own-stocked-gateless-pen-and-puts-the-fence-back
  ;; card bc302253: no stair climbs onto a fence (1.5 high), so the way out is a dig through the wall, mended once past
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "pen" :owner "Fake" :min [-3 60 -3] :max [3 66 3]}]
              cow {:id 7 :name "cow" :kind "passive" :pos {:x 1.5 :y 64 :z 1.5}}
              {:keys [out p seen]} (await (run-job! {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks fence-pen :entities [cow] :inventory [{:name "cobblestone" :count 5}]}
                                                    'jobs.movement.go-to {:pos [8 64 0] :range 1} zones))]
          (is (= {:arrived true} @out))
          (is (= [:clear-path] (map :step (events-of seen :go-to.escalated))))
          (is (= [[[3 64 0]]] (map :cells (events-of seen :go-to.restored))) "the cut fence is put back")
          (is (= "oak_fence" (block p [3 64 0]))))))))

(def glow-room
  "own-plan-footprint/own-room-in-rock: a dirt shell x 8..12, y 61..65, z 8..12 (inside x 9..11, y 62..64, z 9..11) in
  glowstone (a light: never dug) x 4..20, y 60..69, z 4..16 with a band of stone east of it, x 13..17, y 62..63, z 8..13."
  (apply dissoc (merge (box 4 60 4 20 69 16 "glowstone") (box 13 62 8 17 63 13 "stone") (box 8 61 8 12 65 12 "dirt"))
         (for [x (range 9 12) y (range 62 65) z (range 9 12)] (str x "," y "," z))))

(deftest go-to-out-of-its-own-room-buried-in-rock-cuts-one-wall-column-it-has-not-seen-beyond
  ;; card f9fbddde: the rock beyond the walls is unseen from inside; the way is priced over it as guessed rock
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (shape/with-author {:id "room" :parts [{:id "shell" :box [[8 61 8] [12 65 12]] :want "dirt"}]} "Fake")
              world {:self {:pos {:x 11.5 :y 62 :z 9.5}} :blocks glow-room :light [15 15] :sight true
                     :inventory [{:name "dirt" :count 8} {:name "stone_pickaxe" :count 1} {:name "stone_shovel" :count 1}]}
              {:keys [eng p seen step]} (gt/setup world (ew/of-data {"room" room} {} []))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [14 62 12] :range 0})))]
          (core/submit! eng '(recording-parent) {})
          (await (gt/tick-out! eng 400 step))
          (let [own (map :cell (events-of seen :go-to.dug-own-build))]
            (is (= {:arrived true} @out))
            (is (= 2 (count own)) "one column of its own wall")
            (is (= #{12} (set (map first own))) "the east wall, toward the goal")
            (is (every? (set (mapcat :cells (events-of seen :go-to.restored))) own) "and put back")))))))

(def glow-cobble-room
  "glow-room with a cobblestone shell: its own wall is not natural terrain."
  (apply dissoc (merge glow-room (box 8 61 8 12 65 12 "cobblestone"))
         (for [x (range 9 12) y (range 62 65) z (range 9 12)] (str x "," y "," z))))

(deftest go-to-out-of-its-own-cobble-room-buried-in-rock-cuts-one-wall-column-of-its-own-plan
  ;; the dig route's dig child refuses what is not natural terrain unless it is the body's own build the route may cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (shape/with-author {:id "room" :parts [{:id "shell" :box [[8 61 8] [12 65 12]] :want "cobblestone"}]} "Fake")
              world {:self {:pos {:x 11.5 :y 62 :z 9.5}} :blocks glow-cobble-room :light [15 15] :sight true
                     :inventory [{:name "dirt" :count 8} {:name "stone_pickaxe" :count 1} {:name "stone_shovel" :count 1}]}
              {:keys [eng p seen step]} (gt/setup world (ew/of-data {"room" room} {} []))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [14 62 12] :range 2})))]
          (core/submit! eng '(recording-parent) {})
          (await (gt/tick-out! eng 400 step))
          (let [own (map :cell (events-of seen :go-to.dug-own-build))]
            (is (= {:arrived true} @out))
            (is (= 2 (count own)) "one column of its own wall")
            (is (seq (filter #(= :dig-route (:step %)) (events-of seen :go-to.escalated))) "by a dig route")))))))

(def all-glow-room
  "glow-room without the stone band: every cell round the shell is a light, never dug."
  (apply dissoc (merge (box 4 60 4 20 69 16 "glowstone") (box 8 61 8 12 65 12 "dirt"))
         (for [x (range 9 12) y (range 62 65) z (range 9 12)] (str x "," y "," z))))

(deftest go-to-out-of-its-own-room-in-undiggable-rock-stops-cutting-once-the-cuts-show-no-way
  ;; each guess beside a seen light that a look finds a light is evidence: the next route's guesses there are priced
  ;; over the chance they are rock; no caller :worth, so the ways are weighed against each other (past-worth? has its own test)
  ;; and the call ends once none is left, the clock moving
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (shape/with-author {:id "room" :parts [{:id "shell" :box [[8 61 8] [12 65 12]] :want "dirt"}]} "Fake")
              clock (atom 1000000)
              world {:clock clock :self {:pos {:x 11.5 :y 62 :z 9.5}} :blocks all-glow-room :light [15 15] :sight true
                     :inventory [{:name "dirt" :count 8} {:name "stone_pickaxe" :count 1} {:name "stone_shovel" :count 1}]}
              {:keys [eng p seen step]} (gt/setup world (ew/of-data {"room" room} {} []))
              out (atom :not-done)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (recording-parent out 'jobs.movement.go-to {:pos [14 62 12] :range 0})))]
          (core/submit! eng '(recording-parent) {})
          (await (gt/tick-out! eng 400 #(do (swap! clock + 500) (step))))
          (is (<= (count (events-of seen :go-to.dug-own-build)) 6) "a dig route and one stair of its own wall cut, not a column per guess (15 before)")
          (is (= [:dig-route :stair :approach] (map :step (events-of seen :go-to.escalated)))
              "the dig route is not chosen again once the looks showed glowstone")
          (is (= [false :unreachable] ((juxt :arrived :reason) @out)) "the call ends, a failure, with its reason")
          (is (some (comp false? :ok?) (events-of seen :go-to.guess-look)) "a guess look that showed glowstone is told")
          (is (empty? (for [x (range 4 21) y (range 60 70) z (range 4 17)
                            :when (not (and (<= 8 x 12) (<= 61 y 65) (<= 8 z 12)))
                            :when (not= "glowstone" (block p [x y z]))]
                        [x y z (block p [x y z])]))
              "no glowstone is dug, by the dig route or by a stair's cell first seen mid-way"))))))

