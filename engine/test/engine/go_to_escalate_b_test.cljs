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
            [jobs.lib.escape :as escape]
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
  ;; Rowan #22: a 1-wide pocket (two sides walled, pit depth 0), goal on its level behind more than max-door of rock
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

(deftest go-to-never-digs-the-bodys-own-building
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hut (shape/with-author {:id "hut" :parts [{:id "shell" :box [[-3 64 -3] [3 66 3]] :want "dirt"}]} "Fake")
              {:keys [out p seen]} (await (run-job! (merge in-room {:blocks room}) 'jobs.movement.go-to
                                                {:pos [8 64 0] :range 1} {:plans {"hut" hut}}))]
          (is (= false (:arrived @out)))
          (is (= {:step :none :why :own-build} (:escalation @out)) "the wall is the body's own plan")
          (is (empty? (events-of seen :go-to.escalated)))
          (is (empty? (calls p "dig"))))))))

(deftest go-to-never-digs-the-bodys-own-plan-unless-told-to-and-then-warns-of-each-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge in-pit {:blocks pit :plans {"hut" own-pit-wall}})
              {:keys [out p seen]} (await (run-job! world 'jobs.movement.go-to {:pos [10 64 0] :range 1} {:plans (:plans world)}))
              {out2 :out seen2 :seen} (await (run-job! world 'jobs.movement.go-to {:pos [10 64 0] :range 1 :settings {:jobs.lib.access/spare-own-builds false}} {:plans (:plans world)}))
              warned (events-of seen2 :go-to.dug-own-build)]
          (is (= false (:arrived @out)))
          (is (= {:step :none :why :own-build} (:escalation @out)))
          (is (empty? (calls p "dig")))
          (is (= {:arrived true} @out2))
          (is (seq warned) "each own cell dug is named")
          (is (every? #(and (= :warn (:level %)) (= "hut" (:plan %)) (vector? (:cell %))) warned)))))))

(deftest go-to-never-digs-out-of-its-own-pen-holding-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "pen" :owner "Fake" :min [-2 60 -2] :max [2 64 2]}]
              cow {:id 7 :name "cow" :kind "passive" :pos {:x 0.5 :y 61 :z 0.5}}
              go (fn [world zones] (run-job! world 'jobs.movement.go-to {:pos [10 64 0] :range 1} zones))
              {:keys [out p seen]} (await (go (merge in-pit {:blocks pit :entities [cow]}) zones))
              open (await (go (merge in-pit {:blocks pit}) zones))]
          (is (= false (:arrived @out)))
          (is (= {:step :none :why :own-pen} (:escalation @out)) "animals stand in the pit the body owns")
          (is (empty? (calls p "dig")))
          (is (= {:arrived true} @(:out open)) "an own pit with no animals in it is dug out of as before"))))))

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

(deftest go-to-spares-the-wall-of-a-pit-pen-that-is-a-plan-of-the-body-seen-or-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [sight [nil 3]]
          (let [blocks (apply dissoc (merge (box 10 59 10 30 59 22 "grass_block") (box 10 60 10 22 61 22 "stone"))
                              (for [x (range 14 19) z (range 14 19) y [60 61]] (str x "," y "," z)))
                plan (shape/with-author {:id "pit" :parts [{:id "inside" :box [[14 60 14] [18 61 18]] :want [:any "air"]}]} "Fake")
                cows [{:id 7 :name "cow" :kind "passive" :pos {:x 15.5 :y 60 :z 15.5}} {:id 8 :name "cow" :kind "passive" :pos {:x 17.5 :y 60 :z 17.5}}]
                {:keys [out p]} (await (run-job! {:self {:pos {:x 18.4 :y 60 :z 16.5}} :blocks blocks :entities cows :sight sight}
                                                'jobs.movement.go-to {:pos [26 60 16] :range 1} {:plans {"pit" plan}}))]
            (is (= false (:arrived @out)) (str "sight " sight))
            (is (= {:step :none :why :own-pen} (:escalation @out)) (str "sight " sight " " (pr-str @out)))
            (is (empty? (calls p "dig")) (str "sight " sight))))))))

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
                                                    [8 64 0] [3 64 0] false))]
    (is (true? (priced? {})) "open ground: escape/choose has no wall to dig, which is no trap")
    (is (false? (priced? bedrock-pit)) "a pit with nothing to dig is a certain trap")))
