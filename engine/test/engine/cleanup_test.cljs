(ns engine.cleanup-test
  "jobs.access.cleanup: the step decision as a plain function, the standing trigger, then whole cleanups against the
  fake world."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [jobs.lib.ledger :as ledger]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.expr :as expr]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [triggers.access.scaffold-left :as scaffold-left]
            [jobs.lib.threats :as threats]
            [jobs.lib.world-files :as world]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [engine.dig-to-see-test :as d]
            [jobs.lib.dig-look :as look]
            [jobs.access.cleanup :as cleanup]
            [jobs.lib.shelter.keep :as keep]
            [jobs.access.cleanup.step :as cleanup-step]
            [jobs.lib.result :as result]
            [jobs.movement.go-to.result :as go-to-result]))

;; ------------------------------------------------------------------ the step

(defn lookup
  "Air everywhere but the given cells (alternating cell and name), stone under y 64."
  [& cells]
  (let [m (apply hash-map cells)]
    (fn [[_ y _ :as pos]] (get m pos (if (< y 64) "stone" "air")))))

(defn entry [cell & {:as more}]
  (merge {:cell cell :item "dirt" :before "air" :job "j99" :purpose :pillar :state :placed} more))

(defn pillar-cells [from n] (mapv #(update from 1 + %) (range n)))

(defn eye [[x y z]] [(+ x 0.5) (+ y 1.62) (+ z 0.5)])

(defn step [& {:as in}]
  (let [entries (:entries in)
        feet (:feet in [3 64 0])]
    (cleanup-step/next-step (merge {:feet feet :eye (eye feet) :block-at (lookup) :entries entries
                               :ledger (set (map :cell entries)) :zones [] :footprints {} :accept #{:fluid-adjacent}
                               :reach 4.5 :held {}}
                              (dissoc in :feet)))))

(defn column-world [cells] (apply lookup (mapcat (fn [c] [c "dirt"]) cells)))

(deftest on-top-of-its-pillar-it-digs-the-block-under-its-feet
  (let [cells (pillar-cells [0 64 0] 6)]
    (is (= {:step :dig :cell [0 69 0] :item "dirt"}
           (step :feet [0 70 0] :entries (mapv entry cells) :block-at (column-world cells))))))

(deftest a-far-block-under-water-is-held-not-walked-to
  (let [s (step :feet [3 64 0] :entries [(entry [10 64 0])] :block-at (lookup [10 64 0] "dirt" [10 65 0] "water"))]
    (is (= :finish (:step s)))
    (is (= [:submerged] (map :reason (:open s))))))

(deftest a-far-block-the-trigger-would-not-walk-to-is-held-not-walked-to
  (let [walk-blocker #(when (< (get (:cell %) 1) 60) {:reason :far-below})
        s (step :feet [3 64 0] :walk-blocker walk-blocker :entries [(entry [10 50 0]) (entry [10 63 0])]
                :block-at (lookup [10 50 0] "dirt" [10 63 0] "dirt"))]
    (is (= {:step :walk :cell [10 63 0]} s) "the near one is walked to")
    (is (= [:far-below] (map :reason (:open (step :feet [3 64 0] :walk-blocker walk-blocker :entries [(entry [10 50 0])]
                                                  :block-at (lookup [10 50 0] "dirt"))))))))

(deftest cells-under-a-block-that-is-not-its-own-wait
  (let [cells (pillar-cells [0 64 0] 6)
        s (step :feet [0 70 0] :entries (mapv entry (butlast cells)) :block-at (column-world cells))]
    (is (= :finish (:step s)) "the top block is no ledger block: nothing under it is dug")
    (is (= #{:under-body} (set (map :reason (:open s)))))))

(deftest the-block-under-the-feet-is-dug-only-over-a-floor
  (are [below result] (= result ((juxt :step :cell) (step :feet [0 67 0] :entries [(entry [0 66 0])]
                                                          :block-at (lookup [0 66 0] "dirt" [0 65 0] below [0 64 0] "stone"))))
    "stone" [:dig [0 66 0]]
    "air" [:finish nil]
    "water" [:finish nil]
    "magma_block" [:finish nil])
  (is (= [{:cell [0 66 0] :item "dirt" :reason :no-floor-below :block "air"}]
         (:open (step :feet [0 67 0] :entries [(entry [0 66 0])] :block-at (lookup [0 66 0] "dirt" [0 64 0] "stone"))))))

(defn shaft-world
  "A 1x1 shaft through stone whose ground is y 73: dirt at the given cells, stone elsewhere up to y 73, air above."
  [dirt]
  (fn [[x y z :as pos]]
    (cond (some #{pos} dirt) "dirt"
          (and (= [x z] [0 0]) (<= y 73)) "air"
          (<= y 73) "stone"
          :else "air")))

(deftest a-body-on-a-pillar-in-a-shaft-is-not-dug-from-under
  (let [cells [[0 68 0] [0 69 0] [0 70 0]]
        s (step :feet [0 71 0] :entries (mapv entry cells)
                :block-at (shaft-world cells))]
    (is (= :finish (:step s)) "no dig: the walls leave the body no way out of the hole it would fall into")
    (is (= :no-exit (:reason (first (filter #(= [0 70 0] (:cell %)) (:open s))))))))

(deftest a-pillar-top-level-with-the-ground-beside-is-dug-from-on-top
  (let [cells [[0 69 0] [0 70 0]]
        in-world (fn [[x y z :as pos]] (cond (some #{pos} cells) "dirt"
                                             (and (= [x z] [0 0]) (<= y 70)) "air"
                                             (<= y 70) "stone"
                                             :else "air"))]
    ;; feet at 71 standing on [0 70 0]; digging it drops the feet to 70 where the ground (y 70) is solid with air above: a step up
    (is (= [:dig [0 70 0]] ((juxt :step :cell) (step :feet [0 71 0] :entries (mapv entry cells) :block-at in-world))))))

(deftest beside-the-body-the-highest-in-reach-goes-first
  (let [cells [[0 64 0] [0 65 0] [1 64 0]]]
    (is (= [:dig [0 65 0]] ((juxt :step :cell) (step :entries (mapv entry cells) :block-at (column-world cells)))))))

(deftest out-of-reach-it-walks-to-the-nearest
  (let [cells [[12 64 0] [9 64 0]]]
    (is (= {:step :walk :cell [9 64 0]} (step :entries (mapv entry cells) :block-at (column-world cells))))))

(deftest a-cell-held-unreachable-is-not-walked-to-again
  (is (= {:step :finish :open [{:cell [9 64 0] :item "dirt" :reason :unreachable}]}
         (step :entries [(entry [9 64 0])] :block-at (column-world [[9 64 0]]) :held {[9 64 0] {:reason :unreachable}}))))

(deftest refusals-and-unaccepted-hazards-keep-the-entry-open
  (are [in reason] (= reason (:reason (first (:open (apply step (mapcat identity (merge {:entries [(entry [1 64 0])]
                                                                                        :block-at (lookup [1 64 0] "dirt")}
                                                                                       in)))))))
    {:zones [{:name "keep" :min [1 64 0] :max [1 64 0]}]} :zone
    {:footprints {[1 64 0] "farm"}} :footprint
    {:zones nil} :no-zones
    {:block-at (lookup [1 64 0] "dirt" [2 64 0] "lava")} :hazard
    {:block-at (constantly nil)} :not-loaded))

(deftest a-poured-water-source-is-scooped-with-a-bucket-and-needs-one
  (are [bucket? expected] (= expected ((juxt :step :reason) (let [s (step :entries [(entry [1 64 0] :item "water" :purpose :pour)]
                                                                         :block-at (lookup [1 64 0] "water") :bucket? bucket?)]
                                                              (merge s (first (:open s))))))
    true [:dig nil]
    false [:finish :no-bucket]))

(deftest the-footprint-of-a-plan-this-body-made-keeps-its-blocks-unless-told-otherwise
  (are [own-plans spare? result] (= result (:step (step :entries [(entry [1 64 0])] :block-at (lookup [1 64 0] "dirt")
                                                        :footprints {[1 64 0] "farm"} :own-plans own-plans :spare-own-builds spare?)))
    #{"farm"} true :finish
    #{"farm"} false :dig
    #{"other"} false :finish
    #{} true :finish))

(deftest ledger-entries-in-an-own-plan-are-the-plans-not-scaffold
  (let [l [(entry [1 64 0]) (entry [5 64 0]) (entry [9 64 0])]
        footprints {[1 64 0] "hut" [5 64 0] "farm" [9 64 0] "hut"}]
    (are [spare? own cells] (= cells (cleanup-step/plan-cells l footprints own spare?))
      true #{"hut"} #{[1 64 0] [9 64 0]}
      true #{} #{}
      false #{"hut"} #{})))

(deftest water-beside-is-accepted-by-default-lava-is-not
  (is (= :dig (:step (step :entries [(entry [1 64 0])] :block-at (lookup [1 64 0] "dirt" [1 65 0] "water")))))
  (is (= [:lava-adjacent] (:hazards (first (:open (step :entries [(entry [1 64 0])]
                                                        :block-at (lookup [1 64 0] "dirt" [1 65 0] "lava"))))))))

(deftest a-block-near-seen-lava-is-left-open-and-never-walked-to-unless-lava-is-accepted
  (let [near-lava? (fn [[x _ _]] (< x 10))
        far [(entry [30 64 0])]
        near [(entry [5 64 0])]]
    (are [entries accept step-kind] (= step-kind (:step (step :entries entries :lava-near? near-lava? :accept accept)))
      near #{:fluid-adjacent} :finish
      near #{:fluid-adjacent :lava-adjacent} :dig
      far #{:fluid-adjacent} :walk)
    (is (= :lava-near (:reason (first (:open (step :entries near :lava-near? near-lava?))))))))

(deftest a-zone-that-allows-digging-is-no-refusal
  (is (= :dig (:step (step :entries [(entry [1 64 0])] :block-at (lookup [1 64 0] "dirt")
                           :zones [{:name "work" :min [0 60 0] :max [5 70 5] :allow #{:dig}}])))))

(deftest nothing-left-is-a-finish
  (is (= {:step :finish :open []} (step :entries []))))

(deftest a-retreat-plug-stays-while-a-hostile-is-near
  (let [plug (entry [3 64 1] :purpose :retreat-plug)
        pillar (entry [3 65 1])]
    (is (= [{:cell [3 64 1] :item "dirt" :reason :hostile-near}]
           (:open (step :entries [plug] :hostile-near? true :block-at (lookup [3 64 1] "dirt"))))
        "the refuge's plug keeps the hostile off")
    (is (= {:step :dig :cell [3 65 1] :item "dirt"}
           (step :entries [pillar] :hostile-near? true :block-at (lookup [3 65 1] "dirt")))
        "other scaffold is taken back")
    (is (= {:step :dig :cell [3 64 1] :item "dirt"}
           (step :entries [plug] :hostile-near? false :block-at (lookup [3 64 1] "dirt")))
        "the hostile gone, the plug is taken back")))

(deftest a-shelter-seal-stays-while-it-opens-onto-dark-and-while-a-hostile-is-near
  (let [seal (entry [3 64 1] :purpose :shelter)
        plug (entry [3 65 1] :purpose :retreat-plug)
        pillar (entry [3 66 1])
        world (lookup [3 64 1] "dirt" [3 65 1] "dirt" [3 66 1] "dirt")]
    (is (= #{:night} (set (map :reason (:open (step :entries [seal plug] :opens-dark? any? :block-at world))))))
    (is (= [{:cell [3 64 1] :item "dirt" :reason :hostile-near}]
           (:open (step :entries [seal] :hostile-near? true :block-at world))))
    (is (= :dig (:step (step :entries [pillar] :opens-dark? any? :block-at world))) "other scaffold is taken back")
    (is (= :dig (:step (step :entries [seal] :opens-dark? (constantly false) :block-at world))) "opening onto lit cells the seal is taken back")
    (is (= :dig (:step (step :entries [seal] :opens-dark? any? :keep-at-night? false :block-at world))) "unless the night keep is off")))

;; ------------------------------------------------------------------ the trigger

(defn trigger-store [entries jobs]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (ledger/write! s entries)
    (doseq [id jobs] (mem/write! s (mem/job-kind id) {:args {} :children {}} {:cap 1 :ttl :forever}))
    s))

(defn fires?
  ([p s zones] (fires? p s zones {}))
  ([p s zones args] (scaffold-left/scaffold-left p (mem/view s) args (world/of-data {} {} zones))))

(deftest the-trigger-offers-cleanup-for-blocks-no-live-job-owns
  (let [p (tu/fake {:blocks {"0,64,0" "dirt"} :unloaded ["50,64,0"]})]
    (is (true? (fires? p (trigger-store [(entry [0 64 0])] []) [])))
    (is (false? (fires? p (trigger-store [(entry [0 64 0] :job "j4/pillar")] ["j4"]) [])) "the owner is still listed")
    (is (false? (fires? p (trigger-store [] []) [])) "nothing open")
    (is (false? (fires? p (trigger-store [(entry [0 64 0])] []) nil)) "no zone list")
    (is (false? (fires? p (trigger-store [(entry [50 64 0])] []) [])) "the cell is not loaded")
    (is (false? (fires? p (doto (trigger-store [(entry [0 64 0])] []) (ledger/hold! [[0 64 0]])) [])) "held")
    (is (true? (fires? (tu/seeing-after-look p) (trigger-store [(entry [0 64 0])] []) [])) "a loaded cell not yet seen still counts: the run looks at it")))

(deftest the-trigger-leaves-scaffold-beyond-its-radius-for-an-explicit-cleanup
  (let [p (tu/fake {:blocks {"0,64,0" "dirt" "50,64,0" "dirt"}})
        args scaffold-left/defaults]
    (is (= {:near {:radius 32} :checks {:safe? '(not (threatened)) :fit? '(armed)}} args))
    (is (true? (fires? p (trigger-store [(entry [20 64 0])] []) [] args)) "near")
    (is (false? (fires? p (trigger-store [(entry [50 64 0])] []) [] args)) "far: nothing for the reflex")
    (is (false? (fires? p (trigger-store [(entry [50 64 0])] []) [])) "no radius given: the default radius, as a saved register's old spec has")
    (is (= args (:args (:scaffold-left triggers/all))))
    (is (= '(jobs.access.cleanup) (:job (:scaffold-left triggers/all))) "the job's own :orphans default is bounded by the same radius")))

(deftest the-trigger-ignores-scaffold-not-worth-a-walk
  (let [p (tu/fake {:blocks {"0,54,0" "dirt" "0,62,0" "dirt" "10,64,0" "dirt" "10,67,0" "stone" "12,64,0" "dirt"}})
        died (fn [& es] (doto (trigger-store (vec es) []) (mem/write! :died {:pos {:x 14 :y 64 :z 0}} {:cap 1 :ttl :forever})))]
    (is (false? (fires? p (trigger-store [(entry [0 54 0])] []) [])) "ten below")
    (is (true? (fires? p (trigger-store [(entry [0 54 0]) (entry [0 62 0])] []) [])) "another, in reach, still fires")
    (is (false? (fires? p (died (entry [12 64 0])) [])) "within the died radius of a death")
    (is (false? (fires? p (trigger-store [(entry [10 64 0])] []) [])) "under a roof while the body is in the open")))

(deftest the-trigger-waits-for-light-and-for-no-hostile-near
  (let [dirt (trigger-store [(entry [0 64 0])] [])
        night #(tu/with-light-data (tu/fake {:blocks {"0,64,0" "dirt"} :time 14000}))
        hostile (fn [] (doto (tu/fake {:blocks {"0,64,0" "dirt"}})
                         (fake/add-entity! {:id 9 :name "zombie" :kind "hostile" :pos {:x 4 :y 64 :z 0} :health 20})))]
    (is (false? (fires? (night) dirt [])) "dark")
    (is (false? (fires? (night) (trigger-store [(entry [0 64 0] :purpose :retreat-plug)] []) [])) "a retreat plug opening onto dark cells: cleanup keeps it, so no run")
    (is (true? (settings/with-settings {:jobs.lib.shelter.keep/at-night? false}
                 #(fires? (night) (trigger-store [(entry [0 64 0] :purpose :retreat-plug)] []) [])))
        "with the night keep off the plug is taken back in the dark")
    (is (false? (fires? (hostile) dirt [])) "a hostile near")
    (settings/with-settings {:triggers.access.scaffold-left/at-night true :triggers.access.scaffold-left/with-hostile true}
      (fn []
        (is (true? (fires? (night) dirt [])))
        (is (true? (fires? (hostile) dirt [])))))))

(deftest a-body-perched-on-its-pillar-comes-down-in-the-dark
  (let [night #(tu/with-light-data (tu/fake {:self {:pos {:x 0.5 :y 68 :z 0.5}} :blocks {"0,67,0" "dirt" "3,67,0" "dirt"} :time 14000}))
        at (fn [cell] (trigger-store [(entry cell)] []))]
    (is (true? (fires? (night) (at [0 67 0]) [])) "the pillar under the feet")
    (is (false? (fires? (night) (at [3 67 0]) [])) "scaffold beside the body waits for light")))

(deftest a-plug-sealing-the-body-into-a-dark-pocket-does-not-count-the-pocket-as-what-it-opens-onto
  (let [rock {"1,64,0" "dirt" "2,64,0" "stone" "1,63,0" "stone" "1,65,0" "stone" "1,64,1" "stone" "1,64,-1" "stone"}
        plug (trigger-store [(entry [1 64 0] :purpose :retreat-plug)] [])
        night #(tu/with-light-data (tu/fake {:blocks (merge rock %) :time 14000}))]
    (is (true? (fires? (night {}) plug [])) "the plug opens onto the body's own dark cells and solid rock: taken back")
    (is (false? (fires? (night {"2,64,0" "air"}) plug [])) "a dark open cell beyond the plug keeps it")))

(deftest an-unseen-neighbour-is-guessed-dark-only-while-the-bodys-own-cell-is-not-lit
  (let [night #(tu/with-light-data (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks {} :time 14000}))
        lit (fn [light] (let [p (night)] (swap! (fake/state p) merge {:light-default [0 0]} (when light {:light light})) p))
        opens-dark? #(keep/opens-dark? %)]
    (is (true? ((opens-dark? (lit nil)) [3 64 0])) "own cell dark: the unseen side is guessed dark")
    (is (false? ((opens-dark? (lit {[0 64 0] [0 14]})) [3 64 0])) "own cell lit by the opened side: the unseen side is not guessed dark")))

(deftest a-seen-dark-neighbour-keeps-a-shelter-block-only-while-the-body-is-in-the-dark-or-under-a-roof
  (let [world (fn [blocks light] (let [p (tu/with-light-data (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks (merge {"3,64,0" "air" "3,63,0" "stone" "4,64,0" "air" "2,64,0" "air" "3,65,0" "air" "3,64,1" "air" "3,64,-1" "air"} blocks) :time 14000}))]
                                   (aset p "seenBlockAt" (fn [pos] #js {:name (get-in @(fake/state p) [:blocks (str (.-x pos) "," (.-y pos) "," (.-z pos))] "air") :pos pos :age-ms 0}))
                                   (swap! (fake/state p) merge {:light-default [0 0]} {:light (merge (zipmap [[3 64 0] [4 64 0] [2 64 0] [3 65 0] [3 64 1] [3 64 -1]] (repeat [0 0])) light)})
                                   p))
        dark-inside [3 64 0]]
    (is (true? ((keep/opens-dark? (world {} {})) dark-inside)) "body in the dark: the dark cell counts")
    (is (false? ((keep/opens-dark? (world {} {[0 64 0] [0 14]})) dark-inside)) "body in the light outside: the emptied shelter's dark inside does not")
    (is (true? ((keep/opens-dark? (world {"0,66,0" "stone"} {[0 64 0] [0 14]})) dark-inside)) "body under a roof, lit by a torch: still sheltering")))

(deftest a-run-that-removed-a-block-holds-no-night-only-leftovers
  (let [night [{:cell [1 64 0] :reason :night}]
        hostile [{:cell [1 64 0] :reason :hostile-near}]]
    (is (= [] (cleanup/cells-to-hold {:removed [{:cell [2 64 0]}]} night)))
    (is (= hostile (cleanup/cells-to-hold {:removed [{:cell [2 64 0]}]} hostile)))
    (is (= night (cleanup/cells-to-hold {:removed []} night)))))

(deftest the-trigger-waits-for-no-hostile-not-the-clock
  (let [dirt (trigger-store [(entry [0 64 0])] [])
        at (fn [t] (tu/fake {:blocks {"0,64,0" "dirt"} :time t}))]
    (is (true? (fires? (at 400) dirt [])) "no clock grace")
    (is (true? (fires? (at 1000) dirt [])))
    (is (false? (fires? (doto (at 1000) (fake/add-entity! {:id 9 :name "zombie" :kind "hostile" :pos {:x 30 :y 64 :z 0} :health 20})) dirt []))
        "a hostile within the dawn radius, past the 16 the old check looked at")))

(deftest the-trigger-holds-off-after-a-flight-from-a-mob-that-is-still-there
  (let [mob {:id 9 :name "skeleton" :kind "hostile" :pos {:x 80 :y 64 :z 0} :health 20}
        p (doto (tu/fake {:blocks {"0,64,0" "dirt"}}) (fake/add-entity! mob))
        fled (doto (trigger-store [(entry [0 64 0])] [])
               (mem/write! :threat {:mob "skeleton" :id 9 :pos {:x 8 :y 64 :z 0}} threats/threat-policy))]
    (is (false? (fires? p fled [])) "a skeleton fled from a moment ago is out of sight, not gone")
    (is (true? (settings/with-settings {::threats/fled-memory-ms 0} #(fires? p fled []))))
    (is (true? (fires? (tu/fake {:blocks {"0,64,0" "dirt"}}) fled [])) "the skeleton was killed: its flight holds nothing off")))

(deftest cleanup-near-defaults-to-the-body-and-orphans-to-32
  (let [c {:primitives (tu/fake {:blocks {"0,64,0" "dirt"}})}]
    (is (= [5 #{:x :y :z}] ((juxt :radius (comp set keys :pos)) (ledger/near-of-args c {:near {:radius 5}}))) "no :pos: the body")
    (is (= {:pos {:x 9 :y 9 :z 9} :radius 5} (ledger/near-of-args c {:near {:pos {:x 9 :y 9 :z 9} :radius 5}})))
    (is (= 32 (:radius (ledger/near-of-args c {}))) "orphans: 32 by default")
    (is (= 32 (:radius (ledger/near-of-args c {:of :orphans}))))
    (is (nil? (ledger/near-of-args c {:of :all})))
    (is (nil? (ledger/near-of-args c {:of "j3"})))))

(deftest the-trigger-is-a-builtin
  (is (= scaffold-left/scaffold-left (:when (:scaffold-left triggers/all))))
  (is (= '(jobs.access.cleanup) (:job (:scaffold-left triggers/all)))))

(defn blocker [& {:as in}]
  (ledger/walk-blocker (merge {:feet [3 64 0] :block-at (constantly "air") :ledger #{} :max-below 4 :roof-height 4} in) (entry (:cell in [10 64 0]))))

(deftest walk-blocker-says-why-a-scaffold-is-not-worth-a-walk
  (is (= {:reason :far-below} (blocker :feet [3 70 0] :cell [10 60 0])))
  (is (nil? (blocker :feet [3 62 0] :cell [10 60 0])) "two below is a walk")
  (is (nil? (blocker :feet [3 66 0] :cell [10 60 0] :max-below 8)))
  (is (= {:reason :died-near} (blocker :died-near? #(= [10 64 0] %))))
  (is (= {:reason :submerged} (blocker :block-at (lookup [10 64 0] "dirt" [10 65 0] "water"))) "a block under water is a dive away")
  (let [roofed (lookup [10 64 0] "dirt" [10 67 0] "stone")]
    (is (= {:reason :roofed} (blocker :block-at roofed)))
    (is (nil? (blocker :block-at (lookup [10 64 0] "dirt" [10 67 0] "stone" [3 67 0] "stone"))) "a body under a roof itself has nothing to fear in a roof")
    (is (nil? (blocker :block-at (lookup [10 64 0] "dirt" [10 70 0] "stone"))) "a roof out of range is open sky")))

;; ------------------------------------------------------------------ against the fake world

(defn row-world
  "A pit (air at y 62 and 63) from x 1 to 3 with stone either side and the given row cells as dirt at y 63."
  [row-xs]
  (fn [[x y z]]
    (cond
      (and (<= 1 x 3) (= z 0) (= y 63) (some #{x} row-xs)) "dirt"
      (and (<= 1 x 3) (<= -2 z 2) (<= y 63)) "air"
      (< y 64) "stone"
      :else "air")))

(defn row-entries [xs] (mapv #(entry [% 63 0] :purpose :bridge) xs))

(deftest on-a-bridge-it-never-digs-the-way-back-to-the-ground
  (let [s (step :feet [2 64 0] :entries (row-entries [1 2 3]) :block-at (row-world [1 2 3]))]
    (is (= {:step :dig :cell [1 63 0]} (select-keys s [:step :cell])) "one side may go while the other holds")
    (let [s (step :feet [2 64 0] :entries (row-entries [2 3]) :block-at (row-world [2 3]))]
      (is (= {:step :retreat :to [3 64 0]} s) "x 3 is the way back: not dug, the body steps back onto it so x 2 can be dug"))))

(deftest the-last-bridge-cell-over-a-drop-is-dug-from-the-ground-beside-it
  (let [s (step :feet [3 64 0] :entries (row-entries [3]) :block-at (row-world [3]))]
    (is (= {:step :retreat :to [4 64 0]} s) "the floor under the feet has no floor below: the body steps off first")))

(deftest a-floor-cell-over-an-unseen-cell-is-not-dug-from-on-top
  (let [guess (fn [[x y z]] (if (= [2 63 0] [x y z]) "dirt" (if (< y 64) "stone" "air")))
        s (step :feet [2 64 0] :entries [(entry [2 63 0] :purpose :bridge)] :block-at guess :floor-at (constantly nil))]
    (is (not= :dig (:step s)) (pr-str s))))

(deftest ground-walled-at-head-height-is-no-way-off-the-row
  (let [walled (fn [[x y z :as c]] (if (= [4 64 0] c) "stone" ((row-world [2 3]) c)))
        s (step :feet [3 64 0] :entries (row-entries [2 3]) :block-at walled)]
    (is (not= :retreat (:step s)))))

(deftest a-pillar-top-is-no-bridge
  (let [cells (pillar-cells [0 64 0] 3)]
    (is (= :dig (:step (step :feet [0 67 0] :entries (mapv entry cells) :block-at (column-world cells)))))))

(deftest off-the-bridge-the-row-is-dug-nearest-first
  (let [s (step :feet [0 64 0] :entries (row-entries [1 2 3]) :block-at (row-world [1 2 3]))]
    (is (= [:dig [1 63 0]] ((juxt :step :cell) s)))))

(def job 'jobs.access.cleanup)

(defn cell-key [[x y z]] (str x "," y "," z))

(defn floor
  "Stone at y 63 over x0..x1, z -2..2."
  [x0 x1]
  (into {} (for [x (range x0 (inc x1)) z (range -2 3)] [(cell-key [x 63 z]) "stone"])))

(defn block [p cell] (get (:blocks @(fake/state p)) (vec cell)))
(defn feet [p] (let [pos (.-pos (.self p))] [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]))
(defn the-ledger [eng] (ledger/open-entries (mem/view (:store eng))))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn fall!
  "The fake has no gravity: drop the body onto the next block below."
  [p]
  (let [[x _ z :as pos] (:pos (fake/self p))]
    (loop [y (nth pos 1)]
      (if (or (<= y 0) (some? (block p [(js/Math.floor x) (dec y) (js/Math.floor z)])))
        (fake/swap-self! p assoc :pos [x y z])
        (recur (dec y))))))

(defn record-digs!
  "Every dig records [cell feet-before] in digs and lets the body fall after it."
  [p digs]
  (.override (.-world p) "dig"
             (fn ^:async f [token args impl]
               (let [pos (.-pos args)]
                 (swap! digs conj [[(.-x pos) (.-y pos) (.-z pos)] (feet p)])
                 (let [r (await (impl token args))]
                   (fall! p)
                   r)))))

(defn setup
  "An engine over the fake world with the ledger written into body memory; a recording parent (j1) runs cleanup with
  args as its child and keeps its result in :out. :make starts another engine on the same body."
  [{:keys [self blocks entries zones args drops jobs] :or {zones []}}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        dir (tu/tmp-dir)
        p (tu/fake (cond-> {:self {:pos self} :blocks blocks :inventory [{:name "iron_sword" :count 1}]} drops (assoc :drops drops)))
        out (atom :not-done)
        w (world/of-data {} {} zones)
        parent (tu/recording-parent job (or args {}) out)
        make (fn [] (core/create {:primitives p :jobs (merge registry/jobs jobs {'recording-parent parent})
                                  :triggers triggers/all :dir dir :now #(deref clock) :world w
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}))
        eng (make)
        digs (atom [])]
    (ledger/write! (:store eng) entries)
    (record-digs! p digs)
    ;; the fake's collect puts the body where the drop was spawned, mid-air for a pillar's; keep it where it stood
    (.override (.-world p) "collect"
               (fn ^:async f [token args impl]
                 (let [pos (:pos (fake/self p))
                       r (await (impl token args))]
                   (fake/swap-self! p assoc :pos pos)
                   r)))
    {:eng eng :make make :p p :clock clock :seen seen :out out :digs digs}))

(defn ^:async ticks [{:keys [clock]} eng n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn pillar-blocks [cells] (into (merge (floor -2 12)) (map (fn [c] [(cell-key c) "dirt"])) cells))

(deftest a-six-high-pillar-is-dug-from-on-top-descending-with-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (pillar-cells [0 64 0] 6)
              {:keys [eng p out digs seen] :as s} (setup {:self {:x 0.5 :y 70 :z 0.5} :blocks (pillar-blocks cells)
                                                          :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= (mapv (fn [c] [c (update c 1 inc)]) (reverse cells)) @digs) "top-down, each from on top of it")
          (is (every? nil? (map #(block p %) cells)))
          (is (= [] (the-ledger eng)))
          (is (= (mapv (fn [c] {:cell c :item "dirt"}) (reverse cells)) (:removed @out)))
          (is (= [] (:open @out)))
          (is (= 6 (:collected @out)))
          (is (= :done (:status @out))))))))

(deftest every-removal-goes-through-the-dig-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0] [10 64 0]]
              {:keys [eng p seen] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                 :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= 3 (count (filter #(= "dig" (.-name %)) (.-calls (.-world p))))))
          (is (= 3 (count (ce/ends seen :dug)))))))))

(deftest a-removed-wall-torch-is-collected-as-the-torch-it-drops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"0,64,0" "wall_torch"})
                                                :drops {"wall_torch" "torch"}
                                                :entries [(entry [0 64 0] :item "wall_torch")]})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (nil? (block p [0 64 0])))
          (is (= [{:cell [0 64 0] :item "wall_torch"}] (:removed @out)))
          (is (= 1 (:collected @out)) "the torch drop, not a wall_torch, is what the collect looks for")
          (is (= [] (the-ledger eng))))))))

(deftest a-sweep-that-finds-fewer-drops-than-cells-removed-sweeps-again-a-few-times
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0]]
              {:keys [eng out p] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                              :drops {"dirt" []}
                                              :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= 0 (:collected @out)))
          (is (= :done (:status @out)))
          (is (= (cleanup/collect-rounds)
                 (count (filter #(= "wait" (.-name %)) (.-calls (.-world p)))))))))))

(deftest side-blocks-are-dug-from-the-ground-and-a-far-one-after-a-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0] [10 64 0]]
              {:keys [eng p out digs] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                     :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= [[0 65 0] [0 64 0] [10 64 0]] (mapv first @digs)))
          (is (= [3 64 0] (second (first @digs))) "the first two from where it stood")
          (is (not-any? (fn [[cell f]] (= f (update cell 1 inc))) @digs) "never from on top of the block")
          (is (every? nil? (map #(block p %) cells)))
          (is (= [] (the-ledger eng)))
          (is (= 3 (count (:removed @out)))))))))

(deftest an-unseen-cell-reads-as-the-hidden-guess-and-a-seen-one-as-its-block
  (let [p (d/sensing {:blocks (assoc d/ground "0,64,0" "dirt")})
        block-at (access/sensed-at p blocks/hidden-guess)]
    (is (true? (look/unknown? p [3 64 0])))
    (is (= blocks/hidden-guess (block-at [3 64 0])) "unseen: the guess, not the entry's block")
    (.feel p (clj->js {:x 0 :y 64 :z 0}))
    (.glance p (clj->js {:x 0 :y 64 :z 0}))
    (is (false? (look/unknown? p [0 64 0])))
    (is (= "dirt" (block-at [0 64 0])))))

(deftest a-swapped-or-vanished-cell-is-never-dug-and-its-entry-dropped-with-a-note
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out digs seen] :as s}
              (setup {:self {:x 3.5 :y 64 :z 0.5}
                      :blocks (merge (floor -2 5) {"0,64,0" "stone" "1,64,0" "dirt"})
                      :entries [(entry [0 64 0]) (entry [0 65 0]) (entry [1 64 0])]})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [[1 64 0]] (mapv first @digs)))
          (is (= "stone" (block p [0 64 0])))
          (is (= [] (the-ledger eng)))
          (is (= [{:cell [0 64 0] :item "dirt" :found "stone"} {:cell [0 65 0] :item "dirt" :found "air"}] (:dropped @out)))
          (is (= [[0 64 0] [0 65 0]] (mapv :cell (of-kind seen :cleanup.dropped)))))))))

(deftest dirt-that-turned-to-grass-is-still-ours-and-is-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out digs] :as s}
              (setup {:self {:x 3.5 :y 64 :z 0.5}
                      :blocks (merge (floor -2 5) {"1,64,0" "grass_block"})
                      :entries [(entry [1 64 0])]})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [[1 64 0]] (mapv first @digs)))
          (is (nil? (block p [1 64 0])))
          (is (= [] (:dropped @out))))))))

(deftest a-restart-between-the-intent-and-the-dig-digs-the-block-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0]]
              {:keys [eng make p out] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                     :entries (mapv entry cells)})]
          (.hold (.-world p) "dig")
          (core/submit! eng '(recording-parent) {})
          (core/tick! eng)
          (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
          (is (= :removing (:state (ledger/entry-at (the-ledger eng) [0 65 0]))) "the intent was saved before the call")
          (core/shutdown! eng)
          (let [again (make)]
            (await (ticks s again 40))
            (is (every? nil? (map #(block p %) cells)))
            (is (= [] (the-ledger again)))
            (is (= [[0 65 0] [0 64 0]] (mapv :cell (:removed @out))))))))))

(deftest a-restart-after-the-dig-but-before-its-result-counts-it-removed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0]]
              {:keys [eng make p out digs] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                          :entries (mapv entry cells)})]
          (.override (.-world p) "dig" (fn [token args impl] (impl token args) (js/Promise. (fn [_ _]))))
          (core/submit! eng '(recording-parent) {})
          (core/tick! eng)
          (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
          (core/shutdown! eng)
          (is (nil? (block p [0 65 0])))
          (is (= :removing (:state (ledger/entry-at (the-ledger eng) [0 65 0]))))
          (record-digs! p digs)
          (let [again (make)]
            (await (ticks s again 40))
            (is (= [[0 64 0]] (mapv first @digs)) "the dug block is not dug again")
            (is (= [] (the-ledger again)))
            (is (= #{[0 64 0] [0 65 0]} (set (map :cell (:removed @out)))))))))))

(deftest a-zone-refusal-keeps-the-entry-and-reports-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0]]
              {:keys [eng p out digs seen] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                          :entries (mapv entry cells)
                                                          :zones [{:name "keep" :min [0 65 0] :max [0 65 0]}]})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [[0 64 0]] (mapv first @digs)))
          (is (= "dirt" (block p [0 65 0])))
          (is (= [[0 65 0]] (mapv :cell (the-ledger eng))))
          (is (= [{:cell [0 65 0] :item "dirt" :reason :zone :zone "keep"}] (:open @out)))
          (is (re-find #"left 1 blocks: \[0 65 0\] zone" (:text @out)))
          (is (= #{[0 65 0]} (ledger/held-cells (mem/view (:store eng))))))))))

(deftest only-the-named-instance-and-its-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [1 64 0]]
              {:keys [eng p out] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                :entries [(entry [0 64 0] :job "j1/pillar") (entry [1 64 0] :job "j77")]
                                                :args {:of "j1"}})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (nil? (block p [0 64 0])))
          (is (= "dirt" (block p [1 64 0])))
          (is (= [[1 64 0]] (mapv :cell (the-ledger eng))))
          (is (= [[0 64 0]] (mapv :cell (:removed @out)))))))))

(deftest a-column-over-a-hole-stops-where-the-floor-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (pillar-cells [0 65 0] 3)
              {:keys [eng p out digs] :as s} (setup {:self {:x 0.5 :y 68 :z 0.5}
                                                     :blocks (dissoc (pillar-blocks cells) "0,63,0")
                                                     :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [[[0 67 0] [0 68 0]] [[0 66 0] [0 67 0]]] @digs))
          (is (= "dirt" (block p [0 65 0])))
          (is (= [{:cell [0 65 0] :item "dirt" :reason :no-floor-below :block "air"}] (:open @out)))
          (is (= [:nothing-possible :left-open] ((juxt :kind :cause) (:ending @out)))))))))

(deftest cleanup-is-a-contract-job
  (let [j (get registry/jobs job)]
    (is (= [:of :near] (:target j)))
    (is (some? (:defaults j)))
    (is (fn? (:quote-of j)))))

(deftest a-bad-of-argument-is-impossible
  (are [of cause] (= cause (tu/kind-reason (cleanup/bad-args {:args {:of of}})))
    5 [:never :bad-args]
    :orphans [:ok :args]
    :all [:ok :args]
    "j3" [:ok :args]))

(deftest nothing-open-ends-done-without-a-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 0.5 :y 64 :z 0.5} :blocks (floor -2 2) :entries []})]
          (core/submit! eng (list job) {})
          (await (ticks s eng 20))
          (is (empty? (:list (core/state eng))) "ended at once: the goal already holds")
          (is (= 0 (.-length (.-calls (.-world p))))))))))

(defn done-of
  "The kind and reason of cleanup's done? for a started job with memory m over an empty ledger."
  [m]
  (let [{:keys [eng p]} (setup {:self {:x 0.5 :y 64 :z 0.5} :blocks (floor -2 2) :entries []})
        store (:store eng)]
    (mem/write! store (mem/job-kind "j") m)
    (tu/kind-reason (cleanup/done? {:primitives p :args (second (expr/leaf registry/jobs job {})) :root "j" :slots []
                                    :id "j" :engine {:warned (atom #{})} :emit (fn [& _])
                                    :view #(mem/view store)}))))

(deftest a-started-cleanup-with-nothing-left-offered-is-done
  (are [m expected] (= expected (done-of m))
    {:started true} [:ok :nothing-to-do]
    {:started true :collect true} [:later :cleaning]))

(deftest held-cells-are-not-offered-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks [[0 64 0]])
                                            :entries [(entry [0 64 0])]
                                            :zones [{:name "keep" :min [0 64 0] :max [0 64 0]}]})]
          (core/submit! eng (list job) {})
          (await (ticks s eng 10))
          (is (empty? (:list (core/state eng))))
          (core/submit! eng (list job) {})
          (let [n (.-length (.-calls (.-world p)))]
            (await (ticks s eng 10))
            (is (empty? (:list (core/state eng))) "the second ends done: nothing is offered")
            (is (= n (.-length (.-calls (.-world p)))))))))))

(deftest no-zone-list-declines-without-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks [[0 64 0]])
                                                 :entries [(entry [0 64 0])] :zones nil})]
          (core/submit! eng (list job) {})
          (await (ticks s eng 10))
          (is (= "dirt" (block p [0 64 0])))
          (is (empty? (of-kind seen :cleanup.declined))))))))

(deftest a-block-that-needs-a-pickaxe-is-held-no-tool-without-one
  (are [carried reason] (= reason (:reason (first (:open (step :entries [(entry [1 64 0] :item "cobblestone")]
                                                              :block-at (lookup [1 64 0] "cobblestone")
                                                              :can-clear? #(or (not= % "cobblestone") carried))))))
    false :no-tool
    true nil))

(deftest cleanup-of-cobblestone-without-a-pickaxe-digs-nothing-and-leaves-it-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out digs] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"0,64,0" "cobblestone"})
                                                    :entries [(entry [0 64 0] :item "cobblestone")]})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [] @digs))
          (is (= "cobblestone" (block p [0 64 0])))
          (is (= [{:cell [0 64 0] :item "cobblestone" :reason :no-tool :block "cobblestone"}] (:open @out))))))))

(deftest a-pickaxe-whose-break-reaches-the-inventory-after-the-dig-still-says-tool-broke-and-tool-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"0,64,0" "stone"})
                                                :entries [(entry [0 64 0] :item "stone")]})]
          (fake/add-item! p "stone_pickaxe" 1)
          (swap! (fake/state p) update :inventory (fn [inv] (mapv #(assoc % :durability 1 :max-durability 131) inv)))
          ;; the inventory update of the break arrives after the dig resolved: it is gone on the first wait after it
          (.override (.-world p) "wait"
                     (fn ^:async f [token args impl]
                       (swap! (fake/state p) assoc :inventory [])
                       (await (impl token args))))
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= 1 (count (of-kind seen :tool.broke))))
          (is (= 1 (count (of-kind seen :tool.none)))))))))

(deftest a-pickaxe-that-breaks-in-the-dig-says-tool-broke-and-tool-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"0,64,0" "stone"})
                                                :entries [(entry [0 64 0] :item "stone")]})]
          (fake/add-item! p "stone_pickaxe" 1)
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (swap! (fake/state p) assoc :inventory [])
                         r)))
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= 1 (count (of-kind seen :tool.broke))))
          (is (= 1 (count (of-kind seen :tool.none)))))))))

(deftest a-poured-water-source-is-scooped-by-the-bucket-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out]} (setup {:self {:x 0.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"1,64,0" "water"})
                                          :entries [(entry [1 64 0] :item "water" :purpose :pour)]})]
          (swap! (fake/state p) update :inventory conj {:name "bucket" :count 1})
          (core/submit! eng '(recording-parent) {})
          (await (ticks {:clock (atom 1000000)} eng 5))
          (is (= [] (:list (core/state eng))))
          (is (= [{:cell [1 64 0] :item "water"}] (:removed @out)))
          (is (not= "water" (block p [1 64 0])))
          (is (= [] (the-ledger eng)))
          (is (= [] (filter #(= "dig" (.-name %)) (.-calls (.-world p)))) "scooped, not dug"))))))

(deftest a-pillar-is-cleaned-up-in-one-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (pillar-cells [0 64 0] 6)
              {:keys [eng p out] :as s} (setup {:self {:x 0.5 :y 70 :z 0.5} :blocks (pillar-blocks cells)
                                                :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "one tick ends the run")
          (is (every? nil? (map #(block p %) cells)))
          (is (= 6 (count (:removed @out)))))))))

;; ------------------------------------------------------------------ a failed dig counts toward :give-up

(defn ^:async failing-dig
  "Cleanup of one dirt block where the world fails the first fail-n digs (a timeout); resolves the run's state."
  [fail-n]
  (let [{:keys [eng p out] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"0,64,0" "dirt"})
                                          :entries [(entry [0 64 0])]})
        fails (fn ^:async fail [_ _ _] #js {:status "timeout"})
        real (fn ^:async real [token args impl] (await (impl token args)))
        digs (atom (concat (repeat fail-n fails) (repeat real)))]
    (.override (.-world p) "dig"
               (fn ^:async f [token args impl]
                 (await ((ffirst (swap-vals! digs rest)) token args impl))))
    (core/submit! eng '(recording-parent) {})
    (await (ticks s eng 30))
    {:eng eng :p p :out out}))

(deftest a-failed-dig-for-one-pass-does-not-hold-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out]} (await (failing-dig 1))]
          (is (nil? (block p [0 64 0])) "dug on the next pass")
          (is (= [] (the-ledger eng)))
          (is (= [] (:open @out)))
          (is (= #{} (ledger/held-cells (mem/view (:store eng))))))))))

(deftest a-failed-dig-every-pass-holds-the-cell-after-give-up-passes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out]} (await (failing-dig 99))]
          (is (= "dirt" (block p [0 64 0])))
          (is (= [:dig-failed] (mapv :reason (:open @out))))
          (is (= #{[0 64 0]} (ledger/held-cells (mem/view (:store eng))))))))))

(deftest only-a-permanent-walk-verdict-holds-a-cell-unreachable
  (are [e permanent?] (= permanent? (result/permanent-walk? e))
    {:kind :nothing-possible :cause {:reason :unreachable :why :abilities}} true
    {:kind :nothing-possible :cause {:reason :unreachable :why :goal-enclosed}} true
    {:kind :nothing-possible :cause {:reason :unreachable :why :goal-cut-off}} true
    {:kind :nothing-possible :cause {:reason :unreachable :why :one-way}} true
    {:kind :nothing-possible :cause {:reason :unreachable :why :exhausted}} true
    {:kind :nothing-possible :cause :bad-pos} true
    {:kind :nothing-possible :cause :unsupported} true
    {:kind :nothing-possible :cause {:reason :unreachable :why :door-stuck}} false
    {:kind :nothing-possible :cause {:reason :unreachable :why :moved-while-searching}} false
    {:kind :nothing-possible :cause {:reason :unreachable :why :stuck}} false
    {:kind :nothing-possible :cause {:reason :unreachable :why :no-progress}} false
    {:kind :nothing-possible :cause :unreachable} false
    {:kind :done :data {:arrived true}} false
    :continue false
    nil false))

(defn gave-up [why] {:status :stopped :reason :unreachable :why why})

(deftest the-walk-verdicts-read-go-tos-real-endings
  (are [result permanent? unreachable?]
       (let [e (go-to-result/ending nil result)]
         (and (= permanent? (result/permanent-walk? e)) (= unreachable? (result/unreachable-walk? e))))
    {:status :stopped :reason :bad-pos} true true
    {:status :stopped :reason :unsupported} true true
    (gave-up :abilities) true true
    (gave-up :goal-enclosed) true true
    (gave-up :goal-cut-off) true true
    (gave-up :one-way) true true
    (gave-up :goal-not-standable) true true
    (gave-up :goal-on-magma) true true
    (gave-up :exhausted) true true
    (gave-up :off-plan) false true
    (gave-up :needs-health) false true
    (gave-up :door-stuck) false false
    (gave-up :stuck) false false
    (gave-up :no-progress) false false
    (gave-up :moved-while-searching) false false
    (gave-up :search-spent) false false
    {:status :stopped :reason :unreachable :leg true} false false
    {:status :done :arrived true} false false))

(defn stub-go-to
  "A go-to that records its args and the call's :ways in calls and ends with result."
  [calls result]
  {:target [] :ways #{:walk :clear-seal}
   :round (fn [c]
            (swap! calls conj (merge (:args c) (select-keys (ctx/frozen-call c) [:ways])))
            (if (= :stopped (:status result))
              (ctx/end c :nothing-possible {:reason (:reason result) :why (:why result)} (dissoc result :status :reason))
              (ctx/end c :done nil result)))})

(defn ^:async walk-run
  "Cleanup of entries (default one 9 cells away) over blocks (default a dirt block at each) with go-to stubbed to end with result:
  {:calls go-to's args :open reasons}."
  ([result] (walk-run result [[9 64 0]] (pillar-blocks [[9 64 0]])))
  ([result cells blocks]
  (let [calls (atom [])
        {:keys [eng out] :as s} (setup {:self {:x 0.5 :y 64 :z 0.5} :blocks blocks :entries (mapv entry cells)
                                        :jobs {'jobs.movement.go-to (stub-go-to calls result)}})]
    (core/submit! eng '(recording-parent) {})
    (await (ticks s eng 40))
    {:calls @calls :open (mapv :reason (:open @out))})))

(deftest cleanup-walks-through-go-to-never-digging-a-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [calls]} (await (walk-run {:status :stopped :arrived false :reason :unreachable :why :door-stuck}))]
          (is (seq calls))
          (is (every? #(= {:pos {:x 9 :y 64 :z 0} :range 3 :ways #{:walk}} (select-keys % [:pos :range :ways])) calls)))))))

(deftest a-permanent-go-to-failure-holds-the-cell-unreachable-after-one-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [calls open]} (await (walk-run {:status :stopped :arrived false :reason :unreachable :why :goal-enclosed}))]
          (is (= 1 (count calls)))
          (is (= [:unreachable] open)))))))

(def stair-up
  "A stair of dirt from the ground at x 3 up to y 69 at x 9, one step a block."
  (into {} (map (fn [i] [(cell-key [(+ 3 i) (+ 63 i) 0]) "dirt"])) (range 7)))

(deftest a-scaffold-no-walk-reaches-is-held-no-way-with-no-walk-made
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cell [9 70 0]
              {:keys [calls open]} (await (walk-run {:status :done :arrived true} [cell] (assoc (pillar-blocks []) (cell-key cell) "dirt")))]
          (is (= [] calls) "a block in the air: no go-to, no walk out for it")
          (is (= [:no-way] open)))))))

(deftest the-top-of-an-abandoned-stair-is-walked-to-however-high
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cell [9 69 0]
              {:keys [calls]} (await (walk-run {:status :done :arrived true} [cell] (merge (pillar-blocks []) stair-up)))]
          (is (seq calls) "a walk climbs the stair: go-to is asked"))))))

(deftest a-transient-go-to-failure-is-retried-until-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [calls open]} (await (walk-run {:status :stopped :arrived false :reason :unreachable :why :door-stuck}))]
          (is (= 2 (count calls)))
          (is (= [:out-of-reach] open)))))))

(deftest a-bridge-over-a-pit-is-taken-back-and-the-body-ends-on-the-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blocks (into {} (for [x (range -2 8) z (range -2 3) y (range 60 64)
                                    :let [v (row-world [1 2 3])]
                                    :when (not= "air" (v [x y z]))]
                                [(cell-key [x y z]) (v [x y z])]))
              {:keys [eng p out seen] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks blocks
                                                     :entries (row-entries [1 2 3])})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 80))
          (is (every? nil? (map #(block p [% 63 0]) [1 2 3])))
          (is (= [] (the-ledger eng)))
          (is (= [] (:open @out)))
          (is (#{[0 64 0] [4 64 0]} (feet p)) "on the ground, not in the pit")
          (is (= 3 (count (:removed @out)))))))))


(deftest an-unseen-swapped-cell-is-looked-at-first-and-never-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (d/sensing {:inventory [{:name "iron_sword" :count 1}] :self {:pos {:x 3.5 :y 65 :z 0.5}} :blocks (assoc d/ground "0,65,0" "oak_planks" "2,65,0" "dirt")})
              _ (.setOwner p "t1")
              _ (await (.look p "t1" #js {:pos #js {:x 2.5 :y 65.5 :z 0.5}}))
              s (d/setup p 'jobs.access.cleanup {:ignore-zones? true})]
          (ledger/write! (:store (:eng s)) [(entry [0 65 0]) (entry [2 65 0])])
          (await (d/tick-out! s))
          (is (= [[2 65 0]] (mapv (juxt :x :y :z) (d/digs p))) "only the seen scaffold is dug; the unseen cell is looked at first, found swapped, left")
          (is (= "oak_planks" (d/block-at p [0 65 0])))
          (is (= [] (ledger/open-entries (mem/view (:store (:eng s))))) "the entry is dropped once the cell is seen"))))))

(deftest it-looks-around-before-it-declines-cells-it-has-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"0,64,0" "dirt"})
                                                :entries [(entry [0 64 0])]})
              _ (tu/seeing-after-look p)]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (some #(= "look" (.-name %)) (.-calls (.-world p))))
          (is (= [{:cell [0 64 0] :item "dirt"}] (:removed @out))))))))

(deftest a-retreat-plug-beside-a-heard-skeleton-is-left-in-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[2 64 0] [2 65 0]]
              {:keys [eng p out digs] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                     :entries [(entry [2 64 0] :purpose :retreat-plug) (entry [2 65 0] :purpose :retreat-plug)]})]
          (fake/add-entity! p {:id 9 :name "skeleton" :kind "hostile" :pos {:x -4 :y 64 :z 0} :health 20})
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= [] @digs) "Juniper j658: the plug dug next to the skeleton let it in")
          (is (= #{:hostile-near} (set (map :reason (:open @out))))))))))

(deftest a-far-scaffold-block-beside-a-known-hostile-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[10 64 0]]
              {:keys [eng p out digs] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                     :entries (mapv entry cells)})]
          (fake/add-entity! p {:id 9 :name "zombie" :kind "hostile" :pos {:x 12 :y 64 :z 0} :health 20})
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= [] @digs))
          (is (= 3 (js/Math.floor (.-x (.-pos (.self p))))) "the body stayed where it was")
          (is (= [:danger] (mapv :reason (:open @out)))))))))

(deftest a-far-scaffold-block-beside-seen-lava-is-not-walked-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[10 64 0]]
              {:keys [eng p out digs] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (assoc (pillar-blocks cells) "12,64,0" "lava")
                                                     :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= [] @digs))
          (is (= 3 (js/Math.floor (.-x (.-pos (.self p))))) "the body stayed where it was")
          (is (= [:lava-near] (mapv :reason (:open @out)))))))))

(deftest as-a-reflex-it-works-after-its-look-around-instead-of-declining
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (merge (floor -2 5) {"0,64,0" "dirt"})
                                                 :entries [(entry [0 64 0])]})
              _ (tu/seeing-after-look p)]
          (core/register-reflex! eng {:trigger :scaffold-left :args {} :job (list job {})})
          (loop [i 0]
            (swap! (:clock s) + 500)
            (await (core/tick! eng))
            (when (< i 60) (recur (inc i))))
          (is (empty? (filter #(= :declined (:kind %)) @seen)) "a reflex job is not told to continue")
          (is (= [] (the-ledger eng)) "the scaffold is taken back"))))))

(deftest a-body-enclosed-by-leaves-digs-its-pillar-from-under
  (let [cells [[0 68 0] [0 69 0] [0 70 0]]
        world (shaft-world cells)
        leaves #{[1 70 0] [1 71 0]}
        s (step :feet [0 71 0] :entries (mapv entry cells)
                :block-at #(if (leaves %) "oak_leaves" (world %)))]
    (is (= [:dig [0 70 0]] ((juxt :step :cell) s)) "leaves are dug through, not a wall")))

(deftest the-live-respawn-bed-is-never-listed-or-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (d/sensing {:inventory [{:name "iron_sword" :count 1}] :self {:pos {:x 3.5 :y 65 :z 0.5}} :blocks (assoc d/ground "0,65,0" "white_bed" "2,65,0" "dirt")})
              s (d/setup p 'jobs.access.cleanup {:ignore-zones? true :of :all})
              store (:store (:eng s))]
          (ledger/write! store [(entry [0 65 0] :item "white_bed" :purpose :bed) (entry [2 65 0])])
          (mem/write! store :spawn-bed {:pos {:x 1 :y 65 :z 0}} {:cap 1 :ttl :forever})
          (is (= [[2 65 0]] (mapv :cell (ledger/open-entries (mem/view store)))) "the bed's other half is at the spawn: the bed is not scaffold")
          (await (d/tick-out! s))
          (is (= [[2 65 0]] (mapv (juxt :x :y :z) (d/digs p))) "only the dirt is dug")
          (is (= "white_bed" (d/block-at p [0 65 0]))))))))
