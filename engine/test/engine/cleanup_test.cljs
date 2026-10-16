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
                               :reach 4.5 :held {} :guard-walks? true :max-below 4 :roof-height 4}
                              (dissoc in :feet)))))

(defn column-world [cells] (apply lookup (mapcat (fn [c] [c "dirt"]) cells)))

(deftest on-top-of-its-pillar-it-digs-the-block-under-its-feet
  (let [cells (pillar-cells [0 64 0] 6)]
    (is (= {:step :dig :cell [0 69 0] :item "dirt"}
           (step :feet [0 70 0] :entries (mapv entry cells) :block-at (column-world cells))))))

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

(deftest a-shelter-seal-stays-at-night-and-while-a-hostile-is-near
  (let [seal (entry [3 64 1] :purpose :shelter)
        plug (entry [3 65 1] :purpose :retreat-plug)
        pillar (entry [3 66 1])
        world (lookup [3 64 1] "dirt" [3 65 1] "dirt" [3 66 1] "dirt")]
    (is (= #{:night} (set (map :reason (:open (step :entries [seal plug] :night? true :block-at world))))))
    (is (= [{:cell [3 64 1] :item "dirt" :reason :hostile-near}]
           (:open (step :entries [seal] :hostile-near? true :block-at world))))
    (is (= :dig (:step (step :entries [pillar] :night? true :block-at world))) "other scaffold is taken back")
    (is (= :dig (:step (step :entries [seal] :night? false :block-at world))) "by day the seal is taken back")
    (is (= :dig (:step (step :entries [seal] :night? true :keep-at-night? false :block-at world))) "unless the night keep is off")))

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
    (is (false? (fires? p (doto (trigger-store [(entry [0 64 0])] []) (ledger/hold! [[0 64 0]])) [])) "held")))

(deftest the-trigger-leaves-scaffold-beyond-its-radius-for-an-explicit-cleanup
  (let [p (tu/fake {:blocks {"0,64,0" "dirt" "50,64,0" "dirt"}})
        args scaffold-left/defaults]
    (is (= {:within 32} args))
    (is (true? (fires? p (trigger-store [(entry [20 64 0])] []) [] args)) "near")
    (is (false? (fires? p (trigger-store [(entry [50 64 0])] []) [] args)) "far: nothing for the reflex")
    (is (false? (fires? p (trigger-store [(entry [50 64 0])] []) [])) "no radius given: the default radius, as a saved register's old spec has")
    (is (= args (:args (:scaffold-left triggers/all))))
    (is (= [args] (rest (:job (:scaffold-left triggers/all)))) "the job the reflex starts gets the radius too")))

(deftest the-trigger-ignores-scaffold-cleanup-would-hold-far-below
  (let [p (tu/fake {:blocks {"0,54,0" "dirt" "0,62,0" "dirt"}})]
    (is (false? (fires? p (trigger-store [(entry [0 54 0])] []) [])) "ten below: cleanup holds it :far-below each time the hold runs out")
    (is (true? (fires? p (trigger-store [(entry [0 54 0]) (entry [0 62 0])] []) [])) "another, in reach, still fires")))

(deftest the-trigger-waits-for-day-and-for-no-hostile-near
  (let [dirt (trigger-store [(entry [0 64 0])] [])
        night #(tu/fake {:blocks {"0,64,0" "dirt"} :time 14000})
        hostile (fn [] (doto (tu/fake {:blocks {"0,64,0" "dirt"}})
                         (fake/add-entity! {:id 9 :name "zombie" :kind "hostile" :pos {:x 4 :y 64 :z 0} :health 20})))]
    (is (false? (fires? (night) dirt [])) "night")
    (is (false? (fires? (hostile) dirt [])) "a hostile near")
    (settings/with-settings {:triggers.access.scaffold-left/at-night true :triggers.access.scaffold-left/with-hostile true}
      (fn []
        (is (true? (fires? (night) dirt [])))
        (is (true? (fires? (hostile) dirt [])))))))

(deftest the-trigger-waits-out-the-first-light
  (let [dirt (trigger-store [(entry [0 64 0])] [])
        at (fn [t] (tu/fake {:blocks {"0,64,0" "dirt"} :time t}))]
    (is (false? (fires? (at 400) dirt [])) "dawn grace (jobs.lib.dawn)")
    (is (true? (fires? (at 1000) dirt [])))
    (is (false? (fires? (doto (at 1000) (fake/add-entity! {:id 9 :name "zombie" :kind "hostile" :pos {:x 30 :y 64 :z 0} :health 20})) dirt []))
        "a hostile within the dawn radius, past the 16 the old check looked at")))

(deftest the-trigger-holds-off-after-a-flight
  (let [p (tu/fake {:blocks {"0,64,0" "dirt"}})
        fled (doto (trigger-store [(entry [0 64 0])] [])
               (mem/write! :threat {:mob "skeleton" :pos {:x 8 :y 64 :z 0}} threats/threat-policy))]
    (is (false? (fires? p fled [])) "a skeleton fled from a moment ago is out of sight, not gone")
    (is (true? (settings/with-settings {::threats/fled-memory-ms 0} #(fires? p fled []))))))

(deftest cleanup-within-narrows-to-the-body-and-near-wins
  (let [c {:primitives (tu/fake {:blocks {"0,64,0" "dirt"}})}]
    (is (= [32 #{:x :y :z}] ((juxt :radius (comp set keys :pos)) (ledger/near-of-args c {:within 32}))))
    (is (= {:pos {:x 9 :y 9 :z 9} :radius 5} (ledger/near-of-args c {:within 32 :near {:pos {:x 9 :y 9 :z 9} :radius 5}})))
    (is (= 32 (:radius (ledger/near-of-args c {}))) "unowned entries: 32 by default")
    (is (= 32 (:radius (ledger/near-of-args c {:job nil}))))
    (is (nil? (ledger/near-of-args c {:job :all})))
    (is (nil? (ledger/near-of-args c {:job "j3"})))))

(deftest the-trigger-is-a-builtin
  (is (= scaffold-left/scaffold-left (:when (:scaffold-left triggers/all))))
  (is (= '(jobs.access.cleanup {:within 32}) (:job (:scaffold-left triggers/all)))))

(deftest a-scaffold-far-below-the-body-is-not-walked-to
  (let [e (entry [10 60 0])
        world (fn [[_ y _ :as pos]] (cond (= pos [10 60 0]) "dirt" (< y 60) "stone" :else "air"))
        go #(apply step :entries [e] :block-at world %&)]
    (is (= [:finish [{:cell [10 60 0] :item "dirt" :reason :far-below}]] ((juxt :step :open) (go :feet [3 70 0]))))
    (is (= :walk (:step (go :feet [3 62 0]))) "two below is a walk")
    (is (= :walk (:step (go :feet [3 66 0] :max-below 8))))
    (is (= :walk (:step (go :feet [3 70 0] :guard-walks? false))) "an owner's or a caller's cleanup goes anyway")))

(deftest a-scaffold-where-the-body-died-is-not-walked-to
  (let [s (step :feet [3 64 0] :entries [(entry [10 64 0])] :block-at (lookup [10 64 0] "dirt") :died-near? #(= [10 64 0] %))]
    (is (= :died-near (:reason (first (:open s)))))))

(deftest a-scaffold-under-a-roof-is-not-walked-to-from-the-open
  (let [roofed (lookup [10 64 0] "dirt" [10 67 0] "stone")
        under (lookup [10 64 0] "dirt" [10 67 0] "stone" [3 67 0] "stone")
        go #(step :feet [3 64 0] :entries [(entry [10 64 0])] :block-at %)]
    (is (= :roofed (:reason (first (:open (go roofed))))))
    (is (= :walk (:step (go under))) "a body under a roof itself has nothing to fear in a roof")
    (is (= :walk (:step (go (lookup [10 64 0] "dirt" [10 70 0] "stone")))) "a roof out of range is open sky")))

(deftest the-walk-hold-reads-the-weapon-and-the-health-of-the-body
  (let [sword [{:name "iron_sword" :count 1}]]
    (are [world hold] (= hold (:reason (cleanup/walk-hold-of (tu/fake world))))
      {:inventory []} :unarmed
      {:inventory sword} nil
      {:inventory sword :self {:health 3}} :low-health)
    (is (nil? (settings/with-settings {::cleanup/hold-unarmed? false} #(cleanup/walk-hold-of (tu/fake {:inventory []})))))))

(deftest an-unarmed-or-hurt-body-walks-to-no-scaffold
  (are [hold reason] (= reason (:reason (first (:open (step :feet [3 64 0] :entries [(entry [10 64 0])] :walk-hold hold)))))
    {:reason :unarmed} :unarmed
    {:reason :low-health} :low-health)
  (is (= :dig (:step (step :feet [3 64 0] :entries [(entry [4 64 0])] :walk-hold {:reason :unarmed}))) "within reach it is still dug"))

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
      (is (= {:step :retreat :to [4 64 0]} s) "x 3 is the way back: not dug, the body goes to the ground first"))))

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
          (is (= :done (:status @out)))
          (is (= 1 (count (of-kind seen :cleanup.done)))))))))

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
          (is (= 1 (count (of-kind seen :cleanup.left))))
          (is (= #{[0 65 0]} (ledger/held-cells (mem/view (:store eng))))))))))

(deftest only-the-named-instance-and-its-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [1 64 0]]
              {:keys [eng p out] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                :entries [(entry [0 64 0] :job "j1/pillar") (entry [1 64 0] :job "j77")]
                                                :args {:job "j1"}})]
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
    (is (= [:job :near] (:target j)))
    (is (some? (:defaults j)))
    (is (fn? (:quote-of j)))))

(deftest a-bad-job-argument-is-impossible
  (are [job cause] (= cause (tu/kind-reason (cleanup/bad-args {:args {:job job}})))
    5 [:never :bad-args]
    nil [:ok :args]
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
          (is (= 0 (.-length (.-calls (.-world p)))))
          (is (empty? (of-kind seen :cleanup.done))))))))

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

(deftest no-zone-list-declines-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks [[0 64 0]])
                                                 :entries [(entry [0 64 0])] :zones nil})]
          (core/submit! eng (list job) {})
          (await (ticks s eng 10))
          (is (= "dirt" (block p [0 64 0])))
          (is (= 1 (count (of-kind seen :cleanup.declined)))))))))

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
    (gave-up :leg-search-spent) false false
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
          (is (every? #(= {:pos [9 64 0] :range 3 :ways #{:walk}} (select-keys % [:pos :range :ways])) calls)))))))

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
        (let [p (d/sensing {:self {:pos {:x 3.5 :y 65 :z 0.5}} :blocks (assoc d/ground "0,65,0" "oak_planks" "2,65,0" "dirt")})
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
