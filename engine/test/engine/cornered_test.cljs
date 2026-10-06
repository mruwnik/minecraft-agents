(ns engine.cornered-test
  "A retreat in a 1-wide tunnel: it walks back down the stair it came up, and
  a body with no way out escalates (seal with carried blocks, fight with a tool
  or the fist) instead of failing the same round again and again."
  (:require [cljs.test :refer [deftest is async]]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.world-files :as ew]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.retreat :as retreat]))

(defn setup
  "An engine over a fake; its clock moves a second with every primitive call (a whole flight ends by time)."
  [world]
  (let [zones (:zones world)
        clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (dissoc world :zones))
        now (tu/act-clock clock p 1000)
        eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                                  :events (events/make {:body "Fake" :sinks [sink] :now now})}
                           zones (assoc :world (ew/of-data {} {} zones))))]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async first-round [spec world]
  (let [s (setup world)]
    (core/submit! (:eng s) spec {})
    (await (core/tick! (:eng s)))
    s))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn hiding? [seen] (some #(and (= :holding (:kind %)) (= "hiding" (some-> (:reason %) name))) @seen))

(defn sleep [ms] (js/Promise. (fn [r] (js/setTimeout r ms))))

(defn ^:async hidden-round
  "Submit spec over world and run its round, which hides (a hold) while the mob stays. Once it hides the clock jumps
  jump-ms, (look setup-map) is noted, and the mobs leave (or give way to after, entity specs). (prep p) runs first.
  The setup map plus :while-hidden, what look saw."
  ([spec world jump-ms look] (hidden-round spec world jump-ms look []))
  ([spec world jump-ms look after] (hidden-round spec world jump-ms look after (constantly nil)))
  ([spec world jump-ms look after prep]
   (let [{:keys [eng p seen clock] :as s} (setup world)
         noted (atom nil)]
     (prep p)
     (core/submit! eng spec {})
     (let [round (core/tick! eng)]
       (loop [i 0] (when (and (not (hiding? seen)) (< i 250)) (await (sleep 20)) (recur (inc i))))
       (swap! clock + jump-ms)
       (await (sleep 60))
       (reset! noted (look s))
       (swap! (fake/state p) assoc :entities [])
       (doseq [e after] (fake/add-entity! p e))
       (await round))
     (assoc s :while-hidden @noted))))

(defn first-move
  "The target of the first walk of the engine walker."
  [eng]
  (select-keys (first (tu/walked-to eng)) [:x :y :z]))

(defn placed-cells [p]
  (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "place")))

(defn blocked-events [seen] (filterv #(= :retreat_blocked (:kind %)) @seen))

(defn skeleton [x y z] {:id 9 :name "skeleton" :kind "hostile" :pos {:x x :y y :z z} :health 20})

(defn key-of [x y z] (str x "," y "," z))

(defn rock
  "Stone over x0..x1, y0..y1, z -1..1, less the open cells (a set of [x y z])."
  [[x0 x1] [y0 y1] open]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z [-1 0 1]
                 :when (not (open [x y z]))]
             [(key-of x y z) "stone"])))

(def stair-cells
  "A 1-wide stair rising one block per column towards +x along z 0, three cells tall
  (feet, head and the headroom to step up), the feet at 64 + x."
  (set (for [x (range -9 6) dy [0 1 2]] [x (+ 64 x dy) 0])))

(def stair (rock [-10 6] [52 74] stair-cells))

(def retreat '(jobs.survival.retreat))

(deftest a-retreat-walks-back-down-the-stair-it-came-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (first-round retreat {:blocks stair :entities [(skeleton 3 67 0)]}))
              m (first-move eng)]
          (is (< (:x m) -3) "away from the skeleton at the top")
          (is (< (:y m) 61) "down the steps, not along its own height")
          (is (= 0 (:z m)) "in the stair's own column")
          (is (empty? (blocked-events seen))))))))

(deftest respond-unarmed-flees-down-the-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng]} (await (first-round '(jobs.survival.respond-to-hostile)
                                              {:blocks stair :inventory [{:name "stone_pickaxe" :count 1}]
                                               :entities [(skeleton 3 67 0)]}))]
          (is (zero? (count (calls p "attack"))) "a pickaxe is no weapon: it flees first")
          (is (< (:y (first-move eng)) 61) "down the stair"))))))

(def tunnel-cells
  "A flat 1-wide tunnel along x on z 0, feet and head height, x -8..8."
  (set (for [x (range -8 9) y [64 65]] [x y 0])))

(deftest a-body-at-a-block-centre-probes-its-own-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (await (first-round retreat {:self {:pos [0.5 64 0.5]}
                                                         :blocks (rock [-9 9] [62 67] tunnel-cells)
                                                         :entities [(assoc (skeleton 5.5 64 0.5) :visible true)]}))]
          (is (= {:x -6 :y 64 :z 0} (first-move eng)) "x 0.5 is column 0, the tunnel, not column 1"))))))

(def dead-end-cells
  "A 1-wide tunnel along x on z 0 closed at x -1 behind the body: x 0..8."
  (set (for [x (range 0 9) y [64 65]] [x y 0])))

(def dead-end (rock [-3 9] [62 67] dead-end-cells))

(deftest a-cornered-body-with-blocks-seals-itself-in-and-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen while-hidden]}
              (await (hidden-round retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(skeleton 4 64 0)]}
                                   120000 (fn [{:keys [eng p]}] {:list (:list (core/state eng)) :places (count (calls p "place"))})))]
          (is (= [[1 64 0] [1 65 0]] (placed-cells p)) "the open side towards the skeleton, feet then head")
          (is (zero? (count (calls p "attack"))))
          (is (= [0 64 0] (mapv js/Math.floor (:pos (fake/self p)))) "no walk: at most a step to the middle of its own cell")
          (is (= {:list ["j1"] :places 2} while-hidden) "no timer: still sealed two minutes on, nothing more placed")
          (is (empty? (blocked-events seen)) "never a blocked retreat")
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen))) "one notice that it walled itself in")
          (is (= [] (:list (core/state eng))) "done once the skeleton is gone"))))))

(deftest a-seal-in-anothers-zone-is-recorded-for-tidying
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]}
              (await (hidden-round retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(skeleton 4 64 0)]
                                            :zones [{:name "vault" :min [1 60 -1] :max [1 70 1] :owner "Miles"}]}
                                   1000 (constantly nil)))]
          (is (= [[1 64 0] [1 65 0]] (placed-cells p)))
          (is (= [[1 64 0] [1 65 0]]
                 (mapv #(:cell (:data %)) (mem/entries (mem/view (:store eng)) :tidy)))
              "both seal cells are on the tidy list"))))))

(deftest a-sealed-respond-to-hostile-stays-hidden-while-the-hostile-waits-outside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p while-hidden]}
              (await (hidden-round '(jobs.survival.respond-to-hostile)
                                   {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                    :entities [(assoc (skeleton 4 64 0) :name "zombie")]}
                                   100000 (fn [{:keys [eng]}] (:list (core/state eng)))))]
          (is (= [[1 64 0] [1 65 0]] (placed-cells p)) "unarmed: sealed in")
          (is (= ["j1"] while-hidden) "the zombie cannot reach the sealed body, but would were it to leave: it stays")
          (is (= [] (:list (core/state eng))) "the zombie gone: done"))))))

(deftest a-sealed-body-is-done-once-the-hostile-has-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p while-hidden]}
              (await (hidden-round retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(skeleton 4 64 0)]}
                                   31000 (fn [{:keys [eng]}] (:list (core/state eng)))
                                   [{:id 3 :name "zombie" :kind "hostile" :pos {:x 0 :y 64 :z 5} :health 20}]))]
          (is (= ["j1"] while-hidden) "sealed, the skeleton in the tunnel")
          (is (= [] (:list (core/state eng))) "a zombie walled off in the rock is no danger even with the seal gone"))))))

(def long-dead-end
  "dead-end stretched to x 0..16: a zombie can stand beyond :radius and still have a way to the sealed body."
  (rock [-3 17] [62 67] (set (for [x (range 0 17) y [64 65]] [x y 0]))))

(defn zombie-at [x] {:id 9 :name "zombie" :kind "hostile" :pos {:x x :y 64 :z 0} :health 20})

(defn ^:async hide-steps
  "Submit spec over world and run its round. Once it hides, each [entities jump-ms] of steps replaces the mobs and
  moves the clock; after a short real wait the job list is noted. Then the mobs go and the round is awaited.
  The setup map plus :listed (the lists noted) and :waits (wait calls made by the end)."
  [spec world steps]
  (let [{:keys [eng p seen clock] :as s} (setup world)
        listed (atom [])]
    (core/submit! eng spec {})
    (let [round (core/tick! eng)]
      (loop [i 0] (when (and (not (hiding? seen)) (< i 1000)) (await (sleep 20)) (recur (inc i))))
      (doseq [[ents jump-ms] steps]
        (swap! (fake/state p) assoc :entities [])
        (doseq [e ents] (fake/add-entity! p e))
        (swap! clock + jump-ms)
        (await (sleep 200))
        (swap! listed conj (:list (core/state eng))))
      (swap! (fake/state p) assoc :entities [])
      (await round))
    (assoc s :listed @listed :waits (count (calls p "wait")))))

(deftest a-sealed-body-keeps-hiding-while-a-hostile-beyond-the-radius-could-reach-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng listed]}
              (await (hide-steps retreat {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20}]
                                          :entities [(zombie-at 4)]}
                                 [[[(zombie-at 12)] 100000]]))]
          (is (= [["j1"]] listed) "a zombie 12 blocks off, with a way to the open refuge, is still a danger")
          (is (= [] (:list (core/state eng)))))))))

(deftest a-sealed-body-listens-for-a-quiet-period-after-the-last-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng listed]}
              (await (hide-steps '(jobs.survival.retreat {:quiet-s 100000})
                                 {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20}]
                                  :entities [(zombie-at 4)]}
                                 [[[(zombie-at 12)] 0] [[] 10000]]))]
          (is (= [["j1"] ["j1"]] listed) "the mob forgotten 10 s ago: still hiding")
          (is (= [] (:list (core/state eng))) "after the quiet period: done"))))))

(deftest a-sealed-respond-to-hostile-keeps-hiding-through-the-quiet-period
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen waits]}
              (await (hide-steps '(jobs.survival.respond-to-hostile)
                                 {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20}]
                                  :entities [(zombie-at 4)]}
                                 [[[] 0]]))]
          (is (hiding? seen))
          (is (>= waits 25) "about 30 s of holds after the zombie is gone")
          (is (= [] (:list (core/state eng)))))))))

(deftest respond-to-hostile-passes-quiet-s-to-the-retreat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[q ok?] [[0 #(< % 5)] [120 #(>= % 100)]]]
          (let [{:keys [waits]}
                (await (hide-steps (list 'jobs.survival.respond-to-hostile {:quiet-s q})
                                   {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20}]
                                    :entities [(zombie-at 4)]}
                                   [[[] 0]]))]
            (is (ok? waits) (str ":quiet-s " q " held " waits " waits"))))))))

(deftest a-walled-off-hostile-ends-the-hide-only-after-the-quiet-period
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]}
              (await (hidden-round retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(skeleton 4 64 0)]}
                                   0 (constantly nil)
                                   [{:id 3 :name "zombie" :kind "hostile" :pos {:x 0 :y 64 :z 5} :health 20}]))]
          (is (>= (count (calls p "wait")) 25) "about 30 s of holds before it ends"))))))

(def hard-sided-dead-end
  "dead-end with obsidian beside and behind the tunnel: a stone pickaxe digs no pocket into it."
  (into dead-end (concat (for [x (range -3 10) y [64 65] z [-1 1]] [(key-of x y z) "obsidian"])
                         (for [y [64 65]] [(key-of -1 y 0) "obsidian"]))))

(deftest a-cornered-body-without-blocks-digs-a-side-pocket-and-seals-itself-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p while-hidden]}
              (await (hidden-round retreat {:blocks dead-end :inventory [{:name "stone_pickaxe" :count 1}]
                                            :entities [(skeleton 4 64 0)]}
                                   120000 (fn [{:keys [eng]}] (:list (core/state eng)))))]
          (is (zero? (count (calls p "attack"))) "a pocket beats a pickaxe fight")
          (is (= #{[-1 64 0] [-1 65 0]} (set (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)])
                                                 (calls p "dig")))) "feet and head cell of the pocket")
          (is (= #{[0 64 0] [0 65 0]} (set (placed-cells p))) "the dug blocks seal the way in")
          (is (= ["j1"] while-hidden) "hiding in the pocket")
          (is (= [] (:list (core/state eng)))))))))

(deftest a-cornered-body-with-a-pickaxe-fights-when-no-side-can-be-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (first-round retreat {:blocks hard-sided-dead-end
                                                            :inventory [{:name "stone_pickaxe" :count 1}]
                                                            :entities [(skeleton 2 64 0)]}))]
          (is (= 4 (count (calls p "attack"))) "no blocks, no pocket: it hits back until the skeleton (20, 5 a hit) dies")
          (is (= ["stone_pickaxe"] (mapv #(.. % -args -item) (calls p "equip"))) "with the pickaxe in hand")
          (is (empty? (blocked-events seen))))))))

(deftest a-cornered-body-with-nothing-fights-with-its-fist
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (first-round retreat {:blocks dead-end :entities [(skeleton 2 64 0)]}))]
          (is (= 4 (count (calls p "attack"))) "it keeps hitting; no failed rounds")
          (is (= [] (:list (core/state eng))))
          (is (empty? (blocked-events seen))))))))

(deftest a-cornered-body-that-cannot-place-fights-instead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:blocks dead-end
                                                       :inventory [{:name "cobblestone" :count 20}]
                                                       :entities [(assoc (skeleton 1 64 0) :name "zombie")]}))]
          (is (zero? (count (calls p "place"))))
          (is (= 4 (count (calls p "attack"))) "the zombie stands in the gap: the seal fails, it hits back"))))))

(def corpse-zombie
  "A zombie one hit from death whose corpse stays listed after it dies (a death animation)."
  {:id 1 :name "zombie" :kind "hostile" :pos {:x 1 :y 64 :z 0} :health 5 :lingers true})

(deftest a-cornered-fight-that-kills-its-hostile-neither-swings-at-the-corpse-nor-warns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (first-round retreat {:blocks dead-end :entities [corpse-zombie]}))]
          (is (= 1 (count (calls p "attack"))) "one hit kills it; the dead id is not attacked again")
          (is (empty? (blocked-events seen)) "the fight succeeded: no warn")
          (is (= [] (:list (core/state eng))) "done: the corpse chases no one"))))))

;; ---------------------------------------------- the cornered fallback picks the safest option

(defn wall-cells
  "Stone at feet and head height on the given [x z] cells."
  [cells]
  (into {} (for [[x z] cells y [64 65]] [(key-of x y z) "stone"])))

(def doorway
  "A 1-wide doorway along +z under open sky: walls at x -1 and 1, closed behind the body (z -1)."
  (wall-cells (concat (for [z (range -1 4)] [-1 z]) (for [z (range -1 4)] [1 z]) [[0 -1]])))

(defn zombie [x y z] {:id 7 :name "zombie" :kind "hostile" :pos {:x x :y y :z z} :health 20})

(defn creeper [x y z] {:id 8 :name "creeper" :kind "hostile" :pos {:x x :y y :z z} :health 20})

(defn ^:async ticks!
  "n more ticks, a second apart."
  [{:keys [eng clock]} n]
  (dotimes [_ n]
    (swap! clock + 1000)
    (await (core/tick! eng))))

(defn pillar-entries [eng]
  (filterv #(= :pillar (:purpose %)) (ledger/open-entries (mem/view (:store eng)))))

(deftest an-unarmed-body-with-the-zombie-in-its-doorway-pillars-up-instead-of-punching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen while-hidden]}
              (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(zombie 0 64 1)]}
                                   1000 (fn [{:keys [eng p]}] {:list (:list (core/state eng))
                                                               :y (js/Math.floor (second (:pos (fake/self p))))})))]
          (is (zero? (count (calls p "attack"))) "no fist fight")
          (is (= 3 (count (calls p "jumpPlace"))) "a pillar three high, one block a step")
          (is (= {:list ["j1"] :y 67} while-hidden) "on top of it, out of the zombie's reach, while the zombie is below")
          (is (= #{[0 64 0] [0 65 0] [0 66 0]} (set (map :cell (pillar-entries eng))))
              "every pillar block is in the scaffold ledger, for the cleanup")
          (is (= [] (:list (core/state eng))) "done once the zombie has gone")
          (is (empty? (blocked-events seen))))))))

(deftest a-pillar-that-gives-up-short-abandons-the-refuge-and-fights
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:blocks doorway :inventory [{:name "cobblestone" :count 20}] :entities [(zombie 0 64 1)]})
              {:keys [eng p seen]} s]
          (.override (.-world p) "jumpPlace"
                     (fn [_token _args _impl] (js/Promise.resolve #js {:status "failed" :placed 0 :reason "not-raised"})))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= 3 (count (calls p "jumpPlace"))) "the one-call pillar tried and gave up")
          (is (empty? (pillar-entries eng)) "no block went in")
          (is (not (hiding? seen)) "built under 2: not hidden")
          (is (pos? (count (calls p "attack"))) "the pillar option is spent: it fights the zombie in the doorway"))))))

(deftest a-pillar-that-stops-at-two-high-still-hides
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [placed (atom 0)
              {:keys [eng p seen while-hidden]}
              (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(zombie 0 64 1)]}
                                   1000 (fn [{:keys [p]}] (js/Math.floor (second (:pos (fake/self p)))))
                                   [] (fn [p]
                                        (.override (.-world p) "jumpPlace"
                                                   (fn [token args impl]
                                                     (if (<= (swap! placed inc) 2)
                                                       (impl token args)
                                                       (js/Promise.resolve #js {:status "failed" :placed 0 :reason "not-raised"})))))))]
          (is (= 66 while-hidden) "two blocks up, hidden there")
          (is (= #{[0 64 0] [0 65 0]} (set (map :cell (pillar-entries eng)))))
          (is (hiding? seen) "built 2 is enough to hide")
          (is (zero? (count (calls p "attack")))))))))

(deftest a-pillar-of-two-blocks-hides-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen while-hidden]}
              (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(zombie 0 64 1)]}
                                   1000 (fn [{:keys [p]}] (js/Math.floor (second (:pos (fake/self p)))))
                                   [] (fn [p] (.override (.-world p) "jumpPlace"
                                                         (fn [token args impl]
                                                           (if (>= (count (calls p "jumpPlace")) 3)
                                                             (js/Promise.resolve #js {:status "failed" :placed 0 :reason "not-raised"})
                                                             (impl token args)))))))]
          (is (= 66 while-hidden) "up two blocks")
          (is (= 2 (count (pillar-entries eng))))
          (is (zero? (count (calls p "attack"))) "built at 2: hidden, not fighting"))))))

(deftest a-pillared-body-is-done-once-the-zombie-has-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                                            :entities [(zombie 0 64 1)]}
                                                   1000 (constantly nil)))]
          (is (= [] (:list (core/state eng))))
          (is (zero? (count (calls p "attack")))))))))

(deftest hitbox-cells-are-every-cell-a-mob-overlaps
  (is (= #{[0 64 0] [1 64 0] [0 65 0] [1 65 0]} (set (map (juxt :x :y :z) (retreat/hitbox-cells {:x 0.8 :y 64 :z 0.5})))))
  (is (= #{[0 64 0] [0 65 0]} (set (map (juxt :x :y :z) (retreat/hitbox-cells {:x 0.5 :y 64 :z 0.5}))))))

(def deep-dead-end
  "dead-end over solid rock down to y 58: a tunnel with a roof at y 66, a pickaxe digs down into it."
  (rock [-3 9] [58 67] dead-end-cells))

(deftest an-unarmed-body-under-a-roof-digs-down-and-plugs-the-hole
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p while-hidden]}
              (await (hidden-round retreat {:blocks deep-dead-end :inventory [{:name "stone_pickaxe" :count 1}]
                                            :entities [(zombie 1 64 0)]}
                                   1000 (fn [{:keys [p]}] (.-name (.blockAt p #js {:x 0 :y 64 :z 0})))))]
          (is (zero? (count (calls p "attack"))) "a pickaxe and a hole beat a pickaxe fight")
          (is (= [[0 63 0] [0 62 0]] (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "dig"))))
          (is (= 62 (js/Math.floor (second (:pos (fake/self p))))) "two down")
          (is (#{"stone" "cobblestone"} while-hidden) "plugged over the head"))))))

(def room
  "A room three wide (z -1..1) closed at x -2 behind the body, open far towards +x."
  (wall-cells (concat (for [x (range -2 10)] [x -2]) (for [x (range -2 10)] [x 2]) (for [z (range -1 2)] [-2 z]))))

(deftest a-body-cornered-by-a-creeper-backs-off-instead-of-swinging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "iron_sword" :count 1}] :blocks room :entities [(creeper 3 64 0)]})]
          (.override (.-world p) "steer"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (zero? (count (calls p "attack"))) "no melee with a creeper")
          (is (= 1 (count (tu/walked-to eng))) "one step back, into the last cell of the room (then the creeper leaves)")
          (is (= -1 (:x (first-move eng)))))))))

(defn stuck-walks!
  "Make every walk of the engine walker get no nearer: :blocked."
  [p]
  (.override (.-world p) "steer" (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose #js {}}))))

(deftest a-body-that-cannot-walk-away-from-a-creeper-walls-it-off-instead-of-swinging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "iron_sword" :count 1} {:name "dirt" :count 16}]
                                      :entities [(creeper 2.5 64 0.5)]})]
          (stuck-walks! p)
          (.override (.-world p) "place"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (zero? (count (calls p "attack"))) "no melee with a creeper")
          (is (pos? (count (calls p "place"))) "blocks between it and the creeper"))))))

(deftest an-armed-body-that-would-lose-the-fight-seals-in-instead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (hidden-round retreat {:self {:health 4}
                                                        :inventory [{:name "iron_sword" :count 1} {:name "cobblestone" :count 20}]
                                                        :blocks dead-end :entities [(zombie 3 64 0)]}
                                               1000 (constantly nil)))]
          (is (zero? (count (calls p "attack"))) "4 health against a zombie: the sword is no answer")
          (is (= [[1 64 0] [1 65 0]] (placed-cells p))))))))

(deftest an-armed-body-that-wins-still-fights-when-cornered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:inventory [{:name "iron_sword" :count 1} {:name "cobblestone" :count 20}]
                                                       :blocks dead-end :entities [(zombie 1 64 0)]}))]
          (is (= 4 (count (calls p "attack"))) "full health and a sword against one zombie: fight it to the end"))))))

(defn edge-world
  "Floor (stone at y 63) for x <= edge on z 0 and air everywhere else."
  [edge]
  (fn [{:keys [x y z]}] (if (and (= y 63) (<= x edge) (= z 0)) "stone" "air")))

(deftest walk-cells-stop-at-the-edge-of-a-floor
  (is (= [1 2 3] (mapv :x (retreat/walk-cells (edge-world 3) {:x 0.5 :y 64 :z 0.5} [1 0] 6))) "the cells past the edge have no floor")
  (is (= [1 2 3] (mapv :x (retreat/walk-cells (edge-world 3) {:x 0.5 :y 64 :z 0.5} [1 0] 3))))
  (is (= [1 2 3 4 5 6] (mapv :x (retreat/walk-cells (edge-world 9) {:x 0.5 :y 64 :z 0.5} [1 0] 6)))))

(defn sealed-world
  "Floor at y 63 everywhere, stone at feet and head height on the four sides of cell 0,0 only: the diagonals open."
  [{:keys [x y z]}]
  (cond
    (= y 63) "stone"
    (and (#{64 65} y) (#{[1 0] [-1 0] [0 1] [0 -1]} [x z])) "stone"
    :else "air"))

(deftest walk-cells-do-not-cut-a-corner-between-two-blocks
  (is (= [] (retreat/walk-cells sealed-world {:x 0.5 :y 64 :z 0.5} (retreat/unit 1 1) 2))
      "a body walled in on four sides cannot squeeze out diagonally")
  (is (= 2 (count (retreat/walk-cells (fn [{:keys [y]}] (if (= y 63) "stone" "air")) {:x 0.5 :y 64 :z 0.5} (retreat/unit 1 1) 2)))
      "past no corner, the diagonal is open"))
