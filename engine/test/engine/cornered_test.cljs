(ns engine.cornered-test
  "A retreat in a 1-wide tunnel: it walks back down the stair it came up, and
  a body with no way out escalates (seal with carried blocks, fight with a tool
  or the fist) instead of failing the same round again and again."
  (:require [cljs.test :refer [deftest is are async]]
            [clojure.string :as str]
            [jobs.lib.blocks :as lb]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.world-files :as ew]
            [engine.memory :as mem]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.retreat :as retreat]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.survival.retreat-cornered.pick :as pick]
            [jobs.lib.shot :as shot]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-refuge :as refuge]
            [jobs.survival.retreat-cover :as cover]
            [jobs.survival.retreat-walk :as retreat-walk]
            [jobs.lib.seal :as seal]
            [jobs.survival.retreat-pillar :as retreat-pillar]
            [jobs.survival.retreat-seal :as retreat-seal]))

(defn setup
  "An engine over a fake; its clock moves a second with every primitive call (a whole flight ends by time). The world's
  :stair-way (floors) is written to memory as stairs the body cut."
  [world]
  (let [zones (:zones world)
        clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (dissoc world :zones :stair-way))
        now (tu/act-clock clock p 1000)
        eng (core/create (cond-> {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                                  :events (events/make {:body "Fake" :sinks [sink] :now now})}
                           zones (assoc :world (ew/of-data {} {} zones))))]
    (some->> (:stair-way world) (assoc {:dim "overworld"} :floors) (mem/write! (:store eng) :stair-way))
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async first-round [spec world]
  (let [s (setup world)]
    (core/submit! (:eng s) spec {})
    (await (core/tick! (:eng s)))
    s))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn hiding? [seen] (some #(and (= :holding (:kind %)) (= "hiding" (some-> (:reason %) name))) @seen))

(defn sleep [ms] (js/Promise. (fn [r] (js/setTimeout r ms))))

(defn ^:async until-hiding
  "Poll until the round hides (a hold), ends, or n polls of 20 ms pass."
  [round seen n]
  (let [ended (atom false)]
    (.then round #(reset! ended true) #(reset! ended true))
    (loop [i 0] (when (and (not (hiding? seen)) (not @ended) (< i n)) (await (sleep 20)) (recur (inc i))))))

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
       (await (until-hiding round seen 250))
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

(def stair-floors "The floors of stair-cells, as the stairs the body cut." (vec (for [x (range -9 6)] [x (+ 63 x) 0])))

(def retreat '(jobs.survival.retreat))

(def hiding-retreat
  "A retreat that hides on for as long as the test needs (:hide-s far off)."
  '(jobs.survival.retreat {:settings {:jobs.survival.retreat-refuge/hide-s 100000}}))

(deftest a-retreat-walks-back-down-the-stair-it-came-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (await (first-round retreat {:blocks stair :stair-way stair-floors :entities [(skeleton 3 67 0)]}))
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
                                              {:blocks stair :stair-way stair-floors :inventory [{:name "stone_pickaxe" :count 1}]
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
              (await (hidden-round hiding-retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(skeleton 4 64 0)]}
                                   120000 (fn [{:keys [eng p]}] {:list (:list (core/state eng)) :places (count (calls p "place"))})))]
          (is (= [[1 64 0] [1 65 0]] (placed-cells p)) "the open side towards the skeleton, feet then head")
          (is (= #{[1 64 0] [1 65 0]} (set (map :cell (filter #(= :retreat-plug (:purpose %)) (ledger/open-entries (mem/view (:store eng))))))) "the seal blocks are in the scaffold ledger, for the cleanup")
          (is (zero? (count (calls p "attack"))))
          (is (= [0 64 0] (mapv js/Math.floor (:pos (fake/self p)))) "no walk: at most a step to the middle of its own cell")
          (is (= {:list ["j1"] :places 2} while-hidden) "still sealed two minutes on (inside :hide-s), nothing more placed")
          (is (empty? (blocked-events seen)) "never a blocked retreat")
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen))) "one notice that it walled itself in")
          (let [e (first (filter #(= :retreat_sealed (:kind %)) @seen))]
            (is (str/includes? (:text e) "from skeleton at [4 64 0]") "the notice names the mob and where it is")
            (is (= [{:name "skeleton" :pos [4 64 0]}] (:hostiles e)) "and carries them"))
          (is (= [] (:list (core/state eng))) "done once the skeleton is gone"))))))

(deftest a-seal-cell-with-nothing-to-place-against-waits-for-the-others
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [refused (atom false)
              {:keys [seen]}
              (await (hidden-round hiding-retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                                   :entities [(skeleton 4 64 0)]}
                                   120000 (constantly nil) []
                                   (fn [p] (.override (.-world p) "place"
                                                      (fn [token args impl]
                                                        (if (compare-and-set! refused false true)
                                                          (js/Promise.resolve #js {:status "no-support"})
                                                          (impl token args)))))))]
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen))) "sealed in though the first cell had no support")
          (is (empty? (blocked-events seen))))))))

(deftest a-seal-begun-is-finished-when-the-mob-drops-out-of-sense
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks dead-end :inventory [{:name "cobblestone" :count 20}] :entities [(skeleton 4 64 0)]})
              _ (add-watch (fake/state p) :mob-out
                           (fn [_ _ _ w] (when (and (seq (:entities w)) (get-in w [:blocks [1 64 0]])) (swap! (fake/state p) assoc :entities []))))
              round (do (core/submit! eng '(jobs.survival.retreat {}) {})
                        (core/tick! eng))]
          (await round)
          (is (= [[1 64 0] [1 65 0]] (placed-cells p)) "the seal is finished though the skeleton is out of sense")
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen)))))))))

(defn unseen-ceiling!
  "p senses no cell at height y until a look aims within a block of it (a look shows the cell and its neighbours)."
  [p y]
  (let [looked? (fn [pos] (some #(and (= "look" (.-name %))
                                       (let [a (.. % -args -pos)]
                                         (<= (+ (js/Math.abs (- (js/Math.floor (.-x a)) (.-x pos)))
                                                (js/Math.abs (- (js/Math.floor (.-y a)) (.-y pos)))
                                                (js/Math.abs (- (js/Math.floor (.-z a)) (.-z pos))))
                                             1)))
                                (.-calls (.-world p))))]
    (aset p "sensedAt" (fn [pos] (if (and (= y (.-y pos)) (not (looked? pos))) #js {:unknown true} (.blockAt p pos))))))

(deftest a-seal-looks-at-the-unseen-ceiling-before-it-fills-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen while-hidden]}
              (await (hidden-round hiding-retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                                   :entities [(skeleton 4 64 0)]}
                                   1000 (fn [{:keys [eng]}] {:list (:list (core/state eng))})
                                   [] #(unseen-ceiling! % 66)))]
          (is (= [[1 64 0] [1 65 0]] (placed-cells p)) "the stone over the head is looked at, not built into")
          (is (= {:list ["j1"]} while-hidden) "sealed in and hiding")
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen)))))))))

;; low on health in a skeleton's line of fire with no covered way to walk: it hides (seals itself in) instead of fleeing
;; through the arrows; at full health it flees
(deftest a-body-low-on-health-in-a-skeletons-line-of-fire-seals-in-instead-of-running
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (fn [health] {:self {:pos [0.5 64 0.5] :health health} :inventory [{:name "cobblestone" :count 20}]
                                  :entities [(assoc (skeleton 6.5 64 0.5) :visible true)]})
              {:keys [p while-hidden]}
              (await (hidden-round retreat (world 8) 1000 (fn [{:keys [p]}] {:places (count (calls p "place"))})))]
          (is (<= 4 (:places while-hidden)) "walled in where it stood")
          (is (= [0 64 0] (mapv js/Math.floor (:pos (fake/self p)))) "no flight through the line of fire")
          (let [{:keys [eng p]} (await (first-round retreat (world 20)))]
            (is (< (:x (first-move eng)) 0) "at full health it runs")
            (is (zero? (count (calls p "place"))))))))))

;; the seal fills the side facing the skeleton first (feet, then head), so the first blocks stop its arrows, and a seal
;; begun goes on to the end though the body is no longer in the line of fire
(deftest a-low-body-seals-the-side-facing-the-skeleton-first-and-finishes-the-seal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p while-hidden]}
              (await (hidden-round retreat {:self {:pos [0.5 64 0.5] :health 8} :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(assoc (skeleton 0.5 64 -5.5) :visible true)]}
                                   1000 (fn [{:keys [p]}] {:places (count (calls p "place"))})))]
          (is (= [[0 64 -1] [0 65 -1]] (take 2 (placed-cells p))) "the side towards the skeleton first")
          (is (<= 8 (:places while-hidden)) "walled in on every side")
          (is (= [0 64 0] (mapv js/Math.floor (:pos (fake/self p)))) "no walk out of a half-built seal"))))))

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
              (await (hidden-round '(jobs.survival.respond-to-hostile {:settings {:jobs.survival.retreat-refuge/hide-s 100000}})
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
              (await (hidden-round hiding-retreat {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(skeleton 4 64 0)]}
                                   31000 (fn [{:keys [eng]}] (:list (core/state eng)))
                                   [{:id 3 :name "zombie" :kind "hostile" :pos {:x 0 :y 64 :z 5} :health 20}]))]
          (is (= ["j1"] while-hidden) "sealed, the skeleton in the tunnel")
          (is (= [] (:list (core/state eng))) "a zombie walled off in the rock is no danger even with the seal gone"))))))

(def long-dead-end
  "dead-end stretched to x 0..16: a zombie can stand beyond :radius and still have a way to the sealed body."
  (rock [-3 17] [62 67] (set (for [x (range 0 17) y [64 65]] [x y 0]))))

(defn zombie-at [x] {:id 9 :name "zombie" :kind "hostile" :pos {:x x :y 64 :z 0} :health 20})

(defn hidden-ends
  "The :ended of each ending of a flight (the root's, or its retreat child's) that carries one."
  [seen]
  (vec (keep #(or (:ended %) (get-in % [:ending :data :ended])) (filter #(#{:completed :child_ended} (:kind %)) @seen))))

(defn hidden-holds
  "The :hold (:spent or :quiet) of each flight ending that carries one."
  [seen]
  (vec (keep #(or (:hold %) (get-in % [:ending :data :hold])) (filter #(#{:completed :child_ended} (:kind %)) @seen))))

(defn ^:async hide-steps
  "Submit spec over world and run its round. Once it hides, each [entities jump-ms] of steps replaces the mobs and
  moves the clock; after a short real wait the job list is noted. Then the mobs go and the round is awaited.
  The setup map plus :listed (the lists noted) and :waits (wait calls made by the end)."
  [spec world steps]
  (let [{:keys [eng p seen clock] :as s} (setup world)
        listed (atom [])]
    (core/submit! eng spec {})
    (let [round (core/tick! eng)]
      (await (until-hiding round seen 1000))
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
              (await (hide-steps hiding-retreat {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20}]
                                          :entities [(zombie-at 4)]}
                                 [[[(zombie-at 12)] 100000]]))]
          (is (= [["j1"]] listed) "a zombie 12 blocks off, with a way to the open refuge, is still a danger")
          (is (= [] (:list (core/state eng)))))))))

(deftest a-sealed-body-hands-back-after-hide-s-while-the-kept-off-hostile-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spec listed-want] [['(jobs.survival.retreat) [[]]]
                                      ['(jobs.survival.respond-to-hostile) [[]]]
                                      ['(jobs.survival.respond-to-hostile {:settings {:jobs.survival.retreat-refuge/hide-s 100000}}) [["j1"]]]]]
          (let [{:keys [listed]}
                (await (hide-steps spec {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20}]
                                         :entities [(zombie-at 4)]}
                                   [[[(zombie-at 9)] 61000]]))]
            (is (= listed-want listed)
                (str spec ": Juniper j655 hid a minute from a skeleton that never left; sealed and in no real danger, it hands back"))))))))

(deftest a-sealed-body-with-a-melee-mob-at-its-foot-past-hide-s-digs-up-and-away-from-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]}
              (await (hide-steps '(jobs.survival.retreat) {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20} {:name "stone_pickaxe" :count 1}]
                                                           :entities [(zombie-at 4)]}
                                 [[[(zombie-at 3)] 61000] [[(zombie-at 3)] 1000]]))]
          (is (seq (calls p "dig")) "a stair is cut (the old code held on, up to :hide-max-s)")
          (is (every? #(<= (.. % -args -pos -x) 0) (calls p "dig")) "away from the zombie at x 3, none towards it")
          (is (= [:spent] (hidden-holds seen))))))))

(deftest a-sealed-body-listens-for-a-quiet-period-after-the-last-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng listed]}
              (await (hide-steps '(jobs.survival.retreat {:settings {:jobs.survival.retreat-refuge/quiet-s 2000 :jobs.survival.retreat-refuge/hide-s 100000}})
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
          (is (= [:quiet] (hidden-holds seen)))
          (is (= [] (:list (core/state eng)))))))))

(deftest respond-to-hostile-s-quiet-s-setting-reaches-the-retreat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[q ok?] [[0 #(< % 5)] [120 #(>= % 100)]]]
          (let [{:keys [waits]}
                (await (hide-steps (list 'jobs.survival.respond-to-hostile {:settings {:jobs.survival.retreat-refuge/quiet-s q :jobs.survival.retreat-refuge/hide-s 100000}})
                                   {:blocks long-dead-end :inventory [{:name "cobblestone" :count 20}]
                                    :entities [(zombie-at 4)]}
                                   [[[] 0]]))]
            (is (ok? waits) (str "quiet-s " q " held " waits " waits"))))))))

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
              (await (hidden-round hiding-retreat {:blocks dead-end :inventory [{:name "stone_pickaxe" :count 1}]
                                            :entities [(skeleton 4 64 0)]}
                                   120000 (fn [{:keys [eng]}] (:list (core/state eng)))))]
          (is (zero? (count (calls p "attack"))) "a pocket beats a pickaxe fight")
          (is (= #{[-1 64 0] [-1 65 0] [-2 64 0] [-2 65 0]} (set (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)])
                                                 (calls p "dig")))) "two cells deep, feet and head")
          (is (= #{[-1 64 0] [-1 65 0]} (set (placed-cells p))) "the dug blocks seal the near pair")
          (is (= ["j1"] while-hidden) "hiding in the pocket")
          (is (= [] (:list (core/state eng)))))))))

(deftest a-phantom-is-hidden-from-not-outrun
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]}
              (await (hidden-round hiding-retreat {:inventory [{:name "cobblestone" :count 20}]
                                            :entities [{:id 9 :name "phantom" :kind "hostile" :pos {:x 4 :y 64 :z 0} :health 20}]}
                                   120000 (fn [_] nil)))]
          (is (zero? (count (calls p "steer"))) "no walk: a phantom flies faster than the body runs")
          (is (pos? (count (calls p "place"))) "it walls itself in")
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen)))))))))

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

(deftest a-pillar-through-anothers-zone-is-built-only-after-every-other-way-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]}
              (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(zombie 0 64 1)]
                                            :zones [{:name "vault" :min [0 64 0] :max [0 67 0] :owner "Miles"}]}
                                   1000 (constantly nil)))]
          (is (= 3 (count (calls p "jumpPlace"))) "nothing else was left, so the pillar went up")
          (is (= 1 (count (filterv #(= :retreat.trespass-last-resort (:kind %)) @seen))) "and said it trespassed"))))))

(deftest walls-that-need-anothers-zone-wait-for-a-pillar-that-keeps-off-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]}
              (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(zombie 0 64 3)]
                                            :zones [{:name "vault" :min [0 64 1] :max [0 66 1] :owner "Miles"}]}
                                   1000 (constantly nil)))]
          (is (zero? (count (calls p "place"))) "no wall block went into the zone")
          (is (= 3 (count (calls p "jumpPlace"))) "the pillar, which keeps off the zone, went up instead")
          (is (empty? (filterv #(= :shut-in.trespass-last-resort (:kind %)) @seen))))))))

(deftest a-pillar-through-anothers-area-comes-after-every-other-way-and-before-a-last-fight
  (is (= [:plug :pillar :back-off :plug-dig :plug-zone :plug-dig-zone :pillar-zone :fight] (pick/with-zone-pillar (cornered/escape-order {}))))
  (is (= [:fight :plug :pillar :plug-dig :plug-zone :plug-dig-zone :pillar-zone] (pick/with-zone-pillar (cornered/escape-order {:pressed? true :faster? true}))))
  (is (= [:fight] (pick/with-zone-pillar [:fight])) "no plug or pillar in the order, none added"))

(deftest a-lone-melee-mob-within-ten-with-blocks-carried-is-pillared-against-before-any-flee-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:self {:health 10} :inventory [{:name "dirt" :count 39}]
                                                       :entities [(zombie 8 64 0)]}))]
          (is (= 3 (count (calls p "jumpPlace"))) "a pillar three high")
          (is (empty? (calls p "steer")) "no flee step first")
          (is (empty? (calls p "moveTo")) "no flee step first"))))))

(deftest a-pillar-looks-at-the-ground-round-it-before-taking-unseen-cells-for-a-ledge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:self {:health 10} :inventory [{:name "dirt" :count 39}] :entities [(zombie 8 64 0)]})
              {:keys [p eng]} s
              hidden #{[-2 63 0] [-2 64 0] [-1 63 0] [-1 64 0] [-1 65 0]}
              sensed (.-sensedAt p)]
          (tu/set-sensed! p (fn [q] (if (and (hidden [(.-x q) (.-y q) (.-z q)]) (empty? (calls p "look")))
                                      #js {:unknown true}
                                      (sensed q))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= 3 (count (calls p "jumpPlace"))) "unseen cells beside the column read as rock; once looked at they are open air, so the pillar stands"))))))

(deftest the-calls-blocks-are-the-only-blocks-a-refuge-uses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round '(jobs.survival.retreat {:use {:blocks ["dirt"]}})
                                              {:self {:health 10} :inventory [{:name "cobblestone" :count 39}]
                                               :entities [(zombie 8 64 0)]}))]
          (is (empty? (calls p "jumpPlace")) "no pillar of cobblestone the call did not list")
          (is (seq (concat (calls p "steer") (calls p "moveTo"))) "it flees instead"))))))

(deftest a-flee-step-logs-its-speed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (first-round retreat {:self {:health 10} :inventory [{:name "dirt" :count 2}]
                                                          :entities [(zombie 8 64 0)]}))
              e (first (filter #(= :retreat.step-speed (:kind %)) @seen))]
          (is (some? e))
          (is (every? number? [(:blocks e) (:seconds e)])))))))

(deftest a-lone-melee-mob-with-fewer-than-three-blocks-carried-is-fled-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (first-round retreat {:self {:health 10} :inventory [{:name "dirt" :count 2}]
                                                       :entities [(zombie 8 64 0)]}))]
          (is (empty? (calls p "jumpPlace")))
          (is (seq (concat (calls p "steer") (calls p "moveTo")))))))))

(deftest a-pillar-that-gives-up-short-abandons-the-refuge-and-fights
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:blocks doorway :inventory [{:name "cobblestone" :count 20}] :entities [(zombie 0.5 64 1.6)]})
              {:keys [eng p seen]} s]
          (.override (.-world p) "jumpPlace"
                     (fn [_token _args _impl] (js/Promise.resolve #js {:status "failed" :placed 0 :reason "not-raised"})))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= 6 (count (calls p "jumpPlace"))) "the pillar tried, tried once more, and gave up")
          (is (empty? (pillar-entries eng)) "no block went in")
          (is (not (hiding? seen)) "built under 2: not hidden")
          (is (pos? (count (calls p "attack"))) "hit while sealed: it stops hiding and hits back"))))))

(deftest a-pillar-refused-once-is-tried-once-more-and-then-hides
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen while-hidden]}
              (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(zombie 0 64 1)]}
                                   1000 (fn [{:keys [p]}] (js/Math.floor (second (:pos (fake/self p)))))
                                   [] (fn [p]
                                        (.override (.-world p) "jumpPlace"
                                                   (fn [token args impl]
                                                     (if (< (count (calls p "jumpPlace")) 3)
                                                       (js/Promise.resolve #js {:status "failed" :placed 0 :reason "place-failed: the block is still air"})
                                                       (impl token args)))))))]
          (is (= 67 while-hidden) "the second try raised the whole pillar")
          (is (hiding? seen))
          (is (zero? (count (calls p "attack")))))))))

(deftest a-zombie-in-the-cell-of-the-next-block-is-struck-out-and-the-pillar-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen while-hidden]}
              (await (hidden-round retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                            :entities [(zombie 0.5 64 0.9)]}
                                   1000 (fn [{:keys [p]}] (js/Math.floor (second (:pos (fake/self p)))))
                                   [] (fn [p]
                                        (.override (.-world p) "jumpPlace"
                                                   (fn [token args impl]
                                                     (if (empty? (calls p "attack"))
                                                       (js/Promise.resolve #js {:status "failed" :placed 0 :reason "place-failed: the block is still air"})
                                                       (impl token args)))))))]
          (is (pos? (count (calls p "attack"))) "the zombie standing in the cell is struck")
          (is (= 67 while-hidden) "the pillar went on to its full height")
          (is (hiding? seen)))))))

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

(deftest a-pillared-body-keeps-hiding-past-hide-s-while-a-melee-mob-waits-at-its-foot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen listed]}
              (await (hide-steps retreat {:blocks doorway :inventory [{:name "cobblestone" :count 20}]
                                          :entities [(zombie 0 64 1)]}
                                 [[[(zombie 0 64 1)] 61000] [[(zombie 0 64 1)] 400000]]))]
          (is (= [["j1"] []] listed) "held at 61 s with the zombie at the foot, handed back past :hide-max-s")
          (is (= [:hidden] (hidden-ends seen)))
          (is (= [:spent] (hidden-holds seen)) "the ending says the hide was spent, not quiet"))))))

(deftest hitbox-cells-are-every-cell-a-mob-overlaps
  (is (= #{[0 64 0] [1 64 0] [0 65 0] [1 65 0]} (set (map (juxt :x :y :z) (seal/hitbox-cells {:x 0.8 :y 64 :z 0.5})))))
  (is (= #{[0 64 0] [0 65 0]} (set (map (juxt :x :y :z) (seal/hitbox-cells {:x 0.5 :y 64 :z 0.5}))))))

(def deep-dead-end
  "dead-end over solid rock down to y 58: a tunnel with a roof at y 66, a pickaxe digs down into it."
  (rock [-3 9] [58 67] dead-end-cells))

(deftest an-unarmed-body-with-a-zombie-at-its-heels-digs-down-and-roofs-the-pit-over-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]}
              (await (hidden-round '(jobs.survival.retreat) {:blocks deep-dead-end :inventory [{:name "stone_pickaxe" :count 1}]
                                            :entities [(zombie 1 64 0)]}
                                   1000 (fn [_] nil)))]
          (is (zero? (count (calls p "attack"))) "a pickaxe and a pit beat a pickaxe fight")
          (is (= [[0 63 0] [0 62 0]] (mapv #(let [pos (.. % -args -pos)] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "dig"))) "two cells down: quicker than a pocket's four")
          (is (some #{[0 64 0]} (placed-cells p)) "the roof goes in above the head, shut against the zombie"))))))

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

(deftest the-fight-is-reach-only-when-the-nearest-hostile-is-a-creeper-or-the-body-is-blind
  (let [zombie #js {:name "zombie" :d 2}
        creeper #js {:name "creeper" :d 6}
        near-creeper #js {:name "creeper" :d 2}
        far-zombie #js {:name "zombie" :d 6}
        dist #(.-d %)
        offset #(.-o %)]
    (are [blind? hostiles expected] (= expected (pick/reach-only? blind? hostiles dist offset 5))
      false [zombie creeper] false
      false [creeper zombie] false
      false [near-creeper far-zombie] true
      false [creeper] true
      true [zombie] true
      false [] false)))

(deftest a-step-toward-a-zombie-never-brings-the-body-nearer-a-creeper-in-keep-range
  (let [at (fn [name d o] #js {:name name :d d :o o})
        dist #(.-d %)
        offset #(.-o %)
        ro? (fn [hostiles] (pick/reach-only? false hostiles dist offset 5))]
    (is (true? (ro? [(at "zombie" 2 #js [2 0]) (at "creeper" 4 #js [4 0])])) "creeper on the zombie's side")
    (is (false? (ro? [(at "zombie" 2 #js [2 0]) (at "creeper" 4 #js [-4 0])])) "creeper behind: the step away from it is fine")
    (is (false? (ro? [(at "zombie" 2 #js [2 0]) (at "creeper" 4 #js [0 4])])) "creeper at right angles: the step keeps its distance (first order)")
    (is (false? (ro? [(at "zombie" 2 #js [2 0]) (at "creeper" 9 #js [9 0])])) "creeper beyond keep range")))

(deftest an-unarmed-body-at-a-dead-end-never-walks-to-a-creeper-to-punch-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos [0.5 64 0.5]} :blocks dead-end
                                      :entities [(assoc (creeper 6.5 64 0.5) :visible true)]})
              n (atom 0)]
          (.override (.-world p) "blockAt"
                     (fn [token a impl] (when (= 300 (swap! n inc)) (swap! (fake/state p) assoc :entities [])) (impl token a)))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (zero? (count (calls p "attack"))) "no melee with a creeper out of reach")
          (is (every? #(< (:x %) 3) (tu/walked-to eng)) "no step towards the creeper"))))))

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
  (is (= [1 2 3] (mapv :x (retreat-walk/walk-cells (edge-world 3) {:x 0.5 :y 64 :z 0.5} [1 0] 6))) "the cells past the edge have no floor")
  (is (= [1 2 3] (mapv :x (retreat-walk/walk-cells (edge-world 3) {:x 0.5 :y 64 :z 0.5} [1 0] 3))))
  (is (= [1 2 3 4 5 6] (mapv :x (retreat-walk/walk-cells (edge-world 9) {:x 0.5 :y 64 :z 0.5} [1 0] 6)))))

(defn sealed-world
  "Floor at y 63 everywhere, stone at feet and head height on the four sides of cell 0,0 only: the diagonals open."
  [{:keys [x y z]}]
  (cond
    (= y 63) "stone"
    (and (#{64 65} y) (#{[1 0] [-1 0] [0 1] [0 -1]} [x z])) "stone"
    :else "air"))

(deftest walk-cells-do-not-cut-a-corner-between-two-blocks
  (is (= [] (retreat-walk/walk-cells sealed-world {:x 0.5 :y 64 :z 0.5} (retreat-walk/unit 1 1) 2))
      "a body walled in on four sides cannot squeeze out diagonally")
  (is (= 2 (count (retreat-walk/walk-cells (fn [{:keys [y]}] (if (= y 63) "stone" "air")) {:x 0.5 :y 64 :z 0.5} (retreat-walk/unit 1 1) 2)))
      "past no corner, the diagonal is open"))

(defn ^:async first-act-round
  "Submit spec over world and run its round until its first walk or place (5 s at most); then the mobs leave and the
  round ends. The setup map plus :places-before-walk, the place calls made before the first walk."
  [spec world]
  (let [{:keys [eng p] :as s} (setup world)
        acted? #(or (seq (tu/walked-to eng)) (seq (calls p "place")))]
    (core/submit! eng spec {})
    (let [round (core/tick! eng)]
      (loop [i 0] (when (and (not (acted?)) (< i 250)) (await (sleep 20)) (recur (inc i))))
      (let [places (count (calls p "place"))]
        (swap! (fake/state p) assoc :entities [])
        (await round)
        (assoc s :places-before-walk places)))))

;; low on health in a skeleton's line of fire, but a creeper or a zombie close by: no seal or pit beside it (a blast
;; breaks the wall, a zombie stands in the cell to fill); it walks away first
(deftest a-low-body-with-a-creeper-or-zombie-close-walks-instead-of-taking-cover
  (async done
    (tu/run-async done
      (fn ^:async t []
        (loop [[[what close] & more] [["creeper" (assoc (creeper 0.5 64 3.5) :visible true)]
                                      ["zombie" (assoc (zombie 3.5 64 0.5) :visible true)]]]
          (when what
            (let [{:keys [eng p]}
                  (await (first-act-round retreat {:self {:pos [0.5 64 0.5] :health 8}
                                                   :inventory [{:name "cobblestone" :count 20}]
                                                   :entities [(assoc (skeleton 6.5 64 0.5) :visible true) close]}))]
              (is (not-any? #(> (first %) -3) (placed-cells p)) (str what ": no seal beside it " (placed-cells p)))
              (is (< (:x (first-move eng)) 0) (str what ": it walks away first")))
            (recur more)))))))

(def standing-go-to
  "A go-to whose walk stands searching: it ends a leg at once, the body where it was."
  (fn [n] {:ways (:ways (get registry/jobs 'jobs.movement.go-to))
           :round (fn ^:async stand [c] (swap! n inc) (ctx/end c :limit-reached {:reason :leg :line :job-time} {:arrived false :leg true}))}))


;; a go-to that stands searching under a skeleton's fire is no flight: one stalled step, then it takes cover where it stands
(deftest a-low-body-whose-cover-step-stands-still-takes-cover-where-it-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              wall (into {} (for [z (range 3 6) y [64 65 66]] [(key-of 2 y z) "stone"]))
              {:keys [eng p]} (setup {:self {:pos [0.5 64 0.5] :health 1}
                                      :inventory [{:name "cobblestone" :count 20}]
                                      :blocks wall
                                      :entities [(assoc (skeleton 6.5 64 0.5) :visible true)]})
              eng (assoc-in eng [:jobs 'jobs.movement.go-to] (standing-go-to n))]
          (core/submit! eng retreat {})
          (let [round (core/tick! eng)]
            (loop [i 0] (when (and (empty? (calls p "place")) (< i 250)) (await (sleep 20)) (recur (inc i))))
            (swap! (fake/state p) assoc :entities [])
            (await round))
          (is (pos? (count (calls p "place"))) "sealed in")
          (is (<= 1 @n 2) "after one or two stalled steps"))))))

;; a creeper that the glance round turns up is weighed before the cover decision: the low body walks instead of sealing beside it
(deftest a-creeper-sighted-by-the-glance-round-keeps-a-low-body-from-sealing-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:pos [0.5 64 0.5] :health 8}
                                      :inventory [{:name "cobblestone" :count 20}]
                                      :entities [(assoc (skeleton 6.5 64 0.5) :visible true)]})]
          (with-redefs [cover/glance-away! (fn [_ _]
                                                     (fake/add-entity! p (assoc (creeper 0.5 64 3.5) :visible true))
                                                     (js/Promise.resolve nil))]
            (core/submit! eng retreat {})
            (let [round (core/tick! eng)]
              (loop [i 0] (when (and (empty? (tu/walked-to eng)) (empty? (calls p "place")) (< i 250)) (await (sleep 20)) (recur (inc i))))
              (swap! (fake/state p) assoc :entities [])
              (await round)))
          (is (empty? (calls p "place")) "no seal beside the creeper")
          (is (seq (tu/walked-to eng)) "it walks"))))))

;; a mob that shows up in every second glance (flicker) is re-read once, then the body flees anyway
(deftest a-flickering-threat-set-does-not-stall-the-flee
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              look flight/look!
              {:keys [eng p]} (setup {:self {:pos [0.5 64 0.5] :health 20}
                                      :inventory [{:name "cobblestone" :count 20}]
                                      :entities [(assoc (zombie 5.5 64 0.5) :visible true)]})]
          (with-redefs [flight/look! (fn [c] (cond-> (vec (look c)) (even? (swap! n inc)) (conj #js {:id (str "ghost" @n)})))]
            (core/submit! eng retreat {})
            (let [round (core/tick! eng)]
              (loop [i 0] (when (and (empty? (tu/walked-to eng)) (< i 250)) (await (sleep 20)) (recur (inc i))))
              (swap! (fake/state p) assoc :entities [])
              (await round)))
          (is (seq (tu/walked-to eng)) "it walks despite the changing view"))))))

;; a flee step that ran out of its seconds before the body moved is tried again, not taken for a blocked way
(deftest a-flee-step-that-times-out-before-moving-is-tried-again-not-sealed-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              {:keys [eng p]} (setup {:self {:pos [0.5 64 0.5] :health 20}
                                      :inventory [{:name "cobblestone" :count 20}]
                                      :entities [(assoc (zombie 5.5 64 0.5) :visible true)]})
              slow-once {:ways (:ways (get registry/jobs 'jobs.movement.go-to))
                         :round (fn ^:async slow [c]
                                  (if (= 1 (swap! n inc))
                                    (ctx/end c :limit-reached {:reason :leg :line :job-time} {:arrived false})
                                    (ctx/end c :done nil {:arrived true})))}
              eng (assoc-in eng [:jobs 'jobs.movement.go-to] slow-once)]
          (core/submit! eng retreat {})
          (let [round (core/tick! eng)]
            (loop [i 0] (when (and (< @n 3) (empty? (calls p "place")) (< i 250)) (await (sleep 20)) (recur (inc i))))
            (swap! (fake/state p) assoc :entities [])
            (await round))
          (is (empty? (calls p "place")) "no plug after one slow step")
          (is (<= 3 @n) "the step ran again"))))))

;; a skeleton only heard (its rough spot) is no line of fire to hide from: the low body walks
(deftest a-low-body-does-not-seal-in-from-a-heard-only-skeleton
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng places-before-walk]}
              (await (first-act-round retreat {:self {:pos [0.5 64 0.5] :health 8}
                                               :inventory [{:name "cobblestone" :count 20}]
                                               :entities [(assoc (skeleton 6.5 64 0.5) :visible false)]}))]
          (is (zero? places-before-walk) "no seal")
          (is (< (:x (first-move eng)) 0) "it walks away"))))))

;; cover is judged from the cells the body has seen: a wall it has not seen is no cover (an arrow may pass)
(deftest cover-reads-seen-cells-only
  (let [wall (into {} (for [z (range -3 4) y [64 65 66]] [(key-of 3 y z) "stone"]))
        p (tu/fake-on-floor {:self {:pos [0.5 64 0.5]} :blocks wall})
        skel #js {:name "skeleton" :visible true :pos #js {:x 6.5 :y 64 :z 0.5}}
        behind {:x 0 :y 64 :z 0}]
    (is (true? ((cover/cover-fn p [skel]) behind)) "a seen wall covers")
    (aset p "sensedAt" (fn [pos] (if (= 3 (.-x pos)) #js {:unknown true} (.blockAt p pos))))
    (is (false? ((cover/cover-fn p [skel]) behind)) "an unseen one does not")
    (is (nil? (cover/cover-fn p [#js {:name "skeleton" :visible false :pos #js {:x 6.5 :y 64 :z 0.5}}]))
        "a heard-only skeleton gives no line of fire to judge")))

(def enderman (fn [x y z] {:id 10 :name "enderman" :kind "hostile" :pos {:x x :y y :z z} :health 40}))

(deftest an-angry-enderman-is-not-fled-on-foot-nor-fought-the-body-shelters
  (is (= [:water :roof :plug :plug-dig :fight] (cornered/escape-order {:enderman? true :win? true}))
      "no walk away, no pillar (it is 3 tall), no back-off; a fight only last, whatever the odds"))

(deftest a-body-with-an-enderman-near-walls-itself-in-instead-of-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}] :entities [(enderman 2.5 64 0.5)]})]
          (.override (.-world p) "place"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= [{:x 0.5 :y 64 :z 0.5}] (tu/walked-to eng)) "no flee step: only the seal's step to its cell's middle")
          (is (zero? (count (calls p "attack"))))
          (is (pos? (count (calls p "place"))) "blocks between it and the enderman"))))))

(deftest water-cell-is-the-nearest-seen-water-within-reach
  (let [water #{[3 64 0] [-2 64 1] [9 64 0]}
        at (fn [{:keys [x y z]}] (if (water [x y z]) "water" "air"))]
    (is (= {:x -2 :y 64 :z 1} (cornered/water-cell at {:x 0.5 :y 64 :z 0.5} 6)))
    (is (nil? (cornered/water-cell at {:x 20.5 :y 64 :z 0.5} 6)) "none within reach")))

(deftest a-body-with-an-enderman-near-steps-into-water-nearby
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}] :blocks {(key-of 3 64 1) "water"}
                                      :entities [(enderman 2.5 64 0.5)]})]
          (.override (.-world p) "steer"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (fake/swap-self! p assoc :pos [3.5 64 1.5]) (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= {:x 3 :y 64 :z 1} (first (tu/walked-to eng))) "walks to the water, not away")
          (is (zero? (count (calls p "place")))))))))

(deftest an-enderman-that-stays-ends-the-hold-in-water-and-the-next-option-comes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}] :blocks {(key-of 0 64 0) "water"}
                                      :entities [(enderman 2.5 64 0.5)]})]
          (core/submit! eng retreat {})
          (await (settings/with-settings {:jobs.survival.retreat-cornered/hold-cap 2}
                   (fn ^:async tick [] (await (core/tick! eng)))))
          (is (pos? (count (calls p "place"))) "after the capped holds the body walls itself in"))))))

(deftest roof-cell-is-the-nearest-seen-two-high-gap-under-a-roof
  (let [solid #{[0 63 0] [3 63 0] [3 66 0] [-2 63 1] [-2 65 1] [9 63 0] [9 66 0]}
        at (fn [{:keys [x y z]}] (if (solid [x y z]) "stone" "air"))]
    (is (= {:x 3 :y 64 :z 0} (cornered/roof-cell at {:x 0.5 :y 64 :z 0.5} 8)) "a cell with a block at head height is no gap; one beyond reach is skipped")
    (is (nil? (cornered/roof-cell at {:x 20.5 :y 64 :z 0.5} 8)) "none within reach")))

(deftest roof-cell-prefers-a-cell-no-enderman-can-stand-within-two-cells-of
  (let [roof (for [x (range 3 9) z (range -3 4)] [x 66 z])
        floor (for [x (range 0 9) z (range -3 4)] [x 63 z])
        solid (set (concat roof floor))
        at (fn [{:keys [x y z]}] (if (solid [x y z]) "stone" "air"))]
    (is (= {:x 5 :y 64 :z 0} (cornered/roof-cell at {:x 0.5 :y 64 :z 0.5} 8))
        "the edge cell x=3 is open to a 3-high stand at x=2: it takes the first cell 2 in")))

(deftest a-body-with-an-enderman-near-and-no-water-walks-under-a-nearby-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}]
                                      :blocks {(key-of 3 66 0) "stone" (key-of 3 63 0) "stone"}
                                      :entities [(enderman 2.5 64 0.5)]})]
          (.override (.-world p) "steer"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= {:x 3 :y 64 :z 0} (first (tu/walked-to eng))) "walks under the roof, digs and places nothing")
          (is (zero? (count (calls p "place")))))))))

(deftest a-body-with-an-enderman-near-looks-round-for-a-roof-it-has-not-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}]
                                      :blocks {(key-of 3 66 0) "stone" (key-of 3 63 0) "stone"}
                                      :entities [(enderman 2.5 64 0.5)]})
              sensed (.-sensedAt p)
              looked-east? #(some (fn [l] (> (.. l -args -pos -x) 3.5)) (calls p "look"))]
          (aset p "sensedAt" (fn [pos] (if (and (== 66 (.-y pos)) (not (looked-east?))) #js {:unknown true} (sensed pos))))
          (.override (.-world p) "steer"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (looked-east?) "an unseen cell reads as rock, so the body looks round before it gives up on the roof")
          (is (= {:x 3 :y 64 :z 0} (first (tu/walked-to eng))) "walks under the roof")
          (is (zero? (count (calls p "place")))))))))

(deftest a-body-with-an-enderman-near-looks-up-before-it-takes-the-sky-for-a-roof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}] :entities [(enderman 2.5 64 0.5)]})
              sensed (.-sensedAt p)
              looked? #(some (fn [l] (let [at (.. l -args -pos)] (and (== 66 (js/Math.floor (.-y at))) (== 0 (js/Math.floor (.-x at)))))) (calls p "look"))]
          (aset p "sensedAt" (fn [pos] (if (and (>= (.-y pos) 66) (not (looked?))) #js {:unknown true} (sensed pos))))
          (.override (.-world p) "place"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (looked?) "the cell over the head itself is looked at (a look at a cell far above glances only that one), not read as rock")
          (is (pos? (count (calls p "place"))) "open sky is no roof: it walls itself in"))))))

(deftest nearest-first-orders-the-sides-by-the-melee-chaser-and-keeps-support-before-roof
  (let [feet {:x 0 :y 64 :z 0}
        east {:x 1 :y 64 :z 0} west {:x -1 :y 64 :z 0}
        support {:x 1 :y 66 :z 0} roof {:x 0 :y 66 :z 0}
        cells [east west support roof]]
    (is (= [west east support roof] (retreat-seal/nearest-first feet [{:x -3.5 :y 64 :z 0.5}] cells)) "the side towards it first; support, then roof")
    (is (= [east west support roof] (retreat-seal/nearest-first feet [{:x 0.5 :y 66 :z 0.5}] cells)) "a zombie over the roof does not pull the roof ahead of its support")
    (is (= cells (retreat-seal/nearest-first feet [] cells)) "no chaser: as given")))

(deftest a-creeper-beside-an-enderman-keeps-the-creeper-order
  (is (= :back-off (first (cornered/escape-order {:enderman? true :creeper? true})))))

(deftest a-calm-enderman-is-walked-away-from-not-sealed-against
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}] :entities [(enderman 6 64 0.5)]})]
          (.override (.-world p) "steer"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (neg? (:x (first-move eng))) "away from it")
          (is (zero? (count (calls p "place")))))))))

(deftest a-cornered-body-keeps-fighting-a-mob-that-keeps-hurting-it-though-the-walk-to-it-is-blocked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks dead-end :entities [(zombie 6 64 0)]})
              calls (atom 0)
              hurt! (fn []
                      (swap! calls inc)
                      (swap! (fake/state p) update-in [:self :health] - 0.1)
                      (when (>= @calls 60) (swap! (fake/state p) assoc :entities [])))]
          (.override (.-world p) "steer" (fn [_ _ _] (hurt!) (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (.override (.-world p) "wait" (fn [token a impl] (hurt!) (impl token a)))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (<= 60 @calls) "kept trying until the zombie was gone")
          (is (not-any? #(re-find #"cannot-escape" (pr-str %)) @seen) "never gives up on the mob that is hurting it"))))))

(deftest a-body-in-water-hunted-by-a-drowned-tries-to-swim-out-before-anything-else
  (is (= [:swim-out :plug :pillar :back-off :plug-dig :fight] (cornered/escape-order {:swim? true})))
  (is (= [:fight] (cornered/escape-order {:swim? true :win? true})) "an armed body that wins still fights")
  (is (= :back-off (first (cornered/escape-order {:swim? true :creeper? true})))))

(deftest no-pillar-against-a-climber
  (is (= [:plug :back-off :plug-dig :fight] (cornered/escape-order {:climber? true}))
      "a spider climbs the pillar's side to the body on top"))

(deftest a-pillar-comes-first-against-a-mob-in-a-corner-cell-when-it-fits
  (is (= [:pillar :plug :back-off :plug-dig :fight] (cornered/escape-order {:pillar-first? true :pressed? true}))
      "no fist fight first, and the fight stays the last resort"))

(deftest a-pillar-that-leaves-too-few-blocks-comes-after-the-dig-down
  (is (= [:plug :plug-dig :pillar :back-off :fight] (cornered/escape-order {:lean? true}))
      "the pillar top cannot be roofed in without blocks; a pit regains them")
  (is (= [:plug :pillar :back-off :fight] (cornered/escape-order {:lean? true :home? true})) "no dig-down at home")
  (is (= [:plug :plug-dig :fight] (cornered/escape-order {:lean? true :climber? true :faster? true}))))

(deftest no-step-back-from-a-mob-that-outruns-the-body
  (is (= [:plug :plug-dig :fight] (cornered/escape-order {:climber? true :faster? true})))
  (is (= [:plug :pillar :plug-dig :fight] (cornered/escape-order {:faster? true})) "one that cannot climb is pillared against")
  (is (= [:fight] (cornered/escape-order {:faster? true :win? true}))))

(deftest an-armed-body-with-a-hitter-at-it-fights-before-it-builds
  (is (= [:fight :plug :pillar :back-off :plug-dig] (cornered/escape-order {:pressed? true}))
      "four dirt placed beside a zombie that keeps hitting cost three hits")
  (is (= [:fight :plug :pillar :plug-dig] (cornered/escape-order {:pressed? true :faster? true})))
  (is (= :back-off (first (cornered/escape-order {:pressed? true :creeper? true}))) "a creeper is never fought first")
  (is (= :water (first (cornered/escape-order {:pressed? true :enderman? true})))))

(deftest a-beaten-body-fights-first-only-if-armed-or-out-of-blocks
  (are [armed? blocks? fights?] (= fights? (pick/beaten-fights? {:beaten? true :armed? armed? :blocks? blocks?}))
    true true true
    true false true
    false false true
    false true false)
  (is (false? (pick/beaten-fights? {:beaten? false :armed? true :blocks? false}))))

(deftest an-unarmed-body-hit-twice-on-the-run-fights-instead-of-fleeing-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:entities [(zombie 4 64 0)]})
              follow-and-hit! (fn [r]
                                (let [[x y z] (:pos (fake/self p))]
                                  (swap! (fake/state p) #(-> % (update-in [:self :health] - 0.5) (assoc :entities [])))
                                  (fake/add-entity! p (zombie (+ x 4) y z))
                                  (mem/write! (:store eng) :hurt {:amount 0.5 :attacker {:name "zombie"}} {:cap 50 :ttl 60000})
                                  r))
              leave! (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r))))]
          (.override (.-world p) "steer" (fn [token a impl] (-> (impl token a) (.then follow-and-hit!))))
          (.override (.-world p) "attack" leave!)
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (seq (calls p "attack")) "it swings at the zombie that keeps hitting it")
          (is (<= (count (filter #(= :retreat.target (:kind %)) @seen)) 2)
              "no flee step after the second hit"))))))

(def closed-box
  "A 3x3 room around the body, walls to y 69, roof at y 70, floor at y 63."
  (into {} (for [x (range -2 3) z (range -2 3) y (range 63 71)
                 :when (or (= 2 (Math/abs x)) (= 2 (Math/abs z)) (= y 63) (= y 70))]
             [(str x "," y "," z) "stone"])))

(deftest an-unarmed-body-shut-in-a-closed-room-hit-once-fights-instead-of-backing-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks closed-box :entities [(zombie 1 64 0)]})
              leave! (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r))))]
          (mem/write! (:store eng) :hurt {:amount 0.5 :attacker {:name "zombie"}} {:cap 50 :ttl 60000})
          (.override (.-world p) "attack" leave!)
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (seq (calls p "attack")) "one hit in a box with no way out: it swings")
          (is (not-any? #(and (= :child_started (:kind %)) (= :back-off (:slot %))) @seen) "and never steps back"))))))

(defn ^:async beset-round
  "A retreat of an armed body that carries blocks, at 8 health with zombies about and every walk blocked ((on-walk
  setup-map) runs at each); the mobs leave at its first place or attack. The setup map."
  [zombies on-walk]
  (let [{:keys [eng p] :as s} (setup {:self {:health 8}
                                      :inventory [{:name "iron_sword" :count 1} {:name "dirt" :count 16}]
                                      :entities zombies})
        leave! (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r))))]
    (.override (.-world p) "steer" (fn [_ _ _] (on-walk s) (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
    (.override (.-world p) "place" leave!)
    (.override (.-world p) "attack" leave!)
    (core/submit! eng retreat {})
    (await (core/tick! eng))
    s))

(defn hit-and-closed-on!
  "The zombies, kept off so far, hit the body once and stand just out of arm's reach of it."
  [{:keys [eng p]}]
  (swap! (fake/state p) #(-> % (update-in [:self :health] - 2) (assoc :entities [])))
  (fake/add-entity! p (zombie 2.5 64 0))
  (fake/add-entity! p (assoc (zombie 0 64 2.5) :id 8))
  (mem/write! (:store eng) :hurt {:amount 2 :health 6 :attacker {:name "zombie"}} {:cap 50 :ttl 60000}))

;; the hit lands during a flee step, so the step's own hit-on-the-run check (at its start) missed it
(deftest an-armed-body-hit-during-a-step-that-got-nowhere-fights-before-it-builds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (beset-round [(zombie 6 64 0) (assoc (zombie 0 64 6) :id 8)] hit-and-closed-on!))]
          (is (zero? (count (calls p "place"))) "no seal beside the mobs that just hit it")
          (is (= 1 (count (calls p "attack")))))))))

(deftest a-hit-that-is-healed-before-the-next-step-still-corners-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}] :entities [(zombie 4.5 64 0)]})
              steers (atom 0)]
          (.override (.-world p) "steer"
                     (fn [_ _ _]
                       (when (= 1 (swap! steers inc))
                         (swap! (fake/state p) assoc :entities [])
                         (fake/add-entity! p (zombie 2.5 64 0))
                         (mem/write! (:store eng) :hurt {:amount 0.5 :health 20 :attacker {:name "zombie"}} {:cap 50 :ttl 60000}))
                       (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (.override (.-world p) "place"
                     (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r)))))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (pos? (count (calls p "place"))) "walled in after one healed hit, not walked on")
          (is (= 1 @steers) "no second flee step"))))))

(deftest an-armed-body-with-a-melee-mob-at-its-side-fights-before-it-builds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (beset-round [(zombie 1.5 64 0) (assoc (zombie 0 64 2.5) :id 8)] (constantly nil)))]
          (is (zero? (count (calls p "place"))) "no seal with a mob already in the cell beside it")
          (is (= 1 (count (calls p "attack")))))))))

(deftest an-unarmed-body-with-a-zombie-in-a-corner-cell-pillars-before-it-walls-or-fights
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "dirt" :count 16}] :entities [(zombie 1.5 64 1.5)]})
              leave! (fn [token a impl] (-> (impl token a) (.then (fn [r] (swap! (fake/state p) assoc :entities []) r))))]
          (.override (.-world p) "steer" (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (.override (.-world p) "attack" leave!)
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (zero? (count (calls p "place"))) "no wall while the zombie stands in a corner cell")
          (is (pos? (count (calls p "jumpPlace"))) "the pillar needs no corner cell")
          (is (zero? (count (calls p "attack"))) "no fist fight first"))))))

;; an arrow from a skeleton the body never saw: the damage packet names it, nothing else does
(defn shot!
  [{:keys [eng]}]
  (mem/write! (:store eng) :hurt {:amount 3 :health 8 :damageType "arrow" :attacker {:id 99 :name "skeleton"}} {:cap 50 :ttl 60000}))

(deftest an-armed-body-shot-by-an-unseen-skeleton-seals-before-it-fights-the-mob-at-its-side
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (beset-round [(zombie 2.5 64 0) (assoc (zombie 0 64 2.5) :id 8)] shot!))]
          (is (pos? (count (calls p "place"))) "the line of fire is broken first")
          (is (zero? (count (calls p "attack"))) "no blows traded in the open under arrows"))))))

(deftest a-body-shot-by-an-unseen-skeleton-does-not-walk-up-to-a-melee-mob-to-fight-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:self {:health 8} :inventory [{:name "iron_sword" :count 1}]
                                            :entities [(zombie 5 64 0)]})]
          (.override (.-world p) "steer" (fn [_ _ _] (shot! s) (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (core/submit! eng retreat {})
          (await (core/tick! eng))
          (is (= [] (filterv #(> (:x %) 3) (tu/walked-to eng))) "no walk through the fire to the zombie five blocks off")
          (is (zero? (count (calls p "attack")))))))))

(deftest an-unseen-shooters-place-is-a-point-out-along-the-way-the-arrow-came-from
  (let [entry (fn [data] {:data data})
        arrow {:damageType "arrow" :dir {:x 1 :z 0}}]
    (is (= [{:x 12.5 :y 64 :z 0.5}] (shot/shot-spots [(entry arrow)] {:x 0.5 :y 64 :z 0.5} 12)))
    (is (= [{:x 0.5 :y 64 :z -11.5}]
           (shot/shot-spots [(entry {:attacker {:name "pillager"} :dir {:x 0 :z -1}})] {:x 0.5 :y 64 :z 0.5} 12))
        "a ranged attacker is a shooter")
    (is (= [] (shot/shot-spots [(entry {:attacker {:name "zombie"} :dir {:x 1 :z 0}}) (entry {:damageType "arrow"})]
                                 {:x 0 :y 64 :z 0} 12))
        "a melee hit, and a shot with no way known, give no spot")))

(deftest cover-from-an-unseen-shooter-is-the-far-side-of-a-wall
  (let [p (tu/fake-on-floor {:blocks (tu/box 3 64 -2 3 66 2 "stone")})
        spots [{:x 12.5 :y 64 :z 0.5}]
        covered? (cover/cover-fn p [] spots)]
    (is (nil? (cover/cover-fn p [] nil)) "no seen shooter and no spot: no cover to speak of")
    (is (true? (covered? {:x 0 :y 64 :z 0})) "behind the wall")
    (is (false? (covered? {:x 0 :y 64 :z 8})) "in the open beside it")))

(deftest under-fire-in-a-new-cell-the-covers-that-failed-elsewhere-are-tried-again
  (is (= {:tried #{:back-off} :fire-cell [1 64 0]}
         (pick/retry-covers {:tried #{:plug :plug-dig :back-off} :fire-cell [0 64 0]} [1 64 0])))
  (is (= {:tried #{:plug} :fire-cell [0 64 0]} (pick/retry-covers {:tried #{:plug} :fire-cell [0 64 0]} [0 64 0]))
      "the same cell: a failed plug stays failed"))

(deftest an-unarmed-body-at-full-health-seals-in-against-a-spider-instead-of-running-or-punching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]}
              (await (hidden-round '(jobs.survival.respond-to-hostile {:settings {:jobs.survival.retreat-refuge/hide-s 100000}})
                                   {:self {:health 20 :food 20} :inventory [{:name "cobblestone" :count 20}]
                                    :entities [{:id 9 :name "spider" :kind "hostile" :pos {:x 4 :y 64 :z 0} :health 16}]}
                                   120000 (fn [_] nil)))]
          (is (zero? (count (calls p "attack"))) "no fist fight")
          (is (zero? (count (calls p "steer"))) "no walk: a spider outruns the body")
          (is (pos? (count (calls p "place"))) "it walls itself in")
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen)))))))))

(def pool
  "Water x -4..8, y 64..65, z -4..4 over the floor: dry land beyond it, nearest at x -5."
  (into {} (for [x (range -4 9) y [64 65] z (range -4 5)] [(key-of x y z) "water"])))

(deftest a-bare-body-in-water-with-a-drowned-beside-it-swims-to-dry-land-instead-of-punching
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:pos {:x 0.5 :y 64 :z 0.5}} :blocks pool
                                           :entities [{:id 5 :name "drowned" :kind "hostile" :pos {:x 1.5 :y 64 :z 0.5} :health 20}]})]
          (core/submit! eng retreat {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (empty? (calls p "attack")) "no fist fight")
          (is (= :swim-out (:slot (first (filter #(= :retreat.step-speed (:kind %)) @seen)))) "the walk is the swim-out step")
          (is (= -5 (:x (first-move eng))) "the first walk is to the nearest dry land, west of the pool"))))))

(deftest a-sealed-body-that-keeps-taking-hits-leaves-the-seal-and-fights
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks dead-end :inventory [{:name "cobblestone" :count 20}]
                                           :entities [(assoc (zombie 3 64 0) :visible true)]})
              hurt! #(swap! (fake/state p) update-in [:self :health] - 2)]
          (.override (.-world p) "wait" (fn [token a impl] (when (hiding? seen) (hurt!)) (impl token a)))
          (core/submit! eng '(jobs.survival.retreat {:settings {:jobs.survival.retreat-refuge/hide-s 100000}}) {})
          (await (core/tick! eng))
          (is (= 1 (count (filter #(= :retreat_sealed (:kind %)) @seen))) "sealed first")
          (is (pos? (count (calls p "attack"))) "hit while sealed: it stops hiding and hits back"))))))

(deftest a-seal-step-ends-after-the-block-a-melee-mob-hit-the-body-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks dead-end :inventory [{:name "iron_sword" :count 1} {:name "cobblestone" :count 20}]
                                      :entities [(assoc (zombie 2.6 64 0) :visible true) (assoc (zombie 5 64 0) :id 8 :visible true) (assoc (zombie 7 64 0) :id 10 :visible true)]})
              hit-once (atom false)]
          (.override (.-world p) "place"
                     (fn [token args impl]
                       (when (compare-and-set! hit-once false true)
                         (swap! (fake/state p) update-in [:self :health] - 2)
                         (mem/write! (:store eng) :hurt {:amount 2 :health 18 :hits 1 :attacker {:name "zombie"}} {:cap 50 :ttl 60000}))
                       (impl token args)))
          (core/submit! eng '(jobs.survival.retreat {}) {})
          (await (core/tick! eng))
          (is (= [[1 64 0]] (take 1 (placed-cells p))))
          (is (= 1 (count (take-while #(not= "attack" (.-name %)) (filter #(#{"place" "attack"} (.-name %)) (.-calls (.-world p))))))
              "the second block waits until the fight is decided again"))))))

(deftest a-pillar-beside-a-slope-the-mob-can-climb-does-not-fit
  (let [floor (fn [pos] (if (= 63 (second pos)) "stone" "air"))
        with (fn [cells] (fn [pos] (if (contains? cells pos) "stone" (floor pos))))
        column (fn [x h] (for [y (range 64 (+ 64 h))] [x y 0]))
        slope (set (concat (column 1 3) (column 2 2) (column 3 1)))]
    (is (true? (retreat-pillar/pillar-ok? (with #{}) [0 64 0])) "open ground all round")
    (is (false? (retreat-pillar/pillar-ok? (with slope) [0 64 0]))
        "a slope up to the pillar's top: a zombie walks up and reaches the body")
    (is (true? (retreat-pillar/pillar-ok? (with (set (column 1 3))) [0 64 0]))
        "a wall of the same height with no steps up to it: out of reach")))

(defn ^:async pillar-then-arrives
  "A body pillared up against a zombie; once it hides, mob replaces the zombie (the hold's next wait), and the round runs on
  with the mob gone after that. The seen events and the world's health at the end."
  [mob]
  (let [{:keys [eng p seen]} (setup {:blocks doorway :inventory [{:name "cobblestone" :count 20}] :entities [(zombie 0 64 1)]})
        arrived (atom false)]
    (.override (.-world p) "wait"
               (fn [token a impl]
                 (when (and (hiding? seen) (not @arrived))
                   (reset! arrived true)
                   (fake/add-entity! p (assoc mob :visible true)))
                 (when (and @arrived (some #(= :retreat.hide-breached (:kind %)) @seen))
                   (swap! (fake/state p) assoc :entities []))
                 (impl token a)))
    (core/submit! eng hiding-retreat {})
    (await (core/tick! eng))
    {:seen seen :health (:health (:self @(fake/state p)))}))

(deftest a-hidden-body-leaves-before-a-creeper-or-a-climber-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[mob mob-name] [[(creeper 0 64 5) "creeper"]
                                [(assoc (zombie 0 64 5) :name "spider") "spider"]]]
          (let [{:keys [seen]} (await (pillar-then-arrives mob))
                ev (first (filter #(= :retreat.hide-breached (:kind %)) @seen))]
            (is (= mob-name (:mob ev)) "the arrival is logged with the mob")
            (is (empty? (filter #(= :retreat.hide-hurt (:kind %)) @seen)) "left before being hit")))))))

(deftest an-unarmed-cornered-body-looks-for-a-way-to-walk-before-it-punches
  (doseq [[in held] [[{:armed? false :win? false :sweeps 0 :fist-sweeps 1} true]
                     [{:armed? false :win? false :sweeps 1 :fist-sweeps 1} false]
                     [{:armed? false :win? false :sweeps 0 :fist-sweeps 0} false]
                     [{:armed? true :win? false :sweeps 0 :fist-sweeps 1} false]
                     [{:armed? false :win? true :sweeps 0 :fist-sweeps 1} false]]]
    (is (= held (pick/fist-hold? in)) (pr-str in))))

(deftest a-pillared-body-hurt-on-its-pillar-raises-it-instead-of-fighting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:blocks doorway :inventory [{:name "dirt" :count 20}] :entities [(zombie 0 64 1)]})
              hurt (atom false)]
          (.override (.-world p) "wait"
                     (fn [token a impl]
                       (when (and (hiding? seen) (not @hurt))
                         (reset! hurt true)
                         (swap! (fake/state p) update-in [:self :health] - 2))
                       (when (>= (count (calls p "jumpPlace")) 5) (swap! (fake/state p) assoc :entities []))
                       (impl token a)))
          (core/submit! eng hiding-retreat {})
          (await (core/tick! eng))
          (is (= 5 (count (calls p "jumpPlace"))) "three blocks, then two more once the first three were hit")
          (is (zero? (count (calls p "attack"))) "no fist fight")
          (is (= 1 (count (filter #(= :retreat.hide-hurt (:kind %)) @seen))))
          (let [hostiles (:hostiles (first (filter #(= :retreat.hide-hurt (:kind %)) @seen)))]
            (is (= ["zombie"] (map :name hostiles)) "the hit names who was near")
            (is (every? number? (:pos (first hostiles))) "and where")))))))

(defn stub-ctx
  "A ctx over a fake world whose job memory is the atom m."
  [p m]
  {:primitives p :mem m :remember (fn [& _]) :forget (fn [& _]) :emit (fn [& _])})

(defn with-stub-mem [f]
  (with-redefs [ctx/mem (fn [c] @(:mem c))
                ctx/update-mem! (fn [c f & args] (apply swap! (:mem c) f args))
                ctx/latest (constantly nil)]
    (f)))

(deftest a-flight-ends-without-the-plug-it-had-under-way
  (let [m (atom {:plugging {:at {:x 0 :y 64 :z 0} :option :plug :ways #{:walls}}})
        p (tu/fake-on-floor {:blocks dead-end})]
    (with-stub-mem #(flight/end-flight! (stub-ctx p m) :hidden))
    (is (nil? (:plugging @m)) "the next flight does not resume it")))

(deftest a-plug-dig-is-refused-in-a-body-already-shut-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [m (atom {})
              p (tu/fake-on-floor {:blocks (rock [-3 9] [62 67] #{[0 64 0] [0 65 0]})})
              plugs (atom 0)
              out (with-redefs [ctx/call (fn ^:async f [& _] (swap! plugs inc) :continue)
                                ctx/use (constantly nil)
                                ctx/mem (fn [c] @(:mem c))
                                ctx/update-mem! (fn [c f & args] (apply swap! (:mem c) f args))]
                    (await (refuge/plug-option! (assoc (stub-ctx p m) :args {}) :plug-dig #{:pit :pocket})))]
          (is (nil? out))
          (is (zero? @plugs) "no pit or pocket is dug in a sealed cell, the hide-hold loop"))))))

(defn mob-at [mob-name x] #js {:name mob-name :id 9 :seen true :pos #js {:x x :y 64 :z 0} :distance x})

(def hut
  "A closed 5x3 room on z -1..1, x 0..4, two cells tall, in stone."
  (let [open (set (for [x (range 0 5) y [64 65] z [-1 0 1]] [x y z]))]
    (into {} (for [x (range -1 6) y (range 62 68) z (range -2 3) :when (not (open [x y z]))] [(key-of x y z) "stone"]))))

(deftest a-bare-body-in-a-closed-hut-is-boxed-in-by-a-melee-mob-in-the-room-hit-or-not
  (let [boxed? (fn [world mob] (pick/bare-boxed-in? {:primitives (tu/fake-on-floor world) :quote-call {} :view (fn [] {:now 0 :data {:entries {} :policies {}}})} [mob]))
        closed {:blocks hut}]
    (is (true? (boxed? closed (mob-at "zombie" 4))) "never hurt: no hit is needed")
    (is (false? (boxed? closed (mob-at "zombie" 12))) "beyond close-radius")
    (is (false? (boxed? closed (mob-at "skeleton" 4))))
    (is (false? (boxed? closed (mob-at "creeper" 4))))
    (is (false? (boxed? (assoc closed :inventory [{:name "cobblestone" :count 4}]) (mob-at "zombie" 4))) "blocks to wall with")
    (is (false? (boxed? (assoc closed :inventory [{:name "iron_sword" :count 1}]) (mob-at "zombie" 4))) "armed")
    (is (false? (boxed? {:blocks dead-end} (mob-at "zombie" 4))) "a one-wide tunnel may lead on")
    (is (false? (boxed? {} (mob-at "zombie" 4))) "open ground: it can walk")))

(deftest a-walking-body-takes-cover-after-shot-hits-hits-by-a-ranged-mob
  (let [arrow {:data {:damageType "arrow" :attacker {:name "skeleton"}}}
        melee {:data {:attacker {:name "zombie"}}}
        shot-on-run? (fn [entries carried]
                       (with-redefs [ctx/since (constantly entries)
                                     lb/carried (constantly carried)
                                     flight/blocks (constantly nil)
                                     ctx/now (constantly 100000)]
                         (pick/shot-on-the-run? {})))]
    (is (not (shot-on-run? [] [:cobblestone])))
    (is (not (shot-on-run? [arrow] [:cobblestone])) "one arrow is walked off")
    (is (not (shot-on-run? [arrow melee] [:cobblestone])) "melee hits are the beaten rule's")
    (is (shot-on-run? [arrow arrow] [:cobblestone]))
    (is (shot-on-run? [{:data {:attacker {:name "zombie"} :damages [{:attacker {:name "zombie"}} {:damageType "arrow"} {:attacker {:name "skeleton"}}]}}] [:cobblestone])
        "arrows merged into one loss with a blow")
    (is (shot-on-run? [(assoc-in arrow [:data :hits] 2)] [:cobblestone]))
    (is (not (shot-on-run? [arrow arrow] [])) "no blocks, no cover to take")))

(deftest a-pressed-body-under-fire-builds-cover-first-then-fights
  (are [fire? pressed? tries fights?] (= fights? (pick/press-fights? {:fire? fire? :pressed? pressed? :tries tries :limit 2}))
    false true 1 true
    true true 1 false
    true true 2 false
    true true 3 true
    true false 9 false))
