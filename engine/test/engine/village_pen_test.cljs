(ns engine.village-pen-test
  "jobs.village.pen and jobs.lib.villager-pen against the fake world: a villager shut in alone beside its workstation."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.village-trade-test :as vt]
            [jobs.lib.villager-pen :as vp]
            [jobs.lib.villager :as villager]))

(def job 'jobs.village.pen)

(def ws [2 64 0])

(def shut
  "Floor, walls and roof of the pen at ws along -x (its cell [1 64 0]), the lectern's sides across it, the floor and roof
  of the stand [3 64 0] behind it."
  {"1,63,0" "stone" "1,64,1" "stone" "1,65,1" "stone" "1,66,1" "stone" "1,64,-1" "stone" "1,65,-1" "stone"
   "1,66,-1" "stone" "1,66,0" "stone" "2,66,0" "stone" "2,64,1" "stone" "2,64,-1" "stone" "3,63,0" "stone" "3,66,0" "stone"})

(def box "The stand's three other sides, feet and head." {"3,64,1" "stone" "3,65,1" "stone" "3,64,-1" "stone" "3,65,-1" "stone" "4,64,0" "stone" "4,65,0" "stone"})

(def at-stand {:pos {:x 3.5 :y 64 :z 0.5}})

(def slab {:blocks {"2,65,0" "oak_slab"} :states {"2,65,0" {:type "top"}}})

(def plug {"0,64,0" "stone" "0,65,0" "stone"})

(defn world
  "A fake world spec: the lectern at ws, ground under the stand's box (the walk floor leaves out columns the spec
  fills), blocks over it, the slab unless slab? is false."
  [blocks & [{:keys [slab?] :or {slab? true}}]]
  (cond-> {:blocks (merge {"2,64,0" "lectern" "2,63,0" "stone" "4,63,0" "stone" "3,63,1" "stone" "3,63,-1" "stone"} blocks (when slab? (:blocks slab)))}
    slab? (assoc :states (:states slab))))

(defn jobless [pos] (vt/villager nil {:profession "unemployed" :level 1 :pos pos}))

(defn block [p cell] (get-in @(fake/state p) [:blocks cell]))

(deftest the-pen-along-a-side-is-its-cell-floored-walled-three-high-roofed-with-the-workstation-and-its-near-end
  (let [lay (vp/layout ws [-1 0])]
    (is (= [1 64 0] (:cell lay)))
    (is (= (set (map #(mapv js/parseInt (.split % ",")) (keys shut))) (set (:shut lay))))
    (is (= [2 65 0] (:slab lay)))
    (is (= [[0 64 0] [0 65 0]] (:plug lay)))
    (is (= [3 64 0] (:stand lay)))
    (is (= (set (map #(mapv js/parseInt (.split % ",")) (keys box))) (set (:box lay))))))

(deftest a-near-end-is-shut-when-no-two-open-cells-stand-over-each-other-in-it
  (are [blocks shut?] (= shut? (vp/plug-shut? (tu/fake (world (merge shut blocks))) (vp/layout ws [-1 0])))
    plug true
    {"0,65,0" "stone"} true
    {"0,64,0" "stone" "0,66,0" "stone"} true
    {"0,64,0" "stone"} false
    {} false))

(deftest the-side-chosen-is-the-one-the-villager-stands-in-else-the-pen-already-standing
  (let [along-z {"2,63,1" "stone" "1,64,1" "stone" "1,65,1" "stone" "1,66,1" "stone" "3,64,1" "stone" "3,65,1" "stone"
                 "3,66,1" "stone" "2,66,1" "stone" "2,66,0" "stone"}
        p (tu/fake (world along-z))]
    (is (= [-1 0] (vp/choose p ws (clj->js {:pos {:x 1.5 :y 64 :z 0.5}}))))
    (is (= [0 1] (vp/choose p ws (clj->js {:pos {:x 6.5 :y 64 :z 4.5}}))) "the pen along +z needs the fewest blocks")))

(deftest a-villager-is-penned-only-shut-in-under-the-slab-alone-lit-and-the-body-boxed-in-behind
  (let [in (clj->js {:pos {:x 1.5 :y 64 :z 0.5}})
        penned? (fn [spec] (some? (vp/penned (tu/fake (merge {:self at-stand} spec)) ws in)))
        full (merge shut plug box)]
    (is (penned? (world full)))
    (is (not (penned? (world full {:slab? false}))) "no slab over the lectern: the villager gets out when it is broken")
    (is (not (penned? (world (merge shut box)))) "the near end open")
    (is (not (penned? (assoc (world full) :entities [(jobless {:x 1 :y 64 :z 0}) (assoc (jobless {:x 1 :y 64 :z 0}) :id 2 :uuid "v-2")])))
        "another villager in the cell")
    (is (not (penned? (assoc (world full) :time 1000 :light {} :light-default [0 0]))) "the cell dark")
    (is (not (penned? (world (merge shut plug (dissoc box "3,64,1" "3,65,1"))))) "the stand open: another villager reaches it, next to the lectern")
    (is (not (penned? (world (merge full {"2,64,1" "air"})))) "a side of the lectern open")
    (is (not (penned? (assoc (world full) :self {:pos {:x 6.5 :y 64 :z 0.5}}))) "the body outside its stand")))

(defn ^:async child-outcome
  "Run the pen for villager v-1 as the child of a recording parent, the clock moving 700 ms a tick, until the list is
  empty (at most n ticks)."
  [{:keys [eng clock]} args n]
  (let [out (atom :not-done)
        parent (tu/recording-parent job (merge {:villager "v-1" :pos ws :profession "librarian"} args) out)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng (tu/order '(recording-parent {})) {})
    (loop [i 0]
      (when (and (< i n) (seq (:list (core/state eng))))
        (swap! clock + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    @out))

(defn ^:async run
  "Run the pen on world spec, the body in the stand behind the lectern; env-fn may change the world as the clock moves. [result p seen]."
  [spec args n & [env-fn]]
  (let [{:keys [p seen] :as env} (bd/setup (merge {:self at-stand} spec))
        _ (when env-fn (env-fn env))
        result (await (child-outcome env args n))]
    [result p seen]))

(defn places [p] (count (vt/calls p "place")))

(deftest a-pen-standing-with-the-villager-in-it-is-done-with-nothing-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run (assoc (world (merge shut plug box)) :entities [(jobless {:x 1 :y 64 :z 0})]) {} 50))]
          (is (= :done (:status result)) (pr-str result))
          (is (zero? (places p))))))))

(deftest a-villager-beside-a-bare-workstation-is-shut-in-with-a-top-slab-over-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run (assoc (world {} {:slab? false})
                                            :entities [(jobless {:x 1 :y 64 :z 0})]
                                            :inventory [{:name "cobblestone" :count 24} {:name "oak_slab" :count 1} {:name "torch" :count 2}])
                                     {} 400))
              lay (vp/layout ws [-1 0])]
          (is (= :done (:status result)) (pr-str result))
          (is (every? #(vp/solid? (block p %)) (concat (:shut lay) (:plug lay) (:box lay))))
          (is (= ["oak_slab" "top"] [(block p [2 65 0]) (get-in @(fake/state p) [:states [2 65 0] :type])])))))))

(defn ticking
  "World age runs with the clock (a game tick each 50 ms) at day time 1000: the ticks villagers count by."
  [{:keys [p clock]}]
  (swap! (fake/state p) assoc :time 1000)
  (add-watch clock :age (fn [_ _ _ ms] (swap! (fake/state p) assoc-in [:self :worldAge] (js/Math.floor (/ ms 50))))))

(defn ^:async run-ticking
  "run with the world age ticking."
  [spec args n & [env-fn]]
  (await (run spec args n (fn [env] (ticking env) (when env-fn (env-fn env))))))

(deftest a-villager-that-never-walks-in-ends-not-walked-in-and-the-near-end-stays-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world (merge shut box)) :entities [(jobless {:x 6 :y 64 :z 3})])
                                             {:settings {:jobs.lib.villager/take-job-ticks 200}} 400))]
          (is (= {:status :stopped :reason :not-walked-in} (select-keys result [:status :reason])) (pr-str result))
          (is (nil? (block p [0 64 0]))))))))

(deftest a-second-villager-in-the-cell-keeps-the-near-end-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world (merge shut box)) :entities [(jobless {:x 1 :y 64 :z 0}) (assoc (jobless {:x 0 :y 64 :z 0}) :id 2 :uuid "v-2")])
                                             {:settings {:jobs.lib.villager/take-job-ticks 200}} 400))]
          (is (= {:status :stopped :reason :not-walked-in} (select-keys result [:status :reason])) (pr-str result))
          (is (nil? (block p [0 64 0]))))))))

(deftest the-near-end-is-plugged-once-the-villager-walks-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walk-in (fn [{:keys [p clock]}]
                        (let [at (+ @clock 10000)]
                          (add-watch clock :walk-in
                                     (fn [_ _ _ ms] (when (>= ms at)
                                                      (remove-watch clock :walk-in)
                                                      (swap! (fake/state p) assoc-in [:entities 0 :pos] [1.5 64 0.5]))))))
              [result p] (await (run-ticking (assoc (world (merge shut box)) :entities [(jobless {:x 6 :y 64 :z 3})]
                                                    :inventory [{:name "cobblestone" :count 4}])
                                             {} 400 walk-in))]
          (is (= :done (:status result)) (pr-str result))
          (is (= ["cobblestone" "cobblestone"] [(block p [0 64 0]) (block p [0 65 0])]))
          (is (every? #(vp/solid? (block p %)) (:box (vp/layout ws [-1 0]))) "the box dug open to plug the near end is shut again round the body")
          (is (= [3 64 0] (vp/body-cell p))))))))

(deftest the-workstation-is-placed-last-once-the-villager-is-in-and-the-pen-is-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walk-in (fn [{:keys [p clock]}]
                        (let [at (+ @clock 10000)]
                          (add-watch clock :walk-in
                                     (fn [_ _ _ ms] (when (>= ms at)
                                                      (remove-watch clock :walk-in)
                                                      (swap! (fake/state p) assoc-in [:entities 0 :pos] [1.5 64 0.5]))))))
              bare (update (world (merge shut box)) :blocks dissoc "2,64,0")
              [result p] (await (run-ticking (assoc bare :entities [(jobless {:x 6 :y 64 :z 3})]
                                                    :inventory [{:name "lectern" :count 1} {:name "cobblestone" :count 4}])
                                             {} 400 walk-in))
              placed (mapv #(.-item (.-args %)) (vt/calls p "place"))]
          (is (= :done (:status result)) (pr-str result))
          (is (= "lectern" (last placed)) "nothing is placed after the workstation")
          (is (= 1 (count (filter #{"lectern"} placed))))
          (is (= "lectern" (block p ws))))))))

(deftest a-second-villager-beside-an-open-stand-is-shut-out-of-the-cells-next-to-the-workstation
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run (assoc (world (merge shut plug)) :entities [(jobless {:x 1 :y 64 :z 0}) (assoc (jobless {:x 4 :y 64 :z 1}) :id 2 :uuid "v-2")]
                                            :inventory [{:name "cobblestone" :count 6}])
                                     {} 200))]
          (is (= :done (:status result)) (pr-str result))
          (is (every? #(vp/solid? (block p %)) (:box (vp/layout ws [-1 0])))))))))

(deftest a-reused-pen-gone-dark-is-lit-before-it-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lights (fn [{:keys [p]}]
                       (add-watch (fake/state p) :torch (fn [_ s _ w] (when (and (some #{"torch" "wall_torch"} (vals (:blocks w))) (not= [0 14] (:light-default w)))
                                                                        (swap! s assoc :light-default [0 14])))))
              [result p] (await (run (assoc (world (merge shut plug box)) :entities [(jobless {:x 1 :y 64 :z 0})]
                                            :time 1000 :light {} :light-default [0 0] :inventory [{:name "torch" :count 2}])
                                     {} 200 lights))]
          (is (= :done (:status result)) (pr-str result))
          (is (= 1 (count (filter #{"torch" "wall_torch"} (vals (:blocks @(fake/state p))))))))))))

(deftest walking-ticks-of-a-gap-count-only-the-walking-hours-in-it
  (are [from to n] (= n (- (villager/walk-ticks-before to) (villager/walk-ticks-before from)))
    11500 11700 200
    9500 12500 1000
    11500 36000 10490
    -500 500 490))
