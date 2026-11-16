(ns engine.village-pen-test
  "jobs.village.pen and jobs.lib.villager-pen against the fake world: a villager shut in alone beside its workstation."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.village-trade-test :as vt]
            [jobs.lib.need :as need]
            [jobs.lib.villager-pen :as vp]
            [jobs.lib.villager :as villager]))

(def job 'jobs.village.pen)

(def ws [2 64 0])

(def shut
  "Floor, walls and roof of the pen at ws along -x (its cell [1 64 0]), the lectern's sides across it, the floor and roof
  of the stand [3 64 0] behind it."
  {"1,63,0" "stone" "1,64,1" "stone" "1,65,1" "stone" "1,66,1" "stone" "1,64,-1" "stone" "1,65,-1" "stone"
   "1,66,-1" "stone" "1,66,0" "stone" "2,66,0" "stone"
   "2,64,1" "stone" "2,65,1" "stone" "2,64,-1" "stone" "2,65,-1" "stone" "3,63,0" "stone" "3,66,0" "stone"})

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
    {"0,64,0" "oak_fence"} true
    {"0,64,0" "cobblestone_wall"} true
    {"0,64,0" "stone"} false
    {} false))

(deftest a-pen-whose-plug-and-entry-stand-solid-is-built-though-the-floors-under-them-were-never-seen
  (let [p (tu/fake (merge {:self at-stand} (world (merge shut plug box))))
        sensed (.-sensedAt p)
        lay (vp/layout ws [-1 0])]
    (tu/set-sensed! p (fn [q] (if (#{[0 63 0] [4 63 0]} [(.-x q) (.-y q) (.-z q)]) #js {:unknown true :pos q} (sensed q))))
    (is (= lay (vp/boxed p ws)) "buried floors are no part of the pen's shut cells")
    (is (= [] (vp/with-floors p lay (:plug lay)) (vp/with-floors p lay (:box lay))))
    (is (= [[0 63 0] [0 64 0] [0 65 0]] (vp/with-floors (tu/fake (world shut)) lay (:plug lay)))
        "an open near end is laid on its floor first")))

(deftest the-side-chosen-is-the-one-the-villager-stands-in-else-the-pen-already-standing
  (let [along-z {"2,63,1" "stone" "1,64,1" "stone" "1,65,1" "stone" "1,66,1" "stone" "3,64,1" "stone" "3,65,1" "stone"
                 "3,66,1" "stone" "2,66,1" "stone" "2,66,0" "stone"}
        p (tu/fake (world along-z))]
    (is (= [-1 0] (vp/choose p ws (clj->js {:pos {:x 1.5 :y 64 :z 0.5}}))))
    (is (= [0 1] (vp/choose p ws (clj->js {:pos {:x 6.5 :y 64 :z 4.5}}))) "the pen along +z needs the fewest blocks")))

(deftest the-boxed-stand-of-a-built-pen-has-an-exit-cell-but-a-bare-dead-end-has-none
  (let [full (merge shut plug box)
        exit (fn [spec] (vp/exit-cell (tu/fake (merge {:self at-stand} spec)) ws))]
    (is (some? (exit (world full))))
    (is (nil? (exit (world (merge plug box)))) "no pen round the lectern: a wall is no way out of a pen")
    (is (nil? (exit (world full {:slab? false}))))))

(deftest a-body-boxed-in-with-the-villager-in-the-cell-has-an-exit-cell-though-the-cell-s-floor-and-roof-are-never-seen
  (let [hidden #{[1 63 0] [1 66 0]}
        p (tu/fake (merge {:self at-stand :entities [(jobless {:x 1.5 :y 64 :z 0.5})]} (world (merge shut plug box))))
        sensed (.-sensedAt p)]
    (tu/set-sensed! p (fn [q] (if (hidden [(.-x q) (.-y q) (.-z q)]) #js {:unknown true :pos q} (sensed q))))
    (is (some? (vp/boxed p ws)))
    (is (some? (vp/exit-cell p ws)))))

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
    (is (not (penned? (assoc (world full) :self {:pos {:x 4.5 :y 64 :z 0.5}}))) "the body outside its stand")))

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
                                            :inventory [{:name "cobblestone" :count 24} {:name "oak_slab" :count 1} {:name "oak_fence" :count 1} {:name "torch" :count 2}])
                                     {} 400))
              lay (vp/layout ws [-1 0])]
          (is (= :done (:status result)) (pr-str result))
          (is (every? #(vp/solid? (block p %)) (concat (:shut lay) (:box lay))))
          (is (= ["oak_fence" nil] (mapv #(block p %) (:plug lay))))
          (is (= ["oak_slab" "top"] [(block p [2 65 0]) (get-in @(fake/state p) [:states [2 65 0] :type])])))))))

(deftest the-box-is-shut-with-the-blocks-the-body-breaks-out-of-quickest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run (assoc (world (merge shut plug))
                                            :entities [(jobless {:x 1 :y 64 :z 0})]
                                            :inventory [{:name "cobblestone" :count 24} {:name "oak_planks" :count 12} {:name "iron_axe" :count 1} {:name "oak_fence" :count 1} {:name "torch" :count 2}])
                                     {} 400))
              lay (vp/layout ws [-1 0])]
          (is (= :done (:status result)) (pr-str result))
          (is (every? #(= "oak_planks" (block p %)) (:box lay))))))))

(deftest a-fetch-of-quick-blocks-cheaper-than-the-hand-digs-it-saves-is-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom nil)]
          (with-redefs [need/item-price (constantly 0.5)
                        need/fetch! (fn [_ _ args _ _ _] (reset! asked args) {:kind :condition-failed :cause {:reason :need}})]
            (await (run (assoc (world (merge shut plug))
                               :entities [(jobless {:x 1 :y 64 :z 0})]
                               :inventory [{:name "cobblestone" :count 24} {:name "oak_fence" :count 1} {:name "torch" :count 2}])
                        {} 400)))
          (is (some #{"dirt"} (:any-of @asked)) (pr-str @asked)))))))

(deftest a-fetch-of-quick-blocks-dearer-than-the-hand-digs-it-saves-is-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom nil)
              [result p] (with-redefs [need/item-price (constantly 1e6)
                                       need/fetch! (fn [_ _ args _ _ _] (reset! asked args) {:kind :condition-failed :cause {:reason :need}})]
                           (await (run (assoc (world (merge shut plug))
                                              :entities [(jobless {:x 1 :y 64 :z 0})]
                                              :inventory [{:name "cobblestone" :count 24} {:name "oak_fence" :count 1} {:name "torch" :count 2}])
                                       {} 400)))
              lay (vp/layout ws [-1 0])]
          (is (nil? @asked) (pr-str @asked))
          (is (= :done (:status result)) (pr-str result))
          (is (every? #(= "cobblestone" (block p %)) (:box lay))))))))

(deftest a-pen-over-a-drop-floors-the-near-end-and-the-boxs-entry-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run (-> (world {} {:slab? false})
                                         (update :blocks dissoc "4,63,0")
                                         (assoc :entities [(jobless {:x 1 :y 64 :z 0})]
                                                :inventory [{:name "cobblestone" :count 30} {:name "oak_slab" :count 1} {:name "oak_fence" :count 1} {:name "torch" :count 2}]))
                                     {} 400))]
          (is (= :done (:status result)) (pr-str result))
          (is (every? #(vp/solid? (block p %)) [[0 63 0] [4 63 0]]) "the plug and the entry are placed against a floor"))))))

(deftest a-boxed-body-that-never-saw-the-pen-walls-digs-out-to-look-and-fetches-or-places-nothing-for-walls-that-stand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walls #{[1 64 1] [1 65 1] [1 66 1] [1 64 -1] [1 65 -1] [1 66 -1]}
              hide (fn [{:keys [p]}]
                     (let [sensed (.-sensedAt p)
                           shown (atom false)]
                       (.override (.-world p) "look" (fn [token a impl] (when (not= [3 64 0] (vp/body-cell p)) (reset! shown true)) (impl token a)))
                       (tu/set-sensed! p (fn [q] (if (and (walls [(.-x q) (.-y q) (.-z q)]) (not @shown)) #js {:unknown true :pos q} (sensed q))))))
              [result p] (await (run (assoc (world (merge shut plug box)) :entities [(jobless {:x 1 :y 64 :z 0})]
                                            :inventory [{:name "cobblestone" :count 2}])
                                     {} 400 hide))
              lay (vp/layout ws [-1 0])]
          (is (= :done (:status result)) (pr-str result))
          (is (= (set (:entry lay)) (set (map #(let [a (.-pos (.-args %))] [(.-x a) (.-y a) (.-z a)]) (vt/calls p "place"))))
              "only the entry dug to walk out and look is placed back")
          (is (every? #(vp/solid? (block p %)) (:box lay))))))))

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

(defn villager-leaves-view-at-the-stand
  "The villager walks off out of view the moment the body stands in the stand."
  [{:keys [p]}]
  (add-watch (fake/state p) :leave
             (fn [_ s _ w]
               (->> [w] (filter #(>= (get-in % [:self :pos :x] 0) 3))
                    (run! (fn [_] (remove-watch s :leave)
                            (swap! s assoc-in [:entities 0 :pos] [60 64 3])))))))

(deftest a-villager-seen-before-leaves-the-view-at-the-stand-is-still-penned-for-and-waited-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world shut) :entities [(jobless {:x 6 :y 64 :z 3})]
                                                    :inventory [{:name "cobblestone" :count 10}])
                                             {:settings {:jobs.lib.villager/take-job-ticks 200}} 600 villager-leaves-view-at-the-stand))]
          (is (= :not-walked-in (:reason result)) (pr-str result))
          (is (every? #(vp/solid? (block p %)) (:shut (vp/layout ws [-1 0])))))))))

(defn plug-hides-villager
  "While the near end is plugged and the stand's entry still open, the villager in the cell is out of view (the plug is
  in the way); with the box shut round the body it shows again."
  [{:keys [p]}]
  (let [hidden (atom false)]
    (add-watch (fake/state p) :hide
               (fn [_ s _ w]
                 (let [hide? (boolean (and (get-in w [:blocks [0 64 0]]) (not (get-in w [:blocks [4 65 0]]))))]
                   (when (not= hide? @hidden)
                     (reset! hidden hide?)
                     (swap! s assoc-in [:entities 0 :pos] (if hide? [60 64 3] [1.5 64 0.5]))))))))

(deftest a-villager-hidden-by-the-plug-just-placed-is-still-in-the-pen-and-the-plug-is-not-dug-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world shut) :entities [(jobless {:x 1.5 :y 64 :z 0.5})]
                                                    :inventory [{:name "cobblestone" :count 10} {:name "oak_fence" :count 1}])
                                             {:settings {:jobs.lib.villager/take-job-ticks 200}} 600 plug-hides-villager))]
          (is (not= :no-tool (:reason result)) (pr-str result))
          (is (= "oak_fence" (block p [0 64 0])) "the plug is not dug open"))))))

(deftest a-second-villager-in-the-cell-keeps-the-near-end-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world (merge shut box)) :entities [(jobless {:x 1 :y 64 :z 0}) (assoc (jobless {:x 0 :y 64 :z 0}) :id 2 :uuid "v-2")])
                                             {:settings {:jobs.lib.villager/take-job-ticks 200}} 400))]
          (is (= {:status :stopped :reason :not-walked-in} (select-keys result [:status :reason])) (pr-str result))
          (is (nil? (block p [0 64 0]))))))))

(deftest another-villager-employed-in-the-cell-ends-workstation-taken-early
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world (merge shut box)) :entities [(jobless {:x 6 :y 64 :z 3}) (assoc (vt/villager nil {:profession "librarian" :level 1 :pos {:x 1.5 :y 64 :z 0.5}}) :id 2 :uuid "v-2")])
                                             {:settings {:jobs.lib.villager/take-job-ticks 200}} 400))]
          (is (= {:status :stopped :reason :workstation-taken} (select-keys result [:status :reason])) (pr-str result))
          (is (nil? (block p [0 64 0]))))))))

(deftest a-call-that-ends-not-walked-in-with-the-body-boxed-in-digs-the-boxs-entry-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world (merge shut box)) :entities [(jobless {:x 6 :y 64 :z 3})] :self at-stand)
                                             {:settings {:jobs.lib.villager/take-job-ticks 200}} 600))]
          (is (= :not-walked-in (:reason result)) (pr-str result))
          (is (= [nil nil] (mapv #(block p %) [[4 64 0] [4 65 0]])) "the entry the body walks out by is open"))))))

(defn walk-in-after
  "The fake villager walks into the pen's cell 10 s of clock after the run starts."
  [{:keys [p clock]}]
  (let [at (+ @clock 10000)]
    (add-watch clock :walk-in
               (fn [_ _ _ ms] (->> [ms] (filter #(>= % at))
                                   (run! (fn [_] (remove-watch clock :walk-in)
                                           (swap! (fake/state p) assoc-in [:entities 0 :pos] [1.5 64 0.5]))))))))

(defn lured-in
  "The fake villager walks into the pen's cell the moment the lectern stands at ws (a jobless villager paths to a free
  workstation it can reach); lured then holds the cells of the pen's shut and box not solid at that moment."
  [lured]
  (fn [{:keys [p]}]
    (let [lay (vp/layout ws [-1 0])]
      (add-watch (fake/state p) :lure
                 (fn [_ s _ w]
                   (->> [w] (filter #(= "lectern" (get-in % [:blocks ws])))
                        (run! (fn [w] (remove-watch s :lure)
                                (reset! lured (vec (remove #(vp/solid? (get-in w [:blocks %])) (concat (:shut lay) (:box lay)))))
                                (swap! s assoc-in [:entities 0 :pos] [1.5 64 0.5])))))))))

(deftest a-villager-out-of-view-of-the-boxed-body-is-waited-for-and-the-near-end-plugged-once-it-walks-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run-ticking (assoc (world (merge shut box)) :entities [(jobless {:x 60 :y 64 :z 3})]
                                                    :inventory [{:name "cobblestone" :count 4} {:name "oak_fence" :count 1}])
                                             {} 400 walk-in-after))]
          (is (= :done (:status result)) (pr-str result))
          (is (= ["oak_fence" nil] [(block p [0 64 0]) (block p [0 65 0])]))
          (is (every? #(vp/solid? (block p %)) (:box (vp/layout ws [-1 0]))) "the box dug open to plug the near end is shut again round the body")
          (is (= [3 64 0] (vp/body-cell p))))))))

(deftest a-villager-in-an-open-pen-whose-floor-and-roof-the-stand-never-sees-is-plugged-in-without-a-walk-to-see-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [hidden #{[1 63 0] [1 66 0]}
              hide (fn [{:keys [p]}]
                     (let [sensed (.-sensedAt p)]
                       (tu/set-sensed! p (fn [q] (if (hidden [(.-x q) (.-y q) (.-z q)]) #js {:unknown true :pos q} (sensed q))))))
              [result p] (await (run (assoc (world (merge shut box)) :entities [(jobless {:x 1 :y 64 :z 0})]
                                            :inventory [{:name "cobblestone" :count 4} {:name "oak_fence" :count 1}])
                                     {} 400 hide))]
          (is (= :done (:status result)) (pr-str result))
          (is (= ["oak_fence" nil] [(block p [0 64 0]) (block p [0 65 0])]) "the near end plugged behind the villager")
          (is (every? #(vp/solid? (block p %)) (:box (vp/layout ws [-1 0]))))
          (is (= [3 64 0] (vp/body-cell p))))))))

(defn server-refuses-the-near-end
  "Place answers as the server does for a villager standing at x 1.2 in the cell: its box (0.9-1.5) overlaps the near
  end, so a full block there is refused; a fence's post (0.375-0.625 of the cell) clears it."
  [{:keys [p]}]
  (.override (.-world p) "place"
             (fn ^:async f [token a impl]
               (let [pos (.-pos a)]
                 (if (and (= [0 0] [(.-x pos) (.-z pos)]) (#{64 65} (.-y pos)) (not (.endsWith (.-item a) "_fence")))
                   #js {:status "failed" :reason "Server refused" :refusal #js {:entities #js [#js {:name "villager" :id 1}]}}
                   (await (impl token a)))))))

(deftest a-villager-whose-box-overlaps-the-near-end-is-plugged-in-with-a-fence
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (run (assoc (world (merge shut box)) :entities [(jobless {:x 1.2 :y 64 :z 0.5})]
                                            :inventory [{:name "cobblestone" :count 4} {:name "oak_fence" :count 1}])
                                     {} 400 server-refuses-the-near-end))]
          (is (= :done (:status result)) (pr-str result))
          (is (= ["oak_fence" nil] [(block p [0 64 0]) (block p [0 65 0])])))))))

(deftest a-villager-outside-walks-in-to-the-workstation-placed-once-the-pen-is-shut-and-the-body-boxed-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lured (atom :never)
              bare (update (world shut) :blocks dissoc "2,64,0")
              [result p] (await (run-ticking (assoc bare :entities [(jobless {:x 6 :y 64 :z 3})]
                                                    :inventory [{:name "lectern" :count 1} {:name "cobblestone" :count 30} {:name "oak_fence" :count 1} {:name "torch" :count 2}])
                                             {} 600 (lured-in lured)))]
          (is (= :done (:status result)) (pr-str result))
          (is (= [] @lured) "the lectern goes down only once the box round the body is shut")
          (is (= ["oak_fence" nil] [(block p [0 64 0]) (block p [0 65 0])]))
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
                       (add-watch (fake/state p) :torch
                                  (fn [_ s _ w]
                                    (->> (vals (:blocks w))
                                         (filter #{"torch" "wall_torch"})
                                         (take 1)
                                         (filter (constantly (= [0 0] (:light-default w))))
                                         (run! (fn [_] (swap! s assoc :light-default [0 14])))))))
              [result p] (await (run (assoc (world (merge shut plug box)) :entities [(jobless {:x 1 :y 64 :z 0})]
                                            :time 1000 :light {} :light-default [0 0] :inventory [{:name "torch" :count 2}])
                                     {} 200 lights))]
          (is (= :done (:status result)) (pr-str result))
          (is (= 1 (count (filter #{"torch" "wall_torch"} (vals (:blocks @(fake/state p))))))))))))

(deftest a-pen-still-dark-after-its-light-ends-dark-once-the-same-light-is-read-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result] (await (run (assoc (world (merge shut plug box)) :entities [(jobless {:x 1 :y 64 :z 0})]
                                          :time 1000 :light {} :light-default [0 0] :inventory [{:name "torch" :count 2}])
                                   {} 200))]
          (is (= {:status :stopped :reason :dark :light 0} (select-keys result [:status :reason :light :unseen])) (pr-str result)))))))

(deftest walking-ticks-of-a-gap-count-only-the-walking-hours-in-it
  (are [from to n] (= n (- (villager/walk-ticks-before to) (villager/walk-ticks-before from)))
    11500 11700 200
    9500 12500 1000
    11500 36000 10490
    -500 500 490))

(deftest a-fetch-for-the-pens-fillers-never-digs-the-pens-own-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom nil)
              lay (vp/layout ws [-1 0])]
          (with-redefs [need/fetch! (fn [_ _ _ _ _ opts] (reset! asked opts) {:kind :condition-failed :cause {:reason :need}})]
            (await (run (assoc (world {} {:slab? false}) :entities [(jobless {:x 1 :y 64 :z 0})]
                               :inventory [{:name "cobblestone" :count 1} {:name "oak_slab" :count 1} {:name "torch" :count 2}])
                        {} 400)))
          (is (= (set (vp/cells lay)) (set (:avoid @asked))) (pr-str @asked)))))))
