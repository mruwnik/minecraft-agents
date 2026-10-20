(ns engine.mine-support
  "Shared helpers of the engine.mine test namespaces."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is async]]
            [clojure.string]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.blocks :as lb]
            [jobs.lib.look :as lk]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.tools :as tools]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.expr :as expr]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as ew]
            [jobs.gather.dig-for :as mine]
            [jobs.gather.mine.descent :as mine-descent]
            [jobs.gather.mine.end :as mine-end]
            [jobs.gather.mine.targets :as mine-targets]
            [jobs.gather.mine.torches :as mine-torches]
            [jobs.gather.mine.tunnel :as mine-tunnel]
            [jobs.gather.mine-stay :as stay]
            [jobs.access.stair :as stair]
            [plan.shape :as shape]))

(defn spec
  "The job to submit: jobs.gather.dig-for, or the trip jobs.gather.mine with :trip true in args."
  [args]
  (if (:trip args) (list 'jobs.gather.mine (dissoc args :trip)) (list 'jobs.gather.dig-for args)))

(declare as-child)

(def floor-block
  "The ground under the walks: a block no test here mines, so the floor never counts as a target (stone would)."
  "andesite")

(def sight-opts
  "The perception the fake body runs: a shorter radius and coarser rays than the body's, the same rules."
  {:radius 16 :ray-deg 2})

(defn seeing
  "Fake primitives p with the body's perception over them (engine.perception/wrap), as the body runs; p as it is
  when it already has one."
  [p & [rand]]
  (let [[x y z] (get-in @(fake/state p) [:self :pos])
        [x y z] (mapv #(js/Math.floor %) [x y z])]
    (when-not (:light-default @(fake/state p))
      (swap! (fake/state p) update :light #(merge {[x y z] [0 15] [x (inc y) z] [0 15]} %))))
  (if (aget p "perception") p (perception/wrap p (perception/create (fake-raw/create p) (cond-> sight-opts rand (assoc :rand rand))))))

(defn start
  "An engine over primitives p (made from world, on a floor-block floor, when not given) on dir, seeing through
  perception; one sight pass first (what the body sees where it faces before the job; the job's own looks do the
  rest)."
  [{:keys [world p dir clock shared jobs rand]}]
  (let [clock (or clock (atom 1000000))
        [seen sink] (tu/legacy-capture-sink)
        p (seeing (or p (tu/fake-on-floor (-> world (assoc :floor-block floor-block) (update :inventory #(or % [{:name "torch" :count 10}]))))) rand)
        eng (core/create {:primitives p :jobs (or jobs registry/jobs) :triggers triggers/all :dir (or dir (tu/tmp-dir))
                          :now #(deref clock) :world shared
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (perception/pass! (aget p "perception"))
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-ticks
  [{:keys [eng clock]} n]
  (await (tu/run-ticks eng clock n 700)))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks; the setup map."
  [args world n & [rand]]
  (let [s (start {:world world :rand rand})]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n))
    s))

(defn start-lit
  "start for a world whose cells all have light [sky block]."
  [world light]
  (let [p (tu/fake-on-floor (assoc world :floor-block floor-block))]
    (swap! (fake/state p) assoc :light-default light)
    (start {:p p})))

(defn ^:async scenario-lit
  "scenario in a world whose cells all have light [sky block] (a lit tunnel [0 8], a dark one [0 0])."
  [args world n light]
  (let [s (start-lit world light)]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))

(defn done-events
  "The mine job's ending events."
  [s]
  (filterv #(and (#{:completed :stopped} (:kind %)) (#{"jobs.gather.dig-for" "jobs.gather.mine"} (:name %))) @(:seen s)))

(defn done-event [s] (first (done-events s)))

(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))

(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))

(defn job-mem [{:keys [eng]}] (core/job-memory eng "j1"))

(defn calls [{:keys [p]} name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn moved [{:keys [eng]}] (mapv :data (mem/entries (mem/view (:store eng)) :moved)))

(defn dig-count [s] (count (calls s "dig")))

(defn dug-cells [s] (mapv #(let [p (.-pos (.-args %))] [(.-x p) (.-y p) (.-z p)]) (calls s "dig")))

(defn block-at [{:keys [p]} x y z] (tu/true-block-name p x y z))

(defn cells
  "{\"x,y,z\" block} for x in xs, y in ys, z in zs."
  [block xs ys zs]
  (into {} (for [x xs y ys z zs] [(str x "," y "," z) block])))

(def floor
  "Two layers of dirt under the start, on stone (the ground goes on below what the job digs)."
  (merge (cells "stone" (range -2 3) [61] (range -2 3)) (cells "dirt" (range -2 3) [62 63] (range -2 3))))

(def wide-floor
  "The floor out to x 12, for walks home from the far end of a patch."
  (cells "dirt" (range -2 13) [62 63] (range -3 4)))

(def sand-patch (cells "sand" (range 4 7) [64] (range -1 2)))

;; ------------------------------------------------------------------ mend branches
(defn set-pos! [{:keys [p]} x y z] (fake/swap-self! p assoc :pos [x y z]))

;; ------------------------------------------------------------------ the mend must not eat the count
(def bare-pickaxe [{:name "iron_pickaxe" :count 1}])

(def pickaxe
  "The pickaxe and enough torches that the job does not stock up first (jobs.access.stock/torches!)."
  (conj bare-pickaxe {:name "torch" :count 10}))

(def cobble {"stone" "cobblestone"})

(def stone-floor
  "A stone floor under the start (y 62, 63) on a dirt layer, so a body in a pit has support."
  (merge (cells "dirt" (range -2 3) [61] (range -2 3)) (cells "stone" (range -2 3) [62 63] (range -2 3))))

(def stone-hill (cells "stone" [5 6] [64 65] [0 1 2]))

(defn ground-cell? [{:keys [x y z]}] (and (<= -2 x 2) (<= -2 z 2) (#{62 63} y)))

(defn dug-ground [s] (filterv #(ground-cell? (js->clj (.-pos (.-args %)) :keywordize-keys true)) (calls s "dig")))

;; ------------------------------------------------------------------ zones and footprints
(def farm-zone {:name "farm" :min [2 60 -2] :max [4 70 2] :owner "Miles"})

(def pad-plan {:id "pad" :parts [{:id "p" :box [[6 64 0] [6 64 0]] :want "sand"}]})

(def two-in-two-out {"3,64,0" "sand" "3,64,1" "sand" "6,64,0" "sand" "6,64,1" "sand"})

(defn ^:async zoned
  "Submit the job with args in a fake world, sharing world w; run n ticks; the setup map."
  [args world w n]
  (let [s (start {:world world :shared w})]
    (core/submit! (:eng s) (spec args) {})
    (await (run-ticks s n))
    s))

(def their-claim {:id "c9" :owner "Miles" :status :active :until 9999999999999 :min [5 60 -2] :max [7 70 2]})

;; ------------------------------------------------------------------ seen targets and the strip tunnel
(def rock
  "Stone over x -9..9, y 62..67, z -9..9 with the body's cell (0,64,0) and the one over it air: a body in a pocket."
  (dissoc (cells "stone" (range -9 10) (range 62 68) (range -9 10)) "0,64,0" "0,65,0"))

(defn rock-world [extra]
  {:blocks (merge rock extra) :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory pickaxe})

(defn feet [{:keys [p]}] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))

(defn tunnel-ends [s] (events-of s :mine.tunnel-end))

;; ------------------------------------------------------------------ open-start: where the strip tunnel starts over open cells
(defn open-line
  "name-at of an east-running corridor from x 0 at y 64: air at feet and head for xs, stone floor under floor-xs, rest unseen (nil)."
  [xs floor-xs & [extra]]
  (let [m (merge (into {} (for [x xs y [64 65]] [[x y 0] "air"]))
                 (into {} (for [x floor-xs] [[x 63 0] "stone"]))
                 extra)]
    (fn [{:keys [x y z]}] (get m [x y z]))))

(def east [1 0])

(defn start-x [name-at] (:x (mine-tunnel/open-start name-at {:x 0 :y 64 :z 0} east any?)))

(def corridor (into {} (for [x (range 1 6) y [64 65]] [(str x "," y ",0") "air"])))

(defn ^:async bridge-run
  "A 3-long strip tunnel east over a gap in the floor at [1 63 0], carrying inv."
  [inv]
  (await (scenario {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 3}
                   (assoc (rock-world {"1,63,0" "air" "2,63,0" "air"}) :inventory inv) 120)))

(defn no-floor-end? [s] (boolean (some #(= :no-floor (:reason %)) (tunnel-ends s))))

;; ------------------------------------------------------------------ ore depth and variants
(defn deep-rock
  "Stone over x -3..14, y 50..67, z -4..4 with the body's cell (0,64,0) and the one over it air, plus extra."
  [extra]
  {:blocks (merge (dissoc (cells "stone" (range -3 15) (range 50 68) (range -4 5)) "0,64,0" "0,65,0") extra)
   :drops {"iron_ore" "raw_iron" "deepslate_iron_ore" "raw_iron" "stone" "cobblestone" "gravel" "gravel"}
   :inventory (into pickaxe [{:name "torch" :count 8}])})

(def far-cave
  "A cave corridor east of the body (x 1..6, y 64 and 65) with ore at its far end."
  (merge (cells "air" (range 1 7) [64 65] [0]) {"6,64,0" "iron_ore"}))

(def side-cave "A cave pocket behind the wall beside the tunnel's second cell." {"2,64,1" "air" "2,65,1" "air"})

(defn ^:async facing-west-run
  "A stay-level mine with no heading argument, the body facing west, over rock-world extra with cobblestone carried."
  [extra length ticks]
  (let [s (start {:world (-> (rock-world extra) (assoc :yaw 90) (update :inventory into [{:name "cobblestone" :count 5}]))})]
    (core/submit! (:eng s) (spec {:block "iron_ore" :count 30 :tunnel-length length :stay-level true}) {})
    (await (run-ticks s ticks))
    s))

(defn ^:async stay-run
  "A stay-level mine east over rock-world extra with body memory (kind data pairs) written first; the setup."
  [extra memory]
  (let [s (start {:world (update (rock-world extra) :inventory into [{:name "cobblestone" :count 5}])})]
    (doseq [[kind data] memory] (mem/write! (:store (:eng s)) kind data))
    (core/submit! (:eng s) (spec {:block "iron_ore" :direction "east" :tunnel-length 4 :stay-level true}) {})
    (await (run-ticks s 120))
    s))

(defn ^:async side-ore-run
  "A stay-level mine east with :settings {:jobs.gather.mine-stay/ore-reach reach} in rock with ore at (1 65 -2) beside a pocket of air that the body sees through a one-cell window of its own earlier tunnel (no breach)."
  [reach]
  (let [s (start {:world (update (rock-world {"0,65,-1" "air" "0,65,-2" "air" "1,65,-2" "iron_ore"}) :inventory into [{:name "cobblestone" :count 5}])})]
    (mem/write! (:store (:eng s)) :tunnel-way {:dim "overworld" :origin [0 64 -1] :delta [0 -1] :steps 0})
    (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 4 :stay-level true :settings {:jobs.gather.mine-stay/ore-reach reach}}) {})
    (await (run-ticks s 500))
    s))

(defn shutdown-at-cell!
  "On the first dig of one of cells, the engine shuts down (the round is cut), as tu/shutdown-at!; the atom that says so."
  [{:keys [p eng]} cells]
  (let [done? (atom false)]
    (.override (.-world p) "dig"
               (fn [token a impl]
                 (let [pos (.-pos a)]
                   (if (and (not @done?) (contains? cells [(.-x pos) (.-y pos) (.-z pos)]))
                     (do (reset! done? true)
                         (core/shutdown! eng)
                         (js/Promise.reject (core/cut-error)))
                     (impl token a)))))
    done?))

(defn ^:async run-until!
  "run-ticks one at a time until the atom cut? is true, at most n ticks."
  [s cut? n]
  (loop [i 0]
    (when (and (< i n) (not @cut?))
      (await (run-ticks s 1))
      (recur (inc i)))))

(defn ^:async below-ore-run
  "A stay-level mine east, ore at (0 63 -1) one below a cell of the body's own earlier tunnel beside the start; drops as given."
  [drops]
  (let [s (start {:world (-> (rock-world {"0,64,-1" "air" "0,65,-1" "air" "0,63,-1" "iron_ore" "-1,64,0" "air" "-1,65,0" "air"})
                             (assoc :drops drops)
                             (update :inventory into [{:name "cobblestone" :count 5}]))})]
    (mem/write! (:store (:eng s)) :tunnel-way {:dim "overworld" :origin [0 64 -1] :delta [0 -1] :steps 0})
    (core/submit! (:eng s) (spec {:block "iron_ore" :count 1 :direction "east" :tunnel-length 3 :stay-level true}) {})
    (await (run-ticks s 400))
    s))

(defn ^:async run-until
  "Run ticks until (done? s) or n ticks."
  [s n done?]
  (loop [i 0]
    (when (and (< i n) (not (done? s)))
      (await (run-ticks s 1))
      (recur (inc i)))))

(def shaft-plan
  "A mineshaft by Miles: its top is the body's start cell, a stair east down to y 60."
  (-> {:id "s" :kind :mineshaft :parts [{:id "stair" :stair {:from [0 64 0] :heading :east :to-y 60} :want :clear}]}
      (shape/with-author "Miles")))

(defn shafted
  "Submit args in a deep-rock world sharing plans and zones; run n ticks; the setup map."
  [args extra zones n]
  (zoned args (deep-rock extra) (ew/of-data {"s" shaft-plan} {} zones) n))

(def via-shaft {:block "iron_ore" :count 1 :level 60 :trip true})

(def ore-east {"10,60,0" "iron_ore"})

(defn drop-away!
  "The fake's digs throw their drops to cell at (as a drop that bounced off), instead of the dug cell (in sight)."
  [p at]
  (.override (.-world p) "dig"
             (fn [token args impl]
               (.then (impl token args)
                      (fn [r]
                        (swap! (fake/state p) update :entities
                               (fn [es] (mapv #(if (= "item" (:kind %)) (assoc % :pos at :visible true) %) es)))
                        r)))))

(defn hide-drop-until-beside!
  "The fake's dug drops lie out of sight (in a pocket) until the body stands within 1 of cell [x y z]."
  [p [cx cy cz]]
  (let [beside? (fn [w] (let [[x y z] (fake/body-pos w)
                              d (map - [(js/Math.floor x) y (js/Math.floor z)] [cx cy cz])]
                          (<= (reduce + (map * d d)) 1)))]
    (.override (.-world p) "dig"
               (fn [token args impl]
                 (.then (impl token args)
                        (fn [r]
                          (swap! (fake/state p) update :entities (fn [es] (mapv #(if (= "item" (:kind %)) (assoc % :visible false) %) es)))
                          r))))
    (doseq [walk ["moveTo" "steer"]]
      (.override (.-world p) walk
                 (fn [token args impl]
                   (.then (impl token args)
                          (fn [r]
                            (when (beside? @(fake/state p))
                              (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :visible true) es))))
                            r)))))))

(def walled-ore
  "Iron ore two cells east of the start behind a rock cell at feet height, its top seen over that cell; rock over the
  start leaves no room to step up, so no walk gets beside the ore."
  (-> (merge (cells floor-block [0 1 2 3] [63] [-1 0 1])
             (cells "stone" [1 2 3] [64 65] [-1 0 1])
             (cells "stone" [0] [66] [-1 0 1]))
      (dissoc "1,65,0" "2,65,0")
      (assoc "2,64,0" "iron_ore")))

;; ------------------------------------------------------------------ looking round while digging (card 943cac28)
(defn watched [{:keys [eng]}] (mem/entries (mem/view (:store eng)) :watched))

(defn ^:async dark-ore-dug
  "A dark open corridor with ore 6 blocks east of the body (no tunnel), the self map merged in: the cells dug."
  [self]
  (let [world (rock-world (merge (cells "air" (range 1 6) [64 65] [0]) {"6,64,0" "iron_ore"}))
        p (tu/fake-on-floor (assoc world :floor-block floor-block))
        _ (swap! (fake/state p) update :self merge self)
        _ (swap! (fake/state p) assoc :light-default [0 0])
        s (start {:p p})]
    (core/submit! (:eng s) (spec {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 0}) {})
    (await (run-ticks s 120))
    (dug-cells s)))

;; ------------------------------------------------------------------ torches
(def long-rock
  "Stone over x -9..40, y 62..67, z -9..9 with the body's pocket at the origin."
  (dissoc (cells "stone" (range -9 41) (range 62 68) (range -9 10)) "0,64,0" "0,65,0"))

(defn torch-world [inventory]
  {:blocks long-rock :drops {"iron_ore" "raw_iron" "stone" "cobblestone"} :inventory inventory})

(defn torch-xs
  "The sorted x of the torches standing in the tunnel (z -1..1, y 64..65, x 0..40)."
  [s]
  (vec (sort (for [x (range 0 41) y [64 65] z [-1 0 1]
                   :when (#{"torch" "wall_torch"} (block-at s x y z))]
               x))))

(def torches8 [{:name "iron_pickaxe" :count 1} {:name "torch" :count 8}])

;; an open room beside the tunnel's 5th..10th cells, ore on its far wall: unseen until the tunnel reaches it
(def side-room (merge (cells "air" (range 4 12) [64 65] (range 1 10)) {"10,64,8" "iron_ore"}))

(defn side-torch-cells
  "The cells of torches standing off the tunnel line (z 3..9, y 64..65, x 4..11)."
  [s]
  (vec (for [x (range 4 12) y [64 65] z (range 3 10)
             :when (#{"torch" "wall_torch"} (block-at s x y z))]
         [x y z])))

(defn ^:async branch-torches
  "The torch cells off the tunnel line (z 2..) after a run digging iron ore at the cells in a side room (air, z 1..the
  farthest ore, x 4..15), with the run's args."
  [ores args]
  (let [zmax (apply max (map last ores))
        room (merge (cells "air" (range 4 16) [64 65] (range 1 (inc zmax)))
                    (into {} (for [[x y z] ores] [(str x "," y "," z) "iron_ore"])))
        s (await (scenario (merge {:block "iron_ore" :level false :count (count ores) :direction "east" :tunnel-length 16} args)
                           (assoc (torch-world torches8) :blocks (merge long-rock room)) 300))]
    {:dug (dug-cells s)
     :torches (vec (for [x (range 4 16) y [64 65] z (range 2 (inc zmax))
                         :when (#{"torch" "wall_torch"} (block-at s x y z))]
                     [x y z]))}))

;; ------------------------------------------------------------------ honest ends, soil, fetching the pickaxe
(defn ^:async as-child
  "Run the job with args under a contract parent (ctx/call) for n ticks; {:s setup :out the child's ending :fetch the get-tool calls}.
  get-tool is a stub that records its args and, when give is set, hands over that pickaxe (else stops :no-source)."
  [args world give n & [{:keys [move-to before jobs]}]]
  (let [s (start {:world world})
        _ (when before (before s))
        out (atom nil)
        fetched (atom [])
        stub {:ways [:chest :craft :gather]
              :round (fn ^:async get-tool [c]
                       (swap! fetched conj (:args c))
                       (if give
                         (do (fake/add-item! (:p s) give 1)
                             (when move-to (swap! (fake/state (:p s)) assoc-in [:self :pos] move-to))
                             (ctx/end c :done nil {:tool give}))
                         (ctx/end c :condition-failed :no-source)))
              :args {:block {:default nil} :item {:default nil} :kind {:default nil} :how {:default nil} :depth {:default nil}
                     :minutes {:default nil} :fail-minutes {:default nil} :chain {:default []} :explore {:default true}}}
        parent {:round (fn ^:async mining-parent [c]
                         (let [e (await (ctx/call c :kid (if (:trip args) 'jobs.gather.mine 'jobs.gather.dig-for) (dissoc args :trip)))]
                           (if (= :continue e) :continue (do (reset! out e) (ctx/end c :done nil)))))}
        eng (assoc (:eng s) :jobs (merge (assoc (:jobs (:eng s)) 'mining-parent parent 'jobs.items.get-tool stub) jobs))]
    (core/submit! eng '(mining-parent) {})
    (await (run-ticks (assoc s :eng eng) n))
    {:s s :out @out :fetch @fetched}))

(defn soil-over-stone
  "Dirt from y top-down to bottom+1 over stone below, in a block wide enough for a stair east."
  [top bottom]
  (merge (cells "stone" (range -3 13) (range (- bottom 4) (inc bottom)) (range -3 4))
         (cells "dirt" (range -3 13) (range (inc bottom) (inc top)) (range -3 4))))

;; soil over stone wide enough for a stair in any of the four headings
(def soil-all-round (merge (cells "stone" (range -8 9) (range 55 60) (range -8 9)) (cells "dirt" (range -8 9) (range 60 64) (range -8 9))))

(defn ^:async descent-run
  "A stair-down mine trip of soil over stone; {:s setup :go-to the args of each go-to call}."
  [n]
  (let [s (start {:world {:blocks (soil-over-stone 63 59) :drops cobble :inventory [{:name "stone_pickaxe" :count 1}]}})
        go-tos (atom [])
        real (get-in (:eng s) [:jobs 'jobs.movement.go-to])
        spy (assoc real :round (fn [c] (swap! go-tos conj (:args c)) ((:round real) c)))
        eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'jobs.movement.go-to spy))]
    (fake/swap-self! (:p s) assoc :held "stone_pickaxe")
    (core/submit! eng (spec {:block "stone" :count 1 :direction "east" :tunnel-length 4 :trip true}) {})
    (await (run-ticks (assoc s :eng eng) n))
    {:s s :go-to @go-tos}))

;; ------------------------------------------------------------------ a way out
(defn stair-down-east
  "Stone over x -6..12, y 50..63, z -3..3 with a stair cut east from the surface cell (0,64,0): step i stands at (i, 64-i, 0)."
  [steps]
  (apply dissoc (cells "stone" (range -6 13) (range 50 64) (range -3 4))
         (for [i (range 1 (inc steps)) dy [0 1 2]] (str i "," (+ (- 64 i) dy) ",0"))))

;; ------------------------------------------------------------------ unseen faces are no hazard
(defn classify-in
  "mine-targets/classify of the sand at [0 64 3] (in view) with extra blocks, after the start's sight pass."
  [extra]
  (let [s (start {:world {:blocks (merge {"0,64,3" "sand"} extra)}})]
    (mine-targets/classify {:primitives (:p s)} false {:x 0 :y 64 :z 3})))

(defn ^:async walk-options
  "The options of the walks a sand mine makes in six ticks, with the stair memory written first when stair-way is given."
  [stair-way]
  (let [s (start {:world {:blocks {"15,64,0" "sand"}}})
        opts (atom [])]
    (some->> stair-way (mem/write! (:store (:eng s)) :stair-way))
    (await (tu/with-near-stub (fn ^:async f [_ _ _ o] (swap! opts conj o) :partial)
             (fn ^:async b [] (core/submit! (:eng s) (spec {:block "sand" :tunnel-length 0}) {})
               (await (run-ticks s 6)))))
    @opts))

(defn spy-go-to
  "s with its engine's go-to job spied: [s' atom-of-the-args-of-each-go-to-round]."
  [s]
  (let [go-tos (atom [])
        real (get-in (:eng s) [:jobs 'jobs.movement.go-to])
        spy (assoc real :round (fn [c] (swap! go-tos conj (:args c)) ((:round real) c)))]
    [(assoc s :eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'jobs.movement.go-to spy))) go-tos]))

(defn home-go-tos "The go-to calls that aim at the start cell or a cell next to it: the walk home."
  [go-tos]
  (filterv #(let [{:keys [x y z]} (:pos %)] (and (= 64 y) (<= (+ (abs (- x 0.5)) (abs (- z 0.5))) 1))) go-tos))

;; ------------------------------------------------------------------ an own zone is left before the first dig
(def home-zone {:name "home" :min [-3 60 -3] :max [3 70 3] :owner "Fake"})

(def no-shaft-args {:block "iron_ore" :count 1 :direction "east" :level 58})

(def deep-ore (deep-rock {"9,58,1" "iron_ore"}))

(defn ^:async home-walk-run
  "A mine run whose body is moved off its start before the walk home; refuse? says whether a home walk (the targets with x < 2) fails; {:out :ways} the ending and the :ways of each home walk."
  [refuse?]
  (let [setup (atom nil)
        ways (atom [])
        real (get-in (start {:world {}}) [:eng :jobs 'jobs.movement.go-to])
        spy (assoc real :round (fn ^:async go-to [c]
                                 (if (< (:x (:pos (:args c))) 2)
                                   (do (swap! ways conj (:ways (:args c)))
                                       (swap! (fake/state (:p @setup)) assoc-in [:self :pos] [6.5 64 0.5])
                                       (if (refuse? (:args c))
                                         (ctx/end c :nothing-possible {:reason :unreachable} {})
                                         (await ((:round real) c))))
                                   (await ((:round real) c)))))
        {:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :tunnel-length 0 :trip true}
                                       {:blocks {"3,64,0" "iron_ore"} :drops {"iron_ore" "raw_iron"} :inventory pickaxe :yaw 270} nil 300
                                       {:jobs {'jobs.movement.go-to spy} :before #(reset! setup %)}))]
    {:out out :ways @ways}))
