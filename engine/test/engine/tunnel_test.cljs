(ns engine.tunnel-test
  "jobs.access.tunnel: the approach choice, the stops, and whole tunnels against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.access.ledger :as ledger]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as world]
            [jobs.access.tunnel :as tunnel]))

(def job 'jobs.access.tunnel)

;; ---------------------------------------------------------------- pure

(defn world-fn
  "A block-at over a map {[x y z] name}: stone everywhere below y 65 unless named, air above, nil for :unloaded."
  [named]
  (fn [[_ y _ :as cell]]
    (let [n (get named cell (if (< y 65) "stone" "air"))]
      (when-not (= :unloaded n) n))))

(defn approach [named target & {:keys [zones footprints max-length accept] :or {zones [] footprints #{} max-length 24 accept #{}}}]
  (tunnel/approach {:block-at (world-fn named) :zones zones :footprints footprints :ledger #{}}
                   target [0 65 0] max-length accept))

(defn stair-target [{:keys [stand heading]}]
  (let [[dx dz] ({:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]} heading)
        [x y z] stand]
    [(+ x dx) y (+ z dz)]))

(deftest the-surface-of-a-column
  (are [named y] (= y (tunnel/surface (world-fn named) [3 5] 50 80))
    {} 65
    {[3 65 5] "short_grass"} 65
    {[3 65 5] "stone" [3 66 5] "dirt"} 67
    {[3 64 5] "water"} nil
    {[3 64 5] "lava"} nil
    {[3 70 5] :unloaded} nil))

(deftest a-target-straight-below-gets-a-straight-stair
  (let [a (approach {} [0 60 0])]
    (is (= :down (:dir a)))
    (is (= 5 (:steps a)))
    (is (= 0 (:run a)))
    (is (= 6 (:length a)))
    (is (= [0 60 0] (stair-target a)) "the stand's next cell along the heading is the target")))

(deftest eight-down-six-aside-leaves-the-floor-under-the-body
  (is (= {:entry [6 65 9] :heading :north :dir :down :steps 8 :run 0 :length 9 :stand [6 57 1] :target [6 57 0]}
         (approach {} [6 57 0]))
      "the east line from [-3 65 0] is as short and nearer, but its third step cuts the body's floor [0 64 0]"))

(deftest a-line-under-the-body-is-taken-when-it-is-the-only-one
  (let [zones [{:name "north" :min [-30 40 -30] :max [30 80 -1]} {:name "south" :min [-30 40 1] :max [30 80 30]}
               {:name "west" :min [7 40 0] :max [30 80 0]}]]
    (is (= [-3 65 0] (:entry (approach {} [6 57 0] :zones zones))))))

(deftest a-lower-entry-runs-flat-the-rest
  (let [trench (into {} (for [x (range -12 -1) y [61 62 63 64] z (range -12 13)] [[x y z] "air"]))
        a (approach trench [0 61 0])]
    (is (= {:entry [-2 61 0] :heading :east :dir :down :steps 0 :run 1 :length 2 :stand [-1 61 0] :target [0 61 0]} a))))

(deftest a-hazard-on-one-line-takes-another-heading
  (let [a (approach {[-1 63 1] "lava"} [6 57 0])]
    (is (:entry a))
    (is (not= :east (:heading a)))))

(deftest a-zone-or-a-plan-on-one-line-takes-another-heading
  (are [opts] (let [a (apply approach {} [6 57 0] (mapcat identity opts))]
                (and (:entry a) (not= :east (:heading a))))
    {:zones [{:name "cellar" :min [-1 63 0] :max [-1 63 0]}]}
    {:footprints {[-1 63 0] "wall"}}))

(deftest declines-name-the-reason
  (are [named target opts reason] (= reason (:reason (apply approach named target (mapcat identity opts))))
    {} [0 60 0] {:max-length 4} :too-far
    {[0 61 0] "lava"} [0 60 0] {} :hazard
    {[0 62 0] "water"} [0 60 0] {} :hazard
    {[0 61 0] "stone" [0 62 0] "gravel"} [0 60 0] {} :hazard
    {[0 59 0] "air"} [0 60 0] {} :no-floor
    {[1 60 0] "water"} [0 60 0] {} :no-approach
    {} [0 60 0] {:zones [{:name "vault" :min [0 60 0] :max [0 60 0]}]} :zone
    {} [0 60 0] {:footprints {[0 60 0] "wall"}} :footprint
    {} [0 60 0] {:zones nil} :no-zones))

(deftest a-cave-under-every-line-declines
  (let [cave (into {} (for [x (range -12 13) z (range -12 13)] [[x 58 z] "cave_air"]))]
    (is (= :cave-below (:reason (approach cave [0 60 0]))))))

(deftest gravel-over-the-run-is-a-hazard
  (let [trench (into {} (for [x (range -12 -1) y [61 62 63 64] z (range -12 13)] [[x y z] "air"]))
        a (approach (assoc trench [-1 63 0] "gravel") [0 61 0])]
    (is (not= :east (:heading a)) "the run under gravel is not taken")))


(deftest the-line-cells-run-from-the-entry-to-the-stand-then-the-target
  (let [plan {:entry [0 65 0] :heading :north :dir :down :steps 2 :run 1 :target [0 63 -4]}
        cells (tunnel/line-cells plan)]
    (is (= [[0 65 0] [0 64 -1] [0 63 -2] [0 63 -3] [0 63 -4]] cells))
    (is (= 5 (count cells)) "n + 2 cells: 0..n and the target")))

(deftest taxicab-distance
  (are [a b d] (= d (tunnel/taxi a b))
    [0 0 0] [0 0 0] 0
    [0 0 0] [1 -2 3] 6
    [5 5 5] [4 6 3] 4))

(deftest light-falls-one-a-step-from-the-farther-of-head-and-feet
  (let [plan {:entry [0 61 0] :heading :east :dir :down :steps 0 :run 24 :target [25 61 0]}]
    (are [s k light] (= light (tunnel/torch-light plan s k))
      0 0 13
      0 1 12
      0 12 1
      0 13 0
      11 12 12)))

(def sites-plans
  {:straight-down {:entry [0 65 0] :heading :north :dir :down :steps 5 :run 0 :target [0 60 -6]}
   :eight-down {:entry [6 65 9] :heading :north :dir :down :steps 8 :run 0 :target [6 57 0]}
   :flat-run {:entry [-12 61 0] :heading :east :dir :down :steps 0 :run 24 :target [13 61 0]}
   :mixed {:entry [0 65 0] :heading :south :dir :down :steps 4 :run 9 :target [0 61 14]}
   :up {:entry [0 60 0] :heading :west :dir :up :steps 7 :run 2 :target [-10 67 0]}
   :one-run-step {:entry [0 61 0] :heading :east :dir :down :steps 0 :run 1 :target [2 61 0]}
   :nothing {:entry [0 61 0] :heading :east :dir :down :steps 0 :run 0 :target [1 61 0]}})

(deftest torch-sites-of-known-lines
  (are [k sites] (= sites (tunnel/torch-sites (sites-plans k)))
    :straight-down [0]
    :eight-down [0 5]
    :flat-run [0 11 22]
    :one-run-step [0]
    :nothing []))

(deftest sites-light-every-cell-and-each-site-is-lit-by-the-one-before
  (doseq [[k plan] (dissoc sites-plans :nothing)
          :let [sites (tunnel/torch-sites plan)
                n (+ (:steps plan) (:run plan))]]
    (is (every? (fn [cell] (some #(pos? (tunnel/torch-light plan % cell)) sites)) (range 0 (+ n 2)))
        (str k " every cell lit"))
    (is (every? (fn [[before s]] (pos? (tunnel/torch-light plan before (inc s)))) (partition 2 1 sites))
        (str k " the standing cell of a site is lit by the site before"))))

;; ---------------------------------------------------------------- the fake world

(defn stone
  "Stone over x0..x1, y0..y1, z0..z1, as fake blocks."
  [x0 x1 y0 y1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) "stone"])))

(def ground (stone -12 12 50 64 -3 3))
(def pick [{:name "iron_pickaxe" :count 1}])

(defn setup
  "An engine over the fake world; a recording parent runs the tunnel as its child and keeps its result in :out.
  opts: :p and :dir to restart over an earlier engine's world and state (nothing submitted then)."
  [spec args prep & {:keys [dir] :as opts}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (or (:p opts) (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :inventory pick} (dissoc spec :zones :plans))))
        out (atom :not-done)
        w (world/of-data (:plans spec {}) {} (get spec :zones []))
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (or dir (tu/tmp-dir)) :now #(deref clock) :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (prep p)
    (when-not (:p opts) (core/submit! eng '(recording-parent) {}))
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn ^:async tick-out! [{:keys [eng clock] :as s}]
  (loop [i 0]
    (when (and (< i 400) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async tunnel! [spec args prep]
  (await (tick-out! (setup spec args prep))))

(defn digs [p] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true)
                     (filter #(= "dig" (.-name %)) (.-calls (.-world p)))))
(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))
(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))
(defn set-block! [p k n] (.set (.. (.-world p) -state -blocks) k n))

(deftest reaches-a-stand-beside-a-target-eight-down-six-aside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (tunnel! {:blocks (assoc ground "6,57,0" "iron_ore")}
                                                    {:target [6 57 0]} (fn [_])))]
          (is (= :done (:status @out)))
          (is (= :reached (:reason @out)))
          (is (= [5 57 0] (feet p)))
          (is (= [-3 65 0] (:entry @out)))
          (is (= "iron_ore" (block-at p [6 57 0])) "the tunnel leaves the target to its caller")
          (is (= "air" (block-at p [6 58 0])) "and opens the cell over it, so the target's cell can be walked into")
          (is (= 22 (count (:dug @out))) "8 steps of 3 cells, less the air over the ground, and the cell over the target")
          (is (= 1 (count (events-of s :tunnel.done)))))))))

(deftest a-run-after-the-stair-cuts-two-high
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trench (apply dissoc ground (for [x (range -12 -1) y [61 62 63 64] z (range -3 4)] (str x "," y "," z)))
              {:keys [out p]} (await (tunnel! {:blocks trench :self {:pos {:x -5 :y 61 :z 0}}}
                                              {:target [0 61 0]} (fn [_])))]
          (is (= :reached (:reason @out)))
          (is (= [-1 61 0] (feet p)))
          (is (= [{:cell [-1 62 0] :block "stone"} {:cell [-1 61 0] :block "stone"} {:cell [0 62 0] :block "stone"}]
                 (:dug @out))))))))

(deftest lava-showing-up-ahead-stops-and-walks-back-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)
                           n (atom 0)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (when (= 4 (swap! n inc)) (set-block! p "3,61,1" "lava"))
                                    (await (impl token a))))))
              {:keys [out p] :as s} (await (tunnel! {:blocks (assoc ground "6,57,0" "iron_ore")}
                                                    {:target [6 57 0]} prep))]
          (is (= :stopped (:status @out)))
          (is (= :hazard (:reason @out)))
          (is (= [-3 65 0] (feet p)) "back at the entry")
          (is (true? (:out @out)))
          (is (not-any? #(= {:x 3 :y 61 :z 0} %) (digs p)))
          (is (= 1 (count (events-of s :tunnel.stopped)))))))))

(deftest a-declined-approach-digs-nothing-and-does-not-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spec args reason]
                [[{:blocks (assoc ground "6,57,0" "iron_ore") :zones [{:name "test-zone" :min [6 57 0] :max [6 57 0]}]}
                  {:target [6 57 0]} :zone]
                 [{:blocks (assoc ground "6,57,0" "iron_ore")
                   :plans {"wall" {:id "wall" :status :active :parts [{:id "w" :cells [[6 57 0]] :want "stone"}]}}}
                  {:target [6 57 0]} :footprint]
                 [{:blocks ground} {:target [6 57 0] :max-length 6} :too-far]
                 [{:blocks (merge ground (into {} (for [x (range -12 13) z (range -3 4)] [(str x ",55," z) "air"])))}
                  {:target [6 57 0]} :cave-below]
                 [{:blocks ground} {:target [6 57]} :bad-args]]]
          (let [{:keys [out p]} (await (tunnel! spec args (fn [_])))]
            (is (= reason (:reason @out)) (str reason))
            (is (= :stopped (:status @out)) (str reason))
            (is (empty? (digs p)) (str reason))
            (is (= [0 65 0] (feet p)) (str reason))))))))

(deftest no-zone-list-declines-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:blocks ground :zones nil} {:target [6 57 0]} (fn [_]))]
          (dotimes [_ 3] (await (core/tick! eng)))
          (is (empty? (digs p)))
          (is (= 1 (count (events-of s :tunnel.declined)))))))))

(deftest a-cut-mid-dig-resumes-without-digging-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks (assoc ground "6,57,0" "iron_ore")} {:target [6 57 0]} (fn [_]))
              world (.-world p)]
          (dotimes [_ 12] (await (core/tick! eng)))
          (.hold world "dig")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (takeover/take! eng {:who "claude" :why "cut"})
            (await running))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (tick-out! s))
          (is (= :reached (:reason @out)))
          (is (= 22 (count (:dug @out))))
          (is (= 22 (count (distinct (map :cell (:dug @out))))) "each cell recorded once")
          (is (<= (count (digs p)) 23) "only the cut dig is repeated"))))))

(deftest a-restart-resumes-from-the-body-on-the-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              spec {:blocks (assoc ground "6,57,0" "iron_ore")}
              {:keys [eng p]} (setup spec {:target [6 57 0]} (fn [_]) :dir dir)]
          (dotimes [_ 20] (await (core/tick! eng)))
          (let [mid (feet p)
                again (await (tick-out! (setup spec {:target [6 57 0]} (fn [_]) :dir dir :p p)))]
            (is (not= [-3 65 0] mid) "the first engine had entered")
            (is (not= [5 57 0] mid) "and not finished")
            (is (= :reached (:reason @(:out again))))
            (is (= [5 57 0] (feet p)))
            (is (= (count (digs p)) (count (distinct (digs p)))) "no cell dug twice")))))))

(deftest a-blocked-way-back-stops-where-it-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)
                           n (atom 0)]
                       (.override world "steer"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (when (= 3 (swap! n inc)) (set-block! p "-2,65,0" "stone") (set-block! p "-2,66,0" "stone"))
                                      r)))))
              {:keys [out p]} (await (tunnel! {:blocks (assoc ground "6,57,0" "iron_ore")} {:target [6 57 0]} prep))]
          (is (= :stopped (:status @out)))
          (is (= :no-way-back (:reason @out)))
          (is (not= [-3 65 0] (feet p)) "no retreat over a blocked way"))))))

(deftest a-way-back-blocked-on-the-run-stops-at-the-stand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trench (apply dissoc ground (for [x (range -12 -1) y [61 62 63 64] z (range -3 4)] (str x "," y "," z)))
              prep (fn [p]
                     (let [world (.-world p)
                           n (atom 0)]
                       (.override world "steer"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (when (= 2 (swap! n inc))
                                        (doseq [z (range -3 4) y [61 62]] (set-block! p (str "-2," y "," z) "stone")))
                                      r)))))
              {:keys [out p]} (await (tunnel! {:blocks trench :self {:pos {:x -5 :y 61 :z 0}}} {:target [0 61 0]} prep))]
          (is (= :no-way-back (:reason @out)))
          (is (= [-1 61 0] (feet p))))))))

;; ---------------------------------------------------------------- torches

(def torches [{:name "iron_pickaxe" :count 1} {:name "torch" :count 8}])
(def lit-blocks #{"torch" "wall_torch"})
(def eight-down (assoc ground "6,57,0" "iron_ore"))

(defn carried [p item]
  (reduce + (map #(.-count %) (filter #(= item (.-name %)) (array-seq (.-inventory (.self p)))))))

(defn places [p] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true)
                       (filter #(= "place" (.-name %)) (.-calls (.-world p)))))

(defn lit-from-standing
  "The feet cells of the line of result whose light from a torch that stands is under 1."
  [res]
  (let [plan (:line res)
        sites (map :site (:torches res))]
    (remove (fn [k] (some #(pos? (tunnel/torch-light plan % k)) sites))
            (range 0 (+ 2 (:steps plan) (:run plan))))))

(deftest torches-are-hung-on-the-way-in-one-per-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (tunnel! {:blocks eight-down :inventory torches} {:target [6 57 0]} (fn [_])))
              res @out
              sites (tunnel/torch-sites (:line res))]
          (is (= :reached (:reason res)))
          (is (= [5 57 0] (feet p)))
          (is (= 22 (count (:dug res))) "torches never cost a dig")
          (is (= sites (mapv :site (:torches res))))
          (is (every? #(contains? lit-blocks (block-at p (:cell %))) (:torches res)))
          (is (= (mapv :block (:torches res)) (mapv #(block-at p (:cell %)) (:torches res))))
          (is (= (- 8 (count sites)) (carried p "torch")))
          (is (empty? (:unlit res)))
          (is (empty? (lit-from-standing res)) "every feet cell of the line is lit by a torch that stands")
          (is (false? (:keep res))))))))

(deftest a-dead-end-books-its-torches-in-the-ledger-a-kept-tunnel-does-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[keep? n] [[false 2] [true 0]]]
          (let [{:keys [out eng]} (await (tunnel! {:blocks eight-down :inventory torches}
                                                  {:target [6 57 0] :keep keep?} (fn [_])))
                l (ledger/open-entries (mem/view (:store eng)))]
            (is (= :reached (:reason @out)) (str keep?))
            (is (= keep? (:keep @out)) (str keep?))
            (is (= n (count l)) (str keep?))
            (is (every? #(and (= :placed (:state %)) (= :tunnel-torch (:purpose %))) l) (str keep?))
            (is (= (set (map :cell l)) (set (when-not keep? (map :cell (:torches @out))))) (str keep?))))))))

(deftest no-torches-digs-on-and-books-every-site-unlit-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (tunnel! {:blocks eight-down} {:target [6 57 0]} (fn [_])))
              res @out]
          (is (= :reached (:reason res)))
          (is (= 22 (count (:dug res))))
          (is (empty? (:torches res)))
          (is (= (tunnel/torch-sites (:line res)) (mapv :site (:unlit res))))
          (is (every? #(= :no-torches (:reason %)) (:unlit res)))
          (is (= [:no-torches] (mapv :reason (events-of s :tunnel.unlit)))))))))

(deftest too-few-torches-light-the-first-site-and-book-the-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (tunnel! {:blocks eight-down :inventory [{:name "iron_pickaxe" :count 1} {:name "torch" :count 1}]}
                                                    {:target [6 57 0]} (fn [_])))
              res @out]
          (is (= :reached (:reason res)))
          (is (= [0] (mapv :site (:torches res))))
          (is (= [5] (mapv :site (:unlit res))))
          (is (= [:no-torches] (mapv :reason (:unlit res))))
          (is (= 0 (carried p "torch")))
          (is (= 1 (count (events-of s :tunnel.unlit)))))))))

(deftest a-zone-that-forbids-placing-leaves-that-site-unlit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "no-torch" :min [2 60 0] :max [2 61 0] :allow #{:dig}}
              {:keys [out p]} (await (tunnel! {:blocks eight-down :inventory torches :zones [zone]}
                                              {:target [6 57 0]} (fn [_])))
              res @out]
          (is (= :reached (:reason res)))
          (is (= [0] (mapv :site (:torches res))))
          (is (= [[5 :zone]] (mapv (juxt :site :reason) (:unlit res))))
          (is (= 7 (carried p "torch")) "one torch used")
          (is (not-any? #(= {:x 2 :y 61 :z 0} %) (places p))))))))

(deftest a-restart-with-torches-hangs-no-site-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              spec {:blocks eight-down :inventory torches}
              {:keys [eng p]} (setup spec {:target [6 57 0]} (fn [_]) :dir dir)]
          (dotimes [_ 20] (await (core/tick! eng)))
          (let [again (await (tick-out! (setup spec {:target [6 57 0]} (fn [_]) :dir dir :p p)))
                res @(:out again)
                hung (places p)]
            (is (= :reached (:reason res)))
            (is (= (count hung) (count (distinct hung))) "no cell placed twice")
            (is (= (count (tunnel/torch-sites (:line res))) (count (:torches res))))
            (is (empty? (lit-from-standing res)))
            (is (= (count (digs p)) (count (distinct (digs p)))))))))))

;; ---------------------------------------------------------------- the way out

(def with-cobble [{:name "iron_pickaxe" :count 1} {:name "torch" :count 8} {:name "cobblestone" :count 10}])

(defn lava-ahead
  "A prep: the fourth dig puts lava beside the stair further on; a collect leaves the body where it stands."
  [p]
  (let [world (.-world p)
        n (atom 0)]
    (.override world "dig"
               (fn ^:async f [token a impl]
                 (when (= 4 (swap! n inc)) (set-block! p "3,61,1" "lava"))
                 (await (impl token a))))
    (.override world "collect"
               (fn ^:async f [token a impl]
                 (let [at (.. world -state -self -pos)
                       r (await (impl token a))]
                   (set! (.. world -state -self -pos) at)
                   r)))))

(deftest a-stop-in-a-dead-end-takes-the-torches-back-and-seals-the-mouth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p eng] :as s} (await (tunnel! {:blocks eight-down :inventory with-cobble
                                                         :drops {"wall_torch" "torch" "stone" "cobblestone"}}
                                                        {:target [6 57 0]} lava-ahead))
              res @out]
          (is (= :hazard (:reason res)))
          (is (= :stopped (:status res)))
          (is (= [-3 65 0] (feet p)) "back at the entry")
          (is (true? (:out res)))
          (is (= :sealed (:reason (:leave res))))
          (is (= 8 (carried p "torch")) "the torches came back")
          (is (= 1 (count (:taken (:leave res)))))
          (is (empty? (:torches res)) "none stands now")
          (is (every? #(= "cobblestone" (block-at p %)) [[-2 64 0] [-1 64 0] [0 64 0]]) "the mouth is closed")
          (is (= [] (ledger/open-entries (mem/view (:store eng)))))
          (is (= 1 (count (events-of s :tunnel.stopped)))))))))

(deftest a-stop-in-a-kept-tunnel-walks-out-and-leaves-the-torches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (tunnel! {:blocks eight-down :inventory with-cobble
                                               :drops {"wall_torch" "torch" "stone" "cobblestone"}}
                                              {:target [6 57 0] :keep true} lava-ahead))
              res @out]
          (is (= :hazard (:reason res)))
          (is (some? (:walk-out res)) "the plain walk to the entry (the fake cannot step onto the floor torch at the entry)")
          (is (nil? (:leave res)) "no leave-tunnel")
          (is (= 7 (carried p "torch")) "the torch stays")
          (is (= 10 (carried p "cobblestone")))
          (is (= ["torch"] (distinct (map #(if (lit-blocks (block-at p (:cell %))) "torch" "gone") (:torches res))))))))))
