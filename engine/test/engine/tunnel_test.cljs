(ns engine.tunnel-test
  "jobs.access.tunnel: the approach choice, the stops, and whole tunnels against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [clojure.string :as str]
            [jobs.lib.ledger :as ledger]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.job-api :as job-api]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.access.tunnel :as tunnel]
            [jobs.access.tunnel.line :as ln]))

(def job 'jobs.access.tunnel)

;; ---------------------------------------------------------------- pure

(defn world-fn
  "A block-at over a map {[x y z] name}: stone everywhere below y 65 unless named, air above, nil for :unloaded."
  [named]
  (fn [[_ y _ :as cell]]
    (let [n (get named cell (if (< y 65) "stone" "air"))]
      (when-not (= :unloaded n) n))))

(defn approach [named target & {:keys [zones footprints max-length accept ways feet] :or {zones [] footprints #{} max-length 24 accept #{} feet [0 65 0]}}]
  (ln/approach {:block-at (world-fn named) :zones zones :footprints footprints :ledger #{} :ways ways}
                   target feet max-length accept))

(defn stair-target [{:keys [stand heading]}]
  (let [[dx dz] ({:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]} heading)
        [x y z] stand]
    [(+ x dx) y (+ z dz)]))

(deftest the-surface-of-a-column
  (are [named y] (= y (ln/surface (world-fn named) [3 5] 50 80))
    {} 65
    {[3 65 5] "short_grass"} 65
    {[3 65 5] "stone" [3 66 5] "dirt"} 67
    {[3 64 5] "water"} nil
    {[3 64 5] "lava"} nil
    {[3 70 5] :unloaded} nil))

(def cave-room
  (into {} (for [x [-1 0 1] y [60 61] z [-1 0 1]] [[x y z] "air"])))

(deftest a-body-in-a-cave-enters-the-line-where-it-stands
  (let [a (approach cave-room [4 57 0] :feet [0 60 0])]
    (is (= {:entry [0 60 0] :heading :east :dir :down :steps 3 :run 0 :length 4 :stand [3 57 0] :target [4 57 0]} a))))

(deftest an-unseen-column-is-no-surface-entry
  ;; The body sees only its room: every other cell is unseen, so the sky scan (column-at: unseen = air) must not take
  ;; the first seen stone it meets, deep in the rock, as a surface.
  (let [seen (into (set (keys cave-room)) (for [x (range -10 11) z (range -10 11) y [59 55] :when (or (= y 55) (<= -1 x 1) (<= -1 z 1))] [x y z]))
        in {:block-at (world-fn cave-room) :floor-at #(when (seen %) ((world-fn cave-room) %))
            :column-at #(if (seen %) ((world-fn cave-room) %) "air")
            :zones [] :footprints #{} :ledger #{} :ways nil}
        a (ln/approach in [4 57 0] [0 60 0] 24 #{})]
    (is (= [0 60 0] (:entry a)) (pr-str a))))

(deftest a-shorter-surface-entry-beats-the-body-underground
  (is (= (approach {} [2 64 0]) (approach cave-room [2 64 0] :feet [0 60 0]))))

(deftest a-body-far-underground-in-the-line-enters-it-where-it-stands-rather-than-walk-to-a-surface-entry
  (let [far-room (into {} (for [x [-61 -60 -59] y [20 21] z [-1 0 1]] [[x y z] "air"]))
        a (approach far-room [0 20 0] :feet [-60 20 0] :max-length 64)]
    (is (= {:entry [-60 20 0] :heading :east :dir :down :steps 0 :run 59 :length 60 :stand [-1 20 0] :target [0 20 0]} a))))

(deftest a-body-at-the-targets-height-on-its-line-digs-level-rather-than-a-stair-from-the-surface
  (let [room (into {} (for [x [-1 0 1] y [50 51] z [-1 0 1]] [[x y z] "air"]))
        a (approach room [0 50 20] :feet [0 50 0] :max-length 64)]
    (is (= {:entry [0 50 0] :heading :south :dir :down :steps 0 :run 19 :length 20 :stand [0 50 19] :target [0 50 20]} a)
        "the surface stair is 16 long, shorter than the level run")))

(deftest a-body-that-cannot-stand-where-it-is-is-no-entry
  (is (not= [0 60 0] (:entry (approach {} [4 57 0] :feet [0 60 0])))))

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

(deftest the-floor-of-an-earlier-stair-takes-another-heading-or-declines
  (let [a (approach {} [6 57 0] :ways #{[-1 63 0]})]
    (is (:entry a))
    (is (not= :east (:heading a)) "the east line would cut the way"))
  (is (= :undercuts-way (:reason (approach {} [0 60 0] :ways #{[0 61 0]})))))

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
        cells (ln/line-cells plan)]
    (is (= [[0 65 0] [0 64 -1] [0 63 -2] [0 63 -3] [0 63 -4]] cells))
    (is (= 5 (count cells)) "n + 2 cells: 0..n and the target")))

(def sites-plans
  {:straight-down {:entry [0 65 0] :heading :north :dir :down :steps 5 :run 0 :target [0 60 -6]}
   :eight-down {:entry [6 65 9] :heading :north :dir :down :steps 8 :run 0 :target [6 57 0]}
   :flat-run {:entry [-12 61 0] :heading :east :dir :down :steps 0 :run 24 :target [13 61 0]}
   :mixed {:entry [0 65 0] :heading :south :dir :down :steps 4 :run 9 :target [0 61 14]}
   :up {:entry [0 60 0] :heading :west :dir :up :steps 7 :run 2 :target [-10 67 0]}
   :one-run-step {:entry [0 61 0] :heading :east :dir :down :steps 0 :run 1 :target [2 61 0]}
   :nothing {:entry [0 61 0] :heading :east :dir :down :steps 0 :run 0 :target [1 61 0]}})

(deftest torch-sites-of-known-lines
  (are [k sites] (= sites (ln/torch-sites (sites-plans k)))
    :straight-down [0]
    :eight-down [3]
    :flat-run [12 13]
    :one-run-step [0]
    :nothing []))

(deftest sites-light-every-cell-of-the-line
  (doseq [[k plan] (dissoc sites-plans :nothing)]
    (is (= (ln/line-cells plan) (ln/lit-cells plan (ln/torch-sites plan))) (str k " every cell lit"))))

;; ---------------------------------------------------------------- the fake world

(defn stone
  "Stone over x0..x1, y0..y1, z0..z1, as fake blocks."
  [x0 x1 y0 y1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) "stone"])))

(def ground (stone -12 12 50 64 -3 3))
(def pick [{:name "iron_pickaxe" :count 1}])

(defn setup
  "An engine over the fake world; a recording parent runs the tunnel as its child and keeps its result in :out.
  opts: :p and :dir to restart over an earlier engine's world and state (nothing submitted then); :raw true
  keeps the child's ending in :out (else its outcome), and the parent does not wait on the child's conditions.
  :root true submits the job itself, listed (a called child ends at a :later check, only a root waits)."
  [spec args prep & {:keys [dir] :as opts}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (or (:p opts) (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :inventory pick} (dissoc spec :zones :plans))))
        out (atom :not-done)
        w (world/of-data (:plans spec {}) {} (get spec :zones []))
        parent (cond-> (tu/recording-parent job args out (if (:raw opts) identity tu/outcome))
                 (not (:raw opts)) (assoc :can-start? #(ctx/child-status % :kid job args)))
        eng (core/create {:primitives p :jobs (merge registry/jobs {'recording-parent parent} (:jobs opts))
                          :triggers triggers/all :dir (or dir (tu/tmp-dir)) :now #(deref clock) :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (prep p)
    (let [id (when-not (:p opts) (if (:root opts) (core/submit! eng (list job args) {}) (core/submit! eng '(recording-parent) {})))]
      {:eng eng :p p :clock clock :seen seen :out out :id id})))

(defn ^:async tick-out! [{:keys [eng clock] :as s}]
  (loop [i 0]
    (when (and (< i 400) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async tunnel! [spec args prep & opts]
  (await (tick-out! (apply setup spec args prep opts))))

(defn digs [p] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true)
                     (filter #(= "dig" (.-name %)) (.-calls (.-world p)))))
(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))
(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))
(defn set-block! [p k n] (fake/set-block! p (fake/parse-cell k) n))

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
          (is (= 22 (count (:dug @out))) "8 steps of 3 cells, less the air over the ground, and the cell over the target"))))))

(def beside-target
  (-> ground
      (assoc "6,57,0" "iron_ore")
      (dissoc "5,57,0" "5,58,0" "6,58,0")))

(deftest standing-beside-the-target-with-the-cell-over-it-open-is-already-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (tunnel! {:blocks beside-target :self {:pos {:x 5 :y 57 :z 0}}}
                                              {:target [6 57 0]} (fn [_])))]
          (is (= :done (:status @out)))
          (is (:already? @out))
          (is (empty? (digs p)))
          (is (= [5 57 0] (feet p))))))))

(deftest standing-beside-the-target-with-the-cell-over-it-shut-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (tunnel! {:blocks (assoc beside-target "6,58,0" "stone") :self {:pos {:x 5 :y 57 :z 0}}}
                                              {:target [6 57 0]} (fn [_])))]
          (is (= :done (:status @out)))
          (is (not (:already? @out)))
          (is (= "air" (block-at p [6 58 0]))))))))

(deftest a-tunnel-plans-round-the-floor-of-an-earlier-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:blocks (assoc ground "6,57,0" "iron_ore")} {:target [6 57 0]} (fn [_]))
              _ (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors [[-2 64 0]]})
              {:keys [out p]} (await (tick-out! s))]
          (is (= :done (:status @out)))
          (is (= :reached (:reason @out)))
          (is (= "stone" (block-at p [-2 64 0])) "the way's floor is planned round, not cut or stopped at midway"))))))

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

(deftest a-tunnel-underground-goes-on-walking-and-cutting-its-legs-at-night
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [corridor (apply dissoc ground (for [x (range -12 -1) y [61 62] z (range -3 4)] (str x "," y "," z)))
              {:keys [out p]} (await (tunnel! {:blocks corridor :self {:pos {:x -5 :y 61 :z 0}} :time 18000}
                                              {:target [0 61 0]} (fn [p] (tu/seeing-all p) (swap! (fake/state p) assoc :light-default [0 0]))))]
          (is (= :reached (:reason @out)))
          (is (= [-1 61 0] (feet p))))))))

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
          (is (str/starts-with? (:text @out) "tunnel stopped: hazard")))))))

(deftest a-declined-approach-digs-nothing-and-does-not-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[spec args reason]
                [[{:blocks ground} {:target [6 57 0] :max-length 6} :too-far]
                 [{:blocks (merge ground (into {} (for [x (range -12 13) z (range -3 4)] [(str x ",55," z) "air"])))}
                  {:target [6 57 0]} :cave-below]]]
          (let [{:keys [out p]} (await (tunnel! spec args (fn [_])))]
            (is (= reason (:reason @out)) (str reason))
            (is (= :stopped (:status @out)) (str reason))
            (is (empty? (digs p)) (str reason))
            (is (= [0 65 0] (feet p)) (str reason))))))))

(defn rounds [{:keys [seen]}] (count (filter #(= :round_started (:kind %)) @seen)))

(defn ^:async tick-n! [{:keys [eng clock]} n]
  (dotimes [_ n] (swap! clock + 500) (await (core/tick! eng))))

(deftest a-line-refused-on-every-heading-waits-refused-and-ends-no-result
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec [{:blocks (assoc ground "6,57,0" "iron_ore") :zones [{:name "test-zone" :min [6 57 0] :max [6 57 0]}]}
                      {:blocks (assoc ground "6,57,0" "iron_ore")
                       :plans {"wall" {:id "wall" :parts [{:id "w" :cells [[6 57 0]] :want "stone"}]}}}]]
          (let [{:keys [out p] :as s} (setup spec {:target [6 57 0]} (fn [_]) :root true)]
            (await (tick-n! s 6))
            (is (= :not-done @out) "no :stopped result")
            (is (= 1 (count (:list (core/state (:eng s))))) "stays listed")
            (is (= 0 (rounds s)) "no round ran")
            (is (= 1 (count (filter #(and (= :waiting (:kind %)) (= :refused (:reason %))) @(:seen s)))) "told once")
            (is (empty? (digs p)))
            (is (= [0 65 0] (feet p)))))))))

(deftest no-zone-list-ends-condition-failed-before-any-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (tunnel! {:blocks ground :zones nil} {:target [6 57 0]} (fn [_]) :raw true))]
          (is (= [:condition-failed :no-zones] [(:kind @out) (contract/cause-key @out)]))
          (is (empty? (digs p))))))))

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
            (core/cut! eng (core/holder eng) :test nil)
            (await running))
          (await (tick-out! s))
          (is (= :reached (:reason @out)))
          (is (= 22 (count (:dug @out))))
          (is (= 22 (count (distinct (map :cell (:dug @out))))) "each cell recorded once")
          (is (<= (count (digs p)) 23) "only the cut dig is repeated"))))))

(defn ^:async stall-dig!
  "Tick eng until its nth dig, which digs and never resolves (the body stops mid-tunnel, as at a restart); later digs
  are the real thing."
  [eng p n]
  (let [k (atom 0)
        world (.-world p)]
    (.override world "dig" (fn [token args impl]
                             (if (= n (swap! k inc))
                               (do (impl token args) (js/Promise. (fn [_ _])))
                               (impl token args))))
    (loop [i 0]
      (when (and (< i 40) (< @k n))
        (await (js/Promise.race #js [(core/tick! eng)
                                     (js/Promise. (fn [ok] (let [poll (fn poll [] (if (>= @k n) (ok) (js/setTimeout poll 10)))]
                                                             (poll))))]))
        (recur (inc i))))
    (.override world "dig" nil)))

(deftest a-restart-resumes-from-the-body-on-the-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              spec {:blocks (assoc ground "6,57,0" "iron_ore")}
              {:keys [eng p]} (setup spec {:target [6 57 0]} (fn [_]) :dir dir)]
          (await (stall-dig! eng p 10))
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
          (is (some? (:leave @out)) "a dead end goes through leave-tunnel, which digs its own way out")
          (is (not= :walk-failed (:reason (:leave @out))) "the escape got the body out, or the walk did"))))))

(declare get-tool-stub)

(defn ^:async lost-pickaxe-dead-end!
  "A dead end whose way back is blocked and whose pickaxe is gone, so leave-tunnel's escape stair needs one."
  [args]
  (let [calls (atom [])
        prep (fn [p]
               (let [world (.-world p)
                     n (atom 0)]
                 (.override world "steer"
                            (fn ^:async f [token a impl]
                              (let [r (await (impl token a))]
                                (when (= 3 (swap! n inc))
                                  (set-block! p "-2,65,0" "stone") (set-block! p "-2,66,0" "stone")
                                  (swap! (.-state world) assoc :inventory []))
                                r)))))]
    (assoc (await (tunnel! {:blocks (assoc ground "6,57,0" "iron_ore")} (merge {:target [6 57 0]} args) prep
                           :jobs {'jobs.items.get-tool (get-tool-stub calls nil nil)}))
           :calls calls)))

(deftest a-dead-end-without-a-pickaxe-fetches-for-its-escape-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [calls]} (await (lost-pickaxe-dead-end! {}))]
          (is (seq @calls)))))))

(deftest tunnel-fetch-false-is-passed-to-the-leave-tunnel-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [calls]} (await (lost-pickaxe-dead-end! {:fetch false}))]
          (is (empty? @calls)))))))

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
              {:keys [out p]} (await (tunnel! {:blocks trench :self {:pos {:x -5 :y 61 :z 1}}} {:target [0 61 0]} prep))]
          (is (= :no-way-back (:reason @out)))
          (is (= [-1 61 0] (feet p))))))))

;; ---------------------------------------------------------------- torches

(def torches [{:name "iron_pickaxe" :count 1} {:name "torch" :count 8}])
(def lit-blocks #{"torch" "wall_torch"})
(def eight-down (assoc ground "6,57,0" "iron_ore"))
(def long-down
  "A stair of 30-odd cells: two torch sites."
  (assoc (stone -50 12 20 64 -3 3) "6,30,0" "iron_ore"))

(defn carried [p item]
  (reduce + (map #(.-count %) (filter #(= item (.-name %)) (array-seq (.-inventory (.self p)))))))

(defn places [p] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true)
                       (filter #(= "place" (.-name %)) (.-calls (.-world p)))))

(defn lit-from-standing
  "The feet cells of the line of result whose light from a torch that stands is under 1."
  [res]
  (let [plan (:line res)
        sites (map :site (:torches res))]
    (remove (set (ln/lit-cells plan sites)) (ln/line-cells plan))))

(deftest torches-held-in-the-off-hand-are-hung
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (tunnel! {:blocks eight-down :inventory pick :equipment {"offHand" {:name "torch" :count 8}}}
                                            {:target [6 57 0]} (fn [_])))
              res @out]
          (is (= :reached (:reason res)))
          (is (seq (:torches res)) "torches hung from the off-hand stack")
          (is (empty? (:unlit res))))))))

(deftest torches-are-hung-on-the-way-in-one-per-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (tunnel! {:blocks eight-down :inventory torches} {:target [6 57 0]} (fn [_])))
              res @out
              sites (ln/torch-sites (:line res))]
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
        (doseq [keep? [false true]]
          (let [{:keys [out eng]} (await (tunnel! {:blocks long-down :inventory torches}
                                                  {:max-length 60 :target [6 30 0] :keep keep?} (fn [_])))
                l (ledger/open-entries (mem/view (:store eng)))]
            (is (= :reached (:reason @out)) (str keep?))
            (is (= keep? (:keep @out)) (str keep?))
            (is (= (if keep? 0 (count (ln/torch-sites (:line @out)))) (count l)) (str keep?))
            (is (every? #(and (= :placed (:state %)) (= :tunnel-torch (:purpose %))) l) (str keep?))
            (is (= (set (map :cell l)) (set ({false (map :cell (:torches @out)) true []} keep?))) (str keep?))))))))

(deftest a-tunnel-through-leaves-is-dug-by-hand-with-an-empty-bag-and-no-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [leaves (update-vals (assoc ground "6,57,0" "iron_ore") (constantly "oak_leaves"))
              {:keys [out p] :as s} (await (tunnel! {:blocks (assoc leaves "6,57,0" "iron_ore") :inventory []}
                                                    {:target [6 57 0] :fetch false} (fn [_])))]
          (is (= :reached (:reason @out)))
          (is (seq (digs p)) "dug by hand")
          (is (empty? (filter #(= :need (:slot %)) (events-of s :child_started))))
          (is (empty? (filter #(= :no-tool (:reason %)) (events-of s :waiting)))))))))

(deftest no-pickaxe-ends-with-the-stairs-reason-and-digs-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (tunnel! {:blocks eight-down :inventory []} {:target [6 57 0] :fetch false} (fn [_])))]
          (is (= [:stopped :no-tool "pickaxe"] ((juxt :status :reason :tool) @out)))
          (is (empty? (digs p))))))))

(deftest no-torches-digs-on-and-books-every-site-unlit-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (tunnel! {:blocks eight-down} {:target [6 57 0]} (fn [_])))
              res @out]
          (is (= :reached (:reason res)))
          (is (= 22 (count (:dug res))))
          (is (empty? (:torches res)))
          (is (= (ln/torch-sites (:line res)) (mapv :site (:unlit res))))
          (is (every? #(= :no-torches (:reason %)) (:unlit res)))
          (is (= [:no-torches] (mapv :reason (events-of s :tunnel.unlit)))))))))

(deftest an-unlit-site-names-the-cell-its-torch-would-have-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (await (tunnel! {:blocks eight-down :inventory torches} {:target [6 57 0]} (fn [_])))
              dark (await (tunnel! {:blocks eight-down} {:target [6 57 0]} (fn [_])))]
          (is (= (map (juxt :site :cell) (:torches @(:out lit)))
                 (map (juxt :site :cell) (:unlit @(:out dark))))))))))

(deftest too-few-torches-light-the-first-site-and-book-the-rest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (tunnel! {:blocks long-down :inventory [{:name "iron_pickaxe" :count 1} {:name "torch" :count 1}]}
                                                    {:max-length 60 :target [6 30 0]} (fn [_])))
              res @out
              [first-site & others] (ln/torch-sites (:line res))]
          (is (= :reached (:reason res)))
          (is (= [first-site] (mapv :site (:torches res))))
          (is (= others (mapv :site (:unlit res))))
          (is (every? #(= :no-torches (:reason %)) (:unlit res)))
          (is (= 0 (carried p "torch")))
          (is (= 1 (count (events-of s :tunnel.unlit)))))))))

(deftest a-zone-that-forbids-placing-leaves-that-site-unlit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "no-torch" :min [-12 20 -3] :max [12 50 3] :allow #{:dig}}
              {:keys [out p]} (await (tunnel! {:blocks long-down :inventory torches :zones [zone]}
                                              {:max-length 60 :target [6 30 0]} (fn [_])))
              res @out
              [first-site & others] (ln/torch-sites (:line res))]
          (is (= :reached (:reason res)))
          (is (= [first-site] (mapv :site (:torches res))))
          (is (= (map vector others (repeat :zone)) (mapv (juxt :site :reason) (:unlit res))))
          (is (= 7 (carried p "torch")) "one torch used")
          (is (not-any? #(< (:y %) 50) (places p))))))))

(deftest a-restart-with-torches-hangs-no-site-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              spec {:blocks eight-down :inventory torches}
              {:keys [eng p]} (setup spec {:target [6 57 0]} (fn [_]) :dir dir)]
          (await (stall-dig! eng p 10))
          (let [again (await (tick-out! (setup spec {:target [6 57 0]} (fn [_]) :dir dir :p p)))
                res @(:out again)
                hung (places p)]
            (is (= :reached (:reason res)))
            (is (= (count hung) (count (distinct hung))) "no cell placed twice")
            (is (= (count (ln/torch-sites (:line res))) (count (:torches res))))
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
                 (let [at (get-in @(.-state world) [:self :pos])
                       r (await (impl token a))]
                   (swap! (.-state world) assoc-in [:self :pos] at)
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
          (is (str/starts-with? (:text res) "tunnel stopped: hazard")))))))

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

;; ---------------------------------------------------------------- :fetch

(defn get-tool-stub
  "A jobs.items.get-tool that records its args in calls and hands over give (a pickaxe name) or, when nil, stops :no-source."
  [calls p give]
  {:ways [:chest :craft :gather]
   :round (fn ^:async get-tool [c]
            (swap! calls conj (:args c))
            (if give
              (do (fake/add-item! p give 1) (ctx/end c :done nil {:tool give}))
              (ctx/end c :condition-failed :no-source)))
   :args {:block {:default nil} :item {:default nil} :kind {:default nil} :how {:default nil} :depth {:default nil}
          :minutes {:default nil} :fail-minutes {:default nil} :chain {:default []} :explore {:default true}}})

(defn ^:async fetching-tunnel! [give fetch prep & [extra]]
  (let [calls (atom [])
        p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :inventory [] :blocks ground})
        s (setup {} (cond-> (merge {:target [6 57 0]} extra) (not= :default fetch) (assoc :fetch fetch)) prep :p p
                 :jobs {'jobs.items.get-tool (get-tool-stub calls p give)})
        s (assoc s :id (core/submit! (:eng s) '(recording-parent) {}))]
    (await (tick-out! s))
    (assoc s :calls @calls)))

(deftest a-missing-pickaxe-is-fetched-then-the-tunnel-is-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out calls]} (await (fetching-tunnel! "iron_pickaxe" true (fn [_])))]
          (is (= 1 (count calls)))
          (is (= :reached (:reason @out))))))))

(deftest a-pickaxe-that-cannot-be-fetched-ends-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out calls]} (await (fetching-tunnel! nil true (fn [_])))]
          (is (= 1 (count calls)) "tried once, not again")
          (is (= [:stopped :no-tool] ((juxt :status :reason) @out))))))))

(deftest need-drop-false-digs-by-hand-and-fetches-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out calls]} (await (fetching-tunnel! "iron_pickaxe" true (fn [_]) {:need-drop false}))]
          (is (empty? calls))
          (is (= :reached (:reason @out))))))))

(deftest fetch-false-opts-out-and-never-calls-get-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [calls out]} (await (fetching-tunnel! "iron_pickaxe" false (fn [_])))]
          (is (empty? calls))
          (is (= [:stopped :no-tool] ((juxt :status :reason) @out))))))))

(deftest one-call-is-the-whole-tunnel
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock out p]} (setup {:blocks (assoc ground "6,57,0" "iron_ore") :inventory (conj pick {:name "torch" :count 10})} {:target [6 57 0]} (fn [_]))]
          (swap! clock + 500)
          (await (core/tick! eng))
          (is (= :reached (:reason @out)) "one round digs the stair, the run and opens the target")
          (is (= [5 57 0] (feet p))))))))

(deftest a-diagonal-target-underground-is-reached-in-two-legs-from-the-bodys-own-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deep (merge (stone -12 12 0 64 -3 8) {"0,20,0" "air" "0,21,0" "air" "6,20,5" "iron_ore"})
              {:keys [out p]} (await (tunnel! {:self {:pos {:x 0.5 :y 20 :z 0.5}} :blocks deep}
                                              {:target [6 20 5] :max-length 24} (fn [_])))]
          (is (= :reached (:reason @out)) (pr-str (:reason @out)))
          (is (= [5 20 5] (feet p)) "beside the target, after the shorter axis (z) first")
          (is (= "air" (block-at p [0 20 5])) "the first leg ran to the target's row")
          (is (= "iron_ore" (block-at p [6 20 5]))))))))

(deftest a-given-heading-limits-the-lines-to-it
  (let [a (ln/approach {:block-at (world-fn {}) :zones [] :footprints #{} :ledger #{}} [6 57 0] [0 65 0] 24 #{} :north)]
    (is (= :north (:heading a)))))

(deftest a-straight-target-beyond-max-length-underground-is-reached-in-legs-from-the-bodys-own-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deep (merge (stone -12 30 0 64 -3 8) {"0,20,0" "air" "0,21,0" "air" "14,20,0" "iron_ore"})
              {:keys [out p]} (await (tunnel! {:self {:pos {:x 0.5 :y 20 :z 0.5}} :blocks deep}
                                              {:target [14 20 0] :max-length 8} (fn [_])))]
          (is (= :reached (:reason @out)) (pr-str (:reason @out)))
          (is (= [13 20 0] (feet p)) "beside the target, cut along the body's own row"))))))

(deftest a-first-leg-refused-on-the-shorter-axis-is-tried-on-the-other
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [deep (merge (stone -12 12 0 64 -3 8) {"0,20,0" "air" "0,21,0" "air" "6,20,5" "iron_ore"
                                                    "0,20,3" "water"})
              {:keys [out p]} (await (tunnel! {:self {:pos {:x 0.5 :y 20 :z 0.5}} :blocks deep :inventory pick}
                                              {:target [6 20 5] :max-length 24} (fn [_])))]
          (is (= :reached (:reason @out)) (pr-str @out))
          (is (= [6 20 4] (feet p)) "beside the target, after the long axis (x) first"))))))

(deftest a-run-over-a-hole-in-the-floor-is-bridged-with-a-carried-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [row (merge (stone -12 12 0 64 -3 8) {"0,20,0" "air" "0,21,0" "air" "6,20,0" "iron_ore"})
              holed (merge row {"2,19,0" "air" "3,19,0" "air" "2,18,0" "air" "3,18,0" "air"})
              {:keys [out p]} (await (tunnel! {:self {:pos {:x 0.5 :y 20 :z 0.5}} :blocks holed
                                               :inventory (conj pick {:name "dirt" :count 8})}
                                              {:target [6 20 0] :max-length 24} (fn [_])))]
          (is (= :reached (:reason @out)) (pr-str @out))
          (is (= [5 20 0] (feet p)))
          (is (not= "air" (block-at p [2 19 0])) "the missing floor was placed"))))))

(defn gravel-trip
  "go-to with a one-off fault: the first walk into cell fails after gravel has fallen into it; later walks are go-to's."
  [pa cell]
  (let [real (get registry/jobs 'jobs.movement.go-to)
        tripped (atom false)]
    (assoc real :round (fn ^:async trip [c]
                         (if (and (not @tripped) (= cell (vec (:pos (:args c)))))
                           (do (reset! tripped true)
                               (set-block! @pa (str/join "," cell) "gravel")
                               (ctx/end c :nothing-possible {:reason :stuck} {}))
                           (await ((:round real) c)))))))

(deftest gravel-fallen-into-the-next-cell-is-dug-again-not-a-failed-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [row (merge (stone -12 12 0 64 -3 8) {"0,20,0" "air" "0,21,0" "air" "6,20,0" "iron_ore"})
              pa (atom nil)
              {:keys [out p]} (await (tick-out! (setup {:self {:pos {:x 0.5 :y 20 :z 0.5}} :blocks row :inventory pick}
                                                       {:target [6 20 0] :max-length 24} #(reset! pa %)
                                                       :jobs {'jobs.movement.go-to (gravel-trip pa [3 20 0])})))]
          (is (= :reached (:reason @out)) (pr-str (:reason @out)))
          (is (= [5 20 0] (feet p))))))))

(deftest a-run-cell-gravel-keeps-refilling-ends-refills-naming-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [row (merge (stone -12 12 0 64 -3 8) {"0,20,0" "air" "0,21,0" "air" "6,20,0" "iron_ore"})
              prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (set-block! p "1,20,0" "gravel")
                                      (set-block! p "1,21,0" "gravel")
                                      r)))))
              {:keys [out]} (await (tunnel! {:self {:pos {:x 0.5 :y 20 :z 0.5}} :blocks row :inventory pick}
                                            {:target [6 20 0] :max-length 24} prep))]
          (is (= :refills (:reason @out)) (pr-str @out))
          (is (= [1 21 0] (:cell @out)) "the head cell, dug first, is named"))))))

(deftest a-level-run-is-remembered-as-a-tunnel-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [row (merge (stone -12 12 0 64 -3 8) {"0,20,0" "air" "0,21,0" "air" "6,20,0" "iron_ore"})
              {:keys [eng out]} (await (tunnel! {:self {:pos {:x 0.5 :y 20 :z 0.5}} :blocks row} {:target [6 20 0] :keep true} (fn [_])))]
          (is (= :reached (:reason @out)))
          (is (= [{:origin [0 20 0] :delta [1 0] :steps 5}]
                 (map #(select-keys (:data %) [:origin :delta :steps]) (mem/entries (mem/view (:store eng)) :tunnel-way)))))))))

(deftest one-call-is-a-dead-end-and-its-way-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng clock out p]} (setup {:blocks eight-down :inventory with-cobble
                                                :drops {"wall_torch" "torch" "stone" "cobblestone"}}
                                               {:target [6 57 0]} lava-ahead)]
          (swap! clock + 500)
          (await (core/tick! eng))
          (is (= :hazard (:reason @out)) "one round: in, stopped, torches taken, mouth sealed")
          (is (= :sealed (:reason (:leave @out))))
          (is (= [-3 65 0] (feet p))))))))

(deftest the-widest-line-reads-few-blocks-over-open-air
  (let [reads (atom 0)
        air (fn [_] (swap! reads inc) "air")]
    (ln/approach {:block-at air :zones [] :footprints #{} :ledger #{} :ways nil :ignore-zones? true}
                     [4 64 4] [0 65 0] 64 #{})
    (is (< @reads 40000))))

(deftest a-fetch-child-that-ends-done-while-the-no-tool-stop-persists-ends-no-tool-without-spinning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              orig ctx/call
              prep (fn [p]
                     (let [world (.-world p)
                           k (atom 0)]
                       (.override world "steer"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (when (= 3 (swap! k inc))
                                        (swap! (.-state world) assoc :inventory []))
                                      r)))))]
          (set! ctx/call (fn [c slot job args]
                           (if (= :need slot)
                             (do (swap! n inc) (js/Promise.resolve (contract/ending :done nil {:tool "iron_pickaxe"})))
                             (orig c slot job args))))
          (try
            (let [s (await (tunnel! {:blocks (assoc ground "6,57,0" "iron_ore")} {:target [6 57 0]} prep))]
              (is (= [:stopped :no-tool] ((juxt :status :reason) @(:out s))) "the fetch that cured nothing is not asked again"))
            (finally (set! ctx/call orig)))
          (is (<= 1 @n 2) (str "fetch child calls " @n)))))))

(deftest a-tunnel-run-digs-an-exposed-ore-beside-the-way-but-never-its-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [trench (apply dissoc ground (for [x (range -12 -1) y [61 62 63 64] z (range -3 4)] (str x "," y "," z)))
              world (assoc trench "-1,61,1" "coal_ore" "0,61,0" "coal_ore")
              run (fn ^:async run1 [args] (await (tunnel! {:blocks world :self {:pos {:x -5 :y 61 :z 0}}}
                                                          (merge {:target [0 61 0]} args) tu/seeing-all)))
              on (await (run {}))
              off (await (run {:pick-up-ores false}))]
          (is (= :reached (:reason @(:out on))))
          (is (= "air" (block-at (:p on) [-1 61 1])) "the coal beside the run")
          (is (= [{:cell [-1 61 1] :block "coal_ore"}] (mapv #(select-keys % [:cell :block]) (:ores @(:out on)))))
          (is (= "coal_ore" (block-at (:p on) [0 61 0])) "the target is left to its caller")
          (is (= "coal_ore" (block-at (:p off) [-1 61 1])) "opted out"))))))

(deftest a-tunnel-with-only-a-spent-pickaxe-stops-tool-spent-unless-told-otherwise
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[uses args dug? reason] [[3 {:target [6 57 0]} false :tool-spent]
                                         [4 {:target [6 57 0]} true :reached]
                                         [3 {:target [6 57 0] :settings {:jobs.lib.trip/spent-uses 0}} true :reached]]]
          (let [{:keys [out p]} (await (tunnel! {:blocks eight-down :inventory [{:name "iron_pickaxe" :count 1 :durability uses}]} args (fn [_])))]
            (is (= dug? (boolean (seq (digs p)))) (pr-str uses args))
            (is (= reason (:reason @out)) (pr-str uses args))))))))

;; ---------------------------------------------------------------- the contract

(deftest the-job-exports-the-contract-and-quotes-validly
  (let [def (get registry/jobs job)
        c {:primitives (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :blocks ground :inventory pick}) :args {:target {:x 6 :y 57 :z 0}}}
        q ((:quote-of def) c)]
    (is (= [:target] (:target def)))
    (is (some? (:can-start? def)))
    (is (nil? (:check def)))
    (is (nil? (contract/quote-problem q)) (pr-str q))
    (is (= [6 57 0] (:moves-to q)))))

(deftest a-tunnel-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn ^:async run1 [spec args prep]
                    (let [e @(:out (await (tunnel! spec args prep :raw true)))]
                      [(:kind e) (contract/cause-key e)]))
              target {:target [6 57 0]}]
          (is (= [:done :done] (await (run {:blocks eight-down} target (fn [_])))))
          (is (= [:condition-failed :too-far] (await (run {:blocks ground} (assoc target :max-length 6) (fn [_])))))
          (is (= [:condition-failed :refused]
                 (await (run {:blocks eight-down :zones [{:name "z" :min [6 57 0] :max [6 57 0]}]} target (fn [_])))))
          (is (= [:condition-failed :no-zones] (await (run {:blocks eight-down :zones nil} target (fn [_])))))
          (is (= [:impossible :bad-args] (await (run {:blocks eight-down} {:heading :east} (fn [_])))) "a call with no target never starts")
          (is (= [:condition-failed :no-tool] (await (run {:blocks eight-down :inventory []} (assoc target :fetch false) (fn [_])))))
          (is (= [:condition-failed :hazard] (await (run {:blocks eight-down :inventory with-cobble} target lava-ahead))))
          (is (= [:condition-failed :tool-spent]
                 (await (run {:blocks eight-down :inventory [{:name "iron_pickaxe" :count 1 :durability 3}]} target (fn [_]))))))))))

(deftest a-stopped-tunnel-hands-over-what-it-did-as-data
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e @(:out (await (tunnel! {:blocks eight-down :inventory with-cobble} {:target [6 57 0]} lava-ahead :raw true)))]
          (is (= :hazard (:reason (:data e))))
          (is (true? (:out (:data e))))
          (is (seq (:dug (:data e))))
          (is (= [-3 65 0] (:entry (:data e)))))))))

(deftest the-surface-scan-stops-at-the-first-unseen-cell-under-a-seen-one
  ;; The air over the ground is seen, the ground and the rock under it are not: only the ground is left to look at.
  (let [read (atom [])
        block-at (fn [[_ y _ :as c]] (swap! read conj y) (if (< y 65) "stone" "air"))
        seen-at (fn [[_ y _]] (when (> y 65) "air"))]
    (is (nil? (ln/surface block-at seen-at [3 5] 40 80)))
    (is (= 65 (apply min @read)) (pr-str @read))))
