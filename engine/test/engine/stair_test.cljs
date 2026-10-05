(ns engine.stair-test
  "jobs.access.stair: step geometry, the stop rules, and whole stairs against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.job-api :as job-api]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as world]
            [jobs.access.stair :as stair]))

(def job 'jobs.access.stair)

;; ---------------------------------------------------------------- pure

(deftest step-cells-per-direction-and-heading
  (are [dir heading next cut]
       (= {:next next :cut cut :floor (update next 1 dec) :under (update next 1 - 2)}
          (stair/step-cells [0 65 0] dir heading))
    :down :east [1 64 0] [[1 66 0] [1 65 0] [1 64 0]]
    :down :west [-1 64 0] [[-1 66 0] [-1 65 0] [-1 64 0]]
    :down :south [0 64 1] [[0 66 1] [0 65 1] [0 64 1]]
    :down :north [0 64 -1] [[0 66 -1] [0 65 -1] [0 64 -1]]
    :up :east [1 66 0] [[0 67 0] [1 67 0] [1 66 0]]
    :up :west [-1 66 0] [[0 67 0] [-1 67 0] [-1 66 0]]
    :up :south [0 66 1] [[0 67 0] [0 67 1] [0 66 1]]
    :up :north [0 66 -1] [[0 67 0] [0 67 -1] [0 66 -1]]))

(deftest stair-index-finds-the-step-on-the-line-only
  (are [feet dir heading i] (= i (stair/stair-index [0 65 0] feet dir heading 8))
    [0 65 0] :down :east 0
    [3 62 0] :down :east 3
    [0 62 -3] :down :north 3
    [2 67 0] :up :east 2
    [3 63 0] :down :east nil
    [3 62 1] :down :east nil
    [-1 66 0] :down :east nil
    [9 56 0] :down :east nil))

(defn world-fn
  "A block-at over a map {[x y z] name}: stone everywhere below y 65 unless named, air above, nil for :unloaded."
  [named]
  (fn [[_ y _ :as cell]]
    (let [n (get named cell (if (< y 65) "stone" "air"))]
      (when-not (= :unloaded n) n))))

(defn stop [named accept & {:keys [zones dir] :or {zones [] dir :down}}]
  (let [in {:block-at (world-fn named) :feet [0 65 0] :zones zones :footprints #{} :ledger #{}}]
    (stair/stop-of in (stair/step-cells [0 65 0] dir :east) accept)))

(deftest the-stop-rules-of-one-step
  (are [named accept zones reason] (= reason (:reason (stop named accept :zones zones)))
    {} #{:water} [] nil
    {[2 64 0] "lava"} #{:water} [] :hazard
    {[2 64 0] "lava"} #{:water :lava} [] nil
    {[1 64 1] "water"} #{:water} [] nil
    {[1 64 1] "water"} #{} [] :hazard
    {[1 64 1] "water" [1 64 -1] "lava"} #{:water} [] :hazard
    {[1 63 0] "air"} #{:water} [] :no-floor
    {[1 63 0] "lava"} #{:water} [] :no-floor
    {[1 62 0] "cave_air"} #{:water} [] :cave-below
    {[1 62 0] "water"} #{:water} [] :cave-below
    {[1 62 0] :unloaded} #{:water} [] :not-loaded
    {[1 65 0] "water"} #{:water} [] :fluid-in-cut
    {} #{:water} nil :no-zones
    {} #{:water} [{:name "keep" :min [1 60 -1] :max [3 70 1]}] :zone
    {} #{:water} [{:name "dig ok" :min [1 60 -1] :max [3 70 1] :allow #{:dig}}] nil
    {[1 67 0] "gravel" [1 66 0] "stone"} #{:water} [] :hazard
    {[1 67 0] "gravel" [1 66 0] "stone"} #{:falling-block} [] nil))

(deftest a-crop-or-farmland-in-the-cut-is-refused-and-never-dug
  (are [named cell block] (= {:reason :crop :cell cell :block block} (stop named #{}))
    {[1 66 0] "wheat"} [1 66 0] "wheat"
    {[1 65 0] "carrots"} [1 65 0] "carrots"
    {[1 64 0] "farmland"} [1 64 0] "farmland"
    {[1 65 0] "melon_stem"} [1 65 0] "melon_stem"
    {[1 65 0] "attached_pumpkin_stem"} [1 65 0] "attached_pumpkin_stem"))

(deftest a-stop-names-the-cell-and-the-hazards
  (let [s (stop {[2 64 0] "lava"} #{:water})]
    (is (= [1 64 0] (:cell s)))
    (is (= [{:reason :fluid-adjacent :fluid "lava" :at [2 64 0]}] (:hazards s))))
  (is (= {:reason :zone :zone "keep" :cell [1 64 0]}
         (stop {} #{:water} :zones [{:name "keep" :min [1 60 0] :max [1 64 0]}]))))

(deftest gravel-over-the-head-going-up-is-a-hazard
  (let [named {[0 67 0] "stone" [0 68 0] "gravel" [1 65 0] "stone"}]
    (is (= :hazard (:reason (stop named #{:water} :dir :up))))
    (is (nil? (stop named #{:falling-block} :dir :up)))))

(deftest target-steps-from-args
  (are [args steps] (= steps (stair/target-steps args [0 65 0]))
    {:dir :down :heading :east :steps 4} 4
    {:dir :down :heading :east :y 57} 8
    {:dir :up :heading :east :y 71} 6)
  (are [args] (:error (stair/target-steps args [0 65 0]))
    {:dir :down :heading :east}
    {:dir :down :heading :east :y 70}
    {:dir :sideways :heading :east :steps 2}
    {:dir :down :heading :up :steps 2}
    {:dir :down :heading :east :steps 0}))

;; ---------------------------------------------------------------- the fake world

(defn stone
  "Stone over x0..x1, y0..y1, z -2..2, as fake blocks."
  [x0 x1 y0 y1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range -2 3)] [(str x "," y "," z) "stone"])))

(def ground (stone -3 12 50 64))
(def pick [{:name "iron_pickaxe" :count 1}])
(def east {:dir :down :heading :east :steps 3})

(defn setup
  "An engine over the fake world and world data (zones, default none; nil: never read; plans); a recording parent runs
  the job as its child and keeps its result in :out."
  [spec args prep]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :inventory pick} (dissoc spec :zones :plans)))
        out (atom :not-done)
        w (world/of-data (:plans spec {}) {} (get spec :zones []))
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world w
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (prep p)
    (let [id (core/submit! eng '(recording-parent) {})]
      {:eng eng :p p :clock clock :seen seen :out out :id id})))

(defn waiting
  "The reason the parent of the stair child waits with (job.waiting), or nil."
  [{:keys [eng id]}]
  (:waiting (job-api/summary eng id)))

(defn ^:async tick-out! [{:keys [eng clock] :as s}]
  (loop [i 0]
    (when (and (< i 300) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async stair! [spec args prep]
  (await (tick-out! (setup spec args prep))))

(defn digs [p] (mapv #(js->clj (.-pos (.-args %)) :keywordize-keys true)
                     (filter #(= "dig" (.-name %)) (.-calls (.-world p)))))
(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))

(deftest a-stair-queued-in-a-farm-stops-before-digging-the-crop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks (assoc ground "1,64,0" "wheat")} east (fn [_])))]
          (is (= :crop (:reason @out)))
          (is (= [1 64 0] (:cell @out)))
          (is (= "wheat" (:block @out)))
          (is (= 0 (:steps @out)))
          (is (empty? (digs p))))))))

(deftest off-stair-says-where-the-body-is-against-the-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:self :pos] [7 65 4])
                                      r)))))
              {:keys [out]} (await (stair! {:blocks ground} east prep))]
          (is (= :off-stair (:reason @out)))
          (is (= [0 65 0] (:origin @out)))
          (is (= [7 0 4] (:offset @out)))
          (is (= 0 (:steps @out))))))))

(deftest three-steps-down-east-in-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stair! {:blocks ground} east (fn [_])))]
          (is (= :done (:status @out)))
          (is (= 3 (:steps @out)))
          (is (= [3 62 0] (:at @out)))
          (is (= [{:cell [1 64 0] :block "stone"}
                  {:cell [2 64 0] :block "stone"} {:cell [2 63 0] :block "stone"}
                  {:cell [3 64 0] :block "stone"} {:cell [3 63 0] :block "stone"} {:cell [3 62 0] :block "stone"}]
                 (:dug @out)))
          (is (= "stone" (block-at p [3 61 0])) "the floor is never dug")
          (is (= 3 (count (events-of s :stair.step))))
          (is (= 1 (count (events-of s :stair.done)))))))))

(deftest down-by-target-y-north
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (stair! {:blocks ground} {:dir :down :heading :north :y 63} (fn [_])))]
          (is (= :done (:status @out)))
          (is (= [0 63 -2] (:at @out))))))))

(deftest up-out-of-a-one-wide-shaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shaft (apply dissoc ground (for [y (range 59 65)] (str "0," y ",0")))
              {:keys [out]} (await (stair! {:blocks shaft :self {:pos {:x 0 :y 59 :z 0}}}
                                           {:dir :up :heading :east :steps 6} (fn [_])))]
          (is (= :done (:status @out)))
          (is (= [6 65 0] (:at @out)) "on the surface")
          (is (every? #(<= 59 (get-in % [:cell 1])) (:dug @out)) "never digs below the start"))))))

(deftest a-pickaxe-that-breaks-mid-stair-says-tool-broke-and-tool-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc :inventory [])
                                      r)))))
              s (await (stair! {:blocks ground :inventory [{:name "stone_pickaxe" :count 1 :durability 1}]}
                               east prep))]
          (is (= 1 (count (events-of s :tool.broke))))
          (is (= 1 (count (events-of s :tool.none)))))))))

(deftest lava-two-ahead-stops-before-opening-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stair! {:blocks (assoc ground "3,63,0" "lava")}
                                                   (assoc east :steps 5) (fn [_])))]
          (is (= :stopped (:status @out)))
          (is (= :hazard (:reason @out)))
          (is (= 1 (:steps @out)))
          (is (= "lava" (block-at p [3 63 0])))
          (is (not-any? #(= {:x 2 :y 63 :z 0} %) (digs p)) "the cell beside the lava is not dug")
          (is (= "lava" (:fluid (first (:hazards @out)))))
          (is (= 1 (count (events-of s :stair.stopped)))))))))

(deftest water-beside-stops-by-default-and-is-taken-only-when-named
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wet (assoc ground "2,63,1" "water")
              refused (await (stair! {:blocks wet} east (fn [_])))
              taken (:out (await (stair! {:blocks wet} (assoc east :accept #{:water}) (fn [_]))))]
          (is (= :hazard (:reason @(:out refused))))
          (is (= [2 63 0] (:cell @(:out refused))))
          (is (not-any? #(= {:x 2 :y 63 :z 0} %) (digs (:p refused))) "the cell beside the water is not opened")
          (is (= :done (:status @taken))))))))

(deftest water-let-into-the-cut-stops-the-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))
                                          {:keys [x y z]} (js->clj (.-pos a) :keywordize-keys true)]
                                      (swap! (.. world -state) assoc-in [:blocks [x y z]] "water")
                                      r)))))
              {:keys [out]} (await (stair! {:blocks ground} (assoc east :accept #{:water}) prep))]
          (is (= :fluid-in-cut (:reason @out)))
          (is (= [1 64 0] (:cell @out))))))))

(deftest air-under-the-next-floor-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks (dissoc ground "2,61,0")} east (fn [_])))]
          (is (= :cave-below (:reason @out)))
          (is (= [2 61 0] (:cell @out)))
          (is (= 1 (:steps @out)))
          (is (= "stone" (block-at p [2 64 0])) "nothing of the step is cut"))))))

(def cave-gap
  "Open air at x 1, y 62..65 over the stone ground: the floor of the first step up is missing, and so is what is under it."
  (apply dissoc ground (for [y (range 62 65) z (range -2 3)] (str "1," y "," z))))

(def up-east {:dir :up :heading :east :steps 1})

(deftest a-gap-in-the-next-floor-is-bridged-with-carried-filler
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              {:keys [out p]} (await (stair! {:blocks cave-gap :inventory inv} up-east (fn [_])))]
          (is (= :done (:status @out)))
          (is (= [1 66 0] (:at @out)))
          (is (= "cobblestone" (block-at p [1 65 0])) "the floor was placed")
          (is (= 1 (count (filter #(= "place" (.-name %)) (.-calls (.-world p)))))))))))

(deftest a-gap-in-the-next-floor-without-filler-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "diamond" :count 5} {:name "gold_block" :count 5})
              {:keys [out p]} (await (stair! {:blocks cave-gap :inventory inv} up-east (fn [_])))]
          (is (= :no-floor (:reason @out)))
          (is (= :none (:filler @out)))
          (is (re-find #"filler" (:why @out)))
          (is (empty? (filter #(= "place" (.-name %)) (.-calls (.-world p))))))))))

(deftest a-top-level-stair-with-no-progress-ends-stopped-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/capture-sink)
              p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :inventory pick :blocks cave-gap})
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                                :now #(deref clock) :world (world/of-data {} {} [])
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
          (core/submit! eng (list job up-east) {})
          (await (tick-out! {:eng eng :clock clock}))
          (let [ended (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %))) @seen)
                e (first ended)]
            (is (= 1 (count ended)))
            (is (= :stopped (:kind e)))
            (is (= :stopped (get-in e [:data :status])))
            (is (= :no-floor (get-in e [:data :reason])))
            (is (= [1 65 0] (get-in e [:data :cell])))
            (is (= 0 (get-in e [:data :steps])))))))))

(deftest bedrock-in-the-cut-is-unbreakable-not-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [inventory [pick []]]
          (let [{:keys [out p]} (await (stair! {:blocks (assoc ground "1,64,0" "bedrock") :inventory inventory} east (fn [_])))]
            (is (= :unbreakable (:reason @out)))
            (is (= [1 64 0] (:cell @out)))
            (is (= "bedrock" (:block @out)))
            (is (empty? (digs p)))))))))

(deftest lava-for-a-floor-is-never-bridged
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (conj pick {:name "cobblestone" :count 10})
              {:keys [out p]} (await (stair! {:blocks (assoc cave-gap "1,65,0" "lava") :inventory inv} up-east (fn [_])))]
          (is (= :no-floor (:reason @out)))
          (is (empty? (filter #(= "place" (.-name %)) (.-calls (.-world p))))))))))

(deftest a-zone-ahead-stops-at-its-edge
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone {:name "garden" :min [3 50 -2] :max [6 70 2] :allow #{:place}}
              {:keys [out p]} (await (stair! {:blocks ground :zones [zone]} (assoc east :steps 5) (fn [_])))]
          (is (= :zone (:reason @out)))
          (is (= "garden" (:zone @out)))
          (is (= 2 (:steps @out)))
          (is (not-any? #(<= 3 (:x %)) (digs p))))))))

(deftest no-zone-list-stops-before-any-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (stair! {:blocks ground :zones nil} east (fn [_])))]
          (is (= :no-zones (:reason @out)))
          (is (= 0 (:steps @out)))
          (is (empty? (digs p))))))))

(deftest a-full-inventory-stops-before-a-lost-drop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [full (into pick (repeat 35 {:name "dirt" :count 64}))
              {:keys [out p] :as s} (await (stair! {:blocks ground :inventory full} east (fn [_])))]
          (is (= :not-done @out) "the child declines: its parent waits")
          (is (= :no-free-slot (:reason (waiting s))))
          (is (empty? (digs p))))))))

(deftest a-stack-with-room-is-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [full (into pick (cons {:name "cobblestone" :count 10} (repeat 34 {:name "dirt" :count 64})))
              {:keys [out]} (await (stair! {:blocks ground :inventory full} east (fn [_])))]
          (is (= :done (:status @out))))))))

(deftest no-pickaxe-for-stone-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (stair! {:blocks ground :inventory []} east (fn [_])))]
          (is (= :not-done @out) "the child declines: its parent waits")
          (is (= :no-tool (:reason (waiting s))))
          (is (= "pickaxe" (:tool (waiting s))))
          (is (empty? (digs p))))))))

(deftest bad-args-end-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (stair! {:blocks ground} {:dir :down} (fn [_])))]
          (is (= :bad-args (:reason @out)))
          (is (string? (:why @out))))))))

(deftest a-cut-mid-dig-resumes-to-the-same-stair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks ground} east (fn [_]))
              world (.-world p)]
          (await (core/tick! eng))
          (.hold world "dig")
          (let [running (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (takeover/take! eng {:who "claude" :why "cut"})
            (await running))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (tick-out! s))
          (is (= :done (:status @out)))
          (is (= [3 62 0] (:at @out)))
          (is (= 6 (count (:dug @out)))))))))

(deftest a-cut-after-a-dig-landed-still-records-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out] :as s} (setup {:blocks ground} east (fn [_]))
              world (.-world p)]
          (.override world "dig" (fn ^:async f [token a impl]
                                   (let [r (await (impl token a))]
                                     (.override world "dig" nil)
                                     (takeover/take! eng {:who "claude" :why "cut"})
                                     r)))
          (dotimes [_ 3] (await (core/tick! eng)))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (tick-out! s))
          (is (= :done (:status @out)))
          (is (= 6 (count (:dug @out))))
          (is (= 6 (count (digs p)))))))))

(deftest no-way-back-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "steer"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [0 66 0]] "stone")
                                      r)))))
              {:keys [out p]} (await (stair! {:blocks ground} east prep))]
          (is (= :no-way-back (:reason @out)))
          (is (= 1 (:steps @out)))
          (is (= [1 64 0] (:at @out)))
          (is (= 1 (count (digs p))) "nothing more is cut"))))))

(deftest a-step-that-does-not-land-in-the-cut-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "steer"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:self :pos] [1 64 1])
                                      r)))))
              {:keys [out]} (await (stair! {:blocks (dissoc ground "1,64,1")} east prep))]
          (is (= :step-failed (:reason @out)))
          (is (= [1 64 0] (:cell @out))))))))

(deftest a-cell-is-judged-again-right-before-its-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "equip"
                                  (fn ^:async f [token a impl]
                                    (swap! (.. world -state) assoc-in [:blocks [1 64 1]] "lava")
                                    (await (impl token a))))))
              {:keys [out p]} (await (stair! {:blocks ground} east prep))]
          (is (= :hazard (:reason @out)))
          (is (= [1 64 0] (:cell @out)))
          (is (empty? (digs p))))))))

(deftest a-cell-that-keeps-refilling-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prep (fn [p]
                     (let [world (.-world p)]
                       (.override world "dig"
                                  (fn ^:async f [token a impl]
                                    (let [r (await (impl token a))]
                                      (swap! (.. world -state) assoc-in [:blocks [1 64 0]] "gravel")
                                      r)))))
              {:keys [out p]} (await (stair! {:blocks ground} east prep))]
          (is (= :refills (:reason @out)))
          (is (= 3 (count (digs p))))
          (is (= [{:cell [1 64 0] :block "stone"}] (:dug @out)) "a dig that left the same block is not recorded"))))))

(deftest another-plans-footprint-ahead-stops-and-names-the-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "wall" :parts [{:id "w" :cells [[2 63 0]] :want "stone"}]}
              {:keys [out p]} (await (stair! {:blocks ground :plans {"wall" plan}} east (fn [_])))]
          (is (= :footprint (:reason @out)))
          (is (= "wall" (:plan @out)))
          (is (= [2 63 0] (:cell @out)))
          (is (not-any? #(= {:x 2 :y 63 :z 0} %) (digs p))))))))

;; ---------------------------------------------------------------- way-back

(defn way-back-of
  "stair/way-back from (1 64 1) to origin over the block map."
  [blocks origin]
  (stair/way-back {:primitives (tu/fake {:blocks blocks :self {:pos {:x 1 :y 64 :z 1}}})} origin))

(defn slab
  "Stone at y for x in xs, z 0..2."
  [y xs]
  (into {} (for [x xs z (range 3)] [(str x "," y "," z) "stone"])))

(deftest way-back-judges-the-plan-like-walk-plan
  (let [level (merge (slab 63 (range 5)) (slab 63 (range 7 12)))]
    (are [blocks origin expected] (= expected (some-> (way-back-of blocks origin) (dissoc :step)))
      level [10 64 1] nil
      (merge (slab 63 (range 5)) (slab 64 (range 7 12))) [10 65 1]
      {:reason :no-way-back :why :refused :kind :gap-up}
      (merge level (slab 66 (range 4 7))) [10 64 1]
      {:reason :no-way-back :why :refused :kind :gap-low-ceiling})))

(deftest way-back-walks-round-a-gap-under-a-low-ceiling
  (let [at (fn [y xs zs] (into {} (for [x xs z zs] [(str x "," y "," z) "stone"])))
        blocks (merge (at 63 (range 5) (range -8 11)) (at 63 (range 7 13) (range -8 11)) (at 63 [5 6] [9 10])
                      (at 66 (range 4 7) (range -8 9)))]
    (is (nil? (way-back-of blocks [10 64 1])))))

(def around-the-cut {:name "plot" :min [1 60 -1] :max [3 70 1]})

(deftest a-stair-in-a-zone-follows-its-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra status] [["Fake" {} :done]
                                      ["fake" {} :done]
                                      ["Miles" {} :stopped]
                                      ["Miles" {:ignore-zones? true} :done]]]
          (let [{:keys [out]} (await (stair! {:blocks ground :zones [(assoc around-the-cut :owner owner)]}
                                             (merge east extra) (fn [_])))]
            (is (= status (:status @out)) (pr-str [owner extra]))))))))
