(ns engine.dig-to-see-test
  "Stair and tunnel read through perception (Q98): a cell behind stone is unknown and not a refusal; the job digs, looks
  at what the dig laid open, and stops or acts on what it sees. Exposed lava is sealed (:on-lava :seal, the default) or
  stops the job (:on-lava :stop); a seal that fails backs off. The primitives are wrapped; blockAt stays raw."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.stair-cells :as sc]
            [jobs.access.tunnel :as tunnel]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.util :as u]
            [jobs.lib.world-files :as world]))

(defn stone
  "Stone over x0..x1, y0..y1, z0..z1, as fake blocks."
  [x0 x1 y0 y1 z0 z1]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) "stone"])))

(def ground (stone -3 12 50 64 -2 2))
(def kit [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 8}])

(defn sensing
  "Fake primitives for spec wrapped by perception, as a body runs them."
  [spec]
  (let [p (tu/fake (merge {:self {:pos {:x 0 :y 65 :z 0}} :inventory kit} spec))
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})]
    (perception/wrap p per)))

(defn setup
  "An engine over wrapped primitives p; a recording parent runs job with args as its child, its result in :out."
  [p job args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        out (atom :not-done)
        parent (assoc (tu/recording-parent job args out) :can-start? #(ctx/child-status % :kid job args))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn ^:async tick-out!
  ([s] (tick-out! s 400))
  ([{:keys [eng clock] :as s} max-ticks]
  (loop [i 0]
    (when (and (< i max-ticks) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i))))
  s))

(defn ^:async run! [spec job args]
  (await (tick-out! (setup (sensing spec) job args))))

(defn calls [p kind] (mapv #(js->clj (.-args %) :keywordize-keys true)
                           (filter #(= kind (.-name %)) (.-calls (.-world p)))))
(defn digs [p] (mapv :pos (calls p "dig")))
(defn places [p] (mapv :pos (calls p "place")))
(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn feet [p] (let [pos (.-pos (.self p))] (mapv js/Math.floor [(.-x pos) (.-y pos) (.-z pos)])))
(defn events-of [{:keys [seen]} kind] (filter #(= kind (:kind %)) @seen))

(def stair-east {:dir :down :heading :east :steps 3})

(deftest a-cell-behind-stone-reads-the-callers-guess
  (let [p (sensing {:blocks (assoc ground "3,63,0" "lava") :unloaded #{"40,64,0"}})
        guessed (access/sensed-at p "stone")]
    (is (= "stone" (guessed [0 64 0])) "felt: under the feet")
    (is (= "stone" (guessed [3 63 0])) "lava behind stone is not known")
    (is (nil? ((access/sensed-at p nil) [3 63 0])))
    (is (true? (.-unknown (u/sensed p {:x 3 :y 63 :z 0}))))
    (is (nil? (guessed [40 64 0])) "unloaded is nil whatever the guess")))

(deftest stair-lava-behind-stone-is-dug-to-and-sealed-then-the-stair-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (run! {:blocks (assoc ground "3,63,0" "lava")} 'jobs.access.stair stair-east))]
          (is (= :done (:status @out)))
          (is (= 3 (:steps @out)))
          (is (= [3 62 0] (feet p)))
          (is (some #{{:x 3 :y 63 :z 0}} (places p)) "the lava cell is filled")
          (is (not= "lava" (block-at p [3 63 0])))
          (is (= 1 (count (events-of s :stair.sealed)))))))))

(deftest stair-on-lava-stop-stops-where-the-lava-shows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run! {:blocks (assoc ground "3,63,0" "lava")} 'jobs.access.stair
                                           (assoc stair-east :on-lava :stop)))]
          (is (= :stopped (:status @out)))
          (is (= :lava-exposed (:reason @out)))
          (is (= [3 63 0] (:cell @out)))
          (is (= "lava" (block-at p [3 63 0])))
          (is (empty? (places p))))))))

(deftest stair-seal-that-fails-backs-off-a-step-and-digs-no-more
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run! {:blocks (assoc ground "3,63,0" "lava") :inventory [{:name "iron_pickaxe" :count 1}]}
                                           'jobs.access.stair (assoc stair-east :fetch false)))]
          (is (= :stopped (:status @out)))
          (is (= :lava-unsealed (:reason @out)))
          (is (= [3 63 0] (:cell @out)))
          (is (<= (first (feet p)) 1) "backed up the stair, not beside the lava")
          (is (= "lava" (block-at p [3 63 0])))
          (is (not-any? #{{:x 3 :y 63 :z 0} {:x 3 :y 62 :z 0}} (digs p)) "nothing dug into the lava"))))))

(deftest stair-water-behind-stone-shows-after-the-dig-above-and-the-opened-cell-is-plugged
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run! {:blocks (assoc ground "2,63,0" "water")} 'jobs.access.stair stair-east))]
          (is (= :hazard (:reason @out)))
          (is (= [2 64 0] (:plugged @out)))
          (is (some #{{:x 2 :y 64 :z 0}} (digs p)) "dug to see what was under")
          (is (not= "air" (block-at p [2 64 0])) "the dug cell is shut again")
          (is (= "water" (block-at p [2 63 0]))))))))

(def trench
  "Ground with an open trench west of x -1 at y 61..64: the body stands in it at the tunnel's entry."
  (apply dissoc (stone -12 12 50 64 -3 3) (for [x (range -12 -1) y [61 62 63 64] z (range -3 4)] (str x "," y "," z))))

(deftest tunnel-lava-behind-stone-over-the-target-is-sealed-and-the-stand-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p] :as s} (await (run! {:blocks (assoc trench "0,62,0" "lava") :self {:pos {:x -2 :y 61 :z 0}}}
                                                 'jobs.access.tunnel {:target [0 61 0]}))]
          (is (= :reached (:reason @out)))
          (is (= [-1 61 0] (feet p)))
          (is (= :east (:heading @out)))
          (is (some #{{:x 0 :y 62 :z 0}} (places p)) "the lava cell is filled, then dug as the cell over the target")
          (is (= "air" (block-at p [0 62 0])))
          (is (= 1 (count (events-of s :tunnel.sealed)))))))))

;; ---- lava beside or above a cut flows in after one flow delay (30 ticks); the fake has no flow, so the tests add it

(def flow-ms "One overworld lava flow delay: 30 ticks." 1500)

(defn flow!
  "One flow pass at clock ms: an open cell beside a source (a cell of sources still holding lava) that has been so for
  flow-ms becomes lava. since: atom {cell ms}. Sets in-lava when lava lands on the body's feet or head cell."
  [p sources since ms in-lava]
  (let [blocks #(:blocks @(.-state (.-world p)))
        lava? (fn [cell] (and (contains? sources cell) (= "lava" (get (blocks) cell))))
        beside (distinct (for [s sources :when (lava? s) d [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
                               :let [cell (mapv + s d)] :when (contains? #{nil "air"} (get (blocks) cell))]
                           cell))]
    (doseq [cell beside]
      (swap! since update cell #(or % ms))
      (when (>= (- ms (get @since cell)) flow-ms)
        (swap! (.-state (.-world p)) assoc-in [:blocks cell] "lava")
        (swap! since dissoc cell)))
    (doseq [cell (keys @since) :when (not (some #{cell} beside))] (swap! since dissoc cell))
    (let [[fx fy fz] (feet p)]
      (when (some #(= "lava" (get (blocks) %)) [[fx fy fz] [fx (inc fy) fz]])
        (reset! in-lava true)))))

(defn ^:async flow-run!
  "run! with sources: ticks the job out, flowing lava from the source cells beside open air between ticks. The sources
  are never seen (a face point of one beside the cut would show it to a real eye), only the lava that flows out."
  [spec sources job args]
  (let [{:keys [eng clock p] :as s} (setup (sensing spec) job args)
        sensed (.-sensedAt p)
        since (atom {})
        in-lava (atom false)]
    (aset p "sensedAt" (fn [pos] (if (contains? sources [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (sensed pos))))
    (loop [i 0]
      (when (and (< i 400) (seq (:list (core/state eng))))
        (swap! clock + 500)
        (await (core/tick! eng))
        (flow! p sources since @clock in-lava)
        (recur (inc i))))
    (assoc s :in-lava in-lava)))

(def lava-side-sources #{[3 63 1]})

(deftest stair-waits-a-flow-delay-before-it-steps-beside-unseen-lava-and-seals-what-flows-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p in-lava] :as s} (await (flow-run! {:blocks (assoc ground "3,63,1" "lava")} lava-side-sources
                                                              'jobs.access.stair stair-east))]
          (is (false? @in-lava) "the body never stood in flowing lava")
          (is (some #{{:x 3 :y 63 :z 0}} (places p)) "the cut cell lava flowed into is sealed")
          (is (pos? (count (events-of s :stair.sealed)))))))))

(deftest stair-lava-beside-the-line-that-keeps-flowing-in-ends-the-stair-backed-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p in-lava]} (await (flow-run! {:blocks (assoc ground "3,63,1" "lava")} lava-side-sources
                                                        'jobs.access.stair stair-east))
              seals (filterv #{{:x 3 :y 63 :z 0}} (places p))]
          (is (false? @in-lava))
          (is (= :stopped (:status @out)))
          (is (= :lava-unsealed (:reason @out)))
          (is (= 2 (count seals)) "one seal and one reseal, then no more")
          (is (true? (:backed-off @out))))))))

(deftest the-flow-wait-is-30-ticks-at-the-live-game-rate
  (let [at (fn [rate] (settings/set-clock! rate false :assumed) (dig-look/flow-delay-ms))
        slow (at 10)
        fast (at 20)]
    (settings/set-clock! 20 false :assumed)
    (is (>= slow 3000) "30 ticks at 10 TPS take 3 s")
    (is (< fast slow))))

(deftest tunnel-waits-and-seals-lava-that-flows-into-the-cell-over-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out in-lava] :as s} (await (flow-run! {:blocks (assoc trench "0,62,1" "lava") :self {:pos {:x -2 :y 61 :z 0}}}
                                                            #{[0 62 1]} 'jobs.access.tunnel {:target [0 61 0]}))]
          (is (false? @in-lava) "the body never stood in flowing lava")
          (is (pos? (count (events-of s :tunnel.sealed)))))))))

(deftest stair-lava-above-a-cut-is-waited-for-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [tall (merge ground (stone 1 12 65 68 -2 2) {"3,65,0" "lava"})
              {:keys [out in-lava]} (await (flow-run! {:blocks tall} #{[3 65 0]} 'jobs.access.stair
                                                      stair-east))]
          (is (false? @in-lava) "the body never stood in flowing lava")
          (is (contains? #{:done :stopped} (:status @out))))))))

(deftest stop-of-does-not-trust-a-floor-the-body-has-not-seen
  (let [cut [[1 65 0] [1 64 0]]
        in {:unseen? #{[1 63 0]} :ignore-zones? true :block-at #(if (some #{%} cut) "air" "stone")}
        cells {:cut cut :floor [1 63 0] :under [1 62 0]}]
    (is (= {:reason :floor-unseen :cell [1 63 0]} (sc/stop-of in cells #{})))
    (is (nil? (sc/stop-of (assoc in :unseen? #{}) cells #{})) "a seen floor")
    (is (not= :floor-unseen (:reason (sc/stop-of (assoc in :block-at (constantly "stone")) cells #{})))
        "judged once the cut is open: it is dug to see first")))

(deftest stop-of-names-the-plug-when-water-also-fills-a-cut-cell
  (let [cut [[1 65 0] [1 64 0]]
        blocks {[1 65 0] "cobblestone" [1 64 0] "water"}
        in {:ignore-zones? true :plugs #{[1 65 0]} :block-at #(get blocks % "stone")}
        stop (sc/stop-of in {:cut cut :floor [1 63 0] :under [1 62 0]} #{})]
    (is (= [:plugged [1 65 0]] [(:reason stop) (:cell stop)]))))

(defn floor-seen-after-cut!
  "p whose cell floor reads unknown until the second look at it: the first glance missed."
  [p floor]
  (let [seen (atom false)
        looks (atom 0)
        look (.-look p)
        sensed-at (.-sensedAt p)]
    (aset p "look" (fn [token args]
                     (let [pos (.-pos args)]
                       (when (and (= floor [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))])
                                  (= 2 (swap! looks inc)))
                         (reset! seen true))
                       (.call look p token args))))
    (aset p "sensedAt" (fn [pos]
                         (if (and (not @seen) (= floor [(.-x pos) (.-y pos) (.-z pos)]))
                           #js {:unknown true :pos pos}
                           (.call sensed-at p pos))))
    p))

(deftest stair-looks-again-at-a-floor-its-first-look-missed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [open (apply dissoc ground (for [y [64 65 66]] (str "1," y ",0")))
              p (floor-seen-after-cut! (sensing {:blocks open}) [1 63 0])
              {:keys [out]} (await (tick-out! (setup p 'jobs.access.stair stair-east)))]
          (is (= :done (:status @out)) (pr-str @out))
          (is (= 3 (:steps @out))))))))

(defn floor-seen-after-lean!
  "p whose cell floor reads unknown until a look after the body stepped (steer) toward it: from the back of its cell the
  edge of its own floor hides the floor below ahead."
  [p floor]
  (let [leaned (atom false)
        seen (atom false)
        steer (.-steer p)
        look (.-look p)
        sensed-at (.-sensedAt p)]
    (aset p "steer" (fn [token args] (reset! leaned true) (.call steer p token args)))
    (aset p "look" (fn [token args]
                     (let [pos (.-pos args)]
                       (when (and @leaned (= floor [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]))
                         (reset! seen true))
                       (.call look p token args))))
    (aset p "sensedAt" (fn [pos]
                         (if (and (not @seen) (= floor [(.-x pos) (.-y pos) (.-z pos)]))
                           #js {:unknown true :pos pos}
                           (.call sensed-at p pos))))
    p))

;; Sorrel j3047: standing against the riser at the back of its stair cell, the body looked at the next floor twice and
;; stopped :floor-unseen; the line from its eye to that floor crossed the edge of its own floor.
(deftest stair-leans-toward-a-floor-its-looks-missed-and-looks-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [open (apply dissoc ground (for [y [64 65 66]] (str "1," y ",0")))
              p (floor-seen-after-lean! (sensing {:blocks open}) [1 63 0])
              {:keys [out]} (await (tick-out! (setup p 'jobs.access.stair stair-east)))]
          (is (= :done (:status @out)) (pr-str @out))
          (is (= 3 (:steps @out))))))))

(deftest tunnel-passes-its-fetch-to-the-stair-child-for-items-only
  (is (= {:what #{:item}} (tunnel/stair-fetch true)))
  (is (= {:what #{:item}} (tunnel/stair-fetch #{:tool :item})))
  (is (= {:what #{:item} :minutes 2} (tunnel/stair-fetch {:minutes 2})))
  (is (nil? (tunnel/stair-fetch false)))
  (is (nil? (tunnel/stair-fetch #{:tool})) "a tool is the tunnel's own fetch")
  (is (nil? (tunnel/stair-fetch {:what #{:tool}}))))

(deftest tunnel-seal-that-fails-in-its-own-run-leaves-the-stand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run! {:blocks (assoc trench "0,62,0" "lava") :self {:pos {:x -2 :y 61 :z 0}}
                                            :inventory [{:name "iron_pickaxe" :count 1}]}
                                           'jobs.access.tunnel {:target [0 61 0] :fetch false}))]
          (is (= :lava-unsealed (:reason @out)))
          (is (not= [-1 61 0] (feet p)) "stepped back from the stand beside the lava")
          (is (= "lava" (block-at p [0 62 0]))))))))

(def dirt-room
  "A sealed dirt room, inside x -2..2, z -2..2, feet y 64, walls one thick, on a stone floor."
  (merge (stone -6 12 63 63 -6 6)
         (apply dissoc (into {} (for [x (range -3 4) y (range 64 67) z (range -3 4)] [(str x "," y "," z) "dirt"]))
                (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))))

(deftest dig-of-an-unseen-dirt-cell-looks-first-and-needs-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run! {:self {:pos {:x 0 :y 64 :z 0}} :blocks dirt-room :inventory []}
                                           'jobs.blocks.dig {:pos [3 64 0] :fetch false}))]
          (is (= :dug (:reason @out)))
          (is (= "dirt" (:block @out)))
          (is (some #{{:x 3 :y 64 :z 0}} (digs p))))))))

(def snow-mass (update-vals (stone -3 12 50 64 -2 2) (constantly "snow_block")))

(deftest stair-into-unseen-snow-is-dug-by-hand-without-fetching-a-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run! {:blocks snow-mass :inventory []} 'jobs.access.stair stair-east))]
          (is (= :done (:status @(:out s))))
          (is (empty? (events-of s :fetch.started)))
          (is (seq (digs (:p s)))))))))

(def dirt-ground (into {} (for [[k v] ground] [k "dirt"])))

(deftest stair-through-unseen-dirt-needs-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run! {:blocks dirt-ground :inventory []}
                                           'jobs.access.stair (assoc stair-east :fetch false)))]
          (is (not= :no-tool (:reason @out)) (pr-str @out))
          (is (= :done (:status @out)) (pr-str @out))
          (is (= [3 62 0] (feet p))))))))

(deftest tunnel-down-through-unseen-dirt-needs-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (run! {:blocks (into {} (for [[k _] trench] [k "dirt"]))
                                            :inventory [] :self {:pos {:x -2 :y 61 :z 0}}}
                                           'jobs.access.tunnel {:target [6 57 0] :fetch false}))]
          (is (not= :no-tool (:reason @out)) (pr-str @out))
          (is (= :reached (:reason @out)) (pr-str @out))
          (is (<= (second (feet p)) 58)))))))

(deftest stair-dirt-that-a-look-does-not-show-is-not-judged-as-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (sensing {:blocks dirt-ground :inventory []})
              _ (set! (.-look p) (fn [_] (js/Promise.resolve #js {:status "ok"})))
              {:keys [out seen]} (await (tick-out! (setup p 'jobs.access.stair (assoc stair-east :fetch false)) 30))]
          (is (not= :no-tool (:reason @out)) (pr-str @out))
          (is (some #(= :unseen (:reason %)) (filter #(= :waiting (:kind %)) @seen))
              "the stair holds, waiting for cells it has not seen"))))))
