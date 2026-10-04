(ns engine.harvest-test
  "jobs.farm.harvest against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.jobs.util :as u]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as ew]
            [jobs.farm.harvest :as harvest]))

(def clock
  "One clock for every engine of a test run; each tick moves it on, so a backoff pause ends."
  (atom 1000000))

(defn start
  "An engine over primitives p (made from world when not given) on dir."
  [{:keys [world p dir shared]}]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (or p (tu/fake world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (or dir (tu/tmp-dir)) :now #(deref clock)
                          :world (or shared (ew/of-data {} {} []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async run-until
  "Tick while (done? eng) is false, at most n ticks."
  [eng done? n]
  (loop [i 0]
    (if (or (>= i n) (done? eng))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome
  "Run job with args as the child of a recording parent until the list is empty, at most n ticks; the child's result."
  [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn block-at [p x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-name)))
(defn age-at [p x y z] (some-> (.blockAt p #js {:x x :y y :z z}) (.-age)))
(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(def job 'jobs.farm.harvest)
(def origin {:x 0 :y 64 :z 0})

(defn cell-key [x y z] (str x "," y "," z))

(defn field
  "Farmland at y 63 and crop at y 64 over the xs by zs, every crop at age."
  [crop age xs zs]
  (into {} (mapcat (fn [[x z]] [[(cell-key x 63 z) "farmland"] [(cell-key x 64 z) crop]]) (for [x xs z zs] [x z]))))

(defn ages [age xs zs] (into {} (for [x xs z zs] [(cell-key x 64 z) age])))

(def wheat-drops {"wheat" ["wheat" "wheat_seeds"] "carrots" ["carrot" "carrot"]})

(def wheat-world
  {:blocks (field "wheat" 7 (range 2 5) (range 2 5))
   :ages (ages 7 (range 2 5) (range 2 5))
   :drops wheat-drops})

(defn plain-args [p] (merge {:radius 12 :crops nil :give-up 4 :reach 4.2 :replant true} p))

(deftest ripe-crops-picks-ripe-wanted-cells-near-the-centre
  (let [p (tu/fake {:blocks {"1,64,0" "wheat" "2,64,0" "wheat" "3,64,0" "carrots" "4,64,0" "carrots"
                             "5,64,0" "beetroots" "6,64,0" "beetroots" "7,64,0" "potatoes" "30,64,0" "wheat"}
                    :ages {"1,64,0" 7 "2,64,0" 6 "3,64,0" 7 "4,64,0" 6 "5,64,0" 3 "6,64,0" 2 "7,64,0" 7 "30,64,0" 7}})
        xs (fn [cells] (mapv :x cells))]
    (are [args center skipped expected] (= expected (xs (harvest/ripe-crops p (plain-args args) center skipped)))
      {} origin [] [1 3 5 7]
      {:crops ["carrots"]} origin [] [3]
      {:crops ["beetroots" "wheat"]} origin [] [1 5]
      {:radius 4} origin [] [1 3]
      {:radius 2} {:x 7 :y 64 :z 0} [] [5 7]
      {} {:x 30 :y 64 :z 0} [] [30]
      {} origin [{:x 3 :y 64 :z 0}] [1 5 7])))

(deftest a-ripe-field-is-cut-and-replanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:world wheat-world})
              result (await (child-outcome eng job {} 200))]
          (is (= {:cut 9 :replanted 9 :bare [] :lost [] :gave-up false} result))
          (is (every? #(and (= "wheat" (block-at p (first %) 64 (second %))) (= 0 (age-at p (first %) 64 (second %))))
                      (for [x (range 2 5) z (range 2 5)] [x z])))
          (is (= {"wheat" 9} (inv p)))
          (is (= 1 (count (events-of seen :harvest.done)))))))))

(deftest only-ripe-wanted-crops-are-cut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world {:blocks (merge (field "carrots" 7 [1 2] [2]) (field "wheat" 3 [3 4] [2]))
                                              :ages (merge (ages 7 [1 2] [2]) (ages 3 [3 4] [2]))
                                              :drops wheat-drops}})
              result (await (child-outcome eng job {} 200))
              dug (set (map #(let [pos (.-pos (.-args %))] [(.-x pos) (.-z pos)]) (calls p "dig")))]
          (is (= #{[1 2] [2 2]} dug))
          (is (= {:cut 2 :replanted 2 :bare [] :lost [] :gave-up false} result))
          (is (= [3 3] [(age-at p 3 64 2) (age-at p 4 64 2)]))
          (is (= [0 0] [(age-at p 1 64 2) (age-at p 2 64 2)])))))))

(deftest the-debt-is-written-before-each-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world wheat-world})
              seen (atom [])]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [pos (js->clj (.-pos args) :keywordize-keys true)
                             debts (:replant (core/job-memory eng "j1"))]
                         (swap! seen conj (boolean (some #(= pos (:pos %)) debts)))
                         (await (impl token args)))))
          (core/submit! eng (list job {}) {})
          (await (run-until-empty eng 200))
          (is (= 9 (count @seen)))
          (is (every? true? @seen)))))))

(deftest a-restart-mid-harvest-still-replants
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              a (start {:world wheat-world :dir dir})
              p (:p a)]
          (core/submit! (:eng a) (list job {}) {})
          (await (run-until (:eng a) #(pos? (:cut (core/job-memory % "j1") 0)) 20))
          (is (pos? (count (:replant (core/job-memory (:eng a) "j1")))))
          (let [b (start {:p p :dir dir})]
            (await (run-until-empty (:eng b) 200))
            (is (every? #(= "wheat" (block-at p (first %) 64 (second %)))
                        (for [x (range 2 5) z (range 2 5)] [x z])))
            (is (= {"wheat" 9} (inv p)))))))))

(deftest no-seed-leaves-the-cells-bare
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (start {:world {:blocks (field "carrots" 7 [1 2] [2 3])
                                                 :ages (ages 7 [1 2] [2 3])
                                                 :drops {"carrots" []}}})
              result (await (child-outcome eng job {} 100))
              warn (first (events-of seen :harvest.bare))]
          (is (= 4 (:cut result)))
          (is (= 0 (:replanted result)))
          (is (= #{{:x 1 :y 64 :z 2} {:x 2 :y 64 :z 2} {:x 1 :y 64 :z 3} {:x 2 :y 64 :z 3}} (set (:bare result))))
          (is (= (set (:bare result)) (set (map #(into {} %) (:cells warn)))))
          (is (= 1 (count (events-of seen :harvest.bare))) "one event, the event stream carries no severity")
          (is (= [] (:list (core/state eng)))))))))

(deftest unreachable-crops-are-given-up-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (for [x (range 8 13)] (cell-key x 64 0))
              {:keys [eng p seen]} (start {:world {:blocks (zipmap cells (repeat "wheat"))
                                                   :ages (zipmap cells (repeat 7))
                                                   :unreachable cells}})
              result (await (child-outcome eng job {:radius 14} 400))
              warn (first (events-of seen :harvest.gave-up))]
          (is (zero? (count (calls p "dig"))))
          (is (= 1 (count (events-of seen :harvest.gave-up))))
          (is (= 4 (:unreachable warn)))
          (is (= {:cut 0 :replanted 0 :bare [] :lost [] :gave-up true} result)))))))

(deftest farmland-gone-before-replanting-leaves-the-cell-bare
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world {:blocks (field "wheat" 7 [2] [2 3]) :ages (ages 7 [2] [2 3]) :drops wheat-drops}})]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))
                             pos (.-pos args)]
                         (when (= 3 (.-z pos))
                           (.set (.-blocks (.-state (.-world p))) (cell-key (.-x pos) 63 (.-z pos)) "dirt"))
                         r)))
          (let [result (await (child-outcome eng job {} 100))]
            (is (= {:cut 2 :replanted 1 :bare [{:x 2 :y 64 :z 3}] :lost [] :gave-up false} result))))))))

(deftest the-job-is-not-run-while-every-crop-is-unripe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world {:blocks (field "wheat" 3 [2 3] [2]) :ages (ages 3 [2 3] [2])}})]
          (core/submit! eng (list job {}) {})
          (is (nil? (core/tick! eng)))
          (is (zero? (count (.-calls (.-world p))))))))))

(def one-cell
  {:blocks (field "wheat" 7 [3] [0]) :ages (ages 7 [3] [0]) :drops wheat-drops})

(defn place-calls-at [p x] (filterv #(= x (.-x (.-pos (.-args %)))) (calls p "place")))

(deftest a-refused-place-is-retried-from-close-by
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world one-cell})
              refused (atom 0)
              dists (atom [])]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (swap! dists conj (u/dist (u/pos-of (.-pos (.self p))) (u/pos-of (.-pos args))))
                       (if (zero? @refused)
                         (do (swap! refused inc)
                             #js {:status "failed" :reason "the block is still air"})
                         (await (impl token args)))))
          (let [result (await (child-outcome eng job {} 100))]
            (is (= {:cut 1 :replanted 1 :bare [] :lost [] :gave-up false} result))
            (is (= 2 (count (place-calls-at p 3))))
            (is (<= (second @dists) 2) "the retry is from within 2 blocks")
            (is (= "wheat" (block-at p 3 64 0)))))))))

(deftest a-place-that-always-fails-leaves-the-cell-bare-after-three-tries
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world one-cell})]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "failed"}))
          (let [result (await (child-outcome eng job {} 100))]
            (is (= {:cut 1 :replanted 0 :bare [{:x 3 :y 64 :z 0}] :lost [] :gave-up false} result))
            (is (= 3 (count (place-calls-at p 3))))
            (is (= [] (:list (core/state eng))))))))))

(def far-cell
  {:blocks (field "wheat" 7 [8] [0]) :ages (ages 7 [8] [0]) :drops wheat-drops})

(defn far-cells [xs] (map #(cell-key % 64 0) xs))

(deftest a-walk-blocked-without-a-reason-is-tried-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world far-cell})
              blocked (atom 0)]
          (.override (.-world p) "moveTo"
                     (fn ^:async f [token args impl]
                       (if (< @blocked 2)
                         (do (swap! blocked inc) #js {:status "blocked"})
                         (await (impl token args)))))
          (let [result (await (child-outcome eng job {} 100))]
            (is (= {:cut 1 :replanted 1 :bare [] :lost [] :gave-up false} result))))))))

(deftest a-walk-with-no-path-skips-the-crop-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (far-cells (range 8 13))
              {:keys [eng p]} (start {:world {:blocks (zipmap cells (repeat "wheat")) :ages (zipmap cells (repeat 7))
                                              :noPath cells}})
              result (await (child-outcome eng job {:radius 14} 100))]
          (is (= 4 (count (calls p "moveTo"))))
          (is (zero? (count (calls p "dig"))))
          (is (= {:cut 0 :replanted 0 :bare [] :lost [] :gave-up true} result)))))))

(deftest a-crop-is-skipped-at-the-third-blocked-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (far-cells [8 9])
              {:keys [eng p]} (start {:world {:blocks (zipmap cells (repeat "wheat")) :ages (zipmap cells (repeat 7))
                                              :unreachable cells}})
              result (await (child-outcome eng job {} 100))]
          (is (= 6 (count (calls p "moveTo"))))
          (is (= {:cut 0 :replanted 0 :bare [] :lost [] :gave-up false} result))
          (is (= [] (:list (core/state eng)))))))))

(deftest a-replanted-cell-lost-before-the-finish-is-bare
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world one-cell})
              cell {:x 3 :y 64 :z 0}]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))
                             blocks (.-blocks (.-state (.-world p)))]
                         (.delete blocks (cell-key 3 64 0))
                         (.set blocks (cell-key 3 63 0) "dirt")
                         r)))
          (let [result (await (child-outcome eng job {} 100))]
            (is (= {:cut 1 :replanted 1 :bare [cell] :lost [cell] :gave-up false} result))))))))

(deftest the-sweep-is-owed-before-the-first-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world wheat-world})
              owed (atom [])]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (swap! owed conj (:collect (core/job-memory eng "j1")))
                       (await (impl token args))))
          (core/submit! eng (list job {}) {})
          (await (run-until-empty eng 200))
          (is (= 9 (count @owed)))
          (is (every? true? @owed)))))))

(defn send-body-away!
  "After the dig at the one-cell field the body stands 60 blocks away, as after a chase or a respawn."
  [p]
  (.override (.-world p) "dig"
             (fn ^:async f [token args impl]
               (let [r (await (impl token args))]
                 (set! (.-pos (.-self (.-state (.-world p)))) #js {:x 60 :y 64 :z 0})
                 r))))

(deftest a-body-far-from-its-field-walks-back-and-replants
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world one-cell})]
          (send-body-away! p)
          (let [result (await (child-outcome eng job {} 100))
                homeward (filterv #(zero? (.-x (.-pos (.-args %)))) (calls p "moveTo"))]
            (is (= 1 (count homeward)))
            (is (= {:cut 1 :replanted 1 :bare [] :lost [] :gave-up false} result))
            (is (= "wheat" (block-at p 3 64 0)))))))))

(deftest a-centre-that-cannot-be-reached-ends-the-job-with-the-cells-bare
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:world one-cell})
              cell {:x 3 :y 64 :z 0}]
          (send-body-away! p)
          (.override (.-world p) "moveTo"
                     (fn ^:async f [token args impl]
                       (if (zero? (.-x (.-pos args)))
                         #js {:status "blocked"}
                         (await (impl token args)))))
          (let [result (await (child-outcome eng job {} 100))]
            (is (= 3 (count (filterv #(zero? (.-x (.-pos (.-args %)))) (calls p "moveTo")))))
            (is (= {:cut 1 :replanted 0 :bare [cell] :lost [] :gave-up false} result))))))))

;; ------------------------------------------------------------------ zones and claims

(def zoned-world
  {:blocks (field "wheat" 7 [2] [2])
   :ages (ages 7 [2] [2])
   :inventory [{:name "wheat_seeds" :count 4}]
   :drops wheat-drops})

(defn zone-over-crop [owner] {:name "farm" :min [2 60 2] :max [2 70 2] :owner owner})

(deftest a-crop-follows-its-zone-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra cut] [["Fake" {} 1] ["FAKE" {} 1] ["Miles" {} 0] ["Miles" {:ignore-zones? true} 1]]]
          (let [{:keys [eng p seen]} (start {:world zoned-world :shared (ew/of-data {} {} [(zone-over-crop owner)])})]
            (core/submit! eng (list job (merge {:radius 6} extra)) {})
            (await (run-until-empty eng 60))
            (is (= cut (count (calls p "dig"))) (pr-str [owner extra]))
            (is (= (if (zero? cut) [["farm"]] [])
                   (mapv :zones (events-of seen :harvest.declined))) (pr-str [owner extra]))))))))

(deftest no-zone-list-declines-the-harvest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:world zoned-world :shared (ew/of-data {} {} nil)})]
          (core/submit! eng (list job {:radius 6}) {})
          (await (run-until-empty eng 60))
          (is (zero? (count (calls p "dig"))))
          (is (= [:no-zones] (mapv :reason (events-of seen :harvest.declined)))))))))
