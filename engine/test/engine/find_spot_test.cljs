(ns engine.find-spot-test
  "jobs.farm.find-spot against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.perception :as perception]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.util :as u]
            [engine.test-util :as tu :refer [run-until-empty]]
            [engine.triggers :as triggers]
            [jobs.farm.find-spot :as fs]
            [jobs.lib.surface :as surface]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn ^:async child-outcome
  "tu/child-outcome without the status and ending the adapter adds."
  [eng job args n]
  (let [r (await (tu/child-outcome eng job args n))]
    (if (map? r) (dissoc r :status :ending) r)))

(def job 'jobs.farm.find-spot)

(defn patch
  "Blocks of a w x h patch of name at y with NW corner x z."
  [x z w h y name]
  (into {} (for [dx (range w) dz (range h)] [(str (+ x dx) "," y "," (+ z dz)) name])))

(def small {:w 3 :h 3 :range 6})

(defn ice-path
  "Ice at y 63 from x0 to x1, z -1 to 2: ground to walk on that is no spot (an ice top is not a floor to farm)."
  [x0 x1]
  (tu/box x0 63 -1 x1 63 2 "ice"))

;; ---- pure

(defn names-of [m] (fn [{:keys [x y z]}] (get m [x y z] "air")))

(deftest column-top-table
  (are [m expected] (= expected (surface/column-top (names-of m) 0 0 64 12))
    {[0 63 0] "dirt"} 63
    {[0 63 0] "dirt" [0 60 0] "stone"} 63
    {[0 70 0] "stone"} 70
    {} nil
    {[0 63 0] "water"} nil
    {[0 63 0] "lava"} nil
    {[0 63 0] "ice"} nil
    {[0 63 0] "magma_block"} nil
    {[0 63 0] "powder_snow"} nil
    {[0 63 0] "dirt" [0 64 0] "stone"} 64
    {[0 63 0] "grass_block" [0 64 0] "short_grass"} 63
    {[0 63 0] "grass_block" [0 64 0] "poppy"} 63
    {[0 63 0] "grass_block" [0 64 0] "tall_grass" [0 65 0] "tall_grass"} 63
    {[0 63 0] "dirt" [0 64 0] "water" [0 65 0] "air"} nil
    {[0 40 0] "dirt"} nil
    {[0 53 0] "dirt"} 53
    {[0 52 0] "dirt"} nil
    {[0 76 0] "dirt"} 76
    {[0 77 0] "dirt"} nil
    {[0 63 0] "dirt" [0 64 0] "oak_log" [0 65 0] "oak_log" [0 66 0] "oak_leaves"} 63
    {[0 63 0] "dirt" [0 68 0] "birch_leaves" [0 69 0] "birch_leaves"} 63
    {[0 60 0] "dirt" [0 65 0] "birch_leaves"} 60
    {[0 63 0] "dirt" [0 64 0] "oak_leaves"} 63))

(deftest column-top-is-nil-for-unloaded-or-covered
  (is (nil? (surface/column-top (fn [{:keys [y]}] (when (not= y 70) "air")) 0 0 64 12)) "unloaded above the ground")
  (is (nil? (surface/column-top (fn [{:keys [y]}] (cond (= y 63) "dirt" (= y 64) nil :else "air")) 0 0 64 12)) "unloaded above the top")
  (is (= 63 (surface/column-top (fn [{:keys [y]}] (case y 63 "dirt" "cave_air")) 0 0 64 12))))

(deftest score-patch-table
  (are [in expected] (= expected (fs/score-patch in))
    {:tops [63 63 63 63] :water-share 0 :sky true :away 0}
    {:y 63 :level 100 :work 0 :water-share 0 :sky true :away 0 :score 115}
    {:tops [63 63 63 64] :water-share 1 :sky true :away 8}
    {:y 63 :level 75 :work 1 :water-share 1 :sky true :away 8 :score 112}
    {:tops [63 64] :water-share 0 :sky false :away 0}
    {:y 63 :level 50 :work 1 :water-share 0 :sky false :away 0 :score 49}
    {:tops (vec (concat (repeat 30 60) (repeat 3 70))) :water-share 0.5 :sky false :away 200}
    {:y 60 :level 91 :work 30 :water-share 0.5 :sky false :away 200 :score 44}
    {:tops [60 80] :water-share 0 :sky false :away 400}
    {:y 60 :level 50 :work 20 :water-share 0 :sky false :away 400 :score 0}
    {:tops [63 63 63 63] :water-share 0 :sky true :away 0 :farmable 0.5}
    {:y 63 :level 100 :work 0 :water-share 0 :sky true :away 0 :farmable 50 :score 95}
    {:tops [63 nil] :water-share 0 :sky true :away 0} nil))

(deftest hydrated-table
  (are [ws x y z expected] (= expected (fs/hydrated? ws x y z))
    #{[4 63 0]} 0 63 0 true
    #{[4 64 0]} 0 63 0 true
    #{[4 62 0]} 0 63 0 false
    #{[4 65 0]} 0 63 0 false
    #{[5 63 0]} 0 63 0 false
    #{[0 63 -4]} 0 63 0 true
    #{[0 63 5]} 0 63 0 false
    #{} 0 63 0 false))

(deftest max-step-table
  (are [tops h expected] (= expected (fs/max-step tops h))
    [63 63 63 63] 2 0
    [63 64 63 63] 2 1
    [63 63 63 66] 2 3
    [63 63 65 63] 2 2
    [63 70 70 63] 2 7
    [63 63 63 63 63 63 63 63 63] 3 0))

(deftest blocked-fn-leaves-out-plans-and-foreign-zones
  (let [in {:footprints {[5 70 5] "hut"} :zones [{:name "mine" :owner "Me" :min [0 0 0] :max [2 99 2]}
                                                  {:name "theirs" :owner "Other" :min [10 0 0] :max [12 99 2]}]
            :claims [] :self "Me" :now 0}
        b? (fs/blocked-fn in)]
    (are [x y z expected] (= expected (b? x y z))
      5 63 5 true
      6 63 5 false
      1 63 1 false
      11 63 1 true
      13 63 1 false)))

;; ---- job

(def stepped-world
  (merge (patch 2 0 3 3 63 "dirt") {"3,64,1" "dirt"}
         (patch -4 0 3 3 63 "dirt") {"-3,64,1" "dirt" "-3,65,1" "dirt"}))

(deftest max-step-keeps-only-patches-with-small-steps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks stepped-world})
              all (await (child-outcome eng job small 4))
              r (await (child-outcome eng job (assoc small :max-step 1) 4))]
          (is (= [1 2] (sort (map :step (:spots all)))))
          (is (= [{:x 2 :y 63 :z 0}] (mapv :pos (:spots r)))))))))

(def wet-world
  (merge (patch 2 0 3 3 63 "dirt")
         {"6,63,1" "water"}
         (patch -4 0 3 3 63 "dirt")
         (patch 2 -6 3 3 63 "dirt")
         {"3,64,-5" "dirt"}))

(deftest wet-flat-beats-dry-flat-beats-bumpy
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks wet-world})
              r (await (child-outcome eng job small 4))]
          (is (= [{:x 2 :y 63 :z 0} {:x -4 :y 63 :z 0} {:x 2 :y 63 :z -6}] (mapv :pos (:spots r))))
          (is (= {:x 2 :y 63 :z 0} (:spot r)))
          (is (= [1.0 0] (mapv :water-share (take 2 (:spots r))))))))))

(deftest never-picks-water-or-lava
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge (patch 2 0 3 3 63 "water") (patch -4 0 3 3 63 "lava"))})
              r (await (child-outcome eng job small 4))]
          (is (= {:spot nil :reason :none} r)))))))

(deftest no-ground-is-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {:blocks {}})
              r (await (child-outcome eng job small 4))]
          (is (= {:spot nil :reason :none} r)))))))

(deftest limit-caps-the-spots
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks wet-world})
              r (await (child-outcome eng job (assoc small :settings {:jobs.farm.find-spot/limit 1}) 4))]
          (is (= 1 (count (:spots r)))))))))

(deftest find-spot-does-not-move
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks (patch 4 0 3 3 63 "dirt")})]
          (await (child-outcome eng job (assoc small :range 8) 4))
          (is (empty? (tu/walked-to eng)))
          (is (empty? (tu/walk-calls p))))))))

(defn scan-at [world self args from]
  (let [p (tu/seeing-all (tu/fake {:blocks world :self {:pos self}}))]
    (fs/scan p (merge {:w 3 :h 3 :range 6 :depth 12 :limit 50} args) from)))

(deftest scan-reports-sky-for-open-ground-and-not-under-a-roof
  (let [deep (patch 2 0 3 3 2 "stone")
        open (scan-at (merge (patch 2 0 3 3 63 "dirt") deep) {:x 0 :y 64 :z 0} {} {:x 0 :y 64 :z 0})
        roofed (scan-at (merge (patch 2 0 3 3 63 "dirt") {"3,79,1" "oak_leaves"}) {:x 0 :y 64 :z 0} {} {:x 0 :y 64 :z 0})]
    (is (= [true] (mapv :sky open)) "stone far below must not matter")
    (is (= [false] (mapv :sky roofed)) "leaves at from-y + depth + 3 over one column")))

(deftest scan-sees-water-sitting-on-the-platform
  (let [world (assoc (patch -3 -3 9 9 99 "stone") "6,100,0" "water")
        spots (scan-at world {:x 0 :y 100 :z 0} {:range 8} {:x 0 :y 100 :z 0})
        at (fn [x z] (first (filter #(= {:x x :y 99 :z z} (:pos %)) spots)))]
    (is (pos? (:water-share (at 2 -1))) "patch x 2..4 is within 4 of water at x 6")
    (is (zero? (:water-share (at -3 -3))))))

(deftest distance-is-measured-from-center-when-given
  (let [world (merge (patch 2 0 3 3 63 "dirt") (patch -14 0 3 3 63 "dirt"))
        spots (scan-at world {:x 0 :y 64 :z 0} {:range 20} {:x -12 :y 64 :z 1})]
    (is (= {:x -14 :y 63 :z 0} (:pos (first spots))) "the patch near :center beats the one near the body")
    (is (= [{:x -14 :y 63 :z 0} {:x 2 :y 63 :z 0}] (mapv :pos spots)))))

(deftest scan-is-one-call-in-slices-of-a-read-budget
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (patch 2 0 3 3 63 "dirt") (patch -4 4 3 3 63 "dirt") {"6,63,1" "water"})
              a {:w 3 :h 3 :range 10 :depth 12 :limit 3}
              from {:x 0 :y 64 :z 0}
              expected (scan-at world {:x 0 :y 64 :z 0} a from)
              {:keys [eng p]} (setup {:blocks world})
              reads (atom 0)
              orig (.-blockAt p)
              _ (set! (.-blockAt p) (fn [pos] (swap! reads inc) (.call orig p pos)))
              out (atom :not-done)
              parent (tu/recording-parent job a out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))
              ;; one patch row: (2*range+1) z cells x w columns, each reading at most the
              ;; top scan (2*depth) + 1 + the sky scan (2*depth + 8)
              row-reads (* (inc (* 2 (:range a))) (:w a) (+ (* 4 (:depth a)) 9))
              bound (+ (fs/read-budget) row-reads)]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (is (empty? (:list (core/state eng))) "one call scans the whole range")
          (is (> @reads bound) (str "the scan read past one slice: " @reads))
          (is (= expected (:spots @out)))
          (is (= (:pos (first expected)) (:spot @out))))))))

(deftest a-scan-over-open-air-stops-at-the-slice-cap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [a {:w 16 :h 16 :range 48 :depth 7 :limit 3}
              {:keys [eng p]} (setup {:blocks {}})
              reads (atom 0)
              orig (.-blockAt p)
              _ (set! (.-blockAt p) (fn [pos] (swap! reads inc) (.call orig p pos)))
              out (atom :not-done)
              parent (tu/recording-parent job a out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (run-until-empty eng 40))
          (is (empty? (:list (core/state eng))) "the job ends")
          (is (<= @reads 66000) (str "reads over the slice cap: " @reads))
          (is (= :none (:reason @out))))))))

(deftest a-body-moved-mid-scan-does-not-change-the-rows-scanned
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (merge (patch 2 0 3 3 63 "dirt") (patch -4 4 3 3 63 "dirt") {"6,63,1" "water"})
              a {:w 3 :h 3 :range 10 :depth 12 :limit 3}
              expected (scan-at world {:x 0 :y 64 :z 0} a {:x 0 :y 64 :z 0})
              {:keys [eng p]} (setup {:blocks world})
              out (atom :not-done)
              parent (tu/recording-parent job a out)
              eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
          (core/submit! eng '(recording-parent) {})
          (await (core/tick! eng))
          (swap! (fake/state p) assoc-in [:self :pos] [7 64 5])
          (await (run-until-empty eng 40))
          (is (= expected (:spots @out))))))))

(defn scan-reads
  "[reads spots] of one unbounded scan of world from [x y z]."
  [world self a]
  (let [p (tu/seeing-all (tu/fake {:blocks world :self {:pos self}}))
        reads (atom 0)
        orig (.-blockAt p)
        _ (set! (.-blockAt p) (fn [pos] (swap! reads inc) (.call orig p pos)))
        r (fs/scan p a {:x (first self) :y (second self) :z (nth self 2)})]
    [@reads r]))

(deftest flat-ground-stops-at-the-nearest-good-patches
  (let [flat (tu/box -30 62 -30 30 63 30 "grass_block")
        a {:w 5 :h 5 :range 24 :depth 12 :limit 3}
        [reads spots] (scan-reads flat [0 64 0] a)
        [full-reads] (scan-reads flat [0 64 0] (assoc a :limit 100))]
    (is (= 3 (count spots)))
    (is (every? #(<= (js/Math.abs (- (:x (:pos %)) -2)) 2) spots) "all three lie next to the centre")
    (is (< (* 3 reads) full-reads) (str reads " reads against " full-reads " for the whole range"))))

(deftest open-air-with-a-big-patch-reads-far-fewer-columns
  (let [[reads spots] (scan-reads {} [0 64 0] {:w 16 :h 16 :range 48 :depth 7 :limit 3})]
    (is (empty? spots))
    (is (< reads 20000) (str reads " reads"))))

(deftest a-pond-off-centre-beats-dry-ground-by-the-centre
  (let [world (merge (tu/box -30 62 -30 30 63 30 "grass_block") (tu/box 15 63 0 17 63 2 "water"))
        a {:w 5 :h 5 :range 24 :depth 12 :limit 3}
        [_ spots] (scan-reads world [0 64 0] a)]
    (is (pos? (:water-share (first spots))) "the best spot has water within 4")))

(deftest find-spot-looks-around-before-it-judges-the-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks wet-world})
              _ (tu/seeing-after-look p)
              r (await (child-outcome eng job small 8))]
          (is (seq (calls p "look")))
          (is (= [1.0 0] (mapv :water-share (take 2 (:spots r))))))))))

(deftest a-stone-patch-scores-below-a-grass-patch
  (let [p (tu/seeing-all (tu/fake {:blocks (merge (patch 0 0 3 3 63 "stone") (patch 10 0 3 3 63 "grass_block"))}))
        spots (fs/scan p {:w 3 :h 3 :range 12 :depth 4 :limit 2} {:x 1 :y 64 :z 1})]
    (is (= [100 0] (mapv :farmable (sort-by (comp - :farmable) spots))))))

(def pond-east
  (merge (tu/box -30 63 -30 30 63 30 "grass_block") (tu/box 18 63 -1 20 63 1 "water")))

(deftest a-pond-18-blocks-out-is-found-whatever-the-pass-jitter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [j [0 0.1 0.3 0.5 0.7 0.9]]
          (let [raw (tu/blind (tu/fake {:blocks pond-east}))
                p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000) :rand (constantly j)}))
                {:keys [eng]} (setup {:blocks pond-east})
                eng (assoc eng :primitives p)
                r (await (child-outcome eng job {:w 5 :h 5 :range 24} 12))]
            (is (pos? (:water-share (first (:spots r)))) (str "jitter " j ": " (pr-str (first (:spots r)))))))))))

(def hillside
  (into {} (for [i (range 22)] (tu/box (+ 24 (* 4 i)) 0 0 (+ 27 (* 4 i)) i 63 "grass_block"))))

(deftest a-hillside-finds-spots-seen-from-its-slope
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [raw (tu/blind (tu/fake {:blocks hillside :self {:pos [64.5 11 32.5]}}))
              p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (constantly 1000000) :rand (constantly 0.5)}))
              {:keys [eng]} (setup {:blocks hillside})
              eng (assoc eng :primitives p)
              r (await (child-outcome eng job {:w 5 :h 5 :range 24} 20))]
          (is (seq (:spots r)) (pr-str r)))))))

(deftest existing-farmland-ranks-with-farmable-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:blocks (merge (patch 1 0 3 3 63 "farmland") (patch -6 0 3 3 63 "grass_block"))})
              r (await (child-outcome eng job small 4))]
          (is (= [{:x 1 :y 63 :z 0} {:x -6 :y 63 :z 0}] (mapv :pos (:spots r))))
          (is (= [100 100] (mapv :farmable (:spots r)))))))))
