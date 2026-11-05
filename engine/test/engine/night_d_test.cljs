(ns engine.night-d-test
  "The one night handler: the :night trigger and jobs.survival.night. A bed first (seen, remembered within the default-bed-radius setting
  under any roof, or carried, also in the open), then a sleeper on the server (the action bar's sleep count, or a
  sleeping player in sight): logged out in short stints until morning, then roofed or buried: the queue runs, else
  dig in."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.events :as events]
            [engine.hut-shelter-test :as hut]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.perception.store :as store]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.lib.price :as price]
            [jobs.lib.sense :as sense]
            [engine.sense-test :as sense-t]
            [jobs.survival.night.hold :as hold]
            [jobs.lib.util :as u]
            [engine.triggers :as triggers]
            
            [jobs.lib.targets :as targets]
            [jobs.survival.bed-down :as bed-down]
            [jobs.lib.food-animals :as animals]
            [jobs.survival.night :as night]
            [jobs.survival.night.flee :as flee]
            [jobs.survival.night.sites :as sites]
            [engine.night-support :refer [night noon roof holds? write! status! closed-hut holds-after? sleeper offline-why on-return! sleep-count! log-out-child fired wide-ground hillside breached-room run-night-in home-roof lit-world know-home! night-at-home with-child-log walled-home with-walled-home walled-pen bed-world bed-places dug-into-home stone-room block-at room-with-doorway doorway-shut-early dawn-holds-now? dark-world flee-home-night stone-and-dirt-patch flee-with cave-blocks cave-night held-starving zombie]]))

(deftest with-flee-radius-0-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {:settings {:jobs.survival.night.sites/flee-radius 0}}))]
          (st/dawn-after! p 4)
          (await (tu/run-until-empty eng 80))
          (is (empty? (st/emitted seen :shelter.fled)))
          (is (= 1 (count (st/emitted seen :shelter.exposed)))))))))

(deftest a-flee-that-cannot-arrive-is-reported-and-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (with-walled-home dark-world) {}))]
          (st/dawn-after! p 6)
          (let [log (await (with-child-log #(tu/run-until-empty eng 80)))]
            (is (= 1 (count (filter #(= :flee (first %)) log))) "one walk, never the same target twice")
            (let [args (nth (first (filter #(= :flee (first %)) log)) 2)]
              (is (= {:place :home :range 0 :floor {:health 0}} (dissoc args :worth)))
              (is (pos? (:worth args)) "the flee walk is worth what the next way costs more, not the night's 0: go-to's unseen-floor turn-back would end it")))
          (is (= 1 (count (st/emitted seen :shelter.flee_failed))))
          (is (= 1 (count (st/emitted seen :shelter.exposed))))
          (is (= [[:exposed 1]] (mapv (juxt :reason :fled) (st/emitted seen :stopped)))))))))

(deftest a-flee-that-go-to-ends-stranded-is-reported-as-staying-put
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (with-walled-home dark-world) {}))]
          (st/dawn-after! p 6)
          (await (with-child-log #(tu/run-until-empty eng 80)
                   (fn [_ k _ _] (when (= :flee k) {:kind :impossible :cause {:reason :stranded}}))))
          (is (= [true] (mapv :stranded (st/emitted seen :shelter.flee_failed)))))))))

(deftest a-flee-cut-mid-walk-resumes-the-same-target-from-where-the-body-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))
              n (atom 0)
              log (await (with-child-log #(tu/run-until-empty eng 80)
                           (fn [_ k _ _] (when (and (= :flee k) (= 1 (swap! n inc)))
                                           (st/teleport! p 4 64 0)
                                           :continue))))
              flees (filter #(= :flee (first %)) log)]
          (is (<= 2 (count flees)) "walked again after the cut")
          (is (apply = (map #(nth % 2) flees)) "the same target"))))))

(deftest with-nothing-to-dig-here-the-night-flees-to-seen-ground-the-carried-tools-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/wall-stack :blocks stone-and-dirt-patch})]
          (tu/seeing-all p)
          (.override (.-world p) "place"
                     (fn ^:async f [token a impl]
                       (if (< (.-x (.-pos a)) 20)
                         #js {:status "no-support"}
                         (await (impl token a)))))
          (st/dawn-after! p 12)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 120))
          (is (= [:ground] (mapv :target (st/emitted seen :shelter.fled))))
          (is (some #(<= 27 (:x (st/arg-pos %))) (st/calls p "place")) "the pit's roof is placed at the patch"))))))

(deftest the-flee-walks-to-the-target-go-to-costs-cheapest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[log seen calls] (await (flee-with (fn [_] {:uses {:job-time [0 7 0]}})))]
          (is (seq (filter #(= :flee (first %)) log)))
          (is (= [7] (mapv :cost (st/emitted seen :shelter.fled))) "the quote's price is reported")
          (is (every? #(or (:place %) (:pos %)) calls) "each candidate is quoted as the flight would walk it")
          (is (every? #(= ##Inf (:worth %)) calls) "the flight's worth is derived from the quote, so the quote is not refused on the night's worth"))))))

(deftest a-flee-prices-the-fight-at-its-target-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [prices (fn ^:async a [fight]
                       (let [real price/way-fight-hp]
                         (set! price/way-fight-hp (fn [_ _] fight))
                         (try
                           (let [[_ seen] (await (flee-with (fn [_] {:uses {:job-time [0 7 0]}})))]
                             (:prices (first (st/emitted seen :night.answers))))
                           (finally (set! price/way-fight-hp real)))))
              calm (await (prices 0))
              hurt (await (prices 2))
              more #(- (% hurt) (% calm))]
          (is (pos? (more :exposed)) (pr-str calm hurt))
          (is (< (js/Math.abs (- (more :flee) (more :exposed))) 0.5) "the same fight adds as much at the target as here"))))))

(deftest a-flee-search-still-going-walks-nowhere-until-it-answers
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              [log seen calls] (await (flee-with (fn [_] (if (< (swap! n inc) 4) {:estimate true} {:uses {:job-time [0 3 0]}}))))]
          (is (= 4 (count calls)) "asked again after each :searching")
          (is (= 1 (count (filter #(= :flee (first %)) log))) "walks once, after the answer")
          (is (= [3] (mapv :cost (st/emitted seen :shelter.fled)))))))))

(deftest the-flee-quotes-ground-sites-past-the-nearest-until-one-is-reachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far {:x -14 :y 64 :z 2}
              ;; the ground west of the failed sites (x 1 and 10), 35 cells: the old nearest-16 cut never quoted the farthest
              ground (into-array (for [x (range -8 -15 -1) z (range -2 3)] #js {:name "dirt" :pos #js {:x x :y 63 :z z}}))
              [log _ calls] (await (flee-with (fn [{:keys [pos]}] (if (= far pos) {:uses {:job-time [0 7 0]}} (contract/ending :nothing-possible {:reason :no-path} nil)))
                                              ground))]
          (is (= [far] (map #(:pos (nth % 2)) (filter #(= :flee (first %)) log))) "the farthest ground site, the only one go-to reaches, is walked to")
          (is (< 16 (count (filter :floor calls))) "quoted past the nearest 16"))))))

(deftest seen-ground-the-planner-cannot-stand-on-is-never-quoted
  ;; KitOak93 on a snow plateau: powder snow is ground a site's rules took and go-to answers goal-not-standable, 100-200 quotes a round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far {:x -14 :y 64 :z 2}
              powder (for [x (range -8 -12 -1) z (range -2 3)] {:x x :y 63 :z z})
              ground (into-array (concat (for [{:keys [x y z]} powder] #js {:name "powder_snow" :pos #js {:x x :y y :z z}})
                                         (for [z (range -2 3)] #js {:name "dirt" :pos #js {:x -14 :y 63 :z z}})))
              [log _ calls] (await (flee-with (fn [{:keys [pos]}] (if (= far pos) {:uses {:job-time [0 7 0]}} (contract/ending :nothing-possible {:reason :goal-not-standable} nil)))
                                              ground (into {} (for [{:keys [x y z]} powder] [(str x "," y "," z) "powder_snow"]))))
              powder-feet (set (map #(update % :y inc) powder))]
          (is (= [far] (map #(:pos (nth % 2)) (filter #(= :flee (first %)) log))))
          (is (not-any? (comp powder-feet :pos) calls) "no walk onto powder snow is quoted")
          (is (seq (filter :floor calls))))))))

(deftest with-no-reachable-target-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[log seen] (await (flee-with (fn [_] (contract/ending :nothing-possible {:reason :no-path} nil))))]
          (is (empty? (filter #(= :flee (first %)) log)))
          (is (= 1 (count (st/emitted seen :shelter.exposed)))))))))

(deftest with-blocks-to-close-the-open-sides-the-night-flees-to-an-overhang-and-walls-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (cave-night [{:name "dirt" :count 12}])
              log (await (with-child-log #(tu/run-until-empty eng 160)))
              enclose (first (filter #(and (= 'jobs.survival.shut-in (second %)) (= #{:walls} (:ways (nth % 2)))) log))]
          (is (= [:cave] (mapv :target (st/emitted seen :shelter.fled))))
          (is (= 1 (count (st/emitted seen :shut-in.sealed))))
          (is (some? enclose) "walls it in, never a pit")
          (is (every? #(<= 21 (:x (st/arg-pos %))) (st/calls p "place")) "placed only at the cave"))))))

(deftest with-too-few-blocks-the-night-ignores-the-overhang
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (cave-night [{:name "dirt" :count 3}])]
          (await (tu/run-until-empty eng 160))
          (is (empty? (st/emitted seen :shelter.fled))))))))

(deftest logs-are-no-wall-blocks-for-an-overhang
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (cave-night [{:name "oak_log" :count 20}])
              log (await (with-child-log #(tu/run-until-empty eng 160)))]
          (is (empty? (st/emitted seen :shelter.fled)))
          (is (seq (filter #(and (= 'jobs.survival.shut-in (second %)) (not= #{:walls} (:ways (nth % 2)))) log))
              "dug in where it stood, not walled in at the cave"))))))

(deftest an-overhang-whose-roof-the-body-has-not-seen-is-no-flee-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (cave-night [{:name "dirt" :count 8}])
              all (.-seenBlocks p)]
          (aset p "seenBlocks" (fn [q] (.filter (all q) (fn [b] (< (.-y (.-pos b)) 66)))))
          (await (tu/run-until-empty eng 160))
          (is (empty? (st/emitted seen :shelter.fled))))))))

(deftest the-flee-sites-are-every-seen-cave-and-dirt-cell-and-are-kept-while-memory-and-the-body-hold
  ;; 61 x 61 columns across x 1024 (where a cell key's bits wrap) and z 0 under a stone roof, all remembered
  (let [p (tu/fake {:self {:pos [1030 64 -10]}
                    :blocks (merge (tu/box 1000 63 -40 1060 63 20 "dirt") (tu/box 1010 66 -5 1030 66 5 "stone"))})
        raw (fake-raw/create p)
        per (perception/create raw {:now (constantly 1000000)})
        ^js st (:st per)
        record! (fn [x y z id] (store/record! st (store/store-of st (.-dim st)) x y z id 1000000))
        _ (doseq [x (range 1000 1061) y (range 63 67) z (range -40 21)] (record! x y z (.stateAt raw x y z)))
        w (perception/wrap p per)
        kinds #(set (map (juxt :kind (comp (juxt :x :y :z) :pos)) %))
        sites (flee/sites w 64)]
    (is (= (into (set (for [x (range 1010 1031) z (range -5 6)] [:cave [x 64 z]]))
                 (for [x (range 1000 1061) z (range -40 21)] [:ground [x 64 z]]))
           (kinds sites))
        "under the roof a cave; over every dirt block ground; the roof's stone is no ground without a pickaxe")
    (is (= (+ (* 21 11) (* 61 61)) (count sites)))
    (is (identical? sites (flee/sites w 64)) "nothing remembered changed: the same list")
    (record! 1040 66 0 (.stateAt raw 1010 66 0))
    (is (contains? (kinds (flee/sites w 64)) [:cave [1040 64 0]]) "a roof block seen since: a new cave")
    (swap! (fake/state p) assoc-in [:self :pos] [1090 64 -10])
    (is (< (count (flee/sites w 64)) (count sites)) "moved off: the far side is out of the flee radius")))

(deftest with-nothing-seen-the-night-looks-around-and-then-flees-to-the-ground-it-saw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})]
          (tu/seeing-after-look p)
          (.override (.-world p) "place"
                     (fn ^:async f [token a impl]
                       (if (< (.-x (.-pos a)) 20)
                         #js {:status "no-support"}
                         (await (impl token a)))))
          (st/dawn-after! p 12)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 160))
          (is (seq (st/calls p "look")))
          (is (= [:ground] (mapv :target (st/emitted seen :shelter.fled)))))))))

(deftest the-flee-skips-seen-ground-with-water-under-it-for-ground-a-pit-fits-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wet-patch (into {} (for [x (range 8 13) z (range -2 3) y (range 60 63)] [(str x "," y "," z) "water"]))
              {:keys [eng p seen]} (st/setup {:time night :inventory st/dirt-stack :blocks (merge stone-and-dirt-patch wet-patch)})]
          (tu/seeing-all p)
          (st/dawn-after! p 12)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 160))
          (let [fled (st/emitted seen :shelter.fled)]
            (is (= [:ground] (mapv :target fled)))
            (is (<= 27 (:x (:pos (first fled)))) "the solid patch, not the nearer ground over water")))))))

;; ------------------------------------------------------------------ a bed not yet looked at
(deftest a-bed-in-the-room-not-yet-looked-at-is-slept-in-not-replaced-by-the-carried-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup (assoc hut/night-bed-world :inventory [{:name "red_bed" :count 1}]))]
          (tu/seeing-after-look p)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (seq (st/calls p "look")) "looked around first")
          (is (= 1 (count (st/calls p "sleep"))) "slept in the seen bed")
          (is (empty? (st/calls p "place")) "no second bed")
          (is (empty? (st/ended-because seen :bed-place-failed)) "did not decide to put the carried bed down unlooked"))))))

(deftest monsters-near-a-free-bed-hold-the-sleep-as-monsters-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"1,64,0" "red_bed"} :monstersNear true})
              _ (core/submit! eng '(jobs.survival.sleep {:bed {:x 1 :y 64 :z 0}}) {})]
          (tu/seeing-after-look p)
          (await (st/tick-n eng 6))
          (is (empty? (st/emitted seen :bed_unusable)) "not a failed sleep")
          (is (= 1 (count (:list (core/state eng)))) "the call holds")
          (is (= [:monsters-near] (distinct (map :reason (st/emitted seen :holding))))))))))

(deftest a-hostile-near-ends-the-night-nothing-possible-monsters-near-without-calling-bed-down-every-pass
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (held-starving [zombie] {:food 20 :health 20} [{:name "red_bed" :count 1}])]
          (core/submit! eng '(jobs.survival.night) {})
          (await (core/tick! eng))
          (is (empty? (st/calls p "sleep")) "no sleep with a hostile near")
          (is (>= 1 (count (filterv #(and (= :child_started (:kind %)) (= :bed-down (:slot %))) @seen))) "one bed-down, then the round ends")
          (is (= [:monsters-near] (distinct (map :reason (st/emitted seen :stopped))))))))))

(deftest the-food-search-of-a-starving-hold-carries-the-hunger-worth
  (let [{:keys [p]} (held-starving [])
        c {:primitives p :args {}}
        args (hold/food-args c)]
    (is (= (animals/search-benefit {:primitives p :args {:carry 0}}) (:worth args))
        "get-food walks are judged against what food is worth the hungry body, not the night's open-gain")
    (is (pos? (:worth args)))))

(deftest an-occupied-bed-not-yet-looked-at-is-refused-before-the-sleep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})
              id (core/submit! eng '(jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}}) {})]
          (tu/seeing-after-look p)
          (await (st/tick-n eng 6))
          (is (empty? (st/calls p "sleep")) "no sleep on an occupied bed")
          (is (= 1 (count (:list (core/state eng)))) "the call holds for the bed")
          (is (= [:bed-occupied] (distinct (map :reason (st/emitted seen :holding))))))))))

(deftest a-room-not-yet-looked-at-is-looked-at-before-the-carried-bed-has-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}] :skipNight false})]
          (tu/seeing-after-look p)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 12))
          (is (seq (st/calls p "place")) "put the bed down")
          (let [names (mapv #(.-name %) (.-calls (.-world p)))]
            (is (< (.indexOf names "look") (.indexOf names "place")) "the room was looked at before the bed went down")
            (is (not= -1 (.indexOf names "look"))))
          (is (empty? (st/ended-because seen :bed-place-failed))))))))

(deftest the-dig-in-at-an-open-bed-never-digs-a-bed-half
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :self {:pos {:x 1 :y 64 :z 0}}
                                         :blocks (merge st/floor {"1,64,0" "red_bed" "2,64,0" "red_bed"})})]
          (st/know-bed! eng {:x 1 :y 64 :z 0})
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 60))
          (is (empty? (filter #(#{{:x 1 :y 64 :z 0} {:x 2 :y 64 :z 0}} %) (map st/arg-pos (st/calls p "dig")))) "no bed half dug")
          (is (= "red_bed" (.-name (.blockAt p (tu/pos 2 64 0))))))))))

(deftest a-carried-bed-is-never-put-down-on-a-chest-or-a-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}]
                                         :blocks (merge (tu/box -4 63 -4 4 63 4 "chest") {"2,63,2" "crafting_table"})})]
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 30))
          (is (empty? (st/calls p "place")) "no bed on a container floor")
          (is (= 1 (count (st/entries eng :bed-place-failed)))))))))

(deftest sleep-holds-an-occupied-bed-until-its-wall-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})
              id (core/submit! eng '(jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}}) {})]
          (tu/seeing-after-look p)
          (await (st/tick-n eng 3))
          (is (= 1 (count (:list (core/state eng)))) "holds, not condition-failed at the start")
          (is (= [:bed-occupied] (distinct (map :reason (st/emitted seen :holding)))))
          (swap! clock + 3600000)
          (await (st/tick-n eng 3))
          (is (empty? (:list (core/state eng))) "the wall time bounds the hold")
          (is (empty? (st/calls p "sleep"))))))))

(defn ^:async log-out-asked
  "The :log-out? bed-down/carried-answer hands answers, for a body at night with another player asleep and world's extra entities."
  [world]
  (let [asked (atom nil)
        c {:primitives (tu/seeing-all (tu/fake-on-floor (merge {:time night :players ["Alex"]} (update world :entities #(conj (vec %) sleeper))))) :args {} :id "j" :root "j" :slots []
           :view (fn [] {:now 0 :data mem/empty-data}) :engine {:warned (atom #{})}}]
    (with-redefs [bed-down/answers (fn ^:async answers [_ opts] (reset! asked (:log-out? opts)) {:exposed 1})]
      (await (bed-down/carried-answer c)))
    @asked))

(deftest a-log-out-is-priced-only-while-the-body-is-out-of-danger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (await (log-out-asked {}))) "calm: the log-out is one of the answers")
        (is (false? (await (log-out-asked {:entities [zombie]}))) "a hostile near: it is not priced")
        (is (true? (await (log-out-asked {:self {:inWater true :oxygen 10} :blocks {"0,64,0" "water" "0,65,0" "water"}})))
            "low air in water with air reachable is no drowning")
        (is (true? (await (log-out-asked {:self {:inWater true :onGround false}}))) "off the ground in water is no fall")
        (is (false? (await (log-out-asked {:self {:inWater true :oxygen 4} :blocks {"0,64,0" "water" "0,65,0" "water" "0,66,0" "water" "0,67,0" "water"}})))
            "drowning")
        (is (false? (await (log-out-asked {:self {:onGround false}}))) "off the ground in the air")))))

(deftest a-roofed-place-look-is-priced-by-the-way-it-spares-the-dig-in-else-the-cheapest-other
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [c (sense-t/stub-ctx (tu/fake {:self {:pos {:x 0 :y 64 :z 0}}}))
              reads {:cells [[3 64 0]]}
              delta (fn ^:async delta [dig-s prices]
                      (let [opts (atom nil)]
                        (with-redefs [bed-down/dig-in-s (fn ^:async q [_] dig-s)
                                      night/shelter-prices (fn ^:async sp [_] prices)
                                      sense/decide! (fn ([_ _ _] (js/Promise.resolve nil)) ([_ _ _ o] (reset! opts o) (js/Promise.resolve nil)))
                                      night/dig-in! (fn ^:async di [_ _] :again)
                                      hold/busy! (fn [_])]
                          (await (night/shelter! c)))
                        (some-> (:price @opts) (apply [nil reads]) :delta)))]
          (is (= 12 (await (delta 12 [{:niche 50 :stay 30} nil]))) "a dig-in quoted: a place spares it")
          (is (= 30 (await (delta ##Inf [{:niche 50 :stay 30} nil]))) "none quoted: the cheapest other way, not ##Inf")
          (is (= ##Inf (await (delta ##Inf [{} nil]))) "no way priced at all: only sight bounds the looks")
          (is (nil? (await (delta ##Inf :searching))) "the other ways still searching: no look this pass"))))))

(deftest the-other-ways-a-place-spares-are-priced-once-a-dig-in-and-not-when-no-look-follows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [priced (fn ^:async priced [mem passes]
                       (let [c (sense-t/stub-ctx (tu/fake {:self {:pos {:x 0 :y 64 :z 0}}}))
                             n (atom 0)]
                         (reset! (:mem c) mem)
                         (with-redefs [bed-down/dig-in-s (fn ^:async q [_] ##Inf)
                                       night/shelter-prices (fn ^:async sp [_] (swap! n inc) [{:stay 30} nil])
                                       sense/decide! (fn ([_ _ _] (js/Promise.resolve nil)) ([_ _ _ _] (js/Promise.resolve nil)))
                                       night/dig-in! (fn ^:async di [_ _] :again)
                                       hold/busy! (fn [_])]
                           (dotimes [_ passes] (await (night/shelter! c))))
                         @n))]
          (is (= 1 (await (priced {:digging [0 64 0]} 3))) "three passes of one dig-in: priced on the first")
          (is (= 0 (await (priced {:digging [0 64 0] :roof-walk-failed true} 2))) "no look for a place tonight: nothing to price"))))))
