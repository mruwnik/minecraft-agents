(ns engine.night-d-test
  "The one night handler: the :night trigger and jobs.survival.night. A bed first (seen, remembered at any distance
  under any roof, or carried, also in the open), then a sleeper on the server (the action bar's sleep count, or a
  sleeping player in sight): logged out in short stints until morning, then roofed or buried: the queue runs, else
  dig in."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [jobs.lib.cost :as cost]
            [engine.events :as events]
            [engine.hut-shelter-test :as hut]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.goal-worth-test :as gw]
            [engine.perception :as perception]
            [engine.perception.store :as store]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.lib.price :as price]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.sense :as sense]
            [engine.sense-test :as sense-t]
            [jobs.survival.night.hold :as hold]
            [jobs.lib.util :as u]
            [engine.triggers :as triggers]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.survival.bed-down :as bed-down]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.food-animals :as animals]
            [jobs.survival.night :as night]
            [jobs.survival.night.flee :as flee]
            [jobs.survival.night.sites :as sites]
            [jobs.lib.clock :as clock]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.search :as wsearch]
            [engine.night-support :refer [night noon roof holds? write! status! closed-hut holds-after? sleeper offline-why on-return! sleep-count! log-out-child fired wide-ground hillside breached-room run-night-in home-roof lit-world know-home! night-at-home with-child-log walled-home with-walled-home walled-pen bed-world bed-places dug-into-home stone-room block-at room-with-doorway doorway-shut-early dawn-holds-now? dark-world flee-home-night stone-and-dirt-patch flee-with cave-blocks cave-night held-starving zombie]]))

(deftest without-the-flee-way-the-night-holds-exposed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {:ways #{:bed :shelter}}))]
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
              (is (= {:place :home :range 0 :floor {:health 0}} (dissoc args :worth :spend)))
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

(deftest a-flee-walk-that-spends-its-price-nearer-the-target-is-priced-again-one-no-nearer-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[moved? fails] [[true 0] [false 1]]]
          (let [{:keys [eng p seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))
                n (atom 0)
                log (await (with-child-log #(tu/run-until-empty eng 80)
                             (fn [_ k _ _] (when (and (= :flee k) (= 1 (swap! n inc)))
                                             (when moved? (st/teleport! p 6 64 0))
                                             {:kind :limit-reached :cause {:reason :unreachable :why :search-spent :line :job-time}}))))
                flees (filter #(= :flee (first %)) log)]
            (is (and (seq flees) (every? #(pos? (get-in (nth % 2) [:spend :job-time])) flees)) "each walk spends at most the flee's price")
            (is (= fails (count (st/emitted seen :shelter.flee_failed))))
            (is (= (if moved? 1 0) (count (st/emitted seen :shelter.flee_leg))))
            (is (= (if moved? [:roofed-place] []) (mapv :target (st/emitted seen :shelter.fled)))
                (if moved? "walked on to the same target from where the leg ended" "the target is not walked to again tonight"))))))))

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
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
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
          (is (= [7] (map #(get-in (nth % 2) [:spend :job-time]) (filter #(= :flee (first %)) log))) "the walk spends at most that price")
          (is (every? #(or (:place %) (:pos %)) calls) "each candidate is quoted as the flight would walk it")
          (is (and (seq calls) (every? #(nil? (:worth %)) calls)) "each candidate is quoted at the night's worth, inherited: the flight's own worth is derived from the quote"))))))

(deftest a-flee-prices-the-fight-at-its-target-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stays (atom [])
              fought (atom [])
              prices (fn ^:async a [fight]
                       (let [real price/way-fight-hp
                             parts price/stay-parts]
                         (set! price/way-fight-hp (fn [c at] (when-not (= (u/cell-vec at) (u/cell-vec (u/self-pos c))) (swap! fought conj [(:primitives c) at])) fight))
                         (set! price/stay-parts (fn [c chance secs] (let [r (parts c chance secs)] (swap! stays conj [chance secs (:hurt r)]) r)))
                         (try
                           (let [[_ seen] (await (flee-with (fn [_] {:uses {:job-time [0 7 0]}})))]
                             (:prices (first (st/emitted seen :night.answers))))
                           (finally (set! price/way-fight-hp real) (set! price/stay-parts parts)))))
              calm (await (prices 0))
              hurt (await (prices 2))
              more #(- (% hurt) (% calm))]
          (is (pos? (more :exposed)) (pr-str calm hurt))
          (let [[p target] (first @fought)
                ;; the flight's own stay is the shortest one priced at the chance of the cell fought at, the target
                chance (spawn-light/night-chance p target)
                ht (->> @stays (filter #(and (= chance (first %)) (pos? (second %)))) (sort-by second) first last)]
            (is (some? ht) (pr-str @stays))
            (is (< (js/Math.abs (- (more :flee) (cost/hp-cost 2))) 0.15)
                "the fight adds its hp once, on top of the target's own hurt")))))))

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

(deftest a-night-flee-quotes-the-one-site-a-search-picks
  ;; KitOak94: at night each walk's quote is about three times its straight bound, so hundreds of ground sites were
  ;; quoted one by one in one stretch (331 quotes, 29.8 s); one search over them all picks the site, go-to quotes it
  ;; alone, and a call that has spent its search budget quotes the home at the next (each quote 90 ms here)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ground (into-array (for [x (range -2 -41 -1) z (range -6 7)] #js {:name "dirt" :pos #js {:x x :y 63 :z z}}))
              real-target flee/flee-target
              real-quote pick/quote!
              stopped clock/now
              ms (atom 0)
              per-call (atom [])
              quotes (atom 0)]
          (set! clock/now #(deref ms))
          (set! pick/quote! (fn ^:async q [c args]
                              (when (:floor args) (swap! quotes inc) (swap! ms + 90))
                              (await (real-quote c args))))
          (set! flee/flee-target (fn ^:async f [c survey?]
                                   (let [n @quotes
                                         r (await (real-target c survey?))]
                                     (swap! per-call conj (- @quotes n))
                                     r)))
          (try
            (let [{:keys [eng p]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))]
              (aset p "seenBlocks" (fn [_] ground))
              (let [log (await (with-child-log #(tu/run-until-empty eng 80)))]
                (is (seq (filter #(= :flee (first %)) log)))
                (is (seq @per-call))
                (is (every? #(<= % 1) @per-call) (str "of 507 sites and the home, a call quotes one: " (pr-str @per-call)))
                (is (<= @quotes (* 2 (count (filter #(= :flee (first %)) log)))) "the home and one site a flee")))
            (finally (set! flee/flee-target real-target) (set! pick/quote! real-quote) (set! clock/now stopped))))))))

(deftest a-passed-over-flee-site-stays-so-while-what-its-test-read-holds-not-past-its-time
  ;; card 72b694f6: a rejection lapsed with any write in the 7 memory sections round the site
  (let [now (atom 0)
        p (tu/blind (tu/fake {:blocks {"3,63,0" "stone"}}))
        per (perception/create (fake-raw/create p) {:now (constantly 0) :rand (constantly 0.5)})
        w (perception/wrap p per)
        c {:primitives w :view (fn [] {:data mem/empty-data :now @now})}
        site {:kind :ground :pos {:x 3 :y 64 :z 0}}
        see! (fn [pos] (swap! (fake/state p) assoc-in [:blocks pos] "stone") (perception/pass! per))
        ^js st (:st per)
        stamp #(store/section-stamp st (.-dim st) 0 4 0)]
    (swap! (fake/state p) assoc-in [:self :pos] [1 64 0])
    (perception/pass! per)
    (let [[_ reads] (sense/reading w #(mapv (fn [cell] (u/seen-name w cell)) [{:x 3 :y 64 :z 0} {:x 2 :y 65 :z 4}]))]
      (flee/reject! c site reads))
    (perception/pass! per)
    (is ((flee/rejected-fn c) site) "the body moved: the site's own test does not rest on where it stands")
    (is (not ((flee/rejected-fn c) (assoc site :kind :cave))))
    (let [before (stamp)]
      (store/record! st (store/store-of st (.-dim st)) 5 70 5 (.stateAt (:raw per) 3 63 0) 0)
      (is (> (stamp) before) "a cell of the site's own section was written")
      (is ((flee/rejected-fn c) site) "a cell near the site its test did not read changed: the rejection holds"))
    (see! [2 65 4])
    (is (not ((flee/rejected-fn c) site)) "a cell its test read changed")
    (flee/reject! c site nil)
    (reset! now wsearch/search-max-age-ms)
    (is (not ((flee/rejected-fn c) site)) "past its time it is tried again")))

(deftest flee-ground-sites-skip-powder-snow-and-keep-other-seen-ground
  (let [p (tu/fake {})
        solids [{:name "powder_snow" :pos {:x 0 :y 63 :z 0}} {:name "dirt" :pos {:x 1 :y 63 :z 0}} {:name "dirt" :pos {:x 5 :y 40 :z 5}}]]
    (is (= [{:x 1 :y 64 :z 0} {:x 5 :y 41 :z 5}] (map :pos (flee/ground-sites p solids))) "seen dirt stays a candidate, wherever it is")))

(deftest a-cave-site-needs-two-free-cells-not-powder-snow
  (let [roof {:name "stone" :pos {:x 0 :y 66 :z 0}}
        ground {:name "dirt" :pos {:x 0 :y 63 :z 0}}
        site (fn [name] (flee/cave-sites (tu/seeing-all (tu/fake {:blocks {"0,64,0" name "0,65,0" "air"}})) [ground roof]))]
    (is (= [{:kind :cave :pos {:x 0 :y 64 :z 0}}] (site "air")))
    (is (empty? (site "powder_snow")) "feet in snow")))

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

(deftest a-kept-quote-is-shared-only-by-calls-of-the-same-worth
  ;; card 68e09bb4: the key lacked the worth go-to prices the walk at; a call at another worth got the first's quote
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom [])
              quote! pick/quote!
              {:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})
              args {:place :home :range 0}
              probe (tu/job (fn ^:async probe-round [c]
                              (let [at-worth (fn [w] (assoc c :worth (constantly w)))]
                                (await (pick/kept-quote! (at-worth 50) args))
                                (await (pick/kept-quote! (at-worth 50) args))
                                (await (pick/kept-quote! (at-worth 900) args))
                                (ctx/end c :done nil))))
              eng (assoc eng :jobs (assoc registry/jobs 'worth-probe probe))]
          (tu/seeing-all p)
          (set! pick/quote! (fn [_ a] (swap! asked conj a) (js/Promise.resolve {:uses {:job-time [0 1 1]}})))
          (try
            (core/submit! eng (tu/order '(worth-probe)) {})
            (await (tu/run-until-empty eng 20))
            (finally (set! pick/quote! quote!)))
          (is (= 2 (count @asked)) "worth 50 twice: one ask; worth 900: another"))))))

(deftest a-later-pass-asks-no-quote-of-a-flee-walk-while-the-body-and-its-world-hold
  ;; card 68e09bb4: each pass of a night asked go-to again for every flee site's walk, though nothing had changed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom 0)
              quote! pick/quote!
              out (atom nil)
              {:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})
              pass! (fn ^:async pass [c]
                      (let [before @asked
                            target (await (flee/flee-target (assoc c ::pick/pass (atom {})) false))]
                        [(:pos target) (- @asked before)]))
              probe (tu/job (fn ^:async probe-round [c]
                              (let [first-pass (await (pass! c))
                                    second-pass (await (pass! c))
                                    _ (swap! (fake/state p) assoc-in [:self :pos] [1 64 0])
                                    moved (await (pass! c))]
                                (reset! out [first-pass second-pass moved])
                                (ctx/end c :done nil))))
              eng (assoc eng :jobs (assoc registry/jobs 'flee-probe probe))]
          (tu/seeing-all p)
          (set! pick/quote! (fn [c args] (swap! asked inc) (quote! c args)))
          (try
            (core/submit! eng (tu/order '(flee-probe)) {})
            (await (tu/run-until-empty eng 20))
            (finally (set! pick/quote! quote!)))
          (let [[[at n] [again m] [_ k]] @out]
            (is (pos? n) "the first pass quotes the flee walks")
            (is (= at again) "the same flee target")
            (is (zero? m) "nothing changed: the second pass asks none again")
            (is (pos? k) "the body moved: its walks are asked again")))))))

(defn counting-quotes
  "ctx/quote counting the quotes of job sym in counter (its 3-arity goes through the 4-arity)."
  [orig sym counter]
  (fn ([c j args] (orig c j args))
      ([c j args d] (when (= sym j) (swap! counter inc)) (orig c j args d))))

(deftest a-later-pass-asks-shut-in-no-quote-of-digging-in-while-the-body-and-its-world-hold
  ;; card 2e6fefe1: each night pass asked shut-in again for digging in here (40-100 ms), though nothing had changed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom 0)
              quote ctx/quote
              out (atom nil)
              {:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})
              pass! (fn ^:async pass [c]
                      (let [before @asked]
                        (await (bed-down/dig-in-quote (assoc c ::pick/pass (atom {}))))
                        (- @asked before)))
              probe (tu/job (fn ^:async probe-round [c]
                              (let [first-pass (await (pass! c))
                                    second-pass (await (pass! c))
                                    _ (swap! (fake/state p) assoc-in [:self :pos] [1 64 0])
                                    moved (await (pass! c))]
                                (reset! out [first-pass second-pass moved])
                                (ctx/end c :done nil))))
              eng (assoc eng :jobs (assoc registry/jobs 'dig-in-probe probe))]
          (tu/seeing-all p)
          (set! ctx/quote (counting-quotes quote 'jobs.survival.shut-in asked))
          (try
            (core/submit! eng (tu/order '(dig-in-probe)) {})
            (await (tu/run-until-empty eng 20))
            (finally (set! ctx/quote quote)))
          (is (= [1 0 1] @out) "the first pass asks; the second, nothing changed, none; the body moved: asked again"))))))

(deftest one-pass-prices-the-night-answers-once-for-the-same-ways
  ;; card 2e6fefe1: a pass priced bed-down/answers up to three times over (walkable-home's bound, night-answer, shelter-prices)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [priced (atom 0)
              respawn-body bed-down/respawn-body
              c (gw/ctx-of (tu/seeing-all (tu/fake-on-floor {:time night})) {}
                           {::pick/pass (atom {:dig-in (js/Promise.resolve {:seconds 30 :risk-s 0 :exposure-s 0 :morning-s 10})})})]
          (set! bed-down/respawn-body (fn [c] (swap! priced inc) (respawn-body c)))
          (try
            (let [a (await (bed-down/answers c {}))
                  b (await (bed-down/answers c {}))
                  _ (await (bed-down/answers c {:flee {:pos {:x 20 :y 64 :z 0} :cost 10 :seconds 10}}))]
              (is (= a b))
              (is (= 2 @priced) "the same ways twice: priced once; a flee to weigh: priced again"))
            (finally (set! bed-down/respawn-body respawn-body))))))))

(deftest a-flee-search-still-going-on-leaves-digging-in-priced-to-be-taken
  ;; card 998c3088 / 2e6fefe1: walled-in-home ran 483 passes, each :searching back from shelter-prices while digging in was priced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [flee-target flee/flee-target
              c (gw/ctx-of (tu/seeing-all (tu/fake-on-floor {:time night})) {}
                           {:quote-call {:worth 1200 :ways #{:shelter :flee}}
                            ::pick/pass (atom {:dig-in (js/Promise.resolve {:seconds 30 :risk-s 0 :exposure-s 0 :morning-s 10})
                                               :niche (js/Promise.resolve {:seconds ##Inf :risk-s 0 :exposure-s 0 :morning-s 0})})})]
          (set! flee/flee-target (fn [_ _] (js/Promise.resolve :searching)))
          (try
            (let [[prices t] (await (night/shelter-prices c))]
              (is (contains? prices :shelter) "digging in here is priced: good enough to take now (Q147)")
              (is (not (contains? prices :flee)))
              (is (nil? t)))
            (finally (set! flee/flee-target flee-target))))))))

(deftest a-flee-search-still-going-on-is-kept-going-when-the-cheapest-pick-is-staying-in-the-open
  ;; card 2e6fefe1: the search was dropped once a :shelter price existed though the cheapest pick was :exposed (not good enough, Q147)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [flee-target flee/flee-target
              cheapest bed-down/cheapest
              dear {:seconds 30 :risk-s 0 :exposure-s 0 :morning-s 10}
              c (gw/ctx-of (tu/seeing-all (tu/fake-on-floor {:time night})) {}
                           {:quote-call {:worth 1200 :ways #{:shelter :flee}}
                            ::pick/pass (atom {:dig-in (js/Promise.resolve dear)
                                               :niche (js/Promise.resolve {:seconds ##Inf :risk-s 0 :exposure-s 0 :morning-s 0})})})]
          (set! flee/flee-target (fn [_ _] (js/Promise.resolve :searching)))
          (set! bed-down/cheapest (fn [_ _] :exposed))
          (try
            (is (= :searching (await (night/shelter-prices c))))
            (finally (set! flee/flee-target flee-target)
                     (set! bed-down/cheapest cheapest))))))))

(deftest a-kept-job-quote-is-asked-again-when-a-failed-site-is-remembered
  ;; card 2e6fefe1: a way that failed without changing a block left the key as it was, and the stale "available" quote was kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom 0)
              quote ctx/quote
              entries ctx/entries
              c (gw/ctx-of (tu/seeing-all (tu/fake-on-floor {:time night})) {} {:quote-call {:worth 1200}})]
          (set! ctx/quote (fn ([_ _ _] (swap! asked inc) (js/Promise.resolve {:seconds 1}))
                            ([_ _ _ _] (swap! asked inc) (js/Promise.resolve {:seconds 1}))))
          (try
            (await (pick/kept-job-quote! c 'jobs.survival.shut-in {:for :night}))
            (await (pick/kept-job-quote! c 'jobs.survival.shut-in {:for :night}))
            (is (= 1 @asked) "unchanged: kept")
            (set! ctx/entries (fn [_ _] [{:data {:pos {:x 0 :y 64 :z 0} :way :walls :reason :walls-server-refused}}]))
            (await (pick/kept-job-quote! c 'jobs.survival.shut-in {:for :night}))
            (is (= 2 @asked) "a failed site now remembered: asked again")
            (finally (set! ctx/quote quote) (set! ctx/entries entries))))))))

(deftest a-flee-site-is-judged-once-a-listing-and-again-after-a-move-only-once-the-land-near-it-is-written
  ;; card 0ed400bc: each flee-target call judged every seen site again (pit-site/pit-site?, ~2300 sites: 240 ms), moved or not
  ;; card 72b694f6: a write anywhere listed every site again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [judged (atom 0)
              written (atom 0)
              looks flee/site-looks-open?
              site-memory sense/site-memory
              out (atom nil)
              {:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})
              pass! (fn ^:async pass [c]
                      (let [before @judged]
                        (await (flee/flee-target (assoc c ::pick/pass (atom {})) false))
                        (- @judged before)))
              probe (tu/job (fn ^:async probe-round [c]
                              (let [first-pass (await (pass! c))
                                    again (await (pass! c))
                                    _ (swap! (fake/state p) assoc-in [:self :pos] [1 64 0])
                                    moved (await (pass! c))
                                    _ (swap! written inc)
                                    rewritten (await (pass! c))]
                                (reset! out [first-pass again moved rewritten])
                                (ctx/end c :done nil))))
              eng (assoc eng :jobs (assoc registry/jobs 'flee-probe probe))]
          (tu/seeing-all p)
          (set! flee/site-looks-open? (fn [p x] (swap! judged inc) (looks p x)))
          (set! sense/site-memory (fn [_ _] @written))
          (try
            (core/submit! eng (tu/order '(flee-probe)) {})
            (await (tu/run-until-empty eng 20))
            (finally (set! flee/site-looks-open? looks)
                     (set! sense/site-memory site-memory)))
          (let [[first-pass again moved rewritten] @out]
            (is (pos? first-pass))
            (is (zero? again) "nothing written: no site judged again")
            (is (< moved first-pass) "a step aside: only the sites newly in sight are judged")
            (is (zero? rewritten) "the land near every site written: the listing is not begun again, a pick's own tests read the land")))))))

(deftest a-night-pass-past-its-tick-stops-at-each-step-once-and-the-next-takes-on-its-answers
  ;; card 0ed400bc: a pass after a move priced the bed walk, the home walk and the shelter ways in one go (200-400 ms, Q202: one tick)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory st/dirt-stack :blocks stone-and-dirt-patch})
              pass-ms night/pass-ms
              pass night/pass
              spent? night/tick-spent?
              this-pass (atom nil)
              passes (atom [])]
          (tu/seeing-all p)
          (st/dawn-after! p 12)
          (set! night/pass-ms (constantly ##Inf))
          (set! night/tick-spent? (fn [c k] (let [r (spent? c k)] (when r (swap! this-pass assoc :stopped k :memo (::pick/pass c))) r)))
          (set! night/pass (fn ^:async g [c]
                             (let [calls (count (.-calls (.-world p)))
                                   _ (reset! this-pass {})
                                   r (await (pass c))]
                               (swap! passes conj (assoc @this-pass :acted (some #(not= "look" (.-name %)) (drop calls (.-calls (.-world p))))))
                               r)))
          (try
            (core/submit! eng (tu/order '(jobs.survival.night {})) {})
            (await (tu/run-until-empty eng 60))
            (finally (set! night/pass-ms pass-ms)
                     (set! night/pass pass)
                     (set! night/tick-spent? spent?)))
          (let [[stopped [decided]] (split-with :stopped (drop-while (complement :stopped) @passes))]
            (is (= [:bed :home :shelter] (map :stopped stopped)) "each step stops once, in order")
            (is (not-any? :acted stopped) "a pass that stops changes nothing (a look to decide aside)")
            (is (:acted decided) "the pass after the stops acts")
            (is (= 1 (count (distinct (map :memo stopped)))) "the passes of one decision share one pass memo: nothing asked twice")))))))

(deftest a-night-pass-whose-dig-in-quote-spent-its-tick-leaves-the-home-walk-quote-to-the-next
  ;; card f0fe27c7: the home step asked the dig-in quote and the home's walk quote in one pass (85-150 ms, Q202: one tick)
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (flee-home-night (with-walled-home dark-world) {}))
              pass-ms night/pass-ms
              pass night/pass
              spent? night/tick-spent?
              walk night/cheapest-walk!
              kept pick/kept-job-quote!
              this-pass (atom nil)
              passes (atom [])]
          (st/dawn-after! p 6)
          (set! night/pass-ms (constantly ##Inf))
          (set! night/tick-spent? (fn [c k] (let [r (spent? c k)] (when r (swap! this-pass update :stopped conj k)) r)))
          (set! night/cheapest-walk! (fn ^:async w [c xs opts] (swap! this-pass assoc :walk true) (await (walk c xs opts))))
          (set! pick/kept-job-quote! (fn ^:async k [c sym args]
                                       (when (= #{:walls :pit} (:ways args)) (swap! this-pass update :dig-in (fnil inc 0)))
                                       (await (kept c sym args))))
          (set! night/pass (fn ^:async g [c] (reset! this-pass {}) (let [r (await (pass c))] (swap! passes conj @this-pass) r)))
          (try
            (core/submit! eng (tu/order '(jobs.survival.night {})) {})
            (await (tu/run-until-empty eng 80))
            (finally (set! night/pass-ms pass-ms)
                     (set! night/pass pass)
                     (set! night/tick-spent? spent?)
                     (set! night/cheapest-walk! walk)
                     (set! pick/kept-job-quote! kept)))
          (let [[stopped next-pass] (drop-while #(not (some #{:home-walk} (:stopped %))) @passes)]
            (is (= 1 (:dig-in stopped)) "the pass that stops asked the dig-in quote")
            (is (not (:walk stopped)) "and no walk quote")
            (is (:walk next-pass) "the next pass quotes the home walk")
            (is (nil? (:dig-in next-pass)) "on the dig-in quote the stopped pass got")))))))

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
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
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
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
          (await (tu/run-until-empty eng 160))
          (let [fled (st/emitted seen :shelter.fled)]
            (is (= [:ground] (mapv :target fled)))
            (is (<= 27 (:x (:goal (first fled)))) "the solid patch, not the nearer ground over water")))))))

;; ------------------------------------------------------------------ a bed not yet looked at
(deftest a-bed-in-the-room-not-yet-looked-at-is-slept-in-not-replaced-by-the-carried-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup (assoc hut/night-bed-world :inventory [{:name "red_bed" :count 1}]))]
          (tu/seeing-after-look p)
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
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
              _ (core/submit! eng (tu/order '(jobs.survival.sleep {:bed {:x 1 :y 64 :z 0}})) {})]
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
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
          (await (core/tick! eng))
          (is (empty? (st/calls p "sleep")) "no sleep with a hostile near")
          (is (>= 1 (count (filterv #(and (= :child_started (:kind %)) (= :bed-down (:slot %))) @seen))) "one bed-down, then the round ends")
          (is (= [:monsters-near] (distinct (map :reason (st/emitted seen :stopped))))))))))

(deftest the-food-search-of-a-starving-hold-carries-the-hunger-worth
  (let [{:keys [p]} (held-starving [])
        c {:primitives p :args {} :view (fn [] {:now 0 :data {}})}
        args (hold/food-args c)]
    (is (= (animals/search-benefit (assoc c :args {:carry 0})) (:worth args))
        "get-food walks are judged against what food is worth the hungry body, not the night's open-gain")
    (is (pos? (:worth args)))))

(deftest an-occupied-bed-not-yet-looked-at-is-refused-before-the-sleep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})
              id (core/submit! eng (tu/order '(jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}})) {})]
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
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
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
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
          (await (tu/run-until-empty eng 60))
          (is (empty? (filter #(#{{:x 1 :y 64 :z 0} {:x 2 :y 64 :z 0}} %) (map st/arg-pos (st/calls p "dig")))) "no bed half dug")
          (is (= "red_bed" (.-name (.blockAt p (tu/pos 2 64 0))))))))))

(deftest a-carried-bed-is-never-put-down-on-a-chest-or-a-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time night :inventory [{:name "red_bed" :count 1}]
                                         :blocks (merge (tu/box -4 63 -4 4 63 4 "chest") {"2,63,2" "crafting_table"})})]
          (core/submit! eng (tu/order '(jobs.survival.night {})) {})
          (await (tu/run-until-empty eng 30))
          (is (empty? (st/calls p "place")) "no bed on a container floor")
          (is (= 1 (count (st/entries eng :bed-place-failed)))))))))

(deftest sleep-holds-an-occupied-bed-until-its-wall-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (st/setup {:time night :blocks {"6,64,0" "red_bed"} :states {"6,64,0" {:occupied true}}})
              id (core/submit! eng (tu/order '(jobs.survival.sleep {:bed {:x 6 :y 64 :z 0}})) {})]
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
           :view (fn [] {:now 0 :data mem/empty-data}) :engine {:warned (atom #{})} :worth (constantly tu/order-worth)}]
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
        (let [c (assoc (sense-t/stub-ctx (tu/fake {:self {:pos {:x 0 :y 64 :z 0}}})) :worth (constantly 900))
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
          (is (= 900 (await (delta ##Inf [{} nil]))) "no way priced at all: the night's own worth bounds the looks")
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

(deftest a-bed-put-down-in-one-round-is-slept-in-the-next-though-none-is-carried-then
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [c (assoc (sense-t/stub-ctx (tu/seeing-all (tu/fake {:self {:pos {:x 0 :y 64 :z 0}} :time night :blocks {"2,64,0" "red_bed"}}))) :args {:answer :bed})
              foot {:x 2 :y 64 :z 0}
              slept (atom [])]
          (with-redefs [bed-down/put-down-bed! (fn ^:async pd [_] foot)
                        bed-down/bed-answer (fn ^:async o [_ _] :bed)
                        bed-down/sleep! (fn ^:async s [_ bed _] (swap! slept conj bed) :continue)
                        sh-beds/place-wanted? (fn pw ([_] (empty? @slept)) ([_ _] (empty? @slept)))]
            (await (bed-down/round c))
            (await (bed-down/round c)))
          (is (= [foot foot] @slept) "the second round sleeps in the bed the first put down"))))))

(deftest a-priced-site-that-the-home-cannot-undercut-leaves-the-home-unquoted
  ;; Q147: the first good-enough site stands; the home's own quote is asked only when its straight walk is under the site's price
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ground (into-array (for [x (range -3 -5 -1) z (range -2 3)] #js {:name "dirt" :pos #js {:x x :y 63 :z z}}))
              asked (atom [])
              quote! pick/quote!
              out (atom nil)
              {:keys [eng p]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))
              probe (tu/job (fn ^:async probe-round [c]
                              (reset! out (await (flee/flee-target (assoc c ::pick/pass (atom {})) false)))
                              (ctx/end c :done nil)))
              eng (assoc eng :jobs (assoc registry/jobs 'flee-probe probe))]
          (aset p "seenBlocks" (fn [_] ground))
          (set! pick/quote! (fn [_ a] (when (:floor a) (swap! asked conj a)) (js/Promise.resolve {:uses {:job-time [0 0.5 0]}})))
          (try
            (core/submit! eng (tu/order '(flee-probe)) {})
            (await (tu/run-until-empty eng 20))
            (finally (set! pick/quote! quote!)))
          (is (= :ground (:kind @out)) "the ground site is the answer")
          (is (seq @asked) "the site was quoted")
          (is (not-any? :place @asked) "the home's straight walk costs more than the site's price: not quoted"))))))

(def walled-room
  "A stone box (x 10..17, z 10..14, y 64..67) round a room with no way in (x 12..15, z 11..13, y 64..65): the home."
  (merge (into {} (for [x (range 10 18) y (range 64 68) z (range 10 15)] [(str x "," y "," z) "stone"]))
         (into {} (for [x (range 12 16) y [64 65] z (range 11 14)] [(str x "," y "," z) "air"]))))

(deftest a-slow-flee-listing-and-a-field-of-failing-sites-let-the-night-flee-in-tick-slices-while-memory-is-rewritten
  ;; cards cee023a9, 72b694f6: with a live clock the listing took 150-270 ms in one turn, and a write near the sites each
  ;; pass (any cell of their memory sections) lapsed every rejection and listed them all again: the same nearest sites
  ;; were tested every pass and the flee search never answered. Here the clock moves only by the costs below, and
  ;; memory near the sites (cells no site test reads) is written before every pass.
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ms (atom 1000)
              stretch (atom 0) ; ms on the clock since the event loop last ran
              longest (atom 0)
              cost! (fn [d] (swap! ms + d) (swap! longest max (swap! stretch + d)))
              now clock/now
              yield! wplan/yield!
              sites flee/sites
              looks flee/site-looks-open?
              ground-ok? flee/ground-ok?
              flee-target flee/flee-target
              pass night/pass
              passes (atom 0)
              answers (atom [])
              memories (atom [])
              {:keys [eng p]} (st/setup {:time night :inventory [{:name "stone_pickaxe" :count 1}] :blocks (assoc walled-room "0,100,1" "cave_air")
                                         :floor [-2 8 20 16] :self {:pos {:x 4.5 :y 64 :z 12.5}}})
              raw (.-rawWorld p)
              per (perception/create raw {:now (constantly 1000000)}) ; its memory only: the body sees all (seeing-all)
              ^js st (:st per)
              air-ids [(.stateAt raw 0 100 0) (.stateAt raw 0 100 1)]
              rewrite! (fn [n] (doseq [cx [-1 0 1] cz [0 1]]
                                 (store/record! st (store/store-of st (.-dim st)) (+ 8 (* 16 cx)) 78 (+ 8 (* 16 cz)) (nth air-ids (mod n 2)) 1000000)))]
          (tu/seeing-all p)
          (aset p "perception" per)
          (set! (.-mobSource st) p)
          (know-home! eng {:x 12 :y 64 :z 12})
          (set! clock/now #(deref ms))
          (set! wplan/yield! (fn [] (reset! stretch 0) (yield!)))
          (set! flee/sites (fn [p r] (cost! 20) (sites p r)))
          (set! flee/site-looks-open? (fn [p x] (cost! 0.5) (looks p x)))
          (set! flee/ground-ok? (fn [c x] (cost! 3) (ground-ok? c x))) ; no pit fits a one-block floor: every ground site fails
          (set! flee/flee-target (fn ^:async f [c survey?] (let [r (await (flee-target c survey?))] (swap! answers conj [@passes r]) r)))
          (set! night/pass (fn ^:async g [c]
                             (rewrite! @passes)
                             (swap! memories conj (sense/site-memory p {:x 4 :y 64 :z 9}))
                             (reset! stretch 0)
                             (let [r (await (pass c))] (swap! passes inc) r)))
          (try
            (core/submit! eng (tu/order '(jobs.survival.night {:worth 1200})) {})
            (await (tu/run-until-empty eng 120))
            (finally (set! clock/now now)
                     (set! wplan/yield! yield!)
                     (set! flee/sites sites)
                     (set! flee/site-looks-open? looks)
                     (set! flee/ground-ok? ground-ok?)
                     (set! flee/flee-target flee-target)
                     (set! night/pass pass)))
          (is (apply distinct? (take 4 @memories)) "memory near the sites was rewritten every pass")
          (is (some true? (map (fn [[n r]] (and (< n 20) (not= :searching r))) @answers)) "the flee search answers (the home) within 20 passes")
          (is (<= @longest (+ (settings/ticks->ms 1) 6)) "no stretch between event-loop turns over a tick and two site tests"))))))
