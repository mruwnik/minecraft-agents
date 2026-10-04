(ns engine.rail-line-test
  "jobs.build.rail-line: build a rail plan with the from-plan builder and prove the line, against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.build-from-plan-test :as b]
            [engine.core :as core]
            [engine.harvest-test :as h]
            [engine.takeover :as takeover]
            [jobs.build.rail-line :as rail-line]
            [engine.test-util :as tu]
            [plan.rail :as rail]
            [plan.shape :as shape]))

(def job 'jobs.build.rail-line)

(def from [0 64 0])
(def to [29 64 0])

(defn ground
  "Ground of block at height y under a 40 x 9 area around the line."
  [block y]
  (into {} (for [x (range -5 35) z (range -4 5)] [(h/cell-key x y z) block])))

(defn line-plan [opts & [status]]
  {:id "line" :status (or status :active) :parts (:parts (rail/layout from to opts))})

(defn kit
  "Exactly the items the layout counts, the fill as cobblestone, plus extra {item n}."
  [opts & [extra]]
  (let [{:keys [items fill]} (:materials (rail/layout from to opts))]
    (mapv (fn [[name n]] {:name name :count n}) (merge-with + items {"cobblestone" fill} extra))))

(defn spec
  "The fake world: ground at y 63 (the bed is natural ground) or y 62 (the bed is built), the body beside the line."
  [ground-block ground-y inventory]
  {:inventory inventory :blocks (ground ground-block ground-y) :self {:pos {:x 0 :y (inc ground-y) :z 3}}})

(defn ^:async build!
  "Run the job as a child over the world spec and plans: [result seen p]."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen]} (b/start world-spec plans (or zones []))
        result (await (h/child-outcome eng job (merge {:plan "line"} args) 400))]
    [result seen p]))

(defn props [p [x y z]] (js->clj (.-properties (.blockAt p #js {:x x :y y :z z})) :keywordize-keys true))

;; ---------------------------------------------------------------- building and proving

(deftest every-style-and-power-source-builds-a-line-that-passes-the-proof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[opts ground-block ground-y] [[{} "stone" 63]
                                              [{} "grass_block" 63]
                                              [{:power :lever} "stone" 63]
                                              [{:power :block} "stone" 62]
                                              [{:style :all-powered} "stone" 62]
                                              [{:style :all-powered :power :torch} "dirt" 63]]]
          (let [[result seen p] (await (build! (spec ground-block ground-y (kit opts)) {"line" (line-plan opts)} {}))
                label (pr-str opts ground-block)]
            (is (true? (:ok? result)) label)
            (is (= [] (:breaks result)) label)
            (is (= [] (get-in result [:built :wrong])) (str label ": a bed of other sturdy ground is no fault"))
            (is (= "powered_rail" (h/block-at p 3 64 0)) label)
            (is (true? (:powered (props p [3 64 0]))) label)
            (is (= "east_west" (:shape (props p [0 64 0]))) label)
            (is (= 1 (count (h/events-of seen :rail-build.done))) label)
            (is (empty? (h/events-of seen :rail-build.broken)) label)))))))

(deftest a-lever-is-switched-on-after-the-build
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ _ p] (await (build! (spec "stone" 63 (kit {:power :lever})) {"line" (line-plan {:power :lever})} {}))]
          (is (= "lever" (h/block-at p 3 64 -1)))
          (is (true? (:powered (props p [3 64 -1])))))))))

;; ---------------------------------------------------------------- the check

(defn ^:async declines
  "Submit the job as a top-level job, tick a few times: the places made and the events of each kind named."
  [world-spec plans args & [zones]]
  (let [{:keys [eng p seen]} (b/start world-spec plans (if (= :none zones) nil (or zones [])))]
    (core/submit! eng (list job args) {})
    (dotimes [_ 4] (swap! h/clock + 700) (await (core/tick! eng)))
    {:places (count (h/calls p "place"))
     :declined (mapv #(select-keys % [:plan :reason :why :refused :cells]) (h/events-of seen :rail-build.declined))
     :texts (mapv :text (h/events-of seen :rail-build.declined))
     :short (mapv :short (h/events-of seen :rail-build.short))
     :warns (count (filter #(re-find #"^(rail-)?build\." (name (:kind %))) @seen))}))

(def holed-plan
  {:id "line" :status :active
   :parts [{:id "bed" :box [[-1 63 0] [5 63 0]] :want [:any "stone"]}
           {:id "rails" :cells [[0 64 0] [1 64 0] [3 64 0] [4 64 0]] :want {:block "rail" :shape :east_west}}]})

(deftest a-plan-it-cannot-build-declines-with-one-warn-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[plans args zones declined]
                [[{} {:plan "line"} [] [{:plan "line" :reason :plan}]]
                 [{"line" (line-plan {} :proposed)} {:plan "line"} [] [{:plan "line" :reason :plan}]]
                 [{"line" holed-plan} {:plan "line"} [] [{:plan "line" :reason :not-a-line :why :gap}]]
                 [{"line" (line-plan {})} {:plan "line"} :none [{:plan "line" :reason :no-zones}]]
                 [{"line" (line-plan {})} {:plan "line"} [(b/zone "shrine" [7 64 0] [7 64 0] #{})]
                  [{:plan "line" :reason :refused :refused [{:pos [7 64 0] :reason :zone :zone "shrine"}]}]]]]
          (let [r (await (declines (spec "stone" 63 (kit {})) plans args zones))]
            (is (= declined (:declined r)) (pr-str declined))
            (is (= 1 (:warns r)) (pr-str declined))
            (is (= 0 (:places r)) (pr-str declined))))))))

(deftest short-of-powered-rails-declines-naming-the-shortage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (declines (spec "stone" 63 (kit {} {"powered_rail" -2})) {"line" (line-plan {})} {:plan "line"}))]
          (is (= [{"powered_rail" 2}] (:short r)))
          (is (= 1 (:warns r)))
          (is (= 0 (:places r))))))))

(defn built-world
  "The fake world with every cell of the plan already as it wants (names only: the fake works the rail states out)."
  [plan ground-block]
  (merge (ground ground-block 63)
         (into {} (keep (fn [{:keys [pos want]}]
                          (when-not (= :clear want) [(apply h/cell-key pos) (shape/want-block want)])))
               (:cells (shape/expand plan {})))))

(deftest a-sound-line-declines-without-a-word
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (declines {:inventory (kit {}) :blocks (built-world (line-plan {}) "stone")
                                  :self {:pos {:x 0 :y 64 :z 3}}}
                                 {"line" (line-plan {})} {:plan "line"}))]
          (is (= {:places 0 :declined [] :texts [] :short [] :warns 0} r)))))))

;; ---------------------------------------------------------------- redstone blocks on ground that is there

(deftest a-redstone-block-bed-on-natural-ground-declines-once-naming-the-cells-and-what-would-work
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts {:power :block}
              r (await (declines (spec "stone" 63 (kit opts)) {"line" (line-plan opts)} {:plan "line"}))]
          (is (= [{:plan "line" :reason :source-blocked :cells [[3 63 0] [25 63 0]]}] (:declined r)))
          (is (= 1 (:warns r)))
          (is (= 0 (:places r)))
          (is (every? #(re-find % (first (:texts r))) [#"\[3 63 0\]" #"\[25 63 0\]" #":torch" #":lever" #"raised"])))))))

(deftest a-redstone-block-bed-on-a-raised-line-is-not-declined
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [opts {:power :block}
              r (await (declines (spec "stone" 62 (kit opts)) {"line" (line-plan opts)} {:plan "line"}))]
          (is (= [] (:declined r)))
          (is (pos? (:places r))))))))

;; ---------------------------------------------------------------- build.wrong

(deftest sturdy-natural-ground-under-the-line-is-no-wrong-block-of-the-builder
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (ground "grass_block" 63) (h/cell-key 5 63 0) "farmland")
              [result seen] (await (build! {:inventory (kit {}) :blocks world :self {:pos {:x 0 :y 64 :z 3}}}
                                           {"line" (line-plan {})} {}))
              [wrong] (h/events-of seen :build.wrong)]
          (is (= [[5 63 0]] (mapv :pos (:cells wrong))) "only the bed cell that is not sturdy is wrong")
          (is (= 1 (count (h/events-of seen :build.wrong))))
          (is (= [[5 63 0]] (mapv :pos (get-in result [:built :wrong])))))))))

(deftest ground-that-is-sturdy-everywhere-gives-no-build-wrong-notice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[_ seen] (await (build! (spec "grass_block" 63 (kit {})) {"line" (line-plan {})} {}))]
          (is (empty? (h/events-of seen :build.wrong))))))))

;; ---------------------------------------------------------------- cells nobody has seen

(defn missing-cell [want item] {:answer :missing :want want :item item :found "air"})
(def unseen-cell (fn [want item] {:answer :unknown :want want :item item :found nil}))
(def fill [:any "cobblestone" "dirt"])

(deftest unseen-cells-are-owed-fill-only-up-to
  (let [cells [(missing-cell fill "cobblestone") (missing-cell fill "dirt")
               (unseen-cell fill "cobblestone") (unseen-cell fill "cobblestone")]]
    (is (= {"cobblestone | dirt" 1} (rail-line/short-of cells {"cobblestone" 1})) "seen cells are owed for sure")
    (is (= {"cobblestone | dirt" 3} (rail-line/up-to-of cells {"cobblestone" 1})) "with the unseen ones: up to")
    (is (= {} (rail-line/short-of cells {"cobblestone" 2})))
    (is (= {"cobblestone | dirt" 2} (rail-line/up-to-of cells {"cobblestone" 2})))
    (is (= {} (rail-line/up-to-of cells {"cobblestone" 4})))))

;; the two buffer blocks are seen empty and carried; the 11 bed cells at the far end are not seen
(deftest a-far-end-nobody-has-seen-does-not-hold-the-line-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (mapv #(str % ",63,0") (range 20 31))
              world {:inventory (kit {} {"cobblestone" (- 2 (:fill (:materials (rail/layout from to {}))))}) :blocks (ground "stone" 63) :unloaded far
                     :self {:pos {:x 0 :y 64 :z 3}}}
              r (await (declines world {"line" (line-plan {})} {:plan "line"}))]
          (is (= [] (:short r)))
          (is (pos? (:places r))))))))

;; ---------------------------------------------------------------- a line broken after the build

(defn ^:async broken-run
  "Build the line, break the rail at x 7, then submit the job with args: [seen p]."
  [args]
  (let [{:keys [eng p seen]} (b/start (spec "stone" 63 (kit {})) {"line" (line-plan {})} [])]
    (await (h/child-outcome eng job {:plan "line"} 400))
    (.delete (.-blocks (.-state (.-world p))) "7,64,0")
    (core/submit! eng (list job (merge {:plan "line"} args)) {})
    (dotimes [_ 6] (swap! h/clock + 700) (await (core/tick! eng)))
    [seen p]))

(deftest a-rail-broken-after-the-build-is-a-gap-in-the-next-proof
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen p] (await (broken-run {:all-carried false}))
              [warn] (h/events-of seen :rail-build.broken)]
          (is (= "air" (h/block-at p 7 64 0)))
          (is (= [{:pos [7 64 0] :why :gap}] (:breaks warn)))
          (is (= {"rail" 1} (:short warn)))
          (is (= 1 (count (h/events-of seen :rail-build.broken)))))))))

(deftest a-broken-line-without-the-rail-carried-declines-short-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen] (await (broken-run {}))]
          (is (= [{"rail" 1}] (mapv :short (h/events-of seen :rail-build.short))))
          (is (empty? (h/events-of seen :rail-build.broken))))))))

;; ---------------------------------------------------------------- resuming

(deftest a-cut-mid-build-resumes-and-finishes-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (b/start (spec "stone" 63 (kit {})) {"line" (line-plan {})} [])
              release (.hold (.-world p) "place")]
          (core/submit! eng (list job {:plan "line"}) {})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= {:ok true} (takeover/take! eng {:who "claude" :why "test"})))
            (await round)
            (release))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (dotimes [_ 120] (swap! h/clock + 700) (await (core/tick! eng)))
          (is (every? #(= "rail" (h/block-at p % 64 0)) [0 1 28 29]))
          (is (every? #(= "powered_rail" (h/block-at p % 64 0)) [2 3 4 5 24 25 26 27]))
          (is (= 1 (count (h/events-of seen :rail-build.done))))
          (is (empty? (:list (core/state eng)))))))))
