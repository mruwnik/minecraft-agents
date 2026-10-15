(ns engine.bake-test
  "jobs.items.bake against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.withdraw-test :as wt]
            [jobs.items.bake :as bake]
            [jobs.lib.need :as need]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn chest-items [p]
  (into {} (map (juxt :name :count))
        (get-in @(fake/state p) [:containers [10 64 0]])))

(def job 'jobs.items.bake)
(def chest {:x 10 :y 64 :z 0})
(def beside {"11,64,0" "crafting_table"})

(defn ^:async child-outcome
  "The job's ending as a ctx/call child, at most n ticks: {:kind :cause :data}, the data's :baked and :deposited with the
  cause's keyword as :reason when it did not end done (the shape the old result had)."
  [eng job args n]
  (let [e (await (wt/ending-of eng job args n))]
    (merge (select-keys (:data e) [:baked :deposited :zones :claims])
           {:kind (:kind e)}
           (when (not= :done (:kind e)) {:reason (contract/cause-key e)}))))

(defn ^:async bake
  "Setup world, run the job with args; [result p seen]."
  ([world args] (bake world args identity))
  ([world args prepare]
   (let [{:keys [eng p seen]} (setup world)
         _ (prepare p)
         result (await (child-outcome eng job (merge {:chest chest :keep 4 :fetch false} args) 60))]
     [result p seen])))

(defn kinds [seen] (set (map :kind @seen)))

(deftest bake-needs-a-known-chest
  (let [conditions (:conditions (get registry/jobs job))
        problem (fn [args] ((second (first conditions)) {:args args :view (fn [] {:data {} :now 0})}))]
    (is (nil? (problem {:chest chest})))
    (is (= {:reason :no-chest} (problem {:chest nil})))))

(deftest bakes-the-chest-wheat-and-keeps-some-loaves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {}))]
          (is (= {:kind :done :baked 10 :deposited 6} result))
          (is (= {"bread" 6} (chest-items p)) "no wheat left, 6 loaves")
          (is (= {"bread" 4} (inv p)))
          (is (contains? (kinds seen) :bake.done)))))))

(deftest only-whole-loaves-leave-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 31}]} :blocks beside} {}))]
          (is (= {:kind :done :baked 10 :deposited 6} result))
          (is (= {"wheat" 1 "bread" 6} (chest-items p))))))))

(deftest no-table-near-the-chest-takes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async
                [{} {"30,64,0" "crafting_table"}]
                (fn ^:async one [blocks]
                  (let [[result p seen] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks blocks} {}))]
                    (is (= {:kind :condition-failed :baked 0 :deposited 0 :reason :no-table} result) (str blocks))
                    (is (= {"wheat" 30} (chest-items p)))
                    (is (contains? (kinds seen) :bake.no-table))
                    (is (empty? (calls p "transfer")))))))))))

(deftest a-table-never-seen-is-not-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {} tu/blind))]
          (is (= {:kind :condition-failed :baked 0 :deposited 0 :reason :no-table} result))
          (is (empty? (calls p "transfer"))))))))

(deftest a-withdraw-that-gives-up-in-the-top-up-stops-the-bake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ seen] (await (bake {:inventory [{:name "wheat" :count 3}] :containers {"10,64,0" [{:name "bread" :count 8}]} :blocks beside} {}
                                           (fn [p] (.override (.-world p) "transfer" (fn [_ _ _] (js/Promise.resolve #js {:status "unreachable"}))))))]
          (is (= :nothing-possible (:kind result)))
          (is (= :unreachable (:reason result)))
          (is (contains? (kinds seen) :bake.withdraw-failed))
          (is (not (contains? (kinds seen) :bake.done))))))))

(deftest less-than-a-loaf-of-wheat-is-nothing-to-do
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async
                [0 2]
                (fn ^:async one [n]
                  (let [[result p seen] (await (bake {:containers {"10,64,0" [{:name "wheat" :count n}]} :blocks beside} {}))]
                    (is (= {:kind :nothing-possible :baked 0 :deposited 0 :reason :no-wheat} result) (str n))
                    (is (empty? (calls p "craft")) (str n))
                    (is (contains? (kinds seen) :bake.nothing))))))))))

(deftest a-full-chest-ends-with-the_bread_still_carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {}
                                           (fn [p]
                                             (.override (.-world p) "transfer"
                                                        (fn ^:async f [_ args impl]
                                                          (if (= "deposit" (.-direction args))
                                                            #js {:status "full" :moved 0}
                                                            (impl _ args)))))))]
          (is (= {:kind :nothing-possible :baked 10 :deposited 0 :reason :full} result))
          (is (= {"bread" 10} (inv p)))
          (is (empty? (calls p "toss")))
          (is (contains? (kinds seen) :bake.deposit-failed)))))))

(deftest a-nearly-full-inventory-takes-wheat-in-bounded-trips
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "filler_" i) :count 1}) (range 33))
              [result p] (await (bake {:inventory filler :containers {"10,64,0" [{:name "wheat" :count 192}]} :blocks beside} {}))
              wheat-moves (filter #(= "wheat" (.-item (.-args %))) (calls p "transfer"))]
          (is (= {:kind :done :baked 64 :deposited 60} result))
          (is (= {"bread" 60} (chest-items p)))
          (is (every? #(<= (.-count (.-args %)) 64) wheat-moves))
          (is (> (count wheat-moves) 1)))))))

(defn wheat-target-with
  "bake/wheat-target for filler single-item stacks, a carried wheat stack of carried and a bread stack of bread (each when
  positive) and chest-wheat in the chest."
  [filler carried bread chest-wheat]
  (let [stacks (concat (map (fn [i] {:name (str "filler_" i) :count 1}) (range filler))
                       (when (pos? carried) [{:name "wheat" :count carried}])
                       (when (pos? bread) [{:name "bread" :count bread}]))]
    (bake/wheat-target (tu/fake {:inventory (vec stacks)}) chest-wheat)))

(deftest wheat-target-keeps-slots-free-for-the-bread
  (are [filler carried bread chest-wheat target] (= target (wheat-target-with filler carried bread chest-wheat))
    32 0 0 192 128
    33 0 0 192 64
    34 0 0 192 0
    35 0 0 192 0
    33 60 0 192 64
    34 60 0 192 0
    32 0 5 192 128
    33 0 5 192 64
    34 0 5 192 0
    35 0 5 192 0))

(deftest carried-wheat-is-crafted-before-the-chest-is-touched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:inventory [{:name "wheat" :count 6}]
                                       :containers {"10,64,0" [{:name "wheat" :count 9}]} :blocks beside} {}))
              order (mapv #(.-name %) (filter #(contains? #{"craft" "transfer"} (.-name %)) (.-calls (.-world p))))]
          (is (= {:kind :done :baked 5 :deposited 1} result))
          (is (= "craft" (first order)))
          (is (= {"bread" 4} (inv p))))))))

(deftest a-chest-that-goes-missing-ends_with_a_reason_and_tosses_nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {}
                                      (fn [p]
                                        (.override (.-world p) "transfer"
                                                   (fn ^:async f [_ args impl]
                                                     (let [r (await (impl _ args))]
                                                       (swap! (fake/state p) update :containers dissoc [10 64 0])
                                                       r))))))]
          (is (keyword? (:reason result)))
          (is (empty? (calls p "toss")))
          (is (= {"bread" 10} (inv p))))))))

(deftest a-craft-cut-short-between-a-deposit-still-counts-every-loaf
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [first? (atom true)
              [result p] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {:keep 2}
                                      (fn [p]
                                        (.override (.-world p) "craft"
                                                   (fn ^:async f [token args impl]
                                                     (if-not @first?
                                                       (impl token args)
                                                       (do (reset! first? false)
                                                           (await (impl token (clj->js (assoc (js->clj args) "count" 3))))
                                                           #js {:status "partial" :made 3 :reason "interrupted"})))))))]
          (is (= {:kind :done :baked 10 :deposited 8} result))
          (is (= {"bread" 2} (inv p)))
          (is (= {"bread" 8} (chest-items p))))))))

(deftest no-room-for-the-wheat-and-the-bread-ends-inventory-full-taking-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "filler_" i) :count 1}) (range 35))
              [result p seen] (await (bake {:inventory filler :containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {}))]
          (is (= {:kind :condition-failed :baked 0 :deposited 0 :reason :inventory-full} result))
          (is (= {"wheat" 30} (chest-items p)))
          (is (empty? (calls p "transfer")))
          (is (contains? (kinds seen) :bake.inventory-full)))))))

(deftest failures-in-a-row-restart-after-a-withdraw-that-took-wheat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [reads (atom 0)
              [result p seen] (await (bake {:inventory (vec (repeat 33 {:name "stick" :count 1}))
                                            :containers {"10,64,0" [{:name "wheat" :count 12}]} :blocks beside} {}
                                           (fn [p]
                                             (.override (.-world p) "inspectContainer"
                                                        (fn ^:async f [t a impl]
                                                          (if (contains? #{1 2 5 6} (swap! reads inc))
                                                            #js {:status "blocked"}
                                                            (await (impl t a))))))))]
          (is (nil? (:reason result)) "two blocked reads, a good one, two more: never three in a row")
          (is (not (contains? (kinds seen) :bake.gave-up))))))))

(deftest a-withdraw-that-reports-a-take-but-changes-nothing-stops-the-bake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 12}]} :blocks beside} {}
                                           (fn [p]
                                             (.override (.-world p) "transfer"
                                                        (fn ^:async f [_ _ _] #js {:status "ok" :moved 1})))))]
          (is (keyword? (:reason result)))
          (is (contains? (kinds seen) :bake.withdraw-failed)))))))

(defn ^:async bake-with-craft
  "Like bake, but jobs.items.craft is the given definition; [result p seen]."
  [world craft-def]
  (let [{:keys [eng p seen]} (setup world)
        eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.items.craft craft-def))]
    [(await (child-outcome eng job {:chest chest :keep 4} 60)) p seen]))

(deftest a-craft-child-whose-condition-fails-ends-the-bake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (bake-with-craft {:inventory [{:name "wheat" :count 6}]
                                                       :containers {"10,64,0" [{:name "wheat" :count 9}]} :blocks beside}
                                                      {:conditions [[:fn (fn [_] {:reason :nope})]] :args {} :round (fn [_] nil)}))]
          (is (keyword? (:reason result)))
          (is (= {:kind :condition-failed :reason :nope} (select-keys result [:kind :reason])))
          (is (contains? (kinds seen) :bake.craft-failed))
          (is (empty? (calls p "craft"))))))))

(deftest the-body-is-not-drawn-back-to-the-chest-between-the-table-and-the-craft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:inventory [{:name "wheat" :count 6}]
                                       :containers {"10,64,0" [{:name "wheat" :count 9}]} :blocks {"17,64,0" "crafting_table"}} {}))
              names (mapv #(.-name %) (.-calls (.-world p)))
              to-second-craft (vec (take 4 (filter #{"steer" "craft"} names)))]
          (is (= {:kind :done :baked 5 :deposited 1} result))
          (is (= ["steer" "craft" "steer" "craft"] to-second-craft) "chest, craft (too far), table, craft: no walk back between"))))))

(deftest a-bake-is-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside})
              result (await (child-outcome eng job {:chest chest :keep 4} 1))]
          (is (empty? (:list (core/state eng))))
          (is (= {"bread" 4} (inv p)))
          (is (= {:kind :done :baked 10 :deposited 6} result)))))))

(deftest bake-looks-around-before-it-says-there-is-no-table
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {} tu/seeing-after-look))]
          (is (seq (calls p "look")))
          (is (= {:kind :done :baked 10 :deposited 6} result)))))))

(defn short-craft
  "A craft stand-in that ends :need, making loaves in its first call only; calls counts them."
  [loaves calls]
  {:conditions [] :args {} :round (fn [c]
            (let [n (swap! calls inc)
                  made (if (= 1 n) loaves 0)]
              (ctx/end c :condition-failed {:reason :need :short {"wheat" 1}} {:made made :short {"wheat" 1}})))})

(def short-world {:inventory [{:name "wheat" :count 6}] :containers {"10,64,0" [{:name "wheat" :count 9}]} :blocks beside})

(deftest a-craft-short-after-some-bread-counts-as-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)]
          (await (bake-with-craft short-world (short-craft 2 n)))
          (is (> @n 1) "went on after the loaves"))))))

(deftest a-craft-short-with-no-bread-stops-the-bake-in-words
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              [result] (await (bake-with-craft short-world (short-craft 0 n)))]
          (is (= 1 @n))
          (is (= {:kind :condition-failed :reason :need} (select-keys result [:kind :reason]))))))))

(deftest bread-already-carried-or-in-the-chest-is-not-an-empty-bake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[carried] (await (bake {:inventory [{:name "bread" :count 4}] :containers {"10,64,0" []} :blocks beside} {}))
              [topped p] (await (bake {:containers {"10,64,0" [{:name "bread" :count 8}]} :blocks beside} {}))]
          (is (= {:kind :done :baked 0 :deposited 0} carried))
          (is (= {:kind :done :baked 0 :deposited 0} topped))
          (is (= {"bread" 4} (inv p))))))))

(deftest a-call-that-began-is-not-ended-by-a-forgotten-chest
  (let [problem (second (first (:conditions (get registry/jobs job))))
        c {:args {:chest nil} :root "j1" :slots [] :view (fn [] {:data {} :now 0})}]
    (is (= {:reason :no-chest} (problem c)))
    (is (nil? (problem (assoc c :quote-call {}))))))

(deftest a-table-broken-between-steps-is-noticed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [looks (atom 0)
              real bake/nearest-table
              [result] (with-redefs [bake/nearest-table (fn [p c r] (when (= 1 (swap! looks inc)) (real p c r)))]
                         (await (bake {:inventory [{:name "wheat" :count 3}] :containers {"10,64,0" []} :blocks beside} {})))]
          (is (= {:kind :condition-failed :baked 1 :deposited 0 :reason :no-table} result)))))))

(defn asking
  "A stand-in for jobs.items.obtain that records the item asked and ends done, bringing nothing."
  [asked]
  {:conditions [] :args {} :round (fn [c] (swap! asked conj (:item (:args c))) (ctx/end c :done nil))})

(deftest a-bake-with-no-table-fetches-one-unless-fetch-is-false
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[fetch expected] [[true ["crafting_table"]] [false []]]]
          (let [asked (atom [])
                {:keys [eng]} (setup {:inventory [{:name "wheat" :count 3}] :containers {"10,64,0" []}})
                eng (assoc-in eng [:jobs 'jobs.items.obtain] (asking asked))
                result (await (child-outcome eng job {:chest chest :keep 4 :fetch fetch} 60))]
            (is (= expected @asked) (str fetch))
            (is (= :no-table (:reason result)) "the stand-in brought no table, so the bake says so")))))))
