(ns engine.bake-test
  "jobs.items.bake against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.bake :as bake]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
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
(defn chest-items [p]
  (into {} (map (juxt :name :count))
        (get-in @(fake/state p) [:containers [10 64 0]])))

(def job 'jobs.items.bake)
(def chest {:x 10 :y 64 :z 0})
(def beside {"11,64,0" "crafting_table"})

(defn ^:async bake
  "Setup world, run the job with args; [result p seen]."
  ([world args] (bake world args identity))
  ([world args prepare]
   (let [{:keys [eng p seen]} (setup world)
         _ (prepare p)
         result (await (child-outcome eng job (merge {:chest chest :keep 4} args) 60))]
     [result p seen])))

(defn kinds [seen] (set (map :kind @seen)))

(deftest bake-check-wants-a-known-chest
  (are [args ok] (= ok ((:check (get registry/jobs job)) {:args args :view (fn [] {:data {} :now 0})}))
    {:chest chest} true
    {:chest nil} false))

(deftest bakes-the-chest-wheat-and-keeps-some-loaves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {}))]
          (is (= {:baked 10 :deposited 6} result))
          (is (= {"bread" 6} (chest-items p)) "no wheat left, 6 loaves")
          (is (= {"bread" 4} (inv p)))
          (is (contains? (kinds seen) :bake.done)))))))

(deftest only-whole-loaves-leave-the-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 31}]} :blocks beside} {}))]
          (is (= {:baked 10 :deposited 6} result))
          (is (= {"wheat" 1 "bread" 6} (chest-items p))))))))

(deftest no-table-near-the-chest-takes-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async
                [{} {"30,64,0" "crafting_table"}]
                (fn ^:async one [blocks]
                  (let [[result p seen] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks blocks} {}))]
                    (is (= {:baked 0 :deposited 0 :reason "no-table"} result) (str blocks))
                    (is (= {"wheat" 30} (chest-items p)))
                    (is (contains? (kinds seen) :bake.no-table))
                    (is (empty? (calls p "transfer")))))))))))

(deftest a-table-never-seen-is-not-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {} tu/blind))]
          (is (= {:baked 0 :deposited 0 :reason "no-table"} result))
          (is (empty? (calls p "transfer"))))))))

(deftest a-withdraw-that-gives-up-in-the-top-up-stops-the-bake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result _ seen] (await (bake {:inventory [{:name "wheat" :count 3}] :containers {"10,64,0" [{:name "bread" :count 8}]} :blocks beside} {}
                                           (fn [p] (.override (.-world p) "transfer" (fn [_ _ _] (js/Promise.resolve #js {:status "unreachable"}))))))]
          (is (re-find #"^withdraw " (:reason result)))
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
                    (is (= {:baked 0 :deposited 0} result) (str n))
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
          (is (= {:baked 10 :deposited 0 :reason "deposit full"} result))
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
          (is (= {:baked 64 :deposited 60} result))
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
          (is (= {:baked 5 :deposited 1} result))
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
          (is (string? (:reason result)))
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
          (is (= {:baked 10 :deposited 8} result))
          (is (= {"bread" 2} (inv p)))
          (is (= {"bread" 8} (chest-items p))))))))

(deftest no-room-for-the-wheat-and-the-bread-ends-inventory-full-taking-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [filler (mapv (fn [i] {:name (str "filler_" i) :count 1}) (range 35))
              [result p seen] (await (bake {:inventory filler :containers {"10,64,0" [{:name "wheat" :count 30}]} :blocks beside} {}))]
          (is (= {:baked 0 :deposited 0 :reason "inventory-full"} result))
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
          (is (string? (:reason result)))
          (is (contains? (kinds seen) :bake.withdraw-failed)))))))

(defn ^:async bake-with-craft
  "Like bake, but jobs.items.craft is the given definition; [result p seen]."
  [world craft-def]
  (let [{:keys [eng p seen]} (setup world)
        eng (assoc eng :jobs (assoc (:jobs eng) 'jobs.items.craft craft-def))]
    [(await (child-outcome eng job {:chest chest :keep 4} 60)) p seen]))

(deftest a-declined-craft-child-is-counted-and-ends-the-bake
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p seen] (await (bake-with-craft {:inventory [{:name "wheat" :count 6}]
                                                       :containers {"10,64,0" [{:name "wheat" :count 9}]} :blocks beside}
                                                      {:check (constantly false) :round (fn [_] :done)}))]
          (is (string? (:reason result)))
          (is (contains? (kinds seen) :bake.gave-up))
          (is (empty? (calls p "craft"))))))))

(deftest the-body-is-not-drawn-back-to-the-chest-between-the-table-and-the-craft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[result p] (await (bake {:inventory [{:name "wheat" :count 6}]
                                       :containers {"10,64,0" [{:name "wheat" :count 9}]} :blocks {"17,64,0" "crafting_table"}} {}))
              names (mapv #(.-name %) (.-calls (.-world p)))
              to-second-craft (vec (take 4 (filter #{"steer" "craft"} names)))]
          (is (= {:baked 5 :deposited 1} result))
          (is (= ["steer" "craft" "steer" "craft"] to-second-craft) "chest, craft (too far), table, craft: no walk back between"))))))
