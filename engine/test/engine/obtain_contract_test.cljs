(ns engine.obtain-contract-test
  "jobs.items.obtain and jobs.items.get-tool as contract jobs: the exports, done only with the item carried, facts
  condition-failed, failed tries nothing-possible, old callers' :how/:minutes/:chain/:depth as call keys."
  (:require [cljs.test :refer [are deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.fetch-test :as f]
            [engine.library-test :as lt]
            [engine.obtain-iron-test :as oi]
            [engine.obtain-obsidian-test :as oo]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [jobs.lib.access :as access]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.stock :as stock]
            [jobs.items.obtain :as obtain]
            [jobs.items.obtain.table :as table]
            [jobs.lib.look :as look]
            [engine.test-util :as tu]))

(defn export [sym k] (get-in registry/jobs [sym k] (constantly nil)))

(defn kc [e] {:kind (:kind e) :cause (contract/cause-key e)})

(defn ^:async quote-in
  "What job quotes for args, asked by a parent's round."
  [{:keys [eng]} job args]
  (let [out (atom nil)
        parent {:round (fn ^:async quoting-round [c] (reset! out (await (ctx/quote c job args))) (ctx/end c :done nil))}
        eng (assoc-in eng [:jobs 'quoting-parent] parent)]
    (core/submit! eng '(quoting-parent) {})
    (await (tu/run-until-empty eng 5))
    @out))

(deftest both-jobs-export-the-contract-and-quote-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[sym ways] [[(quote jobs.items.obtain) [:chest :craft :smelt :loose :gather :hunt :crops :search]] ['jobs.items.get-tool [:chest :craft :gather]]]
                :let [job (get registry/jobs sym)]]
          (is (fn? (:can-start? job)) (str sym " has can-start?"))
          (is (nil? (:check job)) (str sym " has no old check"))
          (is (vector? (:target job)) (str sym " has a target"))
          (is (= ways (:ways job))))
        (let [q (await (quote-in (f/start (f/bare [{:name "oak_log" :count 2}]) []) 'jobs.items.obtain {:item "oak_planks" :count 4}))]
          (is (nil? (contract/quote-problem q)))
          (is (= [0 4 4] (get-in q [:yields :items "oak_planks"])))
          (is (< (get-in q [:uses :job-time 2]) ##Inf) "a craft from what is carried has a most"))
        (let [q (await (quote-in (f/start (f/bare []) []) 'jobs.items.obtain {:item "oak_planks" :count 4}))]
          (is (nil? (contract/quote-problem q)))
          (is (= ##Inf (get-in q [:uses :job-time 2])) "a gather has no most"))))))

(deftest the-default-time-limit-grows-with-the-count
  (are [n s] (= s (obtain/default-time {:count n}))
    nil 600
    1 600
    27 2160)
  (is (= 5400 (obtain/default-time {:bill {"jungle_log" 36 "stone_sword" 1}}))
      "a bill runs its items' children, each with ::bill-item-s, so its own limit is their sum"))

(deftest planks-crafted-from-a-carried-log-are-done-with-what-was-got
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (f/bare [{:name "oak_log" :count 2}]) [])
              e (await (ending-of s 'jobs.items.obtain {:item "oak_planks" :count 4 :explore false} 30))]
          (is (= :done (:kind e)))
          (is (= {:got 4 :item "oak_planks"} (select-keys (:data e) [:got :item]))))))))

(deftest a-craft-cut-by-its-time-limit-after-it-made-some-is-run-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig ctx/call
              s (f/start (f/bare [{:name "oak_log" :count 2}]) [])
              runs (atom 0)]
          (set! ctx/call (fn [c slot j args]
                           (if (= 'jobs.items.craft j)
                             (do (fake/add-item! (:p s) "oak_planks" 4)
                                 (contract/ending (if (= 1 (swap! runs inc)) :limit-reached :done)
                                                  (if (= 1 @runs) {:line :job-time} nil)
                                                  {:made 4}))
                             (orig c slot j args))))
          (let [e (try (await (ending-of s 'jobs.items.obtain {:item "oak_planks" :count 8 :explore false} 30))
                       (finally (set! ctx/call orig)))]
            (is (= :done (:kind e)))
            (is (= 2 @runs))))))))

(deftest nothing-to-get-it-from-is-condition-failed-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (f/bare []) [])
              e (await (ending-of s 'jobs.items.obtain {:item "wooden_pickaxe" :explore false} 10))]
          (is (= {:kind :condition-failed :cause :no-source} (kc e)))
          (is (re-find #"nothing seen to gather" (get-in e [:cause :why]))))
        (let [s (f/start (f/bare [{:name "oak_log" :count 2}]) [])
              e (await (ending-of s 'jobs.items.obtain {:item "oak_planks" :count 4 :ways #{:chest :gather} :explore false} 10))]
          (is (= {:kind :condition-failed :cause :no-source} (kc e)) "a craft that is not an allowed way is no source"))
        (let [s (f/start (oi/ore-world {"1,64,2" "furnace"} [{:name "raw_iron" :count 1} {:name "coal" :count 1}]) [f/own-zone])
              e (await (ending-of s 'jobs.items.obtain {:item "iron_ingot" :ways #{:craft :gather} :explore false} 10))]
          (is (= {:kind :condition-failed :cause :no-source} (kc e)) "a chain with a smelt step needs the smelt way"))))))

(deftest a-gather-that-ended-with-stone-in-sight-does-not-say-nothing-is-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig ctx/call
              s (f/start (-> (oi/ore-world {"1,64,2" "stone" "2,64,2" "stone"} [{:name "oak_log" :count 3} {:name "stick" :count 2}])
                             (assoc :recipes {"stone_pickaxe" {:count 1 :needs {"cobblestone" 3 "stick" 2} :table true}})) [])]
          (set! ctx/call (fn [c slot j args]
                           (if (= 'jobs.gather.mine j)
                             (contract/ending :nothing-possible :outmatched {:why "a fight costs too much"})
                             (orig c slot j args))))
          (let [e (try (await (ending-of s 'jobs.items.obtain {:item "stone_pickaxe" :ways #{:craft :gather} :explore false} 30))
                       (finally (set! ctx/call orig)))
                why (get-in e [:cause :why])]
            (is (= {:kind :nothing-possible :cause :no-source} (kc e)))
            (is (re-find #"seen and may be taken" why))
            (is (not (re-find #"nothing seen to gather" why)))))))))

(deftest bad-args-are-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (f/bare []) [])
              e (await (ending-of s 'jobs.items.obtain {:any-of []} 5))]
          (is (= {:kind :impossible :cause :bad-args} (kc e))))))))

(deftest get-tool-carried-is-done-with-the-tool-and-none-to-be-had-is-the-obtains-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (f/bare [{:name "stone_pickaxe" :count 1}]) [])
              e (await (ending-of s 'jobs.items.get-tool {:kind "pickaxe"} 5))]
          (is (= :done (:kind e)) "carried: done at once (done?), no round to name the tool"))
        (let [s (f/start (f/bare [{:name "oak_log" :count 3}]) [])
              e (await (ending-of s 'jobs.items.get-tool {:kind "pickaxe"} 60))]
          (is (= :done (:kind e)))
          (is (= "wooden_pickaxe" (get-in e [:data :tool]))))
        (let [s (f/start (f/bare [{:name "dirt" :count 5}]) [])
              e (await (ending-of s 'jobs.items.get-tool {:kind "pickaxe" :explore false} 10))]
          (is (= {:kind :condition-failed :cause :no-source} (kc e))))
        (let [s (f/start (f/bare []) [])
              e (await (ending-of s 'jobs.items.get-tool {:block "not_a_block"} 5))]
          (is (= {:kind :impossible :cause :bad-args} (kc e))))))))

(deftest a-gather-that-brings-in-nothing-is-nothing-possible-with-what-was-tried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (assoc (update (f/bare []) :blocks merge f/tree-blocks) :unreachable (vec (keys f/tree-blocks))) [])
              e (await (ending-of s 'jobs.items.obtain {:item "wooden_pickaxe" :explore false} 200))]
          (is (= {:kind :nothing-possible :cause :no-source} (kc e)))
          (is (= 0 (get-in e [:data :got])))
          (is (get-in e [:data :tried :gather])))))))

(deftest a-gather-whose-walk-fails-ends-with-the-walks-cause-not-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [cause [:zone-exit :not-back]]
          (let [runs (atom 0)
                mine (assoc (get registry/jobs 'jobs.gather.mine) :round (fn ^:async stuck [c] (swap! runs inc) (ctx/end c :nothing-possible cause {:got 0})))
                out (atom nil)
                parent {:round (fn ^:async recording-round [c]
                                 (let [e (await (ctx/call c :kid 'jobs.items.obtain {:item "raw_iron" :explore false}))]
                                   (if (= :continue e) :continue (do (reset! out e) (ctx/end c :done nil)))))}
                s (with-redefs [registry/jobs (assoc registry/jobs 'jobs.gather.mine mine 'recording-parent parent)]
                    (f/start (oi/ore-world {"1,64,4" "iron_ore"} [{:name "stone_pickaxe" :count 1}]) []))]
            (core/submit! (:eng s) '(recording-parent) {})
            (await (f/run-ticks s 200))
            (is (pos? @runs) "the mine ran")
            (is (= {:kind :nothing-possible :cause cause} (kc @out)) (str cause))
            (is (re-find (re-pattern (name cause)) (str (get-in @out [:cause :why]))))))))))

(deftest done-carries-the-got-and-tried-for-a-limit-ending
  (let [done? (export 'jobs.items.obtain :done?)
        c (fn [m inventory] {:primitives (tu/fake {:inventory inventory}) :root "j1" :slots [] :args {:item "oak_log" :count 4}
                             :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data m}]}}})})
        d (done? (c {:start {:have 1 :target 4} :tried [:craft]} [{:name "oak_log" :count 3}]))]
    (is (= [:later {:done 2 :left 1 :result {:got 2 :tried [:craft] :item "oak_log"}}]
           [(first d) (select-keys (nth d 2) [:done :left :result])]))
    (is (= :later (first (done? (c {} [])))) "before the start, nothing carried")
    (is (= [:ok {:done 0 :left 0}]
           (let [d (done? (c {} [{:name "oak_log" :count 4}]))] [(first d) (select-keys (nth d 2) [:done :left])]))
        "the count carried before the start is done")))

(deftest a-water-bucket-with-no-water-seen-is-no-source-that-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (f/bare [{:name "bucket" :count 1}]) [])
              e (await (ending-of s 'jobs.items.obtain {:item "water_bucket" :explore false} 20))]
          (is (= {:kind :condition-failed :cause :no-source} (kc e)))
          (is (re-find #"water" (get-in e [:cause :why]))))))))

(deftest an-empty-bucket-is-filled-at-seen-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (update (f/bare [{:name "bucket" :count 1}]) :blocks merge {"4,63,0" "water"}) [f/own-zone])
              e (await (ending-of s 'jobs.items.obtain {:item "water_bucket" :explore false} 60))]
          (is (= :done (:kind e)))
          (is (= {:got 1 :item "water_bucket"} (select-keys (:data e) [:got :item]))))))))

(deftest water-seen-but-no-bucket-or-iron-is-no-source-that-names-the-bucket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (update (f/bare []) :blocks merge {"4,63,0" "water"}) [f/own-zone])
              e (await (ending-of s 'jobs.items.obtain {:item "water_bucket" :explore false} 20))]
          (is (= {:kind :condition-failed :cause :no-source} (kc e)))
          (is (re-find #"bucket" (get-in e [:cause :why])))
          (is (not (re-find #"no recipe makes" (get-in e [:cause :why])))))))))

(deftest a-bucket-is-crafted-from-iron-then-filled-at-seen-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (-> (f/bare [{:name "iron_ingot" :count 3} {:name "crafting_table" :count 1}]) (update :blocks merge {"4,63,0" "water"})
                          (assoc :recipes {"bucket" {:count 1 :needs {"iron_ingot" 3} :table true}})) [f/own-zone])
              e (await (ending-of s 'jobs.items.obtain {:item "water_bucket" :explore false} 120))]
          (is (= :done (:kind e)))
          (is (= {:got 1 :item "water_bucket"} (select-keys (:data e) [:got :item]))))))))

(deftest a-bucket-short-one-ingot-smelts-the-carried-raw-iron
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (-> (oi/ore-world {"1,64,2" "furnace"} [{:name "iron_ingot" :count 2} {:name "raw_iron" :count 3} {:name "coal" :count 2} {:name "crafting_table" :count 1}])
                             (assoc :recipes {"bucket" {:count 1 :needs {"iron_ingot" 3} :table true}})) [f/own-zone])]
          (await (oi/obtain! s {:item "bucket" :explore false} 20))
          (is (seq (f/events-of s :smelt.loaded)) "the carried raw iron went in the furnace")
          (.advance (.-world (:p s)) 2000)
          (await (f/run-ticks s 200))
          (is (= 1 (get (f/inv s) "bucket")) "the ingot came out and the bucket was crafted"))))))

(deftest no-water-seen-with-the-ingots-carried-says-the-water-is-missing-not-the-chain
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (-> (f/bare [{:name "iron_ingot" :count 5} {:name "crafting_table" :count 1}])
                             (assoc :recipes {"bucket" {:count 1 :needs {"iron_ingot" 3} :table true}})) [f/own-zone])
              e (await (ending-of s 'jobs.items.obtain {:item "water_bucket" :explore false} 20))
              why (get-in e [:cause :why])]
          (is (= {:kind :condition-failed :cause :no-source} (kc e)))
          (is (re-find #"no water source seen" why))
          (is (re-find #"bucket can be made" why))
          (is (not (re-find #"no chain works" why))))))))

(deftest sugar-cane-is-cut-from-a-seen-stand-by-get-seeds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stand (into {"3,63,0" "sand"} (for [y [64 65 66]] [(str "3," y ",0") "sugar_cane"]))
              s (f/start (update (f/bare []) :blocks merge stand) [f/own-zone])
              e (await (ending-of s 'jobs.items.obtain {:item "sugar_cane" :explore false} 60))]
          (is (= :done (:kind e)))
          (is (= {:got 1 :item "sugar_cane"} (select-keys (:data e) [:got :item]))))))))

(deftest sand-and-gravel-are-dug-by-hand-from-a-seen-bed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [item ["sand" "gravel"]]
          (let [bed (into {} (for [x [2 3] z [0 1]] [(str x ",64," z) item]))
                s (f/start (update (f/bare []) :blocks merge bed) [])
                e (await (ending-of s 'jobs.items.obtain {:item item :explore false} 80))]
            (is (= :done (:kind e)) item)))))))

(deftest clay-and-flint-are-dug-from-the-block-that-drops-them
  (is (= [["clay" "clay_ball"] ["gravel" "flint"]]
         (for [item ["clay_ball" "flint"]
               :let [need (first (table/gather-needs {item 1}))]]
           [(get-in need [:args :block]) (get-in need [:args :item])]))))

(def walled-in
  "Bedrock round the body (a hollow box, one glass window) and a stone mass beyond: stone is seen but cannot be got to."
  (merge (into {} (for [x (range -2 3) y (range 62 68) z (range -2 3)
                        :when (or (= 2 (js/Math.abs x)) (= 2 (js/Math.abs z)) (= 62 y) (= 67 y))]
                    [(str x "," y "," z) "bedrock"]))
         (into {} (for [x (range -4 5) y (range 62 68) z (range 3 9)] [(str x "," y "," z) "stone"]))
         {"0,65,2" "glass"}))

(deftest seen-stone-that-cannot-be-reached-is-not-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (-> (f/bare [{:name "wooden_pickaxe" :count 1}]) (update :blocks merge walled-in)) [])
              _ (is (seq (look/seen-blocks (:p s) {:names ["stone"] :radius 16 :max 1})) "the stone is seen through the window")
              e (await (ending-of s 'jobs.items.obtain {:item "cobblestone" :count 4 :explore false} 120))]
          (is (= {:kind :nothing-possible :cause :unreachable} (kc e)))
          (is (re-find #"could not get to a source of" (get-in e [:cause :why]))))))))

(deftest a-craft-from-carried-materials-beats-a-walk-to-a-chest-of-unknown-contents
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (update (f/bare [{:name "oak_planks" :count 2}]) :blocks assoc "14,64,0" "chest") [(assoc f/own-zone :min [-20 60 -20] :max [20 70 20])])
              e (await (ending-of s 'jobs.items.obtain {:item "stick" :count 4 :explore false} 30))]
          (is (= :done (:kind e)))
          (is (= 4 (get-in e [:data :got])))
          (is (empty? (f/events-of s :obtain.chest)) "no walk to the chest"))))))

(deftest a-craft-from-carried-materials-beats-a-walk-to-a-far-chest-known-to-hold-the-item
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far-at "400,64,0"
              s (f/start (update (f/bare [{:name "oak_planks" :count 2}]) :blocks assoc far-at "chest") [(assoc f/own-zone :min [-20 60 -20] :max [500 70 20])])
              _ (mem/write! (:store (:eng s)) stock/stock-kind {:pos [400 64 0] :items {"stick" 8}} stock/stock-policy)
              e (await (ending-of s 'jobs.items.obtain {:item "stick" :count 4 :explore false} 30))]
          (is (= :done (:kind e)))
          (is (empty? (f/events-of s :obtain.chest)) "no walk to the far chest"))))))

(deftest a-chest-in-another-players-zone-is-not-looked-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner looks?] [["Miles" false] ["Fake" true]]]
          (let [s (f/start (update (f/bare []) :blocks assoc "4,64,0" "chest")
                           [(assoc f/own-zone :min [-20 60 -20] :max [-10 70 -10]) {:name "hut" :min [2 60 -2] :max [8 70 2] :owner owner}])
                _ (await (ending-of s 'jobs.items.obtain {:any-of ["wheat"] :count 1 :explore false :ways #{:chest}} 30))]
            (is (= looks? (boolean (seq (f/events-of s :obtain.chest)))) owner)))))))

(deftest a-bucket-is-filled-only-at-a-water-source-not-at-flowing-water
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [water (fn [level] {:blocks (assoc oo/ground "4,63,0" "water") :states {"4,63,0" {:level level}}
                                 :inventory [{:name "bucket" :count 1}] :self {:pos [0.5 64 0.5]}})
              run (fn ^:async run-with [level]
                    (let [{:keys [eng]} (lt/setup (water level))]
                      (await (tu/child-outcome eng 'jobs.items.obtain {:item "water_bucket" :explore false} 100))))
              flowing (await (run "3"))
              source (await (run "0"))]
          (is (= :no-source (:reason flowing)) (pr-str flowing))
          (is (= :done (:status source)) (pr-str source)))))))

(deftest obtain-defaults-to-the-whole-count-so-a-limit-with-part-carried-is-not-done
  (is (= :all (get-in registry/jobs ['jobs.items.obtain :defaults :least]))))

(deftest a-decline-counts-the-refused-targets
  (are [fields text] (= text (access/targets-text fields))
    {} "every target is"
    {:refused 1 :of 1} "the target is"
    {:refused 3 :of 3} "every target is"
    {:refused 1 :of 4} "1 of 4 targets are"))

(defn ^:async torch-status
  "The start status of an obtain of 4 more torches worth `worth` to a parent, asked from the parent's round (jobs.lib.need/item-status)."
  [{:keys [eng]} worth]
  (let [out (atom nil)
        parent {:round (fn ^:async status-round [c]
                         (reset! out (need/item-status c "torch" 4 {:how #{:chest :craft :gather}} worth))
                         (ctx/end c :done nil))}
        eng (assoc-in eng [:jobs 'status-parent] parent)]
    (core/submit! eng '(status-parent) {})
    (await (tu/run-until-empty eng 5))
    @out))

(defn ^:async torch-chest-price
  "The price a torch obtain names when it is worth nothing, with a chest 20 blocks east booked as holding torches, at time of day t."
  [t]
  (let [s (f/start (-> (f/bare []) (assoc :time t) (update :blocks assoc "20,64,0" "chest")) [(assoc f/own-zone :max [30 70 6])])]
    (mem/write! (:store (:eng s)) stock/stock-kind {:pos [20 64 0] :items {"torch" 8}} stock/stock-policy)
    (:price (nth (await (torch-status s 0)) 2))))

(deftest a-worth-is-weighed-against-the-cheapest-ways-price-at-least
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [none (await (torch-status (f/start (f/bare []) []) 1000))]
          (is (= [:later :not-worth ##Inf] [(first none) (:reason (nth none 2)) (:price (nth none 2))]) "no way, however much it is worth"))
        (is (= :ok (first (await (torch-status (f/start (f/bare [{:name "coal" :count 1} {:name "stick" :count 1}]) []) 100))))
            "a craft from carried coal costs its steps")
        (let [day (await (torch-chest-price 6000))
              night (await (torch-chest-price 18000))]
          (is (= day (+ (cost/trip) (:cost (cost/walk-cost {:distance 40})))) "the straight walk there and back: a lower bound, the walk's danger is go-to's quote's")
          (is (= night day) "no danger of its own by the hour"))))))
