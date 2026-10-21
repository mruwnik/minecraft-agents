(ns engine.obtain-contract-test
  "jobs.items.obtain and jobs.items.get-tool as contract jobs: the exports, done only with the item carried, facts
  condition-failed, failed tries nothing-possible, old callers' :how/:minutes/:chain/:depth as call keys."
  (:require [cljs.test :refer [are deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.fetch-test :as f]
            [engine.library-test :as lt]
            [engine.obtain-iron-test :as oi]
            [engine.obtain-obsidian-test :as oo]
            [engine.registry :as registry]
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

(deftest planks-crafted-from-a-carried-log-are-done-with-what-was-got
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (f/bare [{:name "oak_log" :count 2}]) [])
              e (await (ending-of s 'jobs.items.obtain {:item "oak_planks" :count 4 :explore false} 30))]
          (is (= :done (:kind e)))
          (is (= {:got 4 :item "oak_planks"} (select-keys (:data e) [:got :item]))))))))

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
              e (await (ending-of s 'jobs.items.obtain {:item "water_bucket"} 20))]
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

(deftest water-seen-but-no-bucket-carried-is-no-source
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (f/start (update (f/bare []) :blocks merge {"4,63,0" "water"}) [f/own-zone])
              e (await (ending-of s 'jobs.items.obtain {:item "water_bucket" :explore false} 20))]
          (is (= {:kind :condition-failed :cause :no-source} (kc e))))))))

(deftest sugar-cane-is-cut-from-a-seen-stand-by-get-seeds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stand (into {} (for [y [64 65 66]] [(str "3," y ",0") "sugar_cane"]))
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
