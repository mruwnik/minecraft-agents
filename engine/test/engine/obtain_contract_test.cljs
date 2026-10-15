(ns engine.obtain-contract-test
  "jobs.items.obtain and jobs.items.get-tool as contract jobs: the exports, done only with the item carried, facts
  condition-failed, failed tries nothing-possible, old callers' :how/:minutes/:chain/:depth as call keys."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.fetch-test :as f]
            [engine.obtain-iron-test :as oi]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn export [sym k] (get-in registry/jobs [sym k] (constantly nil)))

(defn kc [e] {:kind (:kind e) :cause (contract/cause-key e)})

(defn ^:async quote-in
  "What job quotes for args, asked by a parent's round."
  [{:keys [eng]} job args]
  (let [out (atom nil)
        parent {:check (constantly true)
                :round (fn ^:async quoting-round [c] (reset! out (await (ctx/quote c job args))) :done)}
        eng (assoc-in eng [:jobs 'quoting-parent] parent)]
    (core/submit! eng '(quoting-parent) {})
    (await (tu/run-until-empty eng 5))
    @out))

(deftest both-jobs-export-the-contract-and-quote-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[sym ways] [['jobs.items.obtain [:chest :craft :smelt :gather]] ['jobs.items.get-tool [:chest :craft :gather]]]
                :let [job (get registry/jobs sym)]]
          (is (vector? (:conditions job)) (str sym " has conditions"))
          (is (nil? (:check job)) (str sym " has no old check"))
          (is (vector? (:target job)) (str sym " has a target"))
          (is (= ways (:ways job))))
        (let [q (await (quote-in (f/start (f/bare [{:name "oak_log" :count 2}]) []) 'jobs.items.obtain {:item "oak_planks" :count 4}))]
          (is (nil? (contract/quote-problem q)))
          (is (= [0 4 4] (get-in q [:yields :items "oak_planks"])))
          (is (< (get-in q [:uses :time 2]) ##Inf) "a craft from what is carried has a most"))
        (let [q (await (quote-in (f/start (f/bare []) []) 'jobs.items.obtain {:item "oak_planks" :count 4}))]
          (is (nil? (contract/quote-problem q)))
          (is (= ##Inf (get-in q [:uses :time 2])) "a gather has no most"))))))

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
        (doseq [cause [:zone-exit :not-home]]
          (let [runs (atom 0)
                mine (assoc (get registry/jobs 'jobs.gather.mine) :round (fn ^:async stuck [c] (swap! runs inc) (ctx/end c :nothing-possible cause {:got 0})))
                out (atom nil)
                parent {:check (constantly true)
                        :round (fn ^:async recording-round [c]
                                 (let [e (await (ctx/call c :kid 'jobs.items.obtain {:item "raw_iron" :explore false}))]
                                   (if (= :continue e) :continue (do (reset! out e) :done))))}
                s (with-redefs [registry/jobs (assoc registry/jobs 'jobs.gather.mine mine 'recording-parent parent)]
                    (f/start (oi/ore-world {"1,64,4" "iron_ore"} [{:name "stone_pickaxe" :count 1}]) []))]
            (core/submit! (:eng s) '(recording-parent) {})
            (await (f/run-ticks s 200))
            (is (pos? @runs) "the mine ran")
            (is (= {:kind :nothing-possible :cause cause} (kc @out)) (str cause))
            (is (re-find (re-pattern (name cause)) (str (get-in @out [:cause :why]))))))))))

(deftest done-carries-the-got-and-tried-for-a-limit-ending
  (let [done? (export 'jobs.items.obtain :done?)
        c (fn [m inventory] {:primitives (tu/fake {:inventory inventory}) :root "j1" :slots [] :args {:item "oak_log" :count 3}
                             :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data m}]}}})})
        d (done? (c {:start {:have 1 :target 4} :tried [:craft]} [{:name "oak_log" :count 3}]))]
    (is (= {:done? false :done 2 :left 1 :data {:got 2 :tried [:craft] :item "oak_log"}}
           (select-keys d [:done? :done :left :data])))
    (is (= {:got 0} (:data (done? (c {} [])))) "before the start")))
