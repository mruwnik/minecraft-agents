(ns engine.need-test
  "jobs.lib.need: the fetch jobs as contract children; their endings read as causes of the caller."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [jobs.lib.need :as need]
            [jobs.lib.waits :as waits]))

(def got (atom []))

(def clock (atom 1000000))

(def ending-of
  "The [kind cause] a fetch child ends with for a step's reason; any other reason is :nothing-possible."
  {:no-source [:condition-failed :no-source]
   :timeout [:limit-reached {:line :job-time :reason :timeout}]})

(defn stopper
  "A fetch job: each round records its args and slot, then does (step c), which ends it :done (nil) or with the
  reason's ending (ending-of); :continue yields instead."
  [seen step start]
  {:ways [:chest :craft :smelt :gather]
   :can-start? (fn [_] (or start (contract/ok :ready "ready")))
   :round (fn ^:async r [c]
            (swap! seen conj (assoc (:args c) :slot (peek (:slots c))))
            (let [s (step c)]
              (cond
                (= :continue s) :continue
                s (let [[kind cause] (get ending-of s [:nothing-possible s])] (ctx/end c kind cause))
                :else (ctx/end c :done nil))))})

(defn setup
  "An engine with the caller job running (call-fn c) each round, obtain and get-tool as stoppers doing obtain-step and
  tool-step, and go-to as a walker that puts the body on its :pos. Each round's answer is appended to got; the caller
  ends on one other than :done."
  [call-fn {:keys [obtain-step obtain-start tool-step]}]
  (reset! got [])
  (reset! clock 1000000)
  (let [obtain (atom [])
        tool (atom [])
        walks (atom [])
        p (tu/fake {})
        [seen sink] (tu/capture-sink)
        walker {:ways [:walk :clear-seal]
                :round (fn [c]
                         (let [{:keys [x y z]} (:pos (:args c))]
                           (swap! walks conj (:args c))
                           (swap! (fake/state p) assoc-in [:self :pos] [(+ x 0.5) y (+ z 0.5)])
                           (ctx/end c :done nil)))}
        jobs {'jobs.items.obtain (stopper obtain (or obtain-step (constantly nil)) obtain-start)
              'jobs.items.get-tool (stopper tool (or tool-step (constantly nil)) nil)
              'jobs.movement.go-to walker
              'caller {:args {}
                       :round (fn ^:async r [c]
                                (let [e (await (call-fn c))]
                                  (swap! got conj e)
                                  (if (and (map? e) (not= :done (:kind e))) e :continue)))}}
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now (fn [] @clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now (fn [] @clock)})})]
    (core/submit! eng '(caller {}) {})
    {:eng eng :p p :seen seen :obtain obtain :tool tool :walks walks}))

(defn ^:async run!
  "setup, then n ticks (default 1)."
  ([call-fn steps] (run! call-fn steps 1))
  ([call-fn steps n]
   (let [r (setup call-fn steps)]
     (dotimes [_ n] (await (core/tick! (:eng r))))
     r)))

(defn stop-with [reason] {:obtain-step (constantly reason) :tool-step (constantly reason)})

(defn give! [p item n] (swap! (fake/state p) update :inventory fake/add-to item n))

(defn carried [p item] (fake/carried (:inventory @(fake/state p)) item))

(deftest item!-passes-a-done-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/item! % "iron_ingot" 3) nil))]
          (is (= :done (:kind (last @got))))
          (is (= {:item "iron_ingot" :count 3 :explore false :slot :need}
                 (select-keys (first @(:obtain r)) [:item :count :explore :slot]))))))))

(deftest item!-failure-reads-as-the-need-with-the-childs-ending-as-because
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run! #(need/item! % "iron_ingot" 3) (stop-with :no-source)))
        (is (= :condition-failed (:kind (last @got))) "no-source is a missing fact")
        (is (= {:reason :need :why :no-source :item "iron_ingot" :count 3 :text "needs 3 iron_ingot: no-source"} (:cause (last @got))))
        (is (= :no-source (contract/cause-key (:because (last @got)))))))))

(deftest item-price-reads-the-price-of-the-obtain-childs-not-worth-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[start want] [[(contract/ok :ready "ready") 0]
                              [(contract/later :not-worth "costs 42 s" :price 42 :worth 0) 42]
                              [(contract/later :no-source "no source") ##Inf]
                              [(contract/never :bad-args "bad") ##Inf]]]
          (let [price (atom nil)]
            (await (run! (fn [c] (reset! price (need/item-price c "torch" 4 nil nil)) nil) {:obtain-start start}))
            (is (= want @price) (str (pr-str start)))))))))

(deftest a-fetch-that-waits-for-dawn-is-not-called-but-another-later-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/item! % "iron_ingot" 3)
                             {:obtain-start (contract/later :dark "night above ground, waits for dawn")}))]
          (is (empty? @(:obtain r)) "a readiness wait parks the caller without a call")
          (is (= [:continue] @got) "the caller waits, no ending"))
        (await (run! #(need/item! % "iron_ingot" 3) {:obtain-start (contract/later :no-source "no source")}))
        (is (= {:reason :need :item "iron_ingot" :count 3} (select-keys (:cause (last @got)) [:reason :item :count]))
            "any other later is called: the caller ends :need with the cause")
        (let [r (await (run! #(need/call! % :direct 'jobs.items.obtain {:item "iron_ingot" :count 1})
                             {:obtain-start (contract/later :exposed "dark until dawn")}))]
          (is (empty? @(:obtain r)) "a job calling obtain itself parks the same way")
          (is (= [:continue] @got)))))))

(deftest a-full-bag-is-free-slots-and-a-limit-stays-a-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (run! #(need/blocks! % ["cobblestone" "dirt"] 8) (stop-with :inventory-full)))
        (is (= :free-slots (contract/cause-key (last @got))))
        (is (= ["cobblestone" "dirt"] (get-in (last @got) [:cause :any-of])))
        (await (run! #(need/item! % "coal" 1) (stop-with :timeout)))
        (is (= :limit-reached (:kind (last @got))) "obtain's :timeout is a time limit")
        (is (= :need (contract/cause-key (last @got))))))))

(deftest tool!-names-the-tool-wanted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/tool! % {:block "stone"} {:explore true}) {:tool-step (constantly :no-source)}))]
          (is (= {:reason :tool :why :no-source :block "stone" :text "needs a tool for stone: no-source"} (:cause (last @got))))
          (is (= {:block "stone" :explore true} (select-keys (first @(:tool r)) [:block :explore]))))))))

(deftest a-waiting-child-is-continue-and-the-options-pass-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [rounds (atom 0)
              r (await (run! #(need/item! % "coal" 2 {:slot :coal :how #{:chest}})
                             {:obtain-step #(when (= 1 (swap! rounds inc)) :continue)} 2))]
          (is (= [:continue :done] [(first @got) (:kind (second @got))]))
          (is (= [{:ways #{:chest} :slot :coal}]
                 (distinct (map #(select-keys % [:ways :slot]) @(:obtain r))))))))))

(deftest a-done-fetch-that-leaves-the-need-is-not-solved
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/item! % "coal" 1) nil 2))]
          (is (= :done (:kind (first @got))))
          (is (= {:kind :condition-failed :cause {:reason :need :why :not-solved :item "coal" :count 1 :text "needs 1 coal: not-solved"}}
                 (select-keys (second @got) [:kind :cause]))
              "the same need straight after a done fetch, nothing used up meanwhile")
          (is (= 1 (count @(:obtain r))) "the child is not called again"))))))

(deftest a-want-naming-no-item-skips-the-repeat-guard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/fetch! % 'jobs.items.obtain {:count 1} :need {:count 1} nil) nil 2))]
          (is (= [:done :done] (map :kind @got)))
          (is (= 2 (count @(:obtain r)))))))))

(deftest a-need-again-after-using-up-what-was-fetched-fetches-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p obtain]} (setup #(need/item! % "coal" 1) nil)]
          (give! p "coal" 1)
          (await (core/tick! eng))
          (swap! (fake/state p) assoc :inventory [])
          (await (core/tick! eng))
          (is (= [:done :done] (map :kind @got)))
          (is (= 2 (count @obtain))))))))

(deftest return!-walks-back-to-the-cell-the-fetch-began-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [obtain-step (fn [c] (swap! (fake/state (:primitives c)) assoc-in [:self :pos] [6.5 64 0.5]) nil)
              {:keys [walks p]} (await (run! (fn ^:async f [c]
                                               (or (await (need/return! c))
                                                   (if (pos? (carried (:primitives c) "coal"))
                                                     (ctx/end c :done nil)
                                                     (let [e (await (need/item! c "coal" 1 {:return? true}))]
                                                       (when (= :done (:kind e)) (give! (:primitives c) "coal" 1))
                                                       (if (= :done (:kind e)) :continue e)))))
                                             {:obtain-step obtain-step} 3))]
          (is (= [:continue :done] (map #(if (map? %) (:kind %) %) (take 2 @got))))
          (is (= [{:pos {:x 0 :y 64 :z 0} :range 0}] (map #(select-keys % [:pos :range]) @walks)))
          (is (= [0.5 64 0.5] (get-in @(fake/state p) [:self :pos]))))))))

(deftest return!-walks-back-after-a-fetch-that-failed-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [obtain-step (fn [c] (swap! (fake/state (:primitives c)) assoc-in [:self :pos] [6.5 64 0.5]) :no-source)
              {:keys [walks]} (await (run! (fn ^:async f [c]
                                               (or (await (need/return! c))
                                                   (let [e (await (need/item! c "coal" 1 {:return? true}))]
                                                     (if (= :continue e) e :continue))))
                                             {:obtain-step obtain-step} 3))]
          (is (= [{:pos {:x 0 :y 64 :z 0} :range 0}] (map #(select-keys % [:pos :range]) (take 1 @walks)))))))))

(deftest a-minutes-limit-reaches-the-fetch-child-as-a-time-spend
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/item! % "coal" 1 {:minutes 2}) nil 1))]
          (is (= {:job-time 120} (:spend (first @(:obtain r))))))))))

(deftest a-for-plan-option-reaches-the-obtain-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/item! % "coal" 1 {:for-plan "house"}) nil 1))]
          (is (= "house" (:for-plan (first @(:obtain r))))))))))

(deftest a-center-option-reaches-the-obtain-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run! #(need/item! % "coal" 1 {:center [10 64 20]}) nil 1))]
          (is (= [10 64 20] (:center (first @(:obtain r))))))))))

(deftest a-different-need-in-the-same-slot-starts-the-child-afresh
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [round (atom 0)
              mems (atom [])
              step (fn [c]
                     (swap! mems conj (:start (ctx/mem c)))
                     (ctx/update-mem! c assoc :start {:have 0 :target 26})
                     :continue)]
          (await (run! (fn [c] (need/item! c (if (= 1 (swap! round inc)) "cobblestone" "birch_planks") 16))
                       {:obtain-step step} 2))
          (is (= [nil nil] @mems) "the second need does not inherit the first one's booked target"))))))

(deftest a-shrinking-count-keeps-the-child-and-its-booked-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [round (atom 0)
              mems (atom [])
              step (fn [c]
                     (swap! mems conj (:start (ctx/mem c)))
                     (ctx/update-mem! c assoc :start {:have 0 :target 26})
                     :continue)]
          (await (run! (fn [c] (need/item! c "cobblestone" (- 16 (swap! round inc))))
                       {:obtain-step step} 2))
          (is (= [nil {:have 0 :target 26}] @mems) "the second round resumes the first one's booked target"))))))

(deftest obtain-is-asked-for-the-carried-plus-the-lack-and-the-goal-stays-put
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (setup #(need/item! % "cobblestone" 2) {:obtain-step (constantly :continue)})]
          (give! (:p r) "cobblestone" 3)
          (await (core/tick! (:eng r)))
          (give! (:p r) "cobblestone" 1)
          (await (core/tick! (:eng r)))
          (is (= [5 5] (mapv :count @(:obtain r))) "the 2 more are on top of the 3 carried when the fetch began"))))))

(deftest carry-count-books-the-total-once-per-child-so-a-yielding-child-does-not-move-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (setup (fn [c] (ctx/call c :x 'jobs.items.obtain {:item "cobblestone" :count (need/carry-count c :x ["cobblestone"] 2)}))
                       {:obtain-step (constantly :continue)})]
          (give! (:p r) "cobblestone" 3)
          (await (core/tick! (:eng r)))
          (give! (:p r) "cobblestone" 2)
          (await (core/tick! (:eng r)))
          (is (= [5 5] (mapv :count @(:obtain r))) "the 2 more are on top of the 3 carried when the child began, not of the 5 carried later"))))))

(deftest a-child-that-lacks-a-free-slot-is-waited-out-until-the-pack-stays-full-too-long
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [full (contract/ending :condition-failed {:reason :no-free-slot} nil)
              r (setup #(waits/waiting-out % full) nil)]
          (dotimes [_ 4]
            (await (core/tick! (:eng r)))
            (swap! clock + (/ waits/full-pack-limit-ms 3)))
          (is (= [:continue :continue :continue :inventory-full] (mapv #(if (map? %) (:reason (:cause %)) %) @got))
              "waits while the pack may clear, ends naming the full pack once it stayed full for the limit"))))))

(deftest a-refused-child-is-passed-through-waiting-out
  (let [refused (contract/ending :condition-failed {:reason :refused} nil)]
    (is (= refused (waits/waiting-out {} refused)))))
