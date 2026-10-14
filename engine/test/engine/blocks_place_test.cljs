(ns engine.blocks-place-test
  "jobs.blocks.place against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [box]]
            [jobs.blocks.place :as place]
            [plan.shape :as shape]))

(def job 'jobs.blocks.place)
(def at {:x 2 :y 64 :z 0})
(def body {:pos {:x 0 :y 64 :z 0}})
(def cobble [{:name "cobblestone" :count 4}])

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent, at most n ticks: the child's {:kind :cause} (nil when
  it does not end)."
  [env args n]
  (let [out (atom nil)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause])) :done))))}
        eng (assoc-in (:eng env) [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng n))
    @out))

(deftest a-carried-block-is-placed-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :inventory cobble})
              result (await (bd/child-outcome eng job {:pos [2 64 0] :item "cobblestone"} 5))]
          (is (= {:placed true :pos at :item "cobblestone" :reason :placed :block {:name "cobblestone" :state {}}} (dissoc result :status)))
          (is (= "cobblestone" (bd/block-at p at)))
          (is (= 3 (bd/carried p "cobblestone")))
          (is (= 1 (count (filter #(= :blocks.place.done (:kind %)) @seen)))))))))

(deftest a-refused-place-keeps-the-primitives-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :inventory cobble})
              why "Server refused to place cobblestone at (2, 64, 0): the block is still air"]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "failed" :reason why}))
          (let [result (await (bd/child-outcome eng job {:pos at :item "cobblestone"} 5))
                done-event (first (filter #(= :blocks.place.done (:kind %)) @seen))]
            (is (= :stopped (:status result)))
            (is (= why (:why result)))
            (is (re-find #"still air" (:text done-event)))))))))

(deftest a-refused-place-without-a-reason-says-why-from-its-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[r why] [[#js {:status "occupied" :block "stone"} #"stone"]
                         [#js {:status "no-support"} #"nothing to place against"]
                         [#js {:status "no-item"} #"not carried"]
                         [#js {:status "weird"} #"weird"]]]
          (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble})
                _ (.override (.-world p) "place" (fn ^:async f [_ _ _] r))
                result (await (bd/child-outcome eng job {:pos at :item "cobblestone"} 5))]
            (is (= :failed (:reason result)))
            (is (re-find why (str (:why result))) (.-status r))))))))

(deftest a-use-list-places-the-first-carried-and-any-of-is-its-old-spelling
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:use {:blocks ["cobblestone" "dirt"]}} {:any-of ["cobblestone" "dirt"]}]]
          (let [{:keys [eng p]} (bd/setup {:self body :inventory [{:name "dirt" :count 1}]})
                result (await (bd/child-outcome eng job (assoc args :pos at) 5))]
            (is (= {:placed true :item "dirt"} (select-keys result [:placed :item])) (pr-str args))
            (is (= "dirt" (bd/block-at p at)))))))))

(deftest with-neither-item-nor-list-it-places-a-carried-filler
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble})
              result (await (bd/child-outcome eng job {:pos at} 5))]
          (is (= {:placed true :item "cobblestone"} (select-keys result [:placed :item])))
          (is (= "cobblestone" (bd/block-at p at))))))))

(deftest a-cell-out-of-reach-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble})
              result (await (bd/child-outcome eng job {:pos {:x 12 :y 64 :z 0} :item "cobblestone"} 20))]
          (is (:placed result))
          (is (= "cobblestone" (bd/block-at p {:x 12 :y 64 :z 0}))))))))

(deftest a-zone-appearing-during-the-walk-ends-the-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/zone-on-first-step (bd/setup {:self body :inventory cobble}))
              e (await (ending-of env {:pos [12 64 0] :item "cobblestone"} 5))]
          (is (= :condition-failed (:kind e)))
          (is (= {:reason :not-allowed :by :zone :zone "farm"} (select-keys (:cause e) [:reason :by :zone])))
          (is (empty? (bd/calls (:p env) "place")))
          (is (= 4 (bd/carried (:p env) "cobblestone"))))))))

(deftest a-cell-holding-the-block-already-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks {"2,64,0" "cobblestone"} :inventory cobble})
              result (await (bd/child-outcome eng job {:pos at :item "cobblestone"} 5))]
          (is (= {:placed false :reason :already} (select-keys result [:placed :reason])))
          (is (empty? (bd/calls p "place"))))))))

(deftest grass-or-a-flower-in-the-cell-is-dug-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [plant ["short_grass" "poppy" "snow"]]
          (let [{:keys [eng p]} (bd/setup {:self body :blocks {"2,64,0" plant "2,63,0" "grass_block"} :inventory cobble})
                result (await (bd/child-outcome eng job {:pos at :item "cobblestone"} 8))]
            (is (:placed result) plant)
            (is (= [[2 64 0]] (mapv #(let [a (.-pos (.-args %))] [(.-x a) (.-y a) (.-z a)]) (bd/calls p "dig"))) plant)
            (is (= "cobblestone" (bd/block-at p at)) plant)))))))

(deftest without-the-item-it-waits-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :need :item "cobblestone" :pos at}
               (:cause (await (bd/waiting-after (bd/setup {:self body}) (list job {:pos at :item "cobblestone" :fetch false}) 3)))))
        (is (= {:reason :need :any-of ["cobblestone" "dirt"] :pos at}
               (:cause (await (bd/waiting-after (bd/setup {:self body}) (list job {:pos at :use {:blocks ["cobblestone" "dirt"]} :fetch false}) 3)))))))))

(deftest a-solid-block-in-the-cell-ends-occupied
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})]
          (let [r (await (bd/child-outcome (:eng env) job {:pos at :item "cobblestone"} 5))]
            (is (= {:placed false :reason :occupied :block "stone"} (select-keys r [:placed :reason :block]))))
          (is (empty? (bd/calls (:p env) "dig"))))
        (let [env (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})
              e (await (ending-of env {:pos at :item "cobblestone"} 3))]
          (is (= {:kind :condition-failed :cause {:reason :occupied :block "stone"}} e)))))))

(deftest a-failed-place-is-nothing-possible-and-a-held-block-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble})
              _ (.override (.-world (:p env)) "place" (fn ^:async f [_ _ _] #js {:status "failed" :reason "no"}))
              e (await (ending-of env {:pos at :item "cobblestone"} 3))]
          (is (= {:kind :nothing-possible :cause {:reason :failed :primitive "failed"}} e)))
        (let [env (bd/setup {:self body :blocks {"2,64,0" "cobblestone"} :inventory cobble})]
          (core/submit! (:eng env) (list job {:pos at :item "cobblestone"}) {})
          (await (tu/run-until-empty (:eng env) 5))
          (is (some #(= :completed (:kind %)) @(:seen env))))))))

(defn done-text [seen]
  (:text (first (filter #(= :blocks.place.done (:kind %)) @seen))))

(deftest done-texts-name-the-item-and-the-blocking-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [a (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})
              _ (await (bd/child-outcome (:eng a) job {:pos at :item "cobblestone"} 5))
              b (bd/setup {:self body :blocks {"2,64,0" "cobblestone"} :inventory cobble})
              _ (await (bd/child-outcome (:eng b) job {:pos at :item "cobblestone"} 5))
              c (bd/setup {:self body :inventory cobble})
              _ (await (bd/child-outcome (:eng c) job {:pos [2 64 0] :item "cobblestone"} 5))]
          (is (= "did not place cobblestone at [2 64 0]: occupied by stone" (done-text (:seen a))))
          (is (= "did not place cobblestone at [2 64 0]: already" (done-text (:seen b))))
          (is (= "placed cobblestone at [2 64 0]" (done-text (:seen c)))))))))

(deftest a-cell-in-anothers-zone-waits-not-allowed-and-ignore-zones-records-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble :zones [bd/farm-zone]})]
          (is (= {:reason :not-allowed :pos at :by :zone :zone "farm" :owner "Miles"}
                 (:cause (await (bd/waiting-after env (list job {:pos at :item "cobblestone"}) 3))))))
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble :zones [bd/farm-zone]})
              result (await (bd/child-outcome eng job {:pos at :item "cobblestone" :ignore-zones? true} 5))]
          (is (:placed result))
          (is (= [{:cell [2 64 0] :action :place :was "air" :now "cobblestone"}]
                 (mapv #(select-keys (:data %) [:cell :action :was :now]) (mem/entries (mem/view (:store eng)) :tidy))))
          (is (= "cobblestone" (bd/block-at p at))))))))

(deftest the-bodys-own-cell-waits-own-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :own-body :pos {:x 0 :y 64 :z 0}}
               (:cause (await (bd/waiting-after (bd/setup {:self body :inventory cobble}) (list job {:pos [0 64 0] :item "cobblestone"}) 3)))))))))

(deftest a-cell-with-nothing-to-place-against-waits-no-support
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :no-support :pos {:x 2 :y 67 :z 0}}
               (:cause (await (bd/waiting-after (bd/setup {:self body :inventory cobble}) (list job {:pos [2 67 0] :item "cobblestone"}) 3)))))))))

(deftest no-item-named-ends-with-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (bd/setup {:self body})]
          (is (= {:placed false :reason :bad-args} (select-keys (await (bd/child-outcome eng job {:pos at} 3)) [:placed :reason]))))))))

(deftest a-cell-of-the-bodys-own-plan-is-placed-another-bodys-plan-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "w" :cells [[2 64 0]] :want "cobblestone"}]}
              own (bd/setup {:self body :inventory cobble :plans {"hut" (shape/with-author plan "Fake")}})
              other (bd/setup {:self body :inventory cobble :plans {"hut" (shape/with-author plan "Miles")}})]
          (is (:placed (await (bd/child-outcome (:eng own) job {:pos at :item "cobblestone"} 5))))
          (is (= {:reason :not-allowed :by :footprint :plan "hut"}
                 (select-keys (:cause (await (bd/waiting-after other (list job {:pos at :item "cobblestone"}) 3))) [:reason :by :plan]))))))))

(deftest a-far-cell-is-walked-to-and-placed-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble})]
          (is (await (bd/ended-in-one-tick? env (list job {:pos [12 64 0] :item "cobblestone"}))))
          (is (= "cobblestone" (bd/block-at (:p env) {:x 12 :y 64 :z 0}))))))))

(deftest a-walk-that-fails-twice-in-one-call-ends-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (bd/failing-walks (bd/setup {:self body :inventory cobble}) walks)
              e (await (ending-of env {:pos [12 64 0] :item "cobblestone"} 5))]
          (is (= 2 @walks) "one more try, then it ends")
          (is (= {:kind :condition-failed :cause {:reason :unreachable :why :no-path}} e)))))))

(deftest a-cut-at-the-place-is-resumed-and-the-block-placed-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble})
              left (await (bd/cut-at-act-then-resume env (list job {:pos at :item "cobblestone"}) "place"))]
          (is (< left 40) "the job ended")
          (is (= "cobblestone" (bd/block-at (:p env) at)))
          (is (= 3 (bd/carried (:p env) "cobblestone")) "one block went in")
          (is (= 2 (count (bd/calls (:p env) "place"))) "the cut place and the resumed one"))))))

(deftest a-plant-is-cleared-and-the-cell-filled-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :blocks {"2,64,0" "short_grass" "2,63,0" "grass_block"} :inventory cobble})]
          (is (await (bd/ended-in-one-tick? env (list job {:pos [2 64 0] :item "cobblestone"}))))
          (is (= "cobblestone" (bd/block-at (:p env) at))))))))

(deftest the-support-vanishing-during-the-walk-ends-the-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/after-walks (bd/setup {:self body :inventory cobble}) #(fake/remove-block! % [12 63 0]))
              r (:cause (await (ending-of env {:pos [12 64 0] :item "cobblestone"} 5)))]
          (is (= :no-support (:reason r)))
          (is (empty? (bd/calls (:p env) "place")) "walked, then refused: nothing placed"))))))

(deftest a-seed-is-placed-in-the-bodys-own-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks {"0,63,0" "farmland"} :inventory [{:name "wheat_seeds" :count 2}]})
              result (await (bd/child-outcome eng job {:pos [0 64 0] :item "wheat_seeds"} 5))]
          (is (= {:placed true :item "wheat_seeds"} (select-keys result [:placed :item])))
          (is (= "wheat" (bd/block-at p {:x 0 :y 64 :z 0}))))))))

(deftest boats-needs-at-least-one-item-and-all-boats
  (is (true? (place/boats? ["oak_boat" "bamboo_raft"])))
  (is (false? (place/boats? [])) "no items: the support check stays")
  (is (false? (place/boats? ["oak_boat" "dirt"]))))

;; ------------------------------------------------------------------ a pen holding animals

(def pen-blocks
  "A fence pen x 5..13, z -4..4 with a gate in each end wall, for a body that stands at x 0."
  (-> (merge (box 5 63 -4 13 63 4 "stone") (box 5 64 -4 13 64 -4 "oak_fence") (box 5 64 4 13 64 4 "oak_fence")
             (box 5 64 -4 5 64 4 "oak_fence") (box 13 64 -4 13 64 4 "oak_fence"))
      (assoc "5,64,0" "oak_fence_gate" "13,64,0" "oak_fence_gate")))

(defn pen-env []
  (bd/setup {:self body :blocks pen-blocks :inventory [{:name "dirt" :count 4}]
             :states {"5,64,0" {:open false :facing "east"} "13,64,0" {:open false :facing "east"}}
             :entities [{:id 7 :name "cow" :kind "passive" :pos {:x 8.5 :y 64 :z 0.5}}
                        {:id 8 :name "sheep" :kind "passive" :pos {:x 10.5 :y 64 :z 1.5}}]}))

(defn gate-clicks [p] (count (filterv #(= "useOn" (.-name %)) (.-calls (.-world p)))))

(deftest a-place-beyond-a-pen-works-from-outside-without-using-its-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (pen-env)
              result (await (bd/child-outcome eng job {:pos [16 64 0] :item "dirt"} 40))]
          (is (= true (:placed result)))
          (is (zero? (gate-clicks p)) "the gates stay shut, the walk went round"))))))

(deftest a-place-inside-a-pen-from-outside-declines-naming-the-pen-and-enter-pens-allows-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (pen-env)
              declined (await (bd/child-outcome eng job {:pos [9 64 0] :item "dirt"} 40))]
          (is (not= true (:placed declined)))
          (is (zero? (gate-clicks p)))
          (is (some #(and (= :blocks.pen-kept-out (:kind %)) (re-find #"\[5 64 0\]" (:text %))) @seen)))
        (let [{:keys [eng]} (pen-env)
              allowed (await (bd/child-outcome eng job {:pos [9 64 0] :item "dirt" :enter-pens true} 40))]
          (is (= true (:placed allowed))))))))

(def use-on 'jobs.blocks.use-on)

(deftest a-use-on-the-gate-or-near-the-fence-works-from-outside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [pos [[5 64 0] [7 63 2]]]
          (let [{:keys [eng p seen]} (pen-env)
                result (await (bd/child-outcome eng use-on {:pos pos} 40))]
            (is (not= :unreachable (:reason result)) (pr-str pos))
            (is (= 1 (gate-clicks p)) (pr-str pos))
            (is (empty? (filter #(= :blocks.pen-kept-out (:kind %)) @seen)) (pr-str pos))))))))

(deftest a-use-on-deep-inside-a-pen-declines-naming-the-pen-and-enter-pens-allows-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (pen-env)
              declined (await (bd/child-outcome eng use-on {:pos [10 63 0]} 40))]
          (is (= :unreachable (:reason declined)))
          (is (zero? (gate-clicks p)))
          (is (some #(and (= :blocks.pen-kept-out (:kind %)) (re-find #"\[5 64 0\]" (:text %))) @seen)))
        (let [{:keys [eng]} (pen-env)
              allowed (await (bd/child-outcome eng use-on {:pos [10 63 0] :enter-pens true} 40))]
          (is (not= :unreachable (:reason allowed))))))))

(deftest a-use-on-ends-by-its-click-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[status kind cause] [["used" :done nil]
                                     ["no-item" :condition-failed {:reason :no-item}]
                                     ["unchanged" :nothing-possible {:reason :unchanged}]]]
          (let [env (bd/setup {:self body :inventory cobble})
                _ (.override (.-world (:p env)) "useOn" (fn ^:async f [_ _ _] #js {:status status}))
                out (atom nil)
                parent {:check (constantly true)
                        :round (fn ^:async r [c]
                                 (let [e (await (ctx/call c :kid use-on {:pos at}))]
                                   (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause])) :done))))}
                eng (assoc-in (:eng env) [:jobs 'recording-parent] parent)]
            (core/submit! eng '(recording-parent) {})
            (await (tu/run-until-empty eng 5))
            (is (= {:kind kind :cause cause} (update @out :cause identity)) status)))))))

(deftest a-quote-of-place-and-use-on-grows-with-the-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble})
              out (atom [])
              parent {:check (constantly true)
                      :round (fn ^:async r [c]
                               (doseq [[j args] [[job {:pos at :item "cobblestone"}] [job {:pos [30 64 0] :item "cobblestone"}]
                                                 [use-on {:pos [30 64 0]}]]]
                                 (swap! out conj (await (ctx/quote c j args))))
                               :done)}
              eng (assoc-in (:eng env) [:jobs 'recording-parent] parent)]
          (core/submit! eng '(recording-parent) {})
          (await (tu/run-until-empty eng 5))
          (let [[near far use] @out]
            (is (= {"cobblestone" [1 1 1]} (get-in near [:uses :items])))
            (is (< (second (get-in near [:uses :time])) (second (get-in far [:uses :time]))))
            (is (< (second (get-in near [:uses :time])) (second (get-in use [:uses :time]))))))))))
