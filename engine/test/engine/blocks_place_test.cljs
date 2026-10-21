(ns engine.blocks-place-test
  "jobs.blocks.place against the fake world."
  (:require [engine.child-ends :as ce]
            [cljs.test :refer [deftest is are async]]
            [engine.blocks-dig-test :as bd]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.settings :as settings]
            [engine.memory :as mem]
            [engine.test-util :as tu :refer [box]]
            [jobs.blocks.place :as place]
            [jobs.lib.blocks :as b]
            [jobs.lib.util :as u]
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
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause])) (ctx/end c :done nil)))))}
        eng (assoc-in (:eng env) [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng n))
    @out))

(deftest a-carried-block-is-placed-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :inventory cobble})
              result (await (tu/child-outcome eng job {:pos [2 64 0] :item "cobblestone"} 5))]
          (is (= {:placed true :pos at :item "cobblestone" :reason :placed :block {:name "cobblestone" :state {}}} (dissoc result :status :text)))
          (is (= "cobblestone" (bd/block-at p at)))
          (is (= 3 (bd/carried p "cobblestone")))
          (is (= 1 (count (ce/ends seen :placed)))))))))

(deftest a-refused-place-keeps-the-primitives-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :inventory cobble})
              why "Server refused to place cobblestone at (2, 64, 0): the block is still air"]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "failed" :reason why}))
          (let [result (await (tu/child-outcome eng job {:pos at :item "cobblestone"} 5))
                done-event result]
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
                result (await (tu/child-outcome eng job {:pos at :item "cobblestone"} 5))]
            (is (= :failed (:reason result)))
            (is (re-find why (str (:why result))) (.-status r))))))))

(deftest a-use-list-places-the-first-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [args [{:use {:blocks ["cobblestone" "dirt"]}}]]
          (let [{:keys [eng p]} (bd/setup {:self body :inventory [{:name "dirt" :count 1}]})
                result (await (tu/child-outcome eng job (assoc args :pos at) 5))]
            (is (= {:placed true :item "dirt"} (select-keys result [:placed :item])) (pr-str args))
            (is (= "dirt" (bd/block-at p at)))))))))

(deftest with-neither-item-nor-list-it-places-a-carried-filler
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble})
              result (await (tu/child-outcome eng job {:pos at} 5))]
          (is (= {:placed true :item "cobblestone"} (select-keys result [:placed :item])))
          (is (= "cobblestone" (bd/block-at p at))))))))

(deftest a-cell-out-of-reach-is-walked-to-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble})
              result (await (tu/child-outcome eng job {:pos {:x 12 :y 64 :z 0} :item "cobblestone"} 20))]
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
              result (await (tu/child-outcome eng job {:pos at :item "cobblestone"} 5))]
          (is (= {:status :done :already? true} (select-keys result [:status :already?])) "the engine ends it before a round")
          (is (empty? (bd/calls p "place"))))))))

(deftest grass-or-a-flower-in-the-cell-is-dug-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [plant ["short_grass" "poppy" "snow"]]
          (let [{:keys [eng p]} (bd/setup {:self body :blocks {"2,64,0" plant "2,63,0" "grass_block"} :inventory cobble})
                result (await (tu/child-outcome eng job {:pos at :item "cobblestone"} 8))]
            (is (:placed result) plant)
            (is (= [[2 64 0]] (mapv #(let [a (.-pos (.-args %))] [(.-x a) (.-y a) (.-z a)]) (bd/calls p "dig"))) plant)
            (is (= "cobblestone" (bd/block-at p at)) plant)))))))

(deftest a-light-block-in-the-cell-is-placed-over-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks {"2,64,0" "light" "2,63,0" "stone"} :inventory cobble})
              result (await (tu/child-outcome eng job {:pos at :item "cobblestone"} 8))]
          (is (:placed result))
          (is (empty? (bd/calls p "dig")))
          (is (= "cobblestone" (bd/block-at p at))))))))

(deftest without-the-item-it-waits-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :need :item "cobblestone" :pos at}
               (bd/wait-cause (await (bd/waiting-after (bd/setup {:self body}) (list job {:pos at :item "cobblestone" :ways #{:carried}}) 3)))))
        (is (= {:reason :need :any-of ["cobblestone" "dirt"] :pos at}
               (bd/wait-cause (await (bd/waiting-after (bd/setup {:self body}) (list job {:pos at :use {:blocks ["cobblestone" "dirt"]} :ways #{:carried}}) 3)))))))))

(deftest a-solid-block-in-the-cell-ends-occupied
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})]
          (let [r (await (tu/child-outcome (:eng env) job {:pos at :item "cobblestone"} 5))]
            (is (= {:placed false :reason :occupied :block "stone"} (select-keys r [:placed :reason :block]))))
          (is (empty? (bd/calls (:p env) "dig"))))
        (let [env (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})
              e (await (ending-of env {:pos at :item "cobblestone"} 3))]
          (is (= {:kind :condition-failed :cause {:reason :occupied :block "stone"}} e)))))))

(deftest a-place-the-server-refused-once-is-tried-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (bd/setup {:self body :inventory cobble})
              calls (atom 0)]
          (.override (.-world p) "place"
                     (fn ^:async f [token a real]
                       (if (= 1 (swap! calls inc))
                         #js {:status "failed" :reason "Server refused to place cobblestone: the block is still air"}
                         (await (real token a)))))
          (let [result (await (tu/child-outcome eng job {:pos at :item "cobblestone"} 5))]
            (is (= :done (:status result)))
            (is (= 2 @calls))
            (is (= "cobblestone" (bd/block-at p at)))))))))

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
  (:text (ce/data (first (ce/ends seen :placed)))))

(deftest done-texts-name-the-item-and-the-blocking-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [a (bd/setup {:self body :blocks {"2,64,0" "stone" "2,63,0" "stone"} :inventory cobble})
              _ (await (tu/child-outcome (:eng a) job {:pos at :item "cobblestone"} 5))
              c (bd/setup {:self body :inventory cobble})
              _ (await (tu/child-outcome (:eng c) job {:pos [2 64 0] :item "cobblestone"} 5))]
          (is (= "did not place cobblestone at [2 64 0]: occupied by stone" (done-text (:seen a))))
          (is (= "placed cobblestone at [2 64 0]" (done-text (:seen c)))))))))

(deftest a-cell-in-anothers-zone-waits-not-allowed-and-ignore-zones-records-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble :zones [bd/farm-zone]})]
          (is (= {:reason :not-allowed :pos at :by :zone :zone "farm" :owner "Miles"}
                 (bd/wait-cause (await (bd/waiting-after env (list job {:pos at :item "cobblestone"}) 3))))))
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble :zones [bd/farm-zone]})
              result (await (tu/child-outcome eng job {:pos at :item "cobblestone" :ignore-zones? true} 5))]
          (is (:placed result))
          (is (= [{:cell [2 64 0] :action :place :was "air" :now "cobblestone"}]
                 (mapv #(select-keys (:data %) [:cell :action :was :now]) (mem/entries (mem/view (:store eng)) :tidy))))
          (is (= "cobblestone" (bd/block-at p at))))))))

(deftest a-body-standing-in-the-cell-steps-off-it-and-places
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :inventory cobble})
              result (await (tu/child-outcome eng job {:pos [0 64 0] :item "cobblestone"} 12))
              feet (u/self-pos {:primitives p})]
          (is (:placed result))
          (is (= "cobblestone" (bd/block-at p {:x 0 :y 64 :z 0})))
          (is (not= [0 0] [(js/Math.floor (:x feet)) (js/Math.floor (:z feet))])))))))

(deftest a-cell-with-nothing-to-place-against-waits-no-support
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:reason :no-support :pos {:x 2 :y 67 :z 0}}
               (bd/wait-cause (await (bd/waiting-after (bd/setup {:self body :inventory cobble}) (list job {:pos [2 67 0] :item "cobblestone"}) 3)))))))))

(deftest a-cell-hidden-from-where-the-body-stands-is-placed-from-where-it-sees-it
  ;; the pit floor is in reach of the cell but the surface stone is in the way; :see false places from the pit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [see? [true false]]
          (let [{:keys [eng p]} (bd/setup {:self bd/in-pit :blocks bd/pit-blocks :inventory cobble})
                result (await (tu/child-outcome eng job {:pos [6 65 0] :item "cobblestone" :see see?} 15))
                y (.-y (.-pos (.self p)))]
            (is (= "cobblestone" (bd/block-at p {:x 6 :y 65 :z 0})) (pr-str result))
            (is (= see? (>= y 64)) (str ":see " see? ", feet y " y))))))))

(deftest a-floor-remembered-as-air-long-ago-is-looked-at-and-placed-against
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (bd/setup {:self body :blocks {"2,66,0" "stone"} :inventory cobble})
              floor [2 66 0]
              shown (atom false)]
          (aset p "sensedAt" (fn [pos]
                               (if (and (= floor [(.-x pos) (.-y pos) (.-z pos)]) (not @shown))
                                 #js {:name "air" :ageMs 5000}
                                 (.blockAt p pos))))
          (.override (.-world p) "look" (fn [token a impl] (reset! shown true) (impl token a)))
          (let [result (await (tu/child-outcome eng job {:pos [2 67 0] :item "cobblestone"} 8))]
            (is (= :placed (:reason result)))
            (is (= "cobblestone" (bd/block-at p {:x 2 :y 67 :z 0})))))))))

(deftest bad-args-are-never-from-can-start
  (are [args] (= [:never :bad-args] (tu/kind-reason (place/can-start? {:args args :primitives (:p (bd/setup {:self body}))})))
    {:item "cobblestone"}
    {:pos "x" :item "cobblestone"}))

(deftest no-item-named-ends-impossible-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (bd/setup {:self body})]
          (is (= {:kind :impossible :cause :bad-args} (update (await (ending-of {:eng eng} {:pos at} 3)) :cause :reason))))))))

(deftest a-facing-turns-the-body-so-the-block-is-oriented-that-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [facing ["north" "south" "east" "west"]]
          (let [{:keys [eng]} (bd/setup {:self body :inventory [{:name "oak_stairs" :count 1}]})
                result (await (tu/child-outcome eng job {:pos [2 64 0] :item "oak_stairs" :facing facing} 5))]
            (is (= facing (get-in result [:block :state :facing])))))))))

(deftest facing-with-click-is-bad-args
  (are [args] (= [:never :bad-args] (tu/kind-reason (place/can-start? {:args (merge {:pos [2 64 0] :item "oak_stairs"} args) :primitives (:p (bd/setup {:self body}))})))
    {:facing "east" :click {:against [2 63 0] :cursor [0.5 1 0.5]}}))

(deftest a-cell-of-the-bodys-own-plan-is-placed-another-bodys-plan-refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "hut" :parts [{:id "w" :cells [[2 64 0]] :want "cobblestone"}]}
              own (bd/setup {:self body :inventory cobble :plans {"hut" (shape/with-author plan "Fake")}})
              other (bd/setup {:self body :inventory cobble :plans {"hut" (shape/with-author plan "Miles")}})]
          (is (:placed (await (tu/child-outcome (:eng own) job {:pos at :item "cobblestone"} 5))))
          (is (= {:reason :not-allowed :by :footprint :plan "hut"}
                 (select-keys (bd/wait-cause (await (bd/waiting-after other (list job {:pos at :item "cobblestone"}) 3))) [:reason :by :plan]))))))))

(deftest a-far-cell-is-walked-to-and-placed-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble})]
          (is (await (bd/ended-in-one-tick? env (list job {:pos [12 64 0] :item "cobblestone"}))))
          (is (= "cobblestone" (bd/block-at (:p env) {:x 12 :y 64 :z 0}))))))))

(deftest a-place-never-gives-the-round-back-for-its-step-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory cobble})]
          (is (await (settings/with-settings {:jobs.blocks.place/max-steps 1}
                       #(bd/ended-in-one-tick? env (list job {:pos [12 64 0] :item "cobblestone"}))))))))))

(deftest a-walk-that-fails-twice-in-one-call-ends-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walks (atom 0)
              env (bd/failing-walks (bd/setup {:self body :inventory cobble}) walks)
              e (await (ending-of env {:pos [12 64 0] :item "cobblestone"} 5))]
          (is (= 2 @walks) "one more try, then it ends")
          (is (= {:kind :nothing-possible :cause {:reason :unreachable :why :no-path}} e)))))))

(deftest a-top-level-place-whose-walk-has-no-way-ends-instead-of-waiting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [go-to (get (:jobs (:eng (bd/setup {:self body}))) 'jobs.movement.go-to)
              base (bd/setup {:self body :inventory cobble})
              stub (fn ^:async shut-in-walk [c]
                     (ctx/end c :nothing-possible {:reason :unreachable :why :exhausted} {:arrived false :why :exhausted :text "the body is shut in and this walk may not dig or build out"}))
              env (assoc-in base [:eng :jobs 'jobs.movement.go-to] (assoc go-to :round stub))
              wait (await (bd/waiting-after env (list job {:pos [12 64 0] :item "cobblestone"}) 8))]
          (is (nil? wait) "a shut-in body is a verdict, not a wait")
          (is (re-find #"shut in" (:text (:data (:ending (await (tu/child-outcome (:eng env) job {:pos [12 64 0] :item "cobblestone"} 8)))))) "the ending says why")
          (is (empty? (:list (core/state (:eng env)))) "the job ended"))))))

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
              result (await (tu/child-outcome eng job {:pos [0 64 0] :item "wheat_seeds"} 5))]
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

(deftest a-plans-place-keeps-off-the-beds-and-bottom-slabs-round-the-cell
  (let [p (tu/seeing-all (tu/fake-on-floor {:blocks {"13,64,8" "white_bed" "14,64,8" "oak_slab" "15,64,8" "oak_slab" "16,64,8" "stone"}
                                                    :states {"14,64,8" {:type "bottom"} "15,64,8" {:type "top"}}
                                                    :self {:pos {:x 12.5 :y 64 :z 14.5}}}))]
    (is (= [{:x 13 :y 64 :z 8} {:x 14 :y 64 :z 8}] (b/low-floor-cells {:primitives p} {:x 14 :y 66 :z 8})))))

(defn gate-clicks [p] (count (filterv #(= "useOn" (.-name %)) (.-calls (.-world p)))))

(deftest a-place-beyond-a-pen-works-from-outside-without-using-its-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (pen-env)
              result (await (tu/child-outcome eng job {:pos [16 64 0] :item "dirt"} 40))]
          (is (= true (:placed result)))
          (is (zero? (gate-clicks p)) "the gates stay shut, the walk went round"))))))

(def pens-open {:jobs.lib.blocks/enter-pens true})

(deftest a-place-inside-a-pen-from-outside-declines-naming-the-pen-and-enter-pens-allows-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (pen-env)
              declined (await (tu/child-outcome eng job {:pos [9 64 0] :item "dirt"} 40))]
          (is (not= true (:placed declined)))
          (is (zero? (gate-clicks p)))
          (is (some #(and (= :blocks.pen-kept-out (:kind %)) (re-find #"\[5 64 0\]" (:text %))) @seen)))
        (let [{:keys [eng]} (pen-env)
              allowed (await (settings/with-settings pens-open #(tu/child-outcome eng job {:pos [9 64 0] :item "dirt"} 40)))]
          (is (= true (:placed allowed))))))))

(def use-on 'jobs.items.use)

(deftest a-use-on-the-gate-or-near-the-fence-works-from-outside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [pos [[5 64 0] [7 63 2]]]
          (let [{:keys [eng p seen]} (pen-env)
                result (await (tu/child-outcome eng use-on {:on pos} 40))]
            (is (not= :unreachable (:reason result)) (pr-str pos))
            (is (= 1 (gate-clicks p)) (pr-str pos))
            (is (empty? (filter #(= :blocks.pen-kept-out (:kind %)) @seen)) (pr-str pos))))))))

(deftest a-use-on-deep-inside-a-pen-declines-naming-the-pen-and-enter-pens-allows-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (pen-env)
              declined (await (tu/child-outcome eng use-on {:on [10 63 0]} 40))]
          (is (= :unreachable (:reason declined)))
          (is (zero? (gate-clicks p)))
          (is (some #(and (= :blocks.pen-kept-out (:kind %)) (re-find #"\[5 64 0\]" (:text %))) @seen)))
        (let [{:keys [eng]} (pen-env)
              allowed (await (settings/with-settings pens-open #(tu/child-outcome eng use-on {:on [10 63 0]} 40)))]
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
                parent {:round (fn ^:async r [c]
                                 (let [e (await (ctx/call c :kid use-on {:on at}))]
                                   (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause])) (ctx/end c :done nil)))))}
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
              parent {:round (fn ^:async r [c]
                               (doseq [[j args] [[job {:pos at :item "cobblestone"}] [job {:pos [30 64 0] :item "cobblestone"}]
                                                 [use-on {:on [30 64 0]}]]]
                                 (swap! out conj (await (ctx/quote c j args))))
                               (ctx/end c :done nil))}
              eng (assoc-in (:eng env) [:jobs 'recording-parent] parent)]
          (core/submit! eng '(recording-parent) {})
          (await (tu/run-until-empty eng 5))
          (let [[near far use] @out]
            (is (= {"cobblestone" [1 1 1]} (get-in near [:uses :items])))
            (is (< (second (get-in near [:uses :job-time])) (second (get-in far [:uses :job-time]))))
            (is (< (second (get-in near [:uses :job-time])) (second (get-in use [:uses :job-time]))))))))))

(deftest a-use-on-with-a-bucket-pours-and-scoops-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory [{:name "water_bucket" :count 1}]})
              poured (await (tu/child-outcome (:eng env) use-on {:on at :item "water_bucket"} 40))]
          (is (= [true :used] ((juxt :used :reason) poured)))
          (is (= "water" (.-name (.blockAt (:p env) (clj->js at)))))
          (let [scooped (await (tu/child-outcome (:eng env) use-on {:on at :item "bucket"} 40))]
            (is (= [true "water_bucket"] [(:used scooped) (some #(when (= "water_bucket" (:name %)) (:name %)) (u/inventory (:p env)))]))
            (is (not= "water" (.-name (.blockAt (:p env) (clj->js at)))))))))))

(deftest a-use-on-bucket-with-a-face-pours-in-the-cell-beside-the-clicked-face
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory [{:name "water_bucket" :count 1}]})
              floor (update at :y dec)
              poured (await (tu/child-outcome (:eng env) use-on {:on floor :face "up" :item "water_bucket"} 40))]
          (is (= [true at] ((juxt :used :pos) poured)))
          (is (= "water" (.-name (.blockAt (:p env) (clj->js at))))))))))

(deftest a-use-on-bucket-in-anothers-zone-waits-not-allowed-but-a-plain-click-does-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory [{:name "water_bucket" :count 1}] :zones [bd/farm-zone]})]
          (is (= {:reason :not-allowed :pos at :by :zone :zone "farm" :owner "Miles"}
                 (bd/wait-cause (await (bd/waiting-after env (list use-on {:on at :item "water_bucket"}) 3)))))
          (is (empty? (bd/calls (:p env) "place"))))
        (let [env (bd/setup {:self body :inventory [{:name "water_bucket" :count 1}] :zones [bd/farm-zone]})
              result (await (tu/child-outcome (:eng env) use-on {:on at :item "water_bucket" :ignore-zones? true} 40))]
          (is (= true (:used result))))
        (let [env (bd/setup {:self body :zones [bd/farm-zone]})
              result (await (tu/child-outcome (:eng env) use-on {:on at} 40))]
          (is (not= :not-allowed (:reason result))))))))

(deftest a-use-on-scoop-asks-the-harvest-rule-not-the-place-rule
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zone (assoc bd/farm-zone :allow #{:harvest})
              scoop (bd/setup {:self body :inventory [{:name "bucket" :count 1}] :blocks {"2,64,0" "water"} :zones [zone]})
              pour (bd/setup {:self body :inventory [{:name "water_bucket" :count 1}] :zones [zone]})]
          (is (= true (:used (await (tu/child-outcome (:eng scoop) use-on {:on at :item "bucket"} 40)))))
          (is (= :not-allowed (:reason (bd/wait-cause (await (bd/waiting-after pour (list use-on {:on at :item "water_bucket"}) 3)))))))))))

(deftest a-use-on-with-a-bucket-ends-by-the-place-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [env (bd/setup {:self body :inventory [{:name "bucket" :count 1}]})
              scooped (await (tu/child-outcome (:eng env) use-on {:on at :item "bucket"} 40))]
          (is (= [:stopped :missing] [(:status scooped) (:reason scooped)])))))))

(deftest a-bed-with-no-facing-turns-to-the-side-its-head-has-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (bd/setup {:self body :inventory [{:name "white_bed" :count 1}]
                                       :blocks {"2,64,-1" "stone" "2,64,1" "stone" "1,64,0" "stone"}})
              result (await (tu/child-outcome eng job {:pos [2 64 0] :item "white_bed"} 5))]
          (is (= true (:placed result)))
          (is (= "east" (get-in result [:block :state :facing]))))))))
