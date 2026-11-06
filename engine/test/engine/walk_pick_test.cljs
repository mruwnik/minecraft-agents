(ns engine.walk-pick-test
  "jobs.lib.walk-pick: candidates quoted best-first by their bound, until the best quoted is no dearer than every bound left."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.library-test :as lib]
            [engine.test-util :as tu]
            [jobs.lib.clock :as clock]
            [jobs.lib.rooms :as rooms]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]
            [jobs.movement.go-to.preview :as pv]))

(defn ^:async picked
  "[best! of candidates {:id :bound :s} with go-to's quote stubbed to {:s s} (its :on-quote called first), the ids quoted in order]."
  [candidates]
  (let [quote! pick/quote!
        asked (atom [])]
    (set! pick/quote! (fn [_ x] (some-> (:on-quote x) (apply [])) (swap! asked conj (:id x)) (js/Promise.resolve {:s (:s x) :estimate (:estimate x)})))
    (try [(await (pick/best! nil candidates {:bound :bound :args identity :like :kind :price (fn [_ q] (:s q))})) @asked]
         (finally (set! pick/quote! quote!)))))

(defn ^:async with-death-chance
  "(f)'s value with every go-to quote walk-pick asks for (pick/quote!) saying its walk ends in death with chance p."
  [p f]
  (let [quote! pick/quote!]
    (set! pick/quote! (fn ^:async q [c args] (assoc-in (await (quote! c args)) [:risk :death-chance] p)))
    (try (await (f))
         (finally (set! pick/quote! quote!)))))

(deftest the-quoted-best-stops-the-quotes-once-no-bound-left-can-beat-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [near {:id :near :bound 1 :s 40}
              mid {:id :mid :bound 4 :s 6}
              far {:id :far :bound 8 :s 2}
              [best asked] (await (picked [far near mid]))]
          (is (= :mid (:id (:pick best))) "the nearest is dear on its way: the next is cheaper, and the farthest's bound already dearer")
          (is (= 6 (:price best)))
          (is (= [:near :mid] asked) "the farthest is never quoted"))
        (let [[best asked] (await (picked [{:id :a :bound 1 :s ##Inf} {:id :b :bound 2 :s ##Inf}]))]
          (is (nil? best) "no candidate with a finite price")
          (is (= [:a :b] asked)))))))

(deftest quotes-go-on-only-while-the-saving-they-promise-beats-what-a-quote-costs
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; a night flee's way: each walk's price is three times its bound and a fixed 6 s to leave, so the nearest stays
        ;; the best and every bound under its 6.6 s could still beat it
        (let [cands (mapv (fn [i] (let [b (* 0.2 i)] {:id i :bound b :s (+ 6 (* 3 b))})) (range 1 101))
              stopped clock/now
              ms (atom 0)
              [best asked] (try (set! clock/now #(deref ms))
                                (await (picked (map #(assoc % :on-quote (fn [] (swap! ms + 150))) cands)))
                                (finally (set! clock/now stopped)))]
          (is (= 1 (:id (:pick best))))
          (is (= 18 (count asked)) "with each quote 0.15 s, the 19th's saving, (6.6 - 3.8) / 19 s, is less")
          (is (= 32 (count (second (await (picked cands))))) "a quote that costs nothing goes on to the bound"))))))

(deftest a-quote-still-searching-gives-no-price-ratio-to-its-kind
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; id 2's quote is a search's estimate (no price): its ##Inf ratio would cut the kind's expected saving to a third
        ;; and stop the quoting at id 2; a quote costs 2.5 s, id 3's saving (6.6 - 0.6) / 2 = 3 s beats it
        (let [ms (atom 0)
              cands (mapv (fn [i] (let [b (* 0.2 i)] (cond-> {:id i :bound b :s (+ 6 (* 3 b)) :kind :a :on-quote #(swap! ms + 2500)}
                                                         (= 2 i) (assoc :s ##Inf :estimate true)))) (range 1 11))
              stopped clock/now
              [_ asked] (try (set! clock/now #(deref ms))
                             (await (picked cands))
                             (finally (set! clock/now stopped)))]
          (is (= [1 2 3] asked)))))))

(deftest pick-walk-takes-the-place-whose-quoted-walk-is-cheapest-not-the-nearest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [c {:primitives (tu/fake {})}
              quote! pick/quote!
              asked (atom [])
              round {:x 3 :y 64 :z 0}
              straight {:x 9 :y 64 :z 0}
              to (fn [pos] {:pos pos :range 0})]
          (set! pick/quote! (fn [_ {:keys [pos]}] (swap! asked conj pos) (js/Promise.resolve {:uses {:job-time [0 (if (= pos round) 60 4) 0]}})))
          (try
            (is (= straight (await (pick/pick-walk! c [round straight] to))) "the near place's way round costs more than the far one's walk")
            (is (= straight (await (pick/pick-walk! c [round straight] to #(if (= % round) 0 2)))) "seconds after the walk add to its price")
            (reset! asked [])
            (is (= round (await (pick/pick-walk! c [round] to))))
            (is (empty? @asked) "a lone place is taken unquoted")
            (is (nil? (await (pick/pick-walk! c [] to))))
            (finally (set! pick/quote! quote!))))))))

(deftest by-bound-orders-as-sort-by-asking-each-bound-once
  (let [xs (conj (mapv (fn [i] {:id i :b (mod (* 7 i) 5)}) (range 20)) {:id :far :b ##Inf} {:id :near :b -1})
        asked (atom 0)]
    (is (= (sort-by :b xs) (pick/by-bound #(do (swap! asked inc) (:b %)) xs)) "ties keep their order")
    (is (= (count xs) @asked))))

(deftest a-quoted-walk-costs-its-time-and-the-hp-it-takes-and-risks
  (let [c {:primitives (tu/fake {})}
        q {:uses {:job-time [4 5 15] :health [0 1 1]} :risk {:hp 2}}]
    (is (< (pick/quoted-s c (assoc-in q [:risk :hp] 0)) (pick/quoted-s c q)) "risk adds to the time")
    (is (= 3005 (pick/quoted-s c {:uses {:job-time [4 5 15]} :risk {:hp 300 :lethal-hp 300}}))
        "a death chance is priced whole at an hp's seconds, not capped at the body's health like its hurt")
    (is (= 5 (pick/quoted-s c {:uses {:job-time [4 5 15]}})) "no hp: the mean time")
    (is (zero? (pick/quoted-s c (contract/ending :done nil nil))) "already there")
    (is (infinite? (pick/quoted-s c (contract/ending :nothing-possible {:reason :no-path} nil))))
    (is (infinite? (pick/quoted-s c {:uses {:job-time [4 ##Inf ##Inf]}})))))

(deftest a-quote-that-spent-its-node-budget-is-priced-and-no-way-the-pick-gives-up-on
  (let [c {:primitives (tu/fake {})}
        q (pv/quote-of-route {:found false :reason :budget :near 30 :goal [30 64 0]} false false)]
    (is (js/isFinite (pick/quoted-s c q)) "the pick prices the walk, it is not out of reach")
    (is (not (pick/no-way? q)) "no proof that the land is closed")))

(deftest the-bound-is-never-more-than-go-tos-quote-of-a-straight-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stair (into {} (for [x (range 4 14) y (range 64 (+ 64 (- x 3)))] [(str x "," y ",0") "stone"]))
              {:keys [eng]} (lib/setup {:floor tu/walk-floor :blocks stair})
              out (atom [])
              walks [{:pos {:x 14 :y 64 :z 0} :range 3} {:pos {:x -20 :y 64 :z 5} :range 6} {:pos {:x 30 :y 64 :z 0} :range 0}
                     {:pos {:x 13 :y 74 :z 0} :range 0}]
              parent {:round (fn ^:async r [c]
                               (doseq [args walks]
                                 (swap! out conj [(pick/walk-s (u/self-pos c) args) (second (get-in (await (pick/quote! c args)) [:uses :job-time]))]))
                               (ctx/end c :done nil))}]
          (let [eng (assoc-in eng [:jobs 'quoting-parent] parent)]
            (core/submit! eng '(quoting-parent) {})
            (await (tu/run-until-empty eng 10)))
          (is (= 4 (count @out)) "the last walk climbs a ten-block stair: its height is free in the bound")
          (doseq [[bound mean] @out]
            (is (< 0 bound mean) "a sprint over the flat blocks less the range: under the planner's seconds")))))))

(deftest a-walk-costing-more-than-the-pass-is-worth-is-not-worth
  (let [c {:primitives (tu/fake {})}
        far {:pos {:x 3000 :y 64 :z 0}}]
    (is (= :not-worth (:reason (pick/not-worth c far (fn [] 1)))))
    (is (nil? (pick/not-worth c far (fn [] ##Inf))) "an endless worth affords any finite walk")))

(deftest a-quote-carries-the-walks-args-as-they-are
  ;; a quote that adds a worth the walk lacks could find a route the walk then refuses at the job's worth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen (atom [])]
          (with-redefs [ctx/quote (fn ([_ _ args] (swap! seen conj args) (js/Promise.resolve {})) ([_ _ args _] (swap! seen conj args) (js/Promise.resolve {})))]
            (await (pick/quote! nil {:pos {:x 1 :y 64 :z 1}}))
            (await (pick/quote! nil {:pos {:x 1 :y 64 :z 1} :worth 7})))
          (is (= [nil 7] (mapv :worth @seen))))))))

(deftest a-route-cost-quote-is-asked-at-the-walks-own-args
  ;; enter-room quotes with route-cost and walks with the same args: a worth only on the quote would let it say found for a walk that refuses
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen (atom [])]
          (with-redefs [ctx/quote (fn ([_ _ args] (swap! seen conj args) (js/Promise.resolve {})) ([_ _ args _] (swap! seen conj args) (js/Promise.resolve {})))]
            (await (rooms/route-cost nil {:pos {:x 1 :y 64 :z 1}})))
          (is (= [{:pos {:x 1 :y 64 :z 1}}] @seen)))))))

(deftest cheapest-names-a-priced-pick-though-the-reach-search-still-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [c {:primitives (tu/fake {})}
              none-reachable! pick/none-reachable!
              no-route {:route {:found false :reason :exhausted} :uses {:job-time [0 5 5]}}
              xs [{:x 0 :y 64 :z 6} {:x 0 :y 64 :z 9} {:x 0 :y 64 :z 12}]
              cheapest (fn [] (pick/cheapest! c xs {:target (fn [x] {:pos x :range 0}) :walk (fn [x] {:pos x}) :ok? (constantly true)
                                                    :quote (fn [_ args] (js/Promise.resolve (if (= 6 (:z (:pos args))) no-route {:uses {:job-time [0 20 20]}})))}))]
          (set! pick/none-reachable! (fn [_ _] (js/Promise.resolve :searching)))
          (try (is (some? (:pick (await (cheapest)))) "a priced walk wins over a search still going on")
               (finally (set! pick/none-reachable! none-reachable!))))))))
