(ns engine.walk-pick-test
  "jobs.lib.walk-pick: candidates quoted best-first by their bound, until the best quoted is no dearer than every bound left."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.library-test :as lib]
            [engine.test-util :as tu]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]))

(defn ^:async picked
  "[best! of candidates {:id :bound :s} with go-to's quote stubbed to {:s s}, the ids quoted in order]."
  [candidates]
  (let [quote! pick/quote!
        asked (atom [])]
    (set! pick/quote! (fn [_ x] (swap! asked conj (:id x)) (js/Promise.resolve {:s (:s x)})))
    (try [(await (pick/best! nil candidates {:bound :bound :args identity :price (fn [_ q] (:s q))})) @asked]
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
