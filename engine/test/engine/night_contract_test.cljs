(ns engine.night-contract-test
  "jobs.survival.night as a contract job: it exports the contract, ends with a kind and a cause, and takes only the
  ways (:bed :shelter :flee) the call allows."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.hut-shelter-test :as hut]
            [engine.memory :as mem]
            [engine.night-support :as nt]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.clock :as sh-clock]))

(def night-job 'jobs.survival.night)

(deftest the-quote-is-the-time-left-of-the-night
  (let [q ((:quote-of (get registry/jobs night-job)) {:primitives (tu/fake {:time st/night}) :args {}})
        [least mean most] (get-in q [:uses :job-time])]
    (is (nil? (contract/quote-problem q)))
    (is (= 0 least) "a bed can end it at once")
    (is (= (sh-clock/ticks->s (- sh-clock/day-tick st/night)) mean))
    (is (< mean most) "the morning's leaving on top")))

(deftest a-call-without-the-bed-way-does-not-sleep
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup hut/night-bed-world)]
          (st/know-bed! eng {:x 6 :y 64 :z 0})
          (core/submit! eng (list night-job {:ways #{:shelter :flee}}) {})
          (await (tu/run-until-empty eng 8))
          (is (= [] (st/calls p "sleep"))))))))

(defn ^:async ending-of
  "Run the night with args as a ctx/call child of a recording parent on eng: the child's {:kind :cause :data}."
  [eng args]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid night-job args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) (ctx/end c :done nil)))))}
        eng (assoc-in eng [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng 40))
    @out))

(deftest a-call-without-the-flee-way-holds-exposed-and-ends-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup {:time st/night :blocks st/ground :inventory [{:name "iron_pickaxe" :count 1}]})]
          (st/refuse-placing! p)
          (st/dawn-after! p 4)
          (let [e (await (ending-of eng {:ways #{:bed :shelter}}))]
            (is (= {:kind :nothing-possible :cause :exposed} (select-keys e [:kind :cause])) (pr-str e))
            (is (< 1 (count (distinct (map :pos (get-in e [:data :sites]))))) "more than one site tried, then exposed")
            (is (empty? (st/emitted seen :shelter.fled)))
            (is (= 1 (count (st/emitted seen :shelter.exposed))))))))))

(deftest a-call-without-the-shelter-way-flees-without-digging-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (nt/flee-home-night (update nt/dark-world :blocks merge {"10,66,0" "stone"})
                                                             {:ways #{:bed :flee}}))
              log (await (nt/with-child-log #(tu/run-until-empty eng 80)))]
          (is (empty? (filter #(#{:dig-in :niche :relocate} (first %)) log)) "no pit, niche or relocation")
          (is (= {:x 10 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z])) "walked to the roofed home")
          (is (= [:roofed-place] (mapv :target (st/emitted seen :shelter.fled)))))))))

(deftest an-empty-ways-set-is-bad-args
  (is (some? (contract/call-problem night-job (:ways (get registry/jobs night-job)) {:ways #{}}))))

(deftest a-call-without-the-bed-way-has-no-bed-terms
  (let [{:keys [eng p]} (st/setup hut/night-bed-world)
        _ (st/know-bed! eng {:x 6 :y 64 :z 0})
        _ (st/looked-round! eng p)
        holds #(boolean (sh/night-holds? p (mem/view (:store eng)) % (constantly true)))]
    (is (true? (holds {})) "a bed in reach holds the night")
    (is (false? (holds {:ways #{:shelter :flee}})) "without :bed it does not")))
