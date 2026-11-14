(ns engine.walk-back-search-test
  "The walk back to a place (jobs.lib.walk-pick/walk-back-worth?) and the dig-route searches it can leave running."
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box]]
            [engine.mine-support :refer [soil-over-stone]]
            [jobs.gather.mine.descent :as descent]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.walk-pick :as walk-pick]
            [jobs.movement.go-to.escalate-choose :as choose]))

(defn quoting [mean] (let [q (js/Promise.resolve {:uses {:job-time [0 mean ##Inf]} :route {:found true}})] (fn ([_ _ _] q) ([_ _ _ _] q))))

(defn ^:async worth [route ways]
  (with-redefs [ctx/quote (let [q (js/Promise.resolve {:uses {:job-time [0 10 ##Inf]} :route route})] (fn ([_ _ _] q) ([_ _ _ _] q)))]
    (await (walk-pick/walk-back-worth? {:primitives (tu/fake {})} {:pos [0 0 0] :range 0 :ways ways} 20))))

(deftest a-route-not-found-is-a-cheap-walk-back-only-when-its-ways-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (await (worth {:found true} #{:walk}))))
        (is (false? (await (worth {:found false :reason :no-path} #{:walk}))) "a dead end walked is no walk back")
        (is (true? (await (worth {:found false :reason :no-path :escalating true} #{:walk :clear-seal :dig-route}))) "a dig finds the rest")
        (is (= :searching (await (worth {:found false :reason :searching} #{:walk}))))))))

(deftest a-walk-back-is-priced-by-its-risk-too
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [ctx/quote (let [q (js/Promise.resolve {:uses {:job-time [0 10 ##Inf]} :route {:found true} :risk {:hp 10 :lethal-hp 0}})]
                                  (fn ([_ _ _] q) ([_ _ _ _] q)))]
          (is (false? (await (walk-pick/walk-back-worth? {:primitives (tu/fake {})} {:pos [0 0 0] :range 0 :ways #{:walk}} 20)))
              "10 s of walk that costs half the body's health is dearer than 20 s of work"))))))

;; Brindle S20: stone 5 under its lawn, it walked 100 blocks to the foot of an old stair 15 deep because cutting 15 steps
;; was dearer; a stair to stone stops at the stone.
(deftest a-stone-dig-walks-its-old-stair-only-when-that-is-cheaper-than-a-stair-down-to-stone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:blocks (soil-over-stone 63 59) :self {:pos [0.5 64 0.5]}}))
              old-stair [{:data {:dim "overworld" :floors (vec (for [i (range 1 11)] [i (- 63 i) 0]))}}]
              bottom (fn ^:async bottom [args] (with-redefs [ctx/entries (constantly old-stair) ctx/quote (quoting 25) ctx/mem (constantly {})
                                                             ctx/update-mem! (fn [& _])]
                                          (await (descent/own-stair-bottom {:primitives p :args args}))))]
          (is (nil? (await (bottom {:block "stone" :descend-limit 12})))
              "5 steps to the seen stone cost 18 s, less than the 25 s walk to the foot 10 below")
          (is (= [10 54 0] (:cell (await (bottom {:block "iron_ore" :level 40}))))
              "to an ore's level the walk saves cutting all 10 steps (36 s)"))))))

;; Brindle S2: stone 15 off, mine quoted a walk to each of 8 old stairs 400-800 blocks away every round, and each round began
;; again at the first: 315 rounds of the same partial search, no dig.
(deftest a-stair-way-too-far-for-its-least-walk-is-passed-unquoted-and-one-answered-is-not-asked-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:blocks (soil-over-stone 63 59) :self {:pos [0.5 64 0.5]}}))
              stair (fn [x d] {:data {:dim "overworld" :floors (vec (for [i (range 1 (inc d))] [(+ x i) (- 63 i) 0]))}})
              m (atom {})
              asked (atom [])
              bottom (fn ^:async bottom [ways answer]
                       (with-redefs [ctx/entries (constantly ways) ctx/mem (fn [_] @m) ctx/update-mem! (fn [_ f & args] (apply swap! m f args))
                                     ctx/quote (let [q (fn [_ _ args] (swap! asked conj (:x (:pos args))) (js/Promise.resolve (answer (:x (:pos args)))))]
                                                 (fn ([c s a] (q c s a)) ([c s a _] (q c s a))))]
                         (await (descent/own-stair-bottom {:primitives p :args {:block "iron_ore" :level 40}}))))]
          (is (nil? (await (bottom [(stair 400 10)] (constantly nil)))))
          (is (empty? @asked) "36 s of steps is less than the least walk of 400 blocks: no quote")
          (let [ans (fn [x] (if (= x 12) {:uses {:job-time [0 99 ##Inf]} :route {:found true}} {:route {:found false :reason :searching}}))
                ways [(stair 0 12) (stair 1 10)]]
            (is (= :searching (await (bottom ways ans))))
            (is (= :searching (await (bottom ways ans))))
            (is (= [12 11 11] @asked) "the deeper way, too dear, is quoted once; the other's search goes on")))))))

(def open-ground (merge (box -1 64 -2 12 80 2 "air") (box -1 63 -2 12 63 2 "stone")))

(deftest a-dig-route-search-ends-when-the-heap-is-full
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        search dig-route/search
        seen (atom [])
        ask #(choose/route-answer p [0 64 0] [10 70 0] (constantly true) (constantly false) false false
                                  (assoc (choose/search-opts {:slice (choose/route-slice (str (random-uuid)))}) :pillar-s (fn [_ _ _ _] 1.5) :place-s 1))]
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      #(with-redefs [dig-route/search (fn [b f g opts]
                                        (swap! seen conj (:read-s opts))
                                        (let [run (search b f g opts)]
                                          (fn [n] (let [[a :as r] (run n)] (when a (swap! seen conj (:problem a))) r))))]
         (is (= :searching (ask)) "a search that spends its nodes goes on")
         (with-redefs [dig-route/heap-full? (constantly true)]
           (is (nil? (ask)) "a full heap ends it as no route")
           (is (= :too-big (peek @seen)) "with the reason too-big")
           (is (pos? (first @seen)) "its nodes are priced"))))))

(deftest a-kept-search-goes-on-as-the-chains-time-left-shrinks
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        seeded-search dig-route/seeded-search
        starts (atom 0)
        id (str (random-uuid))
        ask #(choose/route-answer p [0 64 0] [10 70 0] (constantly true) (constantly false) false false
                                  (assoc (choose/search-opts {:slice (choose/route-slice id)}) :bound-s %))]
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      ;; a climb's search begins as seeded-search (its stages are searches of one question)
      #(with-redefs [dig-route/seeded-search (fn [& args] (swap! starts inc) (apply seeded-search args))]
         (ask 100)
         (ask 99.6)
         (is (= 1 @starts) "the second round asks the same search, not a new one")
         (is (nil? (ask 0.5)) "the kept search goes on under the bound as it falls: no route is within 0.5 s")
         (is (= 1 @starts))))))

(deftest a-search-goes-no-further-than-the-worth-a-route-must-be-within
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        ask #(choose/route-answer p [0 64 0] [10 70 0] (constantly true) (constantly false) false false
                                  (assoc (choose/search-opts {:slice (choose/route-slice (str (random-uuid))) :bound-s 100 :guess-risk {} :worth-s %}) :pillar-s (fn [_ _ _ _] 1.5) :place-s 1))]
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      #(do (is (= :searching (ask 100)) "within the worth the search goes on")
           (is (nil? (ask 0.5)) "no route is within 0.5 s: the search ends at once")))))

(deftest a-search-ended-under-a-worth-is-not-the-answer-to-an-ask-without-one
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        id (str (random-uuid))
        ask #(choose/route-answer p [0 64 0] [10 70 0] (constantly true) (constantly false) false false
                                  (assoc (choose/search-opts {:slice (choose/route-slice id) :bound-s 100 :guess-risk {}}) :worth-s % :pillar-s (fn [_ _ _ _] 1.5) :place-s 1))]
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      #(do (is (nil? (ask 0.5)))
           (is (= :searching (ask nil)) "the same ask under no worth searches on its own")))))
