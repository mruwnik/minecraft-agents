(ns engine.walk-back-search-test
  "The walk back to a place (jobs.lib.price/walk-back-worth?) and the dig-route searches it can leave running."
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.test-util :as tu :refer [box]]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.price :as price]
            [jobs.movement.go-to.escalate-choose :as choose]))

(defn ^:async worth [route ways]
  (with-redefs [ctx/quote (let [q (js/Promise.resolve {:uses {:job-time [0 10 ##Inf]} :route route})] (fn ([_ _ _] q) ([_ _ _ _] q)))]
    (await (price/walk-back-worth? {} {:pos [0 0 0] :range 0 :ways ways} 20))))

(deftest a-route-not-found-is-a-cheap-walk-back-only-when-its-ways-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (true? (await (worth {:found true} #{:walk}))))
        (is (false? (await (worth {:found false :reason :no-path} #{:walk}))) "a dead end walked is no walk back")
        (is (true? (await (worth {:found false :reason :no-path :escalating true} #{:walk :clear-seal :dig-route}))) "a dig finds the rest")
        (is (= :searching (await (worth {:found false :reason :searching} #{:walk}))))))))

(def open-ground (merge (box -1 64 -2 12 80 2 "air") (box -1 63 -2 12 63 2 "stone")))

(deftest a-dig-route-search-ends-when-the-heap-is-full
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        search dig-route/search
        seen (atom [])
        ask #(choose/route-answer p [0 64 0] [10 70 0] (constantly true) (constantly false) false false
                                  (assoc (choose/search-opts {:slice (choose/route-slice (str (random-uuid)))}) :pillar-s (fn [_ _ _ _] 1.5) :place-s 1))]
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      #(with-redefs [dig-route/search (fn [b f g opts]
                                        (swap! seen conj (:node-s opts))
                                        (let [run (search b f g opts)]
                                          (fn [n] (let [[a :as r] (run n)] (when a (swap! seen conj (:problem a))) r))))]
         (is (= :searching (ask)) "a search that spends its nodes goes on")
         (with-redefs [dig-route/heap-full? (constantly true)]
           (is (nil? (ask)) "a full heap ends it as no route")
           (is (= :too-big (peek @seen)) "with the reason too-big")
           (is (pos? (first @seen)) "its nodes are priced"))))))

(deftest a-kept-search-goes-on-as-the-chains-time-left-shrinks
  (let [p (tu/fake {:blocks open-ground :self {:pos {:x 0.5 :y 64 :z 0.5}}})
        search dig-route/search
        starts (atom 0)
        id (str (random-uuid))
        ask #(choose/route-answer p [0 64 0] [10 70 0] (constantly true) (constantly false) false false
                                  (assoc (choose/search-opts {:slice (choose/route-slice id)}) :bound-s %))]
    (settings/with-settings {:jobs.lib.dig-route/max-nodes 5}
      #(with-redefs [dig-route/search (fn [& args] (swap! starts inc) (apply search args))]
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
