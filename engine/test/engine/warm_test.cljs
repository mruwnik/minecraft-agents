(ns engine.warm-test
  "jobs.lib.warm: the tables built before the first danger are the ones the danger path asks for."
  (:require [cljs.test :refer [deftest is]]
            [engine.game :as game]
            [jobs.lib.cost.caught :as caught]
            [jobs.lib.cost.table :as table]
            [jobs.lib.warm :as warm]))

(deftest the-warmed-item-table-is-the-one-item-value-asks-for
  (warm/warm!)
  (is (identical? (table/table-for @game/version {} nil)
                  (table/table-for @game/version (table/normal-overrides nil) nil))
      "item-value with no overrides or basis asks the table warm! built"))

(deftest every-group-prior-reads-has-its-posterior-warmed
  (let [warmed (atom #{})
        asked (atom #{})
        record (fn [into] (fn [counts] (swap! into conj counts) {:points #js [] :blows 1 :shape 1}))]
    (with-redefs [caught/posterior (record warmed)] (warm/warm!))
    (with-redefs [caught/posterior (record asked) caught/chance (constantly 0.5)]
      (doseq [which [::caught/flights ::caught/fights]
              mob ["zombie" "husk" "skeleton" "stray" "spider" "cave_spider" "creeper" "enderman"]]
        (caught/prior which mob 20)))
    (is (seq @asked))
    (is (= @asked @warmed) "the first danger's prices (caught/prior) never compute a posterior warm! left out")))
