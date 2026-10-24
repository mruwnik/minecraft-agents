(ns engine.warm-test
  "jobs.lib.warm: the tables built before the first danger are the ones the danger path asks for."
  (:require [cljs.test :refer [deftest is]]
            [engine.game :as game]
            [jobs.lib.cost.table :as table]
            [jobs.lib.warm :as warm]))

(deftest the-warmed-item-table-is-the-one-item-value-asks-for
  (warm/warm!)
  (is (identical? (table/table-for @game/version {} nil)
                  (table/table-for @game/version (table/normal-overrides nil) nil))
      "item-value with no overrides or basis asks the table warm! built"))
