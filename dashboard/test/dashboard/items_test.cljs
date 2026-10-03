(ns dashboard.items-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.items :as items]))

(deftest label-cases
  (are [name expected] (= expected (items/label name))
    "diamond_pickaxe" "DP"
    "oak_log" "OL"
    "gravel" "Gra"
    "cooked_porkchop_extra_long" "CPE"
    "" ""
    nil ""))

(deftest title-cases
  (are [name n expected] (= expected (items/title name n))
    "gravel" 64 "gravel ×64"
    "bread" 1 "bread"
    "bread" nil "bread"))

(deftest icon-candidates-cases
  (are [name expected] (= expected (items/icon-candidates name))
    "bread" ["item/bread.png" "bread.png" "bread_front.png" "bread_side.png"]
    "../etc" nil
    "Bread" nil
    "" nil
    nil nil))

(deftest icon-src-cases
  (are [name expected] (= expected (items/icon-src name))
    "bread" "/api/item-icon/bread.png"
    "a b" nil))
