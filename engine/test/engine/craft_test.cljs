(ns engine.craft-test
  "engine.craft: which recipe a craft reports as missing, and the alternatives, from the candidate recipes."
  (:require [cljs.test :refer [deftest is are]]
            [engine.craft :as craft]))

(deftest shortfall-is-what-the-closest-recipe-lacks
  (are [recipes have want] (= want (craft/shortfall recipes have))
    [] {"a" 1} {}
    [{"a" 2}] {"a" 2} {}
    [{"a" 3 "b" 1}] {"a" 1 "b" 1} {"a" 2}
    [{"oak" 2} {"cherry" 2}] {"oak" 1} {"oak" 1}
    [{"oak" 2} {"cherry" 2}] {"cherry" 1} {"cherry" 1}
    [{"a" 1 "b" 2}] {} {"a" 1 "b" 2}))

(def pickaxe [{"deepslate" 3 "stick" 2} {"blackstone" 3 "stick" 2} {"cobblestone" 3 "stick" 2}])

(deftest a-tie-goes-to-the-more-common-base-item
  (are [recipes have want] (= want (craft/shortfall recipes have))
    pickaxe {"stick" 2} {"cobblestone" 3}
    pickaxe {"stick" 2 "blackstone" 1} {"blackstone" 2}))

(deftest alternatives-are-the-cousins-of-the-missing-name
  (are [recipes have want] (= want (craft/alternatives recipes have))
    pickaxe {"stick" 2} {"cobblestone" ["deepslate" "blackstone"]}
    [{"a" 1}] {} {}
    [] {} {}
    [{"oak_planks" 2} {"cherry_planks" 2}] {} {"oak_planks" ["cherry_planks"]}))

(deftest no-item-hands-back-short-and-only-non-empty-alternatives
  (are [recipes have want] (= want (craft/no-item recipes have))
    [{"wheat" 3}] {"wheat" 1} {:short {"wheat" 2}}
    [{"oak_planks" 2} {"cherry_planks" 2}] {"oak_planks" 1}
    {:short {"oak_planks" 1} :alternatives {"oak_planks" ["cherry_planks"]}}))
