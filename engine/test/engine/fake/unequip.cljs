(ns engine.fake.unequip
  "Unequip in the fake world: empty the main hand into the pockets, over world data {:self {:held} :inventory}.
  Test-only; the same rules as js/fake-unequip.mjs."
  (:require [engine.fake.pockets :as pockets]))

(defn unequip
  "[world' result]: the held item goes away (the pockets keep their stacks, as in the JS) when a slot is free."
  [w]
  (let [held (get-in w [:self :held])]
    (cond
      (nil? held) [w {:status "empty"}]
      (>= (count (:inventory w)) pockets/slots) [w {:status "full"}]
      :else [(assoc-in w [:self :held] nil) {:status "ok" :item held}])))
