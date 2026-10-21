(ns engine.fake.unequip
  "Unequip in the fake world: empty the main hand into the pockets, over world data {:self {:held} :inventory}, or the
  off hand (dest \"off-hand\") or an armour slot (\"head\"...) into the pockets, over {:equipment {\"offHand\" stack}}.
  Test-only; ported from the deleted js/fake-unequip.mjs."
  (:require [engine.fake.pockets :as pockets]))

(defn unequip-hand
  "[world' result]: the held item goes away (the pockets keep their stacks, as in the JS) when a slot is free."
  [w]
  (let [held (get-in w [:self :held])]
    (cond
      (nil? held) [w {:status "empty"}]
      (>= (count (:inventory w)) pockets/slots) [w {:status "full"}]
      :else [(assoc-in w [:self :held] nil) {:status "ok" :item held}])))

(defn unequip-slot
  "[world' result]: the stack in an equipment part (\"offHand\", \"head\"...) goes into the pockets when they have room for it."
  [w part]
  (let [{:keys [name count] :as stack} (get-in w [:equipment part])]
    (cond
      (nil? stack) [w {:status "empty"}]
      (not (pockets/has-room? w name)) [w {:status "full"}]
      :else [(-> w (update :equipment dissoc part) (pockets/give name count)) {:status "ok" :item name}])))

(defn unequip
  ([w] (unequip w {}))
  ([w {:keys [dest]}]
   (cond
     (= "off-hand" dest) (unequip-slot w "offHand")
     (contains? #{"head" "torso" "legs" "feet"} dest) (unequip-slot w dest)
     :else (unequip-hand w))))
