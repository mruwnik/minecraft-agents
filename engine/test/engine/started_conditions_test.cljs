(ns engine.started-conditions-test
  "A start-only condition holds a call back before its first round and never ends it once it began: the engine re-checks
  conditions at every re-entered round, and a frozen call (ctx/frozen-call, here a :quote-call) has begun."
  (:require [cljs.test :refer [deftest is are]]
            [engine.test-util :as tu]
            [jobs.animals.breed :as breed]
            [jobs.animals.leash :as leash]
            [jobs.combat.hunt :as hunt]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.survival.block-arrow-gap :as arrow-gap]
            [jobs.survival.fight-back :as fight-back]))

(defn fact
  "The cause of the first condition of conditions at ctx c (a call with no memory yet)."
  [conditions c]
  ((second (first conditions)) (assoc c :view (fn [] {:data {} :now 0}) :root "j1" :slots [])))

(def hurt {:self {:health 0.5}})

(deftest start-only-facts-hold-a-call-back-only-before-it-began
  (with-redefs [fell-tree/night-sheltered (constantly :roofed)]
    (are [conditions c] (and (some? (fact conditions c)) (nil? (fact conditions (assoc c :quote-call {}))))
      hunt/conditions {:primitives (tu/fake hurt) :args {}}
      fight-back/conditions {:primitives (tu/fake hurt) :args {:min-health 8}}
      breed/conditions {:primitives (tu/fake {}) :args {:mob "cow" :fetch false}}
      leash/conditions {:primitives (tu/fake {}) :args {:fetch false}}
      arrow-gap/conditions {:primitives (tu/fake {}) :args {:radius 16}}
      fell-tree/conditions {:primitives (tu/fake {}) :args {}})))
