(ns engine.blocks-outcome-test
  "jobs.lib.blocks dig-outcome and place-outcome over the ending kinds."
  (:require [cljs.test :refer [deftest are]]
            [engine.contract :as contract]
            [jobs.lib.blocks :as blocks]))

(defn e [kind cause & [data]] (contract/ending kind cause data))

(deftest dig-outcome-reads-endings
  (are [ending want] (= want (blocks/dig-outcome ending))
    :continue :continue
    (e :done :dug) :dug
    (e :done :already-clear {:reason :already-clear}) :missing
    (e :condition-failed :not-allowed) :refused
    (e :condition-failed :need) :unreachable
    (e :impossible :bad-args) :unreachable
    (e :cut-off :cancelled) :failed
    (e :limit-reached :budget) :failed
    (e :nothing-possible :hazard) :refused
    (e :nothing-possible :cannot) :cannot
    (e :nothing-possible :no-tool) :no-tool
    (e :nothing-possible :fluid-adjacent {:primitive "dig"}) :dig
    (e :nothing-possible nil) :failed))

(deftest place-outcome-reads-endings
  (are [ending want] (= want (blocks/place-outcome ending))
    :continue :continue
    (e :done :placed) :placed
    (e :done :already {:reason :already}) :already
    (e :condition-failed :own-body) :refused
    (e :condition-failed :no-support) :no-support
    (e :impossible :bad-args) :unreachable
    (e :cut-off :cancelled) :failed
    (e :limit-reached :budget) :failed
    (e :nothing-possible :occupied) :occupied
    (e :nothing-possible nil) :failed))
