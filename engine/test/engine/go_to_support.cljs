(ns engine.go-to-support
  "Shared helpers of the engine.go-to test namespaces."
  (:require [engine.contract :as contract]
            [engine.test-util :as tu]))

(defn result-of
  "go-to's ending e as the result map it handed over before the contract: its data, and for a stop :status :stopped
  and the cause's :reason. The ending is its meta :ending."
  [e]
  (with-meta (if (= :done (:kind e))
               (:data e)
               (assoc (:data e) :status :stopped :reason (contract/cause-key e)))
    {:ending e}))

(defn hiding
  "p (tu/fake over blocks) that sees every cell but those hidden? [x y z] answers true for (unknown, never seen)."
  [blocks hidden?]
  (let [p (tu/fake {:blocks blocks})]
    (aset p "sensedAt" (fn [pos] (if (hidden? [(.-x pos) (.-y pos) (.-z pos)]) #js {:unknown true} (.blockAt p pos))))
    p))
