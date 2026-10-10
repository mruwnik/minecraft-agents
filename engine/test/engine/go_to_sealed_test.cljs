(ns engine.go-to-sealed-test
  "A body sealed in its own retreat plug (ledger purpose :retreat-plug) with a known hostile near: a walk that may not
  dig out waits with :sealed-hostile-near instead of failing, and walks on once the hostile is gone."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [jobs.lib.child :as child]
            [jobs.lib.ledger :as ledger]))

(def pit
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "dirt"]))
         {"0,66,0" "dirt"}))

(def parent
  {:check (constantly true)
   :round (fn ^:async walker [c] (await (child/call-walk c :walk {:pos [15 64 0] :escalate false})))})

(defn ^:async sealed
  "A noon world with the body in a 1-cell pit whose east side is a retreat plug, a skeleton outside; ticks n."
  [n]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:floor tu/walk-floor :time st/noon :self {:pos {:x 0 :y 64 :z 0}} :blocks pit}))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
        s {:eng eng :p p :seen seen}]
    (ledger/write! (:store eng) [{:cell [1 64 0] :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}])
    (fake/add-entity! p {:id 9 :name "skeleton" :kind "hostile" :pos {:x -6 :y 64 :z 0} :health 20})
    (core/submit! eng '(walker) {})
    (await (st/tick-n eng n))
    s))

(deftest a-walk-from-the-sealed-plug-with-a-hostile-near-holds-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (sealed 6))]
          (is (= {:x 0 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z])))
          (is (= [:sealed-hostile-near] (distinct (map :reason (st/emitted seen :holding)))))
          (is (empty? (st/emitted seen :unreachable))))))))
