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
            [engine.memory :as mem]
            [jobs.lib.body :as body]
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
  ([n] (sealed n nil))
  ([n self] (sealed n self nil))
  ([n self setup]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:floor tu/walk-floor :time st/noon :self (merge {:pos {:x 0 :y 64 :z 0}} self) :blocks pit}))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
        s {:eng eng :p p :seen seen :clock clock}]
    (ledger/write! (:store eng) [{:cell [1 64 0] :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}])
    (fake/add-entity! p {:id 9 :name "skeleton" :kind "hostile" :pos {:x -6 :y 64 :z 0} :health 20})
    (when setup (setup eng))
    (core/submit! eng '(walker) {})
    (await (st/tick-n eng n))
    s)))

(deftest a-walk-from-the-sealed-plug-with-a-hostile-near-holds-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (sealed 6))]
          (is (= {:x 0 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z])))
          (is (= [:sealed-hostile-near] (distinct (map :reason (st/emitted seen :holding)))))
          (is (empty? (st/emitted seen :unreachable))))))))

(deftest the-hold-ends-when-the-hostile-is-gone-with-the-plug-still-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (sealed 6))
              held (count (st/emitted seen :holding))]
          (swap! (fake/state p) update :entities #(vec (remove (comp #{9} :id) %)))
          (await (st/tick-n eng 8))
          (is (pos? held))
          (is (= held (count (st/emitted seen :holding))) "no further :sealed-hostile-near hold")
          (is (seq (st/emitted seen :unreachable)) "the walk ends in its own give-up, not an endless wait"))))))

(deftest the-hold-ends-at-its-deadline-with-the-hostile-still-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (await (sealed 6))]
          (is (empty? (st/emitted seen :unreachable)))
          (swap! clock + (* 10 60 1000))
          (await (st/tick-n eng 8))
          (is (seq (st/emitted seen :unreachable)) "after the deadline the walk goes on to its own give-up"))))))

(deftest a-starving-or-hurt-body-is-not-held
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [self [{:food 2} {:health 4}]]
          (let [{:keys [seen]} (await (sealed 6 self))]
            (is (empty? (st/emitted seen :holding)) (str self))
            (is (seq (st/emitted seen :unreachable)) (str self))))))))

(deftest a-muted-heard-only-hostile-does-not-hold-the-sealed-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[muted? held?] [[true false] [false true]]]
          (let [{:keys [seen]} (await (sealed 6 nil
                                              (fn [eng]
                                                (when muted? (mem/write! (:store eng) :heard-mute {:until 9999999999} body/mute-policy)))))]
            (is (= held? (boolean (seq (st/emitted seen :holding)))) (str "muted " muted?))))))))
