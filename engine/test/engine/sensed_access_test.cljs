(ns engine.sensed-access-test
  "Q98 C2b: placement, dig and the scaffold jobs read what the body senses. A cell behind stone is unknown: it is no
  support to place against, a dig goes ahead past it and looks at what the dig lays open, and a ledger entry on it
  stays open. The primitives are wrapped by perception; blockAt stays raw."
  (:require [cljs.test :refer [deftest is async]]
            [engine.dig-to-see-test :as d]
            [engine.test-util :as tu]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.ledger :as ledger]))

(deftest a-cell-whose-neighbours-are-all-unseen-has-no-support
  (let [p (d/sensing {:blocks (dissoc d/ground "5,60,0")})]
    (is (true? (access/unknown? p [5 59 0])) "inside the rock, nothing is known")
    (is (false? (b/support? p {:x 5 :y 60 :z 0})) "unseen faces are not solid to place against")
    (is (true? (b/support? p {:x 0 :y 65 :z 0})) "the floor under the feet is felt")))

(deftest a-ledger-entry-on-an-unseen-cell-stays-open
  (let [p (d/sensing {:blocks d/ground})
        entry {:cell [5 60 0] :item "cobblestone" :state :intent}]
    (is (= [entry] (ledger/reconcile [entry] (access/sensed-at p nil))))))

(deftest dig-goes-ahead-past-water-it-has-not-seen-and-looks-after
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out p]} (await (d/run! {:blocks (assoc d/ground "1,63,0" "water")} 'jobs.blocks.dig
                                             {:pos {:x 1 :y 64 :z 0} :ignore-zones? true}))]
          (is (true? (:dug @out)))
          (is (some #{{:x 1 :y 64 :z 0}} (d/digs p)))
          (is (false? (access/unknown? p [1 63 0])) "the dug cell's neighbours are looked at"))))))
