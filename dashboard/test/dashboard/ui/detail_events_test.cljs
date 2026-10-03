(ns dashboard.ui.detail-events-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.ui.detail-events :as detail-events]))

(defn event [seq attention]
  {:seq seq :source :job :kind :failed :attention attention :request-id (when (= attention :required) (str "r" seq))})

(deftest event-page-replay-deduplicates-and-retains-current-attention
  (let [page {:stream-id "s1" :generation-id "g1" :cursor {:stream-id "s1" :seq 2}
              :events [(event 1 :notice) (event 2 :required)]
              :outstanding {"r2" {:request-id "r2"}}}
        first (detail-events/reconcile-page nil [] page)
        repeated (detail-events/reconcile-page (:stream first) (:events first) page)]
    (is (= [1 2] (mapv :seq (:events first))))
    (is (= [1 2] (mapv :seq (:events repeated))))
    (is (= {"r2" {:request-id "r2"}} (:outstanding repeated)))
    (is (= [1] (mapv :seq (:notices repeated))))))

(deftest gap-replaces-the-log-tail-and-reconciles-authoritative-inbox
  (let [prior-stream {:stream-id "s1" :seq 88}
        prior [(event 87 :notice) (event 88 :routine)]
        page {:stream-id "s1" :generation-id "g2" :gap? true
              :cursor {:stream-id "s1" :seq 91}
              :events [(event 90 :required) (event 91 :routine)]
              :outstanding {"r90" {:request-id "r90"}}}
        reconciled (detail-events/reconcile-page prior-stream prior page)]
    (is (= [90 91] (mapv :seq (:events reconciled))))
    (is (= {"r90" {:request-id "r90"}} (:outstanding reconciled)))))
