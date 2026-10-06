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

(def lease {:who "dashboard-k3x9ab" :expiresAt 9000})

(deftest drive-poll-keeps-our-lease-and-countdown
  (let [db {:who "dashboard-k3x9ab" :detail-body "Bob" :drive {:driving? true :expires-at 5000}}
        polled (detail-events/drive-polled db "Bob" lease 100)]
    (is (= {:driving? true :expires-at 9000 :manual lease :at 100} (:drive polled)))))

(deftest drive-poll-of-someone-elses-lease-stops-us-driving
  (let [db {:who "dashboard-k3x9ab" :detail-body "Bob" :drive {:driving? true :expires-at 5000}}
        other {:who "claude" :expiresAt 9000}
        polled (detail-events/drive-polled db "Bob" other 100)]
    (is (= {:driving? false :expires-at nil :manual other :at 100} (:drive polled)))))

(deftest drive-poll-for-a-closed-body-changes-nothing
  (let [db {:who "me" :detail-body "Al" :drive {:driving? true}}]
    (is (= db (detail-events/drive-polled db "Bob" nil 100)))))

(deftest failed-drive-poll-keeps-our-lease
  (let [db {:who "dashboard-k3x9ab" :detail-body "Bob" :drive {:driving? true :manual lease :expires-at 9000}}]
    (is (= db (detail-events/drive-poll-failed db "Bob" 100)))))

(deftest failed-drive-poll-of-a-body-we-do-not-drive-clears-it
  (let [db {:who "me" :detail-body "Bob" :drive {:manual {:who "other"}}}]
    (is (nil? (get-in (detail-events/drive-poll-failed db "Bob" 100) [:drive :manual])))))

(deftest closing-the-popup-releases-a-body-we-drive
  (let [db {:who "dashboard-k3x9ab" :world "w" :detail-body "Bob" :detail-events [1] :drive {:driving? true :manual lease}}
        fx (detail-events/close-fx db)]
    (is (nil? (get-in fx [:db :detail-body])))
    (is (= {} (get-in fx [:db :drive])))
    (is (= [:detail-log :detail-drive] (:stop-timers fx)))
    (is (= {:op :release :name "Bob" :world "w"} (select-keys (:drive-invoke fx) [:op :name :world])))))

(deftest closing-the-popup-of-an-undriven-body-releases-nothing
  (is (nil? (:drive-invoke (detail-events/close-fx {:who "me" :detail-body "Bob" :drive {}})))))
