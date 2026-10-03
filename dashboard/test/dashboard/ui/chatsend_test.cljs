(ns dashboard.ui.chatsend-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.chatsend :as cs]))

(deftest sendable
  (are [text pending? expected] (= expected (cs/sendable? {:draft text :status (if pending? :pending :idle)}))
    "hi" false true
    "  hi " false true
    "" false false
    "   " false false
    nil false false
    "hi" true false))

(deftest transitions
  (are [f expected] (= expected (f cs/initial))
    #(cs/begin (assoc % :draft "hi"))
    {:draft "hi" :status :pending :error nil :ack-id 0}
    #(cs/succeeded (cs/begin (assoc % :draft "hi")) 7)
    {:draft "" :status :sent :error nil :ack-id 7}
    #(cs/failed (cs/begin (assoc % :draft "hi")) "RCON failed: x")
    {:draft "hi" :status :failed :error "RCON failed: x" :ack-id 0}
    #(cs/edited % "abc")
    {:draft "abc" :status :idle :error nil :ack-id 0}))

(deftest ack-clears-only-its-own
  (are [state id expected] (= expected (:status (cs/clear-ack state id)))
    {:status :sent :ack-id 3} 3 :idle
    {:status :sent :ack-id 4} 3 :sent
    {:status :pending :ack-id 3} 3 :pending
    {:status :failed :ack-id 3} 3 :failed))

(deftest captions
  (are [state sender expected] (= expected (cs/caption state sender))
    cs/initial nil "to everyone"
    cs/initial "" "to everyone"
    cs/initial "dashboard" "to everyone as dashboard"
    {:status :pending} "dashboard" "sending..."
    {:status :sent} nil "sent"
    {:status :failed :error "boom"} "dashboard" "boom"))

(deftest request-body
  (are [draft expected] (= expected (cs/request-body {:draft draft}))
    "hi" {:text "hi"}
    "  hi  " {:text "hi"}))
