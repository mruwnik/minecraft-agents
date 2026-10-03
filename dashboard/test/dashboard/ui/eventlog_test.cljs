(ns dashboard.ui.eventlog-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.eventlog :as log]))

(defn ev [seq extra] (merge {:seq seq :t (* 1000 seq) :level "info" :source "job" :kind "completed"} extra))

(deftest category-cases
  (are [e expected] (= expected (log/category e))
    {:source "chat" :kind "said"} :chat
    {:source "body" :kind "chat"} :chat
    {:source "body" :kind "hurt"} :combat
    {:source "body" :kind "died"} :combat
    {:source "reflex" :kind "fired" :reflex "flee"} :combat
    {:source "reflex" :kind "fired" :reflex "stuck"} :movement
    {:source "job" :kind "unreachable"} :movement
    {:source "action" :kind "started" :name "go-to"} :movement
    {:source "action" :kind "started" :name "dig"} :jobs
    {:source "job" :kind "completed"} :jobs
    {:source "system" :kind "started"} :system
    {:source "body" :kind "spawned"} :system))

(deftest summary-cases
  (are [e expected] (= expected (log/summary e))
    {:source "job" :kind "completed" :name "jobs.gather"} "jobs.gather"
    {:source "body" :kind "chat" :text "hello"} "hello"
    {:source "action" :kind "started" :name "wait" :args {:ms 3000}} "wait {\"ms\":3000}"
    {:source "action" :kind "done" :name "wait" :ms 12.7} "wait 13ms"
    {:source "job" :kind "failed" :error "boom"} "boom"
    {:source "system" :kind "started"} "started"
    {:source "body" :kind "chat_sent"} "chat sent"))

(def events
  [(ev 1 {:source "chat" :kind "said" :text "hi"})
   (ev 2 {:source "body" :kind "hurt"})
   (ev 3 {:source "job" :kind "failed" :level "error" :error "boom"})
   (ev 4 {:source "system" :kind "started"})])

(deftest rows-filter
  (are [opts expected] (= expected (mapv :seq (log/rows events opts)))
    {} [4 3 2 1]
    {:chip :all} [4 3 2 1]
    {:chip :chat} [1]
    {:chip :combat} [2]
    {:chip :system} [4]
    {:chip :errors} [3]
    {:chip :jobs} [3]
    {:text "BOOM"} [3]
    {:text "hurt"} [2]
    {:chip :all :text "zzz"} []))

(deftest row-shape
  (are [k expected] (= expected (k (first (log/rows events {}))))
    :source-kind "system.started"
    :level "info"
    :category :system
    :t 4000))

(deftest current-action-cases
  (are [evs expected] (= expected (log/current-action evs))
    [] nil
    [(ev 1 {:source "action" :kind "started" :name "wait"})] "wait"
    [(ev 1 {:source "action" :kind "started" :name "wait"}) (ev 2 {:source "action" :kind "done" :name "wait"})] nil
    [(ev 1 {:source "action" :kind "started" :name "wait"}) (ev 2 {:source "job" :kind "yielded"})] "wait"))
