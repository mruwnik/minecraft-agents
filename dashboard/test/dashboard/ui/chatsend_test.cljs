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

(def draft {:draft "hi" :status :idle :error nil :ack-id 0})

(deftest whisper-url-only-for-minecraft-names-in-a-world
  (is (nil? (cs/whisper-url nil "Pacer")))
  (is (nil? (cs/whisper-url "a/b" "Pacer")))
  (are [n expected] (= expected (cs/whisper-url "claude" n))
    "Pacer" "/api/whisper/claude/Pacer"
    "a_1" "/api/whisper/claude/a_1"
    "abcdefghijklmnop" "/api/whisper/claude/abcdefghijklmnop"
    "" nil
    nil nil
    "ab" nil
    "abcdefghijklmnopq" nil
    "a b" nil
    "@a" nil
    "a/b" nil))

(deftest whisper-block-reason-is-offline-only
  (are [online? expected] (= expected (cs/whisper-block-reason online? draft))
    true nil
    false "offline: nobody would hear it"))

(deftest whisper-sendable-refusals
  (are [online? state n expected] (= expected (cs/whisper-sendable? online? state n))
    true draft "Pacer" true
    false draft "Pacer" false
    true (assoc draft :draft " ") "Pacer" false
    true (assoc draft :status :pending) "Pacer" false
    true draft "" false
    true draft "ab" false
    true draft "abcdefghijklmnopq" false
    true draft "a b" false
    true draft "@a" false
    true draft "a/b" false))

(deftest whisper-captions
  (are [state online? expected] (= expected (cs/whisper-caption state online? "Pacer"))
    cs/initial true "whisper to Pacer"
    (cs/begin draft) true "sending..."
    (cs/succeeded draft 1) true "sent"
    (cs/failed draft "boom") true "boom"
    cs/initial false "offline: nobody would hear it"))
