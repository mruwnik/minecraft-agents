(ns dashboard.ui.driving-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.driving :as d]))

(def me "dashboard-k3x9ab")
(def body "Bob")
(def idle-controls {:forward false :back false :left false :right false :jump false :sneak false :sprint false})
(def pos {:x 0 :y 64 :z 0})

(defn manual-of [who] {:who who :why "dashboard" :since 1000000 :controls idle-controls :yaw nil :pitch nil :idleMs 15000 :expiresAt 1015000 :idleLeftS 15})

;; the reply shapes of engine/test/contract/drive-contract.json (and the proxy's 503)
(def ok-take {:ok true :manual (manual-of me)})
(def ok-set {:ok true :manual (assoc (manual-of me) :controls (assoc idle-controls :forward true) :yaw 0 :pitch 0) :pos pos})
(def ok-ping {:ok true :manual (manual-of me) :pos pos})
(def held-by {:ok false :reason "held-by claude"})
(def offline {:ok false :reason "offline"})
(def settling {:ok false :reason "settling"})
(def bad-args {:ok false :reason "bad-args" :text "who is required"})
(def not-taken {:ok false :reason "not-taken"})
(def not-driver {:ok false :reason "not-driver" :holder "claude"})
(def no-body {:ok false :reason "no-body" :text "no running body Bob"})
(def manual-null {:ok true :manual nil :pos pos :offline false :settling false})
(def manual-null-offline {:ok true :manual nil :pos pos :offline true :settling false})
(def manual-null-settling {:ok true :manual nil :pos pos :offline false :settling true})
(def manual-other {:ok true :manual (manual-of "claude") :pos pos :offline false :settling false})
(def get-with-me {:ok true :manual (manual-of me) :pos pos :offline false :settling false})
(def get-me-offline (assoc get-with-me :offline true))

(defn db-with [slice] {:who me :drive {body slice}})
(defn slice-after [fx] (get-in fx [:db :drive body]))
(def idle {:db (db-with {})})
(def driving {:db (db-with {:driving? true :gen 1 :manual (manual-of me) :held #{:forward} :look {:dyaw 3 :dpitch 1}})})

(def cleared {:driving? false :held #{} :look nil})
(def timers-off {:drive/ping-timer {:name body :on? false} :drive/look-timer {:name body :on? false}})
(def timers-on {:drive/ping-timer {:name body :on? true} :drive/look-timer {:name body :on? true}})

(defn post-of [fx] (:drive/post fx))

(deftest take-posts-take
  (let [fx (d/take-fx {:db (db-with {:error "old"})} [::d/take body])]
    (is (= {:name body :gen 0 :body {:op "take" :who me :why "dashboard"}} (post-of fx)))
    (is (nil? (:error (slice-after fx))))))

(deftest take-while-driving-does-nothing
  (is (= {} (d/take-fx driving [::d/take body]))))

(deftest take-reply-ok-starts-driving
  (let [fx (d/reply-fx idle [::d/reply body 0 "take" ok-take])]
    (is (= {:driving? true :gen 1 :manual (:manual ok-take) :since 1000000 :held #{} :look nil :error nil}
           (select-keys (slice-after fx) [:driving? :gen :manual :since :held :look :error])))
    (is (= timers-on (select-keys fx [:drive/ping-timer :drive/look-timer])))))

(deftest take-reply-refusals-set-an-error-and-do-not-drive
  (are [reply text] (let [fx (d/reply-fx idle [::d/reply body 0 "take" reply])]
                      (and (= text (:error (slice-after fx)))
                           (false? (boolean (:driving? (slice-after fx))))
                           (nil? (:drive/ping-timer fx))))
    held-by "cannot take over: held-by claude"
    offline "cannot take over: offline"
    settling "cannot take over: settling"
    bad-args "cannot take over: bad-args"
    no-body "cannot take over: no-body"
    nil "no running body Bob"))

(deftest stale-replies-are-ignored
  (are [event] (= {} (d/reply-fx driving event))
    [::d/reply body 0 "set" nil]
    [::d/reply body 0 "set" manual-null]
    [::d/reply body 0 "take" held-by])
  (are [event] (= {} (d/poll-reply-fx driving event))
    [::d/poll-reply body 0 nil]
    [::d/poll-reply body 0 manual-null])
  (is (= {} (d/request-failed-fx driving [::d/request-failed body 0 "set"]))))

(deftest replies-that-show-we-hold-the-body-keep-driving
  (are [reply] (let [fx (d/reply-fx driving [::d/reply body 1 "set" reply])]
                 (and (true? (:driving? (slice-after fx)))
                      (= #{:forward} (:held (slice-after fx)))
                      (= (:manual reply) (:manual (slice-after fx)))
                      (nil? (:drive/ping-timer fx))))
    ok-set
    ok-ping
    ok-take))

(deftest replies-that-show-we-lost-the-body-clear-every-marker-in-one-step
  (are [reply] (let [fx (d/reply-fx driving [::d/reply body 1 "ping" reply])]
                 (and (= cleared (select-keys (slice-after fx) [:driving? :held :look]))
                      (= timers-off (select-keys fx [:drive/ping-timer :drive/look-timer]))
                      (string? (:error (slice-after fx)))))
    not-taken
    not-driver
    manual-null
    manual-null-offline
    manual-null-settling
    manual-other
    get-me-offline
    held-by
    no-body
    nil))

(deftest a-failed-request-while-driving-drops-everything
  (let [fx (d/request-failed-fx driving [::d/request-failed body 1 "set"])]
    (is (= cleared (select-keys (slice-after fx) [:driving? :held :look])))
    (is (= timers-off (select-keys fx [:drive/ping-timer :drive/look-timer])))))

(deftest a-failed-take-says-there-is-no-body
  (is (= "no running body Bob" (:error (slice-after (d/request-failed-fx idle [::d/request-failed body 0 "take"]))))))

(deftest a-failed-request-when-not-driving-changes-nothing
  (is (= (:db idle) (:db (d/request-failed-fx idle [::d/request-failed body 0 "release"])))))

(deftest a-release-reply-records-the-manual-without-error
  (let [fx (d/reply-fx idle [::d/reply body 0 "release" manual-null])]
    (is (nil? (:manual (slice-after fx))))
    (is (nil? (:error (slice-after fx))))))

(deftest poll-reply-when-not-driving-records-who-drives
  (are [reply manual] (= manual (:manual (slice-after (d/poll-reply-fx idle [::d/poll-reply body 0 reply]))))
    manual-other (:manual manual-other)
    manual-null nil
    get-with-me (:manual get-with-me)))

(deftest poll-reply-nil-when-not-driving-changes-nothing
  (is (= (:db idle) (:db (d/poll-reply-fx idle [::d/poll-reply body 0 nil])))))

(deftest poll-reply-while-driving-follows-the-hold-rule
  (are [reply drops?] (= drops? (false? (:driving? (slice-after (d/poll-reply-fx driving [::d/poll-reply body 1 reply])))))
    get-with-me false
    manual-null true
    manual-other true
    get-me-offline true
    nil true
    no-body true))

(deftest key-down-sends-held-control
  (let [fx (d/key-down-fx driving [::d/key-down body "KeyW" false])
        fx2 (d/key-down-fx driving [::d/key-down body "KeyD" false])]
    (is (= {:op "set" :who me :controls {:right true}} (:body (post-of fx2))))
    (is (= #{:forward :right} (:held (slice-after fx2))))
    (is (= {} fx) "forward is already held")))

(deftest key-down-repeat-does-not-resend-a-control
  (is (= {} (d/key-down-fx driving [::d/key-down body "KeyD" true]))))

(deftest key-down-look-step-sends-relative-look
  (are [code look] (= {:op "set" :who me :look look} (:body (post-of (d/key-down-fx driving [::d/key-down body code false]))))
    "ArrowLeft" {:dyaw -15}
    "ArrowDown" {:dpitch 10}))

(deftest key-down-ignored-when-not-driving-or-not-a-control
  (are [cofx code] (= {} (d/key-down-fx cofx [::d/key-down body code false]))
    idle "KeyW"
    driving "KeyQ"))

(deftest key-up-releases-a-held-control
  (let [fx (d/key-up-fx driving [::d/key-up body "KeyW"])]
    (is (= {:op "set" :who me :controls {:forward false}} (:body (post-of fx))))
    (is (= #{} (:held (slice-after fx))))))

(deftest key-up-ignored-when-not-held-or-not-driving
  (are [cofx code] (= {} (d/key-up-fx cofx [::d/key-up body code]))
    driving "KeyD"
    driving "KeyQ"
    idle "KeyW"))

(deftest mouse-move-accumulates-look
  (let [fx (d/mouse-move-fx driving [::d/mouse-move body 10 20])]
    (is (= 4.5 (:dyaw (:look (slice-after fx)))))
    (is (= 4 (:dpitch (:look (slice-after fx)))))
    (is (= {} (d/mouse-move-fx idle [::d/mouse-move body 10 20])))))

(deftest look-flush-sends-the-pending-look-once
  (let [fx (d/look-flush-fx driving [::d/look-flush body])]
    (is (= {:op "set" :who me :look {:dyaw 3 :dpitch 1}} (:body (post-of fx))))
    (is (nil? (:look (slice-after fx)))))
  (are [cofx] (= {} (d/look-flush-fx cofx [::d/look-flush body]))
    idle
    {:db (db-with {:driving? true :gen 1})}))

(deftest leaving-sends-stop-and-keeps-the-lease
  (are [handler event] (let [fx (handler driving event)]
                         (and (= {:op "stop" :who me} (:body (post-of fx)))
                              (true? (:driving? (slice-after fx)))
                              (= #{} (:held (slice-after fx)))
                              (nil? (:look (slice-after fx)))
                              (not (:keepalive? (post-of fx)))
                              (nil? (:drive/ping-timer fx))))
    d/pointer-lock-lost-fx [::d/pointer-lock-lost body]
    d/blur-fx [::d/blur body]
    d/hidden-fx [::d/hidden body]))

(deftest pagehide-stops-with-keepalive
  (let [fx (d/pagehide-fx driving [::d/pagehide body])]
    (is (= {:op "stop" :who me} (:body (post-of fx))))
    (is (true? (:keepalive? (post-of fx))))
    (is (true? (:driving? (slice-after fx))))))

(deftest leaving-when-not-driving-does-nothing
  (are [handler event] (= {} (handler idle event))
    d/pointer-lock-lost-fx [::d/pointer-lock-lost body]
    d/blur-fx [::d/blur body]
    d/hidden-fx [::d/hidden body]
    d/pagehide-fx [::d/pagehide body]))

(deftest release-clears-markers-and-posts-release
  (let [fx (d/release-fx driving [::d/release body])]
    (is (= cleared (select-keys (slice-after fx) [:driving? :held :look])))
    (is (= {:op "release" :who me} (:body (post-of fx))))
    (is (= timers-off (select-keys fx [:drive/ping-timer :drive/look-timer])))
    (is (= {} (d/release-fx idle [::d/release body])))))

(deftest popup-closed-releases-and-stops-the-poll
  (let [fx (d/popup-closed-fx driving [::d/popup-closed body])]
    (is (= {:op "release" :who me} (:body (post-of fx))))
    (is (= {:name body :on? false} (:drive/poll fx)))
    (is (false? (:driving? (slice-after fx)))))
  (let [fx (d/popup-closed-fx idle [::d/popup-closed body])]
    (is (nil? (post-of fx)))
    (is (= {:name body :on? false} (:drive/poll fx)))))

(deftest popup-opened-starts-the-poll
  (is (= {:name body :on? true} (:drive/poll (d/popup-opened-fx idle [::d/popup-opened body])))))

(deftest ticks
  (is (= {:op "ping" :who me} (:body (post-of (d/ping-tick-fx driving [::d/ping-tick body])))))
  (is (= {} (d/ping-tick-fx idle [::d/ping-tick body])))
  (is (= {:drive/poll-get {:name body :gen 1}} (d/poll-tick-fx driving [::d/poll-tick body]))))

(deftest post-carries-the-gen-the-request-started-in
  (is (= 1 (:gen (post-of (d/ping-tick-fx driving [::d/ping-tick body]))))))

(deftest banner-cases
  (let [t (.getTime (js/Date. 2026 0 1 9 5))]
    (are [slice expected] (= expected (d/banner slice me))
      {} nil
      {:manual nil} nil
      {:driving? true :manual (manual-of me)} "you are driving"
      {:driving? false :manual (manual-of me)} nil
      {:manual (assoc (manual-of "claude") :since t)} "driven by claude since 09:05"
      {:manual (dissoc (manual-of "claude") :since)} "driven by claude")))

(deftest held-by-me-cases
  (are [slice expected] (= expected (d/held-by-me? slice))
    {} false
    {:driving? true} true
    {:driving? false :manual (manual-of me)} false))
