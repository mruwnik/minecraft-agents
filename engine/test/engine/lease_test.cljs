(ns engine.lease-test
  "The takeover lease rules (engine.lease), ported one to one from js/control.test.mjs, plus the two-clock
  tests (ping is a heartbeat, not input) and the wire contract in test/contract/drive-contract.json.
  The three socket-level JS tests (real unix socket mode 0600, bodies over 16 KB, socket path over 100
  bytes) stay in js/control.test.mjs: they test node's http server, not the rules."
  (:require [cljs.test :refer [deftest is are]]
            [engine.lease :as lease]
            [engine.test-util :as tu]))

(def pos {:x 1 :y 2 :z 3})

;; The rig plays the part of engine.takeover: it applies the effects (recording them), answers :take and
;; :drive, and keeps the lease between calls. State: an atom so each test reads like the JS rig.

(defn make-rig
  ([] (make-rig {}))
  ([{:keys [opts take-result drive-fn t]}]
   (atom {:lease nil
          :t (or t 1000)
          :world {:offline false :settling false :pos pos}
          :calls []
          :opts (or opts {})
          :take-result (or take-result {:ok true})
          :drive-fn (or drive-fn (constantly {:pos pos :yaw 90 :pitch 10}))})))

(defn settle
  "Apply a request result the way the engine adapter does: record effects, answer the pending question."
  [rig {:keys [lease effects pending] :as r}]
  (let [now (:t @rig)
        done (case pending
               :take (lease/taken lease (:take-result @rig) now)
               :drive (lease/driven lease ((:drive-fn @rig) (second (first effects))) now)
               r)]
    (swap! rig #(-> % (assoc :lease (:lease done)) (update :calls into effects)))
    (:reply done)))

(defn handle! [rig req]
  (let [{:keys [lease t world opts]} @rig]
    (settle rig (lease/request lease req t world opts))))

(defn post! [rig body] (handle! rig {:method "POST" :path "/drive" :body body}))
(defn get! [rig] (handle! rig {:method "GET" :path "/drive" :body nil}))

(defn tick! [rig]
  (let [{:keys [lease t world opts]} @rig
        {:keys [lease effects]} (lease/tick lease t world opts)]
    (swap! rig #(-> % (assoc :lease lease) (update :calls into effects)))))

(defn advance! [rig ms] (swap! rig update :t + ms))
(defn set-world! [rig patch] (swap! rig update :world merge patch))
(defn calls-of [rig kind] (filterv #(= kind (first %)) (:calls @rig)))
(defn manual [rig] (get-in (get! rig) [:json :manual]))
(defn controls-of [rig] (:controls (manual rig)))

(defn taken-rig
  ([] (taken-rig {}))
  ([config]
   (let [rig (make-rig config)]
     (post! rig {:op "take" :who "claude" :why "test"})
     rig)))

;; take ok pauses the engine and reports the manual state
(deftest take-ok-pauses-the-engine-and-reports-the-manual-state
  (let [rig (make-rig)
        r (post! rig {:op "take" :who "claude" :why "poke"})]
    (is (= 200 (:status r)))
    (is (true? (get-in r [:json :ok])))
    (is (= "claude" (get-in r [:json :manual :who])))
    (is (= "poke" (get-in r [:json :manual :why])))
    (is (= [[:take "claude" "poke"]] (calls-of rig :take)))))

;; take refused when ${label}
(deftest take-refused-when-offline-or-settling
  (are [patch reason] (let [rig (make-rig)]
                        (set-world! rig patch)
                        (let [r (post! rig {:op "take" :who "claude" :why "x"})]
                          (and (= {:ok false :reason reason} (:json r))
                               (empty? (calls-of rig :take)))))
    {:offline true} "offline"
    {:settling true} "settling"))

;; take refused when held by another
(deftest take-refused-when-held-by-another
  (let [rig (taken-rig)]
    (is (= {:ok false :reason "held-by claude"}
           (:json (post! rig {:op "take" :who "view" :why "x"}))))))

;; take refused by the engine passes its reason through
(deftest take-refused-by-the-engine-passes-its-reason-through
  (let [rig (make-rig {:take-result {:ok false :reason "nope"}})
        r (post! rig {:op "take" :who "a" :why "b"})]
    (is (= {:ok false :reason "nope"} (:json r)))
    (is (nil? (manual rig)))))

;; same who taking again is idempotent
(deftest same-who-taking-again-is-idempotent
  (let [rig (taken-rig)
        r (post! rig {:op "take" :who "claude" :why "again"})]
    (is (true? (get-in r [:json :ok])))
    (is (= 1 (count (calls-of rig :take))))))

;; take without who is bad-args
(deftest take-without-who-is-bad-args
  (is (= "bad-args" (get-in (post! (make-rig) {:op "take" :why "x"}) [:json :reason]))))

;; ${op} without a takeover is not-taken
(deftest every-driver-op-without-a-takeover-is-not-taken
  (are [op] (= {:ok false :reason "not-taken"}
               (:json (post! (make-rig) {:op op :who "claude" :controls {:forward true}})))
    "set" "stop" "ping" "release"))

;; ${op} by a non-driver is not-driver with the holder
(deftest every-driver-op-by-a-non-driver-is-not-driver-with-the-holder
  (are [op] (= {:ok false :reason "not-driver" :holder "claude"}
               (:json (post! (taken-rig) {:op op :who "view" :controls {:forward true}})))
    "set" "stop" "ping" "release"))

;; set passes controls and look to body.drive and GET reports them
(deftest set-passes-controls-and-look-to-the-body-and-get-reports-them
  (let [rig (taken-rig)
        r (post! rig {:op "set" :who "claude" :controls {:forward true :sprint true} :look {:yaw 180 :pitch 5}})
        g (:json (get! rig))]
    (is (true? (get-in r [:json :ok])))
    (is (= pos (get-in r [:json :pos])))
    (is (= [:drive {:controls {:forward true :sprint true} :look {:yaw 180 :pitch 5}}] (last (calls-of rig :drive))))
    (is (true? (get-in g [:manual :controls :forward])))
    (is (true? (get-in g [:manual :controls :sprint])))
    (is (false? (get-in g [:manual :controls :back])))
    (is (= 90 (get-in g [:manual :yaw])))
    (is (= 10 (get-in g [:manual :pitch])))
    (is (= pos (:pos g)))))

;; set passes relative look through as given
(deftest set-passes-relative-look-through-as-given
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :look {:dyaw 15 :dpitch -3}})
    (is (= {:dyaw 15 :dpitch -3} (:look (second (last (calls-of rig :drive))))))))

;; GET with no takeover reports manual null
(deftest get-with-no-takeover-reports-manual-null
  (let [g (:json (get! (make-rig)))]
    (is (true? (:ok g)))
    (is (nil? (:manual g)))
    (is (false? (:offline g)))
    (is (false? (:settling g)))))

;; set with ${label} is bad-args
(deftest set-with-bad-arguments-is-bad-args
  (are [args] (let [rig (taken-rig)
                    r (post! rig (merge {:op "set" :who "claude"} args))]
                (and (= 200 (:status r))
                     (false? (get-in r [:json :ok]))
                     (= "bad-args" (get-in r [:json :reason]))
                     (string? (get-in r [:json :text]))
                     (empty? (calls-of rig :drive))))
    {:controls {:fly true}}
    {:controls {:forward 1}}
    {:controls "forward"}
    {:controls {:jump true} :ms 0}
    {:controls {:jump true} :ms 10001}
    {:controls {:jump true} :ms 1.5}
    {:controls {:jump true} :ms "5"}
    {:look {:yaw "north" :pitch 0}}
    {:look 5}))

;; timed hold releases only the named controls after ms
(deftest timed-hold-releases-only-the-named-controls-after-ms
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :controls {:forward true}})
    (post! rig {:op "set" :who "claude" :controls {:jump true} :ms 300})
    (advance! rig 299)
    (tick! rig)
    (is (true? (:jump (controls-of rig))))
    (advance! rig 2)
    (tick! rig)
    (is (= [:drive {:controls {:jump false}}] (last (calls-of rig :drive))))
    (is (false? (:jump (controls-of rig))))
    (is (true? (:forward (controls-of rig))))))

;; timed hold does not release a control that was set again later
(deftest timed-hold-does-not-release-a-control-that-was-set-again-later
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :controls {:jump true} :ms 300})
    (advance! rig 100)
    (post! rig {:op "set" :who "claude" :controls {:jump true}})
    (let [before (count (calls-of rig :drive))]
      (advance! rig 500)
      (tick! rig)
      (is (= before (count (calls-of rig :drive))))
      (is (true? (:jump (controls-of rig)))))))

;; dead-man releases untimed controls after releaseMs and warns once
(deftest dead-man-releases-untimed-controls-after-release-ms-and-warns-once
  (let [rig (taken-rig {:opts {:release-ms 2000}})]
    (post! rig {:op "set" :who "claude" :controls {:forward true}})
    (advance! rig 1999)
    (tick! rig)
    (is (empty? (calls-of rig :stop-driving)))
    (advance! rig 1)
    (tick! rig)
    (tick! rig)
    (advance! rig 500)
    (tick! rig)
    (is (= 1 (count (calls-of rig :stop-driving))))
    (is (= [[:deadman "claude" 2000]] (calls-of rig :deadman)))
    (is (false? (:forward (controls-of rig))))))

;; dead-man default releases untimed controls after 1000 ms of silence, not at 999
(deftest dead-man-default-releases-untimed-controls-after-1000-ms-of-silence-not-at-999
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :controls {:forward true}})
    (advance! rig 999)
    (tick! rig)
    (is (empty? (calls-of rig :stop-driving)))
    (advance! rig 1)
    (tick! rig)
    (is (= 1 (count (calls-of rig :stop-driving))))))

;; dead-man does nothing when no untimed control is held
(deftest dead-man-does-nothing-when-no-untimed-control-is-held
  (let [rig (taken-rig)]
    (advance! rig 5000)
    (tick! rig)
    (is (empty? (calls-of rig :deadman)))))

;; dead-man does not fire for timed holds
(deftest dead-man-does-not-fire-for-timed-holds
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :controls {:forward true} :ms 10000})
    (advance! rig 3000)
    (tick! rig)
    (is (empty? (calls-of rig :deadman)))))

;; ping resets the silence (adapted: ping resets the dead-man clock but not the idle clock)
(deftest ping-resets-the-silence
  (let [rig (taken-rig)
        expires (:expiresAt (manual rig))]
    (post! rig {:op "set" :who "claude" :controls {:forward true}})
    (advance! rig 700)
    (is (true? (get-in (post! rig {:op "ping" :who "claude"}) [:json :ok])))
    (advance! rig 700)
    (tick! rig)
    (is (empty? (calls-of rig :deadman)))
    (advance! rig 400)
    (tick! rig)
    (is (= 1 (count (calls-of rig :deadman))))
    (is (= (+ 1000 15000 0) expires))))

;; a person holding W with the page pinging must not lose the body to the idle limit
(deftest a-held-control-with-a-live-heartbeat-keeps-the-body
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :controls {:forward true}})
    (doseq [_ (range 40)]
      (advance! rig 500)
      (post! rig {:op "ping" :who "claude"})
      (tick! rig))
    (is (empty? (calls-of rig :release)) "20 s held, past the 15 s idle limit")
    (is (true? (:forward (controls-of rig))))))

(deftest a-held-control-after-the-dead-man-fired-does-not-keep-the-body
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :controls {:forward true}})
    (advance! rig 1100)
    (tick! rig)
    (is (= 1 (count (calls-of rig :deadman))))
    (post! rig {:op "ping" :who "claude"})
    (advance! rig 13900)
    (tick! rig)
    (is (= [[:release "claude" "idle" 15000]] (calls-of rig :release)))))

;; idle ends the takeover with reason idle
(deftest idle-ends-the-takeover-with-reason-idle
  (let [rig (taken-rig {:opts {:idle-ms 60000}})]
    (advance! rig 59999)
    (tick! rig)
    (is (empty? (calls-of rig :release)))
    (advance! rig 1)
    (tick! rig)
    (is (= [[:release "claude" "idle" 60000]] (calls-of rig :release)))
    (is (nil? (manual rig)))))

;; going offline during a takeover ends it with reason offline
(deftest going-offline-during-a-takeover-ends-it-with-reason-offline
  (let [rig (taken-rig)]
    (advance! rig 10)
    (set-world! rig {:offline true})
    (tick! rig)
    (is (= [[:release "claude" "offline" 10]] (calls-of rig :release)))
    (is (nil? (manual rig)))))

;; release by the driver is released
(deftest release-by-the-driver-is-released
  (let [rig (taken-rig)]
    (advance! rig 42)
    (is (true? (get-in (post! rig {:op "release" :who "claude"}) [:json :ok])))
    (is (= [[:release "claude" "released" 42]] (calls-of rig :release)))
    (is (nil? (manual rig)))))

;; release by another without force is refused
(deftest release-by-another-without-force-is-refused
  (let [rig (taken-rig)]
    (is (= {:ok false :reason "not-driver" :holder "claude"} (:json (post! rig {:op "release" :who "view"}))))
    (is (empty? (calls-of rig :release)))))

;; release by another with force is forced
(deftest release-by-another-with-force-is-forced
  (let [rig (taken-rig)]
    (is (true? (get-in (post! rig {:op "release" :who "view" :force true}) [:json :ok])))
    (is (= [[:release "claude" "forced" 0]] (calls-of rig :release)))))

;; stop clears every control through the body
(deftest stop-clears-every-control-through-the-body
  (let [rig (taken-rig)]
    (post! rig {:op "set" :who "claude" :controls {:forward true}})
    (is (true? (get-in (post! rig {:op "stop" :who "claude"}) [:json :ok])))
    (is (= [[:stop-driving]] (calls-of rig :stop-driving)))
    (is (false? (:forward (controls-of rig))))))

;; close ends a held takeover with reason shutdown
(deftest close-ends-a-held-takeover-with-reason-shutdown
  (let [rig (taken-rig)]
    (is (= [[:release "claude" "shutdown" 0]] (lease/close (:lease @rig) (:t @rig))))
    (is (= [] (lease/close nil 0)))))

;; ${label} gives ${status}
(deftest routing-and-malformed-bodies-give-400-or-404
  (are [req status] (= status (:status (handle! (make-rig) req)))
    {:method "POST" :path "/drive" :body {:op "dance" :who "a"}} 400
    {:method "POST" :path "/drive" :body nil} 400
    {:method "POST" :path "/drive" :body [1]} 400
    {:method "GET" :path "/nope" :body nil} 404
    {:method "DELETE" :path "/drive" :body nil} 404))

;; idle defaults to 15 s
(deftest idle-defaults-to-15-s
  (let [rig (taken-rig)]
    (advance! rig 14999)
    (tick! rig)
    (is (empty? (calls-of rig :release)))
    (advance! rig 1)
    (tick! rig)
    (is (= "idle" (nth (first (calls-of rig :release)) 2)))))

;; take idleS overrides the idle limit for that takeover
(deftest take-idle-s-overrides-the-idle-limit-for-that-takeover
  (let [rig (make-rig)]
    (post! rig {:op "take" :who "claude" :why "x" :idleS 120})
    (advance! rig 119999)
    (tick! rig)
    (is (empty? (calls-of rig :release)))
    (advance! rig 1)
    (tick! rig)
    (is (= "idle" (nth (first (calls-of rig :release)) 2)))))

;; take with idleS ${x} is bad-args and takes nothing
(deftest take-with-a-bad-idle-s-is-bad-args-and-takes-nothing
  (are [idle-s] (let [rig (make-rig)
                      r (post! rig {:op "take" :who "claude" :why "x" :idleS idle-s})]
                  (and (= "bad-args" (get-in r [:json :reason]))
                       (empty? (calls-of rig :take))
                       (nil? (manual rig))))
    0 0.5 3601 "5" nil js/NaN true))

;; the lease view reports idleMs, expiresAt and idleLeftS, counting down from the last op
;; (adapted: ping no longer moves expiresAt; set does)
(deftest the-lease-view-reports-idle-ms-expires-at-and-idle-left-s-counting-down-from-the-last-op
  (let [rig (make-rig)
        r (post! rig {:op "take" :who "claude" :why "x" :idleS 30})]
    (is (= 30000 (get-in r [:json :manual :idleMs])))
    (is (= 31000 (get-in r [:json :manual :expiresAt])))
    (is (= 30 (get-in r [:json :manual :idleLeftS])))
    (advance! rig 12340)
    (let [g (manual rig)]
      (is (= 31000 (:expiresAt g)))
      (is (= 17.7 (:idleLeftS g))))
    (let [p (get-in (post! rig {:op "ping" :who "claude"}) [:json :manual])]
      (is (= 31000 (:expiresAt p)))
      (is (= 17.7 (:idleLeftS p))))
    (let [s (get-in (post! rig {:op "set" :who "claude" :controls {:jump true}}) [:json :manual])]
      (is (= 43340 (:expiresAt s)))
      (is (= 30 (:idleLeftS s))))))

;; idleLeftS never goes below zero
(deftest idle-left-s-never-goes-below-zero
  (let [rig (taken-rig)]
    (advance! rig 20000)
    (is (= 0 (:idleLeftS (manual rig))))))

;; GET does not touch the lease: it leaves the silence clock and expiry alone
(deftest get-does-not-touch-the-lease-it-leaves-the-silence-clock-and-expiry-alone
  (let [rig (taken-rig)]
    (advance! rig 14000)
    (get! rig)
    (get! rig)
    (is (= 16000 (:expiresAt (manual rig))))
    (advance! rig 1000)
    (tick! rig)
    (is (= "idle" (nth (first (calls-of rig :release)) 2)))))

;; new: the two clocks

(deftest ping-does-not-extend-the-idle-limit
  (let [rig (taken-rig)]
    (advance! rig 14000)
    (post! rig {:op "ping" :who "claude"})
    (advance! rig 1000)
    (tick! rig)
    (is (= [[:release "claude" "idle" 15000]] (calls-of rig :release)))))

(deftest set-extends-the-idle-limit
  (let [rig (taken-rig)]
    (advance! rig 14000)
    (post! rig {:op "set" :who "claude" :controls {:jump true} :ms 10})
    (advance! rig 1000)
    (tick! rig)
    (is (empty? (calls-of rig :release)))
    (is (= 30000 (:expiresAt (manual rig))))))

(deftest input-ops-decides-what-counts-as-input
  (is (= #{"take" "set" "stop" "release"} lease/input-ops))
  (let [rig (taken-rig {:opts {:input-ops (conj lease/input-ops "ping")}})]
    (advance! rig 14000)
    (post! rig {:op "ping" :who "claude"})
    (advance! rig 1000)
    (tick! rig)
    (is (empty? (calls-of rig :release)))
    (is (= 30000 (:expiresAt (manual rig))))))

;; the wire contract, scenario by scenario

(def start 1000000)
(def contract (tu/read-json "test/contract/drive-contract.json"))

(def contract-pos {:x 0 :y 64 :z 0})

(defn contract-rig []
  (let [look (atom {:yaw 0 :pitch 0})]
    (make-rig {:t start
               :opts {:release-ms 1000 :idle-ms 15000}
               :drive-fn (fn [{l :look}]
                           (swap! look merge (select-keys l [:yaw :pitch]))
                           (assoc @look :pos contract-pos))})))

(defn without-expiry [reply] (update reply :json #(cond-> % (:manual %) (update :manual dissoc :expiresAt :idleLeftS))))
(defn expiry-of [reply] (select-keys (get-in reply [:json :manual]) [:expiresAt :idleLeftS]))

(defn mismatch
  "nil when actual matches expected; [at actual-expiry expected-expiry] when only expiresAt/idleLeftS
  differ; [at actual expected] otherwise."
  [at actual expected]
  (cond
    (= expected actual) nil
    (= (without-expiry expected) (without-expiry actual)) [at (expiry-of actual) (expiry-of expected)]
    :else [at actual expected]))

(defn run-step [rig {:keys [at status tick req expect]}]
  (swap! rig assoc :t (+ start at))
  (cond
    status (do (set-world! rig status) nil)
    tick (do (tick! rig) nil)
    :else (mismatch at (select-keys (handle! rig req) [:status :json]) expect)))

(defn run-scenario [steps]
  (let [rig (contract-rig)]
    (set-world! rig {:pos contract-pos})
    (vec (keep #(run-step rig %) steps))))

;; The only intended difference from the JS contract: ping is a heartbeat, not input, so it no longer
;; moves expiresAt (and idleLeftS follows). Everything else must match exactly.
(def expected-different
  {"ping moves expiresAt" [[5000 {:expiresAt 1015000 :idleLeftS 10} {:expiresAt 1020000 :idleLeftS 15}]
                           [5100 {:expiresAt 1015000 :idleLeftS 9.9} {:expiresAt 1020000 :idleLeftS 14.9}]]})

(deftest every-drive-contract-scenario-matches-the-wire
  (doseq [{:keys [name steps]} contract]
    (is (= (get expected-different name []) (run-scenario steps)) name)))
