(ns view.drive-test
  "The view page's drive rules as drive.mjs calls them: JS values in, JS values out (ported from test/view-drive-keys.test.mjs;
   the rules themselves are drive.keys, tested in drive.keys-test)."
  (:require [clojure.test :refer [deftest are is async]]
            [view.drive :as d]))

(defn js= [a b] (= (js->clj a) (js->clj b)))

(deftest control-for-cases
  (are [code expected] (= expected (d/control-for code))
    "KeyW" "forward" "KeyS" "back" "KeyA" "left" "KeyD" "right" "Space" "jump"
    "ShiftLeft" "sneak" "ShiftRight" "sneak" "KeyR" "sprint"
    "ControlLeft" nil "KeyF" nil "ArrowUp" nil))

(deftest look-step-for-cases
  (are [code expected] (js= expected (d/look-step-for code))
    "ArrowLeft" #js {:dyaw -15} "ArrowRight" #js {:dyaw 15} "ArrowUp" #js {:dpitch -10} "ArrowDown" #js {:dpitch 10}
    "KeyW" nil))

(defn near? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest mouse-look-cases
  (are [got dyaw dpitch] (and (near? dyaw (.-dyaw got)) (near? dpitch (.-dpitch got)))
    (d/mouse-look 10 20) 1.5 3
    (d/mouse-look -10 -20) -1.5 -3
    (d/mouse-look 10 10 0.5) 5 5
    (d/mouse-look 0 0) 0 0))

(deftest merge-look-cases
  (are [a b expected] (js= expected (d/merge-look a b))
    #js {:dyaw 1 :dpitch 2} #js {:dyaw 3 :dpitch -1} #js {:dyaw 4 :dpitch 1}
    #js {:dyaw 1} #js {:dpitch 2} #js {:dyaw 1 :dpitch 2}
    #js {} #js {} #js {:dyaw 0 :dpitch 0}))

(deftest banner-text-cases
  (are [manual me expected] (= expected (d/banner-text manual me))
    nil "view" nil
    js/undefined "view" nil
    #js {:who "view" :why "x"} "view" "MANUAL CONTROL (you) — WASD move, space jump, shift sneak, R sprint, arrows/mouse look, G release"
    #js {:who "claude" :why "testing"} "view" "MANUAL CONTROL by claude: testing"))

(deftest who-from-cases
  (are [search expected] (= expected (d/who-from search))
    "?agent=Bob" "view"
    "?agent=Bob&who=dash:Bob" "dash:Bob"
    "?who=a_b-C1" "a_b-C1"
    "?who=" "view"
    "?who=has space" "view"
    "?who=a/b" "view"
    (str "?who=" (apply str (repeat 41 "x"))) "view"
    (str "?who=" (apply str (repeat 40 "x"))) (apply str (repeat 40 "x"))
    "" "view")
  (is (= "fallback" (d/who-from "" "fallback"))))

(deftest should-take-on-click-cases
  (are [args expected] (= expected (d/should-take-on-click args))
    #js {:embed true :driving false :manual nil :me "v"} true
    #js {:embed true :driving false :manual #js {:who "v"} :me "v"} true
    #js {:embed true :driving false :manual #js {:who "claude"} :me "v"} false
    #js {:embed true :driving true :manual #js {:who "v"} :me "v"} false
    #js {:embed false :driving false :manual nil :me "v"} false))

(deftest should-release-on-escape-cases
  (are [args expected] (= expected (d/should-release-on-escape args))
    #js {:code "Escape" :driving true :pointerLocked false} true
    #js {:code "Escape" :driving true :pointerLocked true} false
    #js {:code "Escape" :driving false :pointerLocked false} false
    #js {:code "KeyG" :driving true :pointerLocked false} false))

(deftest leave-action-cases
  (are [event expected] (= expected (d/leave-action event))
    "pointerlock-lost" "stop-release" "hidden" "stop" "blur" "stop" "pagehide" "stop" "click" nil js/undefined nil))

(deftest holds-body-cases
  (are [args expected] (= expected (d/holds-body args))
    #js {:driving true :reply #js {:ok true :manual #js {:who "me"}} :me "me"} true
    #js {:driving false :reply #js {:ok true :manual nil} :me "me"} false
    #js {:driving true :reply #js {:manual nil} :me "me"} false
    #js {:driving true :reply #js {:manual #js {:who "bob"}} :me "me"} false
    #js {:driving true :reply #js {:manual #js {:who "me"} :offline true} :me "me"} false
    #js {:driving true :reply nil :me "me"} false
    #js {:driving true :reply #js {:ok false :reason "not-taken"} :me "me"} false
    #js {:driving true :reply #js {:ok false :reason "not-driver" :manual #js {:who "me"}} :me "me"} false))

(deftest is-stale-cases
  (are [started current expected] (= expected (d/is-stale (js-obj "startedGen" started "currentGen" current)))
    0 0 false 1 1 false 0 1 true 2 5 true))

(deftest should-drop-cases
  (are [args expected] (= expected (d/should-drop args))
    ;; a poll reply started before the take does not drop it
    #js {:driving true :reply #js {:manual nil} :me "me" :startedGen 0 :currentGen 1} false
    ;; a current reply showing manual null drops the takeover
    #js {:driving true :reply #js {:manual nil} :me "me" :startedGen 1 :currentGen 1} true
    ;; a current failed request drops the takeover
    #js {:driving true :reply nil :me "me" :startedGen 1 :currentGen 1} true
    ;; not driving never drops
    #js {:driving false :reply nil :me "me" :startedGen 1 :currentGen 1} false))

(defn deferred []
  (let [d #js {}]
    (set! (.-promise d) (js/Promise. (fn [resolve reject] (set! (.-resolve d) resolve) (set! (.-reject d) reject))))
    d))

(defn tick [] (js/Promise. (fn [resolve] (js/setImmediate resolve))))

(deftest serial-queue-waits-for-the-previous-call
  (async done
    (let [log (atom [])
          first-call (deferred)
          enqueue (d/serial-queue)
          _ (enqueue (fn [] (swap! log conj "start1") (.-promise first-call)))
          second-call (enqueue (fn [] (swap! log conj "start2") "two"))]
      (-> (tick)
          (.then (fn [_]
                   (is (= ["start1"] @log))
                   ((.-resolve first-call) "one")
                   second-call))
          (.then (fn [v]
                   (is (= "two" v))
                   (is (= ["start1" "start2"] @log))))
          (.finally done)))))

(deftest serial-queue-runs-on-after-a-rejection-and-passes-results-through
  (async done
    (let [enqueue (d/serial-queue)
          failed (enqueue (fn [] (js/Promise.reject (js/Error. "boom"))))
          next-call (enqueue (fn [] "ok"))]
      (-> failed
          (.then (fn [_] (is false "should have rejected"))
                 (fn [e] (is (= "boom" (.-message e)))))
          (.then (fn [_] next-call))
          (.then (fn [v] (is (= "ok" v))))
          (.finally done)))))

(defn hanging-fetch [log]
  (fn [url init]
    (js/Promise. (fn [_ reject]
                   (swap! log conj {:url url :init init})
                   (.addEventListener (.-signal init) "abort" #(reject (js/Error. "aborted")))))))

(deftest with-timeout-aborts-a-hung-request-and-passes-url-and-init-through
  (async done
    (let [log (atom [])
          f (d/with-timeout (hanging-fetch log) 10)]
      (-> (f "/x" #js {:method "POST"})
          (.then (fn [_] (is false "should have rejected"))
                 (fn [e]
                   (is (= "aborted" (.-message e)))
                   (is (= "/x" (:url (first @log))))
                   (is (= "POST" (.-method (:init (first @log)))))))
          (.finally done)))))

(deftest with-timeout-returns-a-fast-response-and-does-not-abort-it-later
  (async done
    (let [signals (atom [])
          f (d/with-timeout (fn [_ init] (swap! signals conj (.-signal init)) (js/Promise.resolve "ok")) 10)]
      (-> (f "/x")
          (.then (fn [v]
                   (is (= "ok" v))
                   (js/Promise. (fn [resolve] (js/setTimeout resolve 30)))))
          (.then (fn [_] (is (false? (.-aborted (first @signals))))))
          (.finally done)))))

(deftest a-timed-out-request-does-not-hold-the-serial-queue
  (async done
    (let [f (d/with-timeout (hanging-fetch (atom [])) 10)
          enqueue (d/serial-queue)
          first-call (enqueue (fn [] (.catch (f "/a" #js {}) (fn [_] "timeout"))))
          second-call (enqueue (fn [] "stop"))]
      (-> (js/Promise.all #js [first-call second-call])
          (.then (fn [vs] (is (= ["timeout" "stop"] (vec vs)))))
          (.finally done)))))
