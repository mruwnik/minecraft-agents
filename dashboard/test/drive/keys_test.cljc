(ns drive.keys-test
  (:require [clojure.test :refer [deftest are is]]
            [drive.keys :as k]))

(deftest control-for-cases
  (are [code expected] (= expected (k/control-for code))
    "KeyW" :forward "KeyS" :back "KeyA" :left "KeyD" :right "Space" :jump
    "ShiftLeft" :sneak "ShiftRight" :sneak "KeyR" :sprint
    "ControlLeft" nil "KeyF" nil "ArrowUp" nil nil nil))

(deftest look-step-for-cases
  (are [code expected] (= expected (k/look-step-for code))
    "ArrowLeft" {:dyaw -15} "ArrowRight" {:dyaw 15} "ArrowUp" {:dpitch -10} "ArrowDown" {:dpitch 10}
    "KeyW" nil))

(defn near? [a b] (< (Math/abs (- a b)) 1e-9))

(deftest mouse-look-cases
  (are [mx my sens dyaw dpitch] (let [got (k/mouse-look mx my sens)] (and (near? dyaw (:dyaw got)) (near? dpitch (:dpitch got))))
    10 20 0.15 1.5 3
    -10 -20 0.15 -1.5 -3
    10 10 0.5 5 5
    0 0 0.15 0 0)
  (let [got (k/mouse-look 10 20)] (is (near? 1.5 (:dyaw got))) (is (near? 3 (:dpitch got)))))

(deftest merge-look-cases
  (are [a b expected] (= expected (k/merge-look a b))
    {:dyaw 1 :dpitch 2} {:dyaw 3 :dpitch -1} {:dyaw 4 :dpitch 1}
    {:dyaw 1} {:dpitch 2} {:dyaw 1 :dpitch 2}
    {} {} {:dyaw 0 :dpitch 0}
    nil nil {:dyaw 0 :dpitch 0}))

(deftest banner-text-cases
  (are [manual me expected] (= expected (k/banner-text manual me))
    nil "view" nil
    {:who "view" :why "x"} "view" "MANUAL CONTROL (you) — WASD move, space jump, shift sneak, R sprint, arrows/mouse look, G release"
    {:who "claude" :why "testing"} "view" "MANUAL CONTROL by claude: testing"))

(deftest who-from-cases
  (are [search expected] (= expected (k/who-from search))
    "?agent=Bob" "view"
    "?agent=Bob&who=dash:Bob" "dash:Bob"
    "?who=a_b-C1" "a_b-C1"
    "?who=" "view"
    "?who" "view"
    "?who=has space" "view"
    "?who=has%20space" "view"
    "?who=dash%3ABob" "dash:Bob"
    "?who=a/b" "view"
    "?who=%zz" "view"
    "?who=first&who=second" "first"
    (str "?who=" (apply str (repeat 41 "x"))) "view"
    (str "?who=" (apply str (repeat 40 "x"))) (apply str (repeat 40 "x"))
    "" "view"
    nil "view")
  (is (= "fallback" (k/who-from "" "fallback"))))

(deftest should-take-on-click-cases
  (are [args expected] (= expected (k/should-take-on-click? args))
    {:embed? true :driving? false :manual nil :me "v"} true
    {:embed? true :driving? false :manual {:who "v"} :me "v"} true
    {:embed? true :driving? false :manual {:who "claude"} :me "v"} false
    {:embed? true :driving? true :manual {:who "v"} :me "v"} false
    {:embed? false :driving? false :manual nil :me "v"} false))

(deftest should-release-on-escape-cases
  (are [args expected] (= expected (k/should-release-on-escape? args))
    {:code "Escape" :driving? true :pointer-locked? false} true
    {:code "Escape" :driving? true :pointer-locked? true} false
    {:code "Escape" :driving? false :pointer-locked? false} false
    {:code "KeyG" :driving? true :pointer-locked? false} false))

(deftest leave-action-cases
  (are [event expected] (= expected (k/leave-action event))
    :pointerlock-lost :stop-release :hidden :stop :blur :stop :pagehide :stop :click nil nil nil))

(deftest holds-body-cases
  (are [driving? reply expected] (= expected (k/holds-body? {:driving? driving? :reply reply :me "me"}))
    true {:ok true :manual {:who "me"}} true
    false {:ok true :manual nil} false
    true {:manual nil} false
    true {:manual {:who "bob"}} false
    true {:manual {:who "me"} :offline true} false
    true nil false
    true {:ok false :reason "not-taken"} false
    true {:ok false :reason "not-driver" :manual {:who "me"}} false))

(deftest stale-cases
  (are [started current expected] (= expected (k/stale? {:started-gen started :current-gen current}))
    0 0 false 1 1 false 0 1 true 2 5 true))

(deftest should-drop-cases
  (are [args expected] (= expected (k/should-drop? (assoc args :me "me")))
    {:driving? true :reply {:manual nil} :started-gen 0 :current-gen 1} false
    {:driving? true :reply {:manual nil} :started-gen 1 :current-gen 1} true
    {:driving? true :reply nil :started-gen 1 :current-gen 1} true
    {:driving? false :reply nil :started-gen 1 :current-gen 1} false
    {:driving? true :reply {:manual {:who "me"}} :started-gen 1 :current-gen 1} false))
