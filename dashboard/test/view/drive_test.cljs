(ns view.drive-test
  "view.drive's JS-value adapters (the rules themselves are drive.keys, tested in drive.keys-test)."
  (:require [clojure.test :refer [deftest are is async]]
            [view.drive :as d]))

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
