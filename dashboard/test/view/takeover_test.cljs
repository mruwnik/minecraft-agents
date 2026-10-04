(ns view.takeover-test
  "The view page's takeover state machine (view.takeover) against a fake fetch and a fake DOM surface: what it sends, in which
   order, and what it shows. Ported in behaviour from the former tools/view/web/drive.mjs; the rules are drive.keys."
  (:require [clojure.test :refer [deftest are is async]]
            [view.takeover :as t]))

(defn tick [] (js/Promise. (fn [resolve] (js/setImmediate resolve))))

(defn chain
  "Runs the thunks one after another, each after the previous promise and a tick; returns the promise of all."
  [& thunks]
  (reduce (fn [p thunk] (.then p (fn [_] (.then (js/Promise.resolve (thunk)) (fn [_] (tick))))))
          (js/Promise.resolve nil) thunks))

(defn response [reply]
  #js {:headers #js {:get (fn [_] "application/json")} :json (fn [] (js/Promise.resolve reply))})

(def ok-taken #js {:ok true :manual #js {:who "view" :why "w" :expiresAt 99}})

(defn make-world
  "A driver on a fake env. :reply is (fn [op body]) -> JS reply, :fail for a failed request, nil for no json body."
  ([] (make-world {}))
  ([{:keys [reply embed gate] :or {reply (fn [_ _] ok-taken) embed false}}]
   (let [calls (atom [])
         renders (atom [])
         locks (atom [])
         fetch (fn [url ^js init]
                 (let [body (some-> (.-body init) js/JSON.parse)
                       method (or (.-method init) "GET")
                       op (some-> body (.-op))
                       call {:url url :method method :op op :body (some-> body (js->clj :keywordize-keys true)) :keepalive (.-keepalive init)}]
                   (swap! calls conj call)
                   (let [r (reply op body)
                         answer (fn [] (if (= :fail r) (js/Promise.reject (js/Error. "down")) (js/Promise.resolve (response r))))]
                     (if (and gate (= op (:op gate))) (.then (:promise gate) (fn [_] (answer))) (answer)))))
         env #js {:agent "w/Bob" :me "view" :embed embed :fetch fetch :timedFetch fetch
                  :render (fn [v] (swap! renders conj v))
                  :exitPointerLock (fn [] (swap! locks conj :exit))
                  :requestPointerLock (fn [] (swap! locks conj :request))}]
     {:d (t/create-driver env) :calls calls :renders renders :locks locks})))

(defn ops [w] (mapv :op (filter #(= "POST" (:method %)) @(:calls w))))
(defn last-render [w] (last @(:renders w)))
(defn key-info [code & {:as o}]
  (clj->js (merge {:code code :repeat false :ctrlKey false :metaKey false :altKey false :pointerLocked false} o)))

(defn driving-world
  "A world whose driver has taken over; calls cleared of the take."
  [& [opts]]
  (let [w (make-world opts)]
    (-> (.take ^js (:d w)) (.then (fn [_] (reset! (:calls w) []) (reset! (:renders w) []) w)))))

(deftest agent-key-cases
  (are [v expected] (= expected (t/agent-key? v))
    "claude/ProbeDrive" true "w_1/Bob-2" true "ProbeDrive" false "a/b/c" false "/b" false "a/" false "a b/c" false "" false nil false js/undefined false))

(deftest take-posts-take-and-shows-driving
  (async done
    (let [w (make-world)]
      (-> (.take ^js (:d w))
          (.then (fn [_]
                   (is (= [{:url "/drive/w/Bob" :method "POST" :op "take" :keepalive nil
                            :body {:op "take" :who "view" :why "driven from the view page"}}]
                          (filter #(= "POST" (:method %)) @(:calls w))))
                   (is (true? (.-driving ^js (last-render w))))
                   (is (= "release (G)" (.-buttonText ^js (last-render w))))
                   (is (true? (.-driving ^js (.state ^js (:d w)))))))
          (.finally done)))))

(deftest take-refused-shows-the-reason-and-does-not-drive
  (async done
    (let [w (make-world {:reply (fn [_ _] #js {:ok false :reason "busy"})})]
      (-> (.take ^js (:d w))
          (.then (fn [_]
                   (is (= "cannot take over: busy" (.-text ^js (last-render w))))
                   (is (false? (.-driving ^js (.state ^js (:d w)))))))
          (.finally done)))))

(deftest take-without-a-reply-says-no-running-body
  (async done
    (let [w (make-world {:reply (fn [_ _] :fail)})]
      (-> (.take ^js (:d w))
          (.then (fn [_]
                   (is (= "no running body w/Bob" (.-text ^js (last-render w))))
                   (is (false? (.-driving ^js (.state ^js (:d w)))))))
          (.finally done)))))

(deftest banner-says-you-or-the-other-driver-and-locks-the-button
  (async done
    (let [w (make-world {:reply (fn [_ _] #js {:ok true :manual #js {:who "claude" :why "testing" :expiresAt 5}})})]
      (-> (.poll ^js (:d w))
          (.then (fn [_] (tick)))
          (.then (fn [_]
                   (let [v ^js (last-render w)]
                     (is (= "MANUAL CONTROL by claude: testing" (.-text v)))
                     (is (true? (.-disabled v)))
                     (is (= "driven by claude" (.-title v)))
                     (is (false? (.-driving v)))
                     (is (= 5 (.-expiresAt (.-message v)))))))
          (.finally done)))))

(deftest g-toggles-but-not-with-a-modifier-a-repeat-or-another-driver
  (async done
    (let [w (make-world)]
      (-> (chain
           #(do (.keydown ^js (:d w) (key-info "KeyG" :ctrlKey true)) (js/Promise.resolve nil))
           #(do (.keydown ^js (:d w) (key-info "KeyG" :repeat true)) (js/Promise.resolve nil))
           #(do (is (= [] (ops w))) (.keydown ^js (:d w) (key-info "KeyG")) (js/Promise.resolve nil))
           #(do (is (= ["take"] (ops w))) (is (true? (.-driving ^js (.state ^js (:d w))))) (.keydown ^js (:d w) (key-info "KeyG")) (js/Promise.resolve nil))
           #(do (is (= ["take" "release"] (ops w))) (is (false? (.-driving ^js (.state ^js (:d w))))) (js/Promise.resolve nil)))
          (.finally done)))))

(deftest g-does-nothing-while-someone-else-drives
  (async done
    (let [w (make-world {:reply (fn [_ _] #js {:ok true :manual #js {:who "claude" :why "x"}})})]
      (-> (chain #(.poll ^js (:d w))
                 #(do (.keydown ^js (:d w) (key-info "KeyG")) (js/Promise.resolve nil)))
          (.then (fn [_] (is (= [] (ops w)))))
          (.finally done)))))

(deftest movement-keys-send-controls-and-are-swallowed-only-while-driving
  (async done
    (let [w (make-world)]
      (-> (chain
           #(do (is (false? (.keydown ^js (:d w) (key-info "KeyW")))) (js/Promise.resolve nil))
           #(do (is (= [] (ops w)) "not driving: nothing sent") (js/Promise.resolve nil))
           #(.take ^js (:d w))
           #(do (reset! (:calls w) [])
                (is (true? (.keydown ^js (:d w) (key-info "KeyW"))))
                (is (true? (.keydown ^js (:d w) (key-info "KeyW" :repeat true))))
                (is (true? (.keyup ^js (:d w) (key-info "KeyW"))))
                (is (false? (.keydown ^js (:d w) (key-info "KeyQ"))))
                (js/Promise.resolve nil)))
          (.then (fn [_]
                   (is (= [{:op "set" :who "view" :controls {:forward true}}
                           {:op "set" :who "view" :controls {:forward false}}]
                          (mapv :body @(:calls w))))))
          (.finally done)))))

(deftest arrows-send-look-steps-including-repeats
  (async done
    (-> (driving-world)
        (.then (fn [w]
                 (.keydown ^js (:d w) (key-info "ArrowLeft"))
                 (.keydown ^js (:d w) (key-info "ArrowDown" :repeat true))
                 (.then (tick)
                        (fn [_] (is (= [{:op "set" :who "view" :look {:dyaw -15}}
                                        {:op "set" :who "view" :look {:dpitch 10}}]
                                       (mapv :body @(:calls w))))))))
        (.finally done))))

(deftest escape-with-the-pointer-lock-held-does-not-release
  (async done
    (-> (driving-world)
        (.then (fn [w]
                 (.keydown ^js (:d w) (key-info "Escape" :pointerLocked true))
                 (.then (tick) (fn [_] (is (= [] (ops w)))))))
        (.finally done))))

(deftest escape-without-lock-releases
  (async done
    (-> (driving-world)
        (.then (fn [w]
                 (.keydown ^js (:d w) (key-info "Escape"))
                 (.then (tick) (fn [_] (is (= ["release"] (ops w)))))))
        (.finally done))))

(deftest click-takes-when-embedded-and-asks-for-the-lock-when-driving
  (async done
    (let [w (make-world {:embed true})]
      (-> (chain #(do (.click ^js (:d w)) (js/Promise.resolve nil))
                 #(do (is (= ["take"] (ops w))) (.click ^js (:d w)) (js/Promise.resolve nil)))
          (.then (fn [_]
                   (is (= ["take"] (ops w)))
                   (is (= [:request] @(:locks w)))))
          (.finally done)))))

(deftest click-outside-an-embed-does-not-take
  (async done
    (let [w (make-world)]
      (-> (chain #(do (.click ^js (:d w)) (js/Promise.resolve nil)))
          (.then (fn [_] (is (= [] (ops w)))))
          (.finally done)))))

(deftest mouse-moves-merge-into-one-look-per-flush
  (async done
    (-> (driving-world)
        (.then (fn [w]
                 (.mousemove ^js (:d w) 10 20 false)
                 (.mousemove ^js (:d w) 10 0 true)
                 (.mousemove ^js (:d w) 10 20 true)
                 (.flushLook ^js (:d w))
                 (.flushLook ^js (:d w))
                 (.then (tick)
                        (fn [_] (let [[look] (map :body @(:calls w))]
                                  (is (= 1 (count @(:calls w))))
                                  (is (= "set" (:op look)))
                                  (is (= 3 (get-in look [:look :dyaw])))
                                  (is (= 3 (get-in look [:look :dpitch]))))))))
        (.finally done))))

(deftest mouse-moves-are-ignored-when-not-driving
  (async done
    (let [w (make-world)]
      (.mousemove ^js (:d w) 10 20 true)
      (.flushLook ^js (:d w))
      (-> (tick) (.then (fn [_] (is (= [] (ops w))))) (.finally done)))))

(deftest ping-only-while-driving
  (async done
    (let [w (make-world)]
      (.ping ^js (:d w))
      (-> (tick)
          (.then (fn [_] (is (= [] (ops w))) (.take ^js (:d w))))
          (.then (fn [_] (.ping ^js (:d w)) (tick)))
          (.then (fn [_] (is (= ["take" "ping"] (ops w)))))
          (.finally done)))))

(deftest blur-and-hidden-stop-but-keep-the-takeover
  (async done
    (-> (driving-world)
        (.then (fn [w]
                 (.leave ^js (:d w) "blur")
                 (.leave ^js (:d w) "hidden")
                 (.then (tick)
                        (fn [_]
                          (is (= ["stop" "stop"] (ops w)))
                          (is (true? (.-driving ^js (.state ^js (:d w)))))))))
        (.finally done))))

(deftest pointer-lock-lost-stops-then-releases
  (async done
    (-> (driving-world)
        (.then (fn [w]
                 (.leave ^js (:d w) "pointerlock-lost")
                 (.then (tick) (fn [_]
                                 (is (= ["stop" "release"] (ops w)))
                                 (is (false? (.-driving ^js (.state ^js (:d w)))))))))
        (.finally done))))

(deftest pagehide-posts-stop-at-once-with-keepalive-and-no-release
  (async done
    (let [gate (let [d #js {}] (set! (.-promise d) (js/Promise. (fn [r] (set! (.-resolve d) r)))) d)]
      (-> (driving-world {:gate {:op "ping" :promise (.-promise gate)}})
          (.then (fn [w]
                   (.ping ^js (:d w)) ; a request in flight holds the queue
                   (-> (tick)
                       (.then (fn [_]
                                (.leave ^js (:d w) "pagehide")
                                (is (= ["ping" "stop"] (ops w)) "stop is not queued behind the hung ping")
                                (is (true? (:keepalive (last @(:calls w)))))
                                ((.-resolve gate) nil)
                                (tick)))
                       (.then (fn [_] (is (= ["ping" "stop"] (ops w))))))))
          (.finally done)))))

(deftest leaving-when-not-driving-sends-nothing
  (async done
    (let [w (make-world)]
      (.leave ^js (:d w) "blur")
      (.leave ^js (:d w) "pagehide")
      (-> (tick) (.then (fn [_] (is (= [] (ops w))))) (.finally done)))))

(deftest pointer-lock-change-leaves-only-after-a-lock-was-held
  (async done
    (-> (driving-world)
        (.then (fn [w]
                 (.pointerLockChange ^js (:d w) false)
                 (.then (tick)
                        (fn [_]
                          (is (= [] (ops w)) "never locked: losing it means nothing")
                          (.pointerLockChange ^js (:d w) true)
                          (.pointerLockChange ^js (:d w) false)
                          (tick)))
                 (.then (tick) (fn [_] (is (= ["stop" "release"] (ops w)))))))
        (.finally done))))

(deftest a-poll-showing-no-manual-drops-the-takeover
  (async done
    (let [answer (atom ok-taken)
          w (make-world {:reply (fn [_ _] @answer)})]
      (-> (.take ^js (:d w))
          (.then (fn [_] (reset! answer #js {:ok true :manual nil}) (.poll ^js (:d w))))
          (.then (fn [_] (tick)))
          (.then (fn [_]
                   (is (false? (.-driving ^js (.state ^js (:d w)))))
                   (is (= "take over (G)" (.-buttonText ^js (last-render w))))
                   (is (= [:exit] @(:locks w)))))
          (.finally done)))))

(deftest a-poll-started-before-a-take-does-not-drop-it
  (async done
    (let [gate (let [d #js {}] (set! (.-promise d) (js/Promise. (fn [r] (set! (.-resolve d) r)))) d)
          w (make-world {:reply (fn [op _] (if (nil? op) #js {:ok true :manual nil} ok-taken))
                         :gate {:op nil :promise (.-promise gate)}})
          polled (.poll ^js (:d w))]
      (-> (.take ^js (:d w))
          (.then (fn [_] ((.-resolve gate) nil) polled))
          (.then (fn [_] (tick)))
          (.then (fn [_]
                   (is (true? (.-driving ^js (.state ^js (:d w)))))
                   (is (= "view" (.-who (.-manual ^js (.state ^js (:d w))))) "the old reply did not overwrite the manual")))
          (.finally done)))))

(deftest requests-reach-the-body-in-order
  (async done
    (let [gate (let [d #js {}] (set! (.-promise d) (js/Promise. (fn [r] (set! (.-resolve d) r)))) d)]
      (-> (driving-world {:gate {:op "set" :promise (.-promise gate)}})
          (.then (fn [w]
                   (.keydown ^js (:d w) (key-info "KeyW"))
                   (.keyup ^js (:d w) (key-info "KeyW"))
                   (.then (tick)
                          (fn [_]
                            (is (= [true] (mapv #(get-in % [:body :controls :forward]) @(:calls w))) "keyup waits for keydown")
                            ((.-resolve gate) nil)
                            (tick)))
                   ))
          (.finally done)))))

(deftest state-and-sent-are-exposed-for-measurement
  (async done
    (let [w (make-world)]
      (-> (.take ^js (:d w))
          (.then (fn [_]
                   (let [s ^js (.state ^js (:d w))]
                     (is (= 99 (.-expiresAt s)))
                     (is (= "view" (.-who (.-manual s))))
                     (is (= ["take"] (mapv #(.-op ^js %) (.-sent ^js (:d w))))))))
          (.finally done)))))
