(ns engine.takeover
  "Manual takeover: someone drives the body by hand through the control socket (js/control.mjs).
  take! cuts the holder like a reflex and gives the ownership token to the driver; the scheduler
  stands still (core/paused?) until release!. The manual state lives in its own atom, never in engine.edn."
  (:require [engine.core :as core]))

(defn take!
  "Take the body for who. {:ok true}, or {:ok false :reason r} (offline, settling, held-by <who>)."
  [eng {:keys [who why]}]
  (cond
    (core/offline? eng) {:ok false :reason "offline"}
    (core/settling? eng) {:ok false :reason "settling"}
    (core/manual? eng) {:ok false :reason (str "held-by " (:who @(:manual eng)))}
    :else
    (let [token (str "m" (swap! (:tokens eng) inc))]
      (when-let [h (core/holder eng)] (core/cut! eng h :takeover nil))
      (core/set-owner! eng token)
      (reset! (:manual eng) {:who who :why why :since (core/now eng) :token token})
      (core/emit! eng {:source :system :kind :takeover_started :level :info :who who :why why
                       :text (str "manual control by " who ": " why "; jobs and reflexes paused")})
      {:ok true})))

(defn release!
  "End the takeover: controls cleared, owner nil, the scheduler resumes on the next tick."
  [eng {:keys [who reason held-ms]}]
  (when (core/manual? eng)
    (.stopDriving (:primitives eng))
    (core/set-owner! eng nil)
    (reset! (:manual eng) nil)
    (core/emit! eng {:source :system :kind :takeover_ended :level :info :who who :reason reason :held-ms held-ms
                     :text (str "manual control by " who " ended: " reason "; jobs and reflexes resume")})))

(defn drive!
  "Set controls and look for the manual token; args is a JS object {controls look}. Returns the JS result."
  [eng args]
  (.drive (:primitives eng) (:token @(:manual eng)) args))

(defn deadman! [eng {:keys [who silent-ms]}]
  (core/emit! eng {:source :system :kind :drive_deadman :level :warn :who who :silent-ms silent-ms
                   :text (str "driver " who " silent " silent-ms " ms: controls released")}))

(defn adapter
  "The body interface js/control.mjs expects, over eng."
  [eng]
  (let [p (:primitives eng)]
    #js {:status (fn [] #js {:offline (core/offline? eng)
                             :settling (core/settling? eng)
                             :pos (.-pos (.self p))})
         :take (fn [a] (let [{:keys [ok reason]} (take! eng {:who (.-who a) :why (.-why a)})]
                         #js {:ok ok :reason reason}))
         :release (fn [a] (release! eng {:who (.-who a) :reason (.-reason a) :held-ms (.-heldMs a)}))
         :drive (fn [a] (drive! eng a))
         :stopDriving (fn [] (.stopDriving p))
         :deadman (fn [a] (deadman! eng {:who (.-who a) :silent-ms (.-silentMs a)}))}))
