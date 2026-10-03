(ns engine.takeover
  "Manual takeover: someone drives the body by hand through the control socket (js/control.mjs, a
  stateless adapter that calls handle). The rules are engine.lease's (pure); this namespace applies their
  effects to the engine and is the only owner of the state: (:manual eng) holds the lease map plus :token.
  take! cuts the holder like a reflex and gives the ownership token to the driver; the scheduler
  stands still (core/paused?) until the lease ends. The manual state never reaches engine.edn."
  (:require [engine.core :as core]
            [engine.lease :as lease]))

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

(defn apply-effect!
  "Do one lease effect to the engine; the answer (take's {:ok :reason}, drive's {:pos :yaw :pitch}) or nil."
  [eng [kind & args]]
  (case kind
    :take (let [[who why] args] (take! eng {:who who :why why}))
    :drive (js->clj (drive! eng (clj->js (first args))) :keywordize-keys true)
    :stop-driving (.stopDriving (:primitives eng))
    :release (let [[who reason held-ms] args] (release! eng {:who who :reason reason :held-ms held-ms}))
    :deadman (let [[who silent-ms] args] (deadman! eng {:who who :silent-ms silent-ms}))))

(defn store!
  "Keep the lease (nil: nobody drives) in (:manual eng), with the ownership token the take left there."
  [eng lease]
  (let [token (:token @(:manual eng))]
    (reset! (:manual eng) (some-> lease (assoc :token (or (:token lease) token))))))

(defn world-of [eng]
  {:offline (core/offline? eng)
   :settling (core/settling? eng)
   :pos (js->clj (.-pos (.self (:primitives eng))) :keywordize-keys true)})

(defn finish
  "The lease and reply once the pending question (:take, :drive) has its answer."
  [{:keys [lease reply pending]} answer now]
  (case pending
    :take (lease/taken lease answer now)
    :drive (lease/driven lease answer now)
    {:lease lease :reply reply}))

(defn handle
  "One control request: method, path and the parsed JSON body (a JS value or nil). Returns #js {:status :json}."
  [eng opts method path body]
  (let [now (core/now eng)
        req {:method method :path path :body (js->clj body :keywordize-keys true)}
        r (lease/request @(:manual eng) req now (world-of eng) opts)
        answers (mapv #(apply-effect! eng %) (:effects r))
        {:keys [lease reply]} (finish r (first answers) now)]
    (store! eng lease)
    #js {:status (:status reply) :json (clj->js (:json reply))}))

(defn tick!
  "Time passing for the lease: apply what lease/tick decides (idle, offline, due holds, the dead-man)."
  [eng opts]
  (let [{:keys [lease effects]} (lease/tick @(:manual eng) (core/now eng) (world-of eng) opts)]
    (run! #(apply-effect! eng %) effects)
    (store! eng lease)))

(defn close!
  "End a held takeover for shutdown (call before core/shutdown!)."
  [eng]
  (run! #(apply-effect! eng %) (lease/close @(:manual eng) (core/now eng)))
  (store! eng nil))
