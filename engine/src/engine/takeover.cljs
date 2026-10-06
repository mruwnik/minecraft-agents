(ns engine.takeover
  "Manual takeover: someone drives the body by hand through the control socket (js/control.mjs, a
  stateless adapter that calls handle). The rules are engine.lease's (pure); this namespace applies their
  effects to the engine and is the only owner of the state: (:manual eng) holds the lease map plus :token.
  take! cuts the holder like a reflex and gives the ownership token to the driver; the scheduler
  runs only the driver's slot job (core/tick!) until the lease ends. The manual state never reaches engine.edn."
  (:require [engine.core :as core]
            [engine.entity-observations :as entity-observations]
            [engine.lease :as lease]))

(defn own-lease? [eng who]
  (and (core/manual? eng) (= who (:who @(:manual eng)))))

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
      (reset! (:manual-job eng) nil)
      (reset! (:manual eng) {:who who :why why :since (core/now eng) :token token})
      (core/emit! eng {:source :system :kind :takeover_started :level :info :who who :why why
                       :text (str "manual control by " who ": " why "; jobs and reflexes paused")})
      {:ok true})))

(defn release!
  "End the takeover: the slot job is cancelled, controls cleared, owner nil, the loop and triggers resume on the next tick."
  [eng {:keys [who reason held-ms]}]
  (when (core/manual? eng)
    (when-let [id (core/manual-job eng)] (core/cancel! eng id :release))
    (reset! (:manual-job eng) nil)
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
   :away (let [a (core/away eng)] (when-not (= :connection (:by a)) a)) ; offline with no :away: a kick or crash
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
  "Route one control-socket request: /entities answers from the seen-entities store, anything else is a /drive request in its JSON wire format."
  [eng opts method path body content-type]
  (cond
    (= path "/entities") (entity-observations/request (:seen-entities eng) method)
    :else
    (let [body-map (js->clj body :keywordize-keys true)]
      (cond
        (and (= path "/drive") (= method "POST") (= "stop" (:op body-map)) (own-lease? eng (:who body-map))
             (core/manual-job eng))
        (do (core/cancel! eng (core/manual-job eng) :driver)
            (handle eng opts method path body content-type))
        (and (= path "/drive") (= method "POST") (= "set" (:op body-map)) (own-lease? eng (:who body-map))
             (core/manual-job eng) (not (contains? (:failed (core/state eng)) (core/manual-job eng))))
        #js {:status 409 :json #js {:ok false :reason "job-running" :job (core/manual-job eng)}}
        :else
        (let [now (core/now eng)
              req {:method method :path path :body body-map}
              r (lease/request @(:manual eng) req now (world-of eng) opts)
              answers (mapv #(apply-effect! eng %) (:effects r))
              {:keys [lease reply]} (finish r (first answers) now)]
          (store! eng lease)
          #js {:status (:status reply) :json (clj->js (:json reply))})))))

(defn tick!
  "Time passing for the lease: apply what lease/tick decides (idle, offline, due holds, the dead-man). A slot round in
  flight counts as the driver's activity and beats the lease."
  [eng opts]
  (when (:id (core/running eng))
    (swap! (:manual eng) #(some-> % (assoc :last-beat (core/now eng)))))
  (let [{:keys [lease effects]} (lease/tick @(:manual eng) (core/now eng) (world-of eng) opts)]
    (run! #(apply-effect! eng %) effects)
    (store! eng lease)))

(defn close!
  "End a held takeover for shutdown (call before core/shutdown!)."
  [eng]
  (run! #(apply-effect! eng %) (lease/close @(:manual eng) (core/now eng)))
  (store! eng nil))
