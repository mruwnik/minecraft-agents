(ns engine.takeover
  "Manual takeover: someone drives the body by hand through the control socket (js/control.mjs, a
  stateless adapter that calls handle). The rules are engine.lease's (pure); this namespace applies their
  effects to the engine and is the only owner of the state: (:manual eng) holds the lease map plus :token.
  take! cuts the holder like a reflex and gives the ownership token to the driver; the scheduler
  stands still (core/paused?) until the lease ends. The manual state never reaches engine.edn."
  (:require [engine.core :as core]
            [engine.entity-observations :as entity-observations]
            [engine.hooks :as hooks]
            [engine.lease :as lease]
            [cljs.reader :as reader]
            [clojure.string :as str]
            ["crypto" :as crypto]))

(def max-world-ops 32)
(def max-queued-world-ops
  "World actions that may wait behind the running one; more are refused queue-full."
  8)
(declare cancel-active! start-next-world-op!)
(def world-actions
  {:move-to {:method "moveTo" :timeout 10}
   :dig {:method "dig" :timeout 10}
   :place {:method "place" :timeout 5}
   :use-on {:method "useOn" :timeout 5}
   :interact {:method "interact" :timeout 2}
   :wear {:method "equip" :timeout 6}})

(defn world-reply [status value]
  #js {:status status :contentType "application/edn" :text (str (pr-str value) "\n")})

(defn finite-number? [x] (and (number? x) (js/Number.isFinite x)))
(defn valid-pos? [pos]
  (and (map? pos)
       (every? finite-number? ((juxt :x :y :z) pos))
       (<= -30000000 (:x pos) 30000000)
       (<= -64 (:y pos) 320)
       (<= -30000000 (:z pos) 30000000)))
(defn valid-label? [x] (and (string? x) (<= 1 (count x) 80)))
(defn valid-item? [x] (and (string? x) (<= 1 (count x) 64) (re-matches #"[a-z0-9_:-]+" x)))
(defn action-args-error [action args]
  (let [position-valid? (valid-pos? (:pos args))
        allowed (case action
                  :move-to #{:pos :range :maxDistance :timeoutS}
                  :dig #{:pos}
                  :place #{:pos :item}
                  :use-on #{:pos :item :face}
                  :interact #{:id :item}
                  :wear #{:item}
                  #{})]
    (cond
      (not (map? args)) "args must be a map"
      (seq (remove allowed (keys args))) "unknown action argument"
      (= action :move-to)
      (or (when-not position-valid? "move-to needs a finite :pos {x y z} in world bounds")
          (when (and (contains? args :range)
                     (not (and (finite-number? (:range args)) (<= 0 (:range args) 4)))) "range must be 0..4")
          (when (and (contains? args :maxDistance)
                     (not (and (finite-number? (:maxDistance args)) (<= 1 (:maxDistance args) 64)))) "maxDistance must be 1..64")
          (when (and (contains? args :timeoutS)
                     (not (and (finite-number? (:timeoutS args)) (<= 1 (:timeoutS args) 10)))) "timeoutS must be 1..10 (a manual move-to is one bounded step; for a longer walk submit the job jobs.movement.go-to, or chain move-to calls)"))
      (#{:dig :place :use-on} action)
      (or (when-not position-valid? (str (name action) " needs a finite :pos {x y z} in world bounds"))
          (when (and (= action :place) (not (valid-item? (:item args)))) "place needs a short :item name")
          (when (and (= action :use-on) (:item args) (not (valid-item? (:item args)))) "item must be a short item name")
          (when (and (= action :use-on) (:face args) (not (contains? #{"up" "down" "north" "south" "west" "east"} (:face args)))) "face must be up/down/north/south/west/east"))
      (= action :interact)
      (or (when-not (and (integer? (:id args)) (cljs.core/pos? (:id args))) "interact needs a positive entity :id")
          (when (and (:item args) (not (valid-item? (:item args)))) "item must be a short item name"))
      (= action :wear)
      (when (and (contains? args :item) (not (valid-item? (:item args)))) "item must be a short item name")
      :else "unknown action")))

(defn compact-inventory [eng]
  (let [s (.self (:primitives eng))
        inv (js->clj (.-inventory s) :keywordize-keys true)]
    {:position (js->clj (.-pos s) :keywordize-keys true)
     :health (.-health s) :food (.-food s)
     :inventory (->> inv (map #(select-keys % [:name :count :slot])) (take 40) vec)
     :more? (> (count inv) 40)}))

(defn world-record [eng id]
  (get-in @(:world-ops eng) [:records id]))

(defn current-attempt? [eng id attempt]
  (let [record (world-record eng id)]
    (and (= id (:active @(:world-ops eng)))
         (= attempt (:attempt-id record))
         (= :running (:status record)))))

(defn set-world-record! [eng id record]
  (swap! (:world-ops eng)
         (fn [{:keys [records order] :as ops}]
           (let [new? (not (contains? records id))
                 ids (if new? (conj order id) order)
                 ids (if (> (count ids) max-world-ops) (subvec ids (- (count ids) max-world-ops)) ids)
                 records (-> records (assoc id record) (select-keys ids))]
             (assoc ops :records records :order ids)))))

(defn action-event! [eng id kind data]
  (core/emit! eng {:source :action :kind kind :action-id id :data data}))

(defn compact-result [result]
  (when (map? result)
    (let [drops (:drops result)]
      (cond-> (-> (select-keys result [:status :pos :distance :reason :block :needed :before :after :consumed :hurt :health :worn :item])
                  (update :reason #(when % (subs (str %) 0 (min 160 (count (str %)))))))
        (seq drops) (assoc :drops (->> drops (take 8) vec))
        (> (count drops) 8) (assoc :more-drops? true)))))

(defn op-view [record]
  (cond-> (select-keys record [:request-id :action :status])
    (:result record) (assoc :result (compact-result (:result record)))
    (:reason record) (assoc :reason (let [s (str (:reason record))] (subs s 0 (min 200 (count s)))))))

(defn op-refuse [reason & [extra]] {:ok false :reason reason :detail extra})

(defn own-lease? [eng who]
  (and (core/manual? eng) (= who (:who @(:manual eng)))))

(defn not-driver-refusal
  "not-driver with {:holder :idle-left-s} of the current lease; not-taken when nobody drives."
  [eng]
  (if-not (core/manual? eng)
    (op-refuse "not-taken")
    (let [view (lease/view @(:manual eng) (core/now eng))]
      (op-refuse "not-driver" {:holder (:who view) :idle-left-s (:idleLeftS view)}))))

(defn rotate-token!
  "Give the lease a new ownership token synchronously: primitives cut the old promise and clear held controls."
  [eng]
  (let [token (str "m" (swap! (:tokens eng) inc))]
    (core/set-owner! eng token)
    (swap! (:manual eng) assoc :token token)))

(defn cancel-active! [eng reason]
  (when-let [id (:active @(:world-ops eng))]
    (when-let [record (world-record eng id)]
      (when (= :running (:status record))
        (set-world-record! eng id (assoc record :status :cancelled :reason reason :finished-at (core/now eng)))
        (swap! (:world-ops eng) assoc :active nil)
        ;; Rotate the token synchronously: primitives cut the old promise and clear held controls.
        (rotate-token! eng)
        (action-event! eng id :done {:name (name (:action record)) :status :cut :reason reason})
        id))))

(defn drop-queued!
  "Cancel queued world action id (it never starts): status :cancelled with reason, and its one action.done event."
  [eng id reason]
  (swap! (:world-ops eng) update :queue #(filterv (fn [q] (not= id q)) %))
  (when-let [record (world-record eng id)]
    (when (= :queued (:status record))
      (set-world-record! eng id (assoc record :status :cancelled :reason reason :finished-at (core/now eng)))
      (action-event! eng id :done {:name (name (:action record)) :status :cut :reason reason}))))

(defn drop-queue!
  "Cancel every queued world action, in order."
  [eng reason]
  (run! #(drop-queued! eng % reason) (:queue @(:world-ops eng))))

(defn cancel-world-op! [eng who id]
  (let [record (world-record eng id)]
    (cond
      (not (own-lease? eng who)) (not-driver-refusal eng)
      (nil? record) (op-refuse "operation-not-found")
      (not= who (:who record)) (op-refuse "not-driver" {:holder (:who record)})
      (= :queued (:status record)) (do (drop-queued! eng id "cancelled") {:ok true :operation (op-view (world-record eng id))})
      (not= :running (:status record)) {:ok true :operation (op-view record)}
      :else (do (cancel-active! eng "cancelled")
                (start-next-world-op! eng)
                {:ok true :operation (op-view (world-record eng id))}))))

;; ------------------------------------------------------------------ move-to through go-to's walker

(defn cell-pos [pos] ((:manual/cell-of hooks/all) {:x (:x pos) :y (:y pos) :z (:z pos)}))

(defn walker-result
  "The result of a walk round in moveTo's shape: {:status :pos :distance} and a :reason when it did not arrive."
  [eng target {:keys [status result]}]
  (let [here (js->clj (.-pos (.self (:primitives eng))) :keywordize-keys true)
        reason (or (:reason result) (some-> (:status result) name))]
    (cond-> {:status status :pos here :distance (core/distance here target)}
      (and (not= "arrived" status) reason) (assoc :reason (str (name reason)
                                                              (when-let [cells (seq (:cells result))] (str " at " (pr-str cells))))))))

(defn walker-applies?
  "Whether a move-to goes through the walker: the body can sense the world for planning and the target is within maxDistance
  (a farther one is a hop, which only the primitive makes)."
  [eng args]
  (let [p (:primitives eng)
        here (js->clj (.-pos (.self p)) :keywordize-keys true)]
    (and ((:manual/plannable? hooks/all) p)
         (<= (core/distance here (:pos args)) (or (:maxDistance args) 64)))))

(def dropped-walk
  "A manual move-to whose connection dropped: neither arrived nor blocked, the body reconnects by itself."
  {:status "offline" :reason "the body lost its connection; it reconnects by itself"})

(defn ^:async walk-move-to!
  "move-to as go-to walks: the :manual/walk-round! hook (engine.hooks) with doors, gates and trapdoors opened and shut again, bounded by
  timeout-s (past it the lease's token is rotated, which cuts the walk). Resolves to a clj map in moveTo's result shape."
  [eng token {:keys [pos range]} timeout-s]
  (let [c (core/make-ctx eng {:root "manual-move-to" :slots [] :chain ["manual-move-to"] :token token :args {} :round 0 :reflex nil})
        cell (cell-pos pos)
        timed-out (atom false)
        timer (js/setTimeout (fn [] (reset! timed-out true) (rotate-token! eng)) (* 1000 timeout-s))]
    (try
      (let [round (await ((:manual/walk-round! hooks/all) c cell (or range 1) {:doors :shut :timeout-s timeout-s}))]
        (if (core/offline? eng) dropped-walk (walker-result eng cell round)))
      (catch :default e
        (cond
          (core/offline? eng) dropped-walk
          (not @timed-out) (throw e)
          :else (assoc (walker-result eng cell {:status "partial"}) :reason "timeout")))
      (finally (js/clearTimeout timer)))))

(defn timeout-s-of
  "The lease seconds world action action with args needs."
  [eng action args]
  (cond
    (= action :move-to) (or (:timeoutS args) 10)
    (and (= action :dig) (map? args) (valid-pos? (:pos args))) ((:manual/dig-timeout-s hooks/all) (:primitives eng) args)
    :else (:timeout (world-actions action) 10)))

(defn finish-world-op!
  "Book the end of the active world action (when attempt is still its current attempt) and start the next queued one."
  [eng request-id attempt status fields event]
  (when (current-attempt? eng request-id attempt)
    (set-world-record! eng request-id (merge (world-record eng request-id) {:status status :finished-at (core/now eng)} fields))
    (swap! (:world-ops eng) assoc :active nil)
    (action-event! eng request-id :done event)
    (start-next-world-op! eng)))

(defn start-world-op!
  "Run world action record now under the lease's current token; it becomes the active one."
  [eng {:keys [request-id action args] :as record}]
  (let [current @(:manual eng)
        token (:token current)
        now (core/now eng)
        call-args (if (= action :move-to) (merge {:timeoutS (timeout-s-of eng action args)} args) args)
        attempt (.randomUUID crypto)
        record (assoc record :status :running :attempt-id attempt :owner-token token :started-at now)]
    ;; A prior raw drive can leave controls held. Clear them before the action so lease ticks cannot
    ;; issue drive/stop calls that interleave with its pathfinder or interaction.
    (.stopDriving (:primitives eng))
    (swap! (:manual eng) assoc :last-beat now :deadman? false :controls lease/all-false :deadlines {})
    (swap! (:world-ops eng) assoc :active request-id)
    (set-world-record! eng request-id record)
    (action-event! eng request-id :started {:name (name action) :args call-args})
    (let [method (:method (world-actions action))
          promise (try
                    (js/Promise.resolve
                      (cond
                        (and (= action :move-to) (walker-applies? eng args))
                        (.then (walk-move-to! eng token args (:timeoutS call-args)) clj->js)
                        (= action :dig) ((:manual/dig! hooks/all) (:primitives eng) token call-args)
                        (= action :wear) ((:manual/wear! hooks/all) (:primitives eng) token call-args)
                        :else
                        (.call (aget (:primitives eng) method) (:primitives eng) token (clj->js call-args))))
                    (catch :default e (js/Promise.reject e)))]
      (.then promise
             (fn [result]
               (let [value (js->clj result :keywordize-keys true)
                     summary (compact-result value)]
                 (finish-world-op! eng request-id attempt :done {:result summary}
                                   {:name (name action) :status (:status value) :result summary})))
             (fn [error]
               (let [message (subs (str (.-message error)) 0 (min 200 (count (str (.-message error)))))]
                 (finish-world-op! eng request-id attempt :failed {:reason message}
                                   {:name (name action) :status :failed :error message}))))
      record)))

(defn start-next-world-op!
  "Start the first queued world action, if none is active and the lease is still its submitter's."
  [eng]
  (when-not (:active @(:world-ops eng))
    (when-let [id (first (:queue @(:world-ops eng)))]
      (swap! (:world-ops eng) update :queue #(vec (rest %)))
      (let [record (world-record eng id)]
        (cond
          (not= :queued (:status record)) (start-next-world-op! eng)
          (not (own-lease? eng (:who record))) (do (drop-queued! eng id "lease-ended") (start-next-world-op! eng))
          :else (start-world-op! eng record))))))

(defn submit-world-op!
  "Run a world action now, or queue it behind the running one (in order, at most max-queued-world-ops waiting)."
  [eng {:keys [who request-id action args]}]
  (let [prior (world-record eng request-id)
        current @(:manual eng)
        idle-ms (:idle-ms current)
        timeout-s (timeout-s-of eng action args)
        active (:active @(:world-ops eng))
        queue (:queue @(:world-ops eng))]
    (cond
      (not (valid-label? who)) (op-refuse "bad-who")
      (not (valid-label? request-id)) (op-refuse "bad-request-id")
      prior (if (and (= who (:who prior)) (= action (:action prior)) (= args (:args prior)))
              {:ok true :operation (op-view prior) :duplicate true}
              (op-refuse "request-id-conflict"))
      (core/offline? eng) (op-refuse "offline")
      (not (own-lease? eng who)) (not-driver-refusal eng)
      (not (contains? world-actions action)) (op-refuse "unknown-action")
      (action-args-error action args) (op-refuse "bad-args" (action-args-error action args))
      (< idle-ms (+ (* timeout-s 1000) 1000)) (op-refuse "lease-too-short" {:minimum-idleS (inc timeout-s)})
      (and active (>= (count queue) max-queued-world-ops))
      (op-refuse "queue-full" {:running active :queued (count queue) :max max-queued-world-ops})
      :else
      (let [record {:request-id request-id :who who :action action :args args :status :queued
                    :submitted-at (core/now eng)}]
        (if active
          (do (set-world-record! eng request-id record)
              (swap! (:world-ops eng) update :queue (fnil conj []) request-id)
              (action-event! eng request-id :queued {:name (name action) :args args :behind active})
              {:ok true :operation (op-view record) :behind active :position (count (:queue @(:world-ops eng)))})
          {:ok true :operation (op-view (start-world-op! eng record))})))))

(defn world-request [eng method body content-type]
  (let [request (try (reader/read-string (or body "")) (catch :default _ ::invalid))]
    (cond
      (not= method "POST") (world-reply 405 (op-refuse "method-not-allowed"))
      (not (re-matches #"application/edn(?:\s*;.*)?" (str/lower-case content-type)))
      (world-reply 415 (op-refuse "content-type-required" {:expected "application/edn"}))
      (not (map? request)) (world-reply 400 (op-refuse "bad-edn"))
      (seq (remove (case (:op request)
                     :submit #{:op :who :request-id :action :args}
                     :status #{:op :who :request-id}
                     :cancel #{:op :who :request-id}
                     :inventory #{:op :who}
                     #{:op}) (keys request)))
      (world-reply 400 (op-refuse "bad-args" {:detail "unknown request field"}))
      :else
      (let [{:keys [op who request-id]} request
            result (case op
                     :submit (submit-world-op! eng request)
                     :status (if (not (valid-label? who))
                               (op-refuse "bad-who")
                               (if-let [record (world-record eng request-id)]
                               (if (= who (:who record)) {:ok true :operation (op-view record)}
                                   (op-refuse "not-driver" {:holder (:who record)}))
                               (op-refuse "operation-not-found")))
                     :cancel (cancel-world-op! eng who request-id)
                     :inventory (if (own-lease? eng who) {:ok true :body (compact-inventory eng)}
                                    (not-driver-refusal eng))
                     (op-refuse "bad-op" {:allowed [:submit :status :cancel :inventory]}))]
        (world-reply (if (:ok result) 200 (case (:reason result) "unknown-action" 400 "bad-args" 400 "bad-op" 400 409)) result)))))

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
    (drop-queue! eng (str "lease-" reason))
    (cancel-active! eng (str "lease-" reason))
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
  "Route one control-socket request: /entities answers from the seen-entities store, /world passes EDN to the owner-token
  action API, anything else is a /drive request in its JSON wire format."
  [eng opts method path body content-type]
  (cond
    (= path "/entities") (entity-observations/request (:seen-entities eng) method)
    (= path "/world") (world-request eng method body content-type)
    :else
    (let [body-map (js->clj body :keywordize-keys true)]
      (if (and (= path "/drive") (= method "POST") (:active @(:world-ops eng))
               (contains? #{"set" "stop"} (:op body-map)) (own-lease? eng (:who body-map)))
        #js {:status 409 :json #js {:ok false :reason "action-running" :requestId (:active @(:world-ops eng))}}
        (let [now (core/now eng)
              req {:method method :path path :body body-map}
              r (lease/request @(:manual eng) req now (world-of eng) opts)
              answers (mapv #(apply-effect! eng %) (:effects r))
              {:keys [lease reply]} (finish r (first answers) now)]
          (store! eng lease)
          #js {:status (:status reply) :json (clj->js (:json reply))})))))

(defn tick!
  "Time passing for the lease: apply what lease/tick decides (idle, offline, due holds, the dead-man)."
  [eng opts]
  (let [{:keys [lease effects]} (lease/tick @(:manual eng) (core/now eng) (world-of eng) opts)]
    (run! #(apply-effect! eng %) effects)
    (store! eng lease)))

(defn close!
  "End a held takeover for shutdown (call before core/shutdown!)."
  [eng]
  (drop-queue! eng "shutdown")
  (cancel-active! eng "shutdown")
  (run! #(apply-effect! eng %) (lease/close @(:manual eng) (core/now eng)))
  (store! eng nil))
