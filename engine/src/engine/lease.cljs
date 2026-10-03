(ns engine.lease
  "The takeover lease rules, pure. Nothing here touches the body, a clock or a socket: every function takes
  the lease (nil when nobody drives), the request or tick input, now and the world, and returns the new
  lease, the wire reply and effects as data, which engine.takeover applies later.

  Lease: {:who :why :since :idle-ms
          :last-beat   the one clock: the last op from the holder (beat-ops, ping included); the lease
                       ends after :idle-ms without one, held untimed controls after release-ms
          :controls {kw bool} :deadlines {kw ms} :yaw :pitch
          :deadman?    the dead-man already fired for this silence}

  Effects: [:take who why] [:drive {:controls m :look m}] [:stop-driving]
           [:release who reason held-ms] [:deadman who silent-ms].
  take and set need an answer from the engine before their reply is complete: request marks them
  :pending :take / :drive and taken / driven finish the job.
  Reply json maps use the wire's camelCase keywords, so clj->js gives today's JSON."
  (:require [clojure.string :as str]))

(def control-order [:forward :back :left :right :jump :sneak :sprint])
(def controls (set control-order))
(def max-ms 10000)
(def max-idle-s 3600)
(def default-release-ms 1000)
(def default-idle-ms 15000)

(def beat-ops
  "The ops that keep the lease (each restarts the one heartbeat clock)."
  #{"take" "set" "stop" "ping" "release"})

(def all-false (zipmap control-order (repeat false)))

(defn result
  ([lease reply] (result lease reply []))
  ([lease reply effects] {:lease lease :reply reply :effects effects}))

(defn reply-of [json] {:status 200 :json json})
(defn with-pos
  "Add :pos to a reply map only when known: an offline body has none and the wire omits the key."
  [m pos]
  (cond-> m (some? pos) (assoc :pos pos)))

(defn ok [json] (reply-of (assoc json :ok true)))
(defn refuse
  ([reason] (refuse reason {}))
  ([reason extra] (reply-of (merge {:ok false :reason reason} extra))))
(defn bad-args [text] (refuse "bad-args" {:text text}))

(def not-found {:status 404 :json {:ok false :reason "not-found"}})
(def bad-request {:status 400 :json {:ok false :reason "bad-request"
                                     :text "body must be an object with a known op"}})

(defn view
  "The wire `manual` map; :expiresAt is the idle deadline, :idleLeftS counts down to it (one decimal)."
  [lease now]
  (when lease
    (let [expires (+ (:last-beat lease) (:idle-ms lease))]
      {:who (:who lease)
       :why (:why lease)
       :since (:since lease)
       :controls (:controls lease)
       :yaw (:yaw lease)
       :pitch (:pitch lease)
       :idleMs (:idle-ms lease)
       :expiresAt expires
       :idleLeftS (/ (Math/round (/ (max 0 (- expires now)) 100)) 10)})))

(defn finite? [v] (and (number? v) (== v v) (not= v ##Inf) (not= v ##-Inf)))

(defn controls-error [held]
  (if-not (map? held)
    "controls must be an object"
    (when-let [[k _] (first (remove (fn [[k v]] (and (contains? controls k) (boolean? v))) held))]
      (str "bad control " (name k) ": names are " (str/join "/" (map name control-order)) " with boolean values"))))

(defn look-error [look]
  (let [msg "look must be {yaw, pitch} or {dyaw, dpitch}"]
    (cond
      (not (map? look)) msg
      (or (empty? look)
          (seq (remove (fn [[k v]] (and (contains? #{:yaw :pitch :dyaw :dpitch} k) (finite? v))) look)))
      (str msg " with numbers"))))

(defn ms-error [ms]
  (when-not (and (integer? ms) (<= 1 ms max-ms))
    (str "ms must be an integer 1.." max-ms)))

(defn validate-set [req]
  (or (when (contains? req :controls) (controls-error (:controls req)))
      (when (contains? req :look) (look-error (:look req)))
      (when (contains? req :ms) (ms-error (:ms req)))))

(defn touch
  "A driver request landed: the heartbeat clock restarts when the op is in beat-ops."
  [lease op now opts]
  (cond-> lease
    (contains? (:beat-ops opts beat-ops) op) (assoc :last-beat now :deadman? false)))

(defn ending [lease reason now]
  [[:release (:who lease) reason (- now (:since lease))]])

(defn valid-idle-s? [v] (and (number? v) (<= 1 v max-idle-s)))

(defn take-op [lease req now world opts]
  (let [{:keys [who why idleS]} req]
    (cond
      (not (and (string? who) (not= "" who))) (result lease (bad-args "who is required"))
      (and (contains? req :idleS) (not (valid-idle-s? idleS)))
      (result lease (bad-args (str "idleS must be a number 1.." max-idle-s)))

      (and lease (= who (:who lease)))
      (let [l (touch (cond-> lease (contains? req :idleS) (assoc :idle-ms (* 1000 idleS))) "take" now opts)]
        (result l (ok {:manual (view l now)})))

      lease (result lease (refuse (str "held-by " (:who lease))))
      (:offline world) (result lease (refuse "offline"))
      (:settling world) (result lease (refuse "settling"))

      :else
      (let [why (if (string? why) why "")
            l {:who who :why why :since now :controls all-false :deadlines {} :yaw nil :pitch nil
               :last-beat now :deadman? false
               :idle-ms (if (contains? req :idleS) (* 1000 idleS) (:idle-ms opts default-idle-ms))}]
        (assoc (result l (ok {:manual (view l now)}) [[:take who why]]) :pending :take)))))

(defn with-driver [f]
  (fn [lease req now world opts]
    (cond
      (nil? lease) (result lease (refuse "not-taken"))
      (not= (:who req) (:who lease)) (result lease (refuse "not-driver" {:holder (:who lease)}))
      :else (f lease req now world opts))))

(defn set-op [lease req now _world opts]
  (let [err (validate-set req)]
    (if err
      (result lease (bad-args err))
      (let [held (:controls req)
            timed (when (contains? req :ms) (for [[c v] held :when v] [c (+ now (:ms req))]))
            l (-> (touch lease "set" now opts)
                  (update :controls merge held)
                  (assoc :deadlines (into (apply dissoc (:deadlines lease) (keys held)) timed)))]
        (assoc (result l (ok {:manual (view l now)}) [[:drive (select-keys req [:controls :look])]])
               :pending :drive)))))

(defn stop-op [lease _req now world opts]
  (let [l (assoc (touch lease "stop" now opts) :controls all-false :deadlines {})]
    (result l (ok (with-pos {:manual (view l now)} (:pos world))) [[:stop-driving]])))

(defn ping-op [lease _req now world opts]
  (let [l (touch lease "ping" now opts)]
    (result l (ok (with-pos {:manual (view l now)} (:pos world))))))

(defn release-op [lease req now _world _opts]
  (let [driver? (and lease (= (:who req) (:who lease)))]
    (cond
      (nil? lease) (result lease (refuse "not-taken"))
      (and (not driver?) (not= true (:force req))) (result lease (refuse "not-driver" {:holder (:who lease)}))
      :else (result nil (ok {:manual nil}) (ending lease (if driver? "released" "forced") now)))))

(def ops
  {"take" take-op
   "set" (with-driver set-op)
   "stop" (with-driver stop-op)
   "ping" (with-driver ping-op)
   "release" release-op})

(defn request
  "Route and apply one HTTP-shaped request {:method :path :body} -> {:lease :reply :effects [:pending]}."
  [lease {:keys [method path body]} now world opts]
  (cond
    (not= path "/drive") (result lease not-found)
    (= method "GET") (result lease (ok (with-pos {:manual (view lease now) :offline (:offline world) :settling (:settling world)}
                                                 (:pos world))))
    (not= method "POST") (result lease not-found)
    (not (and (map? body) (contains? ops (:op body)))) (result lease bad-request)
    :else ((get ops (:op body)) lease body now world opts)))

(defn taken
  "Finish a pending take with the engine's answer {:ok :reason}."
  [lease engine-result now]
  (if (:ok engine-result)
    {:lease lease :reply (ok {:manual (view lease now)})}
    {:lease nil :reply (refuse (:reason engine-result))}))

(defn driven
  "Finish a pending set with the drive result {:pos :yaw :pitch}."
  [lease {:keys [pos yaw pitch]} now]
  (let [l (assoc lease :yaw yaw :pitch pitch)]
    {:lease l :reply (ok (with-pos {:manual (view l now)} pos))}))

(defn expire-holds [lease now release-ms]
  (let [expired (map key (filter (fn [[_ d]] (>= now d)) (:deadlines lease)))
        l (-> lease
              (update :controls into (map (fn [c] [c false]) expired))
              (update :deadlines #(apply dissoc % expired)))
        effects (mapv (fn [c] [:drive {:controls {c false}}]) expired)
        silent (- now (:last-beat l))
        untimed-held? (boolean (some #(and (get (:controls l) %) (not (contains? (:deadlines l) %))) control-order))]
    (cond
      (not untimed-held?) {:lease l :effects effects}
      (< silent release-ms) {:lease l :effects effects}
      (:deadman? l) {:lease l :effects effects}
      :else {:lease (assoc l :controls all-false :deadlines {} :deadman? true)
             :effects (into effects [[:stop-driving] [:deadman (:who l) silent]])})))

(defn tick
  "Time passing: offline and idle (no op from the holder for :idle-ms) end the lease, due timed holds drop,
  the dead-man fires once per silence."
  [lease now world opts]
  (cond
    (nil? lease) {:lease nil :effects []}
    (:offline world) {:lease nil :effects (ending lease "offline" now)}
    (>= (- now (:last-beat lease)) (:idle-ms lease)) {:lease nil :effects (ending lease "idle" now)}
    :else (expire-holds lease now (:release-ms opts default-release-ms))))

(defn close
  "Effects that end a held lease for shutdown."
  [lease now]
  (if lease (ending lease "shutdown" now) []))
