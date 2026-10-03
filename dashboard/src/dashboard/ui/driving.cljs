(ns dashboard.ui.driving
  "Driving a body from the dashboard, as a re-frame slice at [:drive name]:
  {:driving? :manual :gen :held #{control} :look {:dyaw :dpitch} :error :since}.
  Every transition is a pure handler (cofx, event) -> fx map; the effects are data
  (:drive/post, :drive/poll-get, :drive/ping-timer, :drive/look-timer, :drive/poll), run by dashboard.ui.driving-fx.

  Rules: control is kept until this page explicitly releases or the tab closes, pings keep it. Leaving the page's
  focus (pointer lock lost, blur, hidden) only stops the controls; pagehide stops with keepalive; a closed popup
  releases. A failed request, or any reply showing this page no longer holds the body, clears every marker at once.
  A reply to a request that started before the latest take (older gen) is ignored."
  (:require [re-frame.core :as rf]
            [dashboard.ui.trouble :as trouble]
            [drive.keys :as keys]))

(def defaults {:driving? false :gen 0 :held #{}})

(defn slice [db name] (merge defaults (get-in db [:drive name])))
(defn put [db name s] (assoc-in db [:drive name] s))

(defn timers [name on?]
  {:drive/ping-timer {:name name :on? on?} :drive/look-timer {:name name :on? on?}})

(defn post
  "A :drive/post effect value from this slice's gen."
  ([name s who body] (post name s who body false))
  ([name s who body keepalive?]
   (cond-> {:name name :gen (:gen s) :body (assoc body :who who)}
     keepalive? (assoc :keepalive? true))))

(defn lose
  "Everything that says we drive, cleared in one step."
  [db name s error]
  (merge {:db (put db name (assoc s :driving? false :held #{} :look nil :error error))} (timers name false)))

(defn no-body? [reply] (or (nil? reply) (= "no-body" (:reason reply))))

(defn lost-text [name reply]
  (if (no-body? reply) (str "no running body " name) "control lost"))

(defn take-result
  [db name s reply]
  (cond
    (nil? reply) {:db (put db name (assoc s :error (str "no running body " name)))}
    (not (:ok reply)) {:db (put db name (assoc s :error (str "cannot take over: " (:reason reply))))}
    :else (merge {:db (put db name (assoc s :driving? true :gen (inc (:gen s)) :manual (:manual reply) :since (get-in reply [:manual :since])
                                          :held #{} :look nil :error nil))}
                 (timers name true))))

(defn reply-core
  "A POST reply (or a failed request: reply nil) or a poll reply for a request that started in `gen`."
  [db name gen op reply]
  (let [s (slice db name)
        me (:who db)]
    (cond
      (keys/stale? {:started-gen gen :current-gen (:gen s)}) {}
      (= "take" op) (take-result db name s reply)
      (keys/should-drop? {:driving? (:driving? s) :reply reply :me me :started-gen gen :current-gen (:gen s)})
      (lose db name (cond-> s (some? reply) (assoc :manual (:manual reply))) (lost-text name reply))
      (nil? reply) {:db db}
      (or (= "poll" op) (contains? reply :manual)) {:db (put db name (assoc s :manual (:manual reply)))}
      :else {:db db})))

(defn reply-fx [{:keys [db]} [_ name gen op reply]] (reply-core db name gen op reply))
(defn poll-reply-fx [{:keys [db]} [_ name gen reply]] (reply-core db name gen "poll" reply))
(defn request-failed-fx [{:keys [db]} [_ name gen op]] (reply-core db name gen op nil))

(defn take-fx [{:keys [db]} [_ name]]
  (let [s (slice db name)]
    (if (:driving? s)
      {}
      {:db (put db name (assoc s :error nil))
       :drive/post (post name s (:who db) {:op "take" :why "dashboard"})})))

(defn release-fx [{:keys [db]} [_ name]]
  (let [s (slice db name)]
    (if-not (:driving? s)
      {}
      (assoc (lose db name s nil) :drive/post (post name s (:who db) {:op "release"})))))

(defn key-down-fx [{:keys [db]} [_ name code repeat?]]
  (let [s (slice db name)
        control (keys/control-for code)
        step (keys/look-step-for code)
        who (:who db)]
    (cond
      (not (:driving? s)) {}
      (and control (or repeat? (contains? (:held s) control))) {}
      control {:db (put db name (update s :held conj control))
               :drive/post (post name s who {:op "set" :controls {control true}})}
      step {:drive/post (post name s who {:op "set" :look step})}
      :else {})))

(defn key-up-fx [{:keys [db]} [_ name code]]
  (let [s (slice db name)
        control (keys/control-for code)]
    (if-not (and (:driving? s) control (contains? (:held s) control))
      {}
      {:db (put db name (update s :held disj control))
       :drive/post (post name s (:who db) {:op "set" :controls {control false}})})))

(defn mouse-move-fx [{:keys [db]} [_ name movement-x movement-y]]
  (let [s (slice db name)]
    (if-not (:driving? s)
      {}
      {:db (put db name (update s :look keys/merge-look (keys/mouse-look movement-x movement-y)))})))

(defn look-flush-fx [{:keys [db]} [_ name]]
  (let [s (slice db name)]
    (if-not (and (:driving? s) (:look s))
      {}
      {:db (put db name (assoc s :look nil))
       :drive/post (post name s (:who db) {:op "set" :look (:look s)})})))

(defn stop-fx
  "Controls zeroed and the pending look dropped, the lease kept."
  [keepalive? {:keys [db]} [_ name]]
  (let [s (slice db name)]
    (if-not (:driving? s)
      {}
      {:db (put db name (assoc s :held #{} :look nil))
       :drive/post (post name s (:who db) {:op "stop"} keepalive?)})))

(def pointer-lock-lost-fx (partial stop-fx false))
(def blur-fx (partial stop-fx false))
(def hidden-fx (partial stop-fx false))
(def pagehide-fx (partial stop-fx true))

(defn popup-closed-fx [cofx [_ name :as event]]
  (assoc (release-fx cofx event) :drive/poll {:name name :on? false}))

(defn popup-opened-fx [{:keys [db]} [_ name]]
  {:drive/poll {:name name :on? true}
   :drive/poll-get {:name name :gen (:gen (slice db name))}})

(defn ping-tick-fx [{:keys [db]} [_ name]]
  (let [s (slice db name)]
    (if-not (:driving? s) {} {:drive/post (post name s (:who db) {:op "ping"})})))

(defn poll-tick-fx [{:keys [db]} [_ name]]
  {:drive/poll-get {:name name :gen (:gen (slice db name))}})

(doseq [[k f] {::take take-fx ::release release-fx ::key-down key-down-fx ::key-up key-up-fx ::mouse-move mouse-move-fx
               ::pointer-lock-lost pointer-lock-lost-fx ::blur blur-fx ::hidden hidden-fx ::pagehide pagehide-fx
               ::popup-closed popup-closed-fx ::popup-opened popup-opened-fx ::reply reply-fx ::poll-reply poll-reply-fx
               ::request-failed request-failed-fx ::ping-tick ping-tick-fx ::look-flush look-flush-fx ::poll-tick poll-tick-fx}]
  (rf/reg-event-fx k f))

;; ---- view ----

(defn held-by-me? [s] (boolean (:driving? s)))

(defn banner
  "\"you are driving\", \"driven by <who> since HH:MM\" (neutral), or nil when nobody drives."
  [s me]
  (let [{:keys [who since]} (:manual s)]
    (cond
      (:driving? s) "you are driving"
      (or (nil? who) (= who me)) nil
      :else (str "driven by " who (when-let [t (trouble/since-text since)] (str " since " t))))))

(rf/reg-sub ::held-by-me? (fn [db [_ name]] (held-by-me? (slice db name))))
(rf/reg-sub ::banner (fn [db [_ name]] (banner (slice db name) (:who db))))
(rf/reg-sub ::error (fn [db [_ name]] (:error (slice db name))))
