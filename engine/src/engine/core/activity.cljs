(ns engine.core.activity
  "Doing nothing: the in-flight round's activity record (acts in flight, declared holds), job.holding and
  job.idle, body.idle, and the away record of a body a job logged out."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.core.base :refer [emit! job-fields now running state waiting waiting-text]]
            [engine.core.register :refer [levels]]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(a/defargs settings
  {:engine.core.activity/body-idle-s
   {:default 10 :spec (a/num-in 0.001 nil)
    :doc "Seconds with no useful act (only idle-ok jobs act, the rest wait or hold) and no queue change before body.idle."}})

(defn away
  "Why and until when the body is away, or nil while it is online: {:by :job :why :back-at} when a job logged it out
  (the offline act: :by the root job's name, :job the instance, :why from the act's :why, :back-at epoch ms), and
  {:by :connection :why :connection-lost} when it is offline with no log-out on record (a kick or crash; the body
  reconnects by itself)."
  [eng]
  (when (true? (.isOffline (:primitives eng)))
    (or (some-> (:away eng) deref)
        {:by :connection :why :connection-lost})))

(defn log-out-record
  "The away record of an offline act by instance id of root job root, starting at now."
  [eng root id args now]
  (let [ms (.-ms args)]
    (cond-> {:by (keyword (some-> (get-in (state eng) [:instances root :spec]) expr/label))
             :job id
             :why (keyword (or (.-why args) "away"))}
      (number? ms) (assoc :back-at (+ now (min ms 600000))))))

(defn note-activity!
  "Apply (f activity & args) to the in-flight round's activity record when token is that round's.
  The record: {:token :id :in-flight n :last-at ms :hold {:reason :since} :wait-hold {:reason :since}
  :idle-warned? bool :sections [{:section :level :since}]}."
  [eng token f & args]
  (swap! (:activity eng) #(if (and (some? token) (= token (:token %))) (apply f % args) %)))

(defn holding
  "Why listed or reflex job id holds the body still on purpose, {:reason :since}, while its round runs: a
  ctx/hold-still! reason, else the :why of the act :wait in flight. Nil otherwise."
  [eng id]
  (let [a (some-> (:activity eng) deref)]
    (when (and a (= id (:id a)) (= (:token a) (:token (running eng))))
      (or (:hold a) (:wait-hold a)))))

(defn keep-hold!
  "Before the round of job id starts: the hold the previous round declared stays on record for its job (a job that
  yields :continue while it cooks is still holding), and id's own is dropped."
  [eng id]
  (let [{:keys [hold wait-hold] :as prev} @(:activity eng)
        h (or hold wait-hold)]
    (swap! (:holds eng) #(cond-> (dissoc % id)
                           h (assoc (:id prev) h)))))

(defn emit-holding! [eng root reason t]
  (emit! eng (merge (job-fields eng root)
                    {:source :job :kind :holding :level :info :reason reason :since t
                     :text (str "holding: " (if (keyword? reason) (name reason) reason))})))

(defn hold-still!
  "The round of token (of job root) holds the body still on purpose, for reason, until its round ends; nil clears it.
  A new reason emits job.holding. Clearing restarts the idle clock."
  [eng token root reason]
  (let [t (now eng)
        before (:hold @(:activity eng))]
    (if (nil? reason)
      (note-activity! eng token #(-> (dissoc % :hold) (assoc :last-at t)))
      (when (not= reason (:reason before))
        (note-activity! eng token assoc :hold {:reason reason :since t})
        (emit-holding! eng root reason t)))))

(defn sections
  "The sections open in the round of token ({:section :level :since}, outermost first), or nil."
  [eng token]
  (let [a (some-> (:activity eng) deref)]
    (when (and a (some? token) (= token (:token a)))
      (seq (:sections a)))))

(defn section-level
  "The highest level of the sections open in the round of token, or nil."
  [eng token]
  (some->> (sections eng token) (map :level) (apply max-key levels)))

(defn open-section!
  "Open section (a keyword) at level in the round of token. Not opened (false) when the round opens its outermost
  section while a reflex waits (waiting?): the reflex then fires in the gap."
  [eng token section level waiting?]
  (if (or (not= token (:token @(:activity eng))) (and waiting? (empty? (sections eng token))))
    false
    (do (note-activity! eng token update :sections (fnil conj []) {:section section :level level :since (now eng)})
        true)))

(defn close-section!
  "Close the innermost section of the round of token."
  [eng token]
  (note-activity! eng token update :sections #(if (seq %) (pop %) %)))

(defn check-idle!
  "Warn job.idle once per spell when the running round has no act in flight, declared no hold, and its last act
  (or its start) was more than :idle-s ago. An act ends the spell."
  [eng]
  (let [a @(:activity eng)
        t (now eng)
        idle-ms (when a (- t (:last-at a)))]
    (when (and a (= (:token a) (:token (running eng)))
               (zero? (:in-flight a)) (nil? (:hold a)) (not (:idle-warned? a))
               (> idle-ms (* 1000 (:idle-s eng))))
      (swap! (:activity eng) assoc :idle-warned? true)
      (emit! eng (merge (job-fields eng (:id a))
                        {:source :job :kind :idle :level :warn :idle-ms idle-ms
                         :text (str "the round holds the body with no act for " (js/Math.round (/ idle-ms 1000))
                                    " s and declared no hold (ctx/hold-still!)")}))
      nil)))

(defn idle-ok-node?
  "Whether every job of spec node declares (def idle-ok true): content to sit doing nothing (waits)."
  [jobs {:keys [op job child children]}]
  (case op
    :leaf (true? (:idle-ok (get jobs job)))
    :repeat (idle-ok-node? jobs child)
    (every? #(idle-ok-node? jobs %) children)))

(defn idle-ok? [eng id]
  (some->> (get-in (state eng) [:instances id :spec]) (idle-ok-node? (:jobs eng)) true?))

(def body-idle-policy {:cap 1 :ttl :forever})

(defn useful-act?
  "Whether act k of root's round is useful work: any act but :wait, of a job that is not idle-ok."
  [eng root k]
  (and (not= :wait k) (not (idle-ok? eng root))))

(defn body-act!
  "A useful act started (delta 1) or ended (-1): the body is busy meanwhile, and the body.idle clock restarts."
  [eng delta]
  (some-> (:body-idle eng) (swap! #(-> (update % :busy + delta) (assoc :last-at (now eng) :told? false)))))

(defn idle-job
  "What listed or reflex job id does while the body idles: {:job :name} and :waiting, :holding, :failed or :idle-ok."
  [eng id]
  (let [waiting (waiting eng id)
        hold (or (holding eng id) (get @(:holds eng) id))
        failed (get-in (state eng) [:failed id])]
    (cond-> {:job id :name (some-> (get-in (state eng) [:instances id :spec]) expr/label)}
      waiting (assoc :waiting waiting)
      (and hold (not waiting)) (assoc :holding hold)
      failed (assoc :failed (:error failed))
      (idle-ok? eng id) (assoc :idle-ok true))))

(defn idle-job-text [{:keys [job name waiting holding failed idle-ok]}]
  (str job " " name " "
       (cond waiting (waiting-text waiting)
             holding (str "holding: " (pr-str (:reason holding)))
             failed "failed"
             idle-ok "idle-ok"
             :else "no act")))

(defn body-idle-s [eng]
  (or (:body-idle-s eng) (settings/get settings :engine.core.activity/body-idle-s)))

(defn restart-body-idle!
  "Restart the body.idle clock: time paused, offline or driven by hand is not the jobs' idle time."
  [eng]
  (some-> (:body-idle eng) (swap! assoc :last-at (now eng) :told? false)))

(defn tell-body-idle! [eng idle-ms]
  (let [h (running eng)
        ids (cond-> (:list (state eng)) (:reflex h) (conj (:id h)))
        jobs (mapv #(idle-job eng %) ids)]
    (mem/write! (:store eng) :body-idle {:jobs jobs :idle-ms idle-ms} body-idle-policy)
    (emit! eng {:source :body :kind :idle :level :info :jobs jobs :idle-ms idle-ms
                :text (str "body idle " (js/Math.round (/ idle-ms 1000)) " s with no useful act: "
                           (if (seq jobs) (str/join "; " (map idle-job-text jobs)) "no jobs"))})))

(defn check-body-idle!
  "Emit body.idle once per stretch of :body-idle-s with no useful act (useful-act?) and no change of the list: the
  agent hears that the body only fills time, and which jobs wait and why. A useful act or a list change re-arms it."
  [eng]
  (when-let [clock (:body-idle eng)]
    (let [t (now eng)
          listed (:list (state eng))
          {:keys [busy last-at told?] :as b} @clock]
      (cond
        (not= listed (:list b)) (swap! clock assoc :list listed :last-at t :told? false)
        (pos? busy) (swap! clock assoc :last-at t)
        (and (not told?) (>= (- t last-at) (* 1000 (body-idle-s eng))))
        (do (swap! clock assoc :told? true)
            (tell-body-idle! eng (- t last-at)))))))

(defn act-started!
  "Book the start of act k of the round of token: one more act in flight; a :wait with :why holds the body on
  purpose while it lasts (job.holding when the reason is new for this round)."
  [eng token root k args]
  (let [why (when (= :wait k) (some-> args .-why))
        t (now eng)
        new-why? (and (some? why) (not= why (:last-why @(:activity eng))))]
    (note-activity! eng token #(cond-> (update % :in-flight inc)
                                 why (assoc :wait-hold {:reason why :since t} :last-why why)))
    (when (and new-why? (= token (:token @(:activity eng))))
      (emit-holding! eng root why t))))

(defn act-ended! [eng token]
  (note-activity! eng token #(-> (update % :in-flight dec)
                                 (assoc :last-at (now eng))
                                 (dissoc :idle-warned? :wait-hold))))
