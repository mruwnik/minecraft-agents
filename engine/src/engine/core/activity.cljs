(ns engine.core.activity
  "Doing nothing: the in-flight round's activity record (acts in flight, declared holds), job.holding and
  job.idle, and the away record of a body a job logged out."
  (:require [engine.core.base :refer [emit! job-fields now running state]]
            [engine.expr :as expr]))

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
  :idle-warned? bool}."
  [eng token f & args]
  (swap! (:activity eng) #(if (and (some? token) (= token (:token %))) (apply f % args) %)))

(defn holding
  "Why listed or reflex job id holds the body still on purpose, {:reason :since}, while its round runs: a
  ctx/hold-still! reason, else the :why of the act :wait in flight. Nil otherwise."
  [eng id]
  (let [a (some-> (:activity eng) deref)]
    (when (and a (= id (:id a)) (= (:token a) (:token (running eng))))
      (or (:hold a) (:wait-hold a)))))

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
                                    " s and declared no hold (ctx/hold-still!)")})))))

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
