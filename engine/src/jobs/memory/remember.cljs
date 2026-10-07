(ns jobs.memory.remember
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.places :as places]))

(def doc
  "Write one entry of a kind of your own into body memory, in one round. A trigger reads it with (since :kind).
  Example: (seq (jobs.animals.breed {...}) (jobs.memory.remember {:kind :bred-cows})) followed by a trigger whose
  :when is (or (not (known? (since :bred-cows))) (> (since :bred-cows) 1200)) breeds again 20 minutes after the
  last time, and when it was never done.
  :kind is an unnamespaced keyword that nothing else writes. :data is an optional map kept in the entry.
  Without :ttl-s and :cap the kind gets the engine's default policy (cap 50, 1 hour; a kind that already has a
  policy keeps it). After the hour the entry is gone and (since :kind) is unknown again. A job that wants \"every
  3 days\" passes :ttl-s 259200. :ttl-s is in seconds and must be positive, or :forever. :cap is the number of newest entries
  kept, a positive whole number.
  Refused (warn memory.refused with :reason and :text, result {:ok false :reason}, and the job still ends):
  :bad-kind (not a keyword, or namespaced), :reserved-kind (a kind the engine or another job writes, such as
  :hurt, :slept, :scaffold, or the name of a recorded place), :bad-ttl, :bad-cap, :bad-data (not a map).
  Success: info memory.remembered with :memory-kind, :entry and :policy. Result {:ok true :kind}.")

(a/defargs args
  {:kind {:doc "the memory kind to write: an unnamespaced keyword no engine job writes, e.g. :bred-cows" :spec keyword? :default nil}
   :data {:doc "a map kept in the entry, for jobs that read it back" :spec map? :default nil}
   :ttl-s {:doc "seconds the entry is kept (or :forever); nil: the kind's own policy, or the default hour" :spec (a/or-of #{:forever} a/pos-num?) :default nil}
   :cap {:doc "entries of the kind kept, the newest; nil: the kind's own policy, or the default 50" :spec (a/int-in 1 nil) :default nil}})

(defn check [_c] true)

(defn refusal [reason text] {:reason reason :text text})

(defn resolve-args
  "What remember does for args {:kind :data :ttl-s :cap} in a memory view: {:kind kw :data map :policy policy-or-nil},
  or a refusal {:reason :text}."
  [view {:keys [kind data ttl-s cap]}]
  (cond
    (not (and (keyword? kind) (nil? (namespace kind))))
    (refusal :bad-kind (str ":kind is an unnamespaced keyword such as :bred-cows; got " (pr-str kind)))

    (or (contains? places/owned-kinds kind) (places/place-entry? view kind))
    (refusal :reserved-kind (str (name kind) " is a memory kind the engine or a job writes for something else; choose another"))

    (not (or (nil? data) (map? data)))
    (refusal :bad-data (str ":data is a map; got " (pr-str data)))

    (not (or (nil? ttl-s) (= :forever ttl-s) (and (number? ttl-s) (js/isFinite ttl-s) (pos? ttl-s))))
    (refusal :bad-ttl (str ":ttl-s is a positive number of seconds, or :forever; got " (pr-str ttl-s)))

    (not (or (nil? cap) (and (int? cap) (pos? cap))))
    (refusal :bad-cap (str ":cap is a positive whole number; got " (pr-str cap)))

    :else
    {:kind kind :data (or data {})
     :policy (when (or ttl-s cap)
               {:cap (or cap (get-in view [:data :policies kind :cap] (:cap mem/default-policy)))
                :ttl (if ttl-s
                       (if (= :forever ttl-s) :forever (* 1000 ttl-s))
                       (get-in view [:data :policies kind :ttl] (:ttl mem/default-policy)))})}))

(defn round [c]
  (let [r (resolve-args (ctx/view c) (:args c))]
    (if (:reason r)
      (do (ctx/emit! c :memory.refused :warn r)
          (ctx/result! c {:ok false :reason (:reason r)}))
      (let [{:keys [kind data policy]} r]
        (ctx/remember! c kind data policy)
        (ctx/emit! c :memory.remembered :info {:memory-kind kind :entry data :policy policy
                                               :text (str (name kind) " remembered")})
        (ctx/result! c {:ok true :kind kind})))
    :done))
