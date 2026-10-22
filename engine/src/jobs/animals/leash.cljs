(ns jobs.animals.leash
  (:require [jobs.lib.checks :as checks]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Put a lead on one animal of type :mob (such as \"cow\") within :radius and end. A one-shot order that starts and
  ends itself; a cut job resumes.

  One call is the whole run. It takes the nearest animal that is not on anyone's lead, not given up on and not in :skip (uuids or
  ids). The body walks to within 3 blocks (doors :shut) and uses the lead
  it carries (fetching one first when none is carried and an animal waits, jobs.lib.need, unless the ways are only #{:carried}). The use counts only when the sensing then shows the animal on this body's lead. Animals are
  tracked by uuid, by id when it has none.

  An animal is given up on when its walk is blocked or two attempts were out of reach (:unreachable), nothing
  happened (:no-effect), it vanished (:gone), the server said it cannot (:cannot), the use failed (:failed) or
  the lead did not show afterwards (:unconfirmed).

  The data {:reason :animal key
  :id entity-id :given-up {key reason}} and the ending by the reason:
  - :done :leashed: an animal is on the lead (:already when one within :radius, not in :skip, was on it before the call).
  - :condition-failed :no-lead (no lead carried and none fetched; a listed call with :ways #{:carried} waits for one; or the server found none; after a failed fetch the job ends with the fetch's own ending, reason :need, its cause naming the lead),
    :refused (the zone rules refused the animals) or :no-zones (no zone list was read).
  - :nothing-possible when no candidate is left: :unreachable if one was given up as unreachable, else :all-leashed
    (animals present but all led) or :none. The same after three fruitless animals in a row.

  Zones: an animal standing in another owner's zone or claim, or in a plan's footprint, is not led (taking it out of the zone) (warn
  leash.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :radius {:doc "animals within this many blocks count" :spec (a/num-in 0 nil) :default 8}
   :center {:doc "where :radius is measured from; the body's own spot when nil" :spec ::a/pos :default nil}
   :skip {:doc "keys (uuids, else ids) of animals never to leash" :spec (a/coll-of (a/or-of string? number?)) :default []}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds of working, fetching included, before leashing gives up." :spec (a/num-in 0 nil)}
   ::max-in-row {:default 3 :doc "Fruitless animals in a row that end the run." :spec (a/int-in 0 nil)}})
(defn max-in-row [] (settings/get settings ::max-in-row))

(def ways need/fetch-ways)

(def target [:mob])

(def safe? (ready/safe-at-or-roaming
            (fn [c] (work-place/nearest-mob c (remove nil? [(:mob (:args c))]) (or (:radius (:args c)) 8)))))
(def fit? "Survival mode only: a passive animal cannot hurt the body, so a weak body still brings one to food." checks/survival-mode)


(def defaults {:spend {:job-time ::job-time-s}})

(defn can-start?
  "Later :no-lead: no lead carried and ways #{:carried}."
  [c]
  (if (or (need/fetch-opts c) (animals/lead-carried? c))
    (contract/ok :lead "a lead carried or fetched")
    (contract/later :no-lead "no lead carried and the ways are only #{:carried}")))

(defn quote-of [c]
  {:uses {:job-time [0 10 ##Inf]}})

(defn skipped
  "The keys of the animals never to leash: :skip and those given up on."
  [c]
  (into (set (:skip (:args c))) (keys (:given-up (ctx/mem c)))))

(declare herd)

(defn done?
  "Ok when an animal in reach is on this body's lead (the one the run led, else one led before the call, not skipped);
  else later with the animals led against the one asked."
  [c]
  (let [led (if-let [done (:leashed (ctx/mem c))]
              done
              (when-let [e (first (filterv #(and (animals/led-by-me? %) (not (contains? (skipped c) (animals/key-of %)))) (herd c)))]
                {:animal (animals/key-of e) :id (.-id e)}))]
    (if led
      (contract/ok :leashed "an animal is on the lead" :units :animals :done 1 :left 0
                   :result {:reason :leashed :animal (:animal led) :id (:id led) :given-up (:given-up (ctx/mem c) {})})
      (contract/later :leashing "0 animals led" :units :animals :done 0 :left 1))))

(def ending-of
  "The kind and cause of a finish by reason."
  {:leashed [:done nil]
   :no-lead [:condition-failed :no-lead]
   :refused [:condition-failed :refused]
   :no-zones [:condition-failed :no-zones]})

(defn finish!
  "Emit the outcome and end the call: the ending of reason (ending-of, else nothing-possible), or ending when given
  (a failed fetch's)."
  [c reason animal & [ending]]
  (let [result {:reason reason
                :animal (some-> animal animals/key-of)
                :id (some-> animal .-id)
                :given-up (:given-up (ctx/mem c) {})
                :text (str "leash done: " (name reason))}]
    (when (= :leashed reason) (ctx/update-mem! c assoc :leashed {:animal (:animal result) :id (:id result)}))
    (if ending
      (assoc ending :data result)
      (let [[kind cause] (get ending-of reason [:nothing-possible reason])]
        (ctx/end c kind cause result)))))

(defn herd [c]
  (let [{:keys [mob radius center]} (:args c)
        p (:primitives c)]
    (if-not center
      (animals/herd p mob radius)
      (let [around (fn [e] (u/flat-dist (u/pos-of (.-pos e)) center))]
        (->> (animals/herd p mob (+ radius (u/flat-dist (u/self-pos c) center)))
             (filterv #(<= (around %) radius))
             (sort-by #(around %))
             vec)))))

(defn candidates
  "The animals within radius not on a lead, not given up on and not in :skip, nearest first."
  [c]
  (let [skip (skipped c)]
    (->> (herd c)
         (filterv #(and (not (animals/leashed? %)) (not (contains? skip (animals/key-of %)))))
         (animals/allowed c :leash.declined "leash" :take))))

(defn none-reason
  "Why the job ends with no animal left to leash."
  [c]
  (let [given-up (remove #{:gone} (vals (:given-up (ctx/mem c))))]
    (cond
      (some #{:unreachable} given-up) :unreachable
      (seq given-up) :failed
      (animals/refusal c) (animals/refusal c)
      (some animals/leashed? (herd c)) :all-leashed
      :else :none)))

(defn on-my-lead
  "The animal as the sensing shows it now when it is on this body's lead, else nil."
  [c k]
  (let [now (animals/find-by-key (:primitives c) (:mob (:args c)) (:radius (:args c)) k)]
    (when (and now (animals/led-by-me? now)) now)))

(defn ^:async lead!
  "Use the lead on the animal once (an items.use child). :continue while it waits, [:leashed now] when the animal
  is on the lead afterwards, [:no-lead nil] when the server found none, else nil with the outcome booked."
  [c animal]
  (let [k (animals/key-of animal)
        r (await (animals/use-on! c k animal "lead"))]
    (if (= :continue r)
      :continue
      (case (:status r)
        :used (if-let [now (on-my-lead c k)]
                [:leashed now]
                (do (animals/give-up! c k :unconfirmed) (animals/bump-row! c) nil))
        :no-item [:no-lead nil]
        (do (animals/book-failed! c k (:status r)) nil)))))

(defn ^:async engage!
  "Lead the animal; end when fruitless :max-in-row times in a row."
  [c animal]
  (let [r (await (lead! c animal))
        [reason led] (when (vector? r) r)]
    (cond
      (= :continue r) :continue
      reason (finish! c reason led)
      (>= (:in-row (ctx/mem c) 0) (max-in-row)) (finish! c (none-reason c) nil)
      :else :again)))

(defn fetchable?
  "Whether a lead is to be fetched now: none carried, an animal waits and the call's ways allow it."
  [c]
  (and (need/fetch-opts c) (not (animals/lead-carried? c)) (seq (candidates c))))

(defn ^:async no-lead!
  "No lead carried: fetch one when the call's ways allow and an animal waits, else end :no-lead. Resolves to :again once it
  arrived, else the round's result."
  [c]
  (if-not (fetchable? c)
    (finish! c :no-lead nil)
    (let [limits (select-keys (need/fetch-opts c) [:how :explore])
          e (await (need/item! c "lead" 1 limits))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else (finish! c :no-lead nil e)))))

(defn ^:async step [c]
  (let [cands (candidates c)]
    (cond
      (and (empty? cands) (not (look/surveyed? c))) (if (seq (await (look/survey-until! c #(seq (candidates c))))) ; animals count only while in view, so test between looks
                                                      (await (step c))
                                                      :continue)
      (not (animals/lead-carried? c)) (await (no-lead! c))
      (empty? cands) (finish! c (none-reason c) nil)
      :else (await (engage! c (animals/pick c cands))))))

(defn ^:async round
  "The whole attempt: steps (a pace between; done? and can-run? are asked between them) until one ends or yields."
  [c]
  (await (pace/steps! c #(step c))))
