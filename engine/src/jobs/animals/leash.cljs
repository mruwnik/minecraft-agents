(ns jobs.animals.leash
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]))

(def doc
  "Put a lead on one animal of type :mob (such as \"cow\") within :radius and end. A one-shot order that starts and
  ends itself. The check always passes, so a cut job resumes.

  One call is the whole run. It takes the nearest animal that is not on anyone's lead, not given up on and not in :skip (uuids or
  ids). The body walks to within 3 blocks (doors :shut, each leg bounded by :walk-timeout-s) and uses the lead
  it carries (fetching one first when none is carried and an animal waits, jobs.lib.fetch, unless :fetch is false). The use counts only when the sensing then shows the animal on this body's lead. Animals are
  tracked by uuid, by id when it has none.

  An animal is given up on when its walk is blocked or two attempts were out of reach (:unreachable), nothing
  happened (:no-effect), it vanished (:gone), the server said it cannot (:cannot), the use failed (:failed) or
  the lead did not show afterwards (:unconfirmed).

  Ends with info leash.done and a warn leash.gave-up unless the reason is :leashed. Result {:reason :animal key
  :id entity-id :given-up {key reason}}. Reasons:
  - :leashed: an animal is on the lead.
  - :no-lead: no lead carried (and none fetched), or the server found none.
  - :timeout: :timeout-s of work, not counting fetching.
  - When no candidate is left: :unreachable if one was given up as unreachable, else :refused (others given
    up, or the zone rules refused the animals; :no-zones when no zone list was read), :all-leashed (animals present but all led) or :none.
  - The same reasons after three fruitless animals in a row.

  Zones: an animal standing in another owner's zone or claim, or in a plan's footprint, is not led (taking it out of the zone) (warn
  leash.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :radius {:doc "animals within this many blocks count" :spec (a/num-in 0 nil) :default 8}
   :skip {:doc "keys (uuids, else ids) of animals never to leash" :spec (a/coll-of (a/or-of string? number?)) :default []}
   :walk-timeout-s {:doc "bound of one walk towards the animal" :spec (a/num-in 0 nil) :default 5}
   :timeout-s {:doc "seconds of working with the animals (not fetching) before the job gives up" :spec (a/num-in 0 nil) :default 30}
   :fetch {:doc "get a lead when none is carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false ends :no-lead" :spec fetch/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false :spec boolean?}})

(def reach 3)
(def max-in-row 3)

(defn check [_c] true)

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason animal]
  (let [result {:reason reason
                :animal (some-> animal animals/key-of)
                :id (some-> animal .-id)
                :given-up (:given-up (ctx/mem c) {})}]
    (ctx/emit! c :leash.done :info (assoc result :text (str "leash done: " (name reason))))
    (when (not= :leashed reason)
      (ctx/emit! c :leash.gave-up :warn {:reason reason :text (str "leashing stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn herd [c]
  (animals/herd (:primitives c) (:mob (:args c)) (:radius (:args c))))

(defn candidates
  "The animals within radius not on a lead, not given up on and not in :skip, nearest first."
  [c]
  (let [skip (into (set (:skip (:args c))) (keys (:given-up (ctx/mem c))))]
    (->> (herd c)
         (filterv #(and (not (animals/leashed? %)) (not (contains? skip (animals/key-of %)))))
         (animals/allowed c :leash.declined "leash" :take))))

(defn none-reason
  "Why the job ends with no animal left to leash."
  [c]
  (let [given-up (vals (:given-up (ctx/mem c)))]
    (cond
      (some #{:unreachable} given-up) :unreachable
      (seq given-up) :refused
      (animals/refusal c) (animals/refusal c)
      (some animals/leashed? (herd c)) :all-leashed
      :else :none)))

(defn on-my-lead
  "The animal as the sensing shows it now when it is on this body's lead, else nil."
  [c k]
  (let [now (animals/find-by-key (:primitives c) (:mob (:args c)) (:radius (:args c)) k)]
    (when (and now (animals/led-by-me? now)) now)))

(defn ^:async lead!
  "Use the lead on the animal once. :leashed when it is on the lead afterwards, :no-lead when the server
  found none, else nil with the outcome booked."
  [c animal]
  (let [k (animals/key-of animal)
        r (await (ctx/act c :interact #js {:id (.-id animal) :item "lead"}))]
    (case (.-status r)
      "used" (if-let [now (on-my-lead c k)]
               [:leashed now]
               (do (animals/give-up! c k :unconfirmed) (animals/bump-row! c) nil))
      "no-item" [:no-lead nil]
      "no-effect" (do (animals/give-up! c k :no-effect) (animals/bump-row! c) nil)
      "gone" (do (animals/give-up! c k :gone) nil)
      "out-of-reach" (do (animals/book-out-of-reach! c k) nil)
      "cannot" (do (animals/give-up! c k :cannot) (animals/bump-row! c) nil)
      "failed" (do (animals/give-up! c k :failed) (animals/bump-row! c) nil)
      nil)))

(defn ^:async engage!
  "Walk to the animal and lead it; end when fruitless three times in a row."
  [c animal]
  (let [walked (await (animals/walk! c (animals/key-of animal) animal reach {:doors :shut}))
        [reason led] (when (= :there walked) (await (lead! c animal)))]
    (cond
      reason (finish! c reason led)
      (>= (:in-row (ctx/mem c) 0) max-in-row) (finish! c (none-reason c) nil)
      :else :again)))

(defn lead-carried? [c]
  (some #(= "lead" (:name %)) (u/inventory (:primitives c))))

(defn problem
  "The need wait for a lead while none is carried and an animal waits, else nil."
  [c]
  (when (and (not (lead-carried? c)) (seq (candidates c)))
    {:reason :need :item "lead"}))

(defn ^:async no-lead!
  "No lead carried: fetch one when :fetch allows and an animal waits, else end :no-lead. Resolves to :again once it
  arrived, else the round's result."
  [c]
  (if-not (and (fetch/opts c 'jobs.animals.leash) (problem c))
    (finish! c :no-lead nil)
    (or (await (fetch/fetch-untimed! c 'jobs.animals.leash problem))
        (if (problem c) (finish! c :no-lead nil) :again))))

(defn ^:async step [c]
  (let [now (ctx/now c)
        {:keys [timeout-s]} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [m (ctx/mem c)
          cands (candidates c)]
      (cond
        (>= (- now (:started m)) (* 1000 timeout-s)) (finish! c :timeout nil)
        (not (lead-carried? c)) (await (no-lead! c))
        (empty? cands) (finish! c (none-reason c) nil)
        :else (await (engage! c (first cands)))))))

(defn ^:async round
  "The whole attempt: loop the steps until one ends or yields."
  [c]
  (loop []
    (let [r (await (step c))]
      (if (= :again r) (recur) r))))
