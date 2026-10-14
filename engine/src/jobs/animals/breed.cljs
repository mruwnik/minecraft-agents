(ns jobs.animals.breed
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Feed :count animals of type :mob (such as \"cow\") within :radius so that they breed. A one-shot order that
  ends itself; a cut job resumes.

  One call is the whole run. It takes the nearest adult not yet fed, refused or given up on. The body walks to within 3 blocks
  (like go-to it opens a shut gate or door on the way and shuts it behind, so a gated pen is entered and its gate
  left shut; a pen with no way in is :unreachable). It then feeds the first breeding
  food of the mob that it carries, fetching one first when none is carried and animals wait (jobs.lib.need, one
  per animal still to feed, unless :fetch is false) (jobs.lib.animals/breeding-food: wheat for cows, sheep, goats and
  mooshrooms; carrot, potato or beetroot for pigs; seeds for chickens; carrot, golden carrot or dandelion for
  rabbits; flowers for bees). Babies are never fed.

  Per animal:
  - Fed: it went into love mode.
  - Refused: nothing happened (on its breeding cooldown, or already in love). It is not tried again.
  - Given up: :unreachable (walk blocked, or two feedings out of reach), :gone, :cannot, :failed, or :baby
    (the food was eaten without love; a baby the sensing missed, warn breed.baby).

  Ends with info breed.done and a warn breed.gave-up unless the reason is :fed. The data {:reason :fed [keys]
  :refused [keys] :given-up {key reason} :food item :adults n :babies n :hand} and the ending by the reason:
  - :done :fed: :count animals were fed. :done :food-out: the food ran out after some were fed (warn breed.gave-up).
  - :condition-failed :unknown-mob (no breeding food is known for :mob), :bees-indoors (bees at night or in rain),
    :no-food (none carried and none fetched, nothing fed yet; a listed call with :fetch false waits for food; after a failed fetch the cause names the food, :because
    the fetch's ending), :too-few (fewer than two adults stand near before the first feeding, or no candidate is left),
    :refused (some refused, none ate) or :no-zones (the zone rules refused the adults).
  - :limit-reached :timeout: the call's :spend :time (default ::time-s) of work, not counting fetching.
  - :nothing-possible :unreachable (one was given up as unreachable) or :unpaired (an odd number fed), when no
    candidate is left. The same after three fruitless animals in a row.

  Animals follow a body holding their food, so the hand is given back. Before the first feeding the job notes
  what the hand holds. When it ends it equips that item again if still carried, else empties the hand. :hand is
  :restored, :emptied or :full (no free slot: the food stays in hand, warn breed.hand-full). There is no :hand
  when nothing was fed. A cut job restores the hand when it resumes and ends. A job cancelled while cut does not.

  A run that fed at least two animals writes one memory entry of kind :bred/<mob> (data {:fed [uuids]}, default
  one hour). A trigger can use it, e.g. (or (not (known? (since :bred/cow))) (> (since :bred/cow) 1200)).

  Zones: an animal standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  breed.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "the mob type to breed, such as \"cow\"" :spec a/name? :default nil}
   :count {:doc "animals to feed" :spec (a/int-in 1 nil) :default 2}
   :radius {:doc "animals within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :walk-timeout-s {:doc "bound of one walk towards an animal" :spec (a/num-in 0 nil) :default 5}
   :fetch {:doc "get breeding food when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-food" :spec fetch/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false :spec boolean?}})

(a/defargs settings
  {::time-s {:default 120 :doc "Seconds of working with the animals (not fetching) before breeding gives up." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::max-in-row {:default 3 :doc "Animals handled in a row before the body looks again." :spec (a/int-in 0 nil)}})
(defn time-s [] (settings/get settings ::time-s))
(defn reach [] (settings/get settings ::reach))
(defn max-in-row [] (settings/get settings ::max-in-row))

(def target [:mob])

(def defaults {:spend {:time ::time-s}})

(defn no-food
  "The cause :no-food before the call starts: a known mob, no breeding food carried and :fetch off."
  [c]
  (let [{:keys [mob fetch]} (:args c)]
    (when (and (contains? animals/breeding-food mob)
               (not (fetch/parse-arg fetch))
               (nil? (animals/food-carried (:primitives c) mob)))
      {:reason :no-food})))

(def conditions [[:fn no-food]])

(defn legacy-args
  "An old caller's :timeout-s as the call's :spend {:time}."
  [{:keys [timeout-s] :as args}]
  (cond-> (dissoc args :timeout-s)
    (and timeout-s (nil? (get-in args [:spend :time]))) (assoc-in [:spend :time] timeout-s)))

(defn time-limit "Seconds the call may work." [c]
  (or (get-in (ctx/frozen-call c) [:spend :time]) (time-s)))

(defn quote-of [c]
  (let [most (time-limit c)]
    {:uses {:time [0 (min most (* 15 (:count (:args c) 2))) most]}}))

(defn done?
  "The animals fed against :count. Never :done? itself: the run ends with its event and data."
  [c]
  (let [fed (count (:fed (ctx/mem c)))]
    {:done? false :units :animals :done fed :left (max 0 (- (:count (:args c) 2) fed))}))

(def ending-of
  "The kind and cause of a finish by reason."
  {:fed [:done nil]
   :unknown-mob [:condition-failed :unknown-mob]
   :bees-indoors [:condition-failed :bees-indoors]
   :no-food [:condition-failed :no-food]
   :food-out [:done nil]
   :too-few [:condition-failed :too-few]
   :refused [:condition-failed :refused]
   :no-zones [:condition-failed :no-zones]
   :timeout [:limit-reached {:line :time :reason :timeout}]})

(defn ^:async restore-hand!
  "Put the hand back as note-hand! found it. Resolves to :restored, :emptied
  or :full (the food stays in hand)."
  [c before]
  (if (some #(= before (:name %)) (u/inventory (:primitives c)))
    (do (await (ctx/act c :equip #js {:item before})) :restored)
    (let [r (await (ctx/act c :unequip #js {}))]
      (if (= "full" (.-status r)) :full :emptied))))

(defn ^:async hand-result
  "{:hand outcome} once the hand was noted, else {}."
  [c]
  (let [m (ctx/mem c)]
    (if-not (:hand-noted m)
      {}
      (let [outcome (await (restore-hand! c (:hand-before m)))]
        (when (= :full outcome)
          (ctx/emit! c :breed.hand-full :warn {:text "no free slot: the food stays in hand"}))
        {:hand outcome}))))

(defn note-hand!
  "Remember what the hand holds, once per run."
  [c]
  (when-not (:hand-noted (ctx/mem c))
    (ctx/update-mem! c assoc :hand-before (.-held (.self (:primitives c))) :hand-noted true)))

(defn mark-bred!
  "Leave one entry of kind :bred/<mob> when the run fed at least two animals, for (since :bred/cow)."
  [c]
  (let [fed (vec (:fed (ctx/mem c)))]
    (when (>= (count fed) 2)
      (ctx/remember! c (keyword "bred" (:mob (:args c))) {:fed fed}))))

(defn ^:async finish!
  "Put the hand back, emit the outcome and end the call: the ending of reason (ending-of, else nothing-possible), or
  ending when given (a failed fetch's)."
  [c reason & [ending]]
  (let [hand (await (hand-result c))
        m (ctx/mem c)
        {:keys [mob radius]} (:args c)
        p (:primitives c)
        result (merge hand {:reason reason
                :fed (vec (:fed m))
                :refused (vec (:refused m))
                :given-up (:given-up m {})
                :food (or (animals/food-carried p mob) (:food m))
                :adults (count (animals/adults p mob radius))
                :babies (count (animals/babies p mob radius))})
        seen (str "adults " (:adults result) ", babies " (:babies result))]
    (mark-bred! c)
    (ctx/emit! c :breed.done :info (assoc result :text (str "breed done: " (name reason) ", fed " (count (:fed m)) ", " seen)))
    (when (not= :fed reason)
      (ctx/emit! c :breed.gave-up :warn {:reason reason :text (str "breeding stopped: " (name reason) " (" seen ")")}))
    (if ending
      (assoc ending :data result)
      (let [[kind cause] (get ending-of reason [:nothing-possible reason])]
        (ctx/end c kind cause result)))))

(defn candidates
  "The adults within radius not fed, refused or given up on, nearest first."
  [c]
  (let [{:keys [mob radius]} (:args c)
        {:keys [fed refused given-up]} (ctx/mem c)
        skip (into (set fed) (concat refused (keys given-up)))]
    (->> (animals/adults (:primitives c) mob radius)
         (filterv #(not (contains? skip (animals/key-of %))))
         (animals/allowed c :breed.declined "breed" :harvest))))

(defn out-reason
  "Why the job ends with no candidate to feed."
  [c]
  (let [{:keys [fed refused given-up]} (ctx/mem c)]
    (cond
      (some #{:unreachable} (vals given-up)) :unreachable
      (odd? (count fed)) :unpaired
      (seq refused) :refused
      (animals/refusal c) (animals/refusal c)
      :else :too-few)))

(defn fed?
  "Whether the animal went into love mode."
  [r]
  (true? (.-love r)))

(defn baby?
  "Whether the food was eaten without love: a baby the sensing missed."
  [r]
  (and (not (fed? r)) (pos? (or (.-consumed r) 0))))

(defn ^:async feed!
  "Use food on animal once; book the outcome. Resolves to :no-food when the
  food is gone, else :again."
  [c animal food]
  (let [k (animals/key-of animal)
        _ (ctx/update-mem! c assoc :food food)
        _ (note-hand! c)
        r (await (ctx/act c :interact #js {:id (.-id animal) :item food}))]
    (case (.-status r)
      "used" (cond
               (fed? r) (ctx/update-mem! c #(-> % (update :fed (fnil conj []) k) (assoc :in-row 0)))
               (baby? r) (do (animals/give-up! c k :baby)
                             (ctx/emit! c :breed.baby :warn {:uuid k :text "fed a baby the sensing missed; given up on"}))
               (zero? (or (.-consumed r) 0)) (do (ctx/update-mem! c update :refused (fnil conj []) k)
                                                 (animals/bump-row! c))
               :else (animals/reset-row! c))
      "no-effect" (do (ctx/update-mem! c update :refused (fnil conj []) k)
                      (animals/bump-row! c))
      "gone" (animals/give-up! c k :gone)
      "cannot" (do (animals/give-up! c k :cannot) (animals/bump-row! c))
      "failed" (do (animals/give-up! c k :failed) (animals/bump-row! c))
      "out-of-reach" (let [n (inc (get-in (ctx/mem c) [:fails k] 0))]
                       (ctx/update-mem! c assoc-in [:fails k] n)
                       (when (>= n 2) (animals/give-up! c k :unreachable))
                       (animals/bump-row! c))
      nil)
    (if (= "no-item" (.-status r)) :no-food :again)))

(defn ^:async no-food!
  "Out of food: done when some animals were fed, else :no-food (with the failed fetch's ending when given)."
  [c & [ending]]
  (if (seq (:fed (ctx/mem c)))
    (await (finish! c :food-out))
    (await (finish! c :no-food ending))))

(defn ^:async engage!
  "Walk to the animal and feed it; finish when out of food or fruitless three times in a row."
  [c animal food]
  (let [walked (await (animals/walk! c (animals/key-of animal) animal (reach) {:reset-row? true}))
        fed (case walked
              :there (await (feed! c animal food))
              :again)]
    (cond
      (= :no-food fed) (await (no-food! c))
      (>= (:in-row (ctx/mem c) 0) (max-in-row)) (await (finish! c (out-reason c)))
      :else :again)))

(defn bees-indoors?
  [c]
  (let [s (.self (:primitives c))]
    (and (= "bee" (:mob (:args c)))
         (or (not (true? (.-isDay s))) (true? (.-raining s))))))

(defn fetchable?
  "Whether breeding food is to be fetched now: none carried, animals are there to feed, :count is not reached and :fetch
  allows it."
  [c]
  (let [{:keys [mob radius] n :count} (:args c)
        fed (count (:fed (ctx/mem c)))
        p (:primitives c)]
    (boolean
     (and (fetch/parse-arg (:fetch (:args c)))
          (contains? animals/breeding-food mob)
          (< fed n)
          (nil? (animals/food-carried p mob))
          (not (bees-indoors? c))
          (or (pos? fed) (>= (count (animals/adults p mob radius)) 2))
          (seq (candidates c))))))

(defn ^:async fetch-food!
  "No food carried: fetch it when :fetch allows and animals wait, else end :no-food. Resolves to :again once food
  arrived, else the round's result."
  [c]
  (if-not (fetchable? c)
    (await (no-food! c))
    (let [{:keys [mob] n :count} (:args c)
          limits (select-keys (fetch/parse-arg (:fetch (:args c))) [:how :depth :explore])
          e (await (u/untimed! c #(need/blocks! c (get animals/breeding-food mob) (- n (count (:fed (ctx/mem c)))) limits)))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else (await (no-food! c e))))))

(defn ^:async step
  "One walk or feeding, or the end."
  [c]
  (let [now (ctx/now c)
        {:keys [mob] n :count} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [m (ctx/mem c)
          food (animals/food-carried (:primitives c) mob)
          cands (candidates c)]
      (cond
        (>= (- now (:started m)) (* 1000 (time-limit c))) (await (finish! c :timeout))
        (not (contains? animals/breeding-food mob)) (await (finish! c :unknown-mob))
        (bees-indoors? c) (await (finish! c :bees-indoors))
        (>= (count (:fed m)) n) (await (finish! c :fed))
        (nil? food) (await (fetch-food! c))
        (or (empty? cands) (and (empty? (:fed m)) (< (count (animals/adults (:primitives c) mob (:radius (:args c)))) 2))) (await (finish! c (out-reason c)))
        :else (await (engage! c (first cands) food))))))

(defn ^:async round
  "The whole attempt: walk and feed until the job ends."
  [c]
  (animals/reserve-bait! c (:mob (:args c)))
  (loop []
    (let [r (await (step c))]
      (if (= :again r) (recur) r))))
