(ns jobs.animals.breed
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.animals.tend :as tend]
            [jobs.lib.animals :as animals]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
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

  Ends with a warn breed.gave-up unless the reason is :fed. The data {:reason :fed [keys]
  :refused [keys] :given-up {key reason} :food item :adults n :babies n :hand} and the ending by the reason:
  - :done :fed: :count animals were fed. Any stop after at least :least animals were fed (default 2: a pair) is
    done, with the stop as the data reason (warn breed.gave-up); :least :all is :count.
  - :impossible :unknown-mob (no breeding food is known for :mob, checked at the start); bees at night or in rain
    wait :bees-indoors before every round.
  - :condition-failed :no-food (none carried and none fetched, nothing fed yet; a listed call with :fetch false waits for food; a failed fetch ends with the fetch's own ending, reason :need, its cause naming the food), :too-few (fewer than two adults stand near before the first feeding, or no candidate is left),
    :refused (some refused, none ate) or :no-zones (the zone rules refused the adults).
  - :nothing-possible :food-out (the food ran out below :least fed), :unreachable (one was given up as unreachable)
    or :unpaired (an odd number fed), when no candidate is left. The same after three fruitless animals in a row.

  With :box (or :plan and :part, the pen of a plan part as tend reads it) the body knows a pen: when fewer than two adults
  stand near it first walks to the middle of the box (once), and when it ends inside the box it walks back out to where it
  started (go-to shuts the gate behind it).

  Animals follow a body holding their food, so the hand is given back. Before the first feeding the job notes
  what the hand holds. When it ends, however (the cleanup), it equips that item again if still carried, else empties the hand. :hand is
  :restored, :emptied or :full (no free slot: the food stays in hand, warn breed.hand-full). There is no :hand
  when nothing was fed. A cancel or the engine's limit restores it too.

  After a pair was fed with no baby in view, the run waits (yielding :continue) up to the setting calf-wait-s for the
  calf before it counts the babies, so :babies is what the body sees.

  A run that fed at least two animals writes one memory entry of kind :bred/<mob> (data {:fed [uuids]}, default
  one hour). A trigger can use it, e.g. (or (not (known? (since :bred/cow))) (> (since :bred/cow) 1200)).

  Zones: an animal standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  breed.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "the mob type to breed, such as \"cow\"" :spec a/name? :default nil}
   :count {:doc "animals to feed" :spec (a/int-in 1 nil) :default 2}
   :radius {:doc "animals within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :fetch {:doc "get breeding food when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-food; a failed fetch ends :need" :spec need/option? :default true}
   :box {:doc "a known pen {:min :max} to walk to when the animals are not in view, and to leave at the end" :spec a/box? :default nil}
   :plan {:doc "id of a plan of the body's world; with :part, the known pen is that part's :box" :spec a/name? :default nil}
   :part {:doc "with :plan, the id of the pen part" :spec a/name? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds of working, fetching included, before breeding gives up." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::calf-wait-s {:default 4 :doc "Seconds to wait for the calf to be seen after a pair was fed, before the count of babies is reported." :spec (a/num-in 0 nil)}
   ::max-in-row {:default 3 :doc "Animals handled in a row before the body looks again." :spec (a/int-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-in-row [] (settings/get settings ::max-in-row))
(defn calf-wait-ms [] (* 1000 (settings/get settings ::calf-wait-s)))

(def target [:mob])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s} :least 2})

(defn can-start?
  "Never :unknown-mob: no breeding food is known for :mob. Later :no-food: a known mob, no breeding food carried and
  :fetch off."
  [c]
  (let [{:keys [mob fetch]} (:args c)]
    (cond
      (not (contains? animals/breeding-food mob)) (contract/never :unknown-mob (str "no breeding food is known for " (pr-str mob)))
      (and (not (need/parse-arg fetch))
           (nil? (animals/food-carried (:primitives c) mob)))
      (contract/later :no-food (str "no breeding food for " mob " carried and :fetch is off"))
      :else (contract/ok :food "food carried or fetched"))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 15 (:count (:args c) 2)) ##Inf]}})

(defn done?
  "Later with the animals fed against :count: the hand is put back by the run's own ending, so the goal never holds before it."
  [c]
  (let [fed (count (:fed (ctx/mem c)))]
    (contract/later :feeding (str fed " animals fed") :units :animals :done fed :left (max 0 (- (:count (:args c) 2) fed)))))

(defn least-fed
  "Animals that must be fed for the run to be done: the call's :least (:all = :count), at most :count."
  [c]
  (let [n (:count (:args c) 2)
        l (:least (ctx/frozen-call c) 2)]
    (if (= :all l) n (min n l))))

(def ending-of
  "The kind and cause of a finish by reason."
  {:fed [:done nil]
   :no-food [:condition-failed :no-food]
   :food-out [:nothing-possible :food-out]
   :too-few [:condition-failed :too-few]
   :refused [:condition-failed :refused]
   :no-zones [:condition-failed :no-zones]})

(defn ^:async restore-hand!
  "Put the hand back as note-hand! found it. Resolves to :restored, :emptied
  or :full (the food stays in hand)."
  [c before]
  (if (some #(= before (:name %)) (u/inventory (:primitives c)))
    (do (await (ctx/act c :equip #js {:item before})) :restored)
    (let [r (await (ctx/act c :unequip #js {}))]
      (if (= "full" (.-status r)) :full :emptied))))

(defn ^:async cleanup
  "However the run ended: the hand put back as note-hand! found it; done with {:hand outcome} once the hand was noted,
  else with no data."
  [c _]
  (let [m (ctx/mem c)]
    (if-not (:hand-noted m)
      (ctx/end c :done nil {})
      (let [outcome (await (restore-hand! c (:hand-before m)))]
        (when (= :full outcome)
          (ctx/emit! c :breed.hand-full :warn {:text "no free slot: the food stays in hand"}))
        (ctx/end c :done nil {:hand outcome})))))

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

(defn calf-pending?
  "Whether a pair was fed and no baby is in view yet: a calf shows 1-3 s after the hearts, so for :calf-wait-s the
  count of babies would be stale. Notes the first look in mem."
  [c]
  (let [{:keys [mob radius]} (:args c)
        m (ctx/mem c)
        now (ctx/now c)]
    (when (and (>= (count (:fed m)) 2) (empty? (animals/babies (:primitives c) mob radius)))
      (let [from (or (:calf-from m) now)]
        (ctx/update-mem! c assoc :calf-from from)
        (< (- now from) (calf-wait-ms))))))

(defn finish-now!
  "Emit the outcome and end the call; see finish!."
  [c reason ending]
  (let [m (ctx/mem c)
        {:keys [mob radius]} (:args c)
        p (:primitives c)
        result {:reason reason
                :fed (vec (:fed m))
                :refused (vec (:refused m))
                :given-up (:given-up m {})
                :food (or (animals/food-carried p mob) (:food m))
                :adults (count (animals/adults p mob radius))
                :babies (count (animals/babies p mob radius))}
        seen (str "adults " (:adults result) ", babies " (:babies result))
        result (assoc result :text (str "breed done: " (name reason) ", fed " (count (:fed m)) ", " seen))]
    (mark-bred! c)
    (when (not= :fed reason)
      (ctx/emit! c :breed.gave-up :warn {:reason reason :text (str "breeding stopped: " (name reason) " (" seen ")")}))
    (if ending
      (assoc ending :data result)
      (let [[kind cause] (if (>= (count (:fed m)) (least-fed c))
                           [:done nil]
                           (get ending-of reason [:nothing-possible reason]))]
        (ctx/end c kind cause result)))))

(defn finish!
  "Emit the outcome and end the call: the ending of reason (ending-of, else nothing-possible), or ending when given
  (a failed fetch's). The hand is put back by cleanup. Resolves to :continue while a calf is awaited."
  [c reason & [ending]]
  (if (calf-pending? c)
    :continue
    (finish-now! c reason ending)))

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
     (and (need/parse-arg (:fetch (:args c)))
          (< fed n)
          (nil? (animals/food-carried p mob))
          (or (pos? fed) (>= (count (animals/adults p mob radius)) 2))
          (seq (candidates c))))))

(defn can-run?
  "Later :bees-indoors: bees breed at night or in rain only out of the hive, so the job waits for the weather."
  [c]
  (if (bees-indoors? c)
    (contract/later :bees-indoors "bees are indoors at night or in rain")
    (contract/ok :outdoors "the animals are out")))

(defn ^:async fetch-food!
  "No food carried: fetch it when :fetch allows and animals wait, else end :no-food. Resolves to :again once food
  arrived, else the round's result."
  [c]
  (if-not (fetchable? c)
    (await (no-food! c))
    (let [{:keys [mob] n :count} (:args c)
          limits (select-keys (need/parse-arg (:fetch (:args c))) [:how :explore])
          e (await (need/blocks! c (get animals/breeding-food mob) (- n (count (:fed (ctx/mem c)))) limits))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else (await (no-food! c e))))))

(defn pen-box
  "The known pen's box ({:min :max}): :box, else the box of the plan part; nil when neither is known."
  [c]
  (or (:box (:args c)) (:box (:args (tend/pen-of c)))))

(defn pen-cell
  "The [x z] cell of pos, as go-to takes it."
  [{:keys [x z]}]
  [(js/Math.floor x) (js/Math.floor z)])

(defn ^:async walk-to-pen!
  "Walk to the middle of the known pen, once; the cell the body started from is kept for the way out. Resolves to
  :continue while the walk goes on, else :again."
  [c box]
  (when-not (:from (ctx/mem c))
    (ctx/update-mem! c assoc :from (pen-cell (u/self-pos c))))
  (let [r (await (child/call-walk* c :pen {:pos (tend/look-pos box) :range 3}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c assoc :pen-walked true) :again))))

(defn ^:async leave-pen!
  "Walk back to where the run started when it ends with the body inside the known pen. Resolves to :continue while the
  walk goes on, else the ending r."
  [c r]
  (let [box (pen-box c)
        from (:from (ctx/mem c))]
    (if-not (and box from (animals/in-box? box (u/self-pos c)))
      r
      (let [w (await (child/call-walk* c :leave {:pos from :range 1}))]
        (if (= :continue w) :continue r)))))

(defn ^:async step
  "One walk or feeding, or the end."
  [c]
  (let [{:keys [mob] n :count} (:args c)
        m (ctx/mem c)
        food (animals/food-carried (:primitives c) mob)
        cands (candidates c)]
    (cond
      (>= (count (:fed m)) n) (await (finish! c :fed))
      (and (not (:pen-walked m)) (pen-box c) (not (animals/in-box? (pen-box c) (u/self-pos c)))
           (< (count (animals/adults (:primitives c) mob (:radius (:args c)))) 2)) (await (walk-to-pen! c (pen-box c)))
      (and (empty? cands) (empty? (:fed m)) (not (look/surveyed? c))) (await (look/survey! c)) ; animals count only while in view
      (nil? food) (await (fetch-food! c))
      (or (empty? cands) (and (empty? (:fed m)) (< (count (animals/adults (:primitives c) mob (:radius (:args c)))) 2))) (await (finish! c (out-reason c)))
      :else (await (engage! c (first cands) food)))))

(defn ^:async round
  "The whole attempt: walk and feed until the job ends."
  [c]
  (animals/reserve-bait! c (:mob (:args c)))
  (if-let [r (:ending (ctx/mem c))]
    (await (leave-pen! c r))
    (let [r (loop []
              (let [r (await (step c))]
                (if (= :again r) (recur) r)))]
      (if (contract/ending? r)
        (do (ctx/update-mem! c assoc :ending r)
            (await (leave-pen! c r)))
        r))))
