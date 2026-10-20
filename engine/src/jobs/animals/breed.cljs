(ns jobs.animals.breed
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.animals :as animals]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.bait :as bait]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Feed :pairs pairs (two animals each) of type :mob (such as \"cow\") within :radius so that they breed; the goal is the
  calf. A one-shot order that ends itself; a cut job resumes.

  One call is the whole run. It takes the nearest adult not yet fed, refused or given up on. The body walks to within 3 blocks
  (like go-to it opens a shut gate or door on the way and shuts it behind, so a gated pen is entered and its gate
  left shut; a pen with no way in is :unreachable). It then feeds the first breeding
  food of the mob that it carries, fetching one first when none is carried and animals wait (jobs.lib.need, one
  per animal still to feed, unless the ways are only #{:carried}) (jobs.lib.animals/breeding-food: wheat for cows, sheep, goats and
  mooshrooms; carrot, potato or beetroot for pigs; seeds for chickens; carrot, golden carrot or dandelion for
  rabbits; flowers for bees). Babies are never fed.

  Per animal:
  - Fed: it went into love mode.
  - Refused: nothing happened twice (on its breeding cooldown, or already in love; a lost click is tried once more). It is not tried again.
  - Given up: :unreachable (walk blocked, or two feedings out of reach), :gone, :cannot, :failed, or :baby
    (the food was eaten without love; a baby the sensing missed, warn breed.baby).

  The data {:reason :fed [keys]
  :refused [keys] :given-up {key reason} :food item :adults n :babies n :hand} and the ending by the reason:
  - :done :fed: the pairs were fed and a calf is in view that was not there at the start. Any stop after at least :least animals were fed
    (default 2: a pair) with a calf in view is done, with the stop as the data reason; :least :all is all the animals of the pairs.
    A pair fed with no calf seen after the calf wait ends nothing-possible :no-baby.
  - :impossible :unknown-mob (no breeding food is known for :mob, checked at the start); bees at night or in rain
    wait :bees-indoors before every round.
  - :condition-failed :no-food (none carried and none fetched, nothing fed yet; a listed call with :ways #{:carried} waits for food; a failed fetch ends with the fetch's own ending, reason :no-food, its cause naming the food), :too-few (fewer than two adults stand near before the first feeding, or no candidate is left),
    :refused (some refused, none ate) or :no-zones (the zone rules refused the adults).
  - :nothing-possible :food-out (the food ran out below :least fed), :unreachable (one was given up as unreachable)
    or :unpaired (an odd number fed), when no candidate is left. The same after three fruitless animals in a row.

  With :box (or :plan and :part, the pen of a plan part as maintain reads it) the body knows a pen: when fewer than two adults
  stand near it first walks to the middle of the box (once), and when it ends inside the box, however it ends (the
  cleanup, after the hand is put back), it walks back out to where it started (go-to shuts the gate behind it).

  Animals follow a body holding their food, so the hand is given back. Before the first feeding the job notes
  what the hand holds. When it ends, however (the cleanup), it equips that item again if still carried, else empties the hand. :hand is
  :restored, :emptied or :full (no free slot: the food stays in hand, warn breed.hand-full). There is no :hand
  when nothing was fed. A cancel or the engine's limit restores it too.

  After a pair was fed with no baby in view, the run pauses (a jobs.time.noop child) the setting calf-wait-s for the
  calf before it counts the babies, so :babies is what the body sees.

  A run that fed at least two animals writes one memory entry of kind :bred/<mob> (data {:fed [uuids]}, default
  one hour). A trigger can use it, e.g. (or (not (known? (since :bred/cow))) (> (since :bred/cow) 1200)).

  Zones: an animal standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  breed.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "the mob type to breed, such as \"cow\"" :spec a/name? :default nil}
   :pairs {:doc "pairs of animals to feed (two animals each)" :spec (a/int-in 1 nil) :default 1}
   :radius {:doc "animals within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :box {:doc "a known pen {:min :max} to walk to when the animals are not in view, and to leave at the end" :spec pen-read/box-arg :default nil}
   :plan {:doc "id of a plan of the body's world; with :part, the known pen is that part's :box" :spec a/name? :default nil}
   :part {:doc "with :plan, the id of the pen part" :spec a/name? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds of working, fetching included, before breeding gives up." :spec (a/num-in 0 nil)}
   ::calf-wait-s {:default 4 :doc "Seconds to wait for the calf to be seen after a pair was fed, before the count of babies is reported." :spec (a/num-in 0 nil)}
   ::max-in-row {:default 3 :doc "Animals in a row that brought nothing (refused, given up on) before the run ends." :spec (a/int-in 0 nil)}})
(defn max-in-row [] (settings/get settings ::max-in-row))
(defn calf-wait-ms [] (* 1000 (settings/get settings ::calf-wait-s)))

(def ways need/fetch-ways)

(def target [:mob])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s} :least 2})

(defn can-start?
  "Never :unknown-mob: no breeding food is known for :mob. Later :no-food: a known mob, no breeding food carried and
  ways #{:carried}."
  [c]
  (let [{:keys [mob]} (:args c)]
    (cond
      (not (contains? animals/breeding-food mob)) (contract/never :unknown-mob (str "no breeding food is known for " (pr-str mob)))
      (and (not (need/fetch-opts c))
           (nil? (animals/food-carried (:primitives c) mob)))
      (contract/later :no-food (str "no breeding food for " mob " carried and the ways are only #{:carried}"))
      :else (contract/ok :food "food carried or fetched"))))

(defn wanted
  "Animals to feed: two for each of :pairs."
  [c]
  (* 2 (:pairs (:args c) 1)))

(defn new-babies
  "How many babies of the mob are in view beyond those there at the first feeding; none before it."
  [c]
  (let [{:keys [mob radius]} (:args c)
        before (:babies-before (ctx/mem c))]
    (if before (max 0 (- (count (animals/babies (:primitives c) mob radius)) before)) 0)))

(defn quote-of [c]
  {:uses {:job-time [0 (* 15 (wanted c)) ##Inf]}})

(declare result)

(defn done?
  "Ok once as many new babies are in view as :pairs, else later: the goal is the calf, not the feeding."
  [c]
  (let [fed (count (:fed (ctx/mem c)))
        babies (new-babies c)
        left (max 0 (- (:pairs (:args c) 1) babies))
        data [:units :babies :done babies :left left :result (result c :fed)]]
    (if (pos? left)
      (apply contract/later :feeding (str fed " animals fed, " babies " new babies") data)
      (apply contract/ok :calves (str babies " new babies") data))))

(defn least-fed
  "Animals that must be fed for the run to be done: the call's :least (:all = every animal of the pairs), at most all of them."
  [c]
  (let [n (wanted c)
        l (:least (ctx/frozen-call c) 2)]
    (if (= :all l) n (min n l))))

(def ending-of
  "The kind and cause of a finish by reason."
  {:fed [:done nil]
   :no-food [:condition-failed :no-food]
   :food-out [:nothing-possible :food-out]
   :too-few [:condition-failed :too-few]
   :refused [:condition-failed :refused]
   :no-zones [:condition-failed :no-zones]
   :no-baby [:nothing-possible :no-baby]})

(defn ^:async restore-hand!
  "Put the hand back as note-hand! found it. Resolves to :restored, :emptied
  or :full (the food stays in hand)."
  [c before]
  (if (some #(= before (:name %)) (u/inventory (:primitives c)))
    (do (await (ctx/act c :equip #js {:item before})) :restored)
    (let [r (await (ctx/act c :unequip #js {}))]
      (if (= "full" (.-status r)) :full :emptied))))

(declare leave-pen! mark-bred!)

(defn ^:async cleanup
  "However the run ended: the hand put back as note-hand! found it, then the body walked out of the known pen to where
  the run started; done with {:hand outcome} once the hand was noted, else with no data."
  [c _]
  (mark-bred! c)
  (when (and (:hand-noted (ctx/mem c)) (not (:hand-outcome (ctx/mem c))))
    (let [outcome (await (restore-hand! c (:hand-before (ctx/mem c))))]
      (ctx/update-mem! c assoc :hand-outcome outcome)
      (when (= :full outcome)
        (ctx/emit! c :breed.hand-full :warn {:text "no free slot: the food stays in hand"}))))
  (if (= :continue (await (leave-pen! c)))
    :continue
    (ctx/end c :done nil (if-let [h (:hand-outcome (ctx/mem c))] {:hand h} {}))))

(defn note-hand!
  "Remember what the hand holds and the babies in view, once per run, before the first feeding."
  [c]
  (when-not (:hand-noted (ctx/mem c))
    (let [{:keys [mob radius]} (:args c)]
      (ctx/update-mem! c assoc :hand-before (.-held (.self (:primitives c))) :hand-noted true
                       :babies-before (count (animals/babies (:primitives c) mob radius))))))

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
  (let [m (ctx/mem c)
        now (ctx/now c)]
    (when (and (>= (count (:fed m)) 2) (zero? (new-babies c)))
      (let [from (or (:calf-from m) now)]
        (ctx/update-mem! c assoc :calf-from from)
        (< (- now from) (calf-wait-ms))))))

(defn result
  "The run's result so far: why it ended, who was fed, refused and given up on, what is in view and a text."
  [c reason]
  (let [m (ctx/mem c)
        {:keys [mob radius]} (:args c)
        p (:primitives c)
        result {:reason reason
                :fed (vec (:fed m))
                :refused (vec (:refused m))
                :given-up (:given-up m {})
                :food (or (animals/food-carried p mob) (:food m))
                :adults (count (animals/adults p mob radius))
                :babies (count (animals/babies p mob radius))}]
    (assoc result :text (str "breed done: " (name reason) ", fed " (count (:fed m)) ", adults " (:adults result) ", babies " (:babies result)))))

(defn finish-now!
  "Emit the outcome and end the call; see finish!."
  [c reason ending]
  (let [m (ctx/mem c)
        fed (count (:fed m))
        [kind cause] (cond
                       (< fed (least-fed c)) (get ending-of reason [:nothing-possible reason])
                       (and (>= fed 2) (zero? (new-babies c))) (ending-of :no-baby)
                       :else [:done nil])]
    (contract/with-because (ctx/end c kind cause (result c reason)) ending)))

(defn ^:async finish!
  "Emit the outcome and end the call: the ending of reason (ending-of, else nothing-possible), with ending (a failed
  fetch's) as its :because. The hand is put back by cleanup. Resolves to :continue while a calf is awaited (a pause child)."
  [c reason & [ending]]
  (if-not (calf-pending? c)
    (finish-now! c reason ending)
    (let [e (await (child/pause! c (/ (calf-wait-ms) 1000) :calf-wait))]
      (if (= :continue e) :continue (finish-now! c reason ending)))))

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
  "Whether the animal went into love mode (raw: the click's answer as a map)."
  [raw]
  (true? (:love raw)))

(defn baby?
  "Whether the food was eaten without love: a baby the sensing missed."
  [raw]
  (and (not (fed? raw)) (pos? (or (:consumed raw) 0))))

(defn ^:async feed!
  "Use food on animal once through an items.use child; book the outcome. Resolves to :continue while the child
  waits, :no-food when the food is gone, else :again."
  [c animal food]
  (let [k (animals/key-of animal)
        _ (ctx/update-mem! c assoc :food food)
        _ (note-hand! c)
        r (await (animals/use-on! c k animal food))]
    (if (= :continue r)
      :continue
      (let [{:keys [status raw]} r]
        (case status
          :used (cond
                  (fed? raw) (ctx/update-mem! c #(-> % (update :fed (fnil conj []) k) (assoc :in-row 0)))
                  (baby? raw) (do (animals/give-up! c k :baby)
                                  (ctx/emit! c :breed.baby :warn {:uuid k :text "fed a baby the sensing missed; given up on"}))
                  (zero? (or (:consumed raw) 0)) (do (ctx/update-mem! c update :refused (fnil conj []) k)
                                                     (animals/bump-row! c))
                  :else (animals/reset-row! c))
          :no-effect (if (contains? (:retried (ctx/mem c)) k)
                       (do (ctx/update-mem! c update :refused (fnil conj []) k)
                           (animals/bump-row! c))
                       (ctx/update-mem! c update :retried (fnil conj #{}) k))
          :no-item nil
          (animals/book-failed! c k status))
        (if (= :no-item status) :no-food :again)))))

(defn ^:async no-food!
  "Out of food: done when some animals were fed, else :no-food (with the failed fetch's ending when given)."
  [c & [ending]]
  (if (seq (:fed (ctx/mem c)))
    (await (finish! c :food-out))
    (await (finish! c :no-food ending))))

(defn ^:async engage!
  "Feed the animal; finish when out of food or fruitless :max-in-row times in a row."
  [c animal food]
  (let [fed (await (feed! c animal food))]
    (cond
      (= :continue fed) :continue
      (= :no-food fed) (await (no-food! c))
      (>= (:in-row (ctx/mem c) 0) (max-in-row)) (await (finish! c (out-reason c)))
      :else :again)))

(defn bees-indoors?
  [c]
  (let [s (.self (:primitives c))]
    (and (= "bee" (:mob (:args c)))
         (or (not (true? (.-isDay s))) (true? (.-raining s))))))

(defn fetchable?
  "Whether breeding food is to be fetched now: none carried, animals are there to feed, :count is not reached and the call's ways
  allow it."
  [c]
  (let [{:keys [mob radius]} (:args c)
        n (wanted c)
        fed (count (:fed (ctx/mem c)))
        p (:primitives c)]
    (boolean
     (and (need/fetch-opts c)
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
  "No food carried: fetch it when the call's ways allow and animals wait, else end :no-food. Resolves to :again once food
  arrived, else the round's result."
  [c]
  (if-not (fetchable? c)
    (await (no-food! c))
    (let [e (await (bait/food! c (- (wanted c) (count (:fed (ctx/mem c))))))]
      (cond
        (= :continue e) :continue
        (= :ok e) :again
        :else (await (no-food! c e))))))

(defn pen-box
  "The known pen's box ({:min :max}): :box, else the box of the plan part; nil when neither is known."
  [c]
  (or (pen-read/arg-box c) (:box (:args (pen-read/pen-of c)))))

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
  (let [r (await (child/call-walk* c :pen {:pos (pen-read/look-pos box) :range 3}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c assoc :pen-walked true) :again))))

(defn ^:async leave-pen!
  "Walk back to where the run started when it ends with the body inside the known pen. Resolves to :continue while the
  walk goes on, else nil."
  [c]
  (let [box (pen-box c)
        from (:from (ctx/mem c))]
    (when (and box from (animals/in-box? box (u/self-pos c)))
      (when (= :continue (await (child/call-walk* c :leave {:pos from :range 1})))
        :continue))))

(defn ^:async step
  "One walk or feeding, or the end."
  [c]
  (let [{:keys [mob]} (:args c)
        n (wanted c)
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
      :else (await (engage! c (animals/pick c cands) food)))))

(defn ^:async round
  "The whole attempt: steps (a pace between; can-run? is asked between them) until the job ends."
  [c]
  (animals/reserve-bait! c (:mob (:args c)))
  (await (pace/steps! c #(step c))))
