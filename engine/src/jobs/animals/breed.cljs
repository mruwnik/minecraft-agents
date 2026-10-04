(ns jobs.animals.breed
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.util :as u]))

(def doc
  "Feed :count animals of the mob type :mob (a name such as \"cow\") within
  :radius so that they breed: a one-shot order that starts and ends itself.
  Each round takes the nearest adult not fed, refused or given up on yet,
  walks to within 3 blocks (moveTo range 2, :walk-timeout-s) and uses the
  first breeding food of the mob's list that the body carries on it
  (engine.jobs.animals/breeding-food: wheat for cows, sheep, goats and
  mooshrooms; carrot, potato or beetroot for pigs; seeds for chickens; carrot,
  golden carrot or dandelion for rabbits; flowers for bees). Animals are tracked
  by uuid (entity ids change when chunks reload), by id when it has none. An
  animal is fed when it enters love mode; it is refused when nothing happens
  (no-effect: on its breeding cooldown, or already in love); it is given up on
  when its walk is blocked (:unreachable), when two feedings were out of reach
  (:unreachable), when it vanished (:gone), when the server said it cannot
  (:cannot) or the use failed (:failed), or when the food was eaten without
  love, a baby the sensing missed (:baby, and a warn breed.baby). Babies are
  never fed. Done (info
  breed.done, and a warn breed.gave-up with the reason unless it is :fed;
  hands over {:reason :fed [keys] :refused [keys] :given-up {key reason} :food
  item :adults n :babies n}) with :reason :fed once :count animals were fed,
  :timeout after :timeout-s from the first round, :unknown-mob when :mob has
  no breeding food, :bees-indoors for bees at night or in rain, :no-food
  when no food of the mob is carried (or it ran out), and, when no candidate
  is left or fewer than two are present before the first feeding, :unreachable
  when one was given up on as unreachable, :unpaired when an odd number was
  fed, :refused when some refused and none ate, else :too-few. It also ends,
  with the reason of that rule, after three fruitless rounds in a row (blocked
  walks, refusals, out-of-reach, cannot or failed feedings), before the engine
  would back it off. The check
  always passes, so a cut job resumes and ends itself.
  Animals follow a body holding their food, so the hand is given back: right
  before the first feeding the job notes what the hand holds, and when it ends
  (every reason) it equips that item again when still carried, else empties the
  hand (unequip). The handover then has :hand, :restored (equipped the previous
  item), :emptied (unequipped, or already empty) or :full (no free slot: the
  food stays in hand, and a warn breed.hand-full); no :hand when nothing was
  fed. A cut job restores the hand when it resumes and ends; a job cancelled
  while cut does not (there is no cancel hook). A run that fed at least two
  animals leaves one body-memory entry of kind :bred/<mob> (:bred/cow, data
  {:fed [uuids]}, the engine's default policy: an hour), so a trigger on
  (or (not (known? (since :bred/cow))) (> (since :bred/cow) 1200)) breeds again
  20 minutes after the last time; a run that fed nobody or one leaves nothing.")

(def args
  {:mob {:doc "the mob type to breed, such as \"cow\"" :default nil}
   :count {:doc "animals to feed" :default 2}
   :radius {:doc "animals within this many blocks count" :default 16}
   :walk-timeout-s {:doc "bound of one walk towards an animal" :default 5}
   :timeout-s {:doc "seconds from the first round before the job gives up" :default 120}})

(def reach 3)
(def max-in-row 3)

(defn check [_c] true)

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
  "Put the hand back, emit the outcome, hand it to the parent and end the job."
  [c reason]
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
                :babies (count (animals/babies p mob radius))})]
    (mark-bred! c)
    (ctx/emit! c :breed.done :info (assoc result :text (str "breed done: " (name reason) ", fed " (count (:fed m)))))
    (when (not= :fed reason)
      (ctx/emit! c :breed.gave-up :warn {:reason reason :text (str "breeding stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn candidates
  "The adults within radius not fed, refused or given up on, nearest first."
  [c]
  (let [{:keys [mob radius]} (:args c)
        {:keys [fed refused given-up]} (ctx/mem c)
        skip (into (set fed) (concat refused (keys given-up)))]
    (filterv #(not (contains? skip (animals/key-of %))) (animals/adults (:primitives c) mob radius))))

(defn out-reason
  "Why the job ends with no candidate to feed."
  [c]
  (let [{:keys [fed refused given-up]} (ctx/mem c)]
    (cond
      (some #{:unreachable} (vals given-up)) :unreachable
      (odd? (count fed)) :unpaired
      (seq refused) :refused
      :else :too-few)))

(defn give-up! [c k reason]
  (ctx/update-mem! c assoc-in [:given-up k] reason))

(defn bump-row! [c]
  (ctx/update-mem! c update :in-row (fnil inc 0)))

(defn reset-row! [c]
  (ctx/update-mem! c assoc :in-row 0))

(defn ^:async walk!
  "Walk within reach of animal when further than reach. Resolves to :there,
  :partial or :blocked; a blocked walk gives the animal up."
  [c animal]
  (let [tpos (u/pos-of (.-pos animal))]
    (if (<= (u/dist (u/self-pos c) tpos) reach)
      :there
      (let [r (await (ctx/act c :moveTo (clj->js {:pos tpos :range 2 :timeoutS (:walk-timeout-s (:args c))})))]
        (case (.-status r)
          "arrived" (do (reset-row! c) :there)
          "partial" :partial
          (do (give-up! c (animals/key-of animal) :unreachable)
              (bump-row! c)
              :blocked))))))

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
  food is gone, else :continue."
  [c animal food]
  (let [k (animals/key-of animal)
        _ (ctx/update-mem! c assoc :food food)
        _ (note-hand! c)
        r (await (ctx/act c :interact #js {:id (.-id animal) :item food}))]
    (case (.-status r)
      "used" (cond
               (fed? r) (ctx/update-mem! c #(-> % (update :fed (fnil conj []) k) (assoc :in-row 0)))
               (baby? r) (do (give-up! c k :baby)
                             (ctx/emit! c :breed.baby :warn {:uuid k :text "fed a baby the sensing missed; given up on"}))
               :else (reset-row! c))
      "no-effect" (do (ctx/update-mem! c update :refused (fnil conj []) k)
                      (bump-row! c))
      "gone" (give-up! c k :gone)
      "cannot" (do (give-up! c k :cannot) (bump-row! c))
      "failed" (do (give-up! c k :failed) (bump-row! c))
      "out-of-reach" (let [n (inc (get-in (ctx/mem c) [:fails k] 0))]
                       (ctx/update-mem! c assoc-in [:fails k] n)
                       (when (>= n 2) (give-up! c k :unreachable))
                       (bump-row! c))
      nil)
    (if (= "no-item" (.-status r)) :no-food :continue)))

(defn ^:async engage!
  "Walk to the animal and feed it; finish when out of food or fruitless three times in a row."
  [c animal food]
  (let [walked (await (walk! c animal))
        fed (case walked
              :there (await (feed! c animal food))
              :continue)]
    (cond
      (= :no-food fed) (await (finish! c :no-food))
      (>= (:in-row (ctx/mem c) 0) max-in-row) (await (finish! c (out-reason c)))
      :else :continue)))

(defn bees-indoors?
  [c]
  (let [s (.self (:primitives c))]
    (and (= "bee" (:mob (:args c)))
         (or (not (true? (.-isDay s))) (true? (.-raining s))))))

(defn ^:async round [c]
  (let [now (ctx/now c)
        {:keys [mob timeout-s] n :count} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [m (ctx/mem c)
          food (animals/food-carried (:primitives c) mob)
          cands (candidates c)]
      (cond
        (>= (- now (:started m)) (* 1000 timeout-s)) (await (finish! c :timeout))
        (not (contains? animals/breeding-food mob)) (await (finish! c :unknown-mob))
        (bees-indoors? c) (await (finish! c :bees-indoors))
        (>= (count (:fed m)) n) (await (finish! c :fed))
        (nil? food) (await (finish! c :no-food))
        (or (empty? cands) (and (empty? (:fed m)) (< (count (animals/adults (:primitives c) mob (:radius (:args c)))) 2))) (await (finish! c (out-reason c)))
        :else (await (engage! c (first cands) food))))))
