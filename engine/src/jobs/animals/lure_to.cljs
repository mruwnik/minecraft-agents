(ns jobs.animals.lure-to
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.animals.herd-run :as hr]
            [jobs.animals.herd-trip :as ht]
            [jobs.animals.lead-to :as lead-to]
            [jobs.lib.animals :as animals]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]))

(def doc
  "Walk to :pos with one animal of type :mob following the breeding food held in hand (wheat for cows, sheep and goats,
  carrots for pigs, seeds for chickens; jobs.lib.animals/breeding-food), then put the food away. No lead is needed.
  A one-shot order that starts and ends itself. Declines (waits) with :no-mob or :no-destination until both are given;
  a started job always passes.

  A body that holds none of the food first gets it (a jobs.items.obtain child, unless :fetch is false); when none can
  be got the job ends :no-food. It picks the nearest adult within :radius that is on nobody's lead (the zone rules let
  it), holds the food, walks within 6 blocks of it and then to :pos in short go-to legs, the same legs as
  jobs.animals.lead-to (doors left open for the animal and shut afterwards). After each leg the animal is looked at:
  unseen or more than 7 blocks behind, the body holds still (:waiting-for-animal) for it; unseen for 3 s :lost (an allowed adult within 10 blocks takes its place at once); more
  than 10 behind, or still waiting after 10 s, :lagging (animals follow food only within about 10 blocks). Each round
  the food is checked in hand and held again if not (a cut, another job's equip); none left ends :no-food.

  Ends with info lure-to.done and a warn lure-to.gave-up unless the reason is :arrived. Result {:reason :animal key
  :near bool (the animal is within 6 blocks of :pos) :at pos}. Reasons:
  - :arrived: the body stands within :range of :pos and the food is away. The animal follows only while the food is
    in hand, so it is wherever it had got to; :near says whether that is by the spot.
  - :none (no animal to lure within :radius), :no-food, :no-hold (the food would not stay in hand), :lost, :lagging,
    :unreachable (a walk gave up), :timeout (:timeout-s from the first round, not the food fetch).
  Every reason but :arrived ends the job :stopped with a :text.

  Zones: an animal standing in another owner's zone or claim is not lured (:ignore-zones? true skips the check).")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :pos {:doc "where to lure it {:x :y :z}" :spec ::a/pos :default nil}
   :range {:doc "how close to :pos counts as there" :spec (a/num-in 0 nil) :default 2}
   :radius {:doc "animals within this many blocks of where the job starts are lured" :spec (a/num-in 0 nil) :default 8}
   :watch-radius {:doc "how far from the body the lured animal is looked for" :spec (a/num-in 0 nil) :default 64}
   :timeout-s {:doc "seconds from the first round (not the food fetch) before the job gives up" :spec (a/num-in 0 nil) :default 180}
   :fetch {:doc "get the food when none is carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false ends :no-food" :spec fetch/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false :spec boolean?}})

(def near-spot "An animal this close (flat) to :pos counts as :near." 6)
(def lure-reach "The body walks this close to the animal before it leads the way." 6)
(def max-equips "Tries to get the food into the hand before the job ends :no-hold." 3)

(defn check [c]
  (let [{:keys [mob pos]} (:args c)]
    (cond
      (:started (ctx/mem c)) true
      (nil? mob) (ctx/wait c :no-mob)
      (nil? pos) (ctx/wait c :no-destination)
      :else true)))

(defn stop-text [c reason]
  (let [{:keys [mob radius]} (:args c)]
    (case reason
      :none (str "no " mob " seen within " radius " blocks to lure")
      :no-food (str "no food for " mob " carried and none could be got")
      (str "luring stopped: " (name reason)))))

(defn ^:async conclude!
  "Put the food away, emit the outcome, hand it to the parent and end the job."
  [c reason]
  (when (hr/holds-food? c) (await (ctx/act c :unequip #js {})))
  (let [m (ctx/mem c)
        a (lead-to/animal-now c)
        ok? (= :arrived reason)
        result {:reason reason :animal (:animal m) :at (u/self-pos c)
                :near (boolean (and a (<= (lead-to/flat-dist (u/pos-of (.-pos a)) (:pos (:args c))) near-spot)))}]
    (ctx/emit! c :lure-to.done :info (assoc result :text (str "lure-to done: " (name reason))))
    (when-not ok?
      (ctx/emit! c :lure-to.gave-up :warn {:reason reason :text (stop-text c reason)}))
    (ctx/result! c (cond-> result (not ok?) (assoc :status :stopped :text (stop-text c reason))))
    :done))

(defn finish! [c reason]
  (ctx/update-mem! c assoc :ending reason)
  :again)

(defn candidates
  "The adults within radius of the start, on nobody's lead, the zone rules allowing, nearest first."
  [c]
  (let [{:keys [mob radius]} (:args c)
        origin (:origin (ctx/mem c))]
    (->> (animals/adults (:primitives c) mob (+ radius (lead-to/flat-dist origin (u/self-pos c))))
         (filterv #(and (not (animals/leashed? %)) (<= (lead-to/flat-dist origin (u/pos-of (.-pos %))) radius)))
         (sort-by #(lead-to/flat-dist (u/self-pos c) (u/pos-of (.-pos %))))
         vec
         (animals/allowed c :lure-to.declined "lure-to" :take))))

(defn successor
  "Another adult the zone rules allow, within follow-reach of the body: it follows the food as well as the lost one."
  [c]
  (let [me (u/self-pos c)]
    (->> (candidates c)
         (filterv #(<= (lead-to/flat-dist me (u/pos-of (.-pos %))) lead-to/follow-reach))
         first)))

(defn ^:async get-food!
  "No food carried: fetch it when :fetch allows, else (or when none came) end :no-food. :again once it is carried."
  [c]
  (if-not (fetch/opts c 'jobs.animals.lure-to)
    (finish! c :no-food)
    (do (await (fetch/fetch-untimed! c (quote jobs.animals.lure-to) ht/lure-problem))
        (if (animals/food-carried (:primitives c) (:mob (:args c))) :again (finish! c :no-food)))))

(defn ^:async start!
  "Hold the food, walk up to the nearest animal and send the walk on its way."
  [c]
  (let [{:keys [mob]} (:args c)
        p (:primitives c)
        food (animals/food-carried p mob)
        cands (candidates c)
        target (first cands)
        tries (:equip-tries (ctx/mem c) 0)]
    (cond
      (nil? food) (await (get-food! c))
      (empty? cands) (finish! c :none)
      (not= food (.-held (.self p))) (if (>= tries max-equips)
                                       (finish! c :no-hold)
                                       (do (ctx/update-mem! c assoc :equip-tries (inc tries))
                                           (await (ctx/act c :equip (clj->js {:item food})))
                                           :again))
      :else (let [k (animals/key-of target)
                  walked (await (animals/walk! c k target lure-reach {:doors :shut}))]
              (if (= :there walked)
                (do (ctx/update-mem! c assoc :animal k :phase :walk) :again)
                :again)))))

(defn ^:async rehold!
  "The food is not in the hand (a cut, another job's equip): hold it again, or end :no-food when none is carried, or
  :no-hold after max-equips tries in a row."
  [c]
  (let [p (:primitives c)
        food (animals/food-carried p (:mob (:args c)))
        tries (:rehold-tries (ctx/mem c) 0)]
    (cond
      (nil? food) (finish! c :no-food)
      (>= tries max-equips) (finish! c :no-hold)
      :else (do (ctx/update-mem! c assoc :rehold-tries (inc tries))
                (await (ctx/act c :equip (clj->js {:item food})))
                :again))))

(defn ^:async walk-legs! [c]
  (let [r (await (lead-to/walk-legs! c :walk (:pos (:args c)) (:range (:args c)) nil nil lead-to/safe-gap))]
    (cond
      (= :waiting r) :yield
      (= :catching-up r) (await (lead-to/wait-for-animal! c))
      (= :arrived r) (finish! c :arrived)
      (= :failed r) (finish! c :unreachable)
      (and (= :lead-broke (:problem r)) (not (hr/holds-food? c))) :again
      :else (finish! c (:problem r)))))

(defn ^:async walk! [c]
  (if-not (hr/holds-food? c)
    (await (rehold! c))
    (do (ctx/update-mem! c dissoc :rehold-tries)
        (await (walk-legs! c)))))

(defn ^:async step [c]
  (let [now (ctx/now c)
        {:keys [timeout-s]} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [{:keys [phase started]} (ctx/mem c)
          a (when (:animal (ctx/mem c)) (lead-to/animal-now c))]
      (when a (ctx/update-mem! c dissoc :unseen-since))
      (cond
        (:ending (ctx/mem c)) (await (lead-to/close-up! c conclude!))
        (>= (- now started) (* 1000 timeout-s)) (finish! c :timeout)
        (nil? phase) (do (ctx/update-mem! c assoc :method :lure :origin (u/self-pos c) :phase :start) :again)
        (= :start phase) (await (u/untimed! c #(start! c)))
        (nil? a) (if-let [next-one (successor c)]
                   (do (ctx/update-mem! c #(-> % (assoc :animal (animals/key-of next-one)) (dissoc :unseen-since))) :again)
                   (if (= :lost (lead-to/escort-problem c lead-to/safe-gap))
                     (finish! c :lost)
                     (await (lead-to/wait-for-animal! c))))
        :else (do (await (watch/watch! c {})) (await (walk! c)))))))

(defn ^:async round
  "The whole attempt: steps until the job ends; yields only while a child waits on the world."
  [c]
  (loop []
    (let [r (await (step c))]
      (case r
        :again (recur)
        :yield :continue
        r))))
