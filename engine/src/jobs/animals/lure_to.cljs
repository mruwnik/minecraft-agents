(ns jobs.animals.lure-to
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.animals.herd-run :as hr]
            [jobs.animals.escort :as escort]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.animals :as animals]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.pen-gate :as pg]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [jobs.lib.vantage :as vantage]
            [jobs.lib.watch :as watch]))

(def doc
  "Walk to :pos with one animal of type :mob following the breeding food held in hand (wheat for cows, sheep and goats,
  carrots for pigs, seeds for chickens; jobs.lib.animals/breeding-food), then put the food away. No lead is needed.
  A one-shot order that starts and ends itself. Each step it keeps a :gate-held entry fresh for every planned gate seen open within 10 blocks of :pos, so the pen-gate trigger does not shut the gate it leads the animal through. Without :mob or :pos it can never start (impossible :no-mob, :no-destination).

  A body that holds none of the food first gets it (a jobs.items.obtain child, unless :fetch is false); when none can
  be got the job ends :no-food with the fetch's kind and cause as :why. It picks the nearest adult within :radius that is on nobody's lead and not already in :box (the zone rules let
  it), holds the food, walks within 6 blocks of it and then to :pos in short go-to legs, the same legs as
  jobs.animals.lead-to (doors left open for the animal and shut afterwards); the go-to child of a leg plans its walk again after the leg was planned here). After each leg the animal is looked at:
  unseen or more than 7 blocks behind, the body holds still (:waiting-for-animal) for it and the first look of the wait emits info
  escort.waiting (both positions, the distance); unseen for 3 s the body walks back toward where it was last seen (up to
  12 blocks) and then ends :lost (an allowed adult within 10 blocks takes its place at once); seen more than 7 behind and not
  coming nearer for 10 s (jobs.animals.escort/::stall-s), :animal-stalled. An animal that keeps closing in is waited for
  however far it fell behind. The plan of each leg is searched before the body walks: a search still running never sends
  the body the whole way in one go. With no animal in sight at the start it walks to where the chosen one was last seen
  (up to ::sighting-age-s old), looks round once and steps toward a heard animal of the kind (jobs.lib.vantage) before
  ending :no-animal; a sound is a direction, never a place. Each round
  the food is checked in hand and held again if not (a cut, another job's equip); none left ends :no-food.

  Ends with a warn lure-to.gave-up unless the reason is :arrived. Result {:reason :animal key
  :near bool (the animal is within 6 blocks of :pos) :at pos (the body) :animal-at pos (the animal, nil unseen) :in-box bool (with :box)
  :lost / :stalled detail for those reasons}. Reasons:
  - :arrived: the body stands within :range of :pos and the animal is seen in :box (none: within :range + 3 blocks of :pos) and not in the cell of a planned gate, else the
    body holds still for it up to 10 s and the end is :animal-outside (seen elsewhere) or :lost (not seen). The food is then put away (another carried item in hand when no slot is free); :near says whether the animal is by the spot.
  - :animal-outside: the body is at :pos but the animal is not in the destination; the result gives :distance (animal to :pos).
  Every ending the job concludes itself gives :distance: the animal's, or its last sighting's when it is unseen (the engine's limit ending carries no data).
  - :no-animal (no animal to lure within :radius), :no-food, :no-hold (the food would not stay in hand), :lost, :animal-stalled (result :stalled {:animal :body :distance}),
    :unreachable (a walk gave up).
  Endings: :arrived is done; :no-food is condition-failed (the fetch's kind when it gave one); the engine's limit (the call's :spend :job-time, default
  ::timeout-s) is limit-reached; the rest are nothing-possible with the reason as the cause. Every reason but :arrived carries a :text.
  However it ends, its cleanup puts the food away and shuts the gates the legs left open.

  Zones: an animal standing in another owner's zone or claim is not lured (:ignore-zones? true skips the check).")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :pos {:doc "where to lure it {:x :y :z}" :spec ::a/pos :default nil}
   :box {:doc "the destination pen {:min :max} (feet cells, inclusive): animals in it are not lured, and the job arrives only when the led animal stands in it; none: the animal within 6 blocks of :pos" :spec a/box? :default nil}
   :range {:doc "how close to :pos counts as there" :spec (a/num-in 0 nil) :default 2}
   :radius {:doc "animals within this many blocks of where the job starts are lured" :spec (a/num-in 0 nil) :default 8}
   :watch-radius {:doc "how far from the body the lured animal is looked for" :spec (a/num-in 0 nil) :default 64}
   :fetch {:doc "get the food when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-food" :spec need/option? :default true}
   :uuid {:doc "lead only this animal (its uuid or the start of it, as the entities tool shows; the id when it has none); nothing else takes its place. An empty or ambiguous start (more than one animal matches) never starts; one already at the destination ends the job at once (done? :ok)" :spec string? :default nil}
   :include-penned? {:doc "also lure animals standing in a closed pen" :default false :spec boolean?}
   :ignore-zones? :shared})

(a/defargs settings
  {::timeout-s {:default 180 :doc "The call's :spend :job-time when it gives none: seconds before the lure gives up." :spec (a/num-in 0 nil)}
   ::sighting-age-s {:default 30 :doc "Seconds a chosen animal's last sighting stays worth walking back to while the animal is out of sight." :spec (a/num-in 0 nil)}})
(defn sighting-age-ms [] (* 1000 (settings/get settings ::sighting-age-s)))

(def near-spot "An animal this close (flat) to :pos counts as :near." 6)
(def arrive-slack "Without :box the animal arrives when it is within :range plus this many blocks (flat) of :pos: it follows the food a few blocks behind." 3)
(def lure-reach "The body walks this close to the animal before it leads the way." 6)
(def max-equips "Tries to get the food into the hand before the job ends :no-hold." 3)

(def target [:mob :pos])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::timeout-s}})

(defn can-start?
  "Never without :mob or a destination."
  [c]
  (let [{:keys [mob pos]} (:args c)]
    (cond
      (nil? mob) (contract/never :no-mob "lure-to needs a :mob")
      (nil? pos) (contract/never :no-destination "lure-to needs a :pos")
      :else (contract/ok :order "the order is complete"))))

(defn uuid-matches
  "The keys of the animals within :watch-radius whose key starts with the :uuid argument."
  [c]
  (let [{:keys [mob uuid watch-radius]} (:args c)]
    (->> (animals/adults (:primitives c) mob watch-radius)
         (map animals/key-of)
         (filter #(str/starts-with? (str %) uuid))
         distinct
         vec)))

(defn can-run?
  "Never while the :uuid start is empty or names more than one animal in sight (checked until an animal is chosen)."
  [c]
  (let [uuid (:uuid (:args c))
        matches (when (and uuid (nil? (:animal (ctx/mem c)))) (uuid-matches c))]
    (cond
      (nil? matches) (contract/ok :uuid "no uuid start to settle")
      (str/blank? uuid) (contract/never :empty-uuid "the :uuid start is empty: it would match every animal")
      (< 1 (count matches)) (contract/never :ambiguous-uuid (str "the :uuid start " (pr-str uuid) " matches " (count matches) " animals: " (str/join ", " matches)))
      :else (contract/ok :uuid "the :uuid start names at most one animal"))))

(defn quote-of [c]
  {:uses {:job-time [0 60 ##Inf]}})

(declare at-destination?)

(defn named-there
  "The animal the :uuid start names when it already stands at the destination before the lure chose one."
  [c]
  (let [{:keys [uuid mob watch-radius]} (:args c)]
    (when (and uuid (not (str/blank? uuid)) (nil? (:animal (ctx/mem c))))
      (let [found (filterv #(str/starts-with? (str (animals/key-of %)) uuid) (animals/adults (:primitives c) mob watch-radius))]
        (when (and (= 1 (count found)) (at-destination? c (first found)))
          (first found))))))

(defn done?
  "Ok when the named animal already stands at the destination before the start; else later with whether the animal
  arrived: the run ends itself, after it put the food away."
  [c]
  (let [arrived? (= :arrived (:ending (ctx/mem c)))]
    (if-let [a (named-there c)]
      (contract/ok :arrived "the named animal is already at the destination" :units :animals :done 1 :left 0
                   :result {:animal (animals/key-of a)})
      (contract/later :luring (if arrived? "the animal arrived" "the animal is on its way") :units :animals
                      :done (if arrived? 1 0) :left (if arrived? 0 1) :result {:animal (:animal (ctx/mem c))}))))

(defn skipped-text
  "What kept the animals seen within radius from being lured: babies, those on a lead, the zone rules."
  [c]
  (let [{:keys [mob radius]} (:args c)
        seen (animals/herd (:primitives c) mob radius)
        parts (cond-> []
                (some #(true? (.-baby %)) seen) (conj "babies are not lured")
                (some animals/leashed? seen) (conj "some are on a lead")
                (animals/refusal c) (conj (str "zone rules: " (name (animals/refusal c)))))]
    (when (seq parts) (str " (" (count seen) " seen: " (str/join ", " parts) ")"))))

(defn stalled-detail
  "Where the animal that stopped following and the body stand, and how far apart."
  [c]
  (let [me (u/self-pos c)
        animal (or (some-> (escort/animal-now c) .-pos u/pos-of) (:last-seen (ctx/mem c)))]
    {:animal animal :body me :distance (u/flat-dist animal me)}))

(def ground-search "Blocks above and below :pos looked over for the ground of its column." 16)

(defn ground-note
  "Names a :pos with no standable cell in its column cell (and the ground nearest it), else nil."
  [c]
  (let [{:keys [x y z]} (:pos (:args c))
        stand? #(reach/standable-cell? (:primitives c) {:x x :y % :z z})]
    (when-not (stand? y)
      (let [ground (some #(when (stand? %) %) (interleave (range (dec y) (- y ground-search) -1) (range (inc y) (+ y ground-search))))]
        (str " (no standable cell at y" y (if ground (str "; ground at y" ground) " within 16 blocks") ")")))))

(defn lost-detail
  "Why :lost, for the next live look: the tracked animal's key, where it was last seen and how far that is from the body,
  and the nearest adult of the kind the sensing shows now (key, flat distance, leashed)."
  [c]
  (let [me (u/self-pos c)
        {:keys [animal last-seen]} (ctx/mem c)
        near (first (sort-by #(u/flat-dist me (u/pos-of (.-pos %))) (animals/adults (:primitives c) (:mob (:args c)) (:watch-radius (:args c)))))]
    {:tracked animal
     :last-seen last-seen
     :last-seen-dist (some->> last-seen (u/flat-dist me))
     :nearest (when near {:key (animals/key-of near) :dist (u/flat-dist me (u/pos-of (.-pos near))) :leashed (animals/leashed? near)})}))

(defn stop-text [c reason]
  (let [{:keys [mob radius]} (:args c)]
    (case reason
      :no-animal (if-let [uuid (:uuid (:args c))]
              (str "the " mob " " uuid " is not in sight to lure")
              (str "no adult " mob " free to lure within " radius " blocks" (skipped-text c)))
      :animal-outside (str "the body is at the spot but the " mob " stands outside the destination, "
                           (some-> (escort/animal-now c) .-pos u/pos-of (u/flat-dist (:pos (:args c))) js/Math.round) " blocks from it")
      :no-food (str "no food for " mob " carried and none could be got")
      :lost (let [{:keys [last-seen last-seen-dist]} (lost-detail c)]
              (str "lost sight of the " mob (some->> (:uuid (:args c)) (str " "))
                   (when last-seen (str ", last seen at " (pr-str (select-keys last-seen [:x :z])) ", " (js/Math.round last-seen-dist) " blocks from the body"))))
      :animal-stalled (let [{:keys [animal body distance]} (stalled-detail c)]
                        (str "the " mob " stopped following at " (pr-str (select-keys animal [:x :z])) ", "
                             (js/Math.round distance) " blocks from the body at " (pr-str (select-keys body [:x :z]))))
      :unreachable (if (spare/enclosed? c (atom #{}) #js {:pos (clj->js (:pos (:args c)))})
                     "the spot is inside a closed pen; no way in found"
                     (str "luring stopped: unreachable" (ground-note c)))
      (str "luring stopped: " (name reason)))))

(def ending-of
  "The kind and cause of a lure that ended for reason, else nothing-possible with the reason as the cause."
  {:arrived [:done nil]
   :no-food [:condition-failed :no-food]})

(defn conclude!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [m (ctx/mem c)
        a (escort/animal-now c)
        ok? (= :arrived reason)
        result {:reason reason :animal (:animal m) :at (u/self-pos c)
                :animal-at (some-> a .-pos u/pos-of)
                :in-box (when-let [box (:box (:args c))] (boolean (and a (animals/in-box? box (u/pos-of (.-pos a))))))
                :distance (some-> (or (some-> a .-pos u/pos-of) (:last-seen m)) (u/flat-dist (:pos (:args c))))
                :near (boolean (and a (<= (u/flat-dist (u/pos-of (.-pos a)) (:pos (:args c))) near-spot)))}]
    (let [result (cond-> result
                   (= :lost reason) (assoc :lost (lost-detail c))
                   (= :animal-stalled reason) (assoc :stalled (stalled-detail c)))]
      (when-not ok?
        (ctx/emit! c :lure-to.gave-up :warn {:reason reason :text (stop-text c reason)}))
      (let [[kind cause] (get ending-of reason [:nothing-possible reason])
            food (when (= :no-food reason) (:food-ending m))
            [kind cause] (if food [(:kind food) {:reason :no-food :why (get-in food [:cause :why])}] [kind cause])]
        (ctx/end c kind cause (cond-> (assoc result :text (if ok? (str "lure-to done: " (name reason)) (stop-text c reason)))
                                food (assoc :why (:why cause))))))))

(defn finish! [c reason]
  (ctx/update-mem! c assoc :ending reason)
  :again)

(defn in-doorway?
  "True when pos stands in a planned gate's cell: an animal there keeps the gate from shutting and is not yet inside."
  [c {:keys [x y z]}]
  (contains? (pg/gate-index (:world (:engine c))) [(js/Math.floor x) (js/Math.floor (+ y 0.01)) (js/Math.floor z)]))

(defn at-destination?
  "True when the animal stands in :box, else within :range plus arrive-slack of :pos, and not in a gate's cell."
  [c a]
  (let [{:keys [box pos range]} (:args c)
        there (u/pos-of (.-pos a))]
    (and (not (in-doorway? c there))
         (if box
           (animals/in-box? box there)
           (<= (u/flat-dist there pos) (+ range arrive-slack))))))

(defn free-adults
  "The adults within radius of the start, on nobody's lead, none already in :box, nearest first. The animal sighted at the
  start stays one when it wandered past the radius while the food was fetched."
  [c]
  (let [{:keys [mob radius uuid watch-radius include-penned?]} (:args c)
        {:keys [origin sighting]} (ctx/mem c)
        pens (atom #{})
        in-reach? (fn [e] (or (= (:key sighting) (animals/key-of e))
                              (<= (u/flat-dist origin (u/pos-of (.-pos e))) radius)))
        free? (fn [e] (and (not (animals/leashed? e)) (in-reach? e)
                           (not (and (:box (:args c)) (animals/in-box? (:box (:args c)) (u/pos-of (.-pos e)))))
                           (or include-penned? (not (spare/enclosed? c pens e)))))]
    (->> (if uuid
           (filterv #(and (str/starts-with? (str (animals/key-of %)) uuid) (not (animals/leashed? %))) (animals/adults (:primitives c) mob watch-radius))
           (filterv free? (animals/adults (:primitives c) mob (+ radius (if sighting radius 0) (u/flat-dist origin (u/self-pos c))))))
         (sort-by #(u/flat-dist (u/self-pos c) (u/pos-of (.-pos %))))
         vec)))

(defn candidates
  "free-adults the zone rules allow."
  [c]
  (animals/allowed c :lure-to.declined "lure-to" :take (free-adults c)))

(defn successor
  "Another adult the zone rules allow, within follow-reach of the body: it follows the food as well as the lost one."
  [c]
  (let [me (u/self-pos c)]
    (->> (when-not (:uuid (:args c)) (candidates c))
         (filterv #(<= (u/flat-dist me (u/pos-of (.-pos %))) escort/follow-reach))
         first)))

(defn ^:async get-food!
  "No food carried: fetch it when :fetch allows, else (or when none came) end :no-food. :again once it is carried."
  [c]
  (if-not (need/opts c 'jobs.animals.lure-to)
    (finish! c :no-food)
    (let [limits (select-keys (need/parse-arg (:fetch (:args c))) [:how :explore :minutes])
          e (await (need/blocks! c (get animals/breeding-food (:mob (:args c))) 1 limits))]
      (cond
        (= :continue e) :yield
        (animals/food-carried (:primitives c) (:mob (:args c))) :again
        :else (do (ctx/update-mem! c assoc :food-ending e)
                  (finish! c :no-food))))))

(defn ^:async look-for!
  "No animal to lure in sight: walk to where the chosen one was seen when that is fresh (::sighting-age-s), else look round
  once, then step toward a sound of the kind (jobs.lib.vantage; a heard animal gives a direction, never a place).
  :again or :yield after trying; nil when nothing is left to try."
  [c]
  (let [{:keys [mob radius uuid]} (:args c)
        {:keys [sighting looked]} (ctx/mem c)
        found? #(seq (candidates c))
        heard (when-not uuid (vantage/heard c [mob] radius))]
    (cond
      (and sighting (< (- (ctx/now c) (:at sighting)) (sighting-age-ms)))
      (let [r (await (near/go-near! c (:pos sighting) 2 {:doors :shut}))]
        (if (= :partial r)
          :yield
          (do (ctx/update-mem! c dissoc :sighting)
              (await (look/look-around-until! c found? (look/search-effort)))
              :again)))
      (not looked) (do (ctx/update-mem! c assoc :looked true)
                       (await (look/look-around-until! c found? (look/search-effort)))
                       :again)
      (and (seq heard) (not (vantage/done? c))) (if (= :partial (await (vantage/step! c heard found?))) :yield :again)
      :else nil)))

(defn ^:async start!
  "Hold the food, walk up to the nearest animal and send the walk on its way."
  [c]
  (let [{:keys [mob]} (:args c)
        p (:primitives c)
        food (animals/food-carried p mob)
        cands (candidates c)
        target (first cands)
        tries (:equip-tries (ctx/mem c) 0)]
    (when target (ctx/update-mem! c assoc :sighting {:key (animals/key-of target) :pos (u/pos-of (.-pos target)) :at (ctx/now c)}))
    (cond
      (nil? food) (await (get-food! c))
      (empty? cands) (or (await (look-for! c)) (finish! c :no-animal))
      (not= food (.-held (.self p))) (if (>= tries max-equips)
                                       (finish! c :no-hold)
                                       (do (ctx/update-mem! c assoc :equip-tries (inc tries))
                                           (await (ctx/act c :equip (clj->js {:item food})))
                                           :again))
      :else (let [k (animals/key-of target)
                  walked (await (animals/walk! c k target lure-reach {:doors :shut}))]
              (if (= :there walked)
                (do (ctx/update-mem! c #(-> % (assoc :animal k :phase :walk :last-seen (u/pos-of (.-pos target))) (dissoc :sighting))) :again)
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

(defn ^:async arrive!
  "The body stands at :pos: the job ends :arrived once the led animal is at the destination; until then the body holds
  still for it (with :box and the animal stopped just outside, after one walk to the far side of the box), and ends :animal-outside (seen outside) or :lost (not seen) after max-wait-ms."
  [c]
  (let [a (escort/animal-now c)
        now (ctx/now c)
        since (or (:arrive-since (ctx/mem c)) now)]
    (cond
      (and a (at-destination? c a)) (finish! c :arrived)
      (and a (:box (:args c)) (not (:led-deeper (ctx/mem c))) (<= (u/flat-dist (u/self-pos c) (u/pos-of (.-pos a))) escort/lead-deeper-gap))
      (await (escort/lead-deeper! c (:box (:args c)) (u/pos-of (.-pos a))))
      (> (- now since) escort/max-wait-ms) (finish! c (if a :animal-outside :lost))
      :else (do (ctx/update-mem! c assoc :arrive-since since)
                (await (escort/wait-for-animal! c))))))

(defn ^:async walk-legs-to-pos! [c]
  (let [r (await (escort/walk-legs! c :walk (:pos (:args c)) (:range (:args c)) escort/safe-gap))]
    (cond
      (= :waiting r) :yield
      (= :searching r) :again
      (= :catching-up r) (await (escort/wait-for-animal! c))
      (= :arrived r) (await (arrive! c))
      (= :failed r) (finish! c :unreachable)
      (and (= :lead-broke (:problem r)) (not (hr/holds-food? c))) :again
      :else (finish! c (:problem r)))))

(defn ^:async walk-legs! [c]
  (if (:led-deeper (ctx/mem c))
    (await (arrive! c))
    (await (walk-legs-to-pos! c))))

(defn ^:async walk! [c]
  (if-not (hr/holds-food? c)
    (await (rehold! c))
    (do (ctx/update-mem! c dissoc :rehold-tries)
        (await (walk-legs! c)))))

(def gate-hold-range "Planned gates this near the spot are held while the lure runs." 10)

(defn hold-gates!
  "Tell the pen-gate trigger the open planned gates near :pos are open on purpose (fresh :gate-held entries)."
  [c]
  (let [{:keys [x z]} (:pos (:args c))
        near (filter (fn [[gx _ gz]] (<= (js/Math.hypot (- gx x) (- gz z)) gate-hold-range))
                     (keys (pg/gate-index (:world (:engine c)))))]
    (doseq [cell (pg/seen-open-cells (:primitives c) near)]
      (ctx/forget-where! c :gate-held #(= cell (some-> (:cell %) vec)))
      (ctx/remember! c :gate-held {:cell cell :by (:id c)} hr/held-policy))))

(defn ^:async step [c]
  (hold-gates! c)
  (let [{:keys [phase]} (ctx/mem c)
        a (when (:animal (ctx/mem c)) (escort/animal-now c))]
    (when a (ctx/update-mem! c #(-> % (dissoc :unseen-since) (assoc :last-seen (u/pos-of (.-pos a))))))
    (cond
      (:ending (ctx/mem c)) (conclude! c (:ending (ctx/mem c)))
      (nil? phase) (do (ctx/update-mem! c assoc :method :lure :origin (u/self-pos c) :phase :start) :again)
      (= :start phase) (await (u/untimed! c #(start! c)))
      (nil? a) (if-let [next-one (successor c)]
                 (do (ctx/update-mem! c #(-> % (assoc :animal (animals/key-of next-one)) (dissoc :unseen-since))) :again)
                 (if (= :lost (escort/escort-problem c escort/safe-gap))
                   (finish! c :lost)
                   (await (escort/wait-for-animal! c))))
      :else (do (await (watch/watch! c {})) (await (walk! c))))))

(defn ^:async round
  "The whole attempt: steps until the job ends; yields only while a child waits on the world."
  [c]
  (animals/reserve-bait! c (:mob (:args c)))
  (loop []
    (let [r (await (step c))]
      (case r
        :again (recur)
        :yield :continue
        r))))

(defn ^:async cleanup
  "However the lure ended: the food is put away, then the gates the legs left open are shut (escort/close-up!). Done."
  [c _]
  (await (hr/put-food-away! c))
  (let [r (await (escort/close-up! c (fn [c _] (ctx/end c :done nil))))]
    (if (= :yield r) :continue r)))
