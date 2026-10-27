(ns jobs.animals.lure-to
  (:require [jobs.lib.checks :as checks]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [jobs.lib.pen-read :as pen-read]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.notes :as notes]
            [engine.settings :as settings]
            [jobs.lib.bait :as bait]
            [jobs.lib.escort :as escort]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.animals :as animals]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.pen-gate :as pg]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [jobs.lib.vantage :as vantage]
            [jobs.lib.watch :as watch]))

(def doc
  "Walk to :pos with one animal of type :mob following the breeding food held in hand (wheat for cows, sheep and goats,
  carrots for pigs, seeds for chickens; jobs.lib.animals/breeding-food), then put the food away. No lead is needed.
  A one-shot order that starts and ends itself. Each step it keeps a :gate-held entry fresh for every planned gate seen open within 10 blocks of :pos, so the pen-gate trigger does not shut the gate it leads the animal through; at the end (not with :keep-food) it shuts those gates itself. Without :mob or :pos it can never start (impossible :no-mob, :no-destination).

  A body that holds none of the food first gets it (a jobs.items.obtain child, unless the ways are only #{:carried}); when none can
  be got the job ends :no-food with the fetch's ending as its :because. It picks the adult within :radius that is on nobody's lead and not already in :box (the zone rules let
  it) whose trip costs least (go-to's quote of the walk to it plus the least walk from it to :pos, jobs.lib.escort/pick-animal!), holds the food, walks within 6 blocks of it and then to :pos in short go-to legs, the same legs as
  jobs.animals.lead-to (doors left open for the animal and shut afterwards); the go-to child of a leg plans its walk again after the leg was planned here). After each leg the animal is looked at:
  unseen or more than 5 blocks behind (jobs.lib.escort/::lure-gap), the body holds still (:waiting-for-animal) for it and the first look of the wait emits info
  escort.waiting (both positions, the distance); unseen for 3 s the body walks back toward where it was last seen (up to
  20 blocks) then looks round once from there (a small animal hides behind grass or an edge) and only then ends :lost; a third walk back to the same spot (::max-backs of escort) is not made, the animal counts as lost (an allowed adult within 10 blocks takes its place at once); seen beyond the gap and not
  coming nearer for 3 s the body turns back toward it (a cow loses the food past about 10 blocks); after
  ::max-turns such turns, or 10 s without progress (::stall-s), :animal-stalled. An animal that keeps closing in is waited for
  however far it fell behind. The plan of each leg is searched before the body walks: a search still running sends
  the body the whole way in one go-to walk only after escort's ::max-search-rounds. With no animal in sight at the start it walks to where the chosen one was last seen
  (up to ::sighting-age-s old) or, with none, to the nearest animal of the kind noted within ::note-age-s, looks round once and steps toward a heard animal of the kind (jobs.lib.vantage) before
  ending :no-animal; a sound is a direction, never a place. Each round
  the food is checked in hand and held again if not (a cut, another job's equip); none left ends :no-food.

  Result {:reason :end (:led when arrived) :animal key
  :near bool (the animal is within 6 blocks of :pos) :at pos (the body) :animal-at pos (the animal, nil unseen) :in-box bool (with :box)
  :lost / :stalled detail for those reasons}. Reasons:
  - :arrived: the body stands within ::arrive-range of :pos and the animal is seen in :box (none: within ::arrive-range + ::arrive-slack blocks of :pos, in the fenced pen :pos is in, if any) and not in the cell of a planned gate, else the
    body holds still for it up to 10 s and the end is :animal-outside (seen elsewhere) or :lost (not seen). The food is then put away (another carried item in hand when no slot is free); :near says whether the animal is by the spot.
  A :pos inside the destination is walked to exactly (range 0, a fence being see-through), after a step into the destination's gateway when the body is outside it, so the body goes in by the gate.
  A body that stands in the destination (:box, or the fenced pen around :pos) with the animal outside it first walks out of the pen's fence gate to the cell two past it, the animal following, and then in again (the animal follows food over the ground, not across a fence). An animal waiting at the shut gate counts as outside: the body goes out and the legs open the gate on the way in.
  - :animal-outside: the body is at :pos but the animal is not in the destination; the result gives :distance (animal to :pos).
  Every ending the job concludes itself gives :distance: the animal's, or its last sighting's when it is unseen (the engine's limit ending carries no data).
  - :no-animal (no animal to lure within :radius), :no-food, :no-hold (the food would not stay in hand), :lost, :animal-stalled (result :stalled {:animal :body :distance}),
    :unreachable (a walk gave up).
  Endings: :arrived is done; :no-food is condition-failed; the engine's limit (the call's :spend :job-time, default
  ::job-time-s) is limit-reached; the rest are nothing-possible with the reason as the cause. Every reason but :arrived carries a :text.
  However it ends, its cleanup puts the food away (not with :keep-food) and shuts the gates the legs left open.

  Zones: an animal standing in another owner's zone or claim is not lured (:ignore-zones? true skips the check).")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :pos {:doc "where to lure it {:x :y :z}" :spec ::a/pos :default nil}
   :box {:doc "the destination pen {:min :max} (feet cells, inclusive): animals in it are not lured, and the job arrives only when the led animal stands in it; none: the animal within 6 blocks of :pos" :spec pen-read/box-arg :default nil}
   :radius {:doc "animals within this many blocks (flat, height ignored) of where the job starts are lured" :spec (a/num-in 0 nil) :default 8}
   :uuid {:doc "lead only this animal (its uuid or the start of it, or its entity id, as the entities tool shows); nothing else takes its place. An empty or ambiguous start (more than one animal matches) never starts; one already at the destination ends the job at once (done? :ok)" :spec string? :default nil}
   :keep-food {:doc "leave the food in hand when the lure ends, for a parent that goes on with the animal (jobs.animals.herd); the gates the legs left open are still shut" :spec boolean? :default false}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 300 :doc "The call's :spend :job-time when it gives none: seconds of the job's own working time before the lure gives up." :spec (a/num-in 0 nil)}
   ::note-age-s {:default 600 :doc "Seconds a note of an animal of the kind seen within :radius of the start stays worth walking to when none is in sight." :spec (a/num-in 0 nil)}
   ::sighting-age-s {:default 30 :doc "Seconds a chosen animal's last sighting stays worth walking back to while the animal is out of sight." :spec (a/num-in 0 nil)}
   ::arrive-range {:default 2 :doc "How close to :pos counts as there." :spec (a/num-in 0 nil)}
   ::near-spot {:default 6 :doc "An animal this close (flat) to :pos counts as :near." :spec (a/num-in 0 nil)}
   ::arrive-slack {:default 3 :doc "Without :box the animal arrives when it is within ::arrive-range plus this many blocks (flat) of :pos: it follows the food a few blocks behind." :spec (a/num-in 0 nil)}
   ::gate-hold-range {:default 10 :doc "Planned gates this near :pos are held open while the lure runs, in blocks." :spec (a/num-in 0 nil)}
   ::ground-search {:default 16 :doc "Blocks above and below :pos looked over for the ground of its column when the unreachable text names it." :spec (a/int-in 0 nil)}})
(defn note-age-ms [] (* 1000 (settings/get settings ::note-age-s)))

(defn sighting-age-ms [] (* 1000 (settings/get settings ::sighting-age-s)))
(defn near-spot [] (settings/get settings ::near-spot))
(defn arrive-slack [] (settings/get settings ::arrive-slack))
(defn arrive-range [] (settings/get settings ::arrive-range))
(defn gate-hold-range [] (settings/get settings ::gate-hold-range))
(defn ground-search [] (settings/get settings ::ground-search))

(def ways need/fetch-ways)

(def target [:mob :pos])

(def safe? (price/staying :work (ready/safe-at-or-roaming
            (fn [c] (work-place/nearest-mob c (remove nil? [(:mob (:args c))]) (or (:radius (:args c)) 8))))))
(def fit? "Survival mode only: a passive animal cannot hurt the body, so a weak body still brings one to food." checks/survival-mode)


(def defaults {:spend {:job-time ::job-time-s}})

(defn can-start?
  "Never without :mob or a destination, or with an empty :uuid start (it would match every animal)."
  [c]
  (let [{:keys [mob pos uuid]} (:args c)]
    (cond
      (nil? mob) (contract/never :no-mob "lure-to needs a :mob")
      (nil? pos) (contract/never :no-destination "lure-to needs a :pos")
      (and uuid (str/blank? uuid)) (contract/never :empty-uuid "the :uuid start is empty: it would match every animal")
      :else (contract/ok :order "the order is complete"))))

(defn uuid-matches
  "The keys of the animals within the watch-radius setting (escort, 64) whose key starts with the :uuid argument."
  [c]
  (let [{:keys [mob uuid]} (:args c)]
    (->> (animals/adults (:primitives c) mob (escort/watch-radius))
         (filter #(animals/named-by? % uuid))
         (map animals/key-of)
         distinct
         vec)))

(defn can-run?
  "Not while the :uuid start names more than one animal in sight (checked until an animal is chosen): the animals move."
  [c]
  (let [uuid (:uuid (:args c))
        matches (when (and uuid (nil? (:animal (ctx/mem c)))) (uuid-matches c))]
    (cond
      (nil? matches) (contract/ok :uuid "no uuid start to settle")
      (< 1 (count matches)) (contract/later :ambiguous-uuid (str "the :uuid start " (pr-str uuid) " matches " (count matches) " animals: " (str/join ", " matches)))
      :else (contract/ok :uuid "the :uuid start names at most one animal"))))

(defn quote-of [c]
  {:uses {:job-time [0 60 ##Inf]}})

(declare at-destination? walk-legs-in!)

(defn named-there
  "The animal the :uuid start names when it already stands at the destination before the lure chose one."
  [c]
  (let [{:keys [uuid mob]} (:args c)]
    (when (and uuid (not (str/blank? uuid)) (nil? (:animal (ctx/mem c))))
      (let [found (filterv #(animals/named-by? % uuid) (animals/adults (:primitives c) mob (escort/watch-radius)))]
        (when (and (= 1 (count found)) (at-destination? c (first found)))
          (first found))))))

(defn done?
  "Ok when the named animal already stands at the destination before the start; else later with whether the animal
  arrived: the run ends itself, after it put the food away."
  [c]
  (let [arrived? (= :arrived (:ending (ctx/mem c)))]
    (if-let [a (named-there c)]
      (contract/ok :arrived "the named animal is already at the destination" :units :animals :done 1 :left 0
                   :result {:animal (animals/key-of a) :end :led :animal-at (u/pos-of (.-pos a))})
      (contract/later :luring (if arrived? "the animal arrived" "the animal is on its way") :units :animals
                      :done (if arrived? 1 0) :left (if arrived? 0 1)
                      :result (cond-> {:animal (:animal (ctx/mem c))} arrived? (assoc :end :led))))))

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

(defn ground-note
  "Names a :pos with no standable cell in its column cell (and the ground nearest it), else nil."
  [c]
  (let [{:keys [x y z]} (:pos (:args c))
        stand? #(reach/standable-cell? (:primitives c) {:x x :y % :z z} (reach/lookup (:primitives c)))]
    (when-not (stand? y)
      (let [ground (some #(when (stand? %) %) (interleave (range (dec y) (- y (ground-search)) -1) (range (inc y) (+ y (ground-search)))))]
        (str " (no standable cell at y" y (if ground (str "; ground at y" ground) (str " within " (ground-search) " blocks")) ")")))))

(defn lost-detail
  "Why :lost, for the next live look: the tracked animal's key, where it was last seen and how far that is from the body,
  and the nearest adult of the kind the sensing shows now (key, flat distance, leashed)."
  [c]
  (let [me (u/self-pos c)
        {:keys [animal last-seen]} (ctx/mem c)
        near (first (sort-by #(u/flat-dist me (u/pos-of (.-pos %))) (animals/adults (:primitives c) (:mob (:args c)) (escort/watch-radius))))]
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
        result {:reason reason :animal (:animal m) :at (u/self-pos c) :end (escort/end-of reason)
                :animal-at (some-> a .-pos u/pos-of)
                :in-box (when-let [box (pen-read/arg-box c)] (boolean (and a (animals/in-box? box (u/pos-of (.-pos a))))))
                :inside (when-let [box (pen-read/arg-box c)]
                          (count (filter #(animals/in-box? box (u/pos-of (.-pos %))) (animals/herd (:primitives c) (:mob (:args c)) (escort/watch-radius)))))
                :distance (some-> (or (some-> a .-pos u/pos-of) (:last-seen m)) (u/flat-dist (:pos (:args c))))
                :near (boolean (and a (<= (u/flat-dist (u/pos-of (.-pos a)) (:pos (:args c))) (near-spot))))}]
    (let [result (cond-> result
                   (= :lost reason) (assoc :lost (lost-detail c))
                   (= :animal-stalled reason) (assoc :stalled (stalled-detail c)))]
      (let [[kind cause] (get ending-of reason [:nothing-possible reason])
            ended (ctx/end c kind cause (assoc result :text (if ok? (str "lure-to done: " (name reason)) (stop-text c reason))))]
        (contract/with-because ended (:food-ending m))))))

(defn finish! [c reason]
  (ctx/update-mem! c assoc :ending reason)
  :again)

(defn in-doorway?
  "True when pos stands in a planned gate's cell: an animal there keeps the gate from shutting and is not yet inside."
  [c {:keys [x y z]}]
  (contains? (pg/gate-index (:world (:engine c))) [(js/Math.floor x) (js/Math.floor (+ y 0.01)) (js/Math.floor z)]))

(defn animal-cell [a] ((juxt :x :y :z) (animals/cell-of a)))

(defn read-destination
  "The feet cells ([x y z] set) the animal is to end up in: the :box, else the fenced enclosure around :pos; nil when :pos is in the open."
  [c]
  (if-let [{:keys [min max]} (pen-read/arg-box c)]
    (set (for [x (range (:x min) (inc (:x max))) y (range (:y min) (inc (:y max))) z (range (:z min) (inc (:z max)))] [x y z]))
    (spare/enclosure c (u/cell-vec (:pos (:args c))))))

(defn destination-cells
  "read-destination, read once per run (the first step keeps it in memory)."
  [c]
  (let [m (ctx/mem c)]
    (if (contains? m :dest-cells) (:dest-cells m) (read-destination c))))

(defn at-destination?
  "True when the animal stands in :box and not falling (a chicken over a dug-out floor is not yet inside), else within arrive-range plus arrive-slack of :pos (and in the fenced enclosure :pos is in, when it is in one), and not in a gate's cell."
  [c a]
  (let [{:keys [pos]} (:args c)
        box (pen-read/arg-box c)
        there (u/pos-of (.-pos a))]
    (and (not (in-doorway? c there))
         (if box
           (and (animals/in-box? box there) (not (false? (.-onGround a))))
           (and (<= (u/flat-dist there pos) (+ (arrive-range) (arrive-slack)))
                (if-let [pen (destination-cells c)] (contains? pen (animal-cell a)) true))))))

(defn free-adults
  "The adults within radius of the start, on nobody's lead, none already in :box. The animal sighted at the
  start stays one when it wandered past the radius while the food was fetched."
  [c]
  (let [{:keys [mob radius uuid]} (:args c)
        {:keys [origin sighting]} (ctx/mem c)
        pens (atom #{})
        in-reach? (fn [e] (or (= (:key sighting) (animals/key-of e))
                              (<= (u/flat-dist origin (u/pos-of (.-pos e))) radius)))
        free? (fn [e] (and (not (animals/leashed? e)) (in-reach? e)
                           (not (and (pen-read/arg-box c) (animals/in-box? (pen-read/arg-box c) (u/pos-of (.-pos e)))))
                           (not (spare/enclosed? c pens e))))]
    (->> (if uuid
           (filterv #(and (animals/named-by? % uuid) (not (animals/leashed? %))) (animals/adults (:primitives c) mob (escort/watch-radius)))
           (filterv free? (animals/adults (:primitives c) mob (+ radius (if sighting radius 0) (u/flat-dist origin (u/self-pos c)))))))))

(defn candidates
  "free-adults the zone rules allow."
  [c]
  (animals/allowed c :lure-to.declined "lure-to" :take (free-adults c)))

(defn successor
  "Another adult the zone rules allow, within follow-reach of the body: it follows the food as well as the lost one."
  [c]
  (let [me (u/self-pos c)]
    (->> (when-not (:uuid (:args c)) (candidates c))
         (filterv #(<= (u/flat-dist me (u/pos-of (.-pos %))) (escort/follow-reach)))
         first)))

(defn ^:async get-food!
  "No food carried: fetch it when the call's ways allow, else (or when none came) end :no-food. :again once it is carried."
  [c]
  (let [e (await (bait/food! c 1))]
    (cond
      (= :continue e) :yield
      (= :ok e) :again
      :else (do (ctx/update-mem! c assoc :food-ending e)
                (finish! c :no-food)))))

(defn noted-sighting
  "The nearest recent :seen note of an animal of :mob within :radius of the start not yet walked to, as a sighting."
  [c]
  (let [{:keys [mob radius]} (:args c)
        {:keys [origin noted-tried]} (ctx/mem c)
        now (ctx/now c)
        me (u/self-pos c)
        at (fn [n] (let [[x y z] (:pos n)] {:x x :y y :z z}))]
    (when-let [n (->> (notes/notes c)
                      (filter #(and (= :seen (:kind %)) (= mob (:what %)) (:id %) (not (contains? (set noted-tried) (:id %)))
                                    (< (- now (:t %)) (note-age-ms)) (<= (u/flat-dist origin (at %)) radius)))
                      (sort-by #(u/flat-dist me (at %)))
                      first)]
      {:key (:id n) :pos (at n) :at now :note-id (:id n)})))

(defn ^:async look-for!
  "No animal to lure in sight: walk to where the chosen one was seen when that is fresh (::sighting-age-s), else to the nearest animal of the kind another search or look noted within ::note-age-s, else look round
  once, then step toward a sound of the kind (jobs.lib.vantage; a heard animal gives a direction, never a place).
  :again or :yield after trying; nil when nothing is left to try."
  [c]
  (let [{:keys [mob radius uuid]} (:args c)
        {:keys [sighting looked]} (ctx/mem c)
        found? #(seq (candidates c))
        heard (when-not uuid (vantage/heard c [mob] radius))
        noted (when-not (or uuid sighting) (noted-sighting c))]
    (cond
      noted (do (ctx/update-mem! c #(-> % (assoc :sighting (dissoc noted :note-id)) (update :noted-tried (fnil conj []) (:note-id noted))))
                :again)
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
  "Hold the food, walk up to the animal whose trip costs least (escort/pick-animal!; the one chosen before while it is
  still a candidate) and send the walk on its way."
  [c]
  (let [{:keys [mob pos]} (:args c)
        p (:primitives c)
        food (animals/food-carried p mob)
        cands (candidates c)
        chosen (:key (:sighting (ctx/mem c)))
        target (or (some #(when (= chosen (animals/key-of %)) %) cands)
                   (await (escort/pick-animal! c cands 2 pos)))]
    (when target (ctx/update-mem! c assoc :sighting {:key (animals/key-of target) :pos (u/pos-of (.-pos target)) :at (ctx/now c)}))
    (cond
      (nil? food) (await (get-food! c))
      (empty? cands) (or (await (look-for! c)) (finish! c :no-animal))
      (not= food (bait/held c)) (if (= :no-hold (await (bait/hold! c food))) (finish! c :no-hold) :again)
      :else (let [k (animals/key-of target)
                  walked (await (animals/walk! c k target (escort/lure-reach) {:doors :shut}))]
              (if (= :there walked)
                (do (ctx/update-mem! c #(-> % (assoc :animal k :phase :walk :last-seen (u/pos-of (.-pos target))) (dissoc :sighting))) :again)
                :again)))))

(defn ^:async rehold!
  "The food is not in the hand (a cut, another job's equip): hold it again, or end :no-food when none is carried, or
  :no-hold after the shared fruitless tries in a row."
  [c]
  (let [p (:primitives c)
        food (animals/food-carried p (:mob (:args c)))]
    (cond
      (nil? food) (finish! c :no-food)
      :else (if (= :no-hold (await (bait/hold! c food))) (finish! c :no-hold) :again))))

(defn gates-of
  "The fence gates of the destination cells: [{:gate cell-map :past cell two past it, outside}], the past cell at the ground there (the gate may hang over a slope)."
  [c cells]
  (let [block-at (u/seen-block-at-fn (:primitives c))]
    (for [[x y z] cells [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
          :let [g {:x (+ x dx) :y y :z (+ z dz)}]
          :when (and (not (contains? cells [(:x g) y (:z g)])) (some-> (block-at g) .-name (str/ends-with? "_fence_gate")))]
      (let [wx (+ (:x g) (* 2 dx)) wz (+ (:z g) (* 2 dz))
            wy (or (some #(when (reach/standable-cell? (:primitives c) {:x wx :y % :z wz} (reach/lookup (:primitives c))) %) [y (dec y) (- y 2) (inc y)]) y)]
        {:gate {:x (+ (:x g) 0.5) :y y :z (+ (:z g) 0.5)} :past {:x (+ wx 0.5) :y wy :z (+ wz 0.5)}}))))

(defn gate-waypoint
  "The cell two past the destination's nearest fence gate (to the animal) when the body stands inside the destination and
  the animal outside it; else nil. The animal follows the food over the ground, so a gate on the far side is walked round
  to, not leaned across, and an animal stopped at the shut gate is let in by walking out and in again."
  [c a]
  (let [cells (destination-cells c)
        me (u/cell-vec (u/self-pos c))
        there (u/pos-of (.-pos a))]
    (when (and (seq cells) (contains? cells me) (not (contains? cells (animal-cell a))))
      (first (sort-by #(u/flat-dist there %) (map :past (gates-of c cells)))))))

(defn entry-gate
  "The cell of the destination's gate nearest the body when the body stands outside the destination; else nil. The body
  steps into the gateway first: from there the pen is in sight and the way in is short."
  [c]
  (let [cells (destination-cells c)
        me (u/self-pos c)]
    (when (and (seq cells) (not (contains? cells (u/cell-vec me))))
      (first (sort-by #(u/flat-dist me %) (map :gate (gates-of c cells)))))))

(defn ^:async arrive!
  "The body stands at :pos: the job ends :arrived once the led animal is at the destination; until then the body holds
  still for it (with :box and the animal stopped just outside, after one walk to the far side of the box), and ends :animal-outside (seen outside) or :lost (not seen) after max-wait-ms."
  [c]
  (let [a (escort/animal-now c)
        now (ctx/now c)
        since (or (:arrive-since (ctx/mem c)) now)]
    (cond
      (and a (at-destination? c a)) (finish! c :arrived)
      (and a (not (:gated (ctx/mem c))) (gate-waypoint c a))
      (do (ctx/update-mem! c #(-> % (assoc :via-gate (gate-waypoint c a)) (dissoc :arrive-since))) :again)
      (and a (pen-read/arg-box c) (not (:led-deeper (ctx/mem c))) (<= (u/flat-dist (u/self-pos c) (u/pos-of (.-pos a))) (escort/lead-deeper-gap)))
      (await (escort/lead-deeper! c (pen-read/arg-box c) (u/pos-of (.-pos a))))
      (> (- now since) (escort/max-wait-ms)) (finish! c (if a :animal-outside :lost))
      :else (do (ctx/update-mem! c assoc :arrive-since since)
                (await (escort/wait-for-animal! c))))))

(defn spot-in-destination?
  "True when :pos is a cell of the destination (box or fenced pen): the body must stand in it, not just within range of it across the fence."
  [c]
  (contains? (set (destination-cells c)) (u/cell-vec (:pos (:args c)))))

(defn ^:async walk-legs-to-pos! [c]
  (if-let [wp (when (and (spot-in-destination? c) (not (:entered (ctx/mem c)))) (entry-gate c))]
    (do (ctx/update-mem! c assoc :via-gate wp :via-flag :entered) :again)
    (await (walk-legs-in! c))))

(defn ^:async walk-legs-in! [c]
  (let [r (await (escort/walk-legs! c :walk (:pos (:args c)) (if (spot-in-destination? c) 0 (arrive-range)) (escort/lure-gap)))]
    (when-not (= :arrived r) (ctx/update-mem! c dissoc :arrive-since))
    (cond
      (= :waiting r) :yield
      (= :searching r) :again
      (= :catching-up r) (await (escort/wait-for-animal! c))
      (= :arrived r) (await (arrive! c))
      (= :failed r) (finish! c :unreachable)
      (and (= :lead-broke (:problem r)) (not (bait/holds-food? c))) :again
      :else (finish! c (:problem r)))))

(defn ^:async via-gate!
  "Walk out to the waypoint outside the gate with the animal following; then the legs to :pos go in through it."
  [c waypoint]
  (let [r (await (escort/walk-legs! c :gate waypoint (if (= :entered (:via-flag (ctx/mem c))) 0 1) (escort/lure-gap)))
        done #(ctx/update-mem! c (fn [m] (-> m (dissoc :via-gate :via-flag :arrive-since) (assoc (or (:via-flag m) :gated) true))))]
    (cond
      (= :waiting r) :yield
      (= :searching r) :again
      (= :catching-up r) (await (escort/wait-for-animal! c))
      (or (= :arrived r) (= :failed r)) (do (done) :again)
      (and (= :lead-broke (:problem r)) (not (bait/holds-food? c))) :again
      :else (finish! c (:problem r)))))

(defn ^:async walk-legs! [c]
  (let [{:keys [led-deeper via-gate]} (ctx/mem c)]
    (cond
      led-deeper (await (arrive! c))
      via-gate (await (via-gate! c via-gate))
      :else (await (walk-legs-to-pos! c)))))

(defn ^:async walk! [c]
  (if-not (bait/holds-food? c)
    (await (rehold! c))
    (do (child/progress! c :equip)
        (await (walk-legs! c)))))

(defn hold-gates!
  "Tell the pen-gate trigger the open planned gates near :pos are open on purpose (fresh :gate-held entries)."
  [c]
  (let [{:keys [x z]} (:pos (:args c))
        near (filter (fn [[gx _ gz]] (<= (js/Math.hypot (- gx x) (- gz z)) (gate-hold-range)))
                     (keys (pg/gate-index (:world (:engine c)))))]
    (doseq [cell (pg/seen-open-cells (:primitives c) near)]
      (pg/hold! c cell)
      (ctx/update-mem! c update :held-gates (fnil conj #{}) (vec cell)))))

(defn ^:async step [c]
  (hold-gates! c)
  (let [{:keys [phase]} (ctx/mem c)
        a (when (:animal (ctx/mem c)) (escort/animal-now c))]
    (when a (ctx/update-mem! c #(-> % (dissoc :unseen-since :looked-lost) (assoc :last-seen (u/pos-of (.-pos a))))))
    (cond
      (:ending (ctx/mem c)) (conclude! c (:ending (ctx/mem c)))
      (nil? phase) (do (ctx/update-mem! c assoc :method :lure :origin (u/self-pos c) :phase :start :dest-cells (read-destination c)) :again)
      (= :start phase) (await (start! c))
      (nil? a) (if-let [next-one (successor c)]
                 (do (ctx/update-mem! c #(-> % (assoc :animal (animals/key-of next-one)) (dissoc :unseen-since))) :again)
                 (if (= :lost (escort/escort-problem c (escort/lure-gap)))
                   (if (await (escort/lost-after-look? c)) (finish! c :lost) :again)
                   (await (escort/wait-for-animal! c))))
      :else (do (await (watch/watch! c {})) (await (walk! c))))))

(defn ^:async round
  "The whole attempt: steps until the job ends; yields only while a child waits on the world. One comfort section
  (ctx/atomic :lure): a chore reflex does not cut in and leave the animal standing, up to its cap."
  [c]
  (animals/reserve-bait! c (:mob (:args c)))
  (await (ctx/atomic c :comfort :lure
                     #(pace/steps! c (fn ^:async s [] (let [r (await (step c))] (if (= :yield r) :continue r)))))))

(defn ^:async shut-held!
  "Without :keep-food, the plans whose gates were held open during the run (a gate opened by hand included) are
  shut by jobs.maintenance.shut children, so the animal does not walk out behind the body. :yield while one waits, else nil."
  [c]
  (let [held (:held-gates (ctx/mem c))
        index (when (and (seq held) (not (:keep-food (:args c)))) (pg/gate-index (:world (:engine c))))]
    (loop [plans (sort (distinct (keep index held)))]
      (when-let [id (first plans)]
        (let [e (await (ctx/call c (keyword (str "shut-held-" id)) 'jobs.maintenance.shut {:plan id}))]
          (if (= :continue e) :yield (recur (rest plans))))))))

(defn ^:async cleanup
  "However the lure ended: the food is put away unless :keep-food, the gates held open are shut (not with :keep-food),
  then the gates the legs left open are shut (escort/close-up!). Done."
  [c _]
  (when-not (:keep-food (:args c)) (await (bait/put-food-away! c)))
  (let [r (or (await (shut-held! c))
              (await (escort/close-up! c (fn [c _] (ctx/end c :done nil)))))]
    (if (= :yield r) :continue r)))
