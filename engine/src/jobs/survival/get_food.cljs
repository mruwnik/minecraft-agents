(ns jobs.survival.get-food
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.surface-up :as surface]
            [jobs.lib.foods :as foods]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.cost.food :as food-cost]
            [jobs.lib.reserve :as reserve]
            [jobs.survival.eat :as eat]
            [jobs.lib.food-animals :as animals]
            [jobs.survival.get-food.cook :as cook]
            [jobs.survival.get-food.report :as report]
            [jobs.survival.get-food.risk :as risk]
            [jobs.survival.get-food.sources :as sources]))

(def doc
  "Keep the body fed. Hungry means food below :food plus one per missing hp, at most 18 (the hungry trigger's test).
  One call is one whole attempt. It eats what is carried (the eat job), up to 18, or up to 20 while health is below
  :health; fed, it ends done {:food n}. Still hungry, it takes the first allowed way that has something to do, then eats again.
  The ways (call key :ways, default all):
  :bake  No food carried but 3 or more wheat: bake bread (the craft job, enough loaves for the hunger, within 32 blocks of
         a table). A failed bake is not retried in this job.
  :cook  Raw food carried (chicken, beef ...) with a furnace or smoker seen or remembered (the :furnace place) within cook's ::cook-far-radius (64), or no furnace but enough raw food
         to pay for making one (jobs.lib.cost/cook-worth?): cook it (the smelt job, which fetches a furnace and fuel, waiting out the cook in
         the round), then eat it. Done before eating unless a danger is sensed or food is 0 and health would not outlast the cook plus cook's ::cook-margin-hp. Not at night nor while the night
         handler holds. A failed cook is not retried in this job (jobs.survival.get-food.cook).
  :source  The newest :food-source entry {:pos :kind} within ::source-radius, or a chest it booked food in (memory :fetch/stock) within sources' ::stock-radius (256), reached by go-to:
         :farm: dig its mature crops (blocks.dig, which collects the drops). It does not replant.
         :chest: withdraw its best food (storage.withdraw); with no food but 3 or more wheat, wheat in multiples of 3,
         enough for the hunger, for :bake.
         :animals: hunt there (:hunt).
         A chest found empty or a source go-to cannot reach is forgotten; a farm with nothing ripe is passed over.
  :collect  Harvest items lying within 8 blocks (collect-drops).
  :hunt  The nearest passive food animal within ::hunt-radius (combat.hunt over the animal's kind, which collects its drops).
  :crops  Mature crops, sweet berry bushes or melons within ::hunt-radius (blocks.dig).
  :fish  Nothing left: with a fishing rod carried, once per job it fishes two (jobs.food.fish, which looks for a bank beside water within its ::bank-radius) and eats or cooks them.
  :pen   The surplus of its own pens: an animal in a zone or plan of its own in sight, else it walks (go-to) to the
         nearest zone or plan of its own within ::source-radius it has not visited in this job and culls there (animals.cull over its box, keeping
         the pair; the kill's rule is :own-pen). Its stock is its reserve: taken after the wild food in reach, before fishing and the search.
  :field Last: it walks to the nearest zone or plan of its own it has not visited in this job and cuts and replants the ripe wheat there
         (jobs.farm.harvest); the wheat is baked next (:bake).
  Then, once per job (:explore, default on; a starving body again after foods/::starving-rest-ms) it searches for food animals and crops (jobs.explore.search, the
  ::explore setting; not while the search would wait (dark sky, danger near)) and goes on if it found some (an animal only a note gave, with none in sight, is walked toward first; a hungry body under rock climbs out first, jobs.lib.surface-up); at ::eat-bad-food (6) or below it then eats harmful food carried (rotten
  flesh, with :allow-bad). Still nothing: it ends nothing-possible :no-food (the text says what was searched, the nearest known source) and writes
  :hungry, which widens the next search; the hungry trigger rests after that ending (its :rest-s).
  :eat false: only gathers, done when the pack holds :carry food points (jobs.survival.eat fetches this way; a job cannot call its own caller).
  :carry n (default the 3-day reserve, 60 points; 0: only feed): a fed body also takes the ways until the pack holds n food points, else ends nothing-possible :no-carry.
  A fed body (not hungry, nothing to top up with, no :carry short) is done at once.
  A go-to that waits on the world, or a child still going after jobs.lib.child's call cap, yields :continue; a reflex
  call (it never yields) waits it out in the round.
  More than 12 ways in one call without getting fed ends nothing-possible :still-hungry.
  Ready: safe? and fit? always ok: it eats carried food whatever is near. Its walks and searches run under :checks (job-safe-or-starving); a merely hungry body waits for dawn in the open (intended).
  Fragile (health at or below foods/::fragile-health, 6) with no food carried, it walks out (a search, a source, its pen,
  an animal, a furnace to cook at) only on a calm day (jobs.lib.dawn/calm?) and not past known hostiles whose route
  danger (jobs.lib.cost/route-danger) is its health or more; with nothing else to try it writes a :food-too-dangerous entry (no :hungry) and ends condition-failed
  :too-dangerous; the hungry trigger then rests (its :rest-s) while the body stays fragile
  (jobs.survival.get-food.risk). Not shut in a shelter, it first looks around from where it stands (once per place) so hostiles outside its view count.
  Wheat is cut only by :field, in a zone or plan of its own. Babies, animals in another's zone or claim, animals in a zone or plan of the body's own (but for the :pen way) or inside a closed enclosure (::spare-pens), kinds in ::off-limits, and a breeding pair (2 up to ::keep-adults (2) adults of a kind in sight; a lone adult is hunted; kept even when starving, unless ::kept-lifted-below lifts it) are never hunted (:ignore-zones? lifts the others' zone only). Each animal chosen is a food.killed info with its name, cell, adults counted, :keep, food and the rule that allowed it; the data carries :killed. A chest in another's zone or claim is skipped and never taken from, even when starving
  (one get-food.skipped warning). A crop in another's zone or claim is dug only as the last way, when no permitted crop, field or other way is left and the body is hungry with nothing edible carried (one
  get-food.trespass-last-resort warning). :ignore-zones? lifts both.
  A cut call starts again from the world: carried food first, then the ways.
  Memory: reads :food-source and :hungry. Writes :hungry {:food} (cap 10, one hour). Job memory (hints for
  this run): :no-bake, :no-cook, :tried-sources, :tried-fields, :skipped-blocks, :skipped-animals. Quote: F, a search.")

(a/defargs args
  {:food {:doc "hungry below this much food (of 20)" :spec (a/num-in 0 20) :default foods/default-food}
   :health {:doc "below this health eat up to a full bar" :spec (a/num-in 0 20) :default foods/default-health}
   :explore {:doc "with nothing known or seen, walk and look for food animals and crops once per job (a starving body again after foods/::starving-rest-ms; a hungry body under rock first climbs out; a starving one searches there): false, true (the ::explore setting) or a map of the search's options over it (:max-distance, :max-legs, :spend; not :radius)" :spec (a/or-of boolean? map?) :default true}
   :carry {:doc "carry food worth this many hunger points: a fed body takes the ways below until the pack holds it (0: only feed)" :spec (a/num-in 0 nil) :default food-cost/reserve-points}
   :eat {:doc "eat what is carried and what the ways bring: false only gathers (the way jobs.survival.eat fetches: it is the caller, and a job cannot call its own caller), done when the pack holds :carry food points" :spec boolean? :default true}
   :ignore-zones? :shared})

(def harvest-items
  "What the drops of a harvest or a kill may be called (a delay: the food table is read on first use, not at load)."
  (delay (into (set (filter foods/edible? (keys (foods/table)))) #{"wheat" "wheat_seeds" "beetroot_seeds" "poisonous_potato"})))

(def hungry-policy {:cap 10 :ttl (* 60 60 1000)})

(def widen-max
  "The search grows with each recent give-up (:hungry entry) up to this many times its size."
  3)

(defn widen
  "How many times the search radius is multiplied: 1 plus the :hungry give-ups within the policy ttl, at most widen-max."
  [c]
  (min widen-max (inc (ctx/count-in c :hungry (:ttl hungry-policy)))))

(a/defargs settings
  {::hunt-radius {:default 24 :doc "How far to look for animals and wild crops, in blocks (widened by recent give-ups)." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 1800 :doc "Seconds one attempt at getting fed may take: a hunt, a search, a cook." :spec (a/num-in 0 nil)}
   ::drop-radius {:default 8 :doc "Blocks around the body in which dropped food is picked up." :spec (a/int-in 0 nil)}
   ::explore {:default {:max-distance 64 :max-legs 12 :spend {:job-time 120}} :doc "jobs.explore.search options of the search for food (:explore true)." :spec map?}
   ::kept-lifted-below {:default -1 :doc "At this much food or less, with no food carried, the last adults of a kind (kept for breeding) are hunted too; -1 (default) never." :spec (a/int-in -1 20)}
   ::eat-bad-food {:default 6 :doc "At this much food or less the last resort eats harmful food carried (rotten flesh); above it harmful food is not food." :spec (a/num-in 0 20)}
   ::max-chases {:default 3 :doc "Chases of one animal in a call before it is passed over as tried: a missed chase is repeated, a fleeing rabbit is chased again." :spec (a/int-in 1 nil)}
   ::max-ways {:default 12 :doc "Ways one call tries without getting fed before it ends :still-hungry." :spec (a/int-in 0 nil)}})

(defn hunt-radius
  "How far the hunt and the crop way look: the widened ::hunt-radius, or as far as the search saw what it found (:found-reach), so a find at the edge is not out of reach."
  [c]
  (max (* (widen c) (settings/get settings ::hunt-radius)) (:found-reach (ctx/mem c) 0)))

(defn drop-radius [] (settings/get settings ::drop-radius))

(defn max-ways [] (settings/get settings ::max-ways))
(defn max-chases [] (settings/get settings ::max-chases))

(defn eat-bad-food [] (settings/get settings ::eat-bad-food))

(def food-level cook/food-level)

(defn hungry-now? [c]
  (let [self (.self (:primitives c))]
    (foods/hungry? (.-food self) (.-health self) (:args c))))

(defn tryable-food?
  "Food worth a round: any edible item, and harmful food (rotten flesh) only at or below the eat-bad line."
  [c item]
  (and (foods/food? (:name item))
       (or (not (contains? foods/harmful (:name item))) (<= (food-level c) (eat-bad-food)))))

(defn carry-short?
  "A :carry is asked for and the pack holds fewer food points than that."
  [c]
  (< (foods/stock-points (u/inventory (:primitives c))) (:carry (:args c))))

(defn eats? [c] (not (false? (:eat (:args c)))))

(defn wants-food? [c] (or (and (eats? c) (hungry-now? c)) (carry-short? c)))

(defn starving?
  "Food 0, or health at or below 6 and hungry: the rest after a fruitless search is only foods/::starving-rest-ms."
  [c]
  (foods/starving? (.self (:primitives c)) (:args c)))


;; ------------------------------------------------------------------ ways

(def ways [:bake :cook :source :collect :hunt :crops :fish :pen :field])

(def defaults {:checks {:safe? '(job-safe-or-starving) :fit? '(always)}
               :spend {:job-time ::job-time-s} :ways (set ways)})

(def meta {:category :reflex :tags [:food]})

(def safe? ready/ok)

(def fit? ready/ok)

;; ------------------------------------------------------------------ gathering and hunting

(defn ^:async collect-drops!
  "Pick up the harvest items within radius (collect-drops): :again when some came in, else nil."
  [c radius]
  (let [e (await (child/run!* c :collect 'jobs.items.collect-drops {:radius radius :items (vec @harvest-items)}))]
    (cond
      (= :continue e) :continue
      (pos? (:collected (:data e) 0)) :again)))

(defn animal-opts [c own?] {:radius (hunt-radius c) :lift (settings/get settings ::kept-lifted-below) :own? own?})

(defn hunt-left?
  "Whether an animal in sight is left to hunt (own?: in its own pens too)."
  [c own?]
  (some? (animals/nearest c (animal-opts c own?))))

(defn starved?
  "Food at the ::kept-lifted-below line or less with no food carried: the last adults of a kind may go too."
  [c]
  (let [self (.self (:primitives c))]
    (and (<= (.-food self) (settings/get settings ::kept-lifted-below)) (not (foods/carries-food? self)))))

(defn keep-for
  "Adults of the kind the kill leaves alive: the spare rule's keep, none when starved or when the animal is a lone adult
  (the hunt would end :keep at once with fewer than keep in range)."
  [c {:keys [adults]}]
  (if (or (starved? c) (< (or adults 0) (spare/keep-adults))) 0 (spare/keep-adults)))

(def meat-of
  "The raw meat each food animal drops: what a hunt of it is for."
  {"cow" "beef" "mooshroom" "beef" "pig" "porkchop" "sheep" "mutton" "chicken" "chicken" "rabbit" "rabbit"})

(defn kill-job
  "The real job and args that kill the food animal of target: animals.cull over the own pen it stands in (own?), else combat.hunt."
  [c own? {animal :entity :as target}]
  (let [common {:mob (.-name animal) :count 1 :keep (keep-for c target) :spend {:job-time 60}
                :checks {:safe? '(job-safe-or-starving) :fit? '(always)}}
        pos (u/pos-of (.-pos animal))
        box (when own? (sources/box-around c pos))]
    (if own?
      ['jobs.animals.cull (cond-> (assoc common :radius 8 :center (select-keys pos [:x :z]))
                            box (-> (dissoc :radius :center) (assoc :box {:min (zipmap [:x :y :z] (first box)) :max (zipmap [:x :y :z] (second box))})))]
      ['jobs.combat.hunt (-> common (dissoc :mob) (assoc :item (meat-of (.-name animal)) :radius (hunt-radius c) :ways #{:carried}))])))

(defn ^:async hunt!
  "Kill the nearest food animal with the real job (combat.hunt; own?: animals.cull over its own pen): :again, or nil when
  there is none (after a look around) or the walk to it is refused (risk). A hunt that gave up on the animal (it fled) is repeated, up to ::max-chases times."
  [c own?]
  (let [target (await (animals/nearest-or-look c (animal-opts c own?)))
        animal (:entity target)]
    (cond
      (= :continue target) :continue
      (nil? target) nil
      (risk/refused? c (u/pos-of (.-pos animal))) nil
      :else
      (let [[job args] (kill-job c own? target)
            e (await (child/run!* c :kill job args))]
        (if (= :continue e)
          :continue
          (let [id (.-id animal)
                tries (inc (get-in (ctx/mem c) [:chases id] 0))]
            (ctx/update-mem! c assoc-in [:chases id] tries)
            (when (or (not= :gave-up (:reason (:data e))) (>= tries (max-chases)))
              (ctx/update-mem! c update :skipped-animals (fnil conj []) id))
            (animals/note-kill! c target)
            :again))))))

;; ------------------------------------------------------------------ bread

(defn can-bake?
  "No edible food carried, 3 or more wheat, and no failed bake in this job."
  [c]
  (and (nil? (eat/carried-best c))
       (>= (sources/carried-count c "wheat") 3)
       (not (:no-bake (ctx/mem c)))))

(defn ^:async bake!
  "Way :bake: :again when bread was made, else nil with :no-bake remembered."
  [c]
  (let [loaves (min (sources/loaves-wanted c) (quot (sources/carried-count c "wheat") 3))
        e (await (child/run!* c :bake 'jobs.items.craft {:item "bread" :count (need/carry-count c :bake ["bread"] loaves) :ways #{:carried}}))]
    (cond
      (= :continue e) :continue
      (>= (:made (:data e) 0) 1) :again
      :else (let [r (when-not (= :done (:kind e)) (contract/cause-key e))
                  reason (if (or (nil? r) (= :need r)) "short" (name r))]
              (ctx/update-mem! c assoc :no-bake reason)
              nil))))

;; ------------------------------------------------------------------ start

(defn fed?
  "Neither hungry, nor topping up, nor short of :carry."
  [c]
  (let [self (.self (:primitives c))]
    (if-not (eats? c)
      (not (carry-short? c))
      (not (or (wants-food? c)
             (foods/top-up? (.-food self) (.-health self) (foods/carried-names self (reserve/spared (ctx/view c) self)))
             (foods/eat-now? self (:args c) (reserve/spared (ctx/view c) self)))))))

(def danger-policy {:cap 1 :ttl (* 60 60 1000)})

(def target [])

(def can-start? ready/ok)

(defn done?
  "Fed (fed?); the food points still wanted."
  [c]
  (checks/goal (fed? c) [:fed "fed"] [:hungry "not fed"] {:units :food :left (max 0 (- foods/top-up-food (food-level c)))}))

(defn quote-of [_c]
  {:uses {:job-time [0 90 ##Inf]}})

;; ------------------------------------------------------------------ the round

(defn ^:async use-source!
  "Way :source: :again or :continue when the source gave something to do, else nil."
  [c source]
  (if (risk/refused? c (:pos source))
    (do (sources/pass-over! c (:pos source)) :again)
    (case (:kind source)
      :farm (await (sources/farm! c source))
      :chest (await (sources/chest! c source))
      :animals (case (await (sources/go-near! c (:pos source)))
                 :continue :continue
                 :far (sources/bury-source! c (:pos source))
                 (do (sources/pass-over! c (:pos source)) (await (hunt! c false))))
      nil)))

(defn ^:async pen!
  "Way :pen: hunt the surplus of its own pens in sight, else walk to the nearest zone or plan of its own not visited in
  this job and hunt there. :again after a visit, nil with none left."
  [c]
  (if (hunt-left? c true)
    (await (hunt! c true))
    (when-let [pos (first (sources/own-areas c))]
      (sources/pass-over! c pos)
      (if (risk/refused? c pos)
        :again
        (case (await (sources/go-near! c pos))
          :continue :continue
          (or (await (hunt! c true)) :again))))))

(defn ^:async field!
  "Way :field: walk to the nearest zone or plan of its own not visited in this job and cut and replant its ripe wheat
  (farm.harvest), for :bake. :again after a visit, nil with none left."
  [c]
  (when-let [[lo hi :as box] (first (sources/own-boxes c :tried-fields))]
    (let [pos (sources/box-middle box)]
      (ctx/update-mem! c update :tried-fields (fnil conj []) pos)
      (if (risk/refused? c pos)
        :again
        (let [e (await (child/run!* c :field 'jobs.farm.harvest
                                    {:area {:pos pos :radius (inc (apply max (map #(/ (js/Math.abs (- %1 %2)) 2) lo hi)))} :crops ["wheat"]}))]
          (if (= :continue e) :continue :again))))))

(defn ^:async dig-crops!
  [c last-resort?]
  (sources/dig-ripe! c (merge sources/ripe-age sources/wild-food) (u/self-pos c) (hunt-radius c) last-resort?))

(defn ^:async next-way!
  "The first allowed way with something to do: :again (try eating, then the ways, again), :continue (yield), or nil when
  none is left."
  [c]
  (let [source (when (ctx/way? c :source) (sources/known-source c))]
    (when (and (foods/fragile? (.self (:primitives c))) (not (sh-inside/sheltered-in c)))
      (await (look/survey-until! c :near (constantly nil))))
    (or (when (and (ctx/way? c :bake) (can-bake? c)) (await (bake! c)))
        (when (and (ctx/way? c :cook) (cook/can-cook? c)) (await (cook/cook! c)))
        (when source (await (use-source! c source)))
        (when (ctx/way? c :collect) (await (collect-drops! c (drop-radius))))
        (when (ctx/way? c :hunt) (await (hunt! c false)))
        (when (ctx/way? c :crops) (await (dig-crops! c false)))
        (when (ctx/way? c :pen) (await (pen! c)))
        (when (ctx/way? c :field) (await (field! c)))
        (when (and (ctx/way? c :crops) (sources/last-resort? c)) (await (dig-crops! c true))))))

(defn search-waits?
  "Whether the search would wait: the search child's own safe-if-lit under the children's default (ready/or-starving) is not ok. Asked here so a child's :later does not end this job."
  [c]
  (not= :ok (first (ready/or-starving ready/safe-if-lit c))))

(defn explore-done?
  "Whether this job's search is over: it ran to its end (\"started\" is a search a cut or a wait left open, resumed), and a starving body's foods/::starving-rest-ms has not passed since."
  [mem starving? now rest-ms]
  (boolean (and (:explored mem) (not= "started" (:explored mem)) (not (and starving? (>= (- now (:explored-at mem 0)) rest-ms))))))

(def search-max-distance "The largest :max-distance jobs.explore.search accepts." 128)

(defn widen-explore [opts k]
  (-> opts (update :max-distance #(some-> % (* k) (min search-max-distance))) (update :max-legs #(some-> % (* k))) (update-in [:spend :job-time] #(some-> % (* k)))))

(def noted-range "Blocks from an animal another body noted that the walk toward it ends; its place is then looked at." 12)

(defn ^:async walk-to-noted!
  "The walk toward an animal the search found only in a note (nothing is in sight to hunt): :continue while it goes, :again once it ends."
  [c]
  (let [pos (:walk-to (ctx/mem c))
        e (await (child/run!* c :noted-walk 'jobs.movement.go-to (ready/forage-walk c {:pos (zipmap [:x :y :z] pos) :range noted-range :ways #{:walk}})))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c dissoc :walk-to) :again))))

(defn spared-uuids
  "The uuids of the animals passed over (a baby, a pen's, the last adults): the search must not end on one the hunt cannot use."
  [c]
  (into (set (:skip-ids (ctx/mem c))) (keep :uuid) (animals/passed-over c (animal-opts c false))))

(defn only-spared?
  "Whether the search's finds are all animals the hunt passes over."
  [found spared]
  (and (seq found) (every? #(and (:id %) (spared (:id %))) found)))

(defn explore-again?
  "Whether the search ran and passed-over animals have been met since (a miss, a pair, a baby), within child/max-fruitless searches: it runs again with them skipped."
  [c]
  (let [m (ctx/mem c)]
    (boolean (and (:explored m) (:explored-spared m)
                  (< (:explore-runs m 0) (child/max-fruitless))
                  (> (count (spared-uuids c)) (:explored-spared m))))))

(defn ^:async explore!
  "Once per job (again after meeting more passed-over animals, explore-again?), with nothing known or seen: search for food animals and ripe crops (jobs.explore.search, :explore),
  not in a dark sky (a body sheltered for the night stays in) nor in the dark or with danger near unless starving (safe-if-lit), nor underground (a hungry body under rock first climbs out, jobs.lib.surface-up; a starving one searches there), nor when risk refuses it. :again when the search found something to hunt or dig, else nil with what it
  covered remembered."
  [c]
  (when (explore-again? c) (ctx/update-mem! c dissoc :explored))
  (let [e (:explore (:args c))]
    (cond
      (and e (:walk-to (ctx/mem c))) (await (walk-to-noted! c))
      (or (not e) (explore-done? (ctx/mem c) (starving? c) (ctx/now c) (foods/starving-rest-ms))) nil
      (risk/refused? c nil)
      (do (ctx/update-mem! c assoc :explored (str "not searched: " (:too-dangerous (ctx/mem c)))) nil)
      (search-waits? c)
      (do (ctx/update-mem! c assoc :explored "not searched: dark or danger near") nil)
      (and (sh-sense/buried? (:primitives c)) (not (starving? c)))
      (case (await (surface/step! c))
        :continue :continue
        :out :again
        (do (ctx/update-mem! c assoc :explored "not searched: no way up out of the cave") nil))
      :else
      (do
        (when-not (= "started" (:explored (ctx/mem c)))
          (ctx/update-mem! c #(-> % (assoc :explored "started" :explored-at (ctx/now c) :explored-spared (count (spared-uuids c))) (update :explore-runs (fnil inc 0)))))
        (let [opts (widen-explore (merge (settings/get settings ::explore) (when (map? e) e)) (widen c))
              e (await (child/run!* c :explore 'jobs.explore.search
                                    (child/search-args (assoc opts :skip-ids (vec (spared-uuids c)) :target (vec (concat (sort animals/food-animals) (keys sources/ripe-age) (keys sources/wild-food)))))
                                    {:max-calls 1}))]
          (if (= :continue e)
            :continue
            (let [res (:data e)]
              (ctx/update-mem! c assoc :found-reach
                               (reduce max 0 (for [{[x y z] :pos} (:found res) :when x] (+ 4 (u/dist (u/self-pos c) {:x x :y y :z z})))))
              (if (and (only-spared? (:found res) (spared-uuids c)) (< (:explore-runs (ctx/mem c)) (child/max-fruitless)))
                (do (ctx/update-mem! c #(-> % (assoc :skip-ids (vec (spared-uuids c))) (dissoc :explored)))
                    :again)
                (do
                  (ctx/update-mem! c assoc :explored
                                   (report/explored-text res))
                  (when-let [noted (and (nil? (animals/nearest c (animal-opts c false))) (first (filter :id (:found res))))]
                    (ctx/update-mem! c assoc :walk-to (:pos noted)))
                  (when (= :done (:kind e)) :again))))))))))

(defn rod-carried?
  "Whether a fishing rod is carried: jobs.food.fish itself looks for a bank beside seen water two deep within its ::bank-radius."
  [c]
  (boolean (some #(= "fishing_rod" (:name %)) (u/inventory (:primitives c)))))

(defn ^:async fish!
  "With nothing else to do, a rod carried and a bank in sight: jobs.food.fish for two fish, once per job (:fished), not at
  a dark sky unless starving with no danger near. :again when it caught any (they are eaten or cooked next), :continue while
  the child waits on the world, else nil."
  [c]
  (when (and (not (:fished (ctx/mem c))) (not (search-waits? c)) (rod-carried? c))
    (let [e (await (child/run!* c :fish 'jobs.food.fish {:count 2 :least 1 :ways #{:carried}} {:max-calls 1}))]
      (if (= :continue e)
        :continue
        (do (ctx/update-mem! c assoc :fished true)
            (when (pos? (:got (:data e) 0)) :again))))))

(defn ^:async eat-bad!
  "Last resort when food is at or below the ::eat-bad-food line: eat what hurts (rotten flesh) with :allow-bad.
  :again when the food level rose, else nil."
  [c]
  (when (and (eats? c)
             (<= (food-level c) (eat-bad-food))
             (some #(contains? foods/harmful (:name %)) (u/inventory (:primitives c))))
    (let [before (food-level c)
          e (await (child/run!* c :eat-bad 'jobs.survival.eat {:until foods/top-up-food :allow-bad true :get-food? false}))]
      (if (= :continue e) :continue (when (> (food-level c) before) :again)))))

(defn ^:async eat-carried!
  "Eat what is carried (the eat job) up to 18, or 20 below the :health line: :continue when the eat job is still
  going after the call cap, else nil."
  [c]
  (when (eats? c)
    (let [low? (< (.-health (.self (:primitives c))) (:health (:args c)))
          e (await (child/run!* c :eat 'jobs.survival.eat {:until (if low? 20 foods/top-up-food) :get-food? false}))]
      (when (= :continue e) :continue))))


(defn stop!
  "End nothing-possible with reason; text is what the stopped event says."
  [c reason text & {:as extra}]
  (ctx/end c :nothing-possible reason (merge {:text text} extra)))

(defn none!
  "End :no-food (what was searched, what was passed over) and write :hungry, which widens the next search."
  [c]
  (let [food (food-level c)
        around (animals/nearby c (hunt-radius c))
        passed (animals/passed-over c (animal-opts c false))
        text (report/none-text c {:radius (hunt-radius c) :passed passed :around around :explored (:explored (ctx/mem c))})]
    (ctx/remember! c :hungry {:food food} hungry-policy)
    (stop! c :no-food text
           :food food :animals passed :nearby around)))

(defn carry-stop!
  "Fed but the pack cannot be filled: end :no-carry (no :hungry entry, the body is not hungry)."
  [c]
  (let [have (foods/stock-points (u/inventory (:primitives c)))]
    (stop! c :no-carry (str "not hungry (food " (food-level c) ", nothing eaten) but carries " have " of " (:carry (:args c))
                            " food points and found no more within " (js/Math.round (hunt-radius c)) " blocks"
                            (when-let [x (:explored (ctx/mem c))] (str ", explored: " x)))
           :food (food-level c) :carry (:carry (:args c)) :have have)))

(defn ^:async round
  "One whole attempt: eat, then while hungry take the next way and eat again. A step that yields (:continue) is
  waited out in a reflex round (a reflex never yields), else the round yields :continue and keeps the child; a cut
  call starts again from the world."
  [c]
  (ctx/update-mem! c dissoc :too-dangerous)
  (loop [n 0]
    (let [cooked (when (and (ctx/way? c :cook) (cook/cook-first? c)) (await (cook/cook! c)))
          eaten (if (or (= :continue cooked) (= :again cooked)) cooked (await (eat-carried! c)))
          way (when-not (or (= :continue eaten) (= :again cooked))
                (cond
                  (not (wants-food? c)) ::fed
                  (>= n (max-ways)) ::enough
                  :else (await (next-way! c))))]
      (cond
        (not (ctx/alive? c)) :continue
        (= :again cooked) (recur n)
        (or (= :continue eaten) (= :continue way)) (if (:reflex c) (do (await (pace/pace!)) (recur n)) :continue)
        (= ::fed way) (ctx/end c :done nil (cond-> {:food (food-level c)} (seq (:killed (ctx/mem c))) (assoc :killed (:killed (ctx/mem c)))))
        (= ::enough way) (stop! c :still-hungry (str "still hungry (food " (food-level c) ") after " n " ways")
                                :food (food-level c))
        (= :again way) (recur (inc n))
        :else (let [fished (await (fish! c))
                    more (cond fished fished (not (:explore (:args c))) nil :else (await (explore! c)))
                    bad (when-not more (await (eat-bad! c)))]
                (cond
                  (or (= :continue more) (= :continue bad)) (if (:reflex c) (do (await (pace/pace!)) (recur n)) :continue)
                  (or more bad) (recur (inc n))
                  (not (hungry-now? c)) (carry-stop! c)
                  (:too-dangerous (ctx/mem c))
                  (let [why (str "hungry (food " (food-level c) "), " (:too-dangerous (ctx/mem c)))]
                    (ctx/remember! c :food-too-dangerous {:why why} danger-policy)
                    (ctx/end c :condition-failed {:reason :too-dangerous :why why} {:food (food-level c)}))
                  :else (none! c)))))))
