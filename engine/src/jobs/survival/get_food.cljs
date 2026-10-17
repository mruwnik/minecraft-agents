(ns jobs.survival.get-food
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.look :as look]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.lib.foods :as foods]
            [jobs.survival.eat :as eat]
            [jobs.survival.get-food.animals :as animals]
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
         a table). A failed bake is remembered as :no-bake for 10 minutes and the ladder goes on.
  :cook  Raw food carried (chicken, beef ...) with a furnace or smoker seen or remembered (the :furnace place) within cook's ::cook-far-radius (64), or no furnace but enough raw food
         to pay for making one (jobs.lib.cost/cook-worth?): cook it (the smelt job, which fetches a furnace and fuel, waiting out the cook in
         the round), then eat it. Done before eating unless a danger is sensed or food is 0 and health would not outlast the cook plus cook's ::cook-margin-hp. Not at night nor while the night
         handler holds. A failed cook is remembered as :no-cook for 10 minutes (the cooking is jobs.survival.get-food.cook).
  :source  The newest :food-source entry {:pos :kind} within :source-radius, or a chest it booked food in (memory :fetch/stock) within sources' ::stock-radius (256), reached by go-to:
         :farm: dig its mature crops (blocks.dig, which collects the drops). It does not replant.
         :chest: withdraw its best food (storage.withdraw); with no food but 3 or more wheat, wheat in multiples of 3,
         enough for the hunger, for :bake.
         :animals: hunt there (:hunt).
         A chest found empty or a source go-to cannot reach is forgotten; a farm with nothing ripe is passed over.
  :collect  Harvest items lying within 8 blocks (collect-drops).
  :hunt  The nearest passive food animal within :hunt-radius (combat.attack, then collect-drops).
  :crops  Mature crops, sweet berry bushes or melons within :hunt-radius (blocks.dig).
  :fish  Nothing left: with a fishing rod carried, once per job it fishes two (jobs.food.fish, which looks for a bank beside water within ::fish-radius (48)) and eats or cooks them.
  :pen   The surplus of its own pens: an animal in a zone or plan of its own in sight, else it walks (go-to) to the
         nearest zone or plan of its own within :source-radius it has not visited in this job and hunts there (the
         same keep rules; the kill's rule is :own-pen). Its stock is its reserve: taken after the wild food in reach, before fishing and the search.
  Then, once per job (:explore, default on; a starving body again after foods/::starving-rest-ms) it searches for food animals and crops (jobs.explore.search, the
  ::explore setting; not while the search would wait (dark sky, danger near) nor while the night handler holds) and goes on if it found some; at ::eat-bad-food (6) or below it then eats harmful food carried (rotten
  flesh, with :allow-bad). Still nothing: it ends nothing-possible :no-food (the text says what was searched, the nearest known source) and writes
  :hungry. For :ask-cooldown-ms after that a call still eats and still does the ways :bake, :cook, :collect, :hunt and :crops, but not a source it
  already knew when it gave up (one learned since is tried first; a starving body, food 0 or health below 7, rests only foods/::starving-rest-ms).
  :stock n: a fed body also takes the ways until the pack holds n food points, else ends nothing-possible :no-stock; while the pack is short of it the ask cooldown does not block the search.
  A fed body (not hungry, nothing to top up with, no :stock short) is done at once.
  Start condition (a listed call waits, a called one ends condition-failed): :no-food-found, hungry inside the ask cooldown with no food carried, nothing to bake or cook, no source learned since, no animal to hunt and no ripe crop in range (harmful food above ::eat-bad-food does not count): it looks around once from here first. Mid-call it ends condition-failed :no-food-found when the cooldown is on and nothing is left to try.
  A go-to that waits on the world, or a child still going after jobs.lib.child's call cap, yields :continue; a reflex
  call (it never yields) waits it out in the round.
  More than 12 ways in one call without getting fed ends nothing-possible :still-hungry.
  Ready: safe? and fit? always ok: it eats carried food whatever is near. Its walks and searches run under :checks (job-safe-or-starving); a merely hungry body waits for dawn in the open (intended).
  Fragile (health at or below foods/::fragile-health, 6) with no food carried, it walks out (a search, a source, its pen,
  an animal, a furnace to cook at) only on a calm day (jobs.lib.dawn/calm?) and not past known hostiles whose route
  danger (jobs.lib.cost/route-danger) is its health or more; with nothing else to try it writes a :food-too-dangerous entry (no :hungry) and yields (a reflex ends condition-failed
  :too-dangerous); can-run? is then :later and the hungry trigger rests (its :rest-s) while the body stays fragile
  (jobs.survival.get-food.risk). Not shut in a shelter, it first looks around from where it stands (once per place) so hostiles outside its view count. For ::danger-rest-s (300) after that, while still fragile, above food 0, with nothing to eat, a call
  is :later :too-dangerous (can-start?, can-run?) without acting, so the hungry reflex backs off.
  Wheat is never harvested. Babies, animals in another's zone or claim, animals in a zone or plan of the body's own (but for the :pen way) or inside a closed enclosure (:spare-pens, default true), kinds in :off-limits (default the jobs.lib.animal-spare setting), and the last :keep-adults (2) adults of a kind in sight (for breeding; never hunted, even when starving, unless ::kept-lifted-below lifts it) are never hunted (:ignore-zones? lifts the others' zone only). Each animal attacked is a food.killed info with its name, cell, adults counted, :keep-adults, food and the rule that allowed it; the data carries :killed. A chest in another's zone or claim is skipped and never taken from, even when starving
  (one get-food.skipped warning). A crop in another's zone or claim is dug only when no other is (one
  get-food.trespass-last-resort warning). :ignore-zones? lifts both.
  A cut call starts again from the world: carried food first, then the ways.
  Memory: reads :food-source and :hungry. Writes :hungry {:food} (cap 10, one hour) and :no-bake. Job memory (hints for
  this run): :tried-sources, :skipped-blocks, :skipped-animals. Quote: F, a search.")

(a/defargs args
  {:food {:doc "hungry below this much food (of 20)" :spec (a/num-in 0 20) :default foods/default-food}
   :health {:doc "below this health eat up to a full bar" :spec (a/num-in 0 20) :default foods/default-health}
   :source-radius {:doc "how far away a remembered food source still counts, in blocks" :spec (a/num-in 0 nil) :default 64}
   :hunt-radius {:doc "how far to look for animals and wild crops, in blocks" :spec (a/num-in 0 nil) :default 24}
   :take {:doc "most items to withdraw from a chest in one go" :spec (a/int-in 1 nil) :default 16}
   :attack-gap-ms {:doc "least time between two swings at an animal; nil: the held weapon's cooldown" :spec (a/num-in 0 nil) :default nil}
   :explore {:doc "with nothing known or seen, walk and look for food animals and crops once per job (a starving body again after foods/::starving-rest-ms; not underground unless starving): false, true (the ::explore setting) or jobs.explore.search options over it" :spec (a/or-of boolean? map?) :default true}
   :stock {:doc "also carry food worth this many hunger points: a fed body takes the ways below until the pack holds it (0: only feed)" :spec (a/num-in 0 nil) :default 0}
   :ask-cooldown-ms {:doc "after finding nothing, how long before saying so (and searching) again" :spec (a/num-in 0 nil) :default (* 10 60 1000)}
   :spare-pens {:doc "never hunt animals in a closed enclosure, nor those in a zone or plan of the body's own but by the :pen way (false: a job asked to get food may take them all)" :spec boolean? :default true}
   :keep-adults {:doc "adults of a kind in sight that are never hunted, so they can breed (hunted only when ::kept-lifted-below lifts it)" :spec (a/int-in 0 nil) :default 2}
   :off-limits {:doc "animal kinds never hunted; nil: the jobs.lib.animal-spare/off-limits setting" :spec (a/or-of (a/pred "nil" nil?) (a/coll-of a/item?)) :default nil}
   :ignore-zones? :shared})

(def harvest-items
  "What the drops of a harvest or a kill may be called."
  (into (set (filter foods/edible? (keys (foods/table)))) #{"wheat" "wheat_seeds" "beetroot_seeds" "poisonous_potato"}))

(def hungry-policy {:cap 10 :ttl (* 60 60 1000)})

(def widen-max
  "The search grows with each recent give-up (:hungry entry) up to this many times its size."
  3)

(defn widen
  "How many times the search radius is multiplied: 1 plus the :hungry give-ups within the policy ttl, at most widen-max."
  [c]
  (min widen-max (inc (ctx/count-in c :hungry (:ttl hungry-policy)))))

(defn hunt-radius [c] (* (widen c) (:hunt-radius (:args c))))

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one attempt at getting fed may take: a hunt, a search, a cook." :spec (a/num-in 0 nil)}
   ::drop-radius {:default 8 :doc "Blocks around the body in which dropped food is picked up." :spec (a/int-in 0 nil)}
   ::explore {:default {:max-distance 64 :max-legs 12 :spend {:job-time 120}} :doc "jobs.explore.search options of the search for food (:explore true)." :spec map?}
   ::fish-radius {:default 48 :doc "Blocks within which seen water two deep with a bank makes fishing a way, with a fishing rod carried." :spec (a/int-in 0 nil)}
   ::kept-lifted-below {:default -1 :doc "At this much food or less, with no food carried, the last adults of a kind (kept for breeding) are hunted too; -1 (default) never." :spec (a/int-in -1 20)}
   ::eat-bad-food {:default 6 :doc "At this much food or less the last resort eats harmful food carried (rotten flesh); above it harmful food is not food." :spec (a/num-in 0 20)}
   ::danger-rest-s {:default 300 :doc "Seconds after a fragile body found every way too dangerous in which a call ends at once, acting nothing, so the hungry trigger's backoff grows." :spec (a/num-in 0 nil)}
   ::max-ways {:default 12 :doc "Ways one call tries without getting fed before it ends :still-hungry." :spec (a/int-in 0 nil)}})

(defn drop-radius [] (settings/get settings ::drop-radius))

(defn max-ways [] (settings/get settings ::max-ways))

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

(defn stock-short?
  "A :stock is asked for and the pack holds fewer food points than that."
  [c]
  (< (foods/stock-points (u/inventory (:primitives c))) (:stock (:args c))))

(defn wants-food? [c] (or (hungry-now? c) (stock-short? c)))

(defn starving?
  "Food 0, or health at or below 6 and hungry: the rest after a fruitless search is only foods/::starving-rest-ms."
  [c]
  (foods/starving? (.self (:primitives c)) (:args c)))

(defn gave-up-recently?
  "Whether a fruitless search is still inside the ask cooldown; an explicit :stock ask not yet met is never blocked by it."
  [c]
  (and (not (stock-short? c))
       (pos? (ctx/count-in c :hungry (if (starving? c)
                                       (min (:ask-cooldown-ms (:args c)) (foods/starving-rest-ms))
                                       (:ask-cooldown-ms (:args c)))))))

;; ------------------------------------------------------------------ ways

(def ways [:bake :cook :source :collect :hunt :crops :fish :pen])

(def defaults {:checks {:safe? '(job-safe-or-starving) :fit? '(always)}
               :spend {:job-time ::job-time-s} :ways (set ways)})

(def safe? ready/ok)

(def fit? ready/ok)

(defn way?
  "Whether the call allows way k."
  [c k]
  (contains? (set (or (:ways (ctx/frozen-call c)) (:ways (:args c)) (:ways defaults))) k))

;; ------------------------------------------------------------------ gathering and hunting

(defn ^:async collect-drops!
  "Pick up the harvest items within radius (collect-drops): :again when some came in, else nil."
  [c radius]
  (let [e (await (child/run!* c :collect 'jobs.forestry.collect-drops {:radius radius :filter (vec harvest-items)}))]
    (cond
      (= :continue e) :continue
      (pos? (:collected (:data e) 0)) :again)))

(defn animal-opts [c own?] {:radius (hunt-radius c) :lift (settings/get settings ::kept-lifted-below) :own? own?})

(defn hunt-left?
  "Whether an animal in sight is left to hunt (own?: in its own pens too)."
  [c own?]
  (some? (animals/nearest c (animal-opts c own?))))

(defn ^:async hunt!
  "Kill the nearest food animal (combat.attack; own?: its own pens' too, see animals/in-sight) and pick up its drops:
  :again, or nil when there is none (after a look around) or the walk to it is refused (risk). An animal is attacked
  once per job, killed or not."
  [c own?]
  (when-let [{animal :entity :as target} (await (animals/nearest-or-look c (animal-opts c own?)))]
    (when-not (risk/refused? c (u/pos-of (.-pos animal)))
      (let [e (await (child/run!* c :attack 'jobs.combat.attack
                                  {:targets [(.-id animal)] :radius (hunt-radius c) :attack-gap-ms (:attack-gap-ms (:args c))
                                   :lost-s 0 :spend {:job-time 60}
                                   :checks {:safe? '(job-safe-or-starving)}}))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c update :skipped-animals (fnil conj []) (.-id animal))
              (animals/note-kill! c target)
              (or (await (collect-drops! c (drop-radius))) :again)))))))

;; ------------------------------------------------------------------ bread

(defn can-bake?
  "No edible food carried, 3 or more wheat, and no live :no-bake entry."
  [c]
  (and (nil? (eat/carried-best c))
       (>= (sources/carried-count c "wheat") 3)
       (zero? (ctx/count-in c :no-bake cook/no-bake-ms))))

(defn ^:async bake!
  "Way :bake: :again when bread was made, else nil with :no-bake remembered."
  [c]
  (let [loaves (min (sources/loaves-wanted c) (quot (sources/carried-count c "wheat") 3))
        e (await (child/run!* c :bake 'jobs.items.craft {:item "bread" :count loaves :radius 32 :fetch false}))]
    (cond
      (= :continue e) :continue
      (>= (:made (:data e) 0) 1) :again
      :else (let [r (when-not (= :done (:kind e)) (contract/cause-key e))
                  reason (if (or (nil? r) (= :need r)) "short" (name r))]
              (ctx/remember! c :no-bake {:reason reason} cook/no-bake-policy)
              (ctx/emit! c :food.no-bake :info {:text (str "cannot bake bread: " reason)})
              nil))))

;; ------------------------------------------------------------------ start

(defn nothing-to-try?
  "Hungry inside the ask cooldown with no food carried, no bread to bake, no source learned since giving up, no
  animal to hunt and no ripe crop in range: a call would only end."
  [c]
  (and (gave-up-recently? c)
       (not-any? #(tryable-food? c %) (u/inventory (:primitives c)))
       (not (and (way? c :bake) (can-bake? c)))
       (not (and (way? c :cook) (cook/can-cook? c)))
       (not (and (way? c :source) (sources/fresh-source c)))
       (or (not (way? c :hunt)) (not (hunt-left? c false)))
       (or (not (way? c :pen)) (not (hunt-left? c true)))
       (or (not (way? c :crops)) (empty? (sources/ripe-blocks c (merge sources/ripe-age sources/wild-food) (u/self-pos c) (hunt-radius c))))))

(defn no-food-cause [c]
  {:reason :no-food-found
   :why (str "hungry (food " (food-level c) ") but no food is carried, known or in sight within "
             (hunt-radius c) " blocks; searched recently")})

(defn fed?
  "Neither hungry, nor topping up, nor short of :stock."
  [c]
  (let [self (.self (:primitives c))]
    (not (or (wants-food? c)
             (foods/top-up? (.-food self) (.-health self) (foods/carried-names self (foods/reserved (ctx/view c))))
             (foods/eat-now? self (:args c) (foods/reserved (ctx/view c)))))))

(defn no-food-found
  "Something to try while hungry, else :no-food-found (:look until a look round from here found nothing)."
  [c]
  (if (and (hungry-now? c) (nothing-to-try? c))
    (checks/cause-status (if (look/surveyed? c :near) :later :look) (no-food-cause c))
    (contract/ok :food-to-try "not hungry, or food to try")))

(def danger-policy {:cap 1 :ttl (* 60 60 1000)})

(defn too-dangerous
  ":later :too-dangerous while a call ended so within ::danger-rest-s and the body is still fragile, not at food 0, with nothing
  carried to eat: the call ends before it acts, so a hungry reflex's backoff counts it."
  [c]
  (let [{:keys [t data]} (ctx/latest c :food-too-dangerous)]
    (if (and t
             (< (- (ctx/now c) t) (* 1000 (settings/get settings ::danger-rest-s)))
             (foods/fragile? (.self (:primitives c)))
             (pos? (.-food (.self (:primitives c))))
             (not-any? #(tryable-food? c %) (u/inventory (:primitives c))))
      (contract/later :too-dangerous "food was too dangerous to get a moment ago" :why (:why data))
      (contract/ok :safe-enough "not resting from a danger"))))

(def target [])

(def can-start? (cond/and no-food-found too-dangerous))

(def can-run? too-dangerous)

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
  this job (not inside the ask cooldown) and hunt there. :again after a visit, nil with none left."
  [c]
  (if (hunt-left? c true)
    (await (hunt! c true))
    (when-let [pos (when-not (gave-up-recently? c) (first (sources/own-areas c)))]
      (sources/pass-over! c pos)
      (if (risk/refused? c pos)
        :again
        (case (await (sources/go-near! c pos))
          :continue :continue
          (or (await (hunt! c true)) :again))))))

(defn ^:async next-way!
  "The first allowed way with something to do: :again (try eating, then the ways, again), :continue (yield), or nil when
  none is left. Inside the ask cooldown only a source learned since counts."
  [c]
  (let [source (when (way? c :source) (if (gave-up-recently? c) (sources/fresh-source c) (sources/known-source c)))]
    (when (and (foods/fragile? (.self (:primitives c))) (not (sh/sheltered-in c)))
      (await (look/survey-until! c :near (constantly nil))))
    (or (when (and (way? c :bake) (can-bake? c)) (await (bake! c)))
        (when (and (way? c :cook) (cook/can-cook? c)) (await (cook/cook! c)))
        (when source (await (use-source! c source)))
        (when (way? c :collect) (await (collect-drops! c (drop-radius))))
        (when (way? c :hunt) (await (hunt! c false)))
        (when (way? c :crops) (await (sources/dig-ripe! c (merge sources/ripe-age sources/wild-food) (u/self-pos c) (hunt-radius c))))
        (when (way? c :pen) (await (pen! c))))))

(defn night-blocks-search?
  "Whether the search would wait: the search child's own safe-if-lit under the children's default (ready/or-starving) is not ok, or the night handler holds and the body is not starving with no
  danger near. Asked here so a child's :later does not end this job."
  [c]
  (let [calm? (= :ok (first (ready/threatened c)))]
    (boolean
     (or (and (cook/night-holds? c) (not (and (starving? c) calm?)))
         (not= :ok (first (ready/or-starving ready/safe-if-lit c)))))))

(defn explore-done?
  "Whether this job's search is over: it ran, and a starving body's foods/::starving-rest-ms has not passed since."
  [mem starving? now rest-ms]
  (boolean (and (:explored mem) (not (and starving? (>= (- now (:explored-at mem 0)) rest-ms))))))

(def search-max-distance "The largest :max-distance jobs.explore.search accepts." 128)

(defn widen-explore [opts k]
  (-> opts (update :max-distance #(some-> % (* k) (min search-max-distance))) (update :max-legs #(some-> % (* k))) (update-in [:spend :job-time] #(some-> % (* k)))))

(defn ^:async explore!
  "Once per job, with nothing known or seen: search for food animals and ripe crops (jobs.explore.search, :explore),
  not at night (a body sheltered for the night stays in) nor while the night handler holds, unless starving with no
  danger near (jobs.lib.danger/danger-near?), nor underground (sh/buried?) unless starving and not fragile, nor when risk refuses it. :again when the search found something to hunt or dig, else nil with what it
  covered remembered."
  [c]
  (let [e (:explore (:args c))]
    (cond
      (or (not e) (explore-done? (ctx/mem c) (starving? c) (ctx/now c) (foods/starving-rest-ms))) nil
      (risk/refused? c nil)
      (do (ctx/update-mem! c assoc :explored (str "not searched: " (:too-dangerous (ctx/mem c)))) nil)
      (night-blocks-search? c)
      (do (ctx/update-mem! c assoc :explored "not searched at night or while the night handler holds") nil)
      (and (sh/buried? (:primitives c)) (or (not (starving? c)) (foods/fragile? (.self (:primitives c)))))
      (do (ctx/update-mem! c assoc :explored "not searched underground: the caves are dark and full of mobs; surface or eat first") nil)
      :else
      (do
        (ctx/update-mem! c assoc :explored "started" :explored-at (ctx/now c))
        (let [opts (widen-explore (merge (settings/get settings ::explore) (when (map? e) e)) (widen c))
              e (await (child/run!* c :explore 'jobs.explore.search
                                    (assoc opts :target (vec (concat (sort animals/food-animals) (keys sources/ripe-age) (keys sources/wild-food))))
                                    {:max-calls 1}))]
          (if (= :continue e)
            :continue
            (let [res (:data e)]
              (ctx/update-mem! c assoc :explored
                               (str (count (:found res)) " found, " (get-in res [:coverage :legs] 0) " legs"
                                    (when (:why res) (str ", " (name (:why res))))))
              (when (= :done (:kind e)) :again))))))))

(defn rod-carried?
  "Whether a fishing rod is carried: jobs.food.fish itself looks for a bank beside seen water two deep within ::fish-radius."
  [c]
  (boolean (some #(= "fishing_rod" (:name %)) (u/inventory (:primitives c)))))

(defn ^:async fish!
  "With nothing else to do, a rod carried and a bank in sight: jobs.food.fish for two fish, once per job (:fished), not at
  night unless starving with no danger near. :again when it caught any (they are eaten or cooked next), :continue while
  the child waits on the world, else nil."
  [c]
  (when (and (not (:fished (ctx/mem c))) (not (night-blocks-search? c)) (rod-carried? c))
    (let [e (await (child/run!* c :fish 'jobs.food.fish {:count 2 :least 1 :radius (settings/get settings ::fish-radius) :fetch false} {:max-calls 1}))]
      (if (= :continue e)
        :continue
        (do (ctx/update-mem! c assoc :fished true)
            (when (pos? (:got (:data e) 0)) :again))))))

(defn ^:async eat-bad!
  "Last resort when food is at or below the ::eat-bad-food line: eat what hurts (rotten flesh) with :allow-bad.
  :again when the food level rose, else nil."
  [c]
  (when (and (<= (food-level c) (eat-bad-food))
             (some #(contains? foods/harmful (:name %)) (u/inventory (:primitives c))))
    (let [before (food-level c)
          e (await (child/run!* c :eat-bad 'jobs.survival.eat {:until foods/top-up-food :allow-bad true}))]
      (if (= :continue e) :continue (when (> (food-level c) before) :again)))))

(defn ^:async eat-carried!
  "Eat what is carried (the eat job) up to 18, or 20 below the :health line: :continue when the eat job is still
  going after the call cap, else nil."
  [c]
  (let [low? (< (.-health (.self (:primitives c))) (:health (:args c)))
        e (await (child/run!* c :eat 'jobs.survival.eat {:until (if low? 20 foods/top-up-food)}))]
    (when (= :continue e) :continue)))


(defn stop!
  "End nothing-possible with reason; text is what the stopped event says."
  [c reason text & {:as extra}]
  (ctx/end c :nothing-possible reason (merge {:text text} extra)))

(defn none!
  "End :no-food (what was searched, what was passed over) and write :hungry, which starts the ask cooldown."
  [c]
  (let [food (food-level c)
        around (animals/nearby c (hunt-radius c))
        passed (animals/passed-over c (animal-opts c false))
        text (report/none-text c {:radius (hunt-radius c) :passed passed :around around :explored (:explored (ctx/mem c))})]
    (ctx/remember! c :hungry {:food food} hungry-policy)
    (stop! c :no-food (str text "; the hungry reflex rests for "
                           (js/Math.round (/ (:ask-cooldown-ms (:args c)) 60000))
                           " min unless food is carried, wheat to bake is, or a food source is learned")
           :food food :animals passed :nearby around)))

(defn stock-stop!
  "Fed but the pack cannot be filled: end :no-stock (no :hungry entry, the body is not hungry)."
  [c]
  (let [have (foods/stock-points (u/inventory (:primitives c)))]
    (stop! c :no-stock (str "fed (food " (food-level c) ") but carries " have " of " (:stock (:args c))
                            " food points and found no more within " (hunt-radius c) " blocks")
           :food (food-level c) :stock (:stock (:args c)) :have have)))

(defn ^:async round
  "One whole attempt: eat, then while hungry take the next way and eat again. A step that yields (:continue) is
  waited out in a reflex round (a reflex never yields), else the round yields :continue and keeps the child; a cut
  call starts again from the world."
  [c]
  (ctx/update-mem! c dissoc :too-dangerous)
  (loop [n 0]
    (let [cooked (when (and (way? c :cook) (cook/cook-first? c)) (await (cook/cook! c)))
          eaten (if (or (= :continue cooked) (= :again cooked)) cooked (await (eat-carried! c)))
          way (when-not (or (= :continue eaten) (= :again cooked))
                (cond
                  (not (wants-food? c)) ::fed
                  (>= n (max-ways)) ::enough
                  :else (await (next-way! c))))]
      (cond
        (not (ctx/alive? c)) :continue
        (= :again cooked) (recur n)
        (or (= :continue eaten) (= :continue way)) (if (:reflex c) (do (await (child/pace!)) (recur n)) :continue)
        (= ::fed way) (ctx/end c :done nil (cond-> {:food (food-level c)} (seq (:killed (ctx/mem c))) (assoc :killed (:killed (ctx/mem c)))))
        (= ::enough way) (stop! c :still-hungry (str "still hungry (food " (food-level c) ") after " n " ways")
                                :food (food-level c))
        (= :again way) (recur (inc n))
        :else (let [fished (when-not (gave-up-recently? c) (await (fish! c)))
                    more (cond fished fished (or (gave-up-recently? c) (not (:explore (:args c)))) nil :else (await (explore! c)))
                    bad (when-not more (await (eat-bad! c)))]
                (cond
                  (or (= :continue more) (= :continue bad)) (if (:reflex c) (do (await (child/pace!)) (recur n)) :continue)
                  (or more bad) (recur (inc n))
                  (not (hungry-now? c)) (stock-stop! c)
                  (:too-dangerous (ctx/mem c))
                  (let [why (str "hungry (food " (food-level c) "), " (:too-dangerous (ctx/mem c)))]
                    (ctx/remember! c :food-too-dangerous {:why why} danger-policy)
                    (if (:reflex c)
                      (ctx/end c :condition-failed {:reason :too-dangerous :why why} {:food (food-level c)})
                      :continue))
                  (gave-up-recently? c) (ctx/end c :condition-failed (no-food-cause c) {:food (food-level c)})
                  :else (none! c)))))))
