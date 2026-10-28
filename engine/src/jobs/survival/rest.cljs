(ns jobs.survival.rest
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.blocks :as lb]
            [jobs.lib.foods :as foods]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.eat :as eat]
            [jobs.survival.get-food.risk :as risk]
            [jobs.survival.respond.state :as flight]))

(def doc
  "Rest a hurt body until it has healed, as a player would before going on. Food first: under foods/full-food it eats what it
  carries (jobs.survival.eat); under 18 with nothing carried it waits for a calm moment (no hostile known near, no dark sky above ground, and
  no night outside, from a cave too; jobs.survival.get-food.risk) shut in, saying why, then gets food the safe way (jobs.survival.eat, which fetches through
  get-food under get-food's own risk rules; at food 0, or when the shut-in failed, it does not wait). Then it shuts in where it stands (jobs.survival.shut-in, ways :room, :walls, :pit, :pocket; its
  blocks go to the scaffold ledger and jobs.access.cleanup takes them back; walls that ran out of blocks get more with
  jobs.items.obtain, :explore false, once a stay when calm, and are tried again) and holds still (:healing) while natural regeneration works, a bite of carried food
  whenever food is under foods/full-food so it goes on. A shut-in that fails warns rest.unsheltered and the body rests where it
  stands. Done {:health} once one more hp is not worth its wait and food (cost.health/heal-worth?: 15 hp by default). Nothing-possible :no-food {:health :text} when no food was
  got (the eat ending's text), :hurt-resting {:health} when health falls while resting, :not-healing when no hp came back in
  ::stall-holds holds in a row, :harmed while burning or poisoned (their reflexes heal that). Run by the low-health trigger,
  which then waits until health falls or (:no-food) food comes to hand. Every ending but done carries :next (jobs.lib.next-call): for :no-food a get-food that searches, then the rest again. Quote: job time up to ::job-time-s.")

(a/defargs args {})

(a/defargs settings
  {::pause-s {:default 4 :doc "Seconds of one hold between two health readings (natural regeneration heals 1 hp in 4 s at food 18)." :spec (a/num-in 0 nil)}
   ::spare-blocks {:default 4 :doc "Blocks at least that walls short of blocks get before they are tried again." :spec (a/int-in 1 nil)}
   ::stall-holds {:default 8 :doc "Holds in a row with no hp gained after which the rest gives up (counted, not timed)." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 900 :doc "Seconds a rest may take, a wait for a calm moment included." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :reflex :tags [:shelter]})

(def safe? ready/ok)

(def fit? ready/ok)

(defn health [c] (.-health (.self (:primitives c))))

(defn food [c] (.-food (.self (:primitives c))))

(def gave-up-policy "The health of the last give-up, which the low-health trigger compares with." {:cap 2 :ttl :forever})

(defn give-up!
  "End nothing-possible with cause, and keep the health in body memory :rest-gave-up so the trigger waits only while it does not fall."
  [c cause data]
  (mem/write! (:store (:engine c)) :rest-gave-up {:health (:health data)} gave-up-policy)
  (ctx/end c :nothing-possible cause data))

(defn done? [c]
  (checks/goal (not (cost-health/heal-worth? (health c) (settings/get settings ::pause-s))) [:rested "healed"] [:resting "resting to heal"]
               {:result {:health (health c)}}))

(defn quote-of [_] {:uses {:job-time [0 60 (settings/get settings ::job-time-s)]}})

(defn uncalm
  "Why a walk out for food or blocks waits, as text, else nil: get-food.risk's reasons, and the night outside, from a
  cave too: food is mostly fetched from above ground."
  [c]
  (let [p (:primitives c)]
    (or (risk/uncalm p c)
        (when (sh-sense/dark-outside? p) "night outside"))))

(defn ^:async more-blocks!
  "Walls that ran out of blocks: when calm and not yet tried this stay, get the open cells' worth more (at least
  ::spare-blocks) with jobs.items.obtain, :explore false. Whether it got them."
  [c e]
  (when (and (= :no-block (get-in e [:data :stops :walls :reason])) (not (:more-blocks (ctx/mem c))) (not (uncalm c)))
    (ctx/update-mem! c assoc :more-blocks true)
    (let [p (:primitives c)
          names (flight/blocks c)
          have (reduce + 0 (map :count (lb/carried c names)))
          need (max (settings/get settings ::spare-blocks) (count (dig-cells/open-cells p (sh-sense/feet p))))]
      (= :done (:kind (await (child/run!* c :blocks 'jobs.items.obtain {:any-of names :count (+ have need) :explore false})))))))

(defn ^:async shelter!
  "Shut in once per stay (again after more-blocks!). A failure is told and the rest goes on in place."
  [c]
  (let [e (await (ctx/call c :shut-in 'jobs.survival.shut-in {:ways #{:room :walls :pit :pocket}}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c assoc :sheltered true) :again)
      (await (more-blocks! c e)) :again
      :else (do (ctx/update-mem! c assoc :sheltered false)
                (ctx/emit! c :rest.unsheltered :warn {:pos (sh-sense/feet (:primitives c))
                                                       :text (str "could not shut in to rest (" (or (:text (:data e)) (:cause e)) "); resting here")})
                :again))))

(defn why
  "The first text along ending e's :because chain (eat's :no-food carries get-food's ending), else its cause's name."
  [e]
  (or (some #(or (:text (:data %)) (:why (:cause %))) (take-while map? (iterate :because e)))
      (some-> (contract/cause-key e) name)))

(defn ^:async eat!
  "Eat up to full, fetching through get-food when fetch? (the walk ends the stay's shelter): :again once food is 18+ or
  the carried food is eaten up, else the :no-food ending."
  [c fetch?]
  (let [e (await (child/run!* c :eat 'jobs.survival.eat {:until foods/full-food :get-food? fetch?}))]
    (when fetch? (ctx/update-mem! c dissoc :sheltered :last-health :still))
    (cond
      (= :continue e) :continue
      (>= (food c) foods/top-up-food) :again
      (and (not fetch?) (not (eat/carried-best c))) :again
      :else (give-up! c :no-food {:health (health c) :text (str "no food to heal on: " (why e))}))))

(defn ^:async feed!
  "Food under 18 and nothing carried: wait shut in for a calm moment (not at food 0), then eat what get-food brings.
  Unsheltered, waiting neither heals nor guards the body, so it goes for food at once under get-food's own risk rules."
  [c]
  (let [sheltered (:sheltered (ctx/mem c))]
    (cond
      (or (zero? (food c)) (not (uncalm c)) (false? sheltered)) (await (eat! c true))
      sheltered (let [r (await (child/pause! c (settings/get settings ::pause-s)
                                             (str "food " (food c) " of " foods/top-up-food " heals nothing and none is carried; shut in until calm (" (uncalm c) ")")))]
                  (if (= :continue r) :continue :again))
      :else (await (shelter! c)))))

(defn ^:async heal-step!
  "A bite while food is under full and food is carried, one hold of ::pause-s, then judge the health it read before against now."
  [c]
  (when (and (< (food c) foods/full-food) (eat/carried-best c))
    (await (child/run!* c :eat 'jobs.survival.eat {:until foods/full-food :max-bites 1 :get-food? false})))
  (let [{:keys [last-health still]} (ctx/mem c)
        h (health c)
        still (if (and last-health (> h last-health)) 0 (inc (or still -1)))]
    (cond
      (and last-health (< h last-health))
      (give-up! c :hurt-resting {:health h :text (str "health fell to " (.toFixed h 1) " while resting")})

      (>= still (settings/get settings ::stall-holds))
      (give-up! c :not-healing {:health h :text (str "no hp came back in " still " holds of rest")})

      :else
      (do (ctx/update-mem! c assoc :last-health h :still still)
          (let [r (await (child/pause! c (settings/get settings ::pause-s) :healing))]
            (if (= :continue r) :continue :again))))))

(defn ^:async step! [c]
  (let [p (:primitives c)]
    (cond
      (or (body/burning? p) (body/poisoned? p))
      (give-up! c :harmed {:health (health c) :text "burning or poisoned: rest does not heal that"})

      (and (< (food c) foods/top-up-food) (eat/carried-best c)) (await (eat! c false))

      (< (food c) foods/top-up-food) (await (feed! c))
      (contains? (ctx/mem c) :sheltered) (await (heal-step! c))
      :else (await (shelter! c)))))

(defn ^:async round
  "The whole rest in one round: food, shelter, then holds until done? holds."
  [c]
  (let [e (await (pace/steps! c #(step! c)))]
    (next-call/on-ending c e 'jobs.survival.rest args
                         (when (and (contract/ending? e) (= :no-food (contract/cause-key e))) (next-call/call 'jobs.survival.get-food {:explore true})))))
