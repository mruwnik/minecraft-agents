(ns jobs.combat.attack
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.game :as game]
            [engine.ctx :as ctx]
            [jobs.lib.hostile-fight :as hostile]
            [jobs.lib.checks :as checks]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.look :as look]
            [jobs.lib.vantage :as vantage]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Attack the entities :targets names until none is left within :radius.
  A target is an entity id, a player's username (players match only by username) or a mob type such as \"zombie\".
  Items never match; the body never targets itself.
  One call is the whole fight: it takes the nearest target not given up on, holds the first of :use :tools carried (default
  the best weapon), walks within reach (jobs.lib.near/go-near!) and swings once per weapon cooldown until it ends.
  It does not swing at an entity whose `hittable` sensing is false (glass in the way) except in a :hostiles fight: it
  walks closer and counts a blocked walk.
  A target is given up on, with a :reason: :unreachable (three blocked walks or out-of-reach swings in a row; a landed
  hit resets the count, an out-of-reach swing right after an arrived walk does not count), :no-damage (::no-damage-hits
  swings in a row that did none) or :too-many-hits (::max-hits hits without a kill). A killed target or player is not attacked
  again in this run, even after respawning.
  Returns {:reason :killed [ids] :given-up {id reason} :names {id name} :text}, plus the shelter hint when a target was
  given up as :unreachable from inside the body's own shelter. Ends
  - :done :cleared: every target seen was killed and none is within :radius.
  - :nothing-possible :gave-up / :lost: every target seen was killed or given up on and one was given up on / some target
    was neither.
  - :condition-failed :absent: no target after a look round and, for one heard but not seen, up to four steps toward the
    sound (jobs.lib.vantage). :absent :wait makes the call wait (can-start? :no-target) until one is present: a standing guard.
  - :limit-reached: the engine's limit (default :spend :job-time ::job-time-s), with the same data.
  The job does not guard the body's health: the survival register above it cuts it.
  {:targets :hostiles} is the reflex fight (respond-to-hostile, respond.options): hostiles within :radius (ranged ones
  within :ranged-radius), except the ids in :skip. Only a hostile the body has seen is fought; one only heard is turned
  toward (:nothing-possible :heard-only), never walked to. One beyond hostile-fight/::leash of where the fight began is not a target.
  A melee hostile beyond reach is waited for, never walked to, with :hold or when it stands in or beside unlit space (hostile-fight/dark-around?: a player
  does not chase into unlit space, more may be there unseen); the wait declines :nothing-possible :hold-timeout after hostile-fight/::hold-s
  without a swing; a ranged one farther than hostile-fight/::ranged-chase while another is in range ends :too-far (not with :commit). No
  hostile in range: :done :cleared. When the body lost health since the call began the job yields (:continue). Every ending
  but :done also carries :struck {id {:name :hits :health ...}}; a caller passes that ending's data back as :prior.")

(a/defargs args
  {:targets {:doc "entity ids, player usernames and mob types to kill; a single one may be given bare" :spec (a/or-of number? string? #{:hostiles} (a/coll-of (a/or-of number? string?))) :default []}
   :radius {:doc "targets within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :ranged-radius {:doc "with :hostiles, ranged hostiles (skeletons and the like) within this many blocks count; nil: :radius" :spec (a/num-in 0 nil) :default nil}
   :hold {:doc "true: never walk toward a melee mob; strike one that comes within reach" :spec boolean? :default false}
   :prior {:doc "with :hostiles, the data of an earlier call's declined ending (:killed :given-up :names :struck): the fight goes on from its counts" :spec map? :default nil}
   :commit {:doc "with :hostiles, fight to the end: a ranged hostile is walked to however far it is" :spec boolean? :default false}
   :skip {:doc "with :hostiles, entity ids already dead: not fought" :spec (a/coll-of number?) :default []}
   :absent {:doc ":done ends the job with reason :absent when no target is present after a 2 s grace; :wait makes the check decline until one is" :spec #{:done :wait} :default :done}})

(a/defargs settings
  {::max-hits {:default 40 :doc "Hits without a kill before a target is given up on." :spec (a/int-in 1 nil)}
   ::vehicle-gap-ms {:default 150 :doc "Least time between swings at a minecart or boat: its damage drains 1 a tick and each hit adds only 10, so slower hits never break it." :spec (a/int-in 0 nil)}
   ::reach {:default 3 :doc "How near the target the body stands to swing, in blocks." :spec (a/int-in 0 nil)}
   ::no-damage-hits {:default 16 :doc "Swings in a row that do no damage before a target is given up on (a lagging server drops many swings at the cooldown, so it is many)." :spec (a/int-in 1 nil)}
   ::seen-memory-s {:default 5 :doc "Seconds a hostile seen and then out of sight (round a corner) still counts as a target, walked to at its last place; one only ever heard never does." :spec (a/num-in 0 nil)}
   ::walk-timeout-s {:default 5 :doc "Seconds one walk towards a target may take." :spec (a/num-in 0 nil)}
   ::forage-walk-timeout-s {:default 30 :doc "Seconds one walk towards an animal may take while the body can not heal and walks over its health budget (that search is slow)." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds from the first round before a fight gives up." :spec (a/num-in 0 nil)}})
(defn max-hits [] (settings/get settings ::max-hits))
(defn reach [] (settings/get settings ::reach))
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:targets])

(def defaults {:spend {:job-time ::job-time-s} :use {:tools :attack} :checks {:safe? '(not (exposed))}})
(declare present)

(defn place
  "The cell of the nearest present target when the targets are not hostiles' (a hostile comes to the body), else nil."
  [c]
  (when-not (or (hostile/hostiles? c) (:hold (:args c)))
    (some-> (first (present c)) .-pos u/pos-of (update-vals js/Math.floor))))

(def safe? (price/staying :work (ready/safe-at-or place ready/safe-if-lit)))

(defn fit?
  "ready/fit?, but not for passive animals alone: they cannot hurt the body, and killing one is the way to the food that heals (survival mode only)."
  [c]
  (let [targets (when-not (hostile/hostiles? c) (present c))]
    (if (and (seq targets) (every? #(= "passive" (.-kind %)) targets))
      (checks/survival-mode c)
      (ready/fit? c))))

(defn target-list
  "targets as a vector: nil is empty, a bare id or name is one target."
  [targets]
  (cond
    (nil? targets) []
    (or (number? targets) (string? targets)) [targets]
    :else (vec targets)))

(defn matches?
  "Whether JS entity e is one of targets. The body itself and items never
  match; a username matches a player not in killed-players; a mob type
  matches a non-player by name."
  [targets self-name killed-players e]
  (let [kind (.-kind e)
        player? (= "player" kind)
        match (fn [t]
                (cond
                  (number? t) (= t (.-id e))
                  (not (string? t)) false
                  player? (and (= t (.-username e)) (not (contains? killed-players t)))
                  :else (= t (.-name e))))]
    (cond
      (= "item" kind) false
      (and player? (or (= self-name (.-username e)) (= self-name (.-name e)))) false
      :else (boolean (some match targets)))))

(defn lost?
  "Whether hostile e is out of mind: not in sight and never seen, or not sensed for ::seen-memory-s."
  [e]
  (and (= "hostile" (.-kind e))
       (not (.-visible e))
       (not (and (.-seen e) (<= (.-ageMs e) (* 1000 (settings/get settings ::seen-memory-s)))))))

(defn present
  "The entities matching :targets within :radius, nearest first; ones already
  killed (their id stays after a respawn) are left out, and so is a hostile of a type or name target only heard or not seen for ::seen-memory-s (it
  is turned toward, never walked to). Name and id targets filter before the 64 cap of the sense."
  [c]
  (let [p (:primitives c)
        {:keys [targets radius]} (:args c)
        {:keys [killed killed-players]} (ctx/mem c)
        dead-ids (set killed)
        self-name (.-username (.self p))
        wanted (target-list targets)
        names (when (and (seq wanted) (every? string? wanted)) {:names (vec wanted)})
        ids (when (and (seq wanted) (every? number? wanted)) {:ids (vec wanted)})]
    (->> (combat/sensed p (merge {:radius radius :max 64} names ids))
         (remove #(contains? dead-ids (.-id %)))
         (remove #(and (not ids) (lost? %)))
         (filterv #(matches? wanted self-name (set killed-players) %)))))

(defn candidates
  "The present targets not given up on."
  [c]
  (if (hostile/hostiles? c)
    (hostile/targets c)
    (let [given-up (:given-up (ctx/mem c) {})]
      (into [] (remove #(contains? given-up (.-id %))) (present c)))))

(defn heard
  "The unseen animals the body hears within :radius that :targets name (an id or a mob type)."
  [c]
  (let [{:keys [targets radius]} (:args c)
        wanted (set (target-list targets))]
    (filterv #(or (wanted (:id %)) (wanted (:name %))) (vantage/heard c nil radius))))

(def no-target
  "With :absent :wait, or :targets :hostiles: later :no-target while no target is present (for :hostiles, one the body has seen)."
  (checks/refusing
   :target "a target is present"
   (fn [c]
     (when (and (or (hostile/hostiles? c) (= :wait (:absent (:args c)))) (empty? (candidates c)))
       {:reason :no-target}))))

(def can-start? no-target)

(defn fight-data
  "The fight's data so far."
  [c reason]
  (let [{:keys [killed given-up names]} (ctx/mem c)
        given-up (or given-up {})]
    (cond-> {:reason reason :killed (vec killed) :given-up given-up :names (or names {})
             :text (str "attack " (name reason) ": killed " (count killed)
                        (apply str (for [[id why] given-up] (str ", gave up on " (get names id id) " " id ": " (name why)))))}
      (some #{:unreachable} (vals given-up)) (merge (sh-inside/shelter-hint c)))))

(defn done?
  "Done once every target seen is killed and none is present; the data is the fight so far."
  [c]
  (let [{:keys [killed seen]} (ctx/mem c)
        dead? (set killed)
        data [:units :targets :done (count killed) :left (count (remove dead? seen))]
        cleared? (if (hostile/hostiles? c)
                   (empty? (hostile/in-range c))
                   (and (seq seen) (every? dead? seen) (empty? (present c))))]
    (if cleared?
      (apply contract/ok :cleared "every target is killed" (conj data :result (fight-data c :cleared)))
      (apply contract/later :fighting "the fight is on" (conj data :result (fight-data c :limit-reached))))))

(defn quote-of [c]
  (let [most (or (get-in (ctx/frozen-call c) [:spend :job-time]) (time-s))]
    {:uses {:job-time [0 (min 30 most) most]}}))

(defn book-kill!
  "Add id to :killed; its username, if any, is not attacked again."
  [c id username]
  (ctx/update-mem! c (fn [m]
                       (cond-> (update m :killed (fnil conj []) id)
                         username (update :killed-players (fnil conj #{}) username)))))

(defn display-name
  "A player's username, else the entity's name."
  [target]
  (or (.-username target) (.-name target)))

(defn give-up!
  [c target reason]
  (ctx/update-mem! c #(-> % (assoc-in [:given-up (.-id target)] reason) (assoc-in [:names (.-id target)] (display-name target)))))

(defn fail-count
  "mem's failure count for target id after one more failure with the body at health. With restart? it starts over when
  the body lost health since the last failure (a :hostiles fight: a mob that keeps hurting it is not given up on)."
  [mem id health restart?]
  (if (and restart? (< health (get-in mem [:fail-health id] ##-Inf)))
    1
    (inc (get-in mem [:fails id] 0))))

(defn fail!
  "Count a blocked walk or out-of-reach swing at target; give up at u/max-failures."
  [c target]
  (let [id (.-id target)
        health (.-health (.self (:primitives c)))
        n (fail-count (ctx/mem c) id health (hostile/hostiles? c))]
    (ctx/update-mem! c #(-> % (assoc-in [:fails id] n) (assoc-in [:fail-health id] health)))
    (when (>= n (u/max-failures)) (give-up! c target :unreachable))))

(defn reset-fails!
  "A landed swing is progress: target's failure count starts again at 0."
  [c target]
  (ctx/update-mem! c assoc-in [:fails (.-id target)] 0))

(defn vehicle?
  "Whether target is a minecart or boat: it has no health, so a hit shows no damage until it breaks."
  [target]
  (boolean (some->> (.-name target) (re-find #"(minecart|boat|raft)$"))))

(defn quiet-hit?
  "Whether a hit did no damage: reported hurt false, or a known health that did not drop. A vehicle's hit never is."
  [target result previous]
  (let [h (.-health result)]
    (and (not (vehicle? target))
         (or (false? (.-hurt result))
             (and (number? h) (number? previous) (not (< h previous)))))))

(defn note-hit!
  "Record a landed hit on target, and give up on it when it takes no damage or too many hits."
  [c target result]
  (let [id (.-id target)
        h (.-health result)
        quiet? (quiet-hit? target result (get-in (ctx/mem c) [:health id]))]
    (ctx/update-mem! c (fn [m]
                         (cond-> (-> m
                                     (update-in [:hits id] (fnil inc 0))
                                     (assoc-in [:names id] (display-name target))
                                     (assoc-in [:quiet id] (if quiet? (inc (get-in m [:quiet id] 0)) 0)))
                           (number? h) (assoc-in [:health id] h))))
    (let [{:keys [hits quiet]} (ctx/mem c)
          no-damage-hits (settings/get settings ::no-damage-hits)]
      (cond
        (>= (get quiet id 0) no-damage-hits) (give-up! c target :no-damage)
        (>= (get hits id 0) (max-hits)) (give-up! c target :too-many-hits)))))

(defn walk-opts
  "The options of one walk towards a target: a :hostiles fight leaves doors as they are and aims again every ::chase-timeout-s, the mob moves.
  A walk to an animal may spend hp (with a longer leg, ::forage-walk-timeout-s) while the body can not heal (ready/forage-walk): the animal is the food."
  [c]
  (if (hostile/hostiles? c)
    {:dangers false :ways #{:walk :clear-seal} :look-round false :leg-s (hostile/chase-timeout-s)}
    (let [walk (ready/forage-walk c {:dangers false :ways #{:walk :clear-seal} :look-round false :doors :shut :leg-s (settings/get settings ::walk-timeout-s)})]
      (cond-> walk (:floor walk) (assoc :leg-s (settings/get settings ::forage-walk-timeout-s))))))

(defn in-reach?
  "Whether target is within reach by feet, or by the eye to the middle of its body as the attack primitive judges it (a
  spider on a ledge above is hit from below although its feet are further than reach); the second only with a clear line
  (hittable not false), so a mob above a ceiling does not stop the walk."
  [c target]
  (let [feet (u/self-pos c)
        tpos (u/pos-of (.-pos target))
        eye (update feet :y + game/eye-height)]
    (or (<= (u/dist feet tpos) (reach))
        (and (not (false? (.-hittable target)))
             (<= (u/dist eye (update tpos :y + 0.5)) (reach))))))

(def chase-gap-ms "How long a held body waits for a mob to come before it looks again." 200)

(defn ^:async walk!
  "Walk within reach of target when further than reach (near/go-near! range 2, doors :shut, each leg bounded by
  ::walk-timeout-s). Resolves to :there (no walk needed), :arrived (the walk got within range), :partial or :blocked."
  [c target]
  (let [tpos (u/pos-of (.-pos target))]
    (cond
      (in-reach? c target) :there
      (hostile/held? c target) (do (await (combat/wait-gap! c (ctx/now c) chase-gap-ms))
                                                                 :held)
      :else
      (case (await (near/go-near! c tpos 2 (walk-opts c)))
        :there :arrived
        :partial :partial
        :blocked))))

(defn ^:async swing!
  "Face and hit target once. walked? says a walk just arrived: an out-of-reach
  swing then means the target moved on, and is not counted as a failure."
  [c target walked?]
  (let [tpos (u/pos-of (.-pos target))]
    (ctx/update-mem! c assoc :last-attack (ctx/now c))
    (await (ctx/act c :look #js {:pos #js {:x (:x tpos) :y (+ 1 (:y tpos)) :z (:z tpos)}}))
    (let [a (await (ctx/act c :attack #js {:id (.-id target)}))]
      (case (.-status a)
        "killed" (do (reset-fails! c target)
                     (book-kill! c (.-id target) (or (.-username target) nil)))
        "out-of-reach" (when-not walked? (fail! c target))
        "hit" (do (reset-fails! c target)
                  (note-hit! c target a))
        nil))))

(defn ^:async close-in!
  "Walk right up to a target that cannot be hit from here, and count a failure."
  [c target]
  (await (near/go-near! c (u/pos-of (.-pos target)) 1 (walk-opts c)))
  (fail! c target))

(defn ^:async swing-or-close-in!
  "Swing at target unless it reports hittable false (a block in the way): then close in. A :hostiles fight swings regardless:
  a mob hitting the body through a gap the sensing does not show must still be struck back."
  [c target walked?]
  (if (and (false? (.-hittable target)) (not (hostile/hostiles? c)))
    (await (close-in! c target))
    (await (swing! c target walked?))))

(defn finish!
  "End the call: done when cleared, else the ending of reason."
  [c reason]
  (let [result (fight-data c reason)]
    (case reason
      :cleared (ctx/end c :done nil result)
      :absent (ctx/end c :condition-failed :absent result)
      (ctx/end c :nothing-possible reason result))))

(defn settled-reason
  "How a run that finds no target ends: :cleared when every target
  ever seen was killed, :gave-up when each was killed or given up on and one
  was given up on, else :lost."
  [c]
  (let [{:keys [seen killed given-up]} (ctx/mem c)
        dead? (set killed)
        settled? (into dead? (keys given-up))]
    (cond
      (every? dead? seen) :cleared
      (every? settled? seen) :gave-up
      :else :lost)))

(defn weapon-for
  "The name of the first of uses (item names or parts of one) carried, the best of those an entry matches."
  [p uses]
  (let [carried (map :name (u/inventory p))]
    (some (fn [use] (->> carried (filter #(str/includes? % use)) (sort-by weapon/weapon-score >) first)) uses)))

(declare strike!)
(def back-leg-s "Seconds one step back from a creeper may take." 2)
(def look-settle-ms "Time for sensing to show the creeper again after the body turns back to it." 400)

(defn ^:async back-off!
  "Step out of a creeper's reach after a hit, or from one whose fuse is lit (it blows on its own, harmlessly past ~6 blocks): true when the body moved. Nothing when no open cell gains on it."
  [c creeper]
  (let [cell (hostile/back-cell-of c creeper)]
    (when cell
      (await (ctx/call c :back 'jobs.movement.run-away {:pos cell :range 0 :leg-s back-leg-s :checks {:safe? '(always) :fit? '(always)}}))
      (let [{:keys [x y z]} (u/pos-of (.-pos creeper))]
        (await (ctx/act c :look #js {:pos #js {:x x :y (+ 1 y) :z z}}))
        (await (ctx/act c :wait #js {:ms look-settle-ms}))
        true))))

(defn ^:async engage!
  "Equip, walk to the nearest target and swing once; a creeper is waited for, hit once and backed away from."
  [c target]
  (await (combat/equip-best! c (weapon-for (:primitives c) (ctx/use c :tools))))
  (if (and (hostile/hostiles? c) (true? (.-fusing target)) (await (back-off! c target)))
    :again
    (await (strike! c target)))
  :again)

(defn ^:async strike!
  "Walk to target, swing once and, for a creeper, step back out of its blast."
  [c target]
  (case (await (walk! c target))
    :there (await (swing-or-close-in! c target false))
    :arrived (await (swing-or-close-in! c target true))
    (:partial :held) nil
    (fail! c target))
  (when (and (hostile/hostiles? c) (combat/creeper? target) (not (some #{(.-id target)} (:killed (ctx/mem c)))) (<= (u/dist (u/self-pos c) (u/pos-of (.-pos target))) (reach)))
    (await (back-off! c target))))

(defn swing-gap-ms
  "The least time between swings at target: ::vehicle-gap-ms for a vehicle, else the call's or weapon's gap."
  [c target]
  (if (vehicle? target)
    (settings/get settings ::vehicle-gap-ms)
    (combat/gap-ms c)))

(defn ^:async wait-swing-gap!
  "Wait out the rest of the swing gap. A vehicle's is too short to look round in (watch! would outlast it)."
  [c target]
  (if-not (vehicle? target)
    (await (combat/wait-gap! c (:last-attack (ctx/mem c)) (combat/gap-ms c)))
    (let [left (- (+ (:last-attack (ctx/mem c)) (swing-gap-ms c target)) (ctx/now c))]
      (when (pos? left) (await (ctx/act c :wait #js {:ms left}))))))

(defn declined
  "The ending of a :hostiles fight that stops with hostiles left, for the retreat to take over: cause, the fight's data
  and the hits that landed (:struck)."
  [c cause]
  (ctx/end c :nothing-possible cause (assoc (fight-data c cause) :struck (hostile/struck (ctx/mem c)))))

(defn ^:async hostile-step
  "One step of a :hostiles fight: decline (hold ran out, ranged mob too far, only heard, all given up), yield when the body was hurt,
  look at a heard mob, wait out the swing gap, or swing once. :again, :continue or an ending."
  [c]
  (let [p (:primitives c)
        target (first (hostile/targets c))
        last-attack (:last-attack (ctx/mem c))]
    (when-not (:start (ctx/mem c)) (ctx/update-mem! c assoc :start (u/self-pos c) :started-at (ctx/now c)))
    (cond
      (and target (hostile/held-idle? c (hostile/held? c target))) (declined c :hold-timeout)
      (and target (hostile/too-far-to-chase? c target)) (declined c :too-far)
      (and target (combat/creeper? target) (not (hostile/hit-and-back-ok? c (hostile/in-range c)))) (declined c :creeper-unsafe)
      (and last-attack (< (.-health (.self p)) (:health0 (ctx/mem c)))) :continue
      (nil? target) (if-let [heard (hostile/heard c)]
                      (do (await (ctx/act c :look (let [{:keys [x y z]} (danger-q/mob-pos p heard)] #js {:pos #js {:x x :y (+ 1 y) :z z}})))
                          (declined c :heard-only))
                      (if (seq (hostile/in-range c)) (declined c :given-up) (finish! c :cleared)))
      (combat/within-gap? c (swing-gap-ms c target)) (do (await (wait-swing-gap! c target)) :again)
      :else (do (ctx/update-mem! c update :seen (fnil conj #{}) (.-id target))
                (await (engage! c target))
                (cond
                  (empty? (hostile/in-range c)) (finish! c :cleared)
                  (empty? (hostile/targets c)) (declined c :given-up)
                  :else :again)))))

(defn ^:async step
  "One step of the fight: end, look for a lost target, wait out the swing gap, or walk and swing once. :again or the ending."
  [c]
  (let [{:keys [seen]} (ctx/mem c)
        targets (candidates c)
        found? #(seq (candidates c))]
    (cond
      (and (empty? targets) (seq (present c))) (finish! c :gave-up)
      (empty? targets) (cond
                         (and (seq seen) (not= :lost (settled-reason c))) (finish! c (settled-reason c))
                         (await (look/survey-until! c found?)) :again
                         (await (vantage/step! c (heard c) found?)) :again
                         :else (finish! c (if (empty? seen) :absent (settled-reason c))))
      :else (do (ctx/update-mem! c update :seen (fnil into #{}) (mapv (fn [e] (.-id e)) targets))
                (if (combat/within-gap? c (swing-gap-ms c (first targets)))
                  (do (await (wait-swing-gap! c (first targets))) :again)
                  (await (engage! c (first targets))))))))

(defn ^:async round
  "The whole fight: steps until the job finishes (cleared, gave up, lost, absent) or the engine's limit ends it."
  [c]
  (if (hostile/hostiles? c)
    (do (when-not (:primed (ctx/mem c))
          (ctx/update-mem! c merge (hostile/carry-over (:prior (:args c))) {:primed true}))
        (ctx/update-mem! c assoc :health0 (.-health (.self (:primitives c))))
        (await (pace/steps! c #(hostile-step c))))
    (await (pace/steps! c #(step c)))))

(def meta {:tags [:fight]})
