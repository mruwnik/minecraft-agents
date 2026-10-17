(ns jobs.combat.attack
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.combat :as combat]
            [jobs.lib.look :as look]
            [jobs.lib.vantage :as vantage]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.shelter :as sh]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Attack the entities :targets names until none is left within :radius.
  A target is an entity id (a number), a player's username, or a mob type such as \"zombie\"
  (every mob of that name within :radius). Players match only by username. Items never match. The body never targets itself.
  One call is the whole fight: it takes the nearest target not given up on, holds the first of the call's :use :tools
  carried (default the best weapons carried, swords over axes; each entry is an item name or a part of one), walks within
  reach (jobs.lib.near/go-near!, doors :shut, each leg bounded by :walk-timeout-s) and swings, again and again until
  it ends. Swings at most once per :attack-gap-ms (nil: the held weapon's cooldown); the rest of the gap is spent looking
  round and waiting.
  It does not swing when the entity's `hittable` sensing is false (no clear line to its hitbox, for example glass).
  It walks closer instead, and counts that as a blocked walk.
  A target is given up on, with an attack.gave-up warning and a :reason:
  - :unreachable: three blocked walks or out-of-reach swings in a row. A landed hit resets the count.
    An out-of-reach swing right after a walk that arrived (the target moved on) does not count.
  - :no-damage: :no-damage-hits swings in a row that did no damage.
  - :too-many-hits: :max-hits hits without a kill.
  A kill counts only when the attack reports killed. A killed target or player is not attacked again in this run,
  even after respawning.
  Ends with the data {:reason :killed [ids] :given-up {id reason}}; the ending is
  - :done (:cleared): every target seen was killed and none is within :radius.
  - :nothing-possible :gave-up: every target seen was killed or given up on, and one was given up on.
  - :nothing-possible :lost: some target was neither killed nor given up on.
  - :limit-reached: the engine's limit (default :spend :job-time ::job-time-s) ends it, with the same data (done?'s :data).
  - :condition-failed :absent: no target was present after a look round and, for a target heard but not seen, up to four steps toward the sound (jobs.lib.vantage).
  :absent :done (the default) lets the job start and end with :absent, so {:targets [123 \"zombie\"]} is a one-shot order.
  :absent :wait makes the call wait (can-start? :no-target) until a target is present, so {:targets \"zombie\" :absent :wait} repeated is a standing guard.
  Creepers get no special handling. The job does not guard the body's health: the survival register sits above it and cuts it.")

(a/defargs args
  {:targets {:doc "entity ids, player usernames and mob types to kill; a single one may be given bare" :spec (a/or-of number? string? (a/coll-of (a/or-of number? string?))) :default []}
   :radius {:doc "targets within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :attack-gap-ms {:doc "least time between swings; nil: the held weapon's cooldown" :spec (a/num-in 0 nil) :default nil}
   :no-damage-hits {:doc "swings in a row that do no damage before a target is given up on" :spec (a/int-in 1 nil) :default 4}
   :max-hits {:doc "hits without a kill before a target is given up on" :spec (a/int-in 1 nil) :default 40}
   :walk-timeout-s {:doc "bound of one walk towards a target" :spec (a/num-in 0 nil) :default 5}
   :absent {:doc ":done ends the job with reason :absent when no target is present after a 2 s grace; :wait makes the check decline until one is" :spec #{:done :wait} :default :done}})

(a/defargs settings
  {::reach {:default 3 :doc "How near the target the body stands to swing, in blocks." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds from the first round before a fight gives up." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:targets])

(def defaults {:spend {:job-time ::job-time-s} :use {:tools :attack} :checks {:safe? '(not (exposed))}})
(def safe? ready/safe-if-lit)
(def fit? ready/fit?)

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

(defn present
  "The entities matching :targets within :radius, nearest first; ones already
  killed (their id stays after a respawn) are left out. Name and id targets filter before the 64 cap of the sense."
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
         (filterv #(matches? wanted self-name (set killed-players) %)))))

(defn candidates
  "The present targets not given up on."
  [c]
  (let [given-up (:given-up (ctx/mem c) {})]
    (into [] (remove #(contains? given-up (.-id %))) (present c))))

(defn heard
  "The unseen animals the body hears within :radius that :targets name (an id or a mob type)."
  [c]
  (let [{:keys [targets radius]} (:args c)
        wanted (set (target-list targets))]
    (filterv #(or (wanted (:id %)) (wanted (:name %))) (vantage/heard c nil radius))))

(def no-target
  "With :absent :wait: later :no-target while no target is present."
  (checks/refusing
   :target "a target is present"
   (fn [c]
     (when (and (= :wait (:absent (:args c))) (empty? (candidates c)))
       {:reason :no-target}))))

(def can-start? no-target)

(defn fight-data
  "The fight's data so far."
  [c reason]
  (let [m (ctx/mem c)]
    {:reason reason :killed (vec (:killed m)) :given-up (:given-up m {})}))

(defn done?
  "Done once every target seen is killed and none is present; the data is the fight so far."
  [c]
  (let [{:keys [killed seen]} (ctx/mem c)
        dead? (set killed)
        data [:units :targets :done (count killed) :left (count (remove dead? seen))]
        cleared? (and (seq seen) (every? dead? seen) (empty? (present c)))]
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
  (ctx/update-mem! c assoc-in [:given-up (.-id target)] reason)
  (ctx/emit! c :attack.gave-up :warn (merge {:target (.-id target) :name (display-name target) :reason reason
                                             :text (str "giving up on " (display-name target) " " (.-id target) ": " (name reason))}
                                            (when (= :unreachable reason) (sh/shelter-hint c)))))

(defn fail!
  "Count a blocked walk or out-of-reach swing at target; give up at u/max-failures."
  [c target]
  (let [n (inc (get-in (ctx/mem c) [:fails (.-id target)] 0))]
    (ctx/update-mem! c assoc-in [:fails (.-id target)] n)
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
                                     (assoc-in [:quiet id] (if quiet? (inc (get-in m [:quiet id] 0)) 0)))
                           (number? h) (assoc-in [:health id] h))))
    (let [{:keys [hits quiet]} (ctx/mem c)
          {:keys [no-damage-hits max-hits]} (:args c)]
      (cond
        (>= (get quiet id 0) no-damage-hits) (give-up! c target :no-damage)
        (>= (get hits id 0) max-hits) (give-up! c target :too-many-hits)))))

(defn ^:async walk!
  "Walk within reach of target when further than reach (near/go-near! range 2, doors :shut, each leg bounded by
  :walk-timeout-s). Resolves to :there (no walk needed), :arrived (the walk got within range), :partial or :blocked."
  [c target]
  (let [tpos (u/pos-of (.-pos target))]
    (if (<= (u/dist (u/self-pos c) tpos) (reach))
      :there
      (case (await (near/go-near! c tpos 2 {:doors :shut :spend {:job-time (:walk-timeout-s (:args c))} :dangers false :ways #{:walk :clear-seal} :look-round false}))
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
  (await (near/go-near! c (u/pos-of (.-pos target)) 1 {:doors :shut :spend {:job-time (:walk-timeout-s (:args c))} :dangers false :ways #{:walk :clear-seal} :look-round false}))
  (fail! c target))

(defn ^:async swing-or-close-in!
  "Swing at target unless it reports hittable false (a block in the way): then close in."
  [c target walked?]
  (if (false? (.-hittable target))
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

(defn ^:async engage!
  "Equip, walk to the nearest target and swing once."
  [c target]
  (await (combat/equip-best! c (weapon-for (:primitives c) (ctx/use c :tools))))
  (case (await (walk! c target))
    :there (await (swing-or-close-in! c target false))
    :arrived (await (swing-or-close-in! c target true))
    :partial nil
    (fail! c target))
  :again)

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
                (if (combat/within-gap? c)
                  (do (await (combat/wait-gap! c (:last-attack (ctx/mem c)) (combat/gap-ms c))) :again)
                  (await (engage! c (first targets))))))))

(defn ^:async round
  "The whole fight: steps until the job finishes (cleared, gave up, lost, absent) or the engine's limit ends it."
  [c]
  (await (pace/steps! c #(step c))))
