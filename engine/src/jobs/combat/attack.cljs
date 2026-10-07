(ns jobs.combat.attack
  (:require [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Attack the entities :targets names until none is left within :radius.
  A target is an entity id (a number), a player's username, or a mob type such as \"zombie\"
  (every mob of that name within :radius). Players match only by username. Items never match. The body never targets itself.
  One call is the whole fight: it takes the nearest target not given up on, holds the best weapon carried, walks within
  reach (jobs.lib.near/walk-near!, doors :shut, each walk bounded by :walk-timeout-s) and swings, again and again until
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
  Ends with info attack.done and the result {:reason :killed [ids] :given-up {id reason}}. :reason is:
  - :cleared: no target within :radius for :lost-s (waiting in 1 s steps), and every target seen was killed.
  - :gave-up: every target seen was killed or given up on, and one was given up on.
  - :lost: some target was neither killed nor given up on.
  - :timeout: :timeout-s after the first round (attack.timeout warning).
  - :absent: no target was present after a 2 s grace for the world's entities to arrive.
  :absent :done (the default) lets the job start and end with :absent, so {:targets [123 \"zombie\"]} is a one-shot order.
  :absent :wait makes the check decline until a target is present, so {:targets \"zombie\" :absent :wait} repeated is a standing guard.
  Otherwise the check passes while a target not given up on is within :radius, and always once the job has started.
  Creepers get no special handling. The job does not guard the body's health: the survival register sits above it and cuts it.")

(def args
  {:targets {:doc "entity ids, player usernames and mob types to kill; a single one may be given bare" :default []}
   :radius {:doc "targets within this many blocks count" :default 16}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}
   :attack-gap-ms {:doc "least time between swings; nil: the held weapon's cooldown" :default nil}
   :lost-s {:doc "seconds without a target in radius before the job is done" :default 5}
   :timeout-s {:doc "seconds from the first round before the job gives up" :default 120}
   :no-damage-hits {:doc "swings in a row that do no damage before a target is given up on" :default 4}
   :max-hits {:doc "hits without a kill before a target is given up on" :default 40}
   :walk-timeout-s {:doc "bound of one walk towards a target" :default 5}
   :absent {:doc ":done ends the job with reason :absent when no target is present after a 2 s grace; :wait makes the check decline until one is" :default :done}})

(def reach 3)
(def absent-grace-ms
  "How long a job that has seen nothing waits for the world's entities to arrive."
  2000)

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
  killed (their id stays after a respawn) are left out. Name targets filter before the 64 cap of the sense."
  [c]
  (let [p (:primitives c)
        {:keys [targets radius]} (:args c)
        {:keys [killed killed-players]} (ctx/mem c)
        dead-ids (set killed)
        self-name (.-username (.self p))
        wanted (target-list targets)
        names (when (and (seq wanted) (every? string? wanted)) {:names (vec wanted)})]
    (->> (combat/sensed p (merge {:radius radius :max 64} names))
         (remove #(contains? dead-ids (.-id %)))
         (filterv #(matches? wanted self-name (set killed-players) %)))))

(defn candidates
  "The present targets not given up on."
  [c]
  (let [given-up (:given-up (ctx/mem c) {})]
    (into [] (remove #(contains? given-up (.-id %))) (present c))))

(defn check [c]
  (boolean (or (:started (ctx/mem c))
               (not= :wait (:absent (:args c)))
               (seq (candidates c)))))

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
    (when (>= n u/max-failures) (give-up! c target :unreachable))))

(defn reset-fails!
  "A landed swing is progress: target's failure count starts again at 0."
  [c target]
  (ctx/update-mem! c assoc-in [:fails (.-id target)] 0))

(defn quiet-hit?
  "Whether a hit did no damage: reported hurt false, or a known health that did not drop."
  [result previous]
  (let [h (.-health result)]
    (or (false? (.-hurt result))
        (and (number? h) (number? previous) (not (< h previous))))))

(defn note-hit!
  "Record a landed hit on target, and give up on it when it takes no damage or too many hits."
  [c target result]
  (let [id (.-id target)
        h (.-health result)
        quiet? (quiet-hit? result (get-in (ctx/mem c) [:health id]))]
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
  "Walk within reach of target when further than reach (near/walk-near! range 2, doors :shut, each steer bounded by
  :walk-timeout-s). Resolves to :there (no walk needed), :arrived (the walk got within range), :partial or :blocked."
  [c target]
  (let [tpos (u/pos-of (.-pos target))]
    (if (<= (u/dist (u/self-pos c) tpos) reach)
      :there
      (case (await (near/walk-near! c tpos 2 {:doors :shut :timeout-s (:walk-timeout-s (:args c)) :dangers false}))
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
  (await (near/walk-near! c (u/pos-of (.-pos target)) 1 {:doors :shut :timeout-s (:walk-timeout-s (:args c)) :dangers false}))
  (fail! c target))

(defn ^:async swing-or-close-in!
  "Swing at target unless it reports hittable false (a block in the way): then close in."
  [c target walked?]
  (if (false? (.-hittable target))
    (await (close-in! c target))
    (await (swing! c target walked?))))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [m (ctx/mem c)
        result {:reason reason :killed (vec (:killed m)) :given-up (:given-up m {})}]
    (when (= :timeout reason)
      (ctx/emit! c :attack.timeout :warn (assoc result :text "attack timed out")))
    (ctx/emit! c :attack.done :info (assoc result :text (str "attack done: " (name reason) ", killed " (count (:killed m)))))
    (ctx/result! c result)
    :done))

(defn settled-reason
  "How a run that saw nothing for :lost-s ends: :cleared when every target
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

(defn gap-ms
  "The least time between swings: :attack-gap-ms, else the held weapon's cooldown."
  [c]
  (or (:attack-gap-ms (:args c))
      (combat/attack-gap-ms (.-held (.self (:primitives c))))))

(defn within-gap?
  "Whether the last swing was less than the gap ago."
  [c]
  (let [last-attack (:last-attack (ctx/mem c))]
    (and last-attack (< (- (ctx/now c) last-attack) (gap-ms c)))))

(defn ^:async engage!
  "Equip, walk to the nearest target and swing once."
  [c target]
  (await (combat/equip-best! c (combat/best-weapon (:primitives c) (:weapons (:args c)))))
  (case (await (walk! c target))
    :there (await (swing-or-close-in! c target false))
    :arrived (await (swing-or-close-in! c target true))
    :partial nil
    (fail! c target))
  :again)

(defn ^:async wait! [c ms]
  (await (ctx/act c :wait #js {:ms ms}))
  :again)

(defn ^:async step
  "One step of the fight: finish, wait, wait out the swing gap, or walk and swing once. :again or :done."
  [c]
  (let [now (ctx/now c)
        {:keys [timeout-s lost-s]} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [{:keys [started seen last-seen]} (ctx/mem c)
          targets (candidates c)]
      (cond
        (>= (- now started) (* 1000 timeout-s)) (finish! c :timeout)
        (and (empty? targets) (seq (present c))) (finish! c :gave-up)
        (and (empty? targets) (empty? seen)) (if (>= (- now started) absent-grace-ms)
                                               (finish! c :absent)
                                               (await (wait! c 500)))
        (empty? targets) (if (>= (- now (or last-seen started)) (* 1000 lost-s))
                           (finish! c (settled-reason c))
                           (await (wait! c 1000)))
        :else (do (ctx/update-mem! c #(-> % (assoc :last-seen now) (update :seen (fnil into #{}) (mapv (fn [e] (.-id e)) targets))))
                  (if (within-gap? c)
                    (do (await (combat/wait-gap! c (:last-attack (ctx/mem c)) (gap-ms c))) :again)
                    (await (engage! c (first targets)))))))))

(defn ^:async round
  "The whole fight: steps until the job finishes (cleared, gave up, lost, timeout, absent)."
  [c]
  (await (pace/steps! c #(step c))))
