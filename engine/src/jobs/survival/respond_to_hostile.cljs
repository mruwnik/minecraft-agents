(ns jobs.survival.respond-to-hostile
  (:require [engine.args :as a]
            [jobs.lib.blocks :as lb]
            [jobs.lib.body :as body]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.goal :as goal]
            [jobs.lib.result :as r]
            [jobs.lib.shelter :as sh]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "A hostile is near: fight it (jobs.survival.fight-back, best weapon equipped) when the odds are fair,
  else retreat (jobs.survival.retreat).
  One whole attempt per round: decides afresh before each call of the child, while a danger is near.
  Under a roof at night, with every danger only heard or a flyer, it stays in (hostile.sealed-hold) and fights only what comes within reach.
  Never fights a creeper or an enderman (a hit would anger it: the retreat handles it). Otherwise fights when the damage the fight is expected to cost
  leaves at least :reserve health (jobs.lib.cost/fight-damage: weapon, armour worn, each mob's kind
  and what is left of it after the hits landed, the dangers killed nearest first; bare hands land a hit every
  `jobs.lib.cost.fight/fist-gap-ms`, so a body with no weapon retreats from a whole spider or zombie).
  A fight begun goes on against a single mob above `jobs.lib.cost.fight/hysteresis-health`, and at any health against
  a mob the body cannot outrun (jobs.lib.cost/outruns?: a spider at any food).
  If the chosen child declines, the other one runs; but a fight that declines because every danger near is only heard
  (none in sight to hit), with the body not hurt in the last 5 s, runs no retreat: heard mobs are muted (jobs.lib.body/heard-muted?), a
  hostile.heard-only event names the mob, and the job is done, so the job it cut goes on. A mob that comes into sight
  fires the reflex through the mute.
  Only ranged mobs with a line of fire near and the body under a roof, carrying building blocks and a tool that digs
  stone (a block in the doorway must not shut it in): it stops the arrows instead (jobs.survival.block-arrow-gap, no
  fetch), once per call; when that places nothing it decides as above.
  Done once no real danger (as the hostile-near trigger, jobs.lib.danger, in sight) is within :radius
  (:ranged-radius for ranged mobs) and the retreat is not hiding (sealed in, up a pillar or down a pit).
  A child that stops (a retreat that cannot escape) stops it with that cause; three calls in a row that change
  neither the body's cell, its health, the dangers near nor the fight's hits stop it :no_response; never :continue.
  Memory: writes one :hostile entry {:mob :decision} plus :pos (seen) or :direction :band :from (heard only) per encounter,
  and says it in one hostile.decided event.
  A danger reflex: never backed off.")

(a/defargs args
  {:radius {:doc "hostiles within this many blocks count" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :reserve {:doc "health a fight must be expected to leave" :spec (a/num-in 0 20) :default 4}
   :quiet-s {:doc "passed to the retreat: a hidden body keeps its refuge this many seconds after the last danger" :spec (a/num-in 0 nil) :default 30}
   :hide-s {:doc "passed to the retreat: a hidden body hands back after this many seconds once no real danger is on it" :spec (a/num-in 0 nil) :default 60}
   :goal {:doc "passed to the retreat: where the flight leans; nil: the destination of the walk this reflex interrupted (jobs.lib.goal), read once" :spec goal/pos-spec :default nil}
   :stay-inside {:doc "at night a body shut in a closed room or its own shelter fights from there and never steps out toward a mob; under any roof it does not flee for mobs it only hears (false: it may)" :spec boolean? :default true}
   :weapons {:doc "item name substrings that count as weapons" :spec (a/coll-of (a/or-of string? keyword?)) :default combat/default-weapons}})

(def hostile-policy {:cap 50 :ttl (* 60 60 1000)})

(defn near
  "The hostiles that count: ones the body has seen (jobs.lib.danger/known-hostiles), melee within :radius and ranged
  within :ranged-radius. One behind a wall cannot reach or shoot the body, so
  it is left alone, as is one with no walkable way to the body (jobs.lib.danger),
  as the hostile-near trigger does."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (danger-q/dangers (:primitives c) radius {:ranged-radius ranged-radius} {})))


(def other {:fight :flee :flee :fight})

(def child-jobs {:fight 'jobs.survival.fight-back :flee 'jobs.survival.retreat})

(def hold-radius
  "Hostiles known within this many blocks count for holding position at night."
  16)

(defn hold?
  "Whether the fight stays where it is: at night, with more than one hostile known (the ones in view are not all there are),
  or with any when the body is shut in a closed room or its own shelter and :stay-inside is on."
  [c]
  (let [p (:primitives c)]
    (boolean (and (sh/night? p)
                  (or (> (count (danger-q/known-hostiles p hold-radius {})) 1)
                      (and (:stay-inside (:args c)) (or (sh/shut-in-room? p) (sh/sheltered-in c))))))))

(defn stay-under-roof?
  "Whether the body keeps its roof: :stay-inside on, night, a roof over the feet, and every danger near either only heard
  or a flyer (a phantom cannot get under a roof), none a creeper or an enderman. Fight-back then holds and the retreat does not run."
  [c hs]
  (let [p (:primitives c)]
    (boolean (and (:stay-inside (:args c))
                  (seq hs)
                  (sh/night? p)
                  (sh/roofed? p sh/default-roof-height)
                  (every? #(or (combat/flyer? %) (not (danger-q/seen-only? %))) hs)
                  (not-any? #(or (combat/creeper? %) (combat/enderman? %)) hs)))))

(defn ^:async run-child [c decision {:keys [radius ranged-radius weapons reserve quiet-s hide-s]} hold]
  (let [child-args (case decision
                     :fight {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons
                             :hold (or hold (hold? c))}
                     :flee {:radius radius :ranged-radius ranged-radius :weapons weapons :reserve reserve :quiet-s quiet-s :hide-s hide-s
                            :goal (:goal (ctx/mem c))})]
    (await (ctx/call-child c decision (child-jobs decision) child-args))))

(defn encounter-text [{:keys [mob decision pos direction band]}]
  (str (name decision) ": " mob
       (if pos
         (str " seen at " (pr-str (mapv #(js/Math.floor (% pos)) [:x :y :z])))
         (str " heard " (name band) ", " (name direction)))))

(defn log-encounter! [c threat decision]
  (when-not (:logged (ctx/mem c))
    (let [entry (assoc (threats/place-of (:primitives c) threat) :mob (.-name threat) :decision decision)]
      (ctx/remember! c :hostile entry hostile-policy)
      (ctx/emit! c :hostile.decided :info (assoc (dissoc entry :from) :text (encounter-text entry))))
    (ctx/update-mem! c assoc :logged true)))

(defn mob-of
  "What fight-damage needs of hostile e: its name, distance (jobs.lib.danger/mob-distance: a heard one by its band) and
  the hits fight-back has landed on it (struck, its :struck entry, or nil)."
  [p struck e]
  (cond-> {:name (.-name e) :distance (danger-q/mob-distance p e) :hits (:hits struck 0)}
    (number? (:health struck)) (assoc :health (:health struck))))

(defn note-hits
  "Pure: the times (ms) of hits taken in the last window-ms (only a flight counts them), given the earlier ones, now and the health before and now."
  [hits now before after window-ms]
  (let [hits (filterv #(< (- now %) window-ms) hits)]
    (cond-> hits (< after before) (conj now))))

(def hit-window-ms "A flight hit twice within this long is failing: the body fights." 5000)

(defn mute-heard!
  "Mutes heard mobs (jobs.lib.body/heard-muted?) when every danger in hs is heard only, else lifts the mutes: a mob
  the body sees counts again."
  [c hs]
  (cond
    (empty? hs) nil
    (every? (complement danger-q/seen-only?) hs)
    (ctx/remember! c :heard-mute {:until (+ (ctx/now c) (body/next-mute-ms (ctx/view c)))} body/mute-policy)
    :else (ctx/forget-where! c :heard-mute (constantly true))))

(defn leave-heard?
  "Whether a fight that found no target leaves the dangers hs alone instead of fleeing: every one only heard (none
  seen, so none to walk to or hit), none a creeper, and the body not hurt within hit-window-ms (an unseen mob may be
  the one hitting it)."
  [c hs]
  (and (not-any? danger-q/seen-only? hs)
       (not-any? combat/creeper? hs)
       (empty? (:hits (ctx/mem c)))
       (empty? (ctx/since c :hurt (- (ctx/now c) hit-window-ms)))))

(defn leave-heard!
  "Mutes heard mobs (mute-heard!) and says which one was left, so the job the reflex cut goes on; :left-heard."
  [c hs]
  (let [mute-ms (body/next-mute-ms (ctx/view c))
        entry (assoc (threats/place-of (:primitives c) (first hs)) :mob (.-name (first hs)))]
    (mute-heard! c hs)
    (ctx/emit! c :hostile.heard-only :info
               (assoc (dissoc entry :from) :mute-s (/ mute-ms 1000)
                      :text (str "the " (:mob entry) " heard " (name (:band entry)) ", " (name (:direction entry))
                                 " is not in sight and the body can afford the fight: no flight; heard mobs are left alone for "
                                 (/ mute-ms 1000) " s, one that comes into sight is fought")))
    :left-heard))

(defn ^:async respond [c near]
  (let [{:keys [reserve weapons] :as a} (:args c)
        p (:primitives c)
        self (.self p)
        m (ctx/mem c)
        hits (note-hits (when (= :flee (:decision m)) (:hits m)) (ctx/now c) (:last-health m (.-health self)) (.-health self) hit-window-ms)
        _ (ctx/update-mem! c assoc :hits hits :last-health (.-health self))
        chosen (if (some combat/enderman? near)
                   :flee
                   (cost/decide {:health (.-health self)
                               :fighting? (= :fight (:decision m))
                               :single? (= 1 (count near))
                               :cannot-outrun? (not (cost/outruns? (or (.-food self) 20) (map #(.-name %) (remove combat/ranged? near))))
                               :creeper? (boolean (some combat/creeper? near))
                               :reserve reserve
                               :damage (cost/fight-damage {:weapon (combat/best-weapon p weapons)
                                                           :equipment (cost/equipment-of (.-equipment self))
                                                           :mobs (map #(mob-of p (get-in (ctx/mem c) [:children :fight :struck (.-id %)]) %) near)})}))
        stay? (stay-under-roof? c near)
        decision (if (and (= :flee chosen) (>= (count hits) 2) (not-any? combat/creeper? near) (not-any? combat/enderman? near))
                   :fight
                   chosen)
        decision (if stay? :fight decision)]
    (log-encounter! c (first near) decision)
    (when (and stay? (not (:stay-noted (ctx/mem c))))
      (ctx/update-mem! c assoc :stay-noted true)
      (ctx/emit! c :hostile.sealed-hold :info {:text "under a roof at night: the mobs near are heard only or flyers, so the body stays in" :mobs (mapv #(.-name %) near)}))
    (ctx/update-mem! c assoc :decision decision)
    (let [result (await (run-child c decision a stay?))]
      (cond
        (or (not= :declined result) stay?) result
        (and (= :fight decision) (leave-heard? c near)) (leave-heard! c near)
        :else (await (run-child c (other decision) a false))))))

(def cover-sides
  "Of the four sides of the body's cell, how many must be walled (feet and head cell solid) for it to hold cover: a
  doorway leaves one open."
  3)

(defn covered?
  "Whether the body is roofed and walled on cover-sides sides, as a cell with a doorway is (a canopy or overhang is not)."
  [p]
  (let [{:keys [x y z]} (sh/feet p)
        walled? (fn [[dx dz]] (every? #(sh/solid-at? p {:x (+ x dx) :y (+ y %) :z (+ z dz)}) [0 1]))]
    (and (sh/roofed? p sh/default-roof-height)
         (<= cover-sides (count (filter walled? [[1 0] [-1 0] [0 1] [0 -1]]))))))

(defn gap-wanted?
  "Whether to stop arrows with a block rather than fight or flee: only ranged mobs near, the body in cover (covered?), building blocks
  carried and a tool that digs stone (the way out of a sealed doorway), and the gap job not yet tried
  (tried?)."
  [c hs tried?]
  (let [p (:primitives c)]
    (boolean (and (not tried?)
                  (every? combat/ranged? hs)
                  (covered? p)
                  (lb/pick c lb/building-blocks)
                  (tools/can-harvest? p "stone")))))

(defn ^:async gap!
  "One call of block-arrow-gap (never fetching); what it placed is the outcome, nothing placed lets the round decide as usual."
  [c]
  (ctx/update-mem! c assoc :gap-tried true)
  (await (ctx/call-child c :gap 'jobs.survival.block-arrow-gap {:radius (:ranged-radius (:args c)) :fetch false})))

(defn hiding?
  "Whether the retreat child holds a refuge (sealed in, a pillar or a pit): a hostile it hides from is out of sight
  and has no way to the body, yet is still there."
  [c]
  (some? (get-in (ctx/mem c) [:children :flee :refuge])))

(defn check
  "A real danger near, or the retreat hiding from one (it says when that is over)."
  [c]
  (or (boolean (hiding? c))
      (boolean (seq (near c)))
      (ctx/wait c {:reason :no-hostile})))

(defn stopped-child
  "[slot result] of the child that stopped in this round's last call, or nil."
  [c]
  (some #(let [res (ctx/child-result c %)] (when (= :stopped (:status res)) [% res])) [:flee :fight]))

(def max-unchanged
  "Calls of a child in a row that leave the body and the dangers as they were before the reflex stops :no_response."
  3)

(defn signature
  "What a call may change: the body's cell, its health (a fight that costs health is a response), the ids of the
  dangers near and what the fight child counted (hits landed, swings without damage, blocked walks, kills)."
  [c hs]
  {:health (.-health (.self (:primitives c)))
   :cell (let [{:keys [x y z]} (u/self-pos c)] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])
   :ids (set (map #(.-id %) hs))
   :fight (select-keys (get-in (ctx/mem c) [:children :fight]) [:struck :no-damage :blocked :killed])})

(defn ^:async round
  "One whole attempt: respond (or let the retreat hide on) while a danger is near; stopped when a child stops, or
  :no_response after max-unchanged calls in a row that changed nothing; never for the time a danger stays."
  [c]
  (when-not (contains? (ctx/mem c) :goal)
    (ctx/update-mem! c assoc :goal (or (:goal (:args c)) (goal/interrupted-goal (ctx/view c) (:root c)))))
  (loop [unchanged 0]
    (let [hs (near c)
          before (signature c hs)
          outcome (cond
                    (hiding? c) (await (run-child c :flee (:args c) false))
                    (empty? hs) nil
                    (gap-wanted? c hs (:gap-tried (ctx/mem c))) (await (gap! c))
                    :else (await (respond c hs)))
          after (near c)]
      (if-let [[slot res] (stopped-child c)]
        (do (mute-heard! c hs)
            (r/stop! c (:reason res) (:text res) :cause (r/cause-of slot res)))
        (cond
          (= :left-heard outcome) :done
          (not (or (seq after) (hiding? c))) :done
          :else
          (let [n (if (= before (signature c after)) (inc unchanged) 0)]
            (if (>= n max-unchanged)
              (do (mute-heard! c hs)
                  (ctx/emit! c :no_response :warn {:text "the hostile is still near and nothing changed in three tries"})
                  (r/stop! c :no_response "a hostile is still near and three tries changed nothing"))
              (do (await (pace/pace!)) (recur n)))))))))
