(ns jobs.survival.respond-to-hostile
  (:require [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.result :as r]
            [jobs.lib.shelter :as sh]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.dig-in-cells :as dig-cells]))

(def doc
  "A hostile is near: fight it (jobs.survival.fight-back, best weapon equipped) when the odds are fair,
  else retreat (jobs.survival.retreat).
  One whole attempt per round: decides afresh before each call of the child, while a danger is near.
  Never fights a creeper. Otherwise fights when the damage the fight is expected to cost
  leaves at least :reserve health (jobs.lib.cost/fight-damage: weapon, armour worn, each mob's kind
  and what is left of it after the hits landed, the dangers killed nearest first).
  If the chosen child declines, the other one runs.
  Only ranged mobs with a line of fire near and the body under a roof, carrying building blocks and a tool that digs
  stone (a block in the doorway must not shut it in): it stops the arrows instead (jobs.survival.block-arrow-gap, no
  fetch), once per call; when that places nothing it decides as above.
  Done once no real danger (as the hostile-near trigger, jobs.lib.reach, in sight) is within :radius
  (:ranged-radius for ranged mobs) and the retreat is not hiding (sealed in, up a pillar or down a pit).
  A child that stops (a retreat that cannot escape) stops it with that cause; three calls in a row that change
  neither the body's cell, its health nor the dangers near stop it :no_response; never :continue.
  Memory: writes one :hostile entry {:mob :decision} plus :pos (seen) or :direction :band :from (heard only) per encounter.
  A danger reflex: never backed off.")

(def args
  {:radius {:doc "hostiles within this many blocks count" :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks count" :default 16}
   :reserve {:doc "health a fight must be expected to leave" :default 4}
   :quiet-s {:doc "passed to the retreat: a hidden body keeps its refuge this many seconds after the last danger" :default 30}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}})

(def hostile-policy {:cap 50 :ttl (* 60 60 1000)})

(defn near
  "The hostiles that count: ones the body has seen (jobs.lib.danger/known-hostiles), melee within :radius and ranged
  within :ranged-radius. One behind a wall cannot reach or shoot the body, so
  it is left alone, as is one with no walkable way to the body (jobs.lib.reach),
  as the hostile-near trigger does."
  [c]
  (let [{:keys [radius ranged-radius]} (:args c)]
    (danger-q/dangers (:primitives c) radius {:ranged-radius ranged-radius} {})))


(def other {:fight :flee :flee :fight})

(def child-jobs {:fight 'jobs.survival.fight-back :flee 'jobs.survival.retreat})

(defn ^:async run-child [c decision {:keys [radius ranged-radius weapons reserve quiet-s]}]
  (let [child-args (case decision
                     :fight {:range radius :ranged-range ranged-radius :min-health 0 :weapons weapons}
                     :flee {:radius radius :ranged-radius ranged-radius :weapons weapons :reserve reserve :quiet-s quiet-s})]
    (await (ctx/call-child c decision (child-jobs decision) child-args))))

(defn log-encounter! [c threat decision]
  (when-not (:logged (ctx/mem c))
    (ctx/remember! c :hostile (assoc (threats/place-of (:primitives c) threat) :mob (.-name threat) :decision decision)
                   hostile-policy)
    (ctx/update-mem! c assoc :logged true)))

(defn mob-of
  "What fight-damage needs of hostile e: its name, distance (jobs.lib.danger/mob-distance: a heard one by its band) and
  the hits fight-back has landed on it (struck, its :struck entry, or nil)."
  [p struck e]
  (cond-> {:name (.-name e) :distance (danger-q/mob-distance p e) :hits (:hits struck 0)}
    (number? (:health struck)) (assoc :health (:health struck))))

(defn ^:async respond [c near]
  (let [{:keys [reserve weapons] :as a} (:args c)
        p (:primitives c)
        self (.self p)
        decision (cost/decide {:health (.-health self)
                               :creeper? (boolean (some combat/creeper? near))
                               :reserve reserve
                               :damage (cost/fight-damage {:weapon (combat/best-weapon p weapons)
                                                           :equipment (cost/equipment-of (.-equipment self))
                                                           :mobs (map #(mob-of p (get-in (ctx/mem c) [:children :fight :struck (.-id %)]) %) near)})})]
    (log-encounter! c (first near) decision)
    (ctx/update-mem! c assoc :decision decision)
    (let [result (await (run-child c decision a))]
      (if (= :declined result)
        (await (run-child c (other decision) a))
        result))))

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
                  (dig-cells/pick c dig-in/building-blocks)
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
  (or (hiding? c) (boolean (seq (near c)))))

(defn stopped-child
  "[slot result] of the child that stopped in this round's last call, or nil."
  [c]
  (some #(let [res (ctx/child-result c %)] (when (= :stopped (:status res)) [% res])) [:flee :fight]))

(def max-unchanged
  "Calls of a child in a row that leave the body and the dangers as they were before the reflex stops :no_response."
  3)

(defn signature
  "What a call may change: the body's cell, its health (a fight that costs health is a response) and the ids of the
  dangers near."
  [c hs]
  {:health (.-health (.self (:primitives c)))
   :cell (let [{:keys [x y z]} (u/self-pos c)] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])
   :ids (set (map #(.-id %) hs))})

(defn ^:async round
  "One whole attempt: respond (or let the retreat hide on) while a danger is near; stopped when a child stops, or
  :no_response after max-unchanged calls in a row that changed nothing; never for the time a danger stays."
  [c]
  (loop [unchanged 0]
    (let [hs (near c)
          before (signature c hs)]
      (cond
        (hiding? c) (await (run-child c :flee (:args c)))
        (empty? hs) nil
        (gap-wanted? c hs (:gap-tried (ctx/mem c))) (await (gap! c))
        :else (await (respond c hs)))
      (let [after (near c)]
        (if-let [[slot res] (stopped-child c)]
          (r/stop! c (:reason res) (:text res) :cause (r/cause-of slot res))
          (cond
            (not (or (seq after) (hiding? c))) :done
            :else
            (let [n (if (= before (signature c after)) (inc unchanged) 0)]
              (if (>= n max-unchanged)
                (do (ctx/emit! c :no_response :warn {:text "the hostile is still near and nothing changed in three tries"})
                    (r/stop! c :no_response "a hostile is still near and three tries changed nothing"))
                (do (await (pace/pace!)) (recur n))))))))))
