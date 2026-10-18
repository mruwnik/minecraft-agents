(ns jobs.survival.respond-to-hostile
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.choose :as choose]
            [engine.args :as a]
            [jobs.lib.checks :as checks]
            [jobs.lib.body :as body]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.goal :as goal]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.lib.look :as look]
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
  If the fight ends other than :done (its memory put back) or the retreat ends :condition-failed, the other one runs; but a fight that declines because every danger near is only heard
  (none in sight to hit), with the body not hurt in the last ::hit-window-s (5), runs no retreat: heard mobs are muted (jobs.lib.body/heard-muted?), a
  hostile.heard-only event names the mob, and the job is done, so the job it cut goes on. A mob that comes into sight
  fires the reflex through the mute.
  Only ranged mobs with a line of fire near and the body under a roof, carrying building blocks and a tool that digs
  stone (a block in the doorway must not shut it in): it stops the arrows instead (jobs.survival.block-arrow-gap, no
  fetch), once per call; when that places nothing it decides as above.
  Done once no real danger (as the hostile-near trigger, jobs.lib.danger, in sight) is within :radius
  (:ranged-radius for ranged mobs) and the retreat is not hiding (sealed in, up a pillar or down a pit).
  Done at once (0 hostiles) with no real danger near. The weapons are the call's :use :tools (default the
  weapons carried), handed to both children as their :use :tools.
  Ends :done when the danger is gone or left heard-only; :nothing-possible with the retreat's own kind and cause (:cannot-escape)
  when the retreat ends otherwise, or :no-response after ::max-unchanged (3) calls in a row that change
  neither the body's cell, its health, the dangers near nor the fight's hits; never :continue. Its time budget (::job-time-s) may end it :limit-reached; the hostile trigger fires again at once while the danger stands.
  Memory: writes one :hostile entry {:mob :decision} plus :pos (seen) or :direction :band :from (heard only) per encounter,
  and says it in one hostile.decided event.
  A danger reflex: never backed off.")

(a/defargs args
  {:radius {:doc "hostiles within this many blocks count" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles (skeletons and the like) within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :reserve {:doc "health a fight must be expected to leave" :spec (a/num-in 0 20) :default 4}
   :goal {:doc "passed to the retreat: where the flight leans; nil: the destination of the walk this reflex interrupted (jobs.lib.goal), read once" :spec goal/pos-spec :default nil}
   :stay-inside {:doc "at night a body shut in a closed room or its own shelter fights from there and never steps out toward a mob; under any roof it does not flee for mobs it only hears (false: it may)" :spec boolean? :default true}})

(a/defargs settings
  {::job-time-s {:default 480 :doc "Seconds a response to a hostile may take: above retreat's (a hide runs long)." :spec (a/num-in 0 nil)}
   ::hold-radius {:default 16 :doc "Hostiles known within this many blocks count for holding position at night." :spec (a/num-in 0 nil)}
   ::hit-window-s {:default 5 :doc "A flight hit twice within this long is failing: the body fights. Also how recent a hurt keeps a fight from leaving heard mobs, and makes a round look once more." :spec (a/num-in 0 nil)}
   ::max-unchanged {:default 3 :doc "Calls of a child in a row that leave the body and the dangers as they were before the reflex stops :no-response." :spec (a/int-in 1 nil)}
   ::struck-s {:default 30 :doc "Seconds the hits the fight landed outlive the round (:struck-hits), so a refired reflex still counts them." :spec (a/num-in 0 nil)}
   ::struck-cap {:default 8 :doc "How many :struck-hits entries memory keeps." :spec (a/int-in 1 nil)}
   ::quote-mean-s {:default 20 :doc "Seconds an encounter is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(defn hold-radius [] (settings/get settings ::hold-radius))
(defn hit-window-ms [] (* 1000 (settings/get settings ::hit-window-s)))
(defn max-unchanged [] (settings/get settings ::max-unchanged))
(defn struck-policy [] {:cap (settings/get settings ::struck-cap) :ttl (* 1000 (settings/get settings ::struck-s))})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :use {:tools :attack}})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)

(defn weapons
  "The weapons of the call: its :use :tools, else the default substrings."
  [c]
  (or (ctx/use c :tools) combat/default-weapons))

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

(defn hold?
  "Whether the fight stays where it is: at night, with more than one hostile known (the ones in view are not all there are),
  or with any when the body is shut in a closed room or its own shelter and :stay-inside is on."
  [c]
  (let [p (:primitives c)]
    (boolean (and (sh-sense/night? p)
                  (or (> (count (danger-q/known-hostiles p (hold-radius) {})) 1)
                      (and (:stay-inside (:args c)) (or (sh-sense/shut-in-room? p) (sh-inside/sheltered-in c))))))))

(defn stay-under-roof?
  "Whether the body keeps its roof: :stay-inside on, night, a roof over the feet, and every danger near either only heard
  or a flyer (a phantom cannot get under a roof), none a creeper or an enderman. Fight-back then holds and the retreat does not run."
  [c hs]
  (let [p (:primitives c)]
    (boolean (and (:stay-inside (:args c))
                  (seq hs)
                  (sh-sense/night? p)
                  (sh-sense/roofed? p sh-sense/default-roof-height)
                  (every? #(or (combat/flyer? %) (not (danger-q/seen-only? %))) hs)
                  (not-any? #(or (combat/creeper? %) (combat/enderman? %)) hs)))))

(defn ^:async run-child
  "One call of the child for decision: :continue, :done, :declined (a fight that ended otherwise, its memory put back,
  or a retreat that ended :condition-failed: the other child may run), or the ending of a retreat that ended otherwise."
  [c decision {:keys [radius ranged-radius reserve]} hold]
  (let [child-args (case decision
                     :fight {:range radius :ranged-range ranged-radius :use {:tools (weapons c)}
                             :hold (or hold (hold? c))}
                     :flee {:radius radius :ranged-radius ranged-radius :use {:tools (weapons c)} :reserve reserve
                            :goal (:goal (ctx/mem c))})
        e (await (ctx/call c decision (child-jobs decision) child-args))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :done
      (= :fight decision) (do (when-let [m (:memory (:data e))] (ctx/update-mem! c assoc-in [:children :fight] m))
                              :declined)
      (= :condition-failed (:kind e)) :declined
      :else e)))

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

(defn struck-of
  "The :struck entry of mob id: the fight child's own, else the one an earlier round left in memory (:struck-hits)."
  [c id]
  (or (get-in (ctx/mem c) [:children :fight :struck id])
      (get-in (mem/latest (ctx/view c) :struck-hits) [:data id])))

(defn keep-struck!
  "Writes the fight child's :struck map to memory, so a refire after a lost round keeps the hits."
  [c]
  (when-let [struck (seq (get-in (ctx/mem c) [:children :fight :struck]))]
    (ctx/remember! c :struck-hits (into {} struck) (struck-policy))))

(defn note-hits
  "Pure: the times (ms) of hits taken in the last window-ms (only a flight counts them), given the earlier ones, now and the health before and now."
  [hits now before after window-ms]
  (let [hits (filterv #(< (- now %) window-ms) hits)]
    (cond-> hits (< after before) (conj now))))

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
       (empty? (ctx/since c :hurt (- (ctx/now c) (hit-window-ms))))))

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

(defn begun-fight?
  "Pure: whether the :hostile entry e (decision, mob, time t) is a fight against one of the mobs now near (names) that
  was decided or last struck at (the later of e's time and last-hit-t) within window-ms of now."
  [e last-hit-t names now window-ms]
  (boolean (and e (= :fight (get-in e [:data :decision]))
                (some #{(get-in e [:data :mob])} names)
                (> (max (:t e) (or last-hit-t 0)) (- now window-ms)))))

(defn fight-begun?
  "Whether this call or the last refire decided to fight these mobs: a reflex that refired mid-fight is the same fight,
  which lasts ::struck-s past its entry or its last hit; a fight against another mob is a fresh one."
  [c m near]
  (or (= :fight (:decision m))
      (begun-fight? (mem/latest (ctx/view c) :hostile)
                    (:t (mem/latest (ctx/view c) :struck-hits))
                    (map #(.-name %) near)
                    (ctx/now c)
                    (* 1000 (settings/get settings ::struck-s)))))

(defn ^:async respond [c near]
  (let [{:keys [reserve] :as a} (:args c)
        p (:primitives c)
        self (.self p)
        m (ctx/mem c)
        hits (note-hits (when (= :flee (:decision m)) (:hits m)) (ctx/now c) (:last-health m (.-health self)) (.-health self) (hit-window-ms))
        _ (ctx/update-mem! c assoc :hits hits :last-health (.-health self))
        chosen (if (some combat/enderman? near)
                   :flee
                   (cost/decide {:health (.-health self)
                               :fighting? (fight-begun? c m near)
                               :single? (= 1 (count near))
                               :cannot-outrun? (not (cost/outruns? (or (.-food self) 20) (map cost/mob-kind (remove combat/ranged? near))))
                               :creeper? (boolean (some combat/creeper? near))
                               :reserve reserve
                               :damage (cost/fight-damage {:weapon (combat/best-weapon p (weapons c))
                                                           :equipment (cost/equipment-of (.-equipment self))
                                                           :mobs (map #(mob-of p (struck-of c (.-id %)) %) near)})}))
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
      (keep-struck! c)
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
  (let [{:keys [x y z]} (sh-sense/feet p)
        walled? (fn [[dx dz]] (every? #(sh-sense/solid-at? p {:x (+ x dx) :y (+ y %) :z (+ z dz)}) [0 1]))]
    (and (sh-sense/roofed? p sh-sense/default-roof-height)
         (<= cover-sides (count (filter walled? [[1 0] [-1 0] [0 1] [0 -1]]))))))

(defn gap-possible?
  "Whether the gap job could stop the arrows but for cover: only ranged mobs near, building blocks and a tool that digs stone
  (the way out of a sealed doorway) carried, and the gap job not yet tried (tried?)."
  [c hs tried?]
  (let [p (:primitives c)]
    (boolean (and (not tried?)
                  (every? combat/ranged? hs)
                  (choose/filler c :build)
                  (tools/can-harvest? p "stone")))))

(defn gap-wanted?
  "Whether to stop arrows with a block rather than fight or flee: gap-possible? and the body in cover (covered?)."
  [c hs tried?]
  (boolean (and (gap-possible? c hs tried?) (covered? (:primitives c)))))

(defn cover-unseen?
  "Whether the gap job is possible and the body has not looked around yet while the cover it holds may be walls it has not seen
  (its view cone leaves the sides and back): it looks before it flees."
  [c hs tried?]
  (boolean (and (gap-possible? c hs tried?) (not (look/surveyed? c)) (not (covered? (:primitives c))))))

(defn ^:async gap!
  "One call of block-arrow-gap (never fetching): :continue or its ending's kind; nothing placed lets the round decide as usual.
  Tried once it has ended; a :continue (its look around) calls it again."
  [c]
  (let [e (await (ctx/call c :gap 'jobs.survival.block-arrow-gap {:radius (:ranged-radius (:args c)) :fetch false}))]
    (when-not (= :continue e) (ctx/update-mem! c assoc :gap-tried true))
    (if (= :continue e) :continue (:kind e))))

(defn hiding?
  "Whether the retreat child holds a refuge (sealed in, a pillar or a pit): a hostile it hides from is out of sight
  and has no way to the body, yet is still there."
  [c]
  (some? (get-in (ctx/mem c) [:children :flee :refuge])))


(defn done?
  "No real danger near and the retreat not hiding, before the start only: the round ends an encounter it began."
  [c]
  (let [n (count (near c))
        clear? (and (not (checks/running? c)) (zero? n) (not (hiding? c)))]
    (checks/goal clear? [:clear "no danger near"] [:danger (str n " dangers near")] {:units :hostiles :done 0 :left n})))

(defn quote-of
  "Find-out: an encounter lasts as long as the danger stays."
  [_c]
  {:uses {:job-time [0 (settings/get settings ::quote-mean-s) ##Inf]}})

(defn signature
  "What a call may change: the body's cell, its health (a fight that costs health is a response), the ids of the
  dangers near and what the fight child counted (hits landed, swings without damage, blocked walks, kills)."
  [c hs]
  {:health (.-health (.self (:primitives c)))
   :cell (let [{:keys [x y z]} (u/self-pos c)] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])
   :ids (set (map #(.-id %) hs))
   :fight (select-keys (get-in (ctx/mem c) [:children :fight]) [:struck :no-damage :blocked :killed])})

(defn fight-unsettled?
  "Whether a round that now sees no danger may be a blink: the fight child yielded :continue or the body was hurt within
  hit-window-ms (a mob knocked a cell off is still there). The round then looks once more before it ends."
  [c outcome]
  (boolean (or (= :continue outcome)
               (seq (ctx/since c :hurt (- (ctx/now c) (hit-window-ms)))))))

(defn ^:async round
  "One whole attempt: respond (or let the retreat hide on) while a danger is near; :nothing-possible when the retreat
  ends other than :done, or :no-response after max-unchanged calls in a row that changed nothing; never for the time a
  danger stays."
  [c]
  (when-not (contains? (ctx/mem c) :goal)
    (ctx/update-mem! c assoc :goal (or (:goal (:args c)) (goal/interrupted-goal (ctx/view c) (:root c)))))
  (loop [unchanged 0 looked? false]
    (let [hs (near c)
          before (signature c hs)
          outcome (cond
                    (hiding? c) (await (run-child c :flee (:args c) false))
                    (empty? hs) nil
                    (gap-wanted? c hs (:gap-tried (ctx/mem c))) (await (gap! c))
                    (cover-unseen? c hs (:gap-tried (ctx/mem c))) (await (look/look-around! c))
                    :else (await (respond c hs)))
          after (near c)]
      (if (contract/ending? outcome)
        (do (mute-heard! c hs)
            (ctx/end c (:kind outcome) (:cause outcome) (some->> (:text (:data outcome)) (hash-map :text))))
        (cond
          (= :left-heard outcome) (ctx/end c :done nil)
          (and (empty? after) (not (hiding? c)) (not (and (not looked?) (fight-unsettled? c outcome)))) (ctx/end c :done nil)
          (and (empty? after) (not (hiding? c))) (do (await (pace/pace!)) (recur unchanged true))
          :else
          (let [n (if (= before (signature c after)) (inc unchanged) 0)]
            (if (>= n (max-unchanged))
              (do (mute-heard! c hs)
                  (ctx/end c :nothing-possible :no-response {:text (str "a hostile is still near and " n " tries changed nothing")}))
              (do (await (pace/pace!)) (recur n false)))))))))
