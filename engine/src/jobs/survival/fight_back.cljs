(ns jobs.survival.fight-back
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Equip the best weapon, walk up to the nearest hostile within :range and hit it, at most one swing per :attack-gap-ms (nil: the held weapon's cooldown).
  Only a hostile the body has seen is fought: one only heard is judged by its direction and band (jobs.lib.danger), never
  walked to or hit. With only heard mobs in range it does not start (:no-target); one that goes unseen mid-fight is turned toward and the job declines, so the retreat takes over.
  With :hold the body stays where it is: a melee hostile beyond reach is waited for, until ::hold-s (15) seconds pass with no swing, then the job declines (a ranged one is still walked up to).
  A ranged hostile farther than ::ranged-chase (6) is not walked to while another is in range (the job declines).
  A hostile more than :leash blocks from the point the job started at is not chased: with none other in range the job is not started (:no-target); one that falls past the leash mid-fight ends the job as a decline, so the retreat takes over.
  Done (:already before the start) when no hostile is within :range, with the result {:killed [ids]}.
  Every other ending is a decline, so the retreat takes over: :nothing-possible with the cause :hold-timeout (the :hold wait ran out), :too-far, :heard-only, :given-up;
  its data are {:killed :struck :memory}, the job memory as it ended.
  The weapons are the call's :use :tools (default the weapons carried).
  A hostile the walk toward is blocked for three times is given up on, with a fight_unreachable warning, and no longer counts,
  unless the body lost health since the last blocked walk (the count then starts over).
  So is one that three swings in a row did no damage to (the attack result says hurt false), with a fight_no_damage warning.
  When no other hostile is in range the job declines, so respond-to-hostile retreats instead.
  A killed mob, or one whose id is in :skip, is not swung at again while its corpse is listed.
  Job memory (for the parent): :struck {id {:name :hits :health}} for hits that did damage, and :killed.")

(a/defargs args
  {:range {:doc "hostiles within this many blocks are fought" :spec (a/num-in 0 nil) :default 4}
   :ranged-range {:doc "ranged hostiles (skeletons and the like) within this many blocks are fought" :spec (a/num-in 0 nil) :default 16}
   :leash {:doc "hostiles farther than this from where the job started are not chased" :spec (a/num-in 0 nil) :default 20}
   :skip {:doc "entity ids already dead: not fought" :spec (a/coll-of number?) :default []}
   :attack-gap-ms {:doc "least time between swings; nil: the held weapon's cooldown" :spec (a/num-in 0 nil) :default nil}
   :hold {:doc "true: never walk toward a melee mob; strike one that comes within reach (night, with more hostiles about than the body sees)" :spec boolean? :default false}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a fight may take." :spec (a/num-in 0 nil)}
   ::hold-s {:default 15 :doc "Seconds a held fight waits without a swing for a mob to come within reach before it declines." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "How near the target the body stands to hit, in blocks." :spec (a/int-in 0 nil)}
   ::ranged-chase {:default 6 :doc "Farthest a ranged mob may be for the body to cross open ground to it while another hostile is in range; beyond it the fight declines." :spec (a/num-in 0 nil)}})
(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :use {:tools :attack}})

(def safe? ready/ok)

(def fit? ready/ok)

(defn hold-ms [] (* 1000 (settings/get settings ::hold-s)))
(defn reach [] (settings/get settings ::reach))
(defn ranged-chase [] (settings/get settings ::ranged-chase))

(defn blocked-count
  "mem's blocked-walk count for hostile id after one more walk blocked with the body at health: it starts over when the
  body lost health since the last blocked walk (a mob that keeps hurting it is not given up on)."
  [mem id health]
  (if (< health (get-in mem [:blocked-health id] ##-Inf))
    1
    (inc (get-in mem [:blocked id] 0))))

(defn given-up-in?
  "Whether mem says the walk towards hostile id has been blocked, or swings at it done no damage, n times."
  [mem id n]
  (or (>= (get-in mem [:blocked id] 0) n)
      (>= (get-in mem [:no-damage id] 0) n)))

(defn given-up?
  "Whether hostile e is given up on (given-up-in?)."
  [c e]
  (given-up-in? (ctx/mem c) (.-id e) (u/max-failures)))

(defn start-of
  "Where the job started: the first round's position (the current one before it)."
  [c]
  (or (:start (ctx/mem c)) (u/self-pos c)))

(defn in-leash?
  "Whether hostile e stands within :leash of the start point."
  [c e]
  (<= (u/dist (start-of c) (u/pos-of (.-pos e))) (:leash (:args c))))

(defn in-range
  "The hostiles within :range (ranged ones within :ranged-range, a heard one by its band): the visible
  ones nearest first, then the hidden melee ones (a ranged mob without a line of fire, by jobs.lib.danger/danger? as the hostile-near trigger, is dropped)."
  [c]
  (let [{:keys [range ranged-range skip]} (:args c)
        dead (into (set skip) (:killed (ctx/mem c)))
        all (->> (danger-q/known-hostiles (:primitives c) range {:ranged-radius (max range ranged-range)})
                 (remove #(contains? dead (.-id %)))
                 (remove #(and (combat/ranged? %) (not (danger-q/danger? (:primitives c) %)))))]
    (into (filterv #(.-visible %) all) (remove #(.-visible %)) all)))

(defn targets
  "The hostiles in range the body has seen, within the leash and not given up on, nearest first."
  [c]
  (->> (in-range c) (filter danger-q/seen-only?) (filter #(in-leash? c %)) (remove #(given-up? c %))))

(defn can-start?
  "A hostile to fight, else :no-target."
  [c]
  (if (seq (targets c)) (contract/ok :target "a hostile to fight") (contract/later :no-target "no hostile to fight in range")))

(defn done?
  "Done once no hostile is in range; the kills so far are the result."
  [c]
  (let [n (count (in-range c))
        killed (vec (:killed (ctx/mem c)))]
    (if (zero? n)
      (contract/ok :clear "no hostile in range" :units :hostiles :done (count killed) :left 0 :result {:killed killed})
      (contract/later :fighting (str n " hostiles in range") :units :hostiles :done (count killed) :left n))))

(defn quote-of [_c] {:uses {:job-time [0 10 60]}})

(defn weapons
  "The weapons of the call: its :use :tools, else the default substrings."
  [c]
  (or (ctx/use c :tools) combat/default-weapons))

(defn note-blocked!
  "Count a blocked walk towards target; warn once when it is given up on."
  [c target]
  (let [health (.-health (.self (:primitives c)))
        n (blocked-count (ctx/mem c) (.-id target) health)]
    (ctx/update-mem! c #(-> % (assoc-in [:blocked (.-id target)] n)
                            (assoc-in [:blocked-health (.-id target)] health)))
    (when (= n (u/max-failures))
      (ctx/emit! c :fight_unreachable :warn {:text (str "cannot reach the " (.-name target) ", giving up on it")}))))

(defn note-hit!
  "Record a landed hit on target in :struck {id {:name :hits :health}}, so a
  parent can tell how close the mob is to dying;
  :health is the attack's reported health, when the layer knows it."
  [c target result]
  (let [h (.-health result)]
    (ctx/update-mem! c update-in [:struck (.-id target)]
                     (fn [m] (cond-> (-> (or m {}) (assoc :name (.-name target)) (update :hits (fnil inc 0)))
                               (number? h) (assoc :health h))))))

(defn note-swing!
  "Count a swing at target that did no damage in a row (reset by one that did); warn once when it is given up on."
  [c target result]
  (if (.-hurt result)
    (do (ctx/update-mem! c assoc-in [:no-damage (.-id target)] 0)
        (note-hit! c target result))
    (let [n (inc (get-in (ctx/mem c) [:no-damage (.-id target)] 0))]
      (ctx/update-mem! c assoc-in [:no-damage (.-id target)] n)
      (when (= n (u/max-failures))
        (ctx/emit! c :fight_no_damage :warn {:text (str "swings at the " (.-name target) " do no damage, giving up on it")})))))

(def chase-timeout-s
  "Bound of one walk toward the mob: it moves, so the walk aims again at where it is now this often."
  10)

(def chase-gap-ms "How long a held body waits for a mob to come before it looks again." 200)

(defn ^:async swing!
  "Walk into reach if needed, then face and hit target once. Resolves to the
  walk's outcome: :there, :partial, :blocked or :held (waited for the mob)."
  [c target]
  (let [tpos (u/pos-of (.-pos target))
        far? (> (u/dist (u/self-pos c) tpos) (reach))
        hold? (and (:hold (:args c)) (not (combat/ranged? target)))
        r (cond
            (not far?) :there
            hold? (do (await (combat/wait-gap! c (ctx/now c) chase-gap-ms))
                      :held)
            :else (await (near/go-near! c tpos 2 {:spend {:job-time chase-timeout-s} :dangers false :ways #{:walk :clear-seal} :look-round false})))]
    (when (= :blocked r) (note-blocked! c target))
    (when (= :there r)
      (ctx/update-mem! c assoc :last-attack (ctx/now c))
      (await (ctx/act c :look #js {:pos #js {:x (:x tpos) :y (+ 1 (:y tpos)) :z (:z tpos)}}))
      (let [a (await (ctx/act c :attack #js {:id (.-id target)}))]
        (when (= "hit" (.-status a)) (note-swing! c target a))
        (when (= "killed" (.-status a)) (ctx/update-mem! c update :killed (fnil conj []) (.-id target)))))
    r))

(defn too-far-to-chase?
  "Whether target is a ranged mob beyond ranged-chase while another hostile is in range."
  [c target]
  (and (combat/ranged? target)
       (> (danger-q/mob-distance (:primitives c) target) (ranged-chase))
       (some #(not= (.-id %) (.-id target)) (in-range c))))

(defn declined
  "The ending of a fight that stops with hostiles left, for the retreat to take over: kind and cause, and the job
  memory with the kills and hits a caller reads."
  [c kind cause]
  (let [m (dissoc (ctx/mem c) :contract)]
    (ctx/end c kind cause {:killed (vec (:killed m)) :struck (:struck m) :memory m})))

(defn held-idle?
  "Whether a held fight has waited hold-ms since its last swing (or its start) with no mob coming within reach."
  [c]
  (let [m (ctx/mem c)]
    (and (:hold (:args c))
         (> (- (ctx/now c) (or (:last-attack m) (:started-at m))) (hold-ms)))))

(defn ^:async step
  "One step of the fight: look at a heard mob, wait out the swing gap, or swing once. :again, :continue or an ending."
  [c]
  (let [p (:primitives c)
        target (first (targets c))
        last-attack (:last-attack (ctx/mem c))]
    (when-not (:start (ctx/mem c)) (ctx/update-mem! c assoc :start (u/self-pos c) :started-at (ctx/now c)))
    (cond
      (and target (held-idle? c)) (declined c :nothing-possible :hold-timeout)
      (and target (too-far-to-chase? c target)) (declined c :nothing-possible :too-far)
      (and last-attack (< (.-health (.self p)) (:health0 (ctx/mem c)))) :continue
      (nil? target) (if-let [heard (first (remove danger-q/seen-only? (in-range c)))]
                      (do (await (ctx/act c :look (let [{:keys [x y z]} (danger-q/mob-pos p heard)] #js {:pos #js {:x x :y (+ 1 y) :z z}})))
                          (declined c :nothing-possible :heard-only))
                      (if (seq (in-range c)) (declined c :nothing-possible :given-up) (ctx/end c :done nil {:killed (vec (:killed (ctx/mem c)))})))
      (combat/within-gap? c) (do (await (combat/wait-gap! c last-attack (combat/gap-ms c))) :again)
      :else
      (do (await (combat/equip-best! c (combat/best-weapon p (weapons c))))
          (await (swing! c target))
          (cond
            (empty? (in-range c)) (ctx/end c :done nil {:killed (vec (:killed (ctx/mem c)))})
            (empty? (targets c)) (declined c :nothing-possible :given-up)
            :else :again)))))

(defn ^:async round
  "The whole fight: steps until no hostile is left (:done) or the retreat should take over (an ending other than :done)."
  [c]
  (ctx/update-mem! c assoc :health0 (.-health (.self (:primitives c))))
  (await (pace/steps! c #(step c))))
