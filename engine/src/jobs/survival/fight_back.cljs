(ns jobs.survival.fight-back
  (:require [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Equip the best weapon, walk up to the nearest hostile within :range and hit it, at most one swing per :attack-gap-ms.
  Only a hostile the body has seen is fought: one only heard is judged by its direction and band (jobs.lib.danger), never
  walked to or hit; the body turns toward it and the job declines, so the retreat takes over.
  A hostile more than :leash blocks from the point the job started at is not chased (the job declines, so the retreat takes over).
  Declines when health is below :min-health.
  Ends when no hostile is within :range, with the result {:killed [ids]}.
  A hostile the walk toward is blocked for three times is given up on, with a fight_unreachable warning, and no longer counts.
  So is one that three swings in a row did no damage to (the attack result says hurt false), with a fight_no_damage warning.
  When no other hostile is in range the job declines, so respond-to-hostile retreats instead.
  A killed mob, or one whose id is in :skip, is not swung at again while its corpse is listed.
  Job memory (for the parent): :struck {id {:name :hits :health}} for hits that did damage, and :killed.")

(def args
  {:range {:doc "hostiles within this many blocks are fought" :default 4}
   :ranged-range {:doc "ranged hostiles (skeletons and the like) within this many blocks are fought" :default 16}
   :leash {:doc "hostiles farther than this from where the job started are not chased" :default 20}
   :min-health {:doc "decline below this health" :default 8}
   :weapons {:doc "item name substrings that count as weapons" :default combat/default-weapons}
   :skip {:doc "entity ids already dead: not fought" :default []}
   :attack-gap-ms {:doc "least time between swings" :default 600}})

(def reach 3)

(defn given-up?
  "Whether the walk towards hostile e has been blocked, or swings at it done no damage, u/max-failures times."
  [c e]
  (or (>= (get-in (ctx/mem c) [:blocked (.-id e)] 0) (u/max-failures))
      (>= (get-in (ctx/mem c) [:no-damage (.-id e)] 0) (u/max-failures))))

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

(defn check [c]
  (let [health (.-health (.self (:primitives c)))]
    (cond
      (< health (:min-health (:args c))) (ctx/wait c {:reason :low-health :health health :min (:min-health (:args c))})
      (empty? (targets c)) (ctx/wait c {:reason :no-target})
      :else true)))

(defn note-blocked!
  "Count a blocked walk towards target; warn once when it is given up on."
  [c target]
  (let [n (inc (get-in (ctx/mem c) [:blocked (.-id target)] 0))]
    (ctx/update-mem! c assoc-in [:blocked (.-id target)] n)
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

(defn ^:async swing!
  "Walk into reach if needed, then face and hit target once. Resolves to the
  walk's outcome: :there, :partial or :blocked."
  [c target]
  (let [tpos (u/pos-of (.-pos target))
        r (if (> (u/dist (u/self-pos c) tpos) reach)
            (await (near/go-near! c tpos 2 {:leg-s chase-timeout-s :dangers false :escalate false :look-round false}))
            :there)]
    (when (= :blocked r) (note-blocked! c target))
    (when (= :there r)
      (ctx/update-mem! c assoc :last-attack (ctx/now c))
      (await (ctx/act c :look #js {:pos #js {:x (:x tpos) :y (+ 1 (:y tpos)) :z (:z tpos)}}))
      (let [a (await (ctx/act c :attack #js {:id (.-id target)}))]
        (when (= "hit" (.-status a)) (note-swing! c target a))
        (when (= "killed" (.-status a)) (ctx/update-mem! c update :killed (fnil conj []) (.-id target)))))
    r))

(defn ^:async step
  "One step of the fight: look at a heard mob, wait out the swing gap, or swing once. :again, :continue, :done or :declined."
  [c]
  (let [{:keys [weapons attack-gap-ms min-health]} (:args c)
        p (:primitives c)
        target (first (targets c))
        last-attack (:last-attack (ctx/mem c))]
    (when-not (:start (ctx/mem c)) (ctx/update-mem! c assoc :start (u/self-pos c)))
    (cond
      (and last-attack (< (.-health (.self p)) min-health)) :declined
      (and last-attack (< (.-health (.self p)) (:health0 (ctx/mem c)))) :continue
      (nil? target) (if-let [heard (first (remove danger-q/seen-only? (in-range c)))]
                      (do (await (ctx/act c :look (let [{:keys [x y z]} (danger-q/mob-pos p heard)] #js {:pos #js {:x x :y (+ 1 y) :z z}})))
                          :declined)
                      (if (seq (in-range c)) :declined :done))
      (and last-attack (< (- (ctx/now c) last-attack) attack-gap-ms)) (do (await (combat/wait-gap! c last-attack attack-gap-ms)) :again)
      :else
      (do (await (combat/equip-best! c (combat/best-weapon p weapons)))
          (await (swing! c target))
          (cond
            (empty? (in-range c)) (do (ctx/result! c {:killed (vec (:killed (ctx/mem c)))}) :done)
            (empty? (targets c)) :declined
            :else :again)))))

(defn ^:async round
  "The whole fight: steps until no hostile is left (:done) or the retreat should take over (:declined)."
  [c]
  (ctx/update-mem! c assoc :health0 (.-health (.self (:primitives c))))
  (await (pace/steps! c #(step c))))
