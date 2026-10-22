(ns jobs.survival.respond.options
  "The cornered options of a flight (jobs.survival.respond-flight): the fight, the back-off and the stand (jobs.survival.stand-in), and the settings of the retreat jobs."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.hurt :as hurt]
            [jobs.lib.body :as body]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.escape :as escape]
            [jobs.lib.hostile-fight :as hostile]
            [jobs.lib.look :as look]
            [jobs.lib.pillar :as pl]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as result]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]
            [jobs.lib.shot :as shot]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.respond.pillar :as pillar]
            [jobs.survival.respond.refuge :as refuge]
            [jobs.lib.flee-walk :as walk]))

(def tool-weapons
  "Item name substrings a cornered body with no weapon and no seal swings: any of them beats the fist."
  ["_pickaxe" "_shovel" "_hoe"])



(defn block-at-fn
  "pos -> the block name the body sees or remembers there. A cell it has not seen reads as rock (behind the body or a
  wall is no free way until it looks: glance-away!); nil (open: the walk finds out) when the cell is not loaded."
  [p]
  (let [at (access/sensed-at p access/hidden-guess)]
    (fn [{:keys [x y z]}] (at [x y z]))))


(a/defargs settings
  {::lost-memory-ms {:default 30000 :doc "Milliseconds a mob the flight lost (its :threat entry) still counts, at the place it was last known, when choosing where to flee; a creeper counts triple." :spec (a/int-in 0 nil)}
   ::fist-sweeps {:default 1 :doc "Times a body with no weapon, every cornered option failed, looks for a way to walk before it punches a hostile it would not beat; 0: it punches at once." :spec (a/int-in 0 nil)}
   ::hit-reach {:default 3 :doc "Blocks within which a melee hostile counts as the one that hit the body." :spec (a/num-in 0 nil)}
   ::glance-reach {:default 3 :doc "Blocks along a direction to the feet cell a glance looks at." :spec (a/int-in 0 nil)}
   ::glance-memory {:default 24 :doc "Most glanced cells a flight remembers." :spec (a/int-in 0 nil)}
   ::cover-gap {:default 5 :doc "Blocks within which a melee chaser keeps a low body from taking cover where it stands." :spec (a/num-in 0 nil)}
   ::close-radius {:default 8 :doc "Blocks within which a hostile that attacks at close range (a creeper too, not a ranged mob or an enderman) bounds a flee step to close-leg-s, decided at the step's start." :spec (a/num-in 0 nil)}
   ::close-leg-s {:default 1 :doc "Seconds a flee step may search or walk while a close-range hostile is within close-radius, or the body was hurt: a body that stands searching there is hit for the whole leg." :spec (a/num-in 0 nil)}
   ::hurt-window-s {:default 5 :doc "Seconds after the body lost health in which a flee step is bounded to close-leg-s, even with no hostile seen in reach (an unseen attacker hits as hard)." :spec (a/num-in 0 nil)}
   ::fire-hits {:default 1 :doc "Hits by a ranged mob (the damage's attacker, or an arrow) within hurt-window-s that put a cornered body under fire: cover comes before any fight, and a shooter it has not seen is never weighed as beaten. 0: never." :spec (a/int-in 0 nil)}
   ::shot-hits {:default 2 :doc "Hits by a ranged mob within hurt-window-s that make a walking body take cover (seal, or break the line of fire) instead of walking on under fire it cannot answer. 0: it keeps walking." :spec (a/int-in 0 nil)}
   ::cover-tries {:default 2 :doc "Cornered steps under fire in which cover (seal, back off) is tried before an armed body fights a melee mob pressing it: a mob standing in the cells to wall keeps every seal failing, and each back-off step makes the covers open again." :spec (a/int-in 1 nil)}
   ::beaten-hits {:default 2 :doc "Hits by a melee mob near (the damage names one within close-radius; not a creeper or an enderman) within hurt-window-s that show the flight is failing: the body fights it first, armed or not. 0: never." :spec (a/int-in 0 nil)}
   ::press-gap {:default 2 :doc "Blocks within which a melee mob (not a creeper or an enderman) makes a cornered armed body fight before it builds a seal or pillar, hit or not: the mob strikes before a block is placed and stands in the cells to fill." :spec (a/num-in 0 nil)}
   ::hold-cap {:default 60 :doc "Holds (one wait-ms each) a flight makes in water or under a roof against an angry enderman before it tries the next option (seal, pit, pocket, fight); 0: none." :spec (a/int-in 0 nil)}
   ::refuge-gap {:default 10 :doc "Blocks within which a lone melee hostile the body cannot outfight is pillared against (else sealed against) before any flee step, when blocks are carried; 0 never." :spec (a/num-in 0 nil)}
   ::flight-timeout-s {:default 5 :doc "Seconds bounding one step of the flight: the mob moves, so the next step aims again." :spec (a/num-in 0 nil)}
   ::enderman-close {:default 3 :doc "Blocks within which an enderman counts as angry though it has not hurt the body yet (a calm one does not come this near)." :spec (a/num-in 0 nil)}
   ::creeper-keep {:default 5 :doc "Blocks within which a creeper stops the fight walking toward a mob on its side: the fight swings from where the body stands, never a step nearer the creeper." :spec (a/num-in 0 nil)}})
(defn flight-timeout-s [] (settings/get settings ::flight-timeout-s))
(defn enderman-close [] (settings/get settings ::enderman-close))
(defn creeper-keep [] (settings/get settings ::creeper-keep))

(defn fist-sweeps [] (settings/get settings ::fist-sweeps))
(defn hit-reach [] (settings/get settings ::hit-reach))

(defn hits-total
  "The hits an attack ending's (or prior's) :struck counts over all mobs."
  [struck] (reduce + 0 (map :hits (vals struck))))

(defn ^:async fight!
  "Fight back with the best of weapons (the fist when none is carried) whatever
  the health, kept while the hostile stays close: :again, nil when the fight
  cannot reach any hostile, or every target is already dead (the option failed this flight, else the flight spins without time passing; hits new in this call reset the sweep count). reach-only?: only a hostile within hit-reach is
  swung at, none walked to. A fight that ends other than done keeps its memory (its given-up counts) for the next."
  [c weapons reach-only?]
  (let [radius (danger-q/hostile-radius)
        ranged-radius (danger-q/ranged-radius)
        radius (if reach-only? (min radius (hit-reach)) radius)
        ranged-radius (if reach-only? radius ranged-radius)]
    (ctx/update-mem! c assoc :cornered true)
    (ctx/update-mem! c #(cond-> % (:cornered (:children %)) (update-in [:children :cornered] dissoc :start)))
    (let [hits-before (hits-total (:struck (:fight-prior (ctx/mem c))))
          r (await (ctx/call c :cornered 'jobs.combat.attack
                             {:targets :hostiles :radius radius :ranged-radius ranged-radius :use {:tools (mapv name weapons)}
                              :skip (flight/dead-ids c) :prior (:fight-prior (ctx/mem c)) :checks {:safe? '(always) :fit? '(always)}}))
          ended (when (contract/ending? r) r)]
      (ctx/update-mem! c update :dead #(into (vec %) (:killed (:data ended))))
      (if (and ended (or (not= :done (:kind ended)) (= :already (:cause ended))))
        (do (when (:struck (:data ended)) (ctx/update-mem! c assoc :fight-prior (select-keys (:data ended) [:killed :given-up :names :struck])))
            (flight/tried! c :fight)
            (when (> (hits-total (:struck (:data ended))) hits-before)
              (ctx/update-mem! c assoc :sweeps 0))
            nil)
        :again))))


;; ------------------------------------------------------------------ cornered: the safest option

(defn outrun?
  "Whether the body gets away on foot from every one of hostiles that attacks at close range (jobs.lib.cost/outruns?,
  at its food). Creepers, endermen and ranged mobs have their own rules."
  [c hostiles]
  (cost/outruns? (room-lib/food (:primitives c))
                 (->> hostiles
                      (remove #(or (combat/creeper? %) (combat/enderman? %) (combat/ranged? %)))
                      (map cost/mob-kind))))

(defn near-hostiles
  "The hostiles a cornered body weighs: within :radius, ranged ones within :ranged-radius, the dead skipped."
  [c]
  (let [radius (danger-q/hostile-radius)
        ranged-radius (danger-q/ranged-radius)]
    (flight/near-known (:primitives c) (set (flight/dead-ids c)) radius ranged-radius)))

(defn angry-enderman?
  "Whether one of hostiles is an enderman that is angry (the client's flag), hurt the body this flight (its health is under the flight's start) or
  stands within enderman-close: a calm one is walked away from like any mob, never sealed against. Once found angry it stays so for the flight
  while one is known within follow range (the client's flag does not hold, and the enderman may teleport out of the near radius: the body must not leave the water it stepped into)."
  [c hostiles]
  (let [p (:primitives c)
        hurt? (boolean (some-> (:flight-start (ctx/mem c)) (->> (body/mob-hit-since? (ctx/view c)))))
        angry? (boolean (or (and (:angry-enderman (ctx/mem c)) (some combat/enderman? (concat hostiles (flight/known-chasers p))))
                            (some #(and (combat/enderman? %) (or hurt? (true? (.-angry %)) (<= (danger-q/mob-distance p %) (enderman-close)))) hostiles)))]
    (when angry? (ctx/update-mem! c assoc :angry-enderman true))
    angry?))

(defn hitters
  "Of hostiles, the melee ones (not a creeper or an enderman) within hit-reach."
  [c hostiles]
  (filterv #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                 (<= (danger-q/mob-distance (:primitives c) %) (hit-reach)))
           hostiles))

(defn close-radius [] (settings/get settings ::close-radius))

(defn hurt-by
  "The causes of a :hurt memory entry (the raw body event: its attacker's name, else the damage's cause)."
  [e]
  (hurt/causes-of (:data e)))

(defn hit-on-the-run!
  "Whether a melee chaser hit the body since the previous flight step: health lost with a hitter near, or a hurt entry
  naming a melee hostile within close-radius (a fed body heals the hit within a second, so health alone misses it). A chaser
  that hits it cannot be outrun. Noted in job memory (:hit-on-the-run), with the health, for the cornered options' odds."
  [c]
  (let [p (:primitives c)
        health (.-health (.self p))
        {:keys [step-health step-at]} (ctx/mem c)
        now (ctx/now c)
        near (near-hostiles c)
        names (->> near
                   (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                                 (<= (danger-q/mob-distance p %) (close-radius))))
                   (map #(.-name %))
                   set)
        named? (and step-at (some #(some names (hurt-by %)) (ctx/since c :hurt step-at)))
        entries (when step-at (ctx/since c :hurt step-at))
        mob-or-unlogged? (or (empty? entries) (some #(hurt/mob-hit? (:data %)) entries))
        hit? (boolean (or (and step-health (< health step-health) (seq (hitters c near)) mob-or-unlogged?) named?))]
    (ctx/update-mem! c assoc :step-health health :step-at now :hit-on-the-run hit?)
    hit?))

(defn fight-wins?
  "Whether fighting hostiles is expected to leave :reserve health (jobs.lib.hostile-fight/wins?, as the response weighs)."
  [c hostiles]
  (hostile/wins? c hostiles))

(defn back-off-target
  "A cell to back off to: of the ends of the open cells (escape/up to 2, see walk-cells) in the eight compass directions,
  the farthest from threat, when it gains at least a block on it and passes no hazard; nil when none does."
  [block-at from threat hazards]
  (let [now (u/dist from threat)]
    (->> (range 0 360 45)
         (keep #(peek (walk/walk-cells block-at from (walk/rotate [1 0] %) 2)))
         (remove #(walk/near-hazard? hazards from %))
         (filter #(>= (u/dist % threat) (inc now)))
         (sort-by #(- (u/dist % threat)))
         first)))

(defn glance-reach [] (settings/get settings ::glance-reach))

(defn glance-memory [] (settings/get settings ::glance-memory))

(defn cover-gap [] (settings/get settings ::cover-gap))

(defn refuge-gap [] (settings/get settings ::refuge-gap))

(defn press-gap [] (settings/get settings ::press-gap))

(defn close-leg-s [] (settings/get settings ::close-leg-s))

(defn lost-memory-ms [] (settings/get settings ::lost-memory-ms))

(defn ^:async leg-step!
  "One flee step to target as a jobs.movement.run-away child of at most leg-s seconds (y-margin (16) blocks past body and target bound its route): true when the body arrived or got on a leg nearer, false when go-to gave up, waited or was declined (mem :leg-timed-out: it ran out of the leg's seconds unhurt)."
  ([c slot target range leg-s] (await (leg-step! c slot target range leg-s (respond.goal/leg-margin))))
  ([c slot target range leg-s y-margin]
  (let [from (u/self-pos c)
        t0 (ctx/now c)
        health0 (.-health (.self (:primitives c)))
        r (await (ctx/call c slot 'jobs.movement.run-away
                           (cond-> {:pos target :range range :leg-s leg-s} y-margin (assoc :y-margin y-margin))))
        res (when (contract/ending? r) (:data r))
        _ (when (= :goal-not-standable (result/why r)) (ctx/update-mem! c update :refused (fnil conj #{}) (mapv target [:x :y :z])))
        s (/ (- (ctx/now c) t0) 1000)
        d (u/dist from (u/self-pos c))
        _ (ctx/update-mem! c assoc :leg-timed-out (and (= :limit-reached (:kind r)) (>= (.-health (.self (:primitives c))) health0)))]
    (ctx/emit! c :retreat.step-speed :debug {:slot slot :blocks (/ (js/Math.round (* 10 d)) 10) :seconds (/ (js/Math.round (* 10 s)) 10)
                                             :bps (when (pos? s) (/ (js/Math.round (* 10 (/ d s))) 10))})
    (boolean (or (:arrived res) (:nearer res))))))

(defn step! [c slot target range] (leg-step! c slot target range (flight-timeout-s)))

(defn ^:async back-off!
  "Step back from the place from (back-off-target): :again, nil when there is no such cell or the walk is blocked."
  [c from]
  (when-let [target (back-off-target (block-at-fn (:primitives c)) (u/self-pos c) from
                                     (keep (comp :pos :data) (ctx/entries c :hazard)))]
    (if-not (await (step! c :back-off target 0))
      (do (flight/tried! c :back-off) nil)
      :again)))

(defn ^:async hold!
  "One hold (a pause of flight/wait-ms) in a spot an enderman cannot use: :again, or nil once the flight has held
  hold-cap times (the option is then tried! and the next one of the order comes)."
  [c option why]
  (if (>= (:holds (ctx/mem c) 0) (settings/get settings ::hold-cap))
    (do (flight/tried! c option) nil)
    (do (ctx/update-mem! c update :holds (fnil inc 0))
        (await (child/pause! c (/ (flight/wait-ms) 1000) why))
        :again)))

(defn in-water?
  "Whether the body's feet cell reads as water."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    (= "water" ((block-at-fn (:primitives c)) {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)}))))

(defn hurt-window-s [] (settings/get settings ::hurt-window-s))

(defn recently-hurt?
  "Whether the body lost health within hurt-window-s."
  [c]
  (pos? (ctx/count-in c :hurt (* 1000 (hurt-window-s)))))

(defn stand-leg-s
  "Seconds one leg to a place to stand may take: close-leg-s once the body was hurt (hurt?), else flight-timeout-s; a search that stands still is hit for its whole leg."
  [hurt?]
  (if hurt? (close-leg-s) (flight-timeout-s)))

(defn ^:async stand!
  "Walk to and stand in a place of kind where (jobs.survival.stand-in: :water, :roof or :dry), a leg a step: :again while it
  walks, and once it stands there a short hold (hold!, why; no hold without a why). nil when there is none or the walk is
  blocked (the option is tried! then)."
  [c option where why]
  (let [e (await (ctx/call c option 'jobs.survival.stand-in {:where where :leg-s (stand-leg-s (recently-hurt? c))}))]
    (cond
      (= :continue e) :again
      (not= :done (:kind e)) (do (flight/tried! c option) nil)
      why (await (hold! c option why))
      :else :again)))

(defn ^:async blocked!
  "Every option failed: forget them all, warn once a flight, pause a moment (why cornered) and go on: the next step
  tries them again from the start (the danger still stands); no pause while the body is being hit or a creeper is near (it closes in
  and blows while the body stands, so the next step must try the fight or a walk at once). The second sweep in a row ends the flight :cannot-escape."
  [c why]
  (ctx/update-mem! c #(-> % (dissoc :tried) flight/count-sweep))
  (when-not (:blocked-warned (ctx/mem c))
    (ctx/update-mem! c assoc :blocked-warned true)
    (ctx/emit! c :retreat_blocked :warn {:text (str why "; every escape failed, trying them again")}))
  (when-not (or (recently-hurt? c) (some combat/creeper? (near-hostiles c)))
    (await (child/pause! c (/ (flight/wait-ms) 1000) "cornered")))
  (if (flight/cannot-escape? (ctx/mem c)) (flight/end-flight! c :cannot-escape) :again))
