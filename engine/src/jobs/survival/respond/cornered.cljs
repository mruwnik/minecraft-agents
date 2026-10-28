(ns jobs.survival.respond.cornered
  "The cornered step of a flight: the conditions that rank its options (pressed, beaten, under fire, boxed in) and cornered!, which takes the first option it can."
  (:require [engine.settings :as settings]
            [jobs.survival.respond-choose :refer [escape-order by-price with-zone-pillar]]
            [jobs.survival.respond.options :as rc :refer [settings near-hostiles hitters arriving close-radius hurt-by press-gap hit-reach fist-sweeps tool-weapons fight! outrun? angry-enderman? cornered-answers shots-in stand! back-off! blocked! recently-hurt? hurt-window-s]]
            [engine.args :as a]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.breath :as breath]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.hurt :as hurt]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.escape :as escape]
            [jobs.lib.hostile-fight :as hostile]
            [jobs.lib.look :as look]
            [jobs.lib.pillar :as pl]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.result :as result]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.respond.state :as flight]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.lib.rooms :as room-lib]
            [jobs.survival.respond.pillar :as pillar]
            [jobs.survival.respond.refuge :as refuge]
            [jobs.lib.flee-walk :as walk]))


(defn fist-hold?
  "Pure: whether a cornered body walks on if it can once more before it fights: no weapon carried (armed? false), the odds
  not won (win?) and fewer than fist-sweeps sweeps done."
  [{:keys [armed? win? sweeps fist-sweeps]}]
  (and (not armed?) (not win?) (< sweeps fist-sweeps)))

(defn reach-only?
  "Pure: whether the fight swings only at what is within reach and walks to nothing: blind? (a body that cannot see), the
  nearest of hostiles by dist is a creeper, or a creeper within creeper-keep lies on the same side as a hostile (offset: [dx dz] from the body):
  a step toward that hostile would bring the body nearer the creeper; or a creeper stands within creeper-keep of a hostile (the walk
  to that hostile ends in the blast, wherever the body is)."
  [blind? hostiles dist offset creeper-keep]
  (let [creepers (filter combat/creeper? hostiles)
        dot (fn [[ax az] [bx bz]] (+ (* ax bx) (* az bz)))
        apart (fn [a b] (let [[ax az] (offset a) [bx bz] (offset b)] (js/Math.hypot (- ax bx) (- az bz))))]
    (boolean (or blind?
                 (some-> (seq hostiles) (->> (apply min-key dist)) combat/creeper?)
                 (some (fn [c] (and (<= (dist c) creeper-keep)
                                    (some #(and (not (combat/creeper? %)) (pos? (dot (offset %) (offset c)))) hostiles)))
                       creepers)
                 (some (fn [c] (some #(and (not (combat/creeper? %)) (<= (apart % c) creeper-keep)) hostiles)) creepers)))))

(defn mob-offset
  "[dx dz] from the body to hostile e."
  [c e]
  (let [from (u/pos-of (.-pos (.self (:primitives c))))
        to (danger-q/mob-pos (:primitives c) e)]
    [(- (:x to) (:x from)) (- (:z to) (:z from))]))

(defn ^:async fight-or-wait!
  "fight!, except that a reach-only fight with no hostile within hit-reach fails the option at once: attack would sit waiting for one to come."
  [c weapons reach-only? hostiles]
  (if (and reach-only? (not-any? #(<= (danger-q/mob-distance (:primitives c) %) (hit-reach)) hostiles))
    (do (flight/tried! c :fight) nil)
    (await (fight! c weapons reach-only?))))

(defn ^:async fist-look!
  "Count a sweep and let the flight step again: it walks away if a way has opened, else the options (all failed) come back here."
  [c]
  (ctx/update-mem! c flight/count-sweep)
  :again)

(defn pressed?
  "Whether an armed body has a melee mob (hitters) on it: one within press-gap, or one within hit-reach and the body
  was hit this step or within hurt-window-s (a hit in an earlier step counts: the step since may have gained nothing)."
  [c hostiles]
  (let [near (hitters c hostiles)
        p (:primitives c)]
    (boolean (and (seq near)
                  (combat/best-weapon p (combat/weapons c))
                  (or (:hit-on-the-run (ctx/mem c))
                      (recently-hurt? c)
                      (some #(<= (danger-q/mob-distance p %) (press-gap)) near))))))

(defn plug-continues?
  "Whether a plug begun here goes on (refuge/plugging-here?): not while an armed body is pressed, the fight comes first."
  [c]
  (and (refuge/plugging-here? c) (not (pressed? c (near-hostiles c)))))

(defn beaten-hits [] (settings/get settings :jobs.survival.respond.options/beaten-hits))

(defn beaten?
  "Whether melee mobs within close-radius hit the body beaten-hits times within hurt-window-s: knocked back and
  stepping away, it is out of hit-reach at each step, yet the flight is failing."
  [c]
  (let [n (beaten-hits)
        p (:primitives c)
        names (->> (near-hostiles c)
                   (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                                 (<= (danger-q/mob-distance p %) (close-radius))))
                   (map #(.-name %))
                   set)]
    (and (pos? n)
         (seq names)
         (<= n (->> (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s))))
                    (filter #(some names (hurt-by %)))
                    (map #(:hits (:data %) 1))
                    (reduce + 0))))))

(defn beaten-fights?
  "Pure: whether a beaten body (beaten?) fights before it seals or pillars: armed, or with no blocks to wall with. An
  unarmed body that carries blocks builds first (a spider cannot be fought with fists)."
  [{:keys [beaten? armed? blocks?]}]
  (boolean (and beaten? (or armed? (not blocks?)))))

(defn fire-hits [] (settings/get settings :jobs.survival.respond.options/fire-hits))

(defn under-fire?
  "Whether a ranged mob hit the body fire-hits times within hurt-window-s, seen or not."
  [c]
  (let [n (fire-hits)]
    (and (pos? n)
         (<= n (transduce (map shots-in) + 0 (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s)))))))))

(defn shot-on-the-run?
  "Whether a ranged mob hit the body shot-hits times within hurt-window-s and it carries blocks to wall with: walking on only feeds the shooter, so the flight takes cover."
  [c]
  (let [n (settings/get settings :jobs.survival.respond.options/shot-hits)]
    (and (pos? n)
         (seq (lb/carried c (flight/blocks c)))
         (<= n (->> (ctx/since c :hurt (- (ctx/now c) (* 1000 (hurt-window-s))))
                    (map shots-in)
                    (reduce + 0))))))

(defn press-fights?
  "Pure: whether a melee mob pressing the body (pressed?) makes it fight before it builds: always unless it is under fire, where cover comes first for limit steps."
  [{:keys [fire? pressed? tries limit]}]
  (boolean (and pressed? (or (not fire?) (> tries limit)))))

(defn retry-covers
  "Pure: mem of a body under fire at feet cell: in another cell than the last time (:fire-cell), the covers that failed
  there (plug, plug-dig) are open again."
  [mem cell]
  (if (= cell (:fire-cell mem))
    mem
    (-> mem (update :tried #(reduce disj (set %) [:plug :plug-zone :plug-dig :plug-dig-zone])) (assoc :fire-cell cell))))

(defn boxed-in?
  "Whether the body stands in a closed room with a melee mob (hitters) within hit-reach and was hit this step or within
  hurt-window-s: no step leads out of it, so backing off only prolongs the beating and the fight is the way."
  [c hostiles]
  (boolean (and (seq (hitters c hostiles))
                (or (:hit-on-the-run (ctx/mem c)) (recently-hurt? c))
                (sh-sense/shut-in-room? (:primitives c)))))

(defn bare-boxed-in?
  "Whether the body is in a closed hut (not a one-wide tunnel, which may lead on) with a melee mob (not a creeper or an enderman) within close-radius in that room (not outside its door) and has no
  weapon and no block to wall with: the fist is all it has, so it fights at once rather than dodge about the room."
  [c hostiles]
  (let [p (:primitives c)
        close (filter #(and (not (combat/ranged? %)) (not (combat/creeper? %)) (not (combat/enderman? %))
                            (<= (danger-q/mob-distance p %) (close-radius)))
                      hostiles)
        in-room? (fn [] (let [cells (:cells (reach-room/room p (sh-sense/feet p)))]
                          (some (fn [e] (let [{:keys [x y z]} (danger-q/mob-pos p e)]
                                          (contains? cells [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])))
                                close)))]
    (boolean (and (seq close)
                  (nil? (combat/best-weapon p (combat/weapons c)))
                  (empty? (lb/carried c (flight/blocks c)))
                  (sh-sense/shut-in-hut? p)
                  (in-room?)))))

(defn mob-in-corner-cell?
  "Whether a melee mob (hitters) stands in a diagonal cell the walls still have to fill: a placed block is refused
  there and it hits across the corner, so the walls cannot close until it is fought off. A mob beside the body is left
  to the pillar."
  [c hostiles]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        corner? (fn [{:keys [x z]}] (and (== 1 (js/Math.abs (- x (:x feet)))) (== 1 (js/Math.abs (- z (:z feet))))))
        open (set (filter corner? (dig-cells/open-cells p feet)))]
    (boolean (some (fn [e] (let [{:keys [x y z]} (danger-q/mob-pos p e)
                                 cell {:x (int (js/Math.floor x)) :y (int (js/Math.floor y)) :z (int (js/Math.floor z))}]
                             (or (open cell) (open (update cell :y inc)))))
                   (hitters c hostiles)))))

(defn ^:async cornered!
  "Nowhere worth walking to: the first option of escape-order (its covers and back-off by price, by-price) that it can
  take and has not failed since the last start over (blocked!). home?: the body is in its home room."
  ([c why] (await (cornered! c why false)))
  ([c why home?]
  (let [weapons (combat/weapons c)
        max-fist-sweeps (fist-sweeps)
        hostiles (near-hostiles c)
        threat (first hostiles)
        fire? (under-fire? c)
        beaten (and (not fire?) (or (beaten? c) (boxed-in? c hostiles)))
        blind? (and fire? (not-any? #(and (combat/ranged? %) (danger-q/seen-only? %)) hostiles))
        _ (when fire? (ctx/update-mem! c retry-covers (let [{:keys [x y z]} (sh-sense/feet (:primitives c))] [x y z])))
        _ (when fire? (ctx/update-mem! c update :fire-tries (fnil inc 0)))
        fire-tries (:fire-tries (ctx/mem c) 0)
        armed? (some? (combat/best-weapon (:primitives c) weapons))
        weighed-mobs (if (and (:hit-on-the-run (ctx/mem c)) (not fire?)) (hitters c hostiles) hostiles)
        weighed (hostile/weigh c (cornered-answers c weighed-mobs home?) weighed-mobs)
        swim? (and (boolean (some #(= "drowned" (.-name %)) hostiles)) (breath/swimming? (:primitives c)))
        ;; afloat, a drowned only heard or out of a swing's reach is no fight the body can win: the swim out comes first
        win? (and (not blind?) (seq hostiles) (not-any? combat/creeper? hostiles) (not-any? combat/enderman? hostiles) (= :fight (:answer weighed))
                  (or (not swim?) (some #(and (danger-q/seen-only? %) (hostile/reaches? c %)) hostiles)))
        prices (:prices weighed)
        pillar-first? (boolean (and (not fire?) (not armed?) (pillar/pillar-item c)
                                    (pillar/pillar-fits? (:primitives c) (pl/feet-cell c))
                                    (mob-in-corner-cell? c hostiles)))
        cheap-pillar? (boolean (and (not fire?) armed? (not win?) (< (get prices :pillar ##Inf) (get prices :fight ##Inf))))
        beaten-fight? (or (beaten-fights? {:beaten? beaten :armed? armed? :blocks? (boolean (seq (lb/carried c (flight/blocks c))))})
                          (and (not fire?) (not pillar-first?) (mob-in-corner-cell? c hostiles))
                          (boolean (some #(and (combat/ranged? %) (hostile/point-blank? c %)) hostiles)))
        order (by-price (escape-order {:win? win?
                             :creeper? (boolean (some combat/creeper? hostiles))
                             :enderman? (angry-enderman? c hostiles)
                             :flyer? (boolean (some combat/flyer? hostiles))
                             :ranged? (boolean (or fire? (and threat (combat/ranged? threat))))
                             :climber? (boolean (some combat/climber? hostiles))
                             :faster? (not (outrun? c hostiles))
                             :arriving? (boolean (and armed? (seq (arriving c hostiles))))
                             :pressed? (or beaten-fight? (press-fights? {:fire? fire? :pressed? (pressed? c hostiles) :tries fire-tries :limit (settings/get settings :jobs.survival.respond.options/cover-tries)}))
                             :swim? swim?
                             :home? home?
                             :lean? (pillar/lean? c)
                             :pillar-first? pillar-first?
                             :pillar-cheaper? cheap-pillar?})
                        prices)]
    (ctx/emit! c :retreat.cornered :debug {:prices (update-vals prices js/Math.round) :weighed (mapv #(str (.-name %) " " (js/Math.round (danger-q/mob-distance (:primitives c) %))) weighed-mobs) :order order})
    (loop [[option & more] (with-zone-pillar order)]
      (let [r (when-not (flight/tried? c option)
                (case option
                  :fight (if (fist-hold? {:armed? (or beaten-fight? armed?) :win? win?
                                          :sweeps (:sweeps (ctx/mem c) 0) :fist-sweeps max-fist-sweeps})
                           (await (fist-look! c))
                           (await (fight-or-wait! c (if (= [:fight] order) weapons (into (vec weapons) tool-weapons)) (reach-only? blind? hostiles #(danger-q/mob-distance (:primitives c) %) #(mob-offset c %) (rc/creeper-keep)) hostiles)))
                  :plug (await (refuge/plug-option! c :plug #{:walls}))
                  :plug-zone (when (:zone-walls (ctx/mem c)) (await (refuge/plug-option! c :plug-zone #{:walls})))
                  :plug-dig (await (refuge/plug-option! c :plug-dig #{:pit :pocket}))
                  :plug-dig-zone (when (and (flight/tried? c :plug-dig) (:zone-dig (ctx/mem c))) (await (refuge/plug-option! c :plug-dig-zone #{:pit :pocket})))
                  :pillar (await (refuge/pillar! c false))
                  :pillar-zone (when-not (flight/tried? c :pillar) (await (refuge/pillar! c true)))
                  :water (await (stand! c :water :water "in water, away from the enderman"))
                  :roof (await (stand! c :roof :roof "under a roof, out of the enderman's reach"))
                  :swim-out (await (stand! c :swim-out :dry nil))
                  :back-off (when-let [chasers (seq (respond.goal/chasers c))]
                              (await (back-off! c chasers)))))]
        (cond
          (some? r) r
          (seq more) (recur more)
          :else (await (blocked! c why))))))))
