(ns jobs.lib.hostile-fight
  "The hostile mode of combat.attack (:targets :hostiles): which dangers near it fights, and when a held or leashed fight stops."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.access :as access]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.look :as look]
            [jobs.lib.flee-walk :as walk]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::hold-s {:default 15 :doc "Seconds a held fight waits without a swing for a mob to come within reach before it declines." :spec (a/num-in 0 nil)}
   ::ranged-chase {:default 6 :doc "Farthest a ranged mob may be for the body to cross open ground to it while another hostile is in range; beyond it the fight declines." :spec (a/num-in 0 nil)}
   ::chase-timeout-s {:default 10 :doc "Seconds one walk toward a hostile may take: it moves, so the walk aims again at where it is now this often." :spec (a/num-in 0 nil)}
   ::point-blank {:default 1.5 :doc "Blocks within which a hostile (a shooter too, not a creeper or an enderman) is in the body's own cell or beside it: a cornered body fights a shooter this close instead of hiding from it." :spec (a/num-in 0 nil)}
   ::creeper-range {:default 7 :doc "Blocks within which an armed, healthy body fights a lone creeper hit-and-back instead of fleeing it (a creeper met this near cannot be outrun)." :spec (a/num-in 0 nil)}
   ::creeper-returns {:default 2 :doc "Flights from a creeper within ::creeper-return-s after which an armed, healthy body fights the next lone creeper however far it is (it keeps coming back). 0: never." :spec (a/int-in 0 nil)}
   ::creeper-return-s {:default 600 :doc "Seconds a flight from a creeper counts toward ::creeper-returns." :spec (a/num-in 0 nil)}
   ::creeper-health-floor {:default 14 :doc "Health below which the body never fights a creeper hit-and-back: a late step back lets part of a blast land." :spec (a/num-in 0 nil)}
   ::ranged-returns {:default 2 :doc "Flights from ranged mobs (skeletons and the like) within ::ranged-return-s after which an armed body of at least ::commit-health fights the next ones to the end, however far, instead of fleeing again (arrows outrun a walker, so the flights only repeat). 0: never." :spec (a/int-in 0 nil)}
   ::ranged-return-s {:default 600 :doc "Seconds a flight from a ranged mob counts toward ::ranged-returns." :spec (a/num-in 0 nil)}
   ::commit-health {:default 10 :doc "Health below which the body does not commit to a fight with ranged mobs it has fled before (::ranged-returns)." :spec (a/num-in 0 nil)}
   ::back-step {:default 6 :doc "Blocks the body backs off from a creeper after each hit: its fuse runs on, so it ends out of the blast's reach (about 6)." :spec (a/num-in 1 nil)}
   ::leash {:default 20 :doc "Blocks from where a hostile fight started beyond which a hostile is not chased." :spec (a/num-in 0 nil)}})

(defn hostiles? [c] (= :hostiles (:targets (:args c))))

(defn chase-timeout-s [] (settings/get settings ::chase-timeout-s))

(defn hold-ms [] (* 1000 (settings/get settings ::hold-s)))


(defn start-of
  "Where the fight started: the first step's position (the current one before it)."
  [c]
  (or (:start (ctx/mem c)) (u/self-pos c)))

(defn in-leash?
  "Whether hostile e stands within the leash of the start point."
  [c e]
  (<= (u/dist (start-of c) (u/pos-of (.-pos e))) (settings/get settings ::leash)))

(defn in-range
  "The hostiles within :radius (ranged ones within :ranged-radius, a heard one by its band): the visible ones nearest
  first, then the hidden melee ones (a ranged mob without a line of fire, by jobs.lib.danger/danger? as the hostile-near
  trigger, is dropped). The killed and the :skip ids are left out."
  [c]
  (let [{:keys [radius ranged-radius skip]} (:args c)
        ranged-radius (or ranged-radius radius)
        dead (into (set skip) (:killed (ctx/mem c)))
        all (->> (danger-q/known-hostiles (:primitives c) radius {:ranged-radius (max radius ranged-radius)})
                 (remove #(contains? dead (.-id %)))
                 (remove #(and (combat/ranged? %) (not (danger-q/danger? (:primitives c) %)))))]
    (into (filterv #(.-visible %) all) (remove #(.-visible %)) all)))

(defn point-blank?
  "Whether hostile e is within ::point-blank of the body and is no creeper or enderman."
  [c e]
  (and (not (combat/creeper? e)) (not (combat/enderman? e))
       (<= (danger-q/mob-distance (:primitives c) e) (settings/get settings ::point-blank))))

(defn targets
  "The hostiles in range the body has seen, within the leash and not given up on, nearest first."
  [c]
  (let [given-up (:given-up (ctx/mem c) {})]
    (->> (in-range c) (filter danger-q/seen-only?) (filter #(in-leash? c %)) (remove #(contains? given-up (.-id %))))))

(defn heard
  "The first hostile in range the body only hears, or nil."
  [c]
  (first (remove danger-q/seen-only? (in-range c))))

(defn too-far-to-chase?
  "Whether target is a ranged mob beyond ::ranged-chase while another hostile is in range, unless the fight is committed (:commit)."
  [c target]
  (and (not (:commit (:args c)))
       (combat/ranged? target)
       (> (danger-q/mob-distance (:primitives c) target) (settings/get settings ::ranged-chase))
       (some #(not= (.-id %) (.-id target)) (in-range c))))

(defn dark-around?
  "Whether mob e stands in or beside unlit space: its cell, the four cells next to it and the one 2 past it from the body,
  those open or not sensed, include one the body sees as dark (look/dark-fn: a cell not sensed counts as dark underground).
  A mob in the dark is hardly seen, so what shows is the lit edge of the dark it stands at."
  [c e]
  (let [p (:primitives c)
        pos (u/pos-of (.-pos e))
        me (u/self-pos c)
        [x y z] (mapv #(js/Math.floor %) [(:x pos) (:y pos) (:z pos)])
        away (fn [d] (let [v (- (d pos) (d me))] (cond (> v 0.5) 2 (< v -0.5) -2 :else 0)))
        cells [[x y z] [(inc x) y z] [(dec x) y z] [x y (inc z)] [x y (dec z)] [(+ x (away :x)) y (+ z (away :z))]]]
    (boolean (when-let [dark (look/dark-fn p)]
               (some (fn [[cx cy cz]]
                       (and (let [n (u/seen-name p {:x cx :y cy :z cz})] (or (nil? n) (bs/passable-or-water? n)))
                            (= 1 ((:at dark) cx cy cz))))
                     cells)))))

(defn held?
  "Whether the call waits for target instead of walking to it: :hold, a creeper (it comes, the body hits and backs off), or in a :hostiles fight a melee mob in or beside unlit space
  (a player does not chase into unlit space: more may be there, unseen)."
  [c target]
  (boolean (and (not (combat/ranged? target))
                (or (:hold (:args c)) (combat/creeper? target) (and (hostiles? c) (dark-around? c target))))))

(defn held-idle?
  "Whether a held fight has waited ::hold-s since its last swing (or its start) with no mob coming within reach."
  ([c] (held-idle? c (:hold (:args c))))
  ([c held?]
   (let [m (ctx/mem c)]
     (and held?
          (> (- (ctx/now c) (or (:last-attack m) (:started-at m))) (hold-ms))))))

(defn struck
  "What the fight counted per mob, as {id {:name :hits :health :blocked :blocked-health :quiet}} (the last three only
  when set), for a parent that weighs how near each mob is to dying and for the next call's carry-over."
  [m]
  (into {} (for [id (distinct (concat (keys (:hits m)) (keys (:fails m)) (keys (:quiet m))))]
             [id (cond-> {:name (get-in m [:names id]) :hits (get-in m [:hits id] 0)}
                   (number? (get-in m [:health id])) (assoc :health (get-in m [:health id]))
                   (pos? (get-in m [:fails id] 0)) (assoc :blocked (get-in m [:fails id]) :blocked-health (get-in m [:fail-health id]))
                   (pos? (get-in m [:quiet id] 0)) (assoc :quiet (get-in m [:quiet id])))])))

(defn carry-over
  "The attack memory that seeds a caller's next call after a fight that ended with hostiles left: the ending data's
  kills, given-up ids, names and per-mob counts (:struck); nil for an ending without :struck (a refused start)."
  [data]
  (when-let [struck (:struck data)]
    (let [pick (fn [k] (into {} (for [[id e] struck :when (some? (get e k))] [id (get e k)])))]
      {:killed (:killed data) :given-up (:given-up data) :names (:names data)
       :hits (pick :hits) :health (pick :health) :quiet (pick :quiet)
       :fails (pick :blocked) :fail-health (pick :blocked-health)})))

(defn mob-of
  "What fight-damage needs of hostile e: its name, distance (jobs.lib.danger/mob-distance: a heard one by its band) and
  the hits the attack has landed on it (struck, its :struck entry, or nil)."
  [p struck e]
  (cond-> {:name (.-name e) :distance (danger-q/mob-distance p e) :hits (:hits struck 0)}
    (number? (:health struck)) (assoc :health (:health struck))))

(defn damage
  "The damage fighting hostiles with the best weapon carried is expected to cost (jobs.lib.cost/fight-damage); struck-of
  maps a mob id to what the attack counted on it (nil: none)."
  [c hostiles struck-of]
  (let [p (:primitives c)
        self (.self p)]
    (cost/fight-damage {:weapon (combat/best-weapon p (combat/weapons c))
                        :equipment (cost/equipment-of (.-equipment self))
                        :mobs (map #(mob-of p (struck-of (.-id %)) %) hostiles)})))

(defn creeper-fled-from
  "How many times the body fled a creeper within ::creeper-return-s, by the :hostile entries (c is a ctx)."
  [c]
  (let [since (- (ctx/now c) (* 1000 (settings/get settings ::creeper-return-s)))]
    (count (filter #(and (> (:t %) since) (= "creeper" (get-in % [:data :mob])) (= :flee (get-in % [:data :decision])))
                   (ctx/entries c :hostile)))))

(defn ranged-fled-from
  "How many times the body fled a ranged mob within ::ranged-return-s, by the :hostile entries (c is a ctx)."
  [c]
  (let [since (- (ctx/now c) (* 1000 (settings/get settings ::ranged-return-s)))]
    (count (filter #(and (> (:t %) since) (contains? weapon/ranged-mobs (get-in % [:data :mob])) (= :flee (get-in % [:data :decision])))
                   (ctx/entries c :hostile)))))

(defn committed?
  "Whether the body fights hostiles to the end whatever it costs: all of them ranged, a sword or axe carried, health at
  least ::commit-health and ::ranged-returns flights from ranged mobs lately (they keep coming back and a flight cannot outrun arrows)."
  [c hostiles]
  (let [p (:primitives c)
        returns (settings/get settings ::ranged-returns)]
    (boolean (and (pos? returns)
                  (seq hostiles)
                  (every? combat/ranged? hostiles)
                  (contains? weapon/weapon-damage-by-name (combat/best-weapon p (combat/weapons c)))
                  (>= (.-health (.self p)) (settings/get settings ::commit-health))
                  (>= (ranged-fled-from c) returns)))))

(defn back-cell
  "The cell n blocks (at most) from from in the open direction that ends farthest from threat, gaining at least a block on
  it; nil when none does. block-at: cell -> block name (as respond.options/block-at-fn)."
  [block-at from threat n]
  (let [now (u/dist from threat)]
    (->> (range 0 360 45)
         (keep #(peek (walk/walk-cells block-at from (walk/rotate (or (walk/unit (- (:x from) (:x threat)) (- (:z from) (:z threat))) [1 0]) %) n)))
         (filter #(>= (u/dist % threat) (inc now)))
         (sort-by #(- (u/dist % threat)))
         first)))

(defn back-step [] (settings/get settings ::back-step))

(defn back-cell-of
  "The cell the body backs off to from creeper over what it senses (back-cell), nil when none."
  [c creeper]
  (let [at (access/sensed-at (:primitives c) access/hidden-guess)]
    (back-cell (fn [{:keys [x y z]}] (at [x y z])) (u/self-pos c) (u/pos-of (.-pos creeper)) (back-step))))

(defn hit-and-back-ok?
  "Whether the body can fight hostiles hit-and-back: exactly one, a creeper, a sword or axe carried, health at least
  ::creeper-health-floor and an open cell to back off to. Ends the fight when it stops holding."
  [c hostiles]
  (let [p (:primitives c)
        creeper (first hostiles)]
    (boolean (and (= 1 (count hostiles))
                  (combat/creeper? creeper)
                  (contains? weapon/weapon-damage-by-name (combat/best-weapon p (combat/weapons c)))
                  (>= (.-health (.self p)) (settings/get settings ::creeper-health-floor))
                  (back-cell-of c creeper)))))

(defn hit-and-back?
  "Whether the body starts a hit-and-back fight (combat.attack): hit-and-back-ok? and the creeper within ::creeper-range or
  fled from ::creeper-returns times lately."
  [c hostiles]
  (let [returns (settings/get settings ::creeper-returns)]
    (boolean (and (hit-and-back-ok? c hostiles)
                  (or (<= (danger-q/mob-distance (:primitives c) (first hostiles)) (settings/get settings ::creeper-range))
                      (and (pos? returns) (>= (creeper-fled-from c) returns)))))))

(defn wins?
  "Whether fighting hostiles leaves :reserve health (jobs.lib.danger/fight-reserve): the one weighing the response and the
  flight's cornered options share. Never against a creeper (hit-and-back? decides that), nor against none."
  ([c hostiles] (wins? c hostiles (constantly nil)))
  ([c hostiles struck-of]
   (boolean (and (seq hostiles)
                 (not-any? combat/creeper? hostiles)
                 (cost/leaves-reserve? {:health (.-health (.self (:primitives c))) :reserve (danger-q/fight-reserve)
                                        :damage (damage c hostiles struck-of)})))))
