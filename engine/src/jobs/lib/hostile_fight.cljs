(ns jobs.lib.hostile-fight
  "The hostile mode of combat.attack (:targets :hostiles): which dangers near it fights, and when a held or leashed fight stops."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::hold-s {:default 15 :doc "Seconds a held fight waits without a swing for a mob to come within reach before it declines." :spec (a/num-in 0 nil)}
   ::ranged-chase {:default 6 :doc "Farthest a ranged mob may be for the body to cross open ground to it while another hostile is in range; beyond it the fight declines." :spec (a/num-in 0 nil)}
   ::chase-timeout-s {:default 10 :doc "Seconds one walk toward a hostile may take: it moves, so the walk aims again at where it is now this often." :spec (a/num-in 0 nil)}
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
  "Whether target is a ranged mob beyond ::ranged-chase while another hostile is in range."
  [c target]
  (and (combat/ranged? target)
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
  "Whether the call waits for target instead of walking to it: :hold, or in a :hostiles fight a melee mob in or beside unlit space
  (a player does not chase into unlit space: more may be there, unseen)."
  [c target]
  (boolean (and (not (combat/ranged? target))
                (or (:hold (:args c)) (and (hostiles? c) (dark-around? c target))))))

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

(defn wins?
  "Whether fighting hostiles leaves :reserve health (jobs.lib.danger/fight-reserve): the one weighing the response and the
  flight's cornered options share. Never against a creeper, nor against none."
  ([c hostiles] (wins? c hostiles (constantly nil)))
  ([c hostiles struck-of]
   (boolean (and (seq hostiles)
                 (not-any? combat/creeper? hostiles)
                 (cost/leaves-reserve? {:health (.-health (.self (:primitives c))) :reserve (danger-q/fight-reserve)
                                        :damage (damage c hostiles struck-of)})))))
