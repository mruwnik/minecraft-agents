(ns jobs.survival.retreat-refuge
  "A cornered body's refuges (jobs.survival.retreat): started and stepped (retreat-seal, retreat-pillar, retreat-pit),
  and the hiding in them."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.spent :as spent]
            [jobs.lib.threats :as threats]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-pillar :as pillar]
            [jobs.survival.retreat-pit :as pit]
            [jobs.survival.retreat-seal :as seal]))

(a/defargs settings
  {::leave-for-breakers {:default true :doc "Whether a hidden body leaves its refuge, before it is hit, for a creeper (its blast breaks any refuge) or, on a pillar, a spider (it climbs) within :breaker-reach." :spec boolean?}
   ::foot-reach {:default 4 :doc "Blocks within which a melee hostile kept off a hidden body counts as waiting at its foot: the hide then outlasts :hide-s, up to ::hide-max-s, since the first step off the refuge would pass it." :spec (a/num-in 0 nil)}
   ::hide-max-s {:default 300 :doc "Seconds after which a hidden body hands back though a melee hostile still waits at its foot." :spec (a/num-in 0 nil)}
   ::breaker-reach {:default 6 :doc "Blocks within which a creeper or a climber counts as breaching a hidden body's refuge." :spec (a/num-in 0 nil)}})
(defn leave-for-breakers? [] (settings/get settings ::leave-for-breakers))
(defn breaker-reach [] (settings/get settings ::breaker-reach))
(defn foot-reach [] (settings/get settings ::foot-reach))
(defn hide-max-s [] (settings/get settings ::hide-max-s))

(def hide-texts
  {:seal "cornered: sealed in with blocks until the hostile leaves"
   :pillar "cornered: pillared up out of reach until the hostile leaves"
   :pit "cornered: dug down and plugged the hole until the hostile leaves"})

(declare hide-now! hide-hold! hostiles-known)

(defn ^:async abandon-refuge!
  "The refuge failed: forget it, never try that option again this flight."
  [c]
  (let [kind (:kind (:refuge (ctx/mem c)))]
    (ctx/update-mem! c dissoc :refuge)
    (flight/tried! c kind)
    :again))

(defn ^:async refuge-round!
  "A step in a refuge: building it (pillar or pit), or hidden in it (to the end of the flight)."
  [c]
  (let [{:keys [kind hidden] :as refuge} (:refuge (ctx/mem c))]
    (if hidden
      (await (hide-hold! c))
      (case (await (if (= :pillar kind) (pillar/step! c refuge) (pit/pit-round! c refuge)))
        :failed (await (abandon-refuge! c))
        :hidden (hide-now! c refuge (hide-texts kind))
        :again))))

(defn ^:async start-refuge!
  "Note refuge (a map with :kind, :anchor the feet cell it starts from, :cells [x y z] the cells it fills) in job
  memory and run its first step."
  [c refuge]
  (ctx/update-mem! c assoc :refuge refuge)
  (await (refuge-round! c)))

(defn ^:async hide!
  "Seal the body in and hide there (a :seal refuge): :again while sealing or once sealed, nil when it cannot seal."
  [c]
  (case (await (seal/seal! c))
    :failed nil
    :again :again
    :sealed (hide-now! c {:kind :seal :anchor (sh/feet (:primitives c)) :cells (vec (:seal-cells (ctx/mem c)))} (hide-texts :seal))))

(defn ^:async pillar!
  "Start a pillar (retreat-pillar/plan!) when it fits: a step's result, else nil."
  [c]
  (when-let [refuge (await (pillar/plan! c))]
    (await (start-refuge! c refuge))))

(defn ^:async pit!
  "Start digging down and plugging (retreat-pit/plan!) when it can: a step's result, else nil."
  [c]
  (when-let [refuge (await (pit/plan! c))]
    (await (start-refuge! c refuge))))

(defn refuge-danger?
  "Whether a hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius), the dead skipped,
  would have a walkable way to the refuge's anchor cell were the refuge's own cells open (jobs.lib.danger): a danger
  the refuge keeps off."
  [c {:keys [anchor cells]}]
  (let [p (:primitives c)
        open (set cells)
        {:keys [ranged-radius]} (:args c)
        reach-of (fn [e] (cond-> (threats/follow-range (.-name e)) (combat/ranged? e) (max ranged-radius)))
        known (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range ranged-radius)]
    (boolean (some #(and (<= (danger-q/mob-distance p %) (reach-of %))
                         (reach/walkable-way? p (danger-q/mob-pos p %) anchor #{} open))
                   known))))

(defn waiting-at-foot?
  "Whether a known melee hostile stands within foot-reach of the body."
  [c]
  (let [p (:primitives c)
        r (foot-reach)]
    (boolean (some #(not (combat/ranged? %)) (flight/near-known p (set (flight/dead-ids c)) r r)))))

(def reach-radius
  "Blocks within which a hostile counts as the one that hurt a hidden body."
  3)

(defn breaker-of
  "The known hostile that would breach the refuge: a creeper (its blast breaks any refuge) or, against a pillar, a climber,
  within breaker-reach; nil when none or leave-for-breakers is off."
  [c {:keys [kind]}]
  (when (leave-for-breakers?)
    (let [p (:primitives c)
          reach (breaker-reach)]
      (->> (flight/near-known p (set (flight/dead-ids c)) reach reach)
           (filter #(or (combat/creeper? %) (and (= :pillar kind) (combat/climber? %))))
           first))))

(defn ^:async hide-hold!
  "Sealed in, up the pillar or down the pit: hold (a :wait, why hiding) while the refuge keeps a danger off
  (refuge-danger?) and :quiet-s after the last one, then end the flight :hidden. Past :hide-s (while a melee hostile waits at the foot, ::hide-max-s) in the refuge with no real
  danger on the body as it stands (danger-q/danger-near?, the hostile-near trigger's test) it ends :hidden at once,
  the trigger fires again should one reach it.
  A body hurt while hidden (a mob reaching it round a corner of the seal) leaves the refuge as failed (abandon-refuge!),
  a pillar raised once first (retreat-pillar/raise!)."
  [c]
  (let [now (ctx/now c)]
    (when-not (:quiet-from (ctx/mem c)) (ctx/update-mem! c assoc :quiet-from now))
    (when-not (:hidden-since (ctx/mem c)) (ctx/update-mem! c assoc :hidden-since now)))
  (loop []
    (when (refuge-danger? c (:refuge (ctx/mem c))) (ctx/update-mem! c assoc :quiet-from (ctx/now c)))
    (let [{:keys [radius ranged-radius quiet-s hide-s]} (:args c)
          {:keys [quiet-from hidden-since refuge hold-health]} (ctx/mem c)
          health (.-health (.self (:primitives c)))
          hurt? (< health (or hold-health health))
          breaker (breaker-of c refuge)
          _ (ctx/update-mem! c assoc :hold-health health)
          quiet? (>= (- (ctx/now c) quiet-from) (* 1000 quiet-s))
          held-ms (- (ctx/now c) hidden-since)
          spent? (and (>= held-ms (* 1000 hide-s))
                      (or (>= held-ms (* 1000 (hide-max-s))) (not (waiting-at-foot? c)))
                      (not (danger-q/danger-near? (:primitives c) radius ranged-radius)))]
      (cond
        hurt? (let [ids (map (fn [e] (.-id e)) (flight/near-known (:primitives c) #{} reach-radius reach-radius))]
                (ctx/update-mem! c #(-> % (dissoc :quiet-from :hidden-since :hold-health) (update :reached (fnil into #{}) ids)))
                (ctx/emit! c :retreat.hide-hurt :warn {:text "hurt while hidden: the refuge does not keep the hostile off" :pos (sh/feet (:primitives c))
                                                       :hostiles (hostiles-known c)})
                (if (= :again (await (pillar/raise! c))) :again (await (abandon-refuge! c))))
        breaker (let [name (.-name breaker)]
                  (ctx/update-mem! c #(-> % (dissoc :quiet-from :hidden-since :hold-health) (update :reached (fnil conj #{}) (.-id breaker))))
                  (ctx/emit! c :retreat.hide-breached :warn {:text (str "a " name " is near: the refuge does not keep it off, leaving before it strikes")
                                                              :mob name :pos (sh/feet (:primitives c))})
                  (await (abandon-refuge! c)))
        quiet? (do (ctx/update-mem! c dissoc :quiet-from :hidden-since)
                   (flight/end-flight! c :hidden {:hold :quiet}))
        spent? (do (ctx/update-mem! c dissoc :quiet-from :hidden-since)
                   (flight/end-flight! c :hidden {:hold :spent}))
        :else (do (await (child/pause! c (/ (flight/wait-ms) 1000) "hiding"))
                  (await (pace/pace!))
                  (recur))))))

(defn hostiles-known
  "The hostiles the body knows within follow range, as [{:name :pos [x y z]}]; empty when it only heard one."
  [c]
  (let [p (:primitives c)]
    (mapv (fn [e] {:name (.-name e) :pos (mapv #(js/Math.floor (% (danger-q/mob-pos p e))) [:x :y :z])})
          (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range (:ranged-radius (:args c))))))

(defn hide-now! [c refuge text]
  (ctx/update-mem! c assoc :refuge (assoc refuge :hidden true))
  (let [placed (spent/log c)
        hostiles (hostiles-known c)
        from (if (seq hostiles)
               (str/join ", " (map #(str (:name %) " at " (pr-str (:pos %))) hostiles))
               "a hostile heard, not seen")]
    (ctx/emit! c :retreat_sealed :warn (merge {:text (str text "; hiding from " from (some->> (spent/text placed "refuge") (str "; "))) :pos (sh/feet (:primitives c)) :hostiles hostiles}
                                              (when (seq placed) (spent/summary placed)))))
  :again)
