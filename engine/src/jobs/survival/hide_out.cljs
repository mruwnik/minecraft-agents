(ns jobs.survival.hide-out
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.ready :as ready]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.threats :as threats]
            [jobs.survival.respond.state :as flight]))

(def doc
  "Wait in a refuge the body is in (sealed in a plug or pit, or up a pillar: the call's :kind, with its :anchor feet cell and :cells filled)
  while it keeps a danger off, one hold (a :wait, why hiding) a round. A danger is a known hostile within its follow range
  (jobs.lib.threats; ranged ones at least danger/::ranged-radius), the ids in :skip left out, that would have a walkable way to
  the anchor were the refuge's own cells open (jobs.lib.reach/walkable-way?).
  Done {:hold :quiet} once the ::quiet-s setting (30 s) has passed since the last such danger; {:hold :spent}
  once ::hide-s (60 s) have passed in the refuge with no real danger on the body as it stands (danger/danger-near?, the
  hostile-near trigger's test), though a hostile stays; a melee hostile within ::foot-reach waiting at the foot holds it to
  ::hide-max-s. Sealed in a plug or pit with such a hostile at the foot past ::hide-s it first digs a stair up and away from
  it (jobs.access.stair, ::dig-out-steps steps; the next heading when one stops), then ends done {:hold :spent :dug-out true}.
  Nothing-possible :hurt {:ids [mobs within ::reach-radius]} when the body loses health in the refuge (a mob reaching it
  round a corner of the seal), :breached {:id :name} when a creeper (its blast breaks any refuge) or, against a pillar, a
  climber is within ::breaker-reach and ::leave-for-breakers is on. Needs nothing; places nothing; the refuge itself stays.
  Called by the flight (jobs.survival.respond.refuge); any agent may submit it from inside a refuge.")

(a/defargs args
  {:kind {:doc "the refuge: :plug, :plug-dig, :pit or :pillar" :spec #{:plug :plug-dig :pit :pillar} :default nil}
   :anchor {:doc "the feet cell the refuge was made at" :spec ::a/pos :default nil}
   :cells {:doc "the cells the refuge filled ([x y z])" :spec (a/coll-of a/position?) :default []}
   :skip {:doc "ids of hostiles to leave out (the dead)" :spec (a/coll-of number?) :default []}})

(a/defargs settings
  {::leave-for-breakers {:default true :doc "Whether a hidden body leaves its refuge, before it is hit, for a creeper (its blast breaks any refuge) or, on a pillar, a spider (it climbs) within ::breaker-reach." :spec boolean?}
   ::foot-reach {:default 4 :doc "Blocks within which a melee hostile kept off a hidden body counts as waiting at its foot: the hide then outlasts ::hide-s, up to ::hide-max-s, since the first step off the refuge would pass it." :spec (a/num-in 0 nil)}
   ::dig-out-steps {:default 6 :doc "Steps of the stair a body sealed in a plug or pit digs up and away from a melee hostile kept off it at its foot, once ::hide-s has passed." :spec (a/int-in 1 nil)}
   ::quiet-s {:default 30 :doc "Seconds a hidden body keeps its refuge after the last danger." :spec (a/num-in 0 nil)}
   ::hide-s {:default 60 :doc "Seconds in its refuge after which a hidden body hands back, though a danger it keeps off stays, once no real danger is on it as it stands." :spec (a/num-in 0 nil)}
   ::hide-max-s {:default 300 :doc "Seconds after which a hidden body hands back though a melee hostile still waits at its foot." :spec (a/num-in 0 nil)}
   ::breaker-reach {:default 6 :doc "Blocks within which a creeper or a climber counts as breaching a hidden body's refuge." :spec (a/num-in 0 nil)}
   ::reach-radius {:default 3 :doc "Blocks within which a hostile counts as the one that hurt a hidden body." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 900 :doc "Seconds a hide may take." :spec (a/num-in 0 nil)}})

(defn leave-for-breakers? [] (settings/get settings ::leave-for-breakers))
(defn breaker-reach [] (settings/get settings ::breaker-reach))
(defn reach-radius [] (settings/get settings ::reach-radius))
(defn quiet-s [] (settings/get settings ::quiet-s))
(defn hide-s [] (settings/get settings ::hide-s))
(defn foot-reach [] (settings/get settings ::foot-reach))
(defn hide-max-s [] (settings/get settings ::hide-max-s))

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)

(def can-start?
  (checks/args-fit "needs a :kind and an :anchor" (fn [c] (let [{:keys [kind anchor]} (:args c)] (and (contains? #{:plug :plug-dig :pit :pillar} kind) (some? anchor))))))

(defn done? [_] (checks/goal false [:hidden "the hide is over"] [:hiding "hiding in the refuge"] {}))

(defn quote-of [_] {:uses {:job-time [0 (quiet-s) (hide-max-s)]}})

(defn dead [c] (set (:skip (:args c))))

(defn refuge-danger?
  "Whether a hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius), the skipped left out,
  would have a walkable way to the refuge's anchor cell were the refuge's own cells open: a danger the refuge keeps off."
  [c]
  (let [p (:primitives c)
        {:keys [anchor cells]} (:args c)
        open (set cells)
        ranged-radius (danger-q/ranged-radius)
        reach-of (fn [e] (cond-> (threats/follow-range (.-name e)) (combat/ranged? e) (max ranged-radius)))
        known (flight/near-known p (dead c) threats/max-follow-range ranged-radius)]
    (boolean (some #(and (<= (danger-q/mob-distance p %) (reach-of %))
                         (reach/walkable-way? p (danger-q/mob-pos p %) anchor #{} open))
                   known))))

(defn waiting-melee
  "The known melee hostiles within foot-reach of the body."
  [c]
  (let [p (:primitives c)
        r (foot-reach)]
    (remove combat/ranged? (flight/near-known p (dead c) r r))))

(defn ^:async dig-out!
  "Dig a stair up (jobs.access.stair, ::dig-out-steps) on the heading away from the mob waiting at the foot, the next heading
  when one stops. :again while it digs, :out when it is done, nil when no heading is left. Progress is the :dig-out entry of
  job memory (heading index and the first mob's place)."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        {:keys [i from] :or {i 0}} (:dig-out (ctx/mem c))
        from (or from (some->> (waiting-melee c) first (danger-q/mob-pos p)))
        order (dig-leave/heading-order feet (when from {:x (- (* 2 (:x feet)) (:x from)) :z (- (* 2 (:z feet)) (:z from))}))
        heading (get order i)]
    (ctx/update-mem! c assoc :dig-out {:i i :from from})
    (when heading
      (let [e (await (ctx/call c (keyword (str "dig-out-" i)) 'jobs.access.stair
                               {:dir :up :heading heading :steps (settings/get settings ::dig-out-steps) :fetch false
                                :checks (ready/unforced c {:fit? '(always)})}))]
        (cond
          (= :continue e) :again
          (= :done (:kind e)) :out
          :else (do (ctx/update-mem! c assoc-in [:dig-out :i] (inc i)) :again))))))

(defn breaker
  "The known hostile that would breach the refuge: a creeper or, against a pillar, a climber, within breaker-reach; nil when none or leave-for-breakers is off."
  [c]
  (when (leave-for-breakers?)
    (let [p (:primitives c)
          reach (breaker-reach)]
      (->> (flight/near-known p (dead c) reach reach)
           (filter #(or (combat/creeper? %) (and (= :pillar (:kind (:args c))) (combat/climber? %))))
           first))))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        now (ctx/now c)
        health (.-health (.self p))
        {:keys [quiet-from hidden-since hold-health]} (ctx/mem c)
        quiet-from (if (refuge-danger? c) now (or quiet-from now))
        hidden-since (or hidden-since now)
        hurt? (< health (or hold-health health))
        _ (ctx/update-mem! c assoc :quiet-from quiet-from :hidden-since hidden-since :hold-health health)
        radius (danger-q/hostile-radius)
        ranged-radius (danger-q/ranged-radius)
        danger-on-body? (fn [] (danger-q/danger-near? p radius ranged-radius))
        held-ms (- now hidden-since)
        breaker (breaker c)
        dig-out? (or (:dig-out (ctx/mem c))
                     (and (#{:plug :pit} (:kind (:args c)))
                          (>= held-ms (* 1000 (hide-s)))
                          (seq (waiting-melee c))
                          (not (danger-on-body?))))]
    (cond
      hurt? (ctx/end c :nothing-possible :hurt
                     {:ids (mapv #(.-id %) (flight/near-known p #{} (reach-radius) (reach-radius)))
                      :text "hurt while hidden: the refuge does not keep the hostile off"})
      breaker (ctx/end c :nothing-possible :breached
                       {:id (.-id breaker) :name (.-name breaker)
                        :text (str "a " (.-name breaker) " is near: the refuge does not keep it off, leaving before it strikes")})
      dig-out? (let [r (await (dig-out! c))]
                 (if (= :again r)
                   :continue
                   (ctx/end c :done nil {:hold :spent :dug-out (= :out r)})))
      (>= (- now quiet-from) (* 1000 (quiet-s))) (ctx/end c :done nil {:hold :quiet})
      (and (>= held-ms (* 1000 (hide-s)))
           (or (>= held-ms (* 1000 (hide-max-s))) (empty? (waiting-melee c)))
           (not (danger-on-body?)))
      (ctx/end c :done nil {:hold :spent})
      :else (do (await (child/pause! c (/ (flight/wait-ms) 1000) "hiding"))
                :continue))))
