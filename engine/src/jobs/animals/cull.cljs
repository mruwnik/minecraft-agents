(ns jobs.animals.cull
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.work-place :as work-place]
            [engine.args :as a]
            [jobs.lib.pen-read :as pen-read]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.util :as u]
            [jobs.lib.hunting :as hunting :refer [census reach]]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]))

(def doc
  "Thin a herd of one kind (:mob) down to :keep adults and collect the drops.

  The herd is the animals of the kind inside a bound: :box when set, else :radius blocks around :center
  ({:x :z}, horizontal), else :radius blocks around the body. Babies are never targets and never count as
  adults. It looks around first (animals count only while in view); with :keep or fewer adults in the bound after that the goal holds and it ends done :already; it waits
  while the zone rules refuse every adult and fewer than :least are killed (:refused, :no-zones, before every round), so a held cull never blocks the queue. The weapons are the call's :use :tools (default
  the best carried).

  One call is the whole run, a loop of these:
  - With a target: the attack child fights it. It is booked killed, or skipped when attack gave up or lost it.
    The setting jobs.lib.hunting/max-skips (3) skips in a row end the job :gave-up.
  - After a kill: collect-drops picks up :drops (nil: the kind's entry in jobs.lib.hunting/drops, else every
    item within the setting jobs.lib.hunting/collect-radius (8)).
  - Otherwise it picks the next candidate: adults in the bound not skipped, those that are not unhittable first
    (an animal behind a wall comes last), then nearest first. The target stays until killed or given up on.

  It first deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!).

  done? holds once :count are killed or at most :keep adults remain, and the drops of the last kill are picked up; the
  engine then ends it :done. The census is live at every step, so the last :keep are never taken.
  Ends with the data {:killed n :remaining adults-in-bound :babies n :reason r :skipped [ids]}, r :count or :keep when
  done. Skipped animals stay alive and still count as adults, so they still breed. Any other reason ends :done when
  the call's :least (default 1) animals were killed; else nothing-possible with r as the cause. It yields (:continue) only while a child waits on the world.
  Other reasons:
  - :unreachable: no candidate is left, some were skipped.
  - :none: no candidate on two looks in a row, with a 1 s wait between; the data reason is :refused (or
    :no-zones) when the zone rules refused the adults.
  - :gave-up: too many skips in a row.

  Zones: an adult standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  cull.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "mob type name of the animals to thin (required: a call without it is :bad-args)" :spec a/name? :default nil}
   :keep {:doc "adults of the kind to leave alive in the bound (babies do not count)" :spec (a/int-in 0 nil) :default 2}
   :center {:doc "nil, or a cell [x y z], [x z], {:x :y :z} or {:x :z}: the bound is :radius blocks (horizontal) around it" :spec a/xz-position? :default nil}
   :radius {:doc "animals within this many blocks of :center, or of the body when there is no :center, are the herd" :spec (a/num-in 0 nil) :default 16}
   :box {:doc "nil, or {:min {:x :y :z} :max {:x :y :z}}: inclusive bound that replaces :center and :radius" :spec pen-read/box-arg :default nil}
   :color {:doc "sheep only: kill only sheep of this wool colour (e.g. \"white\"); nil: any. :keep still counts every adult" :spec a/name? :default nil}
   :count {:doc "most animals to kill in one run; nil: no cap" :spec (a/int-in 1 nil) :default nil}
   :drops {:doc "item names to collect after a kill; nil: the kind's entry in the drops table, else every item" :spec (a/coll-of a/item?) :default nil}
   :ignore-zones? :shared})

(def target [:mob :box :center])

(def safe? (price/staying :work (ready/safe-at-by-night work-place/spot)))


(a/defargs settings
  {::min-health {:default 0 :spec (a/num-in 0 20) :doc "Health (of 20) below which the job waits to heal; 0 = never (its acts are short and harmless). Poison still holds."}
   ::job-time-s {:default 300 :doc "Seconds a cull may take." :spec (a/num-in 0 nil)}})

(defn fit? [c] (ready/fit-min (settings/get settings ::min-health) c))
(def defaults {:least 1 :spend {:job-time ::job-time-s} :use {:tools :attack}})

(defn candidates
  "The adults in the bound not skipped: reachable-looking ones first, then nearest."
  [c adults]
  (let [skipped (set (:skipped (ctx/mem c)))
        color (:color (:args c))
        here (u/self-pos c)]
    (->> (animals/allowed c :cull.declined "cull" :harvest (cond->> adults color (filter #(= color (.-color %)))))
         (remove #(contains? skipped (.-id %)))
         (sort-by (juxt #(if (false? (.-hittable %)) 1 0)
                        #(u/dist here (u/pos-of (.-pos %))))))))

(defn least-killed
  "Animals the call must kill to be done when it stops short: :least (:all = :count, default 1)."
  [c]
  (let [least (:least (ctx/frozen-call c) 1)]
    (if (= :all least) (or (:count (:args c)) 1) least)))

(defn refused
  "Later :refused or :no-zones while the zone rules refuse every adult and fewer than the call's :least are killed (a
  quiet look, the decline warn comes with the first round). Once :least is met a refusal ends the run as :none."
  [c]
  (let [{:keys [color ignore-zones?]} (:args c)
        adults (:adults (census c))
        targets (cond->> adults color (filter #(= color (.-color %))))
        why (when (and (seq targets) (not ignore-zones?) (< (:killed (ctx/mem c) 0) (least-killed c)))
              (animals/refusal-of c :harvest targets))]
    (if why
      (contract/later why (str "cull waits: every " (:mob (:args c)) " in the bound is refused by the zone rules (" (name why) ")"))
      (contract/ok :start "an adult the zone rules allow"))))

(def bad-args (checks/args-fit "needs a :mob" #(some? (:mob (:args %)))))

(def can-start? (cond/and bad-args refused))

(def can-run? refused)

(defn result
  "The cull's result so far, its reason r when given."
  [c r]
  (let [{:keys [adults babies]} (census c)
        {:keys [killed skipped]} (ctx/mem c)]
    (cond-> {:killed (or killed 0) :remaining (count adults) :babies (count babies) :skipped (vec skipped)}
      r (assoc :reason r))))

(defn done?
  "Done once :count are killed or no more than :keep adults stand in the bound, the drops picked up."
  [c]
  (let [{:keys [mob keep] wanted :count} (:args c)
        {:keys [killed collecting]} (ctx/mem c)
        killed (or killed 0)
        adults (count (:adults (census c)))
        reason (cond (and wanted (>= killed wanted)) :count
                     (and (<= adults keep) (look/surveyed? c)) :keep)
        left (cond-> (max 0 (- adults keep)) wanted (min (- wanted killed)) reason (* 0))
        data [:units :animals :done killed :left left :result (result c reason)]]
    (cond
      (and reason (not collecting))
      (apply contract/ok reason (str "cull done: killed " killed ", " adults " " mob " adults left (keep " keep ")") data)
      reason (apply contract/later :collecting "cull picks up the drops" data)
      :else (apply contract/later :surplus (str "cull: " adults " " mob " adults, keeping " keep) data))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 20 (or (:count (:args c)) 4)) ##Inf]}})

(defn finish!
  "End a cull that stopped for reason short of its goal: :done when the call's :least was killed, else
  :nothing-possible with reason as the cause; data its result."
  [c reason]
  (let [killed (:killed (ctx/mem c) 0)
        data (result c reason)]
    (if (>= killed (least-killed c))
      (ctx/end c :done nil data)
      (ctx/end c :nothing-possible reason data))))

(defn attack-opts
  "Options for hunting.attack!: a search around the reach; ends with :gave-up."
  [c]
  {:radius (+ (reach c) 16) :give-up #(finish! % :gave-up)
   :job "jobs.animals.cull" :rule-fn (constantly :cull)})

(defn none!
  "Nothing left to attack: end with the refusal, else :none."
  [c]
  (finish! c (or (animals/refusal c) :none)))

(defn ^:async step
  "One step: attack the target, collect, or pick the next target (done? ends the cull at its goal). :again, :yield (a
  child waits on the world) or an ending."
  [c]
  (let [{:keys [target collecting skipped]} (ctx/mem c)]
    (cond
      target (await (hunting/attack! c (attack-opts c)))
      collecting (await (hunting/collect! c :yield))
      (not (look/surveyed? c)) (let [keep (:keep (:args c))
                                     r (await (look/survey-until! c #(> (count (:adults (census c))) keep)))] ; cows count only while in view, and only inside the cone of the look: test between looks
                                 (when r (ctx/update-mem! c assoc :looked (look/cell-of (u/self-pos c))))
                                 (if (and (zero? (:killed (ctx/mem c) 0)) (= :ok (first (done? c))))
                                   (ctx/end c :done :already (result c :keep))
                                   :continue))
      :else
      (let [next-target (first (candidates c (:adults (census c))))]
        (cond
          (and (nil? next-target) (seq skipped)) (finish! c :unreachable)
          (nil? next-target) (await (hunting/search! c none!))
          :else (do (ctx/update-mem! c assoc :target (.-id next-target) :misses 0)
                    (await (hunting/attack! c (attack-opts c)))))))))

(defn ^:async round
  "The whole attempt: loop the steps until one ends or yields."
  [c]
  (if-let [t (await (trip/ready! c))]
    t
    (let [r (await (pace/steps! c #(step c)))]
      (if (= :yield r) :continue r))))

(def meta {:tags [:food :fight]})
