(ns jobs.animals.herd-trip
  "One animal's trip for jobs.animals.herd: leashing the next, regathering, the steps through the gate and the release."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.animals :as animals]
            [engine.ctx :as ctx]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.pen :as pen]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.animals.herd-run :refer [animal-now deepest end! finish! gate-open? held-by-me? holds-food? in-pen-adults lure? near-pen out-1 read-pen regather-radius set-phase!]]
            [jobs.animals.herd-pen :refer [animal-pos pinned-ms settle-at! step-in-step]]))


(a/defargs settings
  {::lure-reach {:default 6 :doc "A lured animal is waited for once the body is this near it, in blocks." :spec (a/int-in 0 nil)}
   ::look-back-range {:default 12 :doc "How near the body goes to where an animal near the start was last listed, to see it again, in blocks." :spec (a/int-in 0 nil)}
   ::max-tries {:default 2 :doc "Attempts before the job gives up." :spec (a/int-in 0 nil)}})
(defn lure-reach [] (settings/get settings ::lure-reach))
(defn look-back-range [] (settings/get settings ::look-back-range))
(defn max-tries [] (settings/get settings ::max-tries))

(defn tired-keys
  "The keys of animals leashed max-tries times already (tries {key n}) and not in the pen (a set of keys)."
  [tries pen-keys]
  (into [] (comp (filter #(>= (val %) (max-tries))) (map key) (remove pen-keys)) tries))

(defn range-from-origin
  "[center radius] for listing animals: the run's start (the body walks to the pen first) and a radius that covers
  :radius from there seen from the body's spot plus the pen's surroundings."
  [c]
  (let [origin (or (:origin (ctx/mem c)) (u/self-pos c))]
    [origin (+ (:radius (:args c)) (u/dist (u/self-pos c) origin))]))

(defn skip-keys
  "Keys never to leash: babies, animals in the pen, the ones given up on, those already fetched twice and outside."
  [c]
  (let [{:keys [mob]} (:args c)
        answer (read-pen c)
        [_ listed] (range-from-origin c)
        near (animals/herd (:primitives c) mob (+ listed (near-pen)))
        in-pen? #(pen/in-pen? answer (u/pos-of (.-pos %)))
        skip? #(or (true? (.-baby %)) (in-pen? %))
        pen-keys (into #{} (comp (filter in-pen?) (map animals/key-of)) near)]
    (-> (vec (keys (:given-up (ctx/mem c))))
        (into (comp (filter skip?) (map animals/key-of)) near)
        (into (tired-keys (:tries (ctx/mem c)) pen-keys)))))

(defn start-trip!
  "The animal k is with the body (on its lead, or following its food): its trip to the gate starts."
  [c k]
  (set-phase! c :approach {:animal k :led [k] :animal-started (ctx/now c)
                           :tries (update (:tries (ctx/mem c)) k (fnil inc 0))
                           :pos-index 0 :backs 0 :retried false :deepest-tried false}))

(defn ^:async lead-next!
  "Lead one more animal and start its trip, or end when none more can be led."
  [c]
  (let [{:keys [mob radius]} (:args c)
        m (ctx/mem c)
        r (await (ctx/call-child c :leash 'jobs.animals.leash
                                 (cond-> {:mob mob :radius radius :center (:origin (ctx/mem c)) :skip (skip-keys c)} (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))
        res (when (= :done r) (ctx/child-result c :leash))]
    (cond
      (nil? res) :continue
      (= :leashed (:reason res)) (start-trip! c (:animal res))
      (and (empty? (:brought m)) (empty? (:given-up m))) (finish! c (:reason res))
      :else (finish! c nil))))

(defn lead-possible?
  "True when the body carries a lead or what makes one (a slime ball)."
  [c]
  (let [have (into #{} (map :name) (u/inventory (:primitives c)))]
    (boolean (or (have "lead") (have "slime_ball")))))

(defn resolve-method
  "The :method of the run: :lead or :lure as given; :auto leads when a lead can be had, else lures with the mob's food."
  [c]
  (let [{:keys [method mob]} (:args c)]
    (cond
      (not= :auto method) method
      (or (lead-possible? c) (not (contains? animals/breeding-food mob))) :lead
      :else :lure)))

(defn lure-problem
  "The need wait for the mob's food while none is carried, else nil."
  [c]
  (let [mob (:mob (:args c))]
    (when (nil? (animals/food-carried (:primitives c) mob))
      {:reason :need :any-of (get animals/breeding-food mob) :count 1})))

(defn lure-candidates
  "The adults within radius outside the pen, not on a lead, not skipped, the zone rules allowing, nearest first."
  [c]
  (let [{:keys [mob radius]} (:args c)
        [origin listed] (range-from-origin c)
        skip (set (skip-keys c))]
    (->> (animals/adults (:primitives c) mob listed)
         (filterv #(and (not (animals/leashed? %)) (not (skip (animals/key-of %)))
                        (<= (u/dist origin (u/pos-of (.-pos %))) radius)))
         (animals/allowed c :herd.declined "herd" :take))))

(defn ^:async lure-next!
  "Fetch the mob's food when none is carried, hold it, walk within follow range of the nearest animal and start its
  trip; end :no-food when no food can be had, else as lead-next! when no animal is left."
  [c]
  (let [{:keys [mob]} (:args c)
        p (:primitives c)
        m (ctx/mem c)
        food (animals/food-carried p mob)]
    (if-not food
      (let [r (when (fetch/opts c 'jobs.animals.herd) (await (fetch/fetch-untimed! c 'jobs.animals.herd lure-problem)))]
        (cond
          r r
          (animals/food-carried p mob) :continue
          :else (finish! c :no-food)))
      (let [cands (lure-candidates c)
            target (first cands)]
        (cond
          (empty? cands) (if (and (empty? (:brought m)) (empty? (:given-up m)))
                           (finish! c (or (animals/refusal c) :none))
                           (finish! c nil))
          (and (not= food (.-held (.self p))) (>= (:equip-tries m 0) 3)) (finish! c :no-hold)
          (not= food (.-held (.self p))) (do (ctx/update-mem! c update :equip-tries (fnil inc 0))
                                             (await (ctx/act c :equip (clj->js {:item food})))
                                             :continue)
          :else
          (let [k (animals/key-of target)
                _ (ctx/update-mem! c dissoc :equip-tries)
                walked (await (animals/walk! c k target (lure-reach) {:doors :shut}))]
            (if (= :there walked) (start-trip! c k) :continue)))))))

(defn remember-start!
  "Once, before the walk to the pen: the adults of :mob listed within :radius of the body, unleashed, as {:key :pos}. The server
  loads animals only near the body, so those near the start are not listed once the body stands at the pen (look-back!)."
  [c]
  (when-not (contains? (ctx/mem c) :remembered)
    (let [{:keys [mob radius]} (:args c)]
      (ctx/update-mem! c assoc :remembered
                       (->> (animals/adults (:primitives c) mob radius)
                            (remove animals/leashed?)
                            (mapv (fn [e] {:key (animals/key-of e) :pos (u/pos-of (.-pos e))})))))))

(defn look-back-target
  "The remembered animal to go and look for when none is listed within :radius of the start now: the nearest to the body not
  yet looked for, else nil."
  [c]
  (let [m (ctx/mem c)
        looked (set (:looked-back m))]
    (when (empty? (lure-candidates c))
      (->> (:remembered m)
           (remove #(contains? looked (:key %)))
           (sort-by #(u/dist (u/self-pos c) (:pos %)))
           first))))

(defn ^:async look-back!
  "Walk to within look-back-range of where the animal was last listed, so it is loaded and listed again; then it is booked looked-for.
  :continue while the walk goes on."
  [c {:keys [key pos]}]
  (let [r (await (near/go-near! c pos (look-back-range) {:doors :never :escalate false}))]
    (if (= :partial r)
      :continue
      (do (ctx/update-mem! c update :looked-back (fnil conj []) key) :continue))))

(defn ^:async leash-next!
  "Bring one more animal to the body and start its trip, or end when the pen holds enough or none more can be had.
  The first call settles the run's :method."
  [c]
  (let [{:keys [target]} (:args c)]
    (cond
      (not (look/surveyed? c)) (await (look/survey! c)) ; animals count only while in view: turn round once before judging
      (>= (count (in-pen-adults c (read-pen c))) target)
      (finish! c nil)
      :else
      (do (when-not (:method (ctx/mem c)) (ctx/update-mem! c assoc :method (resolve-method c)))
          (if-let [lost (look-back-target c)]
            (await (look-back! c lost))
            (if (lure? c) (await (lure-next! c)) (await (lead-next! c))))))))

(defn prune-led!
  "Drop from :led the animals no longer on this body's lead, booking each (:lead-broke seen, :lost not seen);
  how many were dropped. A lured animal seen beyond follow-range while the gate stands open and the food is in hand
  stays: shutting the gate on it is the pinned steps' call, not a drop's."
  [c]
  (let [waiting? (and (lure? c) (holds-food? c) (gate-open? c))
        gone (keep (fn [k] (let [a (animal-now c k)]
                             (cond (nil? a) [k :lost]
                                   (and (not waiting?) (not (held-by-me? c a))) [k :lead-broke])))
                   (:led (ctx/mem c)))]
    (when (seq gone)
      (ctx/update-mem! c #(-> %
                              (update :led (fn [led] (vec (remove (set (map first gone)) led))))
                              (update :given-up merge (into {} gone)))))
    (count gone)))

(defn regather-or-lose!
  "Nobody is led any more: shut the gate if it stands open, pick the leads up and start again once, else give up :lost."
  [c]
  (if (:regathered (ctx/mem c))
    (end! c :lost)
    (do (ctx/update-mem! c #(-> %
                                (assoc :regathered true :animal nil)
                                (update :given-up (fn [g] (into {} (remove (fn [[_ r]] (= :lead-broke r))) g)))))
        (set-phase! c (if (gate-open? c) :shut-gate :regather)))))

(defn ^:async regather! [c]
  (if (lure? c)
    (set-phase! c :leash)
    (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius (regather-radius) :filter ["lead"]}))]
      (if (= :done r) (set-phase! c :leash) :continue))))



(defn released!
  "The led animal is let go: booked :brought when it stands on a pen cell, else given up :outside (or with the unleash child's reason when its lead stayed on); on to phase next."
  [c next]
  (let [k (:animal (ctx/mem c))
        there (animal-pos c)
        in? (boolean (and there (pen/in-pen? (read-pen c) there)))
        failed (get (:unleash-failed (ctx/mem c)) k)]
    (ctx/update-mem! c #(-> %
                            (dissoc :release-tried :releasing :release-declines :unleash-failed)
                            (assoc :led [] :animal nil)
                            (cond-> (and in? (not failed) (not (some #{k} (:brought %)))) (update :brought (fnil conj []) k))
                            (cond-> (and (or failed (not in?)) (not (contains? (:given-up %) k)))
                              (update :given-up assoc k (or failed :outside)))))
    (set-phase! c next)))

(defn pinned!
  "The animal stays too far behind at position index idx: how the stepping goes on (step-in-step)."
  [c idx]
  (let [m (ctx/mem c)
        backs (:backs m 0)]
    (case (step-in-step backs (boolean (:retried m)))
      :back (set-phase! c :step {:pos-index (max out-1 (dec idx)) :back-step true :backs (inc backs) :deepest-tried false})
      :retry-from-out (set-phase! c :retry-back {:backs 0 :retried true :deepest-tried false})
      :give-up (set-phase! c :give-up-walk))))

(defn step-position!
  "One round of the step through the gate: walk to the position and settle; settled goes on to the next one (the last
  to the shut), pinned to pinned!, a position reached on a back-step to the one it backed from."
  [c]
  (let [{:keys [pos-index back-step]} (ctx/mem c)]
    (settle-at! c pos-index (pinned-ms)
                #(cond
                   back-step (set-phase! c :step {:pos-index (inc pos-index) :back-step false})
                   (= :pinned %) (pinned! c pos-index)
                   (= pos-index deepest) (set-phase! c :shut-behind)
                   :else (set-phase! c :step {:pos-index (inc pos-index)})))))
