(ns jobs.animals.lead-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [engine.path.walk :as walk]
            [engine.places :as places]))

(def doc
  "Put a lead on one animal of the mob type :mob, walk to :pos with it
  following and there either tie it to the fence post at :fence or let it go:
  a one-shot order that starts and ends itself. Phases :leash, :walk, :gather
  and :arrive, each by a child job: jobs.animals.leash (radius :radius),
  jobs.movement.go-to (to :pos, or to the :fence cell when no :pos is given,
  range :range), without :fence a :gather (see below) and then, with :fence,
  a click with an empty hand on that post (useOn) after walking to within 2 of
  it, else jobs.animals.unleash for that animal (which picks the lead up
  again). Before every walking round and again on arrival the animal is looked
  up (within :watch-radius): when it is seen off this body's lead the job ends
  at once with :lead-broke, when it is not seen at all with :lost, never with
  success. A tie counts only when the sensing then shows the animal held by
  something else than this body (leashed, not leashedToMe); one click is
  retried once. With :fence the block there must be a fence (name ending
  _fence) or the job ends :no-fence before leashing anything. Done (info
  lead-to.done, and a warn lead-to.gave-up with the reason unless it is
  :tied or :unleashed; hands over {:reason :animal key :still-led bool :at
  pos}) with :reason :tied or :unleashed on success, :lead-broke or :lost
  as above, :unreachable when the walk gave up (the animal is then still on
  the lead: :still-led true), :tie-failed when the post did not take the
  animal (still on the lead), :timeout after :timeout-s from the first round
  (a cut walk leaves the animal on the lead), :no-fence, and the reason of
  jobs.animals.leash (:no-lead, :none, :unreachable, :refused, :all-leashed,
  :timeout) when no animal got on the lead, or of jobs.animals.unleash
  (:refused, :unreachable, :none, :timeout) when the lead would not come off.
  :gather (no :fence only): a body outwalks a led animal, which trails about
  a lead length behind it, so on arrival the animal is looked up and, when it
  is farther than :gather-radius from :pos, the body walks on past :pos (range
  1) so the lead pulls the animal within :gather-radius, waiting for the animal
  to settle between pulls; at most :gather-tries pulls. A pull goes no farther past :pos
  than keeps the body within 11 blocks of the animal (the lead breaks past 12), and is given up
  after 20 s (its child go-to is dropped) or before it starts when the planned walk to its target passes more than 11
  blocks from the animal (a wall it is jammed at: the detour would break the lead). A cow 10 or more blocks out is never pulled: it is let go
  where it is, :gathered false. A pull that did not arrive, a pull given up, an animal too far out for a pull
  or running out of pulls is no failure: the animal is let go where it is, with a warn
  lead-to.gather-short (:distance from :pos) and :gathered false in the result (:gathered
  is true when the animal was within :gather-radius, or tied).
  The check always passes, so a cut job resumes and ends itself.")

(def args
  {:mob {:doc "the animal's name, such as \"cow\"" :default nil}
   :pos {:doc "where to lead it {:x :y :z}; the :fence cell when nil" :type :pos :default nil}
   :fence {:doc "the fence post {:x :y :z} to tie it to; unleash it at :pos when nil" :type :pos :default nil}
   :range {:doc "how close to :pos counts as there" :default 2}
   :radius {:doc "animals within this many blocks are leashed from where the job starts" :default 8}
   :gather-radius {:doc "without :fence the animal is let go once it is within this many blocks of :pos" :default 3}
   :gather-tries {:doc "without :fence how many times the body walks on to pull a trailing animal nearer" :default 3}
   :watch-radius {:doc "how far from the body the led animal is looked for" :default 64}
   :timeout-s {:doc "seconds from the first round before the job gives up" :default 180}})

(def max-ties 2)

(defn check [_c] true)

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [m (ctx/mem c)
        result {:reason reason
                :animal (:animal m)
                :still-led (boolean (:still-led m))
                :gathered (boolean (:gathered m))
                :at (u/self-pos c)}]
    (ctx/emit! c :lead-to.done :info (assoc result :text (str "lead-to done: " (name reason))))
    (when-not (#{:tied :unleashed} reason)
      (ctx/emit! c :lead-to.gave-up :warn {:reason reason :text (str "leading stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn destination [c]
  (let [{:keys [pos fence]} (:args c)]
    (or pos fence)))

(defn fence-block? [c]
  (some-> (u/block-name (:primitives c) (:fence (:args c))) (.endsWith "_fence")))

(defn animal-now
  "The led animal as the sensing shows it, or nil when it is not seen."
  [c]
  (animals/find-by-key (:primitives c) (:mob (:args c)) (:watch-radius (:args c)) (:animal (ctx/mem c))))

(defn set-phase! [c phase]
  (ctx/update-mem! c assoc :phase phase))

(defn ^:async leash! [c]
  (let [{:keys [mob radius]} (:args c)
        r (await (ctx/call-child c :leash 'jobs.animals.leash {:mob mob :radius radius}))]
    (if-not (= :done r)
      :continue
      (let [res (ctx/child-result c :leash)]
        (if-not (= :leashed (:reason res))
          (finish! c (:reason res))
          (do (ctx/update-mem! c assoc :animal (:animal res) :still-led true :phase :walk)
              :continue))))))

(defn ^:async walk! [c]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (destination c) :range (:range (:args c)) :doors :leave-open}))]
    (if-not (= :done r)
      :continue
      (if (:arrived (ctx/child-result c :walk))
        (do (set-phase! c (if (:fence (:args c)) :arrive :gather)) :continue)
        (finish! c :unreachable)))))

(defn flat-dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:z a) (:z b))))

(def lead-length
  "How far behind a walking body a led animal trails."
  6)

(def moved-eps
  "An animal that moved less than this between two looks has settled."
  0.25)

(def max-settles
  "Looks spent waiting for the animal to settle before a pull."
  20)

(def max-reach
  "The farthest a pull's target lies from the animal. The lead breaks when the body is more than 12 blocks from
  it (Leashable.LEASH_TOO_FAR_DIST on 26.1; 10 before the 1.21.6 rework). A pull walks with range 1, so the body
  can end 1 beyond its target: 10 + 1 leaves a block to spare."
  10)

(def pull-timeout-s
  "A pull walk still going after this long (a detour round a wall, say) is given up."
  20)

(defn pull-point
  "Where the body walks to pull the animal to within radius of pos: past pos, on the line from the
  animal through pos, by the lead length less radius (flat, pos's y) but never so far that the body
  ends more than max-reach from the animal, or nil when the animal is at pos or too far out for any
  pull."
  [animal-pos pos radius]
  (let [d (flat-dist animal-pos pos)
        extra (min (- lead-length radius) (- max-reach d))
        k (/ extra d)]
    (when (and (pos? d) (pos? extra))
      (assoc pos
             :x (+ (:x pos) (* k (- (:x pos) (:x animal-pos))))
             :z (+ (:z pos) (* k (- (:z pos) (:z animal-pos))))))))

(def path-reach
  "How far from the animal any point of a pull's walk may lie. The animal does not move away while pulled, so a
  path that strays farther than this (a detour round a wall it is jammed at) would break the lead: 12 breaks it,
  and a pull's own target lies up to 10 + 1 out."
  11)

(defn plan-or-nil
  "The walk plan to the cell of target (floored by places/parse-pos, as go-to does), nil when there is none or
  planning throws: the check never fails the job."
  [c pw target]
  (when-let [{:keys [x y z]} (:pos (places/parse-pos target))]
    (try (walk/plan-walk c pw [x y z] 1 walk/default-weight)
         (catch :default _ nil))))

(defn path-leaves-reach?
  "True when the walk the body would now take to target passes farther than path-reach from the animal. A body
  that cannot plan (no path sensing, no path, a planner that throws) is not judged here: go-to deals with that."
  [c target animal-pos]
  (let [pw (walk/path-world (:primitives c))
        plan (when pw (plan-or-nil c pw target))]
    (boolean
     (some #(> (js/Math.hypot (- (:px %) (:x animal-pos)) (- (:pz %) (:z animal-pos))) path-reach)
           (:steps plan)))))

(defn stop-gathering!
  "Give up pulling: the animal is let go where it is, :gathered false at distance d from the spot."
  [c d]
  (ctx/update-mem! c assoc :gathered false :gather-dist d)
  (ctx/update-mem! c dissoc :pull-target :pull-started)
  (ctx/update-mem! c update :children dissoc :pull)
  (set-phase! c :arrive)
  :continue)

(defn ^:async pull! [c target animal-pos]
  (let [now (ctx/now c)
        started (:pull-started (ctx/mem c) now)
        d (flat-dist animal-pos (:pos (:args c)))]
    (ctx/update-mem! c assoc :pull-started started)
    (cond
      (>= (- now started) (* 1000 pull-timeout-s)) (stop-gathering! c d)
      (path-leaves-reach? c target animal-pos) (stop-gathering! c d)
      :else
      (let [r (await (ctx/call-child c :pull 'jobs.movement.go-to {:pos target :range 1 :doors :leave-open}))]
        (if-not (= :done r)
          :continue
          (let [arrived (:arrived (ctx/child-result c :pull))]
            (ctx/update-mem! c dissoc :pull-target :pull-started)
            (ctx/update-mem! c update :pulls (fnil inc 0))
            (if arrived :continue (stop-gathering! c d))))))))

(defn ^:async gather!
  "Without :fence: let the animal catch up. A pull in progress is carried on with its stored target;
  otherwise the animal is looked at: near enough (or the pulls spent) is :arrive, one still moving
  is given time to settle (the animal lags the body), else a pull starts."
  [c a]
  (let [{:keys [pos gather-radius gather-tries]} (:args c)
        {:keys [pull-target pulls gather-seen settles] :or {pulls 0 settles 0}} (ctx/mem c)
        animal-pos (u/pos-of (.-pos a))
        d (flat-dist animal-pos pos)]
    (cond
      pull-target (await (pull! c pull-target animal-pos))
      (<= d gather-radius) (do (ctx/update-mem! c assoc :gathered true) (set-phase! c :arrive) :continue)
      (>= pulls gather-tries) (stop-gathering! c d)
      (and (< settles max-settles) (or (nil? gather-seen) (> (flat-dist gather-seen animal-pos) moved-eps)))
      (do (ctx/update-mem! c assoc :gather-seen animal-pos :settles (inc settles)) :continue)
      :else (let [target (pull-point animal-pos pos gather-radius)]
              (ctx/update-mem! c dissoc :gather-seen)
              (if-not target
                (stop-gathering! c d)
                (do (ctx/update-mem! c assoc :pull-target target :settles 0 :gather-dist d)
                    (await (pull! c target animal-pos))))))))

(defn ^:async let-go! [c]
  (set-phase! c :release)
  (let [r (await (ctx/call-child c :unleash 'jobs.animals.unleash {:mob (:mob (:args c)) :animal (:animal (ctx/mem c))
                                                                    :radius (:watch-radius (:args c))}))]
    (if-not (= :done r)
      :continue
      (let [reason (:reason (ctx/child-result c :unleash))]
        (when (= :unleashed reason) (ctx/update-mem! c assoc :still-led false))
        (finish! c reason)))))

(defn tied?
  "True when the animal is held by something else than this body."
  [c]
  (let [a (animal-now c)]
    (and a (animals/leashed? a) (not (animals/led-by-me? a)))))

(defn ^:async tie! [c]
  (let [fence (:fence (:args c))
        near (await (near/walk-near! c fence 2 {:doors :never}))]
    (if-not (= :there near)
      :continue
      (let [r (await (ctx/act c :useOn (clj->js {:pos fence})))
            tries (inc (:ties (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :ties tries)
        (cond
          (tied? c) (do (ctx/update-mem! c assoc :still-led false :gathered true) (finish! c :tied))
          (>= tries max-ties) (finish! c :tie-failed)
          :else (do (ctx/emit! c :lead-to.tie-retry :info {:status (.-status r) :text "the post did not take the animal, trying once more"})
                    :continue))))))

(defn ^:async arrive! [c]
  (let [{:keys [gathered gather-dist]} (ctx/mem c)
        a (animal-now c)
        distance (if a (flat-dist (u/pos-of (.-pos a)) (:pos (:args c))) gather-dist)]
    (cond
      (:fence (:args c)) (await (tie! c))
      gathered (await (let-go! c))
      :else (do (ctx/emit! c :lead-to.gather-short :warn
                           {:distance distance
                            :text (str "the animal is let go " (some-> distance js/Math.round) " blocks from the spot, not gathered")})
                (await (let-go! c))))))

(defn ^:async round [c]
  (let [now (ctx/now c)
        {:keys [timeout-s fence]} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [{:keys [phase started animal]} (ctx/mem c)
          a (when animal (animal-now c))]
      (cond
        (and (nil? phase) fence (not (fence-block? c))) (finish! c :no-fence)
        (>= (- now started) (* 1000 timeout-s)) (finish! c :timeout)
        (nil? phase) (do (set-phase! c :leash) :continue)
        (= :leash phase) (await (leash! c))
        (and (#{:walk :gather :arrive} phase) (nil? a)) (do (ctx/update-mem! c assoc :still-led false) (finish! c :lost))
        (and (#{:walk :gather :arrive} phase) (not (animals/led-by-me? a))) (do (ctx/update-mem! c assoc :still-led false) (finish! c :lead-broke))
        (= :walk phase) (await (walk! c))
        (= :gather phase) (await (gather! c a))
        (= :arrive phase) (await (arrive! c))
        (= :release phase) (await (let-go! c))
        :else (finish! c :lost)))))
