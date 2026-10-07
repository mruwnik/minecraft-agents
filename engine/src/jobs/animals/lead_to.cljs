(ns jobs.animals.lead-to
  (:require [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.blocks :as b]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.walk :as walk]
            [jobs.lib.reach :as reach]
            [jobs.lib.places :as places]))

(def doc
  "Put a lead on one animal of type :mob, walk to :pos with it following, then tie it to the fence post :fence
  or let it go. A one-shot order that starts and ends itself. Declines (waits) with :no-mob or :no-destination until both are given; a started job always passes. A body that carries no
  lead first gets one (phase :get-lead, a jobs.items.obtain child: withdrawn from a chest it has seen or crafted); when none
  can be got the job ends :no-lead at once, it never waits for one.

  Phases, each by a child job:
  - :leash: jobs.animals.leash (radius :radius).
  - :walk: jobs.movement.go-to to :pos (the :fence cell when no :pos), range :range, in legs of a few blocks along
    the planned path (one round, no :continue between legs). After each leg the animal is looked at: off the lead
    :lead-broke; unseen or more than 7 blocks behind, the body waits (the job yields) for it to come into sight or
    catch up; unseen for 3 s (a ledge or tree can hide it) :lost; more than 10 behind, or still waiting
    after 20 rounds, :lagging.
  - :gather (no :fence only): see below.
  - :arrive: with :fence, walk within 2 of the post and click it with an empty hand (useOn), tried twice.
    Without :fence, jobs.animals.unleash lets the animal go and picks the lead up.

  With :fence the block there must be a fence (name ends _fence), else :no-fence before anything is leashed.
  A tie counts only when the sensing then shows the animal held by something else than this body.

  Before each walking round the animal is looked up (within :watch-radius). Seen off this body's lead: ends
  :lead-broke. Not seen: ends :lost after 3 s unseen (see :walk), in every phase.

  Gather: a led animal trails about a lead length behind the body. On arrival, if the animal is farther than
  :gather-radius from :pos, the body walks on past :pos (range 1) so the lead pulls it in. It waits for the
  animal to settle between pulls, at most :gather-tries pulls. A pull stays within 11 blocks of the animal (the
  lead breaks past 12). A pull walks in the same legs, aiming a block past the point that would just gather; it is given up after 20 s (checked after each leg), or before it starts when the planned walk strays more than 11
  blocks from the animal (a wall in the way). An animal 10 or more blocks from :pos is never pulled. None of
  this is a failure: the animal is let go where it is, with a warn
  lead-to.gather-short (:distance from :pos) and :gathered false. :gathered is true when the animal was within
  :gather-radius, or tied.

  Ends with info lead-to.done and a warn lead-to.gave-up unless the reason is :tied or :unleashed. Result
  {:reason :animal key :still-led bool :at pos :gathered bool}. Reasons:
  - :tied, :unleashed: success.
  - :none (no animal seen within :radius), :lead-broke, :lost, :lagging (the animal is still on the lead): as above.
  Every reason but :tied and :unleashed ends the job :stopped with a :text.
  - :unreachable: the walk gave up (the animal is still on the lead).
  - :tie-failed: the post did not take the animal (still on the lead).
  - :timeout: :timeout-s from the first round (a cut walk leaves the animal on the lead).
  - :no-fence.
  - :no-lead: no lead carried and obtain could not get one (the :text says why).
  - The reason of jobs.animals.leash (:no-lead, :none, :unreachable, :refused, :all-leashed, :timeout) when no
    animal got on the lead, or of jobs.animals.unleash (:refused, :unreachable, :none, :timeout) when the lead would not come off.

  Zones: the leash child refuses an animal standing in another owner's zone or claim (see jobs.animals.leash);
  :ignore-zones? true is passed to it.")

(def args
  {:mob {:doc "the animal's name, such as \"cow\"" :default nil}
   :pos {:doc "where to lead it {:x :y :z}; the :fence cell when nil" :type :pos :default nil}
   :fence {:doc "the fence post {:x :y :z} to tie it to; unleash it at :pos when nil" :type :pos :default nil}
   :range {:doc "how close to :pos counts as there" :default 2}
   :radius {:doc "animals within this many blocks are leashed from where the job starts" :default 8}
   :gather-radius {:doc "without :fence the animal is let go once it is within this many blocks of :pos" :default 3}
   :gather-tries {:doc "without :fence how many times the body walks on to pull a trailing animal nearer" :default 3}
   :watch-radius {:doc "how far from the body the led animal is looked for" :default 64}
   :timeout-s {:doc "seconds from the first round before the job gives up" :default 180}
   :ignore-zones? animals/ignore-zones-arg})

(def max-ties 2)

(defn lead-carried? [c]
  (some #(= "lead" (:name %)) (u/inventory (:primitives c))))

(defn check
  "A started job passes. Else :mob and a destination (:pos or :fence) are needed: a missing one waits with that reason.
  A missing lead is fetched, never waited for."
  [c]
  (let [{:keys [mob pos fence]} (:args c)]
    (cond
      (:started (ctx/mem c)) true
      (nil? mob) (ctx/wait c :no-mob)
      (not (or pos fence)) (ctx/wait c :no-destination)
      :else true)))

(defn stop-text
  "The words for a leading that ended without success."
  [c reason]
  (let [{:keys [mob radius]} (:args c)]
    (case reason
      :none (str "no " mob " seen within " radius " blocks to lead")
      :no-lead (str "no lead carried and none could be got" (some->> (:lead-why (ctx/mem c)) (str ": ")))
      (str "leading stopped: " (name reason)))))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job. Any reason but :tied and :unleashed is a stop."
  [c reason]
  (let [m (ctx/mem c)
        ok? (#{:tied :unleashed} reason)
        result {:reason reason
                :animal (:animal m)
                :still-led (boolean (:still-led m))
                :gathered (boolean (:gathered m))
                :at (u/self-pos c)}]
    (ctx/emit! c :lead-to.done :info (assoc result :text (str "lead-to done: " (name reason))))
    (when-not ok?
      (ctx/emit! c :lead-to.gave-up :warn {:reason reason :text (stop-text c reason)}))
    (ctx/result! c (cond-> result (not ok?) (assoc :status :stopped :text (stop-text c reason))))
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

(defn ^:async get-lead!
  "Without a lead: a jobs.items.obtain child gets one (chest or craft). Any child failure ends :no-lead."
  [c]
  (let [oargs {:item "lead" :count 1}
        r (await (ctx/call-child c :obtain 'jobs.items.obtain oargs))]
    (cond
      (= :continue r) :continue
      (= :declined r) (do (ctx/update-mem! c assoc :lead-why (some-> (b/child-wait c :obtain 'jobs.items.obtain oargs) :reason name))
                          (finish! c :no-lead))
      (lead-carried? c) (do (set-phase! c :leash) :continue)
      :else (do (ctx/update-mem! c assoc :lead-why (some-> (ctx/child-result c :obtain) :reason name))
                (finish! c :no-lead)))))

(defn ^:async leash! [c]
  (let [{:keys [mob radius]} (:args c)
        r (await (ctx/call-child c :leash 'jobs.animals.leash
                                 (cond-> {:mob mob :radius radius} (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (if-not (= :done r)
      :continue
      (let [res (ctx/child-result c :leash)]
        (if-not (= :leashed (:reason res))
          (finish! c (:reason res))
          (do (ctx/update-mem! c assoc :animal (:animal res) :still-led true :phase :walk)
              :continue))))))

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
  "The farthest a pull's target lies from the animal. The lead breaks past 12 blocks, and a pull walks with
  range 1, so 10 leaves a block to spare."
  10)

(def pull-timeout-s
  "A pull walk still going after this long (a detour round a wall, say) is given up."
  20)

(def pull-range
  "How close to the pull target the walk must get."
  1)

(defn pull-point
  "Where the body walks to pull the animal to within radius of pos: past pos, on the line from the
  animal through pos, by the lead length less radius plus the walk's range (flat, pos's y: a body that
  stops that far short still leaves the animal within radius) but never so far that the body ends more
  than max-reach from the animal, or nil when the animal is at pos or too far out for any pull."
  [animal-pos pos radius]
  (let [d (flat-dist animal-pos pos)
        extra (min (+ (- lead-length radius) pull-range) (- max-reach d))
        k (/ extra d)]
    (when (and (pos? d) (pos? extra))
      (assoc pos
             :x (+ (:x pos) (* k (- (:x pos) (:x animal-pos))))
             :z (+ (:z pos) (* k (- (:z pos) (:z animal-pos))))))))

(def path-reach
  "How far from the animal any point of a pull's walk may lie. A path that strays farther (a detour round a wall
  the animal is jammed at) would break the lead."
  11)

(defn plan-or-nil
  "The walk plan to the cell of target (floored by places/parse-pos, as go-to does), nil when there is none or
  planning throws a RangeError (a far target the planner cannot index): the check never fails the job."
  [c pw target]
  (when-let [{:keys [x y z]} (:pos (places/parse-pos target))]
    (try (walk/plan-walk c pw [x y z] 1 walk/default-weight)
         (catch js/RangeError _ nil))))

(defn path-leaves-reach?
  "True when the walk the body would now take to target passes farther than path-reach from the animal. A body
  that cannot plan (no path sensing, no path, a planner that throws) is not judged here: go-to deals with that."
  [c target animal-pos]
  (let [pw (walk/path-world (:primitives c))
        plan (when pw (plan-or-nil c pw target))]
    (boolean
     (some #(> (js/Math.hypot (- (:px %) (:x animal-pos)) (- (:pz %) (:z animal-pos))) path-reach)
           (:steps plan)))))

(def leg-steps
  "How many path steps one leg of a walk covers."
  4)

(def follow-reach
  "The animal trailing the body by more than this many blocks is about to break the lead (it breaks past 12)."
  10)

(defn snap-to-ground
  "target with its y moved to the nearest standable cell of its column (down first, then a little up), unchanged when
  it already is one or the column has none: a spot in the air or underground still plans legs."
  [c target]
  (let [{:keys [x y z]} (:pos (places/parse-pos target))
        p (:primitives c)
        y' (when y
             (some #(when (reach/standable-cell? p {:x x :y % :z z}) %)
                   (concat (range y (- y 12) -1) (range (inc y) (+ y 4)))))]
    (if y' {:x x :y y' :z z} target)))

(defn leg-target
  "Where the next leg of a walk to target (within range) goes: the planned path's cell leg-steps on, or nil when the
  rest is within one leg, there is no plan, or the leg would not move the body (the caller walks the whole way)."
  [c target range]
  (let [pw (walk/path-world (:primitives c))
        steps (when pw (:steps (plan-or-nil c pw (snap-to-ground c target))))
        step (when (< leg-steps (count steps)) (nth steps leg-steps))
        me (u/self-pos c)]
    (when (and step (< 1 (flat-dist me {:x (:px step) :z (:pz step)})))
      (select-keys step [:x :y :z]))))

(def safe-gap
  "The body does not walk on while a seen animal trails it by more than this many blocks: the lead breaks past 10."
  7)

(def max-waits
  "Rounds spent waiting for an animal to catch up or come into sight before the job gives it up as :lagging."
  20)

(def unseen-lost-ms
  "Time the animal must stay out of sight, from the first look that missed it, before it counts as :lost. A led animal
  behind a ledge or a tree is not in the sensing, though it is still on the lead; rounds can come within ms, so time
  rather than looks decides."
  3000)

(defn escort-problem
  "Why the walk must stop after a leg: :lost (not seen for unseen-lost-ms), :lead-broke or :lagging (the animal
  farther than follow-reach), :wait (not seen yet, or seen more than gap behind: the body holds still), else nil."
  [c gap]
  (let [a (animal-now c)]
    (if (nil? a)
      (let [now (ctx/now c)
            since (or (:unseen-since (ctx/mem c)) now)]
        (ctx/update-mem! c assoc :unseen-since since)
        (if (<= unseen-lost-ms (- now since)) :lost :wait))
      (do (ctx/update-mem! c dissoc :unseen-since)
          (let [d (flat-dist (u/pos-of (.-pos a)) (u/self-pos c))]
            (cond
              (not (animals/led-by-me? a)) :lead-broke
              (> d follow-reach) :lagging
              (> d gap) :wait))))))

(defn escort-verdict
  "escort-problem with the waits counted: :wait becomes :lagging after max-waits in a row, and a clear look resets the count."
  [c gap]
  (let [p (escort-problem c gap)
        n (inc (:waits (ctx/mem c) 0))]
    (if (= :wait p)
      (do (ctx/update-mem! c assoc :waits n)
          (if (> n max-waits) :lagging :wait))
      (do (ctx/update-mem! c dissoc :waits)
          p))))

(defn ^:async walk-legs!
  "Walk to target (within range) in short go-to legs in child slot, looking at the animal after each (a pull passes
  follow-reach as gap: it strains the lead on purpose; a plain walk safe-gap). The body does not
  outrun the animal: when it is out of sight or trails too far the walk answers :catching-up and looks again at the next
  round before the next leg. Answer :arrived, :failed (a leg did not arrive), :limit (limit-ms from started passed),
  :waiting (a leg's child is waiting on the world: the round goes on later), :catching-up or
  {:problem :lost|:lead-broke|:lagging}."
  [c slot target range started limit-ms gap]
  (let [pre (when (:catching-up (ctx/mem c)) (escort-verdict c gap))]
    (cond
      (= :wait pre) :catching-up
      pre {:problem pre}
      :else
      (do
        (ctx/update-mem! c dissoc :catching-up)
        (loop []
          (let [leg (leg-target c target range)
                r (await (ctx/call-child c slot 'jobs.movement.go-to
                                         {:pos (or leg target) :range (if leg 1 range) :doors :leave-open :escalate false}))]
            (cond
              (not= :done r) :waiting
              (not (:arrived (ctx/child-result c slot))) :failed
              :else (let [problem (escort-verdict c gap)]
                      (cond
                        (= :wait problem) (if leg
                                            (do (ctx/update-mem! c assoc :catching-up true) :catching-up)
                                            :arrived)
                        problem {:problem problem}
                        (not leg) :arrived
                        (and limit-ms (>= (- (ctx/now c) started) limit-ms)) :limit
                        :else (recur))))))))))

(defn ^:async walk! [c]
  (let [r (await (walk-legs! c :walk (destination c) (:range (:args c)) nil nil safe-gap))]
    (cond
      (#{:waiting :catching-up} r) :continue
      (= :arrived r) (do (set-phase! c (if (:fence (:args c)) :arrive :gather)) :continue)
      (= :failed r) (finish! c :unreachable)
      :else (do (ctx/update-mem! c assoc :still-led (= :lagging (:problem r)))
                (finish! c (:problem r))))))

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
      (let [r (await (walk-legs! c :pull target pull-range started (* 1000 pull-timeout-s) follow-reach))]
        (cond
          (#{:waiting :catching-up} r) :continue
          (map? r) (do (ctx/update-mem! c assoc :still-led (= :lagging (:problem r)))
                       (finish! c (:problem r)))
          :else (do (ctx/update-mem! c dissoc :pull-target :pull-started)
                    (ctx/update-mem! c update :pulls (fnil inc 0))
                    (if (= :arrived r) :continue (stop-gathering! c d))))))))

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
                                                                    :radius (:watch-radius (:args c))
                                                                    :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
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
      (when a (ctx/update-mem! c dissoc :unseen-since))
      (cond
        (and (nil? phase) fence (not (fence-block? c))) (finish! c :no-fence)
        (>= (- now started) (* 1000 timeout-s)) (finish! c :timeout)
        (nil? phase) (do (set-phase! c (if (lead-carried? c) :leash :get-lead)) :continue)
        (= :get-lead phase) (await (get-lead! c))
        (= :leash phase) (await (leash! c))
        (and (#{:gather :arrive} phase) (nil? a)) (if (= :lost (escort-problem c safe-gap))
                                                    (do (ctx/update-mem! c assoc :still-led false) (finish! c :lost))
                                                    :continue)
        (and (#{:walk :gather :arrive} phase) a (not (animals/led-by-me? a))) (do (ctx/update-mem! c assoc :still-led false) (finish! c :lead-broke))
        (= :walk phase) (do (await (watch/watch! c {})) (await (walk! c)))
        (= :gather phase) (do (await (watch/watch! c {})) (await (gather! c a)))
        (= :arrive phase) (await (arrive! c))
        (= :release phase) (await (let-go! c))
        :else (finish! c :lost)))))
