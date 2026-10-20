(ns jobs.animals.lead-to
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.lib.animals :as animals]
            [jobs.lib.child :as child]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.walk.world :as wworld]
            [jobs.lib.escort :as escort]))

(def doc
  "Put a lead on one animal of type :mob, walk to :pos with it following, then tie it to the fence post :fence
  or let it go. A one-shot order that starts and ends itself. Without :mob or a destination (:pos or :fence) it can never start
  (impossible :no-mob, :no-destination); with a :fence cell seen and not a fence it waits :no-fence. A body that carries no
  lead first gets one (phase :get-lead, a jobs.items.obtain child: withdrawn from a chest it has seen or crafted, limits
  from the call's ways); when none can be got the job ends :no-lead at once, it never waits for one; with :ways #{:carried} a call
  without a lead waits (can-start? later :no-lead).

  Phases, each by a child job:
  - :leash: jobs.animals.leash (radius :radius).
  - :walk: jobs.movement.go-to to :pos (the :fence cell when no :pos), range :range, in legs of a few blocks along
    the planned path. One call is the whole run; it yields only while a child waits on the world. After each leg the animal is looked at: off the lead
    :lead-broke; unseen or beyond the escort safe gap behind, the body holds still (:waiting-for-animal) for it to come into
    sight or catch up; unseen past the escort unseen-lost time (a ledge or tree can hide it) the body walks back toward where it was last seen, then :lost;
    still beyond the gap and not closing in for the escort stall time, :animal-stalled.
  - :gather (no :fence only): see below.
  - :arrive: with :fence, walk within 2 of the post and click it with an empty hand (a jobs.items.use child), tried twice.
    Without :fence, jobs.animals.unleash lets the animal go and picks the lead up.

  With :fence the block there must be a fence (name ends _fence), else :no-fence before anything is leashed; a post not yet seen (far off) is checked again on arrival.
  A tie counts only when the sensing then shows the animal held by something else than this body.

  Before each walking round the animal is looked up (within the escort watch-radius setting, 64). Seen off this body's lead: ends
  :lead-broke. Not seen: ends :lost after the unseen time (see :walk), in every phase.

  The legs and the pull-reach check are planned here (jobs.lib.escort/leg-plan) to choose where each leg ends and to
  keep the animal within the lead; the go-to child of a leg plans its own walk again, as it must for doors and danger.

  Done when the animal is tied to the :fence post (also one already tied there before the start) or let go, off this
  body's lead, and every gate its legs opened is shut again.

  Gather: a led animal trails about a lead length behind the body. On arrival, if the animal is farther than
  :gather-radius from :pos, the body walks on past :pos (range 1) so the lead pulls it in. It waits for the
  animal to settle between pulls, at most the gather-tries setting pulls (waiting at most max-settles looks before each). A pull stays within the path-reach setting of the animal (the
  lead breaks past 12). A pull walks in the same legs, aiming pull-slack blocks past the point that would just gather; it is given up before it starts when the planned walk strays beyond path-reach
  from the animal (a wall in the way). An animal max-reach or more blocks from :pos is never pulled. None of
  this is a failure: the animal is let go where it is, with a warn
  lead-to.gather-short (:distance from :pos) and :gathered false. :gathered is true when the animal was within
  :gather-radius, or tied.

  Cleanup (however the job ends, the engine's limit and a cancel included): an animal still on this body's lead is let
  go (jobs.animals.unleash), then a jobs.maintenance.shut child shuts the doors and gates the legs left open for
  the animal; the ending's :still-led is then false.

  Result
  {:reason :end (:tie or :free on success) :animal key :still-led bool (false: the cleanup let the animal go) :at pos
  :animal-at pos (nil unseen) :gathered bool :text}. Reasons and endings:
  - :tied, :unleashed: done.
  - :none (no animal seen within :radius), :lead-broke, :lost, :animal-stalled: as above.
  - :unreachable: the walk gave up.
  - :tie-failed: the post did not take the animal.
  - :no-fence: condition-failed, the post turned out not to be a fence on arrival.
  - :no-lead: no lead carried and obtain could not get one (the :text says why; the fetch's ending is the :because).
  - The reason of jobs.animals.leash (:no-lead, :none, :unreachable, :failed, :all-leashed) when no animal got on the lead, or
    of jobs.animals.unleash (:unreachable, :failed, :none) when the lead would not come off; :refused and :no-zones are
    condition-failed.
  - the engine's limit (default :spend :job-time ::job-time-s): :limit-reached with {:animal :still-led} (done?'s :data).
  All but done and condition-failed end nothing-possible with the reason as the cause.

  Zones: the leash child refuses an animal standing in another owner's zone or claim (see jobs.animals.leash);
  :ignore-zones? true is passed to it.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :pos {:doc "where to lead it {:x :y :z}; the :fence cell when nil" :spec ::a/pos :default nil}
   :fence {:doc "the fence post {:x :y :z} to tie it to; unleash it at :pos when nil" :spec ::a/pos :default nil}
   :range {:doc "how close to :pos counts as there" :spec (a/num-in 0 nil) :default 2}
   :radius {:doc "animals within this many blocks of where the job starts are leashed" :spec (a/num-in 0 nil) :default 8}
   :gather-radius {:doc "without :fence the animal is let go once it is within this many blocks of :pos" :spec (a/num-in 0 nil) :default 3}
   :ignore-zones? :shared})

(a/defargs settings
  {::gather-tries {:default 3 :doc "Without :fence how many times the body walks on to pull a trailing animal nearer." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 180 :doc "Seconds of the job's own working time before the leading gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-ties {:default 2 :doc "Ties tried before the job gives up." :spec (a/int-in 0 nil)}
   ::moved-eps {:default 0.25 :doc "An animal that moved less than this many blocks between two looks has settled." :spec (a/num-in 0 nil)}
   ::max-settles {:default 20 :doc "Looks spent waiting for the animal to settle before a pull." :spec (a/int-in 0 nil)}
   ::max-reach {:default 10 :doc "The farthest a pull's target lies from the animal. The lead breaks past engine.game/leash-break blocks, so 10 leaves a margin." :spec (a/num-in 0 nil)}
   ::path-reach {:default 11 :doc "How far from the animal any point of a pull's walk may lie; a path that strays farther (a detour round a wall) would break the lead." :spec (a/num-in 0 nil)}
   ::pull-range {:default 1 :doc "How close to the pull target the walk must get." :spec (a/num-in 0 nil)}
   ::pull-slack {:default 1 :doc "Blocks a pull aims farther than the lead's length strictly needs: the animal comes to a little over it." :spec (a/num-in 0 nil)}
   ::tie-settle-ms {:default 250 :doc "How long to wait between looks at the animal after the click on the post." :spec (a/int-in 0 nil)}
   ::tie-settle-tries {:default 6 :doc "How often the animal is looked at again after the click before the tie counts as not taken." :spec (a/int-in 0 nil)}})
(defn gather-tries [] (settings/get settings ::gather-tries))
(defn max-ties [] (settings/get settings ::max-ties))
(defn moved-eps [] (settings/get settings ::moved-eps))
(defn max-settles [] (settings/get settings ::max-settles))
(defn max-reach [] (settings/get settings ::max-reach))
(defn path-reach [] (settings/get settings ::path-reach))
(defn pull-range [] (settings/get settings ::pull-range))
(defn pull-slack [] (settings/get settings ::pull-slack))

(defn stop-text
  "The words for a leading that ended without success."
  [c reason]
  (let [{:keys [mob radius]} (:args c)]
    (case reason
      :none (str "no " mob " seen within " radius " blocks to lead")
      :no-lead (str "no lead carried and none could be got" (some->> (:lead-why (ctx/mem c)) (str ": ")))
      (str "leading stopped: " (name reason)))))

(def ending-of
  "The kind and cause of a leading that ended for reason, else nothing-possible with the reason as the cause."
  {:tied [:done nil]
   :unleashed [:done nil]
   :no-fence [:condition-failed :no-fence]
   :refused [:condition-failed :refused]
   :no-zones [:condition-failed :no-zones]})

(defn end!
  "End the call with the ending of reason (ending-of) and data; a failed child's ending kept in :because is attached."
  [c reason data]
  (let [[kind cause] (get ending-of reason [:nothing-possible reason])]
    (contract/with-because (ctx/end c kind cause data) (:because (ctx/mem c)))))

(defn child-reason
  "Why a child that did not finish done stopped: the :reason its data names, else its cause."
  [e]
  (or (get-in e [:data :reason]) (contract/cause-key e)))

(defn result-of
  "The result of a leading that ended for reason."
  [c reason]
  (let [m (ctx/mem c)]
    (cond-> {:reason reason
             :animal (:animal m)
             :still-led (boolean (:still-led m))
             :gathered (boolean (:gathered m))
             :at (u/self-pos c)
             :animal-at (some-> (escort/animal-now c) .-pos u/pos-of)}
      (escort/end-of reason) (assoc :end (escort/end-of reason)))))

(defn conclude!
  "Hand the outcome to the parent and end the job."
  [c reason]
  (let [ok? (#{:tied :unleashed} reason)
        result (result-of c reason)]
    (end! c reason (assoc result :text (if ok? (str "lead-to done: " (name reason)) (stop-text c reason))))))

(defn finish!
  "End the leading with reason (conclude! at the next step). Any reason but :tied and :unleashed is a stop."
  [c reason]
  (ctx/update-mem! c assoc :ending reason)
  :again)

(defn fail-with!
  "Finish with reason, the ending e of the child that failed kept as the :because."
  [c reason e]
  (ctx/update-mem! c assoc :because e)
  (finish! c reason))

(defn destination [c]
  (let [{:keys [pos fence]} (:args c)]
    (or pos fence)))

(defn fence-block?
  "True when the post cell is seen and a fence, false when seen and not, nil when never seen."
  [c]
  (some-> (u/seen-name (:primitives c) (:fence (:args c))) (.endsWith "_fence")))

(defn not-fence?
  "True when the post is seen and no fence. A post never seen (far off) is not judged: it is checked on arrival."
  [c]
  (false? (fence-block? c)))

(def ways need/fetch-ways)

(def target [:mob :pos :fence])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s}})

(defn can-start?
  "Never without :mob or a destination; later :no-fence while the :fence cell is seen and no fence, :no-lead with
  no lead carried and ways #{:carried}. A missing lead is otherwise fetched."
  [c]
  (let [{:keys [mob pos fence]} (:args c)]
    (cond
      (nil? mob) (contract/never :no-mob "lead-to needs a :mob")
      (not (or pos fence)) (contract/never :no-destination "lead-to needs a :pos or a :fence")
      (and fence (not-fence? c)) (contract/later :no-fence "lead-to waits: the named cell is not a fence")
      (not (or (need/fetch-opts c) (animals/lead-carried? c))) (contract/later :no-lead "lead-to waits: no lead carried and the ways are only #{:carried}")
      :else (contract/ok :order "the order is complete"))))

(defn quote-of [c]
  {:uses {:job-time [0 60 ##Inf]}})

(defn tied-at-post
  "An animal of :mob in sight held by a knot on the :fence cell, else nil."
  [c]
  (let [{:keys [mob fence]} (:args c)
        at-post? (fn [k] (and (= (:x fence) (js/Math.floor (.-x (.-pos k)))) (= (:z fence) (js/Math.floor (.-z (.-pos k))))))]
    (when fence
      (some #(when-let [k (animals/knot-of c %)] (when (at-post? k) %))
            (animals/herd (:primitives c) mob (escort/watch-radius))))))

(defn gates-shut?
  "True when no gate or door this job's legs opened is still open."
  [c]
  (not-any? #(false? (:shut? %)) (escort/walk-entries c)))

(defn done?
  "Ok when the animal is tied to the post (also one found tied before the start) or let go, off this body's lead, and the
  gates the legs opened are shut; else later."
  [c]
  (let [m (ctx/mem c)
        tied (tied-at-post c)
        reason (cond
                 (or (:still-led m) (not (gates-shut? c))) nil
                 (#{:tied :unleashed} (:ending m)) (:ending m)
                 tied :tied)
        n (if reason 1 0)]
    (if reason
      (contract/ok reason "the animal is tied or let go" :units :animals :done 1 :left 0
                   :result (cond-> (result-of c reason) (not (:animal m)) (assoc :animal (some-> tied animals/key-of))))
      (contract/later :leading "the animal is on the lead" :units :animals :done n :left (- 1 n)
                      :result {:animal (:animal m) :still-led (boolean (:still-led m))}))))

(defn set-phase! [c phase]
  (ctx/update-mem! c assoc :phase phase))

(defn ^:async get-lead!
  "Without a lead: a jobs.items.obtain child gets one (chest or craft). Any child failure ends :no-lead."
  [c]
  (let [limits (select-keys (need/fetch-opts c) [:how :explore :minutes])
        e (await (need/item! c "lead" 1 (merge {:slot :obtain :explore false} limits)))]
    (cond
      (= :continue e) :yield
      (animals/lead-carried? c) (do (set-phase! c :leash) :again)
      :else (do (ctx/update-mem! c assoc :lead-why (name (contract/cause-key e)))
                (fail-with! c :no-lead e)))))

(defn ^:async leash! [c]
  (let [{:keys [mob radius]} (:args c)
        e (await (ctx/call c :leash 'jobs.animals.leash
                           (cond-> {:mob mob :radius radius :center (:origin (ctx/mem c))} (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (cond
      (= :continue e) :yield
      (not= :done (:kind e)) (fail-with! c (child-reason e) e)
      :else (do (ctx/update-mem! c assoc :animal (get-in e [:data :animal]) :still-led true :phase :walk)
                :again))))

(defn pull-point
  "Where the body walks to pull the animal to within radius of pos: past pos, on the line from the
  animal through pos, by the lead length less radius plus the walk's range and a block of slack (flat, pos's y: a body that
  stops that far short still leaves the animal within radius) but never so far that the body ends more
  than max-reach from the animal, or nil when the animal is at pos or too far out for any pull."
  [animal-pos pos radius]
  (let [d (u/flat-dist animal-pos pos)
        extra (min (+ (- game/lead-trail radius) (pull-range) (pull-slack)) (- (max-reach) d))
        k (/ extra d)]
    (when (and (pos? d) (pos? extra))
      (assoc pos
             :x (+ (:x pos) (* k (- (:x pos) (:x animal-pos))))
             :z (+ (:z pos) (* k (- (:z pos) (:z animal-pos))))))))

(defn ^:async path-leaves-reach?
  "True when the walk the body would now take to target passes farther than path-reach from the animal. A body
  that cannot plan (no path sensing, no path, a planner that throws) is not judged here: go-to deals with that."
  [c target animal-pos]
  (let [pw (wworld/path-world (:primitives c))
        plan (when pw (await (escort/plan-or-nil c pw target)))]
    (boolean
     (some #(> (js/Math.hypot (- (:px %) (:x animal-pos)) (- (:pz %) (:z animal-pos))) (path-reach))
           (:steps plan)))))

(defn ^:async walk! [c]
  (let [r (await (escort/walk-legs! c :walk (destination c) (:range (:args c)) (escort/safe-gap)))]
    (cond
      (= :waiting r) :yield
      (#{:searching :stopped} r) :again
      (= :catching-up r) (await (escort/wait-for-animal! c))
      (= :arrived r) (do (set-phase! c (if (:fence (:args c)) :arrive :gather)) :again)
      (= :failed r) (finish! c :unreachable)
      :else (do (ctx/update-mem! c assoc :still-led (= :animal-stalled (:problem r)))
                (finish! c (:problem r))))))

(defn stop-gathering!
  "Give up pulling: the animal is let go where it is, :gathered false at distance d from the spot."
  [c d]
  (ctx/update-mem! c assoc :gathered false :gather-dist d)
  (ctx/update-mem! c dissoc :pull-target)
  (ctx/update-mem! c update :children dissoc :pull)
  (set-phase! c :arrive)
  :again)

(defn ^:async pull! [c target animal-pos]
  (let [d (u/flat-dist animal-pos (:pos (:args c)))]
    (if (await (path-leaves-reach? c target animal-pos))
      (stop-gathering! c d)
      (let [r (await (escort/walk-legs! c :pull target (pull-range) (escort/follow-reach)))]
        (cond
          (= :waiting r) :yield
          (#{:searching :stopped} r) :again
          (= :catching-up r) (await (escort/wait-for-animal! c))
          (map? r) (do (ctx/update-mem! c assoc :still-led (= :animal-stalled (:problem r)))
                       (finish! c (:problem r)))
          :else (do (ctx/update-mem! c dissoc :pull-target)
                    (ctx/update-mem! c update :pulls (fnil inc 0))
                    (if (= :arrived r) :again (stop-gathering! c d))))))))

(defn ^:async gather!
  "Without :fence: let the animal catch up. A pull in progress is carried on with its stored target;
  otherwise the animal is looked at: near enough (or the pulls spent) is :arrive, one still moving
  is given time to settle (the animal lags the body), else a pull starts."
  [c a]
  (let [{:keys [pos gather-radius]} (:args c)
        {:keys [pull-target pulls gather-seen settles] :or {pulls 0 settles 0}} (ctx/mem c)
        animal-pos (u/pos-of (.-pos a))
        d (u/flat-dist animal-pos pos)]
    (cond
      pull-target (await (pull! c pull-target animal-pos))
      (<= d gather-radius) (do (ctx/update-mem! c assoc :gathered true) (set-phase! c :arrive) :again)
      (>= pulls (gather-tries)) (stop-gathering! c d)
      (and (< settles (max-settles)) (or (nil? gather-seen) (> (u/flat-dist gather-seen animal-pos) (moved-eps))))
      (do (ctx/update-mem! c assoc :gather-seen animal-pos :settles (inc settles))
          (await (escort/wait-for-animal! c)))
      :else (let [target (pull-point animal-pos pos gather-radius)]
              (ctx/update-mem! c dissoc :gather-seen)
              (if-not target
                (stop-gathering! c d)
                (do (ctx/update-mem! c assoc :pull-target target :settles 0 :gather-dist d)
                    (await (pull! c target animal-pos))))))))

(defn ^:async let-go! [c]
  (let [e (await (ctx/call c :unleash 'jobs.animals.unleash {:mob (:mob (:args c)) :animal (:animal (ctx/mem c))
                                                              :radius (escort/watch-radius)
                                                              :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue e) :yield
      (not= :done (:kind e)) (fail-with! c (child-reason e) e)
      :else (do (ctx/update-mem! c assoc :still-led false)
                (finish! c (child-reason e))))))

(defn tied?
  "True when the animal is held by something else than this body."
  [c]
  (let [a (escort/animal-now c)]
    (and a (animals/leashed? a) (not (animals/led-by-me? a)))))

(defn tie-settle-ms [] (settings/get settings ::tie-settle-ms))
(defn tie-settle-tries [] (settings/get settings ::tie-settle-tries))

(defn ^:async await-tied!
  "True once the animal is held by something else than this body, looking tries more times a beat apart: the
  state lands a tick or two after the click."
  [c tries]
  (let [r (await (child/poll! c #(tied? c) (tie-settle-ms) tries :tie-settling))]
    (if (= :continue r) :continue (boolean r))))

(defn ^:async tie! [c]
  (let [fence (:fence (:args c))
        near (await (near/go-near! c fence 2 {:doors :never :ways #{:walk :clear-seal}}))]
    (if-not (= :there near)
      (if (= :partial near) :again (finish! c :unreachable))
      (let [r (await (child/use-on! c fence "none" "up"))]
        (if (= :continue r)
          :continue
          (let [tries (inc (:ties (ctx/mem c) 0))]
            (ctx/update-mem! c assoc :ties tries)
            (let [tied (await (await-tied! c (tie-settle-tries)))]
              (cond
                (= :continue tied) :continue
                tied (do (ctx/update-mem! c assoc :still-led false :gathered true) (finish! c :tied))
                (>= tries (max-ties)) (finish! c :tie-failed)
                :else (do (ctx/emit! c :lead-to.tie-retry :info {:status (:status r) :text "the post did not take the animal, trying once more"})
                          :again)))))))))

(defn ^:async arrive! [c]
  (let [{:keys [gathered gather-dist]} (ctx/mem c)
        a (escort/animal-now c)
        distance (if a (u/flat-dist (u/pos-of (.-pos a)) (:pos (:args c))) gather-dist)]
    (cond
      (:fence (:args c)) (await (tie! c))
      gathered (await (let-go! c))
      :else (do (ctx/emit! c :lead-to.gather-short :warn
                           {:distance distance
                            :text (str "the animal is let go " (some-> distance js/Math.round) " blocks from the spot, not gathered")})
                (await (let-go! c))))))

(defn ^:async step [c]
  (let [{:keys [fence]} (:args c)]
    (let [{:keys [phase animal]} (ctx/mem c)
          a (when animal (escort/animal-now c))]
      (when a (ctx/update-mem! c dissoc :unseen-since))
      (cond
        (:ending (ctx/mem c)) (conclude! c (:ending (ctx/mem c)))
        (and fence (contains? #{nil :arrive} phase) (not-fence? c)) (finish! c :no-fence)
        (nil? phase) (do (ctx/update-mem! c assoc :origin (u/self-pos c))
                         (set-phase! c (if (animals/lead-carried? c) :leash :get-lead)) :again)
        (= :get-lead phase) (await (get-lead! c))
        (= :leash phase) (await (leash! c))
        (and (#{:gather :arrive} phase) (nil? a)) (if (= :lost (escort/escort-problem c (escort/safe-gap)))
                                                    (do (ctx/update-mem! c assoc :still-led false) (finish! c :lost))
                                                    (await (escort/wait-for-animal! c)))
        (and (#{:walk :gather :arrive} phase) a (not (animals/led-by-me? a))) (do (ctx/update-mem! c assoc :still-led false) (finish! c :lead-broke))
        (= :walk phase) (do (await (watch/watch! c {})) (await (walk! c)))
        (= :gather phase) (do (await (watch/watch! c {})) (await (gather! c a)))
        (= :arrive phase) (await (arrive! c))
        :else (finish! c :lost)))))

(defn ^:async round
  "The whole attempt: steps until the job ends; yields only while a child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (let [r (await (step c))] (if (= :yield r) :continue r))))))

(defn ^:async cleanup
  "However the leading ended: an animal still on this body's lead is let go (jobs.animals.unleash), then the doors and
  gates the legs left open are shut (escort/close-up!), also when the let-go failed. Done with {:still-led false}, or
  the let-go's failing ending."
  [c _]
  (let [e (when (:still-led (ctx/mem c))
            (await (ctx/call c :unleash 'jobs.animals.unleash {:mob (:mob (:args c)) :animal (:animal (ctx/mem c))
                                                                :radius (escort/watch-radius)
                                                                :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (if (= :continue e)
      :continue
      (do (when e (ctx/update-mem! c assoc :still-led false :unleash-failed (when-not (= :done (:kind e)) e)))
          (let [r (await (escort/close-up! c (fn [c _] (or (:unleash-failed (ctx/mem c))
                                                           (ctx/end c :done nil {:still-led false})))))]
            (if (= :yield r) :continue r))))))
