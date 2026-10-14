(ns jobs.animals.lead-to
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.blocks :as b]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.started :as started]
            [jobs.lib.walk.world :as wworld]
            [jobs.animals.escort :as escort]))

(def doc
  "Put a lead on one animal of type :mob, walk to :pos with it following, then tie it to the fence post :fence
  or let it go. A one-shot order that starts and ends itself. Without :mob or a destination (:pos or :fence) it can never start
  (impossible :no-mob, :no-destination); with a :fence cell seen and not a fence it waits :no-fence. A body that carries no
  lead first gets one (phase :get-lead, a jobs.items.obtain child: withdrawn from a chest it has seen or crafted); when none
  can be got the job ends :no-lead at once, it never waits for one.

  Phases, each by a child job:
  - :leash: jobs.animals.leash (radius :radius).
  - :walk: jobs.movement.go-to to :pos (the :fence cell when no :pos), range :range, in legs of a few blocks along
    the planned path. One call is the whole run; it yields only while a child waits on the world. After each leg the animal is looked at: off the lead
    :lead-broke; unseen or more than 7 blocks behind, the body holds still (:waiting-for-animal) for it to come into sight or
    catch up; unseen for 3 s (a ledge or tree can hide it) the body walks back toward where it was last seen (up to 12 blocks), then :lost;
    still more than 7 behind and not closing in for 10 s (escort/::stall-s), :animal-stalled.
  - :gather (no :fence only): see below.
  - :arrive: with :fence, walk within 2 of the post and click it with an empty hand (useOn), tried twice.
    Without :fence, jobs.animals.unleash lets the animal go and picks the lead up.

  With :fence the block there must be a fence (name ends _fence), else :no-fence before anything is leashed; a post not yet seen (far off) is checked again on arrival.
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

  The legs let doors and gates stay open for the animal to follow; when the leading ends, in any outcome, a
  jobs.maintenance.shut-doors child shuts the ones the legs opened.

  Ends with info lead-to.done and a warn lead-to.gave-up unless the reason is :tied or :unleashed. Result
  {:reason :animal key :still-led bool :at pos :gathered bool :text}. Reasons and endings:
  - :tied, :unleashed: done.
  - :none (no animal seen within :radius), :lead-broke, :lost, :animal-stalled (the animal is still on the lead): as above.
  - :unreachable: the walk gave up (the animal is still on the lead).
  - :tie-failed: the post did not take the animal (still on the lead).
  - :timeout: limit-reached; the call's :spend :time (default ::timeout-s) from the first round, the lead fetch and
    the children included (a cut walk leaves the animal on the lead).
  - :no-fence: condition-failed, the post turned out not to be a fence on arrival.
  - :no-lead: no lead carried and obtain could not get one (the :text says why).
  - The reason of jobs.animals.leash (:no-lead, :none, :unreachable, :all-leashed) when no animal got on the lead, or
    of jobs.animals.unleash (:unreachable, :none) when the lead would not come off; :refused and :no-zones are
    condition-failed.
  All but done, condition-failed and the timeout end nothing-possible with the reason as the cause.

  Zones: the leash child refuses an animal standing in another owner's zone or claim (see jobs.animals.leash);
  :ignore-zones? true is passed to it.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :pos {:doc "where to lead it {:x :y :z}; the :fence cell when nil" :spec ::a/pos :default nil}
   :fence {:doc "the fence post {:x :y :z} to tie it to; unleash it at :pos when nil" :spec ::a/pos :default nil}
   :range {:doc "how close to :pos counts as there" :spec (a/num-in 0 nil) :default 2}
   :radius {:doc "animals within this many blocks of where the job starts are leashed" :spec (a/num-in 0 nil) :default 8}
   :gather-radius {:doc "without :fence the animal is let go once it is within this many blocks of :pos" :spec (a/num-in 0 nil) :default 3}
   :gather-tries {:doc "without :fence how many times the body walks on to pull a trailing animal nearer" :spec (a/int-in 0 nil) :default 3}
   :watch-radius {:doc "how far from the body the led animal is looked for" :spec (a/num-in 0 nil) :default 64}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false :spec boolean?}})

(a/defargs settings
  {::timeout-s {:default 180 :doc "Seconds from the first round before the leading gives up." :spec (a/num-in 0 nil)}
   ::max-ties {:default 2 :doc "Ties tried before the job gives up." :spec (a/int-in 0 nil)}
   ::tie-settle-ms {:default 250 :doc "How long to wait between looks at the animal after the click on the post." :spec (a/int-in 0 nil)}
   ::tie-settle-tries {:default 6 :doc "How often the animal is looked at again after the click before the tie counts as not taken." :spec (a/int-in 0 nil)}})
(defn timeout-s [] (settings/get settings ::timeout-s))
(defn max-ties [] (settings/get settings ::max-ties))

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
   :no-zones [:condition-failed :no-zones]
   :timeout [:limit-reached {:line :time :reason :timeout}]})

(defn end!
  "End the call with the ending of reason (ending-of) and data."
  [c reason data]
  (let [[kind cause] (get ending-of reason [:nothing-possible reason])]
    (ctx/end c kind cause data)))

(defn conclude!
  "Emit the outcome, hand it to the parent and end the job."
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
    (end! c reason (cond-> result (not ok?) (assoc :text (stop-text c reason))))))

(defn finish!
  "End the leading with reason: the gates its walks left open are shut first (close-up!), then conclude!. Any reason but
  :tied and :unleashed is a stop."
  [c reason]
  (ctx/update-mem! c assoc :ending reason)
  :again)

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

(def target [:mob :pos :fence])

(def defaults {:spend {:time ::timeout-s}})

(defn start-fact
  "Impossible without :mob or a destination; a wait :no-fence while the :fence cell is seen and no fence. A missing lead
  is fetched, never waited for."
  [c]
  (let [{:keys [mob pos fence]} (:args c)]
    (cond
      (nil? mob) (contract/impossible :no-mob)
      (not (or pos fence)) (contract/impossible :no-destination)
      (and fence (not-fence? c)) {:reason :no-fence :text "lead-to waits: the named cell is not a fence"})))

(def conditions "Starts while the order is complete and the post is no seen non-fence (start-fact)." [[:fn (started/at-start start-fact)]])

(defn time-limit "Seconds the leading may take." [c]
  (or (get-in (ctx/frozen-call c) [:spend :time]) (timeout-s)))

(defn quote-of [c]
  (let [most (time-limit c)]
    {:uses {:time [0 (min most 60) most]}}))

(defn done?
  "Whether the animal is tied or let go. Never :done? itself: the run ends with its event and data."
  [c]
  {:done? false :units :animals :done (if (#{:tied :unleashed} (:ending (ctx/mem c))) 1 0) :left 1})

(defn set-phase! [c phase]
  (ctx/update-mem! c assoc :phase phase))

(defn ^:async get-lead!
  "Without a lead: a jobs.items.obtain child gets one (chest or craft). Any child failure ends :no-lead."
  [c]
  (let [oargs {:item "lead" :count 1 :explore false}
        r (await (ctx/call-child c :obtain 'jobs.items.obtain oargs))]
    (cond
      (= :continue r) :yield
      (= :declined r) (do (ctx/update-mem! c assoc :lead-why (some-> (b/child-wait c :obtain 'jobs.items.obtain oargs) :reason name))
                          (finish! c :no-lead))
      (animals/lead-carried? c) (do (set-phase! c :leash) :again)
      :else (do (ctx/update-mem! c assoc :lead-why (some-> (ctx/child-result c :obtain) :reason name))
                (finish! c :no-lead)))))

(defn ^:async leash! [c]
  (let [{:keys [mob radius]} (:args c)
        r (await (ctx/call-child c :leash 'jobs.animals.leash
                                 (cond-> {:mob mob :radius radius :center (:origin (ctx/mem c))} (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (if-not (= :done r)
      :yield
      (let [res (ctx/child-result c :leash)]
        (if-not (= :leashed (:reason res))
          (finish! c (get-in res [:ending :data :reason] (:reason res)))
          (do (ctx/update-mem! c assoc :animal (:animal res) :still-led true :phase :walk)
              :again))))))

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
  (let [d (u/flat-dist animal-pos pos)
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

(defn ^:async path-leaves-reach?
  "True when the walk the body would now take to target passes farther than path-reach from the animal. A body
  that cannot plan (no path sensing, no path, a planner that throws) is not judged here: go-to deals with that."
  [c target animal-pos]
  (let [pw (wworld/path-world (:primitives c))
        plan (when pw (await (escort/plan-or-nil c pw target)))]
    (boolean
     (some #(> (js/Math.hypot (- (:px %) (:x animal-pos)) (- (:pz %) (:z animal-pos))) path-reach)
           (:steps plan)))))

(defn ^:async walk! [c]
  (let [r (await (escort/walk-legs! c :walk (destination c) (:range (:args c)) nil nil escort/safe-gap))]
    (cond
      (= :waiting r) :yield
      (= :searching r) :again
      (= :catching-up r) (await (escort/wait-for-animal! c))
      (= :arrived r) (do (set-phase! c (if (:fence (:args c)) :arrive :gather)) :again)
      (= :failed r) (finish! c :unreachable)
      :else (do (ctx/update-mem! c assoc :still-led (= :animal-stalled (:problem r)))
                (finish! c (:problem r))))))

(defn stop-gathering!
  "Give up pulling: the animal is let go where it is, :gathered false at distance d from the spot."
  [c d]
  (ctx/update-mem! c assoc :gathered false :gather-dist d)
  (ctx/update-mem! c dissoc :pull-target :pull-started)
  (ctx/update-mem! c update :children dissoc :pull)
  (set-phase! c :arrive)
  :again)

(defn ^:async pull! [c target animal-pos]
  (let [now (ctx/now c)
        started (:pull-started (ctx/mem c) now)
        d (u/flat-dist animal-pos (:pos (:args c)))]
    (ctx/update-mem! c assoc :pull-started started)
    (cond
      (>= (- now started) (* 1000 pull-timeout-s)) (stop-gathering! c d)
      (await (path-leaves-reach? c target animal-pos)) (stop-gathering! c d)
      :else
      (let [r (await (escort/walk-legs! c :pull target pull-range started (* 1000 pull-timeout-s) escort/follow-reach))]
        (cond
          (= :waiting r) :yield
          (= :searching r) :again
          (= :catching-up r) (await (escort/wait-for-animal! c))
          (map? r) (do (ctx/update-mem! c assoc :still-led (= :animal-stalled (:problem r)))
                       (finish! c (:problem r)))
          :else (do (ctx/update-mem! c dissoc :pull-target :pull-started)
                    (ctx/update-mem! c update :pulls (fnil inc 0))
                    (if (= :arrived r) :again (stop-gathering! c d))))))))

(defn ^:async gather!
  "Without :fence: let the animal catch up. A pull in progress is carried on with its stored target;
  otherwise the animal is looked at: near enough (or the pulls spent) is :arrive, one still moving
  is given time to settle (the animal lags the body), else a pull starts."
  [c a]
  (let [{:keys [pos gather-radius gather-tries]} (:args c)
        {:keys [pull-target pulls gather-seen settles] :or {pulls 0 settles 0}} (ctx/mem c)
        animal-pos (u/pos-of (.-pos a))
        d (u/flat-dist animal-pos pos)]
    (cond
      pull-target (await (pull! c pull-target animal-pos))
      (<= d gather-radius) (do (ctx/update-mem! c assoc :gathered true) (set-phase! c :arrive) :again)
      (>= pulls gather-tries) (stop-gathering! c d)
      (and (< settles max-settles) (or (nil? gather-seen) (> (u/flat-dist gather-seen animal-pos) moved-eps)))
      (do (ctx/update-mem! c assoc :gather-seen animal-pos :settles (inc settles))
          (await (escort/wait-for-animal! c)))
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
      :yield
      (let [reason (:reason (ctx/child-result c :unleash))]
        (when (= :unleashed reason) (ctx/update-mem! c assoc :still-led false))
        (finish! c reason)))))

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
  (cond
    (tied? c) true
    (zero? tries) false
    :else (do (await (ctx/act c :wait #js {:ms (tie-settle-ms)}))
              (await (await-tied! c (dec tries))))))

(defn ^:async tie! [c]
  (let [fence (:fence (:args c))
        near (await (near/go-near! c fence 2 {:doors :never :zone-tolls true :escalate false}))]
    (if-not (= :there near)
      (if (= :partial near) :again (finish! c :unreachable))
      (let [r (await (ctx/act c :useOn (clj->js {:pos fence})))
            tries (inc (:ties (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :ties tries)
        (cond
          (await (await-tied! c (tie-settle-tries))) (do (ctx/update-mem! c assoc :still-led false :gathered true) (finish! c :tied))
          (>= tries (max-ties)) (finish! c :tie-failed)
          :else (do (ctx/emit! c :lead-to.tie-retry :info {:status (.-status r) :text "the post did not take the animal, trying once more"})
                    :again))))))

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
  (let [now (ctx/now c)
        {:keys [fence]} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [{:keys [phase started animal]} (ctx/mem c)
          a (when animal (escort/animal-now c))]
      (when a (ctx/update-mem! c dissoc :unseen-since))
      (cond
        (:ending (ctx/mem c)) (await (escort/close-up! c conclude!))
        (and fence (contains? #{nil :arrive} phase) (not-fence? c)) (finish! c :no-fence)
        (>= (- now started) (* 1000 (time-limit c))) (finish! c :timeout)
        (nil? phase) (do (ctx/update-mem! c assoc :origin (u/self-pos c))
                         (set-phase! c (if (animals/lead-carried? c) :leash :get-lead)) :again)
        (= :get-lead phase) (await (get-lead! c))
        (= :leash phase) (await (leash! c))
        (and (#{:gather :arrive} phase) (nil? a)) (if (= :lost (escort/escort-problem c escort/safe-gap))
                                                    (do (ctx/update-mem! c assoc :still-led false) (finish! c :lost))
                                                    (await (escort/wait-for-animal! c)))
        (and (#{:walk :gather :arrive} phase) a (not (animals/led-by-me? a))) (do (ctx/update-mem! c assoc :still-led false) (finish! c :lead-broke))
        (= :walk phase) (do (await (watch/watch! c {})) (await (walk! c)))
        (= :gather phase) (do (await (watch/watch! c {})) (await (gather! c a)))
        (= :arrive phase) (await (arrive! c))
        (= :release phase) (await (let-go! c))
        :else (finish! c :lost)))))

(defn ^:async round
  "The whole attempt: steps until the job ends; yields only while a child waits on the world."
  [c]
  (loop []
    (let [r (await (step c))]
      (case r
        :again (recur)
        :yield :continue
        r))))
