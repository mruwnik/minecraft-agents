(ns jobs.village.roll
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.blocks :as b]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.villager :as villager]
            [jobs.lib.villager-pen :as vp]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.walk :as walk]
            [jobs.lib.trade-window :as tw]))

(def doc
  "Re-roll the offers of an untraded villager (:villager uuid; nil: the nearest adult, not a nitwit, in sight)
  until one gives :want (with :enchant, an enchanted book of that enchantment, e.g. \"mending\"; :enchant may be a map
  name -> minimum level, {\"mending\" 1 \"unbreaking\" 3}: the first offer with any of them stops the roll, and the done text names it), by breaking and placing
  again the workstation of :profession at :pos.
  Needs: a profession, a wanted item (or enchantment) and a cell the body may break and place at (else impossible, :bad-args); a
  cell not yet seen is looked at, then walked to. An offer matches when it gives :want, is not sold out and costs at
  most :max-price (nil: no limit). A villager out of view is looked for (round, from the cell beside the workstation, at the spot the body last saw it, by day
  toward where one is heard);
  at night it is waited for till day (reason :night-villager-out-of-view), then looked for once more; one seen this call
  is waited for ::away-s by day; then :gone. Says which villager it picked (info roll.villager). The break, the wait for the villager to lose its job and
  the place again are one urgent section (a night or food reflex waits for it); with a spare workstation carried, the
  spare goes in the moment the villager reads jobless and the drop is picked up after, so the villager has no time to
  claim another free job site. Before a break, and while a jobless villager is waited for, the villager must stand shut in
  its pen (jobs.lib.villager-pen/penned: alone in a lit one-wide cell, the workstation under a top slab plugging its far
  end, its other sides shut, the body boxed in behind it); else a jobs.village.pen child shuts it in, and its ending other than done (but :villager-out-of-view, which the
  round looks for) is this call's. A villager that reads another job is never rolled: a named one ends :other-job, else the
  nearest other in sight is picked. Waits (:continue) for the penned villager to take its job, breaking and placing the workstation once (a roll) when
  that costs less than the wait for another villager's claim on it to lapse; villagers take jobs only by day, so at night the wait's reason is
  :night-villagers-take-no-job. :ignore-zones? goes to the dig and place children; :ways (default all) goes to the place child, so #{:carried} places only a workstation already carried (else it waits :need, nothing is fetched or crafted). Re-reads the world every step, so a restart finds a missing
  workstation and places it first; the cleanup places one the call broke however it ends.
  Returns {:found true|false :rolls n :item workstation :offer {:index :cost :gives ..}}: done when an offer
  matches, else
  - condition-failed: :gone, :baby, :locked (an offer was already used, the villager is above level 1 or its window shows xp: it has
    traded, and the game keeps a traded villager's job; a picked one is ruled out for the next nearest), :other-job, :occupied (another block at :pos),
    :cell-unseen.
  - impossible: :nitwit.
  - nothing-possible :no-job: the penned villager took no job in jobs.lib.villager/take-job-ticks game ticks of its walking
    hours; :sites names the other job-site blocks seen (a free one it may hold a claim on).
  - nothing-possible: :no-match (::max-rolls, 20, rolls without a match), :unreachable, :window (offers could not be
    read three times), :place-failed (no room to step off the cell), :workstation-lost (the broken workstation's drop is not carried, i.e. the carried count is below what it was before the break plus one, a spare in the bag does not stand in for it, after the body sneaked into the emptied cell (the pickup reach then covers the cell behind), walked to the drop and to its far side and waited a few polls; nothing is placed, the data names :drop-cell, the cell the drop lies in, and no block above the cell is ever dug), or the ending of the dig, place or pen child.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid or numeric entity id" :spec villager/id? :default nil}
   :pos {:doc "the workstation block, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :profession {:doc "villager profession whose workstation block is broken and placed again, e.g. librarian" :spec string? :default nil}
   :want {:doc "item name an offer is to give; nil with :enchant: enchanted_book" :spec a/item? :default nil}
   :enchant {:doc "enchantment an offer's item is to carry: a name (e.g. mending, any level) or a map name -> minimum level, e.g. {\"mending\" 1 \"unbreaking\" 3}; an offer with any one of them matches" :spec (a/or-of string? (a/map-of string? (a/int-in 1 nil))) :default nil}
   :max-price {:doc "highest price (first cost stack); nil for no limit" :spec (a/int-in 0 nil) :default nil}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(a/defargs settings
  {::max-rolls {:default 20 :doc "Rolls (breaks of the workstation) without a match before the call gives up (:no-match)." :spec (a/int-in 1 200)}
   ::away-s {:default 30 :doc "Seconds a villager seen before may stay out of view (after the workstation is placed) before the call ends :gone." :spec (a/int-in 1 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the waits for the villager's job included (:limit-reached)." :spec (a/int-in 1 nil)}
   ::reach {:default 2 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::leg-s {:default 15 :doc "Seconds one walk leg may take." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 1800 :doc "Seconds one search may take." :spec (a/int-in 1 nil)}})
(defn max-rolls [] (settings/get settings ::max-rolls))
(defn poll-s [] (villager/poll-s))
(defn away-s [] (settings/get settings ::away-s))
(defn reach [] (settings/get settings ::reach))
(defn leg-s [] (settings/get settings ::leg-s))

(defn item-of
  "The workstation item of :profession."
  [args]
  (villager/workstations (:profession args)))

(defn want-of [args] (or (:want args) (when (:enchant args) "enchanted_book")))

(defn worth-of
  "Seconds a root roll is worth: the cost/goal-value of one of the item the wanted offer gives."
  [c]
  (if-let [want (want-of (:args c))] (:value (cost/goal-value [{:name want :count 1}])) 0))

(def target [:villager :pos])

(def safe? (price/staying :work (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c)))))))
(def fit? ready/fed-fit?)


(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn bad-args
  "Never :bad-args when a cell, a profession with a workstation and a wanted item (or enchantment) are not given."
  [c]
  (let [{:keys [villager]} (:args c)
        {:keys [error]} (b/parse (:args c))]
    (cond
      (not (and (or (nil? villager) (villager/id? villager)) (string? (item-of (:args c))) (string? (want-of (:args c)))))
      (contract/never :bad-args "needs a known profession and a wanted item (or enchantment)"
                      :why "needs a known profession and a wanted item (or enchantment)")
      error (contract/never :bad-args error :why error)
      :else (contract/ok :args "a cell, a profession and a wanted item"))))

(defn start-facts
  "With a named villager in view: never :nitwit (never a job), later :baby for a baby and :occupied when another block
  stands at :pos. The rest is found out by the rounds."
  [c]
  (let [{:keys [villager]} (:args c)
        p (:primitives c)
        e (when (villager/id? villager) (villager/find-by-uuid p villager))
        block (when e (some->> (:pos (b/parse (:args c))) (u/seen-name p)))
        item (item-of (:args c))]
    (cond
      (nil? e) (contract/ok :villager "no villager in view to judge")
      (= "nitwit" (.-profession e)) (contract/never :nitwit "a nitwit has no job")
      (.-baby e) (contract/later :baby "the villager is a baby")
      (and block (not (bs/air block)) (not= item block)) (contract/later :occupied (str block " stands at the workstation cell") :block block)
      :else (contract/ok :villager "the villager can take a job"))))

(defn cell-unseen
  "Look :cell-unseen while the workstation cell is not in view or memory (after a look still unseen is the round's to
  find out)."
  [c]
  (let [pos (:pos (b/parse (:args c)))]
    (if (and pos (nil? (u/seen-name (:primitives c) pos)) (not (look/surveyed? c :near)))
      (contract/look :cell-unseen "the workstation cell is not in view or memory")
      (contract/ok :cell "the workstation cell is seen or looked at"))))

(def can-start? (cond/and bad-args start-facts cell-unseen))

(defn rolls [c] (:rolls (ctx/mem c) 0))

(defn data-of
  "The call's data: found with the offer once one matched, the rolls and the item."
  [c extra]
  (let [offer (:offer (ctx/mem c))]
    (merge {:found (boolean offer) :rolls (rolls c) :item (item-of (:args c))}
           (when offer {:offer offer})
           extra)))

(defn done?
  "Ok once an offer matched (found! books it), with the offer as its result; later while the rolls go on."
  [c]
  (let [data [:units :offers :done (if (:offer (ctx/mem c)) 1 0) :left (if (:offer (ctx/mem c)) 0 1) :result (data-of c nil)]]
    (if (:offer (ctx/mem c))
      (apply contract/ok :found (str "found " (or (:matched (ctx/mem c)) (want-of (:args c))) " after " (rolls c) " rolls") data)
      (apply contract/later :rolling (str (rolls c) " rolls without " (want-of (:args c))) data))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 30 (max-rolls)) ##Inf]}})

(def failed-conditions "Causes that end condition-failed: the villager or the cell is not right now. The rest are nothing-possible."
  #{:gone :baby :locked :other-job :occupied :cell-unseen})

(defn stop!
  "End with reason (a keyword) as the cause: condition-failed for those in failed-conditions, else nothing-possible."
  [c reason]
  (ctx/end c (cond (= :nitwit reason) :impossible (failed-conditions (:reason reason reason)) :condition-failed :else :nothing-possible)
           reason (data-of c nil)))

(defn found!
  "Book the matching offer; done? ends the job."
  [c offer]
  (let [e (some->> (:enchant (:args c)) tw/enchant-wants (#(tw/carries % (:gives offer))))]
    (ctx/update-mem! c assoc :offer (select-keys offer [:index :cost :gives])
                     :matched (when e (str (:name e) " " (:level e)))))
  :again)

(defn not-nitwit? [e] (not= "nitwit" (.-profession e)))

(defn other-job?
  "Whether villager e reads a job other than the call's :profession: it holds another job site, so it never takes ours
  and breaking ours does nothing to it."
  [c e]
  (let [job (.-profession e)]
    (boolean (and job (not (#{"unemployed" "nitwit" (:profession (:args c))} job))))))

(defn rollable
  "A filter of the villagers the call may pick: no nitwit, no other job, none ruled out (rule-out!)."
  [c]
  (let [out (:ruled-out (ctx/mem c) #{})]
    (fn [e] (and (not-nitwit? e) (not (other-job? c e)) (not (contains? out (villager/ident e)))))))

(def villager-keys "Memory about the villager picked, dropped when another is picked."
  [:chosen :seen :away-since :view :view-tries :last-seen :flanked :memory-walked :night-waited :vantage :job-at :job-ticks :claim-reset])

(defn pick!
  "The villager rolled: :villager, else the nearest adult that is no nitwit (saying which one it picked, once)."
  [c]
  (let [before (villager/chosen c)
        id (villager/pick! c (rollable c))]
    (when (and id (nil? before))
      (let [e (villager/find-by-uuid (:primitives c) id)]
        (ctx/emit! c :roll.villager :info
                   {:villager id :pos (some-> e (.-pos) u/pos-of) :profession (some-> e (.-profession))
                    :text (str "rolls villager " id ": the nearest adult that is no nitwit")})))
    id))

(defn rule-out!
  "The villager cannot be rolled (reason :other-job or :locked): a named one ends the call; a picked one is left for the
  nearest other in sight that can be (:again; pick! names it), none ends the call."
  [c reason]
  (if (:villager (:args c))
    (stop! c reason)
    (let [id (villager/chosen c)]
      (ctx/update-mem! c #(-> (apply dissoc % villager-keys) (update :ruled-out (fnil conj #{}) id)))
      (if (some (rollable c) (villager/adults (:primitives c))) :again (stop! c reason)))))

(defn ^:async wait-job!
  "Hold still for a poll, saying why the villager's job is awaited; :continue. At night the reason says villagers
  claim workstations only in the day."
  [c why]
  (await (child/pause! c (poll-s) (if (and (= why :villager-takes-no-job) (sh-sense/night? (:primitives c)))
                                    :night-villagers-take-no-job
                                    why)))
  :continue)

(defn jobless?
  "Whether the villager rolled is in view and reads jobless."
  [c]
  (some-> (villager/find-by-uuid (:primitives c) (villager/chosen c)) .-profession (= "unemployed")))

(defn ^:async wait-lose-job!
  "The villager keeps its job after the break. The game takes it a tick after the break from an untraded villager near
  its workstation, and a jobless villager soon looks for any free one: the first time after a break, look every game
  tick for one poll and go on (:again) the moment it reads jobless, so the place follows at once; else, as on later
  rounds (a villager away from the workstation loses the job only once back near it), wait-job! (:continue)."
  [c]
  (if (:lose-polled (ctx/mem c))
    (await (wait-job! c :villager-keeps-job))
    (let [tick-ms (settings/ticks->ms 1)
          _ (ctx/update-mem! c assoc :lose-polled true)
          r (await (child/poll! c #(jobless? c) tick-ms (js/Math.ceil (/ (* 1000 (poll-s)) tick-ms)) :villager-keeps-job))]
      (if (and r (not= :continue r)) :again :continue))))

(defn ^:async walk!
  "One go-to leg toward the villager e. A leg that got no nearer is one try: the next round aims at where the villager
  is then. :again, :continue or the stop."
  [c e]
  (let [r (await (child/call-chase* c :walk
                                   {:pos (u/pos-of (.-pos e)) :entity (.-id e) :range (reach) :leg-s (leg-s) :walk {:ways #{:walk :clear-seal}}}))]
    (cond
      (= :continue r) :continue
      (let [res (:data r)] (or (:arrived res) (:nearer res) (:leg res))) :again
      :else (stop! c :unreachable))))

(defn ^:async approach!
  "The workstation cell is not in view or memory: walk to it (go-to child, slot :cell) once; if it is still unseen
  there, end :cell-unseen. :again, :continue or the stop."
  [c pos]
  (if (:approached (ctx/mem c))
    (stop! c :cell-unseen)
    (let [r (await (child/call-walk* c :cell {:pos pos :range (reach) :ways #{:walk :clear-seal}}))]
      (cond
        (= :continue r) :continue
        (:arrived (:data r)) (do (ctx/update-mem! c assoc :approached true) :again)
        :else (stop! c :unreachable)))))

(defn ^:async read-offers!
  "In reach of the villager: read its offers and decide. :found, :locked, :miss (no match), or :again/:continue/stop."
  [c]
  (let [{:keys [max-price enchant]} (:args c)
        villager (villager/chosen c)
        want (want-of (:args c))
        r (await (ctx/act c :trade (clj->js {:villager villager :op "offers"})))
        status (.-status r)]
    (case status
      "ok" (let [offers (js->clj (.-offers r) :keywordize-keys true)
                 {:keys [offer]} (tw/choose offers {:buy want :max-price max-price :enchant enchant})]
             (u/progress! c)
             (tw/emit-offers! c :roll.offers villager r offers)
             (cond
               (or (some #(pos? (:uses % 0)) offers) (< 1 (or (.-level r) 1)) (pos? (or (.-xp r) 0))) :locked
               offer {:found offer}
               :else :miss))
      "gone" (stop! c (villager/gone-cause c))
      "out-of-reach" :again
      (if (u/count-fail! c)
        (ctx/end c :nothing-possible :window (data-of c nil))
        :continue))))

(defn ending-or-wait?
  "Whether a child! result is :continue or a stop, not the child's result."
  [r]
  (or (keyword? r) (contract/ending? r)))

(defn ^:async back-off!
  "Step three blocks back from the workstation cell along the side the body stands on, so a mob that wants out of the cell
  is not held in by the body at its mouth. :continue or nil."
  [c pos]
  (let [{:keys [x z]} (u/self-pos c)
        dx (- x (+ (:x pos) 0.5))
        dz (- z (+ (:z pos) 0.5))
        [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])
        r (await (child/call-walk* c :back-off {:pos {:x (+ (:x pos) (* 3 sx)) :y (:y pos) :z (+ (:z pos) (* 3 sz))}
                                               :range 1 :ways #{:walk}}))]
    (when (= :continue r) :continue)))

(defn ^:async child!
  "Run a dig or place child in a slot of its own per roll; its result when done, :continue, or a stop. A place refused
  with a mob in the cell (the villager stepped in) waits a poll and is placed again: :continue."
  [c slot job cargs]
  (let [r (await (ctx/call c slot job cargs))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) (:data r)
      (and (= 'jobs.blocks.place job) (seq (:mobs (:data r))))
      (do (ctx/update-mem! c update :place-waits (fnil inc 0))
          (or (await (back-off! c (:pos cargs)))
              (do (await (child/pause! c (poll-s) :mob-in-the-cell))
                  :continue)))
      :else (ctx/end c (:kind r) (:cause r) (data-of c nil)))))

(defn ^:async flush-to-cell!
  "A body in the cell beside the workstation steps to its edge nearest the workstation, so the pickup reach (hitbox grown by a block)
  covers the whole workstation cell and the drop is taken where it falls, even when a slab keeps the body out of the cell."
  [c pos]
  (let [[x y z] (b/feet-cell c)
        dx (- (:x pos) x)
        dz (- (:z pos) z)]
    (when (and (= y (:y pos)) (= 1 (+ (Math/abs dx) (Math/abs dz))))
      (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5 (* 0.2 dx)) :y y :z (+ z 0.5 (* 0.2 dz))} :range 0.1})))))

(defn cell-offset
  "Where in its cell a view spot lies, from the cell centre, at most 0.2 (what moveTo's :offset takes): the slit shows
  the villager from the spot the body saw it from, not from anywhere in the cell."
  [v]
  (-> (- v (js/Math.floor v) 0.5) (max -0.2) (min 0.2)))

(defn line-open?
  "Whether the n cells from the view spot along (sx, sz) have room for feet and head: a farther spot behind a wall is
  reached only by walking round it, away from the cell."
  [c {:keys [x y z]} n sx sz]
  (let [p (:primitives c)
        fx (js/Math.floor x) fy (js/Math.floor y) fz (js/Math.floor z)]
    (every? (fn [[k dy]]
              (bs/passable? (u/seen-name p {:x (+ fx (* k sx)) :y (+ fy dy) :z (+ fz (* k sz))})))
            (for [k (range 1 (inc n)) dy [0 1]] [k dy]))))

(defn ^:async back-to-view!
  "The villager was seen but is out of view (the body moved, or the villager shifted in its cell): step to the spot it
  was first seen from, then one and two blocks farther from the workstation, where a slit shows more of the cell, as
  a player shifts to look again. :again when it set out (three tries a sighting), else nil."
  [c pos]
  (let [{:keys [x y z] :as view} (:view (ctx/mem c))
        n (:view-tries (ctx/mem c) 0)]
    (when (and view (< n 3))
      (let [dx (- x (+ (:x pos) 0.5))
            dz (- z (+ (:z pos) 0.5))
            [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
        (ctx/update-mem! c update :view-tries (fnil inc 0))
        (when (line-open? c view n sx sz)
          (await (ctx/act c :moveTo #js {:pos #js {:x (+ x (* n sx)) :y y :z (+ z (* n sz))} :range 0.1 :offset #js {:dx (cell-offset x) :dz (cell-offset z)}})))
        :again))))

(defn ^:async flank!
  "A villager named by id and not seen from where the body stands (a slab or the workstation hides the cell from a corridor
  cell off to the side): step once to the cell beside the workstation nearest the body, where a player would stand to look
  in. :continue while the walk goes on, else :again once it ended."
  [c pos]
  (let [p (:primitives c)
        me (u/self-pos c)
        open? (fn [x y z] (every? #(bs/passable? (u/seen-name p {:x x :y (+ y %) :z z})) [0 1]))
        spots (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                    :let [x (+ (:x pos) dx) z (+ (:z pos) dz)]
                    :when (open? x (:y pos) z)]
                {:x (+ x 0.5) :y (:y pos) :z (+ z 0.5)})
        spot (first (sort-by #(+ (js/Math.abs (- (:x %) (:x me))) (js/Math.abs (- (:z %) (:z me)))) spots))
        r (when (and spot (not (u/within? me spot 0.6)))
            (await (child/call-walk* c :flank {:pos spot :range 0.3 :ways #{:walk}})))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c assoc :flanked true) :again))))

(declare step!)

(defn ^:async back-steps!
  "Steps (step!) until the workstation this call broke is placed back: :again once it is, else what a step gave."
  [c]
  (let [r (await (pace/steps! c (fn ^:async step-back []
                                  (let [r (await (step! c))]
                                    (if (and (= :again r) (not (:broken (ctx/mem c)))) :back r)))))]
    (if (= :back r) :again r)))

(defn ^:async put-back!
  "back-steps! as one urgent section (ctx/atomic :roll): a night or low-food reflex waits until the workstation is back."
  [c]
  (await (ctx/atomic c :urgent :roll #(back-steps! c))))

(defn ^:async to-cell!
  "Walk (go-to child, slot :to-cell) to where the workstation cell is in reach and in sight, before the urgent section
  opens: the break, the wait for the job to go and the place then happen from one stand. nil when in reach (or the walk
  ended there), :continue, or the stop."
  [c pos]
  (when-not (b/in-reach? c pos true)
    (let [r (await (child/call-walk* c :to-cell (await (b/walk-request c pos true (reach)))))]
      (cond
        (= :continue r) :continue
        (:arrived (:data r)) nil
        :else (stop! c :unreachable)))))

(defn ^:async dig!
  "Walk into reach (to-cell!), then break the workstation and put it back (back-steps!), as one urgent section
  (ctx/atomic :roll); the break and the stand it is made from are booked before the dig child runs, so a cut leaves it
  for the next round and the cleanup."
  [c]
  (let [{:keys [pos]} (:args c)]
    (or (await (to-cell! c pos))
        (await (ctx/atomic c :urgent :roll
                           (fn ^:async dig-and-put-back []
                             (let [slot (keyword (str "dig-" (rolls c)))
                                   _ (await (flush-to-cell! c pos))
                                   have (u/carried-count (:primitives c) (item-of (:args c)))
                                   _ (ctx/update-mem! c #(-> % (assoc :broken true :stand (u/self-pos c) :expect-carried (inc have) :spare (pos? have))
                                                             (dissoc :lose-polled :job-at :job-ticks :drop-ids :crouched :reach-drops :recovers)))
                                   r (await (child! c slot 'jobs.blocks.dig (cond-> {:pos pos :collect (zero? have)} (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
                               (if (ending-or-wait? r)
                                 r
                                 (do (ctx/update-mem! c #(-> % (update :rolls (fnil inc 0)) (assoc :drop-ids (:ids r))))
                                     (await (back-steps! c)))))))))))

(defn ^:async wait-take-job!
  "The penned villager has no job with the workstation placed: wait while game ticks of the villagers' walking hours
  (villager/count-ticks!; none pass at night, when villagers take no job) stay under villager/take-job-ticks, then end
  :no-job naming the other job sites seen (:sites). Another villager may hold the claim on it, which the game drops only
  after those ticks: once walking-hour ticks have passed with no job, by day, the workstation is broken and placed again
  (dig!, once for the villager picked: the break drops every claim, the slab keeps the villager in, and it is then the
  only villager that reaches the workstation) when that (cost/reset-s) costs less than the wait left."
  [c]
  (let [n (villager/count-ticks! c)
        left-s (/ (settings/ticks->ms (- (villager/take-job-ticks) n)) 1000)]
    (cond
      (>= n (villager/take-job-ticks))
      (ctx/end c :nothing-possible :no-job (data-of c {:sites (villager/job-sites c (:pos (:args c)))}))
      (and (pos? n) (not (:claim-reset (ctx/mem c))) (not (sh-sense/night? (:primitives c)))
           (< (cost/reset-s (item-of (:args c))) left-s))
      (do (ctx/update-mem! c assoc :claim-reset true)
          (await (dig! c)))
      :else (await (wait-job! c :villager-takes-no-job)))))

(defn penned? [c e] (some? (vp/penned (:primitives c) (b/cell (:pos (b/parse (:args c)))) e)))

(defn ^:async pen!
  "The villager is not shut in its pen: a jobs.village.pen child (slot :pen) shuts it in. :again once it is, or once the
  child lost the villager from view (the round looks for it); :continue while the child goes on; else the call ends with
  the child's ending."
  [c]
  (let [{:keys [pos profession ignore-zones? ways]} (:args c)
        r (await (ctx/call c :pen 'jobs.village.pen (cond-> {:villager (villager/chosen c) :pos pos :profession profession}
                                                       ignore-zones? (assoc :ignore-zones? true)
                                                       (some? ways) (assoc :ways ways))))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) :again
      (= :villager-out-of-view (contract/cause-key r)) :again
      :else (ctx/end c (:kind r) (:cause r) (data-of c nil)))))

(defn on-cell?
  "Whether the body's feet or head fill the cell pos."
  [c pos]
  (let [[x y z] (b/feet-cell c)]
    (and (= x (:x pos)) (= z (:z pos)) (contains? #{(:y pos) (dec (:y pos))} y))))

(defn ^:async clear-cell!
  "Walk off the workstation cell so it can be placed (jobs.lib.step-off), to the side the dig was made from, never into a dead end behind it. :again, :continue or the stop."
  [c pos]
  (let [r (await (step-off/step-off-zoned! c pos (if-let [stand (:stand (ctx/mem c))] {:near stand} {})))]
    (cond
      (= :continue r) :continue
      (= :arrived r) :again
      :else (stop! c :place-failed))))

(defn ^:async to-stand!
  "Back to where the dig was made from when the body picked up the drop beyond the cell: placing from there would shut it
  in behind the workstation. :again, :continue, or nil when it is there (or not known)."
  [c]
  (let [stand (:stand (ctx/mem c))]
    (when (and stand (not (u/within? (u/self-pos c) stand 1.5)))
      (let [r (await (child/call-walk* c :to-stand {:pos stand :range 1 :ways #{:walk}}))]
        (cond (= :continue r) :continue
              (:arrived (:data r)) :again
              :else nil)))))

(defn drop-spot
  "Where the broken workstation's drop lies, for the body to reach: the nearest seen item of it within 4 blocks of the cell, else the
  cell beyond the workstation from where the dig was made (a drop slides on, away from the digger). Nil when neither is known."
  [c pos item]
  (let [seen (->> (look/seen-items (:primitives c) {:radius 4 :max 32})
                  (filter #(and (= item (.-name (.-item %))) (u/within? (u/pos-of (.-pos %)) pos 4))))
        stand (:stand (ctx/mem c))]
    (cond
      (seq seen) (u/pos-of (.-pos (first (sort-by #(u/dist (u/pos-of (.-pos %)) pos) seen))))
      stand (let [dx (- (+ (:x pos) 0.5) (:x stand))
                  dz (- (+ (:z pos) 0.5) (:z stand))
                  [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
              {:x (+ (:x pos) 0.5 sx) :y (:y pos) :z (+ (:z pos) 0.5 sz)}))))

(defn expect-after-unseen-break
  "The count the bag should hold once the drop of a workstation found gone (no dig of this call booked it) is picked up: what it holds
  now, one more when a drop of the item lies near the cell, at least 1 (a workstation to place)."
  [c pos item]
  (let [have (u/carried-count (:primitives c) item)
        dropped? (seq (filter #(and (= item (.-name (.-item %))) (u/within? (u/pos-of (.-pos %)) pos 4))
                              (look/seen-items (:primitives c) {:radius 4 :max 32})))]
    (max 1 (if dropped? (inc have) have))))

(defn far-side
  "The point 2 blocks beyond spot on the line from the body through it, along the longer axis: the other side of the wall or cell the drop lies in."
  [c spot]
  (let [{:keys [x z]} (u/self-pos c)
        dx (- (:x spot) x)
        dz (- (:z spot) z)]
    (if (>= (js/Math.abs dx) (js/Math.abs dz))
      (assoc spot :x (+ (:x spot) (* 2 (js/Math.sign dx))))
      (assoc spot :z (+ (:z spot) (* 2 (js/Math.sign dz)))))))

(defn crouch-decider
  "A steer decide function: sneaking, straight to each [x z hold] of legs in turn, held there hold ticks; a leg not reached in
  40 ticks is left as it is. Done after the last leg."
  [legs]
  (let [leg (volatile! 0)
        ticks (volatile! 0)
        held (volatile! 0)]
    (fn [js-pose]
      (let [[tx tz hold] (get legs @leg)
            x (.-x js-pose) z (.-z js-pose)
            at? (and (<= (js/Math.abs (- tx x)) 0.1) (<= (js/Math.abs (- tz z)) 0.1))]
        (vswap! ticks inc)
        (when at? (vswap! held inc))
        (if (or (> @held hold) (>= @ticks 40))
          (do (vswap! leg inc) (vreset! ticks 0) (vreset! held 0)
              (if (< @leg (count legs)) #js {:controls #js {:sneak true :forward false}} #js {:done true}))
          (if at?
            #js {:controls #js {:sneak true :forward false}}
            #js {:controls #js {:sneak true :forward true} :yaw (js/Math.atan2 (- x tx) (- z tz))}))))))

(defn ^:async crouch-in!
  "Take a drop that slid into the cell behind the emptied workstation cell (a villager's cell): sneak from the dig stand into
  the workstation cell, up against its far side, so the pickup reach covers the whole cell behind, hold a second and sneak back.
  A crouching body is 1.5 high, so it fits under a top slab over the cell where a villager (1.95) does not: the cell stays
  shut and nothing is dug. Once a break. :again (after a walk back to the stand, or the crouch), or nil."
  [c]
  (let [{:keys [pos]} (:args c)
        stand (:stand (ctx/mem c))]
    (when (and stand (not (:crouched (ctx/mem c))))
      (or (await (to-stand! c))
          (do (ctx/update-mem! c assoc :crouched true)
              (when (u/within? (u/self-pos c) stand 1.5)
                (let [dx (- (+ (:x pos) 0.5) (:x stand))
                      dz (- (+ (:z pos) 0.5) (:z stand))
                      [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])
                      in [(+ (:x pos) 0.5 (* 0.2 sx)) (+ (:z pos) 0.5 (* 0.2 sz))]]
                  (ctx/emit! c :roll.crouch-in :info {:pos pos :text (str "the " (item-of (:args c)) " drop is out of reach: sneaking into its emptied cell to take it")})
                  (await (ctx/act c :steer (walk/steer-args 8 (crouch-decider [[(:x stand) (:z stand) 0] (conj in 20) [(:x stand) (:z stand) 0]]))))
                  :again)))))))

(defn ^:async reach-drop!
  "Take the drop through a gap in a wall (a back passage behind a sealed cell): the first try walks (go-to, walk only) within pickup
  range of the drop's spot, later tries to the far side of it from where the body stood, then presses up against whatever keeps the body
  out, so the pickup box reaches through. :again, :continue, or nil when it was tried four times or no spot is known."
  [c]
  (let [{:keys [pos]} (:args c)
        n (:reach-drops (ctx/mem c) 0)
        spot (when (< n 4) (drop-spot c pos (item-of (:args c))))]
    (when spot
      (ctx/update-mem! c update :reach-drops (fnil inc 0))
      (let [r (await (child/call-walk* c (keyword (str "reach-drop-" n)) (if (zero? n)
                                                       {:pos spot :range 2.5 :ways #{:walk}}
                                                       {:pos (far-side c spot) :range 1.2 :ways #{:walk}})))]
        (cond
          (= :continue r) :continue
          :else (do (when (:arrived (:data r))
                      (await (ctx/act c :moveTo #js {:pos #js {:x (:x spot) :y (:y spot) :z (:z spot)} :range 0.1})))
                    (await (child/pause! c 1 :picking-up-the-drop))
                    :again))))))

(defn ^:async recover!
  "The broken workstation is not carried (its drop slid out of reach, e.g. into the sealed cell beside it): walk to where its drop
  lies and take it through a gap (reach-drop!); failing that stand on its cell when the body can enter it, else stay at the cell's
  edge, and wait a few polls for the drop to come; then end :workstation-lost naming the cell the drop lies in (:drop-cell). Nothing
  above the cell is ever dug: the cell stays shut. :again or :continue."
  [c]
  (let [{:keys [pos]} (:args c)
        tries (:recovers (ctx/mem c) 0)]
    (cond
      (>= tries 5) (let [spot (or (drop-spot c pos (item-of (:args c))) pos)]
                     (ctx/end c :nothing-possible :workstation-lost
                              (data-of c {:drop-cell {:x (js/Math.floor (:x spot)) :y (js/Math.floor (:y spot)) :z (js/Math.floor (:z spot))}})))
      :else (or (await (crouch-in! c))
                (await (reach-drop! c))
                (if (on-cell? c pos)
                  (do (ctx/update-mem! c update :recovers (fnil inc 0))
                      (await (child/pause! c 1 :picking-up-the-drop))
                      :continue)
                  (let [r (await (child/call-walk* c :to-drop {:pos pos :range 0 :ways #{:walk}}))]
                    (cond (= :continue r) :continue
                          (:arrived (:data r)) :again
                          :else (do (ctx/update-mem! c update :recovers (fnil inc 0))
                                    (await (child/pause! c 1 :picking-up-the-drop))
                                    :continue))))))))

(defn ^:async place!
  "Place the workstation again, and the book of the break closed."
  [c]
  (let [{:keys [pos]} (:args c)
        item (item-of (:args c))
        slot (keyword (str "place-" (rolls c) "-" (:place-waits (ctx/mem c) 0)))
        ways (:ways (:args c))
        r (await (child! c slot 'jobs.blocks.place (cond-> {:pos pos :item item :through false}
                                                     (:ignore-zones? (:args c)) (assoc :ignore-zones? true)
                                                     (some? ways) (assoc :ways ways))))]
    (if (ending-or-wait? r)
      r
      (let [{:keys [spare drop-ids] expect :expect-carried} (ctx/mem c)
            have (u/carried-count (:primitives c) item)]
        (ctx/update-mem! c #(cond-> (dissoc % :broken :expect-carried :spare :drop-ids)
                              (and spare expect) (assoc :owed {:ids drop-ids :expect (dec expect)})))
        (if (and expect (not spare) (< have (dec expect)))
          (ctx/end c :nothing-possible :workstation-lost
                   (data-of c {:carried have :expected (dec expect)
                               :text (str "the bag holds " have " " item " after the place, " (dec expect) " expected: one is missing")}))
          :again)))))

(defn ^:async collect-owed!
  "A spare was placed before the broken workstation's drop was picked up: pick it up now (jobs.items.collect-drops child,
  slot :owed); the bag must then hold the count before the break, else :workstation-lost. :again, :continue or the stop."
  [c]
  (let [{:keys [pos]} (:args c)
        item (item-of (:args c))
        {:keys [ids expect]} (:owed (ctx/mem c))
        r (await (child/run!* c :owed 'jobs.items.collect-drops (cond-> {:items [item] :near pos :holes [pos] :expect 1} (seq ids) (assoc :ids ids))))
        have (u/carried-count (:primitives c) item)]
    (cond
      (= :continue r) :continue
      (>= have expect) (do (ctx/update-mem! c dissoc :owed) :again)
      :else (do (ctx/update-mem! c dissoc :owed)
                (ctx/end c :nothing-possible :workstation-lost
                         (data-of c {:carried have :expected expect
                                     :text (str "the bag holds " have " " item " after the spare was placed, " expect " expected: the drop was not picked up")}))))))

(defn ^:async place-or-return!
  "place!, after a walk back to the dig's stand when the body is elsewhere (to-stand!)."
  [c]
  (or (await (to-stand! c)) (await (place! c))))

(defn ^:async leave-box!
  "The body stands boxed in its stand behind the workstation (jobs.lib.villager-pen/exit-cell) and must walk: dig the
  box's entry (jobs.blocks.dig child, slot :exit, by hand when no tool harvests it); the pen child shuts it again
  before the next break. :again, :continue, the stop, or nil when the body is not boxed in."
  [c]
  (when-let [cell (vp/exit-cell (:primitives c) (b/cell (:pos (b/parse (:args c)))))]
    (let [r (await (child! c :exit 'jobs.blocks.dig (cond-> {:pos (u/cell-pos cell) :collect true :need-drop false}
                                                      (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
      (if (ending-or-wait? r) r :again))))

(defn ^:async look-for!
  "The villager is out of view: walk to the cell when far from it and none was seen yet, look from where it was seen and
  round, from the cell beside the workstation, then at where the body last saw it (once a sighting), then by day toward
  where it is heard (vantage steps). At night villagers sleep indoors: wait polls till day, then search once more. A
  villager seen this call is waited for ::away-s by day; then :gone. :again, :continue or the stop."
  [c pos villager]
  (let [m (ctx/mem c)]
    (cond
      (not (or (:seen m) (:approached m) (:memory-walked m) (u/within? (u/self-pos c) pos (reach)))) (await (approach! c pos))
      (await (back-to-view! c pos)) :again
      (await (villager/find-in-view! c (rollable c) look/hazards-only)) :again
      (and (not (:seen m)) (not (:flanked m))) (await (flank! c pos))
      :else
      (or (await (leave-box! c))
          (await (villager/approach-remembered! c villager (reach) (:last-seen m)))
          (cond
            (sh-sense/night? (:primitives c)) (do (ctx/update-mem! c assoc :night-waited true)
                                                  (await (wait-job! c :night-villager-out-of-view)))
            (:night-waited m) (do (ctx/update-mem! c dissoc :night-waited :memory-walked :flanked :away-since :vantage) :again)
            :else
            (or (await (villager/hear-step! c villager))
                (if (and (:seen m) (< (- (ctx/now c) (:away-since m (ctx/now c))) (* 1000 (away-s))))
                  (do (ctx/update-mem! c update :away-since #(or % (ctx/now c)))
                      (await (wait-job! c :villager-out-of-view)))
                  (stop! c (villager/gone-cause c)))))))))

(defn ^:async step!
  "One piece of the search, from what the world and the saved count say now. :again, :continue or :done."
  [c]
  (let [{:keys [pos]} (:args c)
        item (item-of (:args c))
        p (:primitives c)
        villager (pick! c)
        e (when villager (villager/find-by-uuid p villager))
        block (u/seen-name p pos)
        _ (when e (ctx/update-mem! c #(-> % (update :view (fn [v] (or v (u/self-pos c)))) (assoc :view-tries 0 :last-seen (u/pos-of (.-pos e)))
                                          (dissoc :memory-walked :night-waited :vantage))))
        profession (when e (.-profession e))
        out-of-tries? (>= (rolls c) (max-rolls))]
    (cond
      (:owed (ctx/mem c)) (await (collect-owed! c))
      (nil? block) (await (approach! c pos))
      (bs/air block)
      (cond
        (not (:broken (ctx/mem c))) (do (ctx/update-mem! c assoc :broken true :expect-carried (expect-after-unseen-break c pos item)) :again)
        ;; the workstation is gone: wait for the villager to lose the job, then place it back (one that took another job
        ;; never will: place at once, the next step ends :other-job)
        (and e (not= "unemployed" profession) (not (other-job? c e))) (await (wait-lose-job! c))
        (< (u/carried-count p item) (cond-> (:expect-carried (ctx/mem c)) (:spare (ctx/mem c)) dec)) (await (recover! c))
        (on-cell? c pos) (await (clear-cell! c pos))
        :else (await (place-or-return! c)))
      (not= item block) (stop! c :occupied)
      ;; villagers count only while in view; one seen before may have stepped away while it lost its job
      (nil? e) (await (look-for! c pos villager))
      (and e (not (and (:seen (ctx/mem c)) (nil? (:away-since (ctx/mem c)))))) (do (ctx/update-mem! c #(-> % (assoc :seen true) (dissoc :away-since))) :again)
      (.-baby e) (stop! c :baby)
      (= "nitwit" profession) (stop! c :nitwit)
      (other-job? c e) (rule-out! c :other-job)
      (= "unemployed" profession) (if (penned? c e) (await (wait-take-job! c)) (await (pen! c)))
      (not (u/within? (u/self-pos c) (u/pos-of (.-pos e)) (reach))) (or (await (leave-box! c)) (await (walk! c e)))
      :else
      (let [r (await (read-offers! c))]
        (cond
          (:found r) (found! c (:found r))
          (= :locked r) (rule-out! c :locked)
          (= :miss r) (cond
                        out-of-tries? (stop! c :no-match)
                        (penned? c e) (await (dig! c))
                        :else (await (pen! c)))
          :else r)))))

(defn ^:async cleanup
  "However the call ends: a workstation this call broke and did not place back is placed (after stepping off its cell first).
  Done, or the place child's failure."
  [c _]
  (let [{:keys [pos]} (:args c)
        block (u/seen-name (:primitives c) pos)
        r (cond
            (not (and (:broken (ctx/mem c)) block (bs/air block) (pos? (u/carried-count (:primitives c) (item-of (:args c))))))
            (ctx/end c :done nil)
            (on-cell? c pos) (await (clear-cell! c pos))
            :else (await (place-or-return! c)))]
    (when (and (contract/ending? r) (not= :done (:kind r)) (:broken (ctx/mem c)) (some-> (u/seen-name (:primitives c) pos) bs/air))
      (ctx/emit! c :roll.cell-open :warn {:pos pos :text (str "CELL OPEN: the " (item-of (:args c)) " at " (:x pos) "," (:y pos) "," (:z pos) " is not placed back")}))
    (if (= :again r) (await (cleanup c nil)) r)))

(defn ^:async round
  "The whole search: steps (a pace between) until found or stopped; it yields while the villager or a child waits."
  [c]
  (await (pace/steps! c #(if (:broken (ctx/mem c)) (put-back! c) (step! c)))))
