(ns jobs.survival.recover-drops
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.result :as result]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.danger :as dg]
            [jobs.survival.recover-drops.deaths :as dth]
            [jobs.survival.recover-drops.estimate :as est]
            [jobs.survival.recover-drops.kit :as kt]
            [jobs.survival.recover-drops.pile :as pl]
            [jobs.survival.recover-drops.config :as cfg]))

(def doc
  "After a death, go back for the drops when they are worth it.
  Serves one :died entry no :recovered covers (its :death-t): the one a trip is keyed to, else the open pile with the best value net of the walk. A death whose despawn window closed while a newer one is open is closed :abandoned :superseded. With every death decided the job waits (condition :no-drops) naming the decision.
  One run is the whole trip: it holds still (:respawning, then :settling) until the respawn and its 2 s have passed, decides, walks, collects and writes :recovered {:decision :skip|:collected|:partial|:abandoned}. A run cut by a reflex, or a restarted body, goes on with the same trip (the decision, the baseline of carried items and the phase are kept in :recover-trip).
  Ready: fit? ok; safe? is ok: it never waits for light or dawn, the drops despawn 5 min after the death (game/despawn-ticks). The children walk under :checks {:safe? (always)}: the trip prices its own danger; a pile it cannot reach before the despawn ends :too-far or :window-closed.
  Decision (settings ::margin, ::leg-s; args :value-overrides, :danger-overrides): skip when the value of what was carried (jobs.lib.cost/item-value, plus 5 per level of experience) is at most the fetch cost plus ::margin. Cost: a trip of 10, 0.3 per block, and 10 per point of route danger (jobs.lib.cost/route-danger) past the hostiles the body knows of, the killer assumed at the pile for ::killer-guard-ms; infinite for lava, fire, the void, or a walk that ends after the despawn. A pile in water adds ::wet-danger.
  A real danger (jobs.lib.danger/nearest-danger) the hostile-near trigger sees (its radii), or a hit, has the trip weighed again once per mob. A skip for danger alone is not final: the run ends condition-failed :danger, the danger is remembered (:recover-danger) for ::danger-memory-ms and the pile weighed anew, the mobs seen still priced where they stood until the body is within ::see-range of the place and none is known there; the decline counts toward ::max-hostile-cuts; no time left to come back: :abandoned :danger. :retry weighs it now. A skip for value is final.
  Kit: a body without weapon or armour whose pile is worth it were the route safe fetches the best piece a chest of known stock holds (the call's ways, jobs.lib.need) and wears it; a failed fetch is dropped (warn recover-drops.kit-dropped with the ending).
  Fetch: walks to the death point in legs of ::leg-s seconds (weighed again after each), then collects up to 3 passes the visible items within ::collect-radius (jobs.items.collect-drops). Drops out of sight get one look where they were last seen, and one walk round the pile. A pile out of reach behind blocks this body placed within ::seal-radius: they are taken back once (jobs.access.cleanup). A pile far below is reached by a stair (jobs.access.tunnel).
  Events: recover-drops.fetching (a decision to go) and recover-drops.decided (the priced decision, read by the dashboard).
  Endings: :collected is done (also a pile already carried); :skip condition-failed :not-worth; :partial nothing-possible :unreachable or :not-visible; :abandoned by its reason: :window-closed (the pile despawned) condition-failed :gone, :danger and :nothing-found condition-failed, :unreachable nothing-possible. Data :decision :text :pos :items :left. A danger or a walk that did not arrive leaves the death open. A walk go-to refused outright (a verdict on the cell, no nearer) is not counted toward giving up as :unreachable: the window closes it.
  Warns respawn_away_from_bed once per death when the body respawned over ::bed-away-blocks from its respawn bed.
  Memory: reads :died, :respawned, :bed, :slept, :recover-trip, :recover-danger, the scaffold ledger. Writes :recovered, :recover-trip, :recover-danger.")

(a/defargs args
  {:value-overrides {:doc "jobs.lib.cost/item-value overrides, a map: item name or group (ore tool armor food block unknown) -> worth of one item, or {:times n}; e.g. {\"raw_iron\" 500}" :spec (a/map-of string? (a/or-of number? map?)) :default {}}
   :danger-overrides {:doc "jobs.lib.cost/route-danger overrides, a map: mob name -> threat in points of damage before armour, or {:times n}; e.g. {\"creeper\" 100 \"zombie\" 0}" :spec (a/map-of string? (a/or-of number? map?)) :default {}}
   :retry {:doc "try a pile again that an earlier run ended :abandoned or declined for a danger still remembered, while its despawn window is open (a reflex run keeps the decision; an agent asks for another try)" :spec boolean? :default false}})

(a/defargs settings
  {::collect-radius {:default 10 :doc "Blocks from the death point within which the pile's drops are collected (a pile on open ground rolls 6-8 out)." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 300 :doc "Seconds of the call's time limit: the despawn window, which the job keeps itself in game ticks." :spec (a/num-in 0 nil)}
   ::collect-s {:default 10 :doc "Seconds a collect pass at the pile is reckoned to take, in the job's quote." :spec (a/num-in 0 nil)}})

(def ways need/fetch-ways)

(def target [])

(def own-checks
  "The :checks of the job's children: the trip prices its own danger."
  {:safe? '(always)})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s} :ways #{:carried :chest}})

(defn can-run?
  "A death open to serve, else :later with the danger remembered for one, else the decision already taken."
  [c]
  (if (seq (dth/open-deaths c))
    (contract/ok :open-death "a death to serve")
    (checks/cause-status :later (or (dth/danger-wait (ctx/view c)) (est/decided-wait (ctx/view c))))))

(defn done?
  "Ok once the latest death's pile is back or seen gone (its :recovered says :collected or :nothing-found); else how much of the pile is back since the decision."
  [c]
  (let [view (ctx/view c)
        died (mem/latest view :died)
        recovered (last (filter #(= (:t died) (:death-t (:data %))) (mem/entries view :recovered)))]
    (if (and died (#{:collected :nothing-found} (:decision (:data recovered))))
      (contract/ok :recovered (str "the pile is " (if (= :collected (:decision (:data recovered))) "back" "gone")))
      (let [pile (:inventory (:data (dth/current-death c)))
            total (reduce + 0 (map :count pile))
            n (if (:baseline (ctx/mem c)) (pl/recovered-items c pile) 0)]
        (contract/later :recovering (str n " of " total " items back") :units :items :done n :left (- total n))))))

(defn ^:async trip-s
  "[least mean] seconds of the walk to pos as go-to quotes it, the mean with ::collect-s at the pile."
  [c pos]
  (let [[least mean] (or (when pos (get-in (await (ctx/quote c 'jobs.movement.go-to {:pos pos :range (cfg/arrive-range)})) [:uses :job-time])) [0 0])]
    [least (+ mean (settings/get settings ::collect-s))]))

(defn window-ms
  "Milliseconds left before the drops of death entry despawn."
  [c entry]
  (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))))

(defn ^:async quote-of
  "The walk to the pile as go-to quotes it, then ::cfg/collect-s at the pile; at most the despawn window left."
  [c]
  (let [d (first (dth/open-deaths c))
        window-s (if d (/ (window-ms c d) 1000) 0)
        [least mean] (await (trip-s c (:pos (:data d))))]
    {:uses {:job-time [least mean (max mean window-s)]}}))

(def endings
  "The kind and cause of a trip's end by its decision (and for an abandoned pile its reason); a pile not worth the trip or despawned is a fact about the goal, not a limit of the call."
  {:collected [:done nil]
   :skip [:condition-failed :not-worth]
   [:abandoned :window-closed] [:condition-failed :gone]
   [:abandoned :danger] [:condition-failed :danger]
   [:abandoned :nothing-found] [:condition-failed :nothing-found]})

(defn ending-for
  "The ending of a trip that wrote decision with fields: collected is done, a skip condition-failed :not-worth, an abandoned
  pile by its reason (:window-closed and :danger and :nothing-found condition-failed), the rest nothing-possible
  with the reason (:unreachable, or for a partial :unreachable / :not-visible)."
  [c decision fields text pos]
  (let [[kind cause] (or (endings decision) (endings [decision (:reason fields)]) [:nothing-possible (:reason fields)])]
    (ctx/end c kind cause (assoc (select-keys fields [:items :left :detail]) :decision decision :text text :pos pos))))

(defn finish!
  "Write the :recovered entry, report the decision and end the job with its ending."
  [c decision fields]
  (let [death (dth/current-death c)
        pos (:pos (:data death))
        text (cond-> (est/decision-text decision fields (cfg/margin))
               (= :abandoned decision) (str "; the pile at " (str/join " " (map #(js/Math.floor (% pos)) [:x :y :z]))))]
    (ctx/remember! c :recovered (merge {:decision decision :death-t (:t death)} (dissoc fields :at :flight-s)) cfg/recovered-policy)
    (ctx/emit! c :recover-drops.decided (if (= :abandoned decision) :warn :info)
               (assoc (select-keys fields [:value :cost :reason :items :left :parts :top-items :top-mobs])
                      :decision decision :pos pos :text text))
    (ending-for c decision fields text pos)))

(defn own-seal-near
  "The open scaffold ledger entries (jobs.lib.ledger) within seal-radius of pos: blocks this body placed round the pile."
  [c pos]
  (ledger/near-of (ledger/open-entries (ctx/view c)) {:pos pos :radius (cfg/seal-radius)}))

(defn ^:async clear-seal!
  "When the body's own placed blocks stand near the pile (its flee seal): take back those within seal-radius of it
  (jobs.access.cleanup :near) and walk again, after a walk to where the eye sees the nearest of them, within ::seal-sight (the cleanup offers only blocks the body has seen). Tried twice a trip at most; :seal-cleared is set once a run removed a block.
  :next when it ran, else nil."
  [c pos]
  (when-let [seal (and (not (:seal-cleared (ctx/mem c))) (not (child/exhausted? c :seal 2)) (seq (own-seal-near c pos)))]
    (child/fruitless! c :seal)
    (ctx/update-mem! c assoc :phase :go)
    (dth/save-trip! c)
    (ctx/emit! c :recover-drops.seal :info {:pos pos :text "the pile is out of reach behind blocks this body placed: taking them back"})
    (let [[x y z] (:cell (apply min-key #(u/dist (u/self-pos c) (zipmap [:x :y :z] (:cell %))) seal))]
      (await (child/run!* c :seal-walk 'jobs.movement.go-to {:pos {:x x :y y :z z} :range (cfg/seal-sight) :see true :ways #{:walk} :checks own-checks})))
    (let [r (await (child/run!* c :seal 'jobs.access.cleanup {:of :all :near {:pos pos :radius (cfg/seal-radius)} :checks (assoc own-checks :fit? '(always))}))]
      (when (and (= :done (:kind r)) (seq (:removed (:data r))))
        (ctx/update-mem! c assoc :seal-cleared true)))
    :next))

(defn ^:async walk-leg!
  "One leg of the walk to pos: [:danger d] when a danger the trip has not priced is near (no walk is made), else [r] of
  a chase leg of ::leg-s seconds (:continue, or its ending; its :data has the result, with :arrived or :leg)."
  [c pos]
  (if-let [d (dg/threatened? c)]
    [:danger d]
    [(await (child/call-chase* c :go {:pos pos :range (cfg/arrive-range) :leg-s (cfg/leg-s) :walk {:checks own-checks}}))]))

(defn refused?
  "Whether the walk's ending r is go-to's verdict on the goal cell alone, given before any search (as chase passes it on)."
  [r]
  (boolean (some #{:goal-not-standable :goal-on-magma} [(result/why r) (result/why (:because r)) (result/why (get-in r [:cause :because]))])))

(defn ^:async go!
  "Walk one leg (:leg-s) toward the death point: :next once there (the collect phase) or after a leg (the danger and the
  cost are weighed again; one that got no nearer counts as a walk that did not arrive), else :declined (go-to waits on the
  world, or it did not arrive (the body's own seal near the pile is taken back first): the next run walks again while the pile is worth it, and the window closes it). A walk
  go-to refused for the body's own shelter (:inside-own-shelter) is no failed try: the window closes the trip."
  [c pos]
  (let [before (u/dist (u/self-pos c) pos)
        [r danger] (await (walk-leg! c pos))
        walked (:data r)]
    (cond
      (= :danger r) (dg/meet-danger! c danger)
      (or (= :continue r) (and (not= :done (:kind r)) (nil? walked)))
      (dg/decline! c :unreachable (str "the walk to " (pr-str pos) " is waiting on the world") {:pos pos})
      (:inside-own-shelter walked)
      (dg/decline! c :unreachable (str "the walk to " (pr-str pos) " is refused: the body is sealed in its own shelter, which opens by day") {:pos pos})
      (and (not (:arrived walked)) (not (:leg walked)) (await (clear-seal! c pos))) :next
      (:leg walked)
      (let [nearer? (< (u/dist (u/self-pos c) pos) (- before 1))]
        (ctx/update-mem! c #(cond-> (assoc % :recheck true)
                              (not nearer?) (assoc :blocked true)))
        (when-not nearer? (child/fruitless! c :go) (dth/save-trip! c))
        :next)
      (not (:arrived walked))
      (do (ctx/update-mem! c assoc :blocked true)
          (when-not (and (refused? r) (>= (u/dist (u/self-pos c) pos) (- before 1)))
            (child/fruitless! c :go))
          (dth/save-trip! c)
          (dg/decline! c :unreachable (str "the walk to " (pr-str pos) " did not arrive") {:pos pos}))
      :else (do (ctx/update-mem! c assoc :phase :collect :blocked false) (dth/save-trip! c) :next))))

(defn finish-collect!
  "End the collect phase: :collected when all of the pile is back, else :partial naming what is left and why."
  [c pile ids]
  (let [items (pl/recovered-items c pile)
        left (pl/left-over c pile)
        decided (:decided (ctx/mem c))]
    (cond
      (empty? left) (finish! c :collected (assoc decided :items items))
      (zero? items) (finish! c :abandoned (assoc decided :items 0 :reason :nothing-found))
      :else (finish! c :partial (assoc decided :items items :left left :reason (if (seq ids) :unreachable :not-visible))))))

(defn ^:async look-round!
  "Drops of the pile went out of sight (down a shaft): once, walk to the cell the first was last seen in (go-to, range 1)
  and look again. :next with a pass counted when drops are in sight now, else the end of the trip (what is left, with its
  reason). Not an arrival ends the trip :partial too."
  [c [x y z] pos names radius pile]
  (ctx/update-mem! c assoc :looked true)
  (await (child/run!* c :look 'jobs.movement.go-to {:pos {:x x :y y :z z} :range 1 :ways #{:walk :clear-seal} :checks own-checks}))
  (let [again (pl/pile-ids c pos names radius)]
    (if (seq again)
      (do (ctx/update-mem! c update :passes (fnil inc 0)) (dth/save-trip! c) :next)
      (finish-collect! c pile again))))

(defn ^:async search-round!
  "Once: items of the pile are nowhere in sight at the pile (behind a corner, in a hollow). Walk to cells around it
  (go-to, range 1, ::cfg/search-radius out on the four sides) until some are in sight. True when some are."
  [c [x y z] pos names radius]
  (ctx/update-mem! c assoc :searched true)
  (loop [cells (let [r (cfg/search-radius)]
                 (map-indexed (fn [i [dx dz]] [i {:x (+ x dx) :y y :z (+ z dz)}]) [[r 0] [0 r] [(- r) 0] [0 (- r)]]))]
    (when-let [[i cell] (first cells)]
      (await (child/run!* c (keyword (str "search" i)) 'jobs.movement.go-to {:pos cell :range 1 :ways #{:walk :clear-seal} :checks own-checks}))
      (or (seq (pl/pile-ids c pos names radius)) (recur (rest cells))))))

(defn ^:async collect!
  "One collect pass over the pile's visible items: :next after a pass that left some in sight (walk back, at most
  max-collect-passes), else the job's end."
  [c pos radius pile]
  (let [names (vec (distinct (map :name pile)))
        total (reduce + 0 (map :count pile))]
    (if (>= (pl/recovered-items c pile) total)
      (finish-collect! c pile [])
      (let [ids (pl/pile-ids c pos names radius)]
        (if (empty? ids)
          (or (await (clear-seal! c pos))
              (when-not (:searched (ctx/mem c))
                (when (await (search-round! c (mapv pos [:x :y :z]) pos names radius))
                  :next))
              (finish-collect! c pile ids))
          (let [swept (await (child/run!* c :collect 'jobs.items.collect-drops
                                          {:radius (+ radius (cfg/stray-range) 4) :ids ids :items names :checks own-checks}))
                cell (pl/look-cell (:data swept))]
            (if (empty? (pl/left-over c pile))
              (finish-collect! c pile [])
              (let [again (pl/pile-ids c pos names radius)
                    passes (:passes (ctx/mem c) 0)]
                (cond
                  (and (seq again) (< passes (cfg/max-collect-passes)))
                  (do (ctx/update-mem! c assoc :passes (inc passes) :phase :go) (dth/save-trip! c) :next)

                  (and (empty? again) (not (:looked (ctx/mem c))) cell)
                  (await (look-round! c cell pos names radius pile))

                  (and (empty? again) (not (:searched (ctx/mem c))) (await (search-round! c (mapv pos [:x :y :z]) pos names radius)))
                  :next

                  :else (finish-collect! c pile again))))))))))

(defn ^:async wait-while!
  "Hold still (reason) while (pending?) is true: nil when it clears, else :continue for a cut, :closed when the despawn window passes."
  [c entry reason pending?]
  (loop []
    (cond
      (not (pending?)) (do (ctx/hold-still! c nil) nil)
      (not (ctx/alive? c)) :continue
      (dth/window-closed? c entry) (do (ctx/hold-still! c nil) :closed)
      :else (do (await (child/pause! c (cfg/pause-s) reason)) (await (pace/pace!)) (recur)))))

(defn decide!
  "The decision of this trip: the saved one, else the estimate, save and report it. A resumed trip estimates again once (from
  where the body stands, with the hostiles it knows now), and so does a walk after each leg; its baseline stays."
  [c entry]
  (if (and (:decided (ctx/mem c)) (not (:recheck (ctx/mem c))) (or (zero? (:cuts (ctx/mem c) 0)) (:reestimated (ctx/mem c))))
    (:decided (ctx/mem c))
    (let [margin (cfg/margin)
          e (-> (est/estimate c (:data entry) (settings/ticks->ms (game/ticks-since entry (ctx/view c))))
                (update :cost est/finite)
                (assoc :at (ctx/now c)))
          rechecked? (:recheck (ctx/mem c))
          fetch? (> (:value e) (+ (if (= :infinite (:cost e)) js/Infinity (:cost e)) margin))]
      (ctx/update-mem! c #(cond-> (assoc % :decided e :reestimated true :recheck false) (nil? (:baseline %)) (assoc :baseline (storage/carried-totals (u/inventory (:primitives c))))))
      (dth/save-trip! c)
      (when (and fetch? (not rechecked?))
        (ctx/emit! c :recover-drops.fetching :info
                   (assoc (select-keys e [:value :cost :parts :top-items :top-mobs])
                          :pos (:pos (:data entry)) :text (est/decision-text :fetch e margin))))
      e)))


(defn retreat!
  "Remember the danger for hold-ms, report the skip and end condition-failed :danger (see retry-later!)."
  [c entry decided hold-ms mobs]
  (ctx/update-mem! c #(-> % (dissoc :decided) (assoc :hostile-cut true)))
  (dth/save-trip! c)
  (ctx/remember! c :recover-danger {:death-t (:t entry) :until (+ (ctx/now c) hold-ms) :text mobs :mobs (:seen-mobs decided)} cfg/danger-policy)
  (ctx/emit! c :recover-drops.decided :info
             (assoc (select-keys decided [:value :cost :parts :top-items :top-mobs])
                    :decision :skip :retry true :pos (:pos (:data entry)) :hold-s (js/Math.round (/ hold-ms 1000))
                    :text (str (est/decision-text :skip decided (cfg/margin)) "; looks again in " (js/Math.round (/ hold-ms 1000)) " s")))
  (ctx/end c :condition-failed :danger {:top-mobs (:top-mobs decided) :hold-s (js/Math.round (/ hold-ms 1000)) :pos (:pos (:data entry))}))

(defn ^:async retry-later!
  "A skip for danger backs off while the trip can still come back: report it, forget the estimate, remember the danger
  (:recover-danger, for ::danger-memory-ms; an assumed killer alone only while its guard lasts; at most until the walk back
  from the flight's end (est/flight-s) and the collect, as trip-s quotes them, would end at the despawn) and end condition-failed :danger: the hostile reflex
  takes the body away. The died trigger and the check rest for this death meanwhile, then the danger is weighed anew.
  No time left to come back: the pile is abandoned :danger."
  [c entry decided]
  (let [room (- (window-ms c entry) (* 1000 (+ (second (await (trip-s c (:pos (:data entry))))) (:flight-s decided 0))))
        hold-ms (min room (:hold-ms decided (cfg/danger-memory-ms)))
        mobs (str/join ", " (map :name (:top-mobs decided)))]
    (if (pos? room)
      (retreat! c entry decided hold-ms mobs)
      (finish! c :abandoned (assoc decided :reason :danger :detail (str mobs " guard it and no walk back would end before it despawns"))))))

(defn ^:async dig-down!
  "A stair down to the pile (jobs.access.tunnel): :next once it ended there, else the trip ends :abandoned. Not when
  the window left cannot hold it (about ::stair-step-s per block): go-to has no way down and would only stand above."
  [c entry pos {:keys [below aside]}]
  (let [need-s (* (cfg/stair-step-s) (max below aside))
        left-s (/ (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))) 1000)
        give-up! (fn [reason text]
                   (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason reason :detail text)))]
    (if (> need-s left-s)
      (give-up! :window-closed (str "the pile is " below " blocks down: a stair takes about " (js/Math.round need-s) " s, " (js/Math.round left-s) " s are left"))
      (let [args {:target [(js/Math.floor (:x pos)) (js/Math.floor (:y pos)) (js/Math.floor (:z pos))] :max-length 96}]
        (loop [n 0]
          (let [danger (dg/threatened? c)
                ;; a mob the trip was already weighed with must not stop the stair again
                checks (if (seq (:weighed (ctx/mem c))) own-checks {:safe? '(not (threatened))})
                r (when-not danger (await (ctx/call c :dig 'jobs.access.tunnel (assoc args :checks checks))))
                danger (or danger (when (= :condition-failed (:kind r)) (dg/threatened? c)))]
            (cond
              danger (dg/meet-danger! c danger)
              (= :done (:kind r)) :next
              (not (ctx/alive? c)) :continue
              (and (= :continue r) (< n (child/default-max-calls))) (do (await (pace/pace!)) (recur (inc n)))
              (= :continue r) (dg/decline! c :unreachable "the stair down to the pile is waiting on the world" {:pos pos})
              :else (give-up! :unreachable (str "no stair down to the pile: " (pr-str (contract/cause-key r))
                                                    (when (get-in r [:cause :because]) " (no pickaxe could be got)"))))))))))

(defn ^:async trip!
  "The decision, then the walk and the collect passes of a trip inside its window."
  [c entry]
  (let [margin (cfg/margin)
        radius-to-collect (settings/get settings ::collect-radius)
        kit (await (kt/kit! c entry))
        decided (when-not kit (decide! c entry))
        pos (:pos (:data entry))]
    (cond
      kit kit

      (<= (:value decided) (+ (if (= :infinite (:cost decided)) js/Infinity (:cost decided)) margin))
      (if (est/danger-only? decided margin)
        (await (retry-later! c entry decided))
        (if (and (= :too-far (:reason decided)) (pos? (child/fruitless c :go)))
          (finish! c :abandoned (assoc decided :reason :unreachable :detail (est/unreachable-detail c pos)))
          (finish! c :skip decided)))

      (>= (:cuts (ctx/mem c) 0) (cfg/max-hostile-cuts))
      (finish! c :abandoned (assoc decided :reason :danger))

      (child/exhausted? c :go)
      (finish! c :abandoned (assoc decided :reason :unreachable :detail (est/unreachable-detail c pos)))

      (dg/hit-since? c decided) (do (ctx/update-mem! c assoc :recheck true) :next)

      (and (= :collect (:phase (ctx/mem c))) (dg/threatened? c)) (dg/meet-danger! c (dg/threatened? c))

      (and (= :collect (:phase (ctx/mem c))) (> (u/dist (u/self-pos c) pos) (cfg/stray-range)))
      (do (ctx/update-mem! c assoc :phase :go) :next)

      (= :collect (:phase (ctx/mem c))) (await (collect! c pos radius-to-collect (:inventory (:data entry))))
      (and (est/deep-pile c pos) (:blocked (ctx/mem c))) (await (dig-down! c entry pos (est/deep-pile c pos)))
      :else (let [r (await (go! c pos))]
              (if (and (= :unreachable (:cause r)) (:blocked (ctx/mem c)) (est/deep-pile c pos)) :next r)))))

(defn ^:async step!
  "One step of the trip: :next to go on, or the round's end (an ending, or :continue for a cut)."
  [c entry]
  (cond
    (not (ctx/alive? c)) :continue
    (dth/window-closed? c entry)
    (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason (if (:blocked (ctx/mem c)) :unreachable :window-closed)))
    :else (await (trip! c entry))))

(defn warn-away-from-bed!
  "Once per death: warn when the body respawned far from its respawn bed: the live :spawn-bed (set by a click or a
  sleep), else the known :bed it slept in (the respawn point was not set there). Nothing for a bed it never slept in."
  [c entry]
  (let [spawn-bed (:pos (:data (sh-spawn/spawn-bed-live (ctx/view c))))
        bed (or spawn-bed
                (let [b (mem/place (ctx/view c) :bed)]
                  (when (some #(= b (:pos (:data %))) (ctx/entries c :slept)) b)))
        at (:pos (:data (ctx/latest c :respawned)))]
    (when (and bed at (not (:bed-checked (ctx/mem c))))
      (ctx/update-mem! c assoc :bed-checked true)
      (when (> (u/dist bed at) (cfg/bed-away-blocks))
        (ctx/emit! c :respawn_away_from_bed :warn
                   {:bed bed :pos at :text "respawned away from its bed: the respawn point was not set there"})))))

(defn ^:async round
  "One whole run: wait for the respawn and its settling (declared holds), decide, walk, collect, write :recovered, and end
  with the ending of the trip (see the doc). A danger priced into a skip ends it condition-failed :danger, a walk that did not arrive nothing-possible :unreachable, the death staying open."
  [c]
  (doseq [d (dth/stale-deaths c)]
    (ctx/remember! c :recovered {:decision :abandoned :death-t (:t d) :reason :superseded} cfg/recovered-policy))
  (let [entry (dth/current-death c)]
    (when entry (dth/key-to-death! c entry))
    (if (nil? entry)
      (ctx/end c :condition-failed :no-drops)
      (let [waited (or (await (wait-while! c entry :respawning #(not (body/respawned-since? (ctx/view c) entry))))
                       (when (nil? (:decided (ctx/mem c)))
                         (await (wait-while! c entry :settling #(dth/settling? c entry)))))]
        (if (and waited (not= :closed waited))
          waited
          (do (warn-away-from-bed! c entry)
              (loop []
                (let [entry (dth/current-death c)
                      r (if entry (await (step! c entry)) (ctx/end c :condition-failed :no-drops))]
                  (if (= :next r) (recur) r)))))))))
