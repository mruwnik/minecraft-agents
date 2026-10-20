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
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.danger :as dg]
            [jobs.survival.recover-drops.config :as cfg]
            [jobs.survival.recover-drops.deaths :as dth]
            [jobs.survival.recover-drops.ending :as end]
            [jobs.survival.recover-drops.estimate :as est]
            [jobs.survival.recover-drops.fetch :as fetch]
            [jobs.survival.recover-drops.kit :as kt]
            [jobs.survival.recover-drops.pile :as pl]))

(def doc
  "After a death, go back for the drops when they are worth it. Serves the :died entry no :recovered covers (the keyed one, else the open pile best net of the walk); one run is the whole trip and writes :recovered {:decision :skip|:collected|:partial|:abandoned}. A run cut by a reflex or a restart goes on with the same trip (:recover-trip).
  Ready: never waits for light or dawn; the drops despawn 5 min after the death (game/despawn-ticks). The children run under :checks {:safe? (always)}: the trip prices its own danger.
  Decision: the carried items' value (jobs.lib.cost/item-value, args :value-overrides) against the walk's cost (jobs.lib.cost/fetch-cost, route-danger, args :danger-overrides, ::margin); infinite for lava, fire, the void or a walk ending after the despawn.
  Danger: a real danger (jobs.lib.danger/nearest-danger) is weighed once per mob; a skip for danger alone is remembered (:recover-danger, ::danger-memory-ms, the mobs priced where they stood until their place is seen empty (in range, clear line)), after which can-run? is :later :danger and the died trigger rests; :retry weighs now and ends at once.
  Kit: a pile worth it were the route safe, and no weapon or armour: fetch the best piece a known chest holds (the call's ways, jobs.lib.need) and wear it.
  Fetch: legs of ::leg-s (weighed again after each), then up to 3 collect passes (jobs.items.collect-drops), a look and a walk round the pile; own seal blocks near it are taken back (jobs.access.cleanup), a deep pile is reached by a stair (jobs.access.tunnel).
  Ends: :collected done; :skip condition-failed :not-worth; :partial nothing-possible :unreachable or :not-visible; :abandoned by reason (:window-closed condition-failed :gone, :danger, :nothing-found condition-failed, :unreachable nothing-possible). Data :decision :text :pos :value :items :left. A walk that did not arrive leaves the death open.
  Events: recover-drops.kit-dropped (a failed kit fetch); warn respawn_away_from_bed once per death when the respawn is over ::bed-away-blocks from the respawn bed.
  Memory: reads :died :respawned :bed :slept :recover-trip :recover-danger, the scaffold ledger; writes :recovered :recover-trip :recover-danger.")

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

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s} :ways #{:carried :chest}})

(defn can-run?
  "A death to serve, else :later with the danger remembered for one (a backed-off trip waits it out), else the decision already taken."
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
  "The decision of this trip: the saved one, else the estimate, save it. A resumed trip estimates again once (from
  where the body stands, with the hostiles it knows now), and so does a walk after each leg; its baseline stays."
  [c entry]
  (if (and (:decided (ctx/mem c)) (not (:recheck (ctx/mem c))) (or (zero? (:cuts (ctx/mem c) 0)) (:reestimated (ctx/mem c))))
    (:decided (ctx/mem c))
    (let [e (-> (est/estimate c (:data entry) (settings/ticks->ms (game/ticks-since entry (ctx/view c))))
                (update :cost est/finite)
                (assoc :at (ctx/now c)))]
      (ctx/update-mem! c #(cond-> (assoc % :decided e :reestimated true :recheck false) (nil? (:baseline %)) (assoc :baseline (storage/carried-totals (u/inventory (:primitives c))))))
      (dth/save-trip! c)
      e)))


(defn retreat!
  "Remember the danger for hold-ms and report the skip (see retry-later!). With :retry asked the run ends at once; else the
  remembered danger makes can-run? :later, which stops the round between steps."
  [c entry decided hold-ms]
  (ctx/update-mem! c #(-> % (dissoc :decided) (assoc :hostile-cut true)))
  (dth/save-trip! c)
  (ctx/remember! c :recover-danger {:death-t (:t entry) :until (+ (ctx/now c) hold-ms) :text (est/decision-text :skip decided (cfg/margin)) :mobs (:seen-mobs decided)} cfg/danger-policy)
  (if (:retry (:args c))
    (ctx/end c :condition-failed :danger {:top-mobs (:top-mobs decided) :hold-s (js/Math.round (/ hold-ms 1000)) :pos (:pos (:data entry))})
    :next))

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
      (retreat! c entry decided hold-ms)
      (end/finish! c :abandoned (assoc decided :reason :danger :detail (str mobs " guard it and no walk back would end before it despawns"))))))

(defn ^:async dig-down!
  "A stair down to the pile (jobs.access.tunnel): :next once it ended there, else the trip ends :abandoned. Not when
  the window left cannot hold it (about ::stair-step-s per block): go-to has no way down and would only stand above."
  [c entry pos {:keys [below aside]}]
  (let [need-s (* (cfg/stair-step-s) (max below aside))
        left-s (/ (settings/ticks->ms (game/despawn-left-ticks entry (ctx/view c))) 1000)
        give-up! (fn [reason text]
                   (end/finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason reason :detail text)))]
    (if (> need-s left-s)
      (give-up! :window-closed (str "the pile is " below " blocks down: a stair takes about " (js/Math.round need-s) " s, " (js/Math.round left-s) " s are left"))
      (let [args {:target [(js/Math.floor (:x pos)) (js/Math.floor (:y pos)) (js/Math.floor (:z pos))] :max-length 96}]
        (loop [n 0]
          (let [danger (dg/threatened? c)
                ;; a mob the trip was already weighed with must not stop the stair again
                checks (if (seq (:weighed (ctx/mem c))) cfg/own-checks {:safe? '(not (threatened))})
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
          (end/finish! c :abandoned (assoc decided :reason :unreachable :detail (est/unreachable-detail c pos)))
          (end/finish! c :skip decided)))

      (>= (:cuts (ctx/mem c) 0) (cfg/max-hostile-cuts))
      (end/finish! c :abandoned (assoc decided :reason :danger))

      (child/exhausted? c :go)
      (end/finish! c :abandoned (assoc decided :reason :unreachable :detail (est/unreachable-detail c pos)))

      (dg/hit-since? c decided) (do (ctx/update-mem! c assoc :recheck true) :next)

      (and (= :collect (:phase (ctx/mem c))) (dg/threatened? c)) (dg/meet-danger! c (dg/threatened? c))

      (and (= :collect (:phase (ctx/mem c))) (> (u/dist (u/self-pos c) pos) (cfg/stray-range)))
      (do (ctx/update-mem! c assoc :phase :go) :next)

      (= :collect (:phase (ctx/mem c))) (await (fetch/collect! c pos radius-to-collect (:inventory (:data entry))))
      (and (est/deep-pile c pos) (:blocked (ctx/mem c))) (await (dig-down! c entry pos (est/deep-pile c pos)))
      :else (let [r (await (fetch/go! c pos))]
              (if (and (= :unreachable (:cause r)) (:blocked (ctx/mem c)) (est/deep-pile c pos)) :next r)))))

(defn ^:async step!
  "One step of the trip: :next to go on, or the round's end (an ending, or :continue for a cut)."
  [c entry]
  (cond
    (not (ctx/alive? c)) :continue
    (dth/window-closed? c entry)
    (end/finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason (if (:blocked (ctx/mem c)) :unreachable :window-closed)))
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
  with the ending of the trip (see the doc). A danger priced into a skip leaves can-run? :later :danger, a walk that did not arrive nothing-possible :unreachable, the death staying open."
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
              (await (pace/steps! c (fn ^:async recover-step []
                                      (let [entry (dth/current-death c)
                                            r (if entry (await (step! c entry)) (ctx/end c :condition-failed :no-drops))]
                                        (if (= :next r) :again r)))))))))))
