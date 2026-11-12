(ns jobs.lib.child
  "Run a child job to its end inside the parent's round, for children that still answer :continue per step."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.pace :as pace]
            [jobs.lib.waits :as waits]))

(a/defargs settings
  {::default-max-calls {:default 400 :doc "Calls of one run!* before it gives the round back with :continue." :spec (a/int-in 1 nil)}})

(defn default-max-calls [] (settings/get settings ::default-max-calls))

(defn pause!
  "An intentional pause of seconds, holding still with why: a jobs.time.noop child in slot :pause, so reflexes still
  cut in and the time counts on the caller. Resolves to its ending."
  [c seconds why]
  (ctx/call c :pause 'jobs.time.noop {:seconds seconds :why why}))

(defn ^:async poll!
  "Read (look) now and again after each pause of ms, tries pauses at most: the first truthy reading, nil when none
  came, :continue when a pause was cut (the caller yields and polls again on resume)."
  [c look ms tries why]
  (loop [n tries]
    (let [v (look)]
      (cond
        v v
        (zero? n) nil
        :else (let [e (await (pause! c (/ ms 1000) why))]
                (if (= :continue e) :continue (recur (dec n))))))))

(defn ^:async use-on!
  "Click pos with item (nil: as held) on face through a jobs.items.use child in slot :click (the body is in reach:
  it walks only when not). Resolves to :continue while the child waits on the world, else {:status :raw}: status :used
  or the child's reason (:missing :no-item :cannot :unchanged :unreachable ...), raw the primitive's answer (nil when
  none came)."
  [c pos item face]
  (let [e (await (ctx/call c :click 'jobs.items.use
                           (cond-> (merge (select-keys (:args c) [:ignore-zones?]) {:on pos :face face})
                             item (assoc :item item))))]
    (if (= :continue e)
      :continue
      {:status (if (= :done (:kind e)) :used (or (:reason (:data e)) (contract/cause-key e) :failed))
       :raw (:raw (:data e))})))

(defn fruitless
  "Rounds in a row of slot's child that brought nothing (counted with fruitless!), kept in job memory."
  [c slot]
  (get-in (ctx/mem c) [:fruitless slot] 0))

(defn declined?
  "Whether child ending e is a refusal before it ran: a failed condition with no result of its own."
  [e]
  (and (= :condition-failed (:kind e)) (empty? (:data e))))

(defn begin!
  "slot's round starts now, unless one is under way (fruitless! and progress! end it): fruitless! prices the round from
  here, so the caller's own work in it (a walk, a feed) counts."
  [c slot]
  (when-not (get-in (ctx/mem c) [:round-t slot])
    (ctx/update-mem! c assoc-in [:round-t slot] (ctx/now c))))

(defn round-s
  "The seconds of slot's round ending in e: from begin! when it was called, else e's :wall-time; nil (the kind's
  seconds) when the child never ran (declined?)."
  [c slot e]
  (when-not (declined? e)
    (if-let [t (get-in (ctx/mem c) [:round-t slot])]
      (/ (- (ctx/now c) t) 1000)
      (get-in e [:report :uses :wall-time]))))

(defn fruitless!
  "Count one more round of slot's child that brought nothing, keeping its ending e (if given, and not :done) for give-up
  and its seconds (round-s) as the price of the next (exhausted?); the count so far, this one included."
  ([c slot] (fruitless! c slot nil))
  ([c slot e]
   (let [s (round-s c slot e)]
     (ctx/update-mem! c #(cond-> (-> (update-in % [:fruitless slot] (fnil inc 0))
                                     (assoc-in [:fruitless-s slot] s)
                                     (update :round-t dissoc slot))
                           (and (map? e) (not= :done (:kind e))) (assoc-in [:fruitless-ending slot] (dissoc e :report)))))
   (fruitless c slot)))

(defn exhausted?
  "Whether another round of slot's child after its fruitless ones in a row is not worth its price: never before one;
  else not cost/try-again? of kind (default :child-round) at the call's worth, the round priced at the seconds the
  last fruitless one took, else (none, or one that ended at once) at the kind's."
  ([c slot] (exhausted? c slot :child-round))
  ([c slot kind]
   (let [n (fruitless c slot)
         s (get-in (ctx/mem c) [:fruitless-s slot])]
     (and (pos? n)
          (not (cost/try-again? c kind (cond-> {:fails n} (and (number? s) (pos? s)) (assoc :try-s s))))))))

(defn give-up
  "The ending of a caller whose slot's child is exhausted: nothing-possible with cause reason and data, the child's last
  fruitless ending (counted with fruitless!) as its :because."
  [c slot reason data]
  (contract/with-because (ctx/end c :nothing-possible reason data) (get-in (ctx/mem c) [:fruitless-ending slot])))

(defn progress!
  "slot's child brought something: its fruitless rounds start again from none."
  [c slot]
  (ctx/update-mem! c (fn [m] (reduce #(update %1 %2 dissoc slot) m [:fruitless :fruitless-ending :fruitless-s :round-t]))))

(defn ^:async call-walk*
  "A go-to child in slot with args through ctx/call: :continue while the walk goes on (or waits: go-to's safe? says
  later, which makes this round wait with its reason), else go-to's ending."
  [c slot args]
  (waits/waiting-out c (await (ctx/call c slot 'jobs.movement.go-to args))))

(defn ^:async call-chase*
  "A jobs.movement.chase child in slot (one leg toward a moving thing; args as chase's, the go-to call under :walk):
  :continue while it waits, else its ending, a leg {:leg true} included."
  [c slot args]
  (waits/waiting-out c (await (ctx/call c slot 'jobs.movement.chase args))))

(defn ^:async run-with
  "run!* with the child call fn. A child that waits on a check (child-wait) gives the round back at once."
  [call c slot job args max-calls]
  (when (not= args (get-in (ctx/mem c) [:child-args slot]))
    (ctx/update-mem! c #(-> % (update :children dissoc slot) (assoc-in [:child-args slot] args))))
  (loop [n 1]
    (let [r (waits/waiting-out c (await (call c slot job args)))]
      (if (and (= :continue r) (nil? (some-> (:child-wait c) deref)) (< n max-calls) (ctx/alive? c))
        (do (await (pace/pace! c)) (recur (inc n)))
        r))))

(defn run!*
  "run! through ctx/call: resolves to the child's ending, or :continue after :max-calls calls."
  ([c slot job args] (run!* c slot job args {}))
  ([c slot job args {:keys [max-calls] :or {max-calls (default-max-calls)}}]
   (run-with ctx/call c slot job args max-calls)))

(def entry-depth "Blocks below both ends a walk to or from a line's entry may plan." 16)

(defn walk-into-args [slot cell]
  (cond-> {:pos cell :range 0 :ways #{:walk :clear-seal}}
    (#{:in :out} slot) (assoc :settings {:jobs.lib.walk.world/y-margin entry-depth})))

(def back-ways "A walk back to a line the body was cutting digs through a cliff, as a player with a pickaxe would; no stair or pillar inside a stair." #{:walk :clear-seal :dig-route})

(defn walk-back-args "The walk (and, via jobs.lib.price/walk-back-worth?, the quote) of a way back to `cell`: one place for its ways." [slot cell]
  (assoc (walk-into-args slot cell) :ways back-ways))

(defn ^:async walk-into!*
  "walk-into! through ctx/call: :continue while it walks, else go-to's ending."
  [c slot cell]
  (await (call-walk* c slot (walk-into-args slot cell))))

(defn search-args
  "The args of a jobs.explore.search call from caller options: :max-distance is the search's :within cap."
  [opts]
  (cond-> (dissoc opts :max-distance)
    (:max-distance opts) (assoc :within (:max-distance opts))))
