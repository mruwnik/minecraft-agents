(ns jobs.apiary.guard
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.apiary :as apiary :refer [survey]]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.step-off :as step-off]))

(def doc
  "Look around once, then keep the lit campfires of an apiary in the standard column: the fire one block underground with ground on all
  four sides, a carpet on it, then air, then the hive.
  - A raised fire (some side open, so bees fly in sideways) with real walled ground under it is sunk. The carpet
    is taken first, then the fire and the ground block under it are dug out, and a carried campfire is placed
    one block lower. Mining a campfire gives charcoal, not the fire, so one must be carried.
  - Every lit fire with nothing on it gets a non-moss carpet, since an open fire burns landing bees.
  One call works every fire (:continue only while a walk or the carpet pick-up waits on the world). Fires are
  worked nearest first, at most :max actions in a run. A fire that cannot be reached or refuses is
  skipped for the rest of the run. The body never stands in a fire's cell.
  A carpet or campfire the work needs and the body lacks is fetched (jobs.lib.need) unless :fetch is false; a
  failed fetch ends nothing-possible or condition-failed with :reason :need.
  Start facts, found after a quiet look around, make it wait (a skipped job): :nothing-to-guard (no fire in reach; fires
  that all need nothing end done :already), :no-carpet or :no-campfire (the body carries what the work needs, :fetch false), :refused or :no-zones
  (the zone rules refuse every fire).
  Result: {:sunk n :carpeted n :reason r :left {pos reason} :skipped {pos reason} :fires n}. :reason is
  :guarded (work done, nothing left), :limit (:max reached), :safe, :no-fire, :no-carpet, :no-campfire, :need, a skip
  reason (:unreachable, :on-fire, :occupied, :cannot, :place-failed, :refused) or :gave-up (3 fruitless fires
  in a row). Every end but :guarded and :limit also warns apiary.guard-gave-up.
  The ending: :done for :guarded, :limit, :safe, and any other reason when the call's :least (default 1; :all =
  every fire worked, so only those three) actions were done; else condition-failed for :no-fire, :no-carpet, :no-campfire, :occupied, :refused and :no-zones,
  nothing-possible for the rest, with the reason as the cause and the result as its data.
  Zones: a fire in another owner's zone or claim, or in a plan's footprint, is left out of the survey (all refused
  ends :no-fire). Each dig and place is checked again before it (a refusal skips the fire as :refused). The job
  warns apiary.guard-declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.")

(a/defargs args
  {:box {:doc "{:from pos :to pos}, fires inside it only; overrides :center and :radius" :spec (a/map-with {:from a/position? :to a/position?}) :default nil}
   :center {:doc "centre of the search; the body's position when the job first runs when nil" :spec ::a/pos :default nil}
   :radius {:doc "fires within this many blocks of the centre count, when :box is nil" :spec (a/num-in 0 nil) :default 16}
   :max {:doc "actions (sinks and carpets) in one run, at most" :spec (a/int-in 1 nil) :default 12}
   :ignore-zones? :shared
   :fetch {:doc "get a carpet or campfire that is not carried (jobs.lib.need): true, a set of kinds or a map of limits; false waits with :no-carpet / :no-campfire" :spec need/option? :default true}})

(defn permitted?
  "Whether the job may do action (:dig or :place) at pos (one warn per job when refused)."
  [c action pos]
  (gate/allowed? c :apiary.guard-declined "apiary guard" action pos))

(a/defargs settings
  {::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::max-strikes {:default 3 :doc "Strikes at one target before the job gives up on it." :spec (a/int-in 0 nil)}
   ::max-sink-steps {:default 8 :doc "Steps of the sink walk before it gives up." :spec (a/int-in 0 nil)}
   ::max-steps {:default 400 :doc "Steps of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds a guard run may take." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-strikes [] (settings/get settings ::max-strikes))

;; ------------------------------------------------------------------ the plan

(defn end-reason
  "Why the run ends with nothing to act on, given the memory and the survey."
  [m {:keys [left fires]}]
  (cond
    (seq (:skipped m)) (first (vals (:skipped m)))
    (seq left) (first (vals left))
    (pos? (+ (:sunk m 0) (:carpeted m 0))) :guarded
    (zero? fires) :no-fire
    :else :safe))

(defn actions [m] (+ (:sunk m 0) (:carpeted m 0)))

(def failed-conditions "Reasons that end condition-failed: the world or the zones bar the work. The rest are nothing-possible."
  #{:no-fire :no-carpet :no-campfire :occupied :refused :no-zones})

(defn ending-of
  "The ending of a run that stopped for reason: :done for :guarded, :limit and :safe or when the call's :least actions
  were done, else condition-failed for failed-conditions, else nothing-possible; result is its data."
  [c reason result]
  (let [least (:least (ctx/frozen-call c) 1)
        need (if (= :all least) ##Inf least)]
    (cond
      (or (#{:guarded :limit :safe} reason) (>= (actions (ctx/mem c)) need))
      (ctx/end c :done nil (assoc result :text (str "apiary guard done: " (name reason) ", sunk " (:sunk result) ", carpeted " (:carpeted result))))
      (failed-conditions reason) (ctx/end c :condition-failed reason result)
      :else (ctx/end c :nothing-possible reason result))))

(defn finish!
  "Emit the outcome and end the job (ending-of)."
  [c reason seen]
  (let [m (ctx/mem c)
        result {:sunk (:sunk m 0)
                :carpeted (:carpeted m 0)
                :reason reason
                :left (:left seen {})
                :skipped (:skipped m {})
                :fires (:fires seen 0)}]
    (when-not (#{:guarded :limit} reason)
      (ctx/emit! c :apiary.guard-gave-up :warn {:reason reason :left (:left result) :skipped (:skipped result)
                                                :text (str "apiary guard stopped: " (name reason))}))
    (ending-of c reason result)))

(defn skip!
  "Leave the fire alone for the rest of the run and count a fruitless fire."
  [c pos reason]
  (ctx/update-mem! c #(-> % (assoc-in [:skipped (apiary/pos-key pos)] reason) (update :strikes (fnil inc 0)))))

(defn booked!
  "Count a finished action of kind (:sunk or :carpeted) and clear the strikes."
  [c kind]
  (ctx/update-mem! c #(-> % (update kind (fnil inc 0)) (assoc :strikes 0))))

;; ------------------------------------------------------------------ the body

(defn on-fire?
  "True when the body's floored position is the fire's cell or the one above it."
  [c {:keys [x y z]}]
  (let [me (u/self-pos c)
        f (fn [k] (js/Math.floor (k me)))]
    (and (= x (f :x)) (= z (f :z)) (contains? #{y (inc y)} (f :y)))))

(defn ^:async clear-fire!
  "Move off the fire's cell once when standing in it. Resolves to true when clear."
  [c {:keys [x y z] :as fire}]
  (when (on-fire? c fire)
    (await (step-off/step-off-zoned! c fire {:avoid #{[x y z]}})))
  (not (on-fire? c fire)))

;; ------------------------------------------------------------------ carpet

(defn named-at [block-at pos] (some-> (block-at pos) .-name))

(defn ^:async carpet!
  "Place the carpet over the fire; the intent is saved first, so a cut after the placing still books it."
  [c pos item]
  (if-not (permitted? c :place (update pos :y inc))
    (skip! c pos :refused)
    (do (ctx/update-mem! c assoc :carpeting pos)
        (let [outcome (await (blocks/place-cell! c (update pos :y inc) item {:ignore-zones? true}))]
          (when-not (= :continue outcome)
            (ctx/update-mem! c dissoc :carpeting))
          (case outcome
            :continue :continue
            :placed (booked! c :carpeted)
            (skip! c pos outcome))))))

(defn settle-carpet!
  "A cut left a carpet placing unbooked: book it when the carpet stands."
  [c]
  (when-let [pos (:carpeting (ctx/mem c))]
    (ctx/update-mem! c dissoc :carpeting)
    (when-let [above (named-at (apiary/block-at-fn (:primitives c)) (update pos :y inc))]
      (when (apiary/carpet? above) (booked! c :carpeted)))))

;; ------------------------------------------------------------------ sink

(defn lava-beside?
  "Whether lava sits in one of the six cells round fire: its dig would be refused, so nothing is dug for it."
  [block-at {:keys [x y z]}]
  (some (fn [[dx dy dz]] (= "lava" (named-at block-at {:x (+ x dx) :y (+ y dy) :z (+ z dz)})))
        [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))

(defn next-step
  "The next idempotent step of a sink {:fire :kind :carpet} read off the world:
  {:op :dig/:place :pos}, :done when the lowered fire stands, :hazard when lava stands by the fire still to be dug,
  :abort when a cell cannot be read."
  [block-at {:keys [fire kind]}]
  (let [above (update fire :y inc)
        below (update fire :y dec)
        above-name (named-at block-at above)
        fire-name (named-at block-at fire)
        below-block (block-at below)]
    (cond
      (nil? below-block) {:op :abort}
      (contains? #{"campfire" "soul_campfire"} (.-name below-block)) {:op :done}
      (and (contains? #{"campfire" "soul_campfire"} fire-name) (lava-beside? block-at fire)) {:op :hazard}
      (and above-name (apiary/carpet? above-name)) {:op :dig :pos above}
      (contains? #{"campfire" "soul_campfire"} fire-name) {:op :dig :pos fire}
      (not= "air" (.-name below-block)) {:op :dig :pos below}
      :else {:op :place :pos below :item kind})))

(defn ^:async collect-carpet!
  "Pick up the carpet that was dug up."
  [c carpet]
  (await (ctx/call c :collect 'jobs.items.collect-drops {:radius 6 :items [carpet]})))

(defn abandon!
  "Give up the sink: clear it and skip the fire."
  [c pos reason]
  (ctx/update-mem! c dissoc :sinking)
  (skip! c pos reason))

(defn ^:async collect-step!
  "Pick up the dug carpet saved in mem :collecting: :continue while the pick-up waits, any other end closes it."
  [c]
  (let [r (await (collect-carpet! c (:collecting (ctx/mem c))))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :collecting) :again))))

(defn ^:async finish-sink! [c {:keys [carpet]}]
  (ctx/update-mem! c #(cond-> (dissoc % :sinking) carpet (assoc :collecting carpet)))
  (booked! c :sunk)
  (if carpet (await (collect-step! c)) :again))

(defn max-sink-steps [] (settings/get settings ::max-sink-steps))

(defn ^:async sink!
  "Run the steps of the sink in mem :sinking until the lowered fire stands."
  [c]
  (let [{:keys [fire] :as s} (:sinking (ctx/mem c))
        block-at (apiary/block-at-fn (:primitives c))]
    (loop [steps 0 prev nil]
      (let [{:keys [op pos item] :as step} (next-step block-at s)]
        (case (if (or (= step prev) (>= steps (max-sink-steps))) :stuck op)
          :stuck (do (abandon! c fire :cannot) :again)
          :done (await (finish-sink! c s))
          :abort (do (abandon! c fire :cannot) :again)
          :hazard (do (abandon! c fire :refused) :again)
          :dig (if-not (permitted? c :dig pos)
                 (do (abandon! c fire :refused) :again)
                 (let [outcome (await (blocks/dig-cell! c pos {:accept #{:fluid-adjacent :falling-block}
                                                               :ignore-zones? true}))]
                   (case outcome
                     :continue :continue
                     (:dug :missing) (recur (inc steps) step)
                     (do (abandon! c fire outcome) :again))))
          :place (if-not (permitted? c :place pos)
                   (do (abandon! c fire :refused) :again)
                   (let [outcome (await (blocks/place-cell! c pos item {:ignore-zones? true}))]
                     (case outcome
                       :continue :continue
                       (:placed :already) (await (finish-sink! c s))
                       (do (abandon! c fire :place-failed) :again)))))))))

(defn start-sink!
  "Remember the sink so a cut resumes it, then run it."
  [c fire kind]
  (let [above (named-at (apiary/block-at-fn (:primitives c)) (update fire :y inc))]
    (ctx/update-mem! c assoc :sinking {:fire fire :kind kind :carpet (when (and above (apiary/carpet? above)) above)})
    (sink! c)))

;; ------------------------------------------------------------------ rounds

(defn ^:async work!
  "Walk to the fire, stand clear of its cell and do the action once."
  [c {:keys [pos action kind item]}]
  (let [w (await (near/go-near! c pos (reach) {:ways #{:walk :clear-seal}}))]
    (case w
      :partial :continue
      :blocked (do (skip! c pos :unreachable) :again)
      (if-not (await (clear-fire! c pos))
        (do (skip! c pos :on-fire) :again)
        (if (= :sink action)
          (await (start-sink! c pos kind))
          (if (= :continue (await (carpet! c pos item))) :continue :again))))))

(defn ^:async resume-sink!
  "A cut left a sink half done: walk back and run its steps again."
  [c]
  (let [{:keys [fire]} (:sinking (ctx/mem c))
        w (await (near/go-near! c fire (reach) {:ways #{:walk :clear-seal}}))]
    (case w
      :partial :continue
      :blocked (do (abandon! c fire :unreachable) :again)
      (if (await (clear-fire! c fire))
        (await (sink! c))
        (do (abandon! c fire :on-fire) :again)))))

(def target [:box :center])

(def meta {:category :phase :part-of '[jobs.apiary.maintain]})

(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)


(def defaults {:least 1 :spend {:job-time ::job-time-s}})

(defn can-start?
  "Later with the cause that bars the first step: no fire needs work, the body carries no carpet or campfire the work
  needs, or the zone rules refuse every fire. A look first while the body has not looked round near here."
  [c]
  (let [{:keys [box radius center]} (:args c)
        center (or center (u/self-pos c))
        fires (apiary/fires (:primitives c) {:box box :center center :radius radius})
        seen (survey c center true)
        reason (cond
                 (seq (:todo seen)) nil
                 (and (seq (:left seen)) (nil? (need/parse-arg (:fetch (:args c))))) (first (vals (:left seen)))
                 (seq (:left seen)) nil
                 (and (seq fires) (zero? (:fires seen))) (or (:refusal (gate/look c :dig (map :pos fires) {})) :refused)
                 :else :nothing-to-guard)]
    (cond
      (nil? reason) (contract/ok :start "a fire needs work")
      (look/surveyed? c :near) (contract/later reason (str "guard waits: " (name reason)))
      :else (contract/look reason (str "guard waits: " (name reason))))))

(defn done?
  "Ok before any work, after a look round, when fires stand in the area and none needs work; later with the fires
  worked and the fires still to work: the run ends itself with the reason it stopped for."
  [c]
  (let [seen (survey c (apiary/center-of c) true)
        m (ctx/mem c)
        left (+ (count (:todo seen)) (count (:left seen)))]
    (if (and (zero? left) (pos? (:fires seen)) (zero? (actions m)) (empty? (:skipped m)) (look/surveyed? c :near))
      (contract/ok :safe "every fire is guarded" :units :fires :done 0 :left 0
                   :result {:sunk 0 :carpeted 0 :reason :safe :left {} :skipped {} :fires (:fires seen)})
      (contract/later :guarding (str (actions m) " fires worked") :units :fires :done (actions m) :left left))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 10 (or (:max (:args c)) 12)) ##Inf]}})

(def lacked-item {:no-carpet "white_carpet" :no-campfire "campfire"})

(defn ^:async supply!
  "Fetch what the fires left lack when :fetch allows: :again once fetched, :continue while the fetch waits, else the
  ending of the run (reason :need, the fetch's ending as its cause); nil when nothing is lacking or :fetch is off."
  [c seen]
  (let [opts (need/opts c 'jobs.apiary.guard)
        lacks (some lacked-item (vals (:left seen)))]
    (when (and opts lacks)
      (let [e (await (need/item! c lacks 1 opts))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) :again
          :else (let [m (ctx/mem c)]
                  (ctx/emit! c :apiary.guard-gave-up :warn {:reason :need :text "apiary guard stopped: need"})
                  (ctx/end c (:kind e) (:cause e) {:sunk (:sunk m 0) :carpeted (:carpeted m 0) :reason :need
                                                   :left (:left seen {}) :skipped (:skipped m {}) :fires (:fires seen 0)})))))))

(defn ^:async step
  "One piece of the guarding: resume a sink; else survey the fires and finish when the budget is spent, three fires
  in a row failed or none can be worked; else act on the nearest workable fire. :again, :continue while a walk or
  collect waits on the world, or :done."
  [c]
  (let [center (apiary/center-of c)
        _ (ctx/update-mem! c assoc :center center)
        _ (settle-carpet! c)
        _ (await (look/survey! c))
        m (ctx/mem c)]
    (cond
      (:collecting m) (await (collect-step! c))
      (:sinking m) (await (resume-sink! c))
      :else
      (let [seen (survey c center)
            target (first (:todo seen))]
        (cond
          (>= (actions m) (:max (:args c))) (finish! c :limit seen)
          (>= (:strikes m 0) (max-strikes)) (finish! c :gave-up seen)
          (nil? target) (or (await (supply! c seen)) (finish! c (end-reason m seen) seen))
          :else (await (work! c target)))))))

(defn max-steps [] (settings/get settings ::max-steps))

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue)))))
