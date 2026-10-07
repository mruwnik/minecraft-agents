(ns jobs.apiary.guard
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
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
  Result: {:sunk n :carpeted n :reason r :left {pos reason} :skipped {pos reason} :fires n}. :reason is
  :guarded (work done, nothing left), :limit (:max reached), :safe, :no-fire, :no-carpet, :no-campfire, a skip
  reason (:unreachable, :on-fire, :occupied, :cannot, :place-failed, :refused) or :gave-up (3 fruitless fires
  in a row). Every end but :guarded and :limit also warns apiary.guard-gave-up.
  Zones: a fire in another owner's zone or claim, or in a plan's footprint, is left out of the survey (all refused
  ends :no-fire). Each dig and place is checked again before it (a refusal skips the fire as :refused). The job
  warns apiary.guard-declined once, with :reason :refused (or :no-zones when no zone list was read).
  :ignore-zones? true skips the check.")

(a/defargs args
  {:box {:doc "{:from pos :to pos}, fires inside it only; overrides :center and :radius" :spec (a/map-with {:from a/position? :to a/position?}) :default nil}
   :center {:doc "centre of the search; the body's position when the job first runs when nil" :spec ::a/pos :default nil}
   :radius {:doc "fires within this many blocks of the centre count, when :box is nil" :spec (a/num-in 0 nil) :default 16}
   :max {:doc "actions (sinks and carpets) in one run, at most" :spec (a/int-in 1 nil) :default 12}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(defn permitted?
  "Whether the job may do action (:dig or :place) at pos (one warn per job when refused)."
  [c action pos]
  (gate/allowed? c :apiary.guard-declined "apiary guard" action pos))

(defn permitted-fires
  "The fires of the area whose cell the job may dig and the cell over it place on."
  [c fires]
  (let [dig (set (gate/allowed c :apiary.guard-declined "apiary guard" :dig (map :pos fires)))
        place (set (gate/allowed c :apiary.guard-declined "apiary guard" :place (map #(update (:pos %) :y inc) fires)))]
    (filterv #(and (dig (:pos %)) (place (update (:pos %) :y inc))) fires)))

(def reach 3)
(def max-strikes 3)

;; ------------------------------------------------------------------ the plan

(defn plan-fire
  "What to do about one fire: nil when it is safe, {:pos :action :kind/:item} when something carried does it,
  else {:pos :left reason}."
  [block-at inventory {:keys [pos name]}]
  (let [ns (apiary/needs block-at pos)
        kind (apiary/campfire-in inventory name)
        carpet (apiary/carpet-in inventory)]
    (cond
      (empty? ns) nil
      (and (:sink ns) kind) {:pos pos :action :sink :kind kind}
      (and (:carpet ns) carpet) {:pos pos :action :carpet :item carpet}
      :else {:pos pos :left (if (:carpet ns) :no-carpet :no-campfire)})))

(defn survey
  "{:fires n :todo [action maps, nearest first] :left {key reason}} of the fires not skipped."
  [c center]
  (let [p (:primitives c)
        block-at (apiary/block-at-fn p)
        inventory (u/inventory p)
        skipped (:skipped (ctx/mem c) {})
        all (permitted-fires c (apiary/fires p {:box (:box (:args c)) :center center :radius (:radius (:args c))}))
        plans (->> all
                   (remove #(contains? skipped (apiary/pos-key (:pos %))))
                   (keep #(plan-fire block-at inventory %)))]
    {:fires (count all)
     :todo (vec (filter :action plans))
     :left (into {} (keep (fn [pl] (when (:left pl) [(apiary/pos-key (:pos pl)) (:left pl)])) plans))}))

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

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason seen]
  (let [m (ctx/mem c)
        result {:sunk (:sunk m 0)
                :carpeted (:carpeted m 0)
                :reason reason
                :left (:left seen {})
                :skipped (:skipped m {})
                :fires (:fires seen 0)}]
    (ctx/emit! c :apiary.guard-done :info (assoc result :text (str "apiary guard done: " (name reason) ", sunk " (:sunk result) ", carpeted " (:carpeted result))))
    (when-not (#{:guarded :limit} reason)
      (ctx/emit! c :apiary.guard-gave-up :warn {:reason reason :left (:left result) :skipped (:skipped result)
                                                :text (str "apiary guard stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

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

(defn next-step
  "The next idempotent step of a sink {:fire :kind :carpet} read off the world:
  {:op :dig/:place :pos}, :done when the lowered fire stands, :abort when a cell cannot be read."
  [block-at {:keys [fire kind]}]
  (let [above (update fire :y inc)
        below (update fire :y dec)
        above-name (named-at block-at above)
        fire-name (named-at block-at fire)
        below-block (block-at below)]
    (cond
      (nil? below-block) {:op :abort}
      (contains? #{"campfire" "soul_campfire"} (.-name below-block)) {:op :done}
      (and above-name (apiary/carpet? above-name)) {:op :dig :pos above}
      (not= "air" (.-name below-block)) {:op :dig :pos below}
      (contains? #{"campfire" "soul_campfire"} fire-name) {:op :dig :pos fire}
      :else {:op :place :pos below :item kind})))

(defn ^:async collect-carpet!
  "Pick up the carpet that was dug up."
  [c carpet]
  (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius 6 :filter [carpet]})))

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

(def max-sink-steps 8)

(defn ^:async sink!
  "Run the steps of the sink in mem :sinking until the lowered fire stands."
  [c]
  (let [{:keys [fire] :as s} (:sinking (ctx/mem c))
        block-at (apiary/block-at-fn (:primitives c))]
    (loop [steps 0 prev nil]
      (let [{:keys [op pos item] :as step} (next-step block-at s)]
        (case (if (or (= step prev) (>= steps max-sink-steps)) :stuck op)
          :stuck (do (abandon! c fire :cannot) :again)
          :done (await (finish-sink! c s))
          :abort (do (abandon! c fire :cannot) :again)
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
  (let [w (await (near/go-near! c pos reach {:zone-tolls true :escalate false}))]
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
        w (await (near/go-near! c fire reach {:zone-tolls true :escalate false}))]
    (case w
      :partial :continue
      :blocked (do (abandon! c fire :unreachable) :again)
      (if (await (clear-fire! c fire))
        (await (sink! c))
        (do (abandon! c fire :on-fire) :again)))))

(defn check
  "True while a run is under way or some lit fire in the area lacks a carpet or sits too high."
  [c]
  (or (boolean
       (or (:started (ctx/mem c))
           (let [p (:primitives c)
                 block-at (apiary/block-at-fn p)
                 {:keys [box radius]} (:args c)
                 center (or (:center (:args c)) (u/self-pos c))]
             (some #(seq (apiary/needs block-at (:pos %))) (permitted-fires c (apiary/fires p {:box box :center center :radius radius}))))))
      (ctx/wait c {:reason :nothing-to-guard})))

(defn ^:async step
  "One piece of the guarding: resume a sink; else survey the fires and finish when the budget is spent, three fires
  in a row failed or none can be worked; else act on the nearest workable fire. :again, :continue while a walk or
  collect waits on the world, or :done."
  [c]
  (let [center (apiary/center-of c)
        _ (ctx/update-mem! c assoc :started true :center center)
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
          (>= (:strikes m 0) max-strikes) (finish! c :gave-up seen)
          (nil? target) (finish! c (end-reason m seen) seen)
          :else (await (work! c target)))))))

(def max-steps "Steps of one call before it gives the round back with :continue." 400)

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))
